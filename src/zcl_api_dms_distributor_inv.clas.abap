class ZCL_API_DMS_DISTRIBUTOR_INV definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_DISTRIBUTOR_INV IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 23.02.2023
*&Purpose : For Filling Log table of DMS Distributor Purchasing Process
*&Reference: Ramakrishnan J & Praveen Kumar & Gopal raja
*-------------------------------------------------------------------------------------

    TYPES: BEGIN OF input,
             invoice_no   TYPE vbeln_vf,
             invoice_date TYPE fkdat,
           END OF input.
    DATA: gt_input TYPE TABLE OF input,
          gs_input TYPE input.

    TYPES: BEGIN OF output,
             invoice_no   TYPE vbeln_vf,
             invoice_date TYPE fkdat,
             type         TYPE bapi_mtype,
             msg          TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.


    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
    REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.

    lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.
    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.
    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = lr_data ).

    IF lr_data IS BOUND.

      ASSIGN lr_data->* TO <data>.
      ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
      ASSIGN <results>->* TO <table>.

      REFRESH gt_input.
      LOOP AT <table> ASSIGNING <structure>.
        ASSIGN <structure>->* TO <data>.
        ASSIGN COMPONENT `INVOICE_NO` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          gs_input-invoice_no = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        ASSIGN COMPONENT `INVOICE_DATE` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          gs_input-invoice_date = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        APPEND gs_input TO gt_input.
        CLEAR gs_input.
      ENDLOOP.
    ENDIF.

    REFRESH gt_output.
    LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_inv>).
      DATA(l_vbeln) = |{ <fs_inv>-invoice_no ALPHA = IN }|.
*------- Billing Document Number Existence Checks -------------*
      SELECT SINGLE * FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @l_vbeln AND fkdat = @<fs_inv>-invoice_date.
      IF sy-subrc NE 0.
        APPEND VALUE #( invoice_no = <fs_inv>-invoice_no
                        invoice_date = <fs_inv>-invoice_date
                        type = 'E'
                        msg = |Billing Document { <fs_inv>-invoice_no } does not exists| ) TO gt_output.
        CONTINUE.
      ENDIF.
*------- Already Billing Document Number Exists in log table checks ----*
      SELECT SINGLE * FROM zmm_dms_purchase INTO @DATA(ls_log) WHERE vbeln = @l_vbeln AND fkdat = @<fs_inv>-invoice_date.
      IF sy-subrc EQ 0.
        APPEND VALUE #( invoice_no = <fs_inv>-invoice_no
                        invoice_date = <fs_inv>-invoice_date
                        type = 'E'
                        msg = |Billing Document { <fs_inv>-invoice_no } Already Present in log table| ) TO gt_output.
        CONTINUE.
      ELSE.
        "Update Log Table
        DATA(ls_new_entry) = VALUE zmm_dms_purchase( vbeln = l_vbeln
                                                     fkdat = <fs_inv>-invoice_date
                                                     kunnr = ls_vbrk-kunag
                                                     status = '11' ).
        INSERT zmm_dms_purchase FROM ls_new_entry.
*------Sucessfully Updated in Table ---------------------------*
        APPEND VALUE #( invoice_no = l_vbeln
                        invoice_date = <fs_inv>-invoice_date
                        type = 'S'
                        msg = |Billing Document { <fs_inv>-invoice_no } Inserted in Log Table| ) TO gt_output.
      ENDIF.
    ENDLOOP.

    IF gt_output[] IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
