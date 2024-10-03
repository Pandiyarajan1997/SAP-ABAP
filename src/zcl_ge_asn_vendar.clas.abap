class ZCL_GE_ASN_VENDAR definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GE_ASN_VENDAR IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             vendor TYPE string,
             asnno  TYPE string,
           END OF ty_input.

    TYPES: BEGIN OF ty_input1,
             vendor TYPE lifnr,
             asnno  TYPE zasnno,
           END OF ty_input1.
    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_input_tb TYPE TABLE OF ty_input1,
          gs_input_tb TYPE ty_input1.

    TYPES: BEGIN OF ty_msg,
             vendor  TYPE string,
             asnno   TYPE string,
             migo_no TYPE string,
             po_no   TYPE string,
             status  TYPE string,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.


    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: gv_fiscalyr TYPE bapi0002_4-fiscal_year,
          gv_fiscalp  TYPE bapi0002_4-fiscal_period.
    DATA:lv_date TYPE char20.
    DATA: lv_bukrs  TYPE bukrs,
          lv_kunnr  TYPE kunnr,
          lv_amt    TYPE wrbtr,
          lv_refdoc TYPE xblnr1.
    DATA: lv_response TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_data TYPE string.
*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
    REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.

    lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.
*    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.
    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = lr_data ).
*** Deserializing Input JSON to Internal table ***
    IF lr_data IS BOUND.

      ASSIGN lr_data->* TO <data>.
      ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
      ASSIGN <results>->* TO <table>.


      REFRESH gt_input_tb.
      LOOP AT <table> ASSIGNING <structure>.
        ASSIGN <structure>->* TO <data>.

        ASSIGN COMPONENT `VENDOR` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
*          lr_data = <field>.
          ASSIGN <field>->* TO <field_value>.
          gs_input-vendor = <field_value>.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gs_input-vendor
            IMPORTING
              output = gs_input_tb-vendor.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        ASSIGN COMPONENT `ASNNO` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
*          lr_data = <field>.
          ASSIGN <field>->* TO <field_value>.
          gs_input-asnno = <field_value>.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gs_input-asnno
            IMPORTING
              output = gs_input_tb-asnno.
        ENDIF.
        APPEND gs_input_tb TO gt_input_tb.
        UNASSIGN: <field>, <field_value>.
        CLEAR gs_input_tb.
      ENDLOOP.
    ENDIF.

    SELECT asnno,
           lifnr AS vendor
       FROM zmm_ge_asn
      INTO TABLE @DATA(lt_ge_asn)
      FOR ALL ENTRIES IN @gt_input_tb
      WHERE lifnr = @gt_input_tb-vendor AND
            asnno = @gt_input_tb-asnno.

    SORT lt_ge_asn BY  vendor asnno.
    DELETE ADJACENT DUPLICATES FROM lt_ge_asn COMPARING ALL FIELDS.
    SELECT mblnr,
           asnno,
           lifnr AS vendor,
           ebeln
       FROM zmm_ge_asn_migo
      INTO TABLE @DATA(lt_ge_asn_mogo)
      FOR ALL ENTRIES IN @gt_input_tb
      WHERE lifnr = @gt_input_tb-vendor AND
            asnno = @gt_input_tb-asnno.

    LOOP AT gt_input_tb INTO DATA(lw_input).
      CLEAR gw_response.
      gw_response-vendor = VALUE #( lt_ge_asn[ vendor = lw_input-vendor ]-vendor OPTIONAL ).
      gw_response-asnno = VALUE #( lt_ge_asn[ asnno = lw_input-asnno ]-asnno OPTIONAL ).
      IF gw_response-vendor IS INITIAL .
        gw_response-vendor = lw_input-vendor .
        gw_response-asnno = lw_input-asnno.
        gw_response-status = 'Vendor Not Found'.
        APPEND gw_response TO gt_response.
      ENDIF.
      IF gw_response-asnno IS INITIAL.
        gw_response-vendor = lw_input-vendor .
        gw_response-asnno = lw_input-asnno.
        gw_response-status = 'ASN No Not Found'.
        APPEND gw_response TO gt_response.

      ENDIF.
    ENDLOOP.

    LOOP AT lt_ge_asn ASSIGNING FIELD-SYMBOL(<fs_output1>).
      LOOP AT lt_ge_asn_mogo ASSIGNING FIELD-SYMBOL(<fs_output>)
                            WHERE vendor = <fs_output1>-vendor AND
                                  asnno = <fs_output1>-asnno.
        gw_response-vendor = <fs_output>-vendor.
        gw_response-asnno = <fs_output>-asnno.
        gw_response-migo_no = <fs_output>-mblnr.
        IF gw_response-po_no IS INITIAL.
          gw_response-po_no = |-{ <fs_output>-ebeln }|.
        ELSE.
          gw_response-po_no = |{ gw_response-po_no }-{ <fs_output>-ebeln }|.
        ENDIF.
        gw_response-status = 'Success'.
      ENDLOOP.
      APPEND gw_response TO gt_response.
      CLEAR gw_response.
    ENDLOOP.

    IF sy-subrc <> 0.
      gw_response-vendor = <fs_output1>-vendor .
      gw_response-asnno = <fs_output1>-asnno.
      gw_response-status = 'Not Found'.
    ENDIF.
*** Response Part ***
    IF gt_response[] IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
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
