class ZCL_API_CUSTMAT_ROL_MIS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_CUSTMAT_ROL_MIS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
**----------------------------------------------------------------------*
    "Created by: Samsudeen M
    "Created on: 20.02.2023
    "Purpose: Automatic Customer Material ROL Updation from MIS
    "Reference by: Ramakrishnan J
*-----------------------------------------------------------------------*
    TYPES: BEGIN OF ty_response,
             customer TYPE kunnr,
             material TYPE matnr,
             type     TYPE bapi_mtype,
             message  TYPE string,
           END OF ty_response.
    DATA: gt_output TYPE TABLE OF ty_response.
    DATA: gt_input  TYPE ztt_custmat_rol,
          gs_input1 TYPE zstr_custmat_rol.

    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    DATA: lv_response1 TYPE string.

    DATA: lv_body TYPE string.

*Get Body data
    DATA: lv_data TYPE string.
    DATA: lv_remarks TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    FIELD-SYMBOLS: <fgs_input> TYPE zstr_custmat_rol.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

*    DATA(lv_data_tmp) = |{ '[ ' } { lv_data } { ']' }|.
** Deserialize the input our required input ***
    REFRESH: gt_input.
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
     data         = gt_input ).

*---------- Updating the table based on Presence and absence of lineitems -------------*
    SELECT * FROM zcustmat_rol INTO TABLE @DATA(lt_custmat_rol)
                               FOR ALL ENTRIES IN @gt_input
                               WHERE kunnr = @gt_input-customer
                               AND matnr = @gt_input-material.
    IF sy-subrc = 0.
      SORT lt_custmat_rol[] BY kunnr matnr.
    ENDIF.

    LOOP AT gt_input ASSIGNING <fgs_input>.
      DATA(l_customer) = |{ <fgs_input>-customer ALPHA = IN }|.
      DATA(l_material) = CONV matnr18( <fgs_input>-material ).
*---- Customer Number Existence Checks ----------*
      SELECT SINGLE kunnr FROM kna1 INTO @DATA(lv_customer) WHERE kunnr = @l_customer.
      IF sy-subrc NE 0.
        APPEND VALUE #( customer = l_customer
                        material = l_material
                        type = 'E'
                        message = |Customer { <fgs_input>-customer } Does not exists| ) TO gt_output.
        CONTINUE.
      ENDIF.
*---- Material Number Existence Checks ----------*
      SELECT SINGLE matnr FROM mara INTO @DATA(lv_material) WHERE matnr = @l_material.
      IF sy-subrc NE 0.
        APPEND VALUE #( customer = l_customer
                        material = l_material
                        type = 'E'
                        message = |Material Number { <fgs_input>-material } Does not exists| ) TO gt_output.
        CONTINUE.
      ENDIF.
      DATA(ls_custmat_rol) = VALUE #( lt_custmat_rol[ kunnr = l_customer
                                                      matnr = l_material ] OPTIONAL ).
*------ If No entry Presents in ROL table New entry will be inserted -------------------------------------*
      IF ls_custmat_rol IS INITIAL.

        DATA(ls_update) = VALUE zcustmat_rol( kunnr = <fgs_input>-customer
                                              matnr = <fgs_input>-material
                                              new_val = <fgs_input>-rol_qty
                                              new_dat = sy-datum
                                              new_cre =  sy-uname ).
        INSERT zcustmat_rol FROM ls_update.
        APPEND VALUE #( customer = <fgs_input>-customer
                        material = <fgs_input>-material
                        type = 'S'
                        message = |New Entry Inserted in Table Successfully| ) TO gt_output.
        CONTINUE.
        " Already Entry Presents in Table ZCUSTMAT_ROL -------------------------------------------------------------*
      ELSE.
        ls_custmat_rol-old_val = ls_custmat_rol-new_val.
        ls_custmat_rol-old_dat = ls_custmat_rol-new_dat.
        ls_custmat_rol-old_cre = ls_custmat_rol-new_cre.
        ls_custmat_rol-new_val = <fgs_input>-rol_qty.
        ls_custmat_rol-new_dat = sy-datum.
        ls_custmat_rol-new_cre = sy-uname.
        MODIFY zcustmat_rol FROM ls_custmat_rol.
        APPEND VALUE #( customer = <fgs_input>-customer
                        material = <fgs_input>-material
                        type = 'S'
                        message = |Entry Updated in Table Successfully| ) TO gt_output.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    IF gt_output IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'CUSTMATROL'
          ijson           = lv_data
          ojson            = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

    ENDIF.

*------  API Response After Processed -----------------------------------------------------*

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).



  ENDMETHOD.
ENDCLASS.
