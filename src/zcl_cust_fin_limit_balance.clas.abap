class ZCL_CUST_FIN_LIMIT_BALANCE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUST_FIN_LIMIT_BALANCE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             customer TYPE kunnr,
           END OF ty_input.

    DATA: gt_input TYPE STANDARD TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    TYPES: BEGIN OF ty_msg,
             Customer   TYPE  kunnr,
             Name       TYPE  name1_gp,
             Type       TYPE  BAPI_MTYPE,
             Message    TYPE  ZREMARK,
             Bor_code   TYPE  zrfcustcode,
             Cus_status TYPE  zaxis1_dte_status,
             Comp       TYPE  bukrs,
             fintype    TYPE zfincode,
             acnt_id    TYPE zacnt_id,
             acnt_sts   TYPE zaxis1_dte_status,
             cr_limit   TYPE zcrlimit,
             Cr_balance TYPE zbalnce,
             inv_amnt   TYPE netwr,
             final_bal  TYPE wrbtr,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.

    DATA: lv_data TYPE string.

    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: l_msg TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REFRESH: gt_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ).


    IF gt_input[] IS NOT INITIAL.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
**Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_input>-customer
          IMPORTING
            output = <fs_input>-customer.
      ENDLOOP.

    ENDIF.

    IF gt_input[] IS NOT INITIAL.
      SELECT * FROM zsd_fin_cust_bal
        INTO TABLE @DATA(lt_fincustbal)
        FOR ALL ENTRIES IN @gt_input
        WHERE custno = @gt_input-customer.
    ELSE.
      SELECT * FROM zsd_fin_cust_bal
        INTO TABLE lt_fincustbal.
    ENDIF.

    IF lt_fincustbal[] IS NOT INITIAL.
      LOOP AT lt_fincustbal ASSIGNING FIELD-SYMBOL(<fs_fincustbal>).
        APPEND VALUE #(   Customer   = <fs_fincustbal>-custno
                          Name       = <fs_fincustbal>-custname
                          type       = <fs_fincustbal>-type
                          Message    = <fs_fincustbal>-msg
                          Bor_code   = <fs_fincustbal>-rf_custcode
                          Cus_status = <fs_fincustbal>-cust_sts
                          Comp       = <fs_fincustbal>-bukrs
                          fintype    = <fs_fincustbal>-fintype
                          acnt_id    = <fs_fincustbal>-acnt_id
                          acnt_sts   = <fs_fincustbal>-acnt_sts
                          cr_limit   = <fs_fincustbal>-cr_limit
                          Cr_balance = <fs_fincustbal>-balance
                          inv_amnt   = <fs_fincustbal>-inv_amnt
                          final_bal  = <fs_fincustbal>-final_amnt  ) TO gt_response.

      ENDLOOP.
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

*      v_jsonload = | { '[' } { lv_body } { ']' } |.
      v_jsonload = lv_body.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'CUS_CFFIN_BAL'
          ijson           = lv_data
          ojson           = v_jsonload
          distributor     = gs_input-customer
          retailer        = gs_input-customer
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
