class ZCL_API_INV_BYPASS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_INV_BYPASS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*&Created by: Pandiarajan
*&Created On: 08.11.2023
*&Purpose : For Filling data customer invoice to ZSD_INV_CUSTOMER fetching from vbrk
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             comcode     TYPE bukrs,
             invoiceno   TYPE vbeln_vf,
             invoicedate TYPE fkdat,
             emp_req     TYPE pernr_d,
             emp_appr    TYPE pernr_d,
             remarks     TYPE zci_remarks,
           END OF ty_input.
    TYPES: BEGIN OF ty_msg,
             invoiceno   TYPE vbeln,
             comcode     TYPE bukrs,
             invoicedate TYPE fkdat,
             status      TYPE bapi_mtype,
             message     TYPE string,
           END OF ty_msg.

    DATA: gt_input  TYPE TABLE OF ty_input.       "structure for json input
    DATA: gs_final TYPE zsd_inv_customer.         "modify table structure
    DATA : gt_response TYPE TABLE OF ty_msg.      "return response through api
    DATA: lv_data TYPE string.          "for input json string
    DATA: lv_body    TYPE string,       "for output json string
          v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.     "data dec for store api log table
    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
*input conversion for vbeln and inv date
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_inp>).
        <fs_inp>-invoiceno   = |{ <fs_inp>-invoiceno ALPHA = IN }|.
        <fs_inp>-invoicedate = |{ <fs_inp>-invoicedate+4(4) }| && |{ <fs_inp>-invoicedate+2(2) }| && |{ <fs_inp>-invoicedate(2) }|.
      ENDLOOP.
      SELECT vbeln,bukrs,fkdat,kunag,gjahr FROM vbrk INTO TABLE @DATA(lt_vbrk) FOR ALL ENTRIES IN @gt_input
                                                                   WHERE vbeln = @gt_input-invoiceno.
*                                                                   AND   bukrs = @gt_input-comcode
*                                                                   AND   fkdat = @gt_input-invoicedate.
      SORT lt_vbrk BY vbeln bukrs fkdat.
*check the vbeln num from vbrk and matched records to be updated on zsd_inv_customer
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        READ TABLE lt_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) WITH KEY vbeln = <fs_input>-invoiceno
                                                                      bukrs = <fs_input>-comcode
                                                                      fkdat = <fs_input>-invoicedate BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR gs_final.
          gs_final-vbeln        = <fs_vbrk>-vbeln.
          gs_final-bukrs        = <fs_vbrk>-bukrs.
          gs_final-gjahr        = <fs_vbrk>-gjahr.
          gs_final-kunnr        = <fs_vbrk>-kunag.
          gs_final-status       = 10.
          gs_final-ctreated     = sy-datum.
          gs_final-ctreated_by  = sy-uname.
          gs_final-remarks      = <fs_input>-remarks.
          gs_final-emp_req      = <fs_input>-emp_req.
          gs_final-emp_appr     = <fs_input>-emp_appr.
          gs_final-invoice_date = <fs_vbrk>-fkdat.
          MODIFY zsd_inv_customer FROM gs_final.
          IF sy-subrc = 0.
            COMMIT WORK.
            APPEND VALUE #( invoiceno   = <fs_input>-invoiceno
                            comcode     = <fs_input>-comcode
                            invoicedate = <fs_input>-invoicedate
                            status      = 'S'
                            message     = |Customer Invoice is updated for { <fs_input>-invoiceno } | ) TO gt_response.
          ELSE.
            ROLLBACK WORK.
            APPEND VALUE #( invoiceno   = <fs_input>-invoiceno
                            comcode     = <fs_input>-comcode
                            invoicedate = <fs_input>-invoicedate
                            status      = 'E'
                            message     = |invoice not updated for bypass - { <fs_input>-invoiceno } | ) TO gt_response.
          ENDIF.
        ELSE.
          APPEND VALUE #( invoiceno   = <fs_input>-invoiceno
                          comcode     = <fs_input>-comcode
                          invoicedate = <fs_input>-invoicedate
                          status      = 'E'
                          message     = |incorrect Invoice - { <fs_input>-invoiceno } | ) TO gt_response.
        ENDIF.
      ENDLOOP.
    ELSE.
      APPEND VALUE #( invoiceno   = abap_false
                      comcode     = abap_false
                      invoicedate = abap_false
                      status      = 'E'
                      message     = |Please fill the data| ) TO gt_response.
    ENDIF.

    IF gt_response IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'INV_BYPASS'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      v_jsonload = |{ lv_body }|.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
