class ZCL_API_CF_UPD_FROM_CF_SERVER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_CF_UPD_FROM_CF_SERVER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             borrowername        TYPE string,
             buyer               TYPE string,
             buyerpartyid        TYPE string,
             borrowerid          TYPE string,
             anchorname          TYPE string,
             seller              TYPE string,
             invoiceid           TYPE string,
             invoiceno           TYPE string,
             invoicetype         TYPE string,
             invoicedate         TYPE string,
             duedate             TYPE string,
             invoiceamount       TYPE string,
             invoicediscountedon TYPE string,
             dueamount           TYPE string,
             invoiceapprovedon   TYPE string,
             invoiceapproveby    TYPE string,
             status              TYPE string,
           END OF ty_input.
*{
    "borrowerName":"Rainbow Enterprises",
    "buyer":"Rainbow Enterprises",
    "buyerPartyId":3264,                        Insert to zsd_sf_cust_inv
    "borrowerId":14872,                         Insert to zsd_sf_cust_inv
    "anchorName":"SHEENLAC PAINTS LIMITED",
    "seller":"SHEENLAC PAINTS LIMITED",
    "sellerPartyId":3263,
    "invoiceId":39885,                          FIN_INV_ID to zsd_sf_cust_inv
    "invoiceNo":"1104891310002023",     invoiceno
    "invoiceType":"PBF",                        fin_inv_type
    "invoiceDate":"27/09/23",
    "dueDate":"27/10/23",
    "invoiceAmount":25766.1,
    "dueAmount":25766.1,
    "invoiceApprovedOn":"27/09/2023 12:00:00", INV_APPROVEDON INV_APPROVEDAT Insert zsd_sf_cust_inv
    "invoiceApproveBy":"spl@bl.com",
    "status":"DISCOUNTED"
*}

    DATA: gs_input TYPE ty_input.

    TYPES: BEGIN OF ty_msg,
             invoiceno TYPE string,
             status    TYPE string,
             message   TYPE string,
           END OF ty_msg.


    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA: lv_data TYPE string.


    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).
    TRANSLATE gs_input-status TO UPPER CASE.
    IF gs_input-status = 'DISCOUNTED'.
      DATA l_invoicekey TYPE zinvkey.
      l_invoicekey = gs_input-invoiceno.
      SELECT SINGLE * FROM zsd_sf_cust_inv
        INTO @DATA(lw_out)
        WHERE invoicekey = @l_invoicekey.
      IF sy-subrc = 0.
        lw_out-buyerpartyid  = gs_input-buyerpartyid.
        lw_out-borrowerid  = gs_input-borrowerid.
        lw_out-fin_inv_id  = gs_input-invoiceid.
        lw_out-fin_inv_type  = gs_input-invoicetype.
        lw_out-inv_approvedby  = gs_input-invoiceapproveby.
        IF gs_input-invoicediscountedon IS INITIAL.
          gs_input-invoicediscountedon = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum(4) } { sy-uzeit(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }|.
          lw_out-inv_approvedon = sy-datum.
          lw_out-inv_approvedat = sy-uzeit.
        ELSE.
          lw_out-inv_approvedon  = |{ gs_input-invoicediscountedon+6(4) }{ gs_input-invoicediscountedon+3(2) }{ gs_input-invoicediscountedon(2) }|.
          lw_out-inv_approvedat  = |{ gs_input-invoicediscountedon+11(2) }{ gs_input-invoicediscountedon+14(2) }{ gs_input-invoicediscountedon+17(2) }|.
        ENDIF.
        "invoiceDiscountedOn":"27/09/2023 12:00:00"
        lw_out-status = '16'.
        MODIFY zsd_sf_cust_inv FROM lw_out.
*** Response Part ***
        gw_response-invoiceno = gs_input-invoiceno.
        gw_response-status = 'Success'.
        gw_response-message = 'Updated successfully'.
      ELSE.
*** Response Part ***
        gw_response-invoiceno = gs_input-invoiceno.
        gw_response-status = 'Error'.
        gw_response-message = 'Invoice No not found'.
      ENDIF.
    ELSE.
      IF gs_input IS INITIAL.
*** Response Part ***
        gw_response-invoiceno = gs_input-invoiceno.
        gw_response-status = 'Error'.
        gw_response-message = 'No Input fields is found'.
      ELSE.
*** Response Part ***
        gw_response-invoiceno = gs_input-invoiceno.
        gw_response-status = 'Error'.
        gw_response-message = 'Incorrect Status'.
      ENDIF.
    ENDIF.

    APPEND gw_response TO gt_response.
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


    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'CUST_FIN_BILLION_API'
        ijson           = lv_data
        ojson           = v_jsonload
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

*    TYPES: BEGIN OF ty_input,
*             invoiceid         TYPE string,
*             invoiceno         TYPE string,
*             invoicedate       TYPE string,
*             buyerpartyid      TYPE string,
*             buyer             TYPE string,
*             sellerpartyid     TYPE string,
*             seller            TYPE string,
*             invoiceamount     TYPE string,
*             duedate           TYPE string,
*             dueamount         TYPE string,
*             status            TYPE string,
*             invoicetype       TYPE string,
*             invoiceapprovedby TYPE string,
*             invoiceapprovedon TYPE string,
*           END OF ty_input.
*invoiceId
*invoiceNo
*invoiceDate
*buyerPartyId
*buyer
*sellerPartyId
*seller
*invoiceAmount
*dueDate
*dueAmount
*status
*invoiceType
*invoiceApprovedBy
*invoiceApprovedOn
  ENDMETHOD.
ENDCLASS.
