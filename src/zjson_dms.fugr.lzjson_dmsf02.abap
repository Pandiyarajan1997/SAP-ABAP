*----------------------------------------------------------------------*
***INCLUDE LZJSONF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  API_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_PAYLOAD  text
*      -->P_EI_IN  text
*      <--P_EINV_OUT  text
*      <--P_EWAY_OUT  text
*      <--P_RESPONSE  text
*      <--P_GV_MSG  text
*----------------------------------------------------------------------*
FORM api_call  USING    p_v_payload         TYPE string
                        p_ei_in             TYPE zst_irn_hdr
                       CHANGING p_e_out     TYPE zst_irn_out
                                p_eway_out  TYPE j_1ig_ewaybill
                                p_response  TYPE string
                                p_msg       TYPE c.
  DATA : v_duplicate(6) TYPE c,
          gv_irn(64)     TYPE c.

  DATA: lc_msg TYPE REF TO cx_salv_msg.

  CLEAR: v_duplicate, gv_irn,lv_http_return_code,lv_http_error_descr,gv_msg,gv_gstin,gv_auth,gv_geturl,gv_url,gv_ownerid.

******IRN URL AUTH TOKEN GSTIN AND OWNER ID FROM ZTABLE***************
  SELECT * UP TO 1 ROWS FROM zeinv_url INTO wa_url WHERE gstin = p_ei_in-gstin ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  IF wa_url IS NOT INITIAL.
    gv_gstin    = p_ei_in-gstin.
    gv_ownerid  = wa_url-ownerid.
    gv_auth     = wa_url-authtoken.
    gv_url      = wa_url-zirn_cr_url.
    gv_geturl   = wa_url-zirn_geturl.
  ENDIF.

  CLEAR:wa_url.

  IF gv_url IS NOT INITIAL.
************URL - Creation OF IRN Generation ******************************************
*PASS the Headers with POST the API From SAP to ThirdParty API
    TRY .
        CONDENSE v_payload NO-GAPS.
        cl_http_client=>create_by_url( EXPORTING url = gv_url IMPORTING client = http_client ).

        IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.          "For Development & Quality
*****PUT Sandbox Method********
          http_client->request->set_method( if_http_request=>co_request_method_post ).
        ELSE.
*****POST LIVE API Method********
          http_client->request->set_method( if_http_request=>co_request_method_post ).
        ENDIF.

        http_client->request->set_content_type( 'application/json; charset=utf-8' ).
******Header Details JSON **********
        http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-auth-token' value = gv_auth ).
        http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-product​' value = | EInvoice | ).
        http_client->request->set_header_field( EXPORTING  name  = 'Content-Type'   value = 'application/json' ).
        http_client->request->set_header_field( EXPORTING  name  = 'owner_id'       value = gv_ownerid ).
        http_client->request->set_header_field( EXPORTING  name  = 'gstin'          value = gv_gstin ).
******JSON Payload in PUT Method **********
        http_client->request->set_cdata( p_v_payload ).
        http_client->send( ).
      CATCH cx_salv_msg INTO lc_msg .                   "#EC NO_HANDLER
*      DATA(lv_string) = lc_msg->get_text( ).   " <
        lv_string = lc_msg->get_text( ).   " <
        MESSAGE lv_string TYPE 'I'.  "<
    ENDTRY.
* Disable SAP's pop-up for user id and password:
    http_client->propertytype_logon_popup = http_client->co_disabled.    "  POPUP for user id and pwd

*  POST API Receive from Third Party System *****
    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    http_client->response->get_status( IMPORTING code   = lv_http_return_code ).
    http_client->response->get_status( IMPORTING reason = lv_http_error_descr ).

*POST API JSON RESPONSE
    p_response = http_client->response->get_cdata( ).
  ELSE.
    p_msg = abap_true.
    p_response = 'GSTIN Invalid/No URL Found'.
  ENDIF.

  IF lv_http_return_code = '200'.
    SPLIT p_response AT '"InfCd":"' INTO v_str v_duplicate.

    IF v_duplicate <> 'DUPIRN'.
      SPLIT p_response AT '"Desc":"'  INTO v_str v_duplicate.
    ENDIF.

*******Duplicate IRN or Response Not getting at Initial Time*******
*******Getting E-Invoice BY IRN API in GET Method ****************
    IF v_duplicate = 'DUPIRN' OR v_duplicate = 'Duplic'.      "Duplicate IRN already Generated.
      CLEAR: v_str.
      SPLIT p_response AT '"Irn":"' INTO v_str  gv_irn.
      PERFORM get_einvoice_by_irn_api USING p_ei_in gv_irn CHANGING p_response.
    ENDIF.

*************Converting JSON Format to ABAP Structure ********
    PERFORM json_abap_structure  USING p_ei_in CHANGING p_e_out p_eway_out p_response.

  ELSE.
    IF p_msg IS INITIAL.
      p_msg = abap_true.
      p_response = lv_http_error_descr.
    ENDIF.
  ENDIF.


ENDFORM.                    " API_CALL
*&---------------------------------------------------------------------*
*&      Form  JSON_ABAP_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EI_IN  text
*      <--P_P_E_OUT  text
*      <--P_P_EWAY_OUT  text
*      <--P_P_RESPONSE  text
*----------------------------------------------------------------------*
FORM json_abap_structure USING     p_hdr        TYPE zst_irn_hdr
                          CHANGING p_e_out1     TYPE zst_irn_out
                                   p_eway_out1  TYPE j_1ig_ewaybill
                                   p_response1  TYPE string .

  DATA : v_type    TYPE string,
          v_type1   TYPE string,
          msg       TYPE char03,
          v_ackno   TYPE char16,
          v_ackdt   TYPE char19,
          v_ackdt1  TYPE char10,
          v_acktim  TYPE char8,
          v_tempinv TYPE char10,
          v_einv    TYPE string,
          v_qrcode  TYPE string,
          v_irn     TYPE char64,
          v_ewb     TYPE char12,
          v_status  TYPE char03.

  CLEAR: v_type,v_type1,msg,v_ackdt,v_ackno,v_ackdt1,v_acktim,
         v_tempinv, v_einv,v_qrcode,v_irn,v_status,v_ewb.

  SPLIT p_response1 AT '{"Success":' INTO v_type msg .

  IF msg IS INITIAL.
  SPLIT p_response1 AT '"Success":' INTO v_type msg .
  ENDIF.

******JSON Format to ABAP Structure **************
  IF lv_http_return_code = '200' AND lv_http_error_descr = 'OK' AND msg = '"Y"'.

    CLEAR: v_type,v_type1,v_ackdt,v_ackdt1,v_acktim,v_ackno,v_einv,v_qrcode,v_status.

****Acknowledgment date with space****
    SPLIT p_response1 AT '"AckDt":"' INTO v_type v_ackdt.

*****Acknowledgment date and Time*****
    SPLIT v_ackdt AT ' ' INTO v_ackdt1 v_acktim.

    REPLACE ALL OCCURRENCES OF ':' IN v_acktim WITH space.
    CONDENSE v_acktim NO-GAPS.

****Acknowledgment Number****
    IF v_type IS NOT INITIAL.
      SPLIT v_type AT '"AckNo":' INTO v_type1 v_ackno.
      REPLACE ALL OCCURRENCES OF ',' IN v_ackno WITH space.
    ENDIF.

*******IRN No**********************
    CLEAR: v_type.
    SPLIT p_response1 AT '"Irn":"' INTO v_type v_irn.


******Signed Invoice****************
    CLEAR: v_type.
    SPLIT p_response1 AT '","SignedQRCode":"' INTO v_type v_tempinv.
    SPLIT v_type AT '"SignedInvoice":"' INTO v_type1 v_einv.


******Signed QR Code****************
********Status**********************
    CLEAR: v_type,v_type1,v_tempinv.
    SPLIT p_response1 AT '","Status":"' INTO v_type v_status.
    SPLIT v_type AT '"SignedQRCode":"' INTO v_type1 v_qrcode.


    MOVE p_hdr-bukrs  TO p_e_out1-bukrs.
    MOVE p_hdr-vbeln  TO p_e_out1-vbeln.

******Output Structure*********
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_e_out1-vbeln
      IMPORTING
        output = p_e_out1-vbeln.

    MOVE p_hdr-gjahr  TO p_e_out1-gjahr.
    MOVE p_hdr-doctyp TO p_e_out1-doctyp.
    MOVE c_version    TO p_e_out1-version.
    MOVE sy-uname     TO p_e_out1-ernam.
    MOVE sy-datum     TO p_e_out1-erdat.

**********IRN****************
    IF v_irn IS NOT INITIAL.
      MOVE v_irn     TO p_e_out1-irn.
    ENDIF.

***********Acknowledgment Number****
    IF v_ackno IS NOT INITIAL.
      MOVE v_ackno     TO p_e_out1-ack_no.
    ENDIF.

***********Acknowledgment DATE & Time****
    IF v_ackdt IS NOT INITIAL.
      MOVE v_ackdt     TO p_e_out1-ack_date.
    ENDIF.

***********SAP  Entry Time****
    IF v_acktim IS NOT INITIAL.
      MOVE sy-uzeit     TO p_e_out1-erzet.
    ENDIF.

*******Signed QRCODE ********
    IF v_qrcode IS NOT INITIAL.
      MOVE v_qrcode     TO p_e_out1-signed_qrcode.
    ENDIF.

******Signed Einvoice **********
    IF v_einv IS NOT INITIAL.
      MOVE v_einv     TO p_e_out1-signed_inv.
    ENDIF.

*****IRN STATUS ***********
    IF v_status IS NOT INITIAL.
      MOVE v_status TO p_e_out1-irn_status.
    ENDIF.

******End of Output Structure*********

**********************Eway Bill Details****************
******Output Structure*********

    CLEAR:v_type.
    SPLIT p_response1 AT '"EwbNo":' INTO v_type p_eway_out1-ebillno.
    MOVE p_eway_out1-ebillno TO v_ewb.

    IF v_ewb IS NOT INITIAL.

      MOVE sy-mandt     TO p_eway_out1-mandt.
      MOVE p_hdr-bukrs  TO p_eway_out1-bukrs.
      MOVE p_hdr-vbeln  TO p_eway_out1-docno.
      MOVE p_hdr-gjahr  TO p_eway_out1-gjahr.
      MOVE p_hdr-doctyp TO p_eway_out1-doctyp.
      MOVE sy-uname     TO p_eway_out1-ernam.
      MOVE sy-datum     TO p_eway_out1-erdat.
      MOVE sy-datum     TO p_eway_out1-egen_dat.
      MOVE sy-uzeit     TO p_eway_out1-egen_time.

      DATA: v_ewbdt  TYPE char10,
            v_ewbdt1 TYPE char21.


******Eway Bill Status**********
*    IF p_eway_out1-ebillno IS NOT INITIAL.
*      MOVE v_status to p_eway_out1-STATUS    "Char1
*    ENDIF.

****EWB From Date & Time*********
      CLEAR: v_type.
      SPLIT p_response1 AT '"EwbDt":"' INTO v_type v_ewbdt1.

****Valid From date
      REPLACE ALL OCCURRENCES OF '-' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1(8) TO p_eway_out1-vdfmdate.

****Valid From Time
      REPLACE ALL OCCURRENCES OF ':' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1+8(6) TO p_eway_out1-vdfmtime.

**********Valid TO Date**********
*****EWB To date****************
      CLEAR: v_type,v_ewbdt1.
      SPLIT p_response1 AT '"EwbValidTill":"' INTO v_type v_ewbdt1.

****Valid TO date
      REPLACE ALL OCCURRENCES OF '-' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1(8) TO p_eway_out1-vdtodate.

****Valid From Time
      REPLACE ALL OCCURRENCES OF ':' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1+8(6) TO p_eway_out1-vdtotime.

    ENDIF.

  ENDIF.

ENDFORM.                    " JSON_ABAP_STRUCTURE
*&---------------------------------------------------------------------*
*&      Form  GET_EINVOICE_BY_IRN_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EI_IN  text
*      -->P_GV_IRN  text
*      <--P_P_RESPONSE  text
*----------------------------------------------------------------------*
FORM get_einvoice_by_irn_api  USING    p_p_ei_in    TYPE zst_irn_hdr
                                       p_irn        TYPE char64
                              CHANGING p_p_response TYPE string.

************Sandbox URL - Getting Einvoice BY IRN ***************************************************
*  DATA(create_url)   = | https://einvoicing.internal.cleartax.co/v2/eInvoice/get?irn={ p_irn } |.

******PRD API - Getting Einvoice BY IRN Credentials******************
*  DATA(create_url)   = |https://api-einv.cleartax.in/v1/govt/api/Invoice/irn/{ p_irn } |.

  create_url   = |{ gv_geturl }{ p_irn }|.
  CONDENSE create_url NO-GAPS.

  DATA: lc_msg TYPE REF TO cx_salv_msg.
*PASS the Headers with GET the API From ThirdParty API
  TRY .
      cl_http_client=>create_by_url( EXPORTING url = create_url IMPORTING client = http_client ).
*****GET Method********
      http_client->request->set_method( 'GET' ).
      http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-auth-token' value = gv_auth ).
      http_client->request->set_content_type( 'application/json; charset=utf-8' ).
      http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-product​' value = | EInvoice | ).
      http_client->request->set_header_field( EXPORTING  name  = 'gstin' value = gv_gstin ).
      http_client->request->set_header_field( EXPORTING  name  = 'owner_id' value = gv_ownerid ).
      http_client->send( ).
    CATCH cx_salv_msg INTO lc_msg .                     "#EC NO_HANDLER
*      DATA(lv_string) = lc_msg->get_text( ).   " <
      lv_string = lc_msg->get_text( ).   " <
      MESSAGE lv_string TYPE 'I'.  "<
  ENDTRY.

******* GET API Receive from Third Party System *****
  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  http_client->response->get_status( IMPORTING code   = lv_http_return_code ).
  http_client->response->get_status( IMPORTING reason = lv_http_error_descr ).

******Response JSON*******
  p_p_response = http_client->response->get_cdata( ).

ENDFORM.                    " GET_EINVOICE_BY_IRN_API
*&---------------------------------------------------------------------*
*&      Form  ITEM_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM item_dtls .
  CONCATENATE ' {'
                      '"SlNo":'          c_quatation_mark gw_item1-posnr        c_quatation_mark c_comma
                      '"PrdDesc":'       c_quatation_mark gw_item1-makt         c_quatation_mark c_comma
                      '"IsServc":'       c_quatation_mark gw_item1-itm_service  c_quatation_mark c_comma
                      '"HsnCd":'         c_quatation_mark gw_item1-steuc        c_quatation_mark c_comma
                      '"Barcde":'        c_null  c_comma
                      '"Qty":'           gw_item1-fkimg         c_comma
                      '"FreeQty":'       c_zero  c_comma
                      '"Unit":'          c_quatation_mark gw_item1-vrkme        c_quatation_mark c_comma
                      '"UnitPrice":'     gw_item1-uprice        c_comma
                      '"TotAmt":'        gw_item1-amount        c_comma
                      '"Discount":'      c_zero c_comma              """"Need to Change
                      '"PreTaxVal":'     c_zero c_comma
                      '"AssAmt":'        gw_item1-assval        c_comma
                      '"GstRt":'         gw_item1-gst_rate      c_comma
                      '"IgstAmt":'       gw_item1-igstamt       c_comma
                      '"CgstAmt":'       gw_item1-cgstamt       c_comma
                      '"SgstAmt":'       gw_item1-sgstamt       c_comma
                      '"CesRt":'         c_zero c_comma
                      '"CesAmt":'           c_zero c_comma
                      '"CesNonAdvlAmt":'     c_zero c_comma
                      '"StateCesRt":'        c_zero c_comma
                      '"StateCesAmt":'       c_zero c_comma
                      '"StateCesNonAdvlAmt":' c_zero  c_comma
                      '"OthChrg":'           gw_item1-othchrg   c_comma
                      '"TotItemVal":'    gw_item1-totamtval   c_comma
                      '"OrdLineRef":'    c_null   c_comma
                      '"OrgCntry":'      '"IN"'   c_comma       "AG"
                      '"PrdSlNo":'       c_null   c_comma

                      '"BchDtls":{'
                      '"Nm":'            '"Test Batch"' c_comma "c_null
                      '"ExpDt":'        '"' lv_date '"'  c_comma "c_null c_comma
                      '"WrDt":'         '"' lv_date '"'  "c_comma "c_null
                      c_curly_brackets_close c_comma

                      '"AttribDtls": [ {'
                        '"Nm":'         c_null c_comma
                        '"Val":'        c_zero
                        c_curly_brackets_close
                        ']}'              INTO  wa_body_item-item.

ENDFORM.                    " ITEM_DTLS
*&---------------------------------------------------------------------*
*&      Form  PAYLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_PAYLOAD  text
*----------------------------------------------------------------------*
FORM payload  CHANGING p_payload TYPE string .

 IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.          "For Development & Quality Snadbox JSON
  CONCATENATE
  '['             "Comment for Moving PRD
  '{' lv_trans lv_docdtls lv_selldtls lv_buydtls lv_dispdtls  lv_shipdtls
                   '"ItemList":[' lv_itmdtls  '],' lv_valdtls
                        lv_paydtls
                         lv_refdtls
*                          lv_addldocdtls
*                          lv_expdtls
                        lv_ewbdtls  '}}'
                        ']'       "Comment for Moving PRD
                        INTO p_payload.
 ELSE.                                                "Production JSON

CONCATENATE
*  '['             "Commented for Moving PRD
  '{' lv_trans lv_docdtls lv_selldtls lv_buydtls lv_dispdtls  lv_shipdtls
                   '"ItemList":[' lv_itmdtls  '],' lv_valdtls
                        lv_paydtls
                         lv_refdtls
                        lv_ewbdtls  '}'
*                        ']'       "Commented for Moving PRD
                        INTO p_payload.

ENDIF.

ENDFORM.                    " PAYLOAD
