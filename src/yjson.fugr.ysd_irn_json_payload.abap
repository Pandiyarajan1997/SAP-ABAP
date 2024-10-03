FUNCTION YSD_IRN_JSON_PAYLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(EI_IN) TYPE  ZST_IRN_HDR
*"  EXPORTING
*"     REFERENCE(IV_PAYLOAD) TYPE  STRING
*"     REFERENCE(RESPONSE) TYPE  STRING
*"     REFERENCE(EINV_OUT) TYPE  ZST_IRN_OUT
*"     REFERENCE(EWAY_OUT) TYPE  J_1IG_EWAYBILL
*"     REFERENCE(GV_MSG) TYPE  C
*"  TABLES
*"      TT_ITEMDTLS TYPE  ZST_LT_IRN_ITEM_JSON
*"  EXCEPTIONS
*"      NO_ITEM_DETAILS
*"----------------------------------------------------------------------
  CLEAR: LV_TRANS , LV_DOCDTLS,LV_SELLDTLS,LV_BUYDTLS,LV_DISPDTLS,
          LV_SHIPDTLS,LV_REFDTLS,LV_ITMDTLS,LV_VALDTLS ,LV_ADDLDOCDTLS,GV_VBELN,GV_REF,
          LV_EXPDTLS,LV_PAYDTLS,LV_EWBDTLS,V_PAYLOAD,GW_ITEM,GW_ITEM1,LV_HTTP_ERROR_DESCR,LV_HTTP_RETURN_CODE.

  REFRESH: IT_BODY_ITEM[].

  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) INTO LV_DATE SEPARATED BY '/'.

  IF SYST-SYSID = 'DEV' OR SYST-SYSID = 'QAS'.
*****Transaction*****
    CONCATENATE
*    '{ '
*'"CDKey": "1000687",                  '
*'"EInvUserName": "29AAACW3775F000",               '
*'"EInvPassword": "Admin!23..",                '
*'"EFUserName": "29AAACW3775F000",             '
*'"EFPassword": "Admin!23..",                '
*'"GSTIN": "29AAACW3775F000",                '
*'"GetQRImg": "1",                 '
*'"GetSignedInvoice": "1",               '
'"TranDtls": { "SupTyp": "B2B", "RegRev": "Y", "EcmGstin": null, "IgstOnIntra": "N"	'
'},' INTO LV_TRANS.


*    CONCATENATE '"transaction": {' '"Version"' C_COLON C_QUATATION_MARK C_VERSION C_QUATATION_MARK C_COMMA
*                '"TranDtls": { "TaxSch": "GST", "SupTyp": "' EI_IN-SUPTYP '", "RegRev": "N", "EcmGstin": null, "IgstOnIntra": "N"'
*                C_CURLY_BRACKETS_CLOSE C_COMMA INTO LV_TRANS.
  ELSE.
      CONCATENATE
*      '{ '
*'"CDKey": "1000687",                  '
*'"EInvUserName": "29AAACW3775F000",               '
*'"EInvPassword": "Admin!23..",                '
*'"EFUserName": "29AAACW3775F000",             '
*'"EFPassword": "Admin!23..",                '
*'"GSTIN": "29AAACW3775F000",                '
*'"GetQRImg": "1",                 '
*'"GetSignedInvoice": "1",               '
'"TranDtls": { "SupTyp": "B2B", "RegRev": "Y", "EcmGstin": null, "IgstOnIntra": "N"	'
'},' INTO LV_TRANS.
*    CONCATENATE '"Version"' C_COLON C_QUATATION_MARK C_VERSION C_QUATATION_MARK C_COMMA
*                '"TranDtls": { "TaxSch": "GST", "SupTyp": "' EI_IN-SUPTYP '", "RegRev": "N", "EcmGstin": null, "IgstOnIntra": "N"'
*                C_CURLY_BRACKETS_CLOSE C_COMMA INTO LV_TRANS.

  ENDIF.
  MOVE EI_IN-VBELN  TO GV_VBELN.          "Billing doc no.
  MOVE EI_IN-REF_NO TO GV_REF.

  SHIFT GV_VBELN LEFT DELETING LEADING '0'.
  SHIFT GV_REF   LEFT DELETING LEADING '0'.


******Document details******
  CONCATENATE '"DocDtls": {' C_TYP C_QUATATION_MARK EI_IN-DOCTYP C_QUATATION_MARK C_COMMA
                          C_NO C_QUATATION_MARK GV_VBELN C_QUATATION_MARK C_COMMA
                          C_DT C_QUATATION_MARK EI_IN-DDAT C_QUATATION_MARK
                          C_CURLY_BRACKETS_CLOSE C_COMMA INTO LV_DOCDTLS.
*
*  CONCATENATE '"DocDtls": {' C_TYP C_QUATATION_MARK EI_IN-DOCTYP C_QUATATION_MARK C_COMMA
*                          C_NO C_QUATATION_MARK GV_VBELN C_QUATATION_MARK C_COMMA
*                          C_DT C_QUATATION_MARK EI_IN-DDAT C_QUATATION_MARK
*                          C_CURLY_BRACKETS_CLOSE C_COMMA INTO LV_DOCDTLS.

*****Seller Details ******
  PERFORM SELLER_DTLS USING EI_IN.

*****Buyer Details *********
  PERFORM BUYER_DTLS USING EI_IN.

*****Dispatch Details *********
  PERFORM DISPATCH_DTLS USING EI_IN.

*******Shipping Details ******
  PERFORM SHIPDTLS  USING EI_IN.

*******Item Details******
  IF TT_ITEMDTLS[] IS NOT INITIAL.
    CLEAR: V_ASSVAL, V_AMTVAL,V_IGST,V_CGST,V_SGST,V_TCS.

    LOOP AT TT_ITEMDTLS INTO GW_ITEM.
      MOVE-CORRESPONDING GW_ITEM TO GW_ITEM1.

****Assessable Value, Total Value & IGST Value
      V_ASSVAL = V_ASSVAL + GW_ITEM1-ASSVAL.
      V_AMTVAL = V_AMTVAL + GW_ITEM1-TOTAMTVAL.
      V_TCS    = V_TCS    + GW_ITEM1-OTHCHRG.
      V_IGST   = V_IGST   + GW_ITEM1-IGSTAMT.
      V_CGST   = V_CGST   + GW_ITEM1-CGSTAMT.
      V_SGST   = V_SGST   + GW_ITEM1-SGSTAMT.

*******Begin of Single Line items ******
      AT LAST.
        "Paasing Null in inital fields
        IF GW_ITEM1-GST_RATE IS INITIAL .
          GW_ITEM1-GST_RATE = 0.
        ENDIF.
        IF GW_ITEM1-IGSTAMT IS INITIAL .
          GW_ITEM1-IGSTAMT = 0.
        ENDIF.
        IF GW_ITEM1-CGSTAMT IS INITIAL.
          GW_ITEM1-CGSTAMT   = 0..
        ENDIF.
        IF GW_ITEM1-SGSTAMT IS INITIAL.
          GW_ITEM1-SGSTAMT = 0.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '"' IN GW_ITEM1-MAKT WITH SPACE.
        PERFORM ITEM_DTLS.

        APPEND WA_BODY_ITEM TO IT_BODY_ITEM.
        CLEAR: WA_BODY_ITEM.
        EXIT.
      ENDAT.
*******End of Single Line items ******

      "Paasing Null in inital fields
      IF GW_ITEM1-GST_RATE IS INITIAL .
        GW_ITEM1-GST_RATE = |null|.
      ENDIF.
      IF GW_ITEM1-IGSTAMT IS INITIAL .
        GW_ITEM1-IGSTAMT = |null|.
      ENDIF.
      IF GW_ITEM1-CGSTAMT IS INITIAL.
        GW_ITEM1-CGSTAMT   = |null|.
      ENDIF.
      IF GW_ITEM1-SGSTAMT IS INITIAL.
        GW_ITEM1-SGSTAMT = |null|.
      ENDIF.

*******Begin of Multiple Line items ******
      PERFORM ITEM_DTLS.
      CONCATENATE WA_BODY_ITEM-ITEM ',' INTO WA_BODY_ITEM-ITEM.

      APPEND WA_BODY_ITEM TO IT_BODY_ITEM.
      CLEAR: WA_BODY_ITEM.
*******End of Multiple Line items ******
      CLEAR:GW_ITEM,GW_ITEM1.
    ENDLOOP.

********Concenating the Line Item Details *************
    LOOP AT IT_BODY_ITEM INTO WA_BODY_ITEM.
      CONCATENATE LV_ITMDTLS WA_BODY_ITEM-ITEM INTO LV_ITMDTLS.
    ENDLOOP.
***********End of Line item Details****************

*******Val Details ******
    PERFORM VALDTLS  USING EI_IN.


****Payment Details****
    PERFORM PAYDTLS  USING EI_IN.

******Reference Details ***********
    PERFORM REFDTLS  USING EI_IN.


*******Additional Doc Details*****
*    CONCATENATE      '"AddlDocDtls": [{'
*              '"Url": "https://einv-apisandbox.nic.in",'
*              '"Docs": "Test Doc",'
*              '"Info": "Document Test"'
*            '}],'  INTO lv_addldocdtls.

*********EXP Details*********
*    CONCATENATE      '"ExpDtls": {'
*            '"ShipBNo":' '"a-129"' c_comma
*            '"ShipBDt":' '"09/09/2020"' c_comma
*            '"Port":'   '"INABG1"' c_comma
*            '"RefClm":' '"N"' c_comma
*            '"ForCur":' '"AED"' c_comma
*            '"CntCode":' '"AE"'    "c_null
*            '},'   INTO lv_expdtls.

*    CONCATENATE      '"ExpDtls": {'
*            '"ShipBNo":' c_null c_comma
*            '"ShipBDt":' c_null c_comma
*            '"Port":'   c_null c_comma
*            '"RefClm":' c_null c_comma
*            '"ForCur":' c_null c_comma
*            '"CntCode":' c_null    "c_null
*            '},'   INTO lv_expdtls.



******** EWB Details ********
    PERFORM EWBDTLS USING EI_IN.

*******Final Payload Request Body for IRN Generation ************
    PERFORM PAYLOAD CHANGING V_PAYLOAD.

    IF V_PAYLOAD IS NOT INITIAL.
      MOVE V_PAYLOAD TO IV_PAYLOAD.
    ENDIF.

  ELSE.
    RAISE NO_ITEM_DETAILS.
  ENDIF.
  "CLEAR: V_PAYLOAD.
  " V1_PAYLOAD = ;.
*  CONCATENATE
*   INTO V1_PAYLOAD.


  IF v_payload IS NOT INITIAL.
    PERFORM API_CALL USING V_PAYLOAD EI_IN CHANGING EINV_OUT EWAY_OUT RESPONSE GV_MSG.
    " PERFORM api_call USING v_payload ei_in CHANGING einv_out eway_out response gv_msg.
    " PERFORM API_EXAMPLE.

  ENDIF.


ENDFUNCTION.
