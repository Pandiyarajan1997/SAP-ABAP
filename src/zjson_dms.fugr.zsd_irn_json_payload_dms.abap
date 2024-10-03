FUNCTION ZSD_IRN_JSON_PAYLOAD_DMS.
*"--------------------------------------------------------------------
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
*"--------------------------------------------------------------------
 CLEAR: lv_trans , lv_docdtls,lv_selldtls,lv_buydtls,lv_dispdtls,
         lv_shipdtls,lv_refdtls,lv_itmdtls,lv_valdtls ,lv_addldocdtls,gv_vbeln,gv_ref,
         lv_expdtls,lv_paydtls,lv_ewbdtls,v_payload,gw_item,gw_item1,lv_http_error_descr,lv_http_return_code.

  REFRESH: it_body_item[].

  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO lv_date SEPARATED BY '/'.

if syst-sysid = 'DEV' OR syst-sysid = 'QAS'.
*****Transaction*****
  CONCATENATE '"transaction": {' '"Version"' c_colon c_quatation_mark c_version c_quatation_mark c_comma
              '"TranDtls": { "TaxSch": "GST", "SupTyp": "' EI_IN-SUPTYP '", "RegRev": "N", "EcmGstin": null, "IgstOnIntra": "N"'
              c_curly_brackets_close c_comma INTO lv_trans.
ELSE.
  CONCATENATE '"Version"' c_colon c_quatation_mark c_version c_quatation_mark c_comma
              '"TranDtls": { "TaxSch": "GST", "SupTyp": "' EI_IN-SUPTYP '", "RegRev": "N", "EcmGstin": null, "IgstOnIntra": "N"'
              c_curly_brackets_close c_comma INTO lv_trans.

ENDIF.
  MOVE ei_in-vbeln  TO GV_VBELN.          "Billing doc no.
  MOVE ei_in-ref_no TO GV_ref.

  SHIFT GV_vbeln LEFT DELETING LEADING '0'.
  SHIFT gv_ref   LEFT DELETING LEADING '0'.


******Document details******
  CONCATENATE '"DocDtls": {' c_typ c_quatation_mark ei_in-doctyp c_quatation_mark c_comma
                          c_no c_quatation_mark GV_vbeln c_quatation_mark c_comma
                          c_dt c_quatation_mark ei_in-ddat c_quatation_mark
                          c_curly_brackets_close c_comma INTO lv_docdtls.

*****Seller Details ******
  PERFORM seller_dtls USING ei_in.

*****Buyer Details *********
  PERFORM buyer_dtls USING ei_in.

*****Dispatch Details *********
  PERFORM dispatch_dtls USING ei_in.

*******Shipping Details ******
  PERFORM shipdtls  USING ei_in.

*******Item Details******
  IF tt_itemdtls[] IS NOT INITIAL.
    CLEAR: v_assval, v_amtval,v_igst,v_cgst,v_sgst,v_tcs.

    LOOP AT tt_itemdtls INTO gw_item.
      MOVE-CORRESPONDING gw_item TO gw_item1.

****Assessable Value, Total Value & IGST Value
      v_assval = v_assval + gw_item1-assval.
      v_amtval = v_amtval + gw_item1-totamtval.
      v_tcs    = v_tcs    + gw_item1-othchrg.
      v_igst   = v_igst   + gw_item1-igstamt.
      v_cgst   = v_cgst   + gw_item1-cgstamt.
      v_sgst   = v_sgst   + gw_item1-sgstamt.

*******Begin of Single Line items ******
      AT LAST.
        "Paasing Null in inital fields
        IF gw_item1-gst_rate IS INITIAL .
          gw_item1-gst_rate = 0.
        ENDIF.
        IF gw_item1-igstamt IS INITIAL .
          gw_item1-igstamt = 0.
        ENDIF.
        IF gw_item1-cgstamt IS INITIAL.
          gw_item1-cgstamt   = 0..
        ENDIF.
        IF gw_item1-sgstamt IS INITIAL.
          gw_item1-sgstamt = 0.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '"' in gw_item1-makt WITH space.
        PERFORM item_dtls.

        APPEND wa_body_item TO it_body_item.
        CLEAR: wa_body_item.
        EXIT.
      ENDAT.
*******End of Single Line items ******

      "Paasing Null in inital fields
      IF gw_item1-gst_rate IS INITIAL .
        gw_item1-gst_rate = |null|.
      ENDIF.
      IF gw_item1-igstamt IS INITIAL .
        gw_item1-igstamt = |null|.
      ENDIF.
      IF gw_item1-cgstamt IS INITIAL.
        gw_item1-cgstamt   = |null|.
      ENDIF.
      IF gw_item1-sgstamt IS INITIAL.
        gw_item1-sgstamt = |null|.
      ENDIF.

*******Begin of Multiple Line items ******
      PERFORM item_dtls.
      CONCATENATE wa_body_item-item ',' INTO wa_body_item-item.

      APPEND wa_body_item TO it_body_item.
      CLEAR: wa_body_item.
*******End of Multiple Line items ******
      CLEAR:gw_item,gw_item1.
    ENDLOOP.

********Concenating the Line Item Details *************
    LOOP AT it_body_item INTO wa_body_item.
      CONCATENATE lv_itmdtls wa_body_item-item INTO lv_itmdtls.
    ENDLOOP.
***********End of Line item Details****************

*******Val Details ******
    PERFORM valdtls  USING ei_in.


****Payment Details****
    PERFORM paydtls  USING ei_in.

******Reference Details ***********
    PERFORM refdtls  USING ei_in.


*******Additional Doc Details*****
*    CONCATENATE      '"AddlDocDtls": [{'
*              '"Url": "https://einv-apisandbox.nic.in",'
*              '"Docs": "Test Doc",'
*              '"Info": "Document Test"'
*            '}],'  INTO lv_addldocdtls.
*
*********EXP Details*********
*    CONCATENATE      '"ExpDtls": {'
*            '"ShipBNo":' '"a-129"' c_comma
*            '"ShipBDt":' '"09/09/2020"' c_comma
*            '"Port":'   '"INABG1"' c_comma
*            '"RefClm":' '"N"' c_comma
*            '"ForCur":' '"AED"' c_comma
*            '"CntCode":' '"AE"'    "c_null
*            '},'   INTO lv_expdtls.
*
*    CONCATENATE      '"ExpDtls": {'
*            '"ShipBNo":' c_null c_comma
*            '"ShipBDt":' c_null c_comma
*            '"Port":'   c_null c_comma
*            '"RefClm":' c_null c_comma
*            '"ForCur":' c_null c_comma
*            '"CntCode":' c_null    "c_null
*            '},'   INTO lv_expdtls.



******** EWB Details ********
    PERFORM ewbdtls USING ei_in.

*******Final Payload Request Body for IRN Generation ************
    PERFORM payload CHANGING v_payload.

    IF v_payload IS NOT INITIAL.
      MOVE v_payload TO iv_payload.
    ENDIF.

  ELSE.
    RAISE no_item_details.
  ENDIF.

  IF v_payload IS NOT INITIAL.
    PERFORM api_call USING v_payload ei_in CHANGING einv_out eway_out response gv_msg.
  ENDIF.


ENDFUNCTION.
