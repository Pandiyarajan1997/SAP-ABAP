FUNCTION ysd_fm_ewaybill_create.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_EWB) TYPE  ZST_EWAYBILL
*"  EXPORTING
*"     REFERENCE(IV_PAYLOAD) TYPE  STRING
*"     REFERENCE(RESPONSE) TYPE  STRING
*"     REFERENCE(EWAY_OUT) TYPE  J_1IG_EWAYBILL
*"     REFERENCE(GV_MSG) TYPE  C
*"     REFERENCE(EX_ERROR) TYPE  STRING
*"  TABLES
*"      ERROR_MSG TYPE  BAPIRET2_T
*"--------------------------------------------------------------------
******** EWB Details ********
  DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
  CREATE OBJECT lo_log_upd.

  IF im_ewb IS NOT INITIAL.

    CLEAR: lv_werks,lv_branch,wa_ship_port,lv_sel_pin1,lv_sel_pin,
           lv_gstin,lv_fkart,gv_expdtls,wa_error,l_tab_t005u,l_tab_vbpa_adrc,v_transid.

    SELECT SINGLE fkart FROM vbrk INTO lv_fkart  WHERE vbeln = im_ewb-vbeln.
    SELECT werks UP TO 1 ROWS FROM vbrp INTO lv_werks  WHERE vbeln = im_ewb-vbeln ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    "Get the Seller Gstin--->>
    IF lv_werks IS NOT INITIAL.
      SELECT SINGLE j_1bbranch FROM t001w
*           INTO @DATA(lv_branch)
             INTO lv_branch
             WHERE werks EQ lv_werks.
    ENDIF.

    IF lv_branch IS NOT INITIAL.
      SELECT  gstin    "Seller GSTIN
        UP TO 1 ROWS FROM j_1bbranch
*      INTO @DATA(lv_gstin)
        INTO lv_gstin
        WHERE branch = lv_branch ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.

    PERFORM seller_buyer_dtls USING im_ewb.

    READ TABLE l_tab_t005u INTO w_tab_t005u  INDEX 1. "#EC CI_NOORDER "Seller addressAdded by <IT-CAR Tool> during Code Remediation
    READ TABLE l_tab_vbpa_adrc INTO w_tab_vbpa_adrc INDEX 1. "#EC CI_NOORDER "Buyer AddressAdded by <IT-CAR Tool> during Code Remediation

    MOVE w_tab_vbpa_adrc-stcd3        TO lv_buyer.      "Buyer Region/State code
    MOVE w_tab_vbpa_adrc-post_code1   TO lv_buy_pin.    "Buyer Pincode
    MOVE w_tab_t005u-post_code1       TO lv_sel_pin1.    "Seller Pincode
    MOVE lv_gstin                     TO lv_seller.     "SELLER GSTIN

*    move im_ewb-transid+1(2) to v_transid.
**    REPLACE ALL OCCURRENCES OF '"' in v_transid WITH space.
*    CONDENSE v_transid NO-GAPS.

    "  IF lv_fkart = 'YBEO'.                           "Export Entry
    SELECT SINGLE * FROM zsh_port INTO wa_ship_port WHERE werks = lv_werks.
    MOVE wa_ship_port-sh_pin TO lv_sel_pin.     "Export Seller Shipping Port Pincode
    PERFORM exp_dtls.                          "Export Shipping details JSON
    " ENDIF.

    PERFORM ewbdtls USING im_ewb.


    IF lv_ewbdtls IS NOT INITIAL.
      PERFORM api_call USING lv_ewbdtls im_ewb CHANGING eway_out response gv_msg ex_error.
    ENDIF.

***Insert data to table J_1IG_INVREFNUM-->>
    IF eway_out IS NOT INITIAL.

      PERFORM response_move USING eway_out.

****INSERT data to table j_1ig_ewaybill----->>>
      IF wa_j_1ig_ewaybill IS NOT INITIAL.

        CALL FUNCTION 'ZSD_FG_IRN_EWB_UPDATE'
          EXPORTING
            im_irn_dtel = wa_j_1ig_invrefnum
            im_ewb_detl = wa_j_1ig_ewaybill.
*Added by Samsudeen On 01.09.2023
        DATA(im_ewb_tmp) = im_ewb.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-vehno WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-transid WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-transname WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-vehtyp WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-transmode WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-distance WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-transdoc WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN im_ewb_tmp-transdt WITH ''.
        "Purpose: To store the transporter Details in custom table for invoice print
        INSERT INTO zsd_eway_trdtls VALUES @( VALUE #( mandt     = sy-mandt
                                                       bukrs     = im_ewb_tmp-bukrs
                                                       doctyp    = im_ewb_tmp-doctyp
                                                       docno     = im_ewb_tmp-vbeln
                                                       gjahr     = im_ewb_tmp-gjahr
                                                       ebillno   = wa_j_1ig_ewaybill-ebillno
                                                       vehno     = im_ewb_tmp-vehno
                                                       transid   = im_ewb_tmp-transid
                                                       transname = im_ewb_tmp-transname
                                                       vehtyp    = im_ewb_tmp-vehtyp
                                                       transmode = im_ewb_tmp-transmode
                                                       distance  = im_ewb_tmp-distance
                                                       transdoc  = im_ewb_tmp-transdoc
                                                       transdt   = im_ewb_tmp-transdt
                                                       status    = wa_j_1ig_ewaybill-status
                                                       ernam     = wa_j_1ig_ewaybill-ernam
                                                       erdat     = wa_j_1ig_ewaybill-erdat
                                                       erzet     = sy-uzeit ) ).
      ENDIF.
    ENDIF.
*Added by: Samsudeen M
*Added On: 01.09.2023
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'TECHEWAYCREATE'
        ijson           = lv_ewbdtls
        ojson           = response
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

    IF response IS NOT INITIAL.
      REFRESH: it_error[].
      CLEAR:v_str,v_str1,v_str2,v_errorcode,v_msg,wa_error.
      SPLIT response AT '"Success":"' INTO v_str v_msg.
      CLEAR:v_str.

      IF v_msg = 'N'.     "EWAY BILL NOT Created
        PERFORM error_msg TABLES error_msg USING response.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
