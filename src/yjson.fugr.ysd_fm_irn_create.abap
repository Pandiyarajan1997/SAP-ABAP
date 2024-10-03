FUNCTION ysd_fm_irn_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_VBELN) TYPE  VBELN_VF
*"  EXPORTING
*"     REFERENCE(EINV_OUT) TYPE  ZST_IRN_OUT
*"     REFERENCE(EWAY_OUT) TYPE  J_1IG_EWAYBILL
*"  TABLES
*"      ERROR_MSG TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"      NOT_VALID_INVOICE
*"      NO_ITEM_DETAILS
*"----------------------------------------------------------------------
*Created By : Ravi / Raju /
*Author     : Bijay Patro
*Created On : 03.12.2020
*TR number  :
*Description: This FM for e-invoice & e-way bill creation of IRN using input parameter Billing number
*             and calling API with Json in government portal then will update
*             the data to table j_1ig_invrefnum & j_1ig_ewaybill.
*----------------------------------------------------------------------*
* Name         TR#number        Date            Descriptin
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
  DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
  CREATE OBJECT lo_log_upd.

  REFRESH: tt_itemdtls[],error_msg[].

  CLEAR: ei_in, v_payload, response, gv_vbeln, einv_out, v_msg1,eway_out,wa_error, wa_j_1ig_invrefnum, wa_j_1ig_ewaybill.

  IF im_vbeln IS NOT INITIAL.

    SELECT SINGLE vbeln FROM vbrk INTO gv_vbeln WHERE vbeln EQ im_vbeln.

    IF gv_vbeln IS INITIAL.
      RAISE not_valid_invoice.
    ENDIF.

*******Build Invoice Logic for Einvoice and Eway Bill*************
    PERFORM build_data USING im_vbeln CHANGING ei_in tt_itemdtls.
********End of Invoice Logic**************************************

**Below FM for Json & Calling API--->>--->>
    IF tt_itemdtls[] IS NOT INITIAL.

      CALL FUNCTION 'YSD_IRN_JSON_PAYLOAD'
        EXPORTING
          ei_in           = ei_in
        IMPORTING
          iv_payload      = v1_payload
          response        = response
          einv_out        = einv_out
          eway_out        = eway_out
          gv_msg          = v_msg1
        TABLES
          tt_itemdtls     = tt_itemdtls
        EXCEPTIONS
          no_item_details = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
*           Implement suitable error handling here
      ENDIF.
    ENDIF.


***Insert data to table J_1IG_INVREFNUM-->>
    IF einv_out IS NOT INITIAL OR
       eway_out IS NOT INITIAL.

      PERFORM response_move USING einv_out eway_out.

****INSERT data to table j_1ig_invrefnum & j_1ig_ewaybill----->>>
      IF wa_j_1ig_invrefnum IS NOT INITIAL OR
         wa_j_1ig_ewaybill IS NOT INITIAL.
        CALL FUNCTION 'YSD_FG_IRN_EWB_UPDATE'
          EXPORTING
            im_irn_dtel = wa_j_1ig_invrefnum
            im_ewb_detl = wa_j_1ig_ewaybill.

      ENDIF.

    ENDIF.
*Added by: Samsudeen M
*Added On: 01.09.2023
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'TECHIRNCREATE'
        ijson           = v1_payload
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

      IF v_msg IS INITIAL.
        CLEAR:v_str2.
        SPLIT response AT '"ErrorDetails":' INTO v_str v_str2.
        IF v_str2 IS NOT INITIAL.
          v_msg = 'N'.
        ENDIF.
      ENDIF.

      IF v_msg = 'N'.     "IRN NOT Created
        PERFORM error_msg TABLES error_msg.
      ELSEIF v_msg1 = abap_true.
        wa_bapiret-message_v1 = response.
        APPEND wa_bapiret TO error_msg.
        CLEAR:wa_bapiret.
      ENDIF.
    ENDIF.

  ELSE.
    RAISE not_valid_invoice.
  ENDIF.



ENDFUNCTION.
