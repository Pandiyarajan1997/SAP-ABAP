class ZCL_API_REVERSAL_DZ_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_resp,
        dms_id      TYPE xblnr1,
        sap_id      TYPE zref_id,
        distributor TYPE kunnr,
        retailer    TYPE kunnr,
      END OF ty_resp .

  data GS_INPUT type TY_RESP .
  data GV_PLANT type WERKS_D .

  methods FBRA_PROCESS
    importing
      !GJAHR type GJAHR
      !BUKRS type BUKRS
      !AUGBL type AUGBL
      !BELNR type BELNR_D optional
      !BLART type BLART
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE .
  methods FB08_PROCESS
    importing
      !BELNR type BELNR_D
      !GJAHR type GJAHR
      !AUGBL type AUGBL optional
      !BLART type BLART optional
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE
      !REV_BELNR type BELNR_D
      !REV_GJAHR type GJAHR .
  methods VALIDATIONS
    exporting
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_API_REVERSAL_DZ_DMS IMPLEMENTATION.


  METHOD fb08_process.
    DATA : lt_return   TYPE TABLE OF  bapiret2,
           ls_reversal TYPE bapiacrev,
           ls_log      TYPE zfi_dms_dz_rever,
           ls_remove   TYPE zdms_dz_removal,
           obj_key     TYPE bapiacrev-obj_key,
           lv_belnr    TYPE belnr_d,
           lv_gjahr    TYPE gjahr.
    CLEAR : lv_type,lv_msg,lv_gjahr,lv_belnr.
*******************get the object key from bkpf***************
    SELECT SINGLE awtyp,awkey,budat FROM bkpf INTO @DATA(ls_bkpf)
                                              WHERE bukrs = 'DMS1'
                                              AND   belnr = @belnr
                                              AND   gjahr = @gjahr.
    IF sy-subrc NE 0.
      lv_msg  = |Incorrect document number - { belnr } { gjahr } |.
      lv_type = 'E'.
      EXIT.
    ENDIF.
**************************only for DZ document******************
    IF blart = 'DZ'.
**********************call the bdc condain fm for reverse DZ*************
      CALL FUNCTION 'ZDMS_DZ_REVERSE_BDC'
        EXPORTING
          belnr   = belnr
          gjahr   = gjahr
          bukrs   = 'DMS1'
          budat   = sy-datum
        IMPORTING
          message = lv_msg
          status  = lv_type
          e_belnr = lv_belnr
          e_gjahr = lv_gjahr.
**************************only for DG document******************
    ELSEIF blart = 'DG'.
** reversal document filling*************
      ls_reversal-obj_type   = ls_bkpf-awtyp.
      ls_reversal-obj_key    = ls_bkpf-awkey.
      ls_reversal-obj_key_r  = ls_bkpf-awkey.
      ls_reversal-reason_rev = '04'.
      ls_reversal-pstng_date = sy-datum."ls_bkpf-budat.
      ls_reversal-comp_code  = 'DMS1'.
*****************Specify Name of Current Logged-on System (client in R/3 System)*************
      CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
        IMPORTING
          own_logical_system = ls_reversal-obj_sys.

**** Call FB08 BAPI*****************
      CLEAR   : obj_key.
      REFRESH : lt_return.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
        EXPORTING
          reversal = ls_reversal
          bus_act  = 'RFBU'
        IMPORTING
          obj_key  = obj_key
        TABLES
          return   = lt_return.

      READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        LOOP AT lt_return INTO ls_return.
          lv_msg = |{ ls_return-message } , { lv_msg }|.
        ENDLOOP.
        lv_type   = 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        lv_type   = 'S'.
        lv_msg    = |document reversed successfully - { obj_key(10) } - { obj_key+14(4) }|.
        lv_belnr  = obj_key(10).
        lv_gjahr  = obj_key+14(4).
************************delete from the auto cd log table**************
        DELETE FROM zdms_crnote_log WHERE bukrs = 'DMS1'
                                    AND   refid = gs_input-sap_id
                                    AND   distr = gs_input-distributor
                                    AND   kunnr = gs_input-retailer
                                    AND   dgdoc = belnr
                                    AND   dgdyr = gjahr.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_type = 'S'.
***********************add the document removal table ( old doc )**********
      CLEAR : ls_remove.
      ls_remove-distributor = gs_input-distributor.
      ls_remove-dealer      = gs_input-retailer.
      ls_remove-belnr       = belnr.
      ls_remove-gjahr       = gjahr.
      INSERT zdms_dz_removal FROM ls_remove.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
***********************add the document removal table ( new doc )**********
      ls_remove-belnr = lv_belnr.
      ls_remove-gjahr = lv_gjahr.
      INSERT zdms_dz_removal FROM ls_remove.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

********************update thelog table ZFI_DMS_DZ_REVER******************
    CLEAR ls_log.
    ls_log-bukrs        = 'DMS1'.
    ls_log-kunnr        = gs_input-retailer.
    ls_log-xblnr        = gs_input-dms_id.
    ls_log-reference_id = gs_input-sap_id.
    ls_log-doc_no       = belnr.
    ls_log-gjahr        = gjahr.
    ls_log-augbl        = augbl.
    ls_log-blart        = blart.
    ls_log-distributor  = gs_input-distributor.
    ls_log-werks        = gv_plant.
    ls_log-rev_belnr    = lv_belnr.
    ls_log-rev_gjahr    = lv_gjahr.
    ls_log-erdat        = sy-datum.
    ls_log-erzet        = sy-uzeit.
    ls_log-ernam        = sy-uname.
    ls_log-remarks      = lv_msg.
    ls_log-type         = lv_type.
    MODIFY zfi_dms_dz_rever FROM ls_log.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
****************export the ac no & year*************
    rev_belnr = lv_belnr.
    rev_gjahr = lv_gjahr.

  ENDMETHOD.


  METHOD fbra_process.

    DATA : lt_accnt                TYPE TABLE OF  rf05r_acct,
           e_xstor                 TYPE  c,
           ev_reverse_needed_text1 TYPE  string,
           ev_reverse_needed_text2 TYPE  string,
           ev_reverse_needed_text3 TYPE  string,
           ls_log                  TYPE  zfi_dms_dz_rever.

    CLEAR : lv_type,lv_msg.
************call the bapi for FBRA reset*********
    CALL FUNCTION 'CALL_FBRA'
      EXPORTING
        i_bukrs                 = bukrs
        i_augbl                 = augbl
        i_gjahr                 = gjahr
      IMPORTING
        e_xstor                 = e_xstor
        ev_reverse_needed_text1 = ev_reverse_needed_text1
        ev_reverse_needed_text2 = ev_reverse_needed_text2
        ev_reverse_needed_text3 = ev_reverse_needed_text3
      TABLES
        t_accnt                 = lt_accnt.
***********check the bapi response************
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      lv_type = 'S'.
      lv_msg  = 'Cleared document Reset successfully'.
********************update thelog table ZFI_DMS_DZ_REVER******************
      CLEAR ls_log.
      ls_log-bukrs        = bukrs.
      ls_log-kunnr        = gs_input-retailer.
      ls_log-xblnr        = gs_input-dms_id.
      ls_log-reference_id = gs_input-sap_id.
      ls_log-doc_no       = belnr.
      ls_log-augbl        = augbl.
      ls_log-gjahr        = gjahr.
      ls_log-blart        = blart.
      ls_log-distributor  = gs_input-distributor.
      ls_log-werks        = gv_plant.
      ls_log-erdat        = sy-datum.
      ls_log-erzet        = sy-uzeit.
      ls_log-ernam        = sy-uname.
*      ls_log-type         = lv_type.
      ls_log-remarks      = lv_msg.
      MODIFY zfi_dms_dz_rever FROM ls_log.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ELSE.
      lv_type = 'E'.
      lv_msg  = 'Error in Cleared document Reset process'.
    ENDIF.


  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Pandiarajan
    "Created on: 06.05.2024
    "Reference by: Ramakrishnan J
    "Purpose : Reverse the incomming payment - DMS
*-------------------------------------------------------------*

    TYPES: BEGIN OF ty_res,
             dms_id      TYPE xblnr1,
             sap_id      TYPE zref_id,
             distributor TYPE kunnr,
             retailer    TYPE kunnr,
             type        TYPE bapi_mtype,
             msg         TYPE string,
           END OF ty_res.

    DATA : lv_body     TYPE string,
           dms_orderid TYPE zorder_id.

    DATA: gs_response TYPE ty_res.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** deserialize the INPUT our required INPUT ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_input ).
***************input alpha conversion************
    gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
    gs_input-retailer    = |{ gs_input-retailer ALPHA = IN }|.
***************call the validations process**********
    CALL METHOD validations
      IMPORTING
        type    = gs_response-type
        message = gs_response-msg.
**************check the response***********
    IF gs_response-type IS INITIAL.
      gs_response-type = 'E'.
    ENDIF.
    gs_response-distributor = gs_input-distributor.
    gs_response-retailer    = gs_input-retailer.
    gs_response-dms_id      = gs_input-dms_id.
    gs_response-sap_id      = gs_input-sap_id.

*********************error log***********
    IF gs_response-type = 'E'.
      dms_orderid = gs_input-dms_id.
      lo_errorlog_dms->log_entry_store(
        EXPORTING
          type                = 26
          status              = 10
          dms_orderid         = dms_orderid
          distributor         = gs_input-distributor
          plant               = gv_plant
          dealer              = gs_input-retailer
          msg                 = gs_response-msg ).
    ENDIF.

    CLEAR:lv_body .
** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  gs_response
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = lv_body ).

***************store the log table*************
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'DMS_DZ_REVERSE'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = gs_input-distributor
        retailer        = gs_input-retailer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).
*******delete the double click check*********
    DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-dms_id.
  ENDMETHOD.


  METHOD validations.
***************DZ cancellation class declaration************
    DATA : lv_gjahr TYPE gjahr.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-dms_id.
    IF sy-subrc = 0.
      message = | Already Payment cancellation in Processing { gs_input-dms_id }|.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-dms_id ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate payement reversal*********
    SELECT SINGLE * FROM zfi_dms_cust_pay INTO @DATA(ls_payment)
                                          WHERE bukrs        = 'DMS1'
                                          AND   kunnr        = @gs_input-retailer
                                          AND   xblnr        = @gs_input-dms_id
                                          AND   reference_id = @gs_input-sap_id
                                          AND   distributor  = @gs_input-distributor.
    IF sy-subrc NE 0.
      message = | Incorrect details |.
      EXIT.
    ELSEIF sy-subrc = 0 AND ls_payment-status = '70'.
      message = | Already Payment reversed |.
      type    = 'S'.
      EXIT.
    ELSEIF ls_payment-doc_no IS INITIAL.
      message = |Still not processed|.
      EXIT.
    ENDIF.

    CLEAR : gv_plant.
**********check the retailer code*************
    SELECT SINGLE werks FROM kna1 INTO gv_plant WHERE kunnr = gs_input-retailer.
    IF sy-subrc NE 0.
      message = | Retailer is not found - { gs_input-retailer }|.
      EXIT.
    ENDIF.
**********check the distributor code*************
    SELECT SINGLE werks FROM kna1 INTO gv_plant WHERE kunnr = gs_input-distributor.
    IF sy-subrc NE 0.
      message = | Distributor is not found - { gs_input-distributor }|.
      EXIT.
    ELSE.
      IF gv_plant IS INITIAL.
        message = | Distributor plant not maintained - { gs_input-distributor }|.
        EXIT.
      ENDIF.
    ENDIF.

******************validate the incomming payment cleared or not****************
    SELECT SINGLE belnr,bukrs,gjahr,augbl,budat,augdt FROM bsad INTO @DATA(ls_bsad)
                                                      WHERE bukrs = @ls_payment-bukrs
                                                      AND   belnr = @ls_payment-doc_no
                                                      AND   gjahr = @ls_payment-gjahr.
    IF sy-subrc = 0.
*********************check the document is reset or not***********
      SELECT SINGLE * FROM zfi_dms_dz_rever INTO @DATA(ls_check)
                                            WHERE bukrs        = 'DMS1'
                                            AND   kunnr        = @gs_input-retailer
                                            AND   xblnr        = @gs_input-dms_id
                                            AND   reference_id = @gs_input-sap_id
                                            AND   doc_no       = @ls_payment-doc_no
                                            AND   gjahr        = @ls_payment-gjahr.
      IF sy-subrc NE 0.
        IF ls_bsad-augbl IS NOT INITIAL.
******************get the fiscal year***************
          CLEAR : lv_gjahr.
          CALL FUNCTION 'GET_CURRENT_YEAR'
            EXPORTING
              bukrs = 'DMS1'
              date  = ls_bsad-augdt
            IMPORTING
              curry = lv_gjahr.
*********************call the method to reset the clearing documents***********
          CLEAR : message,type.
          CALL METHOD fbra_process
            EXPORTING
              bukrs   = ls_bsad-bukrs
              augbl   = ls_bsad-augbl
              gjahr   = lv_gjahr
              blart   = 'DZ'
              belnr   = ls_bsad-belnr
            IMPORTING
              lv_msg  = message
              lv_type = type.
          IF type = 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        ls_bsad-augbl = ls_check-augbl.
      ENDIF.
    ENDIF.
********************call the method to reverse the document ( DZ )************
*********************check the document is reverse or not***********
    SELECT SINGLE * FROM zfi_dms_dz_rever INTO @ls_check
                                          WHERE bukrs        = 'DMS1'
                                          AND   kunnr        = @gs_input-retailer
                                          AND   xblnr        = @gs_input-dms_id
                                          AND   reference_id = @gs_input-sap_id
                                          AND   doc_no       = @ls_payment-doc_no
                                          AND   gjahr        = @ls_payment-gjahr
                                          AND   type         = 'S'.
    IF sy-subrc NE 0.
      CLEAR : message,type.
      CALL METHOD fb08_process
        EXPORTING
          belnr   = ls_payment-doc_no
          gjahr   = ls_payment-gjahr
          blart   = 'DZ'
          augbl   = ls_bsad-augbl
        IMPORTING
          lv_msg  = message
          lv_type = type.
      IF type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
************************delete only auto cd not generated invoices from the auto cd log table**************
    DELETE FROM zdms_crnote_log WHERE bukrs = 'DMS1'
                                AND   refid = gs_input-sap_id
                                AND   distr = gs_input-distributor
                                AND   kunnr = gs_input-retailer
                                AND   dgdoc = abap_false.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
*********************get the auto credit note datas & reverse************
    SELECT * FROM zdms_crnote_log INTO TABLE @DATA(lt_autocd)
                                  WHERE bukrs = 'DMS1'
                                  AND   refid = @gs_input-sap_id
                                  AND   distr = @gs_input-distributor
                                  AND   kunnr = @gs_input-retailer
                                  AND   xblnr = @gs_input-dms_id.
    IF sy-subrc = 0.

      LOOP AT lt_autocd ASSIGNING FIELD-SYMBOL(<fs_autocd>).

        CLEAR : message,type,ls_bsad.
******************validate the credit note cleared or not****************
        SELECT SINGLE belnr,bukrs,gjahr,augbl,budat,augdt FROM bsad INTO @ls_bsad
                                                          WHERE bukrs = @<fs_autocd>-bukrs
                                                          AND   belnr = @<fs_autocd>-dgdoc
                                                          AND   gjahr = @<fs_autocd>-dgdyr.
        IF sy-subrc = 0.
          IF ls_bsad-augbl IS NOT INITIAL.
******************get the fiscal year***************
            CLEAR : lv_gjahr.
            CALL FUNCTION 'GET_CURRENT_YEAR'
              EXPORTING
                bukrs = 'DMS1'
                date  = ls_bsad-augdt
              IMPORTING
                curry = lv_gjahr.
*********************call the method to reset the clearing documents***********
            CLEAR : message,type.
            CALL METHOD fbra_process
              EXPORTING
                bukrs   = ls_bsad-bukrs
                augbl   = ls_bsad-augbl
                gjahr   = lv_gjahr
                blart   = 'DG'
                belnr   = ls_bsad-belnr
              IMPORTING
                lv_msg  = message
                lv_type = type.
            IF type = 'E'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

*******************reverse the FB08****************
        CALL METHOD fb08_process
          EXPORTING
            belnr   = <fs_autocd>-dgdoc
            gjahr   = <fs_autocd>-dgdyr
            blart   = 'DG'
          IMPORTING
            lv_msg  = message
            lv_type = type.
****************Exit from the loop************
        IF type = 'E'.
          EXIT.
        ENDIF.
      ENDLOOP.
****************Exit from the method************
      IF type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
**************update the main incomming payment log table************
    IF type = 'S'.
      message = |Document Reversed Successfully|.

      UPDATE zfi_dms_cust_pay SET status = '70' WHERE bukrs = 'DMS1'
                                                AND   kunnr = gs_input-retailer
                                                AND   xblnr = gs_input-dms_id
                                                AND   reference_id = gs_input-sap_id.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
