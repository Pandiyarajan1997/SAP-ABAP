*&---------------------------------------------------------------------*
*& Report ZDMS_UPDATE_ZDMS_CRNOTE_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_update_zdms_crnote_log.

DATA : gv_kunnr  TYPE kunnr,
       gv_update TYPE char1.
SELECT-OPTIONS so_kunnr FOR gv_kunnr.

SELECT * FROM zdms_crnote_log INTO TABLE @DATA(lt_crnote) WHERE distr IN @so_kunnr.
*                                                          AND   invno <> @abap_false.
IF sy-subrc = 0.
  SORT lt_crnote BY distr kunnr.

  LOOP AT lt_crnote INTO DATA(ls_crnote).

****************get the invoice no*************
*    IF ls_crnote-invno IS INITIAL.

*      SELECT SINGLE * FROM zdms_retail_acnt INTO @DATA(ls_acnt)
*                                     WHERE distributor = @ls_crnote-distr
*                                     AND   retailer    = @ls_crnote-kunnr
*                                     AND   doc_no      = @ls_crnote-acdno.
*      IF sy-subrc = 0.
*        ls_crnote-invno = ls_acnt-invoice.
*        gv_update = abap_true.
*      ENDIF.
*    ENDIF.
*
******************business area update**********
*    IF ls_crnote-gsber IS INITIAL.
*
*      SELECT SINGLE kunnr,werks FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @ls_crnote-distr.
*      IF sy-subrc = 0.
*        ls_crnote-gsber = ls_kna1-werks.
*        gv_update       = abap_true.
*      ENDIF.
*    ENDIF.
*
**************check auto cd generated or not********
*    IF ls_crnote-dgdoc IS NOT INITIAL AND ls_crnote-crflag IS INITIAL.
*      ls_crnote-crflag = abap_true.
*      gv_update = abap_true.
*    ENDIF.
*
*********************check the invoice flag or payment flag
*    IF ls_crnote-xblnr IS INITIAL AND ls_crnote-invflag IS INITIAL.
*      ls_crnote-invflag = 'I'.
*      gv_update         = abap_true.
*    ELSEIF ls_crnote-xblnr IS NOT INITIAL AND ls_crnote-invflag IS INITIAL.
*      ls_crnote-invflag = 'P'.
*      gv_update         = abap_true.
*    ENDIF.

****************update the new doc no*************
*    IF ls_crnote-new_docno IS INITIAL.

      SELECT SINGLE belnr,gjahr,bktxt FROM bkpf INTO @DATA(ls_bkpf)
                                                WHERE bukrs = @ls_crnote-bukrs
                                                AND   belnr = @ls_crnote-dgdoc
                                                AND   gjahr = @ls_crnote-dgdyr.
      IF ls_bkpf-bktxt IS NOT INITIAL.
        ls_crnote-new_docno = ls_bkpf-bktxt.
        gv_update           = abap_true.
      ENDIF.

*    ENDIF.

************only modify id update flag is X**********
    IF gv_update = abap_true.
      MODIFY zdms_crnote_log FROM ls_crnote.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CLEAR : ls_crnote,gv_update,ls_bkpf.",ls_acnt,ls_kna1,.

  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE : 'Updation Completed' TYPE 'S'.
  ENDIF.

ELSE.

  MESSAGE : 'No date Found' TYPE 'S' DISPLAY LIKE 'E'.

ENDIF.
