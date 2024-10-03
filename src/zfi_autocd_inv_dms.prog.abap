*&---------------------------------------------------------------------*
*& Report ZFI_AUTOCD_INV_DMS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_autocd_inv_dms.
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 09.05.2024
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                :
*
*  Business Logic            : invoice autocredit note based on the customer credit balance
*
*  Released on Date          :
*
*=======================================================================

DATA : gv_kunnr TYPE kna1-kunnr,
       gv_vbeln TYPE vbrk-vbeln,
       gv_fkdat TYPE vbrk-fkdat.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_dist  FOR gv_kunnr,
                   so_kunnr FOR gv_kunnr,
                   so_vbeln FOR gv_vbeln,
                   so_fkdat FOR gv_fkdat.

SELECTION-SCREEN : END OF BLOCK b1.

CLASS lcl_autocd DEFINITION FINAL.

  PUBLIC SECTION.

*****************structure************
    TYPES : BEGIN OF ty_alv,
              distributor TYPE zdist,
              dealer      TYPE kunnr,
              inv_no      TYPE vbeln_vf,
              inv_type    TYPE fkart,
              inv_date    TYPE fkdat,
              amount      TYPE netwr,
              cr_amnt     TYPE wrbtr,
              belnr       TYPE belnr_d,
              gjahr       TYPE gjahr,
              type        TYPE bapi_mtype,
              remarks     TYPE zremark,
            END OF ty_alv.

    DATA : gt_alv TYPE TABLE OF ty_alv.

    METHODS : fetch,
      autocd_process EXPORTING type    TYPE bapi_mtype
                               message TYPE zremark
                               belnr   TYPE belnr_d
                               gjahr   TYPE gjahr
                     CHANGING  ls_inv  TYPE zdms_inv_credit.

ENDCLASS.

CLASS lcl_autocd IMPLEMENTATION.

  METHOD fetch.
    DATA : l_total_amt  TYPE wrbtr,
           ls_alv       TYPE ty_alv,
           lv_msg       TYPE string,
           lv_index     TYPE sy-tabix,
           lobj_dgrever TYPE REF TO zcl_api_reversal_dz_dms.
    CREATE OBJECT lobj_dgrever.
*****************fetch from invoice details*************
    SELECT * FROM zdms_inv_credit INTO TABLE @DATA(lt_inv) WHERE invoice_no  IN @so_vbeln
                                                           AND   inv_date    IN @so_fkdat
                                                           AND   distributor IN @so_dist
                                                           AND   dealer      IN @so_kunnr
                                                           AND   status      EQ '10'.
    IF sy-subrc = 0.

      SORT : lt_inv BY distributor dealer invoice_no.
**********************get the credit note log**************
      SELECT * FROM zdms_crnote_log INTO TABLE @DATA(lt_crnote)
                                    FOR ALL ENTRIES IN @lt_inv
                                    WHERE distr = @lt_inv-distributor
                                    AND   kunnr = @lt_inv-dealer.
      IF sy-subrc = 0.
        SORT : lt_crnote BY acdno acdyr.
      ENDIF.
**********************get the cleared item details*****************
      SELECT belnr,gjahr FROM bsad INTO TABLE @DATA(lt_bsad)
                                   FOR ALL ENTRIES IN @lt_inv
                                   WHERE bukrs = 'DMS1'
                                   AND   belnr = @lt_inv-ac_doc
                                   AND   gjahr = @lt_inv-fisc_year.
      IF sy-subrc = 0.
        SORT lt_bsad BY belnr gjahr.
      ENDIF.

      LOOP AT lt_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).
**********************fetch open item for every new dealer***************
*        AT NEW dealer.
***********************get the OPEN item details*****************
*          SELECT a~kunnr, a~budat, a~bldat, a~blart,a~shkzg,a~waers,
*                 a~dmbtr, a~xblnr, a~belnr ,a~sgtxt,
*                 a~vbeln,  a~umskz ,a~gjahr
*                 FROM bsid AS a INNER JOIN faglflexa AS b
*                 ON  a~gjahr = b~ryear
*                 AND a~belnr = b~docnr
*                 INTO TABLE @DATA(lt_bsid2)
*                 WHERE a~bukrs EQ 'DMS1'
*                 AND a~kunnr EQ @<fs_inv>-dealer
*                 AND a~umskz <> 'H'
*                 AND b~rldnr  = '0L'
*                 AND b~rbukrs = 'DMS1'
*                 AND b~docln  = '000001'
*                 AND b~rbusa  = @<fs_inv>-dist_plant.
*          IF sy-subrc = 0.
*            SORT : lt_bsid2 BY budat.
*          ENDIF.
*        ENDAT.
*************fill the data - display alv screen************
        CLEAR : ls_alv.
        ls_alv-distributor = <fs_inv>-distributor.
        ls_alv-dealer      = <fs_inv>-dealer.
        ls_alv-inv_no      = <fs_inv>-invoice_no.
        ls_alv-inv_type    = <fs_inv>-inv_type.
        ls_alv-inv_date    = <fs_inv>-inv_date.
        ls_alv-amount      = <fs_inv>-net_amount.
******************fill process datas**************
        <fs_inv>-process_by   = sy-uname.
        <fs_inv>-process_date = sy-datum.
        <fs_inv>-process_time = sy-uzeit.
******************************check the invoice is cancelled or not**************
        READ TABLE lt_inv TRANSPORTING NO FIELDS WITH KEY invoice_no = <fs_inv>-invoice_no
                                                          inv_type   = 'C'.
        IF sy-subrc = 0.
**************************check the autocd log table***************
          READ TABLE lt_crnote INTO DATA(ls_cr) WITH KEY acdno = <fs_inv>-ac_doc
                                                         acdyr = <fs_inv>-fisc_year BINARY SEARCH.
          IF sy-subrc = 0.
*************************get the index no********
            CLEAR : lv_index.
            lv_index = sy-tabix.
********************pass the global data to the object**************
            lobj_dgrever->gv_plant             = <fs_inv>-dist_plant.
            lobj_dgrever->gs_input-distributor = <fs_inv>-distributor.
            lobj_dgrever->gs_input-retailer    = <fs_inv>-dealer.
            lobj_dgrever->gs_input-dms_id      = ls_cr-xblnr.
            lobj_dgrever->gs_input-sap_id      = ls_cr-refid.
******************call the fb08 for reverse the DG document*****************
            lobj_dgrever->fb08_process(
              EXPORTING
                belnr   = ls_cr-dgdoc
                gjahr   = ls_cr-dgdyr
                blart   = 'DG'
              IMPORTING
                rev_belnr = <fs_inv>-da_belnr1
                lv_msg  = lv_msg
                lv_type = ls_alv-type ).

            ls_alv-remarks   = lv_msg.

            IF ls_alv-type   = 'S'.
              DELETE lt_crnote INDEX lv_index.
              ls_alv-belnr       = <fs_inv>-da_belnr1.
              ls_alv-gjahr       = <fs_inv>-fisc_year.
              ls_alv-remarks     = 'Cancelled Invoice & DG Reversed'.
              <fs_inv>-status    = '60'.
              <fs_inv>-da_date1  = sy-datum.
              <fs_inv>-da_amt1   = ls_cr-dgamt.
              <fs_inv>-dg_amt1   = ls_cr-dgamt.
              <fs_inv>-dg_belnr1 = ls_cr-dgdoc.
              <fs_inv>-dg_date1  = ls_cr-crdat.
              <fs_inv>-remarks   =  'Cancelled Invoice & DG Reversed'.
            ENDIF.
            APPEND ls_alv TO gt_alv.
            CLEAR : lv_msg,ls_cr.
            CONTINUE.
          ELSE.
            <fs_inv>-status  = '60'.
            <fs_inv>-remarks =  'Cancelled Invoice'.
            ls_alv-type      = 'S'.
            ls_alv-remarks   = 'Cancelled Invoice'.
            APPEND ls_alv TO gt_alv.
            CONTINUE.
          ENDIF.
        ENDIF.
***************************check the autocd log table***************
*        CLEAR : ls_cr.
*        READ TABLE lt_crnote INTO ls_cr WITH KEY acdno = <fs_inv>-ac_doc
*                                                 acdyr = <fs_inv>-fisc_year BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          <fs_inv>-status    = '40'.
*          <fs_inv>-dg_amt1   = ls_cr-dgamt.
*          <fs_inv>-dg_belnr1 = ls_cr-dgdoc.
*          <fs_inv>-dg_date1  = ls_cr-crdat.
*          <fs_inv>-remarks   = 'Already Autocd Generated'.
*          ls_alv-type        = 'S'.
*          ls_alv-remarks     = 'Already Autocd Generated'.
*          APPEND ls_alv TO gt_alv.
*          CONTINUE.
*        ENDIF.
**************************check the open items******************
*        READ TABLE lt_bsid2 TRANSPORTING NO FIELDS WITH KEY belnr = <fs_inv>-ac_doc
*                                                            gjahr = <fs_inv>-fisc_year.
*        IF sy-subrc = 0.
*********************Calculate the credit balance*****************
*          CLEAR : l_total_amt.
*          LOOP AT lt_bsid2 INTO DATA(lw_openitem) WHERE budat LE <fs_inv>-inv_date
*                                                  AND   shkzg = 'H'. " H-Credit amount from Sub-Dealers
*            l_total_amt = l_total_amt + lw_openitem-dmbtr.
*            CLEAR : lw_openitem.
*          ENDLOOP.
*******************check the credit balance amount with invoice amount.
*          LOOP AT lt_bsid2 INTO lw_openitem WHERE budat LE <fs_inv>-inv_date
*                                            AND   shkzg = 'S'.
*            IF l_total_amt GE lw_openitem-dmbtr.
*              l_total_amt = l_total_amt - lw_openitem-dmbtr.
***********************fetch the document number for already cd given or net***************
*              SELECT SINGLE * FROM zdms_crnote_log INTO @DATA(gw_crnote_log)
*                                                   WHERE bukrs  = 'DMS1'
*                                                   AND   acdno  = @lw_openitem-belnr
*                                                   AND   acdyr  = @lw_openitem-gjahr
*                                                   AND   msgtyp = 'S'.
*              IF sy-subrc = 0.
*                CONTINUE.
*              ENDIF.
********************call the auto credit note************
*              IF lw_openitem-belnr = <fs_inv>-ac_doc AND lw_openitem-gjahr = <fs_inv>-fisc_year.
*                CALL METHOD autocd_process
*                  IMPORTING
*                    type    = ls_alv-type
*                    message = ls_alv-remarks
*                    belnr   = ls_alv-belnr
*                    gjahr   = ls_alv-gjahr
*                  CHANGING
*                    ls_inv  = <fs_inv>.
*                IF ls_alv-type = 'S'.
*                  ls_alv-gjahr  = <fs_inv>-fisc_year.
*                ENDIF.
*                APPEND ls_alv TO gt_alv.
*                EXIT.
*              ENDIF.
*            ELSE.
**              IF lw_openitem-belnr = <fs_inv>-ac_doc AND lw_openitem-gjahr = <fs_inv>-fisc_year.
*              CLEAR : l_total_amt.
*              <fs_inv>-status  = '50'.
*              <fs_inv>-remarks = 'Invoice Amount Exceed Credit balance'.
*              ls_alv-type      = 'S'.
*              ls_alv-remarks   = 'Invoice Amount Exceed Credit balance'.
*              APPEND ls_alv TO gt_alv.
*              EXIT.
**              ENDIF.
*            ENDIF.
*          ENDLOOP.
*          CONTINUE.
*        ENDIF.
**************************check the cleared items******************
*        READ TABLE lt_bsad TRANSPORTING NO FIELDS WITH KEY belnr = <fs_inv>-ac_doc
*                                                           gjahr = <fs_inv>-fisc_year BINARY SEARCH.
*        IF sy-subrc = 0.
********************call the auto credit note************
*          CALL METHOD autocd_process
*            IMPORTING
*              type    = ls_alv-type
*              message = ls_alv-remarks
*              belnr   = ls_alv-belnr
*              gjahr   = ls_alv-gjahr
*            CHANGING
*              ls_inv  = <fs_inv>.
*          APPEND ls_alv TO gt_alv.
*        ENDIF.
      ENDLOOP.

***************update the invoice table**********
      MODIFY zdms_inv_credit FROM TABLE lt_inv.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD autocd_process.
    DATA: lv_msg_text TYPE string.
    DATA: lv_objtyp TYPE bapiache09-obj_type.
    DATA: lv_objkey TYPE bapiache09-obj_key.
    DATA: lv_objsys TYPE bapiache09-obj_sys.
    DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
          lt_payable    TYPE TABLE OF bapiacap09,
          lt_recievable TYPE TABLE OF bapiacar09,
          lt_curramnt   TYPE TABLE OF bapiaccr09,
          lt_return     TYPE TABLE OF bapiret2,
          lv_roundoff   TYPE i,
          ls_crlog      TYPE zdms_crnote_log.

****************calculate the autocd amount***************
    ls_inv-dg_amt1 = ls_inv-net_amount * '0.025'.
    CLEAR : lv_roundoff.
    lv_roundoff    = ls_inv-dg_amt1.
    ls_inv-dg_amt1 = lv_roundoff.

*************fill the header details***************
    DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = 'DMS1'
                                        doc_date   = ls_inv-inv_date
                                        pstng_date = ls_inv-inv_date
                                        ref_doc_no = ls_inv-invoice_no
                                        fisc_year  = ls_inv-fisc_year
                                        doc_type   = 'DG' ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return,lt_recievable.
*************fill the recievable details***************
    lt_recievable = VALUE #( ( itemno_acc = '1'
                               customer   = ls_inv-dealer
                               item_text  = |{ ls_inv-invoice_no }-Credit Note @2.5%|
                               bus_area   = ls_inv-dist_plant  ) ).
*************fill the gl account details***************
    lt_glaccount = VALUE #( ( itemno_acc = '2'
                              gl_account = '0042002012'
                              item_text  = |{ ls_inv-invoice_no }-Credit Note @2.5%|
                              bus_area   = ls_inv-dist_plant ) ).
*************fill the amount details***************
    lt_curramnt = VALUE #( ( itemno_acc = '1'
                             currency   = 'INR'
                             amt_doccur = ( ls_inv-dg_amt1 ) * -1 )
                           ( itemno_acc = '2'
                             currency   = 'INR'
                             amt_doccur = ls_inv-dg_amt1  ) ).

**********************call the BAPI to post acnt documnt**********
    REFRESH : lt_return.
    CLEAR   : lv_objkey,lv_objsys,lv_objtyp.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = l_headers
      IMPORTING
        obj_type          = lv_objtyp
        obj_key           = lv_objkey
        obj_sys           = lv_objsys
      TABLES
        accountgl         = lt_glaccount
        accountreceivable = lt_recievable
        accountpayable    = lt_payable
        currencyamount    = lt_curramnt
        return            = lt_return.
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CLEAR lv_msg_text.
      LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
        lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
      ENDLOOP.
      type    = 'E'.
      message = lv_msg_text.
      ls_inv-remarks = lv_msg_text.
      CLEAR ls_inv-dg_amt1.
    ELSE.
      COMMIT WORK AND WAIT.
      ls_inv-dg_belnr1  = lv_objkey+0(10).
      belnr             = lv_objkey+0(10).
      gjahr             = ls_inv-fisc_year.
      ls_inv-status     = '20'.
      ls_inv-remarks    =  'AUTOCD Generated'.
      type              = 'S'.
      message           = 'AUTOCD Generated'.
**************************update the log table**************
      CLEAR :ls_crlog.
**************************get the number series for SAP id**********
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZCT_MANCD'
        IMPORTING
          number                  = ls_crlog-refid
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      ls_crlog-bukrs   = 'DMS1'.
      ls_crlog-distr   = ls_inv-distributor.
      ls_crlog-kunnr   = ls_inv-dealer.
      ls_crlog-acdno   = ls_inv-ac_doc.
      ls_crlog-acdyr   = ls_inv-fisc_year.
      ls_crlog-dgdoc   = lv_objkey+0(10).
      ls_crlog-dgdyr   = ls_inv-fisc_year.
      ls_crlog-invno   = ls_inv-invoice_no.
      ls_crlog-xblnr   = ls_inv-invoice_no.
      ls_crlog-inamt   = ls_inv-net_amount.
      ls_crlog-indat   = ls_inv-inv_date.
      ls_crlog-paydt   = ls_inv-inv_date.
      ls_crlog-dgamt   = ls_inv-dg_amt1.
      ls_crlog-hkont   = '0042002012'.
      ls_crlog-dgper   = '2.5'.
      ls_crlog-crflag  = 'X'.
      ls_crlog-invflag = 'I'.
      ls_crlog-crdat   = sy-datum.
      ls_crlog-crnam   = sy-uname.
      ls_crlog-msgtyp  = 'S'.
      ls_crlog-message = 'AUTOCD Generated - Cr balance'.
      ls_crlog-gsber   = ls_inv-dist_plant.
      MODIFY zdms_crnote_log FROM ls_crlog.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  DATA : lobj_autocd TYPE REF TO lcl_autocd.
  CREATE OBJECT lobj_autocd.

START-OF-SELECTION.

*************call the actual process**********
  lobj_autocd->fetch( ).

*************display the ALV screen************
  IF lobj_autocd->gt_alv IS NOT INITIAL.

    CALL FUNCTION 'Z_POPUP_ALV'
      TABLES
        it_alv = lobj_autocd->gt_alv.

  ELSE.

    MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.
