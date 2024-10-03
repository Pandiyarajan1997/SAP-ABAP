*&---------------------------------------------------------------------*
*& Report ZDMS_BAREA_CHANGE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_barea_change.

TYPES : BEGIN OF ty_excel,
          bukrs TYPE bukrs,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
          gsber TYPE gsber,
          type  TYPE bapi_mtype,
          msg   TYPE string,
        END OF ty_excel.

DATA : lt_excel TYPE TABLE OF ty_excel,
       lt_raw   TYPE truxs_t_text_data,
       gv_kunnr TYPE kunnr,
       lv_bktxt TYPE bktxt.

PARAMETERS : p_fname TYPE rlgrap-filename MODIF ID a.     "for excel file upload
SELECT-OPTIONS : so_dist FOR gv_kunnr MODIF ID a.

PARAMETERS : p_bseg  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,
             p_acdoc RADIOBUTTON GROUP g1,
             p3      RADIOBUTTON GROUP g1.   "header text update for Cr/DR note
*             p_fagl  RADIOBUTTON GROUP g1.

"f4 functionality to file path

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.

START-OF-SELECTION.

**************update the number series for the DG & DR documents*************
  IF p3 = abap_true.

    PERFORM headertxt_noseries.

  ELSE.
****************excel to it**************
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
      EXPORTING
        i_tab_raw_data       = lt_raw
        i_filename           = p_fname
        i_line_header        = abap_true
      TABLES
        i_tab_converted_data = lt_excel.

*************update process***************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
      <fs_excel>-belnr = |{ <fs_excel>-belnr ALPHA = IN }|.

      IF <fs_excel>-gsber IS INITIAL.
        <fs_excel>-type = 'E'.
        <fs_excel>-msg  = 'Business area missing'.
        CONTINUE.
      ENDIF.

      IF p_bseg = abap_true.

        SELECT SINGLE bukrs,belnr,gjahr,gsber FROM bseg INTO @DATA(ls_bseg)
                                              WHERE bukrs = @<fs_excel>-bukrs
                                              AND   belnr = @<fs_excel>-belnr
                                              AND   gjahr = @<fs_excel>-gjahr.
        IF sy-subrc = 0.
***************update the business area - BSEG***************
          UPDATE bseg SET gsber = <fs_excel>-gsber WHERE bukrs = <fs_excel>-bukrs
                                                   AND   belnr = <fs_excel>-belnr
                                                   AND   gjahr = <fs_excel>-gjahr
                                                   AND   gsber NE abap_false.
          IF sy-subrc = 0.
            <fs_excel>-type = 'S'.
            <fs_excel>-msg  = 'Business Area Updated'.
            COMMIT WORK.
          ENDIF.
        ELSE.
          <fs_excel>-type = 'E'.
          <fs_excel>-msg  = 'Incorrect details'.
        ENDIF.

      ELSEIF p_acdoc = abap_true.
        SELECT SINGLE rbukrs,belnr,gjahr,rbusa FROM acdoca INTO @DATA(ls_acdoca)
                                               WHERE rldnr  = '0L'
                                               AND   rbukrs = @<fs_excel>-bukrs
                                               AND   gjahr  = @<fs_excel>-gjahr
                                               AND   belnr  = @<fs_excel>-belnr.

        IF sy-subrc = 0.
***************update the business area - acdoca***************
          UPDATE acdoca SET rbusa = <fs_excel>-gsber WHERE rldnr  = '0L'
                                                     AND   rbukrs = <fs_excel>-bukrs
                                                     AND   gjahr  = <fs_excel>-gjahr
                                                     AND   belnr  = <fs_excel>-belnr
                                                     AND   rbusa  NE abap_false.
          IF sy-subrc = 0.
            <fs_excel>-type = 'S'.
            <fs_excel>-msg  = 'Business Area Updated'.
            COMMIT WORK.
          ENDIF.
        ELSE.
          <fs_excel>-type = 'E'.
          <fs_excel>-msg  = 'Incorrect details'.
        ENDIF.

*  ELSEIF p_fagl = abap_true.
*    SELECT SINGLE rbukrs,docnr,ryear,rbusa FROM faglflexa INTO @DATA(ls_faglflexa)
*                                           WHERE ryear  = @p_gjahr
*                                           AND   docnr  = @p_belnr
*                                           AND   rldnr  = '0L'
*                                           AND   rbukrs = @p_bukrs.
*    IF sy-subrc = 0.
****************update the business area - faglflexa***************
*      UPDATE faglflexa SET rbusa = p_gsber WHERE ryear  = p_gjahr
*                                           AND   docnr  = p_belnr
*                                           AND   rldnr  = '0L'
*                                           AND   rbukrs = p_bukrs
*                                           AND   rbusa  NE abap_false.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ENDIF.
*    ELSE.
*      MESSAGE : 'Incorrect details' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
      ENDIF.
    ENDLOOP.

*************display the data***************
    CALL FUNCTION 'Z_POPUP_ALV'
      TABLES
        it_alv = lt_excel.

  ENDIF.

FORM headertxt_noseries.

  TYPES : BEGIN OF ty_alv,
            distributor TYPE zdist,
            gsber       TYPE gsber,
            blart       TYPE blart,
            belnr       TYPE belnr_d,
            gjahr       TYPE gjahr,
            bktxt       TYPE bktxt,
            type        TYPE bapi_mtype,
            msg         TYPE zremarks,
          END OF ty_alv.

  DATA : lt_alv   TYPE TABLE OF ty_alv,
         lv_msg   TYPE zremarks,
         lv_type  TYPE bapi_mtype,
         lv_blart TYPE blart.

  SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1) WHERE kunnr IN @so_dist
                                                         AND   werks <> @abap_false.
  IF sy-subrc = 0.
    SORT lt_kna1 BY werks.
***************fetch only dms distributor***************
    SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k)
                                   FOR ALL ENTRIES IN @lt_kna1
                                   WHERE bukrs = 'DMS1'
                                   AND   bwkey = @lt_kna1-werks.
    IF sy-subrc = 0.
      SELECT rbukrs,belnr,gjahr,rbusa,budat,blart FROM acdoca
                                            INTO TABLE @DATA(lt_acdoca)
                                            FOR ALL ENTRIES IN @lt_t001k
                                            WHERE rldnr  = '0L'
                                            AND   rbukrs = 'DMS1'
                                            AND   docln  = '000001'
                                            AND   blart  IN ( 'DR' , 'DG' )
                                            AND   rbusa  = @lt_t001k-bwkey
                                            AND   budat  GE '20240401'.
    ENDIF.
  ENDIF.

  IF sy-subrc = 0.
    SORT : lt_acdoca BY rbusa budat belnr.
  ENDIF.

**********************Excess payment removal process*************
  SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete).
  IF sy-subrc = 0.
    SORT lt_delete BY belnr gjahr.
  ENDIF.

  LOOP AT lt_acdoca INTO DATA(ls_acdoca).

**************bypass the exception cases - reversed**********
    READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = ls_acdoca-belnr
                                                         gjahr = ls_acdoca-gjahr BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY werks = ls_acdoca-rbusa BINARY SEARCH.
      lv_type = 'E'.
      lv_msg  = 'Reversed document'.
    ELSE.

*****************read the distributor code**************
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY werks = ls_acdoca-rbusa BINARY SEARCH.
      IF sy-subrc = 0.
        SELECT SINGLE belnr,bktxt FROM bkpf INTO @DATA(ls_bkpf)
                                      WHERE bukrs = @ls_acdoca-rbukrs
                                      AND   belnr = @ls_acdoca-belnr
                                      AND   gjahr = @ls_acdoca-gjahr.
        IF sy-subrc = 0. "AND ls_bkpf-bktxt IS INITIAL.

****************check the document type************
          IF ls_acdoca-blart = 'DG'.
            lv_blart = 'CN'.
          ELSEIF ls_acdoca-blart = 'DR'.
            lv_blart = 'DN'.
          ENDIF.
*********************get the number range*******************
          SELECT SINGLE * FROM zfi_dms_no_serie INTO @DATA(ls_series)
                                                WHERE distributor = @ls_kna1-kunnr
                                                AND   plant       = @ls_acdoca-rbusa
                                                AND   gjahr       = @ls_acdoca-gjahr
                                                AND   doc_type    = @lv_blart.
          IF sy-subrc <> 0.
            ls_series-distributor = ls_kna1-kunnr.
            ls_series-gjahr       = ls_acdoca-gjahr.
            ls_series-mandt       = sy-mandt.
            ls_series-plant       = ls_acdoca-rbusa.
            ls_series-doc_type    = lv_blart.
          ENDIF.
          ls_series-num_range   = ls_series-num_range + 1.
************number series**************
          lv_bktxt = |{ ls_series-plant }{ ls_series-gjahr }{ ls_series-doc_type }{ ls_series-num_range }|.
******************update the new number series in bkfp header text field****************
          UPDATE bkpf SET bktxt = lv_bktxt WHERE bukrs = ls_acdoca-rbukrs
                                           AND   belnr = ls_acdoca-belnr
                                           AND   gjahr = ls_acdoca-gjahr.
          IF sy-subrc = 0.
******************update the next number range***********
            MODIFY zfi_dms_no_serie FROM ls_series.
            COMMIT WORK.
            lv_type = 'S'.
            lv_msg  = 'Number series updated'.
          ENDIF.
        ELSE.
          lv_type  = 'E'.
          lv_msg   = 'Already updated'.
          lv_bktxt = ls_bkpf-bktxt.
        ENDIF.
      ELSE.
        lv_type = 'E'.
        lv_msg  = 'Plant not maintained'.
      ENDIF.
    ENDIF.
**************append for ALV screen**********
    APPEND VALUE #( distributor = ls_kna1-kunnr
                    gsber       = ls_acdoca-rbusa
                    blart       = ls_acdoca-blart
                    belnr       = ls_acdoca-belnr
                    gjahr       = ls_acdoca-gjahr
                    bktxt       = lv_bktxt
                    type        = lv_type
                    msg         = lv_msg ) TO lt_alv.

    CLEAR : ls_acdoca,ls_kna1,ls_series,ls_bkpf,lv_bktxt,lv_msg,lv_type,lv_blart.

  ENDLOOP.

  IF lt_alv IS NOT INITIAL.
*************display the data***************
    CALL FUNCTION 'Z_POPUP_ALV'
      TABLES
        it_alv = lt_alv.
  ENDIF.

ENDFORM.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p3 = abap_true AND screen-name CS 'P_FNAME'.
      screen-active = 0.
    ELSEIF ( p_bseg  = abap_true OR p_acdoc = abap_true ) AND screen-name CS 'SO_DIST'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
