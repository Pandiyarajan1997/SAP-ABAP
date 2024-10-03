FUNCTION y_sample_interface_00001650.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"  EXPORTING
*"     VALUE(E_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"----------------------------------------------------------------------
**"----------------------------------------------------------------------
**"*"LOCAL INTERFACE:
**"  IMPORTING
**"     VALUE(I_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
**"  EXPORTING
**"     VALUE(E_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
**"----------------------------------------------------------------------
*
** CORRECTED FOR ENTRY SHEET LONG TEXT - 29 DEC 2017


  TYPES: BEGIN OF ty_bseg,
           mandt    TYPE bseg-mandt,
           bukrs    TYPE bseg-bukrs,
           belnr    TYPE bseg-belnr,
           gjahr    TYPE bseg-gjahr,
           matnr    TYPE bseg-matnr,
           ebeln    TYPE bseg-ebeln,
           fipos    TYPE bseg-fipos,
           fmxdocnr TYPE bseg-fmxdocnr,
         END OF ty_bseg.

  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           spras TYPE makt-spras,
           maktx TYPE makt-maktx,
         END OF ty_makt,

         BEGIN OF ty_essr,
           lblni TYPE essr-lblni,
           ebeln TYPE essr-ebeln,
         END OF ty_essr.


  e_postab = i_postab.

  CONSTANTS: c_tdid     LIKE thead-tdid  VALUE 'TX01',          " TEXTO DE POSICION
             c_tdobject LIKE thead-tdobject VALUE 'ESSR'.  " OBJETO DE TEXTO OC
  DATA:   c_name LIKE thead-tdname.

  DATA: gs_name1 TYPE name1_gp,
        gt_bseg  TYPE TABLE OF ty_bseg,
        gs_bseg  TYPE ty_bseg,
        gt_makt  TYPE TABLE OF ty_makt,
        gs_makt  TYPE ty_makt,
        gt_essr  TYPE TABLE OF ty_essr,
        gs_essr  TYPE ty_essr,
        t_lines  TYPE TABLE OF tline,
        wa_lines TYPE tline,
        lv_nol   TYPE i,
        lv_long  TYPE string.

  SELECT SINGLE name1
                FROM kna1
                INTO gs_name1
                WHERE kunnr EQ i_postab-kunnr.

  e_postab-kunnr = i_postab-kunnr.
  e_postab-name1 = gs_name1.

  IF sy-tcode EQ 'FBL3N'.

    SELECT mandt "#EC CI_DB_OPERATION_OK[2431747] " ADDED BY <IT-CAR TOOL> DURING CODE REMEDIATION
           bukrs
           belnr
           gjahr
           matnr
           ebeln
           fipos
           fmxdocnr
           FROM bseg CLIENT SPECIFIED
           INTO TABLE gt_bseg
           WHERE mandt EQ sy-mandt AND
           bukrs EQ i_postab-bukrs AND
           belnr EQ i_postab-belnr AND
           gjahr EQ i_postab-gjahr ORDER BY PRIMARY KEY.  " ADDED BY <IT-CAR TOOL> DURING CODE REMEDIATION

*    IF SY-SUBRC EQ 0.
*
*      SELECT MATNR
*             SPRAS
*             MAKTX
*             FROM MAKT
*             INTO TABLE GT_MAKT
*             FOR ALL ENTRIES IN GT_BSEG
*             WHERE SPRAS EQ SY-LANGU AND
*             MATNR EQ GT_BSEG-MATNR.
*    ENDIF.
*
*
*    IF GT_MAKT IS NOT INITIAL.
*
*      LOOP AT GT_MAKT INTO GS_MAKT.
*
*        E_POSTAB-MAKTX = GS_MAKT-MAKTX. " THE APPEND STRUCTURE TO ADD MAKTX IN RFPOS STRUCTURE IS STILL IN DEV . NOT MOVED TO PRD
*
*      ENDLOOP.
*
*
*    ENDIF.

  ENDIF.


*  SELECT LBLNI
*         EBELN
*         FROM ESSR
*         INTO TABLE GT_ESSR
*         WHERE EBELN EQ I_POSTAB-EBELN.

  IF gt_bseg IS NOT INITIAL.

*     CONVERSION_EXIT_ALPHA_INPUT
*      CONVERSION_EXIT_ALPHA_OUTPUT

    "ADDED BY SAMSUDEEN ON 10.04.2023
    DATA(gt_bseg_tmp) = gt_bseg[].
    DELETE ADJACENT DUPLICATES FROM gt_bseg_tmp COMPARING bukrs belnr gjahr fmxdocnr.
    DATA(lv_docno) = VALUE #( gt_bseg_tmp[ 1 ]-fmxdocnr OPTIONAL ).
    IF lv_docno IS NOT INITIAL.
      c_name = lv_docno.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = c_tdid
          language                = sy-langu
          name                    = c_name
          object                  = c_tdobject
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*     IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = t_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        DESCRIBE TABLE t_lines LINES lv_nol.
        IF lv_nol EQ 1.
          e_postab-zgltext = VALUE #( t_lines[ 1 ]-tdline OPTIONAL ).
        ELSE.
          LOOP AT t_lines INTO DATA(l_lines).
            DATA(l_text) = |{ l_lines-tdline }|.
            l_text = |{ l_text }{ l_lines-tdline }|.
          ENDLOOP.
          e_postab-zgltext = l_text.
        ENDIF.
      ENDIF.
    ENDIF.
    "End of Changes on 10.04.2023

*    LOOP AT gt_bseg INTO gs_bseg.
*
*      IF gs_bseg-fmxdocnr IS NOT INITIAL.
*
*
*
*        DESCRIBE TABLE t_lines LINES lv_nol.
*        IF lv_nol EQ 1.
*
*          LOOP AT t_lines INTO wa_lines.
*
*            e_postab-zgltext = wa_lines-tdline.
*
*            CLEAR:wa_lines.
*
*          ENDLOOP.
*
*        ELSE.
*
*          LOOP AT t_lines INTO wa_lines.
*
*            IF sy-tabix EQ 1.
*
*              lv_long = wa_lines-tdline.
*
*            ELSE.
*
*              CONCATENATE lv_long wa_lines-tdline INTO e_postab-zgltext SEPARATED BY space.
*
*            ENDIF.
*
*
*
**          E_POSTAB-ZGLTEXT = WA_LINES-TDLINE.
*
*            CLEAR:wa_lines.
*
*          ENDLOOP.
*        ENDIF.
*        CLEAR: gs_bseg.
*
*      ENDIF.
*    ENDLOOP.

  ENDIF.

ENDFUNCTION.
