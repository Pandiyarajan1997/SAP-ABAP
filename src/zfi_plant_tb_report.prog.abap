*&---------------------------------------------------------------------*
*& Report ZFI_BSEG_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_plant_tb_report.

DATA : g_bukrs TYPE bukrs,
       g_hkont TYPE hkont,
       g_wekrs TYPE werks_d,
       g_gjahr TYPE gjahr,
       g_budat TYPE budat.
DATA: file_str TYPE string.
DATA s_cursor TYPE cursor.
FIELD-SYMBOLS : <it_final> TYPE STANDARD TABLE,
                <wa_final> TYPE any,
                <w_field>  TYPE any.


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS : so_bukrs FOR g_bukrs OBLIGATORY DEFAULT '1000',
                   so_hkont FOR g_hkont,
                   so_budat FOR g_budat,
                   so_werks FOR g_wekrs.

  PARAMETERS: p_gjahr TYPE  gjahr OBLIGATORY.
SELECTION-SCREEN : END OF BLOCK b1.

START-OF-SELECTION.
  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = 'In Progress - Fetch Data from faglflexa'
      i_processed          = 1
      i_total              = 10 ).

  SELECT a~rbukrs AS bukrs,
         a~belnr,
         a~gjahr,
         a~budat,
*         CASE a~budat
*          WHEN '00000000' THEN '20180331'
*          ELSE a~budat
*          END AS budat,
         a~buzei,
         a~activ AS werks,
         a~racct AS hkont,
         b~txt50,
         a~hsl AS dmbtr
   FROM faglflexa AS a
                  INNER JOIN skat AS b ON b~saknr = a~racct
                                      AND b~spras = @sy-langu
                                      AND b~ktopl = 'YAIN'
    INTO TABLE @DATA(lt_bseg)
         WHERE  a~rbukrs  IN @so_bukrs
            AND a~gjahr   = @p_gjahr
            AND a~racct   IN @so_hkont
            AND a~hsl     <> 0.
  LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs>) WHERE budat = '00000000'.
    <fs>-budat+4(4) = '0331'.
    <fs>-budat(4) = <fs>-gjahr + 1.
  ENDLOOP.
  IF so_budat IS NOT INITIAL.
    DELETE lt_bseg WHERE budat NOT IN so_budat.
  ENDIF.

  IF lt_bseg IS NOT INITIAL.
    cl_progress_indicator=>progress_indicate(
      EXPORTING
        i_text               = 'In Progress - Get BSEG Plant '
        i_processed          = 5
        i_total              = 10 ).
    SELECT bukrs, gjahr, belnr, buzei, hkont, werks
      FROM bseg INTO TABLE @DATA(lt_plant)
      FOR ALL ENTRIES IN @lt_bseg
      WHERE bukrs = @lt_bseg-bukrs
        AND belnr = @lt_bseg-belnr
        AND gjahr = @lt_bseg-gjahr
        AND hkont = @lt_bseg-hkont
        AND werks IN @so_werks.
    IF so_werks IS INITIAL.
      DELETE lt_plant WHERE werks EQ ''.
    ENDIF.
    SORT lt_plant BY bukrs belnr hkont werks." shkzg. buzei
    DATA(lt_werks) = lt_plant.
    SORT lt_werks BY werks." shkzg. buzei

    DELETE ADJACENT DUPLICATES FROM lt_werks COMPARING werks.

    IF so_werks IS INITIAL.
      DATA l_werks LIKE LINE OF lt_werks.
      l_werks-werks = 'XXXX'.
      INSERT l_werks INTO lt_werks INDEX 1.
    ELSE.
      READ TABLE lt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>) WITH KEY werks = ''.
      IF sy-subrc = 0.
        READ TABLE so_werks ASSIGNING FIELD-SYMBOL(<lfs_range>) WITH KEY low = ''.
        IF sy-subrc = 0.
          <lfs_range>-low  = 'XXXX'.
        ENDIF.
        <fs_werks>-werks = 'XXXX'.
        LOOP AT lt_plant ASSIGNING FIELD-SYMBOL(<lfs_plant>) WHERE werks = ''.
          <lfs_plant>-werks = 'XXXX'.
        ENDLOOP.
      ENDIF.
    ENDIF.

    DATA it_fieldcatalog TYPE lvc_t_fcat.
    DATA wa_fieldcatalog LIKE LINE OF it_fieldcatalog.

    wa_fieldcatalog-fieldname = 'BUKRS'.
    wa_fieldcatalog-datatype = 'CHAR'.
    wa_fieldcatalog-outputlen = '5'.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    wa_fieldcatalog-fieldname = 'HKONT'.
    wa_fieldcatalog-datatype = 'CHAR'.
    wa_fieldcatalog-outputlen = '10'.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    wa_fieldcatalog-fieldname = 'TXT50'.
    wa_fieldcatalog-datatype = 'CHAR'.
    wa_fieldcatalog-outputlen = '20'.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

    LOOP AT lt_werks INTO DATA(lw_werks).
      wa_fieldcatalog-fieldname = lw_werks-werks.
      wa_fieldcatalog-datatype = 'CURR'.
*    wa_fieldcatalog-outputlen = '7'.
      wa_fieldcatalog-intlen = 23.
      wa_fieldcatalog-decimals = 2.
      APPEND wa_fieldcatalog TO it_fieldcatalog.
    ENDLOOP.

    wa_fieldcatalog-fieldname = 'TOTAL'.
    wa_fieldcatalog-datatype = 'CURR'.
*  wa_fieldcatalog-outputlen = '7'.
    wa_fieldcatalog-intlen = 23.
    wa_fieldcatalog-decimals = 2.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

*******create DYNAMIC table***********************
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = DATA(new_table)
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

    ASSIGN new_table->* TO <it_final>.

********CREATE WORK AREA***************************
    DATA  new_line TYPE REF TO data.
    CREATE DATA new_line LIKE LINE OF <it_final>.
    ASSIGN new_line->* TO <wa_final>.

    DESCRIBE TABLE lt_bseg LINES DATA(l_lines).
    cl_progress_indicator=>progress_indicate(
      EXPORTING
        i_text               = 'In Progress - Preparing Report'
        i_processed          = 7
        i_total              = 10 ).

    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs1>).
      READ TABLE lt_plant ASSIGNING FIELD-SYMBOL(<fs_plant>)
        WITH KEY bukrs = <fs1>-bukrs
                 gjahr = <fs1>-gjahr
                 belnr = <fs1>-belnr
                 hkont = <fs1>-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        <fs1>-werks = <fs_plant>-werks.
      ELSE.
        IF so_werks IS NOT INITIAL.
          CONTINUE.
        ELSE.
          <fs1>-werks = 'XXXX'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF so_werks IS NOT INITIAL.
      SORT lt_bseg BY bukrs werks.
      DELETE lt_bseg WHERE werks NOT IN so_werks.
    ENDIF.
    DATA(l_tfname) = 'HKONT'.
    DATA(l_tfname1) = 'BUKRS'.

    SORT lt_bseg BY bukrs hkont werks.
*  DATA(l_plant) = lt_bseg[ 1 ]-werks.
    DATA(l_glacc) = lt_bseg[ 1 ]-hkont.
    DATA(l_data) = lt_bseg[ 1 ].
    CLEAR <wa_final>.
    MOVE-CORRESPONDING l_data TO <wa_final>.
    ASSIGN COMPONENT l_data-werks OF STRUCTURE <wa_final> TO FIELD-SYMBOL(<fvaluenew>).
    IF <fvaluenew> IS ASSIGNED.
      <fvaluenew> = <fvaluenew> + l_data-dmbtr.
      ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <wa_final> TO FIELD-SYMBOL(<ftvaluenew>).
      IF <ftvaluenew> IS ASSIGNED.
        <ftvaluenew> = <ftvaluenew> + l_data-dmbtr.
      ENDIF.
    ENDIF.
    DELETE lt_bseg INDEX 1.
    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs_h>).
      IF l_glacc <> <fs_h>-hkont.
        APPEND <wa_final> TO <it_final>.
        l_glacc = <fs_h>-hkont.
        CLEAR <wa_final>.
        MOVE-CORRESPONDING <fs_h> TO <wa_final>.
      ENDIF.
      ASSIGN COMPONENT <fs_h>-werks OF STRUCTURE <wa_final> TO <fvaluenew>.
      IF <fvaluenew> IS ASSIGNED.
        <fvaluenew> = <fvaluenew> + <fs_h>-dmbtr.
        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <wa_final> TO <ftvaluenew>.
        IF <ftvaluenew> IS ASSIGNED.
          <ftvaluenew> = <ftvaluenew> + <fs_h>-dmbtr.
        ENDIF.
      ENDIF.
*    READ TABLE <it_final> ASSIGNING FIELD-SYMBOL(<fs_final>)
*                          WITH KEY  (l_tfname1) = <fs_h>-bukrs
*                                    (l_tfname)  = <fs_h>-hkont BINARY SEARCH.
*    IF sy-subrc = 0.
*      ASSIGN COMPONENT <fs_h>-werks OF STRUCTURE <fs_final> TO FIELD-SYMBOL(<fvalue>).
*      IF <fvalue> IS ASSIGNED.
*        <fvalue> = <fvalue> + <fs_h>-dmbtr.
*        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <fs_final> TO FIELD-SYMBOL(<ftvalue>).
*        IF <ftvalue> IS ASSIGNED.
*          <ftvalue> = <ftvalue> + <fs_h>-dmbtr.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      CLEAR <wa_final>.
*      MOVE-CORRESPONDING <fs_h> TO <wa_final>.
*      ASSIGN COMPONENT <fs_h>-werks OF STRUCTURE <wa_final> TO FIELD-SYMBOL(<fvaluenew>).
*      IF <fvaluenew> IS ASSIGNED.
*        <fvaluenew> = <fvaluenew> + <fs_h>-dmbtr.
*        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <wa_final> TO FIELD-SYMBOL(<ftvaluenew>).
*        IF <ftvaluenew> IS ASSIGNED.
*          <ftvaluenew> = <ftvaluenew> + <fs_h>-dmbtr.
*        ENDIF.
*        APPEND <wa_final> TO <it_final>.
*      ENDIF.
*    ENDIF.
    ENDLOOP.
*    IF sy-tabix = l_lines.
    APPEND <wa_final> TO <it_final>.
*    ENDIF.
*  DATA(lv_string) = 'TOTAL = 0'.
*  DELETE <it_final> WHERE (lv_string).

    DATA: gr_alv TYPE REF TO cl_salv_table.
    DATA: lr_columns TYPE REF TO cl_salv_columns_table.

    CALL METHOD cl_salv_table=>factory
      EXPORTING
        list_display = if_salv_c_bool_sap=>false
      IMPORTING
        r_salv_table = gr_alv
      CHANGING
        t_table      = <it_final>. "lt_final.

    DATA functions TYPE REF TO cl_salv_functions_list.

    functions = gr_alv->get_functions( ).
    functions->set_all( ).

    lr_columns = gr_alv->get_columns( ).
    DATA not_found TYPE REF TO cx_salv_not_found.
    TRY.
        DATA(lr_column) = lr_columns->get_column( 'BUKRS' ).
        lr_column->set_medium_text( 'Company Code' ).
        lr_column->set_long_text( 'Company Code' ).

        lr_column = lr_columns->get_column( 'HKONT' ).
        lr_column->set_medium_text( 'G/L Account' ).
        lr_column->set_long_text( 'G/L Account' ).

        lr_column = lr_columns->get_column( 'TXT50' ).
        lr_column->set_medium_text( 'G/L Account Text' ).
        lr_column->set_long_text( 'G/L Account Text' ).
        DATA l_fname TYPE lvc_fname.
        DATA l_stext TYPE scrtext_s.
*      IF so_werks IS  INITIAL.
*        lr_column = lr_columns->get_column( 'XXXX' ).
*        lr_column->set_medium_text( 'Plant_XXXX' ).
*        lr_column->set_long_text( 'Plant_XXXX' ).
*      ENDIF.
        LOOP AT lt_werks INTO lw_werks.
          l_stext = l_fname = lw_werks-werks.
          l_stext = |Plant_{ l_stext }|.
          lr_column = lr_columns->get_column( l_fname ).
          lr_column->set_short_text( l_stext ).
        ENDLOOP.
        lr_column = lr_columns->get_column( 'TOTAL' ).
        lr_column->set_short_text( 'Total' ).
        lr_column->set_medium_text( 'Grand Total' ).
        lr_column->set_long_text( 'Grand Total' ).
      CATCH cx_salv_not_found INTO not_found.
        " error handling
    ENDTRY.
* display ALV
    gr_alv->display( ).
  ELSE.
    MESSAGE 'No Data found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
