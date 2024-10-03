*&---------------------------------------------------------------------*
*& Report ZFI_BSEG_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_bseg_download.

DATA : g_bukrs TYPE bukrs,
       g_hkont TYPE hkont,
       g_gjahr TYPE gjahr.
DATA: file_str TYPE string.
DATA s_cursor TYPE cursor.
*DATA : BEGIN OF itab1 OCCURS 0,
*         line(50) TYPE c,
*       END OF itab1.



SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS : so_bukrs FOR g_bukrs OBLIGATORY DEFAULT '1000',
                   so_hkont FOR g_hkont.

  PARAMETERS: p_gjahr TYPE  gjahr OBLIGATORY.
*              p_file  TYPE localfile DEFAULT 'C:\Users\Temp\Desktop\bseg'.
SELECTION-SCREEN : END OF BLOCK b1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
*    EXPORTING
*      static    = 'X'
*    CHANGING
*      file_name = p_file.

START-OF-SELECTION.
*  file_str = p_file.
*  CONSTANTS lc_werks TYPE werks_d VALUE 'XXXX'.

  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = 'In Progress - Fetch Data from faglflexa'
      i_processed          = 1
      i_total              = 10 ).

  SELECT a~rbukrs AS bukrs,
         a~belnr,
         a~gjahr,
         a~buzei,
         a~activ AS werks,
*         CASE WHEN c~werks EQ @space THEN @lc_werks
*         ELSE c~werks
*         END AS werks,
*         c~h_blart,
         a~racct AS hkont,
         b~txt50,
         a~hsl AS dmbtr
   FROM faglflexa  AS a
                  INNER JOIN skat AS b ON b~saknr = a~racct
                                      AND b~spras = @sy-langu
                                      AND b~ktopl = 'YAIN'
    INTO TABLE @DATA(lt_bseg)
         WHERE  a~rbukrs  IN @so_bukrs
            AND a~gjahr   EQ @p_gjahr
            AND a~racct   IN @so_hkont "NE @space.
            AND a~hsl     NE 0 .


  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = 'In Progress - Get BSEG Plant '
      i_processed          = 5
      i_total              = 10 ).

  SORT lt_bseg BY bukrs belnr hkont." shkzg.
  SELECT bukrs, gjahr, belnr, buzei, hkont, werks
    FROM bseg INTO TABLE @DATA(lt_plant)
    FOR ALL ENTRIES IN @lt_bseg
    WHERE bukrs = @lt_bseg-bukrs
      AND belnr = @lt_bseg-belnr
      AND gjahr = @lt_bseg-gjahr
*      AND buzei = @lt_bseg-buzei
      AND hkont = @lt_bseg-hkont
      AND werks NE @space.

  DESCRIBE TABLE lt_bseg LINES DATA(l_lines).
  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = 'In Progress - Preparing Report'
      i_processed          = 7
      i_total              = 10 ).

  SORT lt_plant BY bukrs belnr hkont." shkzg. buzei
  DATA(lt_werks) = lt_plant.
  SORT lt_werks BY werks." shkzg. buzei
  DELETE ADJACENT DUPLICATES FROM lt_werks COMPARING werks.

  LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs1>).
    READ TABLE lt_plant ASSIGNING FIELD-SYMBOL(<fs_plant>)
      WITH KEY bukrs = <fs1>-bukrs
               gjahr = <fs1>-gjahr
               belnr = <fs1>-belnr
               hkont = <fs1>-hkont BINARY SEARCH.
    IF sy-subrc = 0.
      <fs1>-werks = <fs_plant>-werks.
    ELSE.
      <fs1>-werks = 'XXXX'.
    ENDIF.
  ENDLOOP.

  SORT lt_bseg BY bukrs werks hkont." shkzg.
  DATA lt_final LIKE lt_bseg.
  DATA(l_plant) = lt_bseg[ 1 ]-werks.
  DATA(l_glacc) = lt_bseg[ 1 ]-hkont.
  DATA(lw_data) = lt_bseg[ 1 ].

  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = 'In Progress - Report'
      i_processed          = 8
      i_total              = 10 ).

  LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs_h>)." WHERE shkzg = 'H' AND flag = ''.
    IF sy-tabix = 1.
      CONTINUE.
    ENDIF.
    IF l_plant <> <fs_h>-werks.
      APPEND l_plant TO lt_werks.
*      lw_data-shkzg = ''.
      APPEND lw_data TO lt_final.
*      CLEAR lw_data.
      lw_data = <fs_h>.
      l_plant = <fs_h>-werks.
      l_glacc = <fs_h>-hkont.
      IF sy-tabix = l_lines.
*        lw_data-shkzg = ''.
        APPEND lw_data TO lt_final.
      ENDIF.
    ELSE.
      IF l_glacc <> <fs_h>-hkont.
*        lw_data-shkzg = ''.
        APPEND lw_data TO lt_final.
*        CLEAR lw_data.
        lw_data = <fs_h>.
        l_glacc = <fs_h>-hkont.
        IF sy-tabix = l_lines.
*          lw_data-shkzg = ''.
          APPEND lw_data TO lt_final.
        ENDIF.
      ELSE.
        lw_data-dmbtr = lw_data-dmbtr + <fs_h>-dmbtr.
        IF sy-tabix = l_lines.
*          lw_data-shkzg = ''.
          APPEND lw_data TO lt_final.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


  DELETE lt_final WHERE dmbtr = 0.

  DATA: gr_alv TYPE REF TO cl_salv_table.
  DATA: lr_columns TYPE REF TO cl_salv_columns_table.

  CALL METHOD cl_salv_table=>factory
    EXPORTING
      list_display = if_salv_c_bool_sap=>false
    IMPORTING
      r_salv_table = gr_alv
    CHANGING
      t_table      =  lt_final.

  DATA functions TYPE REF TO cl_salv_functions_list.

  functions = gr_alv->get_functions( ).
  functions->set_all( ).

  lr_columns = gr_alv->get_columns( ).
  DATA not_found TYPE REF TO cx_salv_not_found.
  TRY.
      DATA(lr_column) = lr_columns->get_column( 'WERKS' ).
      lr_column->set_short_text( 'Plant' ).
      lr_column->set_medium_text( 'Plant' ).
      lr_column->set_long_text( 'Plant' ).

*        lr_column = lr_columns->get_column( 'SHKZG' ).
*        lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'BELNR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'BUZEI' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'GJAHR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

* display ALV
  gr_alv->display( ).
*  OPEN CURSOR WITH HOLD  s_cursor FOR
*  SELECT bukrs
*         belnr
**         buzei
*         gjahr
**         shkzg
**         gsber
*         dmbtr
**         pswsl
*         mwskz
*         zuonr
*         sgtxt
*         kostl
**         aufnr
*         hkont
**         bschl
**         augdt
**         augbl
*         werks
**         fipos
**         lifnr
**         kunnr
**         matnr
**         menge
**         meins
**         anln1
**         anln2
**         bpmng
**         bprme
**         ebeln
**         ebelp
*   FROM bseg WHERE bukrs IN  so_bukrs AND gjahr IN  so_gjahr AND hkont NE space.
*  DO.
*    FETCH NEXT CURSOR  s_cursor APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg PACKAGE SIZE 100000.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*  ENDDO.
*  CLOSE CURSOR  s_cursor.

*    itab1-line  =  'Company Code'.
*    APPEND itab1.
*    itab1-line = 'Plant'."'WERKS'.
*    APPEND itab1.
*    itab1-line  =  'Fiscal Year'."'GJAHR'.
*    APPEND itab1.
*    itab1-line = 'G/L Acct'."'HKONT'.
*    APPEND itab1.
*    itab1-line = 'Acc.Doc No'."'BELNR'.
*    APPEND itab1.
*    itab1-line = 'Tax Code'."'MWSKZ'.
*    APPEND itab1.
*    itab1-line  =  'Assignment'."'ZUONR'.
*    APPEND itab1.
*    itab1-line = 'Text'."'SGTXT'.
*    APPEND itab1.
*    itab1-line = 'Cost Center'."'KOSTL'.
*    APPEND itab1.
*    itab1-line = 'Debit/Credit'."'SHKZG'.
*    APPEND itab1.
*    itab1-line  =  'Amount in LC'."'DMBTR'.
*    APPEND itab1.
*
*    file_str = |{ file_str }.txt|.
*
*    CALL FUNCTION 'GUI_DOWNLOAD'
*      EXPORTING
*        filename                = file_str
*        write_field_separator   = cl_abap_char_utilities=>horizontal_tab
**       filetype                = 'txt'
*      TABLES
*        data_tab                = lt_bseg
*        fieldnames              = itab1
*      EXCEPTIONS
*        file_write_error        = 1
*        no_batch                = 2
*        gui_refuse_filetransfer = 3
*        invalid_type            = 4
*        no_authority            = 5
*        unknown_error           = 6
*        header_not_allowed      = 7
*        separator_not_allowed   = 8
*        filesize_not_allowed    = 9
*        header_too_long         = 10
*        dp_error_create         = 11
*        dp_error_send           = 12
*        dp_error_write          = 13
*        unknown_dp_error        = 14
*        access_denied           = 15
*        dp_out_of_memory        = 16
*        disk_full               = 17
*        dp_timeout              = 18
*        file_not_found          = 19
*        dataprovider_exception  = 20
*        control_flush_error     = 21
*        OTHERS                  = 22.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
