*&---------------------------------------------------------------------*
*& Report ZMM_mapl_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_mapl_upd.
TYPES: BEGIN OF ty_material,
         matnr     TYPE mara-matnr,
         old_matnr TYPE  mara-matnr,
       END OF ty_material.
DATA : it_mara TYPE TABLE OF ty_material,
       wa_mara TYPE ty_material.
DATA: lt_excel TYPE TABLE OF alsmex_tabline,
      ls_excel TYPE alsmex_tabline.

PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.


START-OF-SELECTION.
  IF p_file IS INITIAL.
    MESSAGE 'Please select a file' TYPE 'E'.
    EXIT.
  ENDIF.

START-OF-SELECTION.
  IF p_file IS INITIAL.
    MESSAGE 'Please select a file' TYPE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 14
      i_end_row               = 1000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Error reading Excel file' TYPE 'E'.
  ENDIF.

**   Process Excel data into lt_materials
  LOOP AT lt_excel INTO ls_excel.
    CASE ls_excel-col.
      WHEN 1. wa_mara-matnr = ls_excel-value.
      WHEN 2.
        wa_mara-old_matnr = ls_excel-value.
        APPEND wa_mara TO it_mara.
        CLEAR wa_mara.
    ENDCASE.
  ENDLOOP.


  SORT it_mara BY matnr ASCENDING.

  SELECT * FROM mara INTO TABLE @DATA(it_new_mara_tab)
 FOR ALL ENTRIES IN @it_mara
 WHERE matnr = @it_mara-matnr.
  SORT it_new_mara_tab BY matnr ASCENDING.

  SELECT * FROM mapl INTO TABLE @DATA(it_mapl_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-old_matnr.
  SORT it_mapl_tab BY matnr ASCENDING.

  SELECT * FROM mapl INTO TABLE @DATA(it_new_mapl_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-matnr.
  SORT it_mapl_tab BY matnr ASCENDING.

  IF it_mapl_tab IS NOT INITIAL.
    DATA lv_count TYPE i.
    LOOP AT it_mapl_tab INTO DATA(wa_mapl_tab).
      lv_count = lv_count + 1 .
      DATA(lv_index) = line_index( it_mara[ old_matnr = wa_mapl_tab-matnr ] ).
      IF lv_index > 0.
        wa_mara = it_mara[ lv_index ].
        DATA(lv_index1) = line_index( it_new_mara_tab[ matnr = wa_mara-matnr ] ).
        IF lv_index1 > 0 .
          wa_mapl_tab-matnr = wa_mara-matnr.
          DATA(lv_index2) = line_index( it_new_mapl_tab[
          matnr = wa_mapl_tab-matnr
          werks = wa_mapl_tab-werks
          plnty = wa_mapl_tab-plnty
          plnnr = wa_mapl_tab-plnnr
          plnal = wa_mapl_tab-plnal
          zkriz = wa_mapl_tab-zkriz
          zaehl = wa_mapl_tab-zaehl
          datuv = wa_mapl_tab-datuv
          techv = wa_mapl_tab-techv
          aennr = wa_mapl_tab-aennr
          loekz = wa_mapl_tab-loekz
          parkz = wa_mapl_tab-parkz
          andat = wa_mapl_tab-andat
          annam = wa_mapl_tab-annam
          aedat = wa_mapl_tab-aedat
          aenam = wa_mapl_tab-aenam
          lifnr = wa_mapl_tab-lifnr
          kunr = wa_mapl_tab-kunr
          suchfeld = wa_mapl_tab-suchfeld
          vbeln = wa_mapl_tab-vbeln
          posnr = wa_mapl_tab-posnr
          pspnr = wa_mapl_tab-pspnr
          valid_to = wa_mapl_tab-valid_to
          loekz_inherited = wa_mapl_tab-loekz_inherited
          versn = wa_mapl_tab-versn
          versn_source = wa_mapl_tab-versn_source
          versn_source_zkriz = wa_mapl_tab-versn_source_zkriz
          ms_object = wa_mapl_tab-ms_object
          ms_objtype = wa_mapl_tab-ms_objtype

           ] ).

          IF lv_index2 < 1.

            INSERT  mapl FROM  wa_mapl_tab.
            IF sy-subrc = 0.
              WRITE : / lv_count , '  . ',  wa_mara-matnr ,  ' Updated Successfully'.
            ELSE.
              WRITE : / lv_count , '  . ',  wa_mara-matnr ,  ' Not Updated'.
            ENDIF.
          ELSE.
            WRITE : / lv_count , '  . ',  wa_mara-matnr ,  ' Already Updated'.
          ENDIF.
        ELSE.
          WRITE : / lv_count , '  . ',  wa_mara-matnr ,  ' Material Not Found'.
        ENDIF.

      ELSE.
        WRITE : / lv_count , '  . ',  wa_mara-matnr ,  ' Not Found'.
      ENDIF.
    ENDLOOP.

  ENDIF.
