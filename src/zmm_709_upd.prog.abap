*&---------------------------------------------------------------------*
*& Report ZMM_709_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_709_upd.

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

  "*********************************************************************************************************
  " A709 table Update Start from Here.

  SELECT * FROM A709 INTO TABLE @DATA(it_A709_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-old_matnr.
  SORT it_A709_tab BY matnr ASCENDING.

  SELECT * FROM A709 INTO TABLE @DATA(it_new_A709_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-matnr.
  SORT it_new_A709_tab BY matnr ASCENDING.


  IF it_A709_tab IS NOT INITIAL.
    DATA lv_count TYPE i.
    LOOP AT it_A709_tab INTO DATA(wa_A709_tab).

      lv_count = lv_count + 1 .
      DATA(lv_index) = line_index( it_mara[ old_matnr = wa_A709_tab-matnr ] ).
      IF lv_index > 0.
        wa_mara = it_mara[ lv_index ].
        DATA(lv_index1) = line_index( it_new_mara_tab[ matnr = wa_mara-matnr ] ).
        IF lv_index1 > 0 .

          DATA(lv_index2) = line_index( it_new_A709_tab[ matnr = wa_mara-matnr kappl = wa_A709_tab-kappl kschl = wa_A709_tab-kschl
          WKREG = wa_A709_tab-WKREG  REGIO = wa_A709_tab-REGIO datbi = wa_A709_tab-datbi datab = wa_A709_tab-datab
          knumh = wa_A709_tab-knumh kfrst = wa_A709_tab-kfrst  kbstat = wa_A709_tab-kbstat TAXK1 = wa_a709_tab-TAXK1   ] ).

          IF lv_index2 < 1.

            wa_A709_tab-matnr = wa_mara-matnr.
            INSERT  A709 FROM  wa_A709_tab.
            IF sy-subrc = 0.
              WRITE : / lv_count , '  . A709 -> ',  wa_mara-matnr ,  ' Updated Successfully'.
            ELSE.
              WRITE : / lv_count , '  . A709 -> ',  wa_mara-matnr ,  ' Not Updated'.
            ENDIF.
          ELSE.
            WRITE : / lv_count , '  . A709 -> ',  wa_mara-matnr ,  ' Already  Updated Successfully'.
          ENDIF.
        ELSE.
          WRITE : / lv_count , '  . A709 -> ',  wa_mara-matnr ,  ' Material Not Found'.
        ENDIF.

      ELSE.
        WRITE : / lv_count , '  . A709 ->',  wa_mara-matnr ,  ' Not Found'.
      ENDIF.
    ENDLOOP.

  ENDIF.
  " A709 table Update End Here.
  "***********************
