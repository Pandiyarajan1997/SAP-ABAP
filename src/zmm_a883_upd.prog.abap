*&---------------------------------------------------------------------*
*& Report ZMM_A883_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_A883_UPD.

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
  " A883 table Update Start from Here.

  SELECT * FROM A883 INTO TABLE @DATA(it_A883_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-old_matnr.
  SORT it_A883_tab BY matnr ASCENDING.

  SELECT * FROM A883 INTO TABLE @DATA(it_new_A883_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-matnr.
  SORT it_new_A883_tab BY matnr ASCENDING.


  IF it_A883_tab IS NOT INITIAL.
    DATA lv_count TYPE i.
    LOOP AT it_A883_tab INTO DATA(wa_A883_tab).

      lv_count = lv_count + 1 .
      DATA(lv_index) = line_index( it_mara[ old_matnr = wa_A883_tab-matnr ] ).
      IF lv_index > 0.
        wa_mara = it_mara[ lv_index ].
        DATA(lv_index1) = line_index( it_new_mara_tab[ matnr = wa_mara-matnr ] ).
        IF lv_index1 > 0 .

          DATA(lv_index2) = line_index( it_new_A883_tab[ matnr = wa_mara-matnr kappl = wa_A883_tab-kappl kschl = wa_A883_tab-kschl
         EKORG = wa_A883_tab-EKORG VTWEG = wa_a883_tab-VTWEG datbi = wa_A883_tab-datbi datab = wa_A883_tab-datab
          knumh = wa_A883_tab-knumh kfrst = wa_A883_tab-kfrst  kbstat = wa_A883_tab-kbstat   ] ).

          IF lv_index2 < 1.

            wa_A883_tab-matnr = wa_mara-matnr.
            INSERT  A883 FROM  wa_A883_tab.
            IF sy-subrc = 0.
              WRITE : / lv_count , '  . A883 -> ',  wa_mara-matnr ,  ' Updated Successfully'.
            ELSE.
              WRITE : / lv_count , '  . A883 -> ',  wa_mara-matnr ,  ' Not Updated'.
            ENDIF.
          ELSE.
            WRITE : / lv_count , '  . A883 -> ',  wa_mara-matnr ,  ' Already  Updated Successfully'.
          ENDIF.
        ELSE.
          WRITE : / lv_count , '  . A883 -> ',  wa_mara-matnr ,  ' Material Not Found'.
        ENDIF.

      ELSE.
        WRITE : / lv_count , '  . A883 ->',  wa_mara-matnr ,  ' Not Found'.
      ENDIF.
    ENDLOOP.

  ENDIF.
  " A883 table Update End Here.
  "***********************
