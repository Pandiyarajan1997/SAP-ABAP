*&---------------------------------------------------------------------*
*& Report ZMM_EORD_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_eord_upd.
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

  SELECT * FROM eord INTO TABLE @DATA(it_eord_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-old_matnr.
  SORT it_EORD_tab BY matnr ASCENDING.

  SELECT * FROM eord INTO TABLE @DATA(it_new_eord_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-matnr.
  SORT it_EORD_tab BY matnr ASCENDING.

  IF it_eord_tab IS NOT INITIAL.
    DATA lv_count TYPE i.
    LOOP AT it_eord_tab INTO DATA(wa_eord_tab).
      lv_count = lv_count + 1 .
      DATA(lv_index) = line_index( it_mara[ old_matnr = wa_eord_tab-matnr ] ).
      IF lv_index > 0.
        wa_mara = it_mara[ lv_index ].
        DATA(lv_index1) = line_index( it_new_mara_tab[ matnr = wa_mara-matnr ] ).
        IF lv_index1 > 0 .
          wa_eord_tab-matnr = wa_mara-matnr.
          DATA(lv_index2) = line_index( it_new_eord_tab[
          matnr = wa_eord_tab-matnr
                werks = wa_eord_tab-werks
                zeord = wa_eord_tab-zeord
                erdat = wa_eord_tab-erdat
                ernam = wa_eord_tab-ernam
                vdatu = wa_eord_tab-vdatu
                bdatu = wa_eord_tab-bdatu
                lifnr = wa_eord_tab-lifnr
                flifn = wa_eord_tab-flifn
                ebeln = wa_eord_tab-ebeln
                ebelp = wa_eord_tab-ebelp
                febel = wa_eord_tab-febel
                reswk = wa_eord_tab-reswk
                fresw = wa_eord_tab-fresw
                ematn = wa_eord_tab-ematn
                notkz = wa_eord_tab-notkz
                ekorg = wa_eord_tab-ekorg
                vrtyp = wa_eord_tab-vrtyp
                eortp = wa_eord_tab-eortp
                autet = wa_eord_tab-autet
                meins = wa_eord_tab-meins
                logsy = wa_eord_tab-logsy
                sobkz = wa_eord_tab-sobkz
                srm_contract_id = wa_eord_tab-srm_contract_id
                srm_contract_itm = wa_eord_tab-srm_contract_itm
                lastchangedatetime = wa_eord_tab-lastchangedatetime
           ] ).

          IF lv_index2 < 1.

            INSERT  eord FROM  wa_eord_tab.
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
