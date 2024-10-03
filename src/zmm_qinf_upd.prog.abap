*&---------------------------------------------------------------------*
*& Report ZMM_QINF_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_qinf_upd.
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



  SELECT * FROM qinf INTO TABLE @DATA(it_QINF_tab)
  FOR ALL ENTRIES IN @it_mara
  WHERE matnr = @it_mara-old_matnr.
  SORT it_QINF_tab BY matnr ASCENDING.

  SELECT * FROM qinf INTO TABLE @DATA(it_new_QINF_tab)
FOR ALL ENTRIES IN @it_mara
WHERE matnr = @it_mara-matnr.
  SORT it_new_QINF_tab BY matnr ASCENDING.

  IF it_QINF_tab IS NOT INITIAL.
    DATA lv_count TYPE i.
    LOOP AT it_QINF_tab INTO DATA(wa_QINF_tab).

      lv_count = lv_count + 1 .
      DATA(lv_index) = line_index( it_mara[ old_matnr = wa_QINF_tab-matnr ] ).
      IF lv_index > 0.
        wa_mara = it_mara[ lv_index ].
        DATA(lv_index1) = line_index( it_new_mara_tab[ matnr = wa_mara-matnr ] ).
        IF lv_index1 > 0 .
          wa_QINF_tab-matnr = wa_mara-matnr.

          DATA(lv_index2) = line_index( it_new_qinf_tab[
          matnr = wa_QINF_tab-matnr
                  zaehl = wa_QINF_tab-zaehl
                  ersteller = wa_QINF_tab-ersteller
                  erstelldat = wa_QINF_tab-erstelldat
                  aenderer = wa_QINF_tab-aenderer
                  aenderdat = wa_QINF_tab-aenderdat
                  changeddatetime = wa_QINF_tab-changeddatetime
                  lieferant = wa_QINF_tab-lieferant
                  werk = wa_QINF_tab-werk
                  frei_dat = wa_QINF_tab-frei_dat
                  frei_mgkz = wa_QINF_tab-frei_mgkz
                  me = wa_QINF_tab-me
                  frei_mng = wa_QINF_tab-frei_mng
                  best_mg = wa_QINF_tab-best_mg
                  dat_rueck = wa_QINF_tab-dat_rueck
                  sperrgrund = wa_QINF_tab-sperrgrund
                  sprache = wa_QINF_tab-sprache
                  sperrfkt = wa_QINF_tab-sperrfkt
                  qvvorh = wa_QINF_tab-qvvorh
                  noinsp = wa_QINF_tab-noinsp
                  qssysfam = wa_QINF_tab-qssysfam
                  qssysdat = wa_QINF_tab-qssysdat
                  variabnahm = wa_QINF_tab-variabnahm
                  ltextkz = wa_QINF_tab-ltextkz
                  spras = wa_QINF_tab-spras
                  plos = wa_QINF_tab-plos
                  plos2 = wa_QINF_tab-plos2
                  ppap_is_required = wa_QINF_tab-ppap_is_required
                  ppap_level = wa_QINF_tab-ppap_level
                  ppap_status = wa_QINF_tab-ppap_status
                  ppap_id = wa_QINF_tab-ppap_id
                  stsma = wa_QINF_tab-stsma
                  objnr = wa_QINF_tab-objnr
                  noinspabn = wa_QINF_tab-noinspabn
                  vorlabn = wa_QINF_tab-vorlabn
                  revlv = wa_QINF_tab-revlv
                  loekz = wa_QINF_tab-loekz
                  certcontrol = wa_QINF_tab-certcontrol

          ] ).

          IF lv_index2 < 1.
            INSERT  qinf FROM  wa_QINF_tab.
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
