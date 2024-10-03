*&---------------------------------------------------------------------*
*& Report ZMM_MARA_FIELD_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_marc_field_upd.

TABLES: mara,marc.

TYPES: BEGIN OF ty_material,
         matnr TYPE matnr,
         werks TYPE marc-werks,
         dismm TYPE marc-dismm,
         dispo TYPE marc-dispo,
         disls TYPE marc-disls,
         eisbe TYPE marc-eisbe,
         strgr TYPE marc-strgr,
         sbdkz TYPE marc-sbdkz,

       END OF ty_material.

DATA: lt_materials TYPE TABLE OF ty_material,
      ls_material  TYPE ty_material,
      lt_return    TYPE TABLE OF bapiret2,
      ls_return    TYPE bapiret2.

DATA: lt_excel TYPE TABLE OF alsmex_tabline,
      ls_excel TYPE alsmex_tabline.

DATA: ls_headdata TYPE bapimathead,
      ls_mara     TYPE bapi_mara,
      ls_mara_x   TYPE bapi_marax,
      ls_marc     TYPE bapi_marc,
      ls_marc_x   TYPE bapi_marcx.

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
  .


**   Read Excel file into internal table lt_excel
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
      WHEN 1. ls_material-matnr = ls_excel-value.
      WHEN 2. ls_material-werks = ls_excel-value.
      WHEN 3. ls_material-dismm = ls_excel-value.
      WHEN 4. ls_material-dispo = ls_excel-value.
      WHEN 5. ls_material-disls = ls_excel-value.
      WHEN 6. ls_material-eisbe = ls_excel-value.
      WHEN 7. ls_material-strgr = ls_excel-value.
      WHEN 8.
        ls_material-sbdkz = ls_excel-value.
        APPEND ls_material TO lt_materials.
        CLEAR ls_material.
    ENDCASE.

  ENDLOOP.
  DATA : row_id TYPE i VALUE 0.

  SELECT matnr,werks, dismm,  dispo,  disls,  eisbe,  strgr ,sbdkz FROM marc INTO TABLE @DATA(it_marc_tab)
    FOR ALL ENTRIES IN @lt_materials
    WHERE matnr = @lt_materials-matnr AND werks = @lt_materials-werks.
  SORT  it_marc_tab BY matnr  ASCENDING werks ASCENDING.


  LOOP AT lt_materials INTO ls_material.
    row_id = row_id + 1 .
**   assigning values for MARA table
    DATA(index) = line_index( it_marc_tab[ matnr = ls_material-matnr
    werks = ls_material-werks
    dismm = ls_material-dismm
    dispo = ls_material-dispo
    disls = ls_material-disls
    eisbe = ls_material-eisbe
    strgr = ls_material-strgr
    sbdkz = ls_material-sbdkz
    ] ).

    IF index < 1.
      ls_headdata-material = ls_material-matnr.

    ls_marc-plant = ls_material-werks.
    ls_marc-mrp_type = ls_material-dismm.
    ls_marc-lotsizekey = ls_material-disls.
    ls_marc-safety_stk = ls_material-eisbe.
    ls_marc-plan_strgp = ls_material-strgr.
    ls_marc-dep_req_id = ls_material-sbdkz.
    ls_marc-mrp_ctrler = ls_material-dispo.

    ls_marc_x-plant = ls_material-werks.
    ls_marc_x-mrp_type = 'X'.
    ls_marc_x-lotsizekey = 'X'.
    ls_marc_x-safety_stk = 'X'.
    ls_marc_x-plan_strgp = 'X'.
    ls_marc_x-dep_req_id = 'X'.
    ls_marc_x-mrp_ctrler = 'X'.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata    = ls_headdata
          clientdata  = ls_mara
          clientdatax = ls_mara_x
          plantdata   = ls_marc
          plantdatax  = ls_marc_x
        IMPORTING
          return      = ls_return.
      APPEND ls_return TO lt_return.

      IF ls_return IS INITIAL OR ls_return-type = 'S'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        WRITE: / row_id  , ls_headdata-material ,'Successfully Update'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT lt_return INTO ls_return.
          WRITE: / row_id  , ls_material-matnr ,ls_return-type, ls_return-message.
        ENDLOOP.
      ENDIF.
    ELSE.
      WRITE: / row_id  , ls_material-matnr , 'Already Updated '.
    ENDIF.
    CLEAR: ls_headdata, ls_mara, ls_mara_x, ls_marc, ls_marc_x, lt_return.
  ENDLOOP.
