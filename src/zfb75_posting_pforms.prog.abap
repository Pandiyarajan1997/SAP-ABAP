*&---------------------------------------------------------------------*
*& Include          ZFB75_POSTING_PFORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM f4_gethelp.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_path.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_convert_excel
*&---------------------------------------------------------------------*
FORM f_convert_excel .
  IF p_path IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = gt_type
        i_filename           = p_path
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'File is Mandatory' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_actual_process
*&---------------------------------------------------------------------*
FORM f_actual_process .
  REFRESH gt_alv.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
    MOVE-CORRESPONDING <fs_excel> TO <fs_alv>.
    SELECT SINGLE * FROM kna1 INTO @DATA(l_kna1) WHERE kunnr = @<fs_excel>-account.
    IF sy-subrc NE 0.
      <fs_alv>-type = 'E'.
      <fs_alv>-msg = |Customer Number is Incorrect|.
      APPEND <fs_alv> TO gt_alv.
      CONTINUE.
    ENDIF.
    "Business Place Checks
    IF <fs_excel>-bupla IS NOT INITIAL.
      SELECT SINGLE * FROM pbusinessplace INTO @DATA(l_bplace) WHERE bukrs = @p_bukrs
                                                               AND branch = @<fs_excel>-bupla.
      IF sy-subrc NE 0.
        <fs_alv>-type = 'E'.
        <fs_alv>-msg = |Incorrect Business Place|.
        APPEND <fs_alv> TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.
**** G/L Checks *****
    DATA(lv_gl) = VALUE #( gt_skat[ saknr = <fs_excel>-hkont ]-saknr OPTIONAL ).
    IF lv_gl IS INITIAL.
      <fs_alv>-type = 'E'.
      <fs_alv>-msg = |Incorrect G/L Account Number|.
      APPEND <fs_alv> TO gt_alv.
      CONTINUE.
    ENDIF.
*-------------- Costecenter Validatin checks from excel -------------------------------*
    DATA(lv_cc) = VALUE #( gt_csks[ kostl = <fs_excel>-kostl ]-kostl OPTIONAL ).
    IF lv_cc IS INITIAL.
      <fs_alv>-type = 'E'.
      <fs_alv>-msg = |Incorrect Costcenter|.
      APPEND <fs_alv> TO gt_alv.
      CONTINUE.
    ENDIF.
*----- Plant Code Validation from excel ---------------------------------------------*
    IF <fs_excel>-plant IS NOT INITIAL.
      SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @<fs_excel>-plant.
      IF sy-subrc NE 0.
        <fs_alv>-type = 'E'.
        <fs_alv>-msg = |Incorrect Plant Code|.
        APPEND <fs_alv> TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.
    "Sales Office Checks
    IF <fs_excel>-vkbur IS NOT INITIAL.
      SELECT SINGLE * FROM tvbur INTO @DATA(l_salesoff) WHERE vkbur = @<fs_excel>-vkbur.
      IF sy-subrc NE 0.
        <fs_alv>-type = 'E'.
        <fs_alv>-msg = |Incorrect Sales Office|.
        APPEND <fs_alv> TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.
    "Material Group Checks
    IF <fs_excel>-matkl IS NOT INITIAL.
      SELECT SINGLE * FROM t023 INTO @DATA(l_matgrp) WHERE matkl = @<fs_excel>-matkl.
      IF sy-subrc NE 0.
        <fs_alv>-type = 'E'.
        <fs_alv>-msg = |Incorrect Material Group|.
        APPEND <fs_alv> TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_data_Selection
*&---------------------------------------------------------------------*
FORM f_data_selection .
  REFRESH: gt_skat,gt_csks.
  SELECT * FROM csks INTO TABLE gt_csks
           WHERE kokrs = '1000'.
  IF sy-subrc = 0.
    SORT gt_csks[] BY kostl.
  ENDIF.
  SELECT * FROM skat INTO TABLE gt_skat
           WHERE spras = sy-langu.
  IF sy-subrc = 0.
    SORT gt_skat[] BY saknr.
  ENDIF.
ENDFORM.
