*&---------------------------------------------------------------------*
*& Report ZHR_EMPLOYEE_ASSET_DETAILS
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Changed by: Samsudeen M
*&Purpose : Mapping the Asset Details of employee
*&Reference: Ramakrishnan J
*&Date : 14.02.2023
*&---------------------------------------------------------------------*
REPORT zhr_employee_asset_details.
"Data Declarations
INCLUDE zhr_employee_asset_top.
"Selection Screen
INCLUDE zhr_employee_asset_sel.
"Class Definition and Implementation
INCLUDE zhr_employee_asset_cls.

INITIALIZATION.
*****Refreshing the Internal Tables
  REFRESH : gt_upload.

  CREATE OBJECT lo_asset.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
****F4 Help for Browsing the Flat File To Upload
  lo_asset->file_name( ).

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZSEL_STAT'.
  lo_asset->screen_adjust( ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      lo_asset->excel_download( ).
  ENDCASE.

*&---------------------------------------------------------------------*
*&     Start-of-Selection Declarations
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_rad1 IS NOT INITIAL.
    PERFORM initial_selection.
*****Uploading the Data from Flat File
    lo_asset->upload( ).
*****Update to Infotype from Excel Internal Table ***
    lo_asset->update( ).
**** Display Message Aftet Updation ***
    lo_asset->display( ).
  ELSE.
    "Infotype 0040 Display
    PERFORM infotype_40_display.
  ENDIF.

*** Data Initial Selections ***
FORM initial_selection.
  REFRESH: gt_pernr,gt_hire_date,gt_kostl,gt_asset_disp.
** Pernr Selction for Checks **
  SELECT a~pernr
         b~sname INTO CORRESPONDING FIELDS OF TABLE gt_pernr
                 FROM pa0000 AS a INNER JOIN pa0001 AS b
                 ON a~pernr = b~pernr
                 WHERE a~begda LE sy-datum
                 AND a~endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_pernr[] BY pernr.
  ENDIF.
** Fetching Hiring Date From PA0040 **
  SELECT pernr
         dat01 FROM pa0041
               INTO TABLE gt_hire_date
               WHERE dar01 = 'S1'
               AND begda LE sy-datum
               AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_hire_date[] BY pernr.
  ENDIF.
**  Asset Type Checks / Infotype 0040 Subtype Checks **
  SELECT * FROM zhr_asset_type
           INTO TABLE gt_asset_typ.
  IF sy-subrc EQ 0.
    SORT gt_asset_typ[] BY asset_type.
  ENDIF.
*------- Costcenter Master Checks ---------------------*
  SELECT kostl FROM csks INTO TABLE gt_kostl
                         WHERE kokrs = '1000'.
  IF sy-subrc = 0.
    SORT gt_kostl[] BY kostl.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Infotype_40 display
FORM infotype_40_display.
  DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
        gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
        gs_fcat   TYPE slis_fieldcat_alv.

** Employee Asset Display From Infotype 0040 **
  SELECT a~pernr
         a~ename
         b~subty
         b~endda
         b~begda
         b~lobnr
         b~serial_no
         b~asset_tag
         b~bukrs
         b~werks
         b~kostl INTO CORRESPONDING FIELDS OF TABLE gt_asset_disp
                 FROM pa0001 AS a INNER JOIN pa0040 AS b
                 ON a~pernr = b~pernr
                 WHERE a~pernr IN s_pernr AND
                 a~bukrs IN s_bukrs
                 AND a~begda LE sy-datum
                 AND a~endda GE sy-datum
                 AND b~begda LE sy-datum
                 AND b~endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_asset_disp[] BY pernr.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'GT_ASSET_DISP'
      i_structure_name   = 'ZHR_ASSET_DETAILS'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = gt_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'PERNR'.
        gs_fcat-seltext_m  = 'Emp No.'.
        gs_fcat-seltext_l  = 'Emp No.'.
      WHEN 'SNAME'.
        gs_fcat-seltext_m  = 'Emp Name.'.
        gs_fcat-seltext_l  = 'Emp Name.'.
      WHEN 'SUBTY'.
        gs_fcat-seltext_m  = 'Subtype'.
        gs_fcat-seltext_l  = 'Subtype'.
      WHEN 'ENDDA'.
        gs_fcat-seltext_m  = 'End Date'.
        gs_fcat-seltext_l  = 'End Date'.
      WHEN 'BEGDA'.
        gs_fcat-seltext_m  = 'Start Date'.
        gs_fcat-seltext_l  = 'Start Date'.
      WHEN 'LOBNR'.
        gs_fcat-seltext_m  = 'Asset ID'.
        gs_fcat-seltext_l  = 'Asset ID'.
      WHEN 'SERIAL_NO'.
        gs_fcat-seltext_m  = 'Serial No'.
        gs_fcat-seltext_l  = 'Serial NO'.
      WHEN 'ASSET_TAG'.
        gs_fcat-seltext_m  = 'Asset Tag'.
        gs_fcat-seltext_l  = 'Asset Tag'.
      WHEN 'BUKRS'.
        gs_fcat-seltext_m  = 'Company Code'.
        gs_fcat-seltext_l  = 'Company Code'.
      WHEN 'WERKS'.
        gs_fcat-seltext_m  = 'Plant'.
        gs_fcat-seltext_l  = 'Plant'.
      WHEN 'KOSTL'.
        gs_fcat-seltext_m  = 'Cost Center'.
        gs_fcat-seltext_l  = 'Cost Center'.
    ENDCASE.
    MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
  ENDLOOP.

  gs_layout-colwidth_optimize = 'X'.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
      i_save             = 'X'
    TABLES
      t_outtab           = gt_asset_disp[].

ENDFORM.
