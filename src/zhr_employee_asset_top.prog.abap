*&---------------------------------------------------------------------*
*& Include          ZHR_EMPLOYEE_ASSET_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel,
         pernr     TYPE pernr_d,
         subty     TYPE subty,
         lobnr     TYPE lobnr,
         serial_no TYPE zserial_no,
         asset_tag TYPE zasset_tag,
         bukrs     TYPE bukrs,
         werks     TYPE werks_d,
         kostl     TYPE kostl,
       END OF ty_excel.

TYPES: BEGIN OF ty_disp,
         pernr       TYPE pernr_d,
         sname       TYPE smnam,
         t_error(02) TYPE c,
         message     TYPE p08_text150,
       END OF ty_disp.

TYPES: BEGIN OF ty_pernr,
         pernr TYPE pernr_d,
         sname TYPE smnam,
       END OF ty_pernr.

TYPES: BEGIN OF ty_pernr_hire,    "Structure Employee Hire Date Table Data
         pernr TYPE pernr_d,
         dat01 TYPE dardt,
       END OF ty_pernr_hire.

TYPES: BEGIN OF ty_asset_disp,
         pernr     TYPE pernr_d,
         ename     TYPE ename,
         subty     TYPE subty,
         endda     TYPE endda,
         begda     TYPE begda,
         lobnr     TYPE lobnr,
         serial_no TYPE zserial_no,
         asset_tag TYPE zasset_tag,
         bukrs     TYPE bukrs,
         werks     TYPE werks_d,
         kostl     TYPE kostl,
       END OF ty_asset_disp.

TYPES: BEGIN OF ty_csks,
         kostl TYPE kostl,
       END OF ty_csks.

**** Excel data conversion Internal table ****
DATA: gt_upload TYPE TABLE OF ty_excel,
      gs_upload TYPE ty_excel.

**** ALV Display Internal Table ****
DATA: gt_display TYPE TABLE OF ty_disp,
      gs_display TYPE ty_disp.

** Initial pernr Selection table for Checks **
DATA: gt_pernr TYPE TABLE OF ty_pernr,
      gs_pernr TYPE ty_pernr.

** Internal table for Employee Hire Date Details **
DATA: gt_hire_date TYPE TABLE OF ty_pernr_hire,
      gs_hire_date TYPE ty_pernr_hire.

DATA: gt_asset_typ TYPE STANDARD TABLE OF zhr_asset_type,
      gs_asset_typ TYPE zhr_asset_type.

DATA: gt_asset_disp TYPE TABLE OF ty_asset_disp,
      gs_asset_disp TYPE ty_asset_disp.

DATA: gt_kostl TYPE TABLE OF ty_csks.

DATA : lt_type TYPE truxs_t_text_data.

**** ALV Declaration ****
DATA: lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      ls_layout TYPE lvc_s_layo.

DATA: lv_pernr TYPE pa0001-pernr.
*******Local class definations****
CLASS lcl_asset DEFINITION DEFERRED.

DATA: lo_asset TYPE REF TO lcl_asset.
DATA: lr_excel_structure      TYPE REF TO data,
      lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
      lo_table_row_descripter TYPE REF TO cl_abap_structdescr,
      lv_content              TYPE xstring,
      lt_binary_tab           TYPE TABLE OF sdokcntasc,
      lv_filename1            TYPE string,
      lv_path                 TYPE string,
      lv_length               TYPE i,
      lv_fullpath             TYPE string.

DATA: lv_bukrs TYPE bukrs.
DATA: gr_table   TYPE REF TO cl_salv_table.
