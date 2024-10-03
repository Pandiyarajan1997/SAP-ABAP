*&---------------------------------------------------------------------*
*& Include          ZHR_HOADMIN_EMP_TOP
*&---------------------------------------------------------------------*
TABLES:
  p0001,
  zhr_st_appt_letter.
TYPE-POOLS: vrm.

DATA : g_ssn TYPE sy-dynnr.

DATA: gv_name       TYPE vrm_id,
      gt_list       TYPE vrm_values,
      gw_list       LIKE LINE OF gt_list,
      g_letter_typ  TYPE zhr_letter_typ,
      g_pernr       TYPE persno,
      g_date        TYPE dats,
      g_time        TYPE uzeit,
      g_grosspay    TYPE char10,
      g_variable    TYPE char10,
*      g_app_letter    TYPE zhr_appointment_letter,
      gt_emp_letter TYPE STANDARD TABLE OF zhr_emp_letters,
      gw_emp_letter TYPE zhr_emp_letters,
      r1            TYPE c,
      r2            TYPE c.
DATA : g_html_container TYPE REF TO cl_gui_custom_container,
       g_html_control   TYPE REF TO cl_gui_html_viewer.

DATA g_filename TYPE string.
DATA: t_tline TYPE STANDARD TABLE OF tline,
      w_tline TYPE tline.
DATA:go_adobe TYPE REF TO zcl_adobe_esign_process.
DATA:      ok_code LIKE sy-ucomm.
*
*    TYPES: BEGIN OF ty_output,
*             mark(1)     TYPE c,
*             empno       TYPE p0001-pernr,
*             title       TYPE char2,
*             empname     TYPE p0001-ename,
*             designation TYPE p9020-design,
*             department  TYPE p9020-department,
*             location    TYPE p9020-location,
*             repmanager  TYPE p0001-ename,
*             address1    TYPE p0006-stras,
*             address2    TYPE p0006-locat,
*             addresscity TYPE p0006-ort01,
*             addressdist TYPE p0006-ort02,
*             addresspin  TYPE p0006-pstlz,
*             joindate    TYPE p0001-begda,
*             grosspay    TYPE char10,
*             variable    TYPE char10,
*           END OF ty_output.
*SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
*  PARAMETERS: p_date TYPE sy-datum  OBLIGATORY.
*  SELECT-OPTIONS : s_pernr FOR p0001-pernr NO INTERVALS MATCHCODE OBJECT prem OBLIGATORY.
*  PARAMETERS : r1 TYPE c RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND r1,
**               p_gross TYPE p0008-bet01 ,
**               p_varbl TYPE p0008-bet01 ,
*               r2 TYPE c RADIOBUTTON GROUP a.
*SELECTION-SCREEN END OF BLOCK a1.
