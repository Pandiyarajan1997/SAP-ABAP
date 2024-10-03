*&---------------------------------------------------------------------*
*& Report ZFI_HDFC_PAYREF_UPDATE
*&---------------------------------------------------------------------*
"&Created by: Samsuddeen M
"&Created on: 27.03.2023
"Reference by: Ramakrishnan J
"Updating the UTR Number for the Clearing Document Number
*&---------------------------------------------------------------------*
REPORT zfi_hdfc_utr_update.

TYPES: BEGIN OF alv,
         bukrs  TYPE bukrs,
         doc_no TYPE belnr_d,
         gjahr  TYPE gjahr,
         utrno  TYPE char25,
         type   TYPE bapi_mtype,
         msg    TYPE message-msgtx,
       END OF alv.
DATA: gt_alv TYPE TABLE OF alv.

TYPES: BEGIN OF ty_excel,
         comp_code TYPE bukrs,
         docno     TYPE belnr_d,
         fisyear   TYPE gjahr,
         utrno     TYPE char25,
       END OF ty_excel.
DATA: gt_excel TYPE TABLE OF ty_excel.
DATA: gt_type TYPE truxs_t_text_data.

DATA: gt_bseg   TYPE STANDARD TABLE OF bseg,
      gt_accchg TYPE TABLE OF accchg.
DATA: gv_utr_field TYPE    char50 VALUE 'KIDNO'.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
"Selection Screen Design
DATA: lv_belnr TYPE belnr_d,
      lv_blart TYPE blart.
DATA: lr_excel_structure      TYPE REF TO data,
      lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
      lo_table_row_descripter TYPE REF TO cl_abap_structdescr,
      lv_content              TYPE xstring,
      lt_binary_tab           TYPE TABLE OF sdokcntasc,
      lv_filename1            TYPE string,
      lv_path                 TYPE string,
      lv_length               TYPE i,
      lv_fullpath             TYPE string.

INCLUDE zfi_hdfc_utr_update_pforms.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_path TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZSEL_STAT'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM f_excel_download.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM f4_help USING p_path.

START-OF-SELECTION.
  "Excel Conversion
  PERFORM excel_conversion USING p_path.
  "Actual Process
  PERFORM actual_process.
  "Display ALV
  PERFORM alv_display.
