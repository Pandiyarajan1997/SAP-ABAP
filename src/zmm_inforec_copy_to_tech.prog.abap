*&---------------------------------------------------------------------*
*& Report ZMM_INFOREC_COPY_TO_TECH
*&---------------------------------------------------------------------*
*&"Created by: Samsudeen M
"Created on: 05.04.2023
"Purpose : Copying the exact inforecord from Unit V to tech service plant code
"Requirement : Purchase Team
"Reference by: Ramakrishnan J & Goplaraja
*&---------------------------------------------------------------------*
REPORT zmm_inforec_copy_to_tech.
"Data Declarations
INCLUDE zmm_inforec_copy_top.

DATA: lv_matnr TYPE eina-matnr.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR lv_matnr.
  PARAMETERS: p_lifnr  TYPE ekko-lifnr,
              p_ekorg  TYPE ekko-ekorg,
              p_fplant TYPE ekko-reswk,
              p_plant  TYPE marc-werks.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_list AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
"subroutines
INCLUDE zmm_inforec_copy_forms.

AT SELECTION-SCREEN ON p_list.
  "Dynpro Value read on selection screen
  PERFORM f_dynpro_read.

INITIALIZATION.
  "Initialize Value in selection.
  PERFORM f_initial_list.

START-OF-SELECTION.
  "Initial Selection
  PERFORM f_initial_selection.
  "Actual Create and Update of Inforecords
  PERFORM f_actual_process.
  "Display Log
  PERFORM f_display_alv.
