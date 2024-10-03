*&---------------------------------------------------------------------*
*& Report ZMM_DMS_PURCHASING
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created on: 23.02.2023
*&Purpose : Automatic Creation of Info Record, Purchase Order, Goods Receipt,
*           Miro Invoice for DMS Distributor sales Order
*&Reference by: Ramakrishnan J & Praveen Kumar & Gopal Raja
*&---------------------------------------------------------------------*
REPORT zmm_dms_purchasing.

"data Declarations
INCLUDE zmm_dms_purchasing_top.

"Selection Screen Design
DATA: lv_fkdat TYPE vbrk-fkdat,
      lv_vbeln TYPE vbeln_vf.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_lifnr TYPE ekko-lifnr DEFAULT '0010001545'.
  SELECT-OPTIONS: s_vbeln FOR lv_vbeln,
                 s_fkdat FOR lv_fkdat.
SELECTION-SCREEN END OF BLOCK b1.

"Logic for all the Process
INCLUDE zmm_dms_purchasing_class.

DATA: lo_purchasing TYPE REF TO lcl_purchasing.

INITIALIZATION.
  CREATE OBJECT lo_purchasing.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  "data selection
  lo_purchasing->f_data_selection( ).
  "Overall Process
  lo_purchasing->f_overall_process( ).
  "ALV Display
  lo_purchasing->f_display_alv( ).
