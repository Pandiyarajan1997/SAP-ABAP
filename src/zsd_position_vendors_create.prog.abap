*&---------------------------------------------------------------------*
*& Report ZSD_POSITION_VENDORS_CREATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------*
"Created by: Samsudeen M
"Created on: 23.01.2023
"Reference by: Ramakrishnan J
"Purpose: Creating Position Vendors from excel
*------------------------------------------------------------------------*
REPORT zsd_position_vendors_create.

DATA: businesspartner  TYPE bapibus1006_head-bpartner. " Business Partner Number

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
  SELECTION-SCREEN SKIP.
*  PARAMETERS: p_run AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zsd_pos_vendor_create_cls.
DATA : lo_main TYPE REF TO lcl_position_vendor.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  lo_main->f_get_f4( ).

INITIALIZATION.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  lo_main->f_convert_excel_data( ). "Excel Conversion
  lo_main->f_create_extend_bp( ).  "Create Business Partner all process
  lo_main->f_display_alv( ).  "Display ALV
