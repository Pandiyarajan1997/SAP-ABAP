*&---------------------------------------------------------------------*
*& Report ZDMS_OPENING_BALANCE_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_opening_balance_posting.

TYPES: BEGIN OF alv,
         account(10) TYPE c,
         name1       TYPE name1_gp,
         acctyp      TYPE koart,
         blart       TYPE blart,
         refdoc      TYPE xblnr1,
         fisyear     TYPE gjahr,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         amt         TYPE wrbtr,
         item_txt    TYPE sgtxt,
         gsber       TYPE gsber,
         docno       TYPE belnr_d,
         msgtyp      TYPE bapi_mtype,
         message     TYPE string,
       END OF alv.
DATA: gt_alv  TYPE TABLE OF alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
*************************************************************************
************ Selection Screen Design ************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename,
              p_bukrs TYPE t001-bukrs OBLIGATORY.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_rad1 RADIOBUTTON GROUP r1 MODIF ID bl1,
                p_rad2 RADIOBUTTON GROUP r1 MODIF ID bl1.
  SELECTION-SCREEN END OF BLOCK b2.
  PARAMETERS: p_chk3 AS CHECKBOX MODIF ID bl3.
SELECTION-SCREEN END OF BLOCK b1.

***************************************************************************
INCLUDE zdms_opening_balance_post_cls.

DATA: lo_main TYPE REF TO lcl_posting.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_adjust.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.


START-OF-SELECTION.
  CREATE OBJECT lo_main
    EXPORTING
      l_bukrs = p_bukrs
      l_fname = p_fname.

  IF p_fname IS NOT INITIAL.
    "Converting Data from excel to SAP
    lo_main->excel_conversion( ).
    lo_main->actual_process( ).
    lo_main->alv_display( ).
  ELSE.
    MESSAGE 'Filename is Mandatory' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
