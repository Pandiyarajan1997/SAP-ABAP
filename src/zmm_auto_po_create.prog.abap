*&---------------------------------------------------------------------*
*& Report ZMM_AUTO_PO_CREATE
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*Created On: 23.05.2023
*Purpose: Auto Purchase Order Creation based on status
*Reference: Ramakrishnan & Gopal raja & Praveen Kumar
*&---------------------------------------------------------------------*
REPORT zmm_auto_po_create.

DATA: gt_logs     TYPE STANDARD TABLE OF zmm_autopo_log,
      gt_logs_alv TYPE STANDARD TABLE OF zmm_autopo_log.
DATA: ls_header  TYPE bapimepoheader,
      ls_headerx TYPE bapimepoheaderx,
      lt_poitem  TYPE TABLE OF bapimepoitem.
DATA: lv_item TYPE ebelp VALUE '00010'.
DATA: lv_ebeln TYPE ebeln,
      lv_msg   TYPE string.
DATA: msg TYPE char50.
FIELD-SYMBOLS: <fs_logs> TYPE zmm_autopo_log.

CONSTANTS: c_initiated TYPE c VALUE 'I',
           c_error     TYPE c VALUE 'E',
           c_nomap     TYPE c VALUE 'N'.

*****Selection Screen Design ******
DATA: lv_unqid TYPE zpoid.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_unqid FOR lv_unqid.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zmm_auto_po_create_cls.

INITIALIZATION.
  DATA: lo_main_cls TYPE REF TO lcl_po_create.
  CREATE OBJECT lo_main_cls.

START-OF-SELECTION.
  CLEAR msg.
  lo_main_cls->background_job_chk( EXPORTING pname = sy-cprog
                               CHANGING l_msg = msg ).
  IF msg IS INITIAL.
    lo_main_cls->log_data_fetch( ).

    lo_main_cls->initiated_status( ).

    lo_main_cls->no_mapping_status( ).

    lo_main_cls->mail_sending( ).

    lo_main_cls->alv_display( ).
  ELSE.
    WRITE: / 'Error:' ,msg.
  ENDIF.
