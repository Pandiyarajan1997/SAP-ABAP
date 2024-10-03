*&---------------------------------------------------------------------*
*& Report ZFI_PDF_ATTACH_APPL_SERVER
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created On: 16.05.2023
*&Purpose: Adding the PDF to Financial Documents from Application server
*Reference: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zfi_pdf_attach_appl_server.
***Data Declarations***
INCLUDE zfi_pdf_aps_to_frontend_cls.

INITIALIZATION.
  DATA: lo_main TYPE REF TO lcl_pdf_attach.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  "Get all the Files inside the folder of Application Server
  lo_main->list_of_files( ).
  "Actual Process
  lo_main->actual_process( ).
  "Alv Display
  lo_main->build_alv( ).
