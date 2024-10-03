*&---------------------------------------------------------------------*
*& Report ZCUST_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcust_upload.

INCLUDE zcust_upload_cls.
** EXCEL  input structure   *******

INITIALIZATION.
  DATA: lo_upload TYPE REF TO lcl_upload.
  CREATE OBJECT lo_upload.

****** F4 help for fetching excel file
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  lo_upload->f4_help( ).

START-OF-SELECTION.
  "Excel Upload
  lo_upload->convert_xls_to_sap( ).
  "Actual Process
  lo_upload->actual_upload( ).
  "Alv
  lo_upload->build_alv( ).

END-OF-SELECTION.
