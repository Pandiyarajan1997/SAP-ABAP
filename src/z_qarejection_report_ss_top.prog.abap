*&---------------------------------------------------------------------*
*&  Include           Z_QAREJECTION_REPORT_SS_TOP
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : S_WERKS FOR MSEG-WERKS,
                   S_MATNR FOR MSEG-MATNR,
                   S_LIFNR FOR MSEG-LIFNR,
                   S_EBELN FOR MSEG-EBELN,
                   S_BUDAT FOR MSEG-BUDAT_MKPF OBLIGATORY.


SELECTION-SCREEN END OF BLOCK B1.
