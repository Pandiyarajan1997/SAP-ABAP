*&---------------------------------------------------------------------*
*& Report  ZNEW_HSNCODE_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZNEW_HSNCODE_REPORT.

TYPE-POOLS:SLIS.

************** Table Used in Program*****************
TABLES: MARC,MARA.

TYPES: BEGIN OF TY_MARC,
  MATNR TYPE MARC-MATNR,
  WERKS TYPE MARC-WERKS,
  STEUC TYPE MARC-STEUC,
  MAKTX TYPE MAKT-MAKTX,
  END OF TY_MARC.

DATA:IT_MARC TYPE TABLE OF TY_MARC,
      WA_MARC TYPE TY_MARC.

****************TYPE Final Table Start **************
TYPES:BEGIN OF TY_FINAL,
  MATNR TYPE MARC-MATNR,
  WERKS TYPE MARC-WERKS,
  STEUC TYPE MARC-STEUC,
  MAKTX TYPE MAKT-MAKTX,
  END OF TY_FINAL.


DATA: IT_FINAL TYPE TABLE OF TY_FINAL,
  WA_FINAL TYPE TY_FINAL.

"*************TYPE Final Table End**************

"************** Selection Screen Starts*****************

SELECTION-SCREEN BEGIN OF BLOCK HSN WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:S_WERKS FOR MARC-WERKS.
SELECT-OPTIONS:S_MATNR FOR MARC-MATNR.
SELECT-OPTIONS:S_STEUC FOR MARC-STEUC.

SELECTION-SCREEN END OF BLOCK HSN.
************** Selection Screen End*****************

************** Start Of Selection Screen Starts *****************
START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM READ_DATA.

END-OF-SELECTION.

************** END Of Selection Screen Starts *****************

************** ALv Declaraton starts *****************
  DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV,
        WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
        IT_REPID TYPE SY-REPID VALUE SY-REPID.

  PERFORM FIELDCAT.
  PERFORM LAYOUT.
  PERFORM DISPLAY.


************** ALv Declaraton ends *****************

FORM GET_DATA.

  SELECT  MARC~MATNR
          MARC~WERKS
          MARC~STEUC
          MAKT~MAKTX INTO CORRESPONDING FIELDS OF TABLE IT_MARC FROM MARC JOIN MAKT ON MARC~MATNR = MAKT~MATNR
          WHERE
          MARC~MATNR IN S_MATNR AND MARC~WERKS IN S_WERKS AND MARC~STEUC IN S_STEUC .

ENDFORM.                    "GET_DATA


"*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_DATA .

  LOOP AT IT_MARC INTO WA_MARC.

    WA_FINAL-MATNR = WA_MARC-MATNR.
    WA_FINAL-WERKS  = WA_MARC-WERKS.
    WA_FINAL-STEUC  = WA_MARC-STEUC.
    WA_FINAL-MAKTX  = WA_MARC-MAKTX.

    APPEND WA_FINAL TO IT_FINAL.
  ENDLOOP.



CLEAR : WA_MARC.

ENDFORM.                    "READ_DATA

INCLUDE ZNEW_SALES_GST_LAYOUTF01.

INCLUDE ZNEW_SALES_GST_DISPLAYF01.


*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELDCAT .


  WA_FIELDCAT-FIELDNAME ='MATNR'.
  WA_FIELDCAT-TABNAME ='IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S ='Material'.
  WA_FIELDCAT-SELTEXT_L ='Material'.
  WA_FIELDCAT-COL_POS   = 1.
  WA_FIELDCAT-OUTPUTLEN = '25'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME ='MAKTX'.
  WA_FIELDCAT-TABNAME ='IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S ='Material Description'.
  WA_FIELDCAT-SELTEXT_L ='Material Description'.
  WA_FIELDCAT-COL_POS   = 2.
  WA_FIELDCAT-OUTPUTLEN = '50'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME ='WERKS'.
  WA_FIELDCAT-TABNAME ='IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S ='Plant'.
  WA_FIELDCAT-SELTEXT_L ='Plant'.
  WA_FIELDCAT-COL_POS   = 3.
  WA_FIELDCAT-OUTPUTLEN = '5'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME ='STEUC'.
  WA_FIELDCAT-TABNAME ='IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S ='HSN Code'.
  WA_FIELDCAT-SELTEXT_L ='HSN Code'.
  WA_FIELDCAT-COL_POS   = 4.
  WA_FIELDCAT-OUTPUTLEN = '10'.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.


ENDFORM.                    " FIELDCAT
