FUNCTION ZBAPI_VENDOR_ST_ROL_RMPM.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_VENDOR_ROL STRUCTURE  ZSTR_STK_ROL_VEN_RMPM
*"----------------------------------------------------------------------

TYPES:BEGIN OF TY_MARC,
  MATNR TYPE MARC-MATNR,
  WERKS TYPE MARC-WERKS,
  MINBE TYPE MARC-MINBE,
  EISBE TYPE MARC-EISBE,
  TRAME TYPE MARC-TRAME,
  END OF TY_MARC.

  TYPES: BEGIN OF TY_MBEW,
    MATNR TYPE MBEW-MATNR,
    LBKUM TYPE MBEW-LBKUM,
    VERPR TYPE MBEW-VERPR,
    STPRS TYPE MBEW-STPRS,
    END OF TY_MBEW.

  TYPES: BEGIN OF TY_MARM,
    MATNR TYPE MARM-MATNR,
    UMREN TYPE MARM-UMREN,
    UMREZ TYPE MARM-UMREZ,
  END OF TY_MARM.


TYPES:BEGIN OF TY_MAKT,
    MATNR TYPE MARC-MATNR,
    MAKTG TYPE MAKT-MAKTG,
    END OF TY_MAKT.
  TYPES: BEGIN OF TY_MARA,
MATNR TYPE MARA-MATNR,
  MTART TYPE MARA-MTART,
  MATKL TYPE MARA-MATKL,
  MEINS TYPE MARA-MEINS,
 END OF TY_MARA.

  DATA:IT_MARC TYPE TABLE OF TY_MARC,
       WA_MARC TYPE TY_MARC,
       IT_MBEW TYPE TABLE OF TY_MBEW,
       WA_MBEW TYPE TY_MBEW,
       IT_MARM TYPE TABLE OF TY_MARM,
       WA_MARM TYPE TY_MARM,
       IT_MAKT TYPE TABLE OF TY_MAKT,
       WA_MAKT TYPE TY_MAKT,
       IT_MARA TYPE TABLE OF TY_MARA,
       WA_MARA TYPE TY_MARA.

SELECT
    MATNR
    MTART
    MATKL
    MEINS FROM MARA INTO TABLE IT_MARA WHERE MTART IN ('ROH','VERP').

  SELECT
    MATNR
    WERKS
    MINBE
    EISBE
    TRAME FROM MARC
INTO TABLE IT_MARC FOR ALL ENTRIES IN IT_MARA WHERE MATNR = IT_MARA-MATNR AND MINBE <> '0' AND PSTAT <> 'X' AND DISMM <> 'ND'.

  SELECT
    MATNR
 LBKUM
 VERPR
 STPRS FROM MBEW INTO TABLE IT_MBEW FOR ALL ENTRIES IN IT_MARC WHERE MATNR = IT_MARC-MATNR AND BWKEY = IT_MARC-WERKS.

  SELECT
    MATNR
UMREN  UMREZ FROM MARM INTO TABLE IT_MARM FOR ALL ENTRIES IN IT_MARC WHERE MATNR = IT_MARC-MATNR .
  SELECT
    MATNR
    MAKTG FROM MAKT INTO TABLE IT_MAKT FOR ALL ENTRIES IN IT_MARC WHERE MATNR = IT_MARC-MATNR .



  SORT IT_MARC ASCENDING BY MATNR WERKS.
  SORT IT_MBEW ASCENDING BY MATNR.
  SORT IT_MARM ASCENDING BY MATNR.
  SORT IT_MAKT ASCENDING BY MATNR.
  SORT IT_MARA ASCENDING BY MATNR.

  LOOP AT IT_MARC INTO WA_MARC.
  IT_VENDOR_ROL-MATNR = WA_MARC-MATNR.
  IT_VENDOR_ROL-WERKS = WA_MARC-WERKS.
  IT_VENDOR_ROL-MINBE = WA_MARC-MINBE.
 "_VENDOR_ROL-EISBE = WA_MARC-EISBE.
  IT_VENDOR_ROL-TRAME = WA_MARC-TRAME.
  READ TABLE IT_MBEW INTO WA_MBEW WITH KEY MATNR = WA_MARC-MATNR.
  IT_VENDOR_ROL-VERPR = WA_MBEW-VERPR.
  IT_VENDOR_ROL-STPRS = WA_MBEW-STPRS.
  IT_VENDOR_ROL-LBKUM = WA_MBEW-LBKUM.
  READ TABLE IT_MARM INTO WA_MARM WITH KEY MATNR = WA_MARC-MATNR.
  IT_VENDOR_ROL-UMREN = WA_MARM-UMREN.
  "T_VENDOR_ROL-UMREZ = WA_MARM-UMREZ.
  READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MARC-MATNR.
  IT_VENDOR_ROL-MAKTG = WA_MAKT-MAKTG.
  READ TABLE IT_MARA INTO WA_MARA WITH KEY MATNR = WA_MARC-MATNR.
  IT_VENDOR_ROL-MTART = WA_MARA-MTART.
  IT_VENDOR_ROL-MATKL = WA_MARA-MATKL.
  IT_VENDOR_ROL-MEINS = WA_MARA-MEINS.
  APPEND IT_VENDOR_ROL.
  CLEAR:WA_MAKT,WA_MARA,WA_MARC,WA_MARM,WA_MBEW.

ENDLOOP.





ENDFUNCTION.
