*&---------------------------------------------------------------------*
*& Report  ZDNP_PRICE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZUOM_REPORT.

TYPES : BEGIN OF GS_MARA,
        MATNR TYPE MARA-MATNR,
        ERNAM TYPE MARA-ERNAM,
        MTART TYPE MARA-MTART,
        MEINS TYPE MARA-MEINS,
        END OF GS_MARA,

        BEGIN OF GS_MARC,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
        END OF GS_MARC,

        BEGIN OF GS_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
        END OF GS_MAKT,

        BEGIN OF GS_MARM,
        MATNR TYPE MARM-MATNR,
        MEINH TYPE MARM-MEINH,
        UMREZ TYPE MARM-UMREZ,
        UMREN TYPE MARM-UMREN,
        END OF GS_MARM,

        BEGIN OF GS_FINAL,
        MATNR TYPE MARA-MATNR,
        ERNAM TYPE MARA-ERNAM,
        MTART TYPE MARA-MTART,
        MEINS TYPE MARA-MEINS,
        WERKS TYPE MARC-WERKS,
        MAKTX TYPE MAKT-MAKTX,
        MEINH TYPE MARM-MEINH ,
        UMREZ TYPE MARM-UMREZ,
        UMREN TYPE MARM-UMREN,
        BKBETR TYPE KONP-KBETR,
        FUM TYPE STRING ,
        FINVA  TYPE P DECIMALS 3 ,
        END OF GS_FINAL.

DATA: GT_MARA TYPE TABLE OF GS_MARA,
      WA_MARA TYPE GS_MARA,
      GT_MARC TYPE TABLE OF GS_MARC,
      WA_MARC TYPE GS_MARC,
      GT_MAKT TYPE TABLE OF GS_MAKT,
      WA_MAKT TYPE GS_MAKT ,
      GT_MARM TYPE TABLE OF GS_MARM,
      WA_MARM TYPE GS_MARM,
      GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA : GT_FIELDCAT TYPE TABLE OF SLIS_FIELDCAT_ALV,
       WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_WERKS TYPE MARC-WERKS,
       LV_MATNR TYPE MARA-MATNR ,
       LV_MTART TYPE MARA-MTART.


SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME .
SELECT-OPTIONS : SO_WERKS FOR LV_WERKS OBLIGATORY ,
                 SO_MATNR FOR LV_MATNR ,
                 SO_MTART FOR LV_MTART .

SELECTION-SCREEN : END OF BLOCK B1.



*----------------------------------------------------------------------*
*       CLASS PRICE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS PRICE DEFINITION.
  PUBLIC SECTION.
    METHODS : GET_DATA,
              ALV_FIELDCAT,
              DISPLAY_DATA.
ENDCLASS.                    "PRICE DEFINITION

*----------------------------------------------------------------------*
*       CLASS PRICE IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS PRICE IMPLEMENTATION.
  METHOD : GET_DATA.
    PERFORM GET_DATA.
  ENDMETHOD.                    ":
  METHOD: ALV_FIELDCAT.
    PERFORM ALV_FIELDCAT.
  ENDMETHOD.                    "ALV_FIELDCAT
  METHOD DISPLAY_DATA.
    PERFORM DISPLAY_DATA.
  ENDMETHOD.                    "DISPLAY_DATA
ENDCLASS.                    "PRICE IMPLEMENTATION

START-OF-SELECTION.
  DATA : PRICE TYPE REF TO PRICE  .

  CREATE OBJECT PRICE.

  CALL METHOD PRICE->GET_DATA.
  CALL METHOD PRICE->ALV_FIELDCAT.
  CALL METHOD PRICE->DISPLAY_DATA.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  IF SO_MTART IS INITIAL .
    SELECT
    MATNR
    WERKS
      FROM MARC INTO CORRESPONDING FIELDS OF TABLE GT_MARC WHERE MATNR IN SO_MATNR AND WERKS IN SO_WERKS . " AND MATNR = GT_MARA-MATNR.

    SELECT
MATNR
ERNAM
MTART
MEINS
   FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_MARC WHERE MATNR = GT_MARC-MATNR AND MTART IN SO_MTART .
  ELSE .

    SELECT
    MATNR
    ERNAM
    MTART
    MEINS
    FROM MARA INTO TABLE GT_MARA  WHERE MTART IN SO_MTART .


    SELECT
    MATNR
    WERKS
    FROM MARC INTO  TABLE GT_MARC
      FOR ALL ENTRIES IN GT_MARA
       WHERE MATNR = GT_MARA-MATNR AND MATNR IN SO_MATNR AND WERKS IN SO_WERKS . " AND MATNR = GT_MARA-MATNR.


  ENDIF.


  SELECT MATNR
    MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARC WHERE MATNR = GT_MARC-MATNR.

  SELECT MATNR
    MEINH
    UMREZ
    UMREN FROM MARM INTO TABLE GT_MARM FOR ALL ENTRIES IN GT_MARC WHERE MATNR = GT_MARC-MATNR . " AND   MEINH = 'BOT'.


  SORT GT_MARA BY MATNR .

  LOOP AT GT_MARC INTO WA_MARC .
    MOVE-CORRESPONDING WA_MARC TO WA_FINAL.
    READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_MARC-MATNR BINARY SEARCH.
    WA_FINAL-ERNAM = WA_MARA-ERNAM.
    WA_FINAL-MTART = WA_MARA-MTART.
    WA_FINAL-MEINS = WA_MARA-MEINS.
    "  READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MARC-MATNR BINARY SEARCH.
    LOOP AT GT_MAKT INTO WA_MAKT WHERE MATNR = WA_MARC-MATNR .
      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    ENDLOOP.
    LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_MARC-MATNR .
      WA_FINAL-MEINH = WA_MARM-MEINH .
      WA_FINAL-UMREZ = WA_MARM-UMREZ .
      WA_FINAL-UMREN = WA_MARM-UMREN .

      SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
      APPEND WA_FINAL TO GT_FINAL.
      CLEAR : WA_FINAL.

    ENDLOOP.

  ENDLOOP.



  " LOOP AT GT_MARM INTO WA_MARM .
  " MOVE-CORRESPONDING WA_MARM TO WA_FINAL .
  " IF GT_FINAL IS NOT INITIAL .
*  LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_FINAL-MATNR .
*     WA_FINAL-MEINH = WA_MARM-MEINH .
*     WA_FINAL-UMREZ = WA_MARM-UMREZ .
*     WA_FINAL-UMREN = WA_MARM-UMREN .
*
*     " ENDLOOP.
*"     APPEND WA_FINAL TO GT_FINAL.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING MEINH UMREZ UMREN .
*          CLEAR WA_FINAL .
*  "        ENDIF.
*     ENDLOOP.


*LOOP AT GT_FINAL INTO WA_FINAL .
*  IF GT_FINAL IS NOT INITIAL .
*  LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_FINAL-MATNR .
*     WA_FINAL-MEINH = WA_MARM-MEINH .
*     WA_FINAL-UMREZ = WA_MARM-UMREZ .
*     WA_FINAL-UMREN = WA_MARM-UMREN .
*
*     ENDLOOP.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING MEINH UMREZ UMREN .
*          CLEAR WA_FINAL .
*          ENDIF.
*     ENDLOOP.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT .
  WA_FIELDCAT-FIELDNAME   = 'WERKS'.
  WA_FIELDCAT-SELTEXT_M   = 'PLANT'.
  WA_FIELDCAT-COL_POS     = 1.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'MATNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Product Code'.
  WA_FIELDCAT-COL_POS     = 2.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'MAKTX'.
  WA_FIELDCAT-SELTEXT_M   = 'Product Name'.
  WA_FIELDCAT-COL_POS     = 3.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'MTART'.
  WA_FIELDCAT-SELTEXT_M   = 'Material Type'.
  WA_FIELDCAT-COL_POS     = 4.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'MEINS'.
  WA_FIELDCAT-SELTEXT_M   = 'Base Unit'.
  WA_FIELDCAT-COL_POS     = 5.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'MEINH' .
  WA_FIELDCAT-SELTEXT_M   = 'Alternative unit'.
  WA_FIELDCAT-COL_POS     = 6.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'UMREZ'.
  WA_FIELDCAT-SELTEXT_M   = 'Numerator'.
  WA_FIELDCAT-COL_POS     = 7.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'UMREN'.
  WA_FIELDCAT-SELTEXT_M   = 'Denominator'.
  WA_FIELDCAT-COL_POS     = 8.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ERNAM'.
  WA_FIELDCAT-SELTEXT_M   = 'Created By'.
  WA_FIELDCAT-COL_POS     = 9.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'DATBI'.
*  WA_FIELDCAT-SELTEXT_M   = 'Valid To'.
*  WA_FIELDCAT-COL_POS     = 10.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME   = 'MEINH'.
*  WA_FIELDCAT-SELTEXT_M   = 'ALTER MEA'.
*  WA_FIELDCAT-COL_POS     = 12.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'UMREN'.
*  WA_FIELDCAT-SELTEXT_M   = 'DENOMETER'.
*  WA_FIELDCAT-COL_POS     = 11.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


* WA_FIELDCAT-FIELDNAME   = 'UMREN'.
*  WA_FIELDCAT-SELTEXT_M   = 'UMREN'.
*  WA_FIELDCAT-COL_POS     = 9.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*
*  WA_FIELDCAT-FIELDNAME   = 'DATAB'.
*  WA_FIELDCAT-SELTEXT_M   = 'Valid To'.
*  WA_FIELDCAT-COL_POS     = 3.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
ENDFORM.                    " ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                =  SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
   I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = GT_FIELDCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = GT_FINAL
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_DATA

*---------------------------------------------------
*&      Form  ALV_CATALOG_HEADER
*&-------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Material Unit of Measure Report' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.
*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BUDAT_MKPF
*    IMPORTING
*      OUTPUT = LV_BUDAT_MKPF.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
