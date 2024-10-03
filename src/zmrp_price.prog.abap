*&---------------------------------------------------------------------*
*& Report  ZMRP_PRICE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZMRP_PRICE.



TYPES : BEGIN OF GS_A055 ,
        "  KAPPL TYPE A055-KAPPL ,                               " APPPLICATION
          KSCHL TYPE A055-KSCHL ,                               " CONDITION TYPE
          VKORGAU TYPE A055-VKORGAU ,                           " SALES ORG OF ORDER
          WERKS TYPE A055-WERKS ,                               " PLANT
          MATNR TYPE A055-MATNR ,                               " MATERAIL
          DATBI TYPE A055-DATBI ,                               " VALID TO
          DATAB TYPE A055-DATAB ,                               " VALID FROM
          KNUMH TYPE A055-KNUMH ,                               " CONDION RECORD NO
        END OF GS_A055 .

DATA :  GT_A055 TYPE TABLE OF GS_A055 ,
        WA_A055 TYPE GS_A055 .

TYPES : BEGIN OF GS_MAKT ,
          MATNR TYPE MAKT-MATNR,                                " MATERIAL CODE
          MAKTX TYPE MAKT-MAKTX,                                " MATERIAL DESC
        END OF GS_MAKT .

DATA :  GT_MAKT TYPE TABLE OF GS_MAKT ,
        WA_MAKT TYPE GS_MAKT .

TYPES : BEGIN OF GS_KONP ,
          KNUMH TYPE KONP-KNUMH ,                               " COND.RECORD NO
          KSCHL TYPE KONP-KSCHL ,                               " COND.TYPE
          KBETR TYPE KONP-KBETR ,                               " AMOUNT
          KMEIN TYPE KONP-KMEIN ,                               " U.O.M
          LOEVM_KO TYPE KONP-LOEVM_KO  ,                         " DELETION FLAG
       END OF GS_KONP .

DATA : GT_KONP TYPE TABLE OF GS_KONP ,
       WA_KONP TYPE GS_KONP .

TYPES : BEGIN OF GS_MARM ,
           MATNR TYPE MARM-MATNR ,                               " MATERAIL
           MEINH TYPE MARM-MEINH ,                               " U.O.M
           UMREZ TYPE MARM-UMREZ ,                               " NUMERATOR
           UMREN TYPE MARM-UMREN ,                               " DENOMINTOR
        END OF GS_MARM .

DATA : GT_MARM TYPE TABLE OF GS_MARM ,
       WA_MARM TYPE GS_MARM .

TYPES : BEGIN OF GS_MARA,
        MATNR TYPE MARA-MATNR,
        MEINS TYPE MARA-MEINS,
        MTART TYPE MARA-MTART,
        END OF GS_MARA .

DATA : GT_MARA TYPE TABLE OF GS_MARA,
       WA_MARA TYPE GS_MARA .

TYPES : BEGIN OF GS_FINAL ,
            VKORGAU TYPE A055-VKORGAU ,                           " SALES ORG OF ORDER
            WERKS TYPE A055-WERKS ,                               " PLANT
            MATNR TYPE A055-MATNR ,                               " MATERAIL
            DATBI TYPE A055-DATBI ,                               " VALID TO
            DATAB TYPE A055-DATAB ,                               " VALID FROM
            KNUMH TYPE A055-KNUMH ,                               " CONDION RECORD NO
            MAKTX TYPE MAKT-MAKTX ,
            KBETR TYPE KONP-KBETR ,                               " AMOUNT
            KMEIN TYPE KONP-KMEIN ,                               " U.O.M
            MEINS TYPE MARA-MEINS,
            MEINH TYPE MARM-MEINH ,                               " U.O.M
            UMREN TYPE MARM-UMREN ,                               " DENOMINTOR
            V_COUNT TYPE I,
            TINPR TYPE P DECIMALS 3 ,
        END OF GS_FINAL .

DATA : GT_FINAL TYPE TABLE OF GS_FINAL ,
       WA_FINAL TYPE GS_FINAL .

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV ,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV ,
       GT_SORT TYPE SLIS_T_SORTINFO_ALV ,
       WA_SORT TYPE SLIS_SORTINFO_ALV .

DATA : LV_WERKS TYPE A055-WERKS ,
       LV_MATNR TYPE A055-MATNR .

DATA: LV_COUNT TYPE SY-TABIX.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001 .

SELECT-OPTIONS : SO_WERKS FOR LV_WERKS ,

                 SO_MATNR FOR LV_MATNR .

SELECTION-SCREEN : END OF BLOCK C1  .

START-OF-SELECTION .
  PERFORM GET_DATA .
  PERFORM READ_DATA .


  PERFORM FIELD_CATLOG .
  PERFORM ALV_DISPLAY .

END-OF-SELECTION .
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

*  SELECT
*   " KAPPL
*    KSCHL
*    VKORGAU
*    WERKS
*    MATNR
*    MAX( DATBI ) AS DATBI
*    MAX( DATAB ) AS DATAB
*    KNUMH FROM A055 INTO CORRESPONDING FIELDS OF TABLE GT_A055 WHERE KSCHL ='PI01' AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR GROUP BY WERKS MATNR KNUMH VKORGAU KSCHL  .

  SELECT
    " KAPPL
     KSCHL
     VKORGAU
     WERKS
     MATNR
     DATBI
     DATAB
     KNUMH FROM A055 INTO CORRESPONDING FIELDS OF TABLE GT_A055 WHERE KSCHL = 'PI01' AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR .

  SORT GT_A055 BY DATBI DESCENDING . " DESCENDING .

  IF GT_A055[] IS NOT INITIAL .
    SELECT
     KNUMH
     KSCHL
     KBETR
     KMEIN
     LOEVM_KO
        FROM KONP INTO TABLE GT_KONP FOR ALL ENTRIES IN GT_A055 WHERE KNUMH = GT_A055-KNUMH .

    SELECT
    MATNR
    MAKTX
       FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_A055 WHERE MATNR = GT_A055-MATNR .

    SELECT MATNR MEINS MTART FROM MARA INTO CORRESPONDING FIELDS OF TABLE GT_MARA FOR ALL ENTRIES IN GT_A055 WHERE MATNR = GT_A055-MATNR AND MTART = 'FERT' .

    SELECT
  MATNR
  MEINH
  UMREZ
  UMREN
     FROM MARM INTO CORRESPONDING FIELDS OF TABLE GT_MARM  FOR ALL ENTRIES IN GT_A055 WHERE MATNR = GT_A055-MATNR .

  ENDIF.

  SORT GT_A055 BY MATNR DATAB DATBI ." KNUMH .
  SORT GT_KONP BY KNUMH .
  SORT GT_MAKT BY MATNR .
  SORT GT_MARA BY MATNR .

ENDFORM.                    " GET_DATA
*
**&---------------------------------------------------------------------*
**&      Form  READ_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*


FORM READ_DATA .

  LOOP AT GT_A055 INTO WA_A055 .
    WA_FINAL-VKORGAU = WA_A055-VKORGAU .
    WA_FINAL-WERKS = WA_A055-WERKS .
    WA_FINAL-MATNR = WA_A055-MATNR .
    WA_FINAL-DATAB = WA_A055-DATAB .
    WA_FINAL-DATBI = WA_A055-DATBI .
    WA_FINAL-KNUMH = WA_A055-KNUMH .

    READ TABLE GT_KONP INTO WA_KONP WITH KEY KNUMH = WA_A055-KNUMH BINARY SEARCH  .

    WA_FINAL-KBETR = WA_KONP-KBETR .
    WA_FINAL-KMEIN  = WA_KONP-KMEIN .
    " WA_FINAL-TINPR = WA_KONP-KBETR .

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_A055-MATNR BINARY SEARCH .
    WA_FINAL-MAKTX = WA_MAKT-MAKTX .

    LOOP AT GT_MARA INTO WA_MARA WHERE MATNR = WA_A055-MATNR AND MTART = 'FERT' .
      WA_FINAL-MEINS = WA_MARA-MEINS.
    ENDLOOP .

*    READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_A055-MATNR BINARY SEARCH .
*    WA_FINAL-MEINS = WA_MARA-MEINS.

    LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_A055-MATNR  .
      WA_FINAL-MEINH = WA_MARM-MEINH .
      WA_FINAL-UMREN = WA_MARM-UMREN .
      WA_FINAL-TINPR = WA_KONP-KBETR .

    ENDLOOP.


* READ TABLE GT_MARM INTO WA_MARM WITH KEY MATNR = WA_A055-MATNR BINARY SEARCH .
*
*       WA_FINAL-MEINH = WA_MARM-MEINH .
*       WA_FINAL-UMREN = WA_MARM-UMREN .
*       WA_FINAL-TINPR = WA_KONP-KBETR .

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

  ENDLOOP .

  SORT GT_FINAL BY MATNR WERKS KNUMH DESCENDING .   "DATBI

  DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING MATNR WERKS.

  DELETE GT_FINAL WHERE MEINS = ' '.

  SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0' .

  LOOP AT GT_FINAL INTO WA_FINAL .
    IF WA_FINAL-KMEIN EQ 'EA' .

      CLEAR WA_FINAL-TINPR .

      LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_FINAL-MATNR AND MEINH EQ 'BOT' .  " MEINH <> 'BOT' .
        IF WA_FINAL-UMREN <> 0 .
          WA_FINAL-TINPR = WA_FINAL-KBETR / WA_MARM-UMREN .
          CLEAR WA_FINAL-UMREN .

        ENDIF .

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TINPR .
      ENDLOOP .
      CLEAR WA_FINAL .
    ENDIF .


    IF WA_FINAL-KMEIN EQ 'BOT'.                                                                            " ADDED BY MANI CHANGING MRP PER TIN FOR BOT UOM 02.04.2016

      LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_FINAL-MATNR .

        IF WA_MARM-MEINH = 'BOT'.


          WA_FINAL-KBETR = WA_FINAL-KBETR * WA_MARM-UMREN.

          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KBETR.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDLOOP .

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  " PERFORM ALV_LAYOUT USING 1 'Sales Office' 'VKORGAU' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Product Code' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Product Description' 'MAKTX' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Unit Of Measure' 'KMEIN' 'GT_FINAL' '' .
  " PERFORM ALV_LAYOUT USING 4 'DNP Per Case' 'KBETR' 'GT_FINAL' ''.
*BREAK-POINT.

    PERFORM ALV_LAYOUT USING 5 'MRP Per Case' 'KBETR' 'GT_FINAL' '' .

    PERFORM ALV_LAYOUT USING 6 'MRP Per Tin' 'TINPR' 'GT_FINAL' '' .


  PERFORM ALV_LAYOUT USING 1 'Factory' 'WERKS' 'GT_FINAL' '' .

  PERFORM ALV_LAYOUT USING 7 'Valid From' 'DATAB' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Valid To' 'DATBI' 'GT_FINAL' ''.

*
*  PERFORM ALV_LAYOUT USING 8 'SIZE' 'UMREN' 'GT_FINAL' '' .


*  WA_SORT-FIELDNAME = 'VKORGAU'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'WERKS'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

*  WA_SORT-FIELDNAME = 'KMEIN'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

  " PERFORM ALV_LAYOUT USING 10 'TOT.TIN' 'TINPR' 'GT_FINAL' '' .

ENDFORM.                    " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0312   text
*      -->P_0313   text
*      -->P_0314   text
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5.

  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  APPEND WA_FCAT TO GT_FCAT.
  CLEAR WA_FCAT.
ENDFORM.                    " ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
*         I_INTERFACE_CHECK                 = ' '
*         I_BYPASSING_BUFFER                = ' '
*         I_BUFFER_ACTIVE                   = ' '
 I_CALLBACK_PROGRAM                = SY-REPID
*         I_CALLBACK_PF_STATUS_SET          = ' '
*         I_CALLBACK_USER_COMMAND           = ' '
 I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*         I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*         I_CALLBACK_HTML_END_OF_LIST       = ' '
*         I_STRUCTURE_NAME                  =
*         I_BACKGROUND_ID                   = ' '
*         I_GRID_TITLE                      =
*         I_GRID_SETTINGS                   =
IS_LAYOUT                         = LAYOUT
IT_FIELDCAT                       = GT_FCAT[]
*         IT_EXCLUDING                      =
*         IT_SPECIAL_GROUPS                 =
IT_SORT                           = GT_SORT[]
*         IT_FILTER                         =
*         IS_SEL_HIDE                       =
*         I_DEFAULT                         = 'X'
*         I_SAVE                            = ' '
*         IS_VARIANT                        =
*         IT_EVENTS                         =
*         IT_EVENT_EXIT                     =
*         IS_PRINT                          =
*         IS_REPREP_ID                      =
*         I_SCREEN_START_COLUMN             = 0
*         I_SCREEN_START_LINE               = 0
*         I_SCREEN_END_COLUMN               = 0
*         I_SCREEN_END_LINE                 = 0
*         I_HTML_HEIGHT_TOP                 = 0
*         I_HTML_HEIGHT_END                 = 0
*         IT_ALV_GRAPHICS                   =
*         IT_HYPERLINK                      =
*         IT_ADD_FIELDCAT                   =
*         IT_EXCEPT_QINFO                   =
*         IR_SALV_FULLSCREEN_ADAPTER        =
*       IMPORTING
*         E_EXIT_CAUSED_BY_CALLER           =
*         ES_EXIT_CAUSED_BY_USER            =
TABLES
T_OUTTAB                          = GT_FINAL[]
*       EXCEPTIONS
*         PROGRAM_ERROR                     = 1
*         OTHERS                            = 2
      .
  IF SY-SUBRC <> 0.
*       Implement suitable error handling here
  ENDIF.


ENDFORM.                    " ALV_DISPLAY



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
  LS_LINE-INFO = 'PLANT WISE MRP PRICE' .
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
