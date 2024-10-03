*&---------------------------------------------------------------------*
*& Report ZMM_ME2L_DOC
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Ramachandaran M                    *
*& Developer                   : Mr.Manikandan T                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : DNP Price For Customer Material       *
*& Report Name                 : ZDNP_PRICE_CUST_MAT                   *
*& Development Id              : kpabap                                *
*& Related Information         : DNP Price For Customer Material       *
*&---------------------------------------------------------------------*

REPORT ZDNP_PRICE_CUST_MAT.

TYPES : BEGIN OF GS_A936,
        KSCHL TYPE A936-KSCHL,
        VKORG TYPE A936-VKORG,
        VTWEG TYPE A936-VTWEG,                  " DIS.CHANNAL
        WERKS TYPE A936-WERKS,
        MATNR TYPE A936-MATNR,
        DATBI TYPE A936-DATBI,
        DATAB TYPE A936-DATAB,
        KNUMH TYPE A936-KNUMH,
        END OF GS_A936,


        BEGIN OF GS_KONP,
        KNUMH TYPE KONP-KNUMH,
        KSCHL TYPE KONP-KSCHL,
        KBETR TYPE KONP-KBETR,
        KMEIN TYPE KONP-KMEIN,
        END OF GS_KONP,

        BEGIN OF GS_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
        END OF GS_MAKT,

       BEGIN OF GS_KNA1,
        KUNNR TYPE KNA1-KUNNR,
        NAME1 TYPE KNA1-NAME1,
        END OF GS_KNA1,


        BEGIN OF GS_MARM,
        MATNR TYPE MARM-MATNR,
        MEINH TYPE MARM-MEINH,
        UMREN TYPE MARM-UMREN,
        END OF GS_MARM,

        BEGIN OF GS_FINAL,
*         KSCHL TYPE A936-KSCHL,
*        VKORG TYPE A936-VKORG,
*        WERKS TYPE A936-WERKS,
*        MATNR TYPE A936-MATNR,
*        DATBI TYPE A936-DATBI,
*        DATAB TYPE A936-DATAB,
*        KNUMH TYPE A936-KNUMH,
        KAPPL TYPE A305-KAPPL,
        KSCHL TYPE A305-KSCHL,
        VKORG TYPE A305-VKORG,
        VTWEG TYPE A305-VTWEG,
        KUNNR TYPE A305-KUNNR,
        MATNR TYPE A305-MATNR,
        KFRST TYPE A305-KFRST,
        DATBI TYPE A305-DATBI,
        DATAB TYPE A305-DATAB,
        KBSTAT TYPE A305-KBSTAT,
        KNUMH TYPE A305-KNUMH,


        KBETR TYPE KONP-KBETR,
        KMEIN TYPE KONP-KMEIN,
        MAKTX TYPE MAKT-MAKTX,
        NAME1 TYPE KNA1-NAME1,
        MEINH TYPE MARM-MEINH ,
        UMREN TYPE MARM-UMREN,
        BKBETR TYPE KONP-KBETR,
        FUM TYPE STRING ,
        FINVA  TYPE P DECIMALS 3 ,
        DATE1 TYPE DATE,
         END OF GS_FINAL.


TYPES: BEGIN OF STR_A305,
        KAPPL TYPE A305-KAPPL,
        KSCHL TYPE A305-KSCHL,
        VKORG TYPE A305-VKORG,
        VTWEG TYPE A305-VTWEG,
        KUNNR TYPE A305-KUNNR,
        MATNR TYPE A305-MATNR,
        KFRST TYPE A305-KFRST,
        DATBI TYPE A305-DATBI,
        DATAB TYPE A305-DATAB,
        KBSTAT TYPE A305-KBSTAT,
        KNUMH TYPE A305-KNUMH,
      END OF STR_A305.



DATA:WA_A305 TYPE STR_A305,
      IT_A305 TYPE TABLE OF STR_A305.

DATA: GT_A936 TYPE TABLE OF GS_A936,
      WA_A936 TYPE GS_A936,
      GT_KONP TYPE TABLE OF GS_KONP,
      WA_KONP TYPE GS_KONP,
      GT_MAKT TYPE TABLE OF GS_MAKT,
      WA_MAKT TYPE GS_MAKT ,
      GT_KNA1 TYPE TABLE OF GS_KNA1,
      WA_KNA1 TYPE GS_KNA1 ,
      GT_MARM TYPE TABLE OF GS_MARM,
      WA_MARM TYPE GS_MARM,
      GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA : DATE1 TYPE DATE .

DATA : GT_FIELDCAT TYPE TABLE OF SLIS_FIELDCAT_ALV,
       WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_VKORG TYPE A305-VKORG,
      LV_VTWEG TYPE A305-VTWEG,
      LV_KUNNR TYPE A305-KUNNR ,
      LV_BEDAT TYPE A305-DATBI,
      LV_MATNR TYPE A305-MATNR .

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME .
SELECT-OPTIONS : SO_VKORG FOR LV_VKORG OBLIGATORY ,
                 SO_VTWEG FOR LV_VTWEG OBLIGATORY ,
                 SO_KUNNR FOR LV_KUNNR OBLIGATORY ,
                 SO_BEDAT FOR LV_BEDAT  NO INTERVALS NO-EXTENSION, "OBLIGATORY
                 SO_MATNR FOR LV_MATNR .

SELECTION-SCREEN : END OF BLOCK B1.

" DATE1  = SO_BEDAT-LOW   .

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
*  SELECT  KSCHL
*        VKORG
*        VTWEG
*        WERKS
*        MATNR
*       DATBI
*       DATAB
*       KNUMH FROM   A936 INTO CORRESPONDING FIELDS OF TABLE GT_A936 WHERE  VKORG IN SO_VKORG AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR .


  SELECT  KSCHL
          VKORG
          VTWEG
          KUNNR
          MATNR
          KFRST
          DATBI
          DATAB
          KBSTAT
          KNUMH    FROM   A305 INTO CORRESPONDING FIELDS OF TABLE IT_A305 WHERE  VKORG IN SO_VKORG  AND MATNR IN SO_MATNR AND VTWEG IN SO_VTWEG  AND KUNNR IN SO_KUNNR AND DATBI > SO_BEDAT-LOW .


  SORT IT_A305 BY KNUMH  DESCENDING .

  SORT IT_A305 BY KUNNR MATNR .


* KUNNR ADDED PR@$@TH ON 21/01
  DELETE ADJACENT DUPLICATES FROM IT_A305 COMPARING  MATNR KUNNR.

  " DELETE IT_A305 WHERE DATBI < SO_BEDAT .

  IF NOT IT_A305[] IS INITIAL.

    SELECT KNUMH
    KSCHL
    KBETR
    KMEIN   FROM KONP INTO CORRESPONDING FIELDS OF TABLE GT_KONP FOR ALL ENTRIES IN IT_A305 WHERE KNUMH = IT_A305-KNUMH  .

    SELECT MATNR
      MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN IT_A305 WHERE MATNR = IT_A305-MATNR.

        SELECT KUNNR
      NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN IT_A305 WHERE KUNNR = IT_A305-KUNNR.

    SELECT MATNR
      MEINH
      UMREN FROM MARM INTO TABLE GT_MARM FOR ALL ENTRIES IN IT_A305 WHERE MATNR = IT_A305-MATNR . " AND   MEINH = 'BOT'.
  ENDIF.


  SORT IT_A305 BY MATNR KNUMH.
  SORT GT_KONP  BY KNUMH.
  SORT GT_MAKT BY MATNR.
  SORT  GT_MARM BY MATNR.



  LOOP AT IT_A305 INTO WA_A305 .
    MOVE-CORRESPONDING WA_A305 TO WA_FINAL.
    READ TABLE GT_KONP INTO WA_KONP WITH KEY  KNUMH = WA_A305-KNUMH BINARY SEARCH.
    WA_FINAL-KBETR = WA_KONP-KBETR.
    WA_FINAL-KMEIN = WA_KONP-KMEIN.
    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_A305-MATNR BINARY SEARCH.
    WA_FINAL-MAKTX = WA_MAKT-MAKTX.
   " READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_A305-KUNNR BINARY SEARCH.
    LOOP AT GT_KNA1 INTO WA_KNA1 WHERE KUNNR = WA_A305-KUNNR .
    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    ENDLOOP.
    LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_A305-MATNR .
      WA_FINAL-MEINH = WA_MARM-MEINH .
      WA_FINAL-UMREN = WA_MARM-UMREN .

    ENDLOOP.
    WA_FINAL-FINVA = WA_KONP-KBETR .
    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR : WA_FINAL.
  ENDLOOP.

*  LOOP AT GT_FINAL INTO WA_FINAL .
*    WA_FINAL-FUM = 'EA' .
*    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FUM .
*  ENDLOOP .

  SORT GT_FINAL BY MATNR KNUMH DESCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING KUNNR MATNR  .
""""""""""""""""Commented by ram
*  LOOP AT GT_FINAL INTO WA_FINAL .
*    IF WA_FINAL-KMEIN NE 'EA' .
*      CLEAR WA_FINAL-FINVA .
*      LOOP AT GT_MARM INTO WA_MARM WHERE MATNR = WA_FINAL-MATNR AND MEINH = 'BOT' .
*        WA_FINAL-FINVA = WA_FINAL-KBETR * WA_MARM-UMREN .
*        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINVA .
*
*      ENDLOOP .
*      CLEAR WA_FINAL .
*    ENDIF .
*
*  ENDLOOP .

""""""""""""""""Commented by ram
*
*LOOP AT GT_FINAL INTO WA_FINAL.
* READ TABLE GT_MARM INTO WA_MARM WITH KEY MATNR = WA_FINAL-MATNR BINARY SEARCH.
*    WA_FINAL-UMREN = WA_MARM-UMREN.
*    WA_FINAL-BKBETR = WA_FINAL-KBETR /  WA_MARM-UMREN .
*MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING UMREN BKBETR.
*CLEAR WA_FINAL.
*ENDLOOP.

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
  WA_FIELDCAT-FIELDNAME   = 'VKORG'.
  WA_FIELDCAT-SELTEXT_M   = 'Sales Organization'.
  WA_FIELDCAT-COL_POS     = 1.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'VTWEG'.
  WA_FIELDCAT-SELTEXT_M   = 'Distribution Channel'.
  WA_FIELDCAT-COL_POS     = 2.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'KUNNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Customer'.
  WA_FIELDCAT-COL_POS     = 3.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
  WA_FIELDCAT-SELTEXT_M   = 'Customer Name'.
  WA_FIELDCAT-COL_POS     = 4.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'MATNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Product Code'.
  WA_FIELDCAT-COL_POS     = 5.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'MAKTX'.
  WA_FIELDCAT-SELTEXT_M   = 'Product Name'.
  WA_FIELDCAT-COL_POS     = 6.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'KMEIN'.
  WA_FIELDCAT-SELTEXT_M   = 'UOM'.
  WA_FIELDCAT-COL_POS     = 7.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'KBETR'.
  WA_FIELDCAT-SELTEXT_M   = 'DNP Price'.
  WA_FIELDCAT-COL_POS     = 9.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME   = 'FUM'.
*  WA_FIELDCAT-SELTEXT_M   = 'UOM'.
*  WA_FIELDCAT-COL_POS     = 10.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*
*  WA_FIELDCAT-FIELDNAME   = 'FINVA'.
*  WA_FIELDCAT-SELTEXT_M   = 'DNP PER Price in Case'.
*  WA_FIELDCAT-COL_POS     = 11.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*  WA_FIELDCAT-FIELDNAME   = 'WERKS'.
*  WA_FIELDCAT-SELTEXT_M   = 'FACTORY'.
*  WA_FIELDCAT-COL_POS     = 8.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'DATAB'.
  WA_FIELDCAT-SELTEXT_M   = 'Valid From'.
  WA_FIELDCAT-COL_POS     = 12.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'DATBI'.
  WA_FIELDCAT-SELTEXT_M   = 'Valid To'.
  WA_FIELDCAT-COL_POS     = 13.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

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
*   I_CALLBACK_TOP_OF_PAGE            = ' '
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
