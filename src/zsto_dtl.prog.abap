*&---------------------------------------------------------------------*
*& Report  ZSTO_DTL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSTO_DTL.
TYPES : BEGIN OF GS_EKPO,
        MANDT TYPE EKPO-MANDT,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE EKPO-MATNR,
        BUKRS TYPE EKPO-BUKRS,
        WERKS TYPE EKPO-WERKS,
        KTMNG TYPE EKPO-KTMNG,
        MENGE TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
      END OF GS_EKPO.

DATA : GT_EKPO TYPE TABLE OF GS_EKPO,
       WA_EKPO TYPE GS_EKPO.

TYPES : BEGIN OF GS_EKKO ,
         EBELN TYPE EKKO-EBELN,
         BSART TYPE EKKO-BSART,
         SUP_RESWK TYPE EKKO-RESWK,
        END OF GS_EKKO.
DATA : GT_EKKO TYPE TABLE OF GS_EKKO,
       WA_EKKO TYPE GS_EKKO.

TYPES : BEGIN OF GS_EKBE,
        MANDT TYPE EKBE-MANDT,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        ZEKKN TYPE EKBE-ZEKKN,
        VGABE TYPE EKBE-VGABE,
        GJAHR TYPE EKBE-GJAHR,
        BELNR TYPE EKBE-BELNR,
        BUZEI TYPE EKBE-BUZEI,
        BEWTP TYPE EKBE-BEWTP,
        BUDAT TYPE EKBE-BUDAT,
        MENGE TYPE EKBE-MENGE,
        DMBTR TYPE EKBE-DMBTR,
        XBLNR TYPE EKBE-XBLNR,
        MATNR TYPE EKBE-MATNR,
        WERKS TYPE EKBE-WERKS,
        CHARG TYPE EKBE-CHARG,
        BLDAT TYPE EKBE-BLDAT,
        WAERS TYPE EKBE-WAERS,
        BWART TYPE EKBE-BWART,
      END OF GS_EKBE.

DATA : GT_EKBE TYPE TABLE OF GS_EKBE,
        WA_EKBE TYPE GS_EKBE.

TYPES : BEGIN OF GS_MKPF,
        MANDT TYPE MKPF-MANDT,
        MBLNR TYPE MKPF-MBLNR,
        MJAHR TYPE MKPF-MJAHR,
        BLART TYPE MKPF-BLART,
        BLDAT TYPE MKPF-BLDAT,
        BUDAT TYPE MKPF-BUDAT,
        BKTXT TYPE MKPF-BKTXT,
      END OF GS_MKPF.

DATA : GT_MKPF TYPE TABLE OF GS_MKPF,
          WA_MKPF TYPE GS_MKPF.

TYPES : BEGIN OF GS_MSEG,
        MANDT TYPE MSEG-MANDT,
        MBLNR TYPE MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        ZEILE TYPE MSEG-ZEILE,
        BWART TYPE MSEG-BWART,
        MATNR TYPE MSEG-MATNR,
        WERKS TYPE MSEG-WERKS,
        EBELN TYPE MSEG-EBELN,
        EBELP TYPE MSEG-EBELP,
        ZEKKN TYPE MSEG-ZEKKN,
      END OF GS_MSEG.

DATA : GT_MSEG TYPE TABLE OF GS_MSEG,
        WA_MSEG TYPE GS_MSEG.

TYPES : BEGIN OF GS_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
       END OF GS_T001W.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
        WA_T001W TYPE GS_T001W.

TYPES : BEGIN OF GS_MAKT ,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF GS_MAKT.

DATA : GT_MAKT TYPE TABLE OF GS_MAKT,
       WA_MAKT TYPE GS_MAKT.

TYPES : BEGIN OF GS_MARA,
          MATNR TYPE MARA-MATNR,
          VOLUM TYPE MARA-VOLUM,
          SPART TYPE MARA-SPART,
          BRGEW TYPE MARA-BRGEW,
        END OF GS_MARA .

DATA : GT_MARA TYPE TABLE OF GS_MARA,
       WA_MARA TYPE GS_MARA.

TYPES:BEGIN OF TY_MCH1,
  MATNR TYPE MCH1-MATNR,
  LICHA TYPE MCH1-LICHA,
  CHARG TYPE MCH1-CHARG,
  END OF TY_MCH1.

DATA : GT_MCH1 TYPE TABLE OF TY_MCH1,
     WA_MCH1 TYPE TY_MCH1.

TYPES : BEGIN OF GS_FINAL,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        ZEKKN TYPE EKBE-ZEKKN,
        VGABE TYPE EKBE-VGABE,
        GJAHR TYPE EKBE-GJAHR,
        BELNR TYPE EKBE-BELNR,
        BUZEI TYPE EKBE-BUZEI,
        BEWTP TYPE EKBE-BEWTP,
        BUDAT TYPE EKBE-BUDAT,
        MENGE TYPE EKBE-MENGE,
        DMBTR TYPE EKBE-DMBTR,
        XBLNR TYPE EKBE-XBLNR,
        MATNR TYPE EKBE-MATNR,
        WERKS TYPE EKBE-WERKS,
        CHARG TYPE EKBE-CHARG,
        BLDAT TYPE EKBE-BLDAT,
        MATNR1 TYPE EKPO-MATNR,
        BUKRS TYPE EKPO-BUKRS,
        WERKS1 TYPE EKPO-WERKS,
        KTMNG TYPE EKPO-KTMNG,
        MENGE1 TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
        NAME1 TYPE T001W-NAME1,
        NAME2 TYPE T001W-NAME1,
        MAKTX TYPE MAKT-MAKTX,
        VOLUM TYPE MARA-VOLUM,
        SPART TYPE MARA-SPART,
        TOT_VOL TYPE MARA-VOLUM,
        PO_HIS TYPE CHAR20,
        TRIP_NO TYPE ZFREIGHT_HEADER-TRIP_NO ,
        VENDOR_CODE TYPE ZFREIGHT_HEADER-VENDOR_CODE,
        VENDOR_NAME TYPE ZFREIGHT_HEADER-VENDOR_NAME,
        TO_LOC TYPE ZFREIGHT_HEADER-TO_LOC,
        VECHILE_NUMBER TYPE ZFREIGHT_HEADER-VECHILE_NUMBER,
        CRDATE TYPE ZFREIGHT_HEADER-CRDATE,
        CRTIME TYPE ZFREIGHT_HEADER-CRTIME,
        LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
        BKTXT TYPE MKPF-BKTXT,
        BRGEW TYPE MARA-BRGEW,
        WAERS TYPE EKBE-WAERS,
        BWART TYPE EKBE-BWART,
        LICHA TYPE MCH1-LICHA,
    END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : LV_REPLA TYPE EKPO-WERKS,
       LV_DATE TYPE EKBE-BUDAT,
       LV_MATNR TYPE EKBE-MATNR,
       LV_WERKS TYPE EKBE-WERKS.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  :  SO_DATE FOR  LV_DATE OBLIGATORY,
                   SO_WERKS FOR LV_WERKS,
                   SO_MATNR FOR LV_MATNR,
                   SO_REPLA FOR LV_REPLA.
SELECTION-SCREEN : END OF BLOCK C1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  IF SO_WERKS IS NOT INITIAL AND SO_REPLA IS NOT INITIAL.
    SELECT
                    A~MANDT
                    A~EBELN
                    A~EBELP
                    A~ZEKKN
                    A~VGABE
                    A~GJAHR
                    A~BELNR
                    A~BUZEI
                    A~BEWTP
                    A~BUDAT
                    A~MENGE
                    A~DMBTR
                    A~XBLNR
                    A~MATNR
                    A~WERKS
                    A~CHARG
                    A~BLDAT
                    A~WAERS
                A~BWART FROM EKBE AS A INNER JOIN EKKO AS B ON A~EBELN EQ B~EBELN INNER JOIN EKPO AS C ON A~EBELN EQ C~EBELN INTO TABLE GT_EKBE
                WHERE A~BUDAT IN SO_DATE AND A~MATNR IN SO_MATNR AND ( A~BEWTP = 'U' OR A~BEWTP = 'E' ) AND ( A~VGABE EQ '6' OR A~VGABE EQ '1' ) AND
                C~WERKS IN SO_REPLA AND B~RESWK IN SO_WERKS AND ( B~BSART EQ 'UB' OR B~BSART EQ 'ZUB' OR B~BSART EQ 'ZSTO' OR B~BSART EQ 'ZNB' OR B~BSART EQ 'ZIM') AND A~EBELP EQ C~EBELP.

  ELSEIF SO_WERKS IS NOT INITIAL.
    SELECT
         A~MANDT
                    A~EBELN
                    A~EBELP
                    A~ZEKKN
                    A~VGABE
                    A~GJAHR
                    A~BELNR
                    A~BUZEI
                    A~BEWTP
                    A~BUDAT
                    A~MENGE
                    A~DMBTR
                    A~XBLNR
                    A~MATNR
                    A~WERKS
                    A~CHARG
                    A~BLDAT
                    A~WAERS
                A~BWART FROM EKBE AS A INNER JOIN EKKO AS B ON A~EBELN EQ B~EBELN INTO TABLE GT_EKBE WHERE A~BUDAT IN SO_DATE
                AND  A~WERKS IN SO_WERKS AND A~MATNR IN SO_MATNR AND A~BEWTP = 'U'  AND A~VGABE EQ '6' AND ( B~BSART EQ 'UB' OR B~BSART EQ 'ZUB' OR B~BSART EQ 'ZSTO' OR B~BSART EQ 'ZNB' OR B~BSART EQ 'ZIM' ) .
  ELSEIF SO_REPLA IS NOT INITIAL.
    SELECT
    A~MANDT
    A~EBELN
    A~EBELP
    A~ZEKKN
    A~VGABE
    A~GJAHR
    A~BELNR
    A~BUZEI
    A~BEWTP
    A~BUDAT
    A~MENGE
    A~DMBTR
    A~XBLNR
    A~MATNR
    A~WERKS
    A~CHARG
    A~BLDAT
    A~WAERS
    A~BWART FROM EKBE AS A INNER JOIN EKKO AS B ON A~EBELN EQ B~EBELN INTO TABLE GT_EKBE WHERE A~BUDAT IN SO_DATE
     AND A~WERKS IN SO_REPLA AND A~MATNR IN SO_MATNR AND  A~BEWTP = 'E'  AND  A~VGABE EQ '1'
     AND ( B~BSART EQ 'UB' OR B~BSART EQ 'ZUB' OR B~BSART EQ 'ZSTO' OR B~BSART EQ 'ZNB' OR B~BSART EQ 'ZIM' ).
  ELSEIF SO_WERKS IS INITIAL AND SO_REPLA IS INITIAL.
    SELECT
                A~MANDT
                A~EBELN
                A~EBELP
                A~ZEKKN
                A~VGABE
                A~GJAHR
                A~BELNR
                A~BUZEI
                A~BEWTP
                A~BUDAT
                A~MENGE
                A~DMBTR
                A~XBLNR
                A~MATNR
                A~WERKS
                A~CHARG
                A~BLDAT
                A~WAERS
                A~BWART FROM EKBE AS A INNER JOIN EKKO AS B ON A~EBELN EQ B~EBELN INNER JOIN EKPO AS C ON A~EBELN EQ C~EBELN INTO TABLE GT_EKBE
                WHERE A~BUDAT IN SO_DATE AND A~MATNR IN SO_MATNR AND ( A~BEWTP = 'U' OR A~BEWTP = 'E' ) AND ( A~VGABE EQ '6' OR A~VGABE EQ '1' ) AND
                ( B~BSART EQ 'UB' OR B~BSART EQ 'ZUB' OR B~BSART EQ 'ZSTO' OR B~BSART EQ 'ZNB' OR B~BSART EQ 'ZIM' ) AND A~EBELP EQ C~EBELP.

  ENDIF.
  IF GT_EKBE IS NOT INITIAL.

    SELECT
            MANDT
            EBELN
            EBELP
            MATNR
            BUKRS
            WERKS
            KTMNG
            MENGE
            MEINS FROM EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN AND EBELP = GT_EKBE-EBELP." AND WERKS IN SO_REPLA .
    " AND XOBLR = 'X'.

    SELECT
          EBELN
          BSART
          RESWK
          FROM EKKO INTO TABLE GT_EKKO FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN.


    SELECT MATNR
           LICHA
           CHARG  FROM MCH1 INTO TABLE GT_MCH1 FOR ALL ENTRIES IN GT_EKBE WHERE MATNR = GT_EKBE-MATNR AND CHARG = GT_EKBE-CHARG .

    SELECT
         MANDT
         MBLNR
         MJAHR
         ZEILE
         BWART
         MATNR
         WERKS
         EBELN
         EBELP
         ZEKKN
           FROM MSEG INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKBE WHERE  MBLNR = GT_EKBE-BELNR AND MJAHR = GT_EKBE-GJAHR.
    "AND WERKS IN SO_REPLA .
    IF GT_MSEG IS NOT INITIAL.
      SELECT
            MANDT
            MBLNR
            MJAHR
            BLART
            BLDAT
            BUDAT
            BKTXT
              FROM MKPF INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_MSEG WHERE MBLNR = GT_MSEG-MBLNR AND MJAHR = GT_MSEG-MJAHR .
    ENDIF.

    SELECT
      WERKS
      NAME1 FROM T001W INTO TABLE GT_T001W ."FOR ALL ENTRIES IN GT_EKBE WHERE WERKS = GT_EKBE-WERKS.

    SELECT
       MATNR
       MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKBE WHERE MATNR = GT_EKBE-MATNR
        AND SPRAS = sy-langu .

    SELECT
       MATNR
       VOLUM
       SPART
       BRGEW FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_EKBE WHERE MATNR = GT_EKBE-MATNR .

  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

  LOOP AT GT_EKBE INTO WA_EKBE .
    WA_FINAL-BELNR = WA_EKBE-BELNR.
    WA_FINAL-BEWTP = WA_EKBE-BEWTP.
    WA_FINAL-BUDAT = WA_EKBE-BUDAT.
    WA_FINAL-XBLNR = WA_EKBE-XBLNR.
    WA_FINAL-MATNR = WA_EKBE-MATNR .
    WA_FINAL-CHARG = WA_EKBE-CHARG .
    WA_FINAL-BLDAT = WA_EKBE-BLDAT.
    WA_FINAL-EBELN = WA_EKBE-EBELN.
    WA_FINAL-MENGE = WA_EKBE-MENGE.
    WA_FINAL-DMBTR = WA_EKBE-DMBTR.
    WA_FINAL-EBELP  = WA_EKBE-EBELP.
    WA_FINAL-WAERS = WA_EKBE-WAERS.
    WA_FINAL-BWART = WA_EKBE-BWART.
    IF WA_FINAL-BWART = '102' OR WA_FINAL-BWART = '351' OR WA_FINAL-BWART = '641' OR WA_FINAL-BWART = '643'.
      WA_FINAL-DMBTR = WA_FINAL-DMBTR * -1.
      WA_FINAL-MENGE = WA_FINAL-MENGE * -1.
    ENDIF.
    IF WA_EKBE-BEWTP EQ 'U'.
      WA_FINAL-PO_HIS = 'Goods issue' .
    ENDIF.
    IF WA_EKBE-BEWTP EQ 'E'.
      WA_FINAL-PO_HIS = 'Goods receipt'.
    ENDIF.
    READ TABLE GT_MCH1 INTO WA_MCH1 WITH KEY MATNR = WA_EKBE-MATNR CHARG = WA_EKBE-CHARG.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-LICHA = WA_MCH1-LICHA.
    ENDIF.

    READ TABLE GT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKBE-EBELN .
    WA_FINAL-WERKS1 = WA_EKKO-SUP_RESWK.
    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS1.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-NAME1 = WA_T001W-NAME1.
    ENDIF.

    READ TABLE GT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_FINAL-EBELN EBELP = WA_FINAL-EBELP.
    WA_FINAL-WERKS = WA_EKPO-WERKS .
    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS .
    IF SY-SUBRC EQ 0 .
      WA_FINAL-NAME2 = WA_T001W-NAME1.
    ENDIF.
    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    ENDIF.

    READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_FINAL-MATNR.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-SPART = WA_MARA-SPART.
      WA_FINAL-TOT_VOL = WA_FINAL-MENGE * WA_MARA-VOLUM .
      WA_FINAL-BRGEW = WA_FINAL-MENGE * WA_MARA-BRGEW.
    ENDIF.

    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    READ TABLE GT_MSEG INTO WA_MSEG WITH KEY EBELN = WA_EKBE-EBELN EBELP = WA_EKBE-EBELP .
    READ TABLE GT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_MSEG-MBLNR MJAHR = WA_MSEG-MJAHR.
    IF WA_MKPF-BKTXT IS NOT INITIAL .
      WA_FINAL-BKTXT = WA_MKPF-BKTXT.
    ENDIF.
    IF WA_EKKO-EBELN EQ WA_FINAL-EBELN.
      APPEND WA_FINAL TO GT_FINAL.
      CLEAR: WA_MSEG , WA_EKKO,WA_MKPF,WA_MCH1.
    ENDIF.
    SORT GT_FINAL BY BUDAT BELNR ASCENDING.
    CLEAR WA_FINAL.
  ENDLOOP.

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

  PERFORM ALV_LAYOUT USING 1 'Sup.Plant' 'NAME1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Posting Date' 'BUDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Material Code' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Desc' 'MAKTX' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Doc.Date' 'BLDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Batch' 'CHARG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Reference' 'XBLNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Puchase Order History' 'PO_HIS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Rec Plant' 'NAME2' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Mat.Doc' 'BELNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 12 'Division' 'SPART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Puchase Order' 'EBELN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 14 'Qty' 'MENGE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 15 'Qty in Liter' 'TOT_VOL' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Amount in LC' 'DMBTR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Document Header Text' 'BKTXT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 18 'Gross Weight' 'BRGEW' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 19 'Currency' 'WAERS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 20 'Movement Type' 'BWART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 21 'Vendor Batch' 'LICHA' 'GT_FINAL' ''.


ENDFORM.                    " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0338   text
*      -->P_0339   text
*      -->P_0340   text
*      -->P_0341   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  APPEND WA_FCAT TO GT_FCAT.

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
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
     I_CALLBACK_PROGRAM             = SY-REPID
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     I_CALLBACK_USER_COMMAND        = ' '
       I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*     I_STRUCTURE_NAME               =
       IS_LAYOUT                     = LAYOUT
         IT_FIELDCAT                    = GT_FCAT[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
       IT_SORT                        = IT_SORT
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*         I_DEFAULT                      = 'X'
*         I_SAVE                         = ' '
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*     IR_SALV_LIST_ADAPTER           =
*     IT_EXCEPT_QINFO                =
*     I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
        TABLES
          T_OUTTAB                       = GT_FINAL[]
   EXCEPTIONS
     PROGRAM_ERROR                  = 1
     OTHERS                         = 2
                .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Stock Transfer Order Details' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.
ENDFORM.                    "ALV_CATALOG_HEADER
