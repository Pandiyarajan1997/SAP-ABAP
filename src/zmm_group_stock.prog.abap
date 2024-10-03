*&---------------------------------------------------------------------*
*& Report  ZMM_GROUP_STOCK
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  : 12 Aug 2014                           *
*& Title                       : Material Stock Details                *
*& Report Name                 : ZMM_GROUP_STOCK                       *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Display The Stock Details By          *
*                                Material Group                        *
*&---------------------------------------------------------------------*

REPORT  ZMM_GROUP_STOCK.

TYPE-POOLS : SLIS.

*&---------------------------------------------------------------------*
*&  Structure & Internal Table Decleration
*&---------------------------------------------------------------------*

TYPES: BEGIN OF GS_MARA,
       MATNR TYPE MARA-MATNR,                 " Material Number
       MATKL TYPE MARA-MATKL,                 " Material Group
       MTART TYPE MARA-MTART,                 " Material Type
       END OF GS_MARA.

DATA: GT_MARA TYPE TABLE OF GS_MARA,
      WA_MARA TYPE GS_MARA.

TYPES: BEGIN OF GS_T023T,
       MATKL TYPE T023T-MATKL,                " Material Group
       WGBEZ TYPE T023T-WGBEZ,                " Material Group Description
       SPRAS TYPE T023T-SPRAS,                " Language
       END OF GS_T023T.

DATA: GT_T023T TYPE TABLE OF GS_T023T,
      WA_T023T TYPE GS_T023T.

TYPES : BEGIN OF GS_MAKT,
        MATNR   TYPE MAKT-MATNR,
       MAKTX   TYPE MAKT-MAKTX,
      END OF GS_MAKT.

DATA : GT_MAKT  TYPE TABLE OF GS_MAKT,
      WA_MAKT   TYPE GS_MAKT.


TYPES: BEGIN OF GS_MSEG,
       MBLNR TYPE MSEG-MBLNR,                 " Number of Material Document
       ZEILE TYPE MSEG-ZEILE,                 " Item in Material Document
       MATNR TYPE MSEG-MATNR,                 " Material Number
       BWART TYPE MSEG-BWART,                 " Movement Type (Inventory Management)
       WERKS TYPE MSEG-WERKS,                 " Plant
       CHARG TYPE MSEG-CHARG,                 " Batch
       LGORT TYPE MSEG-LGORT,                 " Storage Location
       MENGE TYPE MSEG-MENGE,                 " Quantity
       MEINS TYPE MSEG-MEINS,                 " Base Unit of Measure
       SHKZG TYPE MSEG-SHKZG,                 " Debit/Credit Indicator
       SOBKZ TYPE MSEG-SOBKZ,                 " Special Stock Indicator
       SALK3 TYPE MSEG-SALK3,                 " Value of total valuated stock before the posting
       DMBTR TYPE MSEG-DMBTR,                 " Amount in Local Currency
      END OF GS_MSEG.

DATA: GT_MSEG TYPE TABLE OF GS_MSEG,
      WA_MSEG TYPE GS_MSEG.

TYPES: BEGIN OF GS_MKPF,
       MBLNR TYPE MKPF-MBLNR,                 " Number of Material Document
       MJAHR TYPE MKPF-MJAHR,                 " Year
       BUDAT TYPE MKPF-BUDAT,                 " Posting Date
       END OF GS_MKPF.

DATA: GT_MKPF TYPE TABLE OF GS_MKPF,
      WA_MKPF TYPE GS_MKPF.

TYPES: BEGIN OF GS_FINAL,
       MATNR TYPE MARA-MATNR,                 " Material Number
       MAKTX TYPE MAKT-MAKTX,                 " Material Description
       BUDAT TYPE MKPF-BUDAT,                 " Posting Date
       CHARG TYPE MSEG-CHARG,                 " Batch
       MATKL TYPE MARA-MATKL,                 " Material Group
       WGBEZ TYPE T023T-WGBEZ,                " Material Group Description
       MEINS TYPE MSEG-MEINS,                 " UOM
       OS_QUAN TYPE P DECIMALS 2,             " Opening Stock Quantity
       OS_AMNT TYPE P DECIMALS 2,             " Opening Stock Amount
       RE_QUAN TYPE P DECIMALS 2,             " Recipt Stock Quantity
       RE_AMNT TYPE P DECIMALS 2,             " Recipt Stock Amount
       MS_QUAN TYPE P DECIMALS 2,             " Material Sales Quantity
       MS_AMNT TYPE P DECIMALS 2,             " Material Sales Amount
       GI_QUAN TYPE P DECIMALS 2,             " Issues Stock Quantity
       GI_AMNT TYPE P DECIMALS 2,             " Issues Stock Amount
       CS_QUAN TYPE P DECIMALS 2,             " Closing Stock Quantity
       CS_AMNT TYPE P DECIMALS 2,             " Closing Stock Amount
       CO_RATE TYPE P DECIMALS 2,             " Consumption Rate
       A1_QTY TYPE P DECIMALS 2,
       A2_QTY TYPE P DECIMALS 2,
       A3_QTY TYPE P DECIMALS 2,
       A4_QTY TYPE P DECIMALS 2,
       A5_QTY TYPE P DECIMALS 2,
       A6_QTY TYPE P DECIMALS 2,
       A7_QTY TYPE P DECIMALS 2,
       A8_QTY TYPE P DECIMALS 2,
       A9_QTY TYPE P DECIMALS 2,
       A10_QTY TYPE P DECIMALS 2,
       A11_QTY TYPE P DECIMALS 2,
       A12_QTY TYPE P DECIMALS 2,
       END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA : T_DAYS TYPE I,
      LV_ZFAEDT TYPE SY-DATUM.

DATA: OR_MATKL TYPE T023-MATKL,
      OR_BUDAT TYPE MKPF-BUDAT.

DATA: F_MONTH TYPE BKPF-MONAT,
      LV_YEAR(4) TYPE C.

DATA: RE_MENGE TYPE P DECIMALS 2,
      RE_AMOUNT TYPE P DECIMALS 3,
      GI_MENGE TYPE P DECIMALS 2,
      GI_AMOUNT TYPE P DECIMALS 3,
      OS_MENGE TYPE P DECIMALS 2,
      OS_AMOUNT TYPE P DECIMALS 3,
      CS_MENGE TYPE P DECIMALS 2,
      CS_AMOUNT TYPE P DECIMALS 3,
      MS_MENGE TYPE P DECIMALS 2,
      MS_AMOUNT TYPE P DECIMALS 3.

DATA: RE_STOCK TYPE P DECIMALS 2,
      GI_STOCK TYPE P DECIMALS 2,
      MS_STOCK TYPE P DECIMALS 2,
      RE_OPAMUT TYPE P DECIMALS 2,
      GI_OPAMUT TYPE P DECIMALS 2,
      MS_OPAMUT TYPE P DECIMALS 2.

DATA: VALUE_415 TYPE P DECIMALS 2,
      WA_MSEG1 TYPE GS_MSEG,
      FLAG TYPE I.

** Declaration for ALV Grid **

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      V_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT.

DATA: LS_VARIANT TYPE DISVARIANT.

LS_VARIANT-REPORT = SY-REPID.

DATA: OR_MATNR TYPE MARA-MATNR,
      OR_WERKS TYPE T001W-WERKS,
      OR_MTART TYPE T134-MTART.

*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_WERKS FOR OR_WERKS,
                 SO_MATNR FOR OR_MATNR,
                 SO_MTART FOR OR_MTART.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_DATE TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B2.
*&---------------------------------------------------------------------*
*&  Main Logic Of Program
*&---------------------------------------------------------------------*





  SELECT
    MATNR
    MATKL
    MTART FROM MARA INTO CORRESPONDING FIELDS OF TABLE GT_MARA
    WHERE MTART IN SO_MTART AND
          MATNR IN SO_MATNR.



IF GT_MARA IS NOT INITIAL.
  SELECT MATNR MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR.
ENDIF.
IF GT_MARA[] IS NOT INITIAL.

  SELECT
    MBLNR
    ZEILE
    MATNR
    BWART
    WERKS
    CHARG
    LGORT
        MENGE
    MEINS
    SHKZG
    SOBKZ
    SALK3
    DMBTR FROM MSEG
    INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_MARA
    WHERE MATNR = GT_MARA-MATNR AND
          WERKS IN SO_WERKS AND
          BWART IN ('101','102', '122', '123', '161','162', '105', '201', '202', '221', '222', '261', '262', '231', '232','251', '252',
                    '106', '301', '302', '501', '502', '543', '544', '553', '554', '561', '562', '641','642','413', '414', '601','602').

          " BWART NOT IN ('321','311', '541', '542', '103','104', '503').



  IF GT_MSEG[] IS NOT INITIAL.

    SELECT
      MBLNR
      MJAHR
      BUDAT FROM MKPF
      INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_MSEG
      WHERE MBLNR = GT_MSEG-MBLNR AND
            BUDAT <= P_DATE.
  ENDIF.
ENDIF.

SORT GT_MSEG BY BWART.

*  LOOP AT GT_MARA INTO WA_MARA .
*    WA_FINAL-MATNR = WA_MARA-MATNR.

    LOOP AT GT_MSEG INTO WA_MSEG. "WHERE MATNR = WA_MARA-MATNR.
       WA_FINAL-CHARG = WA_MSEG-CHARG.
       WA_FINAL-MATNR = WA_MSEG-MATNR.

       READ TABLE GT_MAKT INTO WA_MAKT WITH  KEY MATNR = WA_MSEG-MATNR.
       WA_FINAL-MAKTX = WA_MAKT-MAKTX.

      READ TABLE GT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_MSEG-MBLNR.
      IF SY-SUBRC = 0.

        WA_FINAL-BUDAT = WA_MKPF-BUDAT.
        IF WA_MKPF-BUDAT = P_DATE.
          IF WA_MSEG-BWART = '101' OR WA_MSEG-BWART = '123' OR WA_MSEG-BWART = '162' OR WA_MSEG-BWART = '105' OR
             WA_MSEG-BWART = '501' OR WA_MSEG-BWART = '642' OR WA_MSEG-BWART = '561' OR WA_MSEG-BWART = '302'.
            RE_MENGE = RE_MENGE + WA_MSEG-MENGE.
            RE_AMOUNT = RE_AMOUNT + WA_MSEG-DMBTR.

          ELSEIF WA_MSEG-BWART = '102' OR WA_MSEG-BWART = '122' OR WA_MSEG-BWART = '161' OR WA_MSEG-BWART = '106' OR
                 WA_MSEG-BWART = '502' OR WA_MSEG-BWART = '641' OR WA_MSEG-BWART = '562' OR WA_MSEG-BWART = '301'.
            RE_MENGE = RE_MENGE - WA_MSEG-MENGE.
            RE_AMOUNT = RE_AMOUNT - WA_MSEG-DMBTR.

          ELSEIF WA_MSEG-BWART = '201' OR WA_MSEG-BWART = '221' OR WA_MSEG-BWART = '261'  OR WA_MSEG-BWART = '543' OR WA_MSEG-BWART = '553'.
            GI_MENGE = GI_MENGE + WA_MSEG-MENGE.
            GI_AMOUNT = GI_AMOUNT + WA_MSEG-DMBTR.
          ELSEIF WA_MSEG-BWART = '202' OR WA_MSEG-BWART = '222' OR WA_MSEG-BWART = '262' OR WA_MSEG-BWART = '544' OR WA_MSEG-BWART = '554'.
            GI_MENGE = GI_MENGE - WA_MSEG-MENGE.
            GI_AMOUNT = GI_AMOUNT - WA_MSEG-DMBTR.

          ELSEIF WA_MSEG-BWART = '231' OR WA_MSEG-BWART = '251' OR WA_MSEG-BWART = '413' OR WA_MSEG-BWART = '601'.
            MS_MENGE = MS_MENGE + WA_MSEG-MENGE.
            MS_AMOUNT = MS_AMOUNT + WA_MSEG-DMBTR.
          ELSEIF WA_MSEG-BWART = '232' OR WA_MSEG-BWART = '252' OR WA_MSEG-BWART = '414' OR WA_MSEG-BWART = '602'.
            MS_MENGE = MS_MENGE - WA_MSEG-MENGE.
            MS_AMOUNT = MS_AMOUNT - WA_MSEG-DMBTR.
          ENDIF.

        ELSEIF WA_MKPF-BUDAT < P_DATE.

          IF WA_MSEG-SHKZG = 'S'.
            RE_STOCK = RE_STOCK + WA_MSEG-MENGE.
            RE_OPAMUT = RE_OPAMUT + WA_MSEG-DMBTR.
          ELSEIF WA_MSEG-SHKZG = 'H'.
            GI_STOCK = GI_STOCK + WA_MSEG-MENGE.
            GI_OPAMUT = GI_OPAMUT + WA_MSEG-DMBTR.
          ENDIF.

        ENDIF.
        CLEAR WA_MKPF.
      ENDIF.
      CLEAR WA_MSEG.
*    ENDLOOP.
    CLEAR WA_MARA.
*  ENDLOOP.

*  WA_FINAL-MATKL = WA_T023T-MATKL.
*  WA_FINAL-WGBEZ = WA_T023T-WGBEZ.
  WA_FINAL-OS_QUAN = RE_STOCK - GI_STOCK.
  WA_FINAL-OS_AMNT = RE_OPAMUT - GI_OPAMUT.
  WA_FINAL-RE_QUAN = RE_MENGE.
  WA_FINAL-RE_AMNT = RE_AMOUNT.
  WA_FINAL-MS_QUAN = MS_MENGE.
  WA_FINAL-MS_AMNT = MS_AMOUNT.
  WA_FINAL-GI_QUAN = GI_MENGE.
  WA_FINAL-GI_AMNT = GI_AMOUNT.
  WA_FINAL-CS_QUAN = ( WA_FINAL-OS_QUAN + RE_MENGE ) - ( MS_MENGE + GI_MENGE ).
  WA_FINAL-CS_AMNT = ( WA_FINAL-OS_AMNT + RE_AMOUNT ) - ( MS_AMOUNT + GI_AMOUNT ).
  IF WA_FINAL-CS_QUAN IS NOT INITIAL.
    WA_FINAL-CO_RATE = WA_FINAL-CS_AMNT / WA_FINAL-CS_QUAN.
  ENDIF.

   LV_ZFAEDT = WA_FINAL-BUDAT.

     CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = LV_ZFAEDT
            ENDDA = P_DATE
          IMPORTING
            DAYS  = T_DAYS.



        IF T_DAYS BETWEEN 0 AND 30  .
*
            WA_FINAL-A1_QTY = WA_FINAL-A1_QTY + WA_FINAL-CS_QUAN.
*
        ELSEIF T_DAYS BETWEEN 31 AND 40  .

            WA_FINAL-A2_QTY = WA_FINAL-A2_QTY + WA_FINAL-CS_QUAN.

       ELSEIF T_DAYS BETWEEN   41  AND 60.

            WA_FINAL-A3_QTY = WA_FINAL-A3_QTY + WA_FINAL-CS_QUAN.

        ELSEIF T_DAYS BETWEEN 61 AND 90 .

            WA_FINAL-A4_QTY = WA_FINAL-A4_QTY + WA_FINAL-CS_QUAN.

       ELSEIF T_DAYS > 90 .

            WA_FINAL-A5_QTY = WA_FINAL-A5_QTY + WA_FINAL-CS_QUAN.
        ENDIF.

*        IF T_DAYS BETWEEN 8 AND 21 .
*                      A2_AMNT = A2_AMNT + WA_BSIK-DMBTR.
*                    ENDIF.
*        IF T_DAYS BETWEEN 22 AND 30 .
*
*            A3_AMNT = A3_AMNT + WA_BSIK-DMBTR.
*
*        ENDIF.



  APPEND WA_FINAL TO GT_FINAL.

  CLEAR: WA_FINAL,WA_T023T,
         OS_MENGE, OS_AMOUNT,
         RE_MENGE, RE_AMOUNT,
         MS_MENGE, MS_AMOUNT,
         GI_MENGE, GI_AMOUNT,
         RE_STOCK, RE_OPAMUT,
         GI_STOCK, GI_OPAMUT,
         MS_STOCK, MS_OPAMUT.
ENDLOOP.
*
*LOOP AT GT_FINAL INTO WA_FINAL WHERE CHARG = WA_FINAL-CHARG.



*&---------------------------------------------------------------------*
*&  Define ALV Layout
*&---------------------------------------------------------------------*

PERFORM ALV_LAYOUT USING 1 'Material Code' 'MATNR' 'GT_FINAL' ' '.
PERFORM ALV_LAYOUT USING 2 'Material  Discription' 'MAKTX' 'GT_FINAL' ' '.
PERFORM ALV_LAYOUT USING 3 'Batch' 'CHARG' 'GT_FINAL' ' '.
PERFORM ALV_LAYOUT USING 4 'Posting Date' 'BUDAT' 'GT_FINAL' ''.

PERFORM ALV_LAYOUT USING 6 'Closing Stock Qty' 'CS_QUAN' 'GT_FINAL' 'X'.
PERFORM ALV_LAYOUT USING 8 'Closing Stock Amt Rs.' 'CS_AMNT' 'GT_FINAL' 'X'.

PERFORM ALV_LAYOUT USING 9 '0 to 30 Days' 'A1_QTY' 'GT_FINAL' 'X'.
PERFORM ALV_LAYOUT USING 10 '30 to 40 Days.' 'A2_QTY' 'GT_FINAL' 'X'.
PERFORM ALV_LAYOUT USING 11 '40 To 60 Days' 'A3_QTY' 'GT_FINAL' 'X'.
PERFORM ALV_LAYOUT USING 12 '60 To 90 Days' 'A4_QTY' 'GT_FINAL' 'X'.
PERFORM ALV_LAYOUT USING 13 'Above 90 Days' 'A5_QTY' 'GT_FINAL' 'X'.
*PERFORM ALV_LAYOUT USING 8 'Material Sales Amt Rs.' 'MS_AMNT' 'GT_FINAL' 'X'.
*PERFORM ALV_LAYOUT USING 9 'Consumption Qty' 'GI_QUAN' 'GT_FINAL' 'X'.
*PERFORM ALV_LAYOUT USING 10 'Consumption Amt Rs.' 'GI_AMNT' 'GT_FINAL' 'X'.

*PERFORM ALV_LAYOUT USING 13 'Consumption Rate Rs.' 'CO_RATE' 'GT_FINAL' ' '.

*&---------------------------------------------------------------------*
*&  ALV Layout Grid Display
*&---------------------------------------------------------------------*

PERFORM ALV_GRID_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P1  text
*      -->P_P2  text
*      -->P_P3  text
*      -->P_P4  text
*      -->P_P5  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  APPEND WA_FCAT TO GT_FCAT.
ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
     IT_FIELDCAT                       = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
   I_DEFAULT                         = 'X'
   I_SAVE                            = 'X'
   IS_VARIANT                        = LS_VARIANT
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
      T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
