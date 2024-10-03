*&---------------------------------------------------------------------*
*& Report  ZMM_VATCST_REGISTER
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : Savariar.S                            *
*& Created On                  : 05 Sep 2014                           *
*& Title                       : VAT/CST Report                        *
*& Report Name                 : ZVATCST_REPORT                        *
*& Development Id              : Kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : ZVATCST_REPORT                        *

REPORT ZVATCST_REPORT.

** TABLES DECLARATION

TABLES : LFA1,RBKP,RSEG,T001W.


TYPES : BEGIN OF GS_LFA1,

         LIFNR TYPE LFA1-LIFNR,           "Vendor Code
         NAME1 TYPE LFA1-NAME1,            "Name

        END OF GS_LFA1.

TYPES : BEGIN OF GS_T001W,

        WERKS TYPE T001W-WERKS,
        NAME2 TYPE T001W-NAME1,
        LIFNR TYPE T001W-LIFNR,

        END OF GS_T001W.


TYPES : BEGIN OF GS_RBKP,

         BELNR TYPE RBKP-BELNR,          "Inv. Doc. No.
         BUDAT TYPE RBKP-BUDAT,          "Inv.Date
         BUKRS TYPE RBKP-BUKRS,          "Company Code
         GJAHR TYPE RBKP-GJAHR,          "Fiscal Year
         LIFNR TYPE RBKP-LIFNR,          "Invoicing Pty
         RMWWR TYPE RBKP-RMWWR,          "Gross Amt
         WMWST1 TYPE RBKP-WMWST1,        "Vat Amt
         BLART TYPE RBKP-BLART,          "Document Type

       END OF GS_RBKP.

TYPES : BEGIN OF GS_RSEG,

       BELNR TYPE RSEG-BELNR,            "Document Number
       GJAHR TYPE RSEG-GJAHR,            "Fiscal Year
       EBELN TYPE RSEG-EBELN,            "Purchasing Doc.
       BUKRS TYPE RSEG-BUKRS,            "Company Code
       SHKZG TYPE RSEG-SHKZG,            "Debit/Credit
       WRBTR TYPE RSEG-WRBTR,            "Amount
       MWSKZ TYPE RSEG-MWSKZ,            "Tax code
       KSCHL TYPE RSEG-KSCHL,            "Condition type in RSEG
       LFBNR TYPE RSEG-LFBNR,            "Reference Document
       LFPOS TYPE RSEG-LFPOS,            "Reference Documenet item
       WERKS TYPE RSEG-WERKS,            "Plant

       END OF GS_RSEG.

TYPES : BEGIN OF GS_J_1IMOVEND,

        LIFNR TYPE J_1IMOVEND-LIFNR, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1ICSTNO TYPE J_1IMOVEND-J_1ICSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1ILSTNO TYPE J_1IMOVEND-J_1ILSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation

        END OF GS_J_1IMOVEND.



TYPES : BEGIN OF GS_FINAL ,

    NAME2 TYPE T001W-NAME2,        "Plant name

    LIFNR TYPE LFA1-LIFNR,         "Vendor
    NAME1 TYPE LFA1-NAME1,         "Vendor Name

    BELNR TYPE RBKP-BELNR,          "Inv. Doc. No.
    BUDAT TYPE RBKP-BUDAT,          "Inv.Date

    WRBTR TYPE RSEG-WRBTR,          "Amount
    WERKS TYPE RSEG-WERKS,          "Plant

    RMWWR TYPE RBKP-RMWWR,          ""Gross Amt
    WMWST1 TYPE RBKP-WMWST1,        "Vat Amt

    J_1ICSTNO TYPE J_1IMOVEND-J_1ICSTNO,       "#EC CI_USAGE_OK[2877717] "Cst ValueAdded by <IT-CAR Tool> during Code Remediation
    J_1ILSTNO TYPE J_1IMOVEND-J_1ILSTNO,       "#EC CI_USAGE_OK[2877717] "Tin ValueAdded by <IT-CAR Tool> during Code Remediation


  END OF GS_FINAL.


DATA  :GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE GS_LFA1,

       GT_T001W TYPE TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W,

       GT_RBKP TYPE TABLE OF GS_RBKP,
       WA_RBKP TYPE GS_RBKP,

       GT_RSEG TYPE TABLE OF GS_RSEG,
       WA_RSEG TYPE  GS_RSEG,

       GT_J_1IMOVEND TYPE TABLE OF GS_J_1IMOVEND,
       WA_J_1IMOVEND TYPE GS_J_1IMOVEND,

       GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.


DATA: OR_BUKRS TYPE RSEG-BUKRS,             "Company Code
      OR_WERKS TYPE RSEG-WERKS,             "Plant
      OR_LIFNR TYPE RBKP-LIFNR,              "Vendor Code
      OR_BELNR TYPE RSEG-BELNR,              "Document Number
      OR_GJAHR TYPE RSEG-GJAHR,              "Year
      OR_BUDAT TYPE RBKP-BUDAT.              "Posting Date

DATA: L_BUKRS TYPE RSEG-BUKRS,              "Company Code
      L_WERKS TYPE RSEG-WERKS,               "Plant
      L_LIFNR TYPE RSEG-LIFNR,               "Vendor Code
      L_BELNR TYPE RSEG-BELNR,               "Document Number
      L_GJAHR TYPE RSEG-GJAHR,               "Year
      L_BUDAT TYPE RBKP-BUDAT.               "Posting Date



SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_BUKRS FOR OR_BUKRS,"OBLIGATORY,
                 SO_WERKS FOR OR_WERKS,"OBLIGATORY,
                 SO_LIFNR FOR OR_LIFNR,
                 SO_BELNR FOR OR_BELNR,
                 SO_GJAHR FOR OR_GJAHR, "OBLIGATORY,
                 SO_BUDAT FOR OR_BUDAT. "OBLIGATORY,

SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-101.

PARAMETERS: RD_VAT RADIOBUTTON GROUP G1,
            RD_CST RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B2.


START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM FIELD_CATLOG.


  DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
         WA_SORT TYPE SLIS_SORTINFO_ALV.

END-OF-SELECTION.

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



  SELECT
    BELNR
    BUDAT
    BUKRS
    GJAHR
    LIFNR
    RMWWR
    WMWST1
    BLART
 FROM
    RBKP INTO TABLE GT_RBKP WHERE BELNR IN SO_BELNR AND BUDAT IN SO_BUDAT AND LIFNR IN SO_LIFNR AND GJAHR IN SO_GJAHR AND BUKRS IN SO_BUKRS .

SORT GT_RBKP BY BUKRS BELNR LIFNR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_RBKP COMPARING BUKRS BELNR LIFNR.

  IF GT_RBKP[] IS NOT INITIAL.

    SELECT
         BELNR
         GJAHR
         EBELN
         BUKRS
         SHKZG
         WRBTR
         MWSKZ
         KSCHL
         LFBNR
         LFPOS
         WERKS
    FROM
      RSEG INTO TABLE GT_RSEG FOR ALL ENTRIES IN GT_RBKP WHERE BELNR = GT_RBKP-BELNR AND WERKS IN SO_WERKS AND BUKRS IN SO_BUKRS.

  ENDIF.

  IF GT_RSEG[] IS NOT INITIAL.

    SELECT
      WERKS
      NAME2
      LIFNR
      FROM T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_RSEG WHERE WERKS = GT_RSEG-WERKS.

  ENDIF.
  SELECT
    LIFNR
    NAME1
    FROM
    LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_RBKP WHERE LIFNR = GT_RBKP-LIFNR.

  SELECT
    LIFNR
    J_1ICSTNO
    J_1ILSTNO
   FROM
    LFA1 INTO TABLE GT_J_1IMOVEND FOR ALL ENTRIES IN GT_RBKP WHERE LIFNR = GT_RBKP-LIFNR.






  LOOP AT GT_RBKP INTO  WA_RBKP.

    WA_FINAL-BELNR = WA_RBKP-BELNR.
    WA_FINAL-BUDAT = WA_RBKP-BUDAT.
    WA_FINAL-LIFNR = WA_RBKP-LIFNR.
    WA_FINAL-RMWWR = WA_RBKP-RMWWR.
    WA_FINAL-WMWST1 = WA_RBKP-WMWST1.

    LOOP AT GT_RSEG INTO WA_RSEG WHERE BELNR = WA_RBKP-BELNR.
      WA_FINAL-WRBTR = WA_FINAL-WRBTR + WA_RSEG-WRBTR .
    ENDLOOP.

    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR  = WA_RBKP-LIFNR.
    WA_FINAL-NAME1 = WA_LFA1-NAME1.

    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_RSEG-WERKS.

    WA_FINAL-WERKS = WA_T001W-WERKS.
    WA_FINAL-NAME2 = WA_T001W-NAME2.

    READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_RBKP-LIFNR.

    WA_FINAL-J_1ICSTNO = WA_J_1IMOVEND-J_1ICSTNO.
    WA_FINAL-J_1ILSTNO = WA_J_1IMOVEND-J_1ILSTNO.

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

  ENDLOOP.

  DELETE GT_FINAL WHERE WRBTR = 0.

ENDFORM.                    " GET_DATA



"DATA : GT_SORT TYPE SLIS_T_
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .


  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 2 'Plant Name' 'NAME2' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Vendor Code' 'LIFNR' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 5 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Inv.No' 'BELNR' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 9 'Inv.Date' 'BUDAT' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 11 'Purchase Value' 'WRBTR' 'GT_FINAL' 'X' '' '' ''.

  IF RD_VAT = 'X'.

    PERFORM ALV_LAYOUT USING 13 'Tin No' 'J_1ILSTNO' 'GT_FINAL' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 15 'Vat Amt' 'WMWST1' 'GT_FINAL' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 17 'Total Amt' 'RMWWR' 'GT_FINAL' '' '' '' ''.

  ELSEIF RD_CST = 'X'.

    PERFORM ALV_LAYOUT USING 19 'Cst No' 'J_1ICSTNO' 'GT_FINAL' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 21 'Cst Amt' '' 'GT_FINAL' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 23 'Total Amt' '' 'GT_FINAL' '' '' '' ''.

  ENDIF.
  WA_SORT-FIELDNAME = 'NAME1'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.


ENDFORM.                    " FIELD_CATLOG


*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1         text
*      -->P2         text
*      -->P3         text
*      -->P4         text
*      -->P5         text
*      -->P6         text
*      -->P7         text
*      -->P8         text

*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7 P8.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-OUTPUTLEN = P6.
  WA_FCAT-KEY = P7.
  WA_FCAT-NO_OUT = P8.
  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    "ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .



*  DELETE GT_FINAL WHERE MENGE = 0.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
  I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
  I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ''
*   IS_LAYOUT                         =
  IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
  IT_SORT                           = GT_SORT[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
  I_DEFAULT                         = 'X'
  I_SAVE                            = 'A'
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
*   IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
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

  DATA : L_BUKRS(100) TYPE C,
         L_WERKS(100) TYPE C,
         L_LIFNR(100) TYPE C,
         L_BUDAT(100) TYPE C.



  CLEAR : L_BUKRS,
          L_GJAHR.
  IF SO_BUKRS-HIGH IS NOT INITIAL.
    CONCATENATE 'Compay Code :' SO_BUKRS-LOW 'To' SO_BUKRS-HIGH INTO L_BUKRS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Compay Code :' SO_BUKRS-LOW INTO L_BUKRS SEPARATED BY SPACE.
  ENDIF.

  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO L_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant :' SO_WERKS-LOW INTO L_WERKS SEPARATED BY SPACE.
  ENDIF.


  IF SO_LIFNR-HIGH IS NOT INITIAL.
    CONCATENATE 'Vendor Code :' SO_LIFNR-LOW 'To' SO_LIFNR-HIGH INTO L_LIFNR SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Vendor Code :' SO_LIFNR-LOW INTO L_LIFNR SEPARATED BY SPACE.
  ENDIF.


  IF SO_BUDAT-HIGH IS NOT INITIAL.
    CONCATENATE 'Document Date :' SO_BUDAT-LOW 'To' SO_BUDAT-HIGH INTO L_BUDAT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Document Date :' SO_BUDAT-LOW INTO L_BUDAT SEPARATED BY SPACE.
  ENDIF.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_BUKRS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_LIFNR.
  APPEND LS_LINE TO LIT_HEADER.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_BUDAT.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Vendor Vat/Cst Purchase Details Reports' .
  APPEND LS_LINE TO LIT_HEADER.

*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY = ' '.
*  LS_LINE-INFO = LV_BEDAT.
*  APPEND LS_LINE TO LIT_HEADER.

*  CLEAR LS_LINE.

*CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*  EXPORTING
**    IT_LIST_COMMENTARY       = LIT_HEADER
**   I_LOGO                   = 'ZLOGO' .
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
*          .
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BEDAT
*    IMPORTING
*      OUTPUT = LV_BEDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
