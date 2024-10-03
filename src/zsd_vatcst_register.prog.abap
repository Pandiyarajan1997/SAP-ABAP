*&---------------------------------------------------------------------*
*& Report  ZSD_VATCST_REGISTER
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : Govindarajan.M                        *
*& Created On                  : 27 Aug 2014                           *
*& Title                       : Sales Regiser For  VAT/CST            *
*& Report Name                 : ZSD_VATCST_REGISTER                   *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         :   Sales Regiser For  VAT/CST          *
*&---------------------------------------------------------------------*


REPORT ZSD_VATCST_REGISTER.

TYPE-POOLS : SLIS.

*************************************************************************************
**********            Structure Decleration                                    ******
*************************************************************************************

TYPES : BEGIN OF GS_KNA1,
        KUNNR TYPE KNA1-KUNNR,                " Sales Document
        NAME1 TYPE KNA1-NAME1,                              " Name1
        END OF GS_KNA1.

TYPES: BEGIN OF GS_VBRP,
       VBELN TYPE VBRP-VBELN,                " Billing Document
       WERKS TYPE VBRP-WERKS,                " Plant
       MATNR TYPE VBRP-MATNR,                " Material Number
       END OF GS_VBRP.

TYPES : BEGIN OF GS_VBRK,
        BUKRS LIKE VBRK-BUKRS,
        VBELN TYPE VBRK-VBELN,                " Billing Document
        FKART TYPE VBRK-FKART,                " Billing Type
        VKORG TYPE VBRK-VKORG,                " Sales Organization
        KUNRG TYPE VBRK-KUNRG,                " Payer
        NETWR TYPE VBRK-NETWR,                " Bill Amount
        FKDAT TYPE VBRK-FKDAT,                " Billing date for billing index and printout
        KNUMV TYPE VBRK-KNUMV,                " Number of the document condition
        MWSBK TYPE VBRK-MWSBK,                " Tax amount in document currency
       FKSTO TYPE VBRK-FKSTO,
        END OF GS_VBRK.


TYPES : BEGIN OF GS_KONV,
        KNUMV TYPE KONV-KNUMV,                " Number of the document conditio
        KPOSN TYPE KONV-KPOSN,                " Condition item number
        STUNR TYPE KONV-STUNR,                " Step number
        ZAEHK TYPE KONV-ZAEHK,                " Condition counter
        KAWRT TYPE KONV-KAWRT,                " Condition base value
        KBETR TYPE KONV-KBETR,                " Rate (condition amount or percentage)
        KSCHL TYPE KONV-KSCHL,                " Condition type
        KWERT TYPE KONV-KWERT,                " Tax Amount
        KNTYP TYPE KONV-KNTYP,                " Condition category (examples: tax, freight, price, cost)
        KUNNR TYPE KONV-KUNNR,                " Customer number (rebate recipient)
        END OF GS_KONV.

TYPES : BEGIN OF GS_J_1IMOCUST,
        KUNNR TYPE  J_1IMOCUST-KUNNR,            "#EC CI_USAGE_OK[2877717] " Customer NumberAdded by <IT-CAR Tool> during Code Remediation
        J_1ILSTNO TYPE  J_1IMOCUST-J_1ILSTNO,    "#EC CI_USAGE_OK[2877717] " Local Sales Tax NumberAdded by <IT-CAR Tool> during Code Remediation
        END OF GS_J_1IMOCUST.

TYPES: BEGIN OF GS_J_1IMTCHID,
       MATNR TYPE J_1IMTCHID-MATNR,             " Material Number
       J_1ICHID TYPE J_1IMTCHID-J_1ICHID,       " Chapter ID
       END OF GS_J_1IMTCHID.

TYPES : BEGIN OF GS_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
        END OF GS_T001W.



TYPES:  BEGIN OF GS_FINAL,
        BUKRS LIKE VBRK-BUKRS,
        KUNRG TYPE VBRK-KUNRG,                  " Sold-to party
        FKART TYPE VBRK-FKART,                  " billing type                                    " added by mani 07.03.2016
        NAME1 TYPE KNA1-NAME1,                              " Name1
        WERKS TYPE VBRP-WERKS,                  " Plant
        PNAME TYPE T001W-NAME1,                 " Plant Name
        J_1ILSTNO TYPE  J_1IMOCUST-J_1ILSTNO,    "#EC CI_USAGE_OK[2877717] " Local Sales Tax NumberAdded by <IT-CAR Tool> during Code Remediation
        J_1ICHID TYPE J_1IMTCHID-J_1ICHID,      " Commodity Code
        VBELN TYPE VBRK-VBELN,                  " Billing Document
        FKDAT TYPE VBRK-FKDAT,                  " Billing date for billing index and printout
        KAWRT TYPE KONV-KAWRT,                  " Condition base value
        KBETR TYPE KONV-KBETR,                  " Rate (condition amount or percentage)
        KWERT TYPE KONV-KWERT,                  " Tax amount in document currency
        AKWERT TYPE KONV-KWERT,                  " Tax amount in document currency
        TOT_AMT TYPE KONV-KWERT,                " Total Amount
        KSCHL TYPE KONV-KSCHL,                  " Condition type
        KNTYP TYPE KONV-KNTYP,                  " Condition category (examples: tax, freight, price, cost)
        END OF GS_FINAL.

*************************************************************************************
******            Internal table & Work area Declaration                        *****
*************************************************************************************

DATA : GT_VBRK TYPE TABLE OF GS_VBRK,
       WA_VBRK TYPE  GS_VBRK,
       GT_T001W TYPE TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W,
       GT_KNA1 TYPE TABLE OF GS_KNA1,
       WA_KNA1 TYPE GS_KNA1,
       GT_VBRP TYPE TABLE OF GS_VBRP,
       WA_VBRP TYPE GS_VBRP,
       GT_KONV TYPE TABLE OF GS_KONV,
       WA_KONV TYPE GS_KONV,
       GT_J_1IMOCUST TYPE TABLE OF GS_J_1IMOCUST,
       WA_J_1IMOCUST TYPE GS_J_1IMOCUST,
       GT_J_1IMTCHID TYPE TABLE OF GS_J_1IMTCHID,
       WA_J_1IMTCHID TYPE GS_J_1IMTCHID,
       GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

*************************************************************************************
******            Declaration for ALV Grid                                      *****
*************************************************************************************

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      V_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_SORT LIKE LINE OF IT_SORT.
DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA :  GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,
      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX
      GWA_VALUES  TYPE DYNPREAD,                              " WORK AREA FOR LIST BOX
      GV_SELECTED_VALUE(10) TYPE C.


DATA: LS_VARIANT TYPE DISVARIANT.
LS_VARIANT-REPORT = SY-REPID.


*************************************************************************************
******           selection-screen                                            ********                                           *****
*************************************************************************************

DATA : OR_BUKRS TYPE VBRK-BUKRS,
       OR_KUNRG TYPE VBRK-KUNRG,
       OR_KSCHL TYPE KSCHL,
       OR_FKDAT TYPE FKDAT,
       OR_WERKS TYPE VBRP-WERKS,
       OR_MWSKZ TYPE T007A-MWSKZ.


SELECTION-SCREEN: BEGIN OF BLOCK B1.

PARAMETERS: PS_PARM AS LISTBOX VISIBLE LENGTH 50 USER-COMMAND ABC MODIF ID TB1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: SO_BUKRS FOR OR_BUKRS OBLIGATORY,
                SO_KUNRG FOR OR_KUNRG,
                SO_WERKS FOR OR_WERKS,
                IN_FKDAT FOR OR_FKDAT.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-101.
PARAMETERS: RD_VAT RADIOBUTTON GROUP G1 DEFAULT 'X',
            RD_CST RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B3.


SELECTION-SCREEN: BEGIN OF BLOCK AB3 WITH FRAME TITLE TEXT-101.

PARAMETERS:P_IC AS CHECKBOX .

SELECTION-SCREEN: END OF BLOCK AB3.


INITIALIZATION.

  GWA_LIST-KEY = '1'.
  GWA_LIST-TEXT = 'CUSTOMER VAT/CST SALES REPORT'.
  APPEND GWA_LIST TO GT_LIST.
  CLEAR: GWA_LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'PS_PARM'
      VALUES          = GT_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.


  IF PS_PARM IS INITIAL.                                                 " TO SET SET THE INITIAL VALUE SHOWN IN LIST BOX AS 'INVOICE'

    PS_PARM = '1'.

  ENDIF.


  LOOP AT SCREEN.


    IF SCREEN-NAME = 'PS_PARM'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

*IF RD_VAT = 'X'.
*  SET TITLEBAR 'ZTITLE1'.
*ELSEIF RD_CST = 'X'.
*  SET TITLEBAR 'ZTITLE2'.
*ENDIF.


*************************************************************************************
********** Main Logic
*************************************************************************************



START-OF-SELECTION.

  SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
    VBELN
    WERKS
    MATNR FROM VBRP INTO TABLE GT_VBRP
    WHERE WERKS IN SO_WERKS.

  IF GT_VBRP[] IS NOT INITIAL.

*    SELECT
*      MATNR
*      J_1ICHID FROM J_1IMTCHID
*      INTO TABLE GT_J_1IMTCHID FOR ALL ENTRIES IN GT_VBRP
*      WHERE MATNR = GT_VBRP-MATNR.

    IF RD_VAT = 'X'.

      if P_IC ne 'X'.

      SELECT
        BUKRS
        VBELN
        FKART
        VKORG
        KUNRG
        NETWR
        FKDAT
        KNUMV
        MWSBK
        FKSTO
        FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP
        WHERE VBELN = GT_VBRP-VBELN AND KUNRG IN SO_KUNRG AND BUKRS IN SO_BUKRS AND
              FKDAT IN IN_FKDAT AND
*              RFBSK NE 'E' AND
              FKSTO NE 'X' AND
              FKART NOT IN ('YBRE','S1','F1','S2','F2'). "  AND
*              VBTYP NE 'N'.

ELSEIF P_IC = 'X'.                                  " ADDED BY MANI 07.03.2016

        SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        BUKRS
        VBELN
        FKART
        VKORG
        KUNRG
        NETWR
        FKDAT
        KNUMV
        MWSBK
        FKSTO
        FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP
        WHERE VBELN = GT_VBRP-VBELN AND KUNRG IN SO_KUNRG AND BUKRS IN SO_BUKRS AND
              FKDAT IN IN_FKDAT AND
               ( FKART = 'IV' OR FKART = 'IVS' ) .


ENDIF.

      SELECT
       KNUMV
       KPOSN
       STUNR
       ZAEHK
       KAWRT
       KBETR
       KSCHL
       KWERT
       KNTYP
       KUNNR FROM PRCD_ELEMENTS INTO CORRESPONDING FIELDS OF TABLE GT_KONV FOR ALL ENTRIES IN GT_VBRK
       WHERE KNUMV = GT_VBRK-KNUMV AND
             KSCHL IN ('JIVP','JIVA').




    ELSEIF RD_CST = 'X'.

IF P_IC NE 'X'.
      SELECT
       VBELN
       FKART
       VKORG
       KUNRG
       NETWR "SAVARIAR CHANGE ON 02/09/2014
       FKDAT
       KNUMV
       MWSBK
       FROM VBRK INTO CORRESPONDING FIELDS OF TABLE  GT_VBRK FOR ALL ENTRIES IN  GT_VBRP
       WHERE VBELN = GT_VBRP-VBELN AND KUNRG IN SO_KUNRG AND
             FKDAT IN IN_FKDAT AND
             RFBSK NE 'E' AND
              FKSTO NE 'X' AND
             VBTYP NE 'N'.
ELSEIF P_IC EQ 'X'.
        SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
       VBELN
       FKART
       VKORG
       KUNRG
       NETWR "SAVARIAR CHANGE ON 02/09/2014
       FKDAT
       KNUMV
       MWSBK
       FROM VBRK INTO CORRESPONDING FIELDS OF TABLE  GT_VBRK FOR ALL ENTRIES IN  GT_VBRP
       WHERE VBELN = GT_VBRP-VBELN AND KUNRG IN SO_KUNRG AND
             FKDAT IN IN_FKDAT AND
             RFBSK NE 'E' AND
*              FKSTO NE 'X' AND
             VBTYP NE 'N' and ( FKART = 'IV' OR FKART = 'IVS' ) .


  ENDIF.
      SELECT
       KNUMV
       KPOSN
       STUNR
       ZAEHK
       KAWRT
       KBETR
       KSCHL
       KWERT
       KNTYP
       KUNNR FROM PRCD_ELEMENTS INTO CORRESPONDING FIELDS OF TABLE GT_KONV FOR ALL ENTRIES IN GT_VBRK
       WHERE KNUMV = GT_VBRK-KNUMV AND
             KSCHL = 'JIVC' .
    ENDIF.

    IF GT_VBRK[] IS NOT INITIAL.

      SELECT
        KUNNR
        NAME1
        WERKS FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE GT_KNA1 FOR ALL ENTRIES IN GT_VBRK
        WHERE KUNNR = GT_VBRK-KUNRG.


      IF GT_KONV[] IS NOT INITIAL.

        SELECT
          KUNNR
          J_1ILSTNO FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE GT_J_1IMOCUST FOR ALL ENTRIES IN GT_VBRK
          WHERE KUNNR = GT_VBRK-KUNRG.
      ENDIF.
    ENDIF.
  ENDIF.

  IF GT_VBRP[] IS NOT INITIAL.
    SELECT WERKS
           NAME1 FROM  T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_VBRP WHERE WERKS = GT_VBRP-WERKS.
  ENDIF.

  LOOP AT GT_VBRK INTO WA_VBRK.

      wa_final-fkart = wa_vbrk-fkart .
*    READ TABLE GT_J_1IMTCHID INTO WA_J_1IMTCHID WITH KEY MATNR = WA_VBRP-MATNR.
*    IF SY-SUBRC = 0.
    READ TABLE GT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_VBRK-VBELN.
    WA_FINAL-WERKS = WA_VBRP-WERKS.

    READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBRK-KUNRG.
    IF SY-SUBRC = 0.
      WA_FINAL-NAME1 = WA_KNA1-NAME1.
    ENDIF.
    READ TABLE GT_J_1IMOCUST INTO WA_J_1IMOCUST WITH KEY KUNNR = WA_VBRK-KUNRG.
    IF SY-SUBRC = 0.
      WA_FINAL-J_1ILSTNO = WA_J_1IMOCUST-J_1ILSTNO.
    ENDIF.
    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_VBRP-WERKS.
    WA_FINAL-PNAME = WA_T001W-NAME1.

    LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = WA_VBRK-KNUMV.
      WA_FINAL-KAWRT = WA_FINAL-KAWRT + WA_KONV-KAWRT.
      WA_FINAL-KSCHL = WA_KONV-KSCHL.
      WA_FINAL-KNTYP = WA_KONV-KNTYP.
      IF WA_KONV-KSCHL = 'JIVP' OR WA_KONV-KSCHL = 'JIVC'.
        WA_FINAL-KWERT = WA_FINAL-KWERT + WA_KONV-KWERT.
      ELSEIF  WA_KONV-KSCHL = 'JIVA'.
        WA_FINAL-AKWERT = WA_FINAL-AKWERT + WA_KONV-KWERT.
      ENDIF.

       WA_FINAL-KBETR = WA_KONV-KBETR / 1.
      CLEAR WA_KONV.
    ENDLOOP.

    WA_FINAL-KUNRG = WA_VBRK-KUNRG.
    WA_FINAL-FKDAT = WA_VBRK-FKDAT.
    WA_FINAL-VBELN = WA_VBRK-VBELN.
    WA_FINAL-TOT_AMT = WA_FINAL-KAWRT + WA_FINAL-KWERT + WA_FINAL-AKWERT.
*     WA_FINAL-J_1ICHID = WA_J_1IMTCHID-J_1ICHID.
    READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = WA_VBRK-KNUMV.
    IF SY-SUBRC = 0.
      APPEND WA_FINAL TO GT_FINAL.
    ENDIF.
    CLEAR: WA_VBRK, WA_FINAL, WA_VBRP,
           WA_J_1IMOCUST, WA_J_1IMTCHID.

ENDLOOP.


LOOP AT GT_FINAL INTO WA_FINAL.
IF P_IC = 'X'.
IF WA_FINAL-FKART = 'IVS'.

WA_FINAL-KAWRT = - ( WA_FINAL-KAWRT ) .
WA_FINAL-KWERT = - ( WA_FINAL-KWERT ) .
WA_FINAL-AKWERT = - ( WA_FINAL-AKWERT ) .
WA_FINAL-TOT_AMT = - ( WA_FINAL-TOT_AMT ) .

MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KAWRT KWERT AKWERT TOT_AMT .

ENDIF.
ENDIF.

  ENDLOOP.





*&---------------------------------------------------------------------*
*&   ALV_LAYOUT
*&---------------------------------------------------------------------*

  PERFORM ALV_LAYOUT USING 1 'Buyer Code .' 'KUNRG' 'GT_FINAL' ' ' ''.
  PERFORM ALV_LAYOUT USING 2 'Name Of the Buyer.' 'NAME1' 'GT_FINAL' ' ' '' .
  PERFORM ALV_LAYOUT USING 3 'Buyer TIN No.' 'J_1ILSTNO' 'GT_FINAL' ' ' ''.
  PERFORM ALV_LAYOUT USING 4 'Plant' 'WERKS' 'GT_FINAL' ' ' ''.
  PERFORM ALV_LAYOUT USING 5 'Plant Description' 'PNAME' 'GT_FINAL' ' ' '' .
  PERFORM ALV_LAYOUT USING 7 'Invoice No.' 'VBELN' 'GT_FINAL' ' ' ''.
  PERFORM ALV_LAYOUT USING 9 'Invoice Date.' 'FKDAT' 'GT_FINAL' ' ' ''.
  PERFORM ALV_LAYOUT USING 11 'Net Value' 'KAWRT' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 12 'Rate Of Tax %' 'KBETR' 'GT_FINAL' ' ' 'X'."SAVARIAR CHANGE ON 02/09/2014
  IF RD_VAT = 'X'.
    PERFORM ALV_LAYOUT USING 13 'VAT Amount.' 'KWERT' 'GT_FINAL' 'X' ''.
    PERFORM ALV_LAYOUT USING 14 'ADD.VAT Amount.' 'AKWERT' 'GT_FINAL' 'X' ''.
  ELSEIF RD_CST = 'X'.
    PERFORM ALV_LAYOUT USING 16 'CST Amount.' 'KWERT' 'GT_FINAL' 'X' ''.
  ENDIF.
  PERFORM ALV_LAYOUT USING 17 'Net Amount.' 'TOT_AMT' 'GT_FINAL' 'X' ''.

  WA_SORT-FIELDNAME = 'KUNRG'.
  WA_SORT-SUBTOT = 'X'.
  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

*&---------------------------------------------------------------------*
*&   ALV Grid Display
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
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6."SAVARIAR CHANGE ON 02/09/2014
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_OUT = P6.
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

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
   I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
   I_CALLBACK_USER_COMMAND           = 'MY_USER_COMMAND'
   I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
   IT_SORT                           = IT_SORT[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
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
      T_OUTTAB                         = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ALV_GRID_DISPLAY



*&---------------------------------------------------------------------*
*&      Form  MY_USER_COMMAND
*&---------------------------------------------------------------------*
*      FOR SENDING EMAIL WITH EXCEL ATTACHMENT
*----------------------------------------------------------------------*
*      -->R_UCOMM    text
*----------------------------------------------------------------------*
FORM MY_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.

*  break-point.

  CASE R_UCOMM.

    WHEN '&IC1'.

      IF RS_SELFIELD-FIELDNAME = 'VBELN'.

        SET PARAMETER ID 'VF' FIELD RS_SELFIELD-VALUE.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.
ENDFORM.                    "MY_USER_COMMAND


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

  DATA : RV_WERKS(100) TYPE C,
         RV_FKDAT(100) TYPE C,
         LV_BEDAT(50) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : RV_FKDAT,
          RV_WERKS.
  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO RV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code:' SO_WERKS-LOW INTO RV_WERKS SEPARATED BY SPACE.
  ENDIF.

  CONCATENATE 'Document Date :' IN_FKDAT-LOW 'To' IN_FKDAT-HIGH INTO RV_FKDAT SEPARATED BY SPACE.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = RV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = RV_FKDAT.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'VAT/CST Register For Customer' .
  APPEND LS_LINE TO LIT_HEADER.


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

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      INPUT  = RV_FKDAT
    IMPORTING
      OUTPUT = RV_FKDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
