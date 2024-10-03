*&---------------------------------------------------------------------*
*& Report  ZFI_VOUCHER_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCN_DN_VOUCHER_REPORT1.
*&---------------------------------------------------------------------*
*& Report  ZFI_VOUCHER_REPORT_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*





************************************************************************************************
*                 PROGRAM FOR VOUCHER PRINTING
************************************************************************************************
* Program Name          :  ZCN_DN_VOUCHER_REPORT
* Functional Analyst    :  Govindarajan Murugan
* Programmer            :  Govindarajan Murugan
* Start date            : 21.04.2014
* Description           :  Program for Debit / Credit Memo

*----------------------------------------------------------------------------------------------
**************************************************************************************************
*    TABLE  DECLARATIONS
**************************************************************************************************




TABLES : BKPF, BSEG.

**************************************************************************************************
*      TYPE DECLARATIONS
**************************************************************************************************
TYPES : BEGIN OF GS_BKPF,
BLART TYPE BKPF-BLART,
BELNR TYPE BKPF-BELNR,
BLDAT TYPE BKPF-BLDAT,
XBLNR TYPE BKPF-XBLNR,
WAERS TYPE BKPF-WAERS,
KURSF TYPE BKPF-KURSF,
BUDAT TYPE BKPF-BUDAT,
BUKRS TYPE BKPF-BUKRS,
GJAHR TYPE BKPF-GJAHR,
BKTXT TYPE BKPF-BKTXT,
  END OF GS_BKPF.

TYPES : BEGIN OF GS_BSEG,
BSCHL  TYPE BSEG-BSCHL,
GSBER  TYPE BSEG-GSBER,
ZUONR  TYPE BSEG-ZUONR,
SGTXT  TYPE BSEG-SGTXT,
KOSTL  TYPE BSEG-KOSTL,
SAKNR  TYPE BSEG-SAKNR,
KUNNR  TYPE BSEG-KUNNR ,
*PRCTR  TYPE BSEG-PRCTR,
PROJK  TYPE BSEG-PROJK,
DMBTR  TYPE BSEG-DMBTR,
WRBTR  TYPE BSEG-WRBTR,
SHKZG  TYPE BSEG-SHKZG,
HKONT  TYPE BSEG-HKONT,
KOART  TYPE BSEG-KOART,
LIFNR  TYPE BSEG-LIFNR,
BELNR  TYPE BSEG-BELNR,
BUZEI  TYPE BSEG-BUZEI,
EBELN  TYPE BSEG-EBELN,
WERKS  TYPE BSEG-WERKS,
 END OF GS_BSEG.

TYPES : BEGIN OF GS_LFA1,
        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,
       END OF GS_LFA1.

TYPES : BEGIN OF GS_KNA1,
        KUNNR TYPE KNA1-LIFNR,
        NAME1 TYPE KNA1-NAME1,
       END OF GS_KNA1.

TYPES : BEGIN OF GS_SKAT,
       KTOPL TYPE SKAT-KTOPL,
       SAKNR TYPE SKAT-SAKNR,
       TXT50 TYPE SKAT-TXT50,
       END OF GS_SKAT.

TYPES : BEGIN OF GS_FAGLFLEXA,
           RYEAR TYPE FAGLFLEXA-RYEAR,
           DOCNR TYPE FAGLFLEXA-DOCNR,
           RBUKRS TYPE FAGLFLEXA-RBUKRS,
           DOCLN TYPE FAGLFLEXA-DOCLN,
           PRCTR TYPE FAGLFLEXA-PRCTR,
           BUDAT TYPE FAGLFLEXA-BUDAT,
      END OF GS_FAGLFLEXA.




TYPES : BEGIN OF GS_FINAL,
BLART TYPE BKPF-BLART,
BELNR TYPE BKPF-BELNR,
BLDAT TYPE BKPF-BLDAT,
XBLNR TYPE BKPF-XBLNR,
WAERS TYPE BKPF-WAERS,
KURSF TYPE BKPF-KURSF,
BUDAT TYPE BKPF-BUDAT,
BUKRS TYPE BKPF-BUKRS,
GJAHR TYPE BKPF-GJAHR,
BKTXT TYPE BKPF-BKTXT,
BSCHL  TYPE BSEG-BSCHL,
GSBER  TYPE BSEG-GSBER,
ZUONR  TYPE BSEG-ZUONR,
SGTXT  TYPE BSEG-SGTXT,
KOSTL  TYPE BSEG-KOSTL,
SAKNR  TYPE BSEG-SAKNR,
TXT50  TYPE SKAT-TXT50,
KUNNR  TYPE BSEG-KUNNR ,
NAME   TYPE KNA1-NAME1,
PRCTR  TYPE BSEG-PRCTR,
PROJK  TYPE BSEG-PROJK,
DMBTR  TYPE BSEG-DMBTR,
WRBTR  TYPE BSEG-WRBTR,
SHKZG  TYPE BSEG-SHKZG,
HKONT  TYPE BSEG-HKONT,
KOART  TYPE BSEG-KOART,
LIFNR  TYPE BSEG-LIFNR,
NAME1 TYPE LFA1-NAME1,
BUZEI  TYPE BSEG-BUZEI,
EBELN  TYPE BSEG-EBELN,
WERKS TYPE BSEG-WERKS,
 END OF GS_FINAL.


DATA : GT_BSEG TYPE TABLE OF GS_BSEG ,
       GT_BKPF TYPE TABLE OF GS_BKPF ,
       GT_LFA1 TYPE TABLE OF GS_LFA1,
       GT_KNA1 TYPE TABLE OF GS_KNA1,
       GT_SKAT TYPE TABLE OF GS_SKAT,
       GT_FAGLFLEXA TYPE TABLE OF GS_FAGLFLEXA,
       GT_FINAL TYPE TABLE OF GS_FINAL.



DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      V_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_SORT LIKE LINE OF IT_SORT.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : WA_BSEG    TYPE GS_BSEG,
      WA_BKPF    TYPE GS_BKPF,
      WA_LFA1 TYPE GS_LFA1,
      WA_KNA1 TYPE GS_KNA1,
      WA_SKAT TYPE GS_SKAT,
      WA_FAGLFLEXA TYPE GS_FAGLFLEXA,
      WA_FINAL    TYPE GS_FINAL.

DATA :  GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,
      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX
      GWA_VALUES  TYPE DYNPREAD,                              " WORK AREA FOR LIST BOX
      GV_SELECTED_VALUE(10) TYPE C.

DATA : WRK_KTOPL  TYPE T001-KTOPL,
       LV_BLART TYPE BKPF-BLART.

DATA : FM_VOUCHER TYPE RS38L_FNAM.
DATA : LV_HKONT TYPE  BSEG-HKONT .
DATA : CONTROL_PARAM  TYPE SSFCTRLOP,
      LV_BUKRS TYPE BKPF-BUKRS ,
      LV_PRCTR  TYPE FAGLFLEXA-PRCTR,
      LV_WERKS TYPE BSEG-WERKS,
      LV_KUNNR TYPE BSEG-KUNNR.

**************************************************************************************************
*        SELECTION SCREEN
**************************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1.

PARAMETERS: PS_PARM AS LISTBOX VISIBLE LENGTH 50 USER-COMMAND ABC MODIF ID TB1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES

SELECTION-SCREEN: END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:  SO_BUKRS FOR LV_BUKRS,
                 SO_WERKS FOR LV_WERKS NO-DISPLAY,
                   SO_PRCTR FOR LV_PRCTR,
                  SO_HKONT FOR  LV_HKONT,
                  SO_KUNNR FOR LV_KUNNR,
                  SO_BLART FOR LV_BLART OBLIGATORY,
                 SO_BUDAT FOR BKPF-BUDAT.

PARAMETERS:
            PA_GJAHR TYPE BKPF-GJAHR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.



INITIALIZATION.

  GWA_LIST-KEY = '1'.
  GWA_LIST-TEXT = 'VAT CN/DN REPORT'.
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

START-OF-SELECTION.

SELECT RYEAR
           DOCNR
           RBUKRS
           DOCLN
           PRCTR
           BUDAT FROM FAGLFLEXA
           INTO CORRESPONDING FIELDS OF TABLE GT_FAGLFLEXA
           WHERE RYEAR  = PA_GJAHR
             AND BUDAT IN SO_BUDAT
             AND RBUKRS IN SO_BUKRS
             AND PRCTR IN SO_PRCTR.

SORT GT_FAGLFLEXA BY DOCNR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_FAGLFLEXA COMPARING DOCNR.

  SELECT
  BLART
  BELNR
  BLDAT
  XBLNR
  WAERS
  KURSF
  BUDAT
  BUKRS
  GJAHR
  BKTXT FROM BKPF INTO TABLE GT_BKPF  FOR ALL ENTRIES IN GT_FAGLFLEXA WHERE BELNR = GT_FAGLFLEXA-DOCNR AND  BUKRS IN SO_BUKRS AND BUDAT IN SO_BUDAT  AND GJAHR = PA_GJAHR AND
  BLART IN SO_BLART.

  IF GT_BKPF[]  IS  NOT INITIAL.
    SELECT BSCHL "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
GSBER
ZUONR
SGTXT
KOSTL
SAKNR
KUNNR
*PRCTR
PROJK
DMBTR
WRBTR
SHKZG
HKONT
KOART
LIFNR
BELNR
BUZEI
EBELN
WERKS
  FROM BSEG
  INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
  FOR ALL ENTRIES IN GT_BKPF
  WHERE BELNR = GT_BKPF-BELNR
  AND   BUKRS = GT_BKPF-BUKRS
  AND   GJAHR = GT_BKPF-GJAHR AND HKONT IN SO_HKONT AND  WERKS IN SO_WERKS AND KUNNR IN SO_KUNNR. "#EC CI_NOORDER  ""AND PRCTR IN SO_PRCTR. " Hidded By Govind On 12/12/2014Added by <IT-CAR Tool> during Code Remediation
  ENDIF.

  IF GT_BSEG[] IS NOT INITIAL.
   SELECT LIFNR
     NAME1 FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_BSEG WHERE LIFNR = GT_BSEG-LIFNR.

   SELECT KUNNR
     NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_BSEG WHERE KUNNR = GT_BSEG-KUNNR.

ENDIF.

SELECT KTOPL SAKNR TXT50
          FROM SKAT
          INTO TABLE GT_SKAT FOR ALL ENTRIES IN GT_BSEG
          WHERE SAKNR = GT_BSEG-HKONT.



    IF GT_BKPF[]  IS   INITIAL.
      MESSAGE : 'This Selection criteria not matching' TYPE 'I'.
      LEAVE SCREEN.
    ENDIF.

*
*
*  SELECT SINGLE KTOPL
*        FROM T001
*        INTO WRK_KTOPL
*        WHERE BUKRS IN SO_BUKRS.


    LOOP AT GT_BSEG INTO WA_BSEG.
     MOVE-CORRESPONDING WA_BSEG TO WA_FINAL.

     READ TABLE GT_BKPF INTO WA_BKPF WITH KEY   BELNR = WA_BSEG-BELNR.

     WA_FINAL-BELNR = WA_BKPF-BELNR.
     WA_FINAL-BLART = WA_BKPF-BLART.
     WA_FINAL-BLDAT = WA_BKPF-BLDAT.
     WA_FINAL-XBLNR = WA_BKPF-XBLNR.
     WA_FINAL-BKTXT = WA_BKPF-BKTXT.

      IF WA_BSEG-KOART = 'K'.
        READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.
        IF SY-SUBRC = 0.
        WA_FINAL-NAME1 = WA_LFA1-NAME1.
       ENDIF.
         ELSEIF WA_BSEG-KOART = 'D'.
        READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_BSEG-KUNNR.

                 WA_FINAL-NAME1 = WA_KNA1-NAME1.
         ELSE.
               READ TABLE GT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_BSEG-HKONT.
     WA_FINAL-NAME1 = WA_SKAT-TXT50.
ENDIF.


     APPEND WA_FINAL TO GT_FINAL.

      CLEAR WA_FINAL.
    ENDLOOP.



LOOP AT GT_FINAL INTO WA_FINAL.
  READ TABLE GT_FAGLFLEXA INTO WA_FAGLFLEXA WITH KEY DOCNR = WA_FINAL-BELNR.
WA_FINAL-PRCTR = WA_FAGLFLEXA-PRCTR.

  SHIFT WA_FINAL-PRCTR LEFT DELETING LEADING '0'.
  MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PRCTR.
  CLEAR WA_FINAL.
  ENDLOOP.



DELETE GT_FINAL WHERE PRCTR = ''.


*&---------------------------------------------------------------------*
*&   ALV_LAYOUT
*&---------------------------------------------------------------------*


 PERFORM ALV_LAYOUT USING 1 'Document Type' 'BLART' 'GT_FINAL' ' ' .
    PERFORM ALV_LAYOUT USING 1 'G/L Account No' 'HKONT' 'GT_FINAL' ' ' .
  PERFORM ALV_LAYOUT USING 2 'G/L Account Name' 'NAME1' 'GT_FINAL' ' '.
PERFORM ALV_LAYOUT USING 3 'Profit Center.' 'PRCTR' 'GT_FINAL' ' ' .
*  PERFORM ALV_LAYOUT USING 3 'Account Do .' 'TXT50' 'GT_FINAL' ' ' .
  PERFORM ALV_LAYOUT USING 4 'Credit Memo For.' 'NAME1' 'GT_FINAL' ' ' .
    PERFORM ALV_LAYOUT USING 5 'CN No.' 'BELNR' 'IT_FINAL' ' ' .
    PERFORM ALV_LAYOUT USING 6 'CN Date' 'BLDAT' 'IT_FINAL' ' ' .
  PERFORM ALV_LAYOUT USING 7 'Invoice No.' 'XBLNR' 'GT_FINAL' ' '.
*  PERFORM ALV_LAYOUT USING 8 'Invoice Date' 'KAWRT' 'GT_FINAL' ''.
*  PERFORM ALV_LAYOUT USING 9 'Inn.Value' 'KBETR' 'GT_FINAL' ' '.
    PERFORM ALV_LAYOUT USING 10 'CN/DN  Amount.' 'WRBTR' 'GT_FINAL' ''.
     PERFORM ALV_LAYOUT USING 11 'Db/Cr Ind.' 'SHKZG' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 12 'Text.' 'BKTXT' 'GT_FINAL' ''.
*  PERFORM ALV_LAYOUT USING 12 'Net Amount.' 'TOT_AMT' 'GT_FINAL' 'X'.

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
*&      Form  ALV_DISPLAY
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
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = GT_FCAT[]
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
      T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ALV_DISPLAY
