**&---------------------------------------------------------------------*
**& Report  ZPO_PRICING_COND_DETAILS
**&
**&---------------------------------------------------------------------*
*&Functional                   : Mr. Umapathi N                         *
*& Developer                   : Mr. Manikandan T                       *
*& Modified  On                :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :  Mr. Umapathi                         *
*& Title                       : PO pricing Conditional Details        *
*& Report Name                 : ZPP_COOISPI_REPORT                    *
*& Development Id              : kpabap                                *
*& Related Information         : Process Order Information System      *
**&---------------------------------------------------------------------*
*
REPORT ZPO_PRICING_COND_DETAILS.
*
*


TYPE-POOLS:SLIS.
TABLES:EKKO,EKPO,PRCD_ELEMENTS,T001,T001W,T685T. " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool

*-structure for EKKO
TYPES:BEGIN OF TY_EKKO,
      EBELN TYPE EKKO-EBELN,
      BUKRS TYPE EKKO-BUKRS,
      BSART TYPE EKKO-BSART,
      BEDAT TYPE EKKO-BEDAT,
      EKORG TYPE EKKO-EKORG,
      KNUMV TYPE EKKO-KNUMV,
      END OF    TY_EKKO.

*-struct for EKPO

TYPES:BEGIN OF TY_EKPO,
      EBELN TYPE EKPO-EBELN,
      EBELP TYPE EKPO-EBELP,
      MATNR TYPE EKPO-MATNR,
      TXZ01 TYPE EKPO-TXZ01,
      MTART TYPE EKPO-MTART,
      MENGE TYPE EKPO-MENGE,
      WERKS TYPE EKPO-WERKS,
      END OF  TY_EKPO.

*-struct for KONV
TYPES:BEGIN OF TY_KONV,
      KNUMV TYPE KONV-KNUMV,
      KPOSN TYPE KONV-KPOSN,
      KSCHL TYPE KONV-KSCHL,
      KBETR TYPE KONV-KBETR,
      KWERT TYPE KONV-KWERT,
     END OF TY_KONV.

*-struct for T685T

types: BEGIN OF STR_T685T,
        SPRAS TYPE T685T-SPRAS,
        KAPPL TYPE T685T-KAPPL,
        KSCHL TYPE T685T-KSCHL,
        VTEXT TYPE T685T-VTEXT,
      END OF STR_T685T.



*-final strucure
TYPES:BEGIN OF TY_FINAL,
      KNUMV TYPE EKKO-KNUMV,
      EBELP TYPE EKPO-EBELP,
      EBELN TYPE EKKO-EBELN,
      BUKRS TYPE EKKO-BUKRS,
         BSART TYPE EKKO-BSART,
      BEDAT TYPE EKKO-BEDAT,
      EKORG TYPE EKKO-EKORG,
      MATNR TYPE EKPO-MATNR,
       TXZ01 TYPE EKPO-TXZ01,
      MTART TYPE EKPO-MTART,
  MENGE TYPE EKPO-MENGE,
      WERKS TYPE EKPO-WERKS,
      KPOSN TYPE KONV-KPOSN,
      KSCHL TYPE KONV-KSCHL,
      KBETR TYPE KONV-KBETR,
      KWERT TYPE KONV-KWERT,
  VTEXT TYPE T685T-VTEXT,
       END OF   TY_FINAL.

*- decalaring the itab
DATA:IT_EKKO TYPE STANDARD TABLE OF TY_EKKO,
      IT_EKPO TYPE STANDARD TABLE OF TY_EKPO,
      IT_KONV TYPE STANDARD TABLE OF TY_KONV,
      IT_FINAL TYPE STANDARD TABLE OF TY_FINAL,
      IT_TEMP  TYPE STANDARD TABLE OF TY_FINAL,
      WA_EKKO TYPE TY_EKKO,
      WA_EKPO TYPE TY_EKPO,
      WA_KONV TYPE TY_KONV,
      WA_FINAL TYPE TY_FINAL.

data:wa_T685T type str_T685T,
      it_T685T type table of str_T685T.

*- field catlog
DATA:IT_FCAT  TYPE STANDARD TABLE OF SLIS_FIELDCAT_ALV,
       WA_FCAT LIKE LINE OF IT_FCAT,
       WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV.

*-local varaibles.
DATA:LV_TABIX TYPE SY-TABIX.

*-selection-screen desgin.

SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:S_BUKRS FOR EKKO-BUKRS obligatory ,
               S_WERKS FOR EKPO-WERKS,
               S_EBELN FOR EKKO-EBELN,
               S_BSART FOR EKKO-BSART.



SELECTION-SCREEN END OF BLOCK A1.





*- fetch the data.

START-OF-SELECTION.
*-from EKKO
  REFRESH : IT_EKKO[].
  SELECT EBELN
         BUKRS
         BSART
         BEDAT
         EKORG
         KNUMV
         FROM EKKO
         INTO TABLE IT_EKKO
         WHERE EBELN IN S_EBELN
         AND   BUKRS IN S_BUKRS
        " AND   RESWK IN S_RESWK
         AND   BSART IN S_BSART.

  IF IT_EKKO IS NOT INITIAL.
    REFRESH : IT_EKPO[].
*-from ekpo
    SORT IT_EKKO BY EBELN.
    SELECT EBELN
           EBELP
           TXZ01
           MATNR
           WERKS
           MENGE

           MTART
           FROM EKPO
           INTO TABLE IT_EKPO
           FOR ALL ENTRIES IN IT_EKKO
           WHERE EBELN = IT_EKKO-EBELN
           AND WERKS IN S_WERKS.
    "  AND   BEDAT IN S_AEDAT
    " AND   WERKS IN S_WERKS.
    IF SY-SUBRC = 0.
      SORT IT_EKPO BY EBELN.
    ENDIF.

*-from konv
    REFRESH :IT_KONV[].
    SELECT KNUMV
           KPOSN
           KSCHL
           KBETR
           KWERT
           FROM PRCD_ELEMENTS " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
           INTO TABLE IT_KONV
           FOR ALL ENTRIES IN IT_EKKO
           WHERE KNUMV = IT_EKKO-KNUMV.
    " AND KSCHL IN S_KSCHL.
    IF SY-SUBRC = 0.
      SORT IT_KONV BY KNUMV KPOSN.
    ENDIF.



    select SPRAS
           KAPPL
           KSCHL
           VTEXT from T685T
           into table it_T685T
           for all entries in it_konv
           where kschl = it_konv-kschl and SPRAS eq 'EN' and kappl eq 'M'.




  ENDIF.


*-fetching the data into final itab

  LOOP AT IT_EKPO INTO WA_EKPO.
*-Read data from ekko
    READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_FINAL-EBELN = WA_EKKO-EBELN.
      WA_FINAL-BUKRS = WA_EKKO-BUKRS.
      WA_FINAL-BSART = WA_EKKO-BSART.
      WA_FINAL-BEDAT = WA_EKKO-BEDAT.
      WA_FINAL-EKORG = WA_EKKO-EKORG.
      WA_FINAL-KNUMV = WA_EKKO-KNUMV.
      WA_FINAL-EBELP = WA_EKPO-EBELP.
      WA_FINAL-MATNR = WA_EKPO-MATNR.
      WA_FINAL-WERKS = WA_EKPO-WERKS.
      WA_FINAL-TXZ01 = WA_EKPO-TXZ01.
      WA_FINAL-MTART = WA_EKPO-MTART.
      WA_FINAL-MENGE = WA_EKPO-MENGE.
      APPEND WA_FINAL TO IT_FINAL.
      CLEAR:WA_EKKO,
            WA_EKPO,
            WA_KONV,
            WA_FINAL.
    ENDIF.
  ENDLOOP.

  SORT IT_FINAL BY KNUMV EBELP.
*- use parallel cursor method.



  LOOP AT IT_KONV INTO WA_KONV.
    READ TABLE IT_FINAL INTO WA_FINAL WITH KEY KNUMV = WA_KONV-KNUMV EBELP = WA_KONV-KPOSN BINARY SEARCH.
    IF SY-SUBRC = 0.
      CLEAR:LV_TABIX.
      LV_TABIX = SY-TABIX.
      LOOP AT IT_FINAL INTO WA_FINAL FROM LV_TABIX.
        IF WA_FINAL-KNUMV <> WA_KONV-KNUMV OR WA_FINAL-EBELP <> WA_KONV-KPOSN.
          EXIT.
        ELSE.
          WA_FINAL-KPOSN = WA_KONV-KPOSN.
          WA_FINAL-KSCHL = WA_KONV-KSCHL.
          WA_FINAL-KBETR = WA_KONV-KBETR.
          WA_FINAL-KWERT = WA_KONV-KWERT.

    read table it_T685T into wa_T685T
    with key kschl = wa_konv-kschl.
          if sy-subrc = 0.
            wa_final-vtext = wa_T685T-vtext.
            endif.



          APPEND WA_FINAL TO IT_TEMP.
          CLEAR:WA_FINAL.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR:WA_KONV.
  ENDLOOP.


*  DELETE IT_FINAL[] WHERE KWERT = '0.00'.
*  DELETE IT_FINAL[] WHERE KBETR = '0.00'.

  REFRESH: IT_FINAL[].
  IT_FINAL[] = IT_TEMP[].
  FREE:IT_TEMP.


  DELETE IT_FINAL[] WHERE KWERT = '0.00'.
  DELETE IT_FINAL[] WHERE KBETR = '0.00'.
  SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.



END-OF-SELECTION.
*-display  the data.
*field catlog
*  PERFORM FCAT.
*  IF IT_FINAL IS NOT INITIAL.
*    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*      EXPORTING
*        IT_FIELDCAT = IT_FCAT
*      TABLES
*        T_OUTTAB    = IT_FINAL.
*  ELSE.
*    WRITE:'No Data'.
*  ENDIF.




*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM FCAT .
  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'EBELN'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Po.no'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'EBELP'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Po.item'.
  APPEND WA_FCAT TO IT_FCAT.





  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'BUKRS'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Comp.code'.
  APPEND WA_FCAT TO IT_FCAT.



  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'MTART'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Plant'.
  APPEND WA_FCAT TO IT_FCAT.



  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'BSART'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Doc Type'.
  APPEND WA_FCAT TO IT_FCAT.




  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'BEDAT'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Docum Date'.
  APPEND WA_FCAT TO IT_FCAT.




  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'TXZ01'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-NO_ZERO = 'X'.
  WA_FCAT-SELTEXT_M = 'Material Number'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'MATNR'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Material Desc'.
  APPEND WA_FCAT TO IT_FCAT.





  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'WERKS'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Material Type'.
  APPEND WA_FCAT TO IT_FCAT.


  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'MENGE'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Quantity'.
  APPEND WA_FCAT TO IT_FCAT.







*
*  CLEAR WA_FCAT.
*  WA_FCAT-FIELDNAME = 'KNUMV'.
*  WA_FCAT-TABNAME = 'IT_FINAL'.
*  WA_FCAT-SELTEXT_M = 'Doc.Cond'.
*  APPEND WA_FCAT TO IT_FCAT.
*
*  CLEAR WA_FCAT.
*  WA_FCAT-FIELDNAME = 'KPOSN'.
*  WA_FCAT-TABNAME = 'IT_FINAL'.
*  WA_FCAT-SELTEXT_M = 'Cond.item.No'.
*  APPEND WA_FCAT TO IT_FCAT.


  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'KSCHL'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Condition Type'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'VTEXT'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Condition Name'.
  WA_FCAT-DO_SUM = 'X'.
  APPEND WA_FCAT TO IT_FCAT.


  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'KBETR'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Rate/Unit'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR WA_FCAT.
  WA_FCAT-FIELDNAME = 'KWERT'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  WA_FCAT-SELTEXT_M = 'Condition Value'.
  WA_FCAT-DO_SUM = 'X'.
  APPEND WA_FCAT TO IT_FCAT.








  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.




*CLEAR WA_FCAT.
  WA_SORT-FIELDNAME = 'EBELN'.
  WA_SORT-TABNAME = 'IT_FINAL'.
*  WA_FCAT-SELTEXT_M <= 'Po.no'.
  APPEND WA_SORT TO IT_SORT.




*ENDFORM.                    " FCAT


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
   IS_LAYOUT                         = WA_LAYOUT
     IT_FIELDCAT                       = IT_FCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
   IT_SORT                           = IT_SORT
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
      T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.




*  *----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'PO Pricing Condition Details' .
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
