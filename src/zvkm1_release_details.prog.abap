*&---------------------------------------------------------------------*
*& Report  ZVKM1_RELEASE_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZVKM1_RELEASE_DETAILS.

TYPES : BEGIN OF GS_VBAK,
        MANDT TYPE VBAK-MANDT,
        VBELN TYPE VBAK-VBELN,
        ERDAT TYPE VBAK-ERDAT,
        ERZET TYPE VBAK-ERZET,
        ERNAM TYPE VBAK-ERNAM,
        VBTYP TYPE VBAK-VBTYP,
        LIFSK TYPE VBAK-LIFSK,
        FAKSK TYPE VBAK-FAKSK,
        NETWR TYPE VBAK-NETWR,
        WAERK TYPE VBAK-WAERK,
        VKORG TYPE VBAK-VKORG,
        VTWEG TYPE VBAK-VTWEG,
        VKBUR TYPE VBAK-VKBUR,
        KUNNR TYPE VBAK-KUNNR,
      END OF GS_VBAK.

TYPES : BEGIN OF GS_KNA1,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
         REGIO TYPE KNA1-REGIO,
      END OF GS_KNA1.


TYPES : BEGIN OF GS_VBUK,
**            MANDT TYPE VBUK-MANDT,
            "VBELN TYPE CDHDR-OBJECTID,
            VBELN TYPE VBUK-VBELN,
            RFSTK TYPE VBUK-RFSTK,
            RFGSK TYPE VBUK-RFGSK,
            BESTK TYPE VBUK-BESTK,
            LFSTK TYPE VBUK-LFSTK,
            LFGSK TYPE VBUK-LFGSK,
            WBSTK TYPE VBUK-WBSTK,
            FKSTK TYPE VBUK-FKSTK,
            CMPSB TYPE VBUK-CMPSB,
            CMPSG TYPE VBUK-CMPSG,
            CMGST TYPE VBUK-CMGST,
          END OF GS_VBUK.

TYPES : BEGIN OF GS_VBRK,
      MANDT TYPE VBRK-MANDT,
      VBELN TYPE VBRK-VBELN,
      FKART TYPE VBRK-FKART,
      VBTYP TYPE VBRK-VBTYP,
      WAERK TYPE VBRK-WAERK,
      VKORG TYPE VBRK-VKORG,
      NETWR TYPE VBRK-NETWR,
END OF GS_VBRK.

TYPES  : BEGIN OF GS_VBRP,
           MANDT TYPE VBRP-MANDT,
           VBELN TYPE VBRP-VBELN,
           AUBEL TYPE CDHDR-OBJECTID,
           AUPOS TYPE VBRP-AUPOS,
        END OF GS_VBRP.


TYPES : BEGIN OF GS_CDHDR,
        MANDANT TYPE CDHDR-MANDANT,
        OBJECTCLAS TYPE CDHDR-OBJECTCLAS,
        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE CDHDR-CHANGENR,
        USERNAME TYPE CDHDR-USERNAME,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,
        TCODE TYPE CDHDR-TCODE,
      END OF GS_CDHDR.

TYPES : BEGIN OF GS_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
        END OF GS_T001W.

TYPES : BEGIN OF GS_FINAL,
        SL_NO TYPE INT2,
        MANDT TYPE VBAK-MANDT,
        VBELN TYPE VBAK-VBELN,
        ERDAT TYPE VBAK-ERDAT,
        ERZET TYPE VBAK-ERZET,
        ERNAM TYPE VBAK-ERNAM,
        VBTYP TYPE VBAK-VBTYP,
        LIFSK TYPE VBAK-LIFSK,
        FAKSK TYPE VBAK-FAKSK,
        NETWR TYPE VBAK-NETWR,
        WAERK TYPE VBAK-WAERK,
        VKORG TYPE VBAK-VKORG,
        VTWEG TYPE VBAK-VTWEG,
        VKBUR TYPE VBAK-VKBUR,
        KUNNR TYPE VBAK-KUNNR,
        NAME1 TYPE KNA1-NAME1,
        REGIO TYPE KNA1-REGIO,
        USERNAME TYPE CDHDR-USERNAME,
        UDATE TYPE CDHDR-UDATE,
        STA TYPE CHAR30,
        STA1 TYPE CHAR30,
        STATUS TYPE CHAR45,
        INVO TYPE VBRP-VBELN,
        BRNAME TYPE T001W-NAME1,
        INV_NE  TYPE VBRK-NETWR,
      END OF GS_FINAL.

DATA : GT_VBAK TYPE TABLE OF GS_VBAK,
       WA_VBAK TYPE GS_VBAK.

DATA : GT_KNA1 TYPE TABLE OF GS_KNA1,
       WA_KNA1 TYPE GS_KNA1.

DATA : GT_VBRP TYPE TABLE OF GS_VBRP,
     WA_VBRP TYPE GS_VBRP.

DATA : GT_VBUK TYPE TABLE OF GS_VBUK,
       WA_VBUK TYPE GS_VBUK.

DATA : GT_VBRK  TYPE TABLE OF GS_VBRK,
       WA_VBRK TYPE GS_VBRK.

DATA : GT_CDHDR TYPE TABLE OF GS_CDHDR,
       WA_CDHDR TYPE GS_CDHDR.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W.

DATA : LV_ERDAT TYPE VBAK-ERDAT,
       LV_VKBUR TYPE VBAK-VKBUR.

DATA :   GT_FINAL TYPE TABLE OF GS_FINAL,
         WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
     IT_SORT TYPE SLIS_T_SORTINFO_ALV,
     WA_FCAT TYPE SLIS_FIELDCAT_ALV,
     WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : LV_NUM TYPE INT2.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS  : SO_VKBUR FOR LV_VKBUR OBLIGATORY,
                    SO_ERDAT FOR LV_ERDAT OBLIGATORY .

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

  SELECT
    MANDT
    VBELN
    ERDAT
    ERZET
    ERNAM
    VBTYP
    LIFSK
    FAKSK
    NETWR
    WAERK
    VKORG
    VTWEG
    VKBUR
    KUNNR
     FROM VBAK INTO TABLE GT_VBAK WHERE ERDAT IN SO_ERDAT AND VKBUR IN SO_VKBUR.

  SELECT
       KUNNR
       NAME1
       REGIO
     FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_VBAK WHERE KUNNR = GT_VBAK-KUNNR.

  SELECT "#EC CI_DB_OPERATION_OK[2198647] " Added by <IT-CAR Tool> during Code Remediation
*    MANDT,
    VBELN,
    RFSTK,
    RFGSK,
    BESTK,
    LFSTK,
    LFGSK,
    WBSTK,
    FKSTK,
    CMPSB,
    CMPSG,
    CMGST
**    FROM VBUK INTO TABLE GT_VBUK
    FROM V_VBUK_S4 INTO TABLE @GT_VBUK
    FOR ALL ENTRIES IN @GT_VBAK WHERE VBELN = @GT_VBAK-VBELN
    AND ( CMPSB EQ 'B' OR CMPSG = 'B' ) . "AND CMGST  .

  SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
      MANDT
      VBELN
      AUBEL
      AUPOS
         FROM VBRP INTO TABLE GT_VBRP FOR ALL ENTRIES IN GT_VBUK WHERE AUBEL = GT_VBUK-VBELN.

SORT GT_VBRP BY VBELN . " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_VBRP COMPARING VBELN .
  SELECT
     MANDT
     VBELN
     FKART
     VBTYP
     WAERK
     VKORG
     NETWR
        FROM VBRK INTO TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP WHERE VBELN = GT_VBRP-VBELN.

  IF GT_VBRP IS NOT INITIAL.
    SELECT
           MANDANT
           OBJECTCLAS
           OBJECTID
           CHANGENR
           USERNAME
           UDATE
           UTIME
           TCODE
              "FROM CDHDR INTO TABLE GT_CDHDR FOR ALL ENTRIES IN GT_VBUK WHERE OBJECTID = GT_VBUK-VBELN AND ( TCODE EQ 'VKM3' OR TCODE EQ 'ZVKM1') .
      FROM CDHDR INTO TABLE GT_CDHDR FOR ALL ENTRIES IN GT_VBRP WHERE OBJECTID = GT_VBRP-AUBEL AND ( TCODE EQ 'VKM3' OR TCODE EQ 'ZVKM1') .
  ENDIF.

  SELECT
       WERKS
       NAME1 FROM T001W INTO TABLE GT_T001W .

SORT GT_CDHDR BY OBJECTID. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_CDHDR COMPARING OBJECTID.


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

*LOOP AT GT_VBUK INTO WA_VBUK .
*MOVE-CORRESPONDING WA_VBUK TO WA_FINAL .
*  IF WA_VBUK-CMPSB EQ 'B'.
*      WA_FINAL-STA = 'Oldest Item'.
*  ENDIF.
*  IF WA_VBUK-CMPSG EQ 'B' .
*     WA_FINAL-STA1 = 'Party Lock'.
*  ENDIF.
* CONCATENATE WA_FINAL-STA WA_FINAL-STA1 INTO WA_FINAL-STATUS SEPARATED BY space.
*
* READ TABLE GT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_VBUK-VBELN.
*   WA_FINAL-ERDAT = WA_VBAK-ERDAT.
*   WA_FINAL-KUNNR = WA_VBAK-KUNNR.
*   WA_FINAL-NETWR = WA_VBAK-NETWR.
*"  WA_FINAL-STATUS = WA_FINAL-STA + WA_FINAL-STA1  .
*
*READ TABLE GT_VBRP INTO WA_VBRP WITH KEY AUBEL = WA_VBAK-VBELN.
*IF SY-SUBRC EQ 0 .
*  WA_FINAL-INVO = WA_VBRP-VBELN .
*ENDIF.
*READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR.
*  WA_FINAL-NAME1 = WA_KNA1-NAME1.
*  WA_FINAL-REGIO = WA_KNA1-REGIO.
*READ TABLE GT_CDHDR INTO WA_CDHDR WITH KEY OBJECTID = WA_VBAK-VBELN.
*  WA_FINAL-USERNAME = WA_CDHDR-USERNAME.
*  WA_FINAL-UDATE = WA_CDHDR-UDATE.
*  "MOVE-CORRESPONDING WA_VBAK TO GT_FINAL.
*
*  APPEND WA_FINAL TO GT_FINAL.
*  CLEAR : WA_VBAK,WA_FINAL.
*
*ENDLOOP.

  LV_NUM = 0.

  LOOP AT GT_CDHDR INTO WA_CDHDR .
    WA_FINAL-SL_NO = LV_NUM + 1 .
    WA_FINAL-USERNAME = WA_CDHDR-USERNAME.
    WA_FINAL-UDATE = WA_CDHDR-UDATE.
    READ TABLE GT_VBUK INTO WA_VBUK WITH KEY VBELN = WA_CDHDR-OBJECTID .
    WA_FINAL-VBELN = WA_VBUK-VBELN.
    IF WA_VBUK-CMPSB EQ 'B'.
      WA_FINAL-STA = 'Oldest Item'.
    ENDIF.
    IF WA_VBUK-CMPSG EQ 'B' .
      WA_FINAL-STA1 = 'Limit Exceed'.
    ENDIF.

    CONCATENATE WA_FINAL-STA WA_FINAL-STA1 INTO WA_FINAL-STATUS SEPARATED BY SPACE.

*  IF WA_FINAL-STA IS NOT INITIAL AND WA_FINAL-STA1 IS NOT INITIAL.
*
*
*
*  ENDIF.

    READ TABLE GT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_VBUK-VBELN.
    WA_FINAL-ERDAT = WA_VBAK-ERDAT.
    WA_FINAL-KUNNR = WA_VBAK-KUNNR.
    WA_FINAL-NETWR = WA_VBAK-NETWR.
    READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR.
    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    WA_FINAL-REGIO = WA_KNA1-REGIO.
    READ TABLE GT_VBRP INTO WA_VBRP WITH KEY AUBEL = WA_VBAK-VBELN.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-INVO = WA_VBRP-VBELN .
    ENDIF.
    READ TABLE GT_VBRK INTO WA_VBRK WITH KEY VBELN = WA_VBRP-VBELN.
    IF SY-SUBRC EQ 0.
      WA_FINAL-INV_NE = WA_VBRK-NETWR.
    ENDIF.

    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_VBAK-VKBUR .
    IF SY-SUBRC = 0 .
      WA_FINAL-BRNAME = WA_T001W-NAME1.
    ENDIF.
    LV_NUM = WA_FINAL-SL_NO .
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR : WA_FINAL.

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

  PERFORM ALV_LAYOUT USING 10 'Sales Document' 'VBELN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Created on' 'ERDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Customer Code' 'KUNNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Customer Name' 'NAME1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Region Code' 'REGIO' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 9 'Branch Name' 'BRNAME' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Invoice Value' 'INV_NE' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 5 'User ID' 'USERNAME' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Release Date' 'UDATE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Status' 'STATUS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Invoice No' 'INVO' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 12 'Sales order Value' 'NETWR' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 1 'S.No' 'SL_NO' 'GT_FINAL' ''.
ENDFORM.                    " FIELD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0257   text
*      -->P_0258   text
*      -->P_0259   text
*      -->P_0260   text
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
*   EXCEPTIONS
*     PROGRAM_ERROR                  = 1
*     OTHERS                         = 2
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
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Sales order Release Details Report' .
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
