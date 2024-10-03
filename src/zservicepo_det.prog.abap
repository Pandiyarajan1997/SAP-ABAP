*&---------------------------------------------------------------------*
*& Report  ZSERVICEPO_DET
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSERVICEPO_DET.

TABLES : EKKO,EKPO.


TYPES : BEGIN OF GS_EKKO,

      EBELN TYPE EKKO-EBELN,
      BUKRS TYPE EKKO-BUKRS,
      BSART TYPE EKKO-BSART,
      AEDAT TYPE EKKO-AEDAT,
      LIFNR TYPE EKKO-LIFNR,
      KONNR TYPE EKKO-KONNR,
      KTWRT TYPE EKKO-KTWRT,

      END OF GS_EKKO.

DATA : GT_EKKO TYPE TABLE OF GS_EKKO,
       WA_EKKO TYPE GS_EKKO.

DATA : GT_EKKO1 TYPE TABLE OF GS_EKKO,
       WA_EKKO1 TYPE GS_EKKO.

TYPES : BEGIN OF GS_EKPO,

        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE EKPO-MATNR,
        TXZ01 TYPE EKPO-TXZ01,
        WERKS TYPE EKPO-WERKS,
        MATKL TYPE EKPO-MATKL, " Mat.Group
        KNTTP TYPE EKPO-KNTTP, " Acct Assignment Category

        MENGE TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
        BPRME TYPE EKPO-BPRME,
        NETWR TYPE EKPO-NETWR,

        END OF GS_EKPO.

DATA : GT_EKPO TYPE TABLE OF GS_EKPO,
       WA_EKPO TYPE GS_EKPO.

TYPES : BEGIN OF GS_LFA1,

        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,

        END OF GS_LFA1.

DATA : GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE LFA1.


TYPES : BEGIN OF GS_FINAL,

         LIFNR TYPE EKKO-LIFNR,
         BSART TYPE EKKO-BSART,
         NAME1 TYPE LFA1-NAME1,
         KONNR TYPE EKKO-KONNR,
         KTWRT TYPE EKKO-KTWRT,

         EBELN TYPE EKPO-EBELN,

         EBELN1 TYPE EKPO-EBELN,

         EBELP TYPE EKPO-EBELP,
         MATNR TYPE EKPO-MATNR,
         TXZ01 TYPE EKPO-TXZ01,
         WERKS TYPE EKPO-WERKS,
         MENGE TYPE EKPO-MENGE,
         MEINS TYPE EKPO-MEINS,
         BPRME TYPE EKPO-BPRME,
         NETWR TYPE EKPO-NETWR,
         MATKL TYPE EKPO-MATKL, " Mat.Group
         KNTTP TYPE EKPO-KNTTP, " Acct Assignment Category

         POTOT TYPE P DECIMALS 2,
         FIN_VAL TYPE P DECIMALS 2,

         V_POCOUNT TYPE I,

         CELLCOLOR TYPE LVC_T_SCOL,

        END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA IS_FOUND TYPE ABAP_BOOL.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_AGREE TYPE EKKO-KTWRT.
DATA : LV_AGREE1 TYPE EKKO-KTWRT.

DATA : LV_POTOT TYPE EKPO-NETWR.

DATA : LV_POTOT1 TYPE EKKO-KTWRT.

DATA : LV_POTOT2 TYPE EKKO-KTWRT.

DATA: LV_POCOUNT TYPE I VALUE 0.

DATA : LV_BUKRS TYPE EKKO-BUKRS,
       LV_WERKS TYPE EKPO-WERKS,
       LV_EBELN TYPE EKKO-EBELN,
       LV_LIFNR TYPE EKKO-LIFNR,
       LV_AEDAT TYPE EKKO-AEDAT.


SELECTION-SCREEN : BEGIN OF BLOCK S WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: SO_BUKRS FOR LV_BUKRS OBLIGATORY,
                SO_WERKS FOR LV_WERKS,
                SO_EBELN FOR LV_EBELN,
                SO_LIFNR FOR LV_LIFNR,
                SO_AEDAT FOR LV_AEDAT.

*  PARAMETERS : W AS LISTBOX VISIBLE LENGTH 10.


SELECTION-SCREEN : END OF BLOCK S.

START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM GET_FIELDCAT.
  PERFORM BUILD_LAYOUT.
  PERFORM SET_CELL_COLOURS.
  PERFORM GT_DISPLAY.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA .


  SELECT
      EBELN
      BUKRS
      BSART
      AEDAT
      LIFNR
      KONNR
      KTWRT
   FROM
    EKKO INTO TABLE GT_EKKO WHERE EBELN IN SO_EBELN AND BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR AND AEDAT IN SO_AEDAT AND BSART = 'ZSR'.


  IF GT_EKKO[] IS NOT INITIAL.

    SELECT
      EBELN
      BUKRS
      BSART
      AEDAT
      LIFNR
      KONNR
      KTWRT
   FROM

     EKKO INTO TABLE GT_EKKO1 WHERE  ( BSART = 'ZWK' OR BSART = 'ZMK' ) AND BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR.

  ENDIF.

  IF GT_EKKO[] IS NOT INITIAL.

    SELECT
           LIFNR
           NAME1
     FROM
      LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO WHERE LIFNR = GT_EKKO-LIFNR.

  ENDIF.

  SELECT
      EBELN
      EBELP
      MATNR
      TXZ01
      WERKS
      MATKL
      KNTTP
      MENGE
      MEINS
      BPRME
      NETWR

  FROM
    EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO WHERE EBELN = GT_EKKO-EBELN AND WERKS IN SO_WERKS AND EBELN  IN SO_EBELN .

  LOOP AT GT_EKKO INTO WA_EKKO.

*         WA_FINAL-EBELN = WA_EKKO-EBELN.
    WA_FINAL-BSART = WA_EKKO-BSART .
    WA_FINAL-LIFNR = WA_EKKO-LIFNR.
    WA_FINAL-KONNR = WA_EKKO-KONNR.
*         WA_FINAL-KTWRT = WA_EKKO-KTWRT.

    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR.
    IF SY-SUBRC = 0.
      WA_FINAL-NAME1 = WA_LFA1-NAME1.
    ENDIF.

    LOOP AT GT_EKPO INTO WA_EKPO WHERE EBELN = WA_EKKO-EBELN.
      WA_FINAL-KONNR = WA_EKKO-KONNR.
      WA_FINAL-EBELN = WA_EKPO-EBELN.
*      WA_FINAL-EBELN1 = WA_EKPO-EBELN.
      WA_FINAL-EBELP = WA_EKPO-EBELP.
      WA_FINAL-MATNR = WA_EKPO-MATNR.
      WA_FINAL-TXZ01 = WA_EKPO-TXZ01.
      WA_FINAL-WERKS = WA_EKPO-WERKS.
      WA_FINAL-MENGE = WA_EKPO-MENGE.
      WA_FINAL-MEINS = WA_EKPO-MEINS.
      WA_FINAL-BPRME = WA_EKPO-BPRME.
      WA_FINAL-NETWR = WA_EKPO-NETWR.
      WA_FINAL-MATKL = WA_EKPO-MATKL.
      WA_FINAL-KNTTP = WA_EKPO-KNTTP.

      APPEND WA_FINAL TO GT_FINAL.
      CLEAR WA_FINAL.


    ENDLOOP.
  ENDLOOP.

  SORT GT_FINAL DESCENDING BY KONNR.


  LOOP AT GT_FINAL INTO WA_FINAL.

    LV_AGREE = WA_FINAL-KONNR.

*        READ TABLE GT_EKKO1 INTO WA_EKKO1 WITH KEY KONNR = WA_FINAL-KONNR.
  SORT GT_EKKO1.      "Added by SPLABAP during code remediation
    LOOP AT GT_EKKO1 INTO WA_EKKO1 ."WHERE  EBELN = WA_FINAL-EBELN.

      LV_AGREE1 = WA_EKKO1-EBELN.

      IF LV_AGREE = LV_AGREE1.

        ON CHANGE OF LV_AGREE.

          WA_FINAL-KTWRT = WA_EKKO1-KTWRT.

        ENDON.

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KONNR KTWRT.
        CLEAR WA_FINAL.

      ENDIF.

    ENDLOOP.

  ENDLOOP.



*BREAK-POINT.
*
*  LOOP AT GT_FINAL INTO WA_FINAL.
*
*    ON CHANGE OF WA_FINAL-EBELN.
*    CLEAR LV_POTOT.
*    ENDON.
*
*     LV_POTOT = LV_POTOT + WA_FINAL-NETWR .
*     ON CHANGE OF WA_FINAL-EBELN.
*     LV_POTOT1 = WA_FINAL-KTWRT.
*     ENDON.
**     MOVE LV_POTOT TO WA_FINAL-POTOT.
**    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING POTOT .
**    CLEAR WA_FINAL.
*
*     WA_FINAL-FIN_VAL = LV_POTOT1 - LV_POTOT.
*     MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FIN_VAL.
*     CLEAR WA_FINAL.
*   ENDLOOP.
*
*

*
*SORT GT_FINAL DESCENDING BY EBELN1 POTOT.
*
*BREAK-POINT.
*
*   LOOP AT GT_FINAL INTO WA_FINAL.
*
*      LV_POCOUNT = LV_POCOUNT + 1.
*
*      ON CHANGE OF WA_FINAL-EBELN.
*
*        WA_FINAL-V_POCOUNT = LV_POCOUNT.
*
*        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_POCOUNT.
*
*        CLEAR WA_FINAL.
*
*      ENDON.
*
*   ENDLOOP.
*
*
**LOOP AT GT_FINAL INTO WA_FINAL.
**
*  IF WA_FINAL-V_POCOUNT EQ 0 .
*
*    WA_FINAL-POTOT = ''.
*
*    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING POTOT .
*
*    CLEAR WA_FINAL.
*
*    ENDIF.
*
* ENDLOOP.

*   LOOP AT GT_FINAL INTO WA_FINAL.
*
**     IF WA_FINAL-NETWR IS NOT INITIAL AND WA_FINAL-KTWRT IS NOT  INITIAL.
*
*     WA_FINAL-POTOT = LV_POTOT.
*
*     MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING POTOT.
*     CLEAR WA_FINAL.
*
**     ENDIF.
*
*    ENDLOOP.


*SORT GT_FINAL DESCENDING BY EBELN1 KTWRT POTOT.
*
*BREAK-POINT.
*
*  LOOP AT GT_FINAL INTO WA_FINAL.
*
**    IF WA_FINAL-POTOT IS NOT INITIAL AND WA_FINAL-KTWRT IS NOT  INITIAL.
*
*     IF WA_FINAL-NETWR IS NOT INITIAL.
*
*       ON CHANGE OF WA_FINAL-EBELN.
**        AT LAST WITH EBELN.
*
*         LV_POTOT1 = WA_FINAL-KTWRT.
*       ENDON.
**         ENDAT.
*
*      WA_FINAL-FIN_VAL =  LV_POTOT1 - WA_FINAL-NETWR.
*
*      SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0'.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FIN_VAL LIFNR.
*      CLEAR WA_FINAL.
*
*    ENDIF.
*
*  ENDLOOP.

*SORT GT_FINAL ASCENDING BY EBELN.

*LOOP AT GT_FINAL INTO WA_FINAL.
*
*  ON CHANGE OF WA_FINAL-KONNR.
*   CLEAR LV_POTOT.
*  ENDON.
*
*  IF WA_FINAL-KTWRT IS NOT INITIAL.
*
*     LV_POTOT = WA_FINAL-KTWRT - WA_FINAL-NETWR.
*
*     WA_FINAL-POTOT = LV_POTOT.
*
*     MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING POTOT.
*     CLEAR WA_FINAL.
*
*
*   ENDIF.
*
* ENDLOOP.

*
*
*  BREAK-POINT.
*
*  LOOP AT GT_FINAL INTO WA_FINAL.
*
**    ON CHANGE OF WA_FINAL-KONNR.
**    CLEAR  LV_POTOT1.
**    ENDON.
* IF WA_FINAL-KONNR IS NOT INITIAL.
*
*    ON CHANGE OF WA_FINAL-KONNR.
*
*      LV_POTOT = WA_FINAL-KTWRT - WA_FINAL-NETWR .
*      WA_FINAL-FIN_VAL = LV_POTOT.
*      IS_FOUND = 1.
*
*    ENDON.
*
**     ON CHANGE OF WA_FINAL-EBELN.
**     LV_POTOT1 = WA_FINAL-KTWRT.
**     ENDON.
**     MOVE LV_POTOT TO WA_FINAL-POTOT.
**    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING POTOT .
**    CLEAR WA_FINAL.
*
*    IF IS_FOUND NE 1 AND SY-TABIX LE 2 .
*
*      LV_POTOT1 = LV_POTOT - WA_FINAL-NETWR.
*      WA_FINAL-FIN_VAL = LV_POTOT1.
*
*     ELSEIF IS_FOUND NE 1 AND SY-TABIX GE 3.
*
*         LV_POTOT2 = LV_POTOT1 - WA_FINAL-NETWR.
*         WA_FINAL-FIN_VAL = LV_POTOT2.
*      ENDIF.
*
*    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FIN_VAL.
*    CLEAR : WA_FINAL,IS_FOUND.*
*    ENDIF.
*  ENDLOOP.

*  BREAK-POINT.

  LOOP AT GT_FINAL INTO WA_FINAL.

    ON CHANGE OF WA_FINAL-KONNR." OR WA_FINAL-WERKS. WA_FINAL-KTWRT - WA_FINAL-NETWR
      CLEAR LV_POTOT.
    ENDON.
    IF WA_FINAL-KONNR NE ''.
      IF WA_FINAL-NETWR NE 0.
        LV_POTOT = LV_POTOT - WA_FINAL-KTWRT + WA_FINAL-NETWR.
        WA_FINAL-FIN_VAL = LV_POTOT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FIN_VAL.
        CLEAR WA_FINAL.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " GET_DATA



*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FIELDCAT .

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 3 'PO No' 'EBELN' 'GT_FINAL' '' '' 'X'.

*  PERFORM ALV_LAYOUT USING 4 'PO No1' 'EBELN1' 'GT_FINAL' '' '' ''.

  PERFORM ALV_LAYOUT USING 5 'PO Item' 'EBELP' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Document Type' 'BSART' 'GT_FINAL' '' '' ''.

  PERFORM ALV_LAYOUT USING 9 'Acct.Assign.Key' 'KNTTP' 'GT_FINAL' '' '' ''.

  PERFORM ALV_LAYOUT USING 11 'Material Code' 'MATNR' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 13 'Mat.Descrip' 'TXZ01' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 14 'Material Group' 'MATKL' 'GT_FINAL' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 15 'PO Qty' 'MENGE' 'GT_FINAL' '' 'X' ''.
*  PERFORM ALV_LAYOUT USING 17 'Base Unit' 'MEINS' 'GT_FINAL' '' '' ''.

*  PERFORM ALV_LAYOUT USING 18 'Order Unit' 'BPRME' 'GT_FINAL' '' '' ''.

  PERFORM ALV_LAYOUT USING 21 'PO Value' 'NETWR' 'GT_FINAL' 'X' 'X' ''.
  PERFORM ALV_LAYOUT USING 23 'Vendor Code' 'LIFNR' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 25 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 27 'Agre No' 'KONNR' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 29 'Agre Value' 'KTWRT' 'GT_FINAL' 'X' 'X' ''.
*  PERFORM ALV_LAYOUT USING 31 'PO-Totvalue' 'POTOT' 'GT_FINAL' '' 'X' ''.
*  PERFORM ALV_LAYOUT USING 32 'V_POCOUNT' 'V_POCOUNT' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 33 'Pending Value' 'FIN_VAL' 'GT_FINAL' '' 'X' ''.

  WA_SORT-FIELDNAME = 'KONNR'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

*
*  WA_SORT-FIELDNAME = 'KONNR'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.
*
*
*  WA_SORT-FIELDNAME = 'KONNR'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

ENDFORM.                    " GET_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0346   text
*      -->P_0347   text
*      -->P_0348   text
*      -->P_0349   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING P1 P2 P3 P4 P5 P6 P7.

  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  WA_FCAT-HOTSPOT = P7.

  APPEND WA_FCAT TO GT_FCAT.
  CLEAR WA_FCAT.



ENDFORM.                    " ALV_LAYOUT



*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT.
*  LAYOUT-NO_INPUT          = 'X'.
*  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  LAYOUT-TOTALS_TEXT       = 'Totals'(201).
  LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  "CTAB_FNAME
ENDFORM.                    " BUILD_LAYOUT




*&---------------------------------------------------------------------*
*&      Form  SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*       Set colour of individual ALV cell, field
*----------------------------------------------------------------------*
FORM SET_CELL_COLOURS .

  DATA: WA_CELLCOLOR TYPE LVC_S_SCOL.
  DATA: LD_INDEX TYPE SY-TABIX.

*BREAK-POINT.

  LOOP AT GT_FINAL INTO WA_FINAL.
    LD_INDEX = SY-TABIX.

    IF WA_FINAL-FIN_VAL GT 0.
      WA_CELLCOLOR-FNAME = 'FIN_VAL'.
      WA_CELLCOLOR-COLOR-COL = 6.  "color code 1-7, if outside rage defaults to 7
      WA_CELLCOLOR-COLOR-INT = '0'.  "1 = Intensified on, 0 = Intensified off
      WA_CELLCOLOR-COLOR-INV = '0'.  "1 = text colour, 0 = background colour
      APPEND WA_CELLCOLOR TO WA_FINAL-CELLCOLOR.
      MODIFY GT_FINAL FROM WA_FINAL INDEX LD_INDEX TRANSPORTING CELLCOLOR.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_CELL_COLOURS

*&---------------------------------------------------------------------*
*&      Form  GT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GT_DISPLAY .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                =  SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND            = 'COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                        = GT_FCAT[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
     IT_SORT                            = GT_SORT[]
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = GT_FINAL.
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2
  .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " GT_DISPLAY




*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.

  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.

  DATA :
         LV_WERKS(100) TYPE C.
*         LV_BEDAT(50) TYPE C,
*         LV_LIFNR(100) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : LV_WERKS.
*          LV_LIFNR,
  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO LV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW INTO LV_WERKS SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Service Po & Agreement Details ' .
  APPEND LS_LINE TO LIT_HEADER.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.

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


*&---------------------------------------------------------------------*
*&      Form  COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->S_UCOMM    text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM COMMAND USING S_UCOMM LIKE SY-UCOMM SELFIELD TYPE SLIS_SELFIELD.


*BREAK-POINT.

  CASE S_UCOMM.


    WHEN '&IC1'.

      IF SELFIELD-FIELDNAME = 'EBELN'.

        SET PARAMETER ID 'BES' FIELD SELFIELD-VALUE.

        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      ENDIF.

  ENDCASE.



ENDFORM.                    "COMMAND
