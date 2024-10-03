*&---------------------------------------------------------------------*
*& Report  ZSALES_DUTY_REP
*&
*&---------------------------------------------------------------------*
*& Functional                  : Mr.Govindarajan M        *
*& Developer                   : Mr.Ramachandaran M                        *
*& Company                     : Sheenlac Paints Pvt Ltd                     *
*& Verified By                 : Mr.Govindarajan M                             *
*& Title                       : SALES EXCISE DUTY REGISTER           *
*& Report Name                 : ZSALES_DUTY_REP                                       *
*& Development Id              : kpabap                                      *
*& Related Information         : DUTY CALCULATION           *
*&---------------------------------------------------------------------  *

REPORT ZSALES_DUTY_REP.

TYPES : BEGIN OF GS_VBRK,
        VBELN TYPE VBAK-VBELN,
        VTWEG LIKE VBRK-VTWEG,
        FKDAT TYPE VBRK-FKDAT,
        FKART TYPE VBRK-FKART,
        KUNRG TYPE VBRK-KUNRG,
        KNUMV TYPE VBRK-KNUMV,
        WAERK TYPE VBRK-WAERK,
        BUKRS LIKE VBRK-BUKRS,
        GJAHR LIKE VBRK-GJAHR,
        NETWR TYPE VBRK-NETWR,
        SPART LIKE VBRK-SPART,
        END OF GS_VBRK.

TYPES : BEGIN OF GS_VBRP,
        VBELN TYPE VBELN_VF,
        POSNR TYPE VBRP-POSNR,
        WERKS TYPE VBRP-WERKS,
        AUBEL TYPE VBELN_VA,
        VKBUR LIKE VBRP-VKBUR,
        VGBEL TYPE VBRP-VGBEL,
        VGPOS TYPE VBRP-VGPOS,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        FKIMG TYPE VBRP-FKIMG,
        MEINS TYPE VBRP-MEINS,
        PRCTR TYPE VBRP-PRCTR,
        KURSK TYPE VBRP-KURSK,
        END OF GS_VBRP.

TYPES: BEGIN OF GS_KONV,
       KNUMV TYPE KONV-KNUMV,
       KPOSN TYPE KONV-KPOSN,
       KSCHL TYPE KONV-KSCHL,
       KWERT TYPE KONV-KWERT,
       KBETR TYPE KONV-KBETR,
       KAWRT TYPE KONV-KAWRT,
       KUNNR TYPE KUNNR_KO,
       MWSK1 TYPE KONV-MWSK1,
       KAPPL TYPE KONV-KAPPL,
       WAERS TYPE KONV-WAERS,
       KKURS TYPE KONV-KKURS,
       END OF GS_KONV.

TYPES : BEGIN OF GS_FINAL,
        VBELN TYPE VBAK-VBELN,
        VTWEG LIKE VBRK-VTWEG,
        FKDAT TYPE VBRK-FKDAT,
        FKART TYPE VBRK-FKART,
        KUNRG TYPE VBRK-KUNRG,
        KNUMV TYPE VBRK-KNUMV,
        WAERK TYPE VBRK-WAERK,
        BUKRS LIKE VBRK-BUKRS,
        GJAHR LIKE VBRK-GJAHR,
        NETWR TYPE VBRK-NETWR,
        SPART LIKE VBRK-SPART,
        POSNR TYPE VBRP-POSNR,
        WERKS TYPE VBRP-WERKS,
        AUBEL TYPE VBELN_VA,
        VKBUR LIKE VBRP-VKBUR,
        VGBEL TYPE VBRP-VGBEL,
        VGPOS TYPE VBRP-VGPOS,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        FKIMG TYPE VBRP-FKIMG,
        MEINS TYPE VBRP-MEINS,
        PRCTR TYPE VBRP-PRCTR,
        KURSK TYPE VBRP-KURSK,
        VALUE  TYPE P DECIMALS 2,
        BED  TYPE P DECIMALS 2,
        ECESS TYPE P DECIMALS 2,
        SHECSS TYPE P DECIMALS 2,
        TDUTY  TYPE P DECIMALS 2,
    END OF GS_FINAL.

*************************************************************************************

******            Internal table & Work area Declaration                        *****
*************************************************************************************

DATA: GT_VBRK TYPE TABLE OF GS_VBRK,
      WA_VBRK TYPE GS_VBRK,
      GT_VBRP TYPE TABLE OF GS_VBRP,
      WA_VBRP TYPE GS_VBRP,
      GT_KONV TYPE TABLE OF GS_KONV,
      WA_KONV TYPE GS_KONV,
      GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : OR_BUKRS TYPE VBRK-BUKRS,
       OR_VBELN TYPE VBRK-VBELN,
       OR_WERKS TYPE VBRP-WERKS,
       OR_FKDAT TYPE VBRK-FKDAT.

*************************************************************************************
******           selection-screen                                            ********                                           *****
*************************************************************************************

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: LV_BUKRS FOR OR_BUKRS OBLIGATORY,
                LV_WERKS FOR OR_WERKS OBLIGATORY,
                LV_VBELN FOR OR_VBELN,
                LV_FKDAT FOR OR_FKDAT .
SELECTION-SCREEN : END OF BLOCK B1.

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
    VBELN
    VTWEG
    FKDAT
    FKART
    KUNRG
    KNUMV
    FKART
    WAERK
    BUKRS
    GJAHR
    NETWR
    SPART FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK
    WHERE
         VBELN IN LV_VBELN AND  FKDAT IN LV_FKDAT AND BUKRS IN LV_BUKRS  AND FKART = 'YBDP' AND FKSTO <> 'X' . " AND FKART <> 'YBRE' .

  IF GT_VBRK[] IS NOT INITIAL.
    SELECT
    VBELN
    POSNR
    WERKS
    AUBEL
    VKBUR
    VGBEL
    VGPOS
    MATNR
    ARKTX
    FKIMG
    MEINS
    PRCTR
    KURSK FROM VBRP INTO TABLE GT_VBRP FOR ALL ENTRIES IN GT_VBRK
    WHERE VBELN = GT_VBRK-VBELN AND WERKS IN LV_WERKS .
  ENDIF.
  "IF GT_VBRP[] IS NOT INITIAL .


  IF GT_VBRK[] IS NOT INITIAL.

    SELECT
     KNUMV
     KPOSN
     KSCHL
     KWERT
     KBETR
     KAWRT
     KUNNR
     MWSK1
     KAPPL
     WAERS
     KKURS
     FROM PRCD_ELEMENTS INTO TABLE GT_KONV FOR ALL ENTRIES IN GT_VBRK
     WHERE KNUMV = GT_VBRK-KNUMV AND
           KSCHL IN  ('PR00', 'PR01', 'ZS00', 'ZPF1', 'Y004', 'Y007', 'ZEXP', 'ZECS', 'JHCS','ZHCS', 'JIVC', 'ZINS', 'ZSTO', 'JHCT', 'ZINS', 'VPRS', 'JVSR',
               'YBSD', 'Y029', 'Y007', 'YBRD',
              'ZKFR', 'JIVP', 'JADD', 'ZTCS', 'ZSBS', 'JSVD', 'JEC3', 'JEC4', 'ZAED' , 'YBAD' ) AND
           KINAK = ' '.

  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA .

*  BREAK-POINT.
  LOOP AT GT_VBRP INTO WA_VBRP.
    MOVE-CORRESPONDING WA_VBRP TO WA_FINAL.
    READ TABLE GT_VBRK INTO WA_VBRK WITH KEY VBELN = WA_VBRP-VBELN.
    WA_FINAL-FKDAT = WA_VBRK-FKDAT.
* Price
    LOOP AT  GT_KONV INTO WA_KONV WHERE  KNUMV = WA_VBRK-KNUMV AND  KPOSN = WA_VBRP-POSNR .
      CASE WA_KONV-KSCHL.
        WHEN 'PR00' OR 'PR01'.
          WA_FINAL-VALUE = WA_KONV-KWERT  .
        WHEN 'ZEXP'.
          WA_FINAL-BED =  WA_KONV-KWERT .
        WHEN 'ZHCS'.
          WA_FINAL-ECESS =  WA_KONV-KWERT .
        WHEN 'ZECS'.
          WA_FINAL-SHECSS = WA_KONV-KWERT .
      ENDCASE.
    ENDLOOP.


    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
      WA_FINAL-TDUTY = WA_FINAL-BED +  WA_FINAL-ECESS + WA_FINAL-SHECSS  .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TDUTY  .
      CLEAR WA_FINAL.
ENDLOOP.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
FORM FIELD_CATLOG .
  PERFORM ALV_LAYOUT USING 1 'Inv No' 'VBELN' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 2 'Inv Date' 'FKDAT' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Material Code' 'MATNR' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Des' 'ARKTX' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 5 'Unit' 'MEINS' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 6 'Qty' 'FKIMG' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 7 'Base Value' 'VALUE' 'GT_FINAL' 'X' '' .
  PERFORM ALV_LAYOUT USING 8 'BED' 'BED' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 9 'ECESS' 'ECESS' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 10 'SHECSS' 'SHECSS' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 11 'Total Duty' 'TDUTY' 'GT_FINAL' 'X' ''.

ENDFORM.                    " FIELD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT   USING P1 P2 P3 P4 P5 P6.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  APPEND WA_FCAT TO GT_FCAT.
ENDFORM.                    " ALV_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY .

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
      I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
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
      I_DEFAULT                         = 'X'
      I_SAVE                            = 'X'
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

  DATA : OR_WERKS(100) TYPE C.

  CLEAR : OR_WERKS.

  IF LV_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' LV_WERKS-LOW 'To' LV_WERKS-HIGH INTO OR_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code :' LV_WERKS-LOW INTO OR_WERKS SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = OR_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Sales Excise Duty Register' .
  APPEND LS_LINE TO LIT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
