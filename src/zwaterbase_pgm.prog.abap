*&---------------------------------------------------------------------*
*& Report  ZWATERBASE_PGM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZWATERBASE_PGM.

TABLES: ZWATBASE_TABLE,T001W.

TYPE-POOLS:SLIS.

TYPES:BEGIN OF ZWATBASE_TY,
  SL_NO TYPE ZWATBASE_TABLE-SL_NO,
  PLANT TYPE ZWATBASE_TABLE-PLANT,
  ALTERNATIVE TYPE ZWATBASE_TABLE-ALTERNATIVE,
  MATNR TYPE ZWATBASE_TABLE-MATNR,
  STAGE TYPE ZWATBASE_TABLE-STAGE,
  MATNR_ALT TYPE ZWATBASE_TABLE-MATNR_ALT,
  MATNR_PRO TYPE ZWATBASE_TABLE-MATNR_PRO,
  MATNR_PRO1 TYPE ZWATBASE_TABLE-MATNR_PRO1,
  MATNR_PRO2 TYPE ZWATBASE_TABLE-MATNR_PRO2,
  MATNR_PRO3 TYPE ZWATBASE_TABLE-MATNR_PRO3,
  END OF ZWATBASE_TY.

DATA: ZWATBASE_IT TYPE STANDARD TABLE OF ZWATBASE_TY.
DATA: ZWATBASE_WA TYPE ZWATBASE_TY.

DATA: ZWATBASE_SAVE_IT TYPE TABLE OF ZWATBASE_TABLE,
      ZWATBASE_SAVE_WA TYPE ZWATBASE_TABLE.

DATA : OUT_LEN LIKE DD03P-OUTPUTLEN VALUE '600'.

TYPES: BEGIN OF T001W_TY,
       WERKS TYPE WERKS_D,
       NAME1 TYPE NAME1,
  END OF T001W_TY.

DATA: IT  TYPE STANDARD TABLE OF T001W_TY,
      WA  TYPE T001W_TY.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA: IT_HEADER TYPE SLIS_T_LISTHEADER,
      WA_HEADER TYPE SLIS_LISTHEADER.

DATA: LT_FCL TYPE STANDARD TABLE OF SLIS_FIELDCAT_ALV,
      WA_FCL TYPE SLIS_FIELDCAT_ALV.

DATA: F_MATNR TYPE ZWATBASE_TABLE-MATNR,
      T_MATNR TYPE ZWATBASE_TABLE-MATNR,
      PNAM TYPE T001W-NAME1.

DATA: CONCAT(50) TYPE C.
DATA: CONPLNT(50) TYPE C.
DATA: HED(50) TYPE C.

DATA: L_COM VALUE ''''.

SELECTION-SCREEN:BEGIN OF BLOCK WATBASE WITH FRAME TITLE TEXT-001.
PARAMETERS: WB_PLANT LIKE ZWATBASE_TABLE-PLANT.
SELECT-OPTIONS: WB_MATNR FOR ZWATBASE_TABLE-MATNR .
SELECTION-SCREEN:END OF BLOCK WATBASE.

PERFORM GETDATA.
PERFORM ALV_FIELDCAT.
PERFORM ALV_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETDATA .
  MOVE: WB_MATNR-LOW TO F_MATNR,
        WB_MATNR-HIGH TO T_MATNR.

  IF WB_PLANT IS INITIAL AND WB_MATNR IS INITIAL.
    MESSAGE 'Please Enter Plant Number OR Material Number' TYPE 'I'.
    SUBMIT ZWATERBASE_PGM VIA SELECTION-SCREEN.
  ENDIF.

  IF NOT WB_PLANT IS INITIAL AND WB_MATNR IS NOT INITIAL.
    SELECT SL_NO
           PLANT
           ALTERNATIVE
           MATNR
           STAGE
           MATNR_ALT
           MATNR_PRO
           MATNR_PRO1
           MATNR_PRO2
           MATNR_PRO3 FROM ZWATBASE_TABLE INTO TABLE ZWATBASE_IT WHERE PLANT EQ WB_PLANT AND MATNR IN WB_MATNR.
    SELECT SINGLE
            WERKS
            NAME1 FROM T001W INTO WA WHERE WERKS EQ WB_PLANT.
    MOVE:  WA-NAME1 TO PNAM.
    IF ZWATBASE_IT IS INITIAL .
      MESSAGE 'Please Enter Valid Entry' TYPE 'I'.
      SUBMIT ZWATERBASE_PGM VIA SELECTION-SCREEN.
    ENDIF.


  ELSE.
    IF NOT WB_PLANT IS INITIAL AND WB_MATNR IS INITIAL .
      SELECT SL_NO
             PLANT
             ALTERNATIVE
             MATNR
             STAGE
             MATNR_ALT
             MATNR_PRO
             MATNR_PRO1
             MATNR_PRO2
             MATNR_PRO3 FROM ZWATBASE_TABLE INTO TABLE ZWATBASE_IT WHERE PLANT EQ WB_PLANT .

      SELECT SINGLE
          WERKS
          NAME1 FROM T001W INTO WA WHERE WERKS EQ WB_PLANT.
      MOVE:  WA-NAME1 TO PNAM.
      IF ZWATBASE_IT IS INITIAL .
        MESSAGE 'Please Enter Valid Entry' TYPE 'I'.
        SUBMIT ZWATERBASE_PGM VIA SELECTION-SCREEN.
      ENDIF.
    ENDIF.

    IF NOT WB_MATNR IS INITIAL AND WB_PLANT IS INITIAL.
      SELECT SL_NO
             PLANT
             ALTERNATIVE
             MATNR
             STAGE
             MATNR_ALT
             MATNR_PRO
             MATNR_PRO1
             MATNR_PRO2
             MATNR_PRO3 FROM ZWATBASE_TABLE INTO TABLE ZWATBASE_IT WHERE MATNR IN WB_MATNR.
      IF ZWATBASE_IT IS INITIAL .
        MESSAGE 'Please Enter Valid Entry' TYPE 'I'.
        SUBMIT ZWATERBASE_PGM VIA SELECTION-SCREEN.
      ENDIF.
    ENDIF.
  ENDIF.

*  LOOP AT ZWATBASE_IT INTO ZWATBASE_WA.
*    SHIFT ZWATBASE_WA-MATNR LEFT DELETING LEADING '0'.
*    SHIFT ZWATBASE_WA-SL_NO LEFT DELETING LEADING '0'.
*
*    SHIFT PNAM LEFT DELETING LEADING '0'.
*
*    MODIFY ZWATBASE_IT FROM ZWATBASE_WA TRANSPORTING SL_NO MATNR.
*  ENDLOOP.

ENDFORM.                    " GETDATA


*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT .
  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '0'.
  WA_FCL-FIELDNAME = 'SL_NO'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'SL No'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '1'.
  WA_FCL-FIELDNAME = 'PLANT'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Plant'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '2'.
  WA_FCL-FIELDNAME = 'ALTERNATIVE'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Alternative BOM'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '3'.
  WA_FCL-FIELDNAME = 'MATNR'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Header Material'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '4'.
  WA_FCL-FIELDNAME = 'STAGE'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Order Sequence'.
  WA_FCL-EDIT = 'X'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '5'.
  WA_FCL-FIELDNAME = 'MATNR_ALT'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Sub Component'.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '6'.
  WA_FCL-FIELDNAME = 'MATNR_PRO'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_L = 'Processing Instruction'.
  WA_FCL-EDIT = 'X'.
  WA_FCL-OUTPUTLEN = OUT_LEN.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '6'.
  WA_FCL-FIELDNAME = 'MATNR_PRO1'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_L = 'Processing Instruction'.
  WA_FCL-EDIT = 'X'.
  WA_FCL-OUTPUTLEN = OUT_LEN.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '6'.
  WA_FCL-FIELDNAME = 'MATNR_PRO2'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_L = 'Processing Instruction'.
  WA_FCL-EDIT = 'X'.
  WA_FCL-OUTPUTLEN = OUT_LEN.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

*IF ZWATBASE_WA-MATNR_PRO3 IS NOT INITIAL.

  WA_FCL-ROW_POS = '1'.
  WA_FCL-COL_POS = '7'.
  WA_FCL-FIELDNAME = 'MATNR_PRO3'.
  WA_FCL-TABNAME = ZWATBASE_WA ."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_L = 'Processing Instruction'.
  WA_FCL-EDIT = 'X'.
  WA_FCL-OUTPUTLEN = OUT_LEN.
  APPEND: WA_FCL TO LT_FCL.
  CLEAR: WA_FCL.

*ENDIF.


ENDFORM.                    " ALV_FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

*  CONCATENATE 'BOM' L_COM 'S  MANUFACTURING INSTRUCTIONS' INTO HED.

*  CONCATENATE |BOM's MANUFACTURING INSTRUCTIONS| INTO HED.

*  HED = |BOM's MANUFACTURING INSTRUCTIONS|.
  WA_HEADER-TYP = 'H'.
  WA_HEADER-KEY = ' '.
  WA_HEADER-INFO = |BOM'S MANUFACTURING INSTRUCTIONS|.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Plant Number :'.
  IF NOT WB_PLANT IS INITIAL .
    WA_HEADER-INFO = WB_PLANT.
  ELSE.
    WA_HEADER-INFO = 'Not Mentioned'.
  ENDIF.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR:WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Plant Name :'.
  IF NOT WB_PLANT IS INITIAL.
    WA_HEADER-INFO = PNAM.
  ELSE.
    WA_HEADER-INFO = 'Not Mentioned'.
  ENDIF.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR:WA_HEADER.

  CONCATENATE ' From: ' F_MATNR ' TO: ' T_MATNR INTO CONCAT."#EC CI_FLDEXT_OK[2215424]
  "Added by SPLABAP during code remediation
  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Material Number :'.
  IF F_MATNR IS NOT INITIAL AND T_MATNR IS NOT INITIAL.
    WA_HEADER-INFO = CONCAT.
  ELSE.
    IF F_MATNR IS NOT INITIAL.
      WA_HEADER-INFO = F_MATNR.
    ELSEIF T_MATNR IS NOT INITIAL.
      WA_HEADER-INFO = T_MATNR.
    ELSE.
      WA_HEADER-INFO = 'Not Mentioned'.
    ENDIF.
  ENDIF.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
    I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
    I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE           = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                        =  LAYOUT
     IT_FIELDCAT                      =  LT_FCL
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
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
      T_OUTTAB                          = ZWATBASE_IT
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
*CLEAR: WATER_QUALITY.
ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .
ENDFORM.                    "TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE SY-UCOMM.
    WHEN '&DATA_SAVE'.
      LOOP AT ZWATBASE_IT INTO ZWATBASE_WA.
        IF ZWATBASE_WA IS NOT INITIAL.
          MOVE-CORRESPONDING ZWATBASE_WA TO ZWATBASE_SAVE_WA.
          APPEND ZWATBASE_SAVE_WA TO ZWATBASE_SAVE_IT.
        ENDIF.
        UPDATE ZWATBASE_TABLE FROM TABLE ZWATBASE_SAVE_IT .
        COMMIT WORK.
      ENDLOOP.
      MESSAGE 'Record Saved Sucessfully' TYPE 'I'.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
