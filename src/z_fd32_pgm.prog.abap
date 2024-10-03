*&---------------------------------------------------------------------*
*& Report  Z_FD32_PGM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_FD32_PGM
              NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPES: BEGIN OF TY_FD32,
           BUKRS TYPE KNB1-BUKRS,
           KUNNR TYPE KNA1-KUNNR,
           "NAME1 TYPE KNA1-NAME1,
           NETWR TYPE VBRK-NETWR,
        END OF TY_FD32.

DATA:   G_FILE TYPE STRING,
        G_MSG TYPE STRING.

DATA: IT_FD32 TYPE TABLE OF TY_FD32,
      WA_FD32 TYPE TY_FD32.

TYPES:BEGIN OF TY_ZCUST_CRE_CON ,
  BUKRS TYPE ZCUST_CRE_CON-BUKRS,
  KUNNR TYPE ZCUST_CRE_CON-KUNNR,
  NAME1 TYPE ZCUST_CRE_CON-NAME1,

  KLIMK TYPE ZCUST_CRE_CON-KLIMK,
  KLIMK_CUR TYPE ZCUST_CRE_CON-KLIMK_CUR,
  FBL5N_BAL TYPE ZCUST_CRE_CON-FBL5N_BAL,
  END OF TY_ZCUST_CRE_CON.

DATA: IT_ZCUST_CRE_CON TYPE TABLE OF TY_ZCUST_CRE_CON,
      WA_ZCUST_CRE_CON TYPE TY_ZCUST_CRE_CON.

DATA: CUN_UT TYPE INT2,
      CUN_IT TYPE INT2.

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.
DATA : LV_FILE TYPE RLGRAP-FILENAME.

TYPES:BEGIN OF TY_ERROR,
  BUKRS TYPE KNB1-BUKRS,
  KUNNR TYPE KNA1-KUNNR,
  "NAME1 TYPE KNA1-NAME1,
  END OF TY_ERROR.

DATA: IT_ERROR TYPE TABLE OF TY_ERROR,
      WA_ERROR TYPE TY_ERROR.

SELECTION-SCREEN:BEGIN OF BLOCK PRAM WITH FRAME TITLE TEXT-001.
PARAMETERS P_FILE TYPE LOCALFILE OBLIGATORY.
SELECTION-SCREEN:END OF BLOCK PRAM.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

    CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.


START-OF-SELECTION.


LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          = 'x'
    I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  IT_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       =  IT_FD32[]
 EXCEPTIONS
   CONVERSION_FAILED          = 1
  OTHERS                     = 2
            .

  PERFORM: GET_DATA.
  PERFORM: UPDATE_DATA.
  PERFORM: ERR_DISP.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .


  SELECT  BUKRS
          KUNNR
          NAME1
          KLIMK
          KLIMK_CUR
          FBL5N_BAL FROM ZCUST_CRE_CON INTO TABLE IT_ZCUST_CRE_CON . "FOR ALL ENTRIES IN IT_FD32
                                                                      "  WHERE BUKRS = IT_FD32-BUKRS AND KUNNR = IT_FD32-KUNNR .



ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_DATA .

  DATA: TEM_KUNNR TYPE KNA1-KUNNR,
        TEM_BUKRS TYPE KNb1-BUKRS.

 LOOP AT IT_ZCUST_CRE_CON INTO WA_ZCUST_CRE_CON .

   SHIFT WA_ZCUST_CRE_CON-KUNNR LEFT DELETING LEADING '0'.
   MODIFY IT_ZCUST_CRE_CON FROM WA_ZCUST_CRE_CON TRANSPORTING KUNNR.
   CLEAR WA_ZCUST_CRE_CON.

 ENDLOOP.

  LOOP AT IT_FD32 INTO WA_FD32.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = WA_FD32-KUNNR
     IMPORTING
       OUTPUT        = TEM_KUNNR .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = WA_FD32-BUKRS
     IMPORTING
       OUTPUT        = TEM_BUKRS .

    READ TABLE IT_ZCUST_CRE_CON INTO WA_ZCUST_CRE_CON WITH KEY KUNNR = WA_FD32-KUNNR BUKRS = WA_FD32-BUKRS .
    IF WA_ZCUST_CRE_CON IS NOT INITIAL.
      Data: lv_klimk type KLIMK."Added by SPLABAP during code remediation
      lv_klimk = CONV KLIMK( WA_FD32-NETWR )."Added by SPLABAP during code remediation
*        UPDATE ZCUST_CRE_CON SET KLIMK = WA_FD32-NETWR "Commented by SPLABAP during code remediation
        UPDATE ZCUST_CRE_CON SET KLIMK = lv_klimk"Added by SPLABAP during code remediation
        WHERE BUKRS = TEM_BUKRS AND KUNNR = TEM_KUNNR .
      CLEAR: WA_ZCUST_CRE_CON .
      CUN_IT = CUN_IT + 1 .
      COMMIT WORK.
    ELSE.
      WA_ERROR-BUKRS = WA_FD32-BUKRS.
      WA_ERROR-KUNNR = WA_FD32-KUNNR.
      "WA_ERROR-NAME1 = WA_FD32-NAME1.
      APPEND: WA_ERROR TO IT_ERROR.
      CUN_UT = CUN_UT + 1 .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  ERR_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERR_DISP .
  DATA: MSG_CC TYPE STRING.
  DATA: NUM TYPE STRING.

  NUM = CUN_IT.

  CONCATENATE 'Sucessfully updated:' NUM INTO MSG_CC .

  MESSAGE MSG_CC TYPE 'S' DISPLAY LIKE 'I'.

  PERFORM: ERROR_DISPLAY.
ENDFORM.                    " ERR_DISP
*&---------------------------------------------------------------------*
*&      Form  ERROR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERROR_DISPLAY .

ENDFORM.                    " ERROR_DISPLAY
