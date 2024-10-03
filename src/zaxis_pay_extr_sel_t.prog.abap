*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTR_SEL
*&---------------------------------------------------------------------*



*BLOCK - 1 (AUTO PAYMENT & MANUAL PAYMENT OPTIONS)

SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE  TEXT-003 .
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(23) TEXT-001 MODIF ID REZ FOR FIELD P_AUTO.
PARAMETERS: P_AUTO RADIOBUTTON GROUP R1 USER-COMMAND FLG  MODIF ID REZ.  "Auto Payment

SELECTION-SCREEN COMMENT 40(17) TEXT-002 MODIF ID REZ FOR FIELD P_MANU.
PARAMETERS: P_MANU RADIOBUTTON GROUP R1 DEFAULT 'X' MODIF ID REZ .  "Manual Payment

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B0.

*BLOCK - 2 (INPUTS FOR COMPANY CODE)

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-024.

PARAMETERS      : P_BUKRS  LIKE T001-BUKRS .
SELECT-OPTIONS  : S_GSBER  FOR  TGSB-GSBER NO-DISPLAY,
                  S_PRCTR  FOR  CEPC-PRCTR NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK B1.


* BLOCK - 3 (HOUSE BANK ID & ACCOUNT ID)


SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-011.

SELECT-OPTIONS  : S_HBKID  FOR  T012K-HBKID MODIF ID REY DEFAULT '7290' NO INTERVALS NO-EXTENSION ,
                  S_HKTID  FOR  T012K-HKTID MODIF ID REY DEFAULT '7290' NO INTERVALS NO-EXTENSION .

SELECTION-SCREEN END OF BLOCK B4.

* BLOCK - 4 (PAYMENT RUN ID & DATE) APPLICABLE FOR AUTO PAYMENT

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-014.

SELECT-OPTIONS  : S_LAUFI  FOR REGUH-LAUFI MODIF ID AUT NO INTERVALS NO-EXTENSION,
                  S_LAUFD  FOR REGUH-LAUFD MODIF ID AUT NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK B2 .

* BLOCK - 5 (FISCAL YEAR, PAYMENT DOCUMENT NUMBER, POSTING DATE)

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-012.

PARAMETERS      : P_GJAHR  LIKE BSAK-GJAHR  MEMORY ID GJA MODIF ID MAN.
SELECT-OPTIONS  : S_VBLNR  FOR  BSAK-BELNR  MODIF ID MAN,
                  S_BUDAT  FOR  BSAK-BUDAT  MODIF ID MAN.

SELECTION-SCREEN END OF BLOCK B3.

* BLOCK - 5 (Select Payment Method,Then all the payments will go with that payment method)

SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-123.

PARAMETERS : P_PAY(3) AS LISTBOX VISIBLE LENGTH 15 MODIF ID PAY.

SELECTION-SCREEN END OF BLOCK B6.


* BLOCK - 7 (Drop down for Print location for CC and DD payment method)

SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-150.

PARAMETERS : P_PRINT TYPE ORT01_GP AS LISTBOX VISIBLE LENGTH 20 MODIF ID PRT.

SELECTION-SCREEN END OF BLOCK B7.


SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-013.

PARAMETERS : P_TEST AS CHECKBOX DEFAULT 'X' MODIF ID  REW  .

SELECTION-SCREEN END OF BLOCK B5.



AT SELECTION-SCREEN OUTPUT.
  PERFORM FIELDS_FOR_AUTO_MANU.


  REFRESH :  LIST.

  NAME = 'P_PAY'. " Name should be in UPPER CASE

  VALUE-KEY = C_NEFT.
  VALUE-TEXT = C_NEFT_PRD_CODE.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_RTGS.
  VALUE-TEXT = C_RTGS_PRD_CODE.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_FT.
  VALUE-TEXT = C_BANK_TRF.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_CC.
  VALUE-TEXT = C_CHEQUE.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_DD.
  VALUE-TEXT = C_DD.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_IMPS.
  VALUE-TEXT = C_IMPS_PRD_CODE.
  APPEND VALUE TO LIST.

  VALUE-KEY = C_TRF.
  VALUE-TEXT = C_TRANSFER.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = NAME
      VALUES          = LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 0
      OTHERS          = 0.

***-- Drop Down for Print Location

  CLEAR VALUE.
  REFRESH :  LIST.
  NAME = 'P_PRINT'.

***--fetching print location data

  PERFORM READ_PRINT_LOC.

  IF  G_TAB_ZAXIS_TAB_PAYLOC[] IS NOT INITIAL.

    LOOP AT  G_TAB_ZAXIS_TAB_PAYLOC INTO WA_PRINT_LOC.
      VALUE-KEY  = WA_PRINT_LOC-SAP_CITY.
      VALUE-TEXT = WA_PRINT_LOC-SAP_CITY.
      APPEND VALUE TO LIST.
      CLEAR: VALUE,WA_PRINT_LOC.
    ENDLOOP.

    SORT LIST BY KEY.
    DELETE ADJACENT DUPLICATES FROM LIST COMPARING KEY.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = NAME
      VALUES          = LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 0
      OTHERS          = 0.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFI-LOW.

  PERFORM GET_LAUFI_LAUFD_HELP USING 'S_LAUFI-LOW'    "* Help For Payment Run ID (LOW)
                                      'I'
                                      'S_LAUFD-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFI-HIGH.

  PERFORM GET_LAUFI_LAUFD_HELP USING 'S_LAUFI-HIGH'   "* Help For Payment Run ID (HIGH)
                                      'I'
                                      'S_LAUFD-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFD-LOW.

  PERFORM GET_LAUFI_LAUFD_HELP USING 'S_LAUFD-LOW'    "* Help For Payment Run Date (LOW)
                                      'D'
                                      'S_LAUFI-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFD-HIGH.

  PERFORM GET_LAUFI_LAUFD_HELP USING 'S_LAUFD-HIGH'   "* Help For Payment Run Date (High)
                                      'D'
                                      'S_LAUFI-HIGH'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HBKID-LOW.
  PERFORM GET_HBKID_HELP USING 'S_HBKID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HKTID-LOW.
  PERFORM GET_HKTID_HELP USING 'S_HKTID-LOW'.



*&---------------------------------------------------------------------*
*&      Form  fields_for_auto_manu
*&---------------------------------------------------------------------*
** SCREEN ELEMENTS: BASED ON AUTOMATIC / MANUAL PAYMENTS.
*----------------------------------------------------------------------*

FORM FIELDS_FOR_AUTO_MANU .

  LOOP AT SCREEN.
    IF P_MANU = 'X'.

      IF SCREEN-GROUP1 = 'AUT'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'MAN'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.


    IF SCREEN-GROUP1 = 'PAY'.
      SCREEN-ACTIVE = 0.
    ENDIF.

    IF SCREEN-GROUP1 = 'PRT'.
      SCREEN-ACTIVE = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.                    " fields_for_auto_manu



*&---------------------------------------------------------------------*
*&      Form  GET_HBKID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_HBKID_HELP USING P_S_HBKID_LOW.
  TYPES : BEGIN OF TY_HBKID,
          BUKRS TYPE BUKRS,
          HBKID TYPE HBKID,
          END OF TY_HBKID.
  DATA : IT_HBKID  TYPE TABLE OF TY_HBKID.

  CLEAR : IT_DYNPREAD[],IT_RETURN[],IT_HBKID[].


  PERFORM DYNP_VALUES_READ USING 'P_BUKRS'.
  SELECT BUKRS HBKID FROM ZAXIS_TAB_HB INTO TABLE IT_HBKID WHERE BUKRS EQ P_BUKRS.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HBKID'
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = P_S_HBKID_LOW
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_HBKID
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_RETURN INTO WA_RETURN INDEX 1.
    IF SY-SUBRC EQ 0.
      S_HBKID-LOW = WA_RETURN-FIELDVAL.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_HBKID_HELP

*&---------------------------------------------------------------------*
*&      Form  GET_HKTID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0341   text
*----------------------------------------------------------------------*
FORM GET_HKTID_HELP  USING    P_S_HKTID_LOW.
  TYPES : BEGIN OF TY_HKTID,
          BUKRS TYPE BUKRS,
          HBKID TYPE HBKID,
          HKTID TYPE HKTID,
          END OF TY_HKTID.
  DATA : IT_HKTID TYPE TABLE OF TY_HKTID,
         WA_HKTID TYPE TY_HKTID.
  CLEAR : IT_HKTID[],IT_RETURN[],WA_RETURN.

  PERFORM DYNP_VALUES_READ USING 'P_BUKRS'.


  IF S_HBKID-LOW IS INITIAL.
    MOVE 'S_HBKID-LOW' TO WA_DYNPREAD-FIELDNAME.
    APPEND WA_DYNPREAD TO IT_DYNPREAD.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = SY-REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = IT_DYNPREAD
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        INVALID_PARAMETER    = 7
        UNDEFIND_ERROR       = 8
        DOUBLE_CONVERSION    = 9
        STEPL_NOT_FOUND      = 10
        OTHERS               = 11.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      READ TABLE IT_DYNPREAD INTO WA_DYNPREAD WITH KEY FIELDNAME = 'S_HBKID-LOW'.
      IF SY-SUBRC EQ 0.
        S_HBKID-LOW = WA_DYNPREAD-FIELDVALUE.
      ENDIF.
    ENDIF.
  ENDIF.
*  IF sy-tcode EQ c_tcode_extr.
  SELECT BUKRS
         HBKID
         HKTID
         FROM T012K INTO TABLE IT_HKTID
         WHERE BUKRS EQ P_BUKRS
         AND   HBKID EQ S_HBKID-LOW.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HKTID'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = P_S_HKTID_LOW
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_HKTID
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE  IT_RETURN INTO WA_RETURN INDEX 1.
    IF SY-SUBRC EQ 0.
      S_HKTID-LOW = WA_RETURN-FIELDVAL.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_HKTID_HELP
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1774   text
*----------------------------------------------------------------------*
FORM DYNP_VALUES_READ  USING    P_P_BUKRS.


  MOVE P_P_BUKRS TO WA_DYNPREAD-FIELDNAME.
  APPEND WA_DYNPREAD TO IT_DYNPREAD.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = IT_DYNPREAD
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_DYNPREAD INTO WA_DYNPREAD WITH KEY FIELDNAME = P_P_BUKRS.
    IF SY-SUBRC EQ 0.
      P_BUKRS = WA_DYNPREAD-FIELDVALUE.
    ENDIF.
  ENDIF.


  SELECT * FROM T001 INTO TABLE G_TAB_T001 WHERE BUKRS = P_BUKRS.

  IF G_TAB_T001[] IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-107.
  ENDIF.
ENDFORM.                    " DYNP_VALUES_READ




*&---------------------------------------------------------------------*
*&      Form  get_laufi_laufd_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_FNAME   text
*      -->LV_F1TYP   text
*      -->LV_F2NME   text
*----------------------------------------------------------------------*

FORM GET_LAUFI_LAUFD_HELP USING  LV_FNAME TYPE FIELDNAME
                            LV_F1TYP
                            LV_F2NME.

  DATA L_WA_TLAUFK TYPE ILAUFK.
  DATA L_TAB_TLAUFK TYPE TABLE OF ILAUFK INITIAL SIZE 0.

  DATA : L_LAUFI LIKE REGUH-LAUFI,
         L_LAUFD LIKE REGUH-LAUFD.

  DATA L_WA_DYNPREAD TYPE DYNPREAD.
  DATA L_TAB_DYNPREAD TYPE TABLE OF DYNPREAD INITIAL SIZE 0.

  L_WA_TLAUFK-LAUFK = SPACE.
  L_WA_TLAUFK-SIGN  = 'I'.

  APPEND L_WA_TLAUFK TO L_TAB_TLAUFK.

*FM: FOR PAYMENT PROGRAM

  CALL FUNCTION 'F4_ZAHLLAUF'
    EXPORTING
      F1TYP = LV_F1TYP
      F2NME = LV_F2NME
    IMPORTING
      LAUFD = L_LAUFD
      LAUFI = L_LAUFI
    TABLES
      LAUFK = L_TAB_TLAUFK.

  L_WA_DYNPREAD-FIELDNAME  = LV_FNAME.

  IF LV_FNAME CS 'LAUFI'.
    WRITE L_LAUFI TO L_WA_DYNPREAD-FIELDVALUE.
  ELSE.
    WRITE L_LAUFD TO L_WA_DYNPREAD-FIELDVALUE.
  ENDIF.

  APPEND L_WA_DYNPREAD TO L_TAB_DYNPREAD.
  L_WA_DYNPREAD-FIELDNAME  = LV_F2NME.

  IF LV_F2NME CS 'LAUFI'.
    WRITE L_LAUFI TO L_WA_DYNPREAD-FIELDVALUE.
  ELSE.
    WRITE L_LAUFD TO L_WA_DYNPREAD-FIELDVALUE.
  ENDIF.

  APPEND L_WA_DYNPREAD TO L_TAB_DYNPREAD.

*FM: TO CHANGE THE SCREEN FIELDS

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME     = SY-CPROG
      DYNUMB     = '1000'
    TABLES
      DYNPFIELDS = L_TAB_DYNPREAD.

ENDFORM.                    " GET_laufi_laufd_help
*&---------------------------------------------------------------------*
*&      Form  POP_UP_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_107  text
*----------------------------------------------------------------------*
FORM POP_UP_ERROR  USING    P_TEXT_107.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = TEXT-017  "'Error Message'
      TEXT_QUESTION         = P_TEXT_107
      TEXT_BUTTON_1         = 'Close'(015)
      ICON_BUTTON_1         = 'ICON_DUMMY'
      TEXT_BUTTON_2         = 'Exit'(016)
      ICON_BUTTON_2         = 'ICON_INCOMPLETE'
      DISPLAY_CANCEL_BUTTON = ' '
      IV_QUICKINFO_BUTTON_1 = TEXT-018 "'Close this Window'
      IV_QUICKINFO_BUTTON_2 = TEXT-019 "'Exit the Entire Transaction'
    IMPORTING
      ANSWER                = G_POPUP_ANSWER.


  IF G_POPUP_ANSWER = C_CLOSE.
    LEAVE LIST-PROCESSING.
  ELSE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " POP_UP_ERROR
