*&---------------------------------------------------------------------*
*& Report  ZCRE_CON_DAILY_CUST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCRE_CON_DAILY_CUST.

TABLES: KNKA ,  "#EC CI_USAGE_OK[2227014] " Added by SPLABAP during Code Remediation
  KNKK . "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation

DATA: IT_RETURN	TYPE BAPIRETURN,
      IT_OPEN  TYPE STANDARD TABLE OF BAPI3007_2,
      WA_OPEN  LIKE LINE OF IT_OPEN.

* TYPES : BEGIN OF GS_KNB1,
*          KUNNR TYPE KNB1-KUNNR,
*          BUKRS TYPE KNB1-BUKRS,
*          ERDAT TYPE KNB1-ERDAT,
*          ERNAM TYPE KNB1-ERNAM,
*          SPERR TYPE KNB1-SPERR,
*          LOEVM TYPE KNB1-LOEVM,
*          CREDIT_TYPE TYPE KNB1-CREDIT_TYPE,
*        END OF GS_KNB1.

TYPES : BEGIN OF GS_KNKK,
        KUNNR TYPE KNKK-KUNNR, "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
        KKBER TYPE KNKK-KKBER, "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
        KLIMK TYPE KNKK-KLIMK, "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
      END OF GS_KNKK.

DATA : IT_KNA1 TYPE TABLE OF KNA1,
       WA_KNA1 TYPE KNA1.

DATA : IT_KNB1 TYPE TABLE OF KNB1,
       WA_KNB1 TYPE KNB1.

DATA : IT_CUST TYPE TABLE OF ZCUST_CRE_CON,
       WA_CUST TYPE ZCUST_CRE_CON .

DATA : IT_ACC TYPE TABLE OF ZCUST_ACC_TYPE,
       WA_ACC TYPE ZCUST_ACC_TYPE.

DATA: IT_KNKK TYPE TABLE OF GS_KNKK,
      WA_KNKK TYPE GS_KNKK.

DATA : IT2_FINAL TYPE TABLE OF ZCUST_CRE_CON,
       WA2_FINAL TYPE ZCUST_CRE_CON.

DATA : IT_CONDITION TYPE TABLE OF ZCUST_CONDITION,
       WA_CONDITION TYPE ZCUST_CONDITION.

DATA: OPEN_AMT TYPE NETWR.
DATA: LV_AMT TYPE NETWR.

 DATA : NO_DAYS TYPE RFPOS-VERZN .

 data : wa_days type zcust_condition-TO_DAY.

SELECTION-SCREEN:BEGIN OF BLOCK ABC WITH FRAME TITLE TEXT-001. "CUSTOMER CREDIT CONTROL LIMIT SET
PARAMETERS: P_KKBER TYPE KNKK-KKBER obligatory. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
PARAMETERS: P_KUNNR TYPE KNKK-KUNNR obligatory. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
SELECTION-SCREEN:END OF  BLOCK ABC .

START-OF-SELECTION.
  PERFORM DATA_SELECTION.
  PERFORM DATA_RETRIVAL .
  "PERFORM BUILD_FIELDCATALOG_OVERVIEW.
  "PERFORM BUILD_ALV_OVERVIEW.


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECTION .

  SELECT * FROM KNB1 INTO WA_KNB1 WHERE KUNNR = P_KUNNR AND BUKRS = P_KKBER  .
    ENDSELECT.
    SELECT * FROM KNA1 INTO WA_KNA1 WHERE KUNNR = P_KUNNR.
      ENDSELECT.
"IF WA_KNB1-CREDIT_TYPE EQ 'DEFAULT' .
  SELECT * FROM ZCUST_CRE_CON INTO WA_CUST WHERE KUNNR = P_KUNNR AND BUKRS = P_KKBER  .
ENDSELECT.
"ENDIF.



SELECT TO_DAY UP TO 1 ROWS FROM  ZCUST_CONDITION INTO WA_days WHERE COMPANY_CODE = P_KKBER ORDER BY PRIMARY KEY.
ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
  "ENDSELECT.
  CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704] " Added by SPLABAP during Code Remediation
    EXPORTING
      COMPANYCODE       = P_KKBER
      CUSTOMER          = P_KUNNR
      KEYDATE           = SY-DATUM
*     NOTEDITEMS        = ' '
*     SECINDEX          = ' '
    IMPORTING
     RETURN            = IT_RETURN
    TABLES
      LINEITEMS         = IT_OPEN .

    "LOOP AT IT_KNB1 INTO WA_KNB1.
    "MOVE-CORRESPONDING WA_KNB1 TO WA_FINAL.
    CALL FUNCTION 'CUSTOMER_OLDEST_OPEN_ITEM' "#EC CI_USAGE_OK[2628706] " Added by <IT-CAR Tool> during Code Remediation
      EXPORTING
        I_KKBER            = WA_KNB1-BUKRS
        I_KUNNR            = WA_KNB1-KUNNR
*     I_REGUL            = 'X'
*     I_ERLTA            = '01'
*     I_ERLST            = '00'
*     I_XCRCV            = ' '
     IMPORTING
*     E_BELNR            =
*     E_BUKRS            =
*     E_FAEDT            =
*     E_GJAHR            =
*     E_INFAE            =
*     E_KUNNR            =
        E_VERZN            =  NO_DAYS
*     E_WAERS            =
*     E_WRBTR            =
*     E_XODAT            =
*     E_XNDAT            =
*     E_VERTA            =
*     E_VERST            =
*     E_XDATAAKT         =
*   EXCEPTIONS
*     INVALID_CALL       = 1
*     NO_BUKRS           = 2
*     NO_ITEMS           = 3
*     OTHERS             = 4
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    "WA_FINAL-DAYS = NO_DAYS.
    "APPEND WA_FINAL TO IT_FINAL.
    "CLEAR : WA_FINAL.
    "CLEAR : NO_DAYS.
  "ENDLOOP.


  ENDFORM.                    " DATA_RETRIEVAL

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVAL .

IF WA_KNB1-CREDIT_TYPE EQ 'DEFAULT' .
*     LOOP AT IT_OPEN INTO WA_OPEN.
*      IF WA_OPEN-DB_CR_IND = 'S'.
*        OPEN_AMT = OPEN_AMT + WA_OPEN-LC_AMOUNT .
*      ELSEIF WA_OPEN-DB_CR_IND = 'H'.
*        OPEN_AMT = OPEN_AMT - WA_OPEN-LC_AMOUNT .
*      ENDIF.
*    ENDLOOP.

IF NO_DAYS >  WA_days  .
     MESSAGE: 'Credit limit not updated due to Opening balance' TYPE 'I' DISPLAY LIKE 'E' .
  ELSE.

    SELECT * FROM ZCUST_ACC_TYPE INTO WA_ACC WHERE BUKRS = P_KKBER AND TYP_NAM = 'DEFAULT'  . "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
     ENDSELECT.
IF WA_CUST IS NOT INITIAL.
 UPDATE ZCUST_CRE_CON SET KLIMK_CUR = WA_CUST-KLIMK WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR .
 UPDATE KNKK SET KLIMK = WA_CUST-KLIMK WHERE KKBER = P_KKBER AND KUNNR = P_KUNNR. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
  COMMIT WORK .
 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ELSE.
      WA2_FINAL-BUKRS = WA_KNB1-BUKRS.
      WA2_FINAL-KUNNR = WA_KNB1-KUNNR.
      WA2_FINAL-NAME1 = WA_KNA1-NAME1 .
      WA2_FINAL-KLIMK = ' '.
      WA2_FINAL-KLIMK_CUR = WA_ACC-AMOUNT.
      WA2_FINAL-FBL5N_BAL = ' ' .
      INSERT ZCUST_CRE_CON FROM WA2_FINAL  .
      Data: lv_AMOUNT type KLIMK."Added by SPLABAP during code remediation
      lv_AMOUNT = CONV KLIMK( WA_ACC-AMOUNT )."Added by SPLABAP during code remediation
*      UPDATE KNKK SET KLIMK = WA_ACC-AMOUNT "Commented by SPLABAP during code remediation
      UPDATE KNKK SET KLIMK = lv_AMOUNT "Added by SPLABAP during code remediation
      WHERE KKBER = P_KKBER AND KUNNR = P_KUNNR. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
      COMMIT WORK .
      MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
 ENDIF.
ENDIF.

"ENDIF.

ELSEIF WA_KNB1-CREDIT_TYPE EQ 'DOWN PAYMENT' .
     LOOP AT IT_OPEN INTO WA_OPEN WHERE SP_GL_IND = 'A'.
      IF WA_OPEN-DB_CR_IND = 'S'.
        OPEN_AMT = OPEN_AMT + WA_OPEN-LC_AMOUNT .
      ELSEIF WA_OPEN-DB_CR_IND = 'H'.
        OPEN_AMT = OPEN_AMT - WA_OPEN-LC_AMOUNT .
      ENDIF.
    ENDLOOP.

IF NO_DAYS >  WA_days   .
     MESSAGE: 'Credit limit not updated due to Opening balance' TYPE 'I' DISPLAY LIKE 'E' .
  ELSE.

SELECT * FROM ZCUST_ACC_TYPE INTO WA_ACC WHERE BUKRS = P_KKBER AND ACC_TYPE = 'A' . "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
  ENDSELECT.

  LV_AMT = ( - WA_ACC-NO_OF_TIME ) * (  OPEN_AMT ) .

IF WA_CUST IS NOT INITIAL.
  Data LV_KLIMK1 type KLIMK."Added by SPLABAP during code remediation
  LV_KLIMK1 = CONV KLIMK( LV_AMT )."Added by SPLABAP during code remediation
*UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_AMT WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR . "Commented by SPLABAP during code remediation
UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_KLIMK1 WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR . "Added by SPLABAP during code remediation
 COMMIT WORK .
 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ELSE.

      WA2_FINAL-BUKRS = WA_KNB1-BUKRS.
      WA2_FINAL-KUNNR = WA_KNB1-KUNNR.
      WA2_FINAL-NAME1 = WA_KNA1-NAME1 .
      WA2_FINAL-KLIMK = ' '.
      WA2_FINAL-KLIMK_CUR = LV_AMT.
      WA2_FINAL-FBL5N_BAL = ' ' .
     INSERT ZCUST_CRE_CON FROM WA2_FINAL  .


DATA lv_KLIMK type KLIMK."Added by SPLABAP during code remediation
lv_KLIMK = CONV KLIMK( lv_AMT )."Added by SPLABAP during code remediation
*UPDATE KNKK SET KLIMK = LV_AMT "Commented by SPLABAP during code remediation
UPDATE KNKK SET KLIMK = LV_KLIMK "Added by SPLABAP during code remediation
WHERE KKBER = P_KKBER AND KUNNR = P_KUNNR. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
 COMMIT WORK .
 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ENDIF.
ENDIF.

ELSEIF WA_KNB1-CREDIT_TYPE EQ 'GUARANTEES GIVEN' .
     LOOP AT IT_OPEN INTO WA_OPEN WHERE SP_GL_IND = 'G'.
      IF WA_OPEN-DB_CR_IND = 'S'.
        OPEN_AMT = OPEN_AMT + WA_OPEN-LC_AMOUNT .
      ELSEIF WA_OPEN-DB_CR_IND = 'H'.
        OPEN_AMT = OPEN_AMT - WA_OPEN-LC_AMOUNT .
      ENDIF.
    ENDLOOP.

IF NO_DAYS >  WA_days  .
     MESSAGE: 'Credit limit not updated due to Opening balance' TYPE 'I' DISPLAY LIKE 'E' .
  ELSE.

SELECT * FROM ZCUST_ACC_TYPE INTO WA_ACC WHERE BUKRS = P_KKBER AND ACC_TYPE = 'A' . "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
  ENDSELECT.

  LV_AMT = ( - WA_ACC-NO_OF_TIME ) * (  OPEN_AMT ) .

IF WA_CUST IS NOT INITIAL.
LV_KLIMK1 = CONV KLIMK( LV_AMT )."Added by SPLABAP during code remediation
*UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_AMT WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR . "Commented by SPLABAP during code remediation
UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_KLIMK1 WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR .  "Added by SPLABAP during code remediation
 COMMIT WORK .

 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ELSE.

      WA2_FINAL-BUKRS = WA_KNB1-BUKRS.
      WA2_FINAL-KUNNR = WA_KNB1-KUNNR.
      WA2_FINAL-NAME1 = WA_KNA1-NAME1 .
      WA2_FINAL-KLIMK = ' '.
      WA2_FINAL-KLIMK_CUR = LV_AMT.
      WA2_FINAL-FBL5N_BAL = ' ' .
     INSERT ZCUST_CRE_CON FROM WA2_FINAL  .


lv_KLIMK = CONV KLIMK( lv_AMT )."Added by SPLABAP during code remediation
*UPDATE KNKK SET KLIMK = LV_AMT  "Commented by SPLABAP during code remediation
UPDATE KNKK SET KLIMK = LV_KLIMK "Added by SPLABAP during code remediation
WHERE KKBER = P_KKBER AND KUNNR = P_KUNNR. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation

 COMMIT WORK .

 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ENDIF.
ENDIF.

ELSEIF WA_KNB1-CREDIT_TYPE EQ 'SECURITY DEPOSIT' .
     LOOP AT IT_OPEN INTO WA_OPEN WHERE SP_GL_IND = 'H'.
      IF WA_OPEN-DB_CR_IND = 'S'.
        OPEN_AMT = OPEN_AMT + WA_OPEN-LC_AMOUNT .
      ELSEIF WA_OPEN-DB_CR_IND = 'H'.
        OPEN_AMT = OPEN_AMT - WA_OPEN-LC_AMOUNT .
      ENDIF.
    ENDLOOP.

IF NO_DAYS >  WA_days  .
     MESSAGE: 'Credit limit not updated due to Opening balance' TYPE 'I' DISPLAY LIKE 'E' .
  ELSE.

SELECT * FROM ZCUST_ACC_TYPE INTO WA_ACC WHERE BUKRS = P_KKBER AND ACC_TYPE = 'H' . "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
  ENDSELECT.

  LV_AMT = ( - WA_ACC-NO_OF_TIME ) * (  OPEN_AMT ) .

IF WA_CUST IS NOT INITIAL.
  LV_KLIMK1 = CONV KLIMK( LV_AMT )."Added by SPLABAP during code remediation
*UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_AMT WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR .  "Commented by SPLABAP during code remediation
UPDATE ZCUST_CRE_CON SET KLIMK_CUR = LV_KLIMK1 WHERE BUKRS = P_KKBER AND KUNNR = P_KUNNR . "Added by SPLABAP during code remediation
 COMMIT WORK .

 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.
ELSE.

      WA2_FINAL-BUKRS = WA_KNB1-BUKRS.
      WA2_FINAL-KUNNR = WA_KNB1-KUNNR.
      WA2_FINAL-NAME1 = WA_KNA1-NAME1 .
      WA2_FINAL-KLIMK = ' '.
      WA2_FINAL-KLIMK_CUR = LV_AMT.
      WA2_FINAL-FBL5N_BAL = ' ' .
     INSERT ZCUST_CRE_CON FROM WA2_FINAL  .


lv_KLIMK = CONV KLIMK( lv_AMT )."Added by SPLABAP during code remediation
*UPDATE KNKK SET KLIMK = LV_AMT "Commented by SPLABAP during code remediation
UPDATE KNKK SET KLIMK = lv_KLIMK "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
WHERE KKBER = P_KKBER AND KUNNR = P_KUNNR. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation

 COMMIT WORK .

 MESSAGE : 'Credit limit has been updated' TYPE 'I' DISPLAY LIKE 'I'.

ENDIF.
ENDIF.
ENDIF.

clear : WA_CONDITION.
clear : NO_DAYS.


ENDFORM.                    " DATA_RETRIVAL
