*&---------------------------------------------------------------------*
*& Report  ZHR_PAYSLIP_NEW3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHR_PAYSLIP_NEW3.
*&---------------------------------------------------------------------*
*& Report  ZHR_PAYSLIP_NEW1.
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*



*_Tables Declaration
TABLES: T512T,
        PA0008,
        PA0001,
        T569V,
        T247,
        PA0000.                   "Wage type texts

*_ Data Declaration

TYPES : BEGIN OF TY_FINAL,
        STATUS(10) TYPE C,
        MESSAGE(50) TYPE C,
        FIELD(132)  TYPE C,
        END OF TY_FINAL.

*_Table data containing directory to PCL2 payroll results file
DATA: BEGIN OF RGDIR OCCURS 100.
        INCLUDE STRUCTURE PC261.
DATA: END OF RGDIR.

DATA: COUNTRY LIKE T001P-MOLGA,
      NUMBER  LIKE PC261-SEQNR. "Number of last payroll result

DATA : RESULT TYPE PAYIN_RESULT OCCURS 0 WITH HEADER LINE,
       WA_RESULT LIKE LINE OF RESULT.

DATA:W_FMNAME TYPE RS38L_FNAM,
     W_FORMNAME TYPE TDSFNAME.

DATA : IT_P0001 TYPE P0001 OCCURS 0 WITH HEADER LINE,
       WA_P0001 LIKE LINE OF IT_P0001.

DATA : IT_P0001_1 TYPE P0001 OCCURS 0 WITH HEADER LINE,
       WA_P0001_1 LIKE LINE OF IT_P0001_1.

DATA : IT_MAIL_001 TYPE P0001 OCCURS 0 WITH HEADER LINE,
       WA_MAIL_001 TYPE P0001.

DATA : IT_P0002 TYPE P0001 OCCURS 0 WITH HEADER LINE,
       WA_P0002 LIKE LINE OF IT_P0002.


DATA : IT_ACTUAL TYPE P0008 OCCURS 0 ,
       WA_ACTUAL LIKE LINE OF IT_ACTUAL,
       IT_ACTUAL1 TYPE ZHRBP_L OCCURS 0,
       WA_ACTUAL1 LIKE LINE OF IT_ACTUAL1,
       IT_DEDUC TYPE PC207 OCCURS 0 ,
       WA_DEDUC LIKE LINE OF IT_DEDUC,
       IT_PERK TYPE PC207 OCCURS 0 ,
       WA_PERK LIKE LINE OF IT_PERK.


DATA : IT_RT TYPE PC207 OCCURS 0,
       WA_RT LIKE LINE OF IT_RT,
       WA_BT TYPE PC209,
       IT_ESI TYPE PC2_IN08 OCCURS 0,
       WA_ESI LIKE LINE OF IT_ESI.
*       IT_EPF TYPE PC2_IN07 OCCURS 0,
*       WA_EPF LIKE LINE OF IT_EPF.

*!Begin of Changes for trainees Payslip
DATA : IT_OPAT LIKE ZHRTTTR_S OCCURS 0 WITH HEADER LINE.

*DATA : IT_OPAT TYPE TY_OPAT OCCURS 0 WITH HEADER LINE,
*       WA_OPAT LIKE LINE OF IT_OPAT.

DATA : WA_OPAT LIKE LINE OF IT_OPAT.
*! End of Changes for trainees paylslip.

*!Internal table that contains the Earnings Wage-types.

DATA: BEGIN OF IT_WTYPE OCCURS 0,
      LGART LIKE P0008-LGA01,
      END OF IT_WTYPE.
*!Internal table that contains the Deductions Wage-types.

DATA: IT_WTYPD LIKE IT_WTYPE OCCURS 0 WITH HEADER LINE.

DATA : LGART TYPE LGART,BETRG LIKE PA0008-BET01,W_PTEXT TYPE STRING.

DATA : BEGIN OF IT_PERNR OCCURS 0.
        INCLUDE STRUCTURE PA0001.
DATA   END OF IT_PERNR.

DATA : WA_PERNR LIKE LINE OF IT_PERNR.

*DATA : W_BEGDA TYPE SY-DATUM,W_ENDDA TYPE SY-DATUM, W_PLANT TYPE BTRTL.
DATA : W_PLANT TYPE BTRTL.
DATA : IT_OTF TYPE SSFCRESCL OCCURS 0 WITH HEADER LINE,
       IT_OUT TYPE ITCOO OCCURS 0.
DATA : WA_OTF LIKE LINE OF IT_OTF.

DATA : IT_OTF1 TYPE SSFCRESCL OCCURS 0 WITH HEADER LINE,
       IT_OUT1 TYPE ITCOO OCCURS 0 WITH HEADER LINE.
DATA : WA_OTF1 LIKE LINE OF IT_OTF.

DATA : W_SSFCOMPOP TYPE SSFCOMPOP,
       W_SSFCTRLOP TYPE SSFCTRLOP.

DATA : FILE_TABLE TYPE FILE_TABLE OCCURS 0,
       WA_FILE_TABLE LIKE LINE OF FILE_TABLE.

DATA : BEGIN OF IT_BTRTL OCCURS 0,
       WERKS LIKE PA0001-WERKS,
       BTRTL LIKE PA0001-BTRTL,
       BTEXT LIKE T001P-BTEXT,
       END OF IT_BTRTL.

DATA : IT_RETURN TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

DATA : W_REPID TYPE SY-REPID,
       W_DYNNR TYPE SY-DYNNR,
       RC TYPE I.

RANGES : R_PERNR FOR PERNR-PERNR.


* begin of data declarations by  to obtain Trainee days payable ***
DATA: IT_WPBP TYPE HRPAY99_WPBP.
DATA: WA_WPBP LIKE LINE OF IT_WPBP.

DATA: IT_AB TYPE HRPAY99_AB.
DATA: WA_AB LIKE LINE OF IT_AB.

DATA: IT_EPF TYPE HRPAYIN_EPF.
DATA: WA_EPF LIKE LINE OF IT_EPF.

CONSTANTS: C_AWART TYPE AWART VALUE 'LOP'.


DATA: BEGDA1 TYPE BEGDA,
      ENDDA1 TYPE ENDDA,BEGDA TYPE BEGDA.

***DATA : CCODE LIKE P0001-BUKRS. " Commented by * on 10.10.2012
DATA : NO(2) TYPE N.
DATA : C_ADD(80) TYPE C.


DATA : IT_OUT_RGDIR TYPE STANDARD TABLE OF PC261 ,
       WA_OUT_RGDIR TYPE PC261.

*_ Anil
DATA:SSFCTRLOP    TYPE SSFCTRLOP.
DATA:SSFCOMPOP    TYPE SSFCOMPOP.
DATA:IT_OTF_DATA  TYPE SSFCRESCL.
DATA:IT_OTF_FINAL TYPE ITCOO OCCURS 0 WITH HEADER LINE.
DATA: WA_OTF_FINAL LIKE LINE OF IT_OTF_FINAL.

DATA:BIN_FILESIZE TYPE I.
DATA:IT_PDFDATA   TYPE TABLE OF TLINE.
DATA:IT_PDF       TYPE TABLE OF SOLISTI1.

DATA :
G_SENT_TO_ALL   TYPE SONV-FLAG,
G_TAB_LINES     TYPE I. "Types
TYPES: T_DOCUMENT_DATA  TYPE  SODOCCHGI1,
       T_PACKING_LIST   TYPE  SOPCKLSTI1,
       T_ATTACHMENT     TYPE  SOLISTI1,
       T_BODY_MSG       TYPE  SOLISTI1,
       T_RECEIVERS      TYPE  SOMLRECI1,
       T_PDF            TYPE  TLINE.

"Workareas
DATA : W_DOCUMENT_DATA  TYPE  T_DOCUMENT_DATA,
       W_PACKING_LIST   TYPE  T_PACKING_LIST,
       W_ATTACHMENT     TYPE  T_ATTACHMENT,
       W_BODY_MSG       TYPE  T_BODY_MSG,
       W_RECEIVERS      TYPE  T_RECEIVERS,
       W_PDF            TYPE  T_PDF.

"Internal Tables
DATA : I_DOCUMENT_DATA  TYPE STANDARD TABLE OF T_DOCUMENT_DATA,
       I_PACKING_LIST   TYPE STANDARD TABLE OF T_PACKING_LIST,
       I_ATTACHMENT     TYPE STANDARD TABLE OF T_ATTACHMENT,
       I_BODY_MSG       TYPE STANDARD TABLE OF T_BODY_MSG,
       I_RECEIVERS      TYPE STANDARD TABLE OF T_RECEIVERS,
       I_PDF            TYPE STANDARD TABLE OF T_PDF.

*_ Internal Table for Email ID's.
DATA : BEGIN OF IT_USRID OCCURS 0,
       PERNR LIKE PA0105-PERNR,
       USRID_LONG LIKE PA0105-USRID_LONG,
       END OF IT_USRID.
DATA : USRID_LONG LIKE PA0105-USRID_LONG.

DATA : V_MON(10) TYPE C,
       GV_MON(10) TYPE C,
       GV_MONTH(10) TYPE C,
       GV_MONTH1 TYPE PABRP.
DATA : V_MM TYPE  PERMO,
       V_DAT(02)  TYPE C.
DATA : TEXT(500), TEXT1(60), TEXT2(60).
***DATA : count TYPE NUM,
***       ok_code type sy-ucomm.
*_ Anil


"Added by Surendran CLSS on 03.11.2014

DATA : V_LEN_IN LIKE SOOD-OBJLEN,
      V_LEN_OUT LIKE SOOD-OBJLEN,
      V_LEN_OUTN TYPE I,
      V_LINES_TXT TYPE I,
      V_LINES_BIN TYPE I.

DATA : WA_BUFFER TYPE STRING. "To convert from 132 to 255

*DATA: i_otf TYPE itcoo OCCURS 0 WITH HEADER LINE,
*      i_tline TYPE TABLE OF tline WITH HEADER LINE,
*      i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE.

DATA : I_RECORD LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE.
DATA : I_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
"""""""Work Area declarations

DATA :      WA_OBJHEAD TYPE SOLI_TAB,
            W_CTRLOP TYPE SSFCTRLOP,
            W_COMPOP TYPE SSFCOMPOP,
            W_RETURN TYPE SSFCRESCL,
            WA_DOC_CHNG TYPE SODOCCHGI1,
            W_DATA TYPE SODOCCHGI1.
*            wa_buffer TYPE string. "To convert from 132 to 255

* Objects to send mail.
DATA :  I_OBJPACK LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
*        i_objtxt LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        I_OBJTXT  LIKE SOLISTI1 OCCURS 10 WITH HEADER LINE,
        WA_OBJTXT TYPE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJBIN LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_RECLIST LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE.

DATA : LV_MAIL TYPE PA0105-USRID_LONG.
DATA : LV_NAME TYPE PA0002-VORNA.


DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.
*DATA : IT_MONTH

DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA OTF_DATA TYPE TSFOTF.
*DATA : LV_LINE1 TYPE

DATA : IT_MONTH TYPE TABLE OF T247,
       WA_MONTH TYPE T247.

DATA : W_MNAME TYPE T247-LTX,
       W_YNAME  TYPE PA0000-BEGDA.

DATA : IT_P0000 TYPE TABLE OF P0000.

DATA : IT_PA0000 TYPE TABLE OF PA0000,
       WA_PA0000 TYPE PA0000.

DATA : IT_PA2006 TYPE TABLE OF PA2006,
       WA_PA2006 TYPE PA2006.

DATA : LV_TITLE   TYPE PA0002-ANRED,
       LV_TITNAME TYPE T522T-ATEXT.

*break dev01.
***------------------------------------------------------------------***
*** Selection Screen Declaration                                     ***
***------------------------------------------------------------------***
DATA : P_BEGDA TYPE PNPBEGDA,
       P_ENDDA TYPE PNPENDDA.

DATA : V_FT_DAY TYPE P0000-BEGDA,
       V_LT_DAY TYPE P0000-ENDDA.

DATA: STAT2  LIKE PA0000-STAT2,
      STATUS LIKE PA0000-STAT2,
      GV_ABKRS  TYPE ABKRS,             " Payroll Area
      GV_PERMO TYPE PERMO.             " Period  Parameter

DATA : WA_T569V TYPE T569V.


TYPES: BEGIN OF TY_T549Q,
       PERMO TYPE PERMO,             " Period Parameter
       BEGDA TYPE BEGDA,             " Start Date
       ENDDA TYPE ENDDA,             " End Date
       BDATE TYPE D,
       EDATE TYPE D,
       END OF TY_T549Q.

*__Workarea & Internal table declarations for Payroll Periods .
DATA : GW_T549Q     TYPE TY_T549Q,
       GT_T549Q     TYPE STANDARD TABLE OF TY_T549Q INITIAL SIZE 0.

DATA : V_BEGDA TYPE D,
       V_ENDDA TYPE D.

DATA : V_ERROR TYPE C.
DATA : W_PLBL TYPE PA2006-ANZHL,
       W_SLBL TYPE PA2006-ANZHL,
       W_CLBL TYPE PA2006-ANZHL,
       W_PL1 TYPE PA2006-ANZHL,
       W_PL2 TYPE PA2006-ANZHL,
       W_SL1 TYPE PA2006-ANZHL,
       W_SL2 TYPE PA2006-ANZHL,
       W_CL1 TYPE PA2006-ANZHL,
       W_CL2 TYPE PA2006-ANZHL.

****SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
****SELECTION-SCREEN BEGIN OF LINE.
*****SELECTION-SCREEN COMMENT 1(10) TEXT-010 FOR FIELD P_MONTH.
*****PARAMETERS : P_MONTH TYPE PNPPABRP OBLIGATORY, P_YEAR TYPE PNPPABRJ OBLIGATORY.
**********************SELECTION-SCREEN COMMENT 1(27) TEXT-010 FOR FIELD P_BEGDA.
**********************PARAMETERS : P_BEGDA TYPE PNPBEGDA OBLIGATORY, P_ENDDA TYPE PNPENDDA OBLIGATORY.
****SELECTION-SCREEN END OF LINE.
****
****SELECTION-SCREEN COMMENT 1(10) TEXT-010 FOR FIELD P_MONTH1.
****PARAMETERS : P_MONTH1 TYPE PNPPABRP OBLIGATORY, P_YEAR TYPE PNPPABRJ OBLIGATORY.
****
****SELECT-OPTIONS : SO_PERNR FOR PA0001-PERNR NO INTERVALS .
*****--Pers.Subarea -
****SELECT-OPTIONS : SO_ABKRS FOR PA0001-ABKRS NO INTERVALS OBLIGATORY.
****SELECT-OPTIONS : SO_PERSA FOR PA0001-WERKS NO INTERVALS OBLIGATORY.
****SELECT-OPTIONS : SO_BTRTL FOR PA0001-BTRTL NO INTERVALS.
****SELECT-OPTIONS : SO_PERSG FOR PA0001-PERSG NO INTERVALS.
****SELECT-OPTIONS : SO_PERSK FOR PA0001-PERSK NO INTERVALS.
****SELECT-OPTIONS : SO_BUKRS FOR PA0001-BUKRS NO INTERVALS.
****
****SELECTION-SCREEN END OF BLOCK B1.

**************************************************************************
*_Begin of Screen Selection B2
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS : S_ABKRS FOR PA0001-ABKRS NO INTERVALS NO-EXTENSION OBLIGATORY.

***SELECTION-SCREEN BEGIN OF LINE.
***SELECTION-SCREEN COMMENT 1(29) TEXT-012 FOR FIELD RB_CP.
***PARAMETERS : RB_CP RADIOBUTTON GROUP RG1.
***SELECTION-SCREEN END OF LINE.
*_End of Screen Selection B2
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) TEXT-013 FOR FIELD RB_OP.
PARAMETERS : RB_OP TYPE CHECK DEFAULT 'X' NO-DISPLAY .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) TEXT-015.
PARAMETERS : P1_MONTH TYPE  PNPPABRP,
             P1_YEAR  TYPE  PNPPABRJ.
SELECT-OPTIONS : S_DATES FOR PA0001-BEGDA NO-EXTENSION .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B4.

*_Begin of Screen Selection B3
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_PERNR FOR PA0001-PERNR NO INTERVALS OBLIGATORY DEFAULT '700000'.
SELECT-OPTIONS : S_STAT2 FOR PA0000-STAT2 NO INTERVALS.
SELECT-OPTIONS : S_WERKS FOR PA0001-WERKS NO INTERVALS .
SELECT-OPTIONS : S_BTRTL FOR PA0001-BTRTL NO INTERVALS .
SELECT-OPTIONS : S_PERSG FOR PA0001-PERSG NO INTERVALS .
SELECT-OPTIONS : S_BUKRS FOR PA0001-BUKRS NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-006.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) TEXT-016 FOR FIELD P_MAIL.
PARAMETERS : P_MAIL AS CHECKBOX .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B5.
***************************************************************************
INITIALIZATION.

  LOOP AT SCREEN.

    IF SCREEN-NAME EQ 'S_DATES-LOW'.

      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.

    IF SCREEN-NAME EQ 'S_DATES-HIGH'.

      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.


AT SELECTION-SCREEN OUTPUT.

  SELECT SINGLE
       PERMO
       FROM T549A                  " Table for Payroll Areas
       INTO GV_PERMO
       WHERE ABKRS EQ S_ABKRS-LOW.

  IF P1_MONTH IS  INITIAL.

    SELECT SINGLE * FROM T569V
      INTO WA_T569V
      WHERE ABKRS = S_ABKRS-LOW
      AND   VWSAZ ='1'.

    P1_MONTH = WA_T569V-PABRP.
    P1_YEAR  = WA_T569V-PABRJ.

  ENDIF.


  LOOP AT SCREEN.
    REFRESH : S_DATES.
    IF RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.


      MODIFY SCREEN.

      REFRESH : S_DATES.

      SELECT SINGLE BEGDA
                    ENDDA
                    FROM T549Q                 " Table for Payroll Period
                    INTO (S_DATES-LOW , S_DATES-HIGH)
                    WHERE PERMO EQ GV_PERMO
                    AND   PABRP EQ P1_MONTH
                    AND   PABRJ EQ P1_YEAR.
      S_DATES-SIGN = 'I'.
      S_DATES-OPTION = 'EQ'.
      APPEND S_DATES.

      IF SCREEN-NAME EQ 'S_DATES-LOW'.

        IF  RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.

          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDIF.

      IF SCREEN-NAME EQ 'S_DATES-HIGH'.

        IF  RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.

          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDLOOP.

*if S_PERNR is NOT INITIAL.
*
*SELECT SINGLE
*       STAT2 FROM PA0000
*       INTO STAT2
*       WHERE PERNR IN S_PERNR
*       AND   ENDDA GE V_ENDDA.
*
*check STAT2 = '3'.
*
*
*SELECT SINGLE
*       abkrs
*       permo
*       FROM t549a                  " Table for Payroll Areas
*       INTO (gv_abkrs , gv_permo)
*       WHERE abkrs EQ S_ABKRS-LOW.
*
*
*
**__Get Pay Period Begin Date and End Date.
*    IF gv_permo IS NOT INITIAL.
*      SELECT single permo
*                    begda
*                    endda
*                    FROM t549q                 " Table for Payroll Period
*                    INTO corresponding fields of gw_t549q
*                    WHERE permo EQ gv_permo
*                    AND   PABRP EQ P1_MONTH
*                    AND   PABRJ EQ P1_YEAR.
*
*      IF sy-subrc NE 0.
*
*
*        EXIT.
*      ENDIF.
*    ENDIF.
*
**_NEW Coding for new screen.
*
*IF RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.
*
*    SELECT * FROM PA0001 INTO TABLE IT_PERNR WHERE  ABKRS IN S_ABKRS
*                                             AND    PERNR IN S_PERNR
*                                             AND    ENDDA GE gw_t549q-begda
*                                             AND    BEGDA LE gw_t549q-endda
*                                             AND    BUKRS IN S_BUKRS
*                                             AND    WERKS IN S_WERKS
*                                             AND    BTRTL IN S_BTRTL
*                                             AND    PERSG IN S_PERSG.
*ENDIF.
*
*else.
*
*  MESSAGE  'Payroll Area and Selection Inputs doesnt match' Type 'E'.
*
*
*endif.


*AT SELECTION-SCREEN ON BLOCK B3.
*
*if S_PERNR is NOT INITIAL.
*
*SELECT SINGLE
*       STAT2 FROM PA0000
*       INTO STAT2
*       WHERE PERNR IN S_PERNR
*       AND   ENDDA GE V_ENDDA.
*
*check STAT2 = '3'.
*
*
*SELECT SINGLE
*       abkrs
*       permo
*       FROM t549a                  " Table for Payroll Areas
*       INTO (gv_abkrs , gv_permo)
*       WHERE abkrs EQ S_ABKRS-LOW.
*
*
*
**__Get Pay Period Begin Date and End Date.
*    IF gv_permo IS NOT INITIAL.
*      SELECT single permo
*                    begda
*                    endda
*                    FROM t549q                 " Table for Payroll Period
*                    INTO corresponding fields of gw_t549q
*                    WHERE permo EQ gv_permo
*                    AND   PABRP EQ P1_MONTH
*                    AND   PABRJ EQ P1_YEAR.
*
*      IF sy-subrc NE 0.
*
*
*        EXIT.
*      ENDIF.
*    ENDIF.
*
**_NEW Coding for new screen.
*
*IF RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.
*
*    SELECT * FROM PA0001 INTO TABLE IT_PERNR WHERE  ABKRS IN S_ABKRS
*                                             AND    PERNR IN S_PERNR
*                                             AND    ENDDA GE gw_t549q-begda
*                                             AND    BEGDA LE gw_t549q-endda
*                                             AND    BUKRS IN S_BUKRS
*                                             AND    WERKS IN S_WERKS
*                                             AND    BTRTL IN S_BTRTL
*                                             AND    PERSG IN S_PERSG.
*ENDIF.
*
*
*endif.

*_ ADDED BY ANIL ON 19 FEB 2013

*!Start of Selection
START-OF-SELECTION.

  IF S_PERNR IS NOT INITIAL.

*SELECT SINGLE
*       STAT2 FROM PA0000
*       INTO STAT2
*       WHERE PERNR IN S_PERNR
*       AND   ENDDA GE V_ENDDA.
*
*check STAT2 = '3'.
*BREAK-POINT.

    SELECT * FROM PA0000
      INTO TABLE IT_PA0000
      WHERE PERNR IN S_PERNR
      AND   ENDDA = '99991231'.
*  AND   STAT2 IN S_STAT2.


    IF S_STAT2[] IS NOT INITIAL.

      LOOP AT S_STAT2 .

        IF  S_STAT2-SIGN = 'E'
        AND S_STAT2-OPTION = 'EQ'.

          REFRESH : IT_PA0000.

          SELECT * FROM PA0000
            INTO TABLE IT_PA0000
            WHERE PERNR IN S_PERNR
            AND   ENDDA = '99991231'
            AND   STAT2 IN S_STAT2.

*DELETE IT_PA0000 WHERE STAT2 = S_STAT2-LOW.

        ELSEIF S_STAT2-SIGN = 'E'
        AND S_STAT2-OPTION = 'BT'.

          DELETE IT_PA0000 WHERE STAT2 BETWEEN S_STAT2-LOW AND S_STAT2-HIGH.

        ELSEIF S_STAT2-SIGN = 'I'
        AND S_STAT2-OPTION = 'EQ'.

          REFRESH : IT_PA0000.

          SELECT * FROM PA0000
            INTO TABLE IT_PA0000
            WHERE PERNR IN S_PERNR
            AND   ENDDA = '99991231'
            AND   STAT2 IN S_STAT2.

*DELETE IT_PA0000 WHERE STAT2 NE S_STAT2-LOW.

        ENDIF.

        IF S_STAT2-SIGN = 'I'
        AND S_STAT2-OPTION = 'BT'.

          REFRESH : IT_PA0000.

          SELECT * FROM PA0000
            INTO TABLE IT_PA0000
            WHERE PERNR IN S_PERNR
            AND   ENDDA = '99991231'
            AND   STAT2 IN S_STAT2.

        ENDIF.

      ENDLOOP.

    ENDIF.


*IF S_STAT2-LOW IS NOT INITIAL.
*
*  DELETE IT_PA0000 WHERE STAT2 NE S_STAT2-LOW.
*
*ENDIF.
  ENDIF.

*_ ADDED BY ANIL ON 19 FEB 2013

  SELECT SINGLE
         ABKRS
         PERMO
         FROM T549A                  " Table for Payroll Areas
         INTO (GV_ABKRS , GV_PERMO)
         WHERE ABKRS EQ S_ABKRS-LOW.



*__Get Pay Period Begin Date and End Date.
  IF GV_PERMO IS NOT INITIAL.
    SELECT SINGLE PERMO
                  BEGDA
                  ENDDA
                  FROM T549Q                 " Table for Payroll Period
                  INTO CORRESPONDING FIELDS OF GW_T549Q
                  WHERE PERMO EQ GV_PERMO
                  AND   PABRP EQ P1_MONTH
                  AND   PABRJ EQ P1_YEAR.

    IF SY-SUBRC NE 0.


      EXIT.
    ENDIF.
  ENDIF.

*_NEW Coding for new screen.

  IF RB_OP = 'X' AND S_ABKRS IS NOT INITIAL.

    SELECT * FROM PA0001
      INTO TABLE IT_PERNR
      WHERE  ABKRS IN S_ABKRS
      AND    PERNR IN S_PERNR
      AND    ENDDA GE GW_T549Q-BEGDA
      AND    BEGDA LE GW_T549Q-ENDDA
      AND    BUKRS IN S_BUKRS
      AND    WERKS IN S_WERKS
      AND    BTRTL IN S_BTRTL
      AND    PERSG IN S_PERSG.

  ENDIF.

  SORT IT_PERNR BY PERNR.

  LOOP AT IT_PERNR INTO WA_PERNR.

    READ TABLE IT_PA0000 INTO WA_PA0000 WITH KEY PERNR = WA_PERNR-PERNR.

    IF SY-SUBRC NE 0.

      DELETE IT_PERNR WHERE PERNR = WA_PERNR-PERNR.

    ENDIF.
    CLEAR : WA_PERNR , WA_PA0000.
  ENDLOOP.


*LOOP AT IT_PA0000 INTO WA_PA0000.
*
*READ TABLE IT_PERNR INTO WA_PERNR WITH KEY PERNR = WA_PA0000-PERNR.
*
*IF SY-SUBRC NE 0.
*
*  DELETE IT_PERNR WHERE PERNR = WA_PERNR-PERNR.
*
*  ENDIF.
*CLEAR : WA_PERNR.
*ENDLOOP.


*IF IT_PERNR IS NOT INITIAL.
*
*  MESSAGE  'Payroll Area and Selection Inputs doesnt match' Type 'E'.
*
*ENDIF.
*_ Checking for the PayRoll runned Employees
  LOOP AT IT_PERNR INTO WA_PERNR.
    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        PERSNR             = WA_PERNR-PERNR
*       BUFFER             =
*       NO_AUTHORITY_CHECK = ' '
      IMPORTING
        MOLGA              = COUNTRY
      TABLES
        IN_RGDIR           = RGDIR
      EXCEPTIONS
        NO_RECORD_FOUND    = 1
        OTHERS             = 2.
    CHECK SY-SUBRC = 0.
    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        PERSNR             = WA_PERNR-PERNR
*       BUFFER             =
*       NO_AUTHORITY_CHECK = ' '
      IMPORTING
        MOLGA              = COUNTRY
      TABLES
        IN_RGDIR           = RGDIR
      EXCEPTIONS
        NO_RECORD_FOUND    = 1
        OTHERS             = 2.
    CHECK SY-SUBRC = 0.

    IF RGDIR[] IS NOT INITIAL.

      CALL FUNCTION 'CD_SELECT_DATE_RANGE'
        EXPORTING
          FPPER_BEGDA = GW_T549Q-BEGDA
          FPPER_ENDDA = GW_T549Q-ENDDA
        TABLES
          IN_RGDIR    = RGDIR
          OUT_RGDIR   = IT_OUT_RGDIR.

    ENDIF.
    CHECK SY-SUBRC = 0.

    IF IT_OUT_RGDIR IS NOT INITIAL.
      WA_P0001-PERNR = WA_PERNR-PERNR.
      APPEND WA_P0001-PERNR TO IT_P0001.
      APPEND WA_P0001-PERNR TO IT_P0001_1.
    ENDIF.

  ENDLOOP.


**_ Deleting the perners whose payroll is not runned.
  LOOP AT IT_PERNR INTO WA_PERNR.
    READ TABLE IT_P0001 INTO WA_P0001 WITH KEY PERNR = WA_PERNR-PERNR.
    IF SY-SUBRC NE '0'.
      DELETE IT_PERNR WHERE PERNR = WA_PERNR-PERNR.
    ENDIF.
  ENDLOOP.

*_ END OF ADDITION BY ANIL ON 19 FEB 2013
****    SELECT * FROM PA0001 INTO TABLE IT_PERNR WHERE  WERKS IN SO_PERSA
****                                             AND    PERNR IN SO_PERNR
****                                             AND    PERSK IN SO_PERSK
****                                             AND    PERSG IN SO_PERSG
****                                             AND    BUKRS IN SO_BUKRS "
****                                             AND    ABKRS IN SO_ABKRS
****                                             AND    ENDDA GE V_ENDDA.
****    CHECK sy-subrc = 0.
****    SORT IT_PERNR ASCENDING by PERNR.
  DELETE ADJACENT DUPLICATES FROM IT_PERNR COMPARING PERNR.
  P_BEGDA = GW_T549Q-BEGDA.
  P_ENDDA = GW_T549Q-ENDDA.


*
*       W_FORMNAME =  'ZHR_PAYSLIP_NEW1'.
**!Convert Form name into Function module
*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        FORMNAME = W_FORMNAME
*      IMPORTING
*        FM_NAME  = W_FMNAME.
*
**_ Anil
*  W_SSFCOMPOP-TDDEST     = 'SHL01'.
*  W_SSFCTRLOP-PREVIEW    = 'X'.
*  W_SSFCTRLOP-NO_DIALOG  = 'X'.
*  W_SSFCTRLOP-GETOTF     = 'X'.
*
*DELETE IT_P0001 WHERE PERNR IS INITIAL.
*
*CALL FUNCTION            w_fmname    "'/1BCDWB/SF00000044'
*  EXPORTING
**   ARCHIVE_INDEX              =
**   ARCHIVE_INDEX_TAB          =
**   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         = W_SSFCTRLOP
**   MAIL_APPL_OBJ              =
**   MAIL_RECIPIENT             =
**   MAIL_SENDER                =
*   OUTPUT_OPTIONS             = W_SSFCOMPOP
*   USER_SETTINGS              = 'X'
*    PNBEGDA                    = P_BEGDA
*    PNENDDA                    = P_ENDDA
*    PERNR                      = WA_PERNR-PERNR
* IMPORTING
**   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            = IT_OTF
**   JOB_OUTPUT_OPTIONS         =
*  TABLES
*    IT_P0001_1                 = IT_P0001_1
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*
*
**!To Convert into PDF Format pass OTF Data
**      IT_OUT[]  = IT_OTF-OTFDATA.
*
* OTF_DATA[]  = IT_OTF-OTFDATA.
*
*  CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
*    EXPORTING
*      I_OTF                    = OTF_DATA[]
*    EXCEPTIONS
*      CONVERT_OTF_TO_PDF_ERROR = 1
*      CNTL_ERROR               = 2
*      OTHERS                   = 3.
*  IF SY-SUBRC <> 0.
**Not Applicable
*  ENDIF.
*
**ENDIF.
*
*

  DELETE ADJACENT DUPLICATES FROM IT_P0001_1 COMPARING PERNR. " Added By Govind On 21-Mar-201515
****Code added by vcentric -
    EXPORT IT_P0001_1 = IT_P0001_1[] TO MEMORY ID 'IT_P0001_1'.
    EXPORT IT_P0000 = IT_P0000[] TO MEMORY ID 'IT_P0000'.
    EXPORT IT_PA2006 = IT_PA2006[] TO MEMORY ID 'IT_PA2006'.

  IF  SY-TCODE = 'ZHR_PAYSLIP' OR  SY-TCODE = 'ZHR_PAYSLIP1'.
****Code added by vcentric -
    IF P_MAIL = 'X'.

      IT_MAIL_001[] = IT_P0001_1[].

      REFRESH : IT_P0001_1[].

      LOOP AT IT_MAIL_001 INTO WA_MAIL_001.
        REFRESH : IT_P0001_1[].
        IT_P0001_1 = WA_MAIL_001 .
        APPEND IT_P0001_1.


        CLEAR : LV_MAIL , LV_NAME.
        SELECT USRID_LONG
          UP TO 1 ROWS FROM PA0105
          INTO LV_MAIL
          WHERE PERNR = WA_MAIL_001-PERNR
          AND   SUBTY = '0010'
          AND   ENDDA = '99991231' ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

        "Added By Govind On 20-03-2015

        CLEAR : LV_TITLE.
        SELECT ANRED
          UP TO 1 ROWS FROM PA0002
          INTO LV_TITLE
          WHERE PERNR = WA_MAIL_001-PERNR
          AND   ENDDA = '99991231' ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

        CLEAR : LV_TITNAME.
        SELECT SINGLE ATEXT
          FROM T522T
          INTO LV_TITNAME
          WHERE SPRSL = 'EN'
          AND   ANRED = LV_TITLE.
*
*    REFRESH : IT_PA2006.
*    SELECT * FROM PA2006
*      INTO TABLE IT_PA2006
*      WHERE PERNR = WA_MAIL_001-PERNR
*      AND   ENDDA = '99991231'.
*
*      CLEAR : W_PLBL , W_SLBL , W_CLBL , W_PL1 , W_PL2 , W_SL1 , W_SL2 , W_CL1 , W_CL2, LV_TITLE.
*      LOOP AT IT_PA2006 INTO WA_PA2006.
*
*        CASE WA_PA2006-SUBTY.
*
*         WHEN 20. " PL BL
*           W_PL1 = WA_PA2006-ANZHL + W_PL1.
*           W_PL2 = WA_PA2006-KVERB + W_PL2.
*
*         WHEN 10.
*           W_SL1 = WA_PA2006-ANZHL + W_SL1.
*           W_SL2 = WA_PA2006-KVERB + W_SL2.
*
*         WHEN 30.
*           W_CL1 = WA_PA2006-ANZHL + W_CL1.
*           W_CL2 = WA_PA2006-KVERB + W_CL2.
*
*       ENDCASE.
*
*        ENDLOOP.
*
*           W_PLBL = W_PL1 - W_PL2.
*           IF W_PLBL LT 0.
*             W_PLBL = W_PLBL * -1.
*           ENDIF.
*
*           W_SLBL = W_SL1 - W_SL2.
*           IF W_SLBL LT 0.
*             W_SLBL = W_SLBL * -1.
*           ENDIF.
*
*           W_CLBL = W_CL1 - W_CL2.
*           IF W_CLBL LT 0.
*             W_CLBL = W_CLBL * -1.
*           ENDIF.

        SELECT VORNA
          UP TO 1 ROWS FROM PA0002
          INTO LV_NAME
          WHERE PERNR = WA_MAIL_001-PERNR
          AND   ENDDA = '99991231' ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

        CALL FUNCTION 'MONTH_NAMES_GET'
        EXPORTING
          LANGUAGE                    = SY-LANGU
*         IMPORTING
*           RETURN_CODE                 =
         TABLES
           MONTH_NAMES                 = IT_MONTH
*         EXCEPTIONS
*           MONTH_NAMES_NOT_FOUND       = 1
*           OTHERS                      = 2
                 .
        IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


        READ TABLE IT_MONTH INTO WA_MONTH
           WITH KEY MNR = P_ENDDA+4(2).
        IF SY-SUBRC = 0.
          W_MNAME = WA_MONTH-LTX.
        ENDIF.

        W_YNAME = P_ENDDA+2(2).
        REFRESH : IT_MONTH .
        CLEAR : WA_MONTH.

*        W_FORMNAME =  'ZHR_PAYSLIP_NEW1'.
     W_FORMNAME =  'ZHR_PAYSLIP_NEW14'.
*!Convert Form name into Function module
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            FORMNAME = W_FORMNAME
          IMPORTING
            FM_NAME  = W_FMNAME.

*_ Anil
        W_SSFCOMPOP-TDDEST     = 'SHL01'.
        W_SSFCTRLOP-PREVIEW    = 'X'.
        W_SSFCTRLOP-NO_DIALOG  = 'X'.
        W_SSFCTRLOP-GETOTF     = 'X'.

        LOOP AT  IT_P0001_1.
          CLEAR : WA_PERNR.
          DELETE IT_P0001 WHERE PERNR IS INITIAL.
          WA_PERNR-PERNR = WA_MAIL_001-PERNR.


          CALL FUNCTION            W_FMNAME    "'/1BCDWB/SF00000044'
            EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
             CONTROL_PARAMETERS         = W_SSFCTRLOP
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
             OUTPUT_OPTIONS             = W_SSFCOMPOP
             USER_SETTINGS              = 'X'
              PNBEGDA                    = P_BEGDA
              PNENDDA                    = P_ENDDA
              PERNR                      = WA_PERNR-PERNR
*    W_PLBL2                    = W_PLBL
*    W_SLBL2                    = W_SLBL
*    W_CLBL2                    = W_CLBL
*    LV_TITNAME                 = LV_TITNAME

           IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
             JOB_OUTPUT_INFO            = IT_OTF
*   JOB_OUTPUT_OPTIONS         =
            TABLES
              IT_P0001_1                 = IT_P0001_1
              IT_P0000                   = IT_P0000
              IT_PA2006                  = IT_PA2006
           EXCEPTIONS
             FORMATTING_ERROR           = 1
             INTERNAL_ERROR             = 2
             SEND_ERROR                 = 3
             USER_CANCELED              = 4
             OTHERS                     = 5
                    .
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.

*!To Convert into PDF Format pass OTF Data
          IT_OUT[]  = IT_OTF-OTFDATA.
*   OTF_DATA[]  = IT_OTF-OTFDATA.


*  CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
*    EXPORTING
**      I_OTF                    = IT_OUT[]
*      I_OTF                    = OTF_DATA[]
*    EXCEPTIONS
*      CONVERT_OTF_TO_PDF_ERROR = 1
*      CNTL_ERROR               = 2
*      OTHERS                   = 3.
*  IF SY-SUBRC <> 0.
**Not Applicable
*  ENDIF.


          CLEAR : WA_PERNR.

          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              FORMAT                = 'PDF'
              MAX_LINEWIDTH         = 132
            IMPORTING
              BIN_FILESIZE          = V_LEN_IN
            TABLES
              OTF                   = IT_OUT
              LINES                 = I_TLINE
            EXCEPTIONS
              ERR_MAX_LINEWIDTH     = 1
              ERR_FORMAT            = 2
              ERR_CONV_NOT_POSSIBLE = 3
              OTHERS                = 4.
          IF SY-SUBRC <> 0.
          ENDIF.

        ENDLOOP.

        REFRESH : IT_P0001_1[].

        LOOP AT I_TLINE.

          TRANSLATE I_TLINE USING '~'.
          CONCATENATE WA_BUFFER I_TLINE INTO WA_BUFFER.

        ENDLOOP.

        TRANSLATE WA_BUFFER USING '~'.

        DO.

          I_RECORD = WA_BUFFER.
          APPEND I_RECORD.
          SHIFT WA_BUFFER LEFT BY 255 PLACES.

          IF WA_BUFFER IS INITIAL.
            EXIT.
          ENDIF.

        ENDDO.

        """"""'Attachment

        REFRESH: I_RECLIST,
                  I_OBJTXT,
                  I_OBJBIN,
                  I_OBJPACK.

        CLEAR WA_OBJHEAD.


        I_OBJBIN[] = I_RECORD[].

        CLEAR : I_OBJTXT , I_OBJTXT-LINE , WA_OBJTXT.

        IF WA_OBJTXT IS INITIAL.

*                  wa_objtxt-line = '<div>Dear Associate,</div>'.
          CONCATENATE 'Dear' LV_TITNAME '.' LV_NAME ',' INTO WA_OBJTXT-LINE SEPARATED BY SPACE.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>Greetings for the day!,</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          CONCATENATE 'Please find the attached Pay-Slip for the month of' W_MNAME W_YNAME INTO WA_OBJTXT-LINE SEPARATED BY SPACE.
*                  wa_objtxt-line = '<p>Dear Associate,</p><p>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div> </div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>Feel free to contact us for further queries on 044 42949607 or mail us on hrsupport@sheenlac.in</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>Note : While replying to this E-mail please mention your Employee number in the Subject line.</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>SAP System generated statement enclosed which does not Require Signature.</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>Thanks & Regards,</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<br> </br>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

          WA_OBJTXT-LINE = '<div>Payroll Helpdesk,</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

*                  wa_objtxt-line = '<br> </br>'.
*                  APPEND WA_objtxt TO I_objtxt.
*                  CLEAR wa_objtxt.

          WA_OBJTXT-LINE = '<div>Sheenlac Paints Limited.</div>'.
          APPEND WA_OBJTXT TO I_OBJTXT.
          CLEAR WA_OBJTXT.

**               CREATE MESSAGE BODY TITLE AND DESCRIPTION
**               I_OBJTXT = 'TEST WITH PDF-ATTACHMENT!'.
*
*                I_OBJTXT = 'Payslip for the Month'.
*                APPEND I_OBJTXT.

          CONCATENATE 'Pay-Slip for the month of' W_MNAME W_YNAME INTO WA_DOC_CHNG-OBJ_DESCR SEPARATED BY SPACE.

          DESCRIBE TABLE I_OBJTXT LINES V_LINES_TXT.

          READ TABLE I_OBJTXT INDEX V_LINES_TXT.
          WA_DOC_CHNG-OBJ_NAME = 'SMARTFORM'.
          WA_DOC_CHNG-EXPIRY_DAT = SY-DATUM + 10.
          WA_DOC_CHNG-OBJ_DESCR = WA_DOC_CHNG-OBJ_DESCR.
          WA_DOC_CHNG-SENSITIVTY = 'F'.
          WA_DOC_CHNG-DOC_SIZE = V_LINES_TXT * 255.
*                 MAIN TEXT

          CLEAR I_OBJPACK-TRANSF_BIN.
          I_OBJPACK-HEAD_START = 1.
          I_OBJPACK-HEAD_NUM = 0.
          I_OBJPACK-BODY_START = 1.
          I_OBJPACK-BODY_NUM = V_LINES_TXT.
          I_OBJPACK-DOC_TYPE = 'HTM'.
*                I_OBJPACK-DOC_TYPE = 'RAW'.
          APPEND I_OBJPACK.

*                 ATTACHMENT (PDF-ATTACHMENT)

          I_OBJPACK-TRANSF_BIN = 'X'.
          I_OBJPACK-HEAD_START = 1.
          I_OBJPACK-HEAD_NUM = 0.
          I_OBJPACK-BODY_START = 1.

          DESCRIBE TABLE I_OBJBIN LINES V_LINES_BIN.

          READ TABLE I_OBJBIN INDEX V_LINES_BIN.
          I_OBJPACK-DOC_SIZE = V_LINES_BIN * 255 .
          I_OBJPACK-BODY_NUM = V_LINES_BIN.
          I_OBJPACK-DOC_TYPE = 'PDF'.
          I_OBJPACK-OBJ_NAME = 'SMART'.
          I_OBJPACK-OBJ_DESCR = 'Payslip'.
          APPEND I_OBJPACK.

          CLEAR I_RECLIST.
*                I_RECLIST-RECEIVER = 'SURENDRAN.HARIPRASAD@GMAIL.COM'.
          I_RECLIST-RECEIVER = LV_MAIL.
          I_RECLIST-REC_TYPE = 'U'.
          APPEND I_RECLIST.

          CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
            EXPORTING
              DOCUMENT_DATA              = WA_DOC_CHNG
              PUT_IN_OUTBOX              = 'X'
              COMMIT_WORK                = 'X'
            TABLES
              PACKING_LIST               = I_OBJPACK
              OBJECT_HEADER              = WA_OBJHEAD
              CONTENTS_BIN               = I_OBJBIN
              CONTENTS_TXT               = I_OBJTXT
              RECEIVERS                  = I_RECLIST
            EXCEPTIONS
              TOO_MANY_RECEIVERS         = 1
              DOCUMENT_NOT_SENT          = 2
              DOCUMENT_TYPE_NOT_EXIST    = 3
              OPERATION_NO_AUTHORIZATION = 4
              PARAMETER_ERROR            = 5
              X_ERROR                    = 6
              ENQUEUE_ERROR              = 7
              OTHERS                     = 8.
          IF SY-SUBRC <> 0.
*      WRITE:/ 'Error When Sending the File', sy-subrc.
            WA_FINAL-STATUS = 'Error'.
            WA_FINAL-FIELD = WA_MAIL_001-PERNR.
            WA_FINAL-MESSAGE = 'Mail Not Sent'.
            APPEND WA_FINAL TO IT_FINAL.
            CLEAR : WA_FINAL.
          ELSE.
*      WRITE:/ 'Mail sent'.
            WA_FINAL-STATUS = 'Success'.
            WA_FINAL-FIELD = WA_MAIL_001-PERNR.
            WA_FINAL-MESSAGE = 'Mail Sent'.
            APPEND WA_FINAL TO IT_FINAL.
            CLEAR : WA_FINAL.
          ENDIF.

        ENDIF.
        REFRESH : IT_OUT , I_TLINE[] , I_OBJPACK , WA_OBJHEAD , I_OBJBIN , I_OBJTXT , I_RECLIST , I_RECORD[] , I_RECORD , IT_OTF.
        CLEAR : IT_OUT , I_TLINE , WA_OBJHEAD , LV_MAIL , I_OBJTXT , I_OBJPACK ,I_OBJBIN , I_RECLIST , V_LEN_IN , WA_OBJTXT , I_OBJTXT-LINE,
                WA_DOC_CHNG , LV_MAIL, WA_FINAL , I_RECORD , IT_OTF , WA_MAIL_001 , LV_NAME , LV_TITLE , LV_TITNAME.

      ENDLOOP.


    ELSE.

*BREAK-POINT.

*    CLEAR : LV_TITLE.
*    SELECT SINGLE ANRED
*      FROM PA0002
*      INTO LV_TITLE
*      WHERE PERNR = WA_PERNR-PERNR
*      AND   ENDDA = '99991231'.
*
*    CLEAR : LV_TITNAME.
*    SELECT SINGLE ATEXT
*      FROM T522T
*      INTO LV_TITNAME
*      WHERE SPRSL = 'EN'
*      AND   ANRED = LV_TITLE.

*    REFRESH : IT_PA2006.
*    SELECT * FROM PA2006
*      INTO TABLE IT_PA2006
*      WHERE PERNR = WA_PERNR-PERNR
*      AND   ENDDA = '99991231'.
*
*      CLEAR : W_PLBL , W_SLBL , W_CLBL , W_PL1 , W_PL2 , W_SL1 , W_SL2 , W_CL1 , W_CL2.
*      LOOP AT IT_PA2006 INTO WA_PA2006.
*
*        CASE WA_PA2006-SUBTY.
*
*         WHEN 20. " PL BL
*           W_PL1 = WA_PA2006-ANZHL + W_PL1.
*           W_PL2 = WA_PA2006-KVERB + W_PL2.
*
*         WHEN 10.
*           W_SL1 = WA_PA2006-ANZHL + W_SL1.
*           W_SL2 = WA_PA2006-KVERB + W_SL2.
*
*         WHEN 30.
*           W_CL1 = WA_PA2006-ANZHL + W_CL1.
*           W_CL2 = WA_PA2006-KVERB + W_CL2.
*
*       ENDCASE.
*
*        ENDLOOP.
*
*           W_PLBL = W_PL1 - W_PL2.
*           IF W_PLBL LT 0.
*             W_PLBL = W_PLBL * -1.
*           ENDIF.
*
*           W_SLBL = W_SL1 - W_SL2.
*           IF W_SLBL LT 0.
*             W_SLBL = W_SLBL * -1.
*           ENDIF.
*
*           W_CLBL = W_CL1 - W_CL2.
*           IF W_CLBL LT 0.
*             W_CLBL = W_CLBL * -1.
*           ENDIF.

**  LOOP AT IT_PERNR INTO WA_PERNR.
**
**     SELECT SINGLE STAT2 FROM PA0000 INTO STATUS
**              WHERE PERNR EQ WA_PERNR-PERNR
**                AND ENDDA GE P_ENDDA.
**
**      CHECK STATUS EQ '3'.
**
**       W_FORMNAME =  'ZHR_PAYSLIP'. "Added on 24.11.2012
***!Convert Form name into Function module
**    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
**      EXPORTING
**        FORMNAME = W_FORMNAME
**      IMPORTING
**        FM_NAME  = W_FMNAME.
**
**  if SY-UCOMM <> 'CANC'.
**
**CALL FUNCTION      W_FMNAME   " '/1BCDWB/SF00000029'
**  EXPORTING
***   ARCHIVE_INDEX              =
***   ARCHIVE_INDEX_TAB          =
***   ARCHIVE_PARAMETERS         =
**   CONTROL_PARAMETERS         = W_SSFCTRLOP
***   MAIL_APPL_OBJ              =
***   MAIL_RECIPIENT             =
***   MAIL_SENDER                =
***   OUTPUT_OPTIONS             =
***   USER_SETTINGS              = 'X'
**    PNBEGDA                    = P_BEGDA
**    PNENDDA                    = P_ENDDA
**    PERNR                      = WA_PERNR-PERNR
*** IMPORTING
***   DOCUMENT_OUTPUT_INFO       =
***   JOB_OUTPUT_INFO            =
***   JOB_OUTPUT_OPTIONS         =
** EXCEPTIONS
**   FORMATTING_ERROR           = 1
**   INTERNAL_ERROR             = 2
**   SEND_ERROR                 = 3
**   USER_CANCELED              = 4
**   OTHERS                     = 5
**          .
**IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**ENDIF.
**
***_ Anil
**
**
**  W_SSFCOMPOP-TDDEST = 'SHL01'.
**  W_SSFCTRLOP-PREVIEW = 'X'.
**  W_SSFCTRLOP-NO_DIALOG = 'X'.
**  W_SSFCTRLOP-GETOTF = 'X'.
**
**CALL FUNCTION       W_FMNAME   "  '/1BCDWB/SF00000029'
**  EXPORTING
***   ARCHIVE_INDEX              =
***   ARCHIVE_INDEX_TAB          =
***   ARCHIVE_PARAMETERS         =
**   CONTROL_PARAMETERS         = W_SSFCTRLOP
***   MAIL_APPL_OBJ              =
***   MAIL_RECIPIENT             =
***   MAIL_SENDER                =
**   OUTPUT_OPTIONS             = W_SSFCOMPOP
***   USER_SETTINGS              = 'X'
**    PNBEGDA                    = P_BEGDA
**    PNENDDA                    = P_ENDDA
**    PERNR                      = WA_PERNR-PERNR
** IMPORTING
***   DOCUMENT_OUTPUT_INFO       =
**   JOB_OUTPUT_INFO            = IT_OTF
***   JOB_OUTPUT_OPTIONS         =
** EXCEPTIONS
**   FORMATTING_ERROR           = 1
**   INTERNAL_ERROR             = 2
**   SEND_ERROR                 = 3
**   USER_CANCELED              = 4
**   OTHERS                     = 5
**          .
**IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**ENDIF.
**
***!To Convert into PDF Format pass OTF Data
**      IT_OUT[] = IT_OTF-OTFDATA.
** it_otf_final[] = IT_OTF-OTFDATA.
**
**  CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
**    EXPORTING
**      I_OTF                    = IT_OUT[]
**    EXCEPTIONS
**      CONVERT_OTF_TO_PDF_ERROR = 1
**      CNTL_ERROR               = 2
**      OTHERS                   = 3.
**  IF SY-SUBRC <> 0.
***Not Applicable
**  ENDIF.
***_ Anil
**ENDIF.
**ENDLOOP.

      SORT IT_P0001_1 BY PERNR.
      SORT IT_P0000   BY PERNR.

*      W_FORMNAME =  'ZHR_PAYSLIP_NEW1'.
      W_FORMNAME =  'ZHR_PAYSLIP_NEW14'.
*!Convert Form name into Function module
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME = W_FORMNAME
        IMPORTING
          FM_NAME  = W_FMNAME.

*_ Anil
      W_SSFCOMPOP-TDDEST     = 'SHL01'.
      W_SSFCTRLOP-PREVIEW    = 'X'.
      W_SSFCTRLOP-NO_DIALOG  = 'X'.
      W_SSFCTRLOP-GETOTF     = 'X'.

      DELETE IT_P0001 WHERE PERNR IS INITIAL.

*BREAK-POINT.


      CALL FUNCTION            W_FMNAME    "'/1BCDWB/SF00000044'
        EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
         CONTROL_PARAMETERS         = W_SSFCTRLOP
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
         OUTPUT_OPTIONS             = W_SSFCOMPOP
         USER_SETTINGS              = 'X'
          PNBEGDA                    = P_BEGDA
          PNENDDA                    = P_ENDDA
          PERNR                      = WA_PERNR-PERNR
*    W_PLBL2                    = W_PLBL
*    W_SLBL2                    = W_SLBL
*    W_CLBL2                    = W_CLBL
*    LV_TITNAME                 = LV_TITNAME

       IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
         JOB_OUTPUT_INFO            = IT_OTF
*   JOB_OUTPUT_OPTIONS         =
        TABLES
          IT_P0001_1                 = IT_P0001_1
          IT_P0000                   = IT_P0000
          IT_PA2006                  = IT_PA2006
       EXCEPTIONS
         FORMATTING_ERROR           = 1
         INTERNAL_ERROR             = 2
         SEND_ERROR                 = 3
         USER_CANCELED              = 4
         OTHERS                     = 5
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.


*!To Convert into PDF Format pass OTF Data
*      IT_OUT[]  = IT_OTF-OTFDATA.

      OTF_DATA[]  = IT_OTF-OTFDATA.

      CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
        EXPORTING
          I_OTF                    = OTF_DATA[]
        EXCEPTIONS
          CONVERT_OTF_TO_PDF_ERROR = 1
          CNTL_ERROR               = 2
          OTHERS                   = 3.
      IF SY-SUBRC <> 0.
*Not Applicable
      ENDIF.
      CLEAR : LV_TITNAME.
    ENDIF.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  IF IT_FINAL IS NOT INITIAL.

    CLEAR : W_FCAT .
    W_FCAT-FIELDNAME = 'STATUS' .
    W_FCAT-COL_POS = 1 .
    W_FCAT-SELTEXT_L = 'Status' .
    W_FCAT-OUTPUTLEN = 10 .
    W_FCAT-TABNAME = 'IT_FINAL'.
    APPEND W_FCAT TO FCAT.


    CLEAR : W_FCAT .
    W_FCAT-FIELDNAME = 'FIELD' .
    W_FCAT-COL_POS = 2 .
    W_FCAT-SELTEXT_L = 'Employee No' .
    W_FCAT-TABNAME = 'IT_FINAL'.
    W_FCAT-OUTPUTLEN = 50 .
    APPEND W_FCAT TO FCAT.

    CLEAR : W_FCAT .
    W_FCAT-FIELDNAME = 'MESSAGE' .
    W_FCAT-COL_POS = 3 .
    W_FCAT-SELTEXT_L = 'Message' .
    W_FCAT-TABNAME = 'IT_FINAL'.
    W_FCAT-OUTPUTLEN = 50 .
    APPEND W_FCAT TO FCAT.

    S_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
      IS_LAYOUT                         = S_LAYOUT
      IT_FIELDCAT                       = FCAT
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
        T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.



  ENDIF.

*
*
*
*
*
*
*
*
*
*
*
*
*"Added by Surendran CLSS on 03.11.2014
*
**DATA : v_len_in LIKE sood-objlen,
**      v_len_out LIKE sood-objlen,
**      v_len_outn TYPE i,
**      v_lines_txt TYPE i,
**      v_lines_bin TYPE i.
*
**DATA : wa_buffer TYPE string. "To convert from 132 to 255
*
**DATA: i_otf TYPE itcoo OCCURS 0 WITH HEADER LINE,
**      i_tline TYPE TABLE OF tline WITH HEADER LINE,
**      i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE.
*
**DATA :i_record LIKE solisti1 OCCURS 0 WITH HEADER LINE.
*
**"""""""Work Area declarations
**
**DATA :      wa_objhead TYPE soli_tab,
**            w_ctrlop TYPE ssfctrlop,
**            w_compop TYPE ssfcompop,
**            w_return TYPE ssfcrescl,
**            wa_doc_chng TYPE sodocchgi1,
**            w_data TYPE sodocchgi1,
**            wa_buffer TYPE string. "To convert from 132 to 255
*
*
**IF MAIL = 'X'.
*
*
*
*
*
*
*
**data :  file_name  type string,
**        file_path type string,
**        full_path type string.
**DATA : default_filename type string.
**
**DEFAULT_FILENAME = WA_MAIL_001-PERNR.
**
**if file_path is initial.
**
**  call method cl_gui_frontend_services=>file_save_dialog
**    exporting
**      window_title      = 'Save Document'
**      default_extension = 'PDF'
**      default_file_name = default_filename
**      file_filter       = 'PDF'
**    changing
**      filename          = file_name
**      path              = file_path
**      fullpath          = full_path.
**  if sy-subrc <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**  endif.
**
**  file_path = 'C:\Users\user1\Desktop\Payslip'.
**  concatenate file_path '\' default_filename '.pdf' into full_path.
***  export file_path to memory id 'FPATH'.
**else.
**  clear full_path.
**  concatenate file_path '\' default_filename '.pdf' into full_path.
**endif.
**
**call function 'GUI_DOWNLOAD'
**  exporting
**    bin_filesize            = bin_filesize
**    filename                = full_path
**    filetype                = 'BIN'
**  tables
**    data_tab                = i_pdf
**  exceptions
**    file_write_error        = 1
**    no_batch                = 2
**    gui_refuse_filetransfer = 3
**    invalid_type            = 4
**    no_authority            = 5
**    unknown_error           = 6
**    header_not_allowed      = 7
**    separator_not_allowed   = 8
**    filesize_not_allowed    = 9
**    header_too_long         = 10
**    dp_error_create         = 11
**    dp_error_send           = 12
**    dp_error_write          = 13
**    unknown_dp_error        = 14
**    access_denied           = 15
**    dp_out_of_memory        = 16
**    disk_full               = 17
**    dp_timeout              = 18
**    file_not_found          = 19
**    dataprovider_exception  = 20
**    control_flush_error     = 21
**    others                  = 22.
**"-----------------------------------------------------------------------------
**
**endloop.
*
**
**
***  CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
***    EXPORTING
***      I_OTF                    = IT_OUT[]
***    EXCEPTIONS
***      CONVERT_OTF_TO_PDF_ERROR = 1
***      CNTL_ERROR               = 2
***      OTHERS                   = 3.
***  IF SY-SUBRC <> 0.
****Not Applicable
***  ENDIF.
**
**
**   CLEAR : WA_PERNR.
**
**     CALL FUNCTION 'CONVERT_OTF'
**        EXPORTING
**          format                = 'PDF'
**          max_linewidth         = 132
**        IMPORTING
**          bin_filesize          = v_len_in
**        TABLES
**          otf                   = IT_OUT
**          lines                 = i_tline
**        EXCEPTIONS
**          err_max_linewidth     = 1
**          err_format            = 2
**          err_conv_not_possible = 3
**          OTHERS                = 4.
**      IF sy-subrc <> 0.
**      ENDIF.
**
**
**     LOOP AT i_tline.
**
**        TRANSLATE i_tline USING '~'.
**        CONCATENATE wa_buffer i_tline INTO wa_buffer.
**
**      ENDLOOP.
**
**      TRANSLATE wa_buffer USING '~'.
**
**      DO.
**
**        i_record = wa_buffer.
**        APPEND i_record.
**        SHIFT wa_buffer LEFT BY 255 PLACES.
**
**        IF wa_buffer IS INITIAL.
**          EXIT.
**        ENDIF.
**
**      ENDDO.
**
**
**
**
**""""""'Attachment
**
**                REFRESH: I_RECLIST,
**                          I_OBJTXT,
**                          I_OBJBIN,
**                          I_OBJPACK.
**
**                CLEAR WA_OBJHEAD.
**
**                I_OBJBIN[] = I_RECORD[].
**
**                IF I_OBJTXT IS INITIAL.
**
**                  CONCATENATE 'Dear' LV_NAME ','INTO I_OBJTXT-line SEPARATED BY SPACE.
**                  APPEND WA_objtxt TO IT_objtxt.
**                  CLEAR wa_objtxt.
**
**
**
**
**
**
**
**
**
***                 CREATE MESSAGE BODY TITLE AND DESCRIPTION
***                I_OBJTXT = 'TEST WITH PDF-ATTACHMENT!'.
**                I_OBJTXT = 'Payslip for the Month'.
**                APPEND I_OBJTXT.
**
**                DESCRIBE TABLE I_OBJTXT LINES V_LINES_TXT.
**
**                READ TABLE I_OBJTXT INDEX V_LINES_TXT.
**                WA_DOC_CHNG-OBJ_NAME = 'SMARTFORM'.
**                WA_DOC_CHNG-EXPIRY_DAT = SY-DATUM + 10.
**                WA_DOC_CHNG-OBJ_DESCR = 'SMARTFORM'.
**                WA_DOC_CHNG-SENSITIVTY = 'F'.
**                WA_DOC_CHNG-DOC_SIZE = V_LINES_TXT * 255.
***                 MAIN TEXT
**
**                CLEAR I_OBJPACK-TRANSF_BIN.
**                I_OBJPACK-HEAD_START = 1.
**                I_OBJPACK-HEAD_NUM = 0.
**                I_OBJPACK-BODY_START = 1.
**                I_OBJPACK-BODY_NUM = V_LINES_TXT.
**                I_OBJPACK-DOC_TYPE = 'RAW'.
**                APPEND I_OBJPACK.
**
***                 ATTACHMENT (PDF-ATTACHMENT)
**
**                I_OBJPACK-TRANSF_BIN = 'X'.
**                I_OBJPACK-HEAD_START = 1.
**                I_OBJPACK-HEAD_NUM = 0.
**                I_OBJPACK-BODY_START = 1.
**
**                DESCRIBE TABLE I_OBJBIN LINES V_LINES_BIN.
**
**                READ TABLE I_OBJBIN INDEX V_LINES_BIN.
**                I_OBJPACK-DOC_SIZE = V_LINES_BIN * 255 .
**                I_OBJPACK-BODY_NUM = V_LINES_BIN.
**                I_OBJPACK-DOC_TYPE = 'PDF'.
**                I_OBJPACK-OBJ_NAME = 'SMART'.
**                I_OBJPACK-OBJ_DESCR = 'TEST'.
**                APPEND I_OBJPACK.
**
**                CLEAR I_RECLIST.
***                I_RECLIST-RECEIVER = 'SURENDRAN.HARIPRASAD@GMAIL.COM'.
**                I_RECLIST-RECEIVER = LV_MAIL.
**                I_RECLIST-REC_TYPE = 'U'.
**                APPEND I_RECLIST.
**
**    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
**      EXPORTING
**        document_data              = wa_doc_chng
**        put_in_outbox              = 'X'
**        commit_work                = 'X'
**      TABLES
**        packing_list               = i_objpack
**        object_header              = wa_objhead
**        contents_bin               = i_objbin
**        contents_txt               = i_objtxt
**        receivers                  = i_reclist
**      EXCEPTIONS
**        too_many_receivers         = 1
**        document_not_sent          = 2
**        document_type_not_exist    = 3
**        operation_no_authorization = 4
**        parameter_error            = 5
**        x_error                    = 6
**        enqueue_error              = 7
**        OTHERS                     = 8.
**    IF sy-subrc <> 0.
**      WRITE:/ 'Error When Sending the File', sy-subrc.
**    ELSE.
**      WRITE:/ 'Mail sent'.
**    ENDIF.
**
**
**
**
**
**  ENDLOOP.
**
**
**
**
**
**
**
**  ENDIF.
