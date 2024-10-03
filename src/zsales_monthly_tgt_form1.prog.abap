*----------------------------------------------------------------------*
***INCLUDE ZSALES_MONTHLY_TGT_FORM1.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.

  DATA : TEMP TYPE STRING,
          TEMP_DAT TYPE SY-DATUM.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      REFRESH : IT_MODIFY.
      CLEAR : P_MONT , P_YEAR.
      IF SY-DYNNR = '100'.
        LEAVE PROGRAM.
      ELSE.
        CALL SCREEN '0100'.
      ENDIF.
    WHEN 'CREATE'.
      STA = 'X' .
      IF P_MONT > 12 OR P_MONT <= 0  .
        MESSAGE 'Enter Valid Month' TYPE 'E' DISPLAY LIKE 'E'.
        CALL SCREEN '0100'.
      ENDIF .
      CLEAR :TEMP, TEMP_DAT.
      CONCATENATE P_YEAR P_MONT '01'   INTO TEMP.
      TEMP_DAT = TEMP.
      SELECT PARVW NRART FROM TPAR INTO TABLE IT_TPAR WHERE ( PARVW <> 'ZA' AND PARVW <> 'AM' AND PARVW <> 'BR' AND PARVW <> 'BM' AND PARVW <> 'ZH' )
                                                                 AND NRART EQ 'PE'.


      SELECT PARVW PERNR FROM KNVP INTO TABLE IT_KNVP FOR ALL ENTRIES IN IT_TPAR WHERE PARVW = IT_TPAR-PARVW.

* PARVW = IT_TPAR-PARVW AND
      SELECT PERNR
             ENAME
             ENDDA
             BEGDA  FROM PA0001 INTO TABLE IT_PA001 FOR ALL ENTRIES IN IT_KNVP WHERE PERNR = IT_KNVP-PERNR
                                                                                     AND ENDDA >= SY-DATUM .

"SORT : IT_PA001 BY PERNR .
SORT : IT_PA001 DESCENDING BY PERNR ENDDA.
DELETE ADJACENT DUPLICATES FROM IT_PA001 COMPARING PERNR .
SORT : IT_PA001 BY PERNR.

     SELECT PERNR
            ENDDA
            BEGDA
            STAT2 FROM PA0000 INTO TABLE IT_PA000 FOR ALL ENTRIES IN IT_PA001 WHERE PERNR = IT_PA001-PERNR AND ENDDA >= SY-DATUM
      AND  STAT2 = '3' .

       SORT : IT_PA000 DESCENDING BY PERNR ENDDA.
       DELETE ADJACENT DUPLICATES FROM IT_PA001 COMPARING PERNR .
       SORT : IT_PA000 BY PERNR.
       DELETE IT_PA000 WHERE STAT2 <> '3'.
      " SORT : IT_PA000 DESCENDING BY ENDDA.
     "  DELETE ADJACENT DUPLICATES FROM

*      SELECT EMPLOY_CODE
*             SO_NAME
*             EMP_NAME
*             MON
*             YR
*             CX
*             CY
*             DECO
*             SNC
*             PROJECT
*             CKY
*            FROM ZSALES_MONTH_TGT  INTO TABLE IT_MODIFY
*                  FOR ALL ENTRIES IN IT_PA001
*                  WHERE EMPLOY_CODE = IT_PA001-PERNR.
      LOOP AT IT_PA001 INTO WA_PA001.
*          READ TABLE IT_MODIFY INTO WA_MODIFY WITH KEY EMPLOY_CODE = WA_PA001-PERNR.
*          IF SY-SUBRC <> 0.
      READ TABLE IT_PA000 INTO WA_PA000 WITH KEY PERNR = WA_PA001-PERNR .
      IF SY-SUBRC EQ 0 .
        CLEAR WA_MODIFY.
        WA_MODIFY-EMPLOY_CODE = WA_PA001-PERNR.
        WA_MODIFY-EMP_NAME =  WA_PA001-ENAME.
        WA_MODIFY-ENDDA = WA_PA001-ENDDA .
        WA_MODIFY-BEGDA = WA_PA001-BEGDA .
        WA_MODIFY-STAT2 = WA_PA000-STAT2.
        WA_MODIFY-MON = P_MONT .
        WA_MODIFY-YR = P_YEAR .
        APPEND WA_MODIFY TO IT_MODIFY.
      ENDIF.
      CLEAR : WA_MODIFY , WA_PA001, WA_PA000.
      ENDLOOP.
      REFRESH : IT_PA000 , IT_PA001.

*      SORT : IT_MODIFY DESCENDING BY EMPLOY_CODE EMP_NAME  .
*      DELETE ADJACENT DUPLICATES FROM IT_MODIFY COMPARING EMPLOY_CODE EMP_NAME .
   "   DELETE IT_MODIFY WHERE STAT2 <> '3' .

 CLEAR : SY-UCOMM .
      CALL SCREEN '101'.

    WHEN 'MODIFY'.
     STA = 'Y' .
      IF P_MONT > 12 OR P_MONT <= 0  .
        MESSAGE 'Enter Valid Month' TYPE 'E' DISPLAY LIKE 'E'.
        CALL SCREEN '0100'.
      ENDIF .


      SELECT EMPLOY_CODE
              EMP_NAME
               MON
               YR
               CX
               CY
               DECO
               SNC
               PROJECT
               CKY
              FROM ZSALES_MONTH_TGT  INTO CORRESPONDING FIELDS OF TABLE IT_MODIFY

                    WHERE MON = P_MONT AND
                          YR = P_YEAR.
    CLEAR : SY-UCOMM .

      CALL SCREEN '101'.

    WHEN 'DISPLAY'.
       STA = 'Z' .
      IF P_MONT > 12 OR P_MONT <= 0  .
        MESSAGE 'Enter Valid Month' TYPE 'E' DISPLAY LIKE 'E'.
        CALL SCREEN '0100'.
      ENDIF .

      SELECT EMPLOY_CODE
              EMP_NAME
              MON
              YR
              CX
              CY
              DECO
              SNC
              PROJECT
              CKY
             FROM ZSALES_MONTH_TGT  INTO CORRESPONDING FIELDS OF TABLE IT_MODIFY

                   WHERE MON = P_MONT AND
                         YR = P_YEAR.


      CALL SCREEN '102'.
CLEAR : SY-UCOMM .
    WHEN 'SAVE'.


      CLEAR WA_MODIFY.

"IF STA EQ 'Y' .
      LOOP AT IT_MODIFY INTO WA_MODIFY.

        CLEAR WA_FINAL.

        MOVE-CORRESPONDING WA_MODIFY TO WA_FINAL.
        WA_FINAL-MANDT = SY-MANDT.
        WA_FINAL-CKY = 'INR'.

        MODIFY ZSALES_MONTH_TGT FROM WA_FINAL.
        CLEAR WA_FINAL.

      ENDLOOP.



      IF SY-SUBRC = 0 .

        MESSAGE 'Records are successfully updated.....' TYPE 'S'.

      ENDIF.
"ENDIF.


*IF STA EQ 'X' .
*      LOOP AT IT_MODIFY INTO WA_MODIFY.
*
*        CLEAR WA_FINAL.
*
*        MOVE-CORRESPONDING WA_MODIFY TO WA_FINAL.
*        WA_FINAL-MANDT = SY-MANDT.
*        WA_FINAL-CKY = 'INR'.
*
*        UPDATE ZSALES_MONTH_TGT FROM WA_FINAL.
*        CLEAR WA_FINAL.
*
*      ENDLOOP.
*
*      IF SY-SUBRC = 0 .
*
*        MESSAGE 'Records are successfully updated.....' TYPE 'S'.
*
*      ENDIF.
*ENDIF.


 CLEAR : SY-UCOMM .

 clear : P_MONT , P_YEAR .

 CLEAR : WA_FINAL .
 REFRESH : IT_MODIFY .

  ENDCASE.

CLEAR :STA.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  SET PF-STATUS 'ZSALES_MONTHLY_TGT'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
