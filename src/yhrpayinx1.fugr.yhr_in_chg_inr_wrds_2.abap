FUNCTION YHR_IN_CHG_INR_WRDS_2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(AMT_IN_NUM) TYPE  PC207-BETRG
*"  EXPORTING
*"     REFERENCE(AMT_IN_WORDS) TYPE  C
*"  EXCEPTIONS
*"      DATA_TYPE_MISMATCH
*"----------------------------------------------------------------------

  DATA: MAXNO TYPE P.
  MAXNO = 10 ** 9.
  IF ( AMT_IN_NUM >= MAXNO ).
    RAISE DATA_TYPE_MISMATCH.
  ENDIF.
*data declaration-------------------------------------------------*
  DATA: TEN(10),SINGLE(6),FINAL(130),DEC(20),RES TYPE I,RP(7).
  DATA: A1 TYPE I,A2 TYPE I,STR(20),D TYPE P,M TYPE I,WRDREP(20).
  DATA: CNTR TYPE I,F1 TYPE I,F2 TYPE I,F3 TYPE I,F4 TYPE I,F5 TYPE I.
  DATA: F6 TYPE I,F7 TYPE I,F8 TYPE I,F9 TYPE I.

  D = ( AMT_IN_NUM * 100 ) DIV 100.
  RES = ( AMT_IN_NUM * 100 ) MOD 100.

  F1 = RES DIV 10.
  F2 = RES MOD 10.
  PERFORM SETNUM1 USING F1 F2 CHANGING WRDREP.
  F1 = 0. F2 = 0.
  DEC = WRDREP.
  CNTR = 1.
*Go in a loop dividing the numbers by 10 and store the
*residues as a digit in f1 .... f9
  WHILE ( D > 0 ).
    M = D MOD 10.
    D = D DIV 10.
    CASE CNTR.
      WHEN 1. F1 = M.
      WHEN 2. F2 = M.
      WHEN 3. F3 = M.
      WHEN 4. F4 = M.
      WHEN 5. F5 = M.
      WHEN 6. F6 = M.
      WHEN 7. F7 = M.
      WHEN 8. F8 = M.
      WHEN 9. F9 = M.
    ENDCASE.
    CNTR = CNTR + 1.
  ENDWHILE.
  CNTR = CNTR - 1.
*Going in loop and sending pair of digits to function setnum to get
*the standing value of digits in words
  WHILE ( CNTR > 0 ).

    IF ( CNTR <= 2 ).
      PERFORM SETNUM1 USING F2 F1 CHANGING WRDREP.
      CONCATENATE FINAL WRDREP INTO FINAL SEPARATED BY ' '.
    ELSEIF ( CNTR = 3 ).
      IF ( F3 <> 0 ).
        PERFORM SETNUM1 USING 0 F3 CHANGING WRDREP.
        CONCATENATE FINAL WRDREP 'HUNDRED' INTO FINAL SEPARATED BY ' '.
      ENDIF.
    ELSEIF ( CNTR <= 5 ).
      IF ( F5 <> 0 ) OR ( F4 <> 0 ).
        PERFORM SETNUM1 USING F5 F4 CHANGING WRDREP.
        CONCATENATE FINAL WRDREP 'THOUSAND' INTO FINAL SEPARATED BY ' '
  .
      ENDIF.
      IF ( CNTR = 4 ).
        CNTR = 5.
      ENDIF.
    ELSEIF ( CNTR <= 7 ).
      IF ( F7 <> 0 ) OR ( F6 <> 0 ).
        PERFORM SETNUM1 USING F7 F6 CHANGING WRDREP.
        CONCATENATE FINAL WRDREP 'LAKH' INTO FINAL SEPARATED BY ' ' .
      ENDIF.
    ELSEIF ( CNTR <= 9 ).
      PERFORM SETNUM1 USING F9 F8 CHANGING WRDREP.
      CONCATENATE FINAL WRDREP 'CRORE' INTO FINAL SEPARATED BY ' ' .
    ENDIF.

    CNTR = CNTR - 2.
  ENDWHILE.
*Output the final
  IF ( FINAL = ' ONE' ).RP = ''(003).ELSE. RP = ''(001).ENDIF
.
  IF ( FINAL = ' ONE' ).RP = 'RUPEE'(003)."ELSE. RP = 'RUPEES'(001).
  ENDIF . " Modified By Govind 20/03/2014
  IF ( FINAL = '' ) AND ( DEC = '' ).
    FINAL = 'NIL'.
  ELSEIF ( FINAL = '' ).
    CONCATENATE DEC 'PAISE ONLY'(002) INTO FINAL SEPARATED BY ' ' .
  ELSEIF ( DEC = '' ).

    CONCATENATE FINAL RP INTO FINAL SEPARATED BY ' ' .
  ELSE.
    CONCATENATE FINAL RP DEC 'PAISE ONLY'(002) INTO FINAL SEPARATED BY ' ' .
  ENDIF.

  AMT_IN_WORDS = FINAL.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  SETNUM
*&---------------------------------------------------------------------*
*       converts a number into words                                   *
*----------------------------------------------------------------------*
*  -->  a1,a2     two digits for 2nd and 1st place
*  <--  str       outpur in words
*----------------------------------------------------------------------*
DATA: TEN1(10),SINGLE1(6),STR1(20).
*
FORM SETNUM1 USING A1 A2 CHANGING STR1.
  TEN1 = '' . SINGLE1 = ''.
  IF ( A1 = 1 ).

    CASE A2.
      WHEN 0. TEN1 = 'TEN'.
      WHEN 1. TEN1 = 'ELEVEN'.
      WHEN 2. TEN1 = 'TWELVE'.
      WHEN 3. TEN1 = 'THIRTEEN'.
      WHEN 4. TEN1 = 'FOURTEEN'.
      WHEN 5. TEN1 = 'FIFTEEN'.
      WHEN 6. TEN1 = 'SIXTEEN'.
      WHEN 7. TEN1 = 'SEVENTEEN'.
      WHEN 8. TEN1 = 'EIGHTEEN'.
      WHEN 9. TEN1 = 'NINETEEN'.
    ENDCASE.
  ELSE.

    CASE A2.
      WHEN 1. SINGLE1 = 'ONE'.
      WHEN 2. SINGLE1 = 'TWO'.
      WHEN 3. SINGLE1 = 'THREE'.
      WHEN 4. SINGLE1 = 'FOUR'.
      WHEN 5. SINGLE1 = 'FIVE'.
      WHEN 6. SINGLE1 = 'SIX'.
      WHEN 7. SINGLE1 = 'SEVEN'.
      WHEN 8. SINGLE1 = 'EIGHT'.
      WHEN 9. SINGLE1 = 'NINE'.
    ENDCASE.
    CASE A1.
      WHEN 2. TEN1 = 'TWENTY'.
      WHEN 3. TEN1 = 'THIRTY'.
      WHEN 4. TEN1 = 'FORTY'.
      WHEN 5. TEN1 = 'FIFTY'.
      WHEN 6. TEN1 = 'SIXTY'.
      WHEN 7. TEN1 = 'SEVENTY'.
      WHEN 8. TEN1 = 'EIGHTY'.
      WHEN 9. TEN1 = 'NINETY'.
    ENDCASE.

  ENDIF.
  IF ( SINGLE1 <> '' ) AND ( TEN1 <> '' ).
    CONCATENATE TEN1 SINGLE1 INTO STR1 SEPARATED BY ' '.
  ELSEIF SINGLE1 = ''.
    STR1 = TEN1.
  ELSE.
    STR1 = SINGLE1.
  ENDIF.

ENDFORM.                               " SETNUM
