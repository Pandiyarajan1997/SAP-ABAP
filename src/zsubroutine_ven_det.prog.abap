*&---------------------------------------------------------------------*
*&  Include           ZSUBROUTINE_VEN_DET
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_QUERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_QUERY .


  IF LV_FLAG = ' '.
    REFRESH GT_FINAL.
    REFRESH GT_LFA1.
    SELECT LIFNR
           NAME1
           ORT01
           KTOKK
           LOEVM
           FROM LFA1 INTO TABLE GT_LFA1 WHERE LIFNR IN  S_LIFNR AND ( KTOKK = 'YB01'  OR KTOKK = 'YBIV' OR KTOKK = 'YBTR'
          OR KTOKK = 'YBVE' OR KTOKK = 'YBVO' ) AND   LOEVM  NE  'X' .
    SORT GT_LFA1 BY LIFNR ASCENDING.


    REFRESH GT_BSIK.
    SELECT   LIFNR
             SHKZG
             DMBTR
             BUDAT
             BLART
             ZFBDT
             ZBD1T
             FROM BSIK INTO TABLE GT_BSIK WHERE  BLART NE 'KA'.
    SORT GT_BSIK BY  LIFNR ZFBDT DESCENDING.


    REFRESH GT_DB_FINAL.
    SELECT  LIFNR
            NAME1
            ORT01
            BASED_LIMIT
            PURCHASE_LIMIT
            AMOUNT
            LV_DAYS1
            ALLOWED_DAYS
            BALANCE
            FROM ZVEN_DEBIT_TAB INTO TABLE GT_DB_FINAL.


    LOOP AT GT_BSIK INTO WA_BSIK.

      AT NEW LIFNR.
        CLEAR LV_AMOUNT.
      ENDAT.

      IF WA_BSIK-SHKZG EQ 'S' .
        WA_BSIK-DMBTR  = - ( WA_BSIK-DMBTR ).
      ENDIF.

      DUE_DATE =   WA_BSIK-ZFBDT.
      NO_DATE = WA_BSIK-ZBD1T .
      LV_AMOUNT = LV_AMOUNT + WA_BSIK-DMBTR.


      AT END OF LIFNR.
        WA_FINAL1-DUE_DATE = DUE_DATE.
        WA_FINAL1-LIFNR = WA_BSIK-LIFNR.
        WA_FINAL1-AMOUNT    =  LV_AMOUNT.

        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = WA_FINAL1-AMOUNT.


        WA_FINAL1-AMOUNT  =   WA_FINAL1-AMOUNT .
        WA_FINAL1-NO_DATE = NO_DATE.
        APPEND WA_FINAL1 TO GT_FINAL1.
        CLEAR WA_FINAL1.
      ENDAT.
    ENDLOOP.


    LOOP AT GT_LFA1 INTO WA_LFA1.
      WA_FINAL2-LIFNR  = WA_LFA1-LIFNR.
      WA_FINAL2-NAME1  = WA_LFA1-NAME1.
      WA_FINAL2-ORT01  = WA_LFA1-ORT01.

      APPEND WA_FINAL2 TO GT_FINAL2.
      CLEAR WA_FINAL2 .

    ENDLOOP.



    LOOP AT  GT_FINAL1 INTO WA_FINAL1.
      LOOP AT GT_DB_FINAL INTO WA_DB_FINAL WHERE LIFNR = WA_FINAL1-LIFNR.
        PUR_LIMIT  = WA_DB_FINAL-PURCHASE_LIMIT - WA_FINAL1-AMOUNT .

        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = PUR_LIMIT.

        WA_FINAL1-BALANCE =  PUR_LIMIT.

        MODIFY   GT_FINAL1 FROM WA_FINAL1    TRANSPORTING PURCHASE_LIMIT  BALANCE .
      ENDLOOP.
    ENDLOOP.




    LOOP AT GT_FINAL2 INTO WA_FINAL2.
      LOOP AT GT_FINAL1 INTO WA_FINAL1 WHERE LIFNR = WA_FINAL2-LIFNR.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = WA_FINAL1-DUE_DATE
            ENDDA = LT_DATE
          IMPORTING
            DAYS  = LV_DAYS.

        WA_FINAL2-AMOUNT =  WA_FINAL1-AMOUNT.  "sri change  10/02/17
*        WA_FINAL2-PURCHASE_LIMIT = WA_FINAL1-PURCHASE_LIMIT.
        WA_FINAL2-LV_DAYS1 =  LV_DAYS - 1 - WA_FINAL1-NO_DATE.
        WA_FINAL2-BALANCE = WA_FINAL1-BALANCE.
      ENDLOOP.

      LOOP AT GT_DB_FINAL INTO WA_DB_FINAL WHERE LIFNR = WA_FINAL2-LIFNR.

        WA_FINAL2-ALLOWED_DAYS = WA_DB_FINAL-ALLOWED_DAYS.
        WA_FINAL2-BASED_LIMIT = WA_DB_FINAL-BASED_LIMIT.
        WA_FINAL2-PURCHASE_LIMIT = WA_DB_FINAL-PURCHASE_LIMIT.
        PUR_LIMIT  = WA_DB_FINAL-PURCHASE_LIMIT.



        IF WA_DB_FINAL-AMOUNT IS INITIAL.   "sri change  10/02/17
          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              VALUE = PUR_LIMIT.

          WA_FINAL2-BALANCE =  PUR_LIMIT.

        ENDIF.


      ENDLOOP.

      MODIFY GT_FINAL2 FROM WA_FINAL2   TRANSPORTING AMOUNT  LV_DAYS1  PURCHASE_LIMIT BALANCE ALLOWED_DAYS BASED_LIMIT.
    ENDLOOP.






    LOOP AT GT_LFA1 INTO WA_LFA1.
      READ TABLE GT_FINAL2 INTO WA_FINAL2 WITH KEY LIFNR = WA_LFA1-LIFNR.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = WA_FINAL2-LV_DAYS1.


      WA_FINAL-LIFNR    =      WA_FINAL2-LIFNR .
      WA_FINAL-NAME1    =      WA_FINAL2-NAME1 .
      WA_FINAL-ORT01    =      WA_FINAL2-ORT01 .
      WA_FINAL-BASED_LIMIT = WA_FINAL2-BASED_LIMIT.
      WA_FINAL-AMOUNT   =      WA_FINAL2-AMOUNT.
      WA_FINAL-PURCHASE_LIMIT =  WA_FINAL2-PURCHASE_LIMIT.
      WA_FINAL-LV_DAYS1   =  WA_FINAL2-LV_DAYS1  .
      WA_FINAL-ALLOWED_DAYS = WA_FINAL2-ALLOWED_DAYS.
      WA_FINAL-BALANCE   =  WA_FINAL2-BALANCE.

      APPEND WA_FINAL TO GT_FINAL.
      CLEAR WA_FINAL.

    ENDLOOP.


    IF GT_FINAL IS NOT INITIAL.
      REFRESH CONTROL 'TAB' FROM SCREEN '5000'.
    ENDIF.




*break-point.
*
*    LOOP AT SCREEN.
*         loop at gt_final into wa_final.
*   if SCREEN-NAME EQ 'WA_FINAL-BASED_LIMIT'.
*       if wa_final-based_limit is not initial.
*
**     if wa_final2-based_limit is not initial.
*      SCREEN-INPUT = 0.
*      MODIFY SCREEN.
*      endif.
*      endif.
*       endloop.
*
*    ENDLOOP.











    LV_FLAG  = 'X'.
  ENDIF.


  IF LV_FLAG1 = ' '.

    LOOP AT GT_FINAL INTO WA_FINAL.

      WA_FINAL3-LIFNR    =      WA_FINAL-LIFNR .
      WA_FINAL3-NAME1    =      WA_FINAL-NAME1 .
      WA_FINAL3-ORT01    =      WA_FINAL-ORT01 .
      WA_FINAL3-BASED_LIMIT = WA_FINAL-BASED_LIMIT.
      WA_FINAL3-AMOUNT   =      WA_FINAL-AMOUNT.
      WA_FINAL3-PURCHASE_LIMIT =  WA_FINAL-PURCHASE_LIMIT.
      WA_FINAL3-LV_DAYS1   =  WA_FINAL-LV_DAYS1  .
      WA_FINAL3-ALLOWED_DAYS = WA_FINAL-ALLOWED_DAYS.
      WA_FINAL3-BALANCE   =  WA_FINAL-BALANCE.

      APPEND WA_FINAL3 TO GT_FINAL3.
      CLEAR WA_FINAL3.

    ENDLOOP.

    LV_FLAG1 = 'X'.
  ENDIF.


ENDFORM.                    " SELECT_QUERY
