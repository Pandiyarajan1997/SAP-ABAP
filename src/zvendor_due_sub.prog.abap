*&---------------------------------------------------------------------*
*&  Include           ZCUSTOMER_DUE_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_FETCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_FETCH .
  DATA : LV_DIFF(10) TYPE C.
  DATA : LV_NDUEAMT  TYPE P DECIMALS 2,
         T_NDUEAMT TYPE P DECIMALS 2,
         LV_ZFAEDT TYPE SY-DATUM.
  DATA:  CUST_DATE TYPE BSIK-BUDAT,
         BASEL_DAY TYPE SY-DATUM.
**  DATA: DMBTR_STRNG TYPE STRING,
**        DMBTR_MINUS TYPE C VALUE '-'.

  DATA: I_COSTCENTER_LIST LIKE BAPI0012_2 OCCURS 0 WITH HEADER LINE,

        I_RETURN      LIKE BAPIRETURN.


  CLEAR GIT_LFA1.
  SELECT LIFNR NAME1 KTOKK SORTL NAME4 FROM LFA1
         INTO CORRESPONDING FIELDS OF TABLE GIT_LFA1
         WHERE LIFNR IN DD_LIFNR
         AND KTOKK IN DD_KTOKK
         AND SORTL IN DD_SORTL
         AND NAME4 IN DD_VTYPE.

*****************    prabhu created on 02.04.2020 starts***********************
  CLEAR GIT_LFA11.
  SELECT LIFNR NAME1 KTOKK SORTL NAME4 FROM LFA1
         INTO CORRESPONDING FIELDS OF TABLE GIT_LFA11
         WHERE LIFNR IN DD_LIFNR
         AND KTOKK IN DD_KTOKK
         AND SORTL IN DD_SORTL
         AND NAME4 IN DD_VTYPE.
*****************    prabhu created on 02.04.2020 Ends***********************

  SUBMIT ZRFKOPR10
         WITH DD_KTOKK IN DD_KTOKK
         WITH DD_LIFNR IN DD_LIFNR
         WITH DD_BUKRS IN DD_BUKRS
         WITH DD_GJAHR IN DD_GJAHR
         WITH DD_SORTL IN DD_SORTL
         WITH DD_VTYPE IN DD_VTYPE
         WITH BUDAT    IN BUDAT
         WITH NETDT    IN NETDT
**         WITH DD_STIDA  EQ DD_STIDA  "IN DD_DTIDA
         WITH DD_STIDA EQ DD_STIDA
         AND RETURN.
*  BREAK-POINT.
  IMPORT GIT_HBSIK[] FROM MEMORY ID 'BSIK'.

  IF SY-SUBRC = 0.
    CLEAR GIT_FAGLFLEXA.
    SELECT RYEAR DOCNR RBUKRS DOCLN PRCTR FROM FAGLFLEXA
           INTO CORRESPONDING FIELDS OF TABLE GIT_FAGLFLEXA
           FOR ALL ENTRIES IN GIT_HBSIK
           WHERE RYEAR  = GIT_HBSIK-GJAHR
             AND DOCNR  = GIT_HBSIK-BELNR
             AND RBUKRS = GIT_HBSIK-BUKRS
             AND PRCTR IN PROFIT.
*         AND DOCLN  = IT_POS-BUZEI.



    FREE MEMORY ID 'BSIK'.
  ENDIF.

**************************************createed on 15.07.2020 starts **********************

  IF SY-SUBRC = 0 .
    SELECT BUKRS BELNR GJAHR  KOSTL LIFNR FROM BSEG
      INTO TABLE GIT_FINAL2
     FOR ALL ENTRIES IN GIT_HBSIK
     WHERE  BUKRS = GIT_HBSIK-BUKRS
***     and  BUZEI = GIT_HBSIK-BUZEI "'"commented on 17.07.2020
     AND   BELNR = GIT_HBSIK-BELNR
     AND   GJAHR = GIT_HBSIK-GJAHR.

  ENDIF.

*
**************************************createed on 15.07.2020 Ends **********************

*******************************************created by Prabhu on 02.04.2020 starts ***************************
***  CLEAR GIT_FINAL1.

***
***  SUBMIT ZRFKOPR10
***       WITH DD_KTOKK IN DD_KTOKK
***       WITH DD_LIFNR IN DD_LIFNR
***       WITH DD_BUKRS IN DD_BUKRS
***       WITH DD_GJAHR IN DD_GJAHR
***       WITH DD_SORTL IN DD_SORTL
***       WITH DD_VTYPE IN DD_VTYPE
***       WITH BUDAT    IN BUDAT
***       WITH NETDT    IN NETDT
***       WITH DD_STIDA  EQ DD_STIDA  "IN DD_DTIDA
***       AND RETURN.
***  IMPORT GIT_HBSIK[] FROM MEMORY ID 'BSAK'.
***
***

  SELECT BUKRS LIFNR AUGDT  BELNR  XBLNR BUZEI BUDAT BLDAT WAERS GJAHR UMSKZ BLART SGTXT ZBD1T ZBD3T DMBTR SHKZG ZFBDT KOSTL FROM BSAK
         INTO CORRESPONDING FIELDS OF TABLE GIT_FINAL1
        WHERE LIFNR IN DD_LIFNR
        AND   BUKRS IN DD_BUKRS
        AND  BUDAT <= DD_STIDA
***        AND   SHKZG = 'H'\
        AND UMSKZ <> 'F'
***        AND   WAERS = 'INR' commented on 30.04.2020
        AND   AUGDT > DD_STIDA .


***        FOR ALL ENTRIES IN GIT_HBSIK
***        WHERE LIFNR = GIT_HBSIK-LIFNR
***        AND   BUKRS  = GIT_HBSIK-BUKRS
***        AND   SHKZG = 'H'
***        AND   WAERS = 'INR'.

*************************************createed on 15.07.2020 starts **********************
  IF SY-SUBRC = 0 AND GIT_FINAL1 IS NOT INITIAL.

    SELECT BUKRS BELNR GJAHR  KOSTL LIFNR FROM BSEG
     INTO  TABLE GIT_FINAL3
    FOR ALL ENTRIES IN GIT_FINAL1
    WHERE  BUKRS = GIT_FINAL1-BUKRS
    AND   BELNR = GIT_FINAL1-BELNR
***    and  BUZEI = GIT_FINAL1-BUZEI "'"commented on 17.07.2020
    AND   GJAHR = GIT_FINAL1-GJAHR.

  ENDIF.

*************************************createed on 15.07.2020 Ends **********************

  SELECT RYEAR DOCNR RBUKRS DOCLN PRCTR FROM FAGLFLEXA
         INTO CORRESPONDING FIELDS OF TABLE GIT_FAGLFLEXA1
         FOR ALL ENTRIES IN GIT_FINAL1
         WHERE RYEAR  = GIT_FINAL1-GJAHR
           AND DOCNR  = GIT_FINAL1-BELNR
           AND RBUKRS = GIT_FINAL1-BUKRS
           AND PRCTR IN PROFIT.



*******************************************created by Prabhu on 02.04.2020 starts ***************************
***  SHIFT RASTBIS1 LEFT DELETING LEADING '0'.
***  SHIFT RASTBIS2 LEFT DELETING LEADING '0'.
***  SHIFT RASTBIS3 LEFT DELETING LEADING '0'.
***  SHIFT RASTBIS4 LEFT DELETING LEADING '0'.
***  SHIFT RASTBIS5 LEFT DELETING LEADING '0'.

*************Created By Prabhu on 31.03.2020 Strats*************

  SHIFT RASTBIS1 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS2 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS3 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS4 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS5 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS6 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS7 LEFT DELETING LEADING '0'.
  SHIFT RASTBIS8 LEFT DELETING LEADING '0'.

*************Created By Prabhu on 31.03.2020 Ends*************

  CLEAR GIT_FINAL.
*  BREAK-POINT.
***  LOOP AT GIT_HBSIK INTO GS_HBSIK WHERE PRCTR = GS_FAGLFLEXA-PRCTR .
***  append lines of GIT_FINAL1 TO git_hbsik.
  LOOP AT GIT_HBSIK INTO GS_HBSIK.
    MOVE-CORRESPONDING GS_HBSIK TO GS_FINAL  .
*    BREAK-POINT.
    LV_ZFAEDT = GS_HBSIK-BLDAT + GS_HBSIK-ZBD1T.
*********************************Created on 15.05.2020 starts
    LV_DIFF = DD_STIDA - GS_FINAL-NETDT.
*********************************Created on 15.05.2020 ends
*    GS_FINAL-KOSTL = GS_FINAL-KOSTL.  "Created on 27.06.2020 starts

*********************************Created on 17.07.2020 starts
    IF GS_FINAL-KOSTL IS INITIAL .

      LOOP AT GIT_FINAL2 INTO GS_FINAL2 WHERE BELNR = GS_FINAL-BELNR    .

        IF GS_FINAL2-KOSTL IS NOT INITIAL.
          GS_FINAL-KOSTL = GS_FINAL2-KOSTL.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
********************************Created on 17.07.2020 starts

*    IF  LV_ZFAEDT >=  DD_STIDA  OR LV_DIFF = 0   AND  GS_FINAL-UMSKZ NE 'A'." commented on 05.08.2020

    IF  LV_DIFF <= 0  AND  GS_FINAL-UMSKZ NE 'A'.
      GS_FINAL-ZNDUE_AMT = GS_HBSIK-DMSHB.  "changed on 26.05.2020  GS_FINAL-ZNDUE_AMT = GS_HBSIK-DMBTR.

    ENDIF.

*  IF LV_ZFAEDT LE  DD_STIDA   .
*      IF GS_HBSIK-SHKZG = 'S'.
*          LV_NDUEAMT = LV_NDUEAMT + GS_HBSIK-DMBTR.
*          T_NDUEAMT =  GS_HBSIK-DMBTR.
*        ELSEIF GS_HBSIK-SHKZG = 'H'.
*          LV_NDUEAMT = LV_NDUEAMT - GS_HBSIK-DMBTR.
*          GS_FINAL-ZNDUE_AMT = T_NDUEAMT - GS_HBSIK-DMBTR.
*        ENDIF.
*    ENDIF.
*     GS_FINAL-ZNDUE_AMT = T_NDUEAMT.
    IF GS_FINAL-UMSKZ NE 'A'.
      GS_FINAL-UTAGE =  DD_STIDA - GS_FINAL-NETDT. "GS_FINAL-BUDAT.
    ELSE.
      GS_FINAL-UTAGE =  0.
    ENDIF.

***    LV_DIFF = SY-DATUM - GS_FINAL-NETDT.
***    LV_DIFF = DD_STIDA - GS_FINAL-NETDT. "commented on 15.05.2020

***    CONDENSE LV_DIFF.
***    IF ( LV_DIFF >= 0 AND
***         LV_DIFF <= RASTBIS1 ).
***      GS_FINAL-SLAB1 = GS_FINAL-DMSHB.
***    ELSEIF ( LV_DIFF > RASTBIS1 AND
***             LV_DIFF <= RASTBIS2 ).
***      GS_FINAL-SLAB2 = GS_FINAL-DMSHB.
***    ELSEIF ( LV_DIFF > RASTBIS2 AND
***             LV_DIFF <= RASTBIS3 ).
***      GS_FINAL-SLAB3 = GS_FINAL-DMSHB.
***    ELSEIF ( LV_DIFF > RASTBIS3 AND
***             LV_DIFF <= RASTBIS4 ).
***      GS_FINAL-SLAB4 = GS_FINAL-DMSHB.
***    ELSEIF ( LV_DIFF > RASTBIS4 AND
***             LV_DIFF <= RASTBIS5 ).
***      GS_FINAL-SLAB5 = GS_FINAL-DMSHB.
***    ELSEIF LV_DIFF > RASTBIS5.
***      GS_FINAL-SLAB6 = GS_FINAL-DMSHB.
***    ELSEIF LV_DIFF < 0.
***      GS_FINAL-DUE = GS_FINAL-DMSHB.
***    ENDIF.

********************************************* created by prabhu On 31.03.2020 starts  *********************************************

    CONDENSE LV_DIFF.

    IF GS_FINAL-ZNDUE_AMT IS INITIAL.



***    IF ( LV_DIFF >= 0 AND LV_DIFF <= RASTBIS1 AND GS_FINAL-UMSKZ NE 'A' ).  commented on 14.05.2020
***
***      GS_FINAL-SLAB1 = GS_FINAL-DMSHB.

      IF ( LV_DIFF >= RASTBIS1 AND LV_DIFF <= RASTBIS2 AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB2 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF > RASTBIS2 AND LV_DIFF <= RASTBIS3 AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB3 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF > RASTBIS3 AND LV_DIFF <= RASTBIS4  AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB4 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF > RASTBIS4 AND LV_DIFF <= RASTBIS5 AND GS_FINAL-UMSKZ NE 'A').

        GS_FINAL-SLAB5 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF >  RASTBIS5 AND LV_DIFF <= RASTBIS6 AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB6 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF >  RASTBIS6 AND LV_DIFF <= RASTBIS7 AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB7 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF >  RASTBIS7 AND LV_DIFF <= RASTBIS8 AND GS_FINAL-UMSKZ NE 'A' ).

        GS_FINAL-SLAB8 = GS_FINAL-DMSHB.

      ELSEIF ( LV_DIFF > RASTBIS8 AND GS_FINAL-UMSKZ NE 'A' ).    " LV_DIFF > RASTBIS8.

        GS_FINAL-SLAB9 = GS_FINAL-DMSHB.
**
**    ELSEIF LV_DIFF < 0.
**
***      GS_FINAL-DUE = GS_FINAL-DMSHB. "commented on 05.08.2020
**       GS_FINAL-ZNDUE_AMT = GS_FINAL-DMSHB.

      ENDIF.
    ENDIF.

    IF SY-SUBRC = 0.
      IF GS_FINAL-UMSKZ = 'A'.           "Created on 31.03.2020
        GS_FINAL-ADVNC_PAY = GS_FINAL-DMSHB.
      ENDIF.
    ENDIF.

*********************************************    created by prabhu 31.03.2020  Ends  *********************************************


    READ TABLE GIT_LFA1 INTO GS_LFA1
                        WITH KEY LIFNR = GS_HBSIK-LIFNR  .
    IF SY-SUBRC = 0.
      GS_FINAL-NAME1 = GS_LFA1-NAME1.
    ENDIF.

    GS_FINAL-SORTL = GS_LFA1-SORTL.
    GS_FINAL-VTYPE = GS_LFA1-NAME4.

    CLEAR LV_POS.
    LV_POS = GS_HBSIK-BUZEI.
    CLEAR GS_FAGLFLEXA.
    READ TABLE GIT_FAGLFLEXA INTO GS_FAGLFLEXA
                             WITH KEY RYEAR = GS_HBSIK-GJAHR
                                      DOCNR = GS_HBSIK-BELNR
                                      RBUKRS = GS_HBSIK-BUKRS
                                      DOCLN  = LV_POS.
    IF SY-SUBRC = 0.
      GS_FINAL-PROFIT = GS_FAGLFLEXA-PRCTR.
      SHIFT GS_FINAL-PROFIT LEFT DELETING LEADING '0'.
      APPEND GS_FINAL TO GIT_FINAL.
      CLEAR : GS_FINAL, GS_HBSIK.
    ELSEIF GS_HBSIK-BELNR = '5100004502'.
      SHIFT GS_FINAL-PROFIT LEFT DELETING LEADING '0'.
      APPEND GS_FINAL TO GIT_FINAL.
      CLEAR : GS_FINAL, GS_HBSIK.
    ENDIF.
    CLEAR : GS_HBSIK,GS_FINAL.
  ENDLOOP.
*********************************************    created by prabhu 31.03.2020 Starts  *********************************************
  DELETE  GIT_FINAL WHERE  BUDAT > DD_STIDA ." and AUGDT > DD_STIDA ).  ">= DD_STIDA.   DELETE  GIT_FINAL WHERE and AUGDT = ''
*********************************************    created by prabhu 31.03.2020  Ends  *********************************************

*  LOOP AT GIT_FINAL INTO GS_FINAL .
*********************************************    created by prabhu 1.04.2020 Starts  *********************************************


  LOOP AT GIT_FINAL1 INTO GS_FINAL1  .

    GS_FINAL1-BELNR = GS_FINAL1-BELNR.
***    GS_FINAL1-KOSTL = GS_FINAL1-KOSTL. "created on 27.06.2020

********commented on 17.07.2020 starts **********
    IF GS_FINAL1-KOSTL IS INITIAL .

      LOOP AT GIT_FINAL3 INTO GS_FINAL3 WHERE BELNR = GS_FINAL1-BELNR.
        IF GS_FINAL3-KOSTL IS NOT INITIAL.
          GS_FINAL1-KOSTL = GS_FINAL3-KOSTL.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDIF.
********commented on 17.07.2020 ends **********

***
***    LV_ZFAEDT = GS_FINAL1-BLDAT + GS_FINAL1-ZBD1T. "uncomented on 14.05.2020
***
***    IF LV_ZFAEDT =>  DD_STIDA and GS_FINAL1-UMSKZ NE 'A'  .
***
***      GS_FINAL1-ZNDUE_AMT = GS_FINAL1-DMBTR.
***
***    ENDIF.                                        "uncomented on 14.05.2020
***************************** baseline date change on 28.05.2020 starts****************************
    CLEAR BASEL_DAY.

    IF GS_FINAL1-ZBD3T <> 0.
      BASEL_DAY = GS_FINAL1-ZFBDT + GS_FINAL1-ZBD3T.
    ELSE.
      BASEL_DAY = GS_FINAL1-ZFBDT + GS_FINAL1-ZBD1T.
    ENDIF.


    IF GS_FINAL1-BELNR = '5100002604' OR GS_FINAL1-BELNR = '5100000029' OR GS_FINAL1-BELNR = '5100003238'  .
      BASEL_DAY = GS_FINAL1-ZFBDT.
    ENDIF.

***************************** baseline date change on 28.05.2020*********************************

    IF GS_FINAL1-UMSKZ NE 'A'.

**      GS_FINAL1-UTAGE =  DD_STIDA - GS_FINAL1-NETDT.
      GS_FINAL1-UTAGE =  DD_STIDA - BASEL_DAY.

    ELSE.
      GS_FINAL1-UTAGE =  0.
    ENDIF.

**    LV_DIFF = DD_STIDA - GS_FINAL-NETDT. "commented on 29.04.2020 orginal
    GS_FINAL1-NETDT = GS_FINAL1-BUDAT.
***************************new arrear  of dates cal on 29.04.2020 starts
    IF DD_STIDA <> SY-DATUM .

      LV_DIFF = DD_STIDA - GS_FINAL1-NETDT.

    ELSE.
      LV_DIFF = SY-DATUM - GS_FINAL1-NETDT.

    ENDIF.

***************************new arrear  of dates cal on 29.04.2020 ends

    GS_FINAL1-DMSHB = GS_FINAL1-DMBTR . "added on 30.04.2020

    IF  GS_FINAL1-SHKZG = 'H'.
      GS_FINAL1-DMSHB = GS_FINAL1-DMBTR * -1. " commented on 30.04.2020  -1

    ENDIF.

    READ TABLE GIT_LFA11 INTO GS_LFA11 WITH KEY LIFNR = GS_FINAL1-LIFNR  .

    IF SY-SUBRC = 0.
      GS_FINAL1-NAME1 = GS_LFA11-NAME1.
    ENDIF.

    GS_FINAL1-SORTL = GS_LFA11-SORTL.
    GS_FINAL1-VTYPE = GS_LFA11-NAME4.

****    LV_DIFF = SY-DATUM - GS_FINAL-NETDT."commented on 29.04.2020 orginal
***    LV_DIFF = SY-DATUM - GS_FINAL1-NETDT. "commented on 29.04.2020

    CLEAR LV_POS.

    LV_POS = GS_FINAL1-BUZEI.
    CLEAR GS_FAGLFLEXA1.
    READ TABLE GIT_FAGLFLEXA1 INTO GS_FAGLFLEXA1 WITH KEY RYEAR = GS_FINAL1-GJAHR
                                      DOCNR = GS_FINAL1-BELNR
                                      RBUKRS = GS_FINAL1-BUKRS
                                      DOCLN  = LV_POS.

    IF  SY-SUBRC = 0.
      GS_FINAL1-PROFIT = GS_FAGLFLEXA1-PRCTR.
      SHIFT GS_FINAL1-PROFIT LEFT DELETING LEADING '0'.

    ENDIF.

    IF GS_FINAL1-UTAGE <= 0 AND GS_FINAL1-UMSKZ NE 'A'.

      GS_FINAL1-ZNDUE_AMT = GS_FINAL1-DMSHB. "GS_FINAL1-DMBTR. changed on 28.05.2020

    ENDIF.

*    IF ( GS_FINAL1-UTAGE >= 0 AND GS_FINAL1-UTAGE <= RASTBIS1 AND GS_FINAL1-UMSKZ NE 'A' ). " insted of lv_diff i have put GS_FINAL1-UTAGE on 14.05.2020 for all the slabs
*
*      GS_FINAL1-SLAB1 = GS_FINAL1-DMSHB.

    IF GS_FINAL1-ZNDUE_AMT IS INITIAL.



      IF ( GS_FINAL1-UTAGE >= RASTBIS1 AND GS_FINAL1-UTAGE <= RASTBIS2 AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB2 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE > RASTBIS2 AND GS_FINAL1-UTAGE <= RASTBIS3 AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB3 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE > RASTBIS3 AND GS_FINAL1-UTAGE <= RASTBIS4  AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB4 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE > RASTBIS4 AND GS_FINAL1-UTAGE <= RASTBIS5 AND GS_FINAL1-UMSKZ NE 'A').

        GS_FINAL1-SLAB5 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE >  RASTBIS5 AND GS_FINAL1-UTAGE <= RASTBIS6 AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB6 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE >  RASTBIS6 AND GS_FINAL1-UTAGE <= RASTBIS7 AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB7 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE >  RASTBIS7 AND GS_FINAL1-UTAGE <= RASTBIS8 AND GS_FINAL1-UMSKZ NE 'A' ).

        GS_FINAL1-SLAB8 = GS_FINAL1-DMSHB.

      ELSEIF ( GS_FINAL1-UTAGE > RASTBIS8 AND GS_FINAL1-UMSKZ NE 'A' ).    " LV_DIFF > RASTBIS8.

        GS_FINAL1-SLAB9 = GS_FINAL1-DMSHB.

**    ELSEIF GS_FINAL1-UTAGE <= 0 AND GS_FINAL1-UMSKZ NE 'A'.
**
*****      GS_FINAL1-DUE = GS_FINAL1-DMSHB.     "commented on 14.05.2020
**
**      GS_FINAL1-ZNDUE_AMT = GS_FINAL1-DMSHB. "GS_FINAL1-DMBTR. changed on 28.05.2020

      ENDIF.
    ENDIF.

    IF SY-SUBRC = 0.
      IF GS_FINAL1-UMSKZ = 'A'.           "Created on 31.03.2020
        GS_FINAL1-ADVNC_PAY = GS_FINAL1-DMSHB.
      ENDIF.
    ENDIF.

    APPEND  GS_FINAL1 TO GIT_FINAL.
    CLEAR :GS_FINAL1.

  ENDLOOP.

**DELETE  GIT_FINAL1 WHERE  AUGDT = DD_STIDA .


***  APPEND LINES OF GIT_FINAL1 TO GIT_FINAL.
*********************************************    created by prabhu 1.04.2020  Ends  *********************************************

ENDFORM.                    " DATA_FETCH
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_TOP_OF_PAGE   = 'ALV_CATALOG_HEADER'
*     i_callback_pf_status_set = 'ZGOODS'
*     i_callback_user_command  = 'GC_USER_COMMAND'
*     i_background_id          = 'ALV_BACKGROUND'
      IT_FIELDCAT              = GIT_FIELDCAT
      I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IT_SORT                  = IT_SORT             "created by prabhu on01.04.2020
    TABLES
      T_OUTTAB                 = GIT_FINAL
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT .
***  DATA : LV_SLAB1(50) TYPE C,
***         LV_SLAB2(50) TYPE C,
***         LV_SLAB3(50) TYPE C,
***         LV_SLAB4(50) TYPE C,
***         LV_SLAB5(50) TYPE C,
***         LV_SLAB6(50) TYPE C.

*********************************************    created by prabhu on 31.03.2020 starts  *********************************************
  DATA : LV_SLAB1(50) TYPE C,
         LV_SLAB2(50) TYPE C,
         LV_SLAB3(50) TYPE C,
         LV_SLAB4(50) TYPE C,
         LV_SLAB5(50) TYPE C,
         LV_SLAB6(50) TYPE C,
         LV_SLAB7(50) TYPE C,
         LV_SLAB8(50) TYPE C,
         LV_SLAB9(50) TYPE C,
         LV_ADVNC_PAY(50) TYPE C.
*********************************************    created by prabhu on 31.03.2020 End  *********************************************

  DATA : LV_T1(1) TYPE C.

  CLEAR LV_SLAB1.

  IF RASTBIS1 IS INITIAL OR RASTBIS1 EQ ''.

    LV_T1 = '0'.

    CONCATENATE 'From' '0' 'To' LV_T1 INTO LV_SLAB1 SEPARATED BY SPACE.
  ELSE.
    SHIFT RASTBIS1 LEFT DELETING LEADING '0'.
    CONCATENATE 'From' '0' 'To' RASTBIS1 INTO LV_SLAB1 SEPARATED BY SPACE.
  ENDIF.


***  CLEAR : LV_SLAB2,
***          LV_SLAB3,
***          LV_SLAB4,
***          LV_SLAB5,
***          LV_SLAB6.
***
***
***  RASTBIS1 = RASTBIS1 + 1.
***  SHIFT RASTBIS1 LEFT DELETING LEADING '0'.
***  CONCATENATE 'From' RASTBIS1 'To' RASTBIS2 INTO LV_SLAB2 SEPARATED BY SPACE.
***
***  RASTBIS2 = RASTBIS2 + 1.
***  SHIFT RASTBIS2 LEFT DELETING LEADING '0'.
***  CONCATENATE 'From' RASTBIS2 'To' RASTBIS3 INTO LV_SLAB3 SEPARATED BY SPACE.
***
***  RASTBIS3 = RASTBIS3 + 1.
***  SHIFT RASTBIS3 LEFT DELETING LEADING '0'.
***  CONCATENATE 'From' RASTBIS3 'To' RASTBIS4 INTO LV_SLAB4 SEPARATED BY SPACE.
***
***  RASTBIS4 = RASTBIS4 + 1.
***  SHIFT RASTBIS4 LEFT DELETING LEADING '0'.
***  CONCATENATE 'From' RASTBIS4 'To' RASTBIS5 INTO LV_SLAB5 SEPARATED BY SPACE.
***
***  SHIFT RASTBIS5 LEFT DELETING LEADING '0'.
***  CONCATENATE 'Greater than' RASTBIS5 INTO LV_SLAB6 SEPARATED BY SPACE.

*********************************************    created by prabhu on 31.03.2020 starts  *********************************************
  CLEAR : LV_SLAB2,LV_SLAB3,LV_SLAB4,LV_SLAB5,LV_SLAB6,LV_SLAB7,LV_SLAB8,LV_SLAB9.

**  RASTBIS1 = RASTBIS1 + 1. commented on 29.04.2020

  SHIFT RASTBIS1 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS1 'To' RASTBIS2 INTO LV_SLAB2 SEPARATED BY SPACE.

  RASTBIS2 = RASTBIS2 + 1.
  SHIFT RASTBIS2 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS2 'To' RASTBIS3 INTO LV_SLAB3 SEPARATED BY SPACE.

  RASTBIS3 = RASTBIS3 + 1.
  SHIFT RASTBIS3 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS3 'To' RASTBIS4 INTO LV_SLAB4 SEPARATED BY SPACE.

  RASTBIS4 = RASTBIS4 + 1.
  SHIFT RASTBIS4 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS4 'To' RASTBIS5 INTO LV_SLAB5 SEPARATED BY SPACE.

  RASTBIS5 = RASTBIS5 + 1.
  SHIFT RASTBIS5 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS5 'To' RASTBIS6 INTO LV_SLAB6 SEPARATED BY SPACE.

  RASTBIS6 = RASTBIS6 + 1.
  SHIFT RASTBIS6 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS6 'To' RASTBIS7 INTO LV_SLAB7 SEPARATED BY SPACE.

  RASTBIS7 = RASTBIS7 + 1.
  SHIFT RASTBIS7 LEFT DELETING LEADING '0'.
  CONCATENATE 'From' RASTBIS7 'To' RASTBIS8 INTO LV_SLAB8 SEPARATED BY SPACE.


  SHIFT RASTBIS8 LEFT DELETING LEADING '0'.
  CONCATENATE 'Greater than' RASTBIS8 INTO LV_SLAB9 SEPARATED BY SPACE.

***  IF sysbrc = 0.
***
***    IF  .
***
***    ENDIF.
***  ENDIF.

*********************************************    created by prabhu on 31.03.2020 End  *********************************************

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'LIFNR'.
  GS_FIELDCAT-SELTEXT_L = 'Vendor No.'.
  GS_FIELDCAT-SELTEXT_M = 'Vendor No.'.
  GS_FIELDCAT-SELTEXT_S = 'Vendor No.'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'NAME1'.
  GS_FIELDCAT-SELTEXT_L = 'Vendor Name'.
  GS_FIELDCAT-SELTEXT_M = 'Vendor Name'.
  GS_FIELDCAT-SELTEXT_S = 'Vendor Name'.
  GS_FIELDCAT-OUTPUTLEN = '30'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
*
*  GS_FIELDCAT-FIELDNAME = 'SORTL'.
*  GS_FIELDCAT-SELTEXT_L = 'Search Terms'.
*  GS_FIELDCAT-SELTEXT_M = 'Search Terms'.
*  GS_FIELDCAT-SELTEXT_S = 'Search Terms'.
*  GS_FIELDCAT-OUTPUTLEN = '30'.
*  APPEND GS_FIELDCAT TO GIT_FIELDCAT.
*
*  CLEAR GS_FIELDCAT.


  GS_FIELDCAT-FIELDNAME = 'VTYPE'.
  GS_FIELDCAT-SELTEXT_L = 'Vendor Type'.
  GS_FIELDCAT-SELTEXT_M = 'Vendor Type'.
  GS_FIELDCAT-SELTEXT_S = 'Vendor Type'.
  GS_FIELDCAT-OUTPUTLEN = '30'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'BUKRS'.
  GS_FIELDCAT-SELTEXT_L = 'Com. Code'.
  GS_FIELDCAT-SELTEXT_M = 'Com. Code'.
  GS_FIELDCAT-SELTEXT_S = 'Com. Code'.
  GS_FIELDCAT-OUTPUTLEN = '10'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'KOSTL'.     "created on 27.06.2020
  GS_FIELDCAT-SELTEXT_L = 'Cost Center'.
  GS_FIELDCAT-SELTEXT_M = 'Cost Center'.
  GS_FIELDCAT-SELTEXT_S = 'Cost Center'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'BELNR'.
  GS_FIELDCAT-SELTEXT_L = 'Document No.'.
  GS_FIELDCAT-SELTEXT_M = 'Document No.'.
  GS_FIELDCAT-SELTEXT_S = 'Document No.'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'XBLNR'.
  GS_FIELDCAT-SELTEXT_L = 'Ref.Doc. No.'.
  GS_FIELDCAT-SELTEXT_M = 'Ref.Doc. No.'.
  GS_FIELDCAT-SELTEXT_S = 'Ref.Doc. No.'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'BUZEI'.
  GS_FIELDCAT-SELTEXT_L = 'Line Item'.
  GS_FIELDCAT-SELTEXT_M = 'Line Item'.
  GS_FIELDCAT-SELTEXT_S = 'Line Item'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BLART'.
  GS_FIELDCAT-SELTEXT_L = 'Document Type'.
  GS_FIELDCAT-SELTEXT_M = 'Document Type'.
  GS_FIELDCAT-SELTEXT_S = 'Document Type'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'UMSKZ'.
  GS_FIELDCAT-SELTEXT_L = 'Special G/L Indicator'.
  GS_FIELDCAT-SELTEXT_M = 'Special G/L Indicator'.
  GS_FIELDCAT-SELTEXT_S = 'Special G/L Indicator'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BUDAT'.
  GS_FIELDCAT-SELTEXT_L = 'Posting Date'.
  GS_FIELDCAT-SELTEXT_M = 'Posting Date'.
  GS_FIELDCAT-SELTEXT_S = 'Posting Date'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BLDAT'.
  GS_FIELDCAT-SELTEXT_L = 'Document Date'.
  GS_FIELDCAT-SELTEXT_M = 'Document Date'.
  GS_FIELDCAT-SELTEXT_S = 'Document Date'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'GJAHR'.
  GS_FIELDCAT-SELTEXT_L = 'Fiscal Year'.
  GS_FIELDCAT-SELTEXT_M = 'Fiscal Year'.
  GS_FIELDCAT-SELTEXT_S = 'Fiscal Year'.
  GS_FIELDCAT-OUTPUTLEN = '10'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'NETDT'.
  GS_FIELDCAT-SELTEXT_L = 'Net Due Date'.
  GS_FIELDCAT-SELTEXT_M = 'Net Due Date'.
  GS_FIELDCAT-SELTEXT_S = 'Net Due Date'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SGTXT'.
  GS_FIELDCAT-SELTEXT_L = 'Text'.
  GS_FIELDCAT-SELTEXT_M = 'Text'.
  GS_FIELDCAT-SELTEXT_S = 'Text'.
  GS_FIELDCAT-OUTPUTLEN = '35'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'UTAGE'.
  GS_FIELDCAT-SELTEXT_L = 'Arrears in Days'.
  GS_FIELDCAT-SELTEXT_M = 'Arrears in Days'.
  GS_FIELDCAT-SELTEXT_S = 'Arrears in Days'.
  GS_FIELDCAT-OUTPUTLEN = '20'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'PROFIT'.
  GS_FIELDCAT-SELTEXT_L = 'Profit Center'.
  GS_FIELDCAT-SELTEXT_M = 'Profit Center'.
  GS_FIELDCAT-SELTEXT_S = 'Profit Center'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'ZBD1T'.
  GS_FIELDCAT-SELTEXT_L = 'Credit Days'.
  GS_FIELDCAT-SELTEXT_M = 'Credit Days'.
  GS_FIELDCAT-SELTEXT_S = 'Credit Days'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = 'ZNDUE_AMT'.
  GS_FIELDCAT-SELTEXT_L = 'Amount-Not Due'.
  GS_FIELDCAT-SELTEXT_M = 'Amount-Not Due'.
  GS_FIELDCAT-SELTEXT_S = 'Amount-Not Due'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

*  CLEAR GS_FIELDCAT.
*  GS_FIELDCAT-FIELDNAME = 'DUE'.
*  GS_FIELDCAT-SELTEXT_L = 'Due Amount'.
*  GS_FIELDCAT-SELTEXT_M = 'Due Amount'.
*  GS_FIELDCAT-SELTEXT_S = 'Due Amount'.
*  GS_FIELDCAT-OUTPUTLEN = '15'.
*  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'ADVNC_PAY'.
  GS_FIELDCAT-SELTEXT_L = 'Advance Payment'.
  GS_FIELDCAT-SELTEXT_M = 'Advance Payment'.
  GS_FIELDCAT-SELTEXT_S = 'Advance Pay'.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

***  CLEAR GS_FIELDCAT.
***  GS_FIELDCAT-FIELDNAME = 'SLAB1'.
***  GS_FIELDCAT-SELTEXT_L = LV_SLAB1.
***  GS_FIELDCAT-SELTEXT_M = LV_SLAB1.
***  GS_FIELDCAT-SELTEXT_S = LV_SLAB1.
***  GS_FIELDCAT-DO_SUM = 'X' .
***  WA_SORT-SUBTOT = 'X '.
***  GS_FIELDCAT-OUTPUTLEN = '15'.
***  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB2'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB2.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB2.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB2.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB3'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB3.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB3.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB3.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB4'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB4.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB4.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB4.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB5'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB5.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB5.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB5.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

***  CLEAR GS_FIELDCAT.
***  GS_FIELDCAT-FIELDNAME = 'SLAB6'.
***  GS_FIELDCAT-SELTEXT_L = LV_SLAB6.
***  GS_FIELDCAT-SELTEXT_M = LV_SLAB6.
***  GS_FIELDCAT-SELTEXT_S = LV_SLAB6.
***  GS_FIELDCAT-OUTPUTLEN = '15'.
***  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

*********************************************    created by prabhu on 31.03.2020 starts  *********************************************
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB6'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB6.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB6.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB6.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB7'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB7.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB7.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB7.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB8'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB8.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB8.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB8.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SLAB9'.
  GS_FIELDCAT-SELTEXT_L = LV_SLAB9.
  GS_FIELDCAT-SELTEXT_M = LV_SLAB9.
  GS_FIELDCAT-SELTEXT_S = LV_SLAB9.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

***  CLEAR GS_FIELDCAT.
***  GS_FIELDCAT-FIELDNAME = 'ADVNC_PAY'.
***  GS_FIELDCAT-SELTEXT_L = 'Advance Payment'.
***  GS_FIELDCAT-SELTEXT_M = 'Advance Payment'.
***  GS_FIELDCAT-SELTEXT_S = 'Advance Pay'.
***  GS_FIELDCAT-OUTPUTLEN = '15'.
***  APPEND GS_FIELDCAT TO GIT_FIELDCAT.

*********************************************    created by prabhu on 31.03.2020 Ends  *********************************************
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'DMSHB'.
  GS_FIELDCAT-SELTEXT_L = 'Amount'.
  GS_FIELDCAT-SELTEXT_M = 'Amount'.
  GS_FIELDCAT-SELTEXT_S = 'Amount'.
  GS_FIELDCAT-OUTPUTLEN = '15'.
  GS_FIELDCAT-DO_SUM = 'X' .
  WA_SORT-SUBTOT = 'X '.
  APPEND GS_FIELDCAT TO GIT_FIELDCAT.
*********************************************    created by prabhu on 1.04.2020 starts  *********************************************
  WA_SORT-FIELDNAME ='VTYPE'.
  WA_SORT-UP = 'X'.

  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.
*********************************************    created by prabhu on 1.04.2020 Ends  *********************************************
ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER .

  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.

  DATA : LV_LIFNR(100) TYPE C,
         LV_BUKRS(100) TYPE C.

  CLEAR : LV_LIFNR,
          LV_BUKRS.
  IF DD_LIFNR-HIGH IS NOT INITIAL.
    CONCATENATE 'Vendor No :'DD_LIFNR-LOW 'To' DD_LIFNR-HIGH INTO LV_LIFNR SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Vendor No :'DD_LIFNR-LOW INTO LV_LIFNR SEPARATED BY SPACE.
  ENDIF.

  IF DD_BUKRS-HIGH IS NOT INITIAL.
    CONCATENATE 'Company Code :'DD_BUKRS-LOW 'To' DD_BUKRS-HIGH INTO LV_BUKRS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Company Code :'DD_BUKRS-LOW INTO LV_BUKRS SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_LIFNR.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BUKRS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Vendor Over Due Items' .
  APPEND LS_LINE TO LIT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.
*      I_LOGO             = 'ARKKAYS_LOGO'.


ENDFORM.                    " ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*&      Form  DATA_VALIDITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_VALIDITY .
  IF RASTBIS1 GT '998'
  OR RASTBIS2 GT '998'
  OR RASTBIS3 GT '998'
  OR RASTBIS4 GT '998'
  OR RASTBIS5 GT '998'.
    MESSAGE E381.
  ENDIF.

  IF NOT RASTBIS5 IS INITIAL.
    IF  RASTBIS5 GT RASTBIS4
    AND RASTBIS4 GT RASTBIS3
    AND RASTBIS3 GT RASTBIS2
    AND RASTBIS2 GT RASTBIS1.
    ELSE.
      MESSAGE E379.
    ENDIF.
  ELSE.
    IF NOT RASTBIS4 IS INITIAL.
      IF  RASTBIS4 GT RASTBIS3
      AND RASTBIS3 GT RASTBIS2
      AND RASTBIS2 GT RASTBIS1.
      ELSE.
        MESSAGE E379.
      ENDIF.
    ELSE.
      IF NOT RASTBIS3 IS INITIAL.
        IF  RASTBIS3 GT RASTBIS2
        AND RASTBIS2 GT RASTBIS1.
        ELSE.
          MESSAGE E379.
        ENDIF.
      ELSE.
        IF NOT RASTBIS2 IS INITIAL.
          IF  RASTBIS2 GT RASTBIS1.
          ELSE.
            MESSAGE E379.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DATA_VALIDITY
*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
***FORM SORT .
***
***  WA_SORT-FIELDNAME ='VTYPE'.
***  WA_SORT-UP = 'X'.
***  WA_SORT-SUBTOT = 'X '.
***  APPEND WA_SORT TO IT_SORT.
***  CLEAR WA_SORT.
***
***
***ENDFORM.                    " SORT
