"Name: \PR:RQCAAP01\FO:INITIALIZE_AFTER_PROCESSING\SE:BEGIN\EI
ENHANCEMENT 0 ZAAP00F04.

DATA: command TYPE c LENGTH 100,
      lv_line1 TYPE char10,
      zscreen VALUE 'X',
      zsubrc TYPE sy-subrc,
      I1 TYPE CHAR2,
      lv_T001K TYPE T001K,
      ls_lfa1 TYPE lfa1.


if nast-PARNR is NOT INITIAL.


SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = nast-PARNR.

 SELECT SINGLE * from T001K INTO lv_T001K WHERE BWKEY = ls_lfa1-WERKS.

if sy-subrc ne 0.


endif.

ELSEIF P_PARNR is NOT INITIAL.


  SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = p_PARNR.


  SELECT SINGLE * from T001K INTO lv_T001K WHERE BWKEY = ls_lfa1-WERKS.


else.

if vbdpl-WERKs IS NOT INITIAL.

     SELECT SINGLE * from T001K INTO lv_T001K WHERE BWKEY = vbdpl-WERKs.


ELSEIF ls_qals-WERK is NOT INITIAL.

  SELECT SINGLE * from T001K INTO lv_T001K WHERE BWKEY = ls_qals-WERK.

endif.

endif.

    mchar-HSDAT = Ls_QALS-PAENDTERM.
    lv_qty = ls_qals-LOSMENGE.
    CONDENSE lv_qty.



  IF lv_T001K-BUKRS = '1000' ."or vbdpl-WERKS = '1005'.

    LV_CNAME = 'Sheenlac Paints Limited'.

    T001W-NAME1 = lv_cname.

    CALL FUNCTION 'WRITE_FORM'
     EXPORTING
       ELEMENT                        = 'SHEENLAC'
*       FUNCTION                       = 'SET'
*       TYPE                           = 'BODY'
        WINDOW                         = 'LOGO'
*     IMPORTING
*       PENDING_LINES                  =
*     EXCEPTIONS
*       ELEMENT                        = 1
*       FUNCTION                       = 2
*       TYPE                           = 3
*       UNOPENED                       = 4
*       UNSTARTED                      = 5
*       WINDOW                         = 6
*       BAD_PAGEFORMAT_FOR_PRINT       = 7
*       SPOOL_ERROR                    = 8
*       CODEPAGE                       = 9
*       OTHERS                         = 10
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.



  ELSEIF lv_T001K-BUKRS = '1400'.

    LV_CNAME = 'Jenson & Nicholson Paints Pvt. Ltd.'.
    T001W-NAME1 = lv_cname.

    CALL FUNCTION 'WRITE_FORM'
     EXPORTING
       ELEMENT                        = 'JNPAINTS'
*       FUNCTION                       = 'SET'
*       TYPE                           = 'BODY'
        WINDOW                         = 'LOGO'
*     IMPORTING
*       PENDING_LINES                  =
*     EXCEPTIONS
*       ELEMENT                        = 1
*       FUNCTION                       = 2
*       TYPE                           = 3
*       UNOPENED                       = 4
*       UNSTARTED                      = 5
*       WINDOW                         = 6
*       BAD_PAGEFORMAT_FOR_PRINT       = 7
*       SPOOL_ERROR                    = 8
*       CODEPAGE                       = 9
*       OTHERS                         = 10
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.


endif.
*********************************DATA RETRIVAL*********************************************


SELECT PRUEFLOS
       VORGLFNR
       MERKNR
       QPMK_WERKS
       VERWMERKM
       MKVERSION
       KATALGART1
       AUSWMENGE1
       AUSWMGWRK1
       MASSEINHSW
       TOLERANZOB
       TOLERANZUN FROM QAMV INTO TABLE GT_QAMV
                  WHERE PRUEFLOS = LS_QALS-PRUEFLOS.

IF gt_qamv is not INITIAL.

 SELECT WERKS
        KATALOGART
        AUSWAHLMGE
        KTX01      FROM QPAM INTO TABLE GT_QPAM
                   FOR ALL ENTRIES IN GT_QAMV
                   WHERE WERKS = GT_QAMV-AUSWMGWRK1 AND
                         KATALOGART = GT_QAMV-KATALGART1 AND
                         AUSWAHLMGE = GT_QAMV-AUSWMENGE1 AND
                         KSP01  = 'EN'.

 SELECT ZAEHLER
        MKMNR
        VERSION
        SORTFELD FROM QPMK INTO TABLE GT_QPMK
                 FOR ALL ENTRIES IN GT_QAMV
                 WHERE ZAEHLER = GT_QAMV-QPMK_WERKS AND
                       MKMNR   = GT_QAMV-VERWMERKM AND
                       VERSION = GT_QAMV-MKVERSION.

SELECT PRUEFLOS
       VORGLFNR
       MERKNR
       ORIGINAL_INPUT
       KATALGART1
       GRUPPE1
       CODE1
       VERSION1
                 FROM QAMR INTO TABLE GT_QAMR
                 FOR ALL ENTRIES IN GT_QAMV
                 WHERE PRUEFLOS = GT_QAMV-PRUEFLOS AND
                       VORGLFNR = GT_QAMV-VORGLFNR AND
                       MERKNR   = GT_QAMV-MERKNR                        .

SELECT PRUEFLOS
       VORGLFNR
       MERKNR
       KATALGART1
       GRUPPE1
       CODE1
       VERSION1
       ORIGINAL_INPUT FROM qase INTO TABLE gt_qase
                      FOR ALL ENTRIES IN gt_qamr
                      where  PRUEFLOS  = gt_qamr-PRUEFLOS and
                             VORGLFNR  = gt_qamr-VORGLFNR and
                             MERKNR    = gt_qamr-MERKNR .






SELECT KATALOGART
       CODEGRUPPE
       CODE
       VERSION
       KURZTEXT FROM QPCT INTO TABLE GT_QPCT
                FOR ALL ENTRIES IN GT_QASE
                WHERE KATALOGART = GT_QASE-KATALGART1 AND
                      CODEGRUPPE = GT_QASE-GRUPPE1   AND
                      CODE       = GT_QASE-CODE1.




  DATA: LV_UPPER TYPE P DECIMALS 3,
        LV_LOWER TYPE P DECIMALS 3,
        LV_RESULT TYPE P DECIMALS 3,
        LV_UPPER1(15),
        LV_LOWER1(15),
        LV_RESULT1(15),
        LV_UNIT(10).

  LOOP AT GT_QAMV INTO GS_QAMV.

   IF  GS_QAMV-AUSWMENGE1 IS NOT INITIAL .

     READ TABLE GT_QPAM INTO GS_QPAM WITH KEY WERKS = GS_QAMV-AUSWMGWRK1
                                               KATALOGART = GS_QAMV-KATALGART1
                                               AUSWAHLMGE = GS_QAMV-AUSWMENGE1 .

     GS_FINAL-SPEC = GS_QPAM-KTX01.

*     READ TABLE GT_QAMR INTO GS_QAMR WITH KEY PRUEFLOS = GS_QAMV-PRUEFLOS
*                                              VORGLFNR = GS_QAMV-VORGLFNR
*                                              MERKNR   = GS_QAMV-MERKNR.


       READ TABLE GT_QASE INTO GS_QASE WITH KEY    PRUEFLOS = GS_QAMV-PRUEFLOS
                                                   VORGLFNR = GS_QAMV-VORGLFNR
                                                   MERKNR   = GS_QAMV-MERKNR.



     READ TABLE GT_QPCT INTO GS_QPCT WITH KEY KATALOGART = Gs_QASE-KATALGART1
                                              CODEGRUPPE = Gs_QASE-GRUPPE1
                                              CODE       = Gs_QASE-CODE1.


     GS_FINAL-RESU = GS_QPCT-KURZTEXT.


   ELSE.

     READ TABLE GT_QAMR INTO GS_QAMR WITH KEY PRUEFLOS = GS_QAMV-PRUEFLOS
                                              VORGLFNR = GS_QAMV-VORGLFNR
                                              MERKNR   = GS_QAMV-MERKNR.

if GS_QAMR-ORIGINAL_INPUT is NOT INITIAL.

      LV_RESULT1 =  GS_QAMR-ORIGINAL_INPUT.

else.

  READ TABLE GT_QASE INTO GS_QASE WITH KEY    PRUEFLOS = GS_QAMR-PRUEFLOS
                                              VORGLFNR = GS_QAMR-VORGLFNR
                                              MERKNR   = GS_QAMR-MERKNR.

     LV_RESULT1 = GS_QASE-ORIGINAL_INPUT.

endif.
     LV_UPPER = GS_QAMV-TOLERANZOB.
     LV_LOWER = GS_QAMV-TOLERANZUN.

     LV_UPPER1 = LV_UPPER.
     LV_LOWER1 = LV_LOWER.
     LV_UNIT   = GS_QAMV-MASSEINHSW.

     CONDENSE : LV_UPPER1 ,
                LV_LOWER1 ,
                LV_RESULT1,
                LV_UNIT  .

     CONCATENATE  LV_LOWER1 '-'  LV_UPPER1   INTO GS_FINAL-SPEC.

     GS_FINAL-RESU = LV_RESULT1.
     GS_FINAL-UNIT = LV_UNIT .



ENDIF.

READ TABLE GT_QPMK INTO GS_QPMK WITH KEY ZAEHLER = GS_QAMV-QPMK_WERKS
                                         MKMNR   = GS_QAMV-VERWMERKM
                                         VERSION = GS_QAMV-MKVERSION.

GS_FINAL-PARA = GS_QPMK-SORTFELD.

APPEND GS_FINAL TO GT_FINAL.
CLEAR : GS_FINAL,
        LV_UPPER1 ,
        LV_LOWER1 ,
        LV_RESULT,
        LV_UNIT  ,
        LV_UPPER ,
        LV_LOWER .

  ENDLOOP.


  lv_line = '18'.

DATA : I TYPE I VALUE '2'.


*GS_FINAL-PARA = '12345678901234567890123456'.
*GS_FINAL-SPEC = '1234567890123456789012'.
*GS_FINAL-RESU = '1234567890123456789012'.
*GS_FINAL-UNIT = '123456'.
*APPend gs_final to gt_final.
*clear gs_final.

loop at GT_FINAL INTO GS_FINAL.

  lv_sno = lv_sno + 1.

  LV_SNO1  = LV_SNO.
  condense : LV_SNO1.

IF LV_SNO <= 9.
  SHIFT LV_SNO1 RIGHT.
ENDIF.

LV_TP = GS_FINAL-PARA.
LV_S  = GS_FINAL-SPEC.
LV_R  = GS_FINAL-RESU.
LV_U  = GS_FINAL-UNIT.


  CALL FUNCTION 'WRITE_FORM'
 EXPORTING
    ELEMENT                        = 'LINEITEMS'
*   FUNCTION                       = 'SET'
*   TYPE                           = 'BODY'
    WINDOW                         = 'MAIN'
* IMPORTING
*   PENDING_LINES                  =
* EXCEPTIONS
*   ELEMENT                        = 1
*   FUNCTION                       = 2
*   TYPE                           = 3
*   UNOPENED                       = 4
*   UNSTARTED                      = 5
*   WINDOW                         = 6
*   BAD_PAGEFORMAT_FOR_PRINT       = 7
*   SPOOL_ERROR                    = 8
*   CODEPAGE                       = 9
*   OTHERS                         = 10
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.


  lv_line = lv_line + 2.
  lv_line1 = lv_line.
  CONDENSE LV_LINE1.

DO 10 TIMES.
SHIFT LV_TP BY 22 PLACES.
SHIFT LV_S BY 20 PLACES.
SHIFT LV_R BY 19 PLACES.
SHIFT LV_U BY 4 PLACES.

IF LV_TP IS NOT INITIAL OR  LV_S IS NOT INITIAL OR LV_R IS NOT INITIAL.

*IF LV_TP(1) IS NOT INITIAL.
*
*  CONCATENATE '-' LV_TP INTO LV_TP.
*ENDIF.
*
*IF LV_S(1) IS NOT INITIAL.
*
*  CONCATENATE '-' LV_S INTO LV_S.
*
*ENDIF.
*
*IF LV_R(1) IS NOT INITIAL.
*
*  CONCATENATE '-' LV_R INTO LV_R.
*
*ENDIF.



CALL FUNCTION 'WRITE_FORM'
 EXPORTING
    ELEMENT                        = 'EXTRA'
*   FUNCTION                       = 'SET'
*   TYPE                           = 'BODY'
    WINDOW                         = 'MAIN'
* IMPORTING
*   PENDING_LINES                  =
* EXCEPTIONS
*   ELEMENT                        = 1
*   FUNCTION                       = 2
*   TYPE                           = 3
*   UNOPENED                       = 4
*   UNSTARTED                      = 5
*   WINDOW                         = 6
*   BAD_PAGEFORMAT_FOR_PRINT       = 7
*   SPOOL_ERROR                    = 8
*   CODEPAGE                       = 9
*   OTHERS                         = 10
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.
I = I + 2.
ELSE.

  EXIT.
ENDIF.

ENDDO.



  CONCATENATE 'POSITION XORIGIN ''1'' CM YORIGIN ''' LV_LINE1 ''' LN' into command.


  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR command.
I1 = I.
CONDENSE I1.
 CONCATENATE 'BOX HEIGHT ''' I1 ''' LN FRAME ''0.1'' MM' into command.




  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.



  CONCATENATE 'POSITION XORIGIN ''2.5'' CM YORIGIN ''' LV_LINE1 '''LN' into command.


  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR command.

CONCATENATE 'BOX WIDTH ''6'' CM HEIGHT ''' I1 ''' LN FRAME ''0.1'' MM' into command.

*   command = 'BOX WIDTH ''5'' CM HEIGHT ''2'' LN FRAME ''0.1'' MM'.


  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

CLEAR COMMAND.
  CONCATENATE 'POSITION XORIGIN ''8.5'' CM YORIGIN ''' LV_LINE1 '''LN' into command.


  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR command.

CONCATENATE 'BOX WIDTH ''5.5'' CM HEIGHT ''' I1 ''' LN FRAME ''0.1'' MM' into command.



  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.



CLEAR COMMAND.
  CONCATENATE 'POSITION XORIGIN ''14'' CM YORIGIN ''' LV_LINE1 '''LN' into command.


  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR command.

CONCATENATE 'BOX WIDTH ''1.5'' CM HEIGHT ''' I1 ''' LN FRAME ''0.1'' MM' into command.



  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      COMMAND         = command
*   EXCEPTIONS
*     UNOPENED        = 1
*     UNSTARTED       = 2
*     OTHERS          = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

*CALL FUNCTION 'WRITE_FORM'
* EXPORTING
*    ELEMENT                        = 'LINEITEMS'
**   FUNCTION                       = 'SET'
**   TYPE                           = 'BODY'
*    WINDOW                         = 'MAIN'
** IMPORTING
**   PENDING_LINES                  =
** EXCEPTIONS
**   ELEMENT                        = 1
**   FUNCTION                       = 2
**   TYPE                           = 3
**   UNOPENED                       = 4
**   UNSTARTED                      = 5
**   WINDOW                         = 6
**   BAD_PAGEFORMAT_FOR_PRINT       = 7
**   SPOOL_ERROR                    = 8
**   CODEPAGE                       = 9
**   OTHERS                         = 10
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
CLEAR: GS_FINAL, I1 , LV_TP, LV_R , LV_S, LV_U.

I = I / 2.

IF I NE '1'.
  I = I - 1.
DO I TIMES.

 lv_line = lv_line + 2.

ENDDO.
ENDIF.

IF  lv_line >= '58'.

  lv_line = '6'.

ENDIF.

I = 2.
endloop.

else.


  CALL FUNCTION 'WRITE_FORM'
 EXPORTING
    ELEMENT                        = 'NOT_FOUND'
*   FUNCTION                       = 'SET'
*   TYPE                           = 'BODY'
    WINDOW                         = 'MAIN'
* IMPORTING
*   PENDING_LINES                  =
* EXCEPTIONS
*   ELEMENT                        = 1
*   FUNCTION                       = 2
*   TYPE                           = 3
*   UNOPENED                       = 4
*   UNSTARTED                      = 5
*   WINDOW                         = 6
*   BAD_PAGEFORMAT_FOR_PRINT       = 7
*   SPOOL_ERROR                    = 8
*   CODEPAGE                       = 9
*   OTHERS                         = 10
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.



endif.












*
ENDENHANCEMENT.
