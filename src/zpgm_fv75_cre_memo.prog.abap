REPORT ZPGM_FV75_CRE_MEMO
       NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPE-POOLS TRUXS.
TYPE-POOLS SLIS.
DATA:BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF TY_MESSAGE,
         KUNNR        TYPE BSAD-KUNNR,        "Customer
         WRBTR        TYPE BSAD-WRBTR,        "Amount
         BELNR        TYPE BELNR_D,         "Bill/DR memo
         MESS_TYPE(1) TYPE C,
         MESSAGE      TYPE BAPI_MSG,
         STATUS(4)    TYPE C,
         NAME1        TYPE NAME1_GP,          " Customer description
       END OF TY_MESSAGE.

TYPES: BEGIN OF TY_TAB,
  MARK(1)     TYPE C,
  KUNNR       TYPE BSAD-KUNNR,        "Customer
  WRBTR       TYPE BSAD-WRBTR,        "Amount
  BELNR       TYPE BELNR_D,         "Bill/DR memo
END OF TY_TAB.

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
        GS_MESSTAB TYPE BDCMSGCOLL,
        RETURN_VALUES LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

DATA :  GT_MESSAGE    TYPE STANDARD TABLE OF TY_MESSAGE,
      GS_MESSAGE    TYPE TY_MESSAGE.

DATA :  GT_TAB TYPE STANDARD TABLE OF TY_TAB,
      GS_TAB TYPE TY_TAB.

DATA : LV_DATE TYPE CHAR10,
      LV_DATE1 TYPE SY-DATUM.
DATA : GV_BUDAT LIKE FTPOST-FVAL.

DATA : GV_MODE TYPE APQI-PUTACTIVE VALUE 'A'.

DATA : GV_VALUE LIKE FTPOST-FVAL.

DATA : LV_TEXT TYPE CHAR200.

DATA : LV_TEXT1 LIKE T247-LTX.

TYPES : BEGIN OF GS_ZADV_INT_PAY,
       " MANDT TYPE ZADV_INT_PAY-MANDT,
        BUKRS TYPE ZADV_INT_PAY-BUKRS,
        KUNNR TYPE ZADV_INT_PAY-KUNNR,
        INT_DATE TYPE ZADV_INT_PAY-INT_DATE,
        BELNR TYPE ZADV_INT_PAY-BELNR,
        GJAHR TYPE ZADV_INT_PAY-GJAHR,
        DMBTR TYPE ZADV_INT_PAY-DMBTR,
        INT_PER TYPE ZADV_INT_PAY-INT_PER,
        INT_AMT TYPE ZADV_INT_PAY-INT_AMT,
        CNDOC TYPE ZADV_INT_PAY-CNDOC,
        CNDATE TYPE ZADV_INT_PAY-CNDATE,
        CNAMT TYPE ZADV_INT_PAY-CNAMT,
      END OF GS_ZADV_INT_PAY.

DATA : GT_ZADV_INT_PAY TYPE TABLE OF GS_ZADV_INT_PAY,
       WA_ZADV_INT_PAY TYPE GS_ZADV_INT_PAY .

DATA : GT1_ZADV_INT_PAY TYPE TABLE OF GS_ZADV_INT_PAY,
       WA1_ZADV_INT_PAY TYPE GS_ZADV_INT_PAY .

TYPES : BEGIN OF GS_FINAL ,
         BUKRS TYPE ZADV_INT_PAY-BUKRS,
         KUNNR TYPE ZADV_INT_PAY-KUNNR,
         INT_AMT TYPE ZADV_INT_PAY-INT_AMT,
       END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

TYPES : BEGIN OF GS_FV75,
          BUSCS TYPE RF05A-BUSCS,
          ACCNT TYPE INVFO-ACCNT,
          BLDAT TYPE INVFO-BLDAT,
          XBLNR TYPE INVFO-XBLNR,
          BUDAT TYPE INVFO-BUDAT,
          WRBTR TYPE INVFO-WRBTR,
          WAERS TYPE INVFO-WAERS,
        END OF GS_FV75.

START-OF-SELECTION.

  LV_DATE1 = SY-DATUM  .

  DATA : lv_mon LIKE T247-MNR.
  DATA : LV_YEAR TYPE CHAR4 .
  DATA : L_YEAR TYPE CHAR2.
  DATA : LV_STEXT TYPE INVFO-XBLNR.

  DATA : LANG LIKE SY-LANGU VALUE 'EN' .

  lv_mon = LV_DATE1+4(2) .
  LV_YEAR = LV_DATE1+0(4).
  L_YEAR = LV_DATE1+0(2).

    SELECT SINGLE  LTX FROM T247
     INTO LV_TEXT1
     WHERE SPRAS EQ sy-langu AND MNR EQ lv_mon.

**  CALL FUNCTION 'ISP_GET_MONTH_NAME'
**    EXPORTING
**  "   DATE               = SY-DATUM
**      LANGUAGE           =  LANG
**     MONTH_NUMBER       = LV_MON
**   IMPORTING
***     LANGU_BACK         =
**     LONGTEXT           =  LV_TEXT1
**  "   SHORTTEXT          =
**   EXCEPTIONS
**     CALENDAR_ID        = 1
**     DATE_ERROR         = 2
**     NOT_FOUND          = 3
**     WRONG_INPUT        = 4
**     OTHERS             = 5
**            .
**  IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**  ENDIF.

  CONCATENATE 'Interest On Credit for' LV_TEXT1 LV_YEAR INTO LV_TEXT SEPARATED BY SPACE.


  CONCATENATE 'Int.Crdt' lv_mon '/' L_YEAR INTO LV_STEXT SEPARATED BY SPACE.
  CONCATENATE LV_DATE1+6(2) LV_DATE1+4(2) LV_DATE1+0(4) INTO LV_DATE
  SEPARATED BY '.' .
  WRITE LV_DATE TO GV_BUDAT.

  SELECT
          BUKRS
          KUNNR
          INT_DATE
          BELNR
          GJAHR
          DMBTR
          INT_PER
          INT_AMT
          CNDOC
          CNDATE
          CNAMT FROM ZADV_INT_PAY INTO TABLE GT_ZADV_INT_PAY
          UP TO 1 ROWS WHERE CNDOC EQ ' ' AND CNDATE EQ ' '
          ORDER BY PRIMARY KEY. " Added by <IT-CAR Tool> during Code Remediation

  APPEND LINES OF GT_ZADV_INT_PAY TO GT1_ZADV_INT_PAY .

  SORT GT_ZADV_INT_PAY ASCENDING BY  BUKRS KUNNR.
  SORT GT1_ZADV_INT_PAY ASCENDING BY BUKRS KUNNR.

  DELETE ADJACENT DUPLICATES FROM GT_ZADV_INT_PAY COMPARING BUKRS KUNNR .

  LOOP AT GT_ZADV_INT_PAY INTO WA_ZADV_INT_PAY.
    WA_FINAL-BUKRS = WA_ZADV_INT_PAY-BUKRS.
    WA_FINAL-KUNNR = WA_ZADV_INT_PAY-KUNNR.
    LOOP AT GT1_ZADV_INT_PAY INTO WA1_ZADV_INT_PAY WHERE BUKRS = WA_FINAL-BUKRS AND KUNNR = WA_FINAL-KUNNR .
      WA_FINAL-INT_AMT = WA_FINAL-INT_AMT + WA1_ZADV_INT_PAY-INT_AMT .
    ENDLOOP.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.



  DELETE GT_FINAL WHERE INT_AMT EQ '0.00' .

  LOOP AT GT_FINAL INTO WA_FINAL .
    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '1200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'RF05A-BUSCS'
                                  'G'.
    PERFORM BDC_FIELD       USING 'INVFO-ACCNT'
                                  WA_FINAL-KUNNR. "'10001495'."customer code
    PERFORM BDC_FIELD       USING 'INVFO-BLDAT'
                                   GV_BUDAT." '07.09.2020'."document date
    PERFORM BDC_FIELD       USING 'INVFO-XBLNR'
                                  LV_STEXT ."'123'. " "reference
    PERFORM BDC_FIELD       USING 'INVFO-BUDAT'
                                  GV_BUDAT. "'07.09.2020'."posting date
    PERFORM BDC_FIELD       USING 'INVFO-SGTXT'
                                  LV_TEXT.
    WRITE WA_FINAL-INT_AMT TO GV_VALUE.
    CONDENSE GV_VALUE.
    PERFORM BDC_FIELD       USING 'INVFO-WRBTR'
                                   GV_VALUE. " WA_FINAL-INT_AMT." '10000'."amount
    PERFORM BDC_FIELD       USING 'INVFO-WAERS'
                                  'INR'."currency
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'ACGL_ITEM-HKONT(01)'.
    PERFORM BDC_FIELD       USING 'ACGL_ITEM-HKONT(01)'
                                  '25110001'."g/l account
    PERFORM BDC_FIELD       USING 'ACGL_ITEM-WRBTR(01)' "'ACGL_ITEM-WRBTR(01)'
                                   GV_VALUE. "'100'.  "WA_FINAL-INT_AMT. " "'1000'. 10000."
    PERFORM BDC_FIELD       USING 'ACGL_ITEM-HKONT(01)'
                              '25110001'.
    CALL TRANSACTION 'FV75'
                     USING BDCDATA
                      MODE   'N'
                      UPDATE 'A'
                     MESSAGES INTO MESSTAB.
    WAIT UP TO 2 SECONDS.
    COMMIT WORK AND WAIT.
    CLEAR  GV_VALUE .
    CLEAR: GS_MESSTAB, GS_MESSAGE.
    REFRESH : BDCDATA.
    READ TABLE MESSTAB INTO GS_MESSTAB WITH KEY MSGTYP = 'S'
                                               MSGNR = '312'.
    IF SY-SUBRC IS INITIAL.
      COMMIT WORK AND WAIT.
*     DATA: LV_BELNR TYPE BELNR_D.
*     DO 10 TIMES.
*       SELECT SINGLE BELNR FROM BKPF INTO LV_BELNR
*                           WHERE BELNR = GS_MESSTAB-MSGV1.
*       IF SY-SUBRC IS NOT INITIAL.
*         WAIT UP TO 2 SECONDS.
*       ELSE.
*         EXIT.
*       ENDIF.
*     ENDDO.
      GS_MESSAGE-MESS_TYPE = 'S'.
      UPDATE ZADV_INT_PAY SET CNDOC = GS_MESSTAB-MSGV1 CNDATE = LV_DATE1 CNAMT = WA_FINAL-INT_AMT WHERE KUNNR = WA_FINAL-KUNNR AND CNDOC EQ ' ' .
      " MOVE-CORRESPONDING GS_TAB TO GS_MESSAGE.
    ENDIF.
    CLEAR : WA_FINAL,GS_MESSTAB.
    refresh : MESSTAB .
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0075   text
*      -->P_0076   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.              " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0080   text
*      -->P_0081   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> NODATA.
  CLEAR BDCDATA.
*  REFRESH BDCDATA[].
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.
ENDFORM.                    " BDC_FIELD
