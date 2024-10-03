*&---------------------------------------------------------------------*
*& Report  ZKP_MM_DELIVERY_CHALLAN
*&
*&---------------------------------------------------------------------*

REPORT ZKP_NOROO_DELIVERY_CHALLAN.
************************************************************************************************
*                 PROGRAM FOR  Delivery Challan
************************************************************************************************
* Program Name          :  ZFI_VOUCHER_REPORT
* Functional Analyst    :  Mohammad Tazim Hasmi
* Programmer            :  SHANMUGASUNDARAM
* Start date            :
* Description           :  Program for  Delivery challan
* Type Of Program       :  Smartforms
*----------------------------------------------------------------------------------------------

TABLES :  MKPF,  MSEG.
*=================================================================================================
*    Type Declarations
*=================================================================================================
DATA : WA_MKPF   TYPE  MKPF.
DATA : WA_MSEG   TYPE  MSEG.
DATA : WS_MSEG   TYPE  MSEG.
DATA : WA_TOPLNT  TYPE  T001W.
DATA : WA_FRPLNT  TYPE  T001W.
DATA : CONTROL_PARAM  TYPE SSFCTRLOP .
DATA : FM_GET TYPE RS38L_FNAM.
DATA : IT_MKPF   TYPE  TABLE  OF MKPF WITH HEADER LINE.
DATA : IT_MSEG   TYPE  TABLE  OF MSEG WITH HEADER LINE.


*=================================================================================================
*     Selection screens
*=================================================================================================
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : GMBLNR  FOR  MKPF-MBLNR .
*SELECT-OPTIONS : GMJAHR  FOR  MKPF-MJAHR.
PARAMETERS : GMJAHR LIKE MKPF-MJAHR.
*PARAMETERS : GUMWRK LIKE MSEG-UMWRK.
*PARAMETERS : GWERKS LIKE MSEG-WERKS.
SELECTION-SCREEN END OF BLOCK B1.


*=================================================================================================
*    get  data from mkpf
*=================================================================================================
START-OF-SELECTION.
REFRESH IT_MKPF.
SELECT * FROM MKPF INTO CORRESPONDING FIELDS OF TABLE IT_MKPF
         WHERE MBLNR IN GMBLNR AND MJAHR = GMJAHR.

        IF IT_MKPF[]  IS   INITIAL.
          MESSAGE : 'Your Selection criteria not matching' TYPE 'I'.
          LEAVE SCREEN.
        ENDIF.
* BREAK-POINT.
    SELECT * FROM MSEG  INTO CORRESPONDING FIELDS OF TABLE IT_MSEG FOR ALL ENTRIES IN IT_MKPF
         WHERE MBLNR = IT_MKPF-MBLNR AND MJAHR = IT_MKPF-MJAHR AND  BWART = '351' AND SHKZG = 'H'. " Modified By Govind on 10.04.2014 'S' changes To 'H'

      IF IT_MSEG[]  IS   INITIAL. " Added By Govind 0n 04.04.204

    SELECT * FROM MSEG  INTO CORRESPONDING FIELDS OF TABLE IT_MSEG FOR ALL ENTRIES IN IT_MKPF
         WHERE MBLNR = IT_MKPF-MBLNR AND MJAHR = IT_MKPF-MJAHR AND  BWART = '641' AND SHKZG = 'H'.
ENDIF.
 IF IT_MSEG[]  IS   INITIAL. " Added By Govind 0n 04.04.204

    SELECT * FROM MSEG  INTO CORRESPONDING FIELDS OF TABLE IT_MSEG FOR ALL ENTRIES IN IT_MKPF
         WHERE MBLNR = IT_MKPF-MBLNR AND MJAHR = IT_MKPF-MJAHR AND  BWART = '541' AND SHKZG = 'H'.
ENDIF.
 IF IT_MSEG[]  IS   INITIAL.

          MESSAGE : 'No line item found - Check your entry ' TYPE 'I'.
          LEAVE SCREEN.
        ENDIF.

CASE IT_MSEG-BWART .
  WHEN '351'.
  LOOP AT IT_MSEG  INTO WA_MSEG.

    SELECT * UP TO 1 ROWS FROM  MSEG INTO WS_MSEG  WHERE   MBLNR = WA_MSEG-MBLNR AND MJAHR = WA_MSEG-MJAHR
                                                 AND LINE_ID = WA_MSEG-PARENT_ID AND WERKS = WA_MSEG-UMWRK  AND    BWART = '351' AND SHKZG = 'H'   ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation


    IF  SY-SUBRC = 0.
      WA_MSEG-CHARG =  WS_MSEG-CHARG.
    IF WA_MSEG-DMBTR = '0'.
      WA_MSEG-DMBTR = WS_MSEG-DMBTR.
      MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG DMBTR.
    Else.
    MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG.
    endif.
    endif.
    CLEAR WA_MSEG.

  ENDLOOP.
  ENDCASE.

CASE IT_MSEG-BWART . " Added By Govind 0n 04.04.204
  WHEN '541'.
  LOOP AT IT_MSEG  INTO WA_MSEG.

    SELECT * UP TO 1 ROWS FROM  MSEG INTO WS_MSEG  WHERE   MBLNR = WA_MSEG-MBLNR AND MJAHR = WA_MSEG-MJAHR
                                                AND LINE_ID = WA_MSEG-PARENT_ID AND WERKS = WA_MSEG-UMWRK  AND    BWART = '541' AND SHKZG = 'H'  ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation


    IF  SY-SUBRC = 0.
      WA_MSEG-CHARG =  WS_MSEG-CHARG.
    IF WA_MSEG-DMBTR = '0'.
      WA_MSEG-DMBTR = WS_MSEG-DMBTR.
      MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG DMBTR.
    Else.
    MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG.
    endif.
    endif.
    CLEAR WA_MSEG.

  ENDLOOP.
  ENDCASE.

CASE IT_MSEG-BWART . " Added By Govind 0n 04.04.204
WHEN '641'.
   LOOP AT IT_MSEG  INTO WA_MSEG.
 SELECT * UP TO 1 ROWS FROM  MSEG INTO WS_MSEG  WHERE   MBLNR = WA_MSEG-MBLNR AND MJAHR = WA_MSEG-MJAHR
                                               AND LINE_ID = WA_MSEG-PARENT_ID AND WERKS = WA_MSEG-UMWRK   AND  BWART = '641' AND SHKZG = 'H'   ORDER BY PRIMARY KEY.
 ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

     IF  SY-SUBRC = 0.
      WA_MSEG-CHARG =  WS_MSEG-CHARG.
    IF WA_MSEG-DMBTR = '0'.
      WA_MSEG-DMBTR = WS_MSEG-DMBTR.
      MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG DMBTR.
    Else.
    MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING CHARG.
    endif.
    endif.
    CLEAR WA_MSEG.
    ENDLOOP.
    ENDCASE.
*SELECT  SINGLE *  FROM T001W  INTO WA_TOPLNT  WHERE  WERKS = GWERKS.
*SELECT  SINGLE *  FROM T001W  INTO WA_FRPLNT  WHERE  WERKS = GUMWRK.


*=================================================================================================
*     calling smartforms
*=================================================================================================
   CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME                 =  'YKP_MM_DELV_CHAL_NOROO'
   IMPORTING
      FM_NAME                  =  FM_GET .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



   CALL FUNCTION  FM_GET
     EXPORTING
       CONTROL_PARAMETERS         = CONTROL_PARAM
*       WA_T001W                   = WA_FRPLNT
*       IS_T001W                   = WA_TOPLNT
*       WA_MKPF                    = WA_MKPF
     TABLES
       IT_MSEG                    = IT_MSEG
       IT_MKPF                    = IT_MKPF
    EXCEPTIONS
      FORMATTING_ERROR           = 1
      INTERNAL_ERROR             = 2
      SEND_ERROR                 = 3
*      USER_CANCELED              = 4
*      OTHERS                     = 5
             .
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.


**=================================================================================================
**     smartform open
**=================================================================================================
*
* CONTROL_PARAM-NO_OPEN = 'X'.
* CONTROL_PARAM-NO_CLOSE = 'X'.
*
* CALL FUNCTION 'SSF_OPEN'
* EXPORTING
*   CONTROL_PARAMETERS       = CONTROL_PARAM  .
*
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.


**=================================================================================================
**     calling function to smartforms with loop
**=================================================================================================
*LOOP AT IT_MKPF INTO WA_MKPF.
*     REFRESH IT_MSEG.
*   CLEAR : WA_TOPLNT, WA_FRPLNT.
*   PERFORM  GET_DATA.
*
*   CALL FUNCTION  FM_GET
*     EXPORTING
*       CONTROL_PARAMETERS         = CONTROL_PARAM
*       WA_T001W                   = WA_FRPLNT
*       IS_T001W                   = WA_TOPLNT
*       WA_MKPF                    = WA_MKPF
*     TABLES
*       IT_MSEG                    = IT_MSEG
**    EXCEPTIONS
**      FORMATTING_ERROR           = 1
**      INTERNAL_ERROR             = 2
**      SEND_ERROR                 = 3
**      USER_CANCELED              = 4
**      OTHERS                     = 5
*             .
*   IF SY-SUBRC <> 0.
** Implement suitable error handling here
*   ENDIF.
*ENDLOOP.



*CALL FUNCTION 'SSF_CLOSE'
** IMPORTING
**   JOB_OUTPUT_INFO        =
** EXCEPTIONS
**   FORMATTING_ERROR       = 1
**   INTERNAL_ERROR         = 2
**   SEND_ERROR             = 3
**   OTHERS                 = 4
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

*=================================================================================================
*     get data
*=================================================================================================

*FORM GET_DATA.
*  SELECT  *  FROM MSEG   INTO CORRESPONDING FIELDS OF TABLE IT_MSEG WHERE MBLNR = WA_MKPF-MBLNR AND  MJAHR = WA_MKPF-MJAHR AND SHKZG = 'S'.
*  READ  TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_MKPF-MBLNR  MJAHR = WA_MKPF-MJAHR.
*  SELECT  SINGLE *  FROM T001W  INTO WA_TOPLNT  WHERE  WERKS = WA_MSEG-WERKS.
*  SELECT  SINGLE *  FROM T001W  INTO WA_FRPLNT  WHERE  WERKS = WA_MSEG-UMWRK.
*ENDFORM.
