*&---------------------------------------------------------------------*
*& Report  ZFI_VOUCHER_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZKP_CRMEMO_VOUCHER_REPORT.
*&---------------------------------------------------------------------*
*& Report  ZFI_VOUCHER_REPORT_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*





************************************************************************************************
*                 PROGRAM FOR VOUCHER PRINTING
************************************************************************************************
* Program Name          :  ZFI_VOUCHER_REPORT
* Functional Analyst    :  Govindarajan Murugan
* Programmer            :  Govindarajan Murugan
* Start date            : 21.04.2014
* Description           :  Program for Customer Credit Memo
* Type Of Program       :  Smartforms
*----------------------------------------------------------------------------------------------
**************************************************************************************************
*    TABLE  DECLARATIONS
**************************************************************************************************


TABLES : BKPF, BSEG.

**************************************************************************************************
*      DATA DECLARATIONS
**************************************************************************************************
DATA : IT_BSEG TYPE TABLE OF ZFI_RECON_STRU WITH HEADER LINE.
DATA : IT_BKPF TYPE TABLE OF ZFI_RECON_HEAD WITH HEADER LINE.



DATA : WA_BSEG    TYPE ZFI_RECON_STRU .
DATA : WA_BKPF    TYPE ZFI_RECON_HEAD.

DATA : WRK_KTOPL  TYPE T001-KTOPL.
DATA : FM_VOUCHER TYPE RS38L_FNAM.
DATA : CONTROL_PARAM  TYPE SSFCTRLOP .

**************************************************************************************************
*        SELECTION SCREEN
**************************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: PA_BELNR FOR BKPF-BELNR MATCHCODE OBJECT ZSHP_BKPF NO-EXTENSION,
                PA_BUDAT FOR BKPF-BUDAT,
                PA_KUNNR FOR BSEG-KUNNR .  "ADDED BY RAM ON 4/4/16
PARAMETERS : PA_BUKRS TYPE BKPF-BUKRS,
             PA_GJAHR TYPE BKPF-GJAHR.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.
  REFRESH : IT_BKPF.

  SELECT  *  FROM BKPF INTO CORRESPONDING FIELDS OF TABLE IT_BKPF WHERE BUKRS = PA_BUKRS AND BELNR IN PA_BELNR AND GJAHR = PA_GJAHR.
*
*    SELECT *  FROM BKPF
*        INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
*        WHERE BUKRS = PA_BUKRS
*        AND   BELNR IN PA_BELNR.
**        AND   GJAHR = PA_GJAHR
**        AND   BUDAT IN PA_BUDAT .

  IF IT_BKPF[]  IS   INITIAL.
    MESSAGE : 'This Selection criteria not matching' TYPE 'I'.
    LEAVE SCREEN.
  ENDIF.



  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME = 'ZFI_VOUCHER_CRMEMO'
    IMPORTING
      FM_NAME  = FM_VOUCHER.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CONTROL_PARAM-NO_OPEN = 'X'.
  CONTROL_PARAM-NO_CLOSE = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAM.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT IT_BKPF INTO WA_BKPF.

    REFRESH : IT_BSEG.
    PERFORM DATA_SELECTION.

    CALL FUNCTION FM_VOUCHER
      EXPORTING
         CONTROL_PARAMETERS       = CONTROL_PARAM
        GIT_HEAD                   = WA_BKPF
* IMPORTING
      TABLES
        GIT_ITEM                   = IT_BSEG

* EXCEPTIONS
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CLEAR : WA_BKPF.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
* IMPORTING
*   JOB_OUTPUT_INFO        =
* EXCEPTIONS
*   FORMATTING_ERROR       = 1
*   INTERNAL_ERROR         = 2
*   SEND_ERROR             = 3
*   OTHERS                 = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_SELECTION.

*BREAK-POINT.

  IF IT_BKPF[] IS NOT INITIAL.

    REFRESH : IT_BSEG.
    SELECT BSCHL GSBER ZUONR SGTXT KOSTL SAKNR  KUNNR LIFNR  PRCTR "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
      AUGBL                                               "ADDED BY RAM ON 4/4/16
           PROJK SHKZG DMBTR WRBTR HKONT KOART  EBELN
    FROM BSEG
    INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
    FOR ALL ENTRIES IN IT_BKPF  "*  FOR ALL ENTRIES IN IT_BKPF
    WHERE BELNR = IT_BKPF-BELNR " wa_bkpf-belnr Modified by govind on 21.04.2014
    AND   BUKRS = IT_BKPF-BUKRS "wa_bkpf-BUKRS
    AND   GJAHR = IT_BKPF-GJAHR AND BELNR  IN PA_BELNR ""wa_bkpf-GJAHR
    AND KUNNR IN PA_KUNNR.  "#EC CI_NOORDER    ""ADDED BY RAM 4/4/2016Added by <IT-CAR Tool> during Code Remediation

  ENDIF.


  SELECT SINGLE KTOPL
  FROM T001
  INTO WRK_KTOPL
  WHERE BUKRS = PA_BUKRS.

  LOOP AT IT_BSEG INTO WA_BSEG.

    IF WA_BSEG-KOART = 'K'.
      SELECT SINGLE LIFNR NAME1 INTO (WA_BSEG-LIFNR, WA_BSEG-TXT50)
      FROM LFA1 WHERE LIFNR = WA_BSEG-LIFNR .
      IF SY-SUBRC = 0.
        WA_BSEG-HKONT = WA_BSEG-LIFNR.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING HKONT TXT50.
      ENDIF.

      IF WA_BKPF-WAERS NE 'INR'.
        WA_BSEG-WRBTR  = WA_BSEG-DMBTR.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING WRBTR.
      ENDIF.

    ELSEIF WA_BSEG-KOART = 'D'.
      SELECT SINGLE KUNNR NAME1  INTO (WA_BSEG-KUNNR, WA_BSEG-TXT50)
      FROM KNA1 WHERE KUNNR = WA_BSEG-KUNNR .
      IF SY-SUBRC = 0.
        WA_BSEG-HKONT = WA_BSEG-KUNNR.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING HKONT TXT50.
      ENDIF.

      IF WA_BKPF-WAERS NE 'INR'.
        WA_BSEG-WRBTR  = WA_BSEG-DMBTR.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING WRBTR.
      ENDIF.
    ELSE.
      SELECT SAKNR TXT50
      UP TO 1 ROWS FROM SKAT
      INTO (WA_BSEG-SAKNR, WA_BSEG-TXT50)
      WHERE SAKNR = WA_BSEG-HKONT
      AND KTOPL = WRK_KTOPL ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC = 0.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING SAKNR TXT50.
      ENDIF.

      IF WA_BKPF-WAERS NE 'INR'.
        WA_BSEG-WRBTR  = WA_BSEG-DMBTR.
        MODIFY IT_BSEG FROM WA_BSEG INDEX SY-TABIX TRANSPORTING WRBTR.
      ENDIF.
    ENDIF.

    CLEAR : WA_BSEG.

  ENDLOOP.


*  LOOP AT IT_BSEG INTO WA_BSEG WHERE KUNNR <> .
*
*
*
*  ENDLOOP.

  SORT IT_BSEG BY SHKZG DESCENDING.



ENDFORM.                    "DATA_SELECTION

**************************************************************************************************
*    SMARTFORMS PROCESS
**************************************************************************************************
*FORM CALL_SMARTFORM.
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname                 =  'ZFI_VOUCHER'
*   IMPORTING
*     FM_NAME                  =  FM_VOUCHER.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*
*CALL FUNCTION FM_VOUCHER
*  EXPORTING
*    git_head                   = WA_BKPF
** IMPORTING
*  tables
*    git_item                   = IT_BSEG
** EXCEPTIONS
*          .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*
*  ENDFORM.
