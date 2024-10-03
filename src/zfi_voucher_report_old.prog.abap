*&---------------------------------------------------------------------*
*& Report ZFI_VOUCHER_REPORT_OLD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_voucher_report_old.

************************************************************************************************
*                 PROGRAM FOR VOUCHER PRINTING
************************************************************************************************
* Program Name          :  ZFI_VOUCHER_REPORT
* Functional Analyst    :  Gajanan
* Programmer            :  SHANMUGASUNDARAM / ROOPAKUMAAR RAJA PALANI
* Start date            :
* Description           :  Program for VOUCHER PRINTING
* Type Of Program       :  Smartforms
*----------------------------------------------------------------------------------------------


**************************************************************************************************
*    TABLE  DECLARATIONS
**************************************************************************************************


TABLES : bkpf, bseg.

**************************************************************************************************
*      DATA DECLARATIONS
**************************************************************************************************
DATA : it_bseg TYPE TABLE OF zfi_recon_stru WITH HEADER LINE.
DATA : it_bkpf TYPE TABLE OF zfi_recon_head WITH HEADER LINE.

DATA : wa_bseg    TYPE zfi_recon_stru .
DATA : wa_bkpf    TYPE zfi_recon_head.
DATA : wrk_ktopl  TYPE t001-ktopl.
DATA : fm_voucher TYPE rs38l_fnam.
DATA : CONTROL_param  TYPE ssfctrlop .

**************************************************************************************************
*        SELECTION SCREEN
**************************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: pa_belnr FOR bkpf-belnr.
  SELECT-OPTIONS: pa_budat FOR bkpf-budat.
  PARAMETERS : pa_bukrs TYPE bkpf-bukrs,
               pa_gjahr TYPE bkpf-gjahr.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  REFRESH : it_bkpf.
  SELECT  *  FROM bkpf INTO CORRESPONDING FIELDS OF TABLE it_bkpf WHERE bukrs = pa_bukrs AND belnr IN pa_belnr AND gjahr = pa_gjahr.
*
*    SELECT *  FROM BKPF
*        INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
*        WHERE BUKRS = PA_BUKRS
*        AND   BELNR IN PA_BELNR.
**        AND   GJAHR = PA_GJAHR
**        AND   BUDAT IN PA_BUDAT .

  IF it_bkpf[]  IS   INITIAL.
    MESSAGE : 'This Selection criteria not matching' TYPE 'I'.
    LEAVE SCREEN.
  ENDIF.



  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = 'ZFI_VOUCHER'
    IMPORTING
      fm_name  = fm_voucher.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  control_param-no_open = 'X'.
  control_param-no_close = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control_param.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT it_bkpf INTO wa_bkpf.

    REFRESH : it_bseg.
    PERFORM data_selection.

    CALL FUNCTION fm_voucher
      EXPORTING
        control_parameters = control_param
        git_head           = wa_bkpf
* IMPORTING
      TABLES
        git_item           = it_bseg
* EXCEPTIONS
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CLEAR : wa_bkpf.
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
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
FORM data_selection.

*BREAK-POINT.

  IF it_bkpf[] IS NOT INITIAL.

    REFRESH : it_bseg.
    SELECT bschl gsber zuonr sgtxt kostl saknr  kunnr lifnr  prctr "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
           projk shkzg dmbtr wrbtr hkont koart  ebeln
    FROM bseg
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
    FOR ALL ENTRIES IN it_bkpf  "*  FOR ALL ENTRIES IN IT_BKPF
    WHERE belnr = it_bkpf-belnr " wa_bkpf-belnr Modified by govind on 21.04.2014
    AND   bukrs = it_bkpf-bukrs "wa_bkpf-BUKRS
    AND   gjahr = it_bkpf-gjahr. "#EC CI_NOORDER   ""wa_bkpf-GJAHRAdded by <IT-CAR Tool> during Code Remediation


  ENDIF.


  SELECT SINGLE ktopl
  FROM t001
  INTO wrk_ktopl
  WHERE bukrs = pa_bukrs.

  LOOP AT it_bseg INTO wa_bseg.

    IF wa_bseg-koart = 'K'.
      SELECT SINGLE lifnr name1 INTO (wa_bseg-lifnr, wa_bseg-txt50)
      FROM lfa1 WHERE lifnr = wa_bseg-lifnr .
      IF sy-subrc = 0.
        wa_bseg-hkont = wa_bseg-lifnr.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING hkont txt50.
      ENDIF.

      IF wa_bkpf-waers NE 'INR'.
        wa_bseg-wrbtr  = wa_bseg-dmbtr.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING wrbtr.
      ENDIF.

    ELSEIF wa_bseg-koart = 'D'.
      SELECT SINGLE kunnr name1  INTO (wa_bseg-kunnr, wa_bseg-txt50)
      FROM kna1 WHERE kunnr = wa_BSEG-kunnr .
      IF sy-subrc = 0.
        wa_bseg-hkont = wa_bseg-kunnr.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING hkont txt50.
      ENDIF.

      IF wa_bkpf-waers NE 'INR'.
        wa_bseg-wrbtr  = wa_bseg-dmbtr.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING wrbtr.
      ENDIF.
    ELSE.
      SELECT saknr txt50
      UP TO 1 ROWS FROM skat
      INTO (wa_bseg-saknr, wa_bseg-txt50)
      WHERE saknr = wa_bseg-hkont
      AND ktopl = wrk_ktopl ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF sy-subrc = 0.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING saknr txt50.
      ENDIF.

      IF wa_bkpf-waers NE 'INR'.
        wa_bseg-wrbtr  = wa_bseg-dmbtr.
        MODIFY it_bseg FROM wa_bseg INDEX sy-tabix TRANSPORTING wrbtr.
      ENDIF.
    ENDIF.

    CLEAR : wa_bseg.

  ENDLOOP.

  SORT it_bseg BY shkzg DESCENDING.

ENDFORM.
