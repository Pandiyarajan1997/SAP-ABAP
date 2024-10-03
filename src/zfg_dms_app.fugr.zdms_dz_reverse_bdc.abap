FUNCTION zdms_dz_reverse_bdc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(BUDAT) TYPE  BUDAT
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"     REFERENCE(STATUS) TYPE  BAPI_MTYPE
*"     REFERENCE(E_BELNR) TYPE  BELNR_D
*"     REFERENCE(E_GJAHR) TYPE  GJAHR
*"----------------------------------------------------------------------
  DATA lv_msg_text TYPE string.
  DATA : lv_date TYPE char12.
***************bdc data filling****************
  CLEAR gw_bdcdata.
  REFRESH gt_bdcdata.
  gw_bdcdata-program  = 'SAPMF05A'.
  gw_bdcdata-dynpro   = '0105'.
  gw_bdcdata-dynbegin = 'X'.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'BDC_OKCODE'.
  gw_bdcdata-fval = '=BU'.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'RF05A-BELNS'.
  gw_bdcdata-fval = belnr.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'BKPF-BUKRS'.
  gw_bdcdata-fval = bukrs.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'RF05A-GJAHS'.
  gw_bdcdata-fval = gjahr.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'UF05A-STGRD'.
  gw_bdcdata-fval = '04'.
  APPEND gw_bdcdata TO gt_bdcdata.

  CLEAR : lv_date.
  WRITE : budat TO lv_date.
  CLEAR gw_bdcdata.
  gw_bdcdata-fnam = 'BSIS-BUDAT'.
  gw_bdcdata-fval = lv_date.
  APPEND gw_bdcdata TO gt_bdcdata.

  CALL TRANSACTION 'FB08' WITH AUTHORITY-CHECK USING gt_bdcdata
             MODE   'N'
             UPDATE 'A'
             MESSAGES INTO gt_bdcmsg.
  COMMIT WORK AND WAIT.
  CLEAR : gw_bdcmsg.
  READ TABLE gt_bdcmsg INTO gw_bdcmsg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    LOOP AT gt_bdcmsg INTO gw_bdcmsg WHERE msgtyp = 'E'.
      CLEAR lv_msg_text.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = gw_bdcmsg-msgid
          lang      = sy-langu
          no        = gw_bdcmsg-msgnr
          v1        = gw_bdcmsg-msgv1
          v2        = gw_bdcmsg-msgv2
          v3        = gw_bdcmsg-msgv3
          v4        = gw_bdcmsg-msgv4
        IMPORTING
          msg       = lv_msg_text
        EXCEPTIONS
          not_found = 01.
      lv_msg_text = |{ lv_msg_text } { lv_msg_text }|.
    ENDLOOP.
    status  = 'E'.
    message = lv_msg_text.
  ELSE.
    CLEAR : gw_bdcmsg.
    READ TABLE gt_bdcmsg INTO gw_bdcmsg WITH KEY msgtyp = 'S'.
    e_belnr = gw_bdcmsg-msgv1.
    e_gjahr = gjahr.
    status  = 'S'.
    message = |Document reversed successfully - { e_belnr } { e_gjahr }|.
  ENDIF.


ENDFUNCTION.
