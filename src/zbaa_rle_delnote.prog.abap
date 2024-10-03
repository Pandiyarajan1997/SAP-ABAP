
*----------------------------------------------------------------------*
*      Print of a delivery note by SAPscript SMART FORMS               *
*----------------------------------------------------------------------*
REPORT ZBAA_RLE_DELNOTE.

* declaration of data
INCLUDE ZRLE_DELNOTE_DATA_DECLARE1.
*INCLUDE RLE_DELNOTE_DATA_DECLARE.
* definition of forms
INCLUDE ZRLE_DELNOTE_FORMS1.
*INCLUDE RLE_DELNOTE_FORMS.
INCLUDE ZRLE_PRINT_FORMS2.
*INCLUDE RLE_PRINT_FORMS.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM ENTRY USING RETURN_CODE US_SCREEN.

  DATA: LF_RETCODE TYPE SY-SUBRC.
  XSCREEN = US_SCREEN.
  PERFORM PROCESSING USING    US_SCREEN
                     CHANGING LF_RETCODE.
  IF LF_RETCODE NE 0.
    RETURN_CODE = 1.
  ELSE.
    RETURN_CODE = 0.
  ENDIF.

ENDFORM.                    "ENTRY
*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
FORM PROCESSING USING    PROC_SCREEN
                CHANGING CF_RETCODE.

  DATA: LS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ.
  DATA: LS_DLV_DELNOTE        TYPE LEDLV_DELNOTE.
  DATA: LF_FM_NAME            TYPE RS38L_FNAM.
  DATA: LS_CONTROL_PARAM      TYPE SSFCTRLOP.
  DATA: LS_COMPOSER_PARAM     TYPE SSFCOMPOP.
  DATA: LS_RECIPIENT          TYPE SWOTOBJID.
  DATA: LS_SENDER             TYPE SWOTOBJID.
  DATA: LF_FORMNAME           TYPE TDSFNAME.
  DATA: LS_ADDR_KEY           LIKE ADDR_KEY.
  data: ls_job_info           type ssfcrescl.
  data: l_spoolid             type rspoid.


* SmartForm from customizing table TNAPR
  LF_FORMNAME = TNAPR-SFORM.

* determine print data
  PERFORM SET_PRINT_DATA_TO_READ USING    LF_FORMNAME
                                 CHANGING LS_PRINT_DATA_TO_READ
                                 CF_RETCODE.

* determine foreign trade data
  ls_print_data_to_read-IT_FT = 'X'.

  IF CF_RETCODE = 0.
* select print data
    PERFORM GET_DATA USING    LS_PRINT_DATA_TO_READ
                     CHANGING LS_ADDR_KEY
                              LS_DLV_DELNOTE
                              CF_RETCODE.
  ENDIF.

  IF CF_RETCODE = 0.
    PERFORM SET_PRINT_PARAM USING    LS_ADDR_KEY
                            CHANGING LS_CONTROL_PARAM
                                     LS_COMPOSER_PARAM
                                     LS_RECIPIENT
                                     LS_SENDER
                                     CF_RETCODE.
  ENDIF.

  IF CF_RETCODE = 0.
* determine smartform function module for delivery note
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
         EXPORTING  FORMNAME           = LF_FORMNAME
*                 variant            = ' '
*                 direct_call        = ' '
         IMPORTING  FM_NAME            = LF_FM_NAME
         EXCEPTIONS NO_FORM            = 1
                    NO_FUNCTION_MODULE = 2
                    OTHERS             = 3.
    IF SY-SUBRC <> 0.
*   error handling
      CF_RETCODE = SY-SUBRC.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

  IF CF_RETCODE = 0.
*   call smartform delivery note
    CALL FUNCTION LF_FM_NAME
         EXPORTING
                  ARCHIVE_INDEX        = TOA_DARA
                  ARCHIVE_PARAMETERS   = ARC_PARAMS
                  CONTROL_PARAMETERS   = LS_CONTROL_PARAM
*                 mail_appl_obj        =
                  MAIL_RECIPIENT       = LS_RECIPIENT
                  MAIL_SENDER          = LS_SENDER
                  OUTPUT_OPTIONS       = LS_COMPOSER_PARAM
                  USER_SETTINGS        = ' '
                  IS_DLV_DELNOTE       = LS_DLV_DELNOTE
                  IS_NAST              = NAST
*      importing  document_output_info =
                  job_output_info      = ls_job_info
*                 job_output_options   =
       EXCEPTIONS FORMATTING_ERROR     = 1
                  INTERNAL_ERROR       = 2
                  SEND_ERROR           = 3
                  USER_CANCELED        = 4
                  OTHERS               = 5.
    IF SY-SUBRC <> 0.
*   error handling
      CF_RETCODE = SY-SUBRC.
      PERFORM PROTOCOL_UPDATE.
*     get SmartForm protocoll and store it in the NAST protocoll
      PERFORM ADD_SMFRM_PROT.                  "INS_HP_335958

*Fehlermeldung bei Druckansicht ausgeben
      if ls_control_param-preview = 'X'.
        message id syst-msgid type syst-msgty number syst-msgno
        with syst-msgv1 syst-msgv2 syst-msgv3 syst-msgv4.
      endif.
    else.
      read table ls_job_info-spoolids into l_spoolid index 1.
      if sy-subrc is initial.
        export spoolid = l_spoolid to memory id 'KYK_SPOOLID'.
      endif.

    ENDIF.

  ENDIF.

* get SmartForm protocoll and store it in the NAST protocoll
* PERFORM ADD_SMFRM_PROT.                       DEL_HP_335958

ENDFORM.                    "PROCESSING
