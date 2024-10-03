FUNCTION zsd_sales_order_pdf_generate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_VBELN) TYPE  VBELN_VA
*"  EXPORTING
*"     REFERENCE(EP_XSTRING) TYPE  STRING
*"     REFERENCE(ERROR_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      SO_NUM_MISSING
*"----------------------------------------------------------------------
*Created by : Samsudeen M
*Created On: 06.05.2023
*Purpose : Sales Order PDF generation
*----------------------------------------------------------------------*
  DATA: lv_fm_name   TYPE rs38l_fnam,
        lv_bin_value TYPE xstring,
        lv_sonumber  TYPE vbeln_va.
  DATA: lv_string TYPE string.

  DATA: ls_control_param      TYPE ssfctrlop.
  DATA: ls_composer_param     TYPE ssfcompop.
  DATA: lw_otf_fm TYPE ssfcrescl.
  DATA: it_lines TYPE TABLE OF tline,
        it_otf   TYPE TABLE OF itcoo.

  IF im_vbeln IS NOT INITIAL.
    "Calling the Smartforms to get Function Module Name
    CLEAR lv_fm_name.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZSD_SALES_ORDER_FORM'
      IMPORTING
        fm_name            = lv_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc = 0.
      CLEAR: lv_sonumber.
      lv_sonumber = im_vbeln.
*Control Parameters for Smartforms
      ls_composer_param-tddest    = 'LP01'.
      ls_composer_param-xdfcmode  = abap_true.
      ls_composer_param-xsfcmode  = abap_true.
      ls_composer_param-tdnewid   = abap_true.
      ls_composer_param-tdimmed   = abap_true.
      ls_control_param-no_dialog  = abap_true.
      ls_control_param-preview    = space.
      ls_control_param-getotf     = abap_true.
      "Actual Smartforms Function Module Call
      CALL FUNCTION lv_fm_name
        EXPORTING
          control_parameters = ls_control_param
          output_options     = ls_composer_param
          ip_vbeln           = lv_sonumber
        IMPORTING
          job_output_info    = lw_otf_fm
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc = 0.
        REFRESH it_otf.
        it_otf[] = lw_otf_fm-otfdata[].
*Convert the OTF data to Xstring
        CLEAR lv_bin_value.
        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            format                = 'PDF'
          IMPORTING
            bin_file              = lv_bin_value
          TABLES
            otf                   = it_otf
            lines                 = it_lines
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            err_bad_otf           = 4
            OTHERS                = 5.
        IF sy-subrc = 0.
*          ep_xstring = lv_bin_value.
          CLEAR lv_string.
          CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
            EXPORTING
              input  = lv_bin_value
            IMPORTING
              output = lv_string.
          ep_xstring = lv_string.
        ELSE.
          CASE sy-subrc.
            WHEN '1'.
              error_message = |err_max_linewidth|.
            WHEN '2'.
              error_message = |err_format|.
            WHEN '3'.
              error_message = |err_conv_not_possible|.
            WHEN '4'.
              error_message = |err_bad_otf|.
            WHEN '5'.
              error_message = |Others|.
          ENDCASE.
        ENDIF.
      ELSE.
        CASE sy-subrc.
          WHEN '1'.
            error_message = |formatting_error|.
          WHEN '2'.
            error_message = |internal_error|.
          WHEN '3'.
            error_message = |send_error|.
          WHEN '4'.
            error_message = |user_canceled|.
          WHEN '5'.
            error_message = |Others|.
        ENDCASE.
      ENDIF.
    ELSE.
      CASE sy-subrc.
        WHEN '1'.
          error_message = |no_form|.
        WHEN '2'.
          error_message = |no_function_module|.
        WHEN '3'.
          error_message = |Others|.
      ENDCASE.
    ENDIF.
  ELSE.
    RAISE so_num_missing.
  ENDIF.


ENDFUNCTION.
