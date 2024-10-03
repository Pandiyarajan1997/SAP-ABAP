class ZCL_ZSD_SALES_ORDER_PD_DPC_EXT definition
  public
  inheriting from ZCL_ZSD_SALES_ORDER_PD_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSD_SALES_ORDER_PD_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    DATA: lv_fm_name   TYPE rs38l_fnam,
          lv_bin_value TYPE xstring,
          lv_sonumber  TYPE vbeln_va.

    DATA: ls_control_param      TYPE ssfctrlop.
    DATA: ls_composer_param     TYPE ssfcompop.
    DATA: lw_stream TYPE ty_s_media_resource.
    DATA: lw_otf_fm TYPE ssfcrescl.
    DATA: it_lines TYPE TABLE OF tline,
          it_otf   TYPE TABLE OF itcoo.
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
      lv_sonumber = VALUE #( it_key_tab[ 1 ]-value OPTIONAL ).
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
      ENDIF.
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
        CLEAR lw_stream.
        lw_stream-value = lv_bin_value.
        lw_stream-mime_type = 'application/pdf'.
      ENDIF.
"Final Data
      CALL METHOD me->copy_data_to_ref
        EXPORTING
          is_data = lw_stream
        CHANGING
          cr_data = er_stream.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
