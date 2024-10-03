FUNCTION zsd_billinvoice_pdf_gener_dms.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_VBELN) TYPE  VBELN_VF
*"     REFERENCE(IM_GJAHR) TYPE  GJAHR
*"  EXPORTING
*"     REFERENCE(EP_XSTRING) TYPE  STRING
*"     REFERENCE(ERROR_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      SO_NUM_MISSING
*"--------------------------------------------------------------------
*Created by : Pandiarajan
*Created On: 11.05.2024
*Purpose : billing invoice PDF generation
*----------------------------------------------------------------------*
  DATA: lv_fm_name    TYPE rs38l_fnam,
        lv_bin_value  TYPE xstring,
        lv_billnumber TYPE vbeln.
  DATA: lv_string TYPE string.
  DATA: ls_bil_invoice TYPE lbbil_invoice.
  DATA ls_docpara TYPE sfpdocparams.
  DATA ls_outpara TYPE sfpoutputparams.
  DATA ls_output  TYPE fpformoutput.
  DATA  ls_frmname TYPE fpname.
  DATA: lv_fm        TYPE rs38l_fnam.

  IF im_vbeln IS NOT INITIAL AND im_gjahr IS NOT INITIAL.
    SELECT SINGLE * FROM vbrk INTO @DATA(l_invhdr)
      WHERE vbeln = @im_vbeln
      AND gjahr = @im_gjahr.
    DATA(l_set_print) = VALUE lbbil_print_data_to_read(  hd_gen          = abap_true
                                                         hd_adr          = abap_true
                                                         hd_gen_descript = abap_true
                                                         hd_org          = abap_true
                                                         hd_part_add     = abap_true
                                                         hd_kond         = abap_true
                                                         hd_fin          = abap_true
                                                         hd_ref          = abap_true
                                                         hd_tech         = abap_true
                                                         it_gen          = abap_true
                                                         it_adr          = abap_true
                                                         it_price        = abap_true
                                                         it_kond         = abap_true
                                                         it_ref          = abap_true
                                                         it_refdlv       = abap_true
                                                         it_reford       = abap_true
                                                         it_refpurord    = abap_true
                                                         it_refvag       = abap_true
                                                         it_refvg2       = abap_true
                                                         it_refvkt       = abap_true
                                                         it_tech         = abap_true
                                                         it_fin          = abap_true
                                                         it_confitm      = abap_true
                                                         it_confbatch    = abap_true
                                                         msr_hd          = abap_true
                                                         msr_it          = abap_true ).
    DATA(lv_objkey) = CONV nast-objky( l_invhdr-vbeln ).
    CLEAR:ls_bil_invoice.
    CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
      EXPORTING
        if_bil_number         = lv_objkey
        is_print_data_to_read = l_set_print
        if_parvw              = 'RE'
        if_parnr              = l_invhdr-kunag
        if_language           = sy-langu
      IMPORTING
        es_bil_invoice        = ls_bil_invoice
      EXCEPTIONS
        records_not_found     = 1
        records_not_requested = 2
        OTHERS                = 3.
    IF sy-subrc = 0.

      IF ls_bil_invoice IS NOT INITIAL.

        ls_outpara-getpdf = abap_true.
*        ls_outpara-nodialog = ''.
*        ls_outpara-preview = 'X'.

        CALL FUNCTION 'FP_JOB_OPEN'
          CHANGING
            ie_outputparams = ls_outpara
          EXCEPTIONS
            cancel          = 1
            usage_error     = 2
            system_error    = 3
            internal_error  = 4
            OTHERS          = 5.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CLEAR: lv_fm,ls_frmname.
*        CASE l_invhdr-fkart.
*          WHEN 'YBBR' OR 'YBRE' OR 'YBFS' OR 'YFRE' OR 'YSTO'.
*            ls_frmname = 'ZSD_IRN_QR_EINVOICE'.
*          WHEN 'YBDP' OR 'YBTE' OR 'YIRE' OR 'YBTR'.
*            ls_frmname = 'ZSD_IRN_QR_EINVOICE_YBDP'.
*        ENDCASE.
        ls_frmname = 'ZSD_IRN_QR_EINVOICE_DMS'.
        "Get Respective Function module Name based on form Name
        TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
                i_name     = ls_frmname
              IMPORTING
                e_funcname = lv_fm.
          CATCH cx_root.
            RETURN.
        ENDTRY.

        CALL FUNCTION lv_fm
          EXPORTING
            /1bcdwb/docparams  = ls_docpara
            is_bil_invoice     = ls_bil_invoice
          IMPORTING
            /1bcdwb/formoutput = ls_output
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.

        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.

        DATA(out_pdf) = CONV xstring( ls_output-pdf ).
        CLEAR lv_string.
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = out_pdf
          IMPORTING
            output = lv_string.
        ep_xstring = lv_string.
      ENDIF.
    ENDIF.
  ELSE.
    RAISE so_num_missing.
  ENDIF.


ENDFUNCTION.
