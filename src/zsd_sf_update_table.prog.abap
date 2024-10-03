*&---------------------------------------------------------------------*
*& Report ZMM_ASN_UPDATE_MIS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_sf_update_table.
*TABLES: zsd_sf_cust_inv.

*
TYPES: BEGIN OF ty_status,
         custno    TYPE kunnr,
         invoiceno TYPE vbeln,
         status    TYPE text100,
       END OF ty_status.

DATA: lt_msg TYPE STANDARD TABLE OF ty_status,
      lw_msg TYPE ty_status.

DATA: lv_dummy    TYPE c,
      lv_clen     TYPE i,
      lv_line(72) TYPE c,
      lv_len      TYPE i,
      lv_size     TYPE i,
      lv_hex(144) TYPE x.
DATA: lt_otf_data   TYPE tsfotf,
      w_otf_data    TYPE itcoo,
      lv_invno      TYPE zsd_sf_cust_inv-invoiceno,
      lv_kunnr      TYPE zsd_sf_cust_inv-custno,
      lv_string     TYPE string,
      lv_discounted TYPE char1.

FIELD-SYMBOLS: <l_fs> TYPE c.

SELECT-OPTIONS: so_invno FOR lv_invno," MATCHCODE OBJECT f4_vbrk,
                so_kunnr FOR lv_kunnr.
*                so_date  FOR zsd_sf_cust_inv-invoicedate.

INITIALIZATION.


START-OF-SELECTION.

  SELECT * FROM zsd_sf_cust_inv
    INTO TABLE @DATA(lt_cust_inv)
    WHERE  invoiceno IN @so_invno AND
         custno IN @so_kunnr AND
*        invoicedate BETWEEN @so_date-low AND @so_date-high AND
         status = '10'.
  SORT lt_cust_inv BY invoiceno.

  IF lt_cust_inv IS NOT INITIAL.
**********************get the OPEN item details*****************
    SELECT a~kunnr, a~budat, a~bldat, a~blart,a~shkzg,a~waers,
           a~dmbtr, a~xblnr, a~belnr ,a~sgtxt,
           a~vbeln,  a~umskz ,a~gjahr
           FROM bsid AS a
           INTO TABLE @DATA(lt_bsid)
           FOR ALL ENTRIES IN @lt_cust_inv
           WHERE a~bukrs EQ '1000'
           AND   a~kunnr EQ @lt_cust_inv-custno
           AND   a~umskz <> 'H'.
    IF sy-subrc = 0.
      SORT : lt_bsid BY kunnr budat vbeln.
    ENDIF.
**********************get the cleared item details*****************
    SELECT a~kunnr, a~budat, a~bldat, a~blart,a~shkzg,a~waers,
           a~dmbtr, a~xblnr, a~belnr ,a~sgtxt,
           a~vbeln,  a~umskz ,a~gjahr
           FROM bsad AS a
           INTO TABLE @DATA(lt_bsad)
           FOR ALL ENTRIES IN @lt_cust_inv
           WHERE a~bukrs EQ '1000'
           AND   a~kunnr EQ @lt_cust_inv-custno
           AND   a~umskz <> 'H'.
    IF sy-subrc = 0.
      SORT : lt_bsad BY vbeln.
    ENDIF.
**********************get the eway bill details**************
    SORT lt_cust_inv BY invoiceno.
    SELECT  docno
    INTO TABLE @DATA(lt_ewaybill)
     FROM j_1ig_ewaybill
      FOR ALL ENTRIES IN @lt_cust_inv
      WHERE bukrs = @lt_cust_inv-bukrs AND
*            gjahr = @lt_cust_inv-gjahr AND
            docno = @lt_cust_inv-invoiceno AND
            status = 'A'.
    SORT lt_ewaybill BY docno.
    LOOP AT lt_cust_inv ASSIGNING FIELD-SYMBOL(<fs>).
      CLEAR : lv_discounted.
      lw_msg-custno     = <fs>-custno.
      lw_msg-invoiceno  = <fs>-invoiceno.
      READ TABLE lt_ewaybill TRANSPORTING NO FIELDS WITH KEY docno = <fs>-invoiceno BINARY SEARCH.
      IF sy-subrc = 0.
****************check the credit balance for the customer - open item***************
        READ TABLE lt_bsid TRANSPORTING NO FIELDS WITH KEY vbeln = <fs>-invoiceno.
        IF sy-subrc = 0.
          PERFORM : credit_balchk.
********************if the discounted flag 'X' means continue the loop*************
          IF lv_discounted = 'X'.
            CONTINUE.
          ENDIF.
        ELSE.
************************check the cleared items*****************
          READ TABLE lt_bsad TRANSPORTING NO FIELDS WITH KEY vbeln = <fs>-invoiceno BINARY SEARCH.
          IF sy-subrc = 0.
            <fs>-status          = '20'.
            <fs>-inv_approvedby  = sy-uname.
            <fs>-inv_approvedon  = sy-datum.
            <fs>-inv_approvedat  = sy-uzeit.
            <fs>-sheenlacremarks = 'EwayBill generated & cleared items discounted'.
            lw_msg-status        = 'EwayBill generated & cleared items discounted'.
            APPEND lw_msg TO lt_msg.
            MODIFY zsd_sf_cust_inv FROM <fs>.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.
*********************if credit bal is less than the inv amount*************
        <fs>-status = '12'.
        <fs>-inv_file_identifier = |{ <fs>-invoicekey }.pdf|.
        lw_msg-status  = 'EwayBill is found. Status is updated'.

*****************************For SF customer inv*******************
        DATA(l_set_print) = VALUE lbbil_print_data_to_read(  hd_gen = abap_true
                                                             hd_adr = abap_true
                                                             hd_gen_descript = abap_true
                                                             hd_org = abap_true
                                                             hd_part_add = abap_true
                                                             hd_kond = abap_true
                                                             hd_fin = abap_true
                                                             hd_ref = abap_true
                                                             hd_tech = abap_true
                                                             it_gen = abap_true
                                                             it_adr = abap_true
                                                             it_price = abap_true
                                                             it_kond = abap_true
                                                             it_ref = abap_true
                                                             it_refdlv = abap_true
                                                             it_reford = abap_true
                                                             it_refpurord = abap_true
                                                             it_refvag = abap_true
                                                             it_refvg2 = abap_true
                                                             it_refvkt = abap_true
                                                             it_tech = abap_true
                                                             it_fin = abap_true
                                                             it_confitm = abap_true
                                                             it_confbatch = abap_true
                                                             msr_hd = abap_true
                                                             msr_it = abap_true ).
        DATA(lv_objkey) = CONV nast-objky( <fs>-invoiceno ).
        DATA ls_bil_invoice    TYPE lbbil_invoice.
        DATA ls_docpara TYPE sfpdocparams.
        DATA ls_outpara TYPE sfpoutputparams.
        DATA ls_output  TYPE fpformoutput.
        DATA  ls_frmname TYPE fpname.
        DATA: lv_fm    TYPE rs38l_fnam,
              i_objbin TYPE STANDARD TABLE OF solix,
              l_app1   TYPE string.
        CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
          EXPORTING
            if_bil_number         = lv_objkey
            is_print_data_to_read = l_set_print
            if_parvw              = 'RE'
            if_parnr              = <fs>-custno
            if_language           = sy-langu
          IMPORTING
            es_bil_invoice        = ls_bil_invoice
          EXCEPTIONS
            records_not_found     = 1
            records_not_requested = 2
            OTHERS                = 3.
        IF sy-subrc = 0.
          ls_outpara-getpdf = abap_true.

          CALL FUNCTION 'FP_JOB_OPEN'
            CHANGING
              ie_outputparams = ls_outpara
            EXCEPTIONS
              cancel          = 1
              usage_error     = 2
              system_error    = 3
              internal_error  = 4
              OTHERS          = 5.

          CLEAR: lv_fm,ls_frmname.
          ls_frmname = 'ZSD_IRN_QR_EINVOICE_V1'.

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

          DATA(lv_pdf) = CONV xstring( ls_output-pdf ).

*********************only SF customer****************
          IF <fs>-fintype = 'SF'.

            IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
              CONCATENATE '/sapmnt/Sundaram/Document' '/' <fs>-invoicekey '.pdf' INTO l_app1.
            ELSE.
              CONCATENATE '/sapmnt/PRD/Sundaram/Document' '/' <fs>-invoicekey '.pdf' INTO l_app1.
            ENDIF.

*          pdf_content = cl_document_bcs=>xstring_to_solix( lv_pdf  ). "pdf_bin
            OPEN DATASET l_app1 FOR OUTPUT IN BINARY MODE.
            IF sy-subrc = 0.
*            LOOP AT pdf_content INTO DATA(lw_pdf).
*              TRANSFER lw_pdf TO l_app1.
              TRANSFER lv_pdf TO l_app1.
*            ENDLOOP.
              CLOSE DATASET l_app1.
            ENDIF.
*********************only RF customer****************
          ELSEIF <fs>-fintype = 'RF'.
*****************fill the base64****************
            CLEAR lv_string.
            CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
              EXPORTING
                input  = lv_pdf
              IMPORTING
                output = lv_string.

            <fs>-inv_base64 = lv_string.
          ENDIF.

        ENDIF.
      ELSE.
        lw_msg-status  = 'EwayBill is not found.'.
      ENDIF.
      APPEND lw_msg TO lt_msg.
    ENDLOOP.

    DELETE lt_cust_inv WHERE status NE '12'.
    IF lt_cust_inv IS NOT INITIAL.
      MODIFY zsd_sf_cust_inv FROM TABLE lt_cust_inv .
    ENDIF.

    DATA: lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = lt_msg.
      CATCH cx_salv_msg.
    ENDTRY.
    lo_gr_alv->display( ).
  ELSE.
    MESSAGE 'No records found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


FORM credit_balchk.
  DATA : l_total_amt  TYPE wrbtr,
         lv_ref       TYPE string,
         lv_rounddown TYPE i.
********************Calculate the credit balance*****************
  CLEAR : l_total_amt,lv_ref.
  LOOP AT lt_bsid INTO DATA(lw_openitem) WHERE kunnr = <fs>-custno
                                         AND   budat LE <fs>-invoicedate
                                         AND   shkzg = 'H'. " H-Credit amount from Sub-Dealers
    l_total_amt = l_total_amt + lw_openitem-dmbtr.
*****************concatenate the refernce documents***********
    lv_ref = |{ lv_ref } / { lw_openitem-blart } - { lw_openitem-belnr } - { lw_openitem-gjahr } - { lw_openitem-dmbtr }|.
    CLEAR : lw_openitem.
  ENDLOOP.

  IF l_total_amt IS NOT INITIAL.

*********************round down the credit balance amount************
    CLEAR : lv_rounddown.
    lv_rounddown = floor( l_total_amt ).
    l_total_amt  = lv_rounddown.
*****************update the refernce document for the invoice table************
    <fs>-refdoc_crbal = lv_ref.
******************check the credit balance amount with invoice amount.
    LOOP AT lt_bsid INTO lw_openitem WHERE kunnr EQ <fs>-custno
                                     AND   budat LE <fs>-invoicedate
                                     AND   shkzg EQ 'S'.
      IF l_total_amt GE lw_openitem-dmbtr.
        l_total_amt = l_total_amt - lw_openitem-dmbtr.
**********************fetch the document number for already cd given or net***************
*      SELECT SINGLE * FROM zsd_sf_cust_inv INTO @DATA(ls_cust_inv)
*                                           WHERE  invoiceno EQ @lw_openitem-vbeln
*                                           AND    custno    EQ @<fs>-custno
*                                           AND    status    NE '10'.
*      IF sy-subrc = 0.
*        CONTINUE.
*      ELSE.
        IF <fs>-invoiceno = lw_openitem-vbeln.
          <fs>-befr_crbal = l_total_amt + lw_openitem-dmbtr.
          <fs>-aftr_crbal = l_total_amt.
          <fs>-status     = '20'.
          <fs>-inv_approvedby  = sy-uname.
          <fs>-inv_approvedon  = sy-datum.
          <fs>-inv_approvedat  = sy-uzeit.
          <fs>-sheenlacremarks = 'EwayBill generated & credit balance discounted'.
          lw_msg-status        = 'EwayBill generated & credit balance discounted'.
          APPEND lw_msg TO lt_msg.
          MODIFY zsd_sf_cust_inv FROM <fs>.
          IF sy-subrc = 0.
            COMMIT WORK.
          ENDIF.
********************Enable the discounted flag for continue the loop*************
          lv_discounted = 'X'.
          EXIT.
        ENDIF.
*      ENDIF.
      ELSE.
        IF <fs>-invoiceno = lw_openitem-vbeln.
          <fs>-befr_crbal      = l_total_amt.
          <fs>-dueamount       = <fs>-invoiceamount - l_total_amt.
          <fs>-sheenlacremarks = 'EwayBill generated & credit balance partially discounted'.
          lw_msg-status        = 'EwayBill generated & credit balance partially discounted'.
          CLEAR : l_total_amt.
          EXIT.
        ELSE.
          CLEAR : l_total_amt.
          EXIT.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.
