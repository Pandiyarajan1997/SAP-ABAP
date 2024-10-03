class ZCL_API_ASNMIGO2MIRO definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_input,
        vendor  TYPE lifnr,
        del_no  TYPE zasnno,
        docdate TYPE bldat,
        posdate TYPE budat,
        ref_doc TYPE char50,
      END OF ty_input .
  types:
    gt_bdcdata_ty TYPE STANDARD TABLE OF bdcdata .
  types:
    gt_bdcmsgcoll_ty TYPE STANDARD TABLE OF bdcmsgcoll .

  methods MIRO_INVOICE
    importing
      !DEL_NO type ZASNNO
      !DOCDATE type BLDAT
      !POSDATE type BUDAT
      !REF_DOC type CHAR50
    exporting
      !MIRONO type BELNR_D
      !FSLYR type GJAHR
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_API_ASNMIGO2MIRO IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    "Created by: Puratchiveeran
    "Created on: 15.05.2024
    "Reference by: Ramakrishnan J
    "Purpose : MIRO for DMS Process
*-------------------------------------------------------------*

    TYPES:
      BEGIN OF ty_input,
        vendor  TYPE lifnr,
        del_no  TYPE zasnno,
        docdate TYPE bldat,
        posdate TYPE budat,
        ref_doc TYPE char50,
      END OF ty_input .

    TYPES: BEGIN OF ty_msg,
             vendor  TYPE lifnr,
             del_no  TYPE zasnno,
             docdate TYPE bldat,
             posdate TYPE budat,
             ref_doc TYPE char50,
             mirono  TYPE char10,
             fslyr   TYPE GJAHR,
             type    TYPE bapi_mtype,
             message TYPE string,
           END OF ty_msg.

    DATA : ls_response TYPE ty_msg.
    DATA : gs_input TYPE ty_input.

    DATA: lv_body TYPE string.
    DATA: lv_data TYPE string.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    CLEAR : gs_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).
***************input alpha conversion************
    gs_input-vendor = |{ gs_input-vendor ALPHA = IN }|.
*    gs_input-del_no  = |{ gs_input-del_no ALPHA = IN }|.
****************contains the all validations process****************
    CLEAR : ls_response.
    CALL METHOD me->miro_invoice
      EXPORTING
        del_no  = gs_input-del_no
        docdate = gs_input-docdate
        posdate = gs_input-posdate
        ref_doc = gs_input-ref_doc
      IMPORTING
        mirono  = DATA(l_mirono)
        fslyr   = DATA(l_fslyr)
        type    = DATA(l_type)
        message = DATA(l_msg).

***************fill the response********
    IF ls_response-type IS INITIAL.
      ls_response-type = 'E'.
    ENDIF.
    MOVE-CORRESPONDING gs_input TO ls_response.
    ls_response-type = l_type.
    ls_response-message = l_msg.
    ls_response-mirono = l_mirono.
    ls_response-fslyr = l_fslyr.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'ASNMIGO_MIRO'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = gs_input-vendor
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
*    ENDIF.

  ENDMETHOD.


  METHOD miro_invoice.
*--------------------------------------------------------------------------*
    DATA: lw_header TYPE bapi_incinv_create_header.
    DATA: lt_item TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lw_item TYPE bapi_incinv_create_item.
    DATA: l_invno TYPE bapi_incinv_fld-inv_doc_no,
          l_fyear TYPE bapi_incinv_fld-fisc_year.
    DATA: lv_gross_amt TYPE bapi_rmwwr.
    DATA: lv_tcs TYPE bapi_rmwwr.
    DATA: lv_item TYPE rblgp.
    DATA l_ref_itno TYPE lfpos.
    DATA:e_fwnav TYPE bset-fwste,
         e_fwnvv TYPE bset-fwste,
         e_fwste TYPE bset-fwste,
         e_fwast TYPE bset-fwste.
*    DATA: lv_uom  TYPE t006-msehi,
*          lv_flag TYPE isofields-unique.
    DATA: lt_caltax TYPE TABLE OF rtax1u15.
    DATA: l_return TYPE bapiret2.
    DATA: lv_msg   TYPE string.
    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: po_header       TYPE bapiekkol,
          lt_po_htxt      TYPE TABLE OF bapiekkotx,
          lt_po_lineitems TYPE TABLE OF bapiekpo,
          lt_ret          TYPE TABLE OF bapireturn,
          lv_inv          TYPE belnr_d.

    SELECT SINGLE mblnr FROM zmm_ge_asn INTO @DATA(l_migono) WHERE asnno = @del_no.
    IF sy-subrc = 0.
      SELECT a~ebeln AS po_number,
             a~ebelp AS po_item,
             a~mblnr,
             a~mjahr,
             a~zeile,
             a~matnr,
             a~werks,
             a~lgort,
             a~charg,
             a~dmbtr AS net_value,
             a~menge AS quantity,
             a~meins AS unit,
             b~mwskz AS tax_code FROM mseg AS a
            INNER JOIN ekpo AS b ON a~ebeln = b~ebeln AND
                                    a~ebelp = b~ebelp
            INTO TABLE @DATA(lt_po)
            WHERE mblnr = @l_migono.

      lv_item = '000001'.
      SORT lt_po by po_number po_item.
      LOOP AT lt_po INTO DATA(lw_po_lineitems).
        l_ref_itno = l_ref_itno + 1.
        APPEND VALUE #( invoice_doc_item = lv_item
                        po_number        = lw_po_lineitems-po_number
                        po_item          = lw_po_lineitems-po_item
                        tax_code         = lw_po_lineitems-tax_code
                        quantity         = lw_po_lineitems-quantity
                        item_amount      = lw_po_lineitems-net_value
                        po_unit          = lw_po_lineitems-unit
                        ref_doc          = lw_po_lineitems-mblnr " XBLNR_MKPF
                        ref_doc_year     = lw_po_lineitems-mjahr
                        ref_doc_it       = lw_po_lineitems-zeile ) TO lt_item.
        lv_item = lv_item + 1.
        CLEAR lw_po_lineitems.
      ENDLOOP.

**--- Tax calculation based on tax code ---*
      DATA:lv_titem TYPE taxps.
      CLEAR lv_titem.
      lv_titem = '000001'.
      LOOP AT lt_item INTO lw_item.
        CLEAR: e_fwnav,e_fwnvv,e_fwste,e_fwast,lt_caltax.
        DATA(lv_amount) = CONV wrbtr( lw_item-item_amount ).
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = 'DMS1'
            i_mwskz           = lw_item-tax_code
            i_waers           = 'INR'
            i_wrbtr           = lv_amount
          IMPORTING
            e_fwnav           = e_fwnav
            e_fwnvv           = e_fwnvv
            e_fwste           = e_fwste
            e_fwast           = e_fwast
          TABLES
            t_mwdat           = lt_caltax
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            tdt_error         = 13
            txa_error         = 14
            OTHERS            = 15.
        IF sy-subrc = 0.
          DATA(lv_sgst) = VALUE #( lt_caltax[ kschl = 'JISG' ]-wmwst OPTIONAL ).
          DATA(lv_cgst) = VALUE #( lt_caltax[ kschl = 'JICG' ]-wmwst OPTIONAL ).
          DATA(lv_igst) = VALUE #( lt_caltax[ kschl = 'JIIG' ]-wmwst OPTIONAL ).
        ENDIF.
        lv_gross_amt = lv_gross_amt + lw_item-item_amount + lv_cgst + lv_sgst + lv_igst.
        CLEAR : lw_item,lv_sgst,lv_cgst,lv_igst,lw_item,lv_amount.
      ENDLOOP.
*---- Invoice Header Data -------*
      CLEAR lw_header.
      lw_header-invoice_ind    = 'X'.
      lw_header-doc_date       = docdate. "Enter the document date
      lw_header-pstng_date     = posdate. "Enter the posting date
      lw_header-ref_doc_no     = ref_doc.
*      lw_header-ref_doc_no_long = del_no.
      lw_header-comp_code      = '1000'.
      lw_header-gross_amount   = lv_gross_amt.  "Enter the gross amount(aft. tax) for the invoice
      lw_header-calc_tax_ind   = 'X'.
      lw_header-business_place = 'TN01'.
      lw_header-currency       = 'INR'.
*--- Function Module to Create Incoming Invoice -----*
      CLEAR: l_fyear,l_invno,lt_return.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
        EXPORTING
          headerdata       = lw_header
        IMPORTING
          invoicedocnumber = l_invno
          fiscalyear       = l_fyear
        TABLES
          itemdata         = lt_item
          return           = lt_return.

      IF lt_return IS INITIAL.
        CLEAR l_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = l_return.

        mirono = l_invno.
        fslyr  = l_fyear.
        message = |ASN MIGO Invoice Created|.
        type    = 'S'.

      ELSE.
        LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
          lv_msg = | { lw_return-message } , { lv_msg } |.
        ENDLOOP.
        message = lv_msg.
        type    = 'E'.
      ENDIF.
    ELSE.
      message = 'Delivery note number is not matched with ASN No'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
