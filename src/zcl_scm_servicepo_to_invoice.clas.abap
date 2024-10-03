class ZCL_SCM_SERVICEPO_TO_INVOICE definition
  public
  final
  create public .

public section.

  methods CREATE_ENTRYSHEET
    importing
      !EBELN type EBELN
      !EBELP type EBELP
      !DOC_DATE type BLDAT
      !POST_DATE type BUDAT
    exporting
      !ENTRYSHEET type LBLNI
      !RETURN type BAPIRET2_T .
  methods CREATE_MIRO_INVOICE
    importing
      !EBELN type EBELN
      !EBELP type EBELP
      !ENTRYSHEET_NO type LBLNI optional
      !DOC_DATE type BLDAT
      !POST_DATE type BUDAT
      !REF_DOC_NO type XBLNR optional
      !ATTACH1 type XSTRING optional
      !ATTACH2 type XSTRING optional
      !ATTACH3 type XSTRING optional
    exporting
      !INVOCIE_NO type BELNR_D
      !GJAHR type GJAHR
      !RETURN type BAPIRET2_T .
  methods PO_CHANGE
    importing
      !EBELN type EBELN
      !EBELP type EBELP
      !ADDN_QTY type MENGE_D optional
      !ADDN_CHARGES type NETWR optional
      !TEXT type TXZ01 optional
    exporting
      !SUCCESS type FLAG
      !RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SCM_SERVICEPO_TO_INVOICE IMPLEMENTATION.


  METHOD create_entrysheet.
*Created by: Samsudeen M
*Created On: 03.052023
*Purpose : Creating Service Entry Sheet Number Based on Service Purchase Order
*Reference: Gopalraja & Ramakrishnan J
*-------------------------------------------------------------------------------*
    DATA: lv_package   TYPE num15.
*    "Entry sheet Data Declaration
    DATA: ls_header     TYPE bapiessrc,
          l_return      TYPE TABLE OF bapireturn,
          lt_return     TYPE STANDARD TABLE OF bapiret2,
          lt_service    TYPE STANDARD TABLE OF bapiesllc,
          lv_entrysheet TYPE bapiessr-sheet_no.

    IF ebeln IS NOT INITIAL AND ebeln IS NOT INITIAL.
*purchase Order details
      SELECT SINGLE * FROM ekpo INTO @DATA(l_polineitem) WHERE ebeln = @ebeln
                                                         AND ebelp = @ebelp.
      IF sy-subrc = 0.
*Purchase Order Account Assignment Details
        SELECT SINGLE * FROM ekkn INTO @DATA(l_poaccount) WHERE ebeln = @ebeln
                                                          AND ebelp = @ebelp.
        IF sy-subrc = 0.
*Main Package Number
          SELECT SINGLE * FROM esll INTO @DATA(l_packdtls) WHERE packno = @l_polineitem-packno.
          IF sy-subrc = 0.
*Sub Package Number
            SELECT * FROM esll INTO TABLE @DATA(lt_subpackdtls) WHERE packno = @l_packdtls-sub_packno.
          ENDIF.
        ENDIF.
      ENDIF.
* populate header
      CLEAR ls_header.
      ls_header-po_number  = l_polineitem-ebeln.
      ls_header-po_item = l_polineitem-ebelp.
      ls_header-short_text = l_polineitem-txz01.
      ls_header-pckg_no  = lv_package.
      ls_header-acceptance =  'X'.
*      ls_header-fin_entry = 'X'.
      ls_header-location = l_polineitem-txz01.
      ls_header-doc_date   = doc_date.
      ls_header-post_date  = post_date.
      ls_header-pckg_no = l_packdtls-packno.
      DATA(lv_long_txt) = CONV text40( l_polineitem-txz01 ).
      ls_header-ref_doc_no_long = lv_long_txt.
      DATA(lv_subpckg_no) = l_packdtls-packno + 1.
      REFRESH: lt_service.
      APPEND VALUE #( pckg_no    = l_packdtls-packno
                      line_no    = l_packdtls-introw
                      outl_level = '0'
                      outl_ind   = abap_true
                      subpckg_no = lv_subpckg_no ) TO lt_service.

      LOOP AT lt_subpackdtls ASSIGNING FIELD-SYMBOL(<fs_subpack>).
        APPEND VALUE #( pckg_no    = <fs_subpack>-packno
                        line_no    = <fs_subpack>-introw
                        ext_line   = <fs_subpack>-extrow
                        pln_pckg   = <fs_subpack>-packno
                        pln_line   = <fs_subpack>-introw
                        quantity   = <fs_subpack>-menge
                        short_text = <fs_subpack>-ktext1
                        base_uom   = <fs_subpack>-meins
                        gr_price   = <fs_subpack>-netwr ) TO lt_service.
        lv_subpckg_no = lv_subpckg_no + 1.
      ENDLOOP.

      CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
        EXPORTING
          entrysheetheader   = ls_header
        IMPORTING
          entrysheet         = entrysheet
        TABLES
          entrysheetservices = lt_service
          return             = lt_return.
      IF entrysheet IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        LOOP AT lt_return INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
          APPEND lw_ret TO return.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_miro_invoice.
*Created by: Samsudeen M
*Created On: 03.052023
*Purpose : Creating Service Entry Sheet Number Based on Service Purchase Order
*Reference: Gopalraja & Ramakrishnan J
*-------------------------------------------------------------------------------
    "MIRO Invoice Declaration
    DATA: lw_inv_header TYPE bapi_incinv_create_header,
          lt_inv_item   TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lt_acc_item   TYPE STANDARD TABLE OF bapi_incinv_create_account,
          w_acc         TYPE bapi_incinv_create_account,
          lt_withtax    TYPE TABLE OF bapi_incinv_create_withtax,
          lw_inv_item   TYPE bapi_incinv_create_item,
          l_invno       TYPE bapi_incinv_fld-inv_doc_no,
          l_fyear       TYPE bapi_incinv_fld-fisc_year,
          lt_return     TYPE TABLE OF bapiret2,
          lv_gross_amt  TYPE bapi_rmwwr.
    DATA:e_fwnav TYPE bset-fwste,
         e_fwnvv TYPE bset-fwste,
         e_fwste TYPE bset-fwste,
         e_fwast TYPE bset-fwste.
    DATA: lv_tax TYPE wrbtr.
    DATA: lt_caltax TYPE TABLE OF rtax1u15.

    IF ebeln IS NOT INITIAL AND ebelp IS NOT INITIAL.
      SELECT SINGLE lifnr FROM ekko
        INTO @DATA(lv_lifnr)
        WHERE ebeln = @ebeln.
*purchase Order details
      SELECT SINGLE * FROM ekpo
        INTO @DATA(l_polineitem)
        WHERE ebeln = @ebeln
        AND ebelp = @ebelp.
*  Start Changes Delete lines by Puratchi
      CHECK sy-subrc = 0.
*      IF sy-subrc = 0.
**Purchase Order Account Assignment Details
*        SELECT SINGLE * FROM ekkn
*          INTO @DATA(l_poaccount)
*          WHERE ebeln = @ebeln
*          AND ebelp = @ebelp.
* GRN Details
*
*        SELECT SINGLE ven_class FROM lfa1 INTO @DATA(l_regstat)
*          WHERE lifnr = @lv_lifnr.
*        IF sy-subrc = 0.
*          CASE l_regstat.
*            WHEN ''.
**GST Registered Vendors
*              SELECT SINGLE bplace FROM zmm_regven_txbp
*                INTO @DATA(l_bplace)
*                WHERE vendor = @lv_lifnr.
**GST Non Registered Vendors
*            WHEN OTHERS.
*              SELECT SINGLE regio FROM t001w INTO @DATA(l_from_region)
*                WHERE werks = @l_polineitem-werks.
*              IF sy-subrc = 0.
*                SELECT SINGLE bplace FROM zmm_serpo_tax_bp INTO  l_bplace
*                  WHERE from_region = l_from_region.
*              ENDIF.
*          ENDCASE.
*        ENDIF.
**service lineitems
*        DATA(lv_subpackno) = CONV packno( l_polineitem-packno + 1 ).
*        SELECT * FROM esll
*          INTO TABLE @DATA(lt_seritems)
*          WHERE packno = @lv_subpackno.
*        IF sy-subrc = 0.
*
*        ENDIF.
*      ENDIF.
***MIRO Invoice Process Starts after Entrysheet
**      REFRESH: lt_inv_item,lt_caltax,lt_acc_item.
**      APPEND VALUE #( invoice_doc_item = lv_invitem
**                      po_number        = l_polineitem-ebeln
**                      po_item          = l_polineitem-ebelp
**                      tax_code         = l_polineitem-mwskz
***                      quantity         = l_polineitem-menge
**                      item_amount      = l_polineitem-netwr
***                      po_unit          = l_polineitem-meins
**                      sheet_no         = entrysheet_no
**                      sheet_item       = '10' ) TO lt_inv_item.
**Purchase Order Account Assignment Details
*  End Delete lines by Puratchi
*Tax Calculation Based Purchase Order Tax code
      CLEAR: e_fwnav,e_fwnvv,e_fwste,e_fwast,lt_caltax.
      DATA(lv_amount) = CONV wrbtr( l_polineitem-netwr ).
      IF l_polineitem-mwskz EQ 'PG' OR l_polineitem-mwskz EQ 'PE' OR l_polineitem-mwskz EQ 'PH'.
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = l_polineitem-bukrs
            i_mwskz           = l_polineitem-mwskz
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
          lv_gross_amt = lv_amount + lv_cgst + lv_sgst + lv_igst.
        ENDIF.
      ELSE.
        lv_gross_amt = lv_amount + lv_tax.
      ENDIF.

*Tcs Calculation
      SELECT SINGLE * FROM ekko
        INTO @DATA(l_vendor)
        WHERE ebeln = @l_polineitem-ebeln.
      IF sy-subrc = 0.
*Tcs Tax Code
        SELECT SINGLE * FROM lfbw
          INTO @DATA(l_tcs_code)
          WHERE lifnr = @l_vendor-lifnr
          AND bukrs = @l_vendor-bukrs
          AND wt_subjct = @abap_true.
        IF sy-subrc = 0.
*Tcs Tax rate
          SELECT SINGLE * FROM t059z
            INTO @DATA(l_tcs_rate)
            WHERE land1 = 'IN'
            AND witht = @l_tcs_code-witht
            AND wt_withcd = @l_tcs_code-wt_withcd.
          IF sy-subrc = 0.
            REFRESH lt_withtax.
            DATA(l_tds) = VALUE bapi_incinv_create_withtax( split_key    = '000001'
                                                            wi_tax_type  = l_tcs_code-witht
                                                            wi_tax_code  = l_tcs_code-wt_withcd ).
*                                                            wi_tax_base  = l_polineitem-netwr   ). " Deleted by Puratchi
            APPEND l_tds TO lt_withtax.
          ENDIF.
        ENDIF.
      ENDIF.

*  Start Add lines by Puratchi
      SELECT SINGLE ven_class FROM lfa1 INTO @DATA(l_regstat)
        WHERE lifnr = @lv_lifnr.
      IF sy-subrc = 0.
        CASE l_regstat.
          WHEN ''.
*GST Registered Vendors
            SELECT SINGLE bplace FROM zmm_regven_txbp
              INTO @DATA(l_bplace)
              WHERE vendor = @lv_lifnr.
*GST Non Registered Vendors
          WHEN OTHERS.
            SELECT SINGLE regio FROM t001w INTO @DATA(l_from_region)
              WHERE werks = @l_polineitem-werks.
            IF sy-subrc = 0.
              SELECT SINGLE bplace FROM zmm_serpo_tax_bp INTO  l_bplace
                WHERE from_region = l_from_region.
            ENDIF.
        ENDCASE.
      ENDIF.
      DATA: lit_po_acct   TYPE STANDARD TABLE OF bapieskn,
            lit_serpo_acc TYPE STANDARD TABLE OF bapiesll,
            lit_ser_acc   TYPE STANDARD TABLE OF bapieskl,
            w_header      TYPE bapiessr.
      CALL FUNCTION 'BAPI_ENTRYSHEET_GETDETAIL'
        EXPORTING
          entrysheet                    = entrysheet_no
*         LONG_TEXTS                    = ' '
        IMPORTING
          entrysheet_header             = w_header
        TABLES
          entrysheet_account_assignment = lit_po_acct
          entrysheet_services           = lit_serpo_acc
          entrysheet_srv_accass_values  = lit_ser_acc.

      DATA: l_item_no TYPE extrow.
      DATA: lv_invitem TYPE rblgp.

      LOOP AT lit_po_acct INTO DATA(lw_poaccount) .
        lv_invitem = lv_invitem + 1.
        l_item_no = l_item_no + 10.

        APPEND VALUE #( invoice_doc_item = lv_invitem
                        po_number        = l_polineitem-ebeln
                        po_item          = l_polineitem-ebelp
                        item_amount      = lw_poaccount-accass_val
                        sheet_no         = entrysheet_no
                        sheet_item       = l_item_no
                        item_text        = VALUE #( lit_serpo_acc[ ext_line = l_item_no ]-short_text OPTIONAL )
                        tax_code         = l_polineitem-mwskz ) TO lt_inv_item.
*                          quantity         = VALUE #( lit_serpo_acc[ ext_line = l_item_no ]-quantity OPTIONAL )
*                          po_unit          = VALUE #( lit_serpo_acc[ ext_line = l_item_no ]-base_uom OPTIONAL )
        MOVE-CORRESPONDING lw_poaccount TO w_acc.
        w_acc-invoice_doc_item = lv_invitem.
        w_acc-item_amount      = lw_poaccount-accass_val.
        APPEND w_acc TO lt_acc_item.
      ENDLOOP.
* End Add lines by Puratchi

*Invoice Header Data Population
      CLEAR lw_inv_header.
      lw_inv_header-invoice_ind = 'X'.
      lw_inv_header-doc_date = doc_date.   "Enter the document date
      lw_inv_header-pstng_date = post_date. "Enter the posting date
      lw_inv_header-ref_doc_no = ref_doc_no.
      lw_inv_header-item_text = |Frieght Charges for { l_polineitem-txz01 }|.
      lw_inv_header-comp_code = l_polineitem-bukrs.
      lw_inv_header-gross_amount = lv_gross_amt.  "Enter the gross amount(aft. tax) for the invoice
      lw_inv_header-calc_tax_ind = 'X'.
      lw_inv_header-business_place = l_bplace.
      lw_inv_header-currency = 'INR'.
*Function Module to Create Invoice
      CLEAR: l_fyear,l_invno,lt_return.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
        EXPORTING
          headerdata       = lw_inv_header
        IMPORTING
          invoicedocnumber = invocie_no
          fiscalyear       = gjahr
        TABLES
          itemdata         = lt_inv_item
          accountingdata   = lt_acc_item
          withtaxdata      = lt_withtax
          return           = lt_return.
      LOOP AT lt_return INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
        APPEND lw_ret TO return.
      ENDLOOP.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        DATA: lt_attachment TYPE STANDARD TABLE OF tbl1024,
              lv_xstring    TYPE xstring,
              l_awkey       TYPE awkey,
              l_line        TYPE n.
        DATA: lv_out TYPE i.
        DATA: lv_objectid TYPE sapb-sapobjid.
        DATA: lv_objid  TYPE sapb-sapobjid.
        DATA: lv_archiv_id  TYPE saearchivi.
        DATA: ls_toadt TYPE toadt.
        DATA: lv_archid TYPE toav0-archiv_id.
        DATA: lv_filename TYPE toaat-filename,
              l_descr     TYPE toaat-descr.

        l_awkey = |{ invocie_no }{ gjahr }|.
        SELECT SINGLE belnr
          FROM bkpf INTO @DATA(l_belnr)
          WHERE bukrs = @l_polineitem-bukrs
            AND awkey = @l_awkey.
        DO 3 TIMES.
          CLEAR: lt_attachment[],lv_xstring.
          l_line = l_line + 1.
          CASE l_line.
            WHEN 1.
              lv_xstring = attach1.
            WHEN 2.
              lv_xstring = attach2.
            WHEN 3.
              lv_xstring = attach3.
            WHEN OTHERS.
          ENDCASE.
          IF lv_xstring IS INITIAL.
            CONTINUE.
          ENDIF.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = lv_xstring
            IMPORTING
              output_length = lv_out
            TABLES
              binary_tab    = lt_attachment.
          IF sy-subrc = 0.
            DESCRIBE TABLE lt_attachment LINES DATA(lv_lines).
            DATA(length) = CONV sapb-length( lv_lines * 1024 ).
            "Archiv Connection get
            CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
              EXPORTING
                objecttype    = 'BKPF'
                documenttype  = 'ZVENINV'
                client        = sy-mandt
              IMPORTING
                archivid      = lv_archiv_id
              EXCEPTIONS
                nothing_found = 1
                OTHERS        = 2.
            IF sy-subrc = 0.
              "Archiv Create Table function Module
              CLEAR: lv_objid,ls_toadt,lv_objectid.
              lv_objectid = |{ l_belnr }{ l_polineitem-bukrs }{ gjahr }|.
              lv_objid = |{ l_polineitem-bukrs }{ l_belnr }{ gjahr }|.

              CONDENSE lv_objid NO-GAPS.
              l_descr = |{ invocie_no }_Attachment{ l_line }|.
              lv_filename = |{ l_descr }.pdf|.
              CALL FUNCTION 'ARCHIV_CREATE_TABLE'
                EXPORTING
                  ar_object                = 'ZVENINV'
                  object_id                = lv_objectid
                  sap_object               = 'BKPF'
                  flength                  = length "File size
                  filename                 = lv_filename
                  descr                    = l_descr
                IMPORTING
                  outdoc                   = ls_toadt "Return table, decisive parameter ARC_DOC_ID
                TABLES
                  "archiveobject = lt_archiveobj
                  binarchivobject          = lt_attachment
                EXCEPTIONS
                  error_archive            = 01
                  error_communicationtable = 02
                  error_connectiontable    = 03
                  error_kernel             = 04
                  error_parameter          = 05
                  OTHERS                   = 06.
              IF sy-subrc = 0.

                CLEAR lv_archid.
                lv_archid = lv_archiv_id.
                CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
                  EXPORTING
                    arc_doc_id            = ls_toadt-arc_doc_id
                    ar_object             = 'ZVENINV'
                    object_id             = lv_objid
                    sap_object            = 'BKPF'
                  EXCEPTIONS
                    error_connectiontable = 1
                    OTHERS                = 2.
                IF sy-subrc <> 0.
                  APPEND VALUE #( type = 'E'
                                  number = 'Vendor Invoice uploading Error'
                                  message  = 'Vendor Invoice uploading Error' ) TO return .
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD po_change.
*Created By: Samsudeen m
*Created On: 05.05.2023
*Purpose : If Additional Charges there for the Services this is for changing the Purchase Order
*Reference By: SCM team & Suresh B.V & Ramakrishnan & Gopalraja
*------------------------------------------------------------------------------------------------*
    DATA: lt_poitem      TYPE TABLE OF bapimepoitem,
          lt_poitemx     TYPE TABLE OF bapimepoitemx,
          lt_poschedule  TYPE TABLE OF bapimeposchedule,
          lt_poschedulex TYPE TABLE OF bapimeposchedulx,
          lt_account     TYPE TABLE OF bapimepoaccount,
          lt_accountx    TYPE TABLE OF bapimepoaccountx,
          lt_services    TYPE TABLE OF bapiesllc,
          lt_poservices  TYPE TABLE OF bapiesklc,
          lt_itemtext    TYPE TABLE OF bapimepotext,
          lt_return      TYPE bapiret2_t.
    DATA: header  TYPE bapimepoheader,
          headerx TYPE bapieikp.
    DATA: ls_header  TYPE bapimepoheader,
          ls_headerx TYPE bapimepoheaderx.
    DATA: lv_uom        TYPE meins,
          lv_sno        TYPE i VALUE '01',
          lv_fund(10)   TYPE c,
          lv_qty        TYPE bstmg,
          lv_costcenter TYPE kostl.

    IF ebeln IS NOT INITIAL AND ebelp IS NOT INITIAL.
*Purchase Order Details
      SELECT SINGLE * FROM ekko
        INTO @DATA(l_podtls)
        WHERE ebeln = @ebeln.
      IF sy-subrc = 0.
*Purchase Order Lineitem Details
        SELECT SINGLE * FROM ekpo
          INTO @DATA(l_polineitem)
          WHERE ebeln = @ebeln
          AND ebelp = @ebelp.
        IF sy-subrc = 0.
*Purchase Order Account Details
          SELECT * FROM ekkn
            INTO TABLE @DATA(l_poaccount)
            WHERE ebeln = @ebeln
            AND ebelp = @ebelp.
        ENDIF.
      ENDIF.
*Main Package Lineitem
      SELECT SINGLE * FROM esll
        INTO @DATA(l_packdtls)
        WHERE packno = @l_polineitem-packno.
      IF sy-subrc = 0.
        DATA(lv_packno) = l_packdtls-packno.
        DATA(lv_subpackno) = CONV packno( l_packdtls-packno + 1 ).
*Sub Package Lineitems
        SELECT packno,
               introw,
               extrow,
               menge,
               meins,
               netwr,
               ktext1 FROM esll
                      INTO TABLE @DATA(lt_esll)
                      WHERE packno = @lv_subpackno.
        IF sy-subrc = 0.
          SORT lt_esll[] BY extrow DESCENDING.
        ENDIF.
      ENDIF.
*Populating Header Details
      CLEAR: ls_header,ls_headerx.
      ls_header = VALUE bapimepoheader( comp_code  = l_podtls-bukrs
                                        doc_type   = l_podtls-bsart
                                        creat_date = l_podtls-aedat
                                        vendor     = l_podtls-lifnr
                                        langu      = sy-langu
                                        pmnttrms   = l_podtls-zterm
                                        purch_org  = l_podtls-ekorg
                                        pur_group  = l_podtls-ekgrp
                                        currency   = l_podtls-waers
                                        doc_date   = l_podtls-bedat ) .

      ls_headerx = VALUE bapimepoheaderx( comp_code  = abap_true
                                          doc_type   = abap_true
                                          creat_date = abap_true
                                          vendor     = abap_true
                                          langu      = abap_true
                                          pmnttrms   = abap_true
                                          purch_org  = abap_true
                                          pur_group  = abap_true
                                          currency   = abap_true
                                          doc_date   = abap_true ) .
*Populating the Data to Purchase Order Function Module
      REFRESH: lt_poitem,lt_poitemx,lt_poschedule,lt_poschedulex,lt_account,lt_accountx,lt_services,lt_poservices,lt_itemtext.
      CLEAR: lv_qty,lv_uom.
      lv_qty = l_polineitem-menge.
      lv_uom = l_polineitem-meins.
*PO item Table
      APPEND VALUE #( po_item       = l_polineitem-ebelp
                      short_text    = l_polineitem-txz01
                      plant         = l_polineitem-werks
                      matl_group    = l_polineitem-matkl
                      quantity      = lv_qty
                      po_unit       = lv_uom
                      net_price     = l_polineitem-netwr
                      tax_code      = l_polineitem-mwskz
                      unlimited_dlv = 'X'
                      item_cat      = '9'
                      acctasscat    = 'F'
                      pckg_no       = lv_packno
                      srv_based_iv  = '' ) TO lt_poitem.
*PO item Update table
      APPEND VALUE #( po_item       = l_polineitem-ebelp
                      po_itemx      = abap_true
                      short_text    = abap_true
                      plant         = abap_true
                      matl_group    = abap_true
                      quantity      = abap_true
                      po_unit       = abap_true
                      net_price     = abap_true
                      tax_code      = abap_true
                      unlimited_dlv = abap_true
                      item_cat      = abap_true
                      acctasscat    = abap_true
                      pckg_no       = abap_true
                      srv_based_iv  = abap_true ) TO lt_poitemx.
* FUND Calculations
      CLEAR: lv_fund.
      CALL FUNCTION 'ZFUND_GET'
        EXPORTING
          date       = l_podtls-bedat
        IMPORTING
          lv_fund    = lv_fund
        EXCEPTIONS
          enter_date = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
      ENDIF.

*      LOOP AT l_poaccount INTO DATA(ls_acnt).
**Account Assignment Details
*        APPEND VALUE #( po_item    = l_polineitem-ebelp
*                        serial_no  = ls_acnt-zekkn
*                        quantity   = lv_qty
*                        gl_account = ls_acnt-sakto
**                      gl_account = '0046989907'
*                        costcenter = ls_acnt-kostl
*                        fund       = lv_fund ) TO lt_account.
*
*        APPEND VALUE #( po_item     = l_polineitem-ebelp
*                        serial_no   = ls_acnt-zekkn
*                        po_itemx    = abap_true
*                        serial_nox  = abap_true
*                        quantity    = abap_true
*                        gl_account  = abap_true
*                        costcenter  = abap_true
*                        fund        = abap_true ) TO lt_accountx.
*      ENDLOOP.

*Main Package Service Lineitem
      IF l_packdtls IS NOT INITIAL.
        APPEND VALUE #( pckg_no    = lv_packno
                        line_no    = l_packdtls-introw
                        outl_level = '0'
                        subpckg_no = lv_subpackno  ) TO lt_services.
      ENDIF.
*Conversion for Unit of Measure
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lv_uom
          language       = sy-langu
        IMPORTING
          output         = lv_uom
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.

      ENDIF.
**Sub Package Lineitems
*      LOOP AT lt_esll ASSIGNING FIELD-SYMBOL(<fls_esll>).
*        APPEND VALUE #( pckg_no    = <fls_esll>-packno
*                        line_no    = <fls_esll>-introw
*                        ext_line   = <fls_esll>-extrow
*                        outl_level = '0'
*                        outl_ind   = abap_true
*                        short_text = <fls_esll>-ktext1
*                        quantity   = <fls_esll>-menge
*                        base_uom   = lv_uom
*                        gr_price   = <fls_esll>-netwr
*                        pln_pckg   = lv_packno
*                        pln_line   = <fls_esll>-introw  ) TO lt_services.
*
*        APPEND VALUE #( pckg_no    = lv_subpackno
*                        line_no    = <fls_esll>-introw
*                        percentage = '100'
*                        serno_line = lv_sno
*                        serial_no  = lv_sno
*                        quantity   = <fls_esll>-menge
*                        net_value  = <fls_esll>-netwr ) TO lt_poservices.
*
*      ENDLOOP.
*Getting Last Highest line Number & External Number  of Service lineitem
      IF addn_charges IS NOT INITIAL.

        IF lt_esll IS NOT INITIAL.
          DATA(lc_extno) = VALUE extrow( lt_esll[ 1 ]-extrow OPTIONAL ).
          DATA(lc_intno) = VALUE introw( lt_esll[ 1 ]-introw OPTIONAL ).
          READ TABLE lt_esll INTO DATA(ls_esll) INDEX 1.
        ENDIF.
        IF l_poaccount IS NOT INITIAL.
          READ TABLE l_poaccount INTO DATA(ls_acnt) INDEX 1.
          CLEAR : lv_costcenter.
          lv_costcenter = ls_acnt-kostl.
          SORT l_poaccount BY zekkn DESCENDING.
          CLEAR ls_acnt.
          READ TABLE l_poaccount INTO ls_acnt INDEX 1.
        ENDIF.
* Adding Count
        lc_extno = lc_extno + 10.
        lc_intno = lc_intno + 1.
        ls_acnt-zekkn   = ls_acnt-zekkn + 1.
        APPEND VALUE #( pckg_no    = ls_esll-packno
                        line_no    = lc_intno
                        ext_line   = lc_extno
                        outl_level = '0'
                        outl_ind   = abap_true
                        short_text = text
                        quantity   = addn_qty
                        base_uom   = lv_uom
                        gr_price   = addn_charges
                        MATL_GROUP = l_polineitem-matkl ) TO lt_services.

        APPEND VALUE #( pckg_no    = lv_subpackno
                        line_no    = lc_intno
                        percentage = '100'
*                        serno_line = ls_acnt-zekkn
                        serial_no  = ls_acnt-zekkn
                        quantity   = addn_qty
                        net_value  = addn_charges ) TO lt_poservices.

*Account Assignment Details
        APPEND VALUE #( po_item    = l_polineitem-ebelp
                        serial_no  = ls_acnt-zekkn
                        quantity   = lv_qty
                        gl_account = '0046989907'
                        costcenter = lv_costcenter
                        fund       = lv_fund ) TO lt_account.

        APPEND VALUE #( po_item     = l_polineitem-ebelp
                        serial_no   = ls_acnt-zekkn
                        po_itemx    = abap_true
                        serial_nox  = abap_true
                        quantity    = abap_true
                        gl_account  = abap_true
                        costcenter  = abap_true
                        fund        = abap_true ) TO lt_accountx.

      ENDIF.
      SORT lt_services[] BY ext_line ASCENDING.
      SORT lt_poservices[] BY  line_no ASCENDING.
*------ Function module which amends the po lineitems ---------------------------*
      CLEAR: header,headerx,return.
      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder     = ebeln
        IMPORTING
          expheader         = header
          exppoexpimpheader = headerx
        TABLES
          return            = lt_return
          poitem            = lt_poitem
          poitemx           = lt_poitemx
          poaccount         = lt_account
          poaccountx        = lt_accountx
          poservices        = lt_services
          posrvaccessvalues = lt_poservices.

      READ TABLE lt_return INTO DATA(lw_ret)  WITH KEY type = 'E'.
      IF sy-subrc = 0.
        LOOP AT return INTO lw_ret WHERE type = 'E' OR type = 'A'.
          APPEND lw_ret TO return.
        ENDLOOP.
      ELSE.
        success = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
