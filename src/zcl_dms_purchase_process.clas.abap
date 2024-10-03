class ZCL_DMS_PURCHASE_PROCESS definition
  public
  final
  create public .

public section.

  types:
    gt_bdcdata_ty TYPE STANDARD TABLE OF bdcdata .
  types:
    gt_bdcmsgcoll_ty TYPE STANDARD TABLE OF bdcmsgcoll .

  data GW_BDCMSG type BDCMSGCOLL .
  data GT_BDCDATA type GT_BDCDATA_TY .
  data GT_BDCMSGCOLL type GT_BDCMSGCOLL_TY .

  methods INFORECORD_PROCESS
    importing
      !DATA type ZDMS_INFORECORD_ST
      !FLAG type FLAG optional
    exporting
      !MSG_TYPE type BAPI_MTYPE
      !MSG type STRING .
  methods CREATE_PURCHASE_ORDER
    importing
      !LIFNR type LIFNR
      !KUNNR type KUNNR
      !VBELN type VBELN_VF
      !TAXCODE type MWSKZ
      !FLAG type FLAG optional
      !ITEM_TAB type ZDMS_POITEM_TT optional
    exporting
      !PO_NUMBER type EBELN
      !RETURN type BAPIRET2_T .
  methods GOODS_RECIEPT
    importing
      !MOVEMENT_TYPE type BWART
      !PURCHASE_ORDER type EBELN
    exporting
      !MBLNR type MBLNR
      !MJAHR type MJAHR
      !RETURN type BAPIRET2_T .
  methods MIRO_INVOICE
    importing
      !PURCHASE_ORDER type EBELN
    exporting
      !INVOICEDOCNUMBER type BELNR_D
      !FISCALYEAR type GJAHR
      !RETURN type BAPIRET2_T .
protected section.
private section.

  methods FILL_DATA_FROM_DELIVERY
    importing
      !VBELN type VBELN_VF
    exporting
      !ITEM type ZDMS_POITEM_TT .
ENDCLASS.



CLASS ZCL_DMS_PURCHASE_PROCESS IMPLEMENTATION.


  METHOD create_purchase_order.
    "Created by: Samsudeen M
    "Created on: 15.03.2023
    "Reference by: Ramakrishnan J
    "Purpose : Create Purchase order for DMS Process
*-------------------------------------------------------------*
    DATA: lt_poitem      TYPE TABLE OF bapimepoitem,
          lt_poitemx     TYPE TABLE OF bapimepoitemx,
          lt_poschedule  TYPE TABLE OF bapimeposchedule,
          lt_poschedulex TYPE TABLE OF bapimeposchedulx,
          lt_account     TYPE TABLE OF bapimepoaccount,
          lt_accountx    TYPE TABLE OF bapimepoaccountx,
          lt_headertext  TYPE TABLE OF bapimepotext,
          lt_itemtext    TYPE TABLE OF bapimepotext,
          lt_return      TYPE bapiret2_t.
    DATA: ponumber TYPE ebeln.
    DATA: lv_item TYPE ebelp.

*---- Getting Delivery Details for Which the PO has to be created ----*
    IF vbeln IS NOT INITIAL.
      CALL METHOD me->fill_data_from_delivery
        EXPORTING
          vbeln = vbeln
        IMPORTING
          item  = DATA(lt_item).
    ENDIF.
*------- Header DATA for purchase order preparation-------------------*
    IF lifnr IS NOT INITIAL.
      DATA(l_lifnr) = CONV elifn( |{ lifnr ALPHA = IN }| ).
      SELECT SINGLE * FROM lfb1 INTO @DATA(ls_lfb1) WHERE lifnr = @l_lifnr.
      IF sy-subrc = 0.
        DATA(l_payterms) = ls_lfb1-zterm.
      ENDIF.

      DATA(ls_header) = VALUE bapimepoheader( comp_code = 'DMS1'
                                              doc_type = 'ZDMS'
                                              creat_date = sy-datum
                                              vendor = l_lifnr
                                              langu = sy-langu
                                              pmnttrms = l_payterms
                                              purch_org = 'DMS1'
                                              pur_group = 'DMS'
                                              currency = 'INR'
                                              doc_date = sy-datum ) .

      DATA(ls_headerx) = VALUE bapimepoheaderx( comp_code = abap_true
                                                doc_type = abap_true
                                                creat_date = abap_true
                                                vendor = abap_true
                                                langu = abap_true
                                                pmnttrms = abap_true
                                                purch_org = abap_true
                                                pur_group = abap_true
                                                currency = abap_true
                                                doc_date = abap_true ) .
    ENDIF.
*------- lineitem DATA for purchase order preparation-------------------*
    IF lt_item IS NOT INITIAL.

      REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex,lt_itemtext.
      CLEAR lv_item.
      lv_item = '00010'.
      IF kunnr IS NOT INITIAL.
        DATA(l_distributor) = |{ kunnr ALPHA = IN }|.
        SELECT SINGLE werks FROM kna1 INTO @DATA(lv_werks) WHERE kunnr = @l_distributor.
        IF sy-subrc = 0.

        ENDIF.
      ENDIF.
      "Distributor Stock inward
      IF flag IS NOT INITIAL AND item_tab IS NOT INITIAL.
        DATA(lt_item_tab) = lt_item[].
        SORT lt_item_tab[] BY vbeln material erdat DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_item_tab[] COMPARING vbeln material.
        REFRESH lt_item.
        LOOP AT item_tab ASSIGNING FIELD-SYMBOL(<fs_item_tab>).
          DATA(ls_batch) = VALUE #( lt_item_tab[ vbeln    = <fs_item_tab>-vbeln
                                                 material = <fs_item_tab>-material ] OPTIONAL ).
          <fs_item_tab>-matl_group = ls_batch-matl_group.
          <fs_item_tab>-batch      = ls_batch-batch.
        ENDLOOP.
        lt_item[] = item_tab[].
      ENDIF.

      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
        APPEND VALUE #( po_item    = lv_item
                        material   = <fs_item>-material
                        plant      = lv_werks
                        matl_group = <fs_item>-matl_group
                        quantity   = <fs_item>-quantity
                        po_unit    = <fs_item>-po_unit
                        tax_code   = taxcode
                        unlimited_dlv = ''
                      ) TO lt_poitem.

        APPEND VALUE #( po_item       = lv_item
                        po_itemx      = abap_true
                        material      = abap_true
                        plant         = abap_true
                        matl_group    = abap_true
                        quantity      = abap_true
                        po_unit       = abap_true
                        tax_code      = abap_true
                        unlimited_dlv = abap_true
                      ) TO lt_poitemx.

        APPEND VALUE #( po_item  = lv_item
                        quantity = <fs_item>-quantity
                       ) TO lt_poschedule.

        APPEND VALUE #( po_item  = lv_item
                        po_itemx = abap_true
                        quantity = abap_true
                       ) TO lt_poschedulex.

        APPEND VALUE #( po_item   = lv_item
                        text_id   = 'F01'
                        text_form = '*'
                        text_line = <fs_item>-batch
                       ) TO lt_itemtext.
        lv_item = lv_item + 10.

      ENDLOOP.
    ENDIF.
*------- Header Text for Reference ---------*
    REFRESH lt_headertext.
    APPEND VALUE #(  text_id = 'F03'
                     text_form = '*'
                     text_line = vbeln ) TO lt_headertext.

*** Function module which creates new service purchase order ---------------*
    CLEAR: ponumber,return.
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_header
        poheaderx        = ls_headerx
      IMPORTING
        exppurchaseorder = ponumber
      TABLES
        return           = lt_return
        poitem           = lt_poitem
        poitemx          = lt_poitemx
        poschedule       = lt_poschedule
        poschedulex      = lt_poschedulex
        potextheader     = lt_headertext
        potextitem       = lt_itemtext.
    LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
      APPEND lw_return TO return.
    ENDLOOP.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      po_number = ponumber.
    ENDIF.
  ENDMETHOD.


  METHOD fill_data_from_delivery.
    "Created by: Samsudeen M
    "Created On: 15.03.2023
    "Reference by: Ramakrishnan J & Gopalraja
    "Purpose: Preparing Data for Creating Purchase Order Based on Delivery Data of Invoices
*-------------------------------------------------------------------------------------------*
    IF vbeln IS NOT INITIAL.
*----- Fetching Invoice Details based on Input -------*
      SELECT a~vbeln,
             a~fkdat,
             b~posnr,
             b~matnr,
             b~vgbel,
             b~aubel,
             b~aupos INTO TABLE @DATA(lt_invoice)
                     FROM vbrk AS a
                     INNER JOIN vbrp AS b
                     ON a~vbeln = @vbeln
                     AND a~vbeln = b~vbeln
                     WHERE fksto NE 'X'.
      IF sy-subrc = 0.
        SORT lt_invoice[] BY vbeln posnr.
*------ Delivery Data of Invoices Details -------*
        SELECT vbeln,
               posnr,
               erdat,
               matnr,
               lgort,
               charg,
               lfimg,
               vrkme,
               matkl,
               vgpos FROM lips INTO TABLE @DATA(lt_delivery)
                     FOR ALL ENTRIES IN @lt_invoice
                     WHERE vbeln = @lt_invoice-vgbel
                     AND bwart = '601'
                     AND vgbel = @lt_invoice-aubel
                     AND vgpos = @lt_invoice-aupos
                     AND charg NE @space
                     AND lfimg NE 0.
        IF sy-subrc = 0.
          SORT lt_delivery[] BY vbeln matnr.
        ENDIF.
      ENDIF.
      LOOP AT lt_delivery INTO DATA(lw_delivery).
        APPEND VALUE #( material = lw_delivery-matnr
                        matl_group = lw_delivery-matkl
                        quantity = lw_delivery-lfimg
                        po_unit = lw_delivery-vrkme
                        batch = lw_delivery-charg
                        erdat = lw_delivery-erdat
                        vbeln = lw_delivery-vbeln ) TO item.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD goods_reciept.
    "Created by: Samsudeen M
    "Cretaed on: 15.03.2023
    "Reference : Ramakrishnan J & Gopalraja
    "Purpose: Goods Reciept for DMS Distributor
*-------------------------------------------------------------------------*
    DATA: lw_goodsmvt_header TYPE bapi2017_gm_head_01,
          lt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lw_goodsmvt_item   TYPE bapi2017_gm_item_create,
          lt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: lw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code,
          l_return            TYPE bapiret2.
    DATA: po_header       TYPE bapiekkol,
          lt_po_htxt      TYPE TABLE OF bapiekkotx,
          lt_po_lineitems TYPE TABLE OF bapiekpo,
          lt_item_text    TYPE TABLE OF bapiekpotx,
          lt_ret          TYPE TABLE OF bapireturn.
    DATA: lv_item TYPE i.
*--- Function Module to get Purchase Order Details ----*
    CLEAR: po_header,lt_ret,lt_po_lineitems.
    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = purchase_order
        item_texts      = 'X'
      IMPORTING
        po_header       = po_header
      TABLES
        po_header_texts = lt_po_htxt
        po_items        = lt_po_lineitems
        po_item_texts   = lt_item_text
        return          = lt_ret.
    IF lt_ret IS INITIAL.
      DELETE lt_item_text WHERE text_line = space.
*------- Material Document Which Means Goods Receipt -----------*
*-- Header DATA for grn PROCESS ----*
      CLEAR: lw_goodsmvt_header.
      goodsmvt_code_tmp               = '01'.
      lw_goodsmvt_header-pstng_date   = sy-datum.
      lw_goodsmvt_header-doc_date     = sy-datum.
      lw_goodsmvt_header-pr_uname     = sy-uname.
      lw_goodsmvt_header-header_txt   = VALUE #( lt_po_htxt[ 1 ]-text_line OPTIONAL ).
      lw_goodsmvt_header-bill_of_lading = po_header-po_number.
      lw_goodsmvt_header-ref_doc_no     = po_header-po_number.
*---- Material Document Item Data -----*
      REFRESH lt_goodsmvt_item.
      LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).
        DATA(lv_batch) = VALUE #( lt_item_text[ po_number = lw_po_lineitems-po_number
                                                po_item = lw_po_lineitems-po_item
                                                text_id = 'F01' ]-text_line OPTIONAL ).

        APPEND VALUE #( move_type            = movement_type "Movement Type
                        mvt_ind              = 'B'
                        plant                = lw_po_lineitems-plant "Plant
                        material             = lw_po_lineitems-material "Material
                        entry_qnt            = lw_po_lineitems-quantity "Batch Number "PO Quantity OR GRN Quantity
                        move_stloc           = 'D1' "Storage Location
                        stge_loc             = 'D1' "Storage Location
                        batch                = lv_batch "Batch Number
                        po_number            = lw_po_lineitems-po_number "PO Number
                        po_item              = lw_po_lineitems-po_item  "PO Item Number
                        ) TO lt_goodsmvt_item.
      ENDLOOP.

      REFRESH lt_return.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lw_goodsmvt_header
          goodsmvt_code    = goodsmvt_code_tmp
        IMPORTING
          goodsmvt_headret = lw_goodsmvt_headret
        TABLES
          goodsmvt_item    = lt_goodsmvt_item
          return           = lt_return.
      LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
        APPEND lw_return TO return.
      ENDLOOP.
      IF sy-subrc NE 0.
        CLEAR l_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = l_return.
        IF l_return IS INITIAL.
          mblnr = lw_goodsmvt_headret-mat_doc. "Material Document Number Or GRN Number
          mjahr = lw_goodsmvt_headret-doc_year. "Material Document Year
        ENDIF.
      ENDIF.
    ELSE.
      APPEND LINES OF lt_ret TO return.
    ENDIF.
  ENDMETHOD.


  METHOD inforecord_process.
    "Created by: Samsudeen M
    "Created on: 15.03.2023
    "Reference: Ramakrishnan J & Gopalraja
    "Purpose: Info record Creation and Updation for DMS Purchasing
*----------------------------------------------------------------------*
    DATA: data_tab TYPE zdms_inforecord_st.

    "Inforecord Creation
    IF flag NE abap_true.
      CALL FUNCTION 'ZDMS_INFORECORD_CREATION'
        EXPORTING
          im_input = data
        IMPORTING
          msg_type = msg_type
          msg      = msg.
    ELSE."Inforecord Updation
      CALL FUNCTION 'ZDMS_INFORECORD_UPDATION'
        EXPORTING
          im_input = data
        IMPORTING
          msg_type = msg_type
          msg      = msg.
    ENDIF.
  ENDMETHOD.


  METHOD miro_invoice.
    "Created by: Samsudeen m
    "Created on: 15.03.2023
    "Reference : Ramakrishnan J & Gopalraja
    "Purpose : Creating Invoice Document (MIRO) based on Purchase Order
*--------------------------------------------------------------------------*
    DATA: lw_header TYPE bapi_incinv_create_header.
    DATA: lt_item TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lw_item TYPE bapi_incinv_create_item.
    DATA: l_invno TYPE bapi_incinv_fld-inv_doc_no,
          l_fyear TYPE bapi_incinv_fld-fisc_year.
    DATA: lv_gross_amt TYPE bapi_rmwwr.
    DATA: lv_tcs TYPE bapi_rmwwr.
    DATA: lv_item TYPE rblgp.
    DATA:e_fwnav TYPE bset-fwste,
         e_fwnvv TYPE bset-fwste,
         e_fwste TYPE bset-fwste,
         e_fwast TYPE bset-fwste.
    DATA: lv_uom  TYPE t006-msehi,
          lv_flag TYPE isofields-unique.
    DATA: lt_caltax TYPE TABLE OF rtax1u15.
    DATA: l_return TYPE bapiret2.
    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: po_header       TYPE bapiekkol,
          lt_po_htxt      TYPE TABLE OF bapiekkotx,
          lt_po_lineitems TYPE TABLE OF bapiekpo,
          lt_ret          TYPE TABLE OF bapireturn.

    IF purchase_order IS NOT INITIAL.

      CLEAR: po_header,lt_ret,lt_po_lineitems,lt_po_htxt.
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = purchase_order
        IMPORTING
          po_header       = po_header
        TABLES
          po_header_texts = lt_po_htxt
          po_items        = lt_po_lineitems
          return          = lt_ret.
      IF lt_ret IS INITIAL.
        REFRESH: lt_item.
        CLEAR: lv_gross_amt,lv_item.
        lv_item = '000001'.
        DATA(lv_taxcode) = VALUE #( lt_po_lineitems[ 1 ]-tax_code OPTIONAL ).
        LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).
          APPEND VALUE #( invoice_doc_item = lv_item
                          po_number = lw_po_lineitems-po_number
                          po_item = lw_po_lineitems-po_item
                          tax_code = lw_po_lineitems-tax_code
                          quantity = lw_po_lineitems-quantity
                          item_amount = lw_po_lineitems-net_value
                          po_unit = lw_po_lineitems-unit ) TO lt_item.
          lv_item = lv_item + 1.
          CLEAR lw_po_lineitems.
        ENDLOOP.
*--- Tax calculation based on tax code ---*
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
          CLEAR lw_item.
        ENDLOOP.
        "If these Two tax code then only TCS calculation
        IF lv_taxcode = 'H5' OR lv_taxcode = 'H6'.
          CLEAR lv_tcs.
          lv_tcs = ( lv_gross_amt * '0.1' / 100 ).
          lv_gross_amt = lv_gross_amt + lv_tcs.
        ENDIF.

*---- Invoice Header Data -------*
        CLEAR lw_header.
        lw_header-invoice_ind = 'X'.
        lw_header-doc_date = sy-datum.   "Enter the document date
        lw_header-pstng_date = sy-datum. "Enter the posting date
        lw_header-ref_doc_no = VALUE #( lt_po_htxt[ 1 ]-text_line OPTIONAL ).
        lw_header-comp_code = 'DMS1'.
        lw_header-gross_amount = lv_gross_amt.  "Enter the gross amount(aft. tax) for the invoice
        lw_header-calc_tax_ind = 'X'.
        lw_header-business_place = 'TN01'.
        lw_header-currency = 'INR'.
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
        LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
          APPEND lw_return TO return.
        ENDLOOP.
        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          invoicedocnumber = l_invno.
          fiscalyear = l_fyear.
        ENDIF.
      ELSE.
        LOOP AT lt_ret INTO DATA(lw_ret).
          CLEAR l_return.
          MOVE-CORRESPONDING lw_ret TO l_return.
          APPEND l_return TO return.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
