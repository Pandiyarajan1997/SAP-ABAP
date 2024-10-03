class ZCL_PURCHASE_ORDER_CREATE definition
  public
  final
  create public .

public section.

  types:
    zmm_po_item_ty   TYPE STANDARD TABLE OF bapimepoitem .
  types:
    zmm_po_header_ty TYPE STANDARD TABLE OF bapimepotext .
  types:
    zmm_po_itemtxt_ty TYPE STANDARD TABLE OF bapimepotext .

  data ZMM_PO_ITEM_TT type ZMM_PO_ITEM_TY .
  data ZMM_PO_HEADER_TXT type ZMM_PO_HEADER_TY .
  data ZMM_PO_ITEM_TXT type ZMM_PO_ITEMTXT_TY .

  methods CREATE_PURCHASE_ORDER
    importing
      !POHEADER type BAPIMEPOHEADER
      !POHEADERX type BAPIMEPOHEADERX
      !ITEM_TAB like ZMM_PO_ITEM_TT
      !HEADERTEXT like ZMM_PO_HEADER_TXT optional
      !ITEMTEXT like ZMM_PO_ITEM_TXT optional
    exporting
      !PONUMBER type EBELN
      !RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PURCHASE_ORDER_CREATE IMPLEMENTATION.


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
    DATA: lv_ponumber TYPE ebeln.
    DATA: lv_item TYPE ebelp.
    DATA: lv_text TYPE string.

**------- Header DATA for purchase order preparation-------------------*
    DATA(ls_header) = VALUE bapimepoheader( comp_code  = poheader-comp_code
                                            doc_type   = poheader-doc_type
                                            creat_date = poheader-creat_date
                                            vendor     = poheader-vendor
                                            langu      = poheader-langu
                                            pmnttrms   = poheader-pmnttrms
                                            purch_org  = poheader-purch_org
                                            pur_group  = poheader-pur_group
                                            currency   = poheader-currency
                                            doc_date   = poheader-doc_date
                                            suppl_plnt = poheader-suppl_plnt  ) .

    DATA(ls_headerx) = VALUE bapimepoheaderx( comp_code  = abap_true
                                              doc_type   = abap_true
                                              creat_date = abap_true
                                              vendor     = abap_true
                                              langu      = abap_true
                                              pmnttrms   = abap_true
                                              purch_org  = abap_true
                                              pur_group  = abap_true
                                              currency   = abap_true
                                              doc_date   = abap_true
                                              suppl_plnt = abap_true ) .

**------- lineitem DATA for purchase order preparation-------------------*
    IF item_tab IS NOT INITIAL.
      REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex,lt_itemtext.
      CLEAR lv_item.
      lv_item = '00010'.
      LOOP AT item_tab ASSIGNING FIELD-SYMBOL(<fs_item>).
        IF <fs_item>-tax_code IS NOT INITIAL.
          DATA(lv_taxcode) = <fs_item>-tax_code.
          DATA(l_tax_flag) = abap_true.
        ENDIF.
        APPEND VALUE #( po_item    = lv_item
                        material   = <fs_item>-material
                        plant      = <fs_item>-plant
                        quantity   = <fs_item>-quantity
                        po_unit    = <fs_item>-po_unit
                        unlimited_dlv = ''
                        tax_code   = lv_taxcode
                      ) TO lt_poitem.

        APPEND VALUE #( po_item       = lv_item
                        po_itemx      = abap_true
                        material      = abap_true
                        plant         = abap_true
                        quantity      = abap_true
                        po_unit       = abap_true
                        tax_code      = l_tax_flag
                        unlimited_dlv = abap_true
                      ) TO lt_poitemx.

        APPEND VALUE #( po_item  = lv_item
                        quantity = <fs_item>-quantity
                       ) TO lt_poschedule.

        APPEND VALUE #( po_item  = lv_item
                        po_itemx = abap_true
                        quantity = abap_true
                       ) TO lt_poschedulex.

*        APPEND VALUE #( po_item   = lv_item
*                        text_id   = 'F01'
*                        text_form = '*'
*                        text_line = <fs_item>-batch
*                       ) TO lt_itemtext.
*        lv_item = lv_item + 10.

      ENDLOOP.
    ENDIF.
**------- Header Text for Reference ---------*
*    REFRESH lt_headertext.
    IF headertext IS NOT INITIAL.
      CLEAR lv_text.
      LOOP AT headertext INTO DATA(l_text).
        lv_text = |{ lv_text }{ l_text-text_line }|.
      ENDLOOP.
      APPEND VALUE #(  text_id = 'F03'
                       text_form = '*'
                       text_line = lv_text ) TO lt_headertext.
    ENDIF.

*
*** Function module which creates new service purchase order ---------------*
    CLEAR: lv_ponumber,return.
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_header
        poheaderx        = ls_headerx
      IMPORTING
        exppurchaseorder = lv_ponumber
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
      ponumber = lv_ponumber.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
