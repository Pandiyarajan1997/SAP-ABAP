class ZCL_ENTRYSHEET_TO_INVOICE definition
  public
  final
  create public .

public section.

  methods CREATE_ENTRYSHEET
    importing
      !PO_NUMBER type EBELN
      !PO_ITEMNO type EBELP
      !VENDOR_INV type XBLNR1 optional
    exporting
      !ENTRYSHEET type BAPIESSR-SHEET_NO
      !RETURN type BAPIRET2_T .
  methods MIRO_INVOICE
    importing
      !PO_NUMBER type EBELN
      !PO_ITEMNO type EBELP
    exporting
      !INVOICE_NO type BELNR_D
      !FISYEAR type GJAHR
      !RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ENTRYSHEET_TO_INVOICE IMPLEMENTATION.


  METHOD create_entrysheet.
    DATA: lw_poheader     TYPE bapiekkol, "PO Header Details
          lt_poitems      TYPE TABLE OF bapiekpo, "PO Item Details
          lt_poitems_acc  TYPE TABLE OF bapiekkn, "PO Item Account Assignment
          lt_poschedules  TYPE TABLE OF bapieket, "PO Item Schedules
          lt_itemtext     TYPE TABLE OF bapiekpotx, "PO Itemwise Text
          lt_services     TYPE TABLE OF bapiesll,  "PO Service Lineitems
          lt_services_acc TYPE TABLE OF bapieskl, "PO Service Accounts
          lt_return       TYPE TABLE OF bapireturn. "Return Table

    IF po_number IS NOT INITIAL AND po_itemno IS NOT INITIAL .
      CLEAR: lw_poheader.
      REFRESH: lt_poitems,lt_poitems_acc,lt_poschedules,lt_itemtext,lt_services,lt_services_acc,lt_return.
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder              = po_number
          items                      = 'X'
          account_assignment         = 'X'
          schedules                  = 'X'
          item_texts                 = 'X'
          header_texts               = 'X'
          services                   = 'X'
          confirmations              = 'X'
          service_texts              = 'X'
        IMPORTING
          po_header                  = lw_poheader
        TABLES
          po_items                   = lt_poitems
          po_item_account_assignment = lt_poitems_acc
          po_item_schedules          = lt_poschedules
          po_item_texts              = lt_itemtext
          po_item_services           = lt_services
          po_item_srv_accass_values  = lt_services_acc
          return                     = lt_return.

    ENDIF.
  ENDMETHOD.


  method MIRO_INVOICE.

  endmethod.
ENDCLASS.
