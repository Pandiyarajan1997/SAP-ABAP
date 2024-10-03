FUNCTION zmm_service_po_maintain.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(POHEADER) TYPE  BAPIMEPOHEADER
*"     VALUE(PO_FLAG) TYPE  FLAG
*"     VALUE(PONUMBER) TYPE  EBELN OPTIONAL
*"  TABLES
*"      LT_ITEM STRUCTURE  BAPIMEPOITEM
*"      LT_ACCITEM STRUCTURE  BAPIMEPOACCOUNT
*"      LT_MESSAGES TYPE  ZMM_API_LOG_TT
*"      LT_ITEMTEXT STRUCTURE  BAPIMEPOTEXT
*"      LT_CUST STRUCTURE  ZST_AUTOSR_PO_CUST
*"----------------------------------------------------------------------
  "Created_by: Samsudeen M
  "Created_on: 27.01.2023
  "Reference: Ramakrishnan J
  "Purpose: Auto Service PO Creation based on API Input
  "Requirement: Purchase Department
*-----------------------------------------------------------------------
  DATA: lv_qty         TYPE bstmg,
        lv_value       TYPE bapicurext,
        lv_pckg_no     TYPE packno,
        lv_sub_pckg_no TYPE packno,
        lv_uom         TYPE meins,
        lv_sno         TYPE i.
  DATA: lt_poitem      TYPE TABLE OF bapimepoitem,
        lt_poitemx     TYPE TABLE OF bapimepoitemx,
        lt_poschedule  TYPE TABLE OF bapimeposchedule,
        lt_poschedulex TYPE TABLE OF bapimeposchedulx,
        lt_account     TYPE TABLE OF bapimepoaccount,
        lt_accountx    TYPE TABLE OF bapimepoaccountx,
        lt_services    TYPE TABLE OF bapiesllc,
        lt_poservices  TYPE TABLE OF bapiesklc,
        lt_itemtxt     TYPE TABLE OF bapimepotext,
        return         TYPE bapiret2_t.
  DATA: ls_header  TYPE bapimepoheader,
        ls_headerx TYPE bapimepoheaderx.
  DATA: header  TYPE bapimepoheader,
        headerx TYPE bapieikp.
  DATA: po_number TYPE ebeln.
  DATA: lv_msg TYPE string.
  DATA: lc_line_no TYPE introw VALUE '0000000001',
        c_ext_line TYPE extrow VALUE '0000000010'.
*------Process starts from here ---------------------------------------*
  DATA(lv_poitem) = VALUE #( lt_item[ 1 ]-po_item OPTIONAL ).


  TYPES: BEGIN OF ty_rel,
           frggr TYPE ekko-frggr,    "Release Group
           frgco TYPE ekko-frgsx,    "Release Code
           ebeln TYPE ekko-ebeln,    "Purch Doc No.
         END OF ty_rel.
  DATA: lt_rel TYPE TABLE OF ty_rel,
        ls_rel TYPE ty_rel.

  lv_value = VALUE #( lt_item[ 1 ]-net_price OPTIONAL ).
  REFRESH: lt_messages.
  "Item Details for New PO Creation
  REFRESH: lt_poitem,lt_poitemx,lt_poschedule,lt_poschedulex,lt_account,lt_accountx,lt_itemtxt.
  MOVE-CORRESPONDING lt_item[] TO lt_poitem.
  READ TABLE lt_poitem ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX 1.
  IF sy-subrc EQ 0.
    lv_pckg_no        = <fs_item>-pckg_no.
    lv_pckg_no        = lv_pckg_no + 1.
    <fs_item>-pckg_no = lv_pckg_no.
    lv_sub_pckg_no    = lv_pckg_no + 1.
  ENDIF.

  APPEND VALUE #( po_item       = lv_poitem
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
                  pckg_no       = abap_true ) TO lt_poitemx.
  APPEND VALUE #( po_item       = lv_poitem
                  delivery_date = sy-datum
                  quantity      = '1'    ) TO lt_poschedule.
  APPEND VALUE #( po_item       = lv_poitem
                  po_itemx      = abap_true
                  delivery_date = abap_true
                  quantity      = abap_true ) TO lt_poschedulex.
  "Account Details for New Purchase Order Creation
  MOVE-CORRESPONDING lt_accitem[] TO lt_account.
  LOOP AT lt_account INTO DATA(lw_account).
    APPEND VALUE #( po_item       = lv_poitem
                    serial_no     = lw_account-serial_no
                    po_itemx      = abap_true
                    serial_nox    = abap_true
                    quantity      = abap_true
                    net_value     = abap_true
                    gl_account    = abap_true
                    costcenter    = abap_true
                    fund          = abap_true ) TO lt_accountx.

  ENDLOOP.
  APPEND VALUE #( pckg_no       = lv_pckg_no
                  line_no       = lc_line_no
                  outl_level    = '0'
                  subpckg_no    = lv_sub_pckg_no  ) TO lt_services.
  lv_uom = |AU|.

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
  DATA lv_string TYPE string.
  LOOP AT lt_account INTO lw_account.
    lv_string = |{ lt_poitem[ 1 ]-short_text }-{ lt_cust[ costcenter = lw_account-costcenter ]-to_plant }{ lt_cust[ costcenter = lw_account-costcenter ]-customer }|.
    APPEND VALUE #( pckg_no = lv_sub_pckg_no
                    line_no    = lc_line_no
                    ext_line = c_ext_line
                    outl_level = '0'
                    outl_ind = abap_true
                    short_text = lv_string
                    quantity = '1'
                    base_uom = lv_uom
                    gr_price = lw_account-net_value
                    net_value = lw_account-net_value   ) TO lt_services.

    APPEND VALUE #( pckg_no = lv_sub_pckg_no
                    line_no    = lc_line_no
                    percentage = '100'
                    serno_line  = c_ext_line+8(2)
                    serial_no = lw_account-serial_no
                    quantity = '1'
                    net_value = lw_account-net_value ) TO lt_poservices.
    lc_line_no = lc_line_no + 1.
    c_ext_line = c_ext_line + 10.
  ENDLOOP.

  MOVE-CORRESPONDING lt_itemtext[] TO lt_itemtxt[].


  CASE po_flag.
*Creating New Purchase Order
    WHEN 'N'.
      "Header Details for New Purchase Order
      ls_header = CORRESPONDING #( BASE ( ls_header ) poheader ).
      ls_headerx = VALUE bapimepoheaderx( comp_code = abap_true
                                          doc_type = abap_true
                                          creat_date = abap_true
                                          vendor = abap_true
                                          langu = abap_true
                                          pmnttrms = abap_true
                                          purch_org = abap_true
                                          pur_group = abap_true
                                          currency = abap_true
                                          doc_date = abap_true ) .
*** Function module which creates new service purchase order ---------------*

      CLEAR: po_number,return.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader          = ls_header
          poheaderx         = ls_headerx
        IMPORTING
          exppurchaseorder  = po_number
        TABLES
          return            = return
          poitem            = lt_poitem
          poitemx           = lt_poitemx
          poschedule        = lt_poschedule
          poschedulex       = lt_poschedulex
          poaccount         = lt_account
          poaccountx        = lt_accountx
          poservices        = lt_services
          posrvaccessvalues = lt_poservices
          potextitem        = lt_itemtxt.
      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CLEAR lv_msg.
        LOOP AT return INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg = | { lv_msg } { lw_ret-message } |.
        ENDLOOP.
*Error Tables
        APPEND VALUE #( type     = 'E'
                        message  = lv_msg ) TO lt_messages.
      ELSE.
        DATA: lw_bapiret2 TYPE bapiret2.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = lw_bapiret2.

        " PO Relase
        SELECT SINGLE ebeln,
                      frggr,
                      frgsx
          FROM ekko INTO @DATA(lw_ekko)
          WHERE ebeln = @po_number
            AND frgzu = ' '.

        SELECT SINGLE * FROM t16fs
          INTO @DATA(lw_t16fs)
          WHERE frggr = @lw_ekko-frggr
            AND frgsx = @lw_ekko-frgsx.

        IF lw_t16fs-frgc1 IS NOT INITIAL.
          ls_rel-frggr = lw_t16fs-frggr.
          ls_rel-frgco = lw_t16fs-frgc1.
          ls_rel-ebeln = lw_ekko-ebeln.
          APPEND ls_rel TO lt_rel.
          CLEAR:ls_rel.
        ENDIF.
        IF lw_t16fs-frgc2 IS NOT INITIAL.
          ls_rel-frggr = lw_t16fs-frggr.
          ls_rel-frgco = lw_t16fs-frgc2.
          ls_rel-ebeln = lw_ekko-ebeln.
          APPEND ls_rel TO lt_rel.
          CLEAR:ls_rel.
        ENDIF.
        DATA:
          ls_subrc   TYPE sy-subrc,
          ls_status  TYPE bapimmpara-rel_status,
          ls_rel_ind TYPE bapimmpara-po_rel_ind.
        LOOP AT lt_rel INTO ls_rel.

          CALL FUNCTION 'BAPI_PO_RELEASE'
            EXPORTING
              purchaseorder     = ls_rel-ebeln
              po_rel_code       = ls_rel-frgco
              use_exceptions    = 'X'
              no_commit         = 'X'
            IMPORTING
              rel_status_new    = ls_status
              rel_indicator_new = ls_rel_ind
              ret_code          = ls_subrc
            TABLES
              return            = return.

          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            READ TABLE return INTO DATA(wa_return) WITH KEY type  = 'E'.
            IF sy-subrc IS INITIAL.
              DATA(l_msg) = wa_return-message.
            ENDIF.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
        ENDLOOP.
        IF l_msg IS INITIAL.
          APPEND VALUE #( type    = 'S'
                          ebeln   = po_number
                          ebelp   = lv_poitem
                          message = |Purchase Order Created| ) TO lt_messages.
        ELSE.
          APPEND VALUE #( type    = 'S'
                          ebeln   = po_number
                          ebelp   = lv_poitem
                          message = |Purchase Order Created| ) TO lt_messages.
          APPEND VALUE #( type    = 'W'
                          ebeln   = po_number
                          ebelp   = lv_poitem
                          message = l_msg   ) TO lt_messages.
        ENDIF.
      ENDIF.
*-------------------------------------------------------------------------------------------------------------*
*Changing Existing Purchase Order
    WHEN 'U'.
      CLEAR: header,headerx,return[].
      DATA(lt_poitem_2) = lt_poitem.
      DATA(lt_poitemx_2) = lt_poitemx.
      DATA(lt_account_2) = lt_account.
      DATA(lt_accountx_2) = lt_accountx.
      DATA(lt_services_2) = lt_services.
      DATA(lt_poservices_2) = lt_poservices.
      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder     = ponumber
        IMPORTING
          expheader         = header
          exppoexpimpheader = headerx
        TABLES
          return            = return
          poitem            = lt_poitem
          poitemx           = lt_poitemx
          poaccount         = lt_account
          poaccountx        = lt_accountx
          poservices        = lt_services
          posrvaccessvalues = lt_poservices
          potextitem        = lt_itemtext.
      READ TABLE return INTO DATA(ls_ret1) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = lw_bapiret2.

        CLEAR return.
        CLEAR: header,headerx,return[].
        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder     = ponumber
          IMPORTING
            expheader         = header
            exppoexpimpheader = headerx
          TABLES
            return            = return
            poitem            = lt_poitem_2
            poitemx           = lt_poitemx_2
            poaccount         = lt_account_2
            poaccountx        = lt_accountx_2
            poservices        = lt_services_2
            posrvaccessvalues = lt_poservices_2
            potextitem        = lt_itemtext.
        READ TABLE return INTO ls_ret1 WITH KEY type = 'E'.
        IF sy-subrc = 0.
          CLEAR lv_msg.
          LOOP AT return INTO DATA(lw_ret1) WHERE type = 'E'.
            lv_msg = | { lv_msg } { lw_ret1-message } |.
          ENDLOOP.
*Error Tables
          APPEND VALUE #( type     = 'E'
                          message  = lv_msg ) TO lt_messages.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = lw_bapiret2.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = lw_bapiret2.
*        COMMIT WORK.
*        WAIT UP TO 3 SECONDS.
          APPEND VALUE #( type    = 'S'
                          ebeln   = ponumber
                          ebelp   = lv_poitem
                          message = |Purchase Order New lineitem Added| ) TO lt_messages.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = lw_bapiret2.
*        COMMIT WORK.
*        WAIT UP TO 3 SECONDS.
        APPEND VALUE #( type    = 'S'
                        ebeln   = ponumber
                        ebelp   = lv_poitem
                        message = |Purchase Order New lineitem Added| ) TO lt_messages.
      ENDIF.
  ENDCASE.
ENDFUNCTION.
