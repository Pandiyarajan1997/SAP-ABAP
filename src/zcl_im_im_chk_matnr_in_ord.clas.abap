class ZCL_IM_IM_CHK_MATNR_IN_ORD definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IM_CHK_MATNR_IN_ORD IMPLEMENTATION.


  METHOD if_ex_me_process_po_cust~check.

    INCLUDE mm_messages_mac.

        CLEAR ch_failed.

    DATA(lv_otype) = im_hEADER->get_data( ).

    IF lv_otype-bsart = 'ZNB'.

      DATA(lt_items) = im_hEADER->get_items( ).

      LOOP AT lt_items INTO DATA(ls_item).

        DATA(lv_item) = ls_item-item->get_data( ).

        IF lv_item-matnr IS INITIAL.
          mmpur_context 910.
          mmpur_message_forced 'E' '00' '398' 'Trial Message' '' '' ''.
          ch_failed = abap_true.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~OPEN.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~POST.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.
*
*  data(lr_header) = IM_ITEM->GET_HEADER( ).
*
*  DATA(lr_head) = lr_header->GET_DATA( ).


  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
  endmethod.
ENDCLASS.
