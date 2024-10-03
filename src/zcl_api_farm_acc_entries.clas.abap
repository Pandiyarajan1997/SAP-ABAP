class ZCL_API_FARM_ACC_ENTRIES definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_FARM_ACC_ENTRIES IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 12.07.2023
*&Purpose : For Farm Related Accounting Document Entries
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------

    DATA: gt_input TYPE TABLE OF zfarm_api_input_st,
          gs_input TYPE zfarm_api_input_st.
    TYPES: BEGIN OF output,
             bukrs   TYPE bukrs,
             xblnr   TYPE xblnr1,
             enttype TYPE zenrtyp,
             vendor  TYPE lifnr,
             bldat   TYPE bldat,
             budat   TYPE budat,
             workctr TYPE arbpl,
             amount  TYPE wrbtr,
             belnr   TYPE belnr_d,
             gjahr   TYPE gjahr,
             type    TYPE bapi_mtype,
             message TYPE string,
           END OF output.
    DATA: gt_output TYPE STANDARD TABLE OF output.

    DATA: lv_msg TYPE string,
          l_msg  TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    DATA lv_belnr   TYPE belnr_d.
    DATA lv_fisyear TYPE gjahr.

    DATA: lo_object_cls TYPE REF TO zcl_farm_entries_posting.
    CREATE OBJECT lo_object_cls.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.


    "Input of API Request
    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).
    DATA(lv_data_tmp) = |{ lv_data }|.
*    CONDENSE lv_data_tmp NO-GAPS.
    SHIFT lv_data_tmp LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      REFRESH: gt_output.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        IF <fs_input> IS INITIAL .
          lv_msg = |Input is Missing|.
          APPEND VALUE #( bukrs   = <fs_input>-compcode
                          xblnr   = <fs_input>-refdoc
                          enttype = <fs_input>-entrytype
                          vendor  = <fs_input>-vendor
                          bldat   = <fs_input>-docdate
                          budat   = <fs_input>-posdate
                          workctr = <fs_input>-workctr
                          type    = 'E'
                          message = lv_msg ) TO gt_output.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * FROM zfi_farm_ent_log INTO @DATA(l_log)
          WHERE bukrs = @<fs_input>-compcode
          AND xblnr = @<fs_input>-refdoc AND type = 'S'.
        IF sy-subrc EQ 0.
          lv_msg = |{ lv_msg },Document already Created based on same reference|.
          SHIFT lv_msg LEFT DELETING LEADING ','.
          CONDENSE lv_msg.
          APPEND VALUE #( bukrs   = <fs_input>-compcode
                          xblnr   = <fs_input>-refdoc
                          enttype = <fs_input>-entrytype
                          vendor  = <fs_input>-vendor
                          bldat   = <fs_input>-docdate
                          budat   = <fs_input>-posdate
                          workctr = <fs_input>-workctr
                          type    = 'E'
                          message = lv_msg ) TO gt_output.
          CONTINUE.
        ELSE.
          SELECT SINGLE * FROM bsip INTO @DATA(l_duplicate)
            WHERE bukrs = @<fs_input>-compcode
            AND lifnr = @<fs_input>-vendor
            AND xblnr = @<fs_input>-refdoc.
          IF sy-subrc EQ 0.
            lv_msg = |{ lv_msg },Document already Created based on same reference|.
            SHIFT lv_msg LEFT DELETING LEADING ','.
            CONDENSE lv_msg.
            APPEND VALUE #( bukrs   = <fs_input>-compcode
                            xblnr   = <fs_input>-refdoc
                            enttype = <fs_input>-entrytype
                            vendor  = <fs_input>-vendor
                            bldat   = <fs_input>-docdate
                            budat   = <fs_input>-posdate
                            workctr = <fs_input>-workctr
                            type    = 'E'
                            message = lv_msg ) TO gt_output.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF <fs_input>-entrytype IS INITIAL.
          lv_msg = |{ lv_msg },Entry type is Missing|.
        ELSE.
          TRANSLATE <fs_input>-entrytype TO UPPER CASE.
          SELECT SINGLE enttype FROM zfarm_glacc_mast INTO @DATA(l_enttype)
            WHERE enttype = @<fs_input>-entrytype.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Entry type is Not Manintained|.
          ENDIF.
        ENDIF.
*Reference Document Number
        IF <fs_input>-refdoc IS INITIAL.
          lv_msg = |{ lv_msg },Reference Document Number is Mandatory|.
        ELSE.
        ENDIF.
*Company Code
        SELECT SINGLE bukrs FROM t001 INTO @DATA(lv_bukrs)
          WHERE bukrs = @<fs_input>-compcode.
        IF sy-subrc NE 0.
          lv_msg = |{ lv_msg },Incorrect Company Code|.
        ENDIF.

*Vendor Code
        IF <fs_input>-vendor IS NOT INITIAL.
          DATA(lv_vendor) = |{ <fs_input>-vendor ALPHA = IN }|.
          CLEAR <fs_input>-vendor. <fs_input>-vendor = lv_vendor.
          SELECT SINGLE lifnr FROM lfa1 INTO @DATA(lv_lifnr)
            WHERE lifnr = @lv_vendor.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Incorrect Vendor Number|.
          ENDIF.
        ELSE.
          IF <fs_input>-entrytype NE 'LC'.
            lv_msg = |{ lv_msg },Vendor Code is Missing|.
          ENDIF.
        ENDIF.
*Amount
        IF <fs_input>-amount IS INITIAL.
          lv_msg = |{ lv_msg },Amount is Missing|.
        ENDIF.
*Document Date
        IF <fs_input>-docdate IS INITIAL.
          lv_msg = |{ lv_msg },Document Date is Missing|.
        ENDIF.
*Posting Date
        IF <fs_input>-posdate IS INITIAL.
          lv_msg = |{ lv_msg },Posting Date is Missing|.
        ENDIF.
*Posting Date
        IF <fs_input>-workctr IS INITIAL.
          lv_msg = |{ lv_msg },Work Center is Missing|.
        ELSE.
          SELECT SINGLE arbpl FROM crhd INTO @DATA(lv_workcntr)
            WHERE arbpl = @<fs_input>-workctr.
          IF sy-subrc NE 0.
            lv_msg  = |{ lv_msg },Work Center is Incorrect|.
          ENDIF.
        ENDIF.
*Posting Date
        IF <fs_input>-itemtxt IS INITIAL.
          lv_msg = |{ lv_msg },Item text is Missing|.
        ENDIF.
*Error Message Maintainence
        IF lv_msg IS NOT INITIAL.
          SHIFT lv_msg LEFT DELETING LEADING ','.
          CONDENSE lv_msg.
          APPEND VALUE #( bukrs   = <fs_input>-compcode
                          xblnr   = <fs_input>-refdoc
                          enttype = <fs_input>-entrytype
                          vendor  = <fs_input>-vendor
                          bldat   = <fs_input>-docdate
                          budat   = <fs_input>-posdate
                          workctr = <fs_input>-workctr
                          amount  = <fs_input>-amount
                          type    = 'E'
                          message = lv_msg ) TO gt_output.
          DATA(log) = VALUE zfi_farm_ent_log( mandt   = sy-mandt
                                              bukrs   = <fs_input>-compcode
                                              xblnr   = <fs_input>-refdoc
                                              enttype = <fs_input>-entrytype
                                              vendor  = <fs_input>-vendor
                                              bldat   = <fs_input>-docdate
                                              budat   = <fs_input>-posdate
                                              workctr = <fs_input>-workctr
                                              amount  = <fs_input>-amount
                                              erdat   = sy-datum
                                              ernam   = sy-uname
                                              erzet   = sy-uzeit
                                              type    = 'E'
                                              message = lv_msg ).
          MODIFY zfi_farm_ent_log FROM log.
        ELSE.
          CASE <fs_input>-entrytype.
*Labour Charges
            WHEN 'LC'.
* GL Account Document Posting
              CLEAR lv_msg.
              CLEAR: lv_belnr,lv_fisyear,lv_msg.
              CALL METHOD lo_object_cls->create_glaccount_document
                EXPORTING
                  input    = <fs_input>
                IMPORTING
                  belnr    = lv_belnr
                  fisyear  = lv_fisyear
                  msg      = lv_msg
                EXCEPTIONS
                  no_input = 1
                  OTHERS   = 2.
              IF sy-subrc = 0 AND lv_belnr IS NOT INITIAL.
                APPEND VALUE #( bukrs   = <fs_input>-compcode
                                xblnr   = <fs_input>-refdoc
                                enttype = <fs_input>-entrytype
                                vendor  = <fs_input>-vendor
                                bldat   = <fs_input>-docdate
                                budat   = <fs_input>-posdate
                                workctr = <fs_input>-workctr
                                amount  = <fs_input>-amount
                                belnr   = lv_belnr
                                gjahr   = lv_fisyear
                                type    = 'S'
                                message = lv_msg ) TO gt_output.
              ELSE.
                SHIFT lv_msg LEFT DELETING LEADING ','.
                CONDENSE lv_msg.
                APPEND VALUE #( bukrs   = <fs_input>-compcode
                                xblnr   = <fs_input>-refdoc
                                enttype = <fs_input>-entrytype
                                vendor  = <fs_input>-vendor
                                bldat   = <fs_input>-docdate
                                budat   = <fs_input>-posdate
                                workctr = <fs_input>-workctr
                                amount  = <fs_input>-amount
                                belnr   = lv_belnr
                                gjahr   = lv_fisyear
                                type    = 'E'
                                message = lv_msg ) TO gt_output.
              ENDIF.
* All Other Vendor Invoices
            WHEN OTHERS.
              CLEAR: lv_belnr,lv_fisyear,lv_msg.
              CALL METHOD lo_object_cls->create_vendor_invoice
                EXPORTING
                  input   = <fs_input>
                IMPORTING
                  fundctr = DATA(lv_fundctr)
                  belnr   = lv_belnr
                  fisyear = lv_fisyear
                  msg     = lv_msg.
              IF lv_belnr IS NOT INITIAL.
                APPEND VALUE #( bukrs   = <fs_input>-compcode
                                xblnr   = <fs_input>-refdoc
                                enttype = <fs_input>-entrytype
                                vendor  = <fs_input>-vendor
                                bldat   = <fs_input>-docdate
                                budat   = <fs_input>-posdate
                                workctr = <fs_input>-workctr
                                amount  = <fs_input>-amount
                                belnr   = lv_belnr
                                gjahr   = lv_fisyear
                                type    = 'S'
                                message = lv_msg ) TO gt_output.
              ELSE.
                SHIFT lv_msg LEFT DELETING LEADING ','.
                CONDENSE lv_msg.
                APPEND VALUE #( bukrs   = <fs_input>-compcode
                                xblnr   = <fs_input>-refdoc
                                enttype = <fs_input>-entrytype
                                vendor  = <fs_input>-vendor
                                bldat   = <fs_input>-docdate
                                budat   = <fs_input>-posdate
                                workctr = <fs_input>-workctr
                                amount  = <fs_input>-amount
                                belnr   = lv_belnr
                                gjahr   = lv_fisyear
                                type    = 'E'
                                message = lv_msg ) TO gt_output.
              ENDIF.
          ENDCASE.
        ENDIF.
        CLEAR: lv_msg,log.
      ENDLOOP.

      IF gt_output IS NOT INITIAL.
        CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gt_output
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = lv_body ).

        v_jsonload = | { '[' } { lv_body } { ']' } |.

        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'FARMACCCOUNT'
            ijson           = lv_data
            ojson           = v_jsonload
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.

        ENDIF.
*Set JSON Content-Type
        CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD server->response->set_cdata( data = v_jsonload ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
