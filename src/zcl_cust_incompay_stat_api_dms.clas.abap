class ZCL_CUST_INCOMPAY_STAT_API_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUST_INCOMPAY_STAT_API_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             customer    TYPE kunnr,
             distributor TYPE kunnr,
             orderid     TYPE char10,
           END OF ty_input.

    DATA: gt_input TYPE STANDARD TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    TYPES: BEGIN OF ty_msg,
             orderid     TYPE num10,
             customer    TYPE string,
             distributor TYPE kunnr,
             type(1)     TYPE c,
             status      TYPE string,
             remarks     TYPE string,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: l_remarks TYPE string.
    DATA: l_txt TYPE string.

    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: gv_fiscalyr TYPE bapi0002_4-fiscal_year,
          gv_fiscalp  TYPE bapi0002_4-fiscal_period.
    DATA:lv_date TYPE char20.
    DATA:
      lv_kunnr      TYPE kunnr,
      lv_amt        TYPE wrbtr,
      lv_refdoc     TYPE xblnr1,
      lv_refdoc_str TYPE char50.
    DATA: lv_response TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_data TYPE string.
    DATA: lv_referenceid TYPE num10.
    DATA: l_msg TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REFRESH: gt_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ).

    LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).

*** If customer is present in input itself ***
      IF <fs_input>-customer IS NOT INITIAL.
**Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_input>-customer
          IMPORTING
            output = <fs_input>-customer.
      ENDIF.
      IF <fs_input>-distributor IS NOT INITIAL.
**Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_input>-distributor
          IMPORTING
            output = <fs_input>-distributor.
      ENDIF.
    ENDLOOP.
    SELECT kunnr, name1,werks FROM kna1 INTO TABLE @DATA(lt_customer)
      FOR ALL ENTRIES IN @gt_input WHERE kunnr = @gt_input-customer.
    SELECT kunnr, name1,werks FROM kna1
      FOR ALL ENTRIES IN @gt_input WHERE kunnr = @gt_input-distributor
      APPENDING TABLE @lt_customer.

    SORT lt_customer[] BY kunnr.
    LOOP AT gt_input ASSIGNING <fs_input>.
      CLEAR: lv_kunnr,lv_refdoc.
      lv_kunnr = <fs_input>-customer.
      DATA(lv_distributor) = <fs_input>-distributor.

**** select data from log table for checks ****
      SELECT SINGLE * FROM zfi_dms_cust_pay
               INTO @DATA(lw_log_chk)
        WHERE bukrs  = 'DMS1' AND
              kunnr  = @lv_kunnr AND
              distributor = @lv_distributor AND
              reference_id = @<fs_input>-orderid .
      IF sy-subrc <> 0.
        l_msg = 'Order ID is not found'.
      ENDIF.

*** Customer Number checks if present in sap or not ***
      DATA(lv_kunnr_chk) = VALUE #( lt_customer[ kunnr = lv_kunnr ]-kunnr OPTIONAL ).
      IF lv_kunnr_chk IS INITIAL.

        l_msg = | { l_msg } ; Customer Number is not present in SAP|.

      ENDIF.
*** Customer Number checks if present in sap or not ***
      DATA(lv_dist_chk) = VALUE #( lt_customer[ kunnr = lv_distributor ]-kunnr OPTIONAL ).
      IF lv_dist_chk IS INITIAL.

        l_msg = | { l_msg } ; Distributor Number is not present in SAP|.
      ELSE.

        DATA(lv_wrks) = VALUE #( lt_customer[ kunnr = lv_distributor ]-werks OPTIONAL ).
        IF lv_wrks IS INITIAL.
          l_msg = | { l_msg } ; Distributor-{ lv_distributor } is not mapped to any Plants in SAP|.
        ELSE.
          SELECT SINGLE gsber FROM t134g INTO @DATA(l_bis_area)
            WHERE werks =  @lv_wrks AND  spart = '10'.

          SELECT SINGLE incgl FROM zdms_distb_gl INTO @DATA(l_incom_gl)
            WHERE kunnr = @lv_distributor AND werks =  @lv_wrks.
        ENDIF.
      ENDIF.

      IF l_msg IS NOT INITIAL.

        APPEND VALUE #(   customer = <fs_input>-customer
                          distributor = <fs_input>-distributor
                          type     = 'E'
                          status   = l_msg
                          remarks  = ''  ) TO gt_response.
        CLEAR l_msg.

      ELSE.
        CASE lw_log_chk-status.
          WHEN '10'. l_txt = 'Pending for Clearing'.
          WHEN '20'.
            l_txt = 'Processed for Clearing'.
            SELECT SINGLE *
              FROM zdms_deb_ref_mis
              INTO @DATA(l_ref_mis)
              WHERE bukrs = 'DMS1'
                AND doc_no = @lw_log_chk-doc_no
                AND reference_id = @lw_log_chk-reference_id.
            IF sy-subrc = 0.
              IF l_ref_mis-actual_doc1 IS NOT INITIAL.
                l_remarks = |{ l_ref_mis-actual_doc1 }|.
              ENDIF.
              IF l_ref_mis-actual_doc2 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc2 }|.
              ENDIF.
              IF l_ref_mis-actual_doc3 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc3 }|.
              ENDIF.
              IF l_ref_mis-actual_doc4 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc4 }|.
              ENDIF.
              IF l_ref_mis-actual_doc5 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc5 }|.
              ENDIF.
              IF l_ref_mis-actual_doc6 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc6 }|.
              ENDIF.
              IF l_ref_mis-actual_doc7 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc7 }|.
              ENDIF.
              IF l_ref_mis-actual_doc8 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc8 }|.
              ENDIF.
              IF l_ref_mis-actual_doc9 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc9 }|.
              ENDIF.
              IF l_ref_mis-actual_doc10 IS NOT INITIAL.
                l_remarks = |{ l_remarks }-{ l_ref_mis-actual_doc10 }|.
              ENDIF.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        APPEND VALUE #(   orderid = lw_log_chk-reference_id
                          customer = <fs_input>-customer
                          distributor = <fs_input>-distributor
                          type  = 'S'
                          status = l_txt
                          remarks = l_remarks  ) TO gt_response.
        CLEAR <fs_input>.
      ENDIF.
    ENDLOOP.
*** Response Part ***
    IF gt_response[] IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*      v_jsonload = | { '[' } { lv_body } { ']' } |.
      v_jsonload = lv_body.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_CINCOMPYT_STATUS'
          ijson           = lv_data
          ojson           = v_jsonload
          distributor     = gs_input-distributor
          retailer        = gs_input-customer
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

  ENDMETHOD.
ENDCLASS.
