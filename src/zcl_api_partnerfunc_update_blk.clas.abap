class ZCL_API_PARTNERFUNC_UPDATE_BLK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    GTT_KNVP TYPE table of knvp .
  types:
    BEGIN OF gty_input,
             customer TYPE kunnr,
             vkorg    TYPE vkorg,
             details  TYPE zsd_pf_value_tt,
           END OF gty_input .
  types:
    BEGIN OF gty_response,
             customer TYPE kunnr,
             type     TYPE char01,
             msg      TYPE string,
           END OF gty_response .
  types:
    gtt_response TYPE table of gty_response .

  TYPES: gtt_input TYPE table of gty_response .

  methods CHECK_PARTNER_FUNCTIONS
    importing
      !IM_PFDATA type ZSD_PF_VALUE_ST
      !IM_CUSTOMER type KUNNR
    exporting
      !SUBRC type SY-SUBRC
    changing
      !IM_RESPONSE type GTT_RESPONSE .
  methods PROCESS_XN_PF
    importing
      !IM_KNVP_TAB type GTT_KNVP
      !IM_KUNNR type KUNNR
      !IM_POSITION type HROBJID
    exporting
      !EX_SUBRC type SY-SUBRC
    changing
      !CH_SALES type CMDS_EI_SALES_T
      !CH_RESPONSE type GTT_RESPONSE .
  methods PROCESS_OTH_PF
    importing
      !IM_KNVP_TAB type GTT_KNVP
      !IM_KUNNR type KUNNR
      !IM_PFDATA type ZSD_PF_VALUE_ST
    exporting
      !EX_SUBRC type SY-SUBRC
    changing
      !CH_SALES type CMDS_EI_SALES_T
      !CH_RESPONSE type GTT_RESPONSE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_PARTNERFUNC_UPDATE_BLK IMPLEMENTATION.


  METHOD CHECK_PARTNER_FUNCTIONS.

    CLEAR subrc.

    SELECT single * FROM tpar INTO @DATA(ls_tpar) WHERE parvw = @im_pfdata-pfunc.

    IF sy-subrc NE 0.
      APPEND VALUE #( customer = IM_CUSTOMER type = 'E' msg = |Incorrect Partner Type::{ im_pfdata-pfunc }| ) TO IM_response.
      subrc = 4.
    ELSE.

**Check for Value of Partner Function
      CASE ls_tpar-nrart.
        WHEN 'PE'.  "*Personnel Number Check

          DATA(lv_pernr) = CONV pernr_d( im_pfdata-value ).
          IF lv_pernr IS NOT INITIAL.
            SELECT SINGLE pernr FROM pa0000 INTO @DATA(l_pernr) WHERE pernr = @lv_pernr
                                AND begda LE @sy-datum AND endda GE @sy-datum AND stat2 = '3'.
            IF sy-subrc NE 0.
              APPEND VALUE #( customer = IM_CUSTOMER type = 'E'
                              msg      = |Incorrect or Inactive Personnal Number::{ im_pfdata-value }for Partner Function { im_pfdata-pfunc } | ) TO IM_response.
              subrc = 4.
            ENDIF.
          ENDIF.
        WHEN 'KU'.   "Customer Number Checks
          DATA(lv_part_kunnr) = |{ im_pfdata-value ALPHA = IN }|.
          SELECT SINGLE kunnr FROM kna1 INTO @DATA(l_kunnr) WHERE kunnr = @lv_part_kunnr.
          IF sy-subrc NE 0.
            APPEND VALUE #( customer = IM_CUSTOMER type     = 'E'
                            msg      = |Incorrect Customer Number::{ im_pfdata-value }for Partner Function { im_pfdata-pfunc } | ) TO IM_response.
            subrc = 4.
          ENDIF.
        WHEN 'LI'. "Vendor Number Checks
          DATA(lv_part_lifnr) = |{ im_pfdata-value ALPHA = IN }|.
          SELECT SINGLE lifnr FROM lfa1 INTO @DATA(l_vendor) WHERE lifnr = @lv_part_lifnr.
          IF sy-subrc NE 0.
            APPEND VALUE #( customer = IM_CUSTOMER type = 'E'
                            msg      = |Incorrect Vendor Number::{ im_pfdata-value } for Partner Function { im_pfdata-pfunc } | ) TO IM_response.
            subrc = 4.
          ENDIF.
      ENDCASE.

    ENDIF.
  ENDMETHOD.


  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.
*Created By: Samsudeen M
*Created On: 18.05.2023
*Purpose: API for Updating the Partner Function of the customer
*Reference: Ramakrishnan J
*------------------------------------------------------------------------------------------*

    DATA: gs_input TYPE gty_input.

    DATA: gt_response TYPE TABLE OF gty_response.

    DATA: lv_exmsg TYPE string.

    DATA: lt_customers  TYPE cmds_ei_extern_t,
          ls_customers  TYPE cmds_ei_extern,
          lt_sales      TYPE cmds_ei_sales_t,
          lt_sales_func TYPE cmds_ei_functions_t,
          ls_sales_func TYPE cmds_ei_functions.      " Ext. Interface: Partner Roles

    DATA is_master_data           TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_master_data_correct   TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_correct       TYPE cvis_message. " Error Indicator and System Messages
    DATA es_master_data_defective TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_defective     TYPE cvis_message. " Error Indicator and System Messages

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** deserialize the INPUT our required INPUT ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_input ).

    IF gs_input IS NOT INITIAL.

      REFRESH: gt_response.
      IF gs_input-customer IS INITIAL.
        APPEND VALUE #( customer = gs_input-customer type = 'E' msg = |Customer Number Missing| ) TO gt_response.
      ELSEIF gs_input-vkorg IS INITIAL.
        APPEND VALUE #( customer = gs_input-customer type = 'E' msg = |Sales Organization Missing| ) TO gt_response.
      ELSEIF gs_input-details IS INITIAL.
        APPEND VALUE #( customer = gs_input-customer type = 'E' msg = |No Partner Function Data is available to update| ) TO gt_response.
      ELSE.

        DATA(lv_kunnr) = |{ gs_input-customer ALPHA = IN }|.

        IF gs_input-vkorg = '1000' OR gs_input-vkorg = '6000' OR gs_input-vkorg = 'SDMS'.

          SELECT kunnr,vkorg,vtweg,spart FROM knvv
            INTO TABLE @DATA(lt_knvv) WHERE kunnr = @lv_kunnr
                                        AND vkorg = @gs_input-vkorg
                                        AND vtweg = '20'
                                        AND loevm NE 'X'.
          IF sy-subrc = 0.
            SELECT * FROM knvp INTO TABLE @DATA(lt_knvp)
              FOR ALL ENTRIES IN @lt_knvv WHERE kunnr = @lt_knvv-kunnr
                                           AND vkorg = @lt_knvv-vkorg
                                           AND vtweg = @lt_knvv-vtweg
                                           AND spart = @lt_knvv-spart.
          ENDIF.
        ELSE.
          APPEND VALUE #( customer = gs_input-customer
                     type     = 'E'
                     msg      = |Sales org Not Accepted for::{ gs_input-customer }| ) TO gt_response.
        ENDIF.


        IF lt_knvp[] IS INITIAL.
          APPEND VALUE #( customer = gs_input-customer
                          type     = 'E'
                          msg      = |Incorrect Customer Number::{ gs_input-customer } or Not | ) TO gt_response.
        ELSE.

          SORT lt_knvp BY kunnr vkorg vtweg spart.

          LOOP AT gs_input-details ASSIGNING FIELD-SYMBOL(<fs_details>).

            CALL METHOD me->check_partner_functions
              EXPORTING
                im_pfdata   = <fs_details>
                im_customer = gs_input-customer
              IMPORTING
                subrc       = DATA(lv_subrc)
              CHANGING
                im_response = gt_response.

            IF lv_subrc IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            IF <fs_details>-pfunc = 'XN'.

              DATA lv_hrobjid TYPE hrobjid.
              lv_hrobjid = <fs_details>-value.

              CLEAR lv_subrc.
              CALL METHOD me->process_xn_pf
                EXPORTING
                  im_knvp_tab = lt_knvp
                  im_kunnr    = gs_input-customer
                  im_position = lv_hrobjid
                IMPORTING
                  ex_subrc    = lv_subrc
                CHANGING
                  ch_sales    = lt_sales
                  ch_response = gt_response.
              IF lv_subrc NE 0.
                REFRESH lt_sales.
                CONTINUE.
              ENDIF.
            ELSE.
              CLEAR lv_subrc.
              CALL METHOD me->process_oth_pf
                EXPORTING
                  im_knvp_tab = lt_knvp
                  im_kunnr    = gs_input-customer
                  im_pfdata   = <fs_details>
                IMPORTING
                  ex_subrc    = lv_subrc
                CHANGING
                  ch_sales    = lt_sales
                  ch_response = gt_response.
              IF lv_subrc NE 0.
                REFRESH lt_sales.
                CONTINUE.
              ENDIF.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.

      IF lt_sales[] IS INITIAL.

        APPEND VALUE #( customer = gs_input-customer
                        type     = 'S'
                        msg      = |No Partner Function Data to Update| ) TO gt_response.
      ELSE.

        DATA(l_salesdata) = VALUE cmds_ei_cmd_sales( sales = lt_sales[] ).

        ls_customers-header-object_instance-kunnr = lv_kunnr .
        ls_customers-header-object_task = 'U'. "Represents Insert or Create
        ls_customers-sales_data = l_salesdata.
        APPEND ls_customers TO lt_customers.
        is_master_data-customers = lt_customers[].

        cmd_ei_api=>initialize( ).
        CLEAR es_message_defective-is_error.
        CALL METHOD cmd_ei_api=>maintain_bapi
          EXPORTING
*           IV_TEST_RUN              = SPACE
*           IV_COLLECT_MESSAGES      = SPACE
            is_master_data           = is_master_data
          IMPORTING
            es_master_data_correct   = es_master_data_correct
            es_message_correct       = es_message_correct
            es_master_data_defective = es_master_data_defective
            es_message_defective     = es_message_defective.

        IF es_message_defective-is_error IS INITIAL.
          COMMIT WORK.
          APPEND VALUE #( customer = gs_input-customer
                          type     = 'S'
                          msg      = |Customer Partner Function Updated Successfully| ) TO gt_response.
        ELSE.
          LOOP AT es_message_defective-messages INTO DATA(ws).
            IF lv_exmsg IS INITIAL.
              lv_exmsg = ws-message.
            ELSE.
              lv_exmsg = | { lv_exmsg }-{ ws-message }  |.
            ENDIF.

          ENDLOOP.
          APPEND VALUE #( customer = gs_input-customer
                          type     = 'E'
                          msg      = lv_exmsg ) TO gt_response.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( customer = gs_input-customer
                      type     = 'E'
                      msg      = |Please Fill The data| ) TO gt_response.
    ENDIF.


    IF gt_response IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = DATA(lv_body) ).
      DATA(v_jsonload) = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.


*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'PF_UPDATE'
        ijson           = lv_data
        ojson           = lv_body
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc = 0.

    ENDIF.

  ENDMETHOD.


  METHOD PROCESS_OTH_PF.

    DATA: lt_sales        TYPE cmds_ei_sales_t,
          ls_sales        TYPE cmds_ei_sales,
          lt_sales_func   TYPE cmds_ei_functions_t,
          ls_sales_func_t TYPE cmds_ei_functions,
          ls_sales_func   TYPE cmds_ei_functions.      " Ext. Interface: Partner Roles

    DATA: lv_upd TYPE boolean.

    DATA: lv_tabix TYPE sy-tabix.

    DATA: lt_pos TYPE TABLE OF zhr_so_to_top.

    CLEAR ex_subrc.

    SELECT SINGLE * FROM tpar INTO @DATA(ls_tpar) WHERE parvw = @im_pfdata-pfunc.

    IF sy-subrc NE 0.
      APPEND VALUE #( customer = im_kunnr type = 'E' msg = |Incorrect Partner Type::{ im_pfdata-pfunc }| ) TO CH_response.
      ex_subrc = 4.
      EXIT.
    ENDIF.

    DATA(lv_kunnr) = |{ IM_kunnr ALPHA = IN }|.

*   Check first if customer is extended for Division 10
    READ TABLE im_knvp_tab INTO DATA(ls_knvp) WITH KEY kunnr = LV_kunnr spart = '10'.
    IF sy-subrc NE 0.
      APPEND VALUE #( customer = im_kunnr
                      type     = 'E'
                      msg      = |Customer::{ im_kunnr } Not Extended for Division 10| ) TO ch_response.
      ex_subrc = 4.
      EXIT.
    ENDIF.

    READ TABLE im_knvp_tab INTO ls_knvp WITH KEY kunnr = lv_kunnr spart = '10' parvw = im_pfdata-pfunc.
    IF sy-subrc = 0.
      CASE ls_tpar-nrart.
        WHEN 'PE'.
          DATA(lv_pernr) = CONV pernr_d( im_pfdata-value ).
          IF ls_knvp-pernr NE lv_pernr.
            CLEAR ls_sales_func.
            ls_sales_func-task = 'U'.
            ls_sales_func-data_key-parvw = im_pfdata-pfunc.
            ls_sales_func-data-partner = lv_pernr.
            ls_sales_func-datax-partner = 'X'.
            APPEND ls_sales_func TO lt_sales_func.
          ENDIF.
        WHEN 'KU'.
          DATA(lv_part_kunnr) = |{ im_pfdata-value ALPHA = IN }|.
          IF ls_knvp-kunnr NE lv_part_kunnr.
            CLEAR ls_sales_func.
            ls_sales_func-task = 'U'.
            ls_sales_func-data_key-parvw = im_pfdata-pfunc.
            ls_sales_func-data-partner = lv_part_kunnr.
            ls_sales_func-datax-partner = 'X'.
            APPEND ls_sales_func TO lt_sales_func.
          ENDIF.
        WHEN 'LI'.
          DATA(lv_part_lifnr) = |{ im_pfdata-value ALPHA = IN }|.
          IF ls_knvp-lifnr NE lv_part_lifnr.
            CLEAR ls_sales_func.
            ls_sales_func-task = 'U'.
            ls_sales_func-data_key-parvw = im_pfdata-pfunc.
            ls_sales_func-data-partner = lv_part_lifnr.
            ls_sales_func-datax-partner = 'X'.
            APPEND ls_sales_func TO lt_sales_func.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      CLEAR ls_sales_func.   "Inserting new data
      ls_sales_func-task = 'I'.
      ls_sales_func-data_key-parvw = im_pfdata-pfunc.
*      ls_sales_func-data-partner = im_pfdata-value.
      ls_sales_func-datax-partner = 'X'.

      CASE ls_tpar-nrart.
        WHEN 'PE'.
          lv_pernr = CONV pernr_d( im_pfdata-value ).
          ls_sales_func-data-partner = lv_pernr.
        WHEN 'KU'.
          lv_part_kunnr = |{ im_pfdata-value ALPHA = IN }|.
          ls_sales_func-data-partner = lv_part_kunnr.
        WHEN 'LI'.
          lv_part_lifnr = |{ im_pfdata-value ALPHA = IN }|.
          ls_sales_func-data-partner = lv_part_lifnr.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_sales_func TO lt_sales_func.
    ENDIF.

    IF lt_sales_func[] IS NOT INITIAL.
      LOOP AT im_knvp_tab INTO ls_knvp.
        AT NEW spart.

          READ TABLE ch_sales INTO DATA(lch_sales) WITH KEY data_key-vkorg = ls_knvp-vkorg
                                                            data_key-vtweg = ls_knvp-vtweg
                                                            data_key-spart = ls_knvp-spart.
          IF sy-subrc = 0.
            lv_tabix = sy-tabix.
            LOOP AT lt_sales_func INTO ls_sales_func.
              CLEAR ls_sales_func_t.
              ls_sales_func_t-task = ls_sales_func-task.
              ls_sales_func_t-data_key-parvw = ls_sales_func-data_key-parvw.
              ls_sales_func_t-data-partner = ls_sales_func-data-partner.
              ls_sales_func_t-datax-partner = 'X'.
              APPEND ls_sales_func_t TO lch_sales-functions-functions.
              MODIFY ch_sales FROM lch_sales INDEX lv_tabix.
            ENDLOOP.
          ELSE.
            ls_sales = VALUE cmds_ei_sales( task = 'M'
                                            data_key-vkorg = ls_knvp-vkorg
                                            data_key-vtweg = ls_knvp-vtweg
                                            data_key-spart = ls_knvp-spart
                                            functions-functions = lt_sales_func ).
            APPEND ls_sales TO ch_sales.
            CLEAR ls_sales.
          ENDIF.
        ENDAT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD PROCESS_XN_PF.

    DATA: lt_sales        TYPE cmds_ei_sales_t,
          ls_sales        TYPE cmds_ei_sales,
          lt_sales_func   TYPE cmds_ei_functions_t,
          ls_sales_func_t TYPE cmds_ei_functions,
          ls_sales_func   TYPE cmds_ei_functions.      " Ext. Interface: Partner Roles

    DATA: lt_pos TYPE TABLE OF zhr_so_to_top.

    DATA: lv_tabix TYPE sy-tabix.

    DATA(lv_kunnr) = |{ IM_kunnr ALPHA = IN }|.

    CLEAR ex_subrc.

*   Check first if customer is extended for Division 10
    READ TABLE im_knvp_tab INTO DATA(ls_knvp) WITH KEY kunnr = LV_kunnr spart = '10'.
    IF sy-subrc NE 0.
      APPEND VALUE #( customer = im_kunnr
                      type     = 'E'
                      msg      = |Customer::{ im_kunnr } Not Extended for Division 10| ) TO ch_response.
      ex_subrc = 4.
      EXIT.
    ENDIF.

*   get position hierarchy based on the provided position
    CALL FUNCTION 'ZHR_SO_TO_TOP_POSITION'
      EXPORTING
        so_position  = im_position
      TABLES
        it_positions = lt_pos.

    IF lt_pos[] IS INITIAL.
      APPEND VALUE #( customer = im_kunnr
                      type     = 'E'
                      msg      = |Incorrect XN: Position Value::{ im_position }| ) TO ch_response.
      ex_subrc = 4.
      exit.
    ENDIF.

    LOOP AT lt_pos INTO DATA(ls_pos).
      READ TABLE im_knvp_tab INTO ls_knvp WITH KEY kunnr = lv_kunnr spart = '10' parvw = ls_pos-parvw_new.
      IF sy-subrc = 0.
        IF ls_knvp-lifnr NE ls_pos-buspartner. "Changing of Data
          CLEAR ls_sales_func.
          ls_sales_func-task = 'U'.
          ls_sales_func-data_key-parvw = ls_knvp-parvw.
          ls_sales_func-data-partner = ls_pos-buspartner.
          ls_sales_func-datax-partner = 'X'.
          APPEND ls_sales_func TO lt_sales_func.
        ENDIF.
      ELSE.
        CLEAR ls_sales_func.   "Inserting new data
        ls_sales_func-task = 'I'.
        ls_sales_func-data_key-parvw = ls_pos-parvw_new.
        ls_sales_func-data-partner = ls_pos-buspartner.
        ls_sales_func-datax-partner = 'X'.
        APPEND ls_sales_func TO lt_sales_func.
      ENDIF.


* read and compare for the pernr based
      READ TABLE im_knvp_tab INTO ls_knvp WITH KEY kunnr = lv_kunnr spart = '10' parvw = ls_pos-parvw_unc.
      IF sy-subrc = 0.
        IF ls_knvp-pernr NE ls_pos-pernr.
          CLEAR ls_sales_func.
          ls_sales_func-task = 'U'.
          ls_sales_func-data_key-parvw = ls_pos-parvw_unc.
          ls_sales_func-data-partner = ls_pos-pernr.
          ls_sales_func-datax-partner = 'X'.
          APPEND ls_sales_func TO lt_sales_func.
        ENDIF.
      ELSE.
        CLEAR ls_sales_func.
        ls_sales_func-task = 'I'.
        ls_sales_func-data_key-parvw = ls_pos-parvw_unc.
        ls_sales_func-data-partner = ls_pos-pernr.
        ls_sales_func-datax-partner = 'X'.
        APPEND ls_sales_func TO lt_sales_func.
      ENDIF.

    ENDLOOP.

    IF lt_sales_func[] IS NOT INITIAL.
      LOOP AT im_knvp_tab INTO ls_knvp.
        AT NEW spart.

          READ TABLE ch_sales INTO DATA(lch_sales) WITH KEY data_key-vkorg = ls_knvp-vkorg
                                                            data_key-vtweg = ls_knvp-vtweg
                                                            data_key-spart = ls_knvp-spart.
          IF sy-subrc = 0.
            lv_tabix = sy-tabix.
            LOOP AT lt_sales_func INTO ls_sales_func.
              CLEAR ls_sales_func_t.
              ls_sales_func_t-task = ls_sales_func-task.
              ls_sales_func_t-data_key-parvw = ls_sales_func-data_key-parvw.
              ls_sales_func_t-data-partner = ls_sales_func-data-partner.
              ls_sales_func_t-datax-partner = 'X'.
              APPEND ls_sales_func_t TO lch_sales-functions-functions.
              MODIFY ch_sales FROM lch_sales INDEX sy-tabix.
            ENDLOOP.
          ELSE.
            ls_sales = VALUE cmds_ei_sales( task = 'M'
                                            data_key-vkorg = ls_knvp-vkorg
                                            data_key-vtweg = ls_knvp-vtweg
                                            data_key-spart = ls_knvp-spart
                                            functions-functions = lt_sales_func ).
            APPEND ls_sales TO ch_sales.
            CLEAR ls_sales.
          ENDIF.
        ENDAT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
