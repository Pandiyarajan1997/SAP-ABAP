class ZCL_API__PARTNERFUNC_UPDATE definition
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



CLASS ZCL_API__PARTNERFUNC_UPDATE IMPLEMENTATION.


  METHOD check_partner_functions.

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


  METHOD if_http_extension~handle_request.
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

    DATA : lo_check TYPE REF TO zcl_common_check.
    CREATE OBJECT lo_check.

    DATA : lt_cusblk TYPE zsd_tt_cust_block.
    DATA: ls_cusblk TYPE zsd_st_cust_block.

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

*******customer block check********
        SELECT SINGLE bukrs FROM tvko INTO @DATA(lv_bukrs) WHERE vkorg = @gs_input-vkorg.

        CLEAR ls_cusblk.
        ls_cusblk-bukrs = lv_bukrs.
        ls_cusblk-vkorg = gs_input-vkorg.
        ls_cusblk-kunnr = lv_kunnr.

        APPEND ls_cusblk TO lt_cusblk.

        lo_check->customer_block_check(
          EXPORTING
            bukrs                     = lv_bukrs            " Company Code
            vkorg                     = gs_input-vkorg      " Sales Organization
          CHANGING
            cust_tab                  = lt_cusblk ).        " Table Type for Customer Block Check

        CLEAR ls_cusblk.
        READ TABLE lt_cusblk INTO ls_cusblk WITH KEY kunnr = lv_kunnr.
        IF ls_cusblk-block = abap_true.
          APPEND VALUE #( customer = gs_input-customer
                          type     = 'E'
                          msg      = |Customer is blocked in Company code ::{ lv_bukrs } and S.org:: { gs_input-vkorg } | ) TO gt_response.
        ELSE.
          IF gs_input-vkorg = '1000' OR gs_input-vkorg = '6000' OR gs_input-vkorg = 'SDMS'.

            SELECT kunnr,vkorg,vtweg,spart FROM knvv
              INTO TABLE @DATA(lt_knvv) WHERE kunnr = @lv_kunnr AND vkorg = @gs_input-vkorg AND vtweg = '20' AND loevm NE 'X'.
            IF sy-subrc = 0.
              SELECT * FROM knvp INTO TABLE @DATA(lt_knvp)
                FOR ALL ENTRIES IN @lt_knvv WHERE kunnr = @lt_knvv-kunnr AND vkorg = @lt_knvv-vkorg AND vtweg = @lt_knvv-vtweg AND spart = @lt_knvv-spart.
            ELSE.
              APPEND VALUE #( customer = gs_input-customer
                              type     = 'E'
                              msg      = |Customer Number::{ gs_input-customer } Not In Sales Org::{ gs_input-vkorg } | ) TO gt_response.
            ENDIF.
          ELSE.
            APPEND VALUE #( customer = gs_input-customer
                       type     = 'E'
                       msg      = |Sales org Not Accepted for::{ gs_input-customer }| ) TO gt_response.
          ENDIF.
        ENDIF.

        IF lt_knvp[] IS NOT INITIAL.
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
                CONTINUE.
              ENDIF.
* for other types of Partner Functions
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
              ELSE.
                DATA(l_salesdata) = VALUE cmds_ei_cmd_sales( sales = lt_sales[] ).

                ls_customers-header-object_instance-kunnr = lv_kunnr .
                ls_customers-header-object_task = 'M'. "Represents Insert or Create
                ls_customers-sales_data = l_salesdata.
                APPEND ls_customers TO lt_customers.
                is_master_data-customers = lt_customers[].

                cmd_ei_api=>initialize( ).
                CLEAR es_message_defective-is_error.
                CALL METHOD cmd_ei_api=>maintain_bapi
                  EXPORTING
*                   IV_TEST_RUN              = SPACE
*                   IV_COLLECT_MESSAGES      = SPACE
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
            ENDIF.
          ENDLOOP.
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


  METHOD process_oth_pf.

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


  METHOD process_xn_pf.

    DATA: lt_knvp_tab TYPE gtt_knvp,
          lt_knvp_com TYPE gtt_knvp,
          lt_knvp_upd TYPE gtt_knvp.

    DATA: lsv_knvp TYPE knvp.

    DATA: lt_yknvp_u TYPE TABLE OF fknvp,
          lt_Xknvp_u TYPE TABLE OF fknvp,
          lt_yknvp_I TYPE TABLE OF fknvp,
          lt_Xknvp_I TYPE TABLE OF fknvp,
          ls_xknvp   TYPE fknvp,
          ls_yknvp   TYPE fknvp.

    DATA: lv_OBJECTID TYPE cdhdr-objectid.

    DATA: lt_pos TYPE TABLE OF zhr_so_to_top.

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
      EXIT.
    ENDIF.

    lt_knvp_tab[] = im_knvp_tab[].
    SORT lt_knvp_tab BY kunnr vkorg vtweg spart.
    DELETE ADJACENT DUPLICATES FROM lt_knvp_tab COMPARING kunnr vkorg vtweg spart.

*Prepare the new Table based on position hierarchy
    LOOP AT lt_knvp_tab INTO ls_knvp.
      LOOP AT lt_pos INTO DATA(ls_pos).

* for Position Partner
        CLEAR lsv_knvp.
        lsv_knvp-mandt = sy-mandt.
        lsv_knvp-kunnr = ls_knvp-kunnr.
        lsv_knvp-vkorg = ls_knvp-vkorg.
        lsv_knvp-vtweg = ls_knvp-vtweg.
        lsv_knvp-spart = ls_knvp-spart.
        lsv_knvp-lifnr = ls_pos-buspartner.
        lsv_knvp-parvw = ls_pos-parvw_new.
        APPEND lsv_knvp TO lt_knvp_com.

* For Pernr Partner function
        CLEAR lsv_knvp.
        lsv_knvp-mandt = sy-mandt.
        lsv_knvp-kunnr = ls_knvp-kunnr.
        lsv_knvp-vkorg = ls_knvp-vkorg.
        lsv_knvp-vtweg = ls_knvp-vtweg.
        lsv_knvp-spart = ls_knvp-spart.
        lsv_knvp-pernr = ls_pos-pernr.
        lsv_knvp-parvw = ls_pos-parvw_unc.
        APPEND lsv_knvp TO lt_knvp_com.

      ENDLOOP.
    ENDLOOP.

*compare with the original KNVP Table
    LOOP AT lt_knvp_com INTO ls_knvp.

      CLEAR lsv_knvp.
      READ TABLE im_knvp_tab INTO lsv_knvp WITH KEY kunnr = ls_knvp-kunnr vkorg = ls_knvp-vkorg
                                                    vtweg = ls_knvp-vtweg spart = ls_knvp-spart
                                                    parvw = ls_knvp-parvw.
      IF sy-subrc = 0.

        CASE ls_knvp-parvw.
          WHEN 'XN' OR 'XL' OR 'XK' OR 'YD' OR 'XJ' or 'XS'.
            IF lsv_knvp-lifnr = ls_knvp-lifnr.
              CONTINUE.
            ENDIF.
          WHEN 'L5' OR 'L3' OR 'L2' OR 'ZS' OR 'L1' or 'PW'.
            IF lsv_knvp-pernr = ls_knvp-pernr.
              CONTINUE.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        CLEAR: ls_yknvp.
        MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
        ls_yknvp-mandt = sy-mandt.
        ls_yknvp-kz = 'U'.
        APPEND ls_yknvp TO lt_yknvp_u.

*apeend to the update table
        APPEND ls_knvp TO lt_knvp_upd.

*fill the after structure
        CLEAR: ls_xknvp.
        MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
        ls_xknvp-mandt = sy-mandt.
        ls_xknvp-kz = 'U'.
        APPEND ls_Xknvp TO lt_Xknvp_u.

      ELSE.
* THis is a case of new insert for New partner record inserting
        CLEAR lsv_knvp.
        lsv_knvp-mandt = sy-mandt.
        lsv_knvp-kunnr = ls_knvp-kunnr.
        lsv_knvp-vkorg = ls_knvp-vkorg.
        lsv_knvp-vtweg = ls_knvp-vtweg.
        lsv_knvp-spart = ls_knvp-spart.
        lsv_knvp-parvw = ls_knvp-parvw.

* fill the before structure
        CLEAR: ls_yknvp.
        MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
        ls_yknvp-mandt = sy-mandt.
        ls_yknvp-kz = 'I'.
        APPEND ls_yknvp TO lt_yknvp_I.

        CASE ls_knvp-parvw.
          WHEN 'XN' OR 'XL' OR 'XK' OR 'YD' OR 'XJ' or 'XS'.
            lsv_knvp-lifnr = ls_knvp-lifnr.
          WHEN 'L5' OR 'L3' OR 'L2' OR 'ZS' OR 'L1' or 'PW'.
            lsv_knvp-pernr = ls_knvp-pernr.
          WHEN OTHERS.
        ENDCASE.

*apeend to the update table
        APPEND lsv_knvp TO lt_knvp_upd.

*fill the after structure
        CLEAR: ls_xknvp.
        MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
        ls_xknvp-mandt = sy-mandt.
        ls_xknvp-kz = 'I'.
        APPEND ls_Xknvp TO lt_Xknvp_I.

      ENDIF.

    ENDLOOP.


    IF lt_knvp_upd[] IS INITIAL.
      APPEND VALUE #( customer = lv_kunnr
                      type     = 'S'
                      msg      = |No Partner Function Data to Update| ) TO ch_response.
    ELSE.
      MODIFY knvp FROM TABLE lt_knvp_upd.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.

      APPEND VALUE #( customer = lv_kunnr
                      type     = 'S'
                      msg      = |Partner Functions Updated Successfully| ) TO ch_response.

        IF lt_Xknvp_I[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
          CLEAR lv_objectid.
          lv_objectid = lv_kunnr.

          CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
            EXPORTING
              objectid = lv_objectid
              tcode    = 'XD02'
              utime    = sy-uzeit
              udate    = sy-datum
              username = sy-uname
              upd_knvp = 'I'
            TABLES
              xknvp    = lt_xknvp_i
              yknvp    = lt_yknvp_i.
        ENDIF.

        IF lt_Xknvp_u[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
          CLEAR lv_objectid.
          lv_objectid = lv_kunnr.

          CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
            EXPORTING
              objectid = lv_objectid
              tcode    = 'XD02'
              utime    = sy-uzeit
              udate    = sy-datum
              username = sy-uname
              upd_knvp = 'U'
            TABLES
              xknvp    = lt_xknvp_u
              yknvp    = lt_yknvp_u.
        ENDIF.

        COMMIT WORK AND WAIT.

      ELSE.
        APPEND VALUE #( customer = lv_kunnr
                        type     = 'E'
                        msg      = |PF Update Error to KNVP| ) TO ch_response.
        ex_subrc = 4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
