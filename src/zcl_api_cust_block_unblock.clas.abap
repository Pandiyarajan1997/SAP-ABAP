*& Date Created - 26.04.2024
*& Created By - CNABAP (Zakir)
*& Description - API for Customer Block/UnBlock
*& TR No    -    DEVK935497(Active Version),
CLASS zcl_api_cust_block_unblock DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    TYPES:
      BEGIN OF ty_ret_data,
        Customer        TYPE kunnr,
        Customer_type   TYPE char50,
        Block_Indicator TYPE char02,
        Message         TYPE string,
        Type            TYPE bapi_mtype,
      END OF ty_ret_data .
    TYPES:
      TT_Ret_data TYPE TABLE OF ty_ret_data WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_kna1,
        mandt TYPE mandt,
        kunnr TYPE kunnr,
*       land1                     TYPE land1,
        name1 TYPE name1,
*        regio                     TYPE regio,
*        OrderIsBlockedForCustomer TYPE aufsd,
*        CentralDeletionblock      TYPE nodel,
        block TYPE loevm,
*        Centralpostingblock       TYPE sperr,
*        DeliveryIsBlocked         TYPE lifsd_x,
*        BillingBlockedForCustomer TYPE faksd_x,
*        Salesblock                TYPE cassd_x,
      END OF ty_kna1 .
    TYPES:
      tt_kna1 TYPE TABLE OF kna1 WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_knb1,
        mandt TYPE mandt,
        bukrs TYPE bukrs,
        kunnr TYPE kunnr,
*        sperr TYPE sperr, " posting block co .code based
        Block TYPE loevm_x, " deletion flag co. code based
*        nodel TYPE nodel, " deletion block co. code based
      END OF ty_knb1 .
    TYPES:
      tt_knb1 TYPE TABLE OF knb1 WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_knvv,
        kunnr                        TYPE kunnr,
        vkorg                        TYPE vkorg,
        vtweg                        TYPE vtweg,
        spart                        TYPE spart,
        OrderIsBlockedForCustomer    TYPE aufsd,  "aufsd ( salesArea )
        DeliveryIsBlockedForCustomer TYPE lifsd_x, " lifsd
        BillingIsBlockedForCustomer  TYPE faksd_x, " faksd
        Deletionflag                 TYPE loevm,
      END OF ty_knvv .
    TYPES:
      tt_knvv TYPE TABLE OF knvv WITH DEFAULT KEY .

    DATA gt_input TYPE ztt_cust_imp_bl_ubl .
    DATA gs_input TYPE zst_cust_inp_bl_ubl  .
    METHODS: Process_Data IMPORTING ls_input    TYPE zst_cust_inp_bl_ubl
                                    Lt_kna1     TYPE tt_kna1
                                    Lt_knb1     TYPE tt_knb1
                                    Lt_knvv     TYPE tt_knvv
                          EXPORTING et_response TYPE tt_ret_data
                          ,
      Validate_Customer IMPORTING VALUE(ls_input) TYPE zst_cust_inp_bl_ubl
                        EXPORTING gt_kna1         TYPE tt_kna1
                                  gt_knb1         TYPE tt_knb1
                                  gt_knvv         TYPE tt_knvv
                                  Message         TYPE string
                                  Type            TYPE bapi_mtype.
  PROTECTED SECTION.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_api_cust_block_unblock IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    TYPES: BEGIN OF ls_err,
             Customer        TYPE kunnr,
             Cust_type       TYPE char50,
*             Company_Code    TYPE bukrs,
*             Sales_Org       TYPE vkorg,
*             Dist_chn        TYPE vtweg,
*             Division        TYPE spart,
*             Block_type      TYPE char50,
             Block_Indicator TYPE char02,
             Message         TYPE string,
             Type            TYPE bapi_mtype,
           END OF ls_err.


    DATA: GT_response TYPE TABLE OF ty_ret_data.
    DATA: Ls_err_log TYPE ls_err,
          gt_kna1    TYPE tt_kna1,
          gt_knb1    TYPE tt_knb1,
          gt_knvv    TYPE tt_knvv.

    DATA: lv_body TYPE string.
    DATA: lv_data    TYPE string,
          lv_message TYPE string,
          lv_type    TYPE bapi_mtype,
          gs_error   TYPE zst_cust_inp_bl_ubl,
          con_txt    TYPE char50,
          dd07v_wa   TYPE  dd07t,
          ls_value   TYPE  dd07l-domvalue_l,
          rc         TYPE sy-subrc.

    CONSTANTS: c_dom TYPE dd07l-domname VALUE 'ZDOM_CTYPE'.
    FIELD-SYMBOLS:<gs_input> TYPE zst_cust_inp_bl_ubl.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      LOOP AT gt_input INTO gs_input.
        gs_input-customer = |{ gs_input-customer ALPHA  = IN }|.
        IF gs_input-customer_type IS INITIAL OR gs_input-customer_type = '00' .
          MOVE-CORRESPONDING gs_input TO ls_err_log.
          Ls_err_log-message = `Enter Valid Block Type`.
          Ls_err_log-type = 'E'.
          APPEND ls_err_log TO gt_response.
          EXIT.
        ENDIF.

*        IF gt_response IS INITIAL.
        validate_customer(
          EXPORTING
            ls_input = gs_input
          IMPORTING
            message  = lv_message
            type     = lv_type
            gt_kna1      = gt_kna1
            gt_knb1      = gt_knb1
            gt_knvv      = gt_knvv
        ).


        IF lv_message IS INITIAL .
          process_data(
        EXPORTING
          ls_input    = gs_input
          lt_kna1     = gt_kna1
          lt_knb1     = gt_knb1
          lt_knvv     = gt_knvv
          IMPORTING
            et_response = gt_response
      ).

        ELSE.
          MOVE-CORRESPONDING gs_input TO Ls_err_log.
          DATA(ls_val) = SWITCH #(  gs_input-customer_type WHEN 'DT' THEN '01' WHEN 'DD' THEN '02' WHEN 'SB' THEN '03' WHEN 'OT' THEN '04'  ).
          SELECT SINGLE FROM dd07v
          FIELDS ddtext
          WHERE domname = @c_dom
          AND  valpos = @ls_val
          INTO @DATA(ls_text).
          IF sy-subrc = 0.
            con_txt = | { gs_input-customer_type } : { ls_text } |.
            ls_err_log-cust_type = con_txt.
          ENDIF.

          Ls_err_log-message = lv_message.
          Ls_err_log-type = lv_type.
          APPEND Ls_err_log TO gt_response.
        ENDIF.
*        ENDIF.
        CLEAR Ls_err_log.
      ENDLOOP.
      IF gt_response IS NOT INITIAL.
**********************************************************************
* serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gt_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = lv_body ).

*Set JSON Content-Type
        CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD server->response->set_cdata( data = lv_body ).

      ENDIF.

    ELSE.

    ENDIF.


  ENDMETHOD.


  METHOD validate_customer.
*01  Central deletion block for master record
*02  Central Deletion Flag for Master Record
*03  Central Posting block (company code level)
*04  Posting block for company code
*05  Deletion Flag for Master Record (Company Code Level)
*06  Deletion block for master record (company code level)
*07  Central order block for customer
*08  Customer order block (sales area)
*09  Central delivery block for the customer
*10  Customer delivery block (sales area)
*11  Central billing block for customer
*12  Billing block for customer (sales and distribution)
*13  Central sales block for customer
*14  Sales Block for Customer (Sales Area)
*15  Deletion flag for customer (sales level)

    CONSTANTS: c_check   TYPE char2 VALUE '01',
               c_nocheck TYPE char2 VALUE ' '.
    DATA: ls_flag TYPE char2 VALUE '01'.
    DATA:lv_message TYPE string.

    DATA: gs_knb1 TYPE knb1,
          gs_knvv TYPE knvv,
          gs_kna1 TYPE kna1.
    CLEAR:message,type.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" validation from table zcust_blk_chk
    SELECT SINGLE FROM zcust_blk_chk
       FIELDS kunnr, block
       WHERE kunnr = @gs_input-customer
       INTO  @DATA(ls_data).
    IF sy-subrc = 0.
   CASE gs_input-customer_type.
    WHEN 'SB' .
     SELECT Single from knb1 fields kunnr, bukrs
       where kunnr = @gs_input-customer and bukrs in ( 'DMS1' ) "'6000', removed as stopped movement
       into @data(ls_knb1).
       IF sy-subrc ne 0.
        message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
          type = 'E'.
          EXIT.
       ENDIF.
       WHEN 'DT' or 'DD' or 'OT'.
         SELECT Single from knb1 fields kunnr, bukrs
       where kunnr = @gs_input-customer and bukrs = '1000'
       into @data(ps_knb1).
       IF sy-subrc ne 0.
        message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
          type = 'E'.
          EXIT.
       ENDIF.
    ENDCASE.
        IF gs_input-block_indicator = 'X' AND ls_data-block = 'X'.
        message = |Customer { gs_input-customer } Is Already Blocked|.
          type = 'S'.
        EXIT.
      ELSEIF gs_input-block_indicator = '' AND ls_data-block = ''.
        message = |Customer { gs_input-customer } Is Already UnBlocked|.
          type = 'S'.
        EXIT.
      ENDIF.
    ENDIF.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Get Customer Data(kna1,knb1,knvv )
    CASE gs_input-customer_type .
      WHEN 'DT' OR 'DD' OR 'OT'.

        SELECT FROM kna1
              FIELDS *
              WHERE kunnr = @gs_input-customer ORDER BY PRIMARY KEY INTO TABLE @gt_kna1.

        IF sy-subrc NE 0.
          message = |Customer Not Present In Masters { gs_input-customer }|.
          type = 'E'.
          EXIT.
        ELSE.
          SELECT FROM knb1
          FIELDS *
          WHERE kunnr = @gs_input-customer AND bukrs = '1000'
          ORDER BY kunnr , bukrs
          INTO TABLE @gt_knb1.
          IF sy-subrc NE 0.
*            message = |Company code 1000 is not available for customer  { gs_input-customer }  |.
             message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
            type = 'E'.
            EXIT.
          ELSE.
            SELECT FROM knvv                       "#EC CI_NO_TRANSFORM
             FIELDS *
             FOR ALL ENTRIES IN @gt_knb1
             WHERE kunnr = @gt_knb1-kunnr AND vkorg = '1000'
             INTO TABLE @gt_knvv.                  "#EC CI_NO_TRANSFORM

            SORT gt_knvv BY kunnr vkorg .
            IF sy-subrc NE 0.
             message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
              type = 'E'.
              EXIT.
            ELSE.
              READ TABLE gt_knb1 INTO gs_knb1 INDEX 1.
              IF sy-subrc = 0.
                message = COND #( WHEN ls_input-block_indicator = '' AND gs_knb1-loevm = ''
                                          THEN |Customer { gs_knb1-kunnr } Is Already UnBlocked|
                                          WHEN ls_input-block_indicator = 'X' AND gs_knb1-loevm = 'X'
                                          THEN |Customer { gs_knb1-kunnr } Is Already Blocked| ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'SB'.
        SELECT FROM kna1
            FIELDS *
            WHERE kunnr = @gs_input-customer ORDER BY PRIMARY KEY INTO TABLE @gt_kna1.
        IF sy-subrc NE 0.
          message = |Customer Not Present In Masters { gs_input-customer }, { message }|.
          type = 'E'.
          EXIT.
        ELSE.
          SELECT FROM knb1
          FIELDS *
          WHERE kunnr = @gs_input-customer AND bukrs IN ( 'DMS1' )  "'6000',
          ORDER BY kunnr , bukrs
          INTO TABLE @gt_knb1.
          IF sy-subrc NE 0.
*            message = |Company codes 6000, DMS1 is not available for customer  { gs_input-customer }  |.
               message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
            type = 'E'.
            EXIT.
          ELSE.
            IF NOT Line_exists( gt_knb1[ bukrs = '6000' ] ).
              message = |Company codes 6000 is not available for customer  { gs_input-customer }  |.
              type = 'E'.
              EXIT.
            ENDIF.
            IF NOT Line_exists( gt_knb1[ bukrs = 'DMS1' ] ).
              message = |Company codes DMS1 is not available for customer  { gs_input-customer }  |.
              type = 'E'.
              EXIT.
            ENDIF.
            SELECT FROM knvv                       "#EC CI_NO_TRANSFORM
                FIELDS  *
                  FOR ALL ENTRIES IN @gt_knb1
                  WHERE kunnr = @gt_knb1-kunnr AND vkorg IN ( 'SDMS' ) INTO TABLE @gt_knvv ."'6000' ,
            IF sy-subrc NE 0.
                message = |Invalid Customer Type. Please Check the Customer type for Customer { gs_input-customer }  |.
*              message = |Sales organization 6000, SDMS is not available for the customer { gs_input-customer } in th Company Codes 6000, DMS1  |.
              type = 'E'.
              EXIT.
            ELSE.
              IF NOT Line_exists( gt_knvv[ vkorg = '6000' ] ).
                message = |Sales Organization 6000 is not available for customer  { gs_input-customer }  |.
                type = 'E'.
                EXIT.
              ENDIF.
              IF NOT Line_exists( gt_knvv[ vkorg = 'SDMS' ] ).
                message = |Sales Organization SDMS is not available for customer  { gs_input-customer }  |.
                type = 'E'.
                EXIT.
              ENDIF.

              READ TABLE gt_knb1 INTO gs_knb1 WITH KEY bukrs = 'DMS1'. "6000 company stopped
              IF sy-subrc = 0.
                message = COND #( WHEN ls_input-block_indicator = '' AND gs_knb1-loevm = ''
                                      THEN |Customer { gs_knb1-kunnr } Is Already UnBlocked|
                                      WHEN ls_input-block_indicator = 'X' AND gs_knb1-loevm = 'X'
                                      THEN |Customer { gs_knb1-kunnr } Is Already Blocked|  ).
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        message = |Enter Valid Customer Type|.
        type = 'E'.
    ENDCASE.

    IF message IS NOT INITIAL.
      message = message.
      type = 'S'.
    ENDIF.


  ENDMETHOD.


  METHOD process_data.

    CONSTANTS: c_check   TYPE char2 VALUE '01',
               c_nocheck TYPE char2 VALUE ' '.
    DATA: cx_err      TYPE REF TO cx_root,
          err_msg     TYPE ty_ret_data,
          success_msg TYPE ty_ret_data,
          t_xknas     TYPE TABLE OF fknas,
          t_xknb5     TYPE TABLE OF fknb5,
          t_xknbk     TYPE TABLE OF fknbk,
          t_xknva     TYPE TABLE OF fknva,
          t_xknvd     TYPE TABLE OF fknvd,
          t_xknvi     TYPE TABLE OF fknvi,
          t_xknvk     TYPE TABLE OF fknvk,
          t_xknvl     TYPE TABLE OF fknvl,
          t_xknvp     TYPE TABLE OF fknvp,
          t_xknvs     TYPE TABLE OF fknvs,
          t_xknza     TYPE TABLE OF fknza,
          t_yknas     TYPE TABLE OF fknas,
          t_yknb5     TYPE TABLE OF fknb5,
          t_yknbk     TYPE TABLE OF fknbk,
          t_yknva     TYPE TABLE OF fknva,
          t_yknvd     TYPE TABLE OF fknvd,
          t_yknvi     TYPE TABLE OF fknvi,
          t_yknvk     TYPE TABLE OF fknvk,
          t_yknvl     TYPE TABLE OF fknvl,
          t_yknvp     TYPE TABLE OF fknvp,
          t_yknvs     TYPE TABLE OF fknvs,
          t_yknza     TYPE TABLE OF fknza.
    DATA: ls_upd TYPE zcust_blk_chk.
*01  Central deletion block for master record
*02  Central Deletion Flag for Master Record
*03  Central Posting block (company code level)
*04  Posting block for company code
*05  Deletion Flag for Master Record (Company Code Level)
*06  Deletion block for master record (company code level)
*07  Central order block for customer
*08  Customer order block (sales area)
*09  Central delivery block for the customer
*10  Customer delivery block (sales area)
*11  Central billing block for customer
*12  Billing block for customer (sales and distribution)
*13  Central sales block for customer
*14  Sales Block for Customer (Sales Area)
*15  Deletion flag for customer (sales level)
    DATA: gs_kna1  TYPE kna1,
          gs_knb1  TYPE knb1,
          gs_knvv  TYPE knvv,
          y_kna1   TYPE kna1,
          y_knb1   TYPE knb1,
          gt_kna1  TYPE TABLE OF kna1,
          ls_ddo70 TYPE dd07v,
          con_txt  TYPE char50,
          ls_text  TYPE char50.

    READ TABLE lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) INDEX 1.
*          loop at lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).
    IF <fs_kna1> IS ASSIGNED.
      gs_kna1 = CORRESPONDING #( <fs_kna1> ).
      IF ls_input-block_indicator = 'X'.

        gs_kna1-nodel = 'X'.
        gs_kna1-loevm = 'X'.

      ELSE.

        gs_kna1-nodel = ''.
        gs_kna1-loevm = ''.
      ENDIF.
    ENDIF.
    LOOP AT lt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>) .
      DATA(lv_index) = sy-tabix.
      gs_knvv = CORRESPONDING #( <fs_knvv> ).
      IF ls_input-block_indicator = 'X'.
        gs_knvv-aufsd = c_check.
        gs_knvv-lifsd = c_check.
        gs_knvv-faksd = c_check.
        gs_knvv-cassd = 'X'.
        gs_knvv-loevm = 'X'.
      ELSE.
        gs_knvv-aufsd = c_nocheck.
        gs_knvv-lifsd = c_nocheck.
        gs_knvv-faksd = c_nocheck.
        gs_knvv-cassd = ''.
        gs_knvv-loevm = ''.
      ENDIF.
      READ TABLE lt_knb1 ASSIGNING FIELD-SYMBOL(<fs_knb1>) INDEX lv_index.
      IF <fs_knb1> IS ASSIGNED.
        gs_knb1 = CORRESPONDING #( <fs_knb1> ).

        IF ls_input-block_indicator = 'X'.
          gs_knb1-loevm = 'X'.
          gs_knb1-nodel = 'X'.
*          gs_knb1-sperr = 'X'.
        ELSE.
          gs_knb1-loevm = ''.
          gs_knb1-nodel = ''.
*          gs_knb1-sperr = ''.
        ENDIF.

      ENDIF.

      TRY.
          CALL FUNCTION 'CUSTOMER_UPDATE'
            EXPORTING
              i_kna1  = gs_kna1
              i_knb1  = gs_knb1
              i_knvv  = gs_knvv
              i_ykna1 = y_kna1
              i_yknb1 = y_knb1
            TABLES
              t_xknas = t_xknas
              t_xknb5 = t_xknb5
              t_xknbk = t_xknbk
              t_xknva = t_xknva
              t_xknvd = t_xknvd
              t_xknvi = t_xknvi
              t_xknvk = t_xknvk
              t_xknvl = t_xknvl
              t_xknvp = t_xknvp
              t_xknvs = t_xknvs
              t_xknza = t_xknza
              t_yknas = t_yknas
              t_yknb5 = t_yknb5
              t_yknbk = t_yknbk
              t_yknva = t_yknva
              t_yknvd = t_yknvd
              t_yknvi = t_yknvi
              t_yknvk = t_yknvk
              t_yknvl = t_yknvl
              t_yknvp = t_yknvp
              t_yknvs = t_yknvs
              t_yknza = t_yknza.



          CLEAR:gs_kna1,gs_knb1,gs_knvv.

        CATCH cx_root INTO cx_err.
          DATA(ls_val) = SWITCH #(  gs_input-customer_type WHEN 'DT' THEN '01' WHEN 'DD' THEN '02' WHEN 'SB' THEN '03' WHEN 'OT' THEN '04'  ).
          SELECT SINGLE FROM dd07v
         FIELDS ddtext
         WHERE domname = 'ZDOM_CTYPE'
         AND  valpos = @ls_val
         INTO @ls_text.
          IF sy-subrc = 0.
            con_txt = | { gs_input-customer_type } : { ls_text } , { con_txt } |.
          ENDIF.

          err_msg = VALUE ty_ret_data(
              customer        = ls_input-customer    customer_type =  con_txt  block_indicator =  ls_input-block_indicator
             message         = cx_err->get_text( )   type  = 'E'
          ).
          APPEND err_msg TO et_response.
          EXIT.
      ENDTRY.
      COMMIT WORK.
      ls_upd = VALUE #( kunnr = <fs_knb1>-kunnr name1 = <fs_kna1>-name1 bukrs = <fs_knb1>-bukrs vkorg = <fs_knvv>-vkorg block = ls_input-block_indicator ).
      MODIFY zcust_blk_chk FROM ls_upd.
      COMMIT WORK.

      AT LAST.
        DATA(gs_val) = SWITCH #(  gs_input-customer_type WHEN 'DT' THEN '01' WHEN 'DD' THEN '02' WHEN 'SB' THEN '03' WHEN 'OT' THEN '04'  ).
        SELECT SINGLE FROM dd07v
       FIELDS ddtext
       WHERE domname = 'ZDOM_CTYPE'
       AND  valpos = @gs_val
       INTO @ls_text.
        IF sy-subrc = 0.
          con_txt = | { gs_input-customer_type } : { ls_text }  |.
        ENDIF.

        IF ls_input-block_indicator = 'X'.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" Updating Mis_customer_block_table
          data(gs_mis_cust) = VALUE zmis_cust_st( customer_no = ls_input-customer status = '02' created_by = sy-uname create_on = sy-datum
                                                      changed_by = sy-uname changed_on = sy-datum  ).
           MODIFY zmis_cust_st FROM gs_mis_cust .
           COMMIT WORK.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          success_msg = VALUE ty_ret_data(
           customer        = ls_input-customer  customer_type =  con_txt   block_indicator =  ls_input-block_indicator
          message         = |Customer { ls_input-customer } Is Blocked|   type   = 'S'
       ).
          APPEND success_msg TO et_response.
          clear gs_mis_cust.
        ELSE.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" Updating Mis_customer_block_table
         data(ls_mis_cust) = VALUE zmis_cust_st( customer_no = ls_input-customer status = '01' created_by = sy-uname create_on = sy-datum
                                                      changed_by = sy-uname changed_on = sy-datum  ).
           MODIFY zmis_cust_st FROM ls_mis_cust .
           COMMIT WORK..
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          success_msg = VALUE ty_ret_data(
       customer        = ls_input-customer  customer_type =  con_txt   block_indicator =  ls_input-block_indicator
      message         = |Customer { ls_input-customer } Is UnBlocked|   type   = 'S'
   ).
          APPEND success_msg TO et_response.
          clear ls_mis_cust.
        ENDIF.
      ENDAT.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
