*&---------------------------------------------------------------------*
*& Include          ZSD_INV_TO_CSV_CLS
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.

DATA: gv_name TYPE vrm_id,
      gt_list TYPE vrm_values,
      val     TYPE vrm_value.
DATA: lv_kunnr TYPE kna1-kunnr,
      lv_vbeln TYPE vbrk-vbeln.

DATA: gv_token TYPE zriauthstr.
*DATA:p_mail TYPE screen-name.
*************Selection Screen Design ***********************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_ctype TYPE zsd_sf_cust_inv-fintype OBLIGATORY,
              p_septr TYPE char01.
  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS: s_kunnr FOR lv_kunnr,
                    s_invno FOR lv_vbeln.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_r1 RADIOBUTTON GROUP rad1,
                p_r2 RADIOBUTTON GROUP rad1.
  SELECTION-SCREEN END OF BLOCK b3.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETERS:p_r3 AS LISTBOX VISIBLE LENGTH 20 MODIF ID rfg.
  SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_R3'
      values = gt_list.


INITIALIZATION.
  val-key = 1.
  val-text = 'Get Payment Status'.
  CONDENSE val-key.
  APPEND val TO gt_list.

  val-key = 2.
  val-text = 'Approve Status'.
  CONDENSE val-key.
  APPEND val TO gt_list.

  val-key = '' .
  val-text = ''.
  APPEND val TO gt_list.

*************************************************************************
CLASS lcl_inv_to_csv_format DEFINITION.
  PUBLIC SECTION.
**************************RF customer input json format structure*************
    TYPES : BEGIN OF ty_amt,
              value    TYPE netwr,
              currency TYPE string,
            END OF ty_amt.

    TYPES : BEGIN OF ty_addr,
              address_Line1 TYPE string,
              address_Line2 TYPE string,
              city          TYPE string,
              state         TYPE string,
              landmark      TYPE string,
              address_Type  TYPE string,
              pincode       TYPE string,
              country       TYPE string,
            END OF ty_addr.

    TYPES : BEGIN OF ty_meta,
              identifier1 TYPE string,
              identifier2 TYPE string,
              identifier3 TYPE string,
              orderitem1  TYPE string,
              orderitem2  TYPE string,
              orderitem3  TYPE string,
              orderitem4  TYPE string,
              orderitem5  TYPE string,
            END OF ty_meta.

    TYPES : BEGIN OF ty_doc,
              merchant_doc_ref_id TYPE string,
              doc_type            TYPE string,
              doc_format          TYPE string,
              data                TYPE string,
            END OF ty_doc,
            tt_doc TYPE TABLE OF ty_doc WITH DEFAULT KEY.

    TYPES : BEGIN OF ty_input,
              merchant_Payment_Ref_Id  TYPE string,
              amount                   TYPE ty_amt,
              address                  TYPE ty_addr,
              callback_Url             TYPE string,
              auto_Capture             TYPE string,
              merchant_Customer_Ref_Id TYPE string,
*              termtype              TYPE string,
              invoice_Date             TYPE string,
              documents                TYPE tt_doc,
            END OF ty_input.

**************************RF customer output json format structure*************
    TYPES : BEGIN OF ty_amt2,
              value           TYPE string,
              formatted_Value TYPE string,
              currency        TYPE string,
            END OF ty_amt2.
    TYPES : BEGIN OF ty_data2,
              payment_Id               TYPE string,
              merchant_payment_ref_id  TYPE string,
              amount                   TYPE ty_amt2,
              payment_date             TYPE string,
              payment_Url              TYPE string,
              status                   TYPE string,
              auto_Capture             TYPE string,
              uncapturedAmount         TYPE ty_amt2,
              accountId                TYPE string,
              merchant_Customer_Ref_Id TYPE string,
              documents                TYPE tt_doc,
              address                  TYPE ty_addr,
              token                    TYPE string,
            END OF ty_data2.


    TYPES : BEGIN OF ty_output,
              success    TYPE string,
              message    TYPE string,
              request_Id TYPE string,
              timestamp  TYPE string,
              data       TYPE ty_data2,
            END OF ty_output.
    TYPES: BEGIN OF ty_rf_cust,
             InvoiceKey    TYPE zinvkey,
             bukrs         TYPE bukrs,
             status        TYPE z_sf_status,
             Finance       TYPE zfincode,
             Customer      TYPE kunnr,
             Name          TYPE kna1-Name1,
             invoiceno     TYPE vbeln,
             invoicedate   TYPE budat,
             invoicetype   TYPE fkart,
             amount        TYPE netwr,
             payment_refid TYPE zsd_sf_cust_inv-payment_refid,
             payment_url   TYPE zsd_sf_cust_inv-payment_url,
           END OF ty_rf_cust.
    DATA: gt_rf_inv_data TYPE STANDARD TABLE OF ty_rf_cust,
          gs_output      TYPE ty_output.
    TYPES: pt_comms TYPE TABLE OF zcus_cf_cumm.
*TYPES: lt_rf_inv_data type TABLE of zsd_sf_cust_inv .
*data: gt_rf_data type standard TABLE of zsd_sf_cust_inv.
    METHODS: active_invoice_fetch,
      cancel_invoice_fetch,
      script_execution,
      build_alv,
      rf_active_inv_fetch,
      rf_get_pament_status.
    METHODS: rf_get_token IMPORTING im_custype TYPE zfincode
                          EXPORTING ex_token   TYPE zriauthstr.
  PRIVATE SECTION.
*** CSV File Format ***
    TYPES: BEGIN OF ty_csv_file,
             counterparty          TYPE string,
             invoiceno             TYPE string,
             invoicedate           TYPE string,
             pono                  TYPE string,
             basicamount           TYPE string,
             invoiceamount         TYPE string,
             duedate               TYPE string,
             dueamount             TYPE string,
             gstin                 TYPE string,
             pan                   TYPE string,
             partycode             TYPE string,
             sapdocument           TYPE string,
             invoicefileidentifier TYPE string,
             addinfo               TYPE string,
           END OF ty_csv_file.

    TYPES:BEGIN OF ty_get_py,
            merchantPaymenyRefId TYPE string,
            Customer             TYPE string,
            Invoice_amount       TYPE string,
            Invoice_date         TYPE zsd_sf_cust_inv-invoicedate,
            Auto_capture         TYPE string,
            Status               TYPE string,
            Created_on           TYPE char256,
            created_at           TYPE char256,
          END OF ty_get_py.

    DATA: gt_get_py TYPE TABLE OF ty_get_py.
    DATA: gt_inv_table  TYPE TABLE OF ty_csv_file,
          gt_canc_table TYPE TABLE OF ty_csv_file,
          gt_inv_data   TYPE STANDARD TABLE OF zsd_sf_cust_inv,
          gt_canc_data  TYPE STANDARD TABLE OF zsd_sf_cust_inv,
          gt_alv_table  TYPE TABLE OF ty_csv_file,
          gt_truxus     TYPE truxs_t_text_data.
    DATA: gv_inv_amt   TYPE vbrp-netwr,
          gv_duedt     TYPE vbrk-fkdat,
          gv_inv_fname TYPE char100,
          gv_can_fname TYPE char100.
    METHODS:

**********************************************************************RupiFi

      rf_mail_to_dist IMPORTING VALUE(ls_output)    TYPE ty_output
                                VALUE(ls_input)     TYPE zsd_sf_cust_inv
                                VALUE(gt_comms)     TYPE pt_comms
                      RETURNING VALUE(ls_mail_resp) TYPE string  ,
      rf_whatsapp_dist IMPORTING VALUE(ls_output)    TYPE ty_output
                                 VALUE(gs_cust_inv)  TYPE zsd_sf_cust_inv
                                 VALUE(gt_comms)     TYPE pt_comms
                       RETURNING VALUE(gv_http_stat) TYPE string  .





ENDCLASS.

CLASS lcl_inv_to_csv_format IMPLEMENTATION.
  METHOD active_invoice_fetch.
    REFRESH gt_inv_data.
    SELECT * FROM zsd_sf_cust_inv INTO TABLE gt_inv_data
      WHERE status = '12'
      AND fintype = p_ctype
      AND custno IN s_kunnr
      AND invoiceno IN s_invno.
    IF sy-subrc EQ 0.
      REFRESH gt_inv_table.
      DATA(gs_inv_table) = VALUE ty_csv_file( counterparty          = 'CounterParty'
                                              invoiceno             = 'Invoice No'
                                              invoicedate           = 'Invoice Date'
                                              pono                  = 'PO No'
                                              basicamount           = 'Basic Amount'
                                              invoiceamount         = 'Invoice Amount'
                                              duedate               = 'Due Date'
                                              dueamount             = 'Due Amount'
                                              gstin                 = 'GSTIN'
                                              pan                   = 'PAN'
                                              partycode             = 'Party Code'
                                              sapdocument           = 'SAP document'
                                              invoicefileidentifier = 'Invoice file identifier'
                                              addinfo               = 'Addinfo' ).
      APPEND gs_inv_table TO gt_inv_table.
      LOOP AT gt_inv_data ASSIGNING FIELD-SYMBOL(<fgs_data>).
        CLEAR gs_inv_table.
        gs_inv_table-counterparty          = <fgs_data>-custname.
        gs_inv_table-invoiceno             = <fgs_data>-invoicekey.
        DATA(lv_datum)                     = CONV char10( space ).
        WRITE <fgs_data>-invoicedate TO lv_datum DD/MM/YYYY.
        gs_inv_table-invoicedate           = lv_datum.
        gs_inv_table-basicamount           = <fgs_data>-invoiceamount.
        gs_inv_table-invoiceamount         = <fgs_data>-invoiceamount.
        DATA(lv_duedate)                   = CONV char10( space ).
        WRITE <fgs_data>-duedate TO lv_duedate DD/MM/YYYY.
        gs_inv_table-duedate               = lv_duedate.
        gs_inv_table-dueamount             = <fgs_data>-dueamount.
        gs_inv_table-gstin                 = <fgs_data>-gstn.
        gs_inv_table-pan                   = <fgs_data>-pan.
        gs_inv_table-partycode             = <fgs_data>-custno.
        SHIFT gs_inv_table-partycode LEFT DELETING LEADING '0'.
        gs_inv_table-sapdocument           = <fgs_data>-invoiceno.
        gs_inv_table-invoicefileidentifier = <fgs_data>-inv_file_identifier.
        APPEND gs_inv_table TO gt_inv_table.
        CLEAR: lv_datum,lv_duedate.
      ENDLOOP.
*Conversion of internal table to CSV Format
      CLEAR: gt_truxus.
      CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
        EXPORTING
          i_field_seperator    = p_septr
        TABLES
          i_tab_sap_data       = gt_inv_table
        CHANGING
          i_tab_converted_data = gt_truxus
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc = 0.
        IF gt_truxus IS NOT INITIAL.
          SELECT SINGLE low FROM tvarvc INTO @DATA(ls_fname)
            WHERE name = 'FILENAME_SF'
            AND type = 'P'.
          IF sy-subrc EQ 0.
            DATA(l_fname) = CONV char50( ls_fname ).
          ENDIF.
          CLEAR: gv_inv_fname.
          gv_inv_fname = |{ l_fname }AASCS5073J_INV_{ sy-datum }_{ sy-uzeit }.CSV|.
          OPEN DATASET gv_inv_fname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            LOOP AT gt_truxus INTO DATA(lw_truxus).
              TRANSFER lw_truxus TO gv_inv_fname.
            ENDLOOP.
            CLOSE DATASET gv_inv_fname.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cancel_invoice_fetch.
    REFRESH gt_canc_data.
    SELECT * FROM zsd_sf_cust_inv INTO TABLE gt_canc_data
         WHERE status = '17'
         AND fintype = p_ctype
         AND custno IN s_kunnr
         AND invoiceno IN s_invno.
    IF sy-subrc EQ 0.
      REFRESH gt_canc_table.
      DATA(gs_canc_table) = VALUE ty_csv_file( counterparty          = 'CounterParty'
                                               invoiceno             = 'Invoice No'
                                               invoicedate           = 'Invoice Date'
                                               pono                  = 'PO No'
                                               basicamount           = 'Basic Amount'
                                               invoiceamount         = 'Invoice Amount'
                                               duedate               = 'Due Date'
                                               dueamount             = 'Due Amount'
                                               gstin                 = 'GSTIN'
                                               pan                   = 'PAN'
                                               partycode             = 'Party Code'
                                               sapdocument           = 'SAP document'
                                               invoicefileidentifier = 'Invoice file identifier'
                                               addinfo               = 'Addinfo' ).
      APPEND gs_canc_table TO gt_canc_table.
      LOOP AT gt_canc_data ASSIGNING FIELD-SYMBOL(<fgs_data>).
        CLEAR gs_canc_table.
        gs_canc_table-counterparty          = <fgs_data>-custname.
        gs_canc_table-invoiceno             = <fgs_data>-invoicekey.
        DATA(lv_datum)                      = CONV char10( space ).
        WRITE <fgs_data>-invoicedate TO lv_datum DD/MM/YYYY.
        gs_canc_table-invoicedate           = lv_datum.
        gs_canc_table-basicamount           = <fgs_data>-invoiceamount.
        gs_canc_table-invoiceamount         = <fgs_data>-invoiceamount.
        DATA(lv_duedate)                    = CONV char10( space ).
        WRITE <fgs_data>-duedate TO lv_duedate DD/MM/YYYY.
        gs_canc_table-duedate               = lv_duedate.
        gs_canc_table-dueamount             = <fgs_data>-dueamount.
        gs_canc_table-gstin                 = <fgs_data>-gstn.
        gs_canc_table-pan                   = <fgs_data>-pan.
        gs_canc_table-partycode             = <fgs_data>-custno.
        SHIFT gs_canc_table-partycode LEFT DELETING LEADING '0'.
        gs_canc_table-sapdocument           = <fgs_data>-invoiceno.
        gs_canc_table-invoicefileidentifier = <fgs_data>-inv_file_identifier.
        APPEND gs_canc_table TO gt_canc_table.
        CLEAR: lv_datum,lv_duedate.
      ENDLOOP.
*Conversion of internal table to CSV Format
      CLEAR: gt_truxus.
      CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
        EXPORTING
          i_field_seperator    = p_septr
        TABLES
          i_tab_sap_data       = gt_canc_table
        CHANGING
          i_tab_converted_data = gt_truxus
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc = 0.
        IF gt_truxus IS NOT INITIAL.
          SELECT SINGLE low FROM tvarvc INTO @DATA(ls_fname)
            WHERE name = 'FILENAME_CANC_SF'
            AND type = 'P'.
          IF sy-subrc EQ 0.
            DATA(l_fname) = CONV char50( ls_fname ).
          ENDIF.
          CLEAR: gv_can_fname.
          gv_can_fname = |{ l_fname }AASCS5073J_CANCEL_INV{ sy-datum }_{ sy-uzeit }.CSV|.
          OPEN DATASET gv_can_fname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            LOOP AT gt_truxus INTO DATA(lw_truxus).
              TRANSFER lw_truxus TO gv_can_fname.
            ENDLOOP.
            CLOSE DATASET gv_can_fname.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD script_execution.
    IF gt_inv_table[] IS NOT INITIAL.
      "invoice Documents transfer
      CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
        EXPORTING
          commandname                   = 'ZSF_INVDOCS'
*         ADDITIONAL_PARAMETERS         =
*         OPERATINGSYSTEM               = SY-OPSYS
*         TARGETSYSTEM                  = SY-HOST
*         DESTINATION                   =
*         STDOUT                        = 'X'
*         STDERR                        = 'X'
*         TERMINATIONWAIT               = 'X'
*         TRACE                         =
*         DIALOG                        =
*     IMPORTING
*         STATUS                        =
*         EXITCODE                      =
*     TABLES
*         EXEC_PROTOCOL                 =
        EXCEPTIONS
          no_permission                 = 1
          command_not_found             = 2
          parameters_too_long           = 3
          security_risk                 = 4
          wrong_check_call_interface    = 5
          program_start_error           = 6
          program_termination_error     = 7
          x_error                       = 8
          parameter_expected            = 9
          too_many_parameters           = 10
          illegal_command               = 11
          wrong_asynchronous_parameters = 12
          cant_enq_tbtco_entry          = 13
          jobcount_generation_error     = 14
          OTHERS                        = 15.
      IF sy-subrc = 0.
        LOOP AT gt_inv_data ASSIGNING FIELD-SYMBOL(<fs_inv_data>).
          <fs_inv_data>-status        = '13'.
          <fs_inv_data>-filename      = gv_inv_fname.
          <fs_inv_data>-filesent_date = sy-datum.
          <fs_inv_data>-filesent_time = sy-uzeit.
        ENDLOOP.
        MODIFY zsd_sf_cust_inv FROM TABLE gt_inv_data.
      ENDIF.
      "Documents PDF transfer
      CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
        EXPORTING
          commandname                   = 'ZSF_DOCSEND'
*         ADDITIONAL_PARAMETERS         =
*         OPERATINGSYSTEM               = SY-OPSYS
*         TARGETSYSTEM                  = SY-HOST
*         DESTINATION                   =
*         STDOUT                        = 'X'
*         STDERR                        = 'X'
*         TERMINATIONWAIT               = 'X'
*         TRACE                         =
*         DIALOG                        =
*     IMPORTING
*         STATUS                        =
*         EXITCODE                      =
*     TABLES
*         EXEC_PROTOCOL                 =
        EXCEPTIONS
          no_permission                 = 1
          command_not_found             = 2
          parameters_too_long           = 3
          security_risk                 = 4
          wrong_check_call_interface    = 5
          program_start_error           = 6
          program_termination_error     = 7
          x_error                       = 8
          parameter_expected            = 9
          too_many_parameters           = 10
          illegal_command               = 11
          wrong_asynchronous_parameters = 12
          cant_enq_tbtco_entry          = 13
          jobcount_generation_error     = 14
          OTHERS                        = 15.
      IF sy-subrc = 0.

      ENDIF.
    ENDIF.

    IF gt_canc_table[] IS NOT INITIAL.
      "Cancel invoice Documents transfer
      CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
        EXPORTING
          commandname                   = 'ZSF_CANINV'
*         ADDITIONAL_PARAMETERS         =
*         OPERATINGSYSTEM               = SY-OPSYS
*         TARGETSYSTEM                  = SY-HOST
*         DESTINATION                   =
*         STDOUT                        = 'X'
*         STDERR                        = 'X'
*         TERMINATIONWAIT               = 'X'
*         TRACE                         =
*         DIALOG                        =
*     IMPORTING
*         STATUS                        =
*         EXITCODE                      =
*     TABLES
*         EXEC_PROTOCOL                 =
        EXCEPTIONS
          no_permission                 = 1
          command_not_found             = 2
          parameters_too_long           = 3
          security_risk                 = 4
          wrong_check_call_interface    = 5
          program_start_error           = 6
          program_termination_error     = 7
          x_error                       = 8
          parameter_expected            = 9
          too_many_parameters           = 10
          illegal_command               = 11
          wrong_asynchronous_parameters = 12
          cant_enq_tbtco_entry          = 13
          jobcount_generation_error     = 14
          OTHERS                        = 15.
      IF sy-subrc = 0.
        LOOP AT gt_canc_data ASSIGNING FIELD-SYMBOL(<fs_canc_data>).
          <fs_canc_data>-status            = '13'.
          <fs_canc_data>-can_filename      = gv_can_fname.
          <fs_canc_data>-can_filesent_date = sy-datum.
          <fs_canc_data>-can_filesent_time = sy-uzeit.
        ENDLOOP.
        MODIFY zsd_sf_cust_inv FROM TABLE gt_canc_data.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.
    DATA: columns    TYPE REF TO cl_salv_columns_table,
          column     TYPE REF TO cl_salv_column,
          colNames   TYPE salv_t_column_ref,
          colName    LIKE LINE OF colNames,
          txtMedium  TYPE scrtext_m,
          lo_header  TYPE REF TO cl_salv_form_layout_grid,
          lo_h_label TYPE REF TO cl_salv_form_label,
          lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_display   TYPE REF TO cl_salv_display_settings.
    CASE p_ctype.
      WHEN 'SF'.
        DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
        APPEND LINES OF gt_inv_table[] TO gt_alv_table[].
        APPEND LINES OF gt_canc_table[] TO gt_alv_table[].
* create the alv object
        IF gt_alv_table[] IS NOT INITIAL.
          TRY.
              CALL METHOD cl_salv_table=>factory
                IMPORTING
                  r_salv_table = lo_gr_alv
                CHANGING
                  t_table      = gt_alv_table.
            CATCH cx_salv_msg.
          ENDTRY.
          lo_gr_alv->display( ).
        ELSE.
          WRITE: 'No Files to Upload'.
        ENDIF.
      WHEN 'RF'.
        DATA :lo_rf_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
        gt_rf_inv_data = VALUE #( FOR ls_rf_data IN gt_inv_data
                                  ( invoicekey = ls_rf_data-invoicekey
                                    bukrs = ls_rf_data-bukrs
                                    status = ls_rf_data-status
                                    finance = ls_rf_data-fintype
                                    customer = ls_rf_data-custno
                                    name = ls_rf_data-custname
                                    invoiceno = ls_rf_data-invoiceno
                                    invoicedate = ls_rf_data-invoicedate
                                    invoicetype = ls_rf_data-invoicetype
                                    amount = ls_rf_data-invoiceamount
                                    payment_refid = ls_rf_data-payment_refid
                                    payment_url = ls_rf_data-payment_url ) ).
*
*    APPEND LINES OF gt_inv_data TO gt_rf_inv_data[].
*    APPEND LINES OF gt_canc_table[] TO gt_alv_table[].
        IF gt_rf_inv_data[] IS NOT INITIAL.
          TRY.
              CALL METHOD cl_salv_table=>factory
                IMPORTING
                  r_salv_table = lo_rf_alv
                CHANGING
                  t_table      = gt_rf_inv_data.
            CATCH cx_salv_msg.
          ENDTRY.
          TRY.
              columns = lo_rf_alv->get_columns( ).
              columns->set_optimize(
                  value = abap_true
              ).
              column = columns->get_column( columnname = 'PAYMENT_REFID' ).
              column->set_long_text( value = 'PaymentRedID' ).

              column = columns->get_column( columnname = 'PAYMENT_URL' ).
              column->set_long_text( value = 'PaymentURL' ).

            CATCH cx_salv_not_found.
          ENDTRY.
          lo_rf_alv->display( ).
        ELSEIF gt_get_py[]  IS NOT INITIAL.
          TRY.
              CALL METHOD cl_salv_table=>factory
                IMPORTING
                  r_salv_table = lo_rf_alv
                CHANGING
                  t_table      = gt_get_py.
            CATCH cx_salv_msg.
          ENDTRY.
          TRY.
              columns = lo_rf_alv->get_columns( ).
              columns->set_optimize(
                  value = abap_true
              ).
              lo_functions = lo_rf_alv->get_functions( ).
              lo_functions->set_default( abap_true ).
**********************************************************************Set Column width and Column name
              column = columns->get_column( columnname = 'MERCHANTPAYMENYREFID' ).
              column->set_long_text( value = 'MerchantPaymentRefId ' ).

              column = columns->get_column( columnname = 'CUSTOMER' ).
              column->set_medium_text( value = 'CustomerRefId' ).
              column->set_long_text( value = 'MerchantCustomerRefId' ).

              column = columns->get_column( columnname = 'INVOICE_AMOUNT' ).
              column->set_medium_text( value = 'Invoice_Amount' ).
              column->set_long_text( value = 'Invoice_Amount' ).

              column = columns->get_column( columnname = 'AUTO_CAPTURE' ).
              column->set_short_text( value = 'AutoCap' ).
              column->set_long_text( value = 'Auto Capture' ).

              column = columns->get_column( columnname = 'STATUS' ).
              column->set_short_text( value = 'Status' ).
              column->set_medium_text( value = 'Status' ).
              column->set_long_text( value = 'Status' ).

              column = columns->get_column( columnname = 'CREATED_ON' ).
              column->set_short_text( value = 'Created_on' ).
              column->set_medium_text( value = 'Created_on' ).
              column->set_long_text( value = 'Created_on' ).

              column = columns->get_column( columnname = 'CREATED_AT' ).
              column->set_short_text( value = 'Created_at' ).
              column->set_medium_text( value = 'Created_at' ).
              column->set_long_text( value = 'Created_at' ).
**********************************************************************Create Top header
              CREATE OBJECT lo_header.
*     information in Bold
              lo_h_label = lo_header->create_label( row = 1 column = 1 ).
              IF p_r3 = 1.
                lo_h_label->set_text( 'RupiFi GET Payment Status' ).
              ELSEIF p_r3 = 2.
                lo_h_label->set_text( 'RupiFi Approve Payment Status' ).
              ENDIF.
              lo_h_label->get_tooltip( ).
              lo_rf_alv->set_top_of_list( value =  lo_header ).
**********************************************************************Create Zebra style rows
              lo_display = lo_rf_alv->get_display_settings( ).
              lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
            CATCH cx_salv_not_found.
          ENDTRY.
          lo_rf_alv->display( ).
        ELSE.
          WRITE: 'No Files to Upload'.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD rf_active_inv_fetch.

    DATA : ls_input        TYPE ty_input,
           ls_output       TYPE ty_output,
           iv_payload_json TYPE string,
           lv_token        TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.
    DATA: ls_true TYPE string VALUE 'true',
          ls_repl TYPE string VALUE '"true"'.
    DATA: lt_cr_pymnt_res TYPE TABLE OF zrf_cr_pymt_res,
          gt_comms        TYPE TABLE OF zcus_cf_cumm.
    DATA: lv_mail_res  TYPE string,
          lv_whats_res TYPE string,
          v_jsonload   TYPE string.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
* ****************fetch the finance customer invoices***********
    SELECT * FROM zsd_sf_cust_inv
             INTO TABLE @gt_inv_data
             WHERE invoiceno IN @s_invno
             AND   custno    IN @s_kunnr
             AND   fintype = 'RF'
             AND   status    = '12'
             AND   invstatus NE 'CAN'.

    IF sy-subrc = 0.
      """"Get Customer Communication data
      SELECT  FROM zcus_cf_cumm
      FIELDS *
      FOR ALL ENTRIES IN @gt_inv_data
      WHERE fintype = 'RF' AND kunnr = @gt_inv_data-custno "AND com_type in ( 30, 20 )
      INTO TABLE @DATA(It_comms).
*      DATA(l_url) = 'https://credit-line-api-sandbox.rupifi.com/v2.1/payments'.
********************get the URL from the table********************
      SELECT SINGLE * FROM zfi_cf_params INTO @DATA(ls_url) WHERE zsystem  = @sy-sysid
                                                            AND   zcustype = 'RF'
                                                            AND   zfldtype = 'PYURL'.

      IF sy-subrc = 0.
*****************auth token conversion*******************
        DATA(create_url) = CONV string( ls_url-zfldvalue ).
        CONDENSE : gv_token.
        CLEAR    : lv_token.
        lv_token = |Bearer { gv_token }|.
        SORT gt_inv_data BY invoiceno.

        LOOP AT gt_inv_data INTO DATA(ls_inv_data)."ASSIGNING FIELD-SYMBOL(<fs>).
          DATA(lv_index) = sy-tabix.
************Fill the send data rupee Payment API*********
          ls_input-merchant_payment_ref_id = ls_inv_data-invoicekey.
**********amount***********
          ls_input-amount-value         = ls_inv_data-dueamount.
          ls_input-amount-currency      = 'INR'.
************address*************
          ls_input-address-address_line1 = ls_inv_data-street.
          ls_input-address-city         = ls_inv_data-city1.
          ls_input-address-country      = ls_inv_data-country.
          ls_input-address-landmark     = ls_inv_data-custname.
          ls_input-address-pincode      = ls_inv_data-post_code1.
          ls_input-address-state        = ls_inv_data-bezei.
**********************Autocapture**************

          ls_input-auto_capture          = 'true'.
          ls_input-callback_url = COND #( WHEN sy-sysid = 'DEV' THEN |https://webdevqas.sheenlac.com:44306/sap/zapi_service/zrf_callb_pymnt?sap-client=700&merchantPaymentRefId={ ls_inv_data-invoicekey }|
                                          WHEN sy-sysid = 'QAS' THEN |https://webdevqas.sheenlac.com:44302/sap/zapi_service/zrf_callb_pymnt?sap-client=500&merchantPaymentRefId={ ls_inv_data-invoicekey }|
                                          WHEN sy-sysid = 'PRD' THEN |https://sap.sheenlac.com:44301/sap/zapi_service/zrf_callb_pymnt?sap-client=500&merchantPaymentRefId={ ls_inv_data-invoicekey }| ).
*          ls_inv_data-callback_url =  ls_input-callback_url .
**********************"merchantCustomerRefId"************
           iF SY-sysid = 'PRD'.
          ls_input-merchant_customer_ref_id = |SAP_{ ls_inv_data-custno ALPHA = OUT }|.
           ELSEIF sy-sysid = 'DEV'.
           ls_input-merchant_customer_ref_id = |{ ls_inv_data-custno ALPHA = OUT }|.
           ENDIF.
          CONDENSE  ls_input-merchant_customer_ref_id.
*********************INV date*************
          ls_input-invoice_date          = ls_inv_data-inv_date_tstp.
**********************documents************
          ls_input-documents = VALUE #( ( merchant_doc_ref_id = ls_inv_data-invoiceno data = ls_inv_data-inv_base64 doc_type = 'INVOICE'  doc_format =  'PDF'  ) ).
*          ls_input-documents-data       = <fs>-inv_base64.
*          ls_input-documents-docformat  = 'PDF'.

*******************call the rupify URL****************
          cl_http_client=>create_by_url(
              EXPORTING
              url = create_url
              IMPORTING
              client = lo_http_client
              EXCEPTIONS
              argument_not_found = 1
              plugin_not_active = 2
              internal_error = 3
              OTHERS = 4 ).

          CHECK lo_http_client IS BOUND.

          lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

          lo_http_client->request->set_method(
            EXPORTING
              method = if_http_entity=>co_request_method_post ).

          lo_http_client->request->set_content_type(
            EXPORTING
              content_type = if_rest_media_type=>gc_appl_json ).

          "Header Data Fields for API Fixing
          lo_http_client->request->set_header_field( EXPORTING name  = 'Authorization' value =   lv_token ) .
*****************Serialize the INPUT JSON************
          /ui2/cl_json=>serialize(
          EXPORTING
            data         =  ls_input
            pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
          RECEIVING
            r_json         = iv_payload_json ).
          REPLACE ls_repl WITH ls_true INTO iv_payload_json  .
****************Send the response to rupify**************
          lo_http_client->request->set_cdata(
             EXPORTING
             data = iv_payload_json ).

          lo_http_client->send(
           EXCEPTIONS
           http_communication_failure = 1
           http_invalid_state = 2 ).

          CHECK sy-subrc = 0.
          lo_http_client->receive(
           EXCEPTIONS
           http_communication_failure = 1
           http_invalid_state = 2
           http_processing_failed = 3 ).

          lo_http_client->response->get_status(
          IMPORTING
            code = DATA(lv_codes) ).

          lo_http_client->response->get_status(
          IMPORTING
            reason = DATA(lv_http_error) ).
          "Actual API Response If success
          DATA(lv_response) = lo_http_client->response->get_cdata( ).

*****************DeSerialize the OUTPUT JSON************
          /ui2/cl_json=>deserialize(
          EXPORTING
           json         = lv_response
           pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
           data         = ls_output ).

*          IF ls_output-success = abap_true.
          IF lv_codes = '200'.
            ls_inv_data-payment_refid =  ls_output-data-payment_id .
            ls_inv_data-payment_url   = ls_output-data-payment_url.
            ls_inv_data-payment_date  = ls_output-data-payment_date.
            ls_inv_data-response_msg = ls_output-message.
            ls_inv_data-response_status       = ls_output-data-status.
            ls_inv_data-response_time = ls_output-timestamp+11(8).
            ls_inv_data-callback_url = ls_input-callback_url.
            ls_inv_data-accountid = ls_output-data-accountid.
            ls_inv_data-requestid = ls_output-request_id.
            ls_inv_data-response_date = |{ ls_output-timestamp+8(2) }-{ ls_output-timestamp+5(2) }-{ ls_output-timestamp+0(4) }| .
            ls_inv_data-status        = '14'.
            READ TABLE gt_inv_data ASSIGNING FIELD-SYMBOL(<fs>) INDEX lv_index.
            IF sy-subrc = 0.
              <fs>-status = '14'.
              <fs>-payment_refid =  ls_output-data-payment_id .
              <fs>-payment_url   = ls_output-data-payment_url.
            ENDIF.
            MODIFY zsd_sf_cust_inv FROM ls_inv_data.
            IF Sy-subrc = 0.
              COMMIT WORK.
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Passing Response Payment URL to Distributors
*            IF lv_codes = '200'.
***1) Via E-mail
              me->rf_mail_to_dist(
                EXPORTING
                  ls_output    = ls_output
                  ls_input     = ls_inv_data
                  gt_comms     = it_comms
                RECEIVING
                  ls_mail_resp = lv_mail_res
              ).
***2) Via Whatsapp
              me->rf_whatsapp_dist(
                EXPORTING
                  ls_output    = ls_output
                  gs_cust_inv  = ls_inv_data
                  gt_comms     = It_comms
                RECEIVING
                  gv_http_stat = lv_whats_res
              ).
            ENDIF.
          ELSE.
            ls_inv_data-payment_refid =  ls_output-data-payment_id .
            ls_inv_data-payment_url   = ls_output-data-payment_url.
            ls_inv_data-payment_date  = ls_output-data-payment_date.
            ls_inv_data-response_msg = ls_output-message.
            ls_inv_data-response_status       = ls_output-data-status.
            ls_inv_data-response_time = ls_output-timestamp+11(8).
            ls_inv_data-accountid = ls_output-data-accountid.
            ls_inv_data-requestid = ls_output-request_id.
            ls_inv_data-response_date = |{ ls_output-timestamp+8(2) }-{ ls_output-timestamp+5(2) }-{ ls_output-timestamp+0(4) }| .
            ls_inv_data-status        = '15'.
            READ TABLE gt_inv_data ASSIGNING FIELD-SYMBOL(<fs_1>) INDEX lv_index.
            IF sy-subrc = 0.
              <fs_1>-status = '15'.
              <fs_1>-payment_refid =  ls_output-data-payment_id .
              <fs_1>-payment_url   = ls_output-data-payment_url.
            ENDIF.
            MODIFY zsd_sf_cust_inv FROM ls_inv_data.
            IF Sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
          ENDIF.
**********************************************************************Log update payload
          v_jsonload = | { '[' } { lv_response } { ']' } |.
          CALL METHOD lo_log_upd->log_entry_store
            EXPORTING
              apiname         = 'RF_CREATE_PAYMENT'
              ijson           = iv_payload_json
              ojson           = v_jsonload
              distributor     = ls_inv_data-custno
            EXCEPTIONS
              apiname_missing = 1
              json_missing    = 2
              OTHERS          = 3.
        clear ls_output.
        ENDLOOP.
      ELSE.
        MESSAGE 'Rupify URL not found' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'No records found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
*        MODIFY zsd_sf_cust_inv FROM TABLE gt_inv_data.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Passing Response Payment URL to Distributors
*          "Mail to Distributor
*          me->rf_mail_to_dist( ls_output = ls_output ls_input = ls_input  s_cust = s_kunnr ).
*        ENDIF.
*      ELSE.
*        MESSAGE 'Rupify URL not found' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*    ELSE.
*      MESSAGE 'No records found' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.

  ENDMETHOD.

  METHOD rf_get_token.
    CLEAR ex_token.

    CALL FUNCTION 'ZHR_GET_ACCESS_TOKEN_RUPIFI'
      EXPORTING
        focrce_gen            = 'X'
        custype               = im_custype
      IMPORTING
        access_token          = ex_token
      EXCEPTIONS
        communication_failure = 1
        status_failure        = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      CLEAR ex_token.
    ENDIF.


  ENDMETHOD.

  METHOD rf_mail_to_dist.
   DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store,
         iv_payload_json TYPE string,
         v_jsonload type string.
    CONSTANTS:gc_raw     TYPE char03 VALUE 'HTM'.

    DATA: lv_hex        TYPE xstring,
          lt_hex        TYPE solix_tab,
          iv_importance TYPE bcs_docimp VALUE 5.
    DATA: lv_stat TYPE char20.
    DATA: lt_comms TYPE TABLE OF zcus_cf_cumm.
    DATA: lt_bin TYPE solix_tab.
    DATA: gc_subject TYPE so_obj_des .
CREATE OBJECT lo_log_upd.
**********************************************************************Fetch Customer maintanence Data with status - payment accepted by Finance
    """"Get Customer Name from ADRC
    SELECT FROM kna1
    FIELDS kunnr,adrnr
    WHERE kunnr = @ls_input-custno
    INTO TABLE @DATA(gt_cust).
    IF sy-subrc = 0.
      SELECT FROM adrc
      FIELDS addrnumber,name1,name4
      FOR ALL ENTRIES IN @gt_cust
      WHERE addrnumber = @gt_cust-adrnr
      INTO TABLE @DATA(gt_cust_name).
    ENDIF.

    lt_comms = VALUE #( FOR ls_comm IN gt_comms WHERE ( kunnr = ls_input-custno and com_type = 30 )
                               ( CORRESPONDING #( ls_comm ) )  ).
****Fetching Channel Manager pernr, name and E-mail****
    Select SINGLE from zcust_blk_chk fields CH_PERNR where kunnr = @ls_input-custno into @data(chnl_manager_pernr).
    IF sy-subrc = 0.
    Select SINGLE from pa0105 fields USRID_LONG where pernr = @chnl_manager_pernr
                                                  and subty = '0010'
                                                  and endda >= @sy-datum
                                                  and begda <= @sy-datum
                                                into @data(chnl_manager_mail).
    IF sy-subrc = 0.
     Select SINGLE from pa0001 fields SNAME where pernr = @chnl_manager_pernr
                                                  and endda >= @sy-datum
                                                  and begda <= @sy-datum
                                                into @data(chnl_manager_name).
    ENDIF.
    endif.
**********************************************************************
    DATA(gs_cust_name) = VALUE #( gt_cust_name[ name1 = ls_input-custname ]-name4 OPTIONAL ).
    TRY.
        "send request
        DATA(lo_send_req) = cl_bcs=>create_persistent( ).

        "set sender
        lo_send_req->set_sender( i_sender = cl_cam_address_bcs=>create_internet_address(
                                        i_address_string =   CONV #( 'customerfeedback@sheenlac.in' )
                                              i_address_name   = CONV #( 'Sheenlac' )
                                            ) ).

        DATA(ls_invoiceno) = |{ ls_input-invoiceno  }|.
        DATA(ls_inv_date) = |{ ls_input-invoicedate+6(2) }-{ ls_input-invoicedate+4(2) }-{ ls_input-invoicedate+0(4) }|.
        DATA(lt_body) = VALUE bcsy_text(
            ( line = '<HTML> <BODY>'  )
            ( line = |<p style="font-size:14;" > Dear { gs_cust_name }({ ls_input-custname }) ,</p>| ) (  )
 ( line = |Invoice No { ls_invoiceno } for the value of { ls_input-invoiceamount } , Adjusted due amount { ls_input-dueamount } has been raised by Sheenlac on { ls_inv_date }.</br>| ) (  )
            ( line = |Provide your acceptance for discounting by clicking on the following link:| ) (  )
            ( line = |<a style="color:blue" href =" { ls_output-data-payment_url }"> { ls_output-data-payment_url } </a></br></br></br>| ) (  ) (  ) (  )
            ( line =  |</br></br>| ) (  ) (  )
            ( line = |<p style="font-size:14;" ><strong>Note:The link will expire in 30 Days.</strong></p>| )
            ( line = |</BODY></HTML>| )
        ).

        gc_subject = |RupiFi Payment Request ({ ls_input-invoiceno })|.
        "set document
        DATA(lo_document) = cl_document_bcs=>create_document(
                              i_type         = gc_raw
                              i_subject      = gc_subject
                              i_importance   = iv_importance
                              i_text         = lt_body
                            ).

        PERFORM convert_to_binary USING ls_input CHANGING lt_bin.
        lo_document->add_attachment(
          EXPORTING
            i_attachment_type     = 'PDF'
            i_attachment_subject  = |Invoice-{ ls_input-invoiceno }|
            i_att_content_hex     = lt_bin
        ).
        lo_send_req->set_document( i_document = lo_document  ).

        "set receiver
        LOOP AT lt_comms ASSIGNING FIELD-SYMBOL(<fs_comm>).
          lo_send_req->add_recipient(
            EXPORTING
              i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                i_address_string = <fs_comm>-email
                                i_address_name   = CONV #( ls_input-custname )
                             )
              i_express    = 'X'
          ).
* MAIL CC To Channel Manager
              lo_send_req->add_recipient(
                EXPORTING
                  i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                   i_address_string = chnl_manager_mail
                                   i_address_name   = conv #( chnl_manager_name )
                                 )
                  i_copy       = abap_true
              ).
*              CATCH cx_send_req_bcs.
        ENDLOOP.

        DATA(lv_send_mail) = lo_send_req->send(  ).
          iv_payload_json = |Customer Name:{ ls_input-custname }, InvoiceNo:{ ls_input-invoiceno }, Customer E-Mail:{ <fs_comm>-email },Channel Manager Pernr : { chnl_manager_pernr }, Channel Manager E-mail:{ chnl_manager_mail } |.
        IF sy-subrc = 0.
          "commit to send email
          COMMIT WORK.
            /ui2/cl_json=>serialize(
          EXPORTING
            data             = lt_body
            pretty_name      = /ui2/cl_json=>pretty_mode-user
          RECEIVING
            r_json           = v_jsonload
        ).
        ELSE.
          v_jsonload = |Mail Not Sent|.
        ENDIF.
        CALL METHOD lo_log_upd->log_entry_store
            EXPORTING
              apiname         = 'RF_CUST_EMAIL'
              ijson           = iv_payload_json
              ojson           = v_jsonload
              distributor     = ls_input-custno
            EXCEPTIONS
              apiname_missing = 1
              json_missing    = 2
              OTHERS          = 3.

         clear ls_output.
        "Exception handling
      CATCH cx_bcs INTO DATA(gs_bcs).
        DATA(es_message) =  | Error text:{ gs_bcs->get_text( ) } Error Type:{ gs_bcs->error_type } |.
*      Message es_message TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD rf_get_pament_status.

    DATA : ls_input        TYPE ty_input,
           ls_output       TYPE ty_output,
           iv_payload_json TYPE string,
           lv_token        TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.
*****************fetch the finance customer invoices***********
    IF p_r3 = 1.
      SELECT * FROM zsd_sf_cust_inv
               INTO TABLE @gt_inv_data
               WHERE invoiceno IN @s_invno
               AND   custno    IN @s_kunnr
               AND   fintype = 'RF'
               AND   status    = '16'
               AND   invstatus NE 'CAN'.
    ELSEIF p_r3 = 2.
      SELECT * FROM zsd_sf_cust_inv
       INTO TABLE @gt_inv_data
       WHERE invoiceno IN @s_invno
       AND   custno    IN @s_kunnr
       AND   fintype = 'RF'
       AND   status    = '14'
       AND   invstatus NE 'CAN'.
    ENDIF.
    IF sy-subrc = 0.
      CONDENSE : gv_token.
      CLEAR    : lv_token.
      lv_token = |Bearer { gv_token }|.
      SORT gt_inv_data BY invoiceno.
********************get the URL from the table********************
      SELECT SINGLE * FROM zfi_cf_params INTO @DATA(ls_url) WHERE zsystem  = @sy-sysid
                                                                AND   zcustype = 'RF'
                                                                AND   zfldtype = 'PYURL'.
      LOOP AT gt_inv_data INTO DATA(ls_inv_data).

        DATA(create_url) = CONV string( ls_url-zfldvalue ).
*******************call the rupify URL****************
        create_url = |{ create_url }?merchantPaymentRefId={ ls_inv_data-invoicekey }|.
        cl_http_client=>create_by_url(
            EXPORTING
            url = create_url
            IMPORTING
            client = lo_http_client
            EXCEPTIONS
            argument_not_found = 1
            plugin_not_active = 2
            internal_error = 3
            OTHERS = 4 ).

        CHECK lo_http_client IS BOUND.
* lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

        lo_http_client->request->set_method(
                 EXPORTING
                   method = if_http_entity=>co_request_method_get ).


        "Header Data Fields for API Fixing
        lo_http_client->request->set_header_field( EXPORTING name  = 'Authorization' value =   lv_token ) .

****************Send the response to rupify**************
        lo_http_client->send(
            EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state = 2 ).

        CHECK sy-subrc = 0.
        lo_http_client->receive(
         EXCEPTIONS
         http_communication_failure = 1
         http_invalid_state = 2
         http_processing_failed = 3 ).

        lo_http_client->response->get_status(
        IMPORTING
          code = DATA(lv_codes) ).

        lo_http_client->response->get_status(
        IMPORTING
          reason = DATA(lv_http_error) ).
        "Actual API Response If success
        DATA(lv_response) = lo_http_client->response->get_cdata( ).
*****************DeSerialize the OUTPUT JSON************
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
         data         = ls_output ).
        IF p_r3 = 2.
          DATA(lv_out2) = VALUE ty_get_py( merchantpaymenyrefid = ls_output-data-merchant_payment_ref_id
                                         customer = ls_output-data-merchant_customer_ref_id
                                         invoice_amount = ls_output-data-amount-value
                                         invoice_date = ls_inv_data-invoicedate
                                         auto_capture = SWITCH #( ls_output-data-auto_capture WHEN abap_true THEN 'true' ELSE 'false' )
                                         status = ls_output-data-status
                                         created_on = ls_inv_data-response_date
                                         created_at = ls_inv_data-response_time ).
          APPEND lv_out2 TO gt_get_py.
          CLEAR: lv_out2 ,ls_output.
          IF ls_output-data-status = 'CAPTURED'.
            UPDATE zsd_sf_cust_inv SET response_status = ls_output-data-status
                                        status = '16'
                                       inv_approvedon = sy-datum
                                       inv_approvedat = sy-uzeit
                                       inv_approvedby = sy-uname
                    WHERE invoicekey =  ls_output-data-merchant_payment_ref_id.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ELSEIF p_r3 = 1.
          DATA(lv_out) = VALUE ty_get_py( merchantpaymenyrefid = ls_output-data-merchant_payment_ref_id
                                         customer = ls_output-data-merchant_customer_ref_id
                                         invoice_amount = ls_output-data-amount-value
                                         invoice_date = ls_inv_data-invoicedate
                                         auto_capture = SWITCH #( ls_output-data-auto_capture WHEN abap_true THEN 'true' ELSE 'false' )
                                         status = ls_output-data-status
                                         created_on = |{ ls_inv_data-inv_approvedon+6(2) }-{ ls_inv_data-inv_approvedon+4(2) }-{ ls_inv_data-inv_approvedon+0(4) } |
                                         created_at = | { ls_inv_data-inv_approvedat+0(2) }:{ ls_inv_data-inv_approvedat+2(2) }:{ ls_inv_data-inv_approvedat+4(2) }|
                                           ).
          APPEND lv_out TO gt_get_py.
          CLEAR:lv_out, ls_output.
        ENDIF.
      ENDLOOP.
      REFRESH gt_inv_data[].
    ENDIF.

  ENDMETHOD.

  METHOD rf_whatsapp_dist.
**********************************************************************Fetch Customer maintanence Data with status - payment accepted by Finance
     DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store,
         iv_payload_json TYPE string,
         v_jsonload type string.
    DATA: lv_stat TYPE char20,
          lv_msg  TYPE string,
          ls_templ_name type char50.
    CREATE OBJECT lo_log_upd.
    DATA(ls_inv_date) = |{ gs_cust_inv-invoicedate+6(2) }-{ gs_cust_inv-invoicedate+4(2) }-{ gs_cust_inv-invoicedate+0(4) }|.
    "Template name and parameters differs from business purpose
   ls_templ_name = 'invoice_copy2'.
*----------Get Time Taken value-------------*
    SELECT SINGLE low FROM tvarvc INTO @DATA(ls_fname)
            WHERE name = 'ZWAPP_TIME_TAKEN'
            AND type = 'P'.
       if sy-subrc = 0.
       data(ls_time_taken) = CONV char2( ls_fname ).
       endif.
*-----Populate Parameters------*
    data(lt_parameters) = value ztt_key_value_pair(  ( name = 'name' value = gs_cust_inv-custname )
                                                     ( name = 'invoice_number' value = gs_cust_inv-invoiceno )
                                                     ( name = 'total_amount' value = gs_cust_inv-invoiceamount )
                                                     ( name = 'total_price' value = gs_cust_inv-dueamount )
                                                     ( name = 'order_date' value = ls_inv_date )
                                                     ( name = 'file_link' value = gs_cust_inv-payment_url )
                                                     ( name = 'time_taken' value = ls_time_taken )
                                                     ).
**********************************************************************
    LOOP AT gt_comms INTO DATA(gs_whats_cust) WHERE fintype = 'RF' and kunnr = gs_cust_inv-custno and com_type = 20.
      CALL FUNCTION 'ZFM_GEN_WHATSAPP'
        EXPORTING
          iv_template_name  = ls_templ_name
          iv_mobile         = gs_whats_cust-phone
          it_template_data  = lt_parameters
        IMPORTING
          iv_status         =  lv_stat
        EXCEPTIONS
          incorrect_mob_num = 1
        .
      IF sy-subrc ne 0.
        lv_msg = 'Incorrect mob number format:add "91" in prefix'.
      ENDIF.
         iv_payload_json = |Customer Name:{ gs_cust_inv-custname }, Customer Whatsappno:{ gs_whats_cust-phone } InvoiceNo:{ gs_cust_inv-invoiceno }|.
      IF lv_stat = 'SUCCESS'.
        /ui2/cl_json=>serialize(
          EXPORTING
            data             = lt_parameters
            pretty_name      = /ui2/cl_json=>pretty_mode-user
          RECEIVING
            r_json           = v_jsonload
        ).
      ELSE.
    v_jsonload = COND #( when lv_msg is not initial then lv_msg else 'Chat Not Sent' ).
      ENDIF.
        CALL METHOD lo_log_upd->log_entry_store
            EXPORTING
              apiname         = 'RF_CUST_WHATSAPP'
              ijson           = iv_payload_json
              ojson           = v_jsonload
              distributor     = gs_cust_inv-custno
            EXCEPTIONS
              apiname_missing = 1
              json_missing    = 2
              OTHERS          = 3.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

FORM convert_to_binary USING p_input TYPE zsd_sf_cust_inv
                                CHANGING pt_binary TYPE solix_tab.

  DATA(l_set_print) = VALUE lbbil_print_data_to_read(  hd_gen = abap_true
                                                             hd_adr = abap_true
                                                             hd_gen_descript = abap_true
                                                             hd_org = abap_true
                                                             hd_part_add = abap_true
                                                             hd_kond = abap_true
                                                             hd_fin = abap_true
                                                             hd_ref = abap_true
                                                             hd_tech = abap_true
                                                             it_gen = abap_true
                                                             it_adr = abap_true
                                                             it_price = abap_true
                                                             it_kond = abap_true
                                                             it_ref = abap_true
                                                             it_refdlv = abap_true
                                                             it_reford = abap_true
                                                             it_refpurord = abap_true
                                                             it_refvag = abap_true
                                                             it_refvg2 = abap_true
                                                             it_refvkt = abap_true
                                                             it_tech = abap_true
                                                             it_fin = abap_true
                                                             it_confitm = abap_true
                                                             it_confbatch = abap_true
                                                             msr_hd = abap_true
                                                             msr_it = abap_true ).
  DATA(lv_objkey) = CONV nast-objky( p_input-invoiceno ).
  DATA ls_bil_invoice    TYPE lbbil_invoice.
  DATA ls_docpara TYPE sfpdocparams.
  DATA ls_outpara TYPE sfpoutputparams.
  DATA ls_output  TYPE fpformoutput.
  DATA  ls_frmname TYPE fpname.
  DATA: lv_fm    TYPE rs38l_fnam,
        i_objbin TYPE STANDARD TABLE OF solix,
        l_app1   TYPE string.
  CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
    EXPORTING
      if_bil_number         = lv_objkey
      is_print_data_to_read = l_set_print
      if_parvw              = 'RE'
      if_parnr              = p_input-custno
      if_language           = sy-langu
    IMPORTING
      es_bil_invoice        = ls_bil_invoice
    EXCEPTIONS
      records_not_found     = 1
      records_not_requested = 2
      OTHERS                = 3.
  IF sy-subrc = 0.
    ls_outpara-getpdf = abap_true.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outpara
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    CLEAR: lv_fm,ls_frmname.
    ls_frmname = 'ZSD_IRN_QR_EINVOICE_V1'.

    "Get Respective Function module Name based on form Name
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = ls_frmname
          IMPORTING
            e_funcname = lv_fm.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    CALL FUNCTION lv_fm
      EXPORTING
        /1bcdwb/docparams  = ls_docpara
        is_bil_invoice     = ls_bil_invoice
      IMPORTING
        /1bcdwb/formoutput = ls_output
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

  ENDIF.


  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = ls_output-pdf
    TABLES
      binary_tab = pt_binary.

ENDFORM.
