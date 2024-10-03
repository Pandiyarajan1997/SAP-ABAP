CLASS zcl_bp_master_create DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    METHODS validations
      IMPORTING
        !iv_accnt_type   type ktokk
        !ls_input        TYPE zst_cust_create_input  OPTIONAL
        !gs_input        TYPE zst_vendor_create_input OPTIONAL
      RETURNING
        VALUE(gt_return) TYPE bapiret2_t .
    METHODS bp_creation
      IMPORTING
        !gs_input TYPE zst_cust_create_input
      EXPORTING
        !gt_log   TYPE ztt_sd_bp_logs.
    METHODS bp_maintain_customer
      IMPORTING
        !ls_input TYPE zst_cust_create_input
        !iv_accnt_type type ktokk
      EXPORTING
        !gt_log   TYPE ZTT_BP_OUTPUT_LOGS.
    METHODS bp_maintain_vendor
      IMPORTING
        !ls_input TYPE zst_vendor_create_input
        !iv_accnt_type type ktokk
      EXPORTING
        !gt_log   TYPE ZTT_BP_OUTPUT_LOGS.
    METHODS bp_add_roles
      IMPORTING
        !iv_grouping     TYPE bu_group
        !businesspartner TYPE bu_partner .
    METHODS maintain_tax
      IMPORTING
        VALUE(ls_customers) TYPE cmds_ei_extern
        !businesspartner    TYPE bu_partner
        !ls_sales_data      TYPE cmds_ei_cmd_sales
      EXPORTING
        !is_master_data     TYPE cmds_ei_main .
    METHODS set_grouping_char
      IMPORTING
        !ls_group_char   TYPE bp_group_feature
        !businesspartner TYPE bu_partner
      EXPORTING
        !gt_log          TYPE ztt_sd_bp_logs .
    METHODS customer_extnsn
      IMPORTING
        !is_master_data  TYPE cmds_ei_main
        !businesspartner TYPE bu_partner
        !sorg            TYPE vkorg
        !ls_kna1         TYPE kna1
        !bukrs           TYPE bukrs
      EXPORTING
        !gt_log          TYPE ztt_sd_bp_logs .                                   "Ccode and Sorg extensions
    METHODS cvi_roles
      IMPORTING
        !iv_grouping TYPE bu_group
        !ensure_create TYPE cvi_ensure_create
      EXPORTING
        !it_roles    TYPE bus_ei_bupa_roles_t.
    METHODS cvi_address
      IMPORTING
        !gs_input   TYPE zst_bp_addr_comm_data
      EXPORTING
        !iv_address TYPE bus_ei_address.
    METHODS cvi_taxnumber
      IMPORTING
        !taxnumber TYPE bptaxnum
      EXPORTING
        !iv_tax    TYPE bus_ei_taxnumber.
    METHODS create_guid
      EXPORTING
        !v_guid    TYPE sysuuid_c
        !v_err_txt TYPE string .
    METHODS cvi_bp_header
      EXPORTING
        !header   TYPE bus_ei_header
        !cx_error TYPE string.
    METHODS cvi_customer
      IMPORTING
         gs_input    TYPE zst_cust_create_input
      EXPORTING
         iv_customer TYPE cmds_ei_extern
         iv_bukrs type bukrs
         iv_vkorg type vkorg.
     METHODS vi_Bank_Details
        IMPORTING gt_bank type ZTT_BP_PAYMT_TRANS
                  name1 type zst_vendor_create_input-generaldata-name-name1
        EXPORTING BANKDETAIL type BUS_EI_BANKDETAIL.
     METHODS cvi_vendor
       IMPORTING iv_input type zst_vendor_create_input
       EXPORTING iv_vendor type vmds_ei_extern.
    "Taxtype
    METHODS: update_pan IMPORTING value(ls_kna1) TYPE kna1
                                  iv_input       TYPE zst_cust_create_input.
    "Update Customer
    METHODS: update_customer IMPORTING VALUE(ls_kna1) type kna1
                             EXPORTING lt_return type  bapiretm.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: is_input TYPE zst_cust_create_input,
          ls_input type zst_bp_create_input,
          gs_log   TYPE zsd_bp_creation.
    CONSTANTS: c_bp_cat TYPE char1 VALUE '2', " BP category
               c_cntry  TYPE char2 VALUE 'IN'," country
               c_langu  TYPE char1 VALUE 'E', " Language
               c_vtweg  TYPE char2 VALUE '20', " For all company codes
               c_spart  TYPE char2 VALUE '10', " For SDMS
               c_ktgrd  TYPE char2 VALUE '01', " Account Assignment
               c_inco1  TYPE char3 VALUE 'CIF', "Incoterms
               c_lprio  TYPE char2 VALUE '02', "Delivery Priority
               c_kalks  TYPE char2 VALUE '1', "Customer Classification for Pricing Procedure Determination
               c_taxtyp TYPE bptaxtype  VALUE 'IN3'. "Tax Type
ENDCLASS.



CLASS zcl_bp_master_create IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

***** Purpose of this API is for Creating BP master(Customer/Supplier) through MIS*************
"Created by  : Zakir Hussain
"Changed on  : 12-06-2024
"Reference by: Ramakrishnan J/Gopal/Praveen
"-------------------------------------------------------------------------------------------------*
    DATA: gt_log TYPE ZTT_BP_OUTPUT_LOGS.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_data TYPE string.
    DATA: lv_msg TYPE string.
    DATA:ls_cust type zst_bp_create_input.
**********normal api log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = ls_input ).
""""""""""""""""""""""""BP Creation carries on with Input structure"""""""""""""""""""""""""""""""""""""""""""""
CASE ls_input-account_type.
WHEN 'D'.
 bp_maintain_customer(
      EXPORTING
        ls_input = ls_input-customer
        iv_accnt_type = 'D'
      IMPORTING
        gt_log   = gt_log
    ).
WHEN 'K'.
bp_maintain_vendor(
  EXPORTING
    ls_input = ls_input-vendor
     iv_accnt_type = 'K'
  IMPORTING
    gt_log   = gt_log
).

ENDCASE.

    IF gt_log IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_log
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'BP_MAST_CREATE'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = conv #( COND #( WHEN ls_input-account_type = 'D' then 'Customer' ELSE ' ') )
          retailer        = conv #( COND #( WHEN ls_input-account_type = 'K' then 'Vendor' ELSE ' ') )
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc = 0.

      ENDIF.
      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.


  METHOD bp_creation.

    DATA: gv_bpcat(01) TYPE n.
    DATA: gv_error TYPE flag.
    DATA partnercategory              TYPE bapibus1006_head-partn_cat. " Business Partner Category
    DATA partnergroup                 TYPE bapibus1006_head-partn_grp. " Business Partner Grouping
    DATA centraldataperson            TYPE bapibus1006_central_person. " SAP BP: BAPI Structure for Personal Data
    DATA centraldataorganization      TYPE bapibus1006_central_organ. " SAP BP: BAPI Structure for Organization Data
    DATA addressdata                  TYPE bapibus1006_address. " SAP BP: BAPI Structure for Address Data
    DATA businesspartner              TYPE bapibus1006_head-bpartner. " Business Partner Number
    DATA it_telephondata                  TYPE STANDARD TABLE OF bapiadtel. " BAPI Structure for Telephone Numbers (Bus. Address Services)
    DATA wa_telephondata                  TYPE bapiadtel. " BAPI Structure for Telephone Numbers (Bus. Address Services)
    DATA it_maildata                   TYPE STANDARD TABLE OF bapiadsmtp. " BAPI Structure for E-Mail Addresses (Bus. Address Services)
    DATA wa_maildata                   TYPE bapiadsmtp. " BAPI Structure for E-Mail Addresses (Bus. Address Services)
    DATA return                       TYPE STANDARD TABLE OF bapiret2. " Return Parameter
    DATA : gwa_knvi TYPE knvi.
    DATA: i_return TYPE TABLE OF bapiret2.
    DATA is_master_data           TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_master_data_correct   TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_correct       TYPE cvis_message. " Error Indicator and System Messages
    DATA es_master_data_defective TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_defective     TYPE cvis_message. " Error Indicator and System Messages
    DATA: lt_customers    TYPE cmds_ei_extern_t,
          ls_customers    TYPE cmds_ei_extern,         " Complex External Interface for Customers
          ls_sales_data   TYPE cmds_ei_cmd_sales,      " Ext. Interface: Sales Data
          ls_central_data TYPE cmds_ei_central_data,   " External Interface: Central Data
          ls_address      TYPE cvis_ei_address1,       " Ext. Interface: Address of Organization
          lt_sales        TYPE cmds_ei_sales_t,
*        ls_sales        TYPE cmds_ei_sales,          " Ext. Interface: Sales Data
          lt_sales_func   TYPE cmds_ei_functions_t,
          ls_sales_func   TYPE cmds_ei_functions,      " Ext. Interface: Partner Roles
          ls_tax_data     TYPE cmds_ei_tax_ind,        " Ext. Interface: Tax Indicators
          lt_tax_data     TYPE cmds_ei_tax_ind_t,
          ls_message      TYPE cvis_message,           " Error Indicator and System Messages
          lv_contactid    TYPE bapicontact_01-contact. " Number of contact person
    DATA: lt_company      TYPE cmds_ei_company_t,
          ls_company_data TYPE cmds_ei_cmd_company.    " Ext. Interface: Company Code Data
    DATA: lv_r2      TYPE xfeld.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2 .
    DATA iv_ktokd TYPE ktokd. " Customer Account Group
    DATA:et_parvw TYPE cmds_parvw_t,
         wa_parvw TYPE cmds_parvw.


    " Validation of inputs
*    DATA(gt_return) =  me->validations( ls_input = is_input ).

*    IF gt_return IS NOT INITIAL.
*      DATA(iv_count) = lines( gt_return ).
*      IF iv_count > 1 .
*        LOOP AT gt_return INTO DATA(is_ret).
*          DATA(lv_rem) = VALUE text300(  ).
*          lv_rem = is_ret-message_v1.
*          lv_rem = | {    lv_rem } { ',' } { is_ret-message_v1 } |.
*        ENDLOOP.
*      ELSE.
*        lv_rem = VALUE #( gt_return[ 1 ]-message OPTIONAL ).
*      ENDIF.
**      REPLACE ALL OCCURRENCES OF 'gs_data-' IN lv_rem WITH ''.
**      CONDENSE lv_rem NO-GAPS.
*      DATA(ls_alverr) = VALUE zst_bp_log_output( msgtyp = 'E'
*                                               sortl = gs_input-generaldata-searchterm1
*                                               bpname = gs_input-generaldata-name-name1
*                                               msg = |{ 'Error in Inputs' } { gs_input-generaldata-searchterm1 } |
*                                               remarks = lv_rem
*                                                ).
*      APPEND ls_alverr TO gt_log.
*    ELSE.
      CLEAR: businesspartner,gv_error,gv_bpcat.
      SELECT FROM zapi_common_list
      FIELDS field_type,field_key,key_descr
      INTO TABLE @DATA(gt_common_list).
      gv_bpcat = c_bp_cat . "Organization Defaulted
      partnercategory = c_bp_cat. "Organization Defaulted
      partnergroup = gs_input-generaldata-grouping.
*** Central Data filling for business Partner ***
      SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @gs_input-generaldata-title AND langu = 'E'.
      DATA(ls_central) = VALUE bapibus1006_central( searchterm1 = gs_input-generaldata-searchterm1
                                                    title_key = lv_title ).

      IF partnercategory = 2.
        DATA(lv_name1) = gs_input-generaldata-name-name1.
        DATA(lv_name2) = gs_input-generaldata-name-name2.
        centraldataorganization-name1 = lv_name1.
        centraldataorganization-name2 = lv_name2.
        centraldataorganization-name3 = gs_input-generaldata-name-name3.
        centraldataorganization-name4 = gs_input-generaldata-name-name4.

      ELSEIF partnercategory = 1.
        centraldataperson-firstname = gs_input-generaldata-name-name1.
        centraldataperson-lastname = gs_input-generaldata-name-name1.
        centraldataperson-namcountry = c_cntry.
        centraldataperson-correspondlanguage = c_langu.
      ENDIF.

**** Adddress data filling in Business Partner Creation **
      DATA(ls_address1) = VALUE bapibus1006_address( street = gs_input-generaldata-addressdata-street
                                                    str_suppl1 = gs_input-generaldata-addressdata-addr1
                                                    str_suppl2 = gs_input-generaldata-addressdata-addr2
                                                    str_suppl3 = gs_input-generaldata-addressdata-addr3
                                                    house_no = gs_input-generaldata-addressdata-house_num
                                                    district = gs_input-generaldata-addressdata-district
                                                    postl_cod1 = gs_input-generaldata-addressdata-post_code1
                                                    city = gs_input-generaldata-addressdata-city
                                                    country = c_cntry
                                                    region = gs_input-generaldata-addressdata-region
                                                    langu = c_langu ).
      REFRESH: it_telephondata.
      it_telephondata = VALUE #( ( country = c_cntry
                                     telephone = gs_input-generaldata-communication-landline
                                     std_no = 'X'
                                     r_3_user = '3'
                                     home_flag = 'X' )
                                   ( country = c_cntry
                                     telephone =  gs_input-generaldata-communication-mob_number
                                     std_no = 'X'
                                     r_3_user = '1'
                                     home_flag = 'X' ) ).
      REFRESH: it_maildata.
      it_maildata = VALUE #( ( e_mail =  gs_input-generaldata-communication-smtp_addr  "Email
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' ) ).
*** Bapi to create Business Partner ***
      REFRESH: return.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory         = partnercategory
          partnergroup            = partnergroup
          centraldata             = ls_central
          centraldataperson       = centraldataperson
          centraldataorganization = centraldataorganization
          addressdata             = ls_address1
          duplicate_message_type  = 'W'
        IMPORTING
          businesspartner         = businesspartner
        TABLES
          telefondata             = it_telephondata
          e_maildata              = it_maildata
          return                  = return.

      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        gv_error = abap_true.
        LOOP AT  return INTO DATA(ls_return) WHERE type = 'E'.
          APPEND VALUE #( sortl = gs_input-generaldata-searchterm1
                          msg  =  ls_return-message
                          msgtyp = ls_return-type
                          msgnr = ls_return-number
                          uzeit = sy-uzeit
                          erdat = sy-datum
                          uname = sy-uname ) TO gt_log.

        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        IF businesspartner IS NOT INITIAL AND gv_error IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          DATA(ls_log) = VALUE zsd_bp_creation( bu_partner = businesspartner
                                                bu_partnerx = 'B'
                                                msgtyp = 'S'
                                                msg = 'Business Partner Created Successfully'
                                                uzeit = sy-uzeit
                                                erdat = sy-datum
                                                uname = sy-uname
                                                msgnr = '001'
                                                bpname = gs_input-generaldata-name-name1
                                                sortl = ls_central-searchterm1 ).

          DATA(gs_resp) = VALUE zst_bp_log_output( bu_partner = businesspartner
                                                 bu_partnerx = 'B'
                                                 msgtyp = 'S'
                                                 msg = 'Business Partner Created Successfully'
                                                 uzeit = sy-uzeit
                                                 erdat = sy-datum
                                                 uname = sy-uname
                                                 msgnr = '001'
                                                 bpname = gs_input-generaldata-name-name1
                                                 sortl = ls_central-searchterm1 ).
          APPEND gs_resp TO gt_log.
          MODIFY zsd_bp_creation FROM ls_log.
          """""Add Roles to BusinessPartner"""""
          me->bp_add_roles(
            EXPORTING
              iv_grouping     = gs_input-generaldata-grouping
              businesspartner = businesspartner
          ).
**** Adding the tax and Tax number ***
          IF gs_input-generaldata-taxno IS NOT INITIAL ."AND  gs_input-generaldata-taxnumbers-taxcat IS NOT INITIAL.
            DATA: t_return TYPE TABLE OF bapiret2.
            REFRESH t_return.
            CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
              EXPORTING
                businesspartner = businesspartner
                taxtype         = c_taxtyp "Tax type
                taxnumber       = gs_input-generaldata-taxno
              TABLES
                return          = t_return.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
***** Setting Grouping Char to the Business partner
          me->set_grouping_char(                           " Grouping Characteristics
            EXPORTING
              ls_group_char   = gs_input-generaldata-grouping_char
              businesspartner = businesspartner
            IMPORTING
              gt_log          = gt_log
          ).
***** Extending Sales Area and company code for Newly created Business Partner ****
          REFRESH: lt_tax_data,is_master_data-customers,lt_customers[],lt_sales[],lt_sales_func,lt_company[].
          CLEAR: ls_tax_data,ls_sales_func,ls_customers,ls_address,is_master_data,ls_sales_data,lt_sales,ls_company_data.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = businesspartner
            IMPORTING
              output = ls_customers-header-object_instance-kunnr.
*          CLEAR ls_kna1.
          SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @ls_customers-header-object_instance-kunnr.
          IF gs_input-companycode-cust_type = '11' AND gs_input-generaldata-pan_no IS NOT INITIAL.
            me->update_pan(
              EXPORTING
                ls_kna1  = ls_kna1
                iv_input = gs_input
            ).
          ENDIF.
          ls_customers-header-object_task = 'U'. "Represents Insert or Create
          ls_address-postal-data-name          = ls_kna1-name1. "Name of the Vendor
          ls_address-postal-data-country       = ls_kna1-land1.
          ls_address-postal-datax-name         = 'X'.
          ls_address-postal-datax-country      = 'X'.
          ls_address-task                      = 'U'.
*   set the address for the vendor
          ls_customers-central_data-address = ls_address.
*   Mapping company code based on customer type
          DATA: is_bukrs TYPE string.
          is_bukrs = SWITCH #( gs_input-companycode-cust_type WHEN '10' THEN '1000'
                                                              WHEN '11' THEN '1000'
                                                              WHEN '12' THEN 'DMS1'
                                                              WHEN '13' THEN ' '
                                                              WHEN '14' THEN ' '
                                                              WHEN '15' THEN ' '
                                                              WHEN '16' THEN ' ').
          IF is_bukrs CA ','.
            SPLIT is_bukrs AT ',' INTO DATA(bukrs1) DATA(bukrs2).
            CONDENSE: bukrs1,bukrs2.
          ELSE.
            DATA(lv_bukrs) = is_bukrs.
          ENDIF.
*********Company code structure
          DATA(ls_company) =  VALUE cmds_ei_company( ).
*****Fetching Mapping field based on Company Code
          SELECT FROM zmap_table_bp
          FIELDS mapp_field
          WHERE purpose = 'CCODE_TO_RECONACCT' AND
                subs_field IN ( @lv_bukrs, @bukrs1, @bukrs2 )
          INTO TABLE @DATA(lt_reco_acnt).
          DATA(is_reco_acnt) = VALUE akont(  ).
          DATA(count) = VALUE i(  ).
          IF is_bukrs IS NOT INITIAL AND strlen( is_bukrs ) = 4.
            CONDENSE is_bukrs.
            ls_company = VALUE #( task = 'I'
                                  data_key = CONV #( is_bukrs )
*                                  data-zterm = gs_input-companycode-zterm
                                  datax-akont = 'X'
                                  datax-zterm = 'X' ).
            is_reco_acnt = lt_reco_acnt[ 1 ].
            UNPACK is_reco_acnt TO ls_company-data-akont.
            APPEND ls_company TO lt_company.
          ELSE.
            ADD 1 TO count.
            ls_company = VALUE #( task = 'I'
                                  data_key = CONV #( bukrs1 )
                                  data-zterm = gs_input-companycode-zterm  " 1 cocode paymterm
                                  datax-akont = 'X'
                                  datax-zterm = 'X' ).
            is_reco_acnt = lt_reco_acnt[ count ].
            UNPACK is_reco_acnt TO ls_company-data-akont.
            APPEND ls_company TO lt_company.

            ADD 1 TO count.
            ls_company = VALUE #( task = 'I'
                                 data_key = CONV #( bukrs2 )
                                 data-zterm = gs_input-companycode-zterm   "2 cocode paymterm
                                 datax-akont = 'X'
                                 datax-zterm = 'X' ).
            is_reco_acnt = lt_reco_acnt[ count ].
            UNPACK is_reco_acnt TO ls_company-data-akont.
            APPEND ls_company TO lt_company.
          ENDIF.

          ls_company_data-company = lt_company[].
          ls_customers-company_data = ls_company_data.
*   Mapping Sales area  based on customer type
          DATA: is_sorg TYPE string.
*****Fetching Mapping field based on Grouping Char
          SELECT SINGLE FROM zmap_table_bp
          FIELDS mapp_field
          WHERE purpose = 'GROUPCHAR_TO_CUSTGRP' AND
                subs_field = @gs_input-generaldata-grouping_char
          INTO @DATA(lv_cust_grp).
*****Fetching Sales District based on region
          SELECT SINGLE FROM zmap_table_bp
          FIELDS mapp_field
          WHERE purpose = 'REGIO_TO_BZIRK' AND
                subs_field = @gs_input-generaldata-addressdata-region
          INTO @DATA(iv_bzirk).
*****Fetching Salesorg based on Customer type
          is_sorg = SWITCH #( gs_input-companycode-cust_type WHEN '10' THEN '1000'
                                                             WHEN '11' THEN '1000'
                                                             WHEN '12' THEN 'SDMS'
                                                             WHEN '13' THEN ' '
                                                             WHEN '14' THEN ' '
                                                             WHEN '15' THEN ' '
                                                             WHEN '16' THEN ' ').
*          IF is_sorg CA ','.
*            SPLIT is_sorg AT ',' INTO DATA(sorg1) DATA(sorg2).
*            CONDENSE: sorg1,sorg2.
*            DATA(lt_sorgs) = VALUE string_t(  ).
*            APPEND sorg1 TO lt_sorgs.
*            APPEND sorg2 TO lt_sorgs.
*          ENDIF.
*****Manipulation for Sales area extension
          DATA(ls_sales) =  VALUE cmds_ei_sales(  ).
          CASE is_sorg.
            WHEN  'SDMS'.
              DATA(gs_kotnr) = VALUE kunnr(  ).
              UNPACK gs_input-salesorg-ktonr TO gs_kotnr.
              SELECT SINGLE FROM kna1
              FIELDS werks
              WHERE kunnr = @gs_kotnr
              INTO @DATA(iv_vwerk).

              ls_sales = VALUE cmds_ei_sales(  task = 'I'
                                                         data_key-vkorg = CONV #( is_sorg )
                                                         data_key-vtweg = c_vtweg             "Wholesale
                                                         data_key-spart = c_spart       " 10 General
                                                         data-bzirk = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                iv_bzirk ELSE ' ' )
                                                         data-kdgrp = CONV #( lv_cust_grp )
                                                         data-ktgrd = c_ktgrd
                                                         data-vkbur = SWITCH #( is_sorg WHEN '6000' THEN '6001' ELSE iv_vwerk )"iv_vwerk
                                                         data-vkgrp = c_spart  " as said division and sales group is equal
                                                         data-kalks = c_kalks
                                                         data-inco1 = c_inco1
                                                         data-inco2_l = gs_input-generaldata-addressdata-city
                                                         data-vwerk = SWITCH #( is_sorg WHEN '6000' THEN '6000' ELSE iv_vwerk )"iv_vwerk
                                                         data-vsbed = '01'
                                                         data-zterm = SWITCH #( c_spart WHEN '10' THEN 'NT07' ELSE 'NT30' )
                                                         data-lprio = c_lprio
                                                         data-waers = 'INR'
                                                         data-konda = SWITCH #(  gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                '01' ELSE '' )
                                                         data-pltyp = SWITCH #(  gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                '01' ELSE '' )
                                                         data-kabss = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                 '0001' ELSE ' '  )
                                                         data-kkber = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                 '1000' ELSE ' '  )
                                                         datax-bzirk = 'X'
                                                         datax-kdgrp = 'X'
                                                         datax-ktgrd = 'X'
                                                         datax-vkbur = 'X'
                                                         datax-vkgrp = 'X'
                                                         datax-kalks = 'X'
                                                         datax-incov = 'X'
                                                         datax-inco1 = 'X'
                                                         datax-inco2 = 'X'
                                                         datax-vwerk = 'X'
                                                         datax-vsbed = 'X'
                                                         datax-zterm = 'X'
                                                         datax-waers = 'X'
                                                         datax-inco2_l = 'X'
                                                         datax-lprio = 'X'
                                                         datax-konda = 'X'
                                                         datax-pltyp = 'X'
                                                         datax-kabss = 'X'
                                                         datax-kkber = 'X' ).

              REFRESH et_parvw.
              REFRESH lt_sales_func.
              CALL METHOD cmd_ei_api_check=>get_mand_partner_functions
                EXPORTING
                  iv_ktokd = ls_kna1-ktokd
                IMPORTING
                  et_parvw = et_parvw.

              LOOP AT et_parvw INTO wa_parvw.
                ls_sales_func-task = 'I'.
                ls_sales_func-data_key-parvw = wa_parvw-parvw.
                ls_sales_func-data-partner = SWITCH #( wa_parvw-parvw  WHEN 'AG' THEN ls_kna1-kunnr
                                                                       WHEN 'WE' THEN ls_kna1-kunnr
                                                                       WHEN 'RG' THEN ls_kna1-kunnr
                                                                       WHEN 'RE' THEN ls_kna1-kunnr
                                                                       WHEN 'SP' THEN ls_kna1-kunnr
                                                                       WHEN 'BP' THEN ls_kna1-kunnr
                                                                       WHEN 'PY' THEN ls_kna1-kunnr
                                                                       WHEN 'SH' THEN ls_kna1-kunnr
                                                                       WHEN 'L5' THEN '0009003004'
                                                                       WHEN 'XN' THEN '0009003004'
                                                                       WHEN 'ZA' THEN '0000701955'
                                                                       WHEN 'ZC' THEN '0000700537'
                                                                       WHEN 'ZS' THEN '0009003004'
                                                                       WHEN 'ZH' THEN '0000701405'
                                                                       WHEN 'L3' THEN '0009003002'
                                                                       WHEN 'RS' THEN '0009003003'
                                                                       WHEN 'L2' THEN '0009003003'
                                                                       WHEN 'L1' THEN '0009005599'
                                                                        ).

                ls_sales_func-datax-partner = 'X'.
                APPEND ls_sales_func TO lt_sales_func.
                CLEAR ls_sales_func.
                CLEAR wa_parvw.
              ENDLOOP.

              ls_sales_func-task = 'I'.
              ls_sales_func-data_key-parvw = 'SK'.
              ls_sales_func-data-partner = gs_kotnr.
              ls_sales_func-datax-partner = 'X'.
              APPEND ls_sales_func TO lt_sales_func.
              CLEAR ls_sales_func.
              ls_sales-functions-functions = lt_sales_func[].
              APPEND ls_sales TO lt_sales.
              ls_sales_data-sales = lt_sales[].

              me->maintain_tax(
                EXPORTING
                  ls_customers    = ls_customers
                  businesspartner = businesspartner
                  ls_sales_data   = ls_sales_data
                IMPORTING
                  is_master_data  = is_master_data
              ).
            WHEN '1000'.
              DATA(lt_spart) = VALUE sparte_tab( ( '10' )  ( '30' )  ( '35' )
                                                 ( '50' ) ( '60' )  ( '70' ) ( '90' )  ).
              "( '11' ) ( '12' ) ( '31' ) ( '61' ) ( '96' ) ( '45' )
              SELECT FROM zbp_soffice_tabl
                FIELDS *
                WHERE sales_org = @is_sorg AND region = @gs_input-generaldata-addressdata-region
                INTO TABLE @DATA(gt_soffice).
              LOOP AT lt_spart INTO DATA(ls_spart).
                DATA(ls_vkbur) = VALUE #( gt_soffice[ divison = ls_spart ]-soffice OPTIONAL  ).
                ls_sales = VALUE cmds_ei_sales(  task = 'I'
                                                          data_key-vkorg = CONV #( is_sorg )
                                                          data_key-vtweg = c_vtweg             "Wholesale
                                                          data_key-spart = ls_spart
                                                          data-bzirk = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                  iv_bzirk )
                                                          data-kdgrp = CONV #( lv_cust_grp )
                                                          data-ktgrd = c_ktgrd
                                                          data-vkbur = ls_vkbur
                                                          data-vkgrp = ls_spart  " as said division and sales group is equal
                                                          data-kalks = c_kalks
                                                          data-inco1 = c_inco1
                                                          data-inco2_l = gs_input-generaldata-addressdata-city
                                                          data-vwerk = ls_vkbur
                                                          data-vsbed = '01'
                                                          data-zterm = SWITCH #( ls_spart WHEN '10' THEN 'NT07' ELSE 'NT30' )
                                                          data-lprio = c_lprio
                                                          data-waers = 'INR'
                                                          data-konda = SWITCH #(  gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                  '01' ELSE '' )
                                                          data-pltyp = SWITCH #(  gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                  '01' ELSE '' )
                                                          data-kabss = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                   '0001' ELSE ' '  )
                                                          data-kkber = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN
                                                                                   '1000' ELSE ' '  )
                                                          datax-bzirk = 'X'
                                                          datax-kdgrp = 'X'
                                                          datax-ktgrd = 'X'
                                                          datax-vkbur = 'X'
                                                          datax-vkgrp = 'X'
                                                          datax-kalks = 'X'
                                                          datax-incov = 'X'
                                                          datax-inco1 = 'X'
                                                          datax-inco2 = 'X'
                                                          datax-vwerk = 'X'
                                                          datax-vsbed = 'X'
                                                          datax-zterm = 'X'
                                                          datax-waers = 'X'
                                                          datax-inco2_l = 'X'
                                                          datax-lprio = 'X'
                                                          datax-konda = 'X'
                                                          datax-pltyp = 'X'
                                                          datax-kabss = 'X'
                                                          datax-kkber = 'X' ).
                REFRESH et_parvw.
                REFRESH lt_sales_func.
                CALL METHOD cmd_ei_api_check=>get_mand_partner_functions
                  EXPORTING
                    iv_ktokd = ls_kna1-ktokd
                  IMPORTING
                    et_parvw = et_parvw.

                LOOP AT et_parvw INTO wa_parvw.
                  ls_sales_func-task = 'I'.
                  ls_sales_func-data_key-parvw = wa_parvw-parvw.
                  ls_sales_func-data-partner = SWITCH #( wa_parvw-parvw  WHEN 'AG' THEN ls_kna1-kunnr
                                                                         WHEN 'WE' THEN ls_kna1-kunnr
                                                                         WHEN 'RG' THEN ls_kna1-kunnr
                                                                         WHEN 'RE' THEN ls_kna1-kunnr
                                                                         WHEN 'SP' THEN ls_kna1-kunnr
                                                                         WHEN 'BP' THEN ls_kna1-kunnr
                                                                         WHEN 'PY' THEN ls_kna1-kunnr
                                                                         WHEN 'SH' THEN ls_kna1-kunnr
                                                                         WHEN 'L5' THEN '0009003004'
                                                                         WHEN 'XN' THEN '0009003004'
                                                                         WHEN 'ZA' THEN '0000701955'
                                                                         WHEN 'ZC' THEN '0000700537'
                                                                         WHEN 'ZS' THEN '0009003004'
                                                                         WHEN 'ZH' THEN '0000701405'
                                                                         WHEN 'L3' THEN '0009003002'
                                                                         WHEN 'RS' THEN '0009003003'
                                                                         WHEN 'L2' THEN '0009003003'
                                                                         WHEN 'L1' THEN '0009005599'
                                                                          ).

                  ls_sales_func-datax-partner = 'X'.
                  APPEND ls_sales_func TO lt_sales_func.
                  CLEAR ls_sales_func.
                  CLEAR wa_parvw.
                ENDLOOP.

                CLEAR ls_sales_func.
                ls_sales-functions-functions = lt_sales_func[].
                APPEND ls_sales TO lt_sales.
                ls_sales_data-sales = lt_sales[].

                me->maintain_tax(
                  EXPORTING
                    ls_customers    = ls_customers
                    businesspartner = businesspartner
                    ls_sales_data   = ls_sales_data
                  IMPORTING
                    is_master_data  = is_master_data
                ).
              ENDLOOP.
          ENDCASE.
          me->customer_extnsn(
             EXPORTING
               is_master_data  = is_master_data
               businesspartner = businesspartner
               sorg            = CONV #( is_sorg )
               ls_kna1         = ls_kna1
               bukrs           = CONV #( lv_bukrs )
             IMPORTING
               gt_log          = gt_log
           ).
*          ELSE.

*****Manipulation for Sales area extension
*            LOOP AT lt_sorgs INTO DATA(ls_sorg).                  "WHEN SDMS
*
*              ls_sales = VALUE cmds_ei_sales(  task = 'I'
*                                                         data_key-vkorg = CONV #( ls_sorg )
*                                                         data_key-vtweg = c_vtweg             "Wholesale
*                                                         data_key-spart = c_spart       " 10 General
**                                                 data-bzirk = gs_process-bzirk
*                                                 data-kdgrp = CONV #( lv_cust_grp )
*                                                         data-ktgrd = c_ktgrd
**                                                         data-vkbur = SWITCH #( ls_sorg WHEN '6000' THEN '6001' ELSE iv_vwerk )"iv_vwerk
*                                                         data-vkgrp = c_spart  " as said division and sales group is equal
*                                                         data-kalks = c_kalks
*                                                         data-inco1 = c_inco1
*                                                         data-inco2_l = gs_input-generaldata-addressdata-city
**                                                         data-vwerk = SWITCH #( ls_sorg WHEN '6000' THEN '6000' ELSE iv_vwerk )"iv_vwerk
*                                                         data-vsbed = '01'
*                                                         data-zterm = SWITCH #( ls_spart WHEN '10' THEN 'NT07' ELSE 'NT30' )
*                                                         data-lprio = c_lprio
**                                                 data-pltyp = gs_process-pltyp
*                                                         data-waers = 'INR'
*                                                         datax-bzirk = 'X'
*                                                         datax-kdgrp = 'X'
*                                                         datax-ktgrd = 'X'
*                                                         datax-vkbur = 'X'
*                                                         datax-vkgrp = 'X'
*                                                         datax-kalks = 'X'
*                                                         datax-incov = 'X'
*                                                         datax-inco1 = 'X'
*                                                         datax-inco2 = 'X'
*                                                         datax-vwerk = 'X'
*                                                         datax-vsbed = 'X'
*                                                         datax-zterm = 'X'
*                                                         datax-waers = 'X'
*                                                         datax-inco2_l = 'X'
*                                                         datax-lprio = 'X'
*                                                         datax-pltyp = 'X' ).
*
*              REFRESH et_parvw.
*              REFRESH lt_sales_func.
*              CALL METHOD cmd_ei_api_check=>get_mand_partner_functions
*                EXPORTING
*                  iv_ktokd = ls_kna1-ktokd
*                IMPORTING
*                  et_parvw = et_parvw.
*
*              LOOP AT et_parvw INTO wa_parvw.
*                ls_sales_func-task = 'I'.
*                ls_sales_func-data_key-parvw = wa_parvw-parvw.
*                ls_sales_func-data-partner = SWITCH #( wa_parvw-parvw  WHEN 'AG' THEN ls_kna1-kunnr
*                                                                           WHEN 'WE' THEN ls_kna1-kunnr
*                                                                           WHEN 'RG' THEN ls_kna1-kunnr
*                                                                           WHEN 'RE' THEN ls_kna1-kunnr
*                                                                           WHEN 'SP' THEN ls_kna1-kunnr
*                                                                           WHEN 'BP' THEN ls_kna1-kunnr
*                                                                           WHEN 'PY' THEN ls_kna1-kunnr
*                                                                           WHEN 'SH' THEN ls_kna1-kunnr
*                                                                           WHEN 'L5' THEN '0009003004'
*                                                                           WHEN 'XN' THEN '0009003004'
*                                                                           WHEN 'ZA' THEN '0000701955'
*                                                                           WHEN 'ZC' THEN '0000700537'
*                                                                           WHEN 'ZS' THEN '0009003004'
*                                                                           WHEN 'ZH' THEN '0000701405'
*                                                                           WHEN 'L3' THEN '0009003002'
*                                                                           WHEN 'RS' THEN '0009003003'
*                                                                           WHEN 'L2' THEN '0009003003'
*                                                                           WHEN 'L1' THEN '0009005599'
*                                                                            ).
*                ls_sales_func-datax-partner = 'X'.
*                APPEND ls_sales_func TO lt_sales_func.
*                CLEAR ls_sales_func.
*                CLEAR wa_parvw.
*              ENDLOOP. " LOOP AT et_parvw INTO DATA(wa_parvw)
*              ls_sales_func-task = 'I'.
*              ls_sales_func-data_key-parvw = 'SK'.
*              ls_sales_func-data-partner = gs_kotnr.
*              ls_sales_func-datax-partner = 'X'.
*              APPEND ls_sales_func TO lt_sales_func.
*              CLEAR ls_sales_func.
*              ls_sales-functions-functions = lt_sales_func[].
*              APPEND ls_sales TO lt_sales.
*              ls_sales_data-sales = lt_sales[].
*
*              me->maintain_tax(
*                     EXPORTING
*                       ls_customers    = ls_customers
*                       businesspartner = businesspartner
*                       ls_sales_data   = ls_sales_data
*                     IMPORTING
*                       is_master_data  = is_master_data
*                   ).
*
*            ENDLOOP.
*            me->customer_extnsn(
*               EXPORTING
*                 is_master_data  = is_master_data
*                 businesspartner = businesspartner
*                 sorg            = CONV #( ls_sorg )
*                 ls_kna1         = ls_kna1
*               IMPORTING
*                 gt_log          = gt_log
*             ).
*          ENDIF.


**** Updating additional data like Attributes for Customer ***
          DATA: ls_kna1_up TYPE kna1.
          SELECT SINGLE * FROM kna1 INTO @DATA(l_ls_kna1) WHERE kunnr = @businesspartner.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING l_ls_kna1 TO ls_kna1_up.
            ls_kna1_up-stcd3  = gs_input-generaldata-taxno.
            ls_kna1_up-katr1  = gs_input-generaldata-attributes-attr1.
            ls_kna1_up-katr2  = gs_input-generaldata-attributes-attr2.
            ls_kna1_up-katr3  = gs_input-generaldata-attributes-attr3.
            ls_kna1_up-katr4  = gs_input-generaldata-attributes-attr4.
            ls_kna1_up-katr5  = gs_input-generaldata-attributes-attr5.
            ls_kna1_up-katr6  = gs_input-generaldata-attributes-attr6.
            ls_kna1_up-katr7  = gs_input-generaldata-attributes-attr7.
            ls_kna1_up-katr8  = gs_input-generaldata-attributes-attr8.
            ls_kna1_up-katr9  = gs_input-generaldata-attributes-attr9.
            ls_kna1_up-katr10 = gs_input-generaldata-attributes-attr10.

            CALL FUNCTION 'J_3G_KNA1_MODIFY'
              EXPORTING
                i_kna1 = ls_kna1_up.

          ENDIF.
        ELSE.
          LOOP AT es_message_defective-messages INTO DATA(wa).
            WAIT UP TO 1 SECONDS.
            DATA(lv_msg) = wa-message.
            lv_msg = | { lv_msg } { wa-message } |.
          ENDLOOP.
          DATA(ls_error) = VALUE zst_bp_log_output( bu_partner = businesspartner
                                                  bu_partnerx = 'C'
                                                  msg  = lv_msg
                                                  msgtyp = wa-type
                                                  msgnr = wa-number
                                                  uzeit = sy-uzeit
                                                  erdat = sy-datum
                                                  uname = sy-uname
                                                  bpname = ls_kna1-name1
                                                  customer = businesspartner ).
          APPEND ls_error TO gt_log.
          REFRESH i_return.
        ENDIF.
      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD validations.
    TYPES: BEGIN OF ty_hrp1001_obj,
             objid TYPE hrp1001-objid,
           END OF ty_hrp1001_obj.
    DATA: lt_hrp1001_obj TYPE TABLE OF ty_hrp1001_obj.
    DATA: return   TYPE STANDARD TABLE OF bapiret2,
          ls_error TYPE cvis_message,
          ls_msg   TYPE bapiret2,
          lv_msg   TYPE bapi_msg.
IF ls_input is not INITIAL and iv_accnt_type = 'D'.
****************Validation for Customer Master**************
    IF ls_input-generaldata-name-name1 IS INITIAL.
      APPEND VALUE #( type = 'E'
                    number = '002'
                    message = 'Name1 is Mandatory'
                    message_v1 = 'NAME1' ) TO return.
    ENDIF.

    IF ls_input-generaldata-grouping_char IS INITIAL.
      APPEND VALUE #( type = 'E'
          number = '002'
          message = 'grouping_char is Mandatory'
          message_v1 = 'GROUPING_CHAR' ) TO return.
    ELSEIF  strlen( ls_input-generaldata-grouping_char ) > 4 .
      APPEND VALUE #( type = 'E'
        number = '002'
        message = 'Grouping_char Length Exceeds than Required'
        message_v1 = 'GROUPING_CHAR' ) TO return.
    ENDIF.

    IF ls_input-companycode-cust_type IS INITIAL.
      APPEND VALUE #( type = 'E'
                   number = '002'
                   message = 'Customer Type is Mandatory(ie. 11 - Distributor , 12 - Sheenlac Sub-Dealers)'
                   message_v1 = 'CUST_TYPE' ) TO return.
    ELSEIF ls_input-companycode-cust_type = 12 AND ls_input-salesorg-ktonr IS INITIAL.
      APPEND VALUE #( type = 'E'
               number = '002'
               message = |SK Partner number Required for the Customer Type { ls_input-companycode-cust_type } : Sheenlac Sub-Dealer|
               message_v1 = 'KTONR' ) TO return.
   ELSEIF ls_input-companycode-cust_type = 12 AND ls_input-salesorg-ktonr is NOT INITIAL.
     DATA(gs_kotnr) = VALUE kunnr(  ).
        UNPACK ls_input-salesorg-ktonr TO gs_kotnr.
        SELECT SINGLE FROM kna1
        FIELDS werks
        WHERE kunnr = @gs_kotnr
        INTO @DATA(iv_vwerk).
        If sy-subrc <> 0.
          APPEND VALUE #( type = 'E'
               number = '002'
               message = |Delivering Plant is not available for the SKU { gs_kotnr }|
               message_v1 = 'VWERK' ) TO return.
        ENDIF.
    ELSEIF ls_input-companycode-cust_type <> 12 AND  ls_input-salesorg-ktonr IS NOT INITIAL.
      APPEND VALUE #( type = 'E'
       number = '002'
       message = |SK Partner number not required for the Customer Type { ls_input-companycode-cust_type } : Distributor|
       message_v1 = 'KTONR' ) TO return.
*    ELSEIF ls_input-companycode-cust_type = 11.
*        SELECT FROM zbp_soffice_tabl
*        FIELDS *
*        FOR ALL ENTRIES IN @lt_cust
*        WHERE sales_org = @lt_cust-vkorg and region = @gs_input-generaldata-addressdata-region
*         INTO TABLE @DATA(gt_soffice).
    ENDIF.

    IF ls_input-generaldata-taxno IS NOT INITIAL.
      SELECT SINGLE *  FROM  dfkkbptaxnum INTO @DATA(ls_stcd3) WHERE taxnum = @ls_input-generaldata-taxno
                                                               AND taxtype = @c_taxtyp.
      IF sy-subrc EQ 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = |GST number is already exists for customer { ls_stcd3-partner ALPHA = OUT }|
                        message_v1 = 'TAXNO' ) TO return.
      ENDIF.
*    ELSE.
*      APPEND VALUE #( type = 'E'
*                      number = '002'
*                      message = 'GST number is mandatory'
*                      message_v1 = 'TAXNO' ) TO return.
    ENDIF.

    IF ls_input-generaldata-grouping IS NOT INITIAL.
      SELECT SINGLE bu_group FROM tb001 INTO @DATA(lv_bpgrp) WHERE bu_group = @ls_input-generaldata-grouping.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'Business partner group is incorrect'
                        message_v1 = 'BPGRP' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'Business partner group is not available'
                      message_v1 = 'BPGRP' ) TO return.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-title IS NOT INITIAL.
      SELECT SINGLE * FROM tsad3t INTO @DATA(ls_tsad3t) WHERE title_medi = @ls_input-generaldata-title.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = | { 'Title' } { ls_input-generaldata-title } { 'does not exists' } |
                        message_v1 = 'TITLE' )  TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'Title is required field'
                      message_v1 = 'TITLE' ) TO return.
    ENDIF.
******************************************************************************************** "CCode validation Commented
**************************Sales District DERIVING AT THE BACKEND*************************************
*******************************Sales group need to be derived*************************************************************
**************************************Fix Pricing Procedure Default as '1'  ******************************************************
***************************************Shipping plant Defaulted '01'*****************************************************
*********************************************TAX classification derived in backend***********************************************
********************************************************************************************
    IF ls_input-generaldata-attributes-attr1 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr1_check
        EXPORTING
          iv_katr1_new        = ls_input-generaldata-attributes-attr1
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR ls_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          ls_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR1 ' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr2 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr2_check
        EXPORTING
          iv_katr2_new        = ls_input-generaldata-attributes-attr2
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR2' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr3 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr3_check
        EXPORTING
          iv_katr3_new        = ls_input-generaldata-attributes-attr3
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR3' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr4 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr4_check
        EXPORTING
          iv_katr4_new        = ls_input-generaldata-attributes-attr4
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR4' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr5 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr5_check
        EXPORTING
          iv_katr5_new        = ls_input-generaldata-attributes-attr5
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR5' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr6 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr6_check
        EXPORTING
          iv_katr6_new        = ls_input-generaldata-attributes-attr6
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR6' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF ls_input-generaldata-attributes-attr7 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr7_check
        EXPORTING
          iv_katr7_new        = ls_input-generaldata-attributes-attr7
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR7' ) TO return.
      ENDIF.
    ENDIF.

    IF ls_input-generaldata-attributes-attr8 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr8_check
        EXPORTING
          iv_katr8_new        = ls_input-generaldata-attributes-attr8
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR8' ) TO return.
      ENDIF.
    ENDIF.

    IF ls_input-generaldata-attributes-attr9 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr9_check
        EXPORTING
          iv_katr9_new        = ls_input-generaldata-attributes-attr9
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR9' ) TO return.
      ENDIF.
    ENDIF.

    IF ls_input-generaldata-attributes-attr10 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr10_check
        EXPORTING
          iv_katr10_new       = ls_input-generaldata-attributes-attr10
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR10' ) TO return.
      ENDIF.
    ENDIF.
*** PARTNER FUNCTION Derived Back-end  ****
    IF ls_input-companycode-cust_type = 12.
      IF ls_input-salesorg-ktonr IS NOT INITIAL.
        SELECT SINGLE FROM kna1
        FIELDS werks
        WHERE kunnr = @ls_input-salesorg-ktonr
        INTO @DATA(ls_werks).
        IF Sy-subrc = 0 .
          IF ls_werks IS INITIAL.
            lv_msg = |Given partner number is not an SKU|.
            APPEND VALUE #( type = 'E'
                                   number = '002'
                                   message = lv_msg
                                   message_v1 = ls_input-salesorg-ktonr ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND LINES OF return TO gt_return.
    ELSE.
"""""""""""""""""""""""""""""Validation for Vendor Master"""""""""""""""""""""""""""""
 IF gs_input is not INITIAL and  iv_accnt_type = 'K'.
*****************************Name1 validation*****************************************
    IF gs_input-generaldata-name-name1 IS INITIAL .
      APPEND VALUE #( type = 'E'
                    number = '002'
                    message = 'Name1 is Mandatory'
                    message_v1 = 'NAME1' ) TO return.
    ENDIF.
****************************GROUPING_CHAR validation***********************************
    IF gs_input-generaldata-grouping_char IS INITIAL.
      APPEND VALUE #( type = 'E'
          number = '002'
          message = 'grouping_char is Mandatory'
          message_v1 = 'GROUPING_CHAR' ) TO return.
    ELSEIF  strlen( gs_input-generaldata-grouping_char ) > 4 .
      APPEND VALUE #( type = 'E'
        number = '002'
        message = 'Grouping_char Length Exceeds than Required'
        message_v1 = 'GROUPING_CHAR' ) TO return.
    ENDIF.

    IF gs_input-companycode IS INITIAL.
      APPEND VALUE #( type = 'E'
                   number = '002'
                   message = 'Company Code is Mandatory'
                   message_v1 = 'CUST_TYPE' ) TO return.

    ENDIF.
****************************Tax No Validation******************************************
    IF gs_input-generaldata-taxno IS NOT INITIAL.
      SELECT SINGLE *  FROM  dfkkbptaxnum INTO @DATA(gs_stcd3) WHERE taxnum = @gs_input-generaldata-taxno
                                                               AND taxtype = @c_taxtyp.
      IF sy-subrc EQ 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = |GST number is already exists for customer { ls_stcd3-partner ALPHA = OUT }|
                        message_v1 = 'TAXNO' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'GST number is mandatory'
                      message_v1 = 'TAXNO' ) TO return.
    ENDIF.
****************************Grouping Validation******************************************
    IF gs_input-generaldata-grouping IS NOT INITIAL.
      SELECT SINGLE bu_group FROM tb001 INTO @DATA(gv_bpgrp) WHERE bu_group = @gs_input-generaldata-grouping.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'Business partner group is incorrect'
                        message_v1 = 'BPGRP' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'Business partner group is required field'
                      message_v1 = 'BPGRP' ) TO return.
    ENDIF.
************************************Title Validation***********************************************
    IF gs_input-generaldata-title IS NOT INITIAL.
      SELECT SINGLE * FROM tsad3t INTO @DATA(gs_tsad3t) WHERE title_medi = @gs_input-generaldata-title.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = | { 'Title' } { gs_input-generaldata-title } { 'does not exists' } |
                        message_v1 = 'TITLE' )  TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'Title is required field'
                      message_v1 = 'TITLE' ) TO return.
    ENDIF.
*******************************Company code data validation***************************************
    IF gs_input-companycode is NOT INITIAL .      "Check existence of company table of vendor
      LOOP AT gs_input-companycode into data(gs_company) .
      IF gs_company-company_code is NOT INITIAL.           "Company code check
      Select SINGLE from t001 FIELDS bukrs where bukrs = @gs_company-company_code INTO @data(iv_bukrs).
      IF sy-subrc ne 0.
        APPEND VALUE #( type = 'E'
                      number = '003'
                      message = 'Invalid company code'
                      message_v1 = 'company_code' ) TO return.
      ENDIF.
      IF gs_company-reconiliation_acct is NOT INITIAL.
      data(ls_akont) = |{ gs_company-reconiliation_acct alpha = IN }|.
      select SINGLE from SKA1 FIELDS saknr where saknr = @ls_akont INTO @data(iv_saknr).
      IF sy-subrc ne 0.
       APPEND VALUE #( type = 'E'
                      number = '003'
                      message = |Invalid Reconciliation account|
                      message_v1 = 'reconiliation_acct' ) TO return.
      ENDIF.
      IF gs_company-house_bank is NOT INITIAL.
       SELECT SINGLE from T012 FIELDS hbkid where bukrs = @gs_company-company_code and
                                                  hbkid = @gs_company-house_bank
                                            INTO  @data(iv_hbkid).
      IF sy-subrc ne 0.
      APPEND VALUE #( type = 'E'
                      number = '003'
                      message = |Invalid House bank for company code { gs_company-company_code }|
                      message_v1 = 'house_bank' ) TO return.
      ENDIF.
     IF gs_company-minority_indicator is not initial.
     select single from T059M FIELDS mindk where mindk = @gs_company-minority_indicator INTO @data(iv_mindk).
     IF sy-subrc ne 0.
     APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Minority Indicator'
                     message_v1 = 'minority_indicator' ) TO return.
     ENDIF.
     IF gs_company-payment_terms is NOT INITIAL.
     select single from T052 FIELDS zterm where zterm = @gs_company-payment_terms INTO @data(iv_zterm).
     IF sy-subrc ne 0.
     APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Payterm terms'
                     message_v1 = 'payment_terms' ) TO return.
     ENDIF.
     IF gs_company-payment_methods is NOT INITIAL.
     Select single from T042Z FIELDS zlsch WHERE land1 = @c_cntry and zlsch = @gs_company-payment_methods INTO @data(iv_zlsch).
     IF sy-subrc <> 0.
       APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Payterm methods'
                     message_v1 = 'payment_methods' ) TO return.
     ENDIF.
    "Check existence of payment block
    Select single from T008 FIELDS xozsp WHERE xozsp = @gs_company-payment_block INTO @data(iv_xozsp).
    IF sy-subrc ne 0.
    APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Payment block is required field'
                     message_v1 = 'payment_block' ) TO return.
    ENDIF.
    IF gs_company-withholding_tax is NOT INITIAL.
    Loop at gs_company-withholding_tax into data(ls_whtax).
    IF ls_whtax-wtax_type is NOT INITIAL.
        select single from T059P fields witht where land1 = @c_cntry and witht = @ls_whtax-wtax_type into @data(iv_witht).
        If sy-subrc <> 0.
          APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Withholding Tax Type'
                     message_v1 = 'wtax_type' ) TO return.
        ENDIF.

*        ELSE.
*         APPEND VALUE #( type = 'E'
*                     number = '003'
*                     message = ' Withholding Tax Type is required field'
*                     message_v1 = 'wtax_type' ) TO return.
        ENDIF.

    ENDLOOP.
*        ELSE.
*         APPEND VALUE #( type = 'E'
*                     number = '003'
*                     message = 'Withholding Tax Data is Mandatory'
*                     message_v1 = 'withholding_tax' ) TO return.
        ENDIF.
     ELSE.
      APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Payterm methods is required field'
                     message_v1 = 'payment_methods' ) TO return.
     ENDIF.
     ELSE.
      APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Payterm terms is required field'
                     message_v1 = 'payment_terms' ) TO return.
     ENDIF.
     ELSE.
     APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Minority Indicator is required field'
                     message_v1 = 'minority_indicator' ) TO return.
     ENDIF.
      ELSE.
       APPEND VALUE #( type = 'E'
                      number = '003'
                      message = 'House Bank is required field'
                      message_v1 = 'house_bank' ) TO return.
      ENDIF.
      ELSE.
       APPEND VALUE #( type = 'E'
                      number = '003'
                      message = |Reconiliation account is required field|
                      message_v1 = 'reconiliation_acct' ) TO return.
      ENDIF.
      ELSE.
      APPEND VALUE #( type = 'E'
                      number = '003'
                      message = 'Company code data is Mandatory'
                      message_v1 = 'Company_Code' ) TO return.
      ENDIF.
      ENDLOOP.

    ENDIF.
*******************************Purchase Organization data validation***************************************
IF gs_input-purchasing is NOT INITIAL.      "Check existence of Purchase table of vendor
loop at gs_input-purchasing into data(ls_purchase).
IF ls_purchase-purchase_org is NOT INITIAL.
select single from T024E FIELDS ekorg where ekorg = @ls_purchase-purchase_org into @data(is_ekorg).
IF sy-subrc <> 0.
   APPEND VALUE #( type = 'E'
                      number = '003'
                      message = 'Invalid Purchase Org'
                      message_v1 = 'purchase_org' ) TO return.
ENDIF.
IF ls_purchase-payment_terms is NOT INITIAL.
  select single from T052 FIELDS zterm where zterm = @ls_purchase-payment_terms INTO @data(is_zterm).
     IF sy-subrc ne 0.
     APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Payterm terms'
                     message_v1 = 'payment_terms' ) TO return.
       ENDIF.
IF ls_purchase-purchase_grp is NOT INITIAL.
  select single from T024 FIELDS ekgrp where ekgrp = @ls_purchase-purchase_grp into @data(is_ekgrp).
  IF sy-subrc ne 0.
       APPEND VALUE #( type = 'E'
                     number = '003'
                     message = 'Invalid Purchase Group'
                     message_v1 = 'purchase_grp' ) TO return.
       ENDIF.
* IF ls_purchase-planned_delivery_time is INITIAL.
* APPEND VALUE #( type = 'E'
*                     number = '003'
*                     message = 'Planned Delivery Time is Mandatory'
*                     message_v1 = 'planned_delivery_time' ) TO return.
* ENDIF.
    ENDIF.
     ELSE.
 APPEND VALUE #( type = 'E'
         number = '003'
         message = 'Payterm terms is Mandatory'
         message_v1 = 'payment_terms' ) TO return.
      ENDIF.
      ELSE.
 APPEND VALUE #( type = 'E'
          number = '003'
          message = 'Purchasing Org. data is Mandatory'
          message_v1 = 'Company_Code' ) TO return.
     ENDIF.
ENDLOOP.
ENDIF.
    APPEND LINES OF return TO gt_return.

    ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD maintain_tax.
    DATA: ls_central_data TYPE cmds_ei_central_data,
          lt_tax_data     TYPE cmds_ei_tax_ind_t,
          lt_customers    TYPE cmds_ei_extern_t,
          gwa_knvi        TYPE knvi.

    SELECT SINGLE kunnr FROM knvv INTO @DATA(l_kunnr) WHERE kunnr = @ls_customers-header-object_instance-kunnr.
    IF sy-subrc <> 0.
      " Adding tax_data

      CLEAR gwa_knvi.
      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JOIG' .
      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
                        data_key-aland = 'IN'
                        data_key-tatyp = 'JOIG'
                        data-taxkd = '0'
                        datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.

      CLEAR gwa_knvi.
      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JOCG' .

      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
            data_key-aland = 'IN'
            data_key-tatyp = 'JOCG'
            data-taxkd = '0'
            datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.

      CLEAR gwa_knvi.

      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JOSG' .

      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
                        data_key-aland = 'IN'
                        data_key-tatyp = 'JOSG'
                        data-taxkd = '0'
                        datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.


      CLEAR gwa_knvi.
      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JOUG' .

      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
                      data_key-aland = 'IN'
                      data_key-tatyp = 'JOUG'
                      data-taxkd = '1'
                      datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.

      CLEAR gwa_knvi.
      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JTC1' .

      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
                      data_key-aland = 'IN'
                      data_key-tatyp = 'JTC1'
                      data-taxkd =  '0'
                      datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.
      CLEAR gwa_knvi.
      SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                AND aland = 'IN'
                                                AND tatyp = 'JCOS' .

      IF gwa_knvi IS INITIAL.
        APPEND VALUE #( task = 'I'
                      data_key-aland = 'IN'
                      data_key-tatyp = 'JCOS'
                      data-taxkd = '1'
                      datax-taxkd = 'X' ) TO lt_tax_data.
      ENDIF.

      ls_central_data-tax_ind-tax_ind = lt_tax_data.
      ls_customers-central_data = ls_central_data.
      ls_customers-sales_data = ls_sales_data.
      APPEND ls_customers TO lt_customers.
      is_master_data-customers = lt_customers[].
    ENDIF. " IF sy-subrc <> 0
  ENDMETHOD.


  METHOD customer_extnsn.
    DATA es_master_data_correct   TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_correct       TYPE cvis_message. " Error Indicator and System Messages
    DATA es_master_data_defective TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_defective     TYPE cvis_message. " Error Indicator and System Messages


    cmd_ei_api=>initialize( ).
    CALL METHOD cmd_ei_api=>maintain_bapi
      EXPORTING
*       IV_TEST_RUN              = SPACE
*       IV_COLLECT_MESSAGES      = SPACE
        is_master_data           = is_master_data
      IMPORTING
        es_master_data_correct   = es_master_data_correct
        es_message_correct       = es_message_correct
        es_master_data_defective = es_master_data_defective
        es_message_defective     = es_message_defective.
    IF es_message_defective-is_error IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
      DATA(ls_sarea) = VALUE zst_bp_log_output(
                     bu_partner = businesspartner
                     bu_partnerx = 'C'
                     msg = SWITCH #( bukrs WHEN '1000' THEN
                            | Customer Extended to comp code { bukrs } and to Respective sales areas { sorg  } { c_vtweg }|
                            WHEN 'DMS1' THEN
                            | Customer Extended to comp code { bukrs } and sales area { sorg  } { c_vtweg } { c_spart } | )
                     msgtyp = 'S'
                     erdat = sy-datum
                     uzeit = sy-uzeit
                     uname = sy-uname
                     sa_extend = 'X'
                     cc_extend = 'X'
                     msgnr = '002'
                     bpname = ls_kna1-name1 ).
      APPEND ls_sarea TO gt_log.
      UPDATE zsd_bp_creation SET sa_extend = 'X'
                                 cc_extend = 'X'
                                 WHERE bu_partner = businesspartner.
    ELSE.
      LOOP AT es_message_defective-messages INTO DATA(ls_msg).
        DATA(ls_sarea_e) = VALUE zst_bp_log_output(
                  bu_partner = businesspartner
                  bu_partnerx = 'C'
                  msg = ls_msg-message
                  msgtyp = 'E'
                  erdat = sy-datum
                  uzeit = sy-uzeit
                  uname = sy-uname
                  sa_extend = ''
                  cc_extend = ''
                  msgnr = '002'
                  bpname = ls_kna1-name1
                  remarks = |Field:{ ls_msg-field }| ).
        APPEND ls_sarea_e TO gt_log.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD bp_add_roles.
**** Adding role to the Business Partner ***

    DATA businesspartnerrolecategory TYPE bapibus1006_bproles-partnerrolecategory. " BP Role Category
    DATA all_businesspartnerroles    TYPE bapibus1006_x-mark. " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
    DATA businesspartnerrole         TYPE bapibus1006_bproles-partnerrole. " BP Role
    DATA differentiationtypevalue    TYPE bapibus1006_bproles-difftypevalue. " BP: Differentiation type value
    DATA validfromdate               TYPE bapibus1006_bprole_validity-bprolevalidfrom.
    DATA validuntildate              TYPE bapibus1006_bprole_validity-bprolevalidto.
    DATA return                      TYPE STANDARD TABLE OF bapiret2. " Return Parameter
*** Tvarvc variable for Adding roles to BP ****
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
                         WHERE name = 'BP_ROLE_EXTENSION'
                         AND type = 'S'.
    IF sy-subrc = 0.
      LOOP AT lt_tvarv INTO DATA(ls_tvarv).
        businesspartnerrole = SWITCH #( iv_grouping WHEN 'CB01' THEN ls_tvarv-low ELSE ls_tvarv-high ).
        DATA(ls_rltyp) =  businesspartnerrole.
        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner             = businesspartner
            businesspartnerrolecategory = businesspartnerrolecategory
            all_businesspartnerroles    = ' '
            businesspartnerrole         = businesspartnerrole
            differentiationtypevalue    = differentiationtypevalue
*           VALIDFROMDATE               = P_DATE
            validuntildate              = '99991231'
          TABLES
            return                      = return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        DO 5 TIMES.
          SELECT SINGLE rltyp INTO @DATA(lv_rltyp) FROM but100 WHERE partner = @businesspartner
                                                          AND rltyp = @ls_rltyp. " tvarvc type low/high

          IF lv_rltyp IS INITIAL.
            WAIT UP TO 1 SECONDS.  "comment by Roton
          ELSE.
            CONTINUE.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD set_grouping_char.
    DATA:lt_return TYPE TABLE OF bapiret2,
         l_msg     TYPE string.
    DATA(ls_log) = VALUE zsd_bp_creation(  ).
***** Adding Grouping Charactersitics to BP
    DATA(iv_attr) = VALUE  bapi_str_bupa_fs_treasury( group_feature   = ls_group_char  ).
    DATA(iv_attrX) = VALUE  bapi_str_bupa_fs_treasury2_x( group_feature = abap_true ).
    DATA(l_grpfeaturex) = VALUE bapi_str_bupa_fs_treasury2_x( group_feature = abap_true ).
    CLEAR lt_return.
    CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_SET'
      EXPORTING
        businesspartner = businesspartner
        attributes      = iv_attr
        attributesx     = iv_attrX
      TABLES
        return          = lt_return.
    CLEAR l_msg.
    IF line_exists( lt_return[ type = 'E' ] ) OR line_exists( lt_return[ type = 'A' ] ).
      LOOP AT lt_return INTO DATA(lw_return) WHERE ( type = 'E' OR type = 'A' ).
        l_msg = |{ l_msg }/{ lw_return-message }|.
      ENDLOOP.
      ls_log = VALUE zsd_bp_creation( bu_partner = businesspartner
                              bu_partnerx = ''
                              msgtyp = 'E'
                              msg = l_msg
                              uzeit = sy-uzeit
                              erdat = sy-datum
                              uname = sy-uname
                              msgnr = '001'
                              bpname = ls_group_char  ).
      APPEND ls_log TO gt_log .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDMETHOD.
  METHOD update_pan.
    CALL FUNCTION 'ENQUEUE_EXKNA1'
      EXPORTING
        kunnr          = ls_kna1-kunnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      ls_kna1-j_1ipanno = iv_input-generaldata-pan_no .
*      ls_kna1-name3 = iv_input-generaldata-name-name1.
      MODIFY kna1 FROM ls_kna1.
    ENDIF.
*         release table entry
    CALL FUNCTION 'DEQUEUE_EXKNA1'
      EXPORTING
        kunnr = ls_kna1-kunnr.
  ENDMETHOD.

  METHOD bp_maintain_customer.
    DATA: it_data         TYPE cvis_ei_extern_t,
          is_data         TYPE  cvis_ei_extern,
          is_partner      TYPE bus_ei_extern,
          bp_control      TYPE bus_ei_bupa_central_main,
          bp_centraldata  TYPE bus_ei_struc_central,
          bp_organization TYPE bus_ei_struc_central_organ,
          ls_common       TYPE bus_ei_bupa_central,
          ls_role         TYPE bus_ei_roles,
          it_roles        TYPE bus_ei_bupa_roles_t,
          is_customer     TYPE cmds_ei_extern,
          i_data          TYPE CVIS_EI_EXTERN_T,
          gs_resp TYPE zst_bp_output_logs.
   DATA:  ensure_create type cvi_ensure_create.

 " Validation of inputs
    DATA(gt_return) =  me->validations( iv_accnt_type =  iv_accnt_type  ls_input =  ls_input  ).

    IF gt_return IS NOT INITIAL.
      DATA(iv_count) = lines( gt_return ).
      DATA(lv_rem) = VALUE text300(  ).
      IF iv_count > 1 .
        LOOP AT gt_return INTO DATA(is_ret).
*          lv_rem = is_ret-message_v1.
*          lv_rem = | { lv_rem }, { is_ret-message } |.
           lv_rem = |{ is_ret-message } , { lv_rem }|.
        ENDLOOP.
      ELSE.
        lv_rem = VALUE #( gt_return[ 1 ]-message OPTIONAL ).
      ENDIF.
      DATA(ls_alverr) = VALUE zst_bp_output_logs( msgtyp = 'E'
                                               bpname = ls_input-generaldata-name-name1
                                               msg = |{ 'Error in Inputs' } { ls_input-generaldata-searchterm1 } |
                                               account_type = 'Customer'
                                               remarks = lv_rem
                                                ).
      APPEND ls_alverr TO gt_log.
     ELSE.
**** BP Control Data
    bp_control = VALUE #( category = c_bp_cat   grouping = ls_input-generaldata-grouping ).
****BP Central Data
    SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @ls_input-generaldata-title AND langu = 'E'.
    bp_centraldata = VALUE #( searchterm1 = ls_input-generaldata-Name-name1+0(20)
                                title_key = lv_title  ).
****BP Person Data
    bp_organization = VALUE #( name1 = ls_input-generaldata-name-name1
                               name2 = ls_input-generaldata-name-name2
                               name3 = ls_input-generaldata-name-name3
                               name4 = ls_input-generaldata-name-name4

                          ) .
****Common data for BP type filled
    ls_common = VALUE #( data = VALUE #( bp_control = bp_control
                                         bp_centraldata = bp_centraldata
                                         bp_organization = bp_organization )
                      datax = VALUE #( bp_centraldata = VALUE #( searchterm1 = abap_true
                                                                 title_key = abap_true )
                                       bp_organization = VALUE #(  name1 = abap_true
                                                                   name2 = abap_true
                                                                   name3 = abap_true
                                                                   name4 = abap_true ) ) ).
*****Ensuring BP for Customer creation
   ensure_create = VALUE #( create_customer = abap_true ).
*****Get role Data for BP
    me->cvi_roles(
      EXPORTING
        iv_grouping = ls_input-generaldata-grouping
        ensure_create = ensure_create
      IMPORTING
        it_roles    = it_roles
    ).
*****Assign the populated roles to role structure
    DATA(roles) = VALUE bus_ei_roles( roles   = it_roles  ).

*****Get Address data for BP and Assigned
data(is_input) = VALUE zst_bp_addr_comm_data( addressdata = ls_input-generaldata-addressdata
                                              communication = ls_input-generaldata-communication ).
    me->cvi_address(
      EXPORTING
        gs_input   = is_input
      IMPORTING
        iv_address = DATA(address)
    ).

*****Get Taxnumber detail and Assigned
    me->cvi_taxnumber(
      EXPORTING
        taxnumber = ls_input-generaldata-taxno
      IMPORTING
        iv_tax    = DATA(taxnumber)
    ).

    """""""CENTRAL DATA ASSIGNED""""""""""""
    DATA(Central_data) = VALUE bus_ei_central_data(
        common            = ls_common
        role              = roles
        taxnumber         = taxnumber
*        communication     =
        address           = address
    ).

    """""""HEADER FOR BUSINESS PARTNER DATA""""""""""""
    me->cvi_bp_header(
      IMPORTING
        header   = DATA(Header)
        cx_error = DATA(cx_error)
    ).

    IF NOT cx_error IS INITIAL.
      APPEND VALUE #(         msg  =  cx_error
                              msgtyp = 'E'
                              msgnr = '002' ) TO gt_log.
      EXIT.
    ENDIF.

*****Assign header and central data to Partner structure
    is_partner = VALUE #( header = Header central_data = central_data ).

*****Customer Creation and Extension to company code and sales area/areas
    me->cvi_customer(
      EXPORTING
        gs_input    = ls_input
      IMPORTING
        iv_customer = is_customer
        iv_bukrs    = DATA(is_bukrs)
        iv_vkorg    = DATA(is_vkorg)
    ).

    APPEND VALUE #( partner = is_partner
                    customer = is_customer
                    ensure_create-create_customer = abap_true ) TO i_data.
************Passing Grouping Char in ABAP Memory to consume in enhancement ZGROUP_CHAR_SET
   data(ls_grpchar) = VALUE bp_group_feature( ).
   ls_grpchar = ls_input-generaldata-grouping_char.

 EXPORT ls_grpchar = ls_grpchar to MEMORY ID 'GRP'.
    ""Executing class
    cl_md_bp_maintain=>maintain(
      EXPORTING
        i_data     = i_data
*      i_test_run =
      IMPORTING
        e_return   = DATA(lt_return)
    ).
    ENDIF.

    IF lt_return IS NOT INITIAL.
      DATA(return_tab) = VALUE #( lt_return[ 1 ]-object_msg OPTIONAL ).
      IF line_exists( return_tab[ type = 'E' ] ) or line_exists( return_tab[ type = 'A' ] ).
        LOOP AT return_tab INTO DATA(is_return) WHERE type NE 'W'.
          gs_resp = VALUE #( msgtyp = is_return-type
                                                   msg = is_return-message
                                                   msgnr = '002' ).
          APPEND gs_resp TO gt_log.
        ENDLOOP.
      ELSE.
        DATA(iv_partnerid) = VALUE bu_partner( ).
        IMPORT iv_partnerid = iv_partnerid FROM MEMORY ID 'PID'.
*---------Commit the SAP LUW in Single DB LUW----------*
       CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*-------------------------------------------------------*
*        DATA(ls_msg) = SWITCH #( is_bukrs WHEN '1000' THEN
*           | Customer Extended to comp code { is_bukrs } and to Respective sales areas { is_vkorg  } { c_vtweg }|
**          ELSE WHEN 'DMS1' THEN
*           | Customer Extended to comp code { is_bukrs } and sales area { is_vkorg  } { c_vtweg } { c_spart } | ).
          DATA(ls_msg) =  | Customer Extended to comp code { is_bukrs } and to Respective sales areas { is_vkorg  } { c_vtweg }|.
        gs_resp = VALUE #( bu_partner = iv_partnerid
                                           msgtyp = 'S'
                                           msg = 'Business Partner Created Successfully'
                                           msgnr = '001'
                                           bpname = ls_input-generaldata-name-name1
                                           account_type = 'Customer'
                                           customer = VALUE #( customer = iv_partnerid
                                                               msg = ls_msg
                                                               cc_extend = abap_true
                                                               sa_extend = abap_true
                                                                    ) ).
        APPEND gs_resp TO gt_log.
        CLEAR gs_resp.

        DATA(ls_log) = VALUE zsd_bp_creation( bu_partner = iv_partnerid
                                                bu_partnerx = 'B'
                                                msgtyp = 'S'
                                                msg = 'Business Partner Created Successfully'
                                                uzeit = sy-uzeit
                                                erdat = sy-datum
                                                uname = sy-uname
                                                msgnr = '001'
                                                bpname = ls_input-generaldata-name-name1
                                                sortl = ls_input-generaldata-name-name1 ).
       MODIFY zsd_bp_creation FROM ls_log.
        commit work.
      ENDIF.
    ENDIF.
    IF ls_input-companycode-cust_type = '11' AND ls_input-generaldata-pan_no IS NOT INITIAL.
      SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @iv_partnerid.
      me->update_pan(
        EXPORTING
          ls_kna1  = ls_kna1
          iv_input = ls_input
      ).
    ENDIF.
  ENDMETHOD.

  METHOD cvi_roles.
    DATA: ls_roles TYPE bus_ei_bupa_roles,
          role_key TYPE bu_role,
          lt_tb003 TYPE STANDARD TABLE OF tb003,
          lt_tvarv type table of tvarvc.

REFRESH lt_tvarv[].
IF  ensure_create-create_customer = abap_true.
*** Tvarvc variable for Adding roles to BP Customer****
    SELECT * FROM tvarvc INTO TABLE lt_tvarv
                         WHERE name = 'BP_ROLE_EXTENSION'
                         AND type = 'S'
                         and clie_indep = ' ' .
ELSEIF  ensure_create-create_vendor = abap_true.
*** Tvarvc variable for Adding roles to BP Vendor****
    SELECT * FROM tvarvc INTO TABLE lt_tvarv
                         WHERE name = 'BP_ROLE_EXTENSION'
                         AND type = 'S'
                         AND clie_indep = 'A'.
ENDIF.
    IF sy-subrc = 0.
      CASE iv_grouping.
        WHEN 'CB01' OR 'VB01'.
          SELECT  FROM tb003
           FIELDS *
           FOR ALL ENTRIES IN @lt_tvarv
           WHERE role = @lt_tvarv-low+0(6)
           INTO TABLE @lt_tb003.
        WHEN 'CB02'.
          SELECT  FROM tb003
           FIELDS *
           FOR ALL ENTRIES IN @lt_tvarv
           WHERE role = @lt_tvarv-high+0(6)
           INTO TABLE @lt_tb003.

      ENDCASE.
*    IF iv_grouping = 'CB01'.
*      SELECT  FROM tb003
*      FIELDS *
*      FOR ALL ENTRIES IN @lt_tvarv
*      WHERE role = @lt_tvarv-low+0(6)
*      INTO TABLE @lt_tb003.
*      ELSEIF iv_grouping = 'CB02'.
*       SELECT  FROM tb003
*      FIELDS *
*      FOR ALL ENTRIES IN @lt_tvarv
*      WHERE role = @lt_tvarv-high+0(6)
*      INTO TABLE @lt_tb003.
*      ENDIF.
      IF sy-subrc = 0.
        LOOP AT lt_tvarv INTO DATA(ls_tvarv).
          role_key = SWITCH #( iv_grouping WHEN 'CB01' OR 'VB01' THEN ls_tvarv-low WHEN 'CB02' THEN ls_tvarv-high  ).
          READ TABLE lt_tb003 INTO DATA(ls_tb003) WITH KEY role = role_key.
          IF sy-subrc = 0.
            DATA(role_data) = VALUE bus_ei_role_data(
*              difftypevalue =
                rolecategory  = ls_tb003-rolecategory
                valid_from    = sy-datum
                valid_to      = '99991231'
            ).
            DATA(role_datax) = VALUE bus_ei_role_data_x(
                valid_from = abap_true
                valid_to   = abap_true
            ).
            "I = Insert  "D = Delete
            ls_roles = VALUE #( task = 'I' data_key = role_key data = role_data datax = role_datax ).
            APPEND ls_roles TO it_roles.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD cvi_address.
*    DATA: lt_addr type bus_ei_bupa_address_t.
*        BUS_EI_BUPA_ADDRESS
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Address data
    DATA(postal) = VALUE bus_ei_bupa_postal_address(
        data  = VALUE #( street = gs_input-addressdata-street
                         str_suppl1 = gs_input-addressdata-addr1
                         str_suppl2 = gs_input-addressdata-addr2
                         str_suppl3 = gs_input-addressdata-addr3
                         house_no = gs_input-addressdata-house_num
                         district = gs_input-addressdata-district
                         postl_cod1 = gs_input-addressdata-post_code1
                         city = gs_input-addressdata-city
                         country = c_cntry
                         region = gs_input-addressdata-region
                         langu = c_langu  )
        datax = VALUE #( street     = abap_true
                         str_suppl1 = abap_true
                         str_suppl2 = abap_true
                         str_suppl3 = abap_true
                         house_no   = abap_true
                         district   = abap_true
                         postl_cod1 = abap_true
                         city       = abap_true
                         country    = abap_true
                         region     = abap_true
                         langu      = abap_true
                         ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Phone communication data
    DATA(phone) = VALUE bus_ei_bupa_telephone_t(
     (  contact    = VALUE #(   task ='I'
                                data = VALUE #(  country = c_cntry
                                 telephone = gs_input-communication-landline
                                 std_no = 'X'
                                 r_3_user = '3'
                                 home_flag = 'X' )
                               datax = VALUE #(  country = abap_true
                                               telephone  = abap_true
                                               std_no     = abap_true
                                               r_3_user   = abap_true
                                               home_flag  = abap_true   )  ) )
     ( contact     = VALUE #(   task ='I'
                                data = VALUE #( country = c_cntry
                                 telephone =  gs_input-communication-mob_number
                                 std_no = 'X'
                                 r_3_user = '1'
                                 home_flag = 'X' )
                          datax = VALUE #(  country = abap_true
                                           telephone  = abap_true
                                           std_no     = abap_true
                                           r_3_user   = abap_true
                                           home_flag  = abap_true  )  ) )  ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Email Communication Data
    DATA(smtp) = VALUE bus_ei_bupa_smtp_t(
        ( contact = VALUE #( task = 'I'
                             data = VALUE #( e_mail =  gs_input-communication-smtp_addr  "Email
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' )
                             datax = VALUE #( e_mail    = abap_true
                                             std_no     = abap_true
                                             std_recip  = abap_true
                                             home_flag  = abap_true
                                             consnumber = abap_true )  ) )  ).

****Overall Communiation Structure( included phone and email )
    DATA(communication) = VALUE bus_ei_communication(
        phone          = VALUE #( phone = phone )
        smtp           = VALUE #( smtp = smtp )
    ).

**********************************************************************Overall Address Data
    DATA(lt_addr) = VALUE bus_ei_bupa_address_t( (
        task            = 'I'
        data            = VALUE #( postal = postal communication = communication )
    ) ).

    """""Finally passing to adddress structure"""""
    iv_address = VALUE #( addresses = lt_addr ).
  ENDMETHOD.

  METHOD cvi_taxnumber.

    DATA(Taxnumbers) = VALUE bus_ei_bupa_taxnumber_t(
       (  task     = 'I'
        data_key = VALUE #( taxtype = c_taxtyp
                            taxnumber = taxnumber  )  )  ).

    iv_tax = VALUE #( taxnumbers = Taxnumbers ).
  ENDMETHOD.

  METHOD cvi_bp_header.

    me->create_guid(
      IMPORTING
        v_guid    = DATA(lv_guid)
        v_err_txt = DATA(ls_err_txt)
    ).
    ""When Error in GUID creation
    IF ls_err_txt IS NOT INITIAL.
      cx_error   =   ls_err_txt.
      EXIT.
    ENDIF.

    header = VALUE #( object_task = 'I'
                      object_instance-bpartnerguid = lv_guid ).
  ENDMETHOD.

  METHOD create_guid.
    TRY.
        v_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
      CATCH cx_uuid_error INTO DATA(r_uuid_exc).
        v_err_txt = r_uuid_exc->get_text( ).
*    MESSAGE r_uuid_exc->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD cvi_customer.

*    DATA: is_bukrs TYPE bukrs.
    DATA: "is_sorg     TYPE vkorg,
      sa_data_key TYPE cmds_ei_sales_key,
      sa_data     TYPE cmds_ei_sales_data,
      sa_datax    TYPE cmds_ei_sales_datax,
      lt_functions TYPE cmds_ei_functions_t,
      sa_functions TYPE cmds_ei_cmd_functions,
      lt_sales TYPE cmds_ei_sales_t,
      tax_ind TYPE cmds_ei_tax_ind_t,
      cust_addr TYPE cvis_ei_address1,
      cust_central TYPE cmds_ei_cmd_central,
      cust_centrl_data TYPE cmds_ei_central_data,
      ensure_create type cvi_ensure_create,
      ls_header TYPE cmds_ei_header.


    ensure_create = VALUE #( create_customer = abap_true ).
  "" Company Code extension
*****Fetching  Company Code and Sales oRg data
  SELECT  FROM zcusttype_extnsn FIELDS * WHERE act_type = 'D' AND custtype = @gs_input-companycode-cust_type INTO TABLE @DATA(lt_cust).
  IF sy-subrc = 0.
*****Header Company data
    ls_header = VALUE #(  object_instance-kunnr = ' ' object_task = 'I' )."Insert
***************************************************************************************************
    "Company Extension data
    iv_bukrs = lt_cust[ 1 ]-ccode.
    DATA(lt_company) = VALUE cmds_ei_company_t(
                          ( task      = 'I'
                            data_key  = VALUE #( bukrs = iv_bukrs  )
                            data      = VALUE #( akont = lt_cust[ 1 ]-akont  )
                            datax     = VALUE #( akont = abap_true ) ) ).

***************************************************************************************************
    "Sales area Exension data
    ensure_create = VALUE cvi_ensure_create( create_customer = abap_true ).
*****Fetching Mapping field based on Grouping Char
    SELECT SINGLE FROM zmap_table_bp
    FIELDS mapp_field
    WHERE purpose = 'GROUPCHAR_TO_CUSTGRP' AND
          subs_field = @gs_input-generaldata-grouping_char
    INTO @DATA(lv_cust_grp).
    IF sy-subrc = 0.

    ENDIF.
*****Fetching Sales District based on region
    SELECT SINGLE FROM zmap_table_bp
    FIELDS mapp_field
    WHERE purpose = 'REGIO_TO_BZIRK' AND
          subs_field = @gs_input-generaldata-addressdata-region
    INTO @DATA(iv_bzirk).
*****Fetching Salesorg based on Customer type
 IF gs_input-salesorg-ktonr is NOT INITIAL ." lt_cust[ 1 ]-ccode <> '1000'.
        DATA(gs_kotnr) = VALUE kunnr(  ).
        UNPACK gs_input-salesorg-ktonr TO gs_kotnr.
        SELECT SINGLE FROM kna1
        FIELDS werks
        WHERE kunnr = @gs_kotnr
        INTO @DATA(iv_vwerk).
        ELSE.
        SELECT FROM zbp_soffice_tabl
        FIELDS *
        FOR ALL ENTRIES IN @lt_cust
        WHERE sales_org = @lt_cust-vkorg and region = @gs_input-generaldata-addressdata-region
         INTO TABLE @DATA(gt_soffice).
ENDIF.

LOOP AT lt_cust INTO DATA(ls_cust).
    iv_vkorg = ls_cust-vkorg.
        "Sales Area details
        sa_data_key = VALUE #( vkorg = ls_cust-vkorg vtweg = ls_cust-dist_channel spart = ls_cust-divison ).

        sa_data = VALUE #(   bzirk = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN iv_bzirk ELSE ' ' )
                             kdgrp = CONV #( lv_cust_grp )
                             ktgrd = c_ktgrd
                             vkbur = COND #( WHEN iv_vwerk is not INITIAL THEN iv_vwerk ELSE VALUE #( gt_soffice[ divison = ls_cust-divison ]-soffice OPTIONAL  )  )
                             vkgrp = ls_cust-divison  " as said division and sales group are equal
                             kalks = c_kalks
                             inco1 = c_inco1
                             inco2_l = gs_input-generaldata-addressdata-city
                             vwerk = COND #( WHEN iv_vwerk is not INITIAL THEN iv_vwerk ELSE VALUE #( gt_soffice[ divison = ls_cust-divison ]-soffice OPTIONAL  ) )
                             vsbed = '01'
                             zterm = SWITCH #( ls_cust-divison WHEN '10' THEN 'NT07' ELSE 'NT30' )
                             lprio = c_lprio
                             waers = 'INR'
                             konda = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN '01' ELSE '' )
                             pltyp = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN '01' ELSE '' )
                             kabss = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN '0001' ELSE ' '  )
                             kkber = SWITCH #( gs_input-generaldata-grouping WHEN 'CB02' THEN '1000' ELSE ' '  )  ).

        sa_datax = VALUE #( bzirk   =  abap_true
                            kdgrp   =  abap_true
                            ktgrd   =  abap_true
                            vkbur   =  abap_true
                            vkgrp   =  abap_true
                            kalks   =  abap_true
                            inco1   =  abap_true
                            inco2_l =  abap_true
                            vwerk   =  abap_true
                            vsbed   =  abap_true
                            zterm   =  abap_true
                            lprio   =  abap_true
                            waers   =  abap_true
                            konda   =  abap_true
                            pltyp   =  abap_true
                            kabss   =  abap_true
                            kkber   =  abap_true
                               ).
        ""Partner Function Details
        lt_functions = VALUE #(
          (  task    = 'I' data_key-parvw = 'L5' data-partner   = '0009003004'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'ZS' data-partner   = '0009003004'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'ZC' data-partner   = '0000700537'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'ZA' data-partner   = '0000701955'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'ZH' data-partner   = '0000701405'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'L1' data-partner   = '0009005599'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'L2' data-partner   = '0009003003'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'L3' data-partner   = '0009003002'  datax-partner  =  abap_true   )
          (  task    = 'I' data_key-parvw = 'SK' data-partner   = gs_kotnr      datax-partner  =  abap_true   )
           ).

        sa_functions = VALUE #( functions = lt_functions ).


       APPEND VALUE #(  task      = 'I'
                        data_key  = sa_data_key
                        data      = sa_data
                        datax     = sa_datax
                        functions = sa_functions ) to lt_sales.

ENDLOOP.


  ""Tax indicators Detail
        tax_ind = VALUE #(
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOIG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOCG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOSG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOUG' ) data-taxkd  = '1' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JTC1' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JCOS' ) data-taxkd  = '1' datax-taxkd = abap_true )  ).

        ""Customer Address
        cust_addr = VALUE #(
            task          = 'U'
            postal        = VALUE #( data-name  = gs_input-generaldata-name-name1 data-country = c_cntry
                                     datax-name = abap_true  datax-country = abap_true ) ).
        ""Customer Central Data
        cust_central = VALUE #(
            data  = VALUE #( stcd3  = gs_input-generaldata-taxno
                             katr1  = gs_input-generaldata-attributes-attr1
                             katr2  = gs_input-generaldata-attributes-attr2
                             katr3  = gs_input-generaldata-attributes-attr3
                             katr4  = gs_input-generaldata-attributes-attr4
                             katr5  = gs_input-generaldata-attributes-attr5
                             katr6  = gs_input-generaldata-attributes-attr6
                             katr7  = gs_input-generaldata-attributes-attr7
                             katr8  = gs_input-generaldata-attributes-attr8
                             katr9  = gs_input-generaldata-attributes-attr9
                             katr10 = gs_input-generaldata-attributes-attr10 )
            datax = VALUE #( stcd3  = abap_true
                             katr1  = abap_true
                             katr2  = abap_true
                             katr3  = abap_true
                             katr4  = abap_true
                             katr5  = abap_true
                             katr6  = abap_true
                             katr7  = abap_true
                             katr8  = abap_true
                             katr9  = abap_true
                             katr10 = abap_true ) ).

        cust_centrl_data = VALUE #( central  = cust_central address = cust_addr tax_ind-tax_ind = tax_ind ).

  ""Final Customer Company and Sales area Extension Structure
   ensure_create = VALUE cvi_ensure_create( create_customer = abap_true ).
        iv_customer = VALUE #( header       = ls_header
                               central_data = cust_centrl_data
                               company_data = VALUE #( company = lt_company )
                               sales_data   = VALUE #( sales = lt_sales )   ) .
ENDIF.
  ENDMETHOD.

  METHOD bp_maintain_vendor.
      DATA: it_data         TYPE cvis_ei_extern_t,
          is_data         TYPE  cvis_ei_extern,
          is_partner      TYPE bus_ei_extern,
          bp_control      TYPE bus_ei_bupa_central_main,
          bp_centraldata  TYPE bus_ei_struc_central,
          bp_organization TYPE bus_ei_struc_central_organ,
          ls_common       TYPE bus_ei_bupa_central,
          ls_role         TYPE bus_ei_roles,
          it_roles        TYPE bus_ei_bupa_roles_t,
          is_vendor       TYPE vmds_ei_extern,
          i_data          TYPE CVIS_EI_EXTERN_T,
          gs_resp TYPE zst_bp_output_logs.
   DATA: ensure_create type cvi_ensure_create,
         lt_bank_data type BUS_EI_BUPA_BANKDETAIL_T.

      " Validation of inputs
    DATA(gt_return) =  me->validations( iv_accnt_type = iv_accnt_type  gs_input =  ls_input  ).
     IF gt_return IS NOT INITIAL.
      DATA(iv_count) = lines( gt_return ).
      DATA(lv_rem) = VALUE text300(  ).
      IF iv_count > 1 .
        LOOP AT gt_return INTO DATA(is_ret).
*          lv_rem = is_ret-message_v1.
*          lv_rem = | { lv_rem }, { is_ret-message } |.
           lv_rem = |{ is_ret-message } , { lv_rem }|.
        ENDLOOP.
      ELSE.
        lv_rem = VALUE #( gt_return[ 1 ]-message OPTIONAL ).
      ENDIF.
      DATA(ls_alverr) = VALUE zst_bp_output_logs( msgtyp = 'E'
                                               bpname = ls_input-generaldata-name-name1
                                               msg = |{ 'Error in Inputs' } { ls_input-generaldata-searchterm1 } |
                                               account_type = 'Vendor'
                                               remarks = lv_rem
                                                ).
      APPEND ls_alverr TO gt_log.
      ELSE.
**** BP Control Data
    bp_control = VALUE #( category = c_bp_cat   grouping = ls_input-generaldata-grouping ).
****BP Central Data
    SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @ls_input-generaldata-title AND langu = 'E'.
    bp_centraldata = VALUE #( searchterm1 = ls_input-generaldata-Name-name1+0(20)
                              searchterm2 = ls_input-generaldata-name-name1+20(20)
                                title_key = lv_title  ).
****BP Person Data
    bp_organization = VALUE #( name1 = ls_input-generaldata-name-name1
                               name2 = ls_input-generaldata-name-name2
                               name3 = ls_input-generaldata-name-name3
                               name4 = ls_input-generaldata-name-name4

                          ) .
****Common data for BP type filled
    ls_common = VALUE #( data = VALUE #( bp_control = bp_control
                                         bp_centraldata = bp_centraldata
                                         bp_organization = bp_organization )
                      datax = VALUE #( bp_centraldata = VALUE #( searchterm1 = abap_true
                                                                 searchterm2 = abap_true
                                                                 title_key = abap_true )
                                       bp_organization = VALUE #(  name1 = abap_true
                                                                   name2 = abap_true
                                                                   name3 = abap_true
                                                                   name4 = abap_true ) ) ).
*****Ensuring BP for Vendor creation
    ensure_create = VALUE #( create_vendor = abap_true ).
*****Get role Data for BP
    me->cvi_roles(
      EXPORTING
        iv_grouping = ls_input-generaldata-grouping
        ensure_create = ensure_create
      IMPORTING
        it_roles    = it_roles
    ).
*****Assign the populated roles to role structure
    DATA(roles) = VALUE bus_ei_roles( roles   = it_roles  ).
*****Get Address data for BP and Assigned
data(is_input) = VALUE zst_bp_addr_comm_data( addressdata = ls_input-generaldata-addressdata
                                              communication = ls_input-generaldata-communication ).
    me->cvi_address(
      EXPORTING
        gs_input   = is_input
      IMPORTING
        iv_address = DATA(address)
    ).

*****Get Bank details and Assigned
   me->vi_bank_details(
     EXPORTING
       gt_bank    = ls_input-generaldata-payment_transaction
       name1      = ls_input-generaldata-name-name1
     IMPORTING
       bankdetail = DATA(Bank_Details)
   ).

*****Get Taxnumber detail and Assigned
    me->cvi_taxnumber(
      EXPORTING
        taxnumber = ls_input-generaldata-taxno
      IMPORTING
        iv_tax    = DATA(taxnumber)
    ).

"""""""CENTRAL DATA ASSIGNED""""""""""""
    DATA(Central_data) = VALUE bus_ei_central_data(
        common            = ls_common
        role              = roles
        bankdetail       = bank_details
        taxnumber         = taxnumber
*        communication     =
        address           = address
    ).

"""""""HEADER FOR BUSINESS PARTNER DATA""""""""""""
    me->cvi_bp_header(
      IMPORTING
        header   = DATA(Header)
        cx_error = DATA(cx_error)
    ).

    IF NOT cx_error IS INITIAL.
      APPEND VALUE #(         msg  =  cx_error
                              msgtyp = 'E'
                              msgnr = '002' ) TO gt_log.
      EXIT.
    ENDIF.

*****Assign header and central data to Partner structure
    is_partner = VALUE #( header = Header central_data = central_data ).

*****Vendor Creation and Extension to company code and Purchase organizations
    me->cvi_vendor(
      EXPORTING
        iv_input  = ls_input
      IMPORTING
        iv_vendor = is_vendor
    ).
     APPEND VALUE #( partner = is_partner
                 vendor = is_vendor
                 ensure_create = VALUE #( create_vendor = abap_true )  ) TO i_data.
************Passing Grouping Char in ABAP Memory to consume in enhancement ZGROUP_CHAR_SET
   data(ls_grpchar) = VALUE bp_group_feature( ).
   ls_grpchar = ls_input-generaldata-grouping_char.

 EXPORT ls_grpchar = ls_grpchar to MEMORY ID 'GRP'.

    ""Executing class
    cl_md_bp_maintain=>maintain(
      EXPORTING
        i_data     = i_data
*      i_test_run =
      IMPORTING
        e_return   = DATA(lt_return)
    ).

  IF lt_return IS NOT INITIAL.
      DATA(return_tab) = VALUE #( lt_return[ 1 ]-object_msg OPTIONAL ).
      IF line_exists( return_tab[ type = 'E' ] ) or line_exists( return_tab[ type = 'A' ] ).
        LOOP AT return_tab INTO DATA(is_return) WHERE type NE 'W'.
          gs_resp = VALUE #( msgtyp = is_return-type
                                                   msg = is_return-message
                                                   msgnr = '002' ).
          APPEND gs_resp TO gt_log.
        ENDLOOP.
      ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        DATA(iv_partnerid) = VALUE bu_partner( ).
        IMPORT iv_partnerid = iv_partnerid FROM MEMORY ID 'PID'.
*------------------Get Lifnr from updated table A_businesspartner-----------------*
Select single from A_BusinessPartner FIELDS Supplier where BusinessPartner = @iv_partnerid into @data(iv_supplier).
        gs_resp = VALUE #( bu_partner = iv_partnerid
                                           msgtyp = 'S'
                                           msg = 'Business Partner Created Successfully'
                                           msgnr = '002'
                                           bpname = ls_input-generaldata-name-name1
                                           account_type = 'Vendor'
                                           vendor = VALUE #(  vendor = iv_supplier
                                                               msg = | Vendor Extended to respective company codes and Purchase organizations |
                                                               cc_extend = abap_true
                                                               porg_extend = abap_true
                                                                    ) ).
        APPEND gs_resp TO gt_log.
        CLEAR gs_resp.

        DATA(ls_log) = VALUE zsd_bp_creation( bu_partner = iv_partnerid
                                                bu_partnerx = 'B'
                                                msgtyp = 'S'
                                                msg = 'Business Partner Created Successfully'
                                                uzeit = sy-uzeit
                                                erdat = sy-datum
                                                uname = sy-uname
                                                msgnr = '002'
                                                bpname = ls_input-generaldata-name-name1
                                                sortl = ls_input-generaldata-name-name1 ).
       MODIFY zsd_bp_creation FROM ls_log.
        Commit work.
      ENDIF.
    ENDIF.

  ENDIF.
  ENDMETHOD.

  METHOD vi_bank_details.
  DATA: iv_key TYPE numc4,
        lt_bupa_bank type bus_ei_bupa_bankdetail_t.
*****Fetch the Bank User details from BNKA
*Select From BNKA FIELDS *
*FOR ALL ENTRIES IN @gt_bank
* where banks = @c_cntry and bankl = @gt_bank-bank_key
* INTO TABLE @data(lt_bnka).
  LOOP AT  gt_bank ASSIGNING FIELD-SYMBOL(<fs_data>).
    ADD 1 TO iv_key.
*   data(ls_bnka) = VALUE #( lt_bnka[ bankl = <fs_data>-bank_key ] OPTIONAL ).
    DATA(ls_bupa_bank) = VALUE bus_ei_bupa_bankdetail(
        task            = 'I'                          "I =Insert
        data_key        = CONV #( iv_key )
        data            = VALUE #(
        bank_ctry           = c_cntry
        bank_key            = <fs_data>-bank_key
        bank_acct           = <fs_data>-acc_number
        ctrl_key            = <fs_data>-control_key
        accountholder       =  name1
*        bankaccountname     = ls_bnka-banka
        bankdetailvalidfrom = sy-datum
        bankdetailvalidto   = '99991231'
    )
        datax           = VALUE #(
        bank_ctry           = abap_true
        bank_key            = abap_true
        bank_acct           = abap_true
        ctrl_key            = abap_true
        accountholder       = abap_true
*        bankaccountname     = abap_true
        bankdetailvalidfrom = abap_true
        bankdetailvalidto   = abap_true
    )
    ).
    APPEND ls_bupa_bank TO lt_bupa_bank.
  ENDLOOP.

  bankdetail = VALUE #( bankdetails    = lt_bupa_bank ).
  ENDMETHOD.

  METHOD cvi_vendor.
    DATA: is_bukrs TYPE bukrs, is_ekorg type ekorg.
    DATA: ls_header TYPE vmds_ei_header,
          vend_central type vmds_ei_vmd_central,
          vend_addr TYPE cvis_ei_address1,
          tax_ind TYPE cmds_ei_tax_ind_t,
          lt_banks type cvis_ei_bankdetail_t,
          vend_centrl_data type vmds_ei_central_data,
          lt_whtax type vmds_ei_wtax_type_t,
          lt_company type vmds_ei_company_t,
          lt_purchasing type vmds_ei_purchasing_t.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Other Mappings beyond ccode and ekorg
*****Header data
    ls_header = VALUE #(  object_instance-lifnr = ' ' object_task = 'I' ).    "Insert
**central data
vend_central = VALUE #(  data = VALUE #( stcd3 =  iv_input-generaldata-taxno ven_class = iv_input-generaldata-gst_vendor_classification
                                        j_1ipanno = iv_input-generaldata-pan_no   j_1ipanref = iv_input-generaldata-pan_no
                                        ktokk = iv_input-generaldata-account_group )
                         datax = VALUE #( stcd3 = abap_true ven_class = abap_true j_1ipanno = abap_true j_1ipanref = abap_true
                                          ktokk = abap_true )  ).
**Address central
 vend_addr = VALUE #(
            task          = 'U'
            postal        = VALUE #( data-name  = iv_input-generaldata-name-name1 data-country = c_cntry
                                     datax-name = abap_true  datax-country = abap_true ) ).
**Tax indicators Detail
   tax_ind = VALUE #(
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOIG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOCG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOSG' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JOUG' ) data-taxkd  = '1' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JTC1' ) data-taxkd  = '0' datax-taxkd = abap_true )
         (  task        = 'I' data_key    =  VALUE #( aland = c_cntry tatyp = 'JCOS' ) data-taxkd  = '1' datax-taxkd = abap_true )  ).

**Bank data
Loop at iv_input-generaldata-payment_transaction into data(ls_bank).
DATA(bankdetail) = VALUE cvis_ei_cvi_bankdetail(
    task     = 'U'
    data_key = VALUE #(
    banks = c_cntry
    bankl = ls_bank-bank_key
    bankn = ls_bank-acc_number )
    data     = VALUE #( bkont  = ls_bank-control_key )
    datax    = VALUE #( bkont  =  abap_true )
).
Append bankdetail to lt_banks.
ENDLOOP.
data(bank_detail) = VALUE cvis_ei_bankdetail( bankdetails = lt_banks ).
""""Correlating to central structure vendor
vend_centrl_data = VALUE vmds_ei_central_data( central  = vend_central address = vend_addr bankdetail = bank_detail ).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Company code Data
LOOP AT iv_input-companycode into data(ls_company).
    DATA(lv_akont) =  VALUE akont( ).
*****Fetching Mapping field based on Company Code
    SELECT SINGLE FROM zmap_table_bp
    FIELDS mapp_field
    WHERE purpose = 'CCODE_TO_RECONACCT' AND
          subs_field = @ls_company-company_code
    INTO  @DATA(ls_reco_acnt).
    IF sy-subrc = 0.
      UNPACK ls_reco_acnt TO lv_akont.
    ENDIF.
""""Withhold tax data table
 LOOP AT ls_company-withholding_tax into data(ls_whtax).
 IF ls_whtax-wtax_type is not initial.
 data(Whold_tax) = VALUE vmds_ei_wtax_type(
     task     = 'I'  "Insert
     data_key = VALUE #( witht = ls_whtax-wtax_type )
     data     = VALUE #(  wt_subjct = abap_true
                          wt_withcd = ls_whtax-wtax_code )
     datax    =   VALUE #(  wt_subjct = abap_true
                          wt_withcd = abap_true )
 ).
 APPEND whold_tax to lt_whtax.
 ENDIF.
 ENDLOOP.
"""Company Code Data
 DATA(gs_company) = VALUE vmds_ei_company(
     task      = 'I'   "I = Insert
     data_key  = VALUE #( bukrs =  ls_company-company_code )
     data      = VALUE #(
         akont  = |{ ls_company-reconiliation_acct ALPHA = IN }|
         zwels  = ls_company-payment_methods
         hbkid  = ls_company-house_bank
         zterm  = ls_company-payment_terms
         togru  = ls_company-tolerance_group
         mindk  = ls_company-minority_indicator
         cerdt  = |{ ls_company-cert_date+6(4) }{ ls_company-cert_date+3(2) }{ ls_company-cert_date+0(2) }|
         reprf  = abap_true
         qland = c_cntry )  "Check Flag for Double Invoices or Credit Memos
     datax     = VALUE #(
         akont  = abap_true
         zwels  = abap_true
         hbkid  = abap_true
         zterm  = abap_true
         mindk  = abap_true
         cerdt  = abap_true
         reprf  = abap_true
         qland  = abap_true )
     wtax_type = VALUE vmds_ei_wtax_type_s( wtax_type = lt_whtax )
 ).

APPEND gs_company to lt_company.
ENDLOOP.

"""Purchase Org data
LOOP AT iv_input-purchasing into data(ls_purchasing).
DATA(gs_purchase) = VALUE vmds_ei_purchasing(
    task        = 'I' "insert
    data_key    = VALUE #( ekorg = ls_purchasing-purchase_org )
    data        = VALUE #(
            waers           = 'INR'
            zterm           = ls_purchasing-payment_terms
            kalsk           = ls_purchasing-schema_grp_supplier
            kzaut           = abap_true   "Automatic Generation of Purchase Order Allowed
            webre           = abap_true   "Indicator: GR-Based Invoice Verification
            ekgrp           = ls_purchasing-purchase_grp
            plifz           = ls_purchasing-planned_delivery_time )
    datax       = VALUE #(
            waers           = abap_true
            zterm           = abap_true
            kalsk           = abap_true
            kzaut           = abap_true
            webre           = abap_true
            ekgrp           = abap_true
            plifz           = abap_true )
).
APPEND gs_purchase to lt_purchasing.
ENDLOOP.
***Vendor Structure data population
 iv_vendor = VALUE #(
                     header          = ls_header
                     central_data    = vend_centrl_data
                     company_data    = VALUE #( company = lt_company )
                     purchasing_data = VALUE #( purchasing = lt_purchasing ) ).

  ENDMETHOD.

  METHOD update_customer.
    DATA: iv_cust_extern   TYPE cmds_ei_extern,
          ls_header        TYPE cmds_ei_header,
          ls_data          TYPE cmds_ei_vmd_central_data,
          cust_central     TYPE cmds_ei_cmd_central,
          cust_centrl_data TYPE cmds_ei_central_data.

****Header data****
    ls_header = VALUE #( object_instance = VALUE #( kunnr = ls_kna1-kunnr )   object_task = 'U' ). "Update

****Central data****
    cust_central = VALUE #(
        data  = CORRESPONDING #( BASE ( ls_data ) ls_kna1  )
        datax = VALUE #(
        aufsd              = COND #( WHEN ls_kna1-aufsd                IS NOT INITIAL THEN abap_true ELSE abap_false )
        bahne              = COND #( WHEN ls_kna1-bahne                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bahns              = COND #( WHEN ls_kna1-bahns                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bbbnr              = COND #( WHEN ls_kna1-bbbnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bbsnr              = COND #( WHEN ls_kna1-bbsnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        begru              = COND #( WHEN ls_kna1-begru                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        brsch              = COND #( WHEN ls_kna1-brsch                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bubkz              = COND #( WHEN ls_kna1-bubkz                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        faksd              = COND #( WHEN ls_kna1-faksd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        fiskn              = COND #( WHEN ls_kna1-fiskn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        knrza              = COND #( WHEN ls_kna1-knrza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        konzs              = COND #( WHEN ls_kna1-konzs                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ktokd              = COND #( WHEN ls_kna1-ktokd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kukla              = COND #( WHEN ls_kna1-kukla                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lifnr              = COND #( WHEN ls_kna1-lifnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lifsd              = COND #( WHEN ls_kna1-lifsd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        locco              = COND #( WHEN ls_kna1-locco                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        loevm              = COND #( WHEN ls_kna1-loevm                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        niels              = COND #( WHEN ls_kna1-niels                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        counc              = COND #( WHEN ls_kna1-counc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cityc              = COND #( WHEN ls_kna1-cityc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        rpmkr              = COND #( WHEN ls_kna1-rpmkr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        sperr              = COND #( WHEN ls_kna1-sperr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd1              = COND #( WHEN ls_kna1-stcd1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd2              = COND #( WHEN ls_kna1-stcd2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkza              = COND #( WHEN ls_kna1-stkza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkzu              = COND #( WHEN ls_kna1-stkzu                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xzemp              = COND #( WHEN ls_kna1-xzemp                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        vbund              = COND #( WHEN ls_kna1-vbund                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stceg              = COND #( WHEN ls_kna1-stceg                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        gform              = COND #( WHEN ls_kna1-gform                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran1              = COND #( WHEN ls_kna1-bran1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran2              = COND #( WHEN ls_kna1-bran2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran3              = COND #( WHEN ls_kna1-bran3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran4              = COND #( WHEN ls_kna1-bran4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran5              = COND #( WHEN ls_kna1-bran5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        umjah              = COND #( WHEN ls_kna1-umjah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        uwaer              = COND #( WHEN ls_kna1-uwaer                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        jmzah              = COND #( WHEN ls_kna1-jmzah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        jmjah              = COND #( WHEN ls_kna1-jmjah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr1              = COND #( WHEN ls_kna1-katr1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr2              = COND #( WHEN ls_kna1-katr2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr3              = COND #( WHEN ls_kna1-katr3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr4              = COND #( WHEN ls_kna1-katr4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr5              = COND #( WHEN ls_kna1-katr5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr6              = COND #( WHEN ls_kna1-katr6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr7              = COND #( WHEN ls_kna1-katr7                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr8              = COND #( WHEN ls_kna1-katr8                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr9              = COND #( WHEN ls_kna1-katr9                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr10             = COND #( WHEN ls_kna1-katr10                IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkzn              = COND #( WHEN ls_kna1-stkzn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        umsa1              = COND #( WHEN ls_kna1-umsa1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        periv              = COND #( WHEN ls_kna1-periv                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ktocd              = COND #( WHEN ls_kna1-ktocd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dtams              = COND #( WHEN ls_kna1-dtams                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dtaws              = COND #( WHEN ls_kna1-dtaws                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        hzuor              = COND #( WHEN ls_kna1-hzuor                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        civve              = COND #( WHEN ls_kna1-civve                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        milve              = COND #( WHEN ls_kna1-milve                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        fityp              = COND #( WHEN ls_kna1-fityp                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcdt              = COND #( WHEN ls_kna1-stcdt                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd3              = COND #( WHEN ls_kna1-stcd3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd4              = COND #( WHEN ls_kna1-stcd4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xicms              = COND #( WHEN ls_kna1-xicms                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xxipi              = COND #( WHEN ls_kna1-xxipi                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xsubt              = COND #( WHEN ls_kna1-xsubt                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cfopc              = COND #( WHEN ls_kna1-cfopc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        txlw1              = COND #( WHEN ls_kna1-txlw1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        txlw2              = COND #( WHEN ls_kna1-txlw2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc01              = COND #( WHEN ls_kna1-ccc01                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc02              = COND #( WHEN ls_kna1-ccc02                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc03              = COND #( WHEN ls_kna1-ccc03                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc04              = COND #( WHEN ls_kna1-ccc04                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cassd              = COND #( WHEN ls_kna1-cassd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg1              = COND #( WHEN ls_kna1-kdkg1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg2              = COND #( WHEN ls_kna1-kdkg2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg3              = COND #( WHEN ls_kna1-kdkg3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg4              = COND #( WHEN ls_kna1-kdkg4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg5              = COND #( WHEN ls_kna1-kdkg5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        nodel              = COND #( WHEN ls_kna1-nodel                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kfrepre         = COND #( WHEN ls_kna1-j_1kfrepre            IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kftbus          = COND #( WHEN ls_kna1-j_1kftbus             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kftind          = COND #( WHEN ls_kna1-j_1kftind             IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd5              = COND #( WHEN ls_kna1-stcd5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd6              = COND #( WHEN ls_kna1-stcd6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cvp_xblck          = COND #( WHEN ls_kna1-cvp_xblck             IS NOT INITIAL THEN abap_true ELSE abap_false )
        suframa            = COND #( WHEN ls_kna1-suframa               IS NOT INITIAL THEN abap_true ELSE abap_false )
        rg                 = COND #( WHEN ls_kna1-rg                    IS NOT INITIAL THEN abap_true ELSE abap_false )
        exp                = COND #( WHEN ls_kna1-exp                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        uf                 = COND #( WHEN ls_kna1-uf                    IS NOT INITIAL THEN abap_true ELSE abap_false )
        rgdate             = COND #( WHEN ls_kna1-rgdate                IS NOT INITIAL THEN abap_true ELSE abap_false )
        ric                = COND #( WHEN ls_kna1-ric                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        rne                = COND #( WHEN ls_kna1-rne                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        rnedate            = COND #( WHEN ls_kna1-rnedate               IS NOT INITIAL THEN abap_true ELSE abap_false )
        cnae               = COND #( WHEN ls_kna1-cnae                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        legalnat           = COND #( WHEN ls_kna1-legalnat              IS NOT INITIAL THEN abap_true ELSE abap_false )
        crtn               = COND #( WHEN ls_kna1-crtn                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        icmstaxpay         = COND #( WHEN ls_kna1-icmstaxpay            IS NOT INITIAL THEN abap_true ELSE abap_false )
        indtyp             = COND #( WHEN ls_kna1-indtyp                IS NOT INITIAL THEN abap_true ELSE abap_false )
        tdt                = COND #( WHEN ls_kna1-tdt                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        comsize            = COND #( WHEN ls_kna1-comsize               IS NOT INITIAL THEN abap_true ELSE abap_false )
        decregpc           = COND #( WHEN ls_kna1-decregpc              IS NOT INITIAL THEN abap_true ELSE abap_false )
        ort02              = COND #( WHEN ls_kna1-ort02                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear6              = COND #( WHEN ls_kna1-dear6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lzone              = COND #( WHEN ls_kna1-lzone                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xknza              = COND #( WHEN ls_kna1-xknza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear1              = COND #( WHEN ls_kna1-dear1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear2              = COND #( WHEN ls_kna1-dear2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear3              = COND #( WHEN ls_kna1-dear3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear5              = COND #( WHEN ls_kna1-dear5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        werks              = COND #( WHEN ls_kna1-werks                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        adrnr              = COND #( WHEN ls_kna1-adrnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        delivery_date_rule = COND #( WHEN ls_kna1-delivery_date_rule    IS NOT INITIAL THEN abap_true ELSE abap_false )
        kna1_eew_cust      = COND #( WHEN ls_kna1-kna1_eew_cust         IS NOT INITIAL THEN abap_true ELSE abap_false )
        rule_exclusion     = COND #( WHEN ls_kna1-rule_exclusion        IS NOT INITIAL THEN abap_true ELSE abap_false )
        alc                = COND #( WHEN ls_kna1-alc                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        pmt_office         = COND #( WHEN ls_kna1-pmt_office            IS NOT INITIAL THEN abap_true ELSE abap_false )
        fee_schedule       = COND #( WHEN ls_kna1-fee_schedule          IS NOT INITIAL THEN abap_true ELSE abap_false )
        duns               = COND #( WHEN ls_kna1-duns                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        duns4              = COND #( WHEN ls_kna1-duns4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psofg              = COND #( WHEN ls_kna1-psofg                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psois              = COND #( WHEN ls_kna1-psois                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson1              = COND #( WHEN ls_kna1-pson1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson2              = COND #( WHEN ls_kna1-pson2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson3              = COND #( WHEN ls_kna1-pson3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psovn              = COND #( WHEN ls_kna1-psovn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psotl              = COND #( WHEN ls_kna1-psotl                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psohs              = COND #( WHEN ls_kna1-psohs                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psost              = COND #( WHEN ls_kna1-psost                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo1              = COND #( WHEN ls_kna1-psoo1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo2              = COND #( WHEN ls_kna1-psoo2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo3              = COND #( WHEN ls_kna1-psoo3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo4              = COND #( WHEN ls_kna1-psoo4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo5              = COND #( WHEN ls_kna1-psoo5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexcd           = COND #( WHEN ls_kna1-j_1iexcd              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexrn           = COND #( WHEN ls_kna1-j_1iexrn              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexrg           = COND #( WHEN ls_kna1-j_1iexrg              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexdi           = COND #( WHEN ls_kna1-j_1iexdi              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexco           = COND #( WHEN ls_kna1-j_1iexco              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1icstno          = COND #( WHEN ls_kna1-j_1icstno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ilstno          = COND #( WHEN ls_kna1-j_1ilstno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ipanno          = COND #( WHEN ls_kna1-j_1ipanno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexcicu         = COND #( WHEN ls_kna1-j_1iexcicu            IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1isern           = COND #( WHEN ls_kna1-j_1isern              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ipanref         = COND #( WHEN ls_kna1-j_1ipanref            IS NOT INITIAL THEN abap_true ELSE abap_false )
        zkostl             = COND #( WHEN ls_kna1-zkostl                IS NOT INITIAL THEN abap_true ELSE abap_false )
        zcapacity          = COND #( WHEN ls_kna1-zcapacity             IS NOT INITIAL THEN abap_true ELSE abap_false )
        zlatitude          = COND #( WHEN ls_kna1-zlatitude             IS NOT INITIAL THEN abap_true ELSE abap_false )
        zlongitude         = COND #( WHEN ls_kna1-zlongitude            IS NOT INITIAL THEN abap_true ELSE abap_false )
        zfincode           = COND #( WHEN ls_kna1-zfincode              IS NOT INITIAL THEN abap_true ELSE abap_false )
        zcustype           = COND #( WHEN ls_kna1-zcustype              IS NOT INITIAL THEN abap_true ELSE abap_false )
        zdays              = COND #( WHEN ls_kna1-zdays                 IS NOT INITIAL THEN abap_true ELSE abap_false )
    ) ).

    "Update Central data to its central main structure"
    cust_centrl_data = VALUE #(  central  = cust_central ).

    iv_cust_extern = VALUE #(
        header       = ls_header
        central_data = cust_centrl_data
    ).

     ""Executing class
    cl_md_bp_maintain=>maintain(
      EXPORTING
        i_data     = VALUE #( ( customer = iv_cust_extern ) )
*        i_test_run =
      IMPORTING
        e_return   = lt_return
    ).


  ENDMETHOD.

ENDCLASS.
