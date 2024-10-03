*&---------------------------------------------------------------------*
*& Include          ZSD_CUSTOMER_MASTER_TOP
*&---------------------------------------------------------------------*

TABLES: but000,kna1,lfa1,pa0006,pa0002.
*** Create only business partner ****
TYPES: BEGIN OF ty_file_bp,
         bpcat    TYPE bapibus1006_head-partn_cat, " Category
         bpgrp    TYPE bapibus1006_head-partn_grp, "BP Group
         title    TYPE ad_titletx, "Title
         sterm1   TYPE sortl, "Search term
         fname    TYPE bu_namep_f, "First Name
         lname    TYPE bu_namep_l, "Last Name
         street   TYPE ad_street,  "Street
         addr1    TYPE ad_strspp1, "Address1
         addr2    TYPE ad_strspp1, "Address2
         addr3    TYPE ad_strspp1, "Address3
         houseno  TYPE ad_hsnm1, "House Number
         district TYPE ad_city2,
         country  TYPE land1, "Country
         langu    TYPE spras, "Language
         region   TYPE regio, "Region
         city     TYPE ad_city1, "City
         pcode    TYPE ad_pstcd1, "Postal Code
         telno    TYPE ad_tlnmbr, "Mobile Number
         mobile   TYPE ad_tlnmbr, "Mobile Number
         email    TYPE ad_smtpadr, "Email ID
         taxno    TYPE stcd3, "GST Number
       END OF ty_file_bp.

TYPES: BEGIN OF ty_bp_all,
         bp         TYPE bapibus1006_head-bpartner,
         bpcat      TYPE bapibus1006_head-partn_cat, " Category
         bpgrp      TYPE bapibus1006_head-partn_grp, "BP Group
         title      TYPE ad_titletx, "Title
         sterm1     TYPE sortl, "Search term
         fname      TYPE bu_namep_f, "First Name
         lname      TYPE bu_namep_l, "Last Name
         name3      TYPE bu_nameor3,
         name4      TYPE bu_nameor4,
         street     TYPE ad_street,  "Street
         addr1      TYPE ad_strspp1, "Address1
         addr2      TYPE ad_strspp1, "Address2
         addr3      TYPE ad_strspp1, "Address3
         houseno    TYPE ad_hsnm1, "House Number
         district   TYPE ad_city2,
         country    TYPE land1, "Country
         langu      TYPE spras, "Language
         region     TYPE regio, "Region
         city       TYPE ad_city1, "City
         pcode      TYPE ad_pstcd1, "Postal Code
         telno      TYPE ad_tlnmbr, "Mobile Number
         mobile     TYPE ad_tlnmbr, "Mobile Number
         email      TYPE ad_smtpadr, "Email ID
         taxcat     TYPE bptaxtype,
         taxno      TYPE bapibus1006tax-taxnumber, "GST Number
         bukrs      TYPE lfb1-bukrs, " Company Code
         akont      TYPE akont,      "Recon Account
         zterm      TYPE dzterm,     "Payment Terms
         vkorg      TYPE vkorg, "Sales Org
         vtweg      TYPE vtweg, "distribution Channel
         spart      TYPE spart, "Division
         bzirk      TYPE knvv-bzirk, " Sales District
         kdgrp      TYPE knvv-kdgrp, " Customer Group
         vkbur      TYPE knvv-vkbur, " Sales office
         vkgrp      TYPE knvv-vkgrp, "Sales Group New
         ktgrd      TYPE knvv-ktgrd, "Acct Assignment Grp
         kalks      TYPE knvv-kalks, " Pricing
         incov      TYPE knvv-incov, "Incoming terms version
         inco1      TYPE knvv-inco1, " Incoterms (Part 1)
         inco2      TYPE knvv-inco2, " Incoterms (Part 2)
         inco_l     TYPE inco2_l, " Inco terms Location
         vwerk      TYPE knvv-vwerk, "Delivery Plant
         vsbed      TYPE knvv-vsbed, "shipping Conditions
         taxkd01(1), "Tax1-JOSG
         taxkd02(1), "Tax1-JOCG
         taxkd03(1), "Tax1-JOIG
         taxkd04(1), "Tax1-JOUG
         taxkd05(1), "Tax1-JOCS
         taxkd06(1), "Tax1-JTC1
         attr1      TYPE katr1, "Attribute 1
         attr2      TYPE katr2, "Attribute 2
         attr3      TYPE katr3, "Attribute 3
         attr4      TYPE katr4, "Attribute 4
         attr5      TYPE katr5, "Attribute 5
         attr6      TYPE katr6, "Attribute 6
         attr7      TYPE katr7, "Attribute 7
         attr8      TYPE katr8, "Attribute 8
         attr9      TYPE katr9, "Attribute 9
         attr10     TYPE katr10, "Attribute 10
         pf_ns      TYPE pernr_d, "Pf1
         pf_rs      TYPE pernr_d, "Pf2
         pf_as      TYPE pernr_d, "Pf3
         pf_so      TYPE pernr_d, "Pf4
         pf_sk      TYPE kunnr, "Pf5
         pf_za      TYPE pernr_d, "Pf6
         pf_zc      TYPE pernr_d, "Pf7
         pf_zh      TYPE pernr_d, "Pf8
         pf_zs      TYPE pernr_d, "Pf9
         lprio      TYPE lprio_di,
         pltyp      TYPE pltyp,
       END OF ty_bp_all.
"Company Code Extension
TYPES : BEGIN OF ty_cc_ext,
          customer TYPE kunnr,
          bukrs    TYPE bukrs,
          zterm    TYPE dzterm,
          akont    TYPE akont,
        END OF ty_cc_ext.
TYPES : BEGIN OF ty_cc_alv,
          customer TYPE kunnr,
          name     TYPE name1_gp,
          bukrs    TYPE bukrs,
          zterm    TYPE dzterm,
          akont    TYPE akont,
          type     TYPE bapi_mtype,
          msg      TYPE string,
        END OF ty_cc_alv.

TYPES: BEGIN OF ty_pa0000,
        pernr TYPE pa0000-pernr,
        stat2 TYPE pa0000-stat2,
       END OF ty_pa0000.

DATA: gt_pa0000 TYPE table of ty_pa0000,
      gs_pa0000 TYPE ty_pa0000.

DATA: gt_cc_alv TYPE TABLE OF ty_cc_alv.
DATA: gt_cc_excel TYPE TABLE OF ty_cc_ext.
DATA: gt_raw    TYPE truxs_t_text_data.
DATA: gt_bp_all TYPE TABLE OF ty_bp_all.
DATA: gv_bpcat(01) TYPE n.
DATA: gt_bp_create TYPE TABLE OF ty_file_bp.
DATA: gv_error TYPE flag.
DATA: gt_log TYPE STANDARD TABLE OF zsd_bp_creation.
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
DATA: ls_bp_all TYPE ty_bp_all.
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


*===========================================================================*
**Added by Samsudeen M on 16.12.2023
INCLUDE <icon>.
TYPES: BEGIN OF ts_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         land1 TYPE kna1-land1,
       END OF ts_kna1.
DATA: gt_kna1 TYPE TABLE OF ts_kna1.

TYPES: BEGIN OF ts_knb1,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
         akont TYPE knb1-akont,
         zterm TYPE knb1-zterm,
       END OF ts_knb1.
DATA: gt_knb1 TYPE TABLE OF ts_knb1.

TYPES: BEGIN OF ts_knvv,
         kunnr  TYPE knvv-kunnr,
         vkorg  TYPE knvv-vkorg,
         vtweg  TYPE knvv-vtweg,
         spart  TYPE knvv-spart,
         bzirk  TYPE knvv-bzirk, " Sales District
         kdgrp  TYPE knvv-kdgrp, " Customer Group
         vkbur  TYPE knvv-vkbur, " Sales office
         vkgrp  TYPE knvv-vkgrp, "Sales Group New
         ktgrd  TYPE knvv-ktgrd, "Acct Assignment Grp
         kalks  TYPE knvv-kalks, " Pricing
         incov  TYPE knvv-incov, "Incoming terms version
         inco1  TYPE knvv-inco1, " Incoterms (Part 1)
         inco2  TYPE knvv-inco2, " Incoterms (Part 2)
         inco_l TYPE inco2_l, " Inco terms Location
         vwerk  TYPE knvv-vwerk, "Delivery Plant
         vsbed  TYPE knvv-vsbed, "shipping Condition
         lprio  TYPE lprio_di,
         pltyp  TYPE pltyp,
         kabss  TYPE knvv-kabss,
         kkber  TYPE knvv-kkber,
       END OF ts_knvv.
DATA: gt_knvv TYPE TABLE OF ts_knvv.

DATA: gt_knvp TYPE STANDARD TABLE OF knvp.

TYPES: BEGIN OF ts_dms_extn,
         status(4),
         kunnr      TYPE kna1-kunnr,
         name1      TYPE kna1-name1,
         land1      TYPE kna1-land1,
         bukrs      TYPE knb1-bukrs,
         akont      TYPE knb1-akont,
         zterm      TYPE knb1-zterm,
         vkorg      TYPE vkorg, "Sales Org
         vtweg      TYPE vtweg, "distribution Channel
         spart      TYPE spart, "Division
         bzirk      TYPE knvv-bzirk, " Sales District
         kdgrp      TYPE knvv-kdgrp, " Customer Group
         vkbur      TYPE knvv-vkbur, " Sales office
         vkgrp      TYPE knvv-vkgrp, "Sales Group New
         ktgrd      TYPE knvv-ktgrd, "Acct Assignment Grp
         kalks      TYPE knvv-kalks, " Pricing
         incov      TYPE knvv-incov, "Incoming terms version
         inco1      TYPE knvv-inco1, " Incoterms (Part 1)
         inco2      TYPE knvv-inco2, " Incoterms (Part 2)
         inco_l     TYPE inco2_l, " Inco terms Location
         vwerk      TYPE knvv-vwerk, "Delivery Plant
         vsbed      TYPE knvv-vsbed, "shipping Conditions
         kabss      TYPE knvv-kabss,
         kkber      TYPE knvv-kkber,
         taxkd01(1), "Tax1-JOSG
         taxkd02(1), "Tax1-JOCG
         taxkd03(1), "Tax1-JOIG
         taxkd04(1), "Tax1-JOUG
         taxkd05(1), "Tax1-JOCS
         taxkd06(1), "Tax1-JTC1
         lprio      TYPE lprio_di,
         pltyp      TYPE pltyp,
         pf_sp      TYPE kunnr,
         pf_bp      TYPE kunnr,
         pf_py      TYPE kunnr,
         pf_sk      TYPE kunnr,
         pf_sh      TYPE kunnr,
         pf_ns      TYPE pernr_d,
         pf_rs      TYPE pernr_d,
         pf_as      TYPE pernr_d,
         pf_so      TYPE pernr_d,
         pf_zc      TYPE pernr_d,
         pf_zs      TYPE pernr_d,
         pf_xj      TYPE lifnr,
         pf_xk      TYPE lifnr,
         pf_xl      TYPE lifnr,
         pf_xn      TYPE lifnr,
         pf_yd      TYPE lifnr,
         message    TYPE string,
       END OF ts_dms_extn.
DATA: gt_dms_extn TYPE TABLE OF ts_dms_extn.
DATA: gr_grid TYPE REF TO cl_gui_alv_grid.
DATA: gt_fieldcat TYPE lvc_t_fcat.
*================================================================================*
