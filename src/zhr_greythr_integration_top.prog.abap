*&---------------------------------------------------------------------*
*& Include          ZHR_GREYTHR_INTEGRATION_TOP
*&---------------------------------------------------------------------*
TABLES:pa0001.

TYPES:BEGIN OF ty_log_report,     "Structure for ALV Display
        pernr  TYPE pa0001-pernr,
        infty  TYPE p0001-infty,
        subty  TYPE pa0105-subty,
        status TYPE c,
        text   TYPE string,
      END OF ty_log_report.

TYPES: BEGIN OF ty_pernr,         "Structure for Employee Selection Table Data
         pernr TYPE pernr_d,
       END OF ty_pernr.

TYPES: BEGIN OF ty_pernr_hire,    "Structure Employee Hire Date Table Data
         pernr TYPE pernr_d,
         dat01 TYPE dardt,
       END OF ty_pernr_hire.

TYPES: BEGIN OF ty_id_get,        "Structure Employee ID Details Table Data
         pernr TYPE pernr_d,
         usrid TYPE sysid,
       END OF ty_id_get.

TYPES: BEGIN OF ty_employee,      "Structure for Employee greytip Id API
         employeeid TYPE string,
         name       TYPE string,
         employeeno TYPE string,
         joindate   TYPE string,
         leftorg    TYPE string,
         email      TYPE string,
       END OF ty_employee.

TYPES: BEGIN OF ty_emp_id,        "Structure for Employee Identity details API
         id            TYPE string,
         idcode        TYPE string,
         employeeid    TYPE string,
         documentno    TYPE string,
         nameasperdoc  TYPE string,
         ifsccode      TYPE string,
         expirydate    TYPE string,
         verified      TYPE string,
         verifieddate  TYPE string,
         aadharappno   TYPE string,
         enablemasking TYPE string,
         guid          TYPE string,
       END OF ty_emp_id.

TYPES: BEGIN OF ty_family,       "Structure For Employee Family Details API
         id         TYPE string,
         name       TYPE string,
         relationid TYPE string,
         dob        TYPE string,
         bloodgroup TYPE string,
         gender     TYPE string,
         profession TYPE string,
       END OF ty_family.

TYPES: BEGIN OF ty_emp_address,    "Structure for Employee Address API
         employeeid  TYPE string,
         address1    TYPE string,
         address2    TYPE string,
         address3    TYPE string,
         city        TYPE string,
         state       TYPE string,
         country     TYPE string,
         pin         TYPE string,
         phone1      TYPE string,
         phone2      TYPE string,
         extnno      TYPE string,
         fax         TYPE string,
         mobile      TYPE string,
         email       TYPE string,
         name        TYPE string,
         addresstype TYPE string,
         companyid   TYPE string,
         recordid    TYPE string,
       END OF ty_emp_address.

TYPES: BEGIN OF ty_qualification,  "Structure for Employee Qualification API
         id                 TYPE string,
         employee           TYPE string,
         qualarea           TYPE string,
         qualdescription    TYPE string,
         quallevel          TYPE string,
         qualyear           TYPE string,
         qualcompletionyear TYPE string,
         duration           TYPE string,
         professionalqual   TYPE string,
         yearstaken         TYPE string,
         institute          TYPE string,
         university         TYPE string,
         grade              TYPE string,
         current            TYPE string,
         qualsubjects       TYPE string,
       END OF ty_qualification.

TYPES: BEGIN OF ty_bank,
         id                TYPE string,
         bankaccountnumber TYPE string,
         accounttype       TYPE string,
         bankname          TYPE string,
         bankbranch        TYPE string,
         branchcode        TYPE string,
         salarypaymentmode TYPE string,
         ddpayableat       TYPE string,
         nameasperbank     TYPE string,
       END OF ty_bank.

TYPES: BEGIN OF ty_url,           "Structure for API URL
         url1 TYPE string,
       END OF ty_url.

TYPES: BEGIN OF ty_bnka,
         banks TYPE banks,   "Structure for BANK KEY Check
         bankl TYPE bankk,
       END OF ty_bnka.

**********Internal table for Employee ID URL For API***************
DATA: lt_url TYPE TABLE OF ty_url,
      ls_url TYPE ty_url.

**********Internal table for Employee Selection based on Selection screen***************
DATA: gt_pernr TYPE TABLE OF ty_pernr,
      gs_pernr TYPE ty_pernr.

**********Internal table for Employee Hire Date Details***************
DATA: gt_pernr_hire TYPE TABLE OF ty_pernr_hire,
      gs_pernr_hire TYPE ty_pernr_hire.

**********Internal table for Employee ID Details From table***************
DATA: gt_id_get TYPE TABLE OF ty_id_get,
      gs_id_get TYPE ty_id_get.

**********Internal table for Greytip ID API***************
DATA: gt_employee TYPE TABLE OF ty_employee,
      gs_employee TYPE ty_employee.

**********Internal table for Employee Identity Details API***************
DATA: gt_id_details TYPE TABLE OF ty_emp_id,
      gs_id_details TYPE ty_emp_id.

*********Internal table for Employee Family Details API***************
DATA: gt_family TYPE TABLE OF ty_family,
      gs_family TYPE ty_family.


**********Internal table for Employee Address Details From API***************
DATA: gt_address TYPE TABLE OF ty_emp_address,
      gs_address TYPE ty_emp_address.

**********Internal table for Employee Bank Details From API***************
DATA: gt_bank TYPE TABLE OF ty_bank,
      gs_bank TYPE ty_bank.

**********Internal table for Employee Qualification Details From API***************
DATA: gt_education TYPE TABLE OF ty_qualification,
      gs_education TYPE ty_qualification.

**********Internal table for ALV Display***************
DATA: gt_log_report  TYPE TABLE OF ty_log_report,
      gs_log_report  TYPE ty_log_report,
      lt_header      TYPE slis_t_listheader,
      ls_header      TYPE slis_listheader,
      today_date(10),
      today_time(10).

**********Internal table for Employee Bank Details check From API***************
DATA: lt_bank TYPE TABLE OF ty_bnka,
      ls_bank TYPE ty_bnka.

DATA: lt_lov TYPE STANDARD TABLE OF zhr_greythr_lov,
      ls_lov TYPE zhr_greythr_lov.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

*********Access Token Internal table and Work area******************
DATA: lt_access TYPE STANDARD TABLE OF zhr_access_token,
      ls_access TYPE zhr_access_token.

DATA: str1       TYPE string,  "Access Token From Both Table and FM"
      str2       TYPE string,
      token      TYPE zaccess, "Function Module Access Token"
      create_url TYPE string,  "URL of API"
      emp_id(8)  TYPE c. "Actual input of API

DATA: lo_http_client TYPE REF TO if_http_client.

*********Internal Table and Work area for URL for all API***********
DATA: lt_tvarvc  TYPE STANDARD TABLE OF tvarvc,
      ls_tvarvc  TYPE tvarvc,
      ls_tvarvc1 TYPE tvarvc.

DATA: lt_greythr TYPE STANDARD TABLE OF zgreythr_tab,
      ls_greythr TYPE zgreythr_tab.

DATA: lv_url TYPE string.
DATA: lv_response   TYPE string, "API Response
      lv_codes      TYPE i,      "STATUS Code
      lv_http_error TYPE string, "STATUS Description
      ls_ret        TYPE bapiret2.

DATA: l_firstflag TYPE flag.
