FUNCTION-POOL YFG_EWAYBILL.                 "MESSAGE-ID ..

* INCLUDE LZFG_EWAYBILLD...                  " Local class definition

"Get Plant address
TYPES: BEGIN OF t_t005u,
            werks       type t001w-werks,
            pstlz       type t001w-pstlz,
            land1       type t001w-land1,
            regio       type t001w-regio,
            bezei       type t005u-bezei,
            addrnumber  type adrc-addrnumber,      "Seller Legal Name
            name1       type adrc-name1,           "Despatch from Name\
            house_num1  type adrc-house_num1,
            street      type adrc-street,          "Street     &"Despatch from add1
            str_suppl3  TYPE adrc-str_suppl3,      "
            location    TYPE adrc-location,        "Despatch from add1
            post_code1  type adrc-post_code1,
            region      type adrc-region,
            mc_city1    type adrc-mc_city1,
       END OF t_t005u.


"Get Buyer details
TYPES: BEGIN OF t_vbpa,
         vbeln    TYPE vbpa-vbeln,
         kunnr    TYPE vbpa-kunnr,      "Buyer / Ship to
         parvw    TYPE vbpa-parvw,      "Partner Functn
         name1    TYPE kna1-name1,     "Buyer Legal Name & "Ship to Legal Name
         stcd3    TYPE kna1-stcd3,      "Buyer Gstin    & "Ship to SGTIN
         land1    TYPE kna1-land1,
         regio    TYPE kna1-regio,        "Buyer State Code
         addrnumber TYPE adrc-addrnumber,
         post_code1 TYPE adrc-post_code1,  "Buyer Pin Code
         str_suppl1 TYPE adrc-str_suppl1,
         house_num1 TYPE adrc-house_num1, "Ship to LEFT
         str_suppl3 TYPE adrc-str_suppl3, "Buyer Address1
         city1      TYPE adrc-city1,      "City1
         location   TYPE adrc-location,   "Despatch from LEFT
         region     TYPE adrc-region,     "Ship to State Code
         bezei      TYPE t005u-bezei,
       END OF t_vbpa.


 DATA: l_tab_vbpa_adrc     TYPE TABLE OF t_vbpa,
       w_tab_vbpa_adrc     TYPE t_vbpa,
      l_tab_t005u         TYPE TABLE OF t_t005u,
      w_tab_t005u         TYPE t_t005u.

CONSTANTS : c_comma                TYPE c VALUE ','.

DATA: lv_ewbdtls         TYPE string,
      lv_fkart           TYPE fkart,
      wa_j_1ig_invrefnum TYPE j_1ig_invrefnum,
      wa_j_1ig_ewaybill  TYPE j_1ig_ewaybill,
      wa_url             TYPE ZEINV_URL,
      lv_sel_pin         TYPE string,
      lv_buy_pin         TYPE string,
      lv_sel_pin1        TYPE string,
      lv_sel_loc         TYPE string,
      lv_buyer           TYPE string, "  char02,
      lv_seller          TYPE string, "  char02,
      lv_distance        TYPE char05,
      LV_REGION          TYPE CHAR02, "#EC CI_USAGE_OK[2689873] " Added by <IT-CAR Tool> during Code Remediation
      v_transid          TYPE string,
      gv_expdtls         TYPE string.


*******Cleartax Sandbox API Credintals **************************
DATA: gv_url     TYPE string,   "VALUE 'https://einvoicing.internal.cleartax.co/v2/eInvoice/generate',
      gv_geturl  TYPE string,
      gv_gstin   TYPE string,   "VALUE '29AAFCD5862R000',
      gv_ownerid TYPE string,   "VALUE '4abbf226-bd72-4f2a-a3c6-5bbf4d9b47f9',
      gv_auth    TYPE string.

******Cleartax Production API Credintals **************************
*Data: gv_url     type string value   'https://api-einv.cleartax.in/v1/govt/api/Invoice',
*      gv_gstin   type string,                      "FETCH DYNAMIC FROM TABLE
*      gv_ownerid type string,                      "FETCH DYNAMIC FROM TABLE
*      gv_auth    type string value ''.

DATA : lv_body_strcture         TYPE string,
       token                    TYPE string,
       lv_http_error_descr      TYPE string,
       lv_http_error_descr_long TYPE xstring,
       response                 TYPE string,
       lv_string                TYPE string,
       WA_SHIP_PORT             TYPE ZSH_PORT,
       lv_http_return_code      TYPE i,
       lv_werks                 TYPE werks_d,
       lv_branch                type J_1BBRANC_,
       lv_gstin                 type J_1IGSTCD3,
       lv_srt1                  TYPE string.

TYPES: BEGIN OF t_error,
         error_code    TYPE  string,
         error_message TYPE string,
         error_source  TYPE string,
       END OF t_error.

DATA: it_error TYPE TABLE OF t_error,
      wa_error TYPE t_error.

DATA: it_bapiret TYPE TABLE OF bapiret2,
      wa_bapiret TYPE bapiret2.
DATA: http_client         TYPE REF TO if_http_client.

******Error Messages***********
DATA: v_errormsg  TYPE STRING,
      v_errorcode TYPE char4,
      v_str       TYPE string,
      v_str1      TYPE string,
      v_msg       TYPE c,
      v_msg1      TYPE c,
      v_str2      TYPE string.

DATA : WA_YZURL TYPE YZEINV_URL.

DATA : GV_EFUSERNAME TYPE YZEINV_URL-EFUSERNAME ,
    GV_EFPASSWORD TYPE YZEINV_URL-EFPASSWORD,
    GV_EINVUSERNAME TYPE YZEINV_URL-EINVUSERNAME,
    GV_EINVPASSWORD TYPE YZEINV_URL-EINVPASSWORD.


DATA : c_quatation_mark    TYPE char01 VALUE '"'.
