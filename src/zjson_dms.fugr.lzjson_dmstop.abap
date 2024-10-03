FUNCTION-POOL ZJSON_DMS.                        "MESSAGE-ID ..

* INCLUDE LZJSOND...                         " Local class definition

TYPES : BEGIN OF t_item_body,
          item TYPE string,
        END OF t_item_body.


TYPES: BEGIN OF t_vbrk,
         vbeln TYPE vbrk-vbeln,     "Doc No.
         fkart TYPE vbrk-fkart ,     "Doc type   & Item Is Servcice
         vkorg TYPE vbrk-vkorg,
         knumv TYPE vbrk-knumv,
         fkdat TYPE vbrk-fkdat ,     "Doc date
         bukrs TYPE vbrk-bukrs,
         xblnr TYPE vbrk-xblnr,     "Refer doc
       END OF t_vbrk.

TYPES: BEGIN OF t_vbrp,
           vbeln  type vbrp-vbeln,
           posnr  TYPE vbrp-posnr,     "SL no of Item
           fkimg  TYPE vbrp-fkimg,     "Item Qty
           vrkme  TYPE vbrp-vrkme,     "Item Unit
           netwr  TYPE vbrp-netwr,
           vgbel  TYPE vbrp-vgbel,
           matnr  TYPE vbrp-matnr,
           arktx  TYPE vbrp-arktx,
           werks  TYPE vbrp-werks,
           steuc  TYPE marc-steuc,    "Item HsnCd
       end of t_vbrp.

TYPES: BEGIN OF t_konv,
           knumv  TYPE konv-knumv,
           KPOSN  TYPE KONV-KPOSN,
           kschl  TYPE konv-kschl,
           kbetr  TYPE konv-kbetr,
           kwert  type konv-kwert,
           KAWRT  type konv-KAWRT,
       END OF t_konv.

TYPES : BEGIN OF t_seller,
             bukrs    TYPE J_1BBRANCH-bukrs,
             branch   TYPE J_1BBRANCH-branch,
             gstin    TYPE J_1BBRANCH-gstin,
        END OF t_seller.

TYPES: BEGIN OF t_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
        END OF t_makt.

TYPES: BEGIN OF t_marc,
         matnr TYPE marc-matnr,
         steuc TYPE marc-steuc,
         werks TYPE marc-werks,
        END OF t_marc.

 TYPES:    begin of ty_t001w,
            werks type werks_d,
            name1 type name1,
            adrnr type adrnr,
            j_1bbranch type j_1bbranc_,
            end of ty_t001w.

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
TYPES: BEGIN OF t_kna1,
         vbeln    TYPE likp-vbeln,
         kunnr    TYPE likp-kunnr,
         kunag    TYPE likp-kunag,     "Sold to Party
         name1    TYPE kna1-name1,     "Buyer Legal Name & "Ship to Legal Name
         stcd3    TYPE kna1-stcd3,      "Buyer Gstin    & "Ship to SGTIN
         land1    TYPE kna1-land1,
         regio    TYPE kna1-regio,        "Buyer State Code
         addrnumber TYPE adrc-addrnumber,
         post_code1 TYPE adrc-post_code1,  "Buyer Pin Code
         str_suppl1 TYPE adrc-str_suppl1,
         house_num1 TYPE adrc-house_num1, "Ship to LEFT
         Street     TYPE adrc-street,     "Buyer Address1
         str_suppl3 TYPE adrc-str_suppl3, "Buyer Address1
         city1      TYPE adrc-city1,      "City1
         location   TYPE adrc-location,   "Despatch from LEFT
         region     TYPE adrc-region,     "Ship to State Code
         bezei      TYPE t005u-bezei,
       END OF t_kna1.

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
         Street     TYPE adrc-street,     "SHIP Address1
         str_suppl3 TYPE adrc-str_suppl3, "Buyer Address1
         city1      TYPE adrc-city1,      "City1
         location   TYPE adrc-location,   "Despatch from LEFT
         region     TYPE adrc-region,     "Ship to State Code
         bezei      TYPE t005u-bezei,
       END OF t_vbpa.

DATA: it_j_1ig_invrefnum TYPE TABLE OF j_1ig_invrefnum,
      it_j_1ig_ewaybill  TYPE TABLE OF j_1ig_ewaybill.


DATA: l_tab_vbrk          TYPE TABLE OF t_vbrk,
      w_tab_vbrk          TYPE t_vbrk,
      l_tab_vbrp          TYPE TABLE OF t_vbrp,
      w_tab_vbrp          TYPE t_vbrp,
      l_tab_prcd          TYPE TABLE OF t_konv,
      w_tab_prcd          TYPE t_konv,
      w_tab_prcd1         TYPE t_konv,
      l_tab_branch        TYPE TABLE OF t_seller,
      w_tab_branch        TYPE t_seller,
      l_tab_makt          TYPE TABLE OF T_MAKT,
      w_tab_makt          TYPE t_makt,
      l_tab_marc          TYPE TABLE OF t_marc,
      w_tab_marc          TYPE t_marc,
      lT_tab_t001w        type table of ty_t001w,
      lS_tab_t001w        type ty_t001w,
      l_tab_t005u         TYPE TABLE OF t_t005u,
      w_tab_t005u         TYPE t_t005u,
      l_tab_kna1_adrc     TYPE TABLE OF t_kna1,
      l_tab_kna1_adrc1    TYPE TABLE OF t_kna1,
      w_tab_kna1_adrc     TYPE t_kna1,
      l_tab_vbpa_adrc     TYPE TABLE OF t_vbpa,
      l_tab_vbpa_adrc1    TYPE TABLE OF t_vbpa,
      w_tab_vbpa_adrc     TYPE t_vbpa.

CONSTANTS : c_comma                TYPE c VALUE ',',
            c_curly_brackets_open  TYPE char01 VALUE '{',
            c_curly_brackets_close TYPE char01 VALUE '}',
            c_quatation_mark       TYPE char01 VALUE '"',
            c_colon                TYPE char01 VALUE':',
            c_version              TYPE char05 VALUE '1.1',
            c_gst                  TYPE char10 VALUE  '"Gstin":',
            c_lglnm                TYPE char10 VALUE  '"LglNm":',
            c_trdnm                TYPE char10 VALUE  '"TrdNm":',
            c_addr1                TYPE char10 VALUE  '"Addr1":',
            c_addr2                TYPE char10 VALUE  '"Addr2":',
            c_loc                  TYPE char10 VALUE  '"Loc":',
            c_pin                  TYPE char10 VALUE  '"Pin":',
            c_pos                  TYPE char10 VALUE  '"Pos":',
            c_ph                   TYPE char10 VALUE  '"Ph":',
            c_em                   TYPE char10 VALUE  '"Em":',
            c_stcd                 TYPE char10 VALUE  '"Stcd":',
            c_nm                   TYPE char10 VALUE  '"Nm":',
            c_dt                   TYPE char10 VALUE  '"Dt":',
            c_no                   TYPE char10 VALUE  '"No":',
            c_typ                  TYPE char10 VALUE  '"Typ":',
            c_b2b                  TYPE char03 VALUE  'B2B',
            c_null                 TYPE char07 VALUE  'null',
            c_zero                 TYPE char01 VALUE  '0'.

*******Data Declaration************************
DATA:             it_body_item       TYPE STANDARD TABLE OF t_item_body,
                  wa_body_item       TYPE t_item_body,
                  v_tcs              TYPE char15,
                  v_assval           TYPE char15,
                  v_igst             TYPE char15,
                  v_cgst             TYPE char15,
                  v_sgst             TYPE char15,
                  v_amtval           TYPE char15,
                  lv_trans           TYPE string,
                  lv_docdtls         TYPE string,
                  lv_selldtls        TYPE string,
                  lv_buydtls         TYPE string,
                  lv_dispdtls        TYPE string,
                  lv_shipdtls        TYPE string,
                  lv_refdtls         TYPE string,
                  lv_addldocdtls     TYPE string,
                  lv_expdtls         TYPE string,
                  lv_itmdtls         TYPE string,
                  lv_valdtls         TYPE string,
                  lv_paydtls         TYPE string,
                  lv_ewbdtls         TYPE string,
                  lv_date            TYPE char10,
*                  v_str              TYPE string,
                  gv_msg             TYPE c,
                  v_payload          TYPE string,
                  gv_vbeln           TYPE char10,
                  gv_ref             TYPE char12,
                  gt_item            TYPE TABLE OF zst_irn_item_json,
                  gw_item            TYPE zst_irn_item_json,
                  gw_item1           TYPE zst_irn_item_json,
                  ei_in              TYPE zst_irn_hdr,
                  tt_itemdtls        TYPE zst_lt_irn_item_json,
                  wa_itemdtls        TYPE zst_irn_item_json,
                  wa_j_1ig_invrefnum TYPE j_1ig_invrefnum,
                  wa_j_1ig_ewaybill  TYPE j_1ig_ewaybill,
                  wa_url             TYPE zeinv_url,
                  lv_string          TYPE string,
                  create_url         TYPE string.

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
       lv_http_return_code      TYPE i.

******Error Messages***********
DATA: v_errormsg  TYPE char40,
      v_errorcode TYPE char4,
      v_str       TYPE string,
      v_str1      TYPE string,
      v_msg       TYPE c,
      v_msg1      TYPE c,
      v_str2      TYPE string.

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
