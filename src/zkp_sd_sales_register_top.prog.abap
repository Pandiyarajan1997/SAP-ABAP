*&---------------------------------------------------------------------*
*& Include          ZKP_SD_SALES_REGISTER_TOP
*&---------------------------------------------------------------------*
TABLES : vbrk,
         vbrp,
         kna1,
         vbak,
         vbap,
         bseg,
         bkpf,
         prcd_elements, " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
         t001,
         tspat,
         j_1irg23d,
         mara.

* ALV Type Pool
TYPE-POOLS : slis.

* Alv Declerations
DATA : fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gd_tab_group TYPE slis_t_sp_group_alv,
       gd_layout    TYPE slis_layout_alv,
       gd_repid     LIKE sy-repid,
       it_sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       it_event     TYPE slis_t_event,
       ls_event     TYPE slis_alv_event,
       fld_lay      TYPE slis_layout_alv.

DATA : layout TYPE slis_layout_alv.

DATA: i_fieldcat  TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv.

DATA: lv_bezei TYPE t005u-bezei,
      lv_regio TYPE t001w-regio. "Added by abidulla for Plant State


*** INTERNAL TABLES

DATA: gs_mesg    LIKE mesg,
      gs_variant LIKE  disvariant,
      gf_exit(1) TYPE c,
      gf_save    LIKE boole-boole.


TYPES : BEGIN OF gs_mara,
          matnr TYPE mara-matnr,
          volum TYPE mara-volum,
          voleh TYPE mara-voleh,
        END OF gs_mara.

DATA : gt_mara TYPE TABLE OF gs_mara,
       wa_mara TYPE gs_mara.

CONSTANTS: c_save(1) TYPE c VALUE 'A',
           c_hlist   LIKE  disvariant-handle  VALUE  'LIST'.

TYPES : BEGIN OF t_t005t,
          spras LIKE t005t-spras,   "Language
          land1 LIKE t005t-land1,   "Country Key
          landx LIKE t005t-landx,   "Description
        END OF t_t005t.

DATA : it_t005t TYPE STANDARD TABLE OF t_t005t INITIAL SIZE 0 WITH HEADER LINE.

*Internal table for vbrk,vbrp and kna1.
TYPES : BEGIN OF ty_sales,
          vbeln            LIKE vbrk-vbeln,
          vtweg            LIKE vbrk-vtweg,     " Distribution Channel
          fkdat            LIKE vbrk-fkdat,     " Billing date
          fkart            LIKE vbrk-fkart,      " Billing type
          knumv            LIKE vbrk-knumv,     " Document condition no.
          kunrg            LIKE vbrk-kunrg,     " Customer
          mwsbk            LIKE vbrk-mwsbk,
          bukrs            LIKE vbrk-bukrs,     " Company Code
          gjahr            LIKE vbrk-gjahr,
          spart            LIKE vbrk-spart,       "DIVISION
          kurrf            LIKE vbrk-kurrf,
*        fkart like vbrk-fkart,
          bupla            LIKE vbrk-bupla,
          sfakn            LIKE vbrk-sfakn,       " original Document
          fksto            LIKE vbrk-fksto,       " Cancel Document
          dtext            LIKE tspat-vtext,     "division text
          werks            LIKE vbrp-werks,      "Plant
          prctr            LIKE vbrp-prctr,      "Profit Center
          fkimg            LIKE mard-labst,      "Quantity
          meins            LIKE vbrp-meins,
          vgbel            LIKE vbrp-vgbel,      "reference document
          posnr            LIKE vbrp-posnr,      "Billing item
          mwsbp            LIKE vbrp-mwsbp,      "Tax amount
          matnr            LIKE vbrp-matnr,      "Material Number
          netwr            LIKE vbrp-netwr,      "Material Number
          arktx            LIKE vbrp-arktx,      "Short text for sales order item
          aubel            LIKE vbrp-aubel,
          vkbur            LIKE vbrp-vkbur,
          charg            LIKE vbrp-charg,

          volum            TYPE vbrp-volum,
          volum1           LIKE vbrp-volum,
          voleh            LIKE vbrp-voleh,
          t_volum          TYPE vbrp-volum,
          mwsk1            LIKE konv-mwsk1,
          kunnr            LIKE kna1-kunnr,
          name1            LIKE kna1-name1,       " Name
          exnum            LIKE j_1iexchdr-exnum,
          sale_rate        LIKE konv-kbetr,
          glacc            LIKE konv-sakn1,
          netwr1           LIKE vbrk-netwr,
          pac_for          TYPE mard-vklab,
          disc             TYPE mard-vklab,
          crbt             TYPE mard-vklab,
          crbt1            TYPE mard-vklab,
          crbt2            TYPE mard-vklab,
          crbt3            TYPE mard-vklab,
          crbt4            TYPE mard-vklab,
          adisc            TYPE mard-vklab,
          bed              TYPE mard-vklab,
          aed              TYPE mard-vklab,
          ecess            TYPE mard-vklab,
          shecss           TYPE mard-vklab,
          cst              TYPE mard-vklab,
          acst             TYPE mard-vklab,
          ins              TYPE mard-vklab,
          frigt            TYPE mard-vklab,
          vat              TYPE mard-vklab,
*        avat     type mard-vklab,
          avat             TYPE mard-vklab,
          y029             TYPE mard-vklab,
          y007             TYPE mard-vklab,
          y008             TYPE mard-vklab,
          ybrd             TYPE mard-vklab,
          zprb             TYPE mard-vklab,
          zprb1            TYPE mard-vklab,
          tcs              TYPE mard-vklab,
          stb              TYPE mard-vklab,
          st               TYPE mard-vklab,
          est              TYPE mard-vklab,
          sst              TYPE mard-vklab,
          btvalue          TYPE mard-vklab,
          total            TYPE mard-vklab,
          ybsd             TYPE mard-vklab,
          round            TYPE mard-vklab,
*---------Added by Abidulla for GST,Plant state,customer GSTIN
          sgst             TYPE mard-vklab,
          cgst             TYPE mard-vklab,
          igst             TYPE mard-vklab,
          ugst             TYPE mard-vklab,
          ccess            TYPE mard-vklab,
          bezei1           TYPE t005u-bezei,
          stcd3            TYPE kna1-stcd3,
          sgstr            TYPE konv-kbetr,
          cgstr            TYPE konv-kbetr,
          igstr            TYPE konv-kbetr,
          ugstr            TYPE konv-kbetr,
          ccessr           TYPE konv-kbetr,
*-----------End by Abidulla
          total_tax_amount TYPE mard-vklab,
          kbetr            LIKE konv-kbetr,
          mcod1            TYPE skat-mcod1,
          j_1ilstno        TYPE j_1imocust-j_1ilstno, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          vtext            TYPE tvfkt-vtext,
          belnr            TYPE bseg-belnr,
*       accesble_val type mard-VKLAB,
          auart            TYPE auart, " Sales Document Type
          bezei            TYPE bezei20, " Sales Document Text
          j_1ichid         TYPE j_1ichid, " Chapter ID
*        TAX_NO(20) type c,   "INV_TAX_NOS-TAX_NO,
*       TAX_NO type ZINV_TAX_NOS-TAX_NO,
        END OF ty_sales.

DATA : it_sales  TYPE TABLE OF ty_sales.
*       ls_sales  TYPE ty_sales,
*       it_sales1 TYPE TABLE OF ty_sales,
*       ls_sales1 TYPE ty_sales.

DATA : BEGIN OF itab_header OCCURS 0,
         name(40) TYPE c,
       END OF itab_header.

* Internal Table for KNA1
DATA : BEGIN OF it_konv OCCURS 0,
         knumv LIKE konv-knumv,   " Number of the document condition
         kposn LIKE konv-kposn,
         kwert LIKE konv-kwert,   " Condition value
         krech LIKE konv-krech,
         kschl LIKE konv-kschl,   " Condition types
*       kWERT LIKE konv-kWERT,   " VALUE
         mwsk1 LIKE konv-mwsk1,   " Sales Tax Code
         sakn1 LIKE konv-sakn1,   " Gl aCCOUNT
         kbetr LIKE konv-kbetr,
       END OF it_konv.

* Internal Table for BKPF
TYPES : BEGIN OF t_bkpf ,

          bukrs LIKE bkpf-bukrs,    " Comapny Code
          belnr LIKE bkpf-belnr,    " Accounting Document Number
          gjahr LIKE bkpf-gjahr,    " Fiscal Year
          waers LIKE bkpf-waers,

*        hwae2 LIKE bkpf-hwae2,

          awkey LIKE bkpf-awkey,    " Object key
          awtyp LIKE bkpf-awtyp,    " Reference procedure

        END OF t_bkpf.


DATA : it_bkpf TYPE STANDARD TABLE OF t_bkpf INITIAL SIZE 0 WITH HEADER LINE.
.
DATA : it_makt TYPE STANDARD TABLE OF makt INITIAL SIZE 0 WITH HEADER LINE.
DATA : it_j1ix TYPE STANDARD TABLE OF j_1iexchdr INITIAL SIZE 0 WITH HEADER LINE.


* Internal Table for BSEG
TYPES : BEGIN OF t_bseg ,
          bukrs LIKE bseg-bukrs,  " Comapany Code
          belnr LIKE bseg-belnr,  " Document Number
          gjahr LIKE bseg-gjahr,  " Fiscal year
          buzei LIKE bseg-buzei,
          buzid LIKE bseg-buzid,  " Identification of the Line Item
          shkzg LIKE bseg-shkzg,  " Debit/Credit
          gsber LIKE bseg-gsber,  " Business Area
*       pswsl LIKE bseg-pswsl,  " Update Currency for GL Trans Fig
          hkont LIKE bseg-hkont,  " G/L Account Number
          matnr LIKE bseg-matnr,  " Material number
          dmbe2 LIKE bseg-dmbe2,  " Amount in Second Local Currency
          dmbtr LIKE bseg-dmbtr,  " Local currency
          wrbtr LIKE bseg-wrbtr,  " Document Currency
        END OF t_bseg.


DATA : it_bseg TYPE STANDARD TABLE OF t_bseg INITIAL SIZE 0 WITH HEADER LINE.


*DATA : it_bseg1 TYPE STANDARD TABLE OF t_bseg INITIAL SIZE 0 WITH HEADER LINE.

* Internal Table for BSET


TYPES : BEGIN OF t_bset ,
          bukrs LIKE bset-bukrs,  " Comapany Code
          belnr LIKE bset-belnr,  " Document Number
          gjahr LIKE bset-gjahr,  " Fiscal year
          fwste LIKE bset-fwste,
          h2ste LIKE bset-h2ste,
        END OF t_bset.


DATA : it_bset TYPE STANDARD TABLE OF t_bset INITIAL SIZE 0 WITH HEADER
LINE.

TYPES : BEGIN OF st_j_1imocust,
          kunnr     TYPE j_1imocust-kunnr, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          j_1ilstno TYPE j_1imocust-j_1ilstno, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        END OF st_j_1imocust.

DATA : it_j_1imocust TYPE TABLE OF st_j_1imocust.
DATA : wa_j_1imocust TYPE st_j_1imocust.

TYPES : BEGIN OF st_skat,
          saknr TYPE skat-saknr,
          txt20 TYPE skat-txt20,
        END OF st_skat.

DATA : it_skat TYPE TABLE OF st_skat.
DATA : wa_skat TYPE st_skat.

TYPES : BEGIN OF st_tvfkt,
          fkart TYPE tvfkt-fkart,
          vtext TYPE tvfkt-vtext,
        END OF st_tvfkt.

DATA : it_tvfkt TYPE TABLE OF st_tvfkt,
       wa_tvfkt TYPE st_tvfkt.
*       it_taxinv TYPE TABLE OF ZINV_TAX_NOS,
*       wa_taxinv type ZINV_TAX_NOS.

DATA : str_file TYPE string,
       v_butxt  TYPE t001-butxt,
       v_name1  TYPE t001w-name1..

TYPES : BEGIN OF st_vbfa,
          vbeln TYPE vbfa-vbeln,
          vbelv TYPE vbfa-vbelv,
        END OF st_vbfa.

TYPES : BEGIN OF st_vbak,
          vbeln TYPE vbak-vbeln,        " Sales Document
          auart TYPE vbak-auart,        " Sales Document Type
          bezei TYPE tvakt-bezei,       " Sales Doc Desc
        END OF st_vbak.

DATA : it_vbfa TYPE TABLE OF st_vbfa.
DATA : it_vbak TYPE TABLE OF st_vbak.
DATA : wa_vbfa TYPE st_vbfa.


TYPES : BEGIN OF st_j_1irg23d,
          vbeln     TYPE  j_1irg23d-vbeln,
          posnr     TYPE  j_1irg23d-posnr,
          depexnum  TYPE  j_1irg23d-depexnum,
          exbed     TYPE  j_1irg23d-exbed,
          ecs       TYPE  j_1irg23d-ecs,
          exaddtax1 TYPE  j_1irg23d-exaddtax1,
          exaed     TYPE  j_1irg23d-exaed,
        END OF st_j_1irg23d.

DATA : it_j_1irg23d TYPE TABLE OF st_j_1irg23d.
DATA : wa_j_1irg23d TYPE st_j_1irg23d.

TYPES : BEGIN OF st_bkpf1,
          awkey TYPE bkpf-awkey,
          belnr TYPE bkpf-belnr,
        END OF st_bkpf1.

DATA : it_bkpf1 TYPE TABLE OF st_bkpf1.
DATA : wa_bkpf1 TYPE st_bkpf1.

TYPES : BEGIN OF st_bkpf_temp,
          awkey TYPE bkpf-awkey,
        END OF st_bkpf_temp.

DATA : it_bkpf_temp TYPE TABLE OF st_bkpf_temp.
DATA : wa_bkpf_temp TYPE st_bkpf_temp.


TYPES : BEGIN OF st_bseg1,
          vbeln TYPE bseg-vbeln,
          belnr TYPE bseg-belnr,
        END OF st_bseg1.

DATA : it_bseg1 TYPE TABLE OF st_bseg1.
DATA : wa_bseg1 TYPE st_bseg1.

*----Added by Samsudeen M ----*
TYPES: BEGIN OF ty_plant,
         werks TYPE werks_d,
         regio TYPE regio,
       END OF ty_plant.
DATA: gt_plant TYPE TABLE OF ty_plant,
      gs_plant TYPE ty_plant.


CONSTANTS : c_tcode_sales  TYPE sy-tcode VALUE 'ZSD01'.
CONSTANTS : c_tcode_traiff TYPE sy-tcode VALUE 'ZTARIFF'.
CONSTANTS : c_active TYPE boolean VALUE 'X'.
CONSTANTS : c_inactive TYPE boolean VALUE space.

FIELD-SYMBOLS <gfs_sales> LIKE LINE OF it_sales.
FIELD-SYMBOLS <gfs_vbak> LIKE LINE OF it_vbak.
