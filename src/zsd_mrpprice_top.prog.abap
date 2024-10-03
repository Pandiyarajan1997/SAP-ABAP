*&---------------------------------------------------------------------*
*& Include          ZSD_MRPPRICE_TOP
*&---------------------------------------------------------------------*
** Cust.Region/Material **

TYPES: BEGIN OF ty_a832,
         kappl TYPE kappl,
         kschl TYPE kscha,
         regio TYPE regio,
         matnr TYPE matnr,
         datbi TYPE kodatbi,
         datab TYPE kodatab,
         knumh TYPE knumh,
       END OF ty_a832.
** Internal table for A832  **
DATA: gt_a832 TYPE TABLE OF ty_a832,
      gs_a832 TYPE ty_a832.
** Sales conditions structure for MRP pricing **
** Internal table for a832  **
DATA: gt_a055  TYPE STANDARD TABLE OF a055,
      gs_a055  TYPE a055,
      gt1_a055 TYPE STANDARD TABLE OF a055,
      gs1_a055 TYPE a055,
      gt2_a055 TYPE STANDARD TABLE OF a055,
      gs2_a055 TYPE a055,
      gt3_a055 TYPE STANDARD TABLE OF a055,
      gs3_a055 TYPE a055.
** Conditions (Item) Prices  **
TYPES: BEGIN OF ty_konp,
         knumh TYPE knumh,
         kappl TYPE kappl,
         kschl TYPE kscha,
         kbetr TYPE kbetr_kond,
         konwa TYPE konwa,
         kmein TYPE kmein,
       END OF ty_konp.
** Internal table for konp **
DATA: gt_konp  TYPE TABLE OF ty_konp,
      gt_konp1 TYPE TABLE OF ty_konp,
      gt_konp2 TYPE TABLE OF ty_konp,
      gs_konp  TYPE ty_konp,
      gs_konp1 TYPE ty_konp,
      gs_konp2 TYPE ty_konp,
      gt_konp3 TYPE TABLE OF ty_konp,
      gs_konp3 TYPE ty_konp,
      gt_konp4 TYPE TABLE OF ty_konp,
      gs_konp4 TYPE ty_konp.
** ALV Display Structures **
TYPES: BEGIN OF ty_alv,
         matnr    TYPE matnr,
         vkorg    TYPE vkorg,
         plant    TYPE plant,
         dpl      TYPE kbetr_kond,
         matgrp   TYPE matkl,
         percen   TYPE kgcper,
         old_pri  TYPE kbetr_kond,
         price    TYPE kbetr_kond,
         uom      TYPE kmein,
         msg(150) TYPE c,
       END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.
** MAra Structure **
TYPES: BEGIN OF ty_mara,
         matnr       TYPE matnr,
         zmrp_percen TYPE zkgcper,
       END OF ty_mara.
DATA: gt_mara TYPE TABLE OF ty_mara,
      gs_mara TYPE ty_mara.
** Internal Table For Material Group **
DATA: gt_matper TYPE STANDARD TABLE OF zmatgrp_perc,
      gs_matper TYPE zmatgrp_perc.
** Internal table for ranges of region **
DATA: lt_regio TYPE RANGE OF kna1-regio,
      ls_regio LIKE LINE OF lt_regio.

DATA: gt_a304 TYPE STANDARD TABLE OF a304,
      gs_a304 TYPE a304.

DATA: lt_vkorg TYPE RANGE OF knvp-vkorg,
      ls_vkorg LIKE LINE OF lt_vkorg.
** Internal table for ranges of plant **
DATA: lt_werks  TYPE RANGE OF marc-werks,
      ls_werks  LIKE LINE OF lt_werks,
      lt_werks1 TYPE RANGE OF marc-werks,
      ls_werks1 LIKE LINE OF lt_werks1,
      lt_werks2 TYPE RANGE OF marc-werks,
      ls_werks2 LIKE LINE OF lt_werks2.
** PR00 AND PI01 Price Variable **
DATA: lv_pri_pr00(13) TYPE c,  "PR00 Price
      lv_pri_pi01(13) TYPE c.  "PI01 Price
DATA: lv_price(13)    TYPE c,  "Calculation Price
      lv_calprice(13) TYPE c,  "Final Price
      lv_final        TYPE i,  "Final Price
      lv_uom          TYPE kmein,
      lv_uom1          TYPE kmein.

** Internal Table for BDC **
DATA: wa_bdcdata TYPE bdcdata,
      it_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gs_bdcmsg  TYPE bdcmsgcoll.

DATA: lv_datum(10) TYPE c.
DATA: lv_org TYPE vkorg.
DATA: lv_werks TYPE werks_d.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.
