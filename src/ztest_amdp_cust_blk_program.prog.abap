*&---------------------------------------------------------------------*
*& Report ztest_amdp_cust_blk_program
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_amdp_cust_blk_program.

*zcl_amdp_cust_blk=>get_data(
*  EXPORTING
*    iv_client   = sy-mandt
*    iv_bukrs    = '1000'
*    iv_vkorg    = '1000'
*  IMPORTING
*    it_blk_data = data(lt_result)
*).
*
*
*if lt_result is not initial.
*
*endif.
tables:mchb.
DATA : lv_bukrs TYPE t001k-bukrs,
       lv_werks TYPE t001w-werks,
       lv_matnr TYPE mara-matnr,
       lv_mtart TYPE mara-mtart,
       lv_kunnr TYPE kna1-kunnr,
       lv_lgort TYPE mchb-lgort,
       lv_spart      TYPE mara-spart.
 TYPES: BEGIN OF gs_mara,
         matnr TYPE mara-matnr,              " Material Code
         mtart TYPE mara-mtart,              "CHANGES ON 20/09/2014
         matkl TYPE mara-matkl,              " Material Group
         meins TYPE mara-meins,              " UOM
         spart TYPE mara-spart,              " Division
         volum TYPE mara-volum,               " Volume
         xchpf TYPE mara-xchpf,              "cHANGES ON 20/09/2014
       END OF gs_mara.

DATA: gt_mara TYPE TABLE OF gs_mara,
      wa_mara TYPE gs_mara.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-101.
  SELECT-OPTIONS : so_bukrs FOR lv_bukrs OBLIGATORY DEFAULT 'DMS1',
                   so_werks FOR lv_werks,
                   so_matnr FOR lv_matnr,
                   so_mtart FOR lv_mtart,
                   s_charg FOR mchb-charg MODIF ID a,
                   so_lgort FOR lv_lgort MODIF ID b,
                   so_spart FOR lv_spart,
                   so_kunnr FOR lv_kunnr.

  PARAMETERS : p_date TYPE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN: END OF BLOCK b1.

 SELECT
      matnr
      mtart
      matkl
      meins
      spart
      volum
      xchpf FROM mara INTO TABLE gt_mara
      WHERE  matnr IN so_matnr AND spart IN so_spart AND xchpf = 'X' AND mtart IN so_mtart.

zcl_stk_age_ctb=>fetch_data(
  EXPORTING
    iv_client   = sy-mandt
    it_mara     = gt_mara
  IMPORTING
    it_stk_data = data(lt_data)
).
