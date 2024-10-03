FUNCTION zbapi_stk_agewise_dms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BUKRS) TYPE  BUKRS DEFAULT 'DMS1'
*"     VALUE(IR_KUNNR) TYPE  KUNNR_TTY
*"     VALUE(IV_BLDAT) TYPE  BLDAT DEFAULT SY-DATUM
*"     VALUE(IV_BATCH) TYPE  XFELD OPTIONAL
*"  EXPORTING
*"     VALUE(GT_BATCH) TYPE  ZTT_STK_AGEWISE_DMS_WIBATCH
*"----------------------------------------------------------------------

  DATA : lv_dummy01(50) TYPE c.
  DATA: ls_final TYPE ztt_stk_agewise_dms  WITH HEADER LINE.
  FIELD-SYMBOLS: <ls_final> TYPE zst_stk_agewise_dms.
  DATA:ir_bukrs    TYPE /accgo/tt_app_r_bukrs,
       range_kunnr TYPE rsdsselopt_t.
  DATA:lo_data     TYPE REF TO data,
       g_gen_table TYPE REF TO data,
       lo_dref     TYPE REF TO data,
       lt_fcat     TYPE lvc_t_fcat,
       l_fieldname TYPE fieldname,
       gt_final    TYPE STANDARD TABLE OF zst_stk_agewise_dms.

  IF iv_batch = abap_true.
    DATA(wi_batch) = 'X'.
  ELSE.
    DATA(wo_batch) = 'X'.
  ENDIF.

  ir_bukrs = VALUE #( ( sign = 'I' opt = 'EQ' low = iv_bukrs  ) ).   "Obligatory Default

  range_kunnr = VALUE #( FOR ls_kunnr IN ir_kunnr
                          ( sign = if_fsbp_const_range=>sign_include
                            option = if_fsbp_const_range=>option_equal
                            low = ls_kunnr-kunnr )  ).

  cl_salv_bs_runtime_info=>set( EXPORTING  display  = abap_false  metadata = abap_true data  = abap_true ).

  SUBMIT zstk_agewise_report_dms WITH so_bukrs IN ir_bukrs
                                 WITH so_kunnr IN range_kunnr
                                 WITH p_date EQ iv_bldat
                                 WITH wi_batch EQ wi_batch
                                 WITH wo_batch EQ wo_batch
                                 AND RETURN.

  IMPORT  gt_final = gt_final FROM MEMORY ID 'ITAB'.
  FREE MEMORY ID 'ITAB'.


  IF iv_batch = abap_true.
*    gt_wibatch = CORRESPONDING #( BASE ( gt_wibatch ) gt_final  ).
    gt_batch  = CORRESPONDING #( gt_final MAPPING customer_number = dist
                                                   name = dist_name
                                                   plant = werks
                                                   plant_name = name1
                                                   Material_Number  = matnr
                                                   Material_Description  = maktx
                                                   Material_type  = mtart
                                                   Batch_Number  = charg
                                                   Created_On  = ersda
                                                   Volume  = volum
                                                   stock_qty  = clabs
                                                   Base_Unit  = meins
                                                   Unit_Price  = price
                                                   Stock_Value_Unrestricted  = stockvalue
                                                   stock_qty_unrestricted_ltrs  = vclabs
                                                   Price_in_Ltrs  = stockvalue1
                                                   lt_30_Days_Case  = zage1
                                                   lt_30_Days_Stock_Value  = stockcase1
                                                   lt_30_Stock_Qty_Ltrs  = vzage1
                                                   31_45_Days_Case  = zage2
                                                   31_45_Days_Stock_Value  = stockcase2
                                                   31_45_Stock_Qty_Ltrs  = vzage2
                                                   46_60_Days_Case  = zage3
                                                   46_60_Days_Stock_Value  = stockcase3
                                                   46_60_Stock_Qty_Ltrs  = vzage3
                                                   61_75_Days_Case  = zage4
                                                   61_75_Days_Stock_Value  = stockcase4
                                                   61_75_Stock_Qty_Ltrs  = vzage4
                                                   76_90_Days_Case  = zage5
                                                   76_90_Days_Stock_Value  = stockcase5
                                                   76_90_Stock_Qty_Ltrs  = vzage5
                                                   91_120_Days_Case  = zage6
                                                   91_120_Days_Stock_Value  = stockcase6
                                                   91_120_Stock_Qty_Ltrs  = vzage6
                                                   121_150_Days_Case =   zage7
                                                   121_150_Days_Stock_Value  = stockcase7
                                                   121_150_Stock_Qty_Ltrs  = vzage7
                                                   151_180_Days_Case  = zage8
                                                   151_180_Days_Stock_Value  = stockcase8
                                                   151_180_Stock_Qty_Ltrs  = vzage8
                                                   gt_180_Days_Case  = zage9
                                                   gt_180_Days_Stock_Value  = stockcase9
                                                   gt_180_Stock_Qty_Ltrs  = vzage9   ).

  ELSEIF wo_batch = abap_true.
*     gt_wobatch = CORRESPONDING #( BASE ( gt_wobatch ) gt_final  ).
    gt_batch  = CORRESPONDING #( gt_final MAPPING customer_number = dist
                                                    name = dist_name
                                                    plant = mwerks
                                                    plant_name = name1
                                                    Material_Number	=	mmatnr
                                                    Material_Description  = maktx
                                                    Material_type	=	mtart
                                                    Created_On  = mersda
                                                    Volume  = volum
                                                    stock_qty = labst
                                                    Base_Unit = meins
                                                    Unit_Price  = price
                                                    stock_value_unrestricted = stockvalue
                                                    stock_qty_unrestricted_ltrs = mlabst
                                                    price_in_ltrs  = stockvalue1
                                                    lt_30_Days_Case	=	zage1
                                                    lt_30_Days_Stock_Value  = stockcase1
                                                    lt_30_Stock_Qty_Ltrs  = vzage1
                                                    31_45_Days_Case	=	zage2
                                                    31_45_Days_Stock_Value  = stockcase2
                                                    31_45_Stock_Qty_Ltrs  = vzage2
                                                    46_60_Days_Case	=	zage3
                                                    46_60_Days_Stock_Value  = stockcase3
                                                    46_60_Stock_Qty_Ltrs  = vzage3
                                                    61_75_Days_Case	=	zage4
                                                    61_75_Days_Stock_Value  = stockcase4
                                                    61_75_Stock_Qty_Ltrs  = vzage4
                                                    76_90_Days_Case	=	zage5
                                                    76_90_Days_Stock_Value  = stockcase5
                                                    76_90_Stock_Qty_Ltrs  = vzage5
                                                    91_120_Days_Case  = zage6
                                                    91_120_Days_Stock_Value	=	stockcase6
                                                    91_120_Stock_Qty_Ltrs	=	vzage6
                                                    121_150_Days_Case	=	zage7
                                                    121_150_Days_Stock_Value  = stockcase7
                                                    121_150_Stock_Qty_Ltrs  = vzage7
                                                    151_180_Days_Case	=	zage8
                                                    151_180_Days_Stock_Value  = stockcase8
                                                    151_180_Stock_Qty_Ltrs  = vzage8
                                                    gt_180_Days_Case  = zage9
                                                    gt_180_Days_Stock_Value	=	stockcase9
                                                    gt_180_Stock_Qty_Ltrs	=	vzage9 ).
  ENDIF.


ENDFUNCTION.
