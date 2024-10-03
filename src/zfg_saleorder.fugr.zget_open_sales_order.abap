FUNCTION zget_open_sales_order.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  /ACCGO/CMN_TT_BUKRS OPTIONAL
*"     VALUE(KUNNR) TYPE  /ACCGO/TT_KUNNR OPTIONAL
*"  TABLES
*"      IT_OPEN_SO STRUCTURE  ZSTR_OPEN_SO
*"----------------------------------------------------------------------
  "Created by: Samsudeen M
  "Created on: 21.03.2023
  "Purpose: Fetching Open Sales Order Details
  "Reference by: Ramakrishnan J & Vimal Kumar
*-----------------------------------------------------------------------
  DATA: lr_bukrs TYPE RANGE OF vbak-bukrs_vf,
        lr_kunnr TYPE RANGE OF vbak-kunnr,
        lr_auart TYPE RANGE OF vbak-auart.

  REFRESH: lr_bukrs,lr_kunnr,lr_auart.
  IF bukrs IS NOT INITIAL.
    LOOP AT bukrs INTO DATA(lw_bukrs).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lw_bukrs-bukrs ) TO lr_bukrs.
    ENDLOOP.
  ENDIF.

  IF kunnr IS NOT INITIAL.
    LOOP AT kunnr INTO DATA(lw_kunnr).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lw_kunnr-customer ) TO lr_kunnr.
    ENDLOOP.
  ENDIF.

*---- Exclusion of Sales order type ------*
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
           WHERE name ='OPEN_SALES_ORDER_BAPI'
           AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO DATA(lw_tvarvc).
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = lw_tvarvc-low ) TO lr_auart.
    ENDLOOP.
  ENDIF.

  "Open Sales order fetching
  SELECT a~vbeln,
         a~auart,
         a~erdat AS created_date,
         a~bukrs_vf,
         a~vkorg,
         a~vtweg,
         a~spart,
         a~vkbur,
         a~kunnr,
         c~name1,
         b~posnr,
         b~matnr,
         b~arktx,
         b~kwmeng,
         b~netwr,
         b~werks,
         b~vrkme,
         a~cmgst AS credit_status,
*         CASE
*          WHEN cmgst = 'A'
*             THEN 'Credit check was executed, document OK'
*          WHEN cmgst = 'B'
*             THEN 'Credit check was executed, document not OK'
*          WHEN cmgst = 'C'
*             THEN 'Credit check was executed, document not OK, partial release'
*          WHEN cmgst = 'D'
*             THEN 'Document released by credit representative'
*          ELSE 'Credit check was not executed/Status not set'
*         END AS credit_stxt,
         b~gbsta AS status
*         CASE
*          WHEN gbsta = 'A'
*             THEN 'Not yet processed'
*         WHEN gbsta = 'B'
*             THEN 'Partially processed'
*         ELSE 'Not Relevant'
*         END AS status_txt
                 INTO TABLE @DATA(lt_sales_order)
                 FROM vbak AS a INNER JOIN vbap AS b
                 ON a~vbeln = b~vbeln
                 INNER JOIN kna1 AS c
                 ON a~kunnr = c~kunnr
                 WHERE a~auart NOT IN @lr_auart
                 AND a~bukrs_vf IN @lr_bukrs
                 AND a~kunnr IN @lr_kunnr
                 AND a~gbstk NE 'C'
                 AND b~gbsta NE 'C'.
  IF sy-subrc = 0.
    SORT lt_sales_order[] BY vbeln posnr.
* Fetching Open Quanatity of sales order
    SELECT vbeln,
           posnr,
           matnr,
           omeng FROM vbbe
                 INTO TABLE @DATA(lt_open_qty)
                 FOR ALL ENTRIES IN @lt_sales_order
                 WHERE vbeln = @lt_sales_order-vbeln
                 AND posnr = @lt_sales_order-posnr
                 AND matnr = @lt_sales_order-matnr.
    IF sy-subrc = 0.
      SORT lt_open_qty[] BY vbeln posnr.
    ENDIF.
  ENDIF.
  APPEND LINES OF lt_sales_order TO it_open_so.

  LOOP AT it_open_so ASSIGNING FIELD-SYMBOL(<fs_open_so>).
    DATA(lv_openqty) = VALUE #( lt_open_qty[ vbeln = <fs_open_so>-vbeln
                                             posnr = <fs_open_so>-posnr
                                             matnr = <fs_open_so>-matnr ]-omeng OPTIONAL ).
    <fs_open_so>-open_qty = lv_openqty.
    CLEAR lv_openqty.
  ENDLOOP.

ENDFUNCTION.
