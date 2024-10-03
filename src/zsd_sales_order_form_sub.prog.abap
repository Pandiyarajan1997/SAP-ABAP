*&---------------------------------------------------------------------*
*& Include          ZSD_SALES_ORDER_FORM_SUB
*&---------------------------------------------------------------------*
FORM get_data.
** Sales order Data from Header Table **
  REFRESH: gt_vbak.
  SELECT vbeln
         erdat
         kunnr FROM vbak
               INTO TABLE gt_vbak
               WHERE vbeln = p_vbeln.
  IF gt_vbak[] IS NOT INITIAL.
    SORT gt_vbak[] BY vbeln.
** Based on Header getting Item Details **
    REFRESH gt_vbap.
    SELECT vbeln
           posnr
           matnr
           arktx
           meins
           netwr
           kwmeng FROM vbap
                  INTO TABLE gt_vbap
                  FOR ALL ENTRIES IN gt_vbak
                  WHERE vbeln = gt_vbak-vbeln.
    IF sy-subrc EQ 0.
      SORT gt_vbap[] BY vbeln posnr.
    ENDIF.
** Based Header Sold to party getting customer Details **
    REFRESH gt_kna1.
    SELECT kunnr
           name1
           stras
           ort01
           land1
           pstlz
           telf1 FROM kna1
                 INTO TABLE gt_kna1
                 FOR ALL ENTRIES IN gt_vbak
                 WHERE kunnr = gt_vbak-kunnr.
    IF sy-subrc EQ 0.
      SORT gt_kna1[] BY kunnr.
    ENDIF.
  ENDIF.
ENDFORM.
FORM process_data.
  LOOP AT gt_vbak INTO gs_vbak WHERE vbeln = p_vbeln.
    CLEAR gs_vbap.
    READ TABLE gt_vbap INTO gs_vbap WITH KEY vbeln = gs_vbak-vbeln.
    IF sy-subrc EQ 0.
      gs_item-posnr = gs_vbap-posnr.
      gs_item-matnr = gs_vbap-matnr.
      gs_item-arktx = gs_vbap-arktx.
      gs_item-meins = gs_vbap-meins.
      gs_item-netwr = gs_vbap-netwr.
      gs_item-kwmeng = gs_vbap-kwmeng.
    ENDIF.
    gs_header-vbeln = gs_vbak-vbeln.
    gs_header-erdat = gs_vbak-erdat.
    CLEAR gs_kna1.
    READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = gs_vbak-kunnr.
    IF sy-subrc EQ 0.
      gs_header-kunnr = gs_kna1-kunnr.
      gs_header-name1 = gs_kna1-name1.
      gs_header-stras = gs_kna1-stras.
      gs_header-ort01 = gs_kna1-ort01.
      gs_header-land1 = gs_kna1-land1.
      gs_header-pstlz = gs_kna1-pstlz.
      gs_header-telf1 = gs_kna1-telf1.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM call_form.
ENDFORM.
