*&---------------------------------------------------------------------*
*& Report  ZZ_TEST_BAPI_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_sku_bapi_create_order.


INCLUDE z_sku_bapi_create_order_top.

DATA: lr_mara TYPE RANGE OF mara-matnr,
      lw_mara LIKE LINE OF lr_mara.

DATA: lr_kunnr TYPE RANGE OF ymard_nsap-kunnr,
      lw_kunnr LIKE LINE OF lr_kunnr.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE t001.
  SELECT-OPTIONS: so_kunnr FOR ymard_nsap-kunnr.
  PARAMETERS    : p_date   TYPE ymard_nsap-erdat.
SELECTION-SCREEN: END OF BLOCK blk1.


* Start-of-selection.
START-OF-SELECTION.

  IF p_date IS INITIAL.
    p_date = sy-datum.
  ENDIF.

*Get all Materials with Deletion flag
  SELECT matnr FROM mara INTO TABLE gt_mara WHERE lvorm = 'X'.
  IF sy-subrc = 0.
    REFRESH lr_mara.
    LOOP AT gt_mara INTO gs_mara.
      lw_mara-sign = 'I'.
      lw_mara-option = 'EQ'.
      lw_mara-low = gs_mara-matnr.
      APPEND lw_mara TO lr_mara.
      CLEAR lw_mara.
    ENDLOOP.
  ENDIF.

*Select all blocked customers to be ignored from program
  SELECT kunnr
    FROM knvv
    INTO TABLE gt_kunnr
   WHERE vkorg = '1000' and loevm = 'X'.

  SELECT kunnr
   FROM kna1
   APPENDING TABLE gt_kunnr
  WHERE aufsd <> Space.

  IF gt_kunnr[] IS NOT INITIAL.

    SORT gt_kunnr BY kunnr.
    DELETE ADJACENT DUPLICATES FROM gt_kunnr COMPARING kunnr.

    REFRESH lr_kunnr.
    LOOP AT gt_kunnr INTO gs_kunnr.
      lw_kunnr-sign = 'I'.
      lw_kunnr-option = 'EQ'.
      lw_kunnr-low = gs_kunnr-kunnr.
      APPEND lw_kunnr TO lr_kunnr.
      CLEAR lw_kunnr.
    ENDLOOP.

  ENDIF.

*Selection DMS Stock from YMARD_NSAP Based on customer & Date
  SELECT *
   FROM ymard_nsap
   INTO TABLE i1_ymard
  WHERE kunnr IN so_kunnr
    AND erdat EQ p_date . " .  ""MATNR = 'ENAFT593E1'  .

* to delete all the materials from the list which are markedt for deletion
  IF i1_ymard[] IS NOT INITIAL AND lr_mara[] IS NOT INITIAL.
    DELETE i1_ymard WHERE matnr IN lr_mara.
  ENDIF.

* to delete all the distributor from the list which are marked for deletion
  IF i1_ymard[] IS NOT INITIAL AND lr_kunnr[] IS NOT INITIAL.
    DELETE i1_ymard WHERE kunnr IN lr_kunnr.
  ENDIF.

  IF i1_ymard[] IS NOT INITIAL.
*Selecting Material, Division and Payment Terms Details from Mara based on DMS Stock table
    SELECT
      matnr
      spart
      zterm
       FROM mara INTO TABLE i1_mara FOR ALL ENTRIES IN i1_ymard WHERE matnr = i1_ymard-matnr .

* Select Customer No, Sales Org, Distribution Channel, Division and Sales office From Customer Master table
    SELECT kunnr
           vkorg
           vtweg
           spart
           vkbur
       FROM knvv INTO TABLE i1_knvv FOR ALL ENTRIES IN i1_ymard WHERE kunnr = i1_ymard-kunnr AND vkorg = '1000' .

* Select Cutomer, Material and ROL Value
    SELECT
       kunnr
       matnr
       new_val
      FROM zcustmat_rol INTO TABLE i_rol FOR ALL ENTRIES IN i1_ymard WHERE kunnr = i1_ymard-kunnr AND matnr = i1_ymard-matnr .

* Select customer Materail and Rol Value for Customer and all Materials
    SELECT
     kunnr
     matnr
     new_val
    FROM zcustmat_rol INTO TABLE I1_ROl FOR ALL ENTRIES IN i1_ymard WHERE ( kunnr EQ i1_ymard-kunnr AND matnr NE i1_ymard-matnr ) .

*delete data from list where materials are flagged for deletion
    IF lr_mara[] is NOT INITIAL.
      delete I1_ROl WHERE matnr in lr_mara.
    ENDIF.

*Selecting Material, Division and Payment Terms Details from Mara based on ROL 2 table
    SELECT
    matnr
    spart
    zterm
     FROM mara INTO TABLE i2_mara FOR ALL ENTRIES IN i1_rol WHERE matnr = i1_rol-matnr .

*Select Doc no, Delivery Status <> Overall COmpleted (C) and SD Document Category = C (Orders)
    SELECT
           vbeln
           lfgsk
           vbtyp
*               FROM VBUK "Commented by SPLABAP during code remediation
               FROM vbak  "Added by SPLABAP during code remediation
      INTO TABLE i_vbuk WHERE lfgsk <> 'C' AND vbtyp = 'C' .

    IF i_vbuk IS NOT INITIAL.
**Select Sales Doc no, Customer No from VBAK for all not completed documents (All Open Orders)
      SELECT
        vbeln
        kunnr
          FROM vbak INTO TABLE i_vbak FOR ALL ENTRIES IN i_vbuk WHERE vbeln = i_vbuk-vbeln.

* Select Sales details for all open orders
      SELECT
        vbeln
        posnr
        matnr
        spart
        kwmeng
        werks
        erdat
          FROM vbap INTO TABLE i_vbap FOR ALL ENTRIES IN i_vbuk WHERE vbeln = i_vbuk-vbeln AND erdat >= '20200501'." '20191001' .  "AND SY_DATE > ERDAT.
      "added on 13/7

      SELECT  vbelv
               posnv
               vbeln
               posnn
               vbtyp_n  FROM vbfa INTO TABLE it_vbfa FOR ALL ENTRIES IN i_vbap WHERE vbelv = i_vbap-vbeln AND posnv = i_vbap-posnr AND vbtyp_n = 'M' ." AND POSNN = 1 .
*          IF IT_VBFA IS NOT INITIAL.
      SELECT vbeln
           fkart
           fkdat
           fksto
           date_of_delivery
           remarks FROM vbrk INTO TABLE it_vbrk FOR ALL ENTRIES IN it_vbfa WHERE vbeln = it_vbfa-vbeln  AND fksto <> 'X'.

      SELECT  vbeln
              posnr
              fkimg
              netwr
              aubel
              aupos FROM vbrp INTO TABLE i_vbrp FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.

    ENDIF. "I_VBUK

    LOOP AT i_rol INTO w_rol.
      DELETE I1_ROl WHERE kunnr = w_rol-kunnr AND matnr = w_rol-matnr.
      CLEAR w_rol.
    ENDLOOP.

    SELECT COUNT(*) INTO i_count FROM ymard_elog .

    w_count = i_count + 1.

    SORT i_vbap BY vbeln.
    SORT i_vbak BY vbeln.

* collec all open orders from VBAP and combine with customer no into itab I_OPEN
    LOOP AT i_vbap INTO w_vbap.
      MOVE-CORRESPONDING w_vbap TO w_open.

      READ TABLE i_vbak INTO w_vbak WITH KEY vbeln = w_open-vbeln BINARY SEARCH .
      IF  sy-subrc = 0.
        w_open-kunnr = w_vbak-kunnr.
      ENDIF.

      APPEND w_open TO i_open.
      CLEAR w_open.
    ENDLOOP.
    "added on 13/7

* Get the delivery quantity from the invoice details table
    LOOP AT i_open INTO w_open .
      LOOP AT i_vbrp INTO w_vbrp WHERE aubel EQ w_open-vbeln AND aupos EQ w_open-posnr .
        w_open-aft_par = w_open-aft_par + w_vbrp-fkimg.
      ENDLOOP.
      w_open-kwmeng = w_open-kwmeng - w_open-aft_par .
      MODIFY i_open FROM w_open TRANSPORTING kwmeng.
      CLEAR: w_open.
      CLEAR : w_vbrp.
    ENDLOOP.
    "ended on 13/7


    APPEND LINES OF i_open TO i1_open.

    SORT i1_ymard BY kunnr matnr.
    SORT i1_mara BY matnr.
    SORT i_rol BY kunnr matnr.

*Based on the dms stock and rol calculate the required Quantity
    LOOP AT i1_ymard INTO w1_ymard.
      MOVE-CORRESPONDING w1_ymard TO w1_final.
      READ TABLE i1_mara INTO w1_mara WITH KEY matnr = w1_final-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        w1_final-m_spart = w1_mara-spart.
        w1_final-zterm = w1_mara-zterm.
      ENDIF.

      READ TABLE i_rol INTO w_rol WITH KEY kunnr = w1_final-kunnr matnr = w1_final-matnr BINARY SEARCH.
      "  IF SY-SUBRC EQ 0.
      w1_final-aft_rol  = w_rol-new_val - ( w1_final-tot_stk - w1_final-pen_ord ) ."ADDED ON BY RAM 1/10/19
      CLEAR : w_rol.

      APPEND w1_final TO i1_final.
      CLEAR w1_final.
    ENDLOOP.

    "added by ram on 26/9

    SORT i2_mara BY matnr.
    SORT i1_rol BY kunnr matnr.

    LOOP AT i1_rol INTO w1_rol.

      wr_final-kunnr = w1_rol-kunnr .
      wr_final-matnr = w1_rol-matnr.
      wr_final-erdat = sy-datum .
      wr_final-erzet = sy-uzeit .
      wr_final-meins = 'EA' .
      wr_final-labst = 0 .
      wr_final-trame = 0 .
      wr_final-tot_stk = 0 .
      wr_final-pen_ord = 0 .
      wr_final-aft_rol = w1_rol-new_val .
      " WR_FINAL-AFT_OPEN = 0 .

      READ TABLE i2_mara INTO w2_mara WITH KEY matnr = wr_final-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        wr_final-m_spart = w2_mara-spart.
        wr_final-zterm = w2_mara-zterm.
      ENDIF.
      APPEND wr_final TO ir_final.
      CLEAR wr_final.
    ENDLOOP.

    APPEND LINES OF ir_final TO i1_final .

*for all the entires add the sales office, distribution channel division
    LOOP AT i1_knvv INTO w1_knvv.

      LOOP AT i1_final INTO w1_final WHERE kunnr = w1_knvv-kunnr AND m_spart = w1_knvv-spart .
        w3_final = w1_final.
        w3_final-vkbur = w1_knvv-vkbur.
        w3_final-c_che = w1_knvv-vtweg.
        w3_final-c_spart = w1_knvv-spart.
        APPEND: w3_final TO i3_final.
      ENDLOOP.
    ENDLOOP.

* clear the I1_Final Table
    REFRESH i1_final .

* append the details of the I3_Final to the I1_Final table
    APPEND LINES OF i3_final TO i1_final.
    DELETE: i1_final WHERE m_spart IS INITIAL.


    SORT : i_open BY kunnr matnr.

    DELETE ADJACENT DUPLICATES FROM i_open COMPARING kunnr matnr.

    REFRESH : i_open.

    SORT : i1_open BY kunnr matnr .

* For combining all the open stock for the customer and material
    LOOP AT i1_open INTO w1_open .
      IF w_open-kunnr = w1_open-kunnr AND w_open-matnr = w1_open-matnr .
        w_open-tot_open = w_open-tot_open + w1_open-kwmeng .
        w1_open-tot_open = w_open-tot_open .
        APPEND: w1_open TO i_open.
        CLEAR w1_open.
      ELSE.
        CLEAR: w_open.
        w_open = w1_open.
        w_open-tot_open = w1_open-kwmeng .
        w1_open-tot_open = w1_open-kwmeng .
        APPEND: w1_open TO i_open.
      ENDIF.
    ENDLOOP.


    SORT : i_open DESCENDING BY kunnr matnr tot_open.

    DELETE ADJACENT DUPLICATES FROM i_open COMPARING kunnr matnr .


    SORT i_open BY kunnr matnr.

    LOOP AT i1_final INTO w1_final .

      READ TABLE i_open INTO w_open WITH KEY kunnr = w1_final-kunnr matnr = w1_final-matnr BINARY SEARCH.
      IF sy-subrc = '0' .
        w1_final-aft_open = w1_final-aft_rol - w_open-tot_open .
      ELSE.
        w1_final-aft_open = w1_final-aft_rol .
      ENDIF.
      MODIFY i1_final FROM w1_final TRANSPORTING aft_open.
      CLEAR : w1_final ,w_open.
    ENDLOOP.

*Delete all entries where the order quantity is less than 0
    DELETE i1_final WHERE aft_open LE 0 .

    APPEND LINES OF i1_final TO i2_final.

    SORT : i2_final BY kunnr m_spart zterm .
    DELETE ADJACENT DUPLICATES FROM i2_final COMPARING kunnr m_spart zterm.


    SELECT kunnr
           vkorg
           vtweg
           spart
           parvw
           kunn2
           pernr
       FROM knvp
       INTO TABLE i1_knvp
       FOR ALL ENTRIES IN i2_final
      WHERE kunnr = i2_final-kunnr AND spart = i2_final-c_spart .
    IF sy-subrc = 0.
      SORT i1_knvp BY kunnr spart parvw.
    ENDIF.

    LOOP AT i2_final INTO w2_final .

      w2_final-sa_org = '1000' .
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'AG' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-sp_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'RE' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-bp_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'RG' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-py_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'WE' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-sh_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L1' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-so_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L2' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-asm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L3' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-rsm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L5' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-nsm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'BR' BINARY SEARCH.
      IF sy-subrc EQ '0'.
        w2_final-br_no = w1_knvp-pernr.
      ENDIF.

      MODIFY i2_final FROM w2_final TRANSPORTING sp_par bp_par py_par sh_par sa_org so_no asm_no rsm_no nsm_no br_no.
      CLEAR w2_final.

    ENDLOOP.
  ENDIF. " I1_YMARD

  DELETE i2_final WHERE aft_open LE 0 .

  LOOP AT i2_final INTO w2_final.
    REFRESH partner.
    CLEAR  header.
    CLEAR  header.
    CLEAR headerx.
    REFRESH item.
    REFRESH itemx.
    REFRESH return.
    REFRESH lt_schedules_in.
    REFRESH lt_schedules_inx.
    REFRESH condition.
    REFRESH conditionx.
    CLEAR gv_price_ok.

    PERFORM header_partner.

    CLEAR : item_no.

    LOOP AT i1_final INTO w1_final WHERE kunnr = w2_final-kunnr AND m_spart = w2_final-c_spart AND zterm = w2_final-zterm .

      w1_final-it_no = item_no + 1 .

      PERFORM lineitem.

      CLEAR : w1_final.
    ENDLOOP.

    PERFORM upload_saleorder.

    CLEAR : w1_final , w2_final .
  ENDLOOP.

  INCLUDE z_sku_bapi_create_ord_headf01.
