*&---------------------------------------------------------------------*
*& Report  ZZ_TEST_BAPI_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_sku_bapi_create_order_new.

INCLUDE z_sku_bapi_create_order_n_top.
*INCLUDE Z_SKU_BAPI_CREATE_ORDER_TOP.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE t001.
  SELECT-OPTIONS: so_kunnr FOR ymard_nsap-kunnr.
  SELECT-OPTIONS: so_matnr FOR ymard_nsap-matnr.
  PARAMETERS    : p_date   TYPE ymard_nsap-erdat.
SELECTION-SCREEN: END OF BLOCK blk1.


* Start-of-selection.
START-OF-SELECTION.


*Selection DMS Stock from MARD Bases on todays Date
  SELECT ymard_nsap~MANDT
         ymard_nsap~KUNNR
         ymard_nsap~MATNR
         ymard_nsap~ERDAT
         ymard_nsap~ERZET
         ymard_nsap~MEINS
         ymard_nsap~LABST
         ymard_nsap~TRAME
         ymard_nsap~TOT_STK
         ymard_nsap~PEN_ORD
         mara~spart as m_spart
         mara~zterm
   FROM ymard_nsap INNER JOIN mara on ymard_nsap~matnr = mara~matnr
   INTO TABLE i1_ymard
  WHERE ymard_nsap~kunnr IN so_kunnr
    AND ymard_nsap~matnr IN so_matnr
    AND ymard_nsap~erdat EQ p_date . " .  ""MATNR = 'ENAFT593E1'  .

  IF i1_ymard[] IS NOT INITIAL.

*Selecting Material, Division and Payment Terms Details from Mara based on DMS Stock table
    REFRESH i1_ymard_tmp.
    i1_ymard_tmp[] = i1_ymard[].
    SORT i1_ymard_tmp BY matnr.
    DELETE ADJACENT DUPLICATES FROM i1_ymard_tmp COMPARING matnr.

    IF i1_ymard_tmp[] IS NOT INITIAL.
      SELECT matnr spart zterm
        FROM mara
        INTO TABLE i1_mara
        FOR ALL ENTRIES IN i1_ymard_tmp
      WHERE matnr = i1_ymard_tmp-matnr .
    ENDIF.


* Select Customer No, Sales Org, Distribution Channel, Division and Sales office From Customer Master table
    REFRESH i1_ymard_tmp.
    i1_ymard_tmp[] = i1_ymard[].
    SORT i1_ymard_tmp BY kunnr.
    DELETE ADJACENT DUPLICATES FROM i1_ymard_tmp COMPARING kunnr.

    IF i1_ymard_tmp[] IS NOT INITIAL.
      SELECT kunnr vkorg vtweg spart vkbur
        FROM knvv
        INTO TABLE i1_knvv
        FOR ALL ENTRIES IN i1_ymard_tmp
      WHERE kunnr = i1_ymard_tmp-kunnr AND vkorg = '1000' .
    ENDIF.

* Select Cutomer, Material and ROL Value
    SELECT kunnr matnr new_val
      FROM zcustmat_rol
      INTO TABLE i_rol
      FOR ALL ENTRIES IN i1_ymard
    WHERE kunnr = i1_ymard-kunnr AND matnr = i1_ymard-matnr .

* Select customer Materail and Rol Value for Customer and all Materials
    SELECT kunnr matnr new_val
      FROM zcustmat_rol
      INTO TABLE I1_ROl
      FOR ALL ENTRIES IN i1_ymard
    WHERE ( kunnr EQ i1_ymard-kunnr AND matnr NE i1_ymard-matnr ) .

*Selecting Material, Division and Payment Terms Details from Mara based on ROL 2 table
    SELECT matnr spart zterm
      FROM mara
      INTO TABLE i2_mara
      FOR ALL ENTRIES IN i1_rol
    WHERE matnr = i1_rol-matnr .

*Select Doc no, Delivery Status <> Overall COmpleted (C) and SD Document Category = C (Orders)
    REFRESH i1_ymard_tmp.
    i1_ymard_tmp[] = i1_ymard[].
    SORT i1_ymard_tmp BY kunnr.
    DELETE ADJACENT DUPLICATES FROM i1_ymard_tmp COMPARING kunnr.

    IF i1_ymard_tmp[] IS NOT INITIAL.
      SELECT vbeln kunnr lfgsk vbtyp
        FROM vbak  "Added by SPLABAP during code remediation
        INTO TABLE i_vbuk
        FOR ALL ENTRIES IN i1_ymard_tmp
       WHERE kunnr = i1_ymard_tmp-kunnr AND lfgsk <> 'C' AND vbtyp = 'C' .
    ENDIF.

    IF i_vbuk IS NOT INITIAL.

**Select Sales Doc no, Customer No from VBAK for all not completed documents (All Open Orders)
*      SELECT vbeln   "Sales Doc No
*             kunnr   "Customer No
*        FROM vbak
*        INTO TABLE i_vbak
*        FOR ALL ENTRIES IN i_vbuk
*      WHERE vbeln = i_vbuk-vbeln.

* Select Sales details for all open orders
      SELECT vbeln  "Sale Doc no
             posnr  "Item No
             matnr  "Material No
             spart  "Divison
             kwmeng "Cumulative order quantity in sales units
             werks  "Plant
             erdat  "Date on which the record was created
             kunnr_ana as KUNNR
        FROM vbap
        INTO TABLE i_vbap
        FOR ALL ENTRIES IN i_vbuk
        WHERE vbeln = i_vbuk-vbeln AND erdat >= '20200501'." '20191001' .  "AND SY_DATE > ERDAT.  "added on 13/7

      IF so_matnr[] IS NOT INITIAL.
        DELETE i_vbap WHERE matnr NOT IN so_matnr[].
      ENDIF.

      IF i_vbap[] IS NOT INITIAL.
        SELECT vbelv   "Sales Doc no
               posnv   "Item no
               vbeln   "Billing Document no
               posnn   "Billing Doc Item No
               vbtyp_n "Document flow indicator
          FROM vbfa
          INTO TABLE it_vbfa
          FOR ALL ENTRIES IN i_vbap
        WHERE vbelv = i_vbap-vbeln AND posnv = i_vbap-posnr AND vbtyp_n = 'M' .

        SELECT vbeln     "Billing Doc No
               fkart     "Billing type
               fkdat     "Billing Date
               fksto     "BIlling doc Cancelled indicator
               date_of_delivery "Delivery Date
               remarks          "Delivery Remarks
          FROM vbrk
          INTO TABLE it_vbrk
          FOR ALL ENTRIES IN it_vbfa
          WHERE vbeln = it_vbfa-vbeln  AND fksto <> 'X'.

        SELECT vbeln    "Billing Doc No
               posnr    "Billing Item no
               fkimg    "Actual Billed Quantity
               netwr    "Net Value
               aubel    "Sales Document No
               aupos    "Sales Doc Item No
          FROM vbrp
          INTO TABLE i_vbrp
          FOR ALL ENTRIES IN it_vbrk
         WHERE vbeln = it_vbrk-vbeln.
      ENDIF.
    ENDIF. "I_VBUK

    LOOP AT i_rol INTO w_rol.
      DELETE I1_ROl WHERE kunnr = w_rol-kunnr AND matnr = w_rol-matnr.
      CLEAR w_rol.
    ENDLOOP.

    SELECT COUNT(*) INTO i_count FROM ymard_elog .

    w_count = i_count + 1.


    SORT i_vbap by vbeln posnr.

    SORT i_vbuk by vbeln kunnr.

    Data: lv_tabix TYPE sy-tabix.

    MOVE-CORRESPONDING i_vbap TO i_open.
*
*    LOOP AT i_vbap INTO w_vbap.
*
*      MOVE-CORRESPONDING w_vbap TO w_open.
*
*      READ TABLE i_vbuk INTO w_vbuk WITH KEY vbeln = w_open-vbeln BINARY SEARCH.
*      IF  sy-subrc = 0.
*        w_open-kunnr = w_vbak-kunnr.
*      ENDIF.
*
*      APPEND w_open TO i_open.
*      CLEAR w_open.
*    ENDLOOP.

    sort i_open by vbeln posnr.
    SORT i_vbrp by aubel aupos.

    LOOP AT i_open INTO w_open .

      READ TABLE i_vbrp INTO w_vbrp with key aubel = w_open-vbeln aupos = w_open-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        clear lv_tabix.
        lv_tabix = sy-tabix.
      ELSE.
        CONTINUE.
      ENDIF.

      LOOP AT i_vbrp INTO w_vbrp FROM lv_tabix.    "WHERE aubel EQ w_open-vbeln AND aupos EQ w_open-posnr .

        IF w_vbrp-aubel = w_open-vbeln and w_vbrp-aupos = w_open-posnr.
          w_open-aft_par = w_open-aft_par + w_vbrp-fkimg.
        ELSE.
          exit.
        ENDIF.
      ENDLOOP.

      w_open-kwmeng = w_open-kwmeng - w_open-aft_par .
      MODIFY i_open FROM w_open TRANSPORTING kwmeng.

      CLEAR wa_open.
      MOVE-CORRESPONDING w_open to wa_open.
      COLLECT wa_open INTO lt_open.

      CLEAR: w_open.
      CLEAR : w_vbrp.
    ENDLOOP.
    "ended on 13/7

    APPEND LINES OF i_open TO i1_open.

    MOVE-CORRESPONDING i1_ymard TO i1_final.

    SORT i1_final by kunnr matnr.
    sort i_rol by kunnr matnr.

    LOOP AT i1_final INTO w1_final.
      CLEAR w_rol.
      READ TABLE i_rol INTO w_rol with key kunnr = w1_final-kunnr matnr = w1_final-matnr BINARY SEARCH.
**No sys-subrc check required as calculation should consider 0 ROL also

      w1_final-aft_rol  = w_rol-new_val - ( w1_final-tot_stk - w1_final-pen_ord ) .

      modify i1_final FROM w1_final.
    ENDLOOP.

*    LOOP AT i1_ymard INTO w1_ymard.
*      MOVE-CORRESPONDING w1_ymard TO w1_final.
*      READ TABLE i1_mara INTO w1_mara WITH KEY matnr = w1_final-matnr.
*      IF sy-subrc EQ 0.
*        w1_final-m_spart = w1_mara-spart.
*        w1_final-zterm = w1_mara-zterm.
*      ENDIF.
*
*      CLEAR w_rol.
*      READ TABLE i_rol INTO w_rol WITH KEY kunnr = w1_final-kunnr matnr = w1_final-matnr.
**No sys-subrc check required as calculation should consider 0 ROL also
*      w1_final-aft_rol  = w_rol-new_val - ( w1_final-tot_stk - w1_final-pen_ord ) ."ADDED ON BY RAM 1/10/19
*      CLEAR : w_rol.
*
*      APPEND w1_final TO i1_final.
*      CLEAR w1_final.
*    ENDLOOP.

    "added by ram on 26/9

    SORT i1_rol by matnr kunnr.
    sort i2_mara by matnr.

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

    LOOP AT i1_knvv INTO w1_knvv.

      LOOP AT i1_final INTO w1_final WHERE kunnr = w1_knvv-kunnr AND m_spart = w1_knvv-spart .
        w3_final = w1_final.
        w3_final-vkbur = w1_knvv-vkbur.
        w3_final-c_che = w1_knvv-vtweg.
        w3_final-c_spart = w1_knvv-spart.
        APPEND: w3_final TO i3_final.
      ENDLOOP.
    ENDLOOP.

    REFRESH i1_final .

    APPEND LINES OF i3_final TO i1_final.
    DELETE: i1_final WHERE m_spart IS INITIAL.

*    SORT : i_open BY kunnr matnr.
*
*    DELETE ADJACENT DUPLICATES FROM i_open COMPARING kunnr matnr.
*
*    REFRESH : i_open.

    SORT : i1_open BY kunnr matnr .

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

    "SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.

    LOOP AT i1_final INTO w1_final .

      READ TABLE i_open INTO w_open WITH KEY kunnr = w1_final-kunnr matnr = w1_final-matnr.
      IF sy-subrc = '0' .
        w1_final-aft_open = w1_final-aft_rol - w_open-tot_open .
      ELSE.
        w1_final-aft_open = w1_final-aft_rol .
      ENDIF.
      MODIFY i1_final FROM w1_final TRANSPORTING aft_open.
      CLEAR : w1_final ,w_open.
    ENDLOOP.

    DELETE i1_final WHERE aft_open LE 0 .

    APPEND LINES OF i1_final TO i2_final.

    SORT : i2_final BY kunnr m_spart zterm .

    DELETE ADJACENT DUPLICATES FROM i2_final COMPARING kunnr m_spart zterm.

    SELECT
      kunnr
      vkorg
      vtweg
      spart
      parvw
      kunn2
      pernr
       FROM knvp INTO TABLE i1_knvp FOR ALL ENTRIES IN i2_final WHERE kunnr = i2_final-kunnr AND spart = i2_final-c_spart .

    LOOP AT i2_final INTO w2_final .
      w2_final-sa_org = '1000' .
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'AG'.
      IF sy-subrc EQ '0'.
        w2_final-sp_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'RE'.
      IF sy-subrc EQ '0'.
        w2_final-bp_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'RG'.
      IF sy-subrc EQ '0'.
        w2_final-py_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'WE'.
      IF sy-subrc EQ '0'.
        w2_final-sh_par = w1_knvp-kunn2.
      ENDIF.

      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L1'.
      IF sy-subrc EQ '0'.
        w2_final-so_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L2'.
      IF sy-subrc EQ '0'.
        w2_final-asm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L3'.
      IF sy-subrc EQ '0'.
        w2_final-rsm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'L5'.
      IF sy-subrc EQ '0'.
        w2_final-nsm_no = w1_knvp-pernr.
      ENDIF.
      READ TABLE i1_knvp INTO w1_knvp WITH KEY kunnr = w2_final-kunnr spart = w2_final-c_spart parvw = 'BR'.
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

  INCLUDE z_sku_bapi_create_ord_hd_n_f01.
*  INCLUDE Z_SKU_BAPI_CREATE_ORD_HEADF01.
