FUNCTION zbapi_customer_open_item_dms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LT_DISTRIBUTOR) TYPE  KUNNR_TTY OPTIONAL
*"  TABLES
*"      LT_OPEN_ITEMS STRUCTURE  ZSTR_OPEN_ITEMS_DMS
*"      LT_CUSTOMER STRUCTURE  KUNNR_STY OPTIONAL
*"      LT_CRBALANCE STRUCTURE  ZSTR_CRBALANCE_DMS
*"  EXCEPTIONS
*"      NO_OPEN_ITEMS
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*" Created by: Pandiaraja
  " Created_on: 16.04.2024
  " Reference By : Ramakrishnan
  " Description: Customer Open Items From FBL5N DMS
*------------------------------------------------------------------------

  CONSTANTS : bukrs TYPE bukrs VALUE 'DMS1'.

  TYPES: BEGIN OF ty_openitems,
           kunnr   TYPE kunnr,
           budat   TYPE budat,
           bldat   TYPE bldat,
           blart   TYPE blart,
           shkzg   TYPE shkzg,
           waers   TYPE waers,
           dmbtr   TYPE dmbtr,
           xblnr   TYPE xblnr,
           belnr   TYPE belnr_d,
           sgtxt   TYPE sgtxt,
           vbeln   TYPE vbeln,
           umskz   TYPE umskz,
           gjahr   TYPE gjahr,
           duedate TYPE cpudt,        "due Date
           cpudt   TYPE cpudt,        "Entry Date
           cputm   TYPE cputm,        "Entry time
         END OF ty_openitems.

  TYPES: BEGIN OF ty_retail_acnt,
           distributor TYPE zdist,
           retailer    TYPE kunnr,
           invoice     TYPE zinvoice_no,
           pos_date    TYPE budat,
           doc_no      TYPE belnr_d,
         END OF ty_retail_acnt.

  DATA: lt_retailacnt TYPE TABLE OF ty_retail_acnt,
        ls_retailacnt TYPE ty_retail_acnt.

  DATA : gs_faede     TYPE faede,
         gs_openitems TYPE ty_openitems,
         gt_bsid      TYPE TABLE OF ty_openitems,
         lv_biiling   TYPE vbeln.

  DATA : l_total_amt TYPE bapidmbtr,
         lv_crbal    TYPE bapidmbtr,
         lv_drbal    TYPE bapidmbtr,
         ls_crbaln   TYPE zstr_crbalance_dms,
         lv_index    TYPE sy-tabix,
         lv_round    TYPE bapidmbtr.

  DATA l_round_off TYPE i.


**************Get dealer details*************
  SELECT kunnr,name1 FROM kna1 INTO TABLE @DATA(lt_dealer).
  IF sy-subrc = 0.
    SORT : lt_dealer BY kunnr.
  ENDIF.
*******************alpha conversion for distributor***************
  IF lt_distributor IS NOT INITIAL.

    LOOP AT lt_distributor ASSIGNING FIELD-SYMBOL(<fs_dist>).
      <fs_dist>-kunnr = |{ <fs_dist>-kunnr ALPHA = IN }|.
    ENDLOOP.
***************get the business area for the distributor**************
    SELECT kunnr,werks,name1 FROM kna1 INTO TABLE @DATA(lt_dist)
                             FOR ALL ENTRIES IN @lt_distributor
                             WHERE kunnr EQ @lt_distributor-kunnr.
  ELSE.
***************get the business area for the distributor**************
    SELECT b~kunnr,b~werks,b~name1 FROM t001k AS a
                           INNER JOIN kna1 AS b
                           ON a~bwkey = b~werks
                           INNER JOIN zcust_blk_chk AS c
                           ON c~kunnr = b~kunnr
                           INTO TABLE @lt_dist
                          WHERE a~bukrs = 'DMS1'
                            AND c~block NE 'X'.
  ENDIF.

  REFRESH lt_retailacnt.
  SELECT distributor retailer invoice pos_date doc_no
    FROM zdms_retail_acnt
    INTO TABLE lt_retailacnt
   WHERE doc_type    = 'RV'
     AND doc_no      NE space.
  IF sy-subrc = 0.
    SORT : lt_retailacnt BY distributor retailer pos_date doc_no.
  ENDIF.

*******************alpha conversion for customer***************
  IF lt_customer IS NOT INITIAL.

    LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<fs_cust>).
      <fs_cust>-kunnr = |{ <fs_cust>-kunnr ALPHA = IN }|.
    ENDLOOP.

  ENDIF.

  REFRESH : lt_open_items,
            lt_crbalance.


  LOOP AT lt_dist INTO DATA(ls_distributor).
**********************Excess payment removal process*************
    SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete)
                                  WHERE distributor = @ls_distributor-kunnr.
    IF sy-subrc = 0.
      SORT lt_delete BY belnr gjahr.
    ENDIF.
* to select the rounding off credit and debit entries created manually on 31st march
    SELECT ryear,docnr FROM faglflexa INTO TABLE @DATA(lt_faglflexa) WHERE ryear = '2023'
       AND rldnr = '0L' AND rbukrs = 'DMS1' AND racct = '0042002009'
       AND rbusa = @ls_distributor-werks AND budat = '20240331'.
    IF sy-subrc = 0.
      SORT : lt_faglflexa BY ryear docnr.
    ENDIF.

    IF lt_customer[] IS NOT INITIAL.

      SELECT kunnr
        FROM zcust_blk_chk
        INTO TABLE @DATA(lt_kunnr)
        FOR ALL ENTRIES IN @lt_customer
        WHERE bukrs = 'DMS1'
          AND vkorg = 'SDMS'
          AND kunnr = @lt_customer-kunnr
*          AND block NE 'X'
          AND dist_werks = @ls_distributor-werks.

*      SELECT a~kunnr
*       FROM bsid AS a INNER JOIN faglflexa AS b
*       ON  a~gjahr = b~ryear
*       AND a~belnr = b~docnr
*       INNER JOIN zcust_blk_chk AS c
*       ON c~kunnr = a~kunnr
*       INTO TABLE lt_kunnr
*       FOR ALL ENTRIES IN @lt_customer
*       WHERE a~kunnr EQ @lt_customer-kunnr
*       AND a~budat <= @sy-datum
*       AND a~budat >= '20230401'
*       AND a~bukrs EQ 'DMS1'
*       AND a~umskz <> 'H'
*       AND b~rldnr  = '0L'
*       AND b~rbukrs = 'DMS1'
*       AND b~docln  = '000001'
*       AND b~rbusa  = @ls_distributor-werks
*       AND c~block NE 'X'.

      SORT : lt_kunnr BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_kunnr COMPARING kunnr.

    ELSE.

      SELECT kunnr
        FROM zcust_blk_chk
        INTO TABLE @lt_kunnr
        WHERE bukrs = 'DMS1'
          AND vkorg = 'SDMS'
*          AND block NE 'X'
          AND dist_werks = @ls_distributor-werks.

*      SELECT a~kunnr
*         FROM bsid AS a INNER JOIN faglflexa AS b
*         ON  a~gjahr = b~ryear
*         AND a~belnr = b~docnr
*         INNER JOIN zcust_blk_chk AS c
*         ON c~kunnr = a~kunnr
*         INTO TABLE @lt_kunnr
*         WHERE a~budat <= @sy-datum
*         AND a~budat >= '20230401'
*         AND a~bukrs EQ 'DMS1'
*         AND a~umskz <> 'H'
*         AND b~rldnr  = '0L'
*         AND b~rbukrs = 'DMS1'
*         AND b~docln  = '000001'
*         AND b~rbusa  = @ls_distributor-werks
*         AND c~block NE 'X'.

      SORT : lt_kunnr BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_kunnr COMPARING kunnr.

    ENDIF.

    LOOP AT lt_kunnr INTO DATA(ls_kunnr).
***********************************

      SELECT a~kunnr, a~budat, a~bldat, a~blart,a~shkzg,a~waers,
             a~dmbtr, a~xblnr, a~belnr ,a~sgtxt,
             a~vbeln,  a~umskz ,a~gjahr
             FROM bsid AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INTO TABLE @gt_bsid
             WHERE a~bukrs EQ 'DMS1'
             AND a~kunnr EQ @ls_kunnr-kunnr
             AND a~umskz <> 'H'
             AND b~rldnr  = '0L'
             AND b~rbukrs = 'DMS1'
             AND b~docln  = '000001'
             AND b~rbusa  = @ls_distributor-werks.
      IF sy-subrc = 0.
        SELECT bukrs,belnr,gjahr,cpudt,cputm FROM bkpf INTO TABLE @DATA(gt_bkpf)
                                     FOR ALL ENTRIES IN @gt_bsid
                                     WHERE bukrs = 'DMS1'
                                     AND   belnr = @gt_bsid-belnr
                                     AND   gjahr = @gt_bsid-gjahr.
        IF sy-subrc = 0.
          SORT : gt_bkpf BY bukrs belnr gjahr.
        ENDIF.
      ENDIF.

      CLEAR : lv_round.
      LOOP AT gt_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
****************get the loop index*****************
        CLEAR : lv_index.
        lv_index = sy-tabix.
**********************Excess payment removal process*************
        IF lt_delete IS NOT INITIAL.
          READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsid>-belnr
                                                               gjahr = <fs_bsid>-gjahr BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE gt_bsid INDEX lv_index.
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR l_round_off.
* if the credirt note is relating to manual round off enetered then round up the value for credit note calculation
        IF <fs_bsid>-blart = 'DG'.

          READ TABLE lt_faglflexa TRANSPORTING NO FIELDS WITH KEY ryear = '2023'
                                                                  docnr = <fs_bsid>-belnr BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR l_round_off.
            l_round_off     = ceil( <fs_bsid>-dmbtr ).
            <fs_bsid>-dmbtr = l_round_off.
          ELSE.
            CLEAR l_round_off.
            l_round_off     = <fs_bsid>-dmbtr.
            <fs_bsid>-dmbtr = l_round_off.
          ENDIF.
* if the debit note is relating to manual round off enetered then round down the value for credit note calculation
        ELSEIF <fs_bsid>-blart = 'DR'.
          READ TABLE lt_faglflexa TRANSPORTING NO FIELDS WITH KEY ryear = '2023'
                                                                  docnr = <fs_bsid>-belnr BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR l_round_off.
            l_round_off     = floor( <fs_bsid>-dmbtr ).
            <fs_bsid>-dmbtr = l_round_off.
            lv_round        = l_round_off.
            DELETE gt_bsid INDEX lv_index.
            CONTINUE.
          ELSE.
            CLEAR l_round_off.
            l_round_off     = <fs_bsid>-dmbtr.
            <fs_bsid>-dmbtr = l_round_off.
          ENDIF.

        ELSE.
          CLEAR l_round_off.
          l_round_off     = <fs_bsid>-dmbtr.
          <fs_bsid>-dmbtr = l_round_off.
        ENDIF.

        <fs_bsid>-duedate = <fs_bsid>-budat + 30.
        READ TABLE gt_bkpf INTO DATA(gs_bkpf) WITH KEY bukrs = 'DMS1'
                                                 belnr = <fs_bsid>-belnr
                                                 gjahr = <fs_bsid>-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bsid>-cpudt = gs_bkpf-cpudt.
          <fs_bsid>-cputm = gs_bkpf-cputm.
        ENDIF.
        CLEAR : gs_bkpf.
      ENDLOOP.

      SORT gt_bsid BY duedate ASCENDING cpudt ASCENDING cputm ASCENDING.
      CLEAR : l_total_amt.
      LOOP AT gt_bsid INTO DATA(lw_openitem) WHERE shkzg = 'H'. " H-Credit amount from Sub-Dealers
        l_total_amt = l_total_amt + lw_openitem-dmbtr.
        CLEAR : lw_openitem.
      ENDLOOP.
***************adjust the rounding off amount***************
      IF lv_round LT l_total_amt.
        l_total_amt = l_total_amt - lv_round.
      ELSE.
        CLEAR : l_total_amt.
      ENDIF.
**************delete the H items**********
      DELETE gt_bsid WHERE shkzg = 'H'.

      LOOP AT gt_bsid INTO lw_openitem WHERE shkzg = 'S'. " S-Debit amount from Sub-Dealers
*****************check the credit balance with invoice amount************
        IF l_total_amt GE lw_openitem-dmbtr.

          l_total_amt = l_total_amt - lw_openitem-dmbtr.
*******************get the actual credit balance***************
          lv_crbal = l_total_amt.

        ELSE.
*******************calculate OPEN items amount(debit bal)*************
          lv_drbal = lv_drbal + lw_openitem-dmbtr.
******************only for past invoices data's**************
          IF lw_openitem-bldat LE '20240331' AND lw_openitem-blart = 'RV'.
            CLEAR ls_retailacnt.
            READ TABLE lt_retailacnt INTO ls_retailacnt WITH KEY distributor = ls_distributor-kunnr
                                                                 retailer    = ls_kunnr-kunnr
                                                                 pos_date    = lw_openitem-bldat
                                                                 doc_no      = lw_openitem-belnr
                                                                 BINARY SEARCH.
            IF sy-subrc = 0.
              lv_biiling = ls_retailacnt-invoice.
            ENDIF.
          ELSE.
            lv_biiling = lw_openitem-vbeln.
            IF lv_biiling IS INITIAL.
              lv_biiling = lw_openitem-belnr.
            ENDIF.
          ENDIF.
*********************read dealer details***********
          READ TABLE lt_dealer INTO DATA(ls_dealer) WITH KEY kunnr = lw_openitem-kunnr BINARY SEARCH.
***********************append the lt_open_items tables****************
          APPEND VALUE #( bukrs         = bukrs
                          distributor   = ls_distributor-kunnr
                          dist_name     = ls_distributor-name1
                          dist_plant    = ls_distributor-werks
                          dealer        = lw_openitem-kunnr
                          dealer_name   = ls_dealer-name1
                          belnr         = lw_openitem-belnr
                          blart         = lw_openitem-blart
                          budat         = lw_openitem-bldat
                          bwwrt         = lw_openitem-dmbtr
                          payable       = lw_openitem-dmbtr - l_total_amt
                          hwaer         = lw_openitem-waers
                          gjahr         = lw_openitem-gjahr
                          sgtxt         = lw_openitem-sgtxt
                          vbeln         = lv_biiling
                          xblnr         = lw_openitem-xblnr
                          faedt         = lw_openitem-duedate ) TO lt_open_items.
          CLEAR : l_total_amt.
        ENDIF.
        CLEAR : lw_openitem,ls_dealer,lv_biiling.
      ENDLOOP.

      IF l_total_amt IS NOT INITIAL.
***********************append the total credit balance****************
        APPEND VALUE #( distributor = ls_distributor-kunnr
                        dealer      = ls_kunnr-kunnr
                        credit_bal  = l_total_amt ) TO lt_crbalance.
      ENDIF.
*******************remove the open items if the credit & debit balance equal***************
      IF lv_crbal IS NOT INITIAL AND lv_drbal IS NOT INITIAL.
        IF lv_crbal = lv_drbal.
          DELETE lt_open_items WHERE  distributor = ls_distributor-kunnr
                               AND    dealer = ls_kunnr-kunnr.
*          CLEAR : ls_crbaln.
*          ls_crbaln-distributor = ls_distributor-kunnr.
*          ls_crbaln-dealer      = ls_kunnr-kunnr.
*          ls_crbaln-credit_bal  = 0.
*          MODIFY TABLE lt_crbalance FROM ls_crbaln.
        ENDIF.
      ENDIF.

      REFRESH : gt_bsid,gt_bkpf.
      CLEAR : ls_kunnr,l_total_amt,lv_crbal,lv_drbal.

    ENDLOOP.

    CLEAR : ls_distributor.
    REFRESH : lt_customer,lt_kunnr,lt_faglflexa,lt_delete.

  ENDLOOP.

ENDFUNCTION.
