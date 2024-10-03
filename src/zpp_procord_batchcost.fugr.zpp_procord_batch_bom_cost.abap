FUNCTION zpp_procord_batch_bom_cost.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      BOM_COST STRUCTURE  ZSTR_PROCORD_BATCH_BOM_COST
*"      CONSUMPTION_COST STRUCTURE  ZSTR_PROCORD_CONSUMPTION_COST
*"      ACTIVITY_COST STRUCTURE  ZSTR_PROCORD_ACTIVITY_COST
*"      AUFNR STRUCTURE  AUFNR_RANG OPTIONAL
*"  EXCEPTIONS
*"      INCORRECT_UOM
*"----------------------------------------------------------------------

*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 24.09.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : batch costing based BOM and actual consumption qty
*=======================================================================

  DATA: lv_req_qty TYPE ekpo-menge.
  DATA: ls_batch_cost    TYPE  zstr_procord_batch_bom_cost,
        ls_activity_cost TYPE  zstr_procord_activity_cost,
        ls_consumption   LIKE LINE OF consumption_cost,
        lt_stpo          TYPE TABLE OF stpo_api02,
        lv_txt           TYPE char40,
        lv_qty           TYPE gamng,
        lv_month         TYPE monat,
        lv_amnt          TYPE wrbtr.

*GET the user decimal notation

*  SELECT SINGLE * FROM usr01 INTO @DATA(ls_usr01) WHERE bname = @sy-uname.
*
*  IF ls_usr01-dcpfm = space OR ls_usr01-dcpfm = 'Y'.
*
*    DATA(lv_dcpfm) = abap_true.
*
*  ENDIF.
*********************get the process order completed data**************
  SELECT a~aufnr,a~auart,a~bukrs,a~werks,a~plnbez,a~ktext,a~stlal,a~stlnr,
         a~idat2,a~objnr,a~gamng,a~gmein,a~stlan,a~sbmng,b~charg
    FROM caufv AS a
    INNER JOIN afpo AS b ON a~aufnr = b~aufnr
    INTO TABLE @DATA(lt_caufv)
    WHERE a~aufnr IN @aufnr
    AND   a~werks = @plant
    AND   a~idat2 BETWEEN @from_date AND @to_date
    AND   b~posnr = '000001'.
  IF sy-subrc = 0.

    SORT : lt_caufv BY aufnr.


***********************get the base material type****************
    SELECT matnr,mtart FROM mara INTO TABLE @DATA(lt_mara)
                                 FOR ALL ENTRIES IN @lt_caufv
                                 WHERE matnr = @lt_caufv-plnbez.
    IF sy-subrc = 0.
      SORT : lt_mara BY matnr.
    ENDIF.

    LOOP AT lt_caufv INTO DATA(ls_caufv).

*********************for the inactive BOM to get and calculate the required qty**************
      REFRESH : lt_stpo.
      CALL FUNCTION 'CSAP_MAT_BOM_READ'
        EXPORTING
          material    = ls_caufv-plnbez
          plant       = ls_caufv-werks
          bom_usage   = ls_caufv-stlan
          alternative = ls_caufv-stlal
        TABLES
          t_stpo      = lt_stpo
        EXCEPTIONS
          error       = 1.

      IF sy-subrc = 0.
        LOOP AT lt_stpo INTO DATA(ls_stpo).

          CLEAR : ls_batch_cost.

****************material Alpha conversion****************8
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = ls_stpo-component
            IMPORTING
              output = ls_stpo-component.
**********************get the material description*******************
          SELECT SINGLE a~matnr,a~mtart,a~matkl,a~meins,b~maktx FROM mara AS a
                                        INNER JOIN makt AS b
                                        ON a~matnr = b~matnr
                                        INTO @DATA(ls_mara)
                                        WHERE a~matnr = @ls_stpo-component.
          IF sy-subrc = 0.
            ls_batch_cost-sub_matdesc = ls_mara-maktx.
            ls_batch_cost-sub_mattype = ls_mara-mtart.
            ls_batch_cost-sub_matgrp  = ls_mara-matkl.
          ENDIF.

          ls_batch_cost-bukrs         = ls_caufv-bukrs.
          ls_batch_cost-plant         = ls_caufv-werks.
          ls_batch_cost-auart         = ls_caufv-auart.
          ls_batch_cost-aufnr         = ls_caufv-aufnr.
          ls_batch_cost-tech_date     = ls_caufv-idat2.
          ls_batch_cost-base_material = ls_caufv-plnbez.
          ls_batch_cost-base_matdesc  = ls_caufv-ktext.
          ls_batch_cost-batch         = ls_caufv-charg.
          ls_batch_cost-base_matqty   = ls_caufv-gamng.
          ls_batch_cost-base_matunit  = ls_caufv-gmein.
          ls_batch_cost-bom_no        = ls_caufv-stlnr.
          ls_batch_cost-stlal         = ls_caufv-stlal.
          ls_batch_cost-sub_mat       = ls_stpo-component.

*********************get the base material type****************
          READ TABLE lt_mara INTO DATA(ls_mara2) WITH KEY matnr = ls_caufv-plnbez BINARY SEARCH.
          IF sy-subrc = 0.
            ls_batch_cost-base_mattype = ls_mara2-mtart.
          ENDIF.

*********************calculate the required qty - BOM****************
          CLEAR : lv_qty.
          CONDENSE ls_stpo-comp_qty.
*          IF lv_dcpfm = abap_true.
*            REPLACE '.' IN ls_stpo-comp_qty WITH ''.
*          ELSE.
*            REPLACE ',' IN ls_stpo-comp_qty WITH ''.
*          ENDIF.
          CALL FUNCTION 'MOVE_CHAR_TO_NUM'
            EXPORTING
              chr             = ls_stpo-comp_qty
            IMPORTING
              num             = lv_qty
            EXCEPTIONS
              convt_no_number = 1
              convt_overflow  = 2
              OTHERS          = 3.

          lv_req_qty = ( ls_caufv-gamng * lv_qty ) / ls_caufv-sbmng.
          ls_batch_cost-bom_qty       = lv_req_qty.
          ls_batch_cost-bom_unit      = ls_stpo-comp_unit.

          IF ls_stpo-comp_unit <> ls_mara-meins.

            ls_batch_cost-actual_unit = ls_mara-meins.

**Convert Material from bom UOM to Material Base UOM
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                i_matnr              = ls_stpo-component
                i_in_me              = ls_stpo-comp_unit
                i_out_me             = ls_mara-meins
                i_menge              = lv_req_qty
              IMPORTING
                e_menge              = ls_batch_cost-actual_qty
              EXCEPTIONS
                error_in_application = 1
                error                = 2
                OTHERS               = 3.
          ELSE.
            ls_batch_cost-actual_qty  = lv_req_qty.
            ls_batch_cost-actual_unit = ls_stpo-comp_unit.
          ENDIF.

*****************get the previous period from techo completed datew****************
          CLEAR : lv_month.
          IF sy-datum+4(2) GE 4.
            lv_month = ls_caufv-idat2+4(2) - 3.
          ELSE.
            lv_month = ls_caufv-idat2+4(2) + 9.
          ENDIF.
**********************get the latest price*****************
          SELECT SINGLE matnr,bwkey,verpr,lfmon,lfgja FROM mbew
                                          INTO @DATA(ls_mbew)
                                          WHERE matnr = @ls_stpo-component
                                          AND   bwkey = @ls_caufv-werks
                                          AND   bwtar = @abap_false.
          IF ls_mbew-lfmon <> lv_month.
            ls_batch_cost-actual_stk_value = ls_mbew-verpr * ls_batch_cost-actual_qty.
            ls_batch_cost-moving_price     = ls_mbew-verpr.
            ls_batch_cost-fiscalyr         = ls_mbew-lfgja.
            ls_batch_cost-fimonth          = ls_mbew-lfmon.
          ELSE.
            SELECT matnr,bwkey,verpr,lfgja,lfmon FROM mbewh
                                                 INTO TABLE @DATA(lt_mbewh)
                                                 WHERE matnr = @ls_stpo-component
                                                 AND   bwkey = @ls_caufv-werks
                                                 AND   bwtar = @abap_false.
            IF sy-subrc = 0.
              SORT lt_mbewh BY lfgja DESCENDING lfmon DESCENDING.
              READ TABLE lt_mbewh INTO DATA(ls_mbewh) INDEX 1.
              ls_batch_cost-actual_stk_value = ls_mbewh-verpr * ls_batch_cost-actual_qty.
              ls_batch_cost-moving_price     = ls_mbewh-verpr.
              ls_batch_cost-fiscalyr         = ls_mbewh-lfgja.
              ls_batch_cost-fimonth          = ls_mbewh-lfmon.
            ENDIF.
          ENDIF.

**********************finally move to the costing table*****************
          APPEND  : ls_batch_cost TO bom_cost[].
          CLEAR   : ls_stpo,ls_mbewh,ls_mbew,lv_req_qty,ls_mara,ls_mara2.
          REFRESH : lt_mbewh.
        ENDLOOP.
      ENDIF.

      CLEAR   : ls_caufv.

    ENDLOOP.

************************get the stock movement data from mseg based on process order***********
    SELECT a~mblnr,a~gjahr,a~matnr,a~menge,a~aufnr,
           a~meins,a~charg,a~erfmg,a~erfme,a~dmbtr,c~maktx,b~mtart,b~matkl
                                   FROM mseg AS a
                                   INNER JOIN mara AS b ON a~matnr = b~matnr
                                   LEFT OUTER JOIN makt AS c ON a~matnr = c~matnr
                                   INTO TABLE @DATA(lt_mseg)
                                   FOR ALL ENTRIES IN @lt_caufv
                                   WHERE a~aufnr = @lt_caufv-aufnr
                                   AND   a~bwart = '261'.
    IF sy-subrc = 0.
      SORT : lt_mseg BY aufnr.
    ENDIF.

    LOOP AT lt_mseg INTO DATA(ls_mseg).

*      get the process order details*************
      CLEAR : ls_consumption,ls_caufv.
      READ TABLE lt_caufv INTO ls_caufv WITH KEY aufnr = ls_mseg-aufnr BINARY SEARCH.
      ls_consumption-bukrs            = ls_caufv-bukrs.
      ls_consumption-plant            = ls_caufv-werks.
      ls_consumption-auart            = ls_caufv-auart.
      ls_consumption-aufnr            = ls_caufv-aufnr.
      ls_consumption-tech_date        = ls_caufv-idat2.
      ls_consumption-base_material    = ls_caufv-plnbez.
      ls_consumption-base_matdesc     = ls_caufv-ktext.
      ls_consumption-batch            = ls_caufv-charg.
      ls_consumption-base_matqty      = ls_caufv-gamng.
      ls_consumption-base_matunit     = ls_caufv-gmein.
      ls_consumption-bom_no           = ls_caufv-stlnr.
      ls_consumption-stlal            = ls_caufv-stlal.
      ls_consumption-sub_mat          = ls_mseg-matnr.
      ls_consumption-sub_mat_batch    = ls_mseg-charg.
      ls_consumption-bom_qty          = ls_mseg-erfmg.
      ls_consumption-bom_unit         = ls_mseg-erfme.
      ls_consumption-actual_qty       = ls_mseg-menge.
      ls_consumption-actual_unit      = ls_mseg-meins.
      ls_consumption-tot_stk_value    = ls_mseg-dmbtr.
      ls_consumption-per_unitprice    = ls_mseg-dmbtr / ls_mseg-menge.
      ls_consumption-sub_matdesc      = ls_mseg-maktx.
      ls_consumption-sub_mattype      = ls_mseg-mtart.
      ls_consumption-sub_matgrp       = ls_mseg-matkl.

*********************get the base material type****************
      READ TABLE lt_mara INTO ls_mara2 WITH KEY matnr = ls_caufv-plnbez BINARY SEARCH.
      IF sy-subrc = 0.
        ls_consumption-base_mattype = ls_mara2-mtart.
      ENDIF.

**********************finally move to the consumption table*****************
      APPEND ls_consumption TO consumption_cost[].
      CLEAR : ls_mseg,ls_mara2.

    ENDLOOP.

**************************activity cost process*************
    SELECT objnr,gjahr,meinh,kstar,wtg001,wtg002,wtg003,wtg004,wtg005,wtg006,
      wtg007,wtg008,wtg009,wtg010,wtg011,wtg012,wtg013,wtg014,wtg015,wtg016
      FROM coss
      INTO TABLE @DATA(lt_coss)
      FOR ALL ENTRIES IN @lt_caufv
      WHERE objnr = @lt_caufv-objnr
      AND   wrttp = '04'.

***********sort the process order table by object number**********
    SORT : lt_caufv BY objnr.

    LOOP AT lt_coss INTO DATA(ls_coss).

******************check the text for the GL Acount************
      CASE ls_coss-kstar.
        WHEN '0094301000'.
          lv_txt = 'Machine Time Cost'.
        WHEN '0094303000'.
          lv_txt = 'Power Time Cost'.
        WHEN '0094304000'.
          lv_txt = 'Prod, Overhead Cost'.
        WHEN '0094311000'.
          lv_txt = 'Labour Time Cost'.
      ENDCASE.

      READ TABLE lt_caufv INTO ls_caufv WITH KEY objnr = ls_coss-objnr BINARY SEARCH.

********************amont checking******************
      CLEAR : lv_amnt.
      lv_amnt = ls_coss-wtg001 + ls_coss-wtg002 + ls_coss-wtg003 + ls_coss-wtg004 + ls_coss-wtg005
                + ls_coss-wtg006 + ls_coss-wtg007 + ls_coss-wtg008 + ls_coss-wtg009 + ls_coss-wtg010
                + ls_coss-wtg011 + ls_coss-wtg012 + ls_coss-wtg013 + ls_coss-wtg014 + ls_coss-wtg015
                + ls_coss-wtg016.

*********************get the base material type****************
      READ TABLE lt_mara INTO ls_mara2 WITH KEY matnr = ls_caufv-plnbez BINARY SEARCH.

**********************move to the final activity table*********************
      APPEND VALUE #( bukrs       = ls_caufv-bukrs
                      plant       = ls_caufv-werks
                      auart       = ls_caufv-auart
                      aufnr       = ls_caufv-aufnr
                      objnr       = ls_coss-objnr
                      meinh       = ls_coss-meinh
                      kstar       = ls_coss-kstar
                      tech_date   = ls_caufv-idat2
                      material    = ls_caufv-plnbez
                      mat_desc    = ls_caufv-ktext
                      mat_type    = ls_mara2-mtart
                      description = lv_txt
                      amount      = lv_amnt ) TO activity_cost[].

      CLEAR : lv_txt,ls_coss,ls_caufv,ls_mara2.

    ENDLOOP.

    SORT : activity_cost[] BY aufnr.

  ENDIF.


ENDFUNCTION.
