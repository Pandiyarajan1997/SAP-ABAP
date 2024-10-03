*&---------------------------------------------------------------------*
*& Report  ZSCHEDULE_STOCK_AGE
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Ramachandran M                     *
*& Developer                   : Mr.Ramachandran M                        *
*& Created On                  : 15 July 2020                           *
*& Title                       : Material Stock Details                *
*& Report Name                 : ZSCHEDULE_STOCK_AGE                     *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Display The Stock Details           *
*                                                 *
*&---------------------------------------------------------------------*

*
REPORT  zschedule_stock_age.

TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&  Structure & Internal Table Decleration
*&---------------------------------------------------------------------*


TABLES:mchb.

tables: ZPLANT_MAT_AGE.

TYPES : BEGIN OF gs_t001k,

        bwkey TYPE t001k-bwkey,
        bukrs TYPE t001k-bukrs,
        END OF gs_t001k.

DATA : gt_t001k TYPE TABLE OF gs_t001k,
       wa_t001k TYPE gs_t001k.

DATA : gt1_t001k TYPE TABLE OF gs_t001k,
       wa1_t001k TYPE gs_t001k.

TYPES: BEGIN OF gs_mchb,
       matnr TYPE mchb-matnr,              " Material Code
       werks TYPE mchb-werks,              " Valuation Area / Plant
       lgort TYPE mchb-lgort,              "Stor. Location

       charg TYPE mchb-charg,              " Batch
       ersda TYPE mchb-ersda,              " Created On
       clabs TYPE mchb-clabs,               " Stock Qty
       cinsm TYPE mchb-cinsm,               "Quality Insp
       cspem TYPE mchb-cspem,               " Blocked
       END OF gs_mchb.

DATA: gt_mchb TYPE TABLE OF gs_mchb,
      wa_mchb TYPE gs_mchb.

DATA: gt1_mchb TYPE TABLE OF gs_mchb,
      wa1_mchb TYPE gs_mchb.

TYPES: BEGIN OF gs_mcha,
       matnr TYPE mcha-matnr,
       werks TYPE mcha-werks,
       charg TYPE mcha-charg,
       bwtar TYPE mcha-bwtar,
       END OF gs_mcha.

DATA: gt_mcha TYPE TABLE OF gs_mcha,
      wa_mcha TYPE gs_mcha.

DATA: gt1_mcha TYPE TABLE OF gs_mcha,
      wa1_mcha TYPE gs_mcha.

TYPES : BEGIN OF gs_mard,
        matnr TYPE mard-matnr,
        werks TYPE mard-werks,
        ersda TYPE mard-ersda,
        labst TYPE mard-labst,
        lgort TYPE mard-lgort,
      END OF gs_mard.

DATA : gt_mard TYPE TABLE OF gs_mard,
        wa_mard TYPE gs_mard.

DATA : gt1_mard TYPE TABLE OF gs_mard,
        wa1_mard TYPE gs_mard.

TYPES : BEGIN OF gs_mbew,
        matnr TYPE mbew-matnr,            "Material
        bwkey TYPE mbew-bwkey,            "Valuation Area / Plant
        bwtar TYPE mbew-bwtar,            "Valuation Type
        lbkum TYPE mbew-lbkum,            "Total Value
        salk3 TYPE mbew-salk3,            "Total Value
        vprsv TYPE mbew-vprsv,            " Price Control
        verpr TYPE mbew-verpr,            "Moving Price
        stprs TYPE mbew-stprs,            "Standard price
        END OF gs_mbew.

DATA: gt_mbew TYPE TABLE OF gs_mbew,
      wa_mbew TYPE gs_mbew.

DATA: gt1_mbew TYPE TABLE OF gs_mbew,
      wa1_mbew TYPE gs_mbew.

TYPES: BEGIN OF gs_mara,
       matnr TYPE mara-matnr,              " Material Code
       mtart TYPE mara-mtart,
       matkl TYPE mara-matkl,              " Material Group
       meins TYPE mara-meins,              " UOM
       spart TYPE mara-spart,              " Division
       volum TYPE mara-volum,               " Volume
       xchpf TYPE mara-xchpf,
       END OF gs_mara.

DATA: gt_mara TYPE TABLE OF gs_mara,
      wa_mara TYPE gs_mara.

DATA: gt1_mara TYPE TABLE OF gs_mara,
      wa1_mara TYPE gs_mara.

TYPES: BEGIN OF gs_makt,
       matnr TYPE makt-matnr,              " Material Number
       maktx TYPE makt-maktx,              " Material Description
       END OF gs_makt.

DATA: gt_makt TYPE TABLE OF gs_makt,
      wa_makt TYPE gs_makt.

DATA: gt1_makt TYPE TABLE OF gs_makt,
      wa1_makt TYPE gs_makt.

TYPES : BEGIN OF gs_t001w,
        werks TYPE t001w-werks,
        name1 TYPE t001w-name1,
        END OF gs_t001w.

DATA : gt_t001w TYPE TABLE OF gs_t001w,
       wa_t001w TYPE gs_t001w.

DATA : gt1_t001w TYPE TABLE OF gs_t001w,
       wa1_t001w TYPE gs_t001w.

TYPES : BEGIN OF ty_marc,
        matnr TYPE marc-matnr,
        werks TYPE marc-werks,
        trame TYPE marc-trame,
        umlmc TYPE marc-umlmc,
        bwesb TYPE  marc-bwesb,
        END OF ty_marc.

DATA : gt_marc TYPE STANDARD TABLE OF ty_marc,
      wa_marc LIKE LINE OF gt_marc.


TYPES:BEGIN OF str_mchb,
      ersda4 TYPE mchb-ersda,
      END OF str_mchb.

DATA:wa4 TYPE str_mchb.



TYPES: BEGIN OF gs_final,
       matnr TYPE mchb-matnr,              " Material Number
       werks TYPE mchb-werks,              " Plant
       lgort TYPE mchb-lgort,              "Storage Location
       spart TYPE mara-spart,              " Division
       meins TYPE mara-meins,
       mtart TYPE mara-mtart,              " Material Type

       name1 TYPE t001w-name1,             " Plant Descriprtion
       bukrs TYPE t001k-bukrs,
       maktx TYPE makt-maktx,              " Material Description
       charg TYPE mchb-charg,              " Batch No
       ersda TYPE mchb-ersda,              " Posting Date / Crated On
       ersda7 TYPE mchb-ersda,              " Posting Date / Crated On

*       ERSDA1 TYPE MCHB-ERSDA,              " Posting Date / Crated On

       clabs TYPE p DECIMALS 3,              " Stock Qty
       trame TYPE p DECIMALS 2,
       cinsm TYPE p DECIMALS 2,               "Quality Insp
       cspem TYPE p DECIMALS 2,               " Blocked
       labst TYPE p DECIMALS 2,              " Stock Qty

       volum TYPE mara-volum,              " Volume
       vclabs TYPE mchb-clabs,             " Stock In Ltrs

       mmatnr TYPE mard-matnr,
       mwerks TYPE mard-werks,
       mersda TYPE mard-ersda,
       mlabst TYPE mard-labst,
       mlgort TYPE mard-lgort,
*       VERPR TYPE MBEW-VERPR,              "Moving Price
*       PRICE TYPE MBEW-STPRS,
       vprsv TYPE mbew-vprsv,              " Price Control

       zage1 TYPE p DECIMALS 2,             " A1 < 30 Days
       vzage1 TYPE p DECIMALS 2,            " Stock In Ltrs

       zage9 TYPE p DECIMALS 2,             " A3 91-180 Days
       vzage9 TYPE p DECIMALS 2,            " Stock In Ltrs
       zage10 TYPE p DECIMALS 2,             " A5 Above 180 Days
       vzage10 TYPE p DECIMALS 2,            " Stock In Ltrs

       price TYPE p DECIMALS 2,       " STOCKVALUE
       lv_price TYPE f,
*       stockvalue TYPE p DECIMALS 2,       " STOCKVALUE
       stockvalue TYPE p DECIMALS 2,       " STOCKVALUE
       stockvalue1 TYPE p DECIMALS 2,       " STOCKVALUE

       stockvalue11 TYPE p DECIMALS 2,       " STOCKVALUE

       stockcase1 TYPE p DECIMALS 2,       " STOCKVALUE
       stockcase2 TYPE p DECIMALS 2,       " STOCKVALUE

       stockcase9 TYPE p DECIMALS 2,       " STOCKVALUE
       stockcase10 TYPE p DECIMALS 2,       " STOCKVALUE



       stock_quality TYPE p DECIMALS 2,       " STOCKVALUE QUALITY
       stock_transit TYPE p DECIMALS 2,       " STOCKVALUE TRANSIT
       stock_blocked TYPE p DECIMALS 2,

       v_werkcount TYPE i,
       v_matcount TYPE i,

       v_count TYPE i,
       END OF gs_final.

TYPES: BEGIN OF gs_tab,
         mandt TYPE ZPLANT_MAT_AGE-mandt,
         plant TYPE ZPLANT_MAT_AGE-PLANT,
         material TYPE ZPLANT_MAT_AGE-MATERIAL,
         QUANTITY TYPE ZPLANT_MAT_AGE-QUANTITY,
         flag TYPE ZPLANT_MAT_AGE-FLAG,
     END OF gs_tab.

DATA: gt_final TYPE TABLE OF gs_final,
      wa_final TYPE gs_final.

DATA: gt1_final TYPE TABLE OF gs_final,
      wa1_final TYPE gs_final.

DATA: gt2_final TYPE TABLE OF gs_final,
      wa2_final TYPE gs_final.

DATA: gt_tab TYPE TABLE OF gs_tab,
      wa_tab TYPE gs_tab.

DATA: lv_werkcount TYPE sy-tabix.
DATA: lv_matcount TYPE sy-tabix.

DATA : lv_mail TYPE pa0105-usrid_long.
DATA: lv_year TYPE bkpf-gjahr,
      lv_month TYPE bkpf-monat,
      lv_spart TYPE mara-spart,
      t_days TYPE i,
      it_attachment TYPE STANDARD TABLE OF solisti1,
      it_packlist TYPE STANDARD TABLE OF sopcklsti1,
      it_docdata TYPE STANDARD TABLE OF sodocchgi1,
      g_tab_lines   TYPE i,
    it_receivers TYPE STANDARD TABLE OF somlreci1,
      it_body_msg TYPE STANDARD TABLE OF solisti1.
" IT_BODY_MSG TYPE STANDARD TABLE OF SOLISTI1,
.

DATA: lv_count TYPE sy-tabix.


*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: gt_fcat TYPE slis_t_fieldcat_alv,
      wa_fcat TYPE slis_fieldcat_alv,
      it_layout TYPE slis_layout_alv,
      gt_events TYPE slis_t_event,
      wa_events TYPE slis_alv_event,
      key TYPE slis_keyinfo_alv,
       it_sort TYPE slis_t_sortinfo_alv,
       wa_body_msg LIKE LINE OF it_body_msg,
         wa_packlist LIKE LINE OF it_packlist,
      wa_receivers LIKE LINE OF it_receivers,
      wa_docdata LIKE LINE OF it_docdata,
       wa_attachment LIKE LINE OF it_attachment,
       wa_sort LIKE LINE OF it_sort.

DATA: ls_variant TYPE disvariant.
ls_variant-report = sy-repid.


DATA : layout TYPE slis_layout_alv.

DATA : lv_bukrs TYPE t001k-bukrs,
       lv_werks TYPE t001w-werks,
       lv_matnr TYPE mara-matnr,
       lv_mtart TYPE mara-mtart,
       lv_lgort TYPE mchb-lgort.
*       lv_price TYPE CHAR255.

DATA : count TYPE i VALUE '0',
       trans_qty TYPE p DECIMALS 2,
       g_sent_to_all TYPE sonv-flag.



DATA:check1 TYPE char10,
      check2 TYPE char10.

DATA:ersdd TYPE mchb-ersda.

START-OF-SELECTION.

*&---------------------------------------------------------------------*
*&  Main Logic
*&---------------------------------------------------------------------*

"  IF wi_batch = 'X'.
    SELECT
      matnr
      mtart
      matkl
      meins
      spart
      volum
      xchpf FROM mara INTO TABLE gt_mara
      WHERE xchpf = 'X' AND ( mtart eq 'FERT' or mtart eq 'HAWA' ).
 " ENDIF.

 " IF wo_batch = 'X'.

    SELECT
      matnr
      mtart
      matkl
      meins
      spart
      volum
      xchpf FROM mara INTO TABLE gt1_mara
      WHERE xchpf = '' AND ( mtart eq 'FERT' or mtart eq 'HAWA' ).

 " ENDIF.

  IF gt_mara[] IS NOT INITIAL or gt1_mara[] is NOT INITIAL.
    SELECT
      matnr
      maktx FROM makt INTO TABLE gt_makt FOR ALL ENTRIES IN gt_mara
      WHERE matnr = gt_mara-matnr .

    SELECT
      matnr
      maktx FROM makt INTO TABLE gt1_makt FOR ALL ENTRIES IN gt1_mara
      WHERE matnr = gt1_mara-matnr .

  "  IF wi_batch = 'X'.

      SELECT
         matnr
         werks
         lgort
         charg
         ersda
         clabs
         cinsm
         cspem FROM mchb INTO TABLE gt_mchb FOR ALL ENTRIES IN gt_mara
        WHERE matnr = gt_mara-matnr AND werks BETWEEN '1100' and  '1179'  "werks in so_werks
        AND ersda LE sy-datum and clabs ne '0.00' . "p_date.

      SELECT matnr
             werks
             charg
             bwtar FROM mcha INTO TABLE gt_mcha FOR ALL ENTRIES IN gt_mchb
             WHERE matnr = gt_mchb-matnr AND werks = gt_mchb-werks AND charg = gt_mchb-charg.


      SELECT
        bwkey
        bukrs
       FROM  t001k INTO TABLE gt_t001k FOR ALL ENTRIES IN gt_mchb WHERE bwkey = gt_mchb-werks AND bukrs = '1000' .

      SELECT matnr
                 werks
                 trame
                 umlmc
                 bwesb FROM marc INTO TABLE gt_marc FOR ALL ENTRIES IN gt_mara  WHERE matnr = gt_mara-matnr    .
*              BWESB FROM MARC INTO TABLE GT_MCHB FOR ALL ENTRIES IN GT_MCHB  WHERE MATNR = GT_MCHB-MATNR AND WERKS = GT_MCHB-WERKS .


   " ENDIF.

   " IF wo_batch = 'X'.

      SELECT
        matnr
        werks
        ersda
        labst
        lgort   FROM mard INTO TABLE gt1_mard FOR ALL ENTRIES IN gt1_mara
        WHERE matnr = gt1_mara-matnr AND werks BETWEEN '1100' and  '1179' . "IN so_werks .

      SELECT
         bwkey
         bukrs
        FROM  t001k INTO TABLE gt1_t001k FOR ALL ENTRIES IN gt1_mard WHERE bwkey = gt1_mard-werks AND bukrs = '1000'.

  "  ENDIF.

    SELECT
         matnr
         bwkey
         bwtar                                                                "+V1
         lbkum                                                                "+V1
         salk3                                                                "+V1
         vprsv
         verpr
         stprs FROM mbew INTO TABLE gt_mbew FOR ALL ENTRIES IN gt_mchb
        WHERE matnr = gt_mchb-matnr  AND bwkey = gt_mchb-werks AND ( vprsv = 'S' OR vprsv = 'V' )  .

    SORT gt_mbew BY matnr bwkey bwtar.                                         "+V1

    IF gt_mbew[] IS NOT INITIAL.

      SELECT werks name1 FROM t001w INTO TABLE gt_t001w
           FOR ALL ENTRIES IN gt_mchb WHERE werks = gt_mchb-werks.

         SELECT werks name1 FROM t001w INTO TABLE gt1_t001w
           FOR ALL ENTRIES IN gt_mchb WHERE werks = gt_mchb-werks.
    ENDIF.

*  ENDIF.

  ENDIF.

  DELETE gt_marc WHERE trame = 0.

*BREAK-POINT."part 1
 "IF wi_batch = 'X'.
    LOOP AT gt_t001k INTO wa_t001k.
      wa_final-bukrs = wa_t001k-bukrs.
      LOOP AT  gt_mchb INTO wa_mchb WHERE werks  = wa_t001k-bwkey .
        wa_final-matnr = wa_mchb-matnr.
        wa_final-werks = wa_mchb-werks.
        wa_final-lgort = wa_mchb-lgort.
        wa_final-charg = wa_mchb-charg.
        wa_final-ersda = wa_mchb-ersda.
        wa_final-clabs = wa_mchb-clabs.
        wa_final-cinsm = wa_mchb-cinsm.
        wa_final-cspem = wa_mchb-cspem.

        SELECT SINGLE MIN( ersda )  FROM mchb INTO wa4 WHERE matnr = wa_mchb-matnr  AND charg = wa_mchb-charg.

        READ TABLE gt_mara INTO wa_mara WITH KEY matnr = wa_mchb-matnr .

        wa_final-mtart = wa_mara-mtart.
        wa_final-spart = wa_mara-spart.
        wa_final-volum = wa_mara-volum.

*    IF WA_FINAL-CLABS <> 0.
        wa_final-meins = wa_mara-meins.
*     ENDIF.

        READ TABLE gt_makt INTO wa_makt WITH KEY matnr = wa_mchb-matnr .

        wa_final-maktx = wa_makt-maktx.

        READ TABLE gt_t001w INTO wa_t001w WITH KEY werks = wa_mchb-werks .
        wa_final-name1 = wa_t001w-name1.

        READ TABLE gt_mcha INTO wa_mcha WITH KEY matnr = wa_mchb-matnr werks = wa_mchb-werks charg = wa_mchb-charg.

        IF sy-subrc = 0.

          READ TABLE gt_mbew INTO wa_mbew WITH KEY matnr = wa_mchb-matnr bwkey = wa_mchb-werks bwtar = wa_mcha-bwtar .

          IF sy-subrc = 0.

            wa_final-price = wa_mbew-salk3 / wa_mbew-lbkum.
            wa_final-lv_price = wa_mbew-salk3 / wa_mbew-lbkum.

          ENDIF.

       ENDIF.
          wa_final-ersda = wa4-ersda4.

          CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
            EXPORTING
*             BEGDA = WA_MCHB-ERSDA
              begda = wa_final-ersda
              endda = sy-DATUM "p_date
            IMPORTING
              days  = t_days.

       IF t_days > 120.
            wa_final-zage9 = wa_mchb-clabs.
          ENDIF.
****************************************
          SHIFT wa_final-matnr LEFT DELETING LEADING '0'.

          LOOP AT  gt_marc INTO wa_marc WHERE  matnr = wa_mchb-matnr  AND werks = wa_mchb-werks.
            IF sy-subrc = 0.
              wa_final-trame =  wa_final-trame + wa_marc-trame.
            ENDIF.
          ENDLOOP.

          ersdd = wa_final-ersda.

          APPEND wa_final TO gt_final.
          CLEAR: wa_final.
          CLEAR:check1,check2.
        ENDLOOP.

      ENDLOOP.

      LOOP AT gt_final INTO wa_final.
        wa_final-vclabs = wa_final-clabs *  wa_final-volum.
        wa_final-vzage1 =  wa_final-zage1 * wa_final-volum.
        wa_final-vzage9 =  wa_final-zage9 * wa_final-volum.

        wa_final-stockvalue = wa_final-clabs * wa_final-lv_price.        "+V1

        IF wa_final-volum NE 0.
          wa_final-stockvalue1 = wa_final-price / wa_final-volum.
        ENDIF.
        wa_final-stockcase1 = wa_final-price * wa_final-zage1 .
        wa_final-stockcase9 = wa_final-price * wa_final-zage9 .
        MODIFY gt_final FROM wa_final TRANSPORTING  vclabs vzage1
        vzage9 stockvalue stockvalue1 stockcase1 stockcase9.
        CLEAR wa_final.
      ENDLOOP.
      SORT gt_final BY werks matnr charg .
      LOOP AT gt_final INTO wa_final .
        lv_count = lv_count  + 1.

        AT NEW matnr.
          wa_final-v_count = lv_count.

          MODIFY gt_final FROM wa_final TRANSPORTING v_count.
          CLEAR wa_final.
        ENDAT.
      ENDLOOP.

      LOOP AT gt_final INTO wa_final.
        IF wa_final-v_count = 0.
          wa_final-trame = ' '.
          MODIFY gt_final FROM wa_final TRANSPORTING trame.
          CLEAR wa_final.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_final INTO wa_final.

        wa_final-stock_quality  = wa_final-cinsm  * wa_final-price.
        wa_final-stock_transit   = wa_final-trame  * wa_final-price.

        wa_final-stock_blocked = wa_final-cspem * wa_final-price .

        MODIFY gt_final FROM wa_final." TRANSPORTING TRAME.

      ENDLOOP.

   " ENDIF.

    DELETE gt_final WHERE clabs EQ 0 AND  trame  EQ 0 AND  cinsm EQ 0 AND cspem EQ 0 .
"part 2
 "  IF wo_batch = 'X'.

      LOOP AT gt1_t001k INTO WA1_t001k.

        WA1_final-bukrs = WA1_t001k-bukrs .

        LOOP AT  gt1_mard INTO WA1_mard  WHERE werks = WA1_t001k-bwkey.
          "WA1_final-mmatnr = WA1_mard-matnr.
          WA1_final-matnr = WA1_mard-matnr.
          "WA1_final-mwerks = WA1_mard-werks.
          WA1_final-werks = WA1_mard-werks.
          WA1_final-mersda = WA1_mard-ersda.
          WA1_final-labst = WA1_mard-labst.
          WA1_final-mlgort = WA1_mard-lgort.

          READ TABLE gt1_mara INTO WA1_mara WITH KEY matnr = WA1_mard-matnr.
          WA1_final-mtart = WA1_mara-mtart.
          WA1_final-spart = WA1_mara-spart.
          WA1_final-volum = WA1_mara-volum.

          WA1_final-meins = WA1_mara-meins.

          READ TABLE gt1_makt INTO WA1_makt WITH KEY matnr = WA1_mard-matnr.

          WA1_final-maktx = WA1_makt-maktx.

          READ TABLE gt1_t001w INTO WA1_t001w WITH KEY werks = WA1_mard-werks.
          WA1_final-name1 = WA1_t001w-name1.

          READ TABLE gt1_mbew INTO WA1_mbew WITH KEY matnr = WA1_mard-matnr bwkey = WA1_mard-werks .

          IF sy-subrc = 0.

            WA1_final-price = WA1_mbew-salk3 / WA1_mbew-lbkum.
            WA1_final-lv_price = WA1_mbew-salk3 / WA1_mbew-lbkum.

          ENDIF.

          CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
            EXPORTING
              begda = WA1_mard-ersda
              endda = sy-DATUM "p_date
            IMPORTING
              days  = t_days.

           IF t_days > 120.
            WA1_final-zage9 = WA1_mard-labst.
          ENDIF.
*************************************************

          SHIFT WA1_final-mmatnr LEFT DELETING LEADING '0'.
          APPEND WA1_final TO gt1_final.
          CLEAR: WA1_final.

          DELETE gt1_final WHERE labst EQ 0.

        ENDLOOP.

      ENDLOOP.


     LOOP AT gt1_final INTO wa1_final.

        wa1_final-mlabst = wa1_final-labst *  wa1_final-volum.
        wa1_final-vzage1 =  wa1_final-zage1 * wa1_final-volum.
        wa1_final-vzage9 =  wa1_final-zage9 * wa1_final-volum.

        wa1_final-stockvalue = wa1_final-labst * wa1_final-lv_price.

        IF wa1_final-volum NE 0.
          wa1_final-stockvalue1 = wa1_final-price / wa1_final-volum.
        ENDIF.

        wa1_final-stockcase1 = wa1_final-price * wa1_final-zage1 .
        wa1_final-stockcase9 = wa1_final-price * wa1_final-zage9 .

        MODIFY gt1_final FROM wa1_final TRANSPORTING  mlabst vzage1 vzage9 stockvalue stockvalue1 stockcase1 stockcase9.
        CLEAR : wa1_final.
      ENDLOOP.

   " ENDIF.

  APPEND LINES OF gt1_final TO gt_final.

  REFRESH gt2_final.

  APPEND LINES OF gt_final to gt2_final.

  delete gt_Final WHERE ZAGE9 eq '0.00'.

  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING werks matnr .

   delete gt2_Final WHERE ZAGE9 eq '0.00'.


  LOOP at gt_final INTO wa_final.
    wa_tab-PLANT = wa_final-werks.
    wa_tab-material = wa_final-matnr.
    wa_tab-FLAG = 'N'.

   loop at gt2_final INTO wa2_final WHERE werks = wa_final-WERKS and matnr = wa_final-matnr.

     wa_tab-QUANTITY = wa_tab-QUANTITY + wa2_final-ZAGE9 .


    ENDLOOP.

    APPEND wa_tab TO gt_tab.

    CLEAR wa_tab.

ENDLOOP.



delete FROM ZPLANT_MAT_AGE .

"CLEAR ZPLANT_MAT_AGE.

FREE ZPLANT_MAT_AGE .

commit WORK.

"WAIT UP TO 10 SECONDS .

"INSERT ZPLANT_MAT_AGE FROM TABLE gt_tab.

MODIFY ZPLANT_MAT_AGE FROM TABLE gt_tab .

COMMIT work.
