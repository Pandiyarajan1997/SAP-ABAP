*&---------------------------------------------------------------------*
*& Report  ZRT_MAT_STOCK_AGE
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Ramakrishnan & Mr.Praveen          *
*& Developer                   : Mr.Pandiarajan                        *
*& Created On                  : 15.05.2024                            *
*& Title                       : Material Stock Details                *
*& Tcode Name                  :                      *
*& Development Id              : kpabap                                *
*& Related Information         : Display The Stock Details By          *
*                                Material Group                        *
*&---------------------------------------------------------------------*

REPORT  zstk_agewise_report_dms.

TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&  Structure & Internal Table Decleration
*&---------------------------------------------------------------------*


TABLES:mchb.

TYPES : BEGIN OF gs_t001k,

          bwkey TYPE t001k-bwkey,
          bukrs TYPE t001k-bukrs,
        END OF gs_t001k.

DATA : gt_t001k TYPE TABLE OF gs_t001k,
       wa_t001k TYPE gs_t001k.


TYPES: BEGIN OF gs_mchb,
         matnr TYPE mchb-matnr,              " Material Code
         werks TYPE mchb-werks,              " Valuation Area / Plant
         lgort TYPE mchb-lgort,              "Stor. Location    Added by savariar s as on 21/10/2014.

         charg TYPE mchb-charg,              " Batch
         ersda TYPE mchb-ersda,              " Created On
         clabs TYPE mchb-clabs,               " Stock Qty
         cinsm TYPE mchb-cinsm,               "Quality Insp
         cspem TYPE mchb-cspem,               " Blocked
         rank type i,                         "Rank
       END OF gs_mchb.

DATA: gt_mchb     TYPE TABLE OF gs_mchb,
      gt_mchb_cop TYPE TABLE OF gs_mchb,
      wa_mchb     TYPE gs_mchb,
      wa_mchb_cop TYPE gs_mchb.

TYPES : BEGIN OF gs_mard,
          matnr TYPE mard-matnr,
          werks TYPE mard-werks,
          ersda TYPE mard-ersda,
          labst TYPE mard-labst,
          lgort TYPE mard-lgort,
        END OF gs_mard.

DATA : gt_mard TYPE TABLE OF gs_mard,
       wa_mard TYPE gs_mard.

TYPES : BEGIN OF gs_mbew,                 "Changes on 03/09/2014
          matnr TYPE mbew-matnr,            "Material
          bwkey TYPE mbew-bwkey,            "Valuation Area / Plant
          vprsv TYPE mbew-vprsv,            " Price Control
          verpr TYPE mbew-verpr,            "Moving Price
          stprs TYPE mbew-stprs,            "Standard price
        END OF gs_mbew.

DATA: gt_mbew TYPE TABLE OF gs_mbew,
      wa_mbew TYPE gs_mbew.


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


TYPES: BEGIN OF gs_makt,
         matnr TYPE makt-matnr,              " Material Number
         maktx TYPE makt-maktx,              " Material Description
       END OF gs_makt.

DATA: gt_makt TYPE TABLE OF gs_makt,
      wa_makt TYPE gs_makt.


TYPES : BEGIN OF gs_t001w,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
        END OF gs_t001w.

DATA : gt_t001w TYPE TABLE OF gs_t001w,
       wa_t001w TYPE gs_t001w.


TYPES : BEGIN OF ty_marc,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
          trame TYPE marc-trame,
          umlmc TYPE marc-umlmc,
          bwesb TYPE  marc-bwesb,
        END OF ty_marc.

DATA : gt_marc TYPE STANDARD TABLE OF ty_marc,
       wa_marc LIKE LINE OF gt_marc.


TYPES:BEGIN OF str_mchb,                     " Added by mani 13.02.2016
        ersda4 TYPE mchb-ersda,
      END OF str_mchb.

DATA:wa4 TYPE str_mchb.                      " Added by mani 13.02.2016



TYPES: BEGIN OF gs_final,
         matnr         TYPE mchb-matnr,              " Material Number
         werks         TYPE mchb-werks,              " Plant
         lgort         TYPE mchb-lgort,              "Storage Location
         spart         TYPE mara-spart,              " Division
         meins         TYPE mara-meins,
         mtart         TYPE mara-mtart,              " Material Type                                  "Added by Savariar S

         name1         TYPE t001w-name1,             " Plant Descriprtion
         bukrs         TYPE t001k-bukrs,
         maktx         TYPE makt-maktx,              " Material Description
         charg         TYPE mchb-charg,              " Batch No
         ersda         TYPE mchb-ersda,              " Posting Date / Crated On
         ersda7        TYPE mchb-ersda,              " Posting Date / Crated On

*       ERSDA1 TYPE MCHB-ERSDA,              " Posting Date / Crated On                     " added by mani

         clabs         TYPE p DECIMALS 3,              " Stock Qty  added by ram on 1/10/2015
         trame         TYPE p DECIMALS 2,
         cinsm         TYPE p DECIMALS 2,               "Quality Insp
         cspem         TYPE p DECIMALS 2,               " Blocked
         labst         TYPE p DECIMALS 2,              " Stock Qty    "CHANGES ON 19/09/2014  BY SAVARIAR

         volum         TYPE mara-volum,              " Volume
         vclabs        TYPE mchb-clabs,             " Stock In Ltrs

         mmatnr        TYPE mard-matnr,            "CHANGES ON 20/09/2014 BY SAVARIAR
         mwerks        TYPE mard-werks,
         mersda        TYPE mard-ersda,
         mlabst        TYPE mard-labst,            " Added by mani 13.02.2016
         mlgort        TYPE mard-lgort,
*       VERPR TYPE MBEW-VERPR,              "Moving Price
*       PRICE TYPE MBEW-STPRS,              "Standard price 03/09/2014
         vprsv         TYPE mbew-vprsv,              " Price Control

         zage1         TYPE p DECIMALS 2,             " A1 < 30 Days
         vzage1        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage2         TYPE p DECIMALS 2,             " A2 31-45 Days
         vzage2        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage3         TYPE p DECIMALS 2,             " A3 46-60 Days
         vzage3        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage4         TYPE p DECIMALS 2,             " A3 61-75 Days
         vzage4        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage5         TYPE p DECIMALS 2,             " A3 76-90 Days
         vzage5        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage6         TYPE p DECIMALS 2,             " A1 91-180 Days
         vzage6        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage7         TYPE p DECIMALS 2,             " > 180 Days
         vzage7        TYPE p DECIMALS 2,            " Stock In Ltrs

         zage8         TYPE p DECIMALS 2,             " A3 61-90 Days
         vzage8        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage9         TYPE p DECIMALS 2,             " A3 91-180 Days
         vzage9        TYPE p DECIMALS 2,            " Stock In Ltrs
         zage10        TYPE p DECIMALS 2,             " A5 Above 180 Days
         vzage10       TYPE p DECIMALS 2,            " Stock In Ltrs

         price         TYPE p DECIMALS 2,       " STOCKVALUE

         stockvalue    TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue1   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue2   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue3   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue4   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue5   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue6   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue7   TYPE p DECIMALS 2,       " STOCKVALUE

         stockvalue8   TYPE p DECIMALS 2,       " STOCKVALUE
         "Added By Govind On 27/11/2014
         stockvalue9   TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue10  TYPE p DECIMALS 2,       " STOCKVALUE
         stockvalue11  TYPE p DECIMALS 2,       " STOCKVALUE

         stockcase1    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase2    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase3    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase4    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase5    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase6    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase7    TYPE p DECIMALS 2,       " STOCKVALUE

         stockcase8    TYPE p DECIMALS 2,       " STOCKVALUE "Added By Govind On 27/11/2014
         stockcase9    TYPE p DECIMALS 2,       " STOCKVALUE
         stockcase10   TYPE p DECIMALS 2,       " STOCKVALUE



         stock_quality TYPE p DECIMALS 2,       " STOCKVALUE QUALITY             "Added By MANI On 12/10/2015
         stock_transit TYPE p DECIMALS 2,       " STOCKVALUE TRANSIT              "Added By MANI On 12/10/2015

         stock_blocked TYPE p DECIMALS 2,

         v_werkcount   TYPE i,
         v_matcount    TYPE i,

         v_count       TYPE i,
         dist          TYPE kunnr,
         dist_name     TYPE name1,
       END OF gs_final.



DATA: gt_final TYPE TABLE OF zst_stk_agewise_dms,
      wa_final TYPE zst_stk_agewise_dms.

DATA: lv_werkcount TYPE sy-tabix.
DATA: lv_matcount TYPE sy-tabix.

DATA : lv_mail TYPE pa0105-usrid_long.  "ADDED BY RAM ON 24/11/2014

DATA: lv_year       TYPE bkpf-gjahr,
      lv_month      TYPE bkpf-monat,
      lv_spart      TYPE mara-spart,
      t_days        TYPE i,
      it_attachment TYPE STANDARD TABLE OF solisti1,
      it_packlist   TYPE STANDARD TABLE OF sopcklsti1,
      it_docdata    TYPE STANDARD TABLE OF sodocchgi1,      "ADDED BY RAM ON  24/11/2014
      g_tab_lines   TYPE i,
      it_receivers  TYPE STANDARD TABLE OF somlreci1,
      it_body_msg   TYPE STANDARD TABLE OF solisti1.
" IT_BODY_MSG TYPE STANDARD TABLE OF SOLISTI1,
.

DATA: lv_count TYPE sy-tabix.


*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: gt_fcat       TYPE slis_t_fieldcat_alv,
      wa_fcat       TYPE slis_fieldcat_alv,
      it_layout     TYPE slis_layout_alv,
      gt_events     TYPE slis_t_event,
      wa_events     TYPE slis_alv_event,
      key           TYPE slis_keyinfo_alv,
      it_sort       TYPE slis_t_sortinfo_alv,
      wa_body_msg   LIKE LINE OF it_body_msg,
      wa_packlist   LIKE LINE OF it_packlist,
      wa_receivers  LIKE LINE OF it_receivers,
      wa_docdata    LIKE LINE OF it_docdata,    "ADDED BY RAM ON 24/11/2014
      wa_attachment LIKE LINE OF it_attachment,
      wa_sort       LIKE LINE OF it_sort.

DATA: ls_variant TYPE disvariant.
ls_variant-report = sy-repid.


DATA : layout TYPE slis_layout_alv.

DATA : lv_bukrs TYPE t001k-bukrs,
       lv_werks TYPE t001w-werks,
       lv_matnr TYPE mara-matnr,
       lv_mtart TYPE mara-mtart,
       lv_kunnr TYPE kna1-kunnr,
       lv_lgort TYPE mchb-lgort.

DATA : count         TYPE i VALUE '0',
       trans_qty     TYPE p DECIMALS 2,
       g_sent_to_all TYPE sonv-flag.

DATA: lv_tabix  TYPE sy-tabix,
      lv_tabix2 TYPE sy-tabix.

DATA:check1 TYPE char10,
     check2 TYPE char10.

DATA:ersdd TYPE mchb-ersda.


*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

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

"Adding Radio Button.

SELECTION-SCREEN: BEGIN OF BLOCK s1 WITH FRAME TITLE TEXT-001.       "Changes on 20/09/2014 by savariar
  PARAMETERS:wi_batch RADIOBUTTON GROUP g1 USER-COMMAND gh,
             wo_batch RADIOBUTTON GROUP g1.
  PARAMETERS: p_bapi AS CHECKBOX USER-COMMAND cbc MODIF ID bk1.
SELECTION-SCREEN:END OF BLOCK s1.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF wo_batch = 'X' AND screen-group1 = 'A'.                        " ADDED BY MANI 13.02.2016
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********added by samsudeen ON (18.08.2022)
  LOOP AT SCREEN.
    IF screen-group1 = 'BK1' .
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.




START-OF-SELECTION.
***************fetch only dms distributor***************
  SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1)
                                     WHERE kunnr IN @so_kunnr
                                     AND   werks IN @so_werks
                                     AND   werks NE @abap_false.
  IF sy-subrc = 0.
    CLEAR : so_werks.
    SORT : lt_kna1 BY werks.
    LOOP AT lt_kna1 INTO DATA(ls_kna1).
      so_werks-sign = 'I'.
      so_werks-option   = 'EQ'.
      so_werks-low = ls_kna1-werks.
      APPEND so_werks.
    ENDLOOP.
  ENDIF.
*&---------------------------------------------------------------------*
*&  Main Logic
*&---------------------------------------------------------------------*

  IF wi_batch = 'X'.
    SELECT
      matnr
      mtart
      matkl
      meins
      spart
      volum
      xchpf FROM mara INTO TABLE gt_mara
      WHERE  matnr IN so_matnr AND spart IN so_spart AND xchpf = 'X' AND mtart IN so_mtart.
  ENDIF.

  IF wo_batch = 'X'.

    SELECT
      matnr
      mtart
      matkl
      meins
      spart
      volum
      xchpf FROM mara INTO TABLE gt_mara
      WHERE  matnr IN so_matnr AND spart IN so_spart AND mtart IN so_mtart. "AND xchpf = '' AND mtart IN so_mtart.

  ENDIF.

  IF gt_mara[] IS NOT INITIAL.
    SELECT
      matnr
      maktx FROM makt INTO TABLE gt_makt FOR ALL ENTRIES IN gt_mara
      WHERE matnr = gt_mara-matnr .

    IF wi_batch = 'X'.                                            "CHANGES ON 20/09/2014 BY SAVARIAR.
      SELECT
       matnr
         werks
         lgort
         charg
         ersda
         clabs
        cinsm
         cspem FROM mchb INTO TABLE gt_mchb FOR ALL ENTRIES IN gt_mara
        WHERE matnr = gt_mara-matnr AND werks IN so_werks AND lgort IN so_lgort AND charg IN s_charg AND ersda LE p_date.    " S_CHARG Added by mani 13.02.2016

*Added by Ram to replace the select min statement
      SELECT
       matnr
         werks
         lgort
         charg
         ersda
         clabs
        cinsm
         cspem FROM mchb INTO TABLE gt_mchb_cop FOR ALL ENTRIES IN gt_mara
        WHERE matnr = gt_mara-matnr AND charg IN s_charg.

*    zcl_stk_age_ctb=>fetch_data(
*      EXPORTING
*        iv_client   = sy-mandt
*        it_mara     = gt_mara
*      IMPORTING
*        it_stk_data = gt_mchb_cop
*    ).

      SELECT
        bwkey
        bukrs
       FROM  t001k INTO TABLE gt_t001k FOR ALL ENTRIES IN gt_mchb WHERE bwkey = gt_mchb-werks AND bukrs IN so_bukrs
                                                    .

      SELECT  matnr
                 werks
                 trame
                 umlmc
                 bwesb FROM marc INTO TABLE gt_marc FOR ALL ENTRIES IN gt_mara  WHERE matnr = gt_mara-matnr    . "Change on 27/10/2014.
*              BWESB FROM MARC INTO TABLE GT_MCHB FOR ALL ENTRIES IN GT_MCHB  WHERE MATNR = GT_MCHB-MATNR AND WERKS = GT_MCHB-WERKS .

    ENDIF.

    IF wo_batch = 'X'.                                            "CHANGES ON 20/09/2014 BY SAVARIAR

      SELECT
        matnr
        werks
        ersda
        labst
        lgort   FROM mard INTO TABLE gt_mard FOR ALL ENTRIES IN gt_mara
        WHERE matnr = gt_mara-matnr AND werks IN so_werks .

      SELECT
         bwkey
         bukrs
        FROM  t001k INTO TABLE gt_t001k FOR ALL ENTRIES IN gt_mard WHERE bwkey = gt_mard-werks AND bukrs IN so_bukrs.

    ENDIF.

    SELECT
         matnr
         bwkey
         vprsv
         verpr
         stprs FROM mbew INTO TABLE gt_mbew FOR ALL ENTRIES IN gt_mchb
        WHERE matnr = gt_mchb-matnr  AND bwkey = gt_mchb-werks AND bwtar = space AND ( vprsv = 'S' OR vprsv = 'V' )  .

    IF gt_mbew[] IS NOT INITIAL.

      SELECT werks name1 FROM t001w INTO TABLE gt_t001w
           FOR ALL ENTRIES IN gt_mchb WHERE werks = gt_mchb-werks.
    ENDIF.

  ENDIF.

  DELETE gt_marc WHERE trame = 0.


  IF wi_batch = 'X'.

* Starting the Parallel Cursor
    SORT gt_t001k BY bwkey.
    SORT gt_mchb BY werks matnr charg.

    SORT gt_mara BY matnr.
    SORT gt_makt BY matnr.
    SORT gt_t001w BY werks.
    SORT gt_mbew BY matnr bwkey.
    SORT gt_marc BY matnr werks.

    SORT gt_mchb_cop BY matnr charg ersda ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_mchb_cop COMPARING matnr charg.


    LOOP AT gt_t001k INTO wa_t001k.
* Read the second internal table with BINARY SEARCH
      READ TABLE gt_mchb TRANSPORTING NO FIELDS WITH KEY werks = wa_t001k-bwkey BINARY SEARCH.
      IF sy-subrc = 0.
* Get the TABIX number
        lv_tabix = sy-tabix.
      ELSE.
        CONTINUE.
      ENDIF.
* Start the LOOP from the first accessed record in previous READ i.e. LV_TABIX
      LOOP AT  gt_mchb INTO wa_mchb FROM lv_tabix .
*   End the LOOP, when there is no more record with similar key
        IF wa_mchb-werks <> wa_t001k-bwkey.
          EXIT.
        ENDIF.
        wa_final-bukrs = wa_t001k-bukrs.
        wa_final-matnr = |{ wa_mchb-matnr ALPHA = IN }| .
        wa_final-werks = wa_mchb-werks.
        wa_final-lgort = wa_mchb-lgort.
        wa_final-charg = wa_mchb-charg.
        wa_final-ersda = wa_mchb-ersda.
        wa_final-clabs = wa_mchb-clabs.
        wa_final-cinsm = wa_mchb-cinsm.
        wa_final-cspem = wa_mchb-cspem.

*        SELECT SINGLE MIN( ersda )  FROM mchb INTO wa4 WHERE matnr = wa_mchb-matnr  AND charg = wa_mchb-charg.                      " Added by mani 13.02.2016

        CLEAR: wa_mchb_cop, wa4.
        READ TABLE gt_mchb_cop INTO wa_mchb_cop WITH KEY matnr = wa_mchb-matnr charg = wa_mchb-charg BINARY SEARCH.
        IF sy-subrc = 0.
          wa4-ersda4 = wa_mchb_cop-ersda.
        ENDIF.


        READ TABLE gt_mara INTO wa_mara WITH KEY matnr = wa_mchb-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_final-mtart = wa_mara-mtart.
          wa_final-spart = wa_mara-spart.
          wa_final-volum = wa_mara-volum.
          wa_final-meins = wa_mara-meins.
        ENDIF.



        READ TABLE gt_makt INTO wa_makt WITH KEY matnr = wa_mchb-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_final-maktx = wa_makt-maktx.
        ENDIF.

        READ TABLE gt_t001w INTO wa_t001w WITH KEY werks = wa_mchb-werks BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_final-name1 = wa_t001w-name1.
        ENDIF.

        READ TABLE gt_mbew INTO wa_mbew WITH KEY matnr = wa_mchb-matnr bwkey = wa_mchb-werks BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-vprsv = wa_mbew-vprsv.
*          IF wa_mbew-vprsv = 'S'.
*            wa_final-price = wa_mbew-stprs.
*          ELSEIF wa_mbew-vprsv = 'V'.
*            wa_final-price = wa_mbew-verpr.
*          ENDIF.
          wa_final-price = SWITCH #( wa_mbew-vprsv when 'S' then wa_mbew-stprs when 'V' then wa_mbew-verpr ).
        ENDIF.

        wa_final-ersda = wa4-ersda4.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            begda = wa_final-ersda
            endda = p_date
          IMPORTING
            days  = t_days.



        IF t_days <= 30.
          wa_final-zage1 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN 31 AND 45.
          wa_final-zage2 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN 46 AND 60.
          wa_final-zage3 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN 61 AND 75.
          wa_final-zage4 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN 76 AND 90.
          wa_final-zage5 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN 91 AND 120.
          wa_final-zage6 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN  121 AND 150.
          wa_final-zage7 = wa_mchb-clabs.
        ELSEIF t_days BETWEEN  151 AND 180.
          wa_final-zage8 = wa_mchb-clabs.
        ELSEIF t_days > 180.
          wa_final-zage9 = wa_mchb-clabs.
        ENDIF.

*        SHIFT wa_final-matnr LEFT DELETING LEADING '0'.

        READ TABLE gt_marc INTO wa_marc WITH KEY matnr = wa_mchb-matnr werks = wa_mchb-werks BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE gt_final TRANSPORTING NO FIELDS WITH KEY matnr = wa_final-matnr werks = wa_mchb-werks.
          IF sy-subrc NE 0.
            wa_final-trame =  wa_marc-trame.
          ENDIF.

        ENDIF.


        ersdd = wa_final-ersda.

        wa_final-vclabs = wa_final-clabs *  wa_final-volum.
        wa_final-vzage1 =  wa_final-zage1 * wa_final-volum.
        wa_final-vzage2 =  wa_final-zage2 * wa_final-volum.
        wa_final-vzage3 =  wa_final-zage3 * wa_final-volum.
        wa_final-vzage4 =  wa_final-zage4 * wa_final-volum.
        wa_final-vzage5 =  wa_final-zage5 * wa_final-volum.
        wa_final-vzage6 =  wa_final-zage6 * wa_final-volum.
        wa_final-vzage7 =  wa_final-zage7 * wa_final-volum.

        wa_final-vzage8 =  wa_final-zage8 * wa_final-volum.
        wa_final-vzage9 =  wa_final-zage9 * wa_final-volum.

        wa_final-stockvalue = wa_final-clabs * wa_final-price.

        IF wa_final-volum NE 0.
          wa_final-stockvalue1 = wa_final-price / wa_final-volum.
        ENDIF.

        wa_final-stockvalue2 = wa_final-stockvalue1 * wa_final-vzage1 .
        wa_final-stockvalue3 = wa_final-stockvalue1 * wa_final-vzage2 .
        wa_final-stockvalue4 = wa_final-stockvalue1 * wa_final-vzage3 .
        wa_final-stockvalue5 = wa_final-stockvalue1 * wa_final-vzage4 .
        wa_final-stockvalue6 = wa_final-stockvalue1 * wa_final-vzage5 .
        wa_final-stockvalue7 = wa_final-stockvalue1 * wa_final-vzage6 .
        wa_final-stockvalue8 = wa_final-stockvalue1 * wa_final-vzage7 .
        wa_final-stockvalue9 = wa_final-stockvalue1 * wa_final-vzage8.
        wa_final-stockvalue10 = wa_final-stockvalue1 * wa_final-vzage9.

        wa_final-stockcase1 = wa_final-price * wa_final-zage1 .
        wa_final-stockcase2 = wa_final-price * wa_final-zage2 .
        wa_final-stockcase3 = wa_final-price * wa_final-zage3 .
        wa_final-stockcase4 = wa_final-price * wa_final-zage4 .
        wa_final-stockcase5 = wa_final-price * wa_final-zage5 .
        wa_final-stockcase6 = wa_final-price * wa_final-zage6 .
        wa_final-stockcase7 = wa_final-price * wa_final-zage7 .
        wa_final-stockcase8 = wa_final-price * wa_final-zage8 .
        wa_final-stockcase9 = wa_final-price * wa_final-zage9 .


        wa_final-stock_quality  = wa_final-cinsm  * wa_final-price.
        wa_final-stock_transit  = wa_final-trame  * wa_final-price.

        wa_final-stock_blocked  = wa_final-cspem * wa_final-price .
********************read the distributor data***************
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY werks = wa_mchb-werks BINARY SEARCH. "wa_final removed by zakir
        IF sy-subrc = 0.
        wa_final-dist      = ls_kna1-kunnr.
        wa_final-dist_name = ls_kna1-name1.
        Endif.
        APPEND wa_final TO gt_final.
        CLEAR: wa_final.
        CLEAR:check1,check2.
      ENDLOOP.

    ENDLOOP.

    SORT gt_final BY werks matnr charg .
*    LOOP AT gt_final INTO wa_final .
*      lv_count = lv_count  + 1.
*
*      AT NEW matnr.
*        wa_final-v_count = lv_count.
*
*        MODIFY gt_final FROM wa_final TRANSPORTING v_count.
*        CLEAR wa_final.
*        CONTINUE.
*      ENDAT.
*      wa_final-trame = ' '.
*      MODIFY gt_final FROM wa_final TRANSPORTING trame.
*      CLEAR wa_final.
*    ENDLOOP.

  ENDIF.

  DELETE gt_final WHERE clabs EQ 0 AND  trame  EQ 0 AND  cinsm EQ 0 AND cspem EQ 0 . " Added By Govind on 27-10-2014



  IF wo_batch = 'X'.
* Starting the Parallel Cursor
    SORT gt_t001k BY bwkey.
    SORT gt_mard BY werks matnr.

    SORT gt_mara BY matnr.
    SORT gt_makt BY matnr.
    SORT gt_t001w BY werks.
    SORT gt_mbew BY matnr bwkey.
    SORT gt_marc BY werks matnr.

    LOOP AT gt_t001k INTO wa_t001k.
* Read the second internal table with BINARY SEARCH
      READ TABLE gt_mard TRANSPORTING NO FIELDS WITH KEY werks = wa_t001k-bwkey BINARY SEARCH.
      IF sy-subrc = 0.
* Get the TABIX number
        lv_tabix = sy-tabix.
      ELSE.
        CONTINUE.
      ENDIF.

* Start the LOOP from the first accessed record in previous READ i.e. LV_TABIX
      LOOP AT  gt_mard INTO wa_mard FROM lv_tabix.  "  WHERE werks = wa_t001k-bwkey.

*   End the LOOP, when there is no more record with similar key
        IF wa_mard-werks <> wa_t001k-bwkey.
          EXIT.
        ENDIF.

        wa_final-bukrs = wa_t001k-bukrs .
        wa_final-mmatnr = |{ wa_mard-matnr ALPHA = IN }|.
        wa_final-mwerks = wa_mard-werks.
        wa_final-mersda = wa_mard-ersda.
        wa_final-labst = wa_mard-labst.
        wa_final-mlgort = wa_mard-lgort.

        READ TABLE gt_mara INTO wa_mara WITH KEY matnr = wa_mard-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-mtart = wa_mara-mtart.
          wa_final-spart = wa_mara-spart.
          wa_final-volum = wa_mara-volum.
          wa_final-meins = wa_mara-meins.
        ENDIF.

        READ TABLE gt_makt INTO wa_makt WITH KEY matnr = wa_mard-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-maktx = wa_makt-maktx.
        ENDIF.

        READ TABLE gt_t001w INTO wa_t001w WITH KEY werks = wa_mard-werks BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-name1 = wa_t001w-name1.
        ENDIF.


        READ TABLE gt_mbew INTO wa_mbew WITH KEY matnr = wa_mard-matnr bwkey = wa_mard-werks BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-vprsv = wa_mbew-vprsv.
*
*          IF wa_mbew-vprsv = 'S'.
*            wa_final-price = wa_mbew-stprs.
*          ELSEIF wa_mbew-vprsv = 'V'.
*            wa_final-price = wa_mbew-verpr.
*
*
*          ENDIF.
    wa_final-price = SWITCH #( wa_mbew-vprsv when 'S' then wa_mbew-stprs when 'V' then wa_mbew-verpr ).
        ENDIF.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            begda = wa_mard-ersda
            endda = p_date
          IMPORTING
            days  = t_days.


        IF t_days <= 30.                                         "changes on 24/09/014 by savariar
          wa_final-zage1 = wa_mard-labst.
        ELSEIF t_days BETWEEN 31 AND 45.
          wa_final-zage2 = wa_mard-labst.
        ELSEIF t_days BETWEEN 46 AND 60.
          wa_final-zage3 = wa_mard-labst.
        ELSEIF t_days BETWEEN 61 AND 75.
          wa_final-zage4 = wa_mard-labst.
        ELSEIF t_days BETWEEN 76 AND 90.
          wa_final-zage5 = wa_mard-labst.

        ELSEIF t_days BETWEEN 91 AND 120. "Added By Govind On 27/11/2014
          wa_final-zage6 = wa_mard-labst.
        ELSEIF t_days BETWEEN 121 AND 150.
          wa_final-zage7 = wa_mard-labst.
        ELSEIF t_days BETWEEN 151 AND 180.
          wa_final-zage8 = wa_mard-labst.
        ELSEIF t_days > 180.
          wa_final-zage9 = wa_mard-labst.
        ENDIF.

*        SHIFT wa_final-mmatnr LEFT DELETING LEADING '0'.

        wa_final-mlabst = wa_final-labst *  wa_final-volum.
        wa_final-vzage1 =  wa_final-zage1 * wa_final-volum.
        wa_final-vzage2 =  wa_final-zage2 * wa_final-volum.
        wa_final-vzage3 =  wa_final-zage3 * wa_final-volum.
        wa_final-vzage4 =  wa_final-zage4 * wa_final-volum.
        wa_final-vzage5 =  wa_final-zage5 * wa_final-volum.
        wa_final-vzage6 =  wa_final-zage6 * wa_final-volum.
        wa_final-vzage7 =  wa_final-zage7 * wa_final-volum.
        wa_final-vzage8 =  wa_final-zage8 * wa_final-volum.
        wa_final-vzage9 =  wa_final-zage9 * wa_final-volum.


        wa_final-stockvalue = wa_final-labst * wa_final-price.

        IF wa_final-volum NE 0.
          wa_final-stockvalue1 = wa_final-price / wa_final-volum.
        ENDIF.

        wa_final-stockvalue2 = wa_final-stockvalue1 * wa_final-vzage1 .
        wa_final-stockvalue3 = wa_final-stockvalue1 * wa_final-vzage2 .
        wa_final-stockvalue4 = wa_final-stockvalue1 * wa_final-vzage3 .
        wa_final-stockvalue5 = wa_final-stockvalue1 * wa_final-vzage4 .
        wa_final-stockvalue6 = wa_final-stockvalue1 * wa_final-vzage5 .
        wa_final-stockvalue7 = wa_final-stockvalue1 * wa_final-vzage6 .
        wa_final-stockvalue8 = wa_final-stockvalue1 * wa_final-vzage7 .
        wa_final-stockvalue9 = wa_final-stockvalue1 * wa_final-vzage8 .
        wa_final-stockvalue10 = wa_final-stockvalue1 * wa_final-vzage9 .

        wa_final-stockcase1 = wa_final-price * wa_final-zage1 .
        wa_final-stockcase2 = wa_final-price * wa_final-zage2 .
        wa_final-stockcase3 = wa_final-price * wa_final-zage3 .
        wa_final-stockcase4 = wa_final-price * wa_final-zage4 .
        wa_final-stockcase5 = wa_final-price * wa_final-zage5 .
        wa_final-stockcase6 = wa_final-price * wa_final-zage6 .
        wa_final-stockcase7 = wa_final-price * wa_final-zage7 .
        wa_final-stockcase8 = wa_final-price * wa_final-zage8 .
        wa_final-stockcase9 = wa_final-price * wa_final-zage9 .
********************read the distributor data***************
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY werks = wa_t001k-bwkey BINARY SEARCH. "wa_final removed by zakir
        if sy-subrc = 0.
        wa_final-dist      = ls_kna1-kunnr.
        wa_final-dist_name = ls_kna1-name1.
        endif.

        APPEND wa_final TO gt_final.
        CLEAR: wa_final.

        IF p_bapi NE 'X'.
          DELETE gt_final WHERE labst EQ 0.
        ENDIF.


      ENDLOOP.

    ENDLOOP.

*    LOOP AT gt_final INTO wa_final.
*
*      wa_final-mlabst = wa_final-labst *  wa_final-volum.
*      wa_final-vzage1 =  wa_final-zage1 * wa_final-volum.
*      wa_final-vzage2 =  wa_final-zage2 * wa_final-volum.
*      wa_final-vzage3 =  wa_final-zage3 * wa_final-volum.
*      wa_final-vzage4 =  wa_final-zage4 * wa_final-volum.
*      wa_final-vzage5 =  wa_final-zage5 * wa_final-volum.
*      wa_final-vzage6 =  wa_final-zage6 * wa_final-volum.
*      wa_final-vzage7 =  wa_final-zage7 * wa_final-volum.
*      wa_final-vzage8 =  wa_final-zage8 * wa_final-volum.
*      wa_final-vzage9 =  wa_final-zage9 * wa_final-volum.
*
*
*      wa_final-stockvalue = wa_final-labst * wa_final-price.
*
*      IF wa_final-volum NE 0.
*        wa_final-stockvalue1 = wa_final-price / wa_final-volum.
*      ENDIF.
*
*      wa_final-stockvalue2 = wa_final-stockvalue1 * wa_final-vzage1 .
*      wa_final-stockvalue3 = wa_final-stockvalue1 * wa_final-vzage2 .
*      wa_final-stockvalue4 = wa_final-stockvalue1 * wa_final-vzage3 .
*      wa_final-stockvalue5 = wa_final-stockvalue1 * wa_final-vzage4 .
*      wa_final-stockvalue6 = wa_final-stockvalue1 * wa_final-vzage5 .
*      wa_final-stockvalue7 = wa_final-stockvalue1 * wa_final-vzage6 .
*      wa_final-stockvalue8 = wa_final-stockvalue1 * wa_final-vzage7 .
*      wa_final-stockvalue9 = wa_final-stockvalue1 * wa_final-vzage8 .
*      wa_final-stockvalue10 = wa_final-stockvalue1 * wa_final-vzage9 .
*
*      wa_final-stockcase1 = wa_final-price * wa_final-zage1 .
*      wa_final-stockcase2 = wa_final-price * wa_final-zage2 .
*      wa_final-stockcase3 = wa_final-price * wa_final-zage3 .
*      wa_final-stockcase4 = wa_final-price * wa_final-zage4 .
*      wa_final-stockcase5 = wa_final-price * wa_final-zage5 .
*      wa_final-stockcase6 = wa_final-price * wa_final-zage6 .
*      wa_final-stockcase7 = wa_final-price * wa_final-zage7 .
*      wa_final-stockcase8 = wa_final-price * wa_final-zage8 .
*      wa_final-stockcase9 = wa_final-price * wa_final-zage9 .
*
*      MODIFY gt_final FROM wa_final TRANSPORTING  mlabst vzage1 vzage2
*      vzage3 vzage4 vzage5 vzage6 vzage7 vzage8 vzage9 stockvalue stockvalue1 stockvalue2 stockvalue3
*      stockvalue4 stockvalue5 stockvalue6 stockvalue7 stockvalue8 stockvalue9 stockvalue10 stockcase1 stockcase2 stockcase3 stockcase4 stockcase5 stockcase6 stockcase7 stockcase8 stockcase9.
*      CLEAR wa_final.
*    ENDLOOP.

  ENDIF.

*************Added by samsudeen M on 15.07.2022***********
  EXPORT gt_final FROM gt_final TO MEMORY ID 'ITAB'.

*&---------------------------------------------------------------------*
*&  ALV Layout
*&---------------------------------------------------------------------*

  IF wi_batch = 'X'.

    PERFORM alv_layout USING 1 'Distributor' 'DIST' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 1 'Dist. Name' 'DIST_NAME' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 1 'Plant Code' 'WERKS' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 2 'Plant Name' 'NAME1' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 3 'Division' 'SPART' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 4 'Material Code' 'MATNR' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 5 'Material Description' 'MAKTX' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 6 'Material Type' 'MTART' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 7 'Batch' 'CHARG' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 8 'Stor. Location' 'LGORT' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 9 'Created On' 'ERSDA' 'GT_FINAL' '' '' '' '50'.
    PERFORM alv_layout USING 11 'Volume' 'VOLUM' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 12  'Stock Qty' 'CLABS' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 13  'Base Unit' 'MEINS' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 14  'In Qual. Insp.' 'CINSM' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 16  'Stock In Transit' 'TRAME' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 17  'Blocked' 'CSPEM' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 19  'Price Ind.' 'VPRSV' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 20  'Unit Price' 'PRICE' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 21  'Stock Value Unrestricted' 'STOCKVALUE' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 22  'Stock Value Quality' 'STOCK_QUALITY' 'GT_FINAL' 'X' 'X' '' '50'.                "Added By MANI On 12/10/2015
    PERFORM alv_layout USING 23  'Stock Value Transit' 'STOCK_TRANSIT' 'GT_FINAL' 'X' 'X' '' '50'.                 "Added By MANI On 12/10/2015

    PERFORM alv_layout USING 24  'Stock Value Blocked' 'STOCK_BLOCKED' 'GT_FINAL' 'X' 'X' '' '50'.                 "Added By MANI On 12/10/2015

    PERFORM alv_layout USING 25  'Stock Qty(Unrestricted) Ltrs' 'VCLABS' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 26  'Price in Ltrs' 'STOCKVALUE1' 'GT_FINAL' '' 'X' '' '50'.

    PERFORM alv_layout USING 27 '< 30 Days(Case)' 'ZAGE1' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 28 '< 30 Days Stock Value' 'STOCKCASE1' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 29 '< 30 Stock Qty Ltrs' 'VZAGE1' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 27 '< 30 Stock Qty Value' 'STOCKVALUE2' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 30 '31-45 Days(Case)' 'ZAGE2' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 31 '31-45  Days Stock Value' 'STOCKCASE2' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 32 '31-45  Stock Qty Ltrs' 'VZAGE2' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 32 '31-45  Stock Qty Value' 'STOCKVALUE3' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 33 '46-60 Days(Case)' 'ZAGE3' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 34 '46-60 Days Stock Value' 'STOCKCASE3' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 35 '46-60 Stock Qty Ltrs' 'VZAGE3' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 36 '46-60 Stock Qty Value' 'STOCKVALUE4' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 36 '61-75 Days(Case)' 'ZAGE4' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 37 '61-75 Days Stock Value' 'STOCKCASE4' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 38 '61-75 Stock Qty Ltrs' 'VZAGE4' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 40 '61-75 Stock Qty Value' 'STOCKVALUE5' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 39 '76-90  Days(Case)' 'ZAGE5' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 40 '76-90 Days Stock Value' 'STOCKCASE5' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 41 '76-90 Stock Qty Ltrs' 'VZAGE5' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 44 '76-90 Stock Qty Value' 'STOCKVALUE6' 'GT_FINAL' 'X' 'X' 'X'.
***************************  "Added By Govind On 27/11/2014
    PERFORM alv_layout USING 42 '91-120  Days(Case)' 'ZAGE6' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 43 '91-120 Days Stock Value' 'STOCKCASE6' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 44 '91-120 Stock Qty Ltrs' 'VZAGE6' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 48 '91-180 Stock Qty Value' 'STOCKVALUE7' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 45 '121-150  Days(Case)' 'ZAGE7' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 46 '121-150 Days Stock Value' 'STOCKCASE7' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 47 '121-150 Stock Qty Ltrs' 'VZAGE7' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 48 '91-120 Stock Qty Value' 'STOCKVALUE8' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 48 '151-180  Days(Case)' 'ZAGE8' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 49 '151-180 Days Stock Value' 'STOCKCASE8' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 50 '151-180 Stock Qty Ltrs' 'VZAGE8' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 51 '> 180  Days(Case)' 'ZAGE9' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 52 '> 180 Days Stock Value' 'STOCKCASE9' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 53 '> 180 Stock Qty Ltrs' 'VZAGE9' 'GT_FINAL' 'X' 'X' '' '50'.
****************************************  "Added By Govind On 27/11/2014

*  PERFORM ALV_LAYOUT USING 52 '> 180 Stock Qty Value' 'STOCKVALUE8' 'GT_FINAL' 'X' 'X' 'X'.
*  PERFORM ALV_LAYOUT USING 53 'Count' 'V_COUNT' 'GT_FINAL' '' '' ''.

    wa_sort-fieldname = 'WERKS'.
*  WA_SORT-SUBTOT = 'X'.
    wa_sort-up = 'X'.
*  WA_SORT-GROUP = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

*   WA_SORT-FIELDNAME = 'MATNR'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.
*
* WA_SORT-FIELDNAME = 'TRAME'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

  ENDIF.

  IF wo_batch = 'X'.                                           "changes on 24/09/014 by savariar

   PERFORM alv_layout USING 1 'Distributor' 'DIST' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 1 'Dist. Name' 'DIST_NAME' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 1 'Plant Code' 'MWERKS' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 2 'Plant Name' 'NAME1' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 3 'Division' 'SPART' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 4 'Material Code' 'MMATNR' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 5 'Material Description' 'MAKTX' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 6 'Material Type' 'MTART' 'GT_FINAL' ' ' '' '' '50'.
    PERFORM alv_layout USING 8 'Storage Location' 'MLGORT' 'GT_FINAL' ' ' '' '' '50'. "  ADD  by sri 28/09/2016
    PERFORM alv_layout USING 10 'Created On' 'MERSDA' 'GT_FINAL' '' '' '' '50'.
    PERFORM alv_layout USING 12 'Volume' 'VOLUM' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 13  'Stock Qty' 'LABST' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 14  'Base Unit' 'MEINS' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 15  'Price Ind.' 'VPRSV' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 16  'Unit Price' 'PRICE' 'GT_FINAL' '' 'X' '' '50'.
    PERFORM alv_layout USING 17  'Stock Value' 'STOCKVALUE' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 18  'Stock Qty Ltrs' 'MLABST' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 20  'Price in Ltrs' 'STOCKVALUE1' 'GT_FINAL' '' 'X' '' '50'.

    PERFORM alv_layout USING 22 '< 30 Days(Case)' 'ZAGE1' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 23 '< 30 Days Stock Value' 'STOCKCASE1' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 24 '< 30 Stock Qty Ltrs' 'VZAGE1' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 25 '< 30 Stock Qty Value' 'STOCKVALUE2' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 26 '31-45 Days(Case)' 'ZAGE2' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 27 '31-45  Days Stock Value' 'STOCKCASE2' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 28 '31-45  Stock Qty Ltrs' 'VZAGE2' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 29 '31-45  Stock Qty Value' 'STOCKVALUE3' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 29 '46-60 Days(Case)' 'ZAGE3' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 30 '46-60 Days Stock Value' 'STOCKCASE3' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 31 '46-60 Stock Qty Ltrs' 'VZAGE3' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 33 '46-60 Stock Qty Value' 'STOCKVALUE4' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 32 '61-75 Days(Case)' 'ZAGE4' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 33 '61-75 Days Stock Value' 'STOCKCASE4' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 34 '61-75 Stock Qty Ltrs' 'VZAGE4' 'GT_FINAL' 'X' 'X' '' '50'.
*  PERFORM ALV_LAYOUT USING 37 '61-75 Stock Qty Value' 'STOCKVALUE5' 'GT_FINAL' 'X' 'X' 'X'.

    PERFORM alv_layout USING 35 '76-90  Days(Case)' 'ZAGE5' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 36 '76-90 Days Stock Value' 'STOCKCASE5' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 37 '76-90 Stock Qty Ltrs' 'VZAGE5' 'GT_FINAL' 'X' 'X' '' '50'.


***************************  "Added By Govind On 27/11/2014
    PERFORM alv_layout USING 38 '91-120  Days(Case)' 'ZAGE6' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 39 '91-120 Days Stock Value' 'STOCKCASE6' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 40 '91-120 Stock Qty Ltrs' 'VZAGE6' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 41 '121-150  Days(Case)' 'ZAGE7' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 42 '121-150 Days Stock Value' 'STOCKCASE7' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 43 '121-150 Stock Qty Ltrs' 'VZAGE7' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 44 '151-180  Days(Case)' 'ZAGE8' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 45 '151-180 Days Stock Value' 'STOCKCASE8' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 46 '151-180 Stock Qty Ltrs' 'VZAGE8' 'GT_FINAL' 'X' 'X' '' '50'.

    PERFORM alv_layout USING 47 '> 180  Days(Case)' 'ZAGE9' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 48 '> 180 Days Stock Value' 'STOCKCASE9' 'GT_FINAL' 'X' 'X' '' '50'.
    PERFORM alv_layout USING 49 '> 180 Stock Qty Ltrs' 'VZAGE9' 'GT_FINAL' 'X' 'X' '' '50'.


    wa_sort-fieldname = 'MWERKS'.
*  WA_SORT-SUBTOT = 'X'.
    wa_sort-up = 'X'.
*  WA_SORT-GROUP = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

  ENDIF.

*&---------------------------------------------------------------------*
*&   ALV Hierarchical Display
*&---------------------------------------------------------------------*

END-OF-SELECTION.

  PERFORM alv_grid_display.

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1  text
*      -->P2  text
*      -->P3  text
*      -->P4  text
*      -->P5  text
*----------------------------------------------------------------------*
FORM alv_layout  USING  p1 p2 p3 p4 p5 p6 p7 p8.
  CLEAR wa_fcat.
  wa_fcat-col_pos = p1.
  wa_fcat-seltext_l = p2.
  wa_fcat-fieldname = p3.
  wa_fcat-tabname = p4.
  wa_fcat-do_sum = p5.
  wa_fcat-no_zero = p6.
  wa_fcat-no_out = p7.
*  wa_fcat-ddic_outputlen = p8.

  APPEND wa_fcat TO gt_fcat.

ENDFORM.                    " ALV_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display .

  layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK       = ' '
*     I_BYPASSING_BUFFER      = ' '
*     I_BUFFER_ACTIVE         = ' '
      i_callback_program      = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS_GET'  " ADDED BY RAM ON 24/11/2014
      i_callback_user_command = 'MY_USER_COMMAND'
      i_callback_top_of_page  = 'ALV_CATALOG_HEADER'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
*     I_GRID_TITLE            =
*     I_GRID_SETTINGS         =
      is_layout               = layout
      it_fieldcat             = gt_fcat[]
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS       =
      it_sort                 = it_sort[]
*     IT_FILTER               =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
*     I_SAVE                  = ' '
*     IS_VARIANT              =
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT                =
*     IS_REPREP_ID            =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       = 0
*     I_HTML_HEIGHT_END       = 0
*     IT_ALV_GRAPHICS         =
*     IT_HYPERLINK            =
*     IT_ADD_FIELDCAT         =
*     IT_EXCEPT_QINFO         =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = gt_final[]
*   EXCEPTIONS
*     PROGRAM_ERROR           = 1
*     OTHERS                  = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY

**&---------------------------------------------------------------------*
**&      Form  PF_STATUS_GET_DETAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->RT_EXTAB   text
**----------------------------------------------------------------------*
*FORM PF_STATUS_GET USING RT_EXTAB TYPE SLIS_T_EXTAB.              "ADDED BY RAM 24/11/2014
*
*  SET PF-STATUS 'MY_STATUS'.
*
*ENDFORM.                    "PF_STATUS_GET_DETAIL

*&---------------------------------------------------------------------*  "ADDED BY RAM 24/11/2014
*&      Form  MY_USER_COMMAND_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM my_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.

    WHEN 'ONLI'.
      PERFORM build_xls_data_table.
      PERFORM send_mail.

    WHEN '&GRH'.
      PERFORM build_graph_data_table_detail.

  ENDCASE.

  IF rs_selfield-fieldname = 'VBELN'.

    SET PARAMETER ID 'VF' FIELD rs_selfield-value.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "MY_USER_COMMAND_DETAIL

*&---------------------------------------------------------------------*
*&      Form  BUILD_XLS_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_xls_data_table.           "ADDED BY RAM ON 24/11/2014

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Plant Code'
               'Plant Name'
               'Division'
               'Material Code'
               'Material Description'
               'Material Type'
               'Storage Location'
               'Created On'
               'Volume'
               'Stock Qty'
               'Base Unit'
               'Price Ind.'
               'Unit Price'
               'Stock Value'
               'Stock Qty Ltrs'
               'Price in Ltrs'
               '< 30 Days(Case)'
               '< 30 Days Stock Value'
               '< 30 Stock Qty Ltrs'
               '< 30 Stock Qty Value'
               '31-45 Days(Case)'
               '31-45  Days Stock Value'
               '31-45  Stock Qty Ltrs'
               '31-45  Stock Qty Value'
               '46-60 Days(Case)'
               '46-60  Days Stock Value'
               '46-60  Stock Qty Ltrs'
               '46-60  Stock Qty Value'
               '61-75 Days(Case)'
               '61-75  Days Stock Value'
               '61-75  Stock Qty Ltrs'
               '61-75  Stock Qty Value'
               '76-90  Days(Case)'
               '76-90  Days Stock Value'
               '76-90  Stock Qty Ltrs'
               '76-90  Stock Qty Value'
               '91-180  Days(Case)'
               '91-180 Days Stock Value'
               '91-180  Stock Qty Ltrs'
               '91-180  Stock Qty Value'
               '> 180  Days(Case)'
               '> 180 Days Stock Value'
               '> 180 Stock Qty Ltrs'
               '> 180 Stock Qty Value'

  INTO  wa_attachment SEPARATED BY  con_tab.

  CONCATENATE con_cret
  wa_attachment
  INTO wa_attachment.

  APPEND wa_attachment TO it_attachment.
  CLEAR  wa_attachment.

  DATA: lv_string_volum       TYPE string,
        lv_string_labst       TYPE string,
        lv_string_price       TYPE string,
        lv_string_stockvalue  TYPE string,
        lv_string_mlabst      TYPE string,
        lv_string_stockvalue1 TYPE string,
        lv_string_zage1       TYPE string,
        lv_string_stockcase1  TYPE string,
        lv_string_vzage1      TYPE string,
        lv_string_stockvalue2 TYPE string,
        lv_string_zage2       TYPE string,
        lv_string_stockcase2  TYPE string,
        lv_string_vzage2      TYPE string,
        lv_string_stockvalue3 TYPE string,
        lv_string_zage3       TYPE string,
        lv_string_stockcase3  TYPE string,
        lv_string_vzage3      TYPE string,
        lv_string_stockvalue4 TYPE string,
        lv_string_zage4       TYPE string,
        lv_string_stockcase4  TYPE string,
        lv_string_vzage4      TYPE string,
        lv_string_stockvalue5 TYPE string,
        lv_string_zage5       TYPE string,
        lv_string_stockcase5  TYPE string,
        lv_string_vzage5      TYPE string,
        lv_string_stockvalue6 TYPE string,
        lv_string_zage6       TYPE string,
        lv_string_stockcase6  TYPE string,
        lv_string_vzage6      TYPE string,
        lv_string_stockvalue7 TYPE string,
        lv_string_zage7       TYPE string,
        lv_string_stockcase7  TYPE string,
        lv_string_vzage7      TYPE string,
        lv_string_stockvalue8 TYPE string.

  LOOP AT gt_final INTO wa_final.

    lv_string_volum = wa_final-volum.
    lv_string_labst = wa_final-labst.
    lv_string_price = wa_final-labst.
    lv_string_stockvalue = wa_final-stockvalue.
    lv_string_mlabst = wa_final-mlabst.
    lv_string_stockvalue1 = wa_final-stockvalue1.
    lv_string_zage1 = wa_final-zage1.
    lv_string_stockcase1 = wa_final-stockcase1.
    lv_string_vzage1 = wa_final-vzage1.
    lv_string_stockvalue2 = wa_final-stockvalue2.
    lv_string_zage2 = wa_final-zage2.
    lv_string_stockcase2 = wa_final-stockcase2.
    lv_string_vzage2 = wa_final-vzage2.
    lv_string_stockvalue3 = wa_final-stockvalue3.
    lv_string_zage3 = wa_final-zage3.
    lv_string_stockcase3 = wa_final-stockcase3.
    lv_string_vzage3 = wa_final-vzage3.
    lv_string_stockvalue4 = wa_final-stockvalue4.
    lv_string_zage4 = wa_final-zage4.
    lv_string_stockcase4 = wa_final-stockcase4.
    lv_string_vzage4 = wa_final-vzage4.
    lv_string_stockvalue5 = wa_final-stockvalue5.
    lv_string_zage5 = wa_final-zage5.
    lv_string_stockcase5 = wa_final-stockcase5.
    lv_string_vzage5 = wa_final-vzage5.
    lv_string_stockvalue6 = wa_final-stockvalue6.
    lv_string_zage6 = wa_final-zage6.
    lv_string_stockcase6 = wa_final-stockcase6.
    lv_string_vzage6 = wa_final-vzage6.
    lv_string_stockvalue7 = wa_final-stockvalue7.
    lv_string_zage7 = wa_final-zage7.
    lv_string_stockcase7 = wa_final-stockcase7.
    lv_string_vzage7 = wa_final-vzage7.
    lv_string_stockvalue8 = wa_final-stockvalue8.

    CONCATENATE  wa_final-werks
                 wa_final-name1
                 wa_final-spart
                 wa_final-mmatnr
                 wa_final-maktx
                 wa_final-mtart
                 wa_final-mersda
                 lv_string_volum
                 lv_string_labst
                 wa_final-meins
                 wa_final-vprsv
                 lv_string_price
                 lv_string_stockvalue
                 lv_string_mlabst
                 lv_string_stockvalue1
                 lv_string_zage1
                 lv_string_stockcase1
                 lv_string_vzage1
                 lv_string_stockvalue2
                 lv_string_zage2
                 lv_string_stockcase2
                 lv_string_vzage2
                 lv_string_stockvalue3
                 lv_string_zage3
                 lv_string_stockcase3
                 lv_string_vzage3
                 lv_string_stockvalue4
                 lv_string_zage4
                 lv_string_stockcase4
                 lv_string_vzage4
                 lv_string_stockvalue5
                 lv_string_zage5
                 lv_string_stockcase5
                 lv_string_vzage5
                 lv_string_stockvalue6
                 lv_string_zage6
                 lv_string_stockcase6
                 lv_string_vzage6
                 lv_string_stockvalue7
                 lv_string_zage7
                 lv_string_stockcase7
                 lv_string_vzage7
                 lv_string_stockvalue8

  INTO wa_attachment SEPARATED BY con_tab.
    " FINAL TABLE TYPE FOR DETAIL REPORT

    CONCATENATE con_cret wa_attachment
    INTO wa_attachment.
    APPEND wa_attachment TO it_attachment.
    CLEAR wa_attachment.
  ENDLOOP.

ENDFORM.                    "BUILD_XLS_DATA_TABLE_DETAIL

*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail .

  DATA : it_mail_001 TYPE p0001 OCCURS 0 WITH HEADER LINE,
         wa_mail_001 TYPE p0001.

  DATA : it_month TYPE TABLE OF t247,
         wa_month TYPE t247.
*BREAK-POINT.
  CLEAR : lv_mail.
*    SELECT SINGLE USRID_LONG " Hidded By Govind
*      FROM PA0105
*      INTO LV_MAIL
*      WHERE PERNR = WA_VBRK-ERNAM
*      AND   SUBTY = '0010'
*      AND   ENDDA = '99991231'.

*    SELECT SINGLE VORNA
*      FROM PA0002
*      INTO LV_NAME
*      WHERE PERNR = WA_MAIL_001-PERNR
*      AND   ENDDA = '99991231'.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
*         IMPORTING
*     RETURN_CODE =
    TABLES
      month_names = it_month
*         EXCEPTIONS
*     MONTH_NAMES_NOT_FOUND       = 1
*     OTHERS      = 2
    .
  IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  wa_docdata-obj_name = 'MAIL WITH EXCEL ATTACHMENT'.
  wa_docdata-obj_descr = 'SAP Reports'.

  PERFORM body_of_mail USING: space, 'Desr Sir/Madam',
  'Please find the attached excel sheet'.

  DESCRIBE TABLE it_body_msg LINES g_tab_lines.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 0.
  wa_packlist-body_start = 1.
  wa_packlist-body_num   = g_tab_lines.
  wa_packlist-doc_type   = 'RAW'.

  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  "Write Packing List for Attachment
  wa_packlist-transf_bin = space.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 1.
  wa_packlist-body_start = g_tab_lines + 1.
  DESCRIBE TABLE it_attachment LINES wa_packlist-body_num.
  wa_packlist-doc_type   = 'XLS'.
  wa_packlist-obj_descr  = 'SAP Invoice Details'.
  wa_packlist-obj_name   = 'XLS_ATTACHMENT'.
  wa_packlist-doc_size   = wa_packlist-body_num * 255.
  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  APPEND LINES OF it_attachment TO it_body_msg.
  "Fill the document data and get size of attachment
  wa_docdata-obj_langu  = sy-langu.
  READ TABLE it_body_msg INTO wa_body_msg INDEX g_tab_lines.
  wa_docdata-doc_size = ( g_tab_lines - 1 ) * 255 + strlen( wa_body_msg ).
*BREAK-POINT.
  "Receivers List.
  wa_receivers-rec_type   = 'U'.  "Internet address
  wa_receivers-receiver   = 'govindarajanm@sheenlac.in'. " "LV_MAIL. ".
  wa_receivers-com_type   = 'INT'.
  wa_receivers-notif_del  = 'X'.
  wa_receivers-notif_ndel = 'X'.
  APPEND wa_receivers TO it_receivers .
  CLEAR:wa_receivers.

*   WA_RECEIVERS-REC_TYPE   = 'U'.  "Internet address
*  WA_RECEIVERS-RECEIVER   = 'govindarajanm@sheenlac.in'.
*  WA_RECEIVERS-COM_TYPE   = 'INT'.
*  WA_RECEIVERS-NOTIF_DEL  = 'X'.
*  WA_RECEIVERS-NOTIF_NDEL = 'X'.
*  APPEND WA_RECEIVERS TO IT_RECEIVERS .
*  CLEAR:WA_RECEIVERS.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'                                 " FUNCTION MODULE FOR MAIL SENDING
    EXPORTING
      document_data              = wa_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = g_sent_to_all
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = it_packlist
*     OBJECT_HEADER              =
*     CONTENTS_BIN               =
      contents_txt               = it_body_msg
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.

    WRITE: 'FAILURE'.

  ELSE.

    WAIT UP TO 2 SECONDS.

    SUBMIT rsconn01 WITH mode = 'INT'
        WITH output = 'X'
        AND RETURN.
  ENDIF.
ENDFORM.                    " SEND_MAIL

*&---------------------------------------------------------------------*
*&      Form  BODY_OF_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_MESSAGE  text
*----------------------------------------------------------------------*
FORM body_of_mail USING l_message.

  wa_body_msg = l_message.
  APPEND wa_body_msg TO it_body_msg.
  CLEAR  wa_body_msg.

ENDFORM.                    " BODY_OF_MAIL

*&---------------------------------------------------------------------*
*&      Form  BUILD_GRAPH_DATA_TABLE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_graph_data_table_detail .

ENDFORM.                    " BUILD_GRAPH_DATA_TABLE_DETAIL

*---------------------------------------------------
*&      Form  ALV_CATALOG_HEADER
*&-------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------
FORM alv_catalog_header.

  DATA : lit_header TYPE  slis_t_listheader,
         ls_line    TYPE slis_listheader.

  DATA : rv_werks(100) TYPE c,
         rv_spart(100) TYPE c,
         var_c_10      TYPE char10,
         lv_bedat(50)  TYPE c.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : rv_spart,
          rv_werks.

  CONCATENATE 'Company Code :' so_bukrs-low INTO rv_werks SEPARATED BY space.

  WRITE sy-datum TO var_c_10 DD/MM/YYYY.
  CONCATENATE 'Date:' var_c_10 INTO rv_spart SEPARATED BY space.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = ' '.
  ls_line-info = rv_werks.
  APPEND ls_line TO lit_header.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = ' '.
  ls_line-info = rv_spart.
  APPEND ls_line TO lit_header.

  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-key  = ' '.
  ls_line-info = 'Material Stock Age Wise Report - DMS' .
  APPEND ls_line TO lit_header.

*  CLEAR LS_LINE.

*CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*  EXPORTING
**    IT_LIST_COMMENTARY       = LIT_HEADER
**   I_LOGO                   = 'ZLOGO' .
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
*          .
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lit_header
      i_logo             = 'ZLOGO'.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = lv_bedat
    IMPORTING
      output = lv_bedat.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
