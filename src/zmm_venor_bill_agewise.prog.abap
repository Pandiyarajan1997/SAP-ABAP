*&---------------------------------------------------------------------*
*& Report  ZMM_VENOR_BILL_AGEWISE
*&
*&---------------------------------------------------------------------*
*&-----------------------  CREATED    ---------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  : 13 May 2014                         *
*& Title                       : Vendor Wise- Age Wise- Bill Wise Report
*& Company                     : Sheenlac Paints Pvt Ltd      *
*& Report Name                 : ZMM_VENOR_BILL_AGEWISE                  *
*& Development Id              : kpabap                               *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : ALV Hierarchical Display For          *
*&                               Vendor Wise - Age Wise -              *
*&                               BillWise Report                       *
*&---------------------------------------------------------------------*
REPORT zmm_venor_bill_agewise.



TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*& Structure & Internal Table Decleration
*&---------------------------------------------------------------------*

TYPES: BEGIN OF gs_lfa1,
         lifnr TYPE lfa1-lifnr,                    " Vendor Code
         name1 TYPE lfa1-name1,                    "Vendor Name
         ktokk TYPE lfa1-ktokk,                  " Vendor Account Group
       END OF gs_lfa1.

DATA: gt_lfa1 TYPE TABLE OF gs_lfa1,
      wa_lfa1 TYPE gs_lfa1.

TYPES: BEGIN OF gs_bsik,
         bukrs TYPE bsik-bukrs,                    " Company Code
         augdt TYPE bsik-augdt,	                                                                                                 " Clearing Date
         augbl TYPE bsik-augbl,	                                                                                                 " Document Number of the Clearing Document
         lifnr TYPE bsik-lifnr,                    " Vendor Code
         belnr TYPE bsik-belnr,                    " Accounting Document Number
         xblnr TYPE bsik-xblnr,                    " Bill Number
         dmbtr TYPE bsik-dmbtr,                    " Bill Amount ( In Doc Currency )
         zfbdt TYPE bsik-zfbdt,                    " Baseline Due Date For Calculation
         budat TYPE bsik-budat,                    " Posting Date in the Document
         bldat TYPE bsik-bldat,                    " Document Date in Document
         zbd1t TYPE bsik-zbd1t,                    " Credit Days
         prctr TYPE bsik-prctr,                    " Profit Center
         shkzg TYPE bsik-shkzg,                    " Debit/Credit Indicator
       END OF gs_bsik.

DATA: gt_bsik TYPE TABLE OF gs_bsik,
      wa_bsik TYPE gs_bsik.

TYPES: BEGIN OF gs_bsak,
         bukrs TYPE bsik-bukrs,                    " Company Code
         augdt TYPE bsik-augdt,	                                                                                                 " Clearing Date
         augbl TYPE bsik-augbl,	                                                                                                 " Document Number of the Clearing Document
         lifnr TYPE bsik-lifnr,                    " Vendor Code
         belnr TYPE bsik-belnr,                    " Accounting Document Number
         xblnr TYPE bsik-xblnr,                    " Bill Number
         dmbtr TYPE bsik-dmbtr,                    " Bill Amount ( In Doc Currency )
         zfbdt TYPE bsik-zfbdt,                    " Baseline Due Date For Calculation
         budat TYPE bsik-budat,                    " Posting Date in the Document
         bldat TYPE bsik-bldat,                    " Document Date in Document
         zbd1t TYPE bsik-zbd1t,                    " Credit Days
         prctr TYPE bsik-prctr,                    " Profit Center
         shkzg TYPE bsik-shkzg,                    " Debit/Credit Indicator
       END OF gs_bsak.

DATA: gt_bsak TYPE TABLE OF gs_bsak,
      wa_bsak TYPE gs_bsak.

TYPES: BEGIN OF gs_header,
         lifnr     TYPE lfa1-lifnr,                    " Vendor Code
         name1     TYPE lfa1-name1,                    " Vendor Name
         dmbtr     TYPE bsik-dmbtr,                    " Total Bill Amount ( In Doc Currency )
         zndue_amt TYPE bsik-dmbtr,                " Bill Amount – Not Due
         za1_amt   TYPE bsik-dmbtr,                  " A1-Amount Due Less Than 30 Days
         za2_amt   TYPE bsik-dmbtr,                  " A2-Amount Due Between 31 – 60 Days
         za3_amt   TYPE bsik-dmbtr,                  " A3-Amount Due Between 61 – 90 Days
         za4_amt   TYPE bsik-dmbtr,                  " A4-Amount Due Between 91 - 180  Days
         za5_amt   TYPE bsik-dmbtr,                  " A5-Amount Due – Above 180  Days
         expand,
       END OF gs_header.

DATA: gt_header TYPE TABLE OF gs_header,
      wa_header TYPE gs_header.

TYPES: BEGIN OF gs_item,
         lifnr         TYPE lfa1-lifnr,                    " Vendor Code
         name1         TYPE lfa1-name1,                     " Vendor Name
         belnr         TYPE bsik-belnr,                    " Accounting Document Number
         xblnr         TYPE bsik-xblnr,                    " Bill Number
         budat         TYPE bsik-budat,                    " Posting Date in the Document
         bldat         TYPE bsik-bldat,                    " Document Date in Document
         zfbdt         TYPE bsik-zfbdt,                    " Baseline Due Date For Calculation
         dmbtr         TYPE bsik-dmbtr,                    " Total Bill Amount ( In Doc Currency )
         zbd1t         TYPE bsik-zbd1t,                    " Credit Days
         zfaedt        TYPE bsik-zfbdt,                   " Net Due Date For Payment
         zndue_amt     TYPE bsik-dmbtr,                " Bill Amount – Not Due
         zb1_amt       TYPE bsik-dmbtr,                  " B1-Amount Due Less Than 30 Days
         zb2_amt       TYPE bsik-dmbtr,                  " B2-Amount Due Between 31 – 60 Days
         zb3_amt       TYPE bsik-dmbtr,                  " B3-Amount Due Between 61 – 90 Days
         zb4_amt       TYPE bsik-dmbtr,                  " B4-Amount Due Between 91 - 180  Days
         zb5_amt       TYPE bsik-dmbtr,                  " B5-Amount Due – Above 180  Days
*------ Added by Samsudeen M ON 17.02.2023------------------------------------------------------ *
         ktokk         TYPE ktokk,
         ktokk_des     TYPE txt30,
         group_feature TYPE bp_group_feature,
         gf_text       TYPE bp_group_feature_name,
       END OF gs_item.

DATA: gt_item TYPE TABLE OF gs_item,
      wa_item TYPE gs_item.

DATA: lv_zfaedt  TYPE sy-datum,
      lv_ndueamt TYPE p DECIMALS 2,
      t_ndueamt  TYPE p DECIMALS 2,
      t_days     TYPE i,
      tot_amnt   TYPE p DECIMALS 2,
      a1_amnt    TYPE p DECIMALS 2,
      a2_amnt    TYPE p DECIMALS 2,
      a3_amnt    TYPE p DECIMALS 2,
      a4_amnt    TYPE p DECIMALS 2,
      a5_amnt    TYPE p DECIMALS 2,
      b1_amnt    TYPE p DECIMALS 2,
      b2_amnt    TYPE p DECIMALS 2,
      b3_amnt    TYPE p DECIMALS 2,
      b4_amnt    TYPE p DECIMALS 2,
      b5_amnt    TYPE p DECIMALS 2.

DATA: or_lifnr TYPE lfa1-lifnr,
      or_bukrs TYPE t001-bukrs,
      or_prctr TYPE cepc-prctr,
      or_ktokk TYPE t077k-ktokk.

TYPES: BEGIN OF gs_faglflexa,
         docnr TYPE faglflexa-docnr,
         prctr TYPE faglflexa-prctr,
         bschl TYPE faglflexa-bschl,
       END OF gs_faglflexa.

DATA: gt_faglflexa  TYPE TABLE OF gs_faglflexa,
      wa_faglflexa  TYPE gs_faglflexa,
      gt_faglflexa1 TYPE TABLE OF gs_faglflexa,
      wa_faglflexa1 TYPE gs_faglflexa.


*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: gt_fcat   TYPE slis_t_fieldcat_alv,
      wa_fcat   TYPE slis_fieldcat_alv,
      it_layout TYPE slis_layout_alv,
      gt_events TYPE slis_t_event,
      wa_events TYPE slis_alv_event,
      key       TYPE slis_keyinfo_alv.

DATA: gt_sort TYPE slis_t_sortinfo_alv,
      wa_sort TYPE slis_sortinfo_alv.

DATA: ls_variant TYPE disvariant.

DATA: lv_date  TYPE sy-datum,
      lv_docnr TYPE faglflexa-docnr..

DATA : l_prctr TYPE cepc-prctr,
       l_lifnr TYPE lfa1-lifnr,
       l_ktokk TYPE lfa1-ktokk.

*&---------------------------------------------------------------------*
*& Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: rd_pro RADIOBUTTON GROUP g3,  "USER-COMMAND UCOM MODIF ID MOD,
              rd_com RADIOBUTTON GROUP g3 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS : so_bukrs FOR or_bukrs MODIF ID mod,
                   so_prctr FOR or_prctr MODIF ID md1.
SELECTION-SCREEN: END OF BLOCK b5.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_date TYPE sy-datum DEFAULT sy-datum OBLIGATORY.
  SELECT-OPTIONS: so_lifnr FOR or_lifnr,
                  so_ktokk FOR or_ktokk.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: rd_basdt RADIOBUTTON GROUP g1 DEFAULT 'X',
              rd_ivpdt RADIOBUTTON GROUP g1,
              rd_invdt RADIOBUTTON GROUP g1.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: rd_woh RADIOBUTTON GROUP g2 DEFAULT 'X',
              rd_wih RADIOBUTTON GROUP g2.
SELECTION-SCREEN: END OF BLOCK b3.
*------- Added by Samsudeen on 22.02.2023
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-007.
  PARAMETERS: rd_hdfc  RADIOBUTTON GROUP g4 DEFAULT 'X',
              rd_nhdfc RADIOBUTTON GROUP g4,
              rd_all   RADIOBUTTON GROUP g4.
SELECTION-SCREEN END OF BLOCK b6.
*----------- End of Changes on 22.02.2022
AT SELECTION-SCREEN OUTPUT.

*  LOOP AT SCREEN.
*    IF rd_pro = 'X'.
*      IF screen-group1 = 'MOD'.
*        screen-input = 1.
*        screen-invisible = 0.
*      ENDIF.
*    ELSE.
*      IF screen-group1 = 'MD1'.
*        screen-input = 0.
*        screen-invisible = 1.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.

AT SELECTION-SCREEN .
  IF so_prctr IS NOT INITIAL .
    SELECT SINGLE prctr FROM cepc INTO l_prctr WHERE prctr IN so_prctr .
    IF sy-subrc <> 0.
*    MESSAGE E001(YMSG) .
      MESSAGE ' Enter valid profit center' TYPE 'E'.
    ENDIF.
  ENDIF .
  IF so_lifnr IS NOT INITIAL.
    SELECT SINGLE lifnr FROM lfa1 INTO l_lifnr WHERE lifnr IN so_lifnr.
    IF sy-subrc NE 0.
      MESSAGE 'Enter valid Vendor Code' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF so_ktokk IS NOT INITIAL.
    SELECT SINGLE ktokk FROM lfa1 INTO l_ktokk WHERE ktokk IN so_ktokk .
    IF sy-subrc NE 0.
      MESSAGE 'Enter Valid Vendor Group' TYPE 'E'.
    ENDIF.
  ENDIF.


START-OF-SELECTION.
  REFRESH: gt_bsik,gt_bsak,gt_faglflexa,gt_lfa1.
*&---------------------------------------------------------------------*
*& Main Logic of Program
*&---------------------------------------------------------------------*

    lv_date = p_date + 1.

  IF rd_com = 'X'.

    IF rd_basdt = 'X'.
      SELECT
        bukrs
        augdt
        augbl
        lifnr
        belnr
        xblnr
        dmbtr
        zfbdt
        budat
        bldat
        zbd1t
        prctr
        shkzg FROM bsik INTO TABLE gt_bsik
        WHERE bukrs IN so_bukrs AND
              umskz NE 'F' AND
              lifnr IN so_lifnr AND
              zfbdt <= p_date.

      SELECT
            lifnr
            augdt
            augbl
            belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
            xblnr
* end Modification
            dmbtr
            budat
            bldat
            zfbdt
            zbd1t
            prctr
            shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak
            WHERE bukrs IN so_bukrs AND
                  umskz NE 'F' AND
                  lifnr IN so_lifnr AND
                  augdt BETWEEN lv_date AND sy-datum AND zfbdt <= p_date.

    ELSEIF rd_ivpdt = 'X'.
      SELECT
        bukrs
        augdt
        augbl
        lifnr
        belnr
        xblnr
        dmbtr
        zfbdt
        budat
        bldat
        zbd1t
        prctr
        shkzg FROM bsik INTO TABLE gt_bsik
        WHERE bukrs IN so_bukrs AND
              umskz NE 'F' AND
              lifnr IN so_lifnr AND
              budat <= p_date.

      SELECT
      lifnr
      augdt
      augbl
      belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
      xblnr
* end Modification
      dmbtr
      budat
      bldat
      zfbdt
      zbd1t
      prctr
      shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak
      WHERE bukrs IN so_bukrs AND
            umskz NE 'F' AND
            lifnr IN so_lifnr AND
            augdt BETWEEN lv_date AND sy-datum AND budat <= p_date.


    ELSEIF rd_invdt = 'X'.
      SELECT
        bukrs
        augdt
        augbl
        lifnr
        belnr
        xblnr
        dmbtr
        zfbdt
        budat
        bldat
        zbd1t
        prctr
        shkzg FROM bsik INTO TABLE gt_bsik
        WHERE bukrs IN so_bukrs AND
              umskz NE 'F' AND
              lifnr IN so_lifnr AND
              bldat <= p_date.
      SELECT
      lifnr
      augdt
      augbl
      belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
      xblnr
* end Modification
      dmbtr
      budat
      bldat
      zfbdt
      zbd1t
      prctr
      shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak
      WHERE bukrs IN so_bukrs AND
            umskz NE 'F' AND
            lifnr IN so_lifnr AND
            augdt BETWEEN lv_date AND sy-datum AND bldat <= p_date.

* Commented bY venkat
*    SELECT
*      BUKRS
*      AUGDT
*      AUGBL
*      LIFNR
*      BELNR
*      XBLNR
*      DMBTR
*      ZFBDT
*      BUDAT
*      BLDAT
*      ZBD1T
*      PRCTR
*      SHKZG FROM BSIK INTO  TABLE GT_BSIK
*      WHERE BUKRS = '1000' AND
*            UMSKZ NE 'F' AND
*            LIFNR IN SO_LIFNR.
* End Comment
    ENDIF.




  ELSEIF rd_pro = 'X'.

    IF so_prctr IS INITIAL.
      SELECT
       docnr
       prctr
       bschl FROM faglflexa INTO CORRESPONDING FIELDS OF TABLE gt_faglflexa.
      SORT gt_faglflexa BY docnr.
      DELETE ADJACENT DUPLICATES FROM gt_faglflexa COMPARING docnr.

    ELSEIF so_prctr IS NOT INITIAL.

      SELECT
      docnr
      prctr
      bschl FROM faglflexa INTO CORRESPONDING FIELDS OF TABLE gt_faglflexa1.

      LOOP AT gt_faglflexa1 INTO wa_faglflexa1 .
        IF wa_faglflexa1-docnr NE lv_docnr.
          lv_docnr = wa_faglflexa1-docnr.
          IF wa_faglflexa1-prctr IN so_prctr.
            APPEND wa_faglflexa1 TO gt_faglflexa.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*
*  SELECT
*   DOCNR
*   PRCTR
*   BSCHL FROM FAGLFLEXA INTO CORRESPONDING FIELDS OF TABLE GT_FAGLFLEXA
*   WHERE PRCTR IN SO_PRCTR.
*
*  SORT GT_FAGLFLEXA BY DOCNR.
*  DELETE ADJACENT DUPLICATES FROM GT_FAGLFLEXA COMPARING DOCNR.

    IF gt_faglflexa[] IS NOT INITIAL.

      IF rd_basdt = 'X'.
        SELECT
           bukrs
          augdt
           augbl
           lifnr
           belnr
           xblnr
           dmbtr
           zfbdt
           budat
           bldat
           zbd1t
           prctr
           shkzg FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik FOR ALL ENTRIES IN gt_faglflexa
           WHERE belnr = gt_faglflexa-docnr AND
                 umskz NE 'F' AND
                 lifnr IN so_lifnr AND
                 zfbdt <= p_date.

        SELECT
    lifnr
    augdt
    augbl
    belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
    xblnr
* End Modification
    dmbtr
    budat
    bldat
    zfbdt
    zbd1t
    prctr
    shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak FOR ALL ENTRIES IN gt_faglflexa
    WHERE belnr = gt_faglflexa-docnr AND
          umskz NE 'F' AND
          lifnr IN so_lifnr AND
                   augdt BETWEEN lv_date AND sy-datum AND zfbdt <= p_date.

      ELSEIF rd_ivpdt = 'X'.
        SELECT
           bukrs
           augdt
           augbl
           lifnr
           belnr
           xblnr
           dmbtr
           zfbdt
           budat
           bldat
           zbd1t
           prctr
           shkzg FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik FOR ALL ENTRIES IN gt_faglflexa
           WHERE belnr = gt_faglflexa-docnr AND
                 umskz NE 'F' AND
                 lifnr IN so_lifnr AND
                 budat <= p_date.

        SELECT
    lifnr
    augdt
    augbl
    belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
    xblnr
* End Modification
    dmbtr
    budat
    bldat
    zfbdt
    zbd1t
    prctr
    shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak FOR ALL ENTRIES IN gt_faglflexa
    WHERE belnr = gt_faglflexa-docnr AND
          umskz NE 'F' AND
          lifnr IN so_lifnr AND
                   augdt BETWEEN lv_date AND sy-datum AND budat <= p_date.


      ELSEIF rd_invdt = 'X'.
        SELECT
           bukrs
           augdt
           augbl
           lifnr
           belnr
           xblnr
           dmbtr
           zfbdt
           budat
           bldat
           zbd1t
           prctr
           shkzg FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik FOR ALL ENTRIES IN gt_faglflexa
           WHERE belnr = gt_faglflexa-docnr AND
                 umskz NE 'F' AND
                 lifnr IN so_lifnr AND
                 bldat <= p_date.

        SELECT
    lifnr
    augdt
    augbl
    belnr
* Modification by Venkat for Invoice Number  on 05/06/2013
    xblnr
* End Modification
    dmbtr
    budat
    bldat
    zfbdt
    zbd1t
    prctr
    shkzg FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak FOR ALL ENTRIES IN gt_faglflexa
    WHERE belnr = gt_faglflexa-docnr AND
          umskz NE 'F' AND
          lifnr IN so_lifnr AND
                   augdt BETWEEN lv_date AND sy-datum AND bldat <= p_date.
      ENDIF.


    ENDIF.


  ENDIF.
* Replaced by Venkat 05/06/2013
*LOOP AT GT_BSAK INTO WA_BSAK.
*  IF WA_BSAK-AUGBL = WA_BSAK-BELNR.
*    DELETE GT_BSAK WHERE BELNR = WA_BSAK-AUGBL.
*  ENDIF.
*ENDLOOP.
*Sort GT_BSAK by BELNR AUGBL.
*delete adjacent duplicates from gt_bsak comparing belnr augbl.
* End replacement

  IF gt_bsak[] IS NOT INITIAL.
    APPEND LINES OF gt_bsak TO gt_bsik.
  ENDIF.

  IF gt_bsik[] IS NOT INITIAL.
    SELECT
      lifnr
      name1
      ktokk FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_bsik
      WHERE lifnr = gt_bsik-lifnr AND
            ktokk IN so_ktokk.
*--------- Added by Samsudeen M on 17.02.2023 --------------------------*
    IF sy-subrc = 0.
      " Fetching Vendor Account Group Text
      SELECT * FROM t077y  INTO TABLE @DATA(gt_ktokk_txt)
                           FOR ALL ENTRIES IN @gt_lfa1
                           WHERE spras = @sy-langu
                           AND ktokk = @gt_lfa1-ktokk.
      IF sy-subrc = 0.
        SORT gt_ktokk_txt[] BY ktokk.
      ENDIF.
      "Fetching Grouping Feature which type of Vendor
      SELECT supplier, businesspartner FROM ibpsupplier
                                       INTO TABLE @DATA(gt_businesspartner)
                                       FOR ALL ENTRIES IN @gt_lfa1
                                       WHERE supplier = @gt_lfa1-lifnr.
      IF sy-subrc = 0.
        "Fetching Grouping Feature Text
        SELECT a~partner,
               a~group_feature,
               b~group_feature_na INTO TABLE @DATA(gt_group_txt)
                                  FROM bp001 AS a INNER JOIN tp24t AS b
                                  ON a~group_feature = b~group_feature
                                  FOR ALL ENTRIES IN @gt_businesspartner
                                  WHERE a~partner = @gt_businesspartner-businesspartner
                                  AND b~spras = @sy-langu.
        IF sy-subrc = 0.
          SORT gt_group_txt[] BY partner.
        ENDIF.
      ENDIF.
    ENDIF.
*----------- End of Changes by Samsudeen on 17.02.2023 -----------------------------------*
  ENDIF.

  LOOP AT gt_lfa1 INTO wa_lfa1.

    LOOP AT gt_bsik INTO wa_bsik WHERE lifnr = wa_lfa1-lifnr.

      IF rd_basdt = 'X'.
        lv_zfaedt = wa_bsik-zfbdt + wa_bsik-zbd1t.   " Net Due Date For Payment Using Base Line Date
      ELSEIF rd_invdt = 'X'.
        lv_zfaedt = wa_bsik-bldat + wa_bsik-zbd1t.   " Net Due Date For Payment Using Invoice Date
      ELSEIF rd_ivpdt = 'X'.
        lv_zfaedt = wa_bsik-budat + wa_bsik-zbd1t.   " Net Due Date For Payment Invoice Posting Date
      ENDIF.

      IF lv_zfaedt >= p_date.
        IF wa_bsik-shkzg = 'S'.
          lv_ndueamt = lv_ndueamt + wa_bsik-dmbtr.
          t_ndueamt = t_ndueamt + wa_bsik-dmbtr.
        ELSEIF wa_bsik-shkzg = 'H'.
          lv_ndueamt = lv_ndueamt - wa_bsik-dmbtr.
          t_ndueamt = t_ndueamt - wa_bsik-dmbtr.
        ENDIF.

      ELSEIF lv_zfaedt < p_date.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            begda = lv_zfaedt
            endda = p_date
          IMPORTING
            days  = t_days.

        IF t_days <= 30.
          IF wa_bsik-shkzg = 'S'.
            a1_amnt = a1_amnt + wa_bsik-dmbtr.
            b1_amnt = b1_amnt + wa_bsik-dmbtr.
          ELSEIF wa_bsik-shkzg = 'H'.
            a1_amnt = a1_amnt - wa_bsik-dmbtr.
            b1_amnt = b1_amnt - wa_bsik-dmbtr.
          ENDIF.
        ELSEIF t_days BETWEEN 31 AND 60.
          IF wa_bsik-shkzg = 'S'.
            a2_amnt = a2_amnt + wa_bsik-dmbtr.
            b2_amnt = b2_amnt + wa_bsik-dmbtr.
          ELSEIF wa_bsik-shkzg = 'H'.
            a2_amnt = a2_amnt - wa_bsik-dmbtr.
            b2_amnt = b2_amnt - wa_bsik-dmbtr.
          ENDIF.
        ELSEIF t_days BETWEEN 61 AND 90.
          IF wa_bsik-shkzg = 'S'.
            a3_amnt = a3_amnt + wa_bsik-dmbtr.
            b3_amnt = b3_amnt + wa_bsik-dmbtr.
          ELSEIF wa_bsik-shkzg = 'H'.
            a3_amnt = a3_amnt - wa_bsik-dmbtr.
            b3_amnt = b3_amnt - wa_bsik-dmbtr.
          ENDIF.
        ELSEIF t_days BETWEEN 91 AND 180.
          IF wa_bsik-shkzg = 'S'.
            a4_amnt = a4_amnt + wa_bsik-dmbtr.
            b4_amnt = b4_amnt + wa_bsik-dmbtr.
          ELSEIF wa_bsik-shkzg = 'H'.
            a4_amnt = a4_amnt - wa_bsik-dmbtr.
            b4_amnt = b4_amnt - wa_bsik-dmbtr.
          ENDIF.
        ELSEIF t_days > 180.
          IF wa_bsik-shkzg = 'S'.
            a5_amnt = a5_amnt + wa_bsik-dmbtr.
            b5_amnt = b5_amnt + wa_bsik-dmbtr.
          ELSEIF wa_bsik-shkzg = 'H'.
            a5_amnt = a5_amnt - wa_bsik-dmbtr.
            b5_amnt = b5_amnt - wa_bsik-dmbtr.
          ENDIF.
        ENDIF.

      ENDIF.

      IF wa_bsik-shkzg = 'S'.
        tot_amnt = tot_amnt + wa_bsik-dmbtr.
        wa_item-dmbtr = wa_bsik-dmbtr.
      ELSEIF wa_bsik-shkzg = 'H'.
        tot_amnt = tot_amnt - wa_bsik-dmbtr.
        wa_item-dmbtr = wa_bsik-dmbtr * -1.
      ENDIF.

      wa_item-lifnr = wa_bsik-lifnr.
      wa_item-name1 = wa_lfa1-name1.
      wa_item-belnr = wa_bsik-belnr.
      wa_item-xblnr = wa_bsik-xblnr.
      wa_item-zfbdt = wa_bsik-zfbdt.
      wa_item-budat = wa_bsik-budat.
      wa_item-bldat = wa_bsik-bldat.
      wa_item-zbd1t = wa_bsik-zbd1t.
      wa_item-zfaedt = lv_zfaedt.
      wa_item-zndue_amt = lv_ndueamt.
      wa_item-zb1_amt = b1_amnt.
      wa_item-zb2_amt = b2_amnt.
      wa_item-zb3_amt = b3_amnt.
      wa_item-zb4_amt = b4_amnt.
      wa_item-zb5_amt = b5_amnt.
*------ Added by Samsudeen M on 17.02.2023 ---------------------------------*
      wa_item-ktokk = wa_lfa1-ktokk. "Vendor Account group
      wa_item-ktokk_des = VALUE #( gt_ktokk_txt[ ktokk = wa_lfa1-ktokk ]-txt30 OPTIONAL ). "Vendor Account Group text
      DATA(lv_bpartner) = VALUE #( gt_businesspartner[ supplier = wa_lfa1-lifnr ]-businesspartner OPTIONAL ).
      IF lv_bpartner IS NOT INITIAL.
        DATA(ls_group) = VALUE #( gt_group_txt[ partner = lv_bpartner ] OPTIONAL ).
        IF ls_group IS NOT INITIAL.
          wa_item-group_feature = ls_group-group_feature. "Grouping Feature
          wa_item-gf_text = ls_group-group_feature_na. "Grouping Feature Text
        ENDIF.
      ENDIF.
*-------------- End of Changes on 17.02.2023 ------------------------------*
      APPEND wa_item TO gt_item.
      CLEAR: wa_item,b1_amnt,b2_amnt,b3_amnt,b4_amnt,b5_amnt,wa_bsik, lv_ndueamt,lv_bpartner,ls_group.

    ENDLOOP.

    wa_header-lifnr = wa_lfa1-lifnr.
    wa_header-name1 = wa_lfa1-name1.
    wa_header-dmbtr = tot_amnt.
    wa_header-zndue_amt = t_ndueamt.
    wa_header-za1_amt = a1_amnt.
    wa_header-za2_amt = a2_amnt.
    wa_header-za3_amt = a3_amnt.
    wa_header-za4_amt = a4_amnt.
    wa_header-za5_amt = a5_amnt.

    APPEND wa_header TO gt_header.
    CLEAR: wa_header, wa_lfa1,tot_amnt,t_ndueamt,
           a1_amnt,a2_amnt,a3_amnt,a4_amnt,a5_amnt.
  ENDLOOP.

  IF rd_basdt = 'X'.
    SORT gt_item BY bldat. "changed by prasad (ZFBDT)
  ELSEIF
     rd_ivpdt = 'X'.
    SORT gt_item BY bldat. "changed by prasad (BUDAT)
  ELSEIF
     rd_invdt = 'X'.
    SORT gt_item BY  bldat.
  ENDIF.
*-------------- Added by Samsudeen M on 22.02.2023 -------------------------------*
  IF rd_all NE abap_true.
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'BAPI_OPEN_INVOICE_VENTYP'
                                                   AND type = 'S'.
    IF sy-subrc = 0.

    ENDIF.
  ENDIF.
  "Only HDFC Vendors
  IF rd_hdfc EQ abap_true.
    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
      DATA(lv_ventype) = VALUE #( lt_tvarvc[ low = <fs_item>-group_feature ]-low OPTIONAL ).
      IF lv_ventype IS INITIAL.
        DELETE gt_item WHERE lifnr = <fs_item>-lifnr.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-------------- Non HDFC Vendors --------------------------------------*
  "Non HDFC Vendor Only
  IF rd_nhdfc EQ abap_true.
    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item1>).
      DATA(lv_ven_type) = VALUE #( lt_tvarvc[ low = <fs_item1>-group_feature ]-low OPTIONAL ).
      IF lv_ven_type IS NOT INITIAL.
        DELETE gt_item WHERE lifnr = <fs_item1>-lifnr.
      ENDIF.
    ENDLOOP.
  ENDIF.
*------------   End Of Changes on 22.02.2023 ----------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&  ALV Layout
*&---------------------------------------------------------------------*
  REFRESH:gt_fcat.
  IF rd_wih = 'X'.
    PERFORM alv_layout USING 1 'Vendor Code' 'LIFNR' 'GT_HEADER' ''.
    PERFORM alv_layout USING 2 'Vendor Name' 'NAME1' 'GT_HEADER' ''.
    PERFORM alv_layout USING 5 '       Total Payable Amt' 'DMBTR' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 7 '               Amount – Not Due' 'ZNDUE_AMT' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 8 '< 30 Days' 'ZA1_AMT' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 9 '31-60 Days' 'ZA2_AMT' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 10 '61-90 Days' 'ZA3_AMT' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 11 '91-180 Days' 'ZA4_AMT' 'GT_HEADER' 'X'.
    PERFORM alv_layout USING 12 'Above 180 Days' 'ZA5_AMT' 'GT_HEADER' 'X'.
  ENDIF.

  IF rd_woh = 'X'.
    PERFORM alv_layout USING 1 'Vendor No' 'LIFNR' 'GT_ITEM' ' '.
    PERFORM alv_layout USING 2 'Vendor Name' 'NAME1' 'GT_ITEM' ' '.
  ENDIF.

  PERFORM alv_layout USING 3 'Document Number' 'BELNR' 'GT_ITEM' ''.
  PERFORM alv_layout USING 4 'Bill Number' 'XBLNR' 'GT_ITEM' ''.


  IF rd_basdt = 'X'.
    PERFORM alv_layout USING 6 'Baseline Due Date' 'ZFBDT' 'GT_ITEM' ''.
    PERFORM alv_layout USING 5 'Invoice Date' 'BLDAT' 'GT_ITEM' ''.
  ELSEIF rd_invdt = 'X'.
    PERFORM alv_layout USING 6 'Invoice Date' 'BLDAT' 'GT_ITEM' ''.
  ELSEIF rd_ivpdt = 'X'.
    PERFORM alv_layout USING 6 'Invoice Posting Date' 'BUDAT' 'GT_ITEM' ''.
    PERFORM alv_layout USING 5 'Invoice Date' 'BLDAT' 'GT_ITEM' ''.
  ENDIF.

  PERFORM alv_layout USING 7 'Bill Amount(OS)' 'DMBTR' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 8 'Credit Days' 'ZBD1T' 'GT_ITEM' ''.
  PERFORM alv_layout USING 9 'Due Date' 'ZFAEDT' 'GT_ITEM' ''.
  PERFORM alv_layout USING 10 'Amount – Not Due' 'ZNDUE_AMT' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 11 '< 30 Days' 'ZB1_AMT' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 12 '31-60 Days' 'ZB2_AMT' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 13 '61-90 Days' 'ZB3_AMT' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 14 '91-180 Days' 'ZB4_AMT' 'GT_ITEM' 'X'.
  PERFORM alv_layout USING 15 'Above 180 Days' 'ZB5_AMT' 'GT_ITEM' 'X'.
*---- Added by Samsudeen M on 17.02.2023----------------------------------------
  PERFORM alv_layout USING 16 'Vendor Accgrp'      'KTOKK'         'GT_ITEM' space.
  PERFORM alv_layout USING 17 'Vendor Accgrp text' 'KTOKK_DES'     'GT_ITEM' space.
  PERFORM alv_layout USING 18 'Grouping Feature'   'GROUP_FEATURE' 'GT_ITEM' space.
  PERFORM alv_layout USING 19 'Group Text'         'GF_TEXT'       'GT_ITEM' space.
*------ End of Changes on 17.02.2023 ---------------------------------------------

  IF rd_woh = 'X'.
    wa_sort-spos = 1.
    wa_sort-fieldname = 'LIFNR'.
    wa_sort-tabname = 'GT_ITEM'.
    wa_sort-up = 'X'.
    wa_sort-subtot = 'X'.                "SUBTOTAL BY THIS FIELD

    APPEND wa_sort TO gt_sort.
    CLEAR wa_sort.
  ENDIF.

  ls_variant-report = sy-repid.
*&---------------------------------------------------------------------*
*&   ALV Hierarchical Display
*&---------------------------------------------------------------------*
  IF rd_wih = 'X'.
*layout
    PERFORM build_layout.
*key information for hierarchy
    PERFORM build_key.
    PERFORM alv_hierarchical_display.
  ELSEIF rd_woh = 'X'.
    PERFORM alv_grid_display.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Dynamic Property Change For Selection Screen
*&---------------------------------------------------------------------*




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
FORM alv_layout  USING    p1 p2 p3 p4 p5.
  CLEAR wa_fcat.
  wa_fcat-col_pos = p1.
  wa_fcat-seltext_l = p2.
  wa_fcat-fieldname = p3.
  wa_fcat-tabname = p4.
  wa_fcat-do_sum = p5.
  APPEND wa_fcat TO gt_fcat.

ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_HIERARCHICAL_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_hierarchical_display .

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
      i_callback_program = sy-cprog
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     I_CALLBACK_USER_COMMAND        = ' '
      is_layout          = it_layout
      it_fieldcat        = gt_fcat[]
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE  = 0
      i_default          = 'X'
      i_save             = 'X'
      is_variant         = ls_variant
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
      i_tabname_header   = 'GT_HEADER'
      i_tabname_item     = 'GT_ITEM'
*     I_STRUCTURE_NAME_HEADER        =
*     I_STRUCTURE_NAME_ITEM          =
      is_keyinfo         = key
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
*     IR_SALV_HIERSEQ_ADAPTER        =
*     IT_EXCEPT_QINFO    =
*     I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab_header    = gt_header[]
      t_outtab_item      = gt_item[]
*   EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ALV_HIERARCHICAL_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .

*to expand the header table for item details
  it_layout-expand_fieldname = 'EXPAND'.

  it_layout-window_titlebar = 'VENDOR WISE - BILL WISE - AGEWISE DISPLAY'.
  it_layout-lights_tabname = 'GT_ITEM'.
  it_layout-colwidth_optimize = 'X'.
*  IT_LAYOUT-NO_HLINE = 'X'.
*  IT_LAYOUT-NO_VLINE = 'X'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_key .
*key infomation for the header and item table
  key-header01 = 'LIFNR'.
  key-item01 = 'LIFNR'.
ENDFORM.                    " BUILD_KEY
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
*     I_CALLBACK_PROGRAM                = ' '
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
*     IS_LAYOUT   =
      it_fieldcat = gt_fcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort     = gt_sort[]
*     IT_FILTER   =
*     IS_SEL_HIDE =
      i_default   = 'X'
      i_save      = 'X'
      is_variant  = ls_variant
*     IT_EVENTS   =
*     IT_EVENT_EXIT                     =
*     IS_PRINT    =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab    = gt_item[]
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS      = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
