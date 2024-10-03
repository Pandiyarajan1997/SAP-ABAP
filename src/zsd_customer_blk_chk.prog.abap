*&---------------------------------------------------------------------*
*& Report ZSD_CUSTOMER_BLK_CHK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_customer_blk_chk.
*************customer block check class***********
DATA : lo_block TYPE REF TO zcl_common_check.
CREATE OBJECT lo_block.
DATA : lt_block     TYPE TABLE OF zsd_st_cust_block,
       lt_customer  TYPE TABLE OF zcust_blk_chk,
       ls_customer  TYPE zcust_blk_chk,
       lt_cust_1000 TYPE TABLE OF zcust_blk_chk,
       ls_cust_1000 TYPE zcust_blk_chk,
       lt_alv       TYPE TABLE OF zcust_blk_chk.

PARAMETERS : p_chk AS CHECKBOX.

START-OF-SELECTION.

  IF p_chk = abap_true.
    PERFORM update.
  ENDIF.

end-of-SELECTION.
*&---------------------------------------------------------------------*
*& Form update
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update .

  DATA: lt_zcust_1000 TYPE TABLE OF zcust_blk_chk,
        ls_zcuct_1000 TYPE zcust_blk_chk.

  DATA: lt_knvp TYPE TABLE OF knvp,
        ls_knvp TYPE knvp.

  DATA: lt_custblkchk TYPE TABLE OF zcust_blk_chk,
        ls_custblkchk TYPE zcust_blk_chk.


*****************get the customer details***********
  SELECT kunnr, name1, werks, stras, ort01,
         pstlz, Regio, ktokd, lifnr,
         katr2, stcd3, j_1ipanno, adrnr, telf1, telf2,
         zkostl, zfincode, zcustype  FROM kna1 INTO TABLE @DATA(lt_kna1).
  IF sy-subrc = 0.
    SORT : lt_kna1 BY kunnr.

    SELECT addrnumber, smtp_addr
      FROM adr6 INTO TABLE @DATA(lt_adr6)
      FOR ALL ENTRIES IN @lt_kna1
      WHERE addrnumber = @lt_kna1-adrnr.
    IF sy-subrc = 0.
      SORT lt_adr6 BY addrnumber.
    ENDIF.
  ENDIF.

  SELECT * FROM zcust_blk_chk INTO TABLE lt_custblkchk.
  IF sy-subrc = 0.
    SORT lt_custblkchk BY bukrs vkorg kunnr.
  ENDIF.

*attribute 2 description
  SELECT * FROM tvk2t INTO TABLE @DATA(lt_tvk2t) WHERE spras = @sy-langu.
  IF sy-subrc = 0.
    SORT lt_tvk2t BY katr2.
  ENDIF.

* group feature description
  SELECT * FROM tp24t INTO TABLE @DATA(lt_tp24t) WHERE spras = @sy-langu.

*Partner and GUID
  SELECT a~kunnr, c~partner, c~group_feature INTO TABLE @DATA(lt_BP001)
    FROM kna1 AS a INNER JOIN cvi_cust_link AS b ON a~kunnr = b~customer
    INNER JOIN but000 AS d ON b~partner_guid = d~partner_guid
    INNER JOIN bp001 AS c ON c~partner = d~partner.
  IF sy-subrc = 0.
    SORT lt_bp001 BY kunnr.
  ENDIF.

*Customer Group Description
  SELECT * FROM t151t INTO TABLE @DATA(lt_T151T) WHERE spras = @sy-langu.
  IF sy-subrc = 0.
    SORT lt_T151T BY kdgrp.
  ENDIF.

* Get employee names from PA0001
  SELECT pernr, ename FROM pa0001
    INTO TABLE @DATA(lt_pa0001)
    WHERE begda LE @sy-datum AND endda GE @sy-datum.
  IF sy-subrc = 0.
    SORT lt_pa0001 BY pernr.
  ENDIF.

* Get position master data
  SELECT objid,stext FROM hrp1000
    INTO TABLE @DATA(lt_hrp1000)
    WHERE plvar = '01'
      AND otype = 'S'
      AND objid BETWEEN '20000000' AND '29999999'
      AND begda <= @sy-datum
      AND endda >= @sy-datum.
  IF sy-subrc = 0.
    SORT lt_hrp1000 BY objid.
  ENDIF.

*KNVV to get customer group
  SELECT kunnr, vkorg, kdgrp, vkbur FROM knvv INTO TABLE @DATA(lt_knvv)
    WHERE vkorg IN ('1000', '6000', 'SDMS') AND vtweg = '20' AND spart = '10'.
  IF sy-subrc = 0.
    SORT lt_knvv BY kunnr vkorg.
  ENDIF.

*get the e-invoice distributor flag
  SELECT distributor FROM zdist_einv_dtls INTO TABLE @DATA(lt_einvdist).
  IF sy-subrc = 0.
    SORT lt_einvdist BY distributor.
  ENDIF.

*check is GST chk completed
  SELECT kunnr, msg_type, STATUS FROM zcust_gst_chk INTO TABLE @DATA(lt_gstchk).
  IF sy-subrc = 0.
    SORT lt_gstchk BY kunnr.
  ENDIF.

* select the SK PF data from KNVP table
  SELECT * FROM knvp INTO TABLE lt_knvp
    WHERE vkorg IN ('1000', '6000', 'SDMS') AND vtweg = '20' AND spart = '10'
    AND parvw IN ('SK','L5','XN','ZC','XZ').
  IF sy-subrc = 0.
    SORT lt_knvp BY kunnr vkorg parvw.
  ENDIF.



**************For 1000 company*********
  SELECT * FROM zc_cust_block_check( p_bukrs = '1000' , p_vkorg = '1000' ) INTO TABLE @lt_block.
*  REFRESH : lt_block.
*  lo_block->customer_block_check(
*    EXPORTING
*      bukrs                     = '1000'      " Company Code
*      vkorg                     = '1000'      " Sales Organization
*    CHANGING
*      cust_tab                  = lt_block   " Table Type for Customer Block Check
*      ).

  LOOP AT lt_block ASSIGNING FIELD-SYMBOL(<fs_block>).

*Attribute 2 and description
    READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_block>-kunnr BINARY SEARCH.
    IF sy-subrc = 0.

      CLEAR ls_customer.
      ls_customer-bukrs  = <fs_block>-bukrs.
      ls_customer-vkorg  = <fs_block>-vkorg.
      ls_customer-kunnr  = <fs_block>-kunnr.
      ls_customer-block  = <fs_block>-block.

      ls_customer-name1     = ls_kna1-name1.
      ls_customer-werks     = ls_kna1-werks.
      ls_customer-stras     = ls_kna1-stras.
      ls_customer-ort01     = ls_kna1-ort01.
      ls_customer-Regio     = ls_kna1-regio.
      ls_customer-pstlz     = ls_kna1-pstlz.
      ls_customer-ktokd     = ls_kna1-ktokd.
      ls_customer-lifnr     = ls_kna1-lifnr.
      ls_customer-katr2     = ls_kna1-katr2.
      ls_customer-stcd3     = ls_kna1-stcd3.
      ls_customer-j_1ipanno = ls_kna1-j_1ipanno.
      ls_customer-telf1     = ls_kna1-telf1.
      ls_customer-telf2     = ls_kna1-telf2.
      ls_customer-zkostl    = ls_kna1-zkostl.
      ls_customer-zcustype  = ls_kna1-zcustype.
      ls_customer-zfincode  = ls_kna1-zfincode.

      READ TABLE lt_tvk2t INTO DATA(ls_tvk2t) WITH KEY katr2 = ls_kna1-katr2 BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-vtext = ls_tvk2t-vtext.
      ENDIF.

      READ TABLE lt_adr6 INTO DATA(ls_adr6) WITH KEY addrnumber = ls_kna1-adrnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-smtp_addr = ls_adr6-smtp_addr.
      ENDIF.

*group feature, partner and description
      READ TABLE lt_BP001 INTO DATA(ls_bp001) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-partner = ls_bp001-partner.
        ls_customer-group_feature = ls_bp001-group_feature.

        IF ls_bp001-group_feature IS NOT INITIAL.
          READ TABLE lt_tp24t INTO DATA(ls_tp24t) WITH KEY group_feature = ls_bp001-group_feature BINARY SEARCH.
          IF sy-subrc = 0.
            ls_customer-group_feature_na = ls_tp24t-group_feature_na.
          ENDIF.
        ENDIF.
      ENDIF.

*customer group and group text
      READ TABLE lt_knvv INTO DATA(ls_knvv) WITH KEY kunnr = ls_kna1-kunnr vkorg = '1000' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-kdgrp = ls_knvv-kdgrp.
        ls_customer-vkbur = ls_knvv-vkbur.
        READ TABLE lt_t151t INTO DATA(ls_t151t) WITH KEY kdgrp = ls_knvv-kdgrp BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-ktext = ls_t151t-ktext.
        ENDIF.
      ENDIF.

*So Pernr and Name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '1000' Parvw = 'L5' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-so_pernr = ls_knvp-pernr.
        READ TABLE lt_pa0001 INTO DATA(ls_pa0001) WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-so_pernr_name = ls_pa0001-ename.
        ENDIF.
      ENDIF.

*CM Pernr and Name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '1000' Parvw = 'ZC' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-ch_pernr = ls_knvp-pernr.
        CLEAR ls_pa0001.
        READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-ch_pernr_name = ls_pa0001-ename.
        ENDIF.
      ENDIF.

*SO Position and name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '1000' Parvw = 'XN' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-so_position = ls_knvp-lifnr+2(8).
        READ TABLE lt_hrp1000 INTO DATA(ls_hrp1000) WITH KEY objid = ls_customer-so_position BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-so_position_name = ls_hrp1000-stext.
        ENDIF.
      ENDIF.

**CH Position and name
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '1000' Parvw = 'XZ' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-ch_position = ls_knvp-lifnr+2(8).
*        CLEAR ls_hrp1000.
*        READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_customer-ch_position BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-ch_position_name = ls_hrp1000-stext.
*        ENDIF.
*      ENDIF.

*check flag for e-invoice distributor
      READ TABLE lt_einvdist WITH KEY distributor = ls_kna1-kunnr BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_customer-zeinv_flag = 'X'.
      ENDIF.

**check flag is GST check is completed.
*      READ TABLE lt_gstchk INTO DATA(ls_gstchk) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-zeinv_chk = ls_gstchk.
*      ENDIF.

      CLEAR ls_custblkchk.
      READ TABLE lt_custblkchk INTO ls_custblkchk
          WITH KEY bukrs = '1000' vkorg = '1000' kunnr = ls_kna1-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_customer NE ls_custblkchk.
          APPEND ls_customer TO lt_customer.
        ENDIF.
      ELSE.
        APPEND ls_customer TO lt_customer.
      ENDIF.

    ENDIF.
  ENDLOOP.

*****************Update the table*************
  IF lt_customer[] IS NOT INITIAL.
    MODIFY zcust_blk_chk FROM TABLE lt_customer.
    IF sy-subrc = 0.
      COMMIT WORK.
      APPEND LINES OF lt_customer TO lt_alv.
    ENDIF.
  ENDIF.


  REFRESH : lt_customer.
  REFRESH lt_cust_1000.

  SELECT * FROM zcust_blk_chk INTO TABLE lt_cust_1000 WHERE bukrs = '1000'.
  SORT lt_cust_1000 BY kunnr.




**************For DMS1 company*********
  SELECT * FROM zc_cust_block_check( p_bukrs = 'DMS1' , p_vkorg = 'SDMS' ) INTO TABLE @lt_block.
*  REFRESH : lt_block.
*  lo_block->customer_block_check(
*    EXPORTING
*      bukrs                     = 'DMS1'      " Company Code
*      vkorg                     = 'SDMS'      " Sales Organization
*    CHANGING
*      cust_tab                  = lt_block   " Table Type for Customer Block Check
*      ).


  LOOP AT lt_block ASSIGNING <fs_block>.

    CLEAR ls_kna1.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_block>-kunnr BINARY SEARCH.
    IF sy-subrc = 0.

      CLEAR ls_customer.
      ls_customer-bukrs  = <fs_block>-bukrs.
      ls_customer-vkorg  = <fs_block>-vkorg.
      ls_customer-kunnr  = <fs_block>-kunnr.
      ls_customer-block  = <fs_block>-block.

      ls_customer-name1             = ls_kna1-name1.
      ls_customer-werks             = ls_kna1-werks.
      ls_customer-stras             = ls_kna1-stras.
      ls_customer-ort01             = ls_kna1-ort01.
      ls_customer-Regio             = ls_kna1-regio.
      ls_customer-pstlz             = ls_kna1-pstlz.
      ls_customer-ktokd             = ls_kna1-ktokd.
      ls_customer-lifnr             = ls_kna1-lifnr.
      ls_customer-katr2             = ls_kna1-katr2.
      ls_customer-stcd3             = ls_kna1-stcd3.
      ls_customer-j_1ipanno         = ls_kna1-j_1ipanno.
      ls_customer-telf1             = ls_kna1-telf1.
      ls_customer-telf2             = ls_kna1-telf2.


      CLEAR ls_adr6.
      READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_kna1-adrnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-smtp_addr = ls_adr6-smtp_addr.
      ENDIF.

      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = <fs_block>-kunnr vkorg = 'SDMS' parvw = 'SK' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-dist = ls_knvp-kunn2.
        READ TABLE lt_cust_1000 INTO ls_cust_1000 WITH KEY kunnr = ls_customer-dist BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-dist_name  = ls_cust_1000-name1.
          ls_customer-dist_werks = ls_cust_1000-werks.
          ls_customer-dist_block = ls_cust_1000-block.
          ls_customer-zeinv_flag = ls_cust_1000-zeinv_flag.
        ENDIF.

      ENDIF.

      CLEAR ls_tvk2t.
      READ TABLE lt_tvk2t INTO ls_tvk2t WITH KEY katr2 = ls_kna1-katr2 BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-vtext = ls_tvk2t-vtext.
      ENDIF.

*group feature, partner and description
      CLEAR ls_bp001.
      READ TABLE lt_BP001 INTO ls_bp001 WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-partner = ls_bp001-partner.
        ls_customer-group_feature = ls_bp001-group_feature.

        IF ls_bp001-group_feature IS NOT INITIAL.
          CLEAR ls_tp24t.
          READ TABLE lt_tp24t INTO ls_tp24t WITH KEY group_feature = ls_bp001-group_feature BINARY SEARCH.
          IF sy-subrc = 0.
            ls_customer-group_feature_na = ls_tp24t-group_feature_na.
          ENDIF.
        ENDIF.
      ENDIF.

*customer group and group text
      CLEAR ls_knvv.
      READ TABLE lt_knvv INTO ls_knvv WITH KEY kunnr = ls_kna1-kunnr vkorg = 'SDMS' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-kdgrp = ls_knvv-kdgrp.
        ls_customer-vkbur = ls_knvv-vkbur.

        CLEAR ls_t151t.
        READ TABLE lt_t151t INTO ls_t151t WITH KEY kdgrp = ls_knvv-kdgrp BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-ktext = ls_t151t-ktext.
        ENDIF.
      ENDIF.

*So Pernr and Name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = 'SDMS' Parvw = 'L5' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-so_pernr = ls_knvp-pernr.
        CLEAR ls_pa0001.
        READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-so_pernr_name = ls_pa0001-ename.
        ENDIF.
      ENDIF.

*CM Pernr and Name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = 'SDMS' Parvw = 'ZC' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-ch_pernr = ls_knvp-pernr.
        CLEAR ls_pa0001.
        READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-ch_pernr_name = ls_pa0001-ename.
        ENDIF.
      ENDIF.

*SO Position and name
      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = 'SDMS' Parvw = 'XN' BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-so_position = ls_knvp-lifnr+2(8).
        CLEAR ls_hrp1000.
        READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_customer-so_position BINARY SEARCH.
        IF sy-subrc = 0.
          ls_customer-so_position_name = ls_hrp1000-stext.
        ENDIF.
      ENDIF.

**CH Position and name
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = 'SDMS' Parvw = 'XZ' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-ch_position = ls_knvp-kunn2+2(8).
*        CLEAR ls_hrp1000.
*        READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_customer-ch_position BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-ch_position_name = ls_hrp1000-stext.
*        ENDIF.
*      ENDIF.


**check flag is GST check is completed.
      READ TABLE lt_gstchk INTO DATA(ls_gstchk) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_customer-zeinv_chk = ls_gstchk-msg_type.
        ls_customer-status    = ls_gstchk-STATUS.
      ENDIF.

      CLEAR ls_custblkchk.
      READ TABLE lt_custblkchk INTO ls_custblkchk
        WITH KEY bukrs = 'DMS1' vkorg = 'SDMS' kunnr = ls_kna1-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_customer NE ls_custblkchk.
          APPEND ls_customer TO lt_customer.
        ENDIF.
      ELSE.
        APPEND ls_customer TO lt_customer.
      ENDIF.

    ENDIF.
  ENDLOOP.
*****************Update the table*************
  IF lt_customer[] IS NOT INITIAL.
    MODIFY zcust_blk_chk FROM TABLE lt_customer.
    IF sy-subrc = 0.
      COMMIT WORK.
      APPEND LINES OF lt_customer TO lt_alv.
      REFRESH : lt_customer.
    ENDIF.
  ENDIF.


*
***************For 6000 company*********
*  SELECT * FROM zc_cust_block_check( p_bukrs = '6000' , p_vkorg = '6000' ) INTO TABLE @lt_block.
**  REFRESH : lt_block.
**  lo_block->customer_block_check(
**    EXPORTING
**      bukrs                     = '6000'      " Company Code
**      vkorg                     = '6000'      " Sales Organization
**    CHANGING
**      cust_tab                  = lt_block   " Table Type for Customer Block Check
**      ).
*
*  LOOP AT lt_block ASSIGNING <fs_block>.
*
*
*    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_block>-kunnr BINARY SEARCH.
*    IF sy-subrc = 0.
*
*      CLEAR ls_customer.
*      ls_customer-bukrs  = <fs_block>-bukrs.
*      ls_customer-vkorg  = <fs_block>-vkorg.
*      ls_customer-kunnr  = <fs_block>-kunnr.
*      ls_customer-block  = <fs_block>-block.
*
*      ls_customer-name1     = ls_kna1-name1.
*      ls_customer-werks     = ls_kna1-werks.
*      ls_customer-stras     = ls_kna1-stras.
*      ls_customer-ort01     = ls_kna1-ort01.
*      ls_customer-Regio     = ls_kna1-regio.
*      ls_customer-pstlz     = ls_kna1-pstlz.
*      ls_customer-ktokd     = ls_kna1-ktokd.
*      ls_customer-lifnr     = ls_kna1-lifnr.
*      ls_customer-katr2     = ls_kna1-katr2.
*      ls_customer-stcd3     = ls_kna1-stcd3.
*      ls_customer-j_1ipanno = ls_kna1-j_1ipanno.
*      ls_customer-telf1     = ls_kna1-telf1.
*      ls_customer-telf2     = ls_kna1-telf2.
*
*      CLEAR ls_adr6.
*      READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_kna1-adrnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-smtp_addr = ls_adr6-smtp_addr.
*      ENDIF.
*
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = <fs_block>-kunnr vkorg = '6000' parvw = 'SK' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-dist = ls_knvp-kunn2.
*        READ TABLE lt_cust_1000 INTO ls_cust_1000 WITH KEY kunnr = ls_customer-dist BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-dist_name  = ls_cust_1000-name1.
*          ls_customer-dist_werks = ls_cust_1000-werks.
*          ls_customer-dist_block = ls_cust_1000-block.
*          ls_customer-zeinv_flag = ls_cust_1000-zeinv_flag.
*        ENDIF.
*
*      ENDIF.
*
*      CLEAR ls_tvk2t.
*      READ TABLE lt_tvk2t INTO ls_tvk2t WITH KEY katr2 = ls_kna1-katr2.
*      IF sy-subrc = 0.
*        ls_customer-vtext = ls_tvk2t-vtext.
*      ENDIF.
*
**group feature, partner and description
*      CLEAR ls_bp001.
*      READ TABLE lt_BP001 INTO ls_bp001 WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-partner = ls_bp001-partner.
*        ls_customer-group_feature = ls_bp001-group_feature.
*
*        IF ls_bp001-group_feature IS NOT INITIAL.
*          CLEAR ls_tp24t.
*          READ TABLE lt_tp24t INTO ls_tp24t WITH KEY group_feature = ls_bp001-group_feature.
*          IF sy-subrc = 0.
*            ls_customer-group_feature_na = ls_tp24t-group_feature_na.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
**customer group and group text
*      CLEAR ls_knvv.
*      READ TABLE lt_knvv INTO ls_knvv WITH KEY kunnr = ls_kna1-kunnr vkorg = '6000' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-kdgrp = ls_knvv-kdgrp.
*        ls_customer-vkbur = ls_knvv-vkbur.
*
*        CLEAR ls_t151t.
*        READ TABLE lt_t151t INTO ls_t151t WITH KEY kdgrp = ls_knvv-kdgrp BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-ktext = ls_t151t-ktext.
*        ENDIF.
*      ENDIF.
*
**So Pernr and Name
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '6000' Parvw = 'L5' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-so_pernr = ls_knvp-pernr.
*        CLEAR ls_pa0001.
*        READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-so_pernr_name = ls_pa0001-ename.
*        ENDIF.
*      ENDIF.
*
**CM Pernr and Name
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '6000' Parvw = 'ZC' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-ch_pernr = ls_knvp-pernr.
*        CLEAR ls_pa0001.
*        READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_knvp-pernr BINARY SEARCH.
*        IF sy-subrc = 0.
*          ls_customer-ch_pernr_name = ls_pa0001-ename.
*        ENDIF.
*      ENDIF.
*
**SO Position and name
*      CLEAR ls_knvp.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '6000' Parvw = 'XN' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_customer-so_position = ls_knvp-kunn2+2(8).
*        CLEAR ls_hrp1000.
*        READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_customer-so_position.
*        IF sy-subrc = 0.
*          ls_customer-so_position_name = ls_hrp1000-stext.
*        ENDIF.
*      ENDIF.
**
***CH Position and name
**      CLEAR ls_knvp.
**      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_kna1-kunnr vkorg = '6000' Parvw = 'XZ' BINARY SEARCH.
**      IF sy-subrc = 0.
**        ls_customer-ch_position = ls_knvp-kunn2+2(8).
**        CLEAR ls_hrp1000.
**        READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_customer-ch_position BINARY SEARCH.
**        IF sy-subrc = 0.
**          ls_customer-ch_position_name = ls_hrp1000-stext.
**        ENDIF.
**      ENDIF.
*
***check flag for e-invoice distributor
**      READ TABLE lt_einvdist WITH KEY distributor = ls_customer-dist BINARY SEARCH TRANSPORTING NO FIELDS.
**      IF sy-subrc = 0.
**        ls_customer-zeinv_flag = 'X'.
**      ENDIF.
*
****check flag is GST check is completed.
**      CLEAR ls_gstchk.
**      READ TABLE lt_gstchk INTO ls_gstchk WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
**      IF sy-subrc = 0.
**        ls_customer-zeinv_chk = ls_gstchk.
**      ENDIF.
**
*      CLEAR ls_custblkchk.
*      READ TABLE lt_custblkchk INTO ls_custblkchk
*        WITH KEY bukrs = '6000' vkorg = '6000' kunnr = ls_kna1-kunnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        IF ls_customer NE ls_custblkchk.
*          APPEND ls_customer TO lt_customer.
*        ENDIF.
*      ELSE.
*        APPEND ls_customer TO lt_customer.
*      ENDIF.
*
*    ENDIF.
*  ENDLOOP.
*
******************Update the table*************
*  IF lt_customer[] IS NOT INITIAL.
*    MODIFY zcust_blk_chk FROM TABLE lt_customer.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*      APPEND LINES OF lt_customer TO lt_alv.
*      REFRESH : lt_customer.
*    ENDIF.
*  ENDIF.

  IF sy-batch = abap_false.
    IF lt_alv IS NOT INITIAL.
      CALL FUNCTION 'Z_POPUP_ALV'
        TABLES
          it_alv = lt_alv.
    ENDIF.
  ENDIF.

ENDFORM.
