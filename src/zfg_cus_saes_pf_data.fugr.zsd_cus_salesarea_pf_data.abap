FUNCTION zsd_cus_salesarea_pf_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJID) TYPE  CHAR15
*"     VALUE(VKORG) TYPE  VKORG
*"     VALUE(VTWEG) TYPE  VTWEG OPTIONAL
*"     VALUE(SPART) TYPE  SPART OPTIONAL
*"  TABLES
*"      IT_CUSTOMER STRUCTURE  ZSTR_CUST_MIS_NEW
*"      IT_CUSTOMER_PARTNER STRUCTURE  ZSTR_CUST_PARTNER_MIS
*"      IT_KUNNR TYPE  /ACCGO/CAS_TT_KUNNR_RANGE OPTIONAL
*"----------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 05.08.2023
*&Purpose: Customer Sales Area and Partner Function Data
*&Reference By: Praveen Kumar & Ramakrishnan J
*-----------------------------------------------------------------------
  TYPES: BEGIN OF ty_knvv,
           kunnr TYPE kunnr,
           vkorg TYPE vkorg,
           vtweg TYPE vtweg,
           spart TYPE spart,
           kdgrp TYPE kdgrp,
           vkbur TYPE vkbur,
           vwerk TYPE vwerk,
           zterm TYPE dzterm,
         END OF ty_knvv.
  DATA: lt_knvv TYPE STANDARD TABLE OF ty_knvv.
  DATA : lt_kunnr TYPE /accgo/cak_tt_customer_range.

  DATA : lr_kunnr  TYPE RANGE OF kunnr.
  DATA : lt_cusblk TYPE zsd_tt_cust_block.
  DATA : lo_check  TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_check.

  IF it_kunnr[] IS  NOT INITIAL.
    LOOP AT it_kunnr INTO DATA(lw_kunnr).
      APPEND VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = lw_kunnr-low ) TO lr_kunnr.
    ENDLOOP.
  ENDIF.

  IF objid EQ 'ZLVR_KNVP'.
    REFRESH: it_customer_partner.
*** Partner Function Data ****
    SELECT kunnr,
           vkorg,
           vtweg,
           spart,
           parvw,
           parza,
           kunn2,
           lifnr,
           pernr,
           parnr,
           knref,
           defpa FROM knvp
      INTO TABLE @DATA(lt_knvp)
      WHERE kunnr IN @lr_kunnr
      AND vkorg EQ @vkorg.
    IF sy-subrc EQ 0.
      SORT lt_knvp[] BY kunnr vkorg vtweg spart.
    ENDIF.

    IF vtweg IS NOT INITIAL.
      DELETE lt_knvp[] WHERE vtweg NE vtweg.
    ENDIF.

    IF spart IS NOT INITIAL.
      DELETE lt_knvp[] WHERE spart NE spart.
    ENDIF.

    LOOP AT lt_knvp ASSIGNING FIELD-SYMBOL(<fls_knvp>).
      APPEND <fls_knvp> TO it_customer_partner.
    ENDLOOP.

  ELSEIF objid EQ 'ZLVR_KNVV'.
    REFRESH: lt_knvv,it_customer.
*** Sales Area Data ***
    SELECT kunnr
           vkorg
           vtweg
           spart
           kdgrp
           vkbur
           vwerk
           zterm FROM knvv
           INTO TABLE lt_knvv
           WHERE kunnr IN lr_kunnr
           AND vkorg EQ vkorg.
    IF sy-subrc EQ 0.
      SORT lt_knvv[] BY kunnr vtweg spart.
**** Specific Distribution Channel ***
      IF vtweg IS NOT INITIAL.
        DELETE lt_knvp[] WHERE vtweg NE vtweg.
      ENDIF.
*** Divison Specific Filter ***
      IF spart IS NOT INITIAL.
        DELETE lt_knvp[] WHERE spart NE spart.
      ENDIF.
**** Customer Master Details ***
      SELECT kunnr,
             name1,
             name2,
             name3,
             name4,
             land1,
             regio,
             cityc,
             adrnr,
             telf2,
             telf1,
             pstlz,
             ktokd,
             ort01,
             stcd3,
             kdkg1,
             kdkg2,
             kdkg3,
             kdkg4,
             kdkg5,
             loevm,
             katr1,
             katr2,
             katr3,
             katr4,
             katr5,
             katr6,
             katr7,
             katr8,
             katr9,
             katr10,
             erdat,
             zkostl,
             zcapacity,
             zlatitude,
             zlongitude,
             zfincode,
             zcustype,
             zdays
             FROM kna1
             INTO TABLE @DATA(lt_kna1)
             FOR ALL ENTRIES IN @lt_knvv
             WHERE kunnr EQ @lt_knvv-kunnr.
*------------------------------------------------------------------------------------------
*      Changed by - pandiarajan
*      changed on date - 07.12.2023
*      Reference by: Ramakrishnan J
*get the customer group feature
      LOOP AT lt_knvv INTO DATA(ls_knvv).
        APPEND VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = ls_knvv-kunnr ) TO lt_kunnr.
      ENDLOOP.
      lo_check->group_feature_fill(
        EXPORTING
          so_kunnr      = lt_kunnr                     " Range for customer number (KUNNR)
        IMPORTING
          lt_grpfeature = DATA(lt_grp)                 " table type grp feature
      ).
*  ******customer block check********
      SELECT SINGLE bukrs FROM tvko INTO @DATA(ls_bukrs) WHERE vkorg = @vkorg.
      lo_check->customer_block_check(
        EXPORTING
          bukrs                     = ls_bukrs            " Company Code
          vkorg                     = vkorg               " Sales Organization
        CHANGING
          cust_tab                  = lt_cusblk        " Table Type for Customer Block Check
      ).
      SORT : lt_cusblk BY kunnr,
             lt_grp    BY customer.

      IF lt_kna1[] IS NOT INITIAL..
        SORT lt_kna1[] BY kunnr.

        SELECT
           land1,
           landx
           FROM t005t INTO TABLE @DATA(it_t005t)
          WHERE spras EQ @sy-langu .

        SELECT
          land1,
          bland,
          bezei
          FROM t005u INTO TABLE @DATA(it_t005u)
          WHERE spras EQ @sy-langu.

        SELECT
           land1,
           regio,
           cityc,
           bezei
           FROM t005h INTO TABLE @DATA(it_t005h)
           WHERE spras EQ @sy-langu.


        SELECT
           ktokd,
           txt30
           FROM t077x INTO TABLE @DATA(it_t077x)
           WHERE spras EQ @sy-langu.  " Added by <IT-CAR Tool> during Code Remediation


        SELECT
           kdgrp,
           ktext
           FROM t151t INTO TABLE @DATA(it_t151t)
           WHERE spras EQ @sy-langu.

        SELECT
          kdkgr,
          vtext
          FROM tvkggt INTO TABLE @DATA(it_tvkggt)
          WHERE spras EQ @sy-langu.

        SELECT
           vkbur,
           bezei
           FROM tvkbt INTO TABLE @DATA(it_tvkbt)
           WHERE spras EQ @sy-langu.

        SELECT
           werks,
           name1
           FROM t001w INTO TABLE @DATA(it_t001w)
           WHERE spras EQ @sy-langu.

        SELECT katr1,  vtext FROM tvk1t INTO TABLE @DATA(it_tvk1)  WHERE spras EQ @sy-langu.
        SELECT katr2,  vtext FROM tvk2t INTO TABLE @DATA(it_tvk2)  WHERE spras EQ @sy-langu.
        SELECT katr3,  vtext FROM tvk3t INTO TABLE @DATA(it_tvk3)  WHERE spras EQ @sy-langu.
        SELECT katr4,  vtext FROM tvk4t INTO TABLE @DATA(it_tvk4)  WHERE spras EQ @sy-langu.
        SELECT katr5,  vtext FROM tvk5t INTO TABLE @DATA(it_tvk5)  WHERE spras EQ @sy-langu.
        SELECT katr6,  vtext FROM tvk6t INTO TABLE @DATA(it_tvk6)  WHERE spras EQ @sy-langu.
        SELECT katr7,  vtext FROM tvk7t INTO TABLE @DATA(it_tvk7)  WHERE spras EQ @sy-langu.
        SELECT katr8,  vtext FROM tvk8t INTO TABLE @DATA(it_tvk8)  WHERE spras EQ @sy-langu.
        SELECT katr9,  vtext FROM tvk9t INTO TABLE @DATA(it_tvk9)  WHERE spras EQ @sy-langu.
        SELECT katr10, vtext FROM tvk0t INTO TABLE @DATA(it_tvk10) WHERE spras EQ @sy-langu.

        SELECT kdkgr, vtext FROM tvkggt INTO TABLE @DATA(it_tvkggt1) WHERE spras EQ @sy-langu.
      ENDIF.

      LOOP AT lt_knvv ASSIGNING FIELD-SYMBOL(<fls_knvv>).
        DATA(att1)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr1  OPTIONAL ).
        DATA(att2)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr2  OPTIONAL ).
        DATA(att3)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr3  OPTIONAL ).
        DATA(att4)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr4  OPTIONAL ).
        DATA(att5)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr5  OPTIONAL ).
        DATA(att6)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr6  OPTIONAL ).
        DATA(att7)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr7  OPTIONAL ).
        DATA(att8)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr8  OPTIONAL ).
        DATA(att9)      = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr9  OPTIONAL ).
        DATA(att10)     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-katr10 OPTIONAL ).
*------------------------------------------------------------------------------------------
        DATA(zkostl)    = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zkostl       OPTIONAL ).
        DATA(zcapacity) = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zcapacity    OPTIONAL ).
        DATA(zlatitude) = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zlatitude    OPTIONAL ).
        DATA(zlongitude) = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zlongitude  OPTIONAL ).
        DATA(zfincode)  = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zfincode     OPTIONAL ).
        DATA(zcustype)  = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zcustype     OPTIONAL ).
        DATA(zdays)     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-zdays        OPTIONAL ).
        DATA(erdat)     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-erdat        OPTIONAL ).
*------------------------------------------------------------------------------------------
        DATA(att1_txt)  = VALUE #( it_tvk1[ katr1 = att1    ]-vtext  OPTIONAL ).
        DATA(att2_txt)  = VALUE #( it_tvk2[ katr2 = att2    ]-vtext  OPTIONAL ).
        DATA(att3_txt)  = VALUE #( it_tvk3[ katr3 = att3    ]-vtext  OPTIONAL ).
        DATA(att4_txt)  = VALUE #( it_tvk4[ katr4 = att4    ]-vtext  OPTIONAL ).
        DATA(att5_txt)  = VALUE #( it_tvk5[ katr5 = att5    ]-vtext  OPTIONAL ).
        DATA(att6_txt)  = VALUE #( it_tvk6[ katr6 = att6    ]-vtext  OPTIONAL ).
        DATA(att7_txt)  = VALUE #( it_tvk7[ katr7 = att7    ]-vtext  OPTIONAL ).
        DATA(att8_txt)  = VALUE #( it_tvk8[ katr8 = att8    ]-vtext  OPTIONAL ).
        DATA(att9_txt)  = VALUE #( it_tvk9[ katr9 = att9   ]-vtext  OPTIONAL ).
        DATA(att10_txt) = VALUE #( it_tvk10[ katr10 = att10 ]-vtext OPTIONAL ).
        DATA(cc1)       = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-kdkg1 OPTIONAL ).
        DATA(cc2)       = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-kdkg2 OPTIONAL ).
        DATA(cc3)       = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-kdkg3 OPTIONAL ).
        DATA(cc4)       = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-kdkg4 OPTIONAL ).
        DATA(cc5)       = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-kdkg5 OPTIONAL ).
        DATA(cc1_txt)   = VALUE #( it_tvkggt1[ kdkgr = cc1 ]-vtext OPTIONAL ).
        DATA(cc2_txt)   = VALUE #( it_tvkggt1[ kdkgr = cc2 ]-vtext OPTIONAL ).
        DATA(cc3_txt)   = VALUE #( it_tvkggt1[ kdkgr = cc3 ]-vtext OPTIONAL ).
        DATA(cc4_txt)   = VALUE #( it_tvkggt1[ kdkgr = cc4 ]-vtext OPTIONAL ).
        DATA(cc5_txt)   = VALUE #( it_tvkggt1[ kdkgr = cc5 ]-vtext OPTIONAL ).
*********fill the datas of customer feature & block status**********
        READ TABLE lt_grp INTO DATA(ls_grp) WITH KEY customer = <fls_knvv>-kunnr BINARY SEARCH.
        READ TABLE lt_cusblk INTO DATA(ls_cusblk) WITH KEY kunnr = <fls_knvv>-kunnr BINARY SEARCH.

        APPEND VALUE #( kunnr     = <fls_knvv>-kunnr
                        name1     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-name1 OPTIONAL )
                        name2     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-name2 OPTIONAL )
                        name3     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-name3 OPTIONAL )
                        name4     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-name4 OPTIONAL )
                        land1     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-land1 OPTIONAL )
                        regio     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-regio OPTIONAL )
                        cityc     = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-regio OPTIONAL )
                        citycx    = VALUE #( lt_kna1[ kunnr = <fls_knvv>-kunnr ]-ort01 OPTIONAL )
                        kdgrp     = <fls_knvv>-kdgrp
                        kdgrpx    = VALUE #( it_t151t[ kdgrp = <fls_knvv>-kdgrp ]-ktext OPTIONAL )
                        vkorg     = <fls_knvv>-vkorg
                        vtweg     = <fls_knvv>-vtweg
                        spart     = <fls_knvv>-spart
                        vkbur     = <fls_knvv>-vkbur
                        vkbur_txt = VALUE #( it_tvkbt[ vkbur = <fls_knvv>-vkbur ]-bezei OPTIONAL )
                        vwerk     = <fls_knvv>-vwerk
                        vwerk_txt = VALUE #( it_t001w[ werks = <fls_knvv>-vwerk ]-name1 OPTIONAL )
                        katr1      = att1
                        katr2      = att2
                        katr3      = att3
                        katr4      = att4
                        katr5      = att5
                        katr6      = att6
                        katr7      = att7
                        katr8      = att8
                        katr9      = att9
                        katr10     = att10
                        txt1       = att1_txt
                        txt2       = att2_txt
                        txt3       = att3_txt
                        txt4       = att4_txt
                        txt5       = att5_txt
                        txt6       = att6_txt
                        txt7       = att7_txt
                        txt8       = att8_txt
                        txt9       = att9_txt
                        txt10      = att10_txt
                        kdkg1      = cc1
                        kdkg2      = cc2
                        kdkg3      = cc3
                        kdkg4      = cc4
                        kdkg5      = cc5
                        vtext1     = cc1_txt
                        vtext2     = cc2_txt
                        vtext3     = cc3_txt
                        vtext4     = cc4_txt
                        vtext5     = cc5_txt
                        zterm      = <fls_knvv>-zterm
                        zkostl     = zkostl
                        zcapacity  = zcapacity
                        zlatitude  = zlatitude
                        zlongitude = zlongitude
                        zfincode   = zfincode
                        zcustype   = zcustype
                        zdays      = zdays
                        erdat      = erdat
                        group_feature    =  ls_grp-group_feature
                        group_feature_na =  ls_grp-group_feature_na
                        cus_status       =  ls_cusblk-block ) TO it_customer.
        CLEAR: att1,att2,att3,att4,att5,att6,att7,att8,att9,att10,cc1,cc2,cc3,cc4,cc5.
        CLEAR: att1_txt,att2_txt,att3_txt,att4_txt,att5_txt,
               att6_txt,att7_txt,att8_txt,att9_txt,att10_txt,
               cc1_txt,cc2_txt,cc3_txt,cc4_txt,cc5_txt,zkostl,zcapacity,zlatitude,zlongitude,zfincode,zcustype,zdays,erdat,ls_grp,ls_cusblk.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFUNCTION.
