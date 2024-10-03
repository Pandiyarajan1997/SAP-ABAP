FUNCTION zget_customer_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LV_KUNNR) TYPE  /ACCGO/CAK_TT_CUSTOMER_RANGE OPTIONAL
*"     VALUE(LV_VKORG) TYPE  VKORG
*"     VALUE(LV_VTWEG) TYPE  VTWEG OPTIONAL
*"     VALUE(LV_LOEVM) TYPE  LOEVM OPTIONAL
*"  TABLES
*"      IT_CUSTOMER STRUCTURE  ZGET_CUST_DETAILS
*"----------------------------------------------------------------------


  TYPES : BEGIN OF ty_adrc ,
            name1      TYPE adrc-name1,
            street     TYPE adrc-street,
            house_num1 TYPE	adrc-house_num1,
            str_suppl1 TYPE	adrc-str_suppl1,
            str_suppl2 TYPE	adrc-str_suppl2,
            str_suppl3 TYPE adrc-str_suppl3,
            location   TYPE adrc-location,
            time_zone	 TYPE adrc-time_zone,
            post_code1 TYPE adrc-post_code1,
            tel_number TYPE adrc-tel_number,
            fax_number TYPE adrc-fax_number,
            addrnumber TYPE adrc-addrnumber,
          END OF ty_adrc.

  TYPES : BEGIN OF ty_adr6 ,
            addrnumber TYPE adr6-addrnumber,
            smtp_addr  TYPE adr6-smtp_addr,
          END OF ty_adr6.


  TYPES : BEGIN OF ty_knvv ,
            kunnr TYPE kunnr,
            vkorg TYPE knvv-vkorg,
            vtweg TYPE knvv-vtweg,

            kdgrp TYPE knvv-kdgrp,
            vkbur TYPE knvv-vkbur,
            vwerk TYPE knvv-vwerk,
          END OF ty_knvv.



  DATA it_kna1 LIKE kna1 OCCURS 0 WITH HEADER LINE .


  DATA:it_knvv TYPE STANDARD TABLE OF ty_knvv,
       wa_knvv TYPE ty_knvv.

  DATA it_a936 TYPE a936 OCCURS 0 WITH HEADER LINE .
  DATA it_a304 TYPE a304 OCCURS 0 WITH HEADER LINE .

  DATA:it_t005t TYPE STANDARD TABLE OF t005t,
       wa_t005t TYPE t005t.

  DATA:it_t005u TYPE STANDARD  TABLE OF t005u,
       wa_t005u TYPE t005u.

  DATA:it_t005h TYPE STANDARD  TABLE OF t005h,
       wa_t005h TYPE t005h.

  DATA:it_t077x TYPE STANDARD  TABLE OF t077x,
       wa_t077x TYPE t077x.


  DATA:it_adrc TYPE STANDARD  TABLE OF ty_adrc,
       wa_adrc TYPE ty_adrc.

  DATA:it_adr6 TYPE STANDARD  TABLE OF ty_adr6,
       wa_adr6 TYPE ty_adr6.

  DATA:it_t151t TYPE STANDARD  TABLE OF t151t,
       wa_t151t TYPE t151t.

  DATA:it_tvkbt TYPE STANDARD  TABLE OF tvkbt,
       wa_tvkbt TYPE tvkbt.

  DATA:it_t001w TYPE STANDARD  TABLE OF t001w,
       wa_t001w TYPE t001w.
  TYPES: BEGIN OF ty_tvk1,
           katr1 TYPE tvk1-katr1,
           txt1  TYPE tvk1t-vtext,
         END OF ty_tvk1.

  DATA:it_tvk1 TYPE TABLE OF ty_tvk1,
       wa_tvk1 TYPE ty_tvk1.

  TYPES: BEGIN OF ty_tvk2,
           katr2 TYPE tvk2-katr2,
           txt2  TYPE tvk2t-vtext,
         END OF ty_tvk2.

  DATA:it_tvk2 TYPE TABLE OF ty_tvk2,
       wa_tvk2 TYPE ty_tvk2.

  TYPES: BEGIN OF ty_tvk3,
           katr3 TYPE tvk3-katr3,
           txt3  TYPE tvk3t-vtext,
         END OF ty_tvk3.

  DATA:it_tvk3 TYPE TABLE OF ty_tvk3,
       wa_tvk3 TYPE ty_tvk3.

  TYPES: BEGIN OF ty_tvk4,
           katr4 TYPE tvk4-katr4,
           txt4  TYPE tvk4t-vtext,
         END OF ty_tvk4.

  DATA:it_tvk4 TYPE TABLE OF ty_tvk4,
       wa_tvk4 TYPE ty_tvk4.

  TYPES: BEGIN OF ty_tvk5,
           katr5 TYPE tvk5-katr5,
           txt5  TYPE tvk5t-vtext,
         END OF ty_tvk5.

  DATA:it_tvk5 TYPE TABLE OF ty_tvk5,
       wa_tvk5 TYPE ty_tvk5.

  TYPES: BEGIN OF ty_tvk6,
           katr6 TYPE tvk6-katr6,
           txt6  TYPE tvk6t-vtext,
         END OF ty_tvk6.

  DATA:it_tvk6 TYPE TABLE OF ty_tvk6,
       wa_tvk6 TYPE ty_tvk6.

  TYPES: BEGIN OF ty_tvk7,
           katr7 TYPE tvk7-katr7,
           txt7  TYPE tvk7t-vtext,
         END OF ty_tvk7.

  DATA:it_tvk7 TYPE TABLE OF ty_tvk7,
       wa_tvk7 TYPE ty_tvk7.

  TYPES: BEGIN OF ty_tvk8,
           katr8 TYPE tvk8-katr8,
           txt8  TYPE tvk8t-vtext,
         END OF ty_tvk8.

  DATA:it_tvk8 TYPE TABLE OF ty_tvk8,
       wa_tvk8 TYPE ty_tvk8.
  TYPES : BEGIN OF gs_tvkggt,
            mandt TYPE tvkggt-mandt,
            spras TYPE tvkggt-spras,
            kdkgr TYPE tvkggt-kdkgr,
            vtext TYPE tvkggt-vtext,
          END OF gs_tvkggt.

  DATA : it_tvkggt TYPE TABLE OF gs_tvkggt,
         wa_tvkggt TYPE tvkggt.
  TYPES:BEGIN OF ty_tvkggt,
          kdkgr    TYPE tvkggt-kdkgr,
          gr_vtext TYPE tvkggt-vtext,
        END OF ty_tvkggt.
  DATA:it_tvkggt1 TYPE TABLE OF ty_tvkggt,
       wa_tvkggt1 TYPE ty_tvkggt.

  DATA it_a005 TYPE a005 OCCURS 0 WITH HEADER LINE .
  DATA it_a305 TYPE a305 OCCURS 0 WITH HEADER LINE .
  DATA : lt_kunnr TYPE /accgo/cak_tt_customer_range.
  DATA : lo_check TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_check.
  DATA : lt_cusblk TYPE zsd_tt_cust_block.

* get customer data
  SELECT
    kunnr
    vkorg
    vtweg
    kdgrp
    vkbur
    vwerk
    FROM  knvv
    INTO TABLE it_knvv
    WHERE kunnr IN lv_kunnr AND
          vkorg EQ lv_vkorg AND
          vtweg EQ lv_vtweg.
*          loevm EQ lv_loevm.


  SORT it_knvv ASCENDING BY kunnr vkorg vtweg.



  DELETE ADJACENT DUPLICATES FROM it_knvv COMPARING kunnr vkorg vtweg.


  IF it_knvv[] IS NOT INITIAL.

    SELECT kunnr
           name1
           kunnr
           name1
           land1
           regio
           cityc
           adrnr
           telf2
           telf1
           pstlz
           ktokd
           ort01
           stcd3
           katr1
            katr2
            katr3
            katr4
            katr5
            katr6
            katr7
            katr8
            kdkg1
            kdkg2
            kdkg3
            kdkg4
            kdkg5
            name3
            werks
            zkostl
            zcapacity
            zlatitude
            zlongitude
            zcustype
            zdays
            zfincode
            erdat
           FROM kna1
           INTO   CORRESPONDING FIELDS OF TABLE it_kna1
           FOR ALL ENTRIES IN it_knvv
           WHERE kunnr EQ it_knvv-kunnr .
*get the customer grp feature
    LOOP AT it_knvv INTO DATA(ls_knvv).
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
*******customer block check********
    SELECT SINGLE bukrs FROM tvko INTO @DATA(ls_bukrs) WHERE vkorg = @lv_vkorg.
    lo_check->customer_block_check(
      EXPORTING
        bukrs                     = ls_bukrs            " Company Code
        vkorg                     = lv_vkorg         " Sales Organization
      CHANGING
        cust_tab                  = lt_cusblk        " Table Type for Customer Block Check
    ).
  ENDIF.

  IF it_kna1[] IS NOT INITIAL .

    SELECT
       spras
       land1
       landx
       FROM t005t INTO CORRESPONDING FIELDS OF  TABLE it_t005t
      WHERE spras = sy-langu .


    SELECT
      spras
      land1
      bland
      bezei
      FROM t005u INTO CORRESPONDING FIELDS OF  TABLE it_t005u
      WHERE spras = sy-langu .

    SELECT
       spras
       land1
       regio
       cityc
       bezei
       FROM t005h INTO CORRESPONDING FIELDS OF  TABLE it_t005h
       WHERE spras = sy-langu.


    SELECT
       spras
       ktokd
       txt30
       FROM t077x INTO CORRESPONDING FIELDS OF  TABLE it_t077x
       WHERE spras = sy-langu ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation


    SELECT
       spras
       kdgrp
       ktext
       FROM t151t INTO CORRESPONDING FIELDS OF  TABLE it_t151t
       WHERE spras = sy-langu.

    SELECT
      addrnumber
      smtp_addr
      FROM adr6 INTO CORRESPONDING FIELDS OF  TABLE it_adr6
      FOR ALL ENTRIES IN it_kna1
      WHERE addrnumber EQ it_kna1-adrnr.


    SELECT
      name1
      street
      house_num1
      str_suppl1
      str_suppl2
      str_suppl3
      location
      time_zone
      post_code1
      tel_number
      fax_number
      addrnumber
      FROM adrc INTO CORRESPONDING FIELDS OF  TABLE it_adrc
      FOR ALL ENTRIES IN it_kna1
      WHERE addrnumber EQ it_kna1-adrnr.

    SELECT
    spras
    kdkgr
    vtext
    FROM tvkggt INTO TABLE it_tvkggt .


    SELECT
       vkbur
       bezei
       FROM tvkbt INTO CORRESPONDING FIELDS OF  TABLE it_tvkbt
       FOR ALL ENTRIES IN it_knvv
       WHERE spras = sy-langu  AND vkbur EQ it_knvv-vkbur .


    SELECT
       werks
       name1
       FROM t001w INTO CORRESPONDING FIELDS OF  TABLE it_t001w
       FOR ALL ENTRIES IN it_knvv
       WHERE werks EQ it_knvv-vwerk .

    SELECT katr1 vtext FROM tvk1t INTO TABLE it_tvk1 FOR ALL ENTRIES IN it_kna1
     WHERE katr1 EQ  it_kna1-katr1.

    SELECT katr2 vtext FROM tvk2t INTO TABLE it_tvk2 FOR ALL ENTRIES IN it_kna1
                WHERE katr2 EQ  it_kna1-katr2.

    SELECT katr3 vtext FROM tvk3t INTO TABLE it_tvk3 FOR ALL ENTRIES IN it_kna1
              WHERE katr3 EQ  it_kna1-katr3.
    SELECT katr4 vtext FROM tvk4t INTO TABLE it_tvk4 FOR ALL ENTRIES IN it_kna1
            WHERE katr4 EQ  it_kna1-katr4.
    SELECT katr5 vtext FROM tvk5t INTO TABLE it_tvk5 FOR ALL ENTRIES IN it_kna1
          WHERE katr5 EQ  it_kna1-katr5.
    SELECT katr6 vtext FROM tvk6t INTO TABLE it_tvk6 FOR ALL ENTRIES IN it_kna1
        WHERE katr6 EQ  it_kna1-katr6.
    SELECT katr7 vtext FROM tvk7t INTO TABLE it_tvk7 FOR ALL ENTRIES IN it_kna1
      WHERE katr7 EQ  it_kna1-katr7.
    SELECT katr8 vtext FROM tvk8t INTO TABLE it_tvk8 FOR ALL ENTRIES IN it_kna1
    WHERE katr8 EQ  it_kna1-katr8.

    SELECT kdkgr vtext FROM tvkggt INTO TABLE it_tvkggt1.

*----- Added by samsudeen 0n 16.02.2023 ----------------------------*
    SELECT * FROM zsd_cus_ton INTO TABLE @DATA(it_capacity_txt)
                              FOR ALL ENTRIES IN @it_kna1
                              WHERE zcapacity = @it_kna1-zcapacity.
    IF sy-subrc = 0.
      SORT it_capacity_txt[] BY zcapacity.
    ENDIF.
*---------------------------------------------------------------------
  ENDIF.

  SORT it_t005t ASCENDING BY land1 .
  SORT it_t005u ASCENDING BY land1 bland.
  SORT it_t005h ASCENDING BY land1 regio cityc.
  SORT it_adrc  ASCENDING BY addrnumber.
  SORT it_adr6  ASCENDING BY addrnumber.
  SORT it_t077x ASCENDING BY ktokd.
  SORT it_t151t ASCENDING BY kdgrp.
  SORT it_kna1 ASCENDING BY kunnr.

  SORT it_tvkbt ASCENDING BY vkbur.
  SORT it_t001w ASCENDING BY werks.
  SORT it_tvkggt1 ASCENDING BY kdkgr.
  SORT : lt_cusblk BY kunnr,
         lt_grp    BY customer.
  DATA : lv_adrnr  TYPE tvbur-adrnr,
         lv_kna1   TYPE kna1,
         lv_kunnr2 TYPE kna1-kunnr.

  LOOP AT it_knvv INTO wa_knvv.
    it_customer-kunnr =   wa_knvv-kunnr.
    it_customer-vkorg =   wa_knvv-vkorg.
    it_customer-vtweg =   wa_knvv-vtweg.
    it_customer-kdgrp =   wa_knvv-kdgrp.

    it_customer-vkbur =   wa_knvv-vkbur.
    it_customer-vwerk =   wa_knvv-vwerk.


    READ TABLE it_kna1 WITH  KEY kunnr = wa_knvv-kunnr BINARY SEARCH .
    IF sy-subrc = 0.
      it_customer-kdkg1  =           it_kna1-kdkg1 .
      it_customer-kdkg2  =           it_kna1-kdkg2 .
      it_customer-kdkg3  =           it_kna1-kdkg3 .
      it_customer-kdkg4  =           it_kna1-kdkg4 .
      it_customer-kdkg5  =           it_kna1-kdkg5 .
      it_customer-name3  =   it_kna1-name3.
*Added by samsudeen on 16.02.2023 -----------------------------------------------
      it_customer-plant = it_kna1-werks.
      it_customer-costcenter = it_kna1-zkostl.
      it_customer-capacity = it_kna1-zcapacity.
      it_customer-latitude = it_kna1-zlatitude.
      it_customer-longitude = it_kna1-zlongitude.
*--------------------------------------------------------------------------------
*Added by pandiarajan on 08.09.2023 -----------------------------------------------
      it_customer-zcustype = it_kna1-zcustype.
      it_customer-zdays    = it_kna1-zdays.
      it_customer-zfincode = it_kna1-zfincode.
      it_customer-erdat    = it_kna1-erdat.
*--------------------------------------------------------------------------------

      READ TABLE it_tvkggt1 INTO wa_tvkggt1 WITH KEY kdkgr = it_customer-kdkg1.
      it_customer-vtext1 = wa_tvkggt1-gr_vtext.
      CLEAR:wa_tvkggt1.
      READ TABLE it_tvkggt1 INTO wa_tvkggt1 WITH KEY kdkgr = it_customer-kdkg2.
      it_customer-vtext2 = wa_tvkggt1-gr_vtext.
      CLEAR:wa_tvkggt1.
      READ TABLE it_tvkggt1 INTO wa_tvkggt1 WITH KEY kdkgr = it_customer-kdkg3.
      it_customer-vtext3 = wa_tvkggt1-gr_vtext.
      CLEAR:wa_tvkggt1.
      READ TABLE it_tvkggt1 INTO wa_tvkggt1 WITH KEY kdkgr = it_customer-kdkg4.
      it_customer-vtext4 = wa_tvkggt1-gr_vtext.
      CLEAR:wa_tvkggt1.
      READ TABLE it_tvkggt1 INTO wa_tvkggt1 WITH KEY kdkgr = it_customer-kdkg5.
      it_customer-vtext5 = wa_tvkggt1-gr_vtext.
      CLEAR:wa_tvkggt1.
*------- Added by samsudeen M -------------------------------------------------------------------------------------------*
      it_customer-capacity_txt = VALUE #( it_capacity_txt[ zcapacity = it_customer-capacity ]-description OPTIONAL ).
*-------------------------------------------------------------------------------------------------------------------------
*        it_customer-name1 =            it_kna1-name1. taken form adrc table below
      it_customer-kunnr =            it_kna1-kunnr.
      it_customer-land1 =            it_kna1-land1.
      it_customer-regio =            it_kna1-regio.
      it_customer-cityc =            it_kna1-cityc.
      it_customer-adrnr =            it_kna1-adrnr.
      it_customer-telf2 =            it_kna1-telf2.
      it_customer-telf1 =            it_kna1-telf1.
      it_customer-pstlz =            it_kna1-pstlz.
      it_customer-ktokd =            it_kna1-ktokd.
      it_customer-citycx =           it_kna1-ort01.
      it_customer-stcd3 =            it_kna1-stcd3.
      it_customer-katr1 =            it_kna1-katr1.
      it_customer-katr2 =            it_kna1-katr2.
      it_customer-katr3 =            it_kna1-katr3.
      it_customer-katr4 =            it_kna1-katr4.
      it_customer-katr5 =            it_kna1-katr5.
      it_customer-katr6 =            it_kna1-katr6.
      it_customer-katr7 =            it_kna1-katr7.
      it_customer-katr8 =            it_kna1-katr8.


      READ TABLE it_t005t INTO wa_t005t WITH  KEY land1 = it_customer-land1 BINARY SEARCH.
      IF sy-subrc = 0.

        it_customer-landx  = wa_t005t-landx .

      ENDIF.

      READ TABLE it_t005u INTO wa_t005u WITH  KEY land1 = it_customer-land1
                                                  bland = it_customer-regio   BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-regiox  = wa_t005u-bezei .
      ENDIF.

      READ TABLE it_t005h INTO wa_t005h WITH  KEY land1 = it_customer-land1
                                                  regio = it_customer-regio
                                                  cityc = it_customer-cityc    BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-citycx  = wa_t005u-bezei .
      ENDIF.


      READ TABLE it_adr6 INTO wa_adr6 WITH  KEY addrnumber = it_customer-adrnr  BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-smtp_addr  = wa_adr6-smtp_addr .
      ENDIF.

      READ TABLE it_adrc INTO wa_adrc WITH  KEY addrnumber = it_customer-adrnr  BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-name1   = wa_adrc-name1.
        it_customer-street  = wa_adrc-street .
        it_customer-house_num1 = wa_adrc-house_num1.
        it_customer-str_suppl1 = wa_adrc-str_suppl1.
        it_customer-str_suppl2 = wa_adrc-str_suppl2.
        it_customer-str_suppl3 = wa_adrc-str_suppl3.
        it_customer-location   = wa_adrc-location.
        it_customer-time_zone  = wa_adrc-time_zone.
        it_customer-post_code1 = wa_adrc-post_code1.
*      IT_CUSTOMER-TEL_NUMBER = WA_ADRC-TEL_NUMBER.
        it_customer-fax_number = wa_adrc-fax_number.
        it_customer-adrnr = wa_adrc-addrnumber.
      ENDIF.

      READ TABLE it_t077x INTO wa_t077x WITH  KEY ktokd = it_customer-ktokd     BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-ktokdx  = wa_t077x-txt30 .
      ENDIF.

      READ TABLE it_t151t INTO wa_t151t WITH  KEY kdgrp = it_customer-kdgrp BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-kdgrpx  = wa_t151t-ktext .
      ENDIF.




      READ TABLE it_tvkbt INTO wa_tvkbt WITH  KEY vkbur = it_customer-vkbur BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-vkburx  = wa_tvkbt-bezei .
      ENDIF.

      READ TABLE it_t001w INTO wa_t001w WITH  KEY werks = it_customer-vwerk BINARY SEARCH.
      IF sy-subrc = 0.
        it_customer-vwerkx  = wa_t001w-name1 .
      ENDIF.

      READ TABLE it_tvk1 INTO wa_tvk1 WITH KEY katr1 = it_customer-katr1.
      IF wa_tvk1 IS NOT INITIAL.
        it_customer-txt1 = wa_tvk1-txt1.
        CLEAR: wa_tvk1.
      ENDIF.

      READ TABLE it_tvk2 INTO wa_tvk2 WITH KEY katr2 = it_customer-katr2.
      IF wa_tvk2 IS NOT INITIAL.
        it_customer-txt2 = wa_tvk2-txt2.
        CLEAR: wa_tvk2.
      ENDIF.

      READ TABLE it_tvk3 INTO wa_tvk3 WITH KEY katr3 = it_customer-katr3.
      IF wa_tvk3 IS NOT INITIAL.
        it_customer-txt3 = wa_tvk3-txt3.
        CLEAR: wa_tvk3.
      ENDIF.

      READ TABLE it_tvk4 INTO wa_tvk4 WITH KEY katr4 = it_customer-katr4.
      IF wa_tvk4 IS NOT INITIAL.
        it_customer-txt4 = wa_tvk4-txt4.
        CLEAR: wa_tvk4.
      ENDIF.

      READ TABLE it_tvk5 INTO wa_tvk5 WITH KEY katr5 = it_customer-katr5.
      IF wa_tvk5 IS NOT INITIAL.
        it_customer-txt5 = wa_tvk5-txt5.
        CLEAR: wa_tvk5.
      ENDIF.

      READ TABLE it_tvk6 INTO wa_tvk6 WITH KEY katr6 = it_customer-katr6.
      IF wa_tvk6 IS NOT INITIAL.
        it_customer-txt6 = wa_tvk6-txt6.
        CLEAR: wa_tvk6.
      ENDIF.

      READ TABLE it_tvk7 INTO wa_tvk7 WITH KEY katr7 = it_customer-katr7.
      IF wa_tvk7 IS NOT INITIAL.
        it_customer-txt7 = wa_tvk7-txt7.
        CLEAR: wa_tvk7.
      ENDIF.

      READ TABLE it_tvk8 INTO wa_tvk8 WITH KEY katr8 = it_customer-katr8.
      IF wa_tvk8 IS NOT INITIAL.
        it_customer-txt8 = wa_tvk8-txt8.
        CLEAR: wa_tvk8.
      ENDIF.



    ENDIF.


*******get Dealer mapping with distributor *********


    CLEAR lv_adrnr.

** get sales office address no ( i.e. dealer refrance no)
    SELECT SINGLE adrnr
      FROM tvbur
      INTO lv_adrnr
      WHERE vkbur EQ it_customer-vkbur.

    IF sy-subrc = 0 AND lv_adrnr NE space .
** get distributor code
      SELECT name2
        UP TO 1 ROWS FROM  adrc INTO lv_kunnr2
        WHERE addrnumber EQ lv_adrnr ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation


      IF lv_kunnr2 CA '0123456789'.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kunnr2
          IMPORTING
            output = lv_kunnr2.


        SELECT SINGLE * FROM kna1 INTO lv_kna1
          WHERE kunnr EQ  lv_kunnr2.

        IF sy-subrc = 0.
          it_customer-kunnr2 = lv_kunnr2.
        ENDIF.
      ENDIF.


    ENDIF.
*********fill the datas of customer feature & block status**********
    READ TABLE lt_grp INTO DATA(ls_grp) WITH KEY customer = wa_knvv-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      it_customer-group_feature    =  ls_grp-group_feature.
      it_customer-group_feature_na =  ls_grp-group_feature_na.
    ENDIF.
    READ TABLE lt_cusblk INTO DATA(ls_cusblk) WITH KEY kunnr = wa_knvv-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      it_customer-cus_status       =  ls_cusblk-block.
    ENDIF.

    APPEND it_customer.
    CLEAR it_customer.

  ENDLOOP.


ENDFUNCTION.
