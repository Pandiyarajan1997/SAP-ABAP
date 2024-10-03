FUNCTION zbapi_vendor_master_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VENDOR) TYPE  LIFNR OPTIONAL
*"     VALUE(REGION) TYPE  REGIO OPTIONAL
*"  TABLES
*"      IT_VENDOR STRUCTURE  ZSTR_VENDOR_MASTER
*"----------------------------------------------------------------------
*** Created By : Samsudeen M
  " Reference By : Praveen Kumar and Ramakrishnan
  " Description  : All Vendor Master Data to MIS
  " TR No: DEVK931782
*-----------------------------------------------------------------------
  TYPES: BEGIN OF ty_lfa1,    " Structure For Vendor Master Data
           lifnr     TYPE lifnr,
           name1     TYPE name1_gp,
           name2     TYPE name2_gp,
           name3     TYPE name3_gp,
           name4     TYPE name4_gp,
           ort01     TYPE ort01_gp,
           ort02     TYPE ort02_gp,
           regio     TYPE regio,
           land1     TYPE land1_gp,
           adrnr     TYPE adrnr,
           ktokk     TYPE ktokk,
           stcd3     TYPE stcd3,
           loevm     TYPE loevm,
           sperr     TYPE sperr,
           telf1     TYPE telf1,
           telf2     TYPE telf2,
           telfx     TYPE telfx,
           teltx     TYPE teltx,
           pstlz     TYPE pstlz,
           j_1ipanno TYPE j_1ipanno,
         END OF ty_lfa1,

         BEGIN OF ty_lfb1,    " Structure For vendor Basic Data
           lifnr TYPE lifnr,
           bukrs TYPE bukrs,
           akont TYPE akont,
           zterm TYPE dzterm,
           hbkid TYPE hbkid,
         END OF ty_lfb1,

         BEGIN OF ty_ibpsupplier,
           supplier        TYPE lifnr,
           businesspartner TYPE bu_partner,
         END OF ty_ibpsupplier,

         BEGIN OF ty_bp001,  "Structure for BP vendor Classification
           partner       TYPE bu_partner,
           group_feature TYPE bp_group_feature,
         END OF ty_bp001.

*********Internal Table For table LFA1******
  DATA: lt_lfa1 TYPE STANDARD TABLE OF ty_lfa1,
        ls_lfa1 TYPE ty_lfa1.

*********Internal Table For table LFB1******
  DATA: lt_lfb1 TYPE STANDARD TABLE OF ty_lfb1,
        ls_lfb1 TYPE ty_lfb1.

  DATA: lt_ibps TYPE STANDARD TABLE OF ty_ibpsupplier,
        ls_ibps TYPE ty_ibpsupplier.

  DATA: lt_bp001 TYPE STANDARD TABLE OF ty_bp001,
        ls_bp001 TYPE ty_bp001.

  DATA: lt_tp24t TYPE STANDARD TABLE OF tp24t,
        ls_tp24t TYPE tp24t.

****WorkArea For Output Table*****
  DATA: ls_vendor TYPE zstr_vendor_master.

  REFRESH: lt_lfa1,lt_lfb1, it_vendor, lt_bp001 , lt_tp24t.

  IF region IS NOT INITIAL AND vendor IS NOT INITIAL.
******Fetching Record from LFA1 Based On Input******
    SELECT lifnr
           name1
           name2
           name3
           name4
           ort01
           ort02
           regio
           land1
           adrnr
           ktokk
           stcd3
           loevm
           sperr
           telf1
           telf2
           telfx
           teltx
           pstlz
           j_1ipanno FROM lfa1
                 INTO TABLE lt_lfa1
                 WHERE lifnr EQ vendor
                 AND regio EQ region.

  ELSEIF region IS INITIAL AND vendor IS INITIAL.

*********Fetching Record from lfa1 no input parameter********
    SELECT lifnr
           name1
           name2
           name3
           name4
           ort01
           ort02
           regio
           land1
           adrnr
           ktokk
           stcd3
           loevm
           sperr
           telf1
           telf2
           telfx
           teltx
           pstlz
           j_1ipanno FROM lfa1
                 INTO TABLE lt_lfa1.

    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.

  ELSEIF region IS INITIAL AND vendor IS NOT INITIAL.

*********Fetching Record from lfa1 no input parameter********
    SELECT lifnr
            name1
            name2
            name3
            name4
            ort01
            ort02
            regio
            land1
            adrnr
            ktokk
            stcd3
            loevm
            sperr
            telf1
            telf2
            telfx
            teltx
            pstlz
            j_1ipanno FROM lfa1
                  INTO TABLE lt_lfa1
                  WHERE lifnr EQ vendor.

    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
  ENDIF.

  IF lt_lfa1[] IS NOT INITIAL.
*******According to Master Data of Vendor Fetching Some Data From LFB1**********
    SELECT lifnr
           bukrs
           akont
           zterm
           hbkid FROM lfb1
                 INTO TABLE lt_lfb1
                 FOR ALL ENTRIES IN lt_lfa1
                 WHERE lifnr = lt_lfa1-lifnr.

    IF sy-subrc EQ 0.
      SORT lt_lfb1[] ASCENDING.
    ENDIF.

**Fetching BP code From IBPSUPPLIER**
    SELECT supplier
           businesspartner FROM ibpsupplier
                           INTO TABLE lt_ibps
                           FOR ALL ENTRIES IN lt_lfa1
                           WHERE supplier = lt_lfa1-lifnr.
    SORT lt_ibps[] BY supplier.
**Fetching Vendor Type ***
    SELECT partner
           group_feature FROM bp001
                         INTO TABLE lt_bp001.
    IF sy-subrc EQ 0.
      SORT lt_bp001[] BY partner ASCENDING.

*** Vendor type code Des ***
      SELECT * FROM tp24t
               INTO TABLE lt_tp24t
               FOR ALL ENTRIES IN lt_bp001
               WHERE spras EQ sy-langu
               AND group_feature EQ lt_bp001-group_feature.
      IF sy-subrc EQ 0.
        SORT lt_tp24t[] BY group_feature ASCENDING.
      ENDIF.
    ENDIF.
** Email Selection from ADR6 Table**
    SELECT * FROM adr6 INTO TABLE @DATA(lt_adr6)
             FOR ALL ENTRIES IN @lt_lfa1
             WHERE addrnumber EQ @lt_lfa1-adrnr.
    IF sy-subrc EQ 0.
      SORT lt_adr6[] BY addrnumber.
    ENDIF.
    " Vendor Bank Details Start by Puratchi
    SELECT lifnr,
           bankn,
           bankl FROM lfbk INTO TABLE @DATA(lt_lfbk)
                 FOR ALL ENTRIES IN  @lt_lfa1
                 WHERE lifnr EQ @lt_lfa1-lifnr.

    SELECT a~partner ,
           a~name_first,
           b~telnr_long
           INTO TABLE @DATA(lt_suppl_det)
           FROM but000 AS a
           INNER JOIN adr2 AS b ON b~persnumber = a~persnumber
           FOR ALL ENTRIES IN @lt_ibps
           WHERE a~partner = @lt_ibps-businesspartner
             AND a~persnumber NE @space.
    " End by Puratchiveeran
  ENDIF.

  CLEAR ls_lfb1.
  LOOP AT lt_lfb1 INTO ls_lfb1.
    CLEAR: ls_vendor.
    ls_vendor-bukrs = ls_lfb1-bukrs.
    ls_vendor-lifnr = ls_lfb1-lifnr.
    ls_vendor-akont = ls_lfb1-akont.
    ls_vendor-zterm = ls_lfb1-zterm.
    ls_vendor-hbkid = ls_lfb1-hbkid.

    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_lfb1-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_vendor-name1 = ls_lfa1-name1.
      ls_vendor-name2 = ls_lfa1-name2.
      ls_vendor-name3 = ls_lfa1-name3.
      ls_vendor-name4 = ls_lfa1-name4.
      ls_vendor-ort01 = ls_lfa1-ort01.
      ls_vendor-ort02 = ls_lfa1-ort02.
      ls_vendor-regio = ls_lfa1-regio.
      ls_vendor-land1 = ls_lfa1-land1.
      ls_vendor-ktokk = ls_lfa1-ktokk.
      ls_vendor-stcd3 = ls_lfa1-stcd3.
      ls_vendor-loevm = ls_lfa1-loevm.
      ls_vendor-sperr = ls_lfa1-sperr.
      ls_vendor-mobile1 = ls_lfa1-telf1.
      ls_vendor-mobile2 = ls_lfa1-telf2.
      ls_vendor-mobile3 = ls_lfa1-telfx.
      ls_vendor-mobile4 = ls_lfa1-teltx.
      ls_vendor-pincode = ls_lfa1-pstlz.
      ls_vendor-panno = ls_lfa1-j_1ipanno.
      " Vendor Bank Details Start by Puratchi
      ls_vendor-accno = VALUE #( lt_lfbk[ lifnr = ls_lfa1-lifnr ]-bankn OPTIONAL ).
      ls_vendor-ifsc = VALUE #( lt_lfbk[ lifnr = ls_lfa1-lifnr ]-bankl OPTIONAL ).
      DATA(l_partner) = VALUE #( lt_ibps[ supplier = ls_lfa1-lifnr ]-businesspartner OPTIONAL ).
      " Vendor Bank Details End by Puratchi
      CLEAR ls_ibps.
      READ TABLE lt_ibps INTO ls_ibps WITH KEY supplier = ls_lfa1-lifnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_bp001.
        READ TABLE lt_bp001 INTO ls_bp001 WITH KEY partner = ls_ibps-businesspartner BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_vendor-vendor_type = ls_bp001-group_feature. "Vendor Type Intimate Code
***Getting Vendor type Code Des from TP24T***
          IF ls_bp001-group_feature IS INITIAL.
            IF ls_vendor-ktokk = 'YBEV'.
              ls_vendor-vendor_type = '1022'.
              ls_vendor-ven_typ_des = 'Employee'.
            ENDIF.
          ELSE.
            CLEAR ls_tp24t.
            READ TABLE lt_tp24t INTO ls_tp24t WITH KEY group_feature = ls_bp001-group_feature BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_vendor-ven_typ_des = ls_tp24t-group_feature_na. "Vendor Type Code Des
            ENDIF.
          ENDIF.
        ELSE.
          IF ls_vendor-ktokk = 'YBEV'.
            ls_vendor-vendor_type = '1022'.
            ls_vendor-ven_typ_des = 'Employee'.
          ENDIF.
        ENDIF.

      ENDIF.
** Mail Fetching **
      READ TABLE lt_adr6 INTO DATA(ls_adr6) WITH KEY addrnumber = ls_lfa1-adrnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_vendor-email = ls_adr6-smtp_addr.
      ENDIF.

    ENDIF.

*if employee vendor and it is blocked then do not send to DMS
    IF ls_vendor-ktokk = 'YBEV' AND ls_vendor-loevm = 'X'.
      CONTINUE.
    ENDIF.

    APPEND ls_vendor TO it_vendor.

  ENDLOOP.
ENDFUNCTION.
