*&---------------------------------------------------------------------*
*& Report ZFI_VEND_ADVANCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_company_gl_bank_report.

TABLES: bsik,bsid,lfa1,kna1,v_t012.",sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-005 FOR FIELD p_bukrs.
    PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
  SELECTION-SCREEN: END OF LINE.
  SELECT-OPTIONS: so_hbank FOR v_t012-hbkid NO INTERVALS." MATCHCODE OBJECT h_t012 NO INTERVALS.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) TEXT-004 FOR FIELD so_budat.
    SELECT-OPTIONS: so_budat FOR bsik-budat OBLIGATORY.
  SELECTION-SCREEN: END OF LINE.
*  SELECTION-SCREEN: BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT 1(31) TEXT-007 FOR FIELD p_gjahr.
*    PARAMETERS: p_gjahr TYPE bsik-gjahr.
*  SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS : r1 TYPE c RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND r1.
    SELECTION-SCREEN COMMENT 2(25) TEXT-002 FOR FIELD r1.
*    SELECT-OPTIONS: s_vendor FOR bsik-lifnr NO INTERVALS.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  r2 TYPE c RADIOBUTTON GROUP a.
    SELECTION-SCREEN COMMENT 2(25) TEXT-003 FOR FIELD r2.
*    SELECT-OPTIONS: s_cust FOR bsid-kunnr NO INTERVALS.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a2.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE TEXT-009.
  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS : r_sumary TYPE c RADIOBUTTON GROUP b DEFAULT 'X' USER-COMMAND r2.
    SELECTION-SCREEN COMMENT 2(25) TEXT-010 FOR FIELD r_sumary.
*    SELECT-OPTIONS: so_summary FOR bsik-lifnr NO INTERVALS.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  r_detail TYPE c RADIOBUTTON GROUP b.
    SELECTION-SCREEN COMMENT 2(25) TEXT-011 FOR FIELD r_detail.
*    SELECT-OPTIONS: so_detail FOR bsid-kunnr NO INTERVALS.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a3.

TYPES: BEGIN OF ty_final_v,
         bukrs  TYPE bukrs,
         hbkid  TYPE hbkid,       " House Bank
*         hktid  TYPE hktid,       " House Bank Account ID
         text1  TYPE t012t-text1, " House Bank Account Names
         bankn  TYPE t012k-bankn, " Bank ac/no
         belnr  TYPE bsis-belnr,  " Document No
         budat  TYPE bsis-budat,  " Posting Date
         gjahr  TYPE bsik-gjahr,
*           blart  TYPE bsis-blart,
         hkont  TYPE lfa1-lifnr,  " Vendor / GL A/c
         gldesc TYPE z_vend_name,     " GL DESCRIPTION
*           gsber  TYPE tgsbt-gsber, " Business Area
*           gtext  TYPE tgsbt-gtext, " Business Area Description
         xblnr  TYPE bkpf-xblnr,
         zuonr  TYPE bseg-zuonr,  " ASSIGNMENT
         dmbtr  TYPE bsis-dmbtr,
         sgtxt  TYPE bsis-sgtxt,  " NARRATION
         bktxt  TYPE bkpf-bktxt,
       END OF ty_final_v.
TYPES: BEGIN OF ty_final_c,
         bukrs  TYPE bukrs,
         hbkid  TYPE hbkid,       " House Bank
*         hktid  TYPE hktid,       " House Bank Account ID
         text1  TYPE t012t-text1, " House Bank Account Names
         bankn  TYPE t012k-bankn, " Bank ac/no
         belnr  TYPE bsis-belnr,  " Document No
         budat  TYPE bsis-budat,  " Posting Date
         gjahr  TYPE bsik-gjahr,
*           blart  TYPE bsis-blart,
         hkont  TYPE kna1-kunnr,  " Customer / GL A/c
         gldesc TYPE z_cust_name,     " GL DESCRIPTION
*           gsber  TYPE tgsbt-gsber, " Business Area
*           gtext  TYPE tgsbt-gtext, " Business Area Description
         xblnr  TYPE bkpf-xblnr,
         zuonr  TYPE bseg-zuonr,  " ASSIGNMENT
         dmbtr  TYPE bsis-dmbtr,
         sgtxt  TYPE bsis-sgtxt,  " NARRATION
         bktxt  TYPE bkpf-bktxt,
       END OF ty_final_c.

DATA: lt_vend TYPE STANDARD TABLE OF ty_final_v,
      wa_data TYPE ty_final_v,
      lt_cust TYPE STANDARD TABLE OF ty_final_c.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_hbank-low.

  SELECT a~bukrs,
         a~hbkid,
         a~bankl,
         b~text1
          FROM t012 AS a INNER JOIN t012t AS b ON a~bukrs = b~bukrs AND
                                                  a~hbkid = b~hbkid AND
                                                  b~spras = @sy-langu

         WHERE a~bukrs = @p_bukrs
         INTO TABLE @DATA(it_t012).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'HBKID'
      dynprofield = 'SO_HBANK-LOW'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = it_t012.

START-OF-SELECTION.
  SELECT
        a~hbkid,
        c~hktid,
        c~bankn,
        c~hkont,
        b~banka FROM t012  AS a INNER JOIN bnka AS b ON b~banks = 'IN' AND
                                                  b~bankl = a~bankl
                                INNER JOIN t012k AS c ON c~bukrs = a~bukrs AND
                                                         c~hbkid = a~hbkid

         WHERE a~bukrs = @p_bukrs AND
         a~hbkid IN @so_hbank
    INTO TABLE @DATA(lt_to12k).
  SORT lt_to12k BY hkont.
  DATA lt_range_itab TYPE RANGE OF hkont.
  IF r1 = abap_true.
    lt_range_itab = VALUE #(
      FOR <ls_itab> IN lt_to12k
      ( sign = 'I'
        option = 'EQ'
        low = CONV num10( <ls_itab>-hkont + 1 ) )
    ).

  ELSE.
    lt_range_itab = VALUE #(
      FOR <ls_itab> IN lt_to12k
      ( sign = 'I'
        option = 'EQ'
        low = CONV num10( <ls_itab>-hkont + 2 ) )
    ).

  ENDIF.


  SELECT bukrs,
         hkont,
         gjahr,
         belnr,
         budat,
         blart,
         bschl,
         gsber,
         dmbtr,
         shkzg,
         sgtxt FROM bsis INTO TABLE @DATA(it_bsis1)
         WHERE hkont IN @lt_range_itab AND
               budat IN @so_budat AND
               bukrs EQ @p_bukrs.

******************To bring cleared item also**********************
  SELECT bukrs,
         hkont,
         gjahr,
         belnr,
         budat,
         blart,
         bschl,
         gsber,
         dmbtr,
         shkzg,
         sgtxt FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE @it_bsis1
         WHERE hkont IN @lt_range_itab AND
               budat IN @so_budat AND
               bukrs EQ @p_bukrs.
*     AND
*               blart IN ('KZ','DZ').

  IF it_bsis1 IS NOT INITIAL.

    SELECT a~bukrs,
           a~hkont,
           a~gjahr,
           a~belnr,
           a~augbl,
           a~bschl,
           a~gsber,
           a~dmbtr,
           a~shkzg,
           b~xblnr,
           b~bktxt,
           a~zuonr,
           a~sgtxt,
           a~lifnr,
           a~kunnr FROM bseg AS a INNER JOIN bkpf AS b ON a~belnr = b~belnr
          INTO TABLE @DATA(it_bseg)
          FOR ALL ENTRIES IN @it_bsis1 WHERE
          a~bukrs EQ @p_bukrs AND
          a~belnr EQ @it_bsis1-belnr AND
          a~hkont NE @it_bsis1-hkont.

    SELECT bukrs,
       hkont,
       gjahr,
       belnr,
       budat,
       blart,
       bschl,
       gsber,
       dmbtr,
       shkzg,
       sgtxt FROM bsis INTO TABLE @DATA(it_bsis)
             FOR ALL ENTRIES IN @it_bsis1
             WHERE budat IN @so_budat AND
                   bukrs EQ @p_bukrs AND
                   belnr EQ @it_bsis1-belnr AND
                   hkont NOT IN @lt_range_itab.

******************To bring cleared item also**********************
    SELECT bukrs,
       hkont,
       gjahr,
       belnr,
       budat,
       blart,
       bschl,
       gsber,
       dmbtr,
       shkzg,
       sgtxt FROM bsas APPENDING TABLE @it_bsis
             FOR ALL ENTRIES IN @it_bsis1
             WHERE budat IN @so_budat AND
                   bukrs EQ @p_bukrs AND
                   belnr EQ @it_bsis1-belnr  AND
                   hkont NOT IN @lt_range_itab.

  ENDIF.
  SORT it_bseg BY belnr.
  SORT it_bsis BY belnr.
  SORT it_bsis1 BY belnr.
  IF it_bsis IS NOT INITIAL.

    SELECT bukrs,
           kunnr,
           gjahr,
           belnr,
           budat,
           blart,
           shkzg,
           gsber,
           dmbtr,
           hkont FROM bsid INTO TABLE @DATA(it_bsid)
                  FOR ALL ENTRIES IN @it_bsis
                  WHERE belnr EQ @it_bsis-belnr AND
                        budat IN @so_budat AND
                        bukrs EQ @p_bukrs.

******************To bring cleared item also**********************
    SELECT bukrs,
           kunnr,
           gjahr,
           belnr,
           budat,
           blart,
           shkzg,
           gsber,
           dmbtr,
           hkont FROM bsad APPENDING TABLE @it_bsid
                  FOR ALL ENTRIES IN @it_bsis
                  WHERE belnr EQ @it_bsis-belnr AND
                        budat IN @so_budat AND
                        bukrs EQ @p_bukrs.
*******************************************************
    SELECT  bukrs,
            lifnr,
            gjahr,
            belnr,
            budat,
            blart,
            shkzg,
            gsber,
            dmbtr,
            hkont
             FROM bsik INTO TABLE @DATA(it_bsik)
               FOR ALL ENTRIES IN @it_bsis
                WHERE belnr EQ @it_bsis-belnr AND
                      budat IN @so_budat AND
                      bukrs EQ @p_bukrs.

******************To bring cleared item also**********************
    SELECT  bukrs,
            lifnr,
            gjahr,
            belnr,
            budat,
            blart,
            shkzg,
            gsber,
            dmbtr,
            hkont
      FROM bsak APPENDING TABLE @it_bsik
      FOR ALL ENTRIES IN @it_bsis
      WHERE belnr EQ @it_bsis-belnr AND
            budat IN @so_budat AND
            bukrs EQ @p_bukrs.

*******************************************************
  ENDIF.

  SORT it_bsid BY belnr.
  SORT it_bsik BY belnr.

  LOOP AT it_bsis1 INTO DATA(wa_bsis1).
************TO NEGLET BRS LINE ITEM******************
    IF wa_bsis1-blart NE 'ZR'.
***************************************************

      READ TABLE it_bsis INTO DATA(wa_bsis) WITH KEY belnr = wa_bsis1-belnr BINARY SEARCH.

      IF sy-subrc <> 0.

        READ TABLE it_bseg INTO DATA(wa_bseg) WITH KEY
                                                      belnr = wa_bsis1-belnr
                                                      gsber = wa_bsis1-gsber
                                                      gjahr = wa_bsis1-gjahr BINARY SEARCH.

        IF sy-subrc EQ 0.

          IF wa_bseg-lifnr IS NOT INITIAL.
            wa_bsis-hkont = wa_bseg-lifnr.
          ELSEIF wa_bseg-kunnr IS NOT INITIAL.
            wa_bsis-hkont = wa_bseg-kunnr.
          ELSE.
            wa_bsis-hkont = wa_bsis1-hkont.
          ENDIF.
          wa_bsis-gjahr = wa_bsis1-gjahr.
          wa_bsis-belnr = wa_bsis1-belnr.
          wa_bsis-budat = wa_bsis1-budat.
          wa_bsis-blart = wa_bsis1-blart.
          wa_bsis-bschl = wa_bsis1-bschl.
          wa_bsis-gsber = wa_bseg-gsber.
          wa_bsis-dmbtr =  wa_bsis1-dmbtr.
          wa_bsis-shkzg = wa_bseg-shkzg.
          wa_bsis-sgtxt = wa_bsis1-sgtxt.

          APPEND wa_bsis TO it_bsis.
          CLEAR: wa_bsis1,wa_bseg.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
  DATA it_bsis2 LIKE it_bsis.
  DATA wa_bsis2 LIKE LINE OF it_bsis2.

  LOOP AT it_bsik INTO DATA(wa_bsik).
    CLEAR wa_bsis2.
************TO NEGLET BRS LINE ITEM******************
    IF wa_bsik-blart NE 'ZR'.
***************************************************
      READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsik-belnr.
      IF sy-subrc EQ 0.
        wa_bsis2-bukrs = wa_bsik-bukrs.
        wa_bsis2-hkont = wa_bsik-lifnr.
        wa_bsis2-gjahr = wa_bsik-gjahr.
        wa_bsis2-belnr = wa_bsik-belnr.
        wa_bsis2-budat = wa_bsik-budat.
        wa_bsis2-blart = wa_bsik-blart.
        wa_bsis2-bschl = wa_bsis-bschl.
        wa_bsis2-shkzg = wa_bsik-shkzg.
        wa_bsis2-gsber = wa_bsik-gsber.
        IF wa_bsik-shkzg EQ 'H'.
          wa_bsis2-dmbtr = wa_bsik-dmbtr * -1.
        ELSEIF  wa_bsik-shkzg EQ 'S'.
          wa_bsis2-dmbtr = wa_bsik-dmbtr.
        ENDIF.

        APPEND wa_bsis2 TO it_bsis2.
        CLEAR: wa_bsik, wa_bsis.
      ENDIF.
    ENDIF.

  ENDLOOP.
  LOOP AT it_bsis INTO wa_bsis.
    CLEAR: wa_bsis2, wa_bseg.

    IF wa_bsis-blart NE 'ZR'.
      READ TABLE it_bsik INTO wa_bsik WITH KEY hkont = wa_bsis-hkont.
      IF sy-subrc <> 0.

        READ TABLE it_bsid INTO DATA(wa_bsid) WITH KEY hkont = wa_bsis-hkont.
        IF sy-subrc <> 0.
          wa_bsis2-bukrs = wa_bsis-bukrs.
          wa_bsis2-hkont = wa_bsis-hkont.
          wa_bsis2-gjahr = wa_bsis-gjahr.
          wa_bsis2-belnr = wa_bsis-belnr.
          wa_bsis2-budat = wa_bsis-budat.
          wa_bsis2-blart = wa_bsis-blart.
          wa_bsis2-bschl = wa_bsis-bschl.
          wa_bsis2-shkzg = wa_bsis-shkzg.
          wa_bsis2-gsber = wa_bsis-gsber.
          IF wa_bsis-shkzg EQ 'H'.
            wa_bsis2-dmbtr =  wa_bsis-dmbtr * -1.
          ELSE.
            wa_bsis2-dmbtr = wa_bsis-dmbtr.
          ENDIF.
          APPEND wa_bsis2 TO it_bsis2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: wa_bsik, wa_bsis.
  LOOP AT it_bsid INTO wa_bsid.
    CLEAR wa_bsis2.
    READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsid-belnr BINARY SEARCH.

    IF sy-subrc EQ 0 AND wa_bsis-blart NE 'ZR'. " TO NEGLET BRS LINE ITEM
      wa_bsis2-bukrs = wa_bsid-bukrs.
      wa_bsis2-hkont = wa_bsid-kunnr.
      wa_bsis2-gjahr = wa_bsid-gjahr.
      wa_bsis2-belnr = wa_bsid-belnr.
      wa_bsis2-budat = wa_bsid-budat.
      wa_bsis2-blart = wa_bsid-blart.
      wa_bsis2-bschl = wa_bsis-bschl.
      wa_bsis2-shkzg = wa_bsid-shkzg.
      wa_bsis2-gsber = wa_bsid-gsber.
      IF wa_bsid-shkzg EQ 'H'.
        wa_bsis2-dmbtr = wa_bsid-dmbtr * -1.
      ELSEIF wa_bsid-shkzg EQ 'S'.
        wa_bsis2-dmbtr = wa_bsid-dmbtr.
      ENDIF.
      APPEND wa_bsis2 TO it_bsis2.
    ENDIF.
  ENDLOOP.

  CLEAR: wa_bsik, wa_bsis, wa_bsid.

  SORT it_bsis2 BY belnr.
  IF it_bsis2 IS NOT INITIAL.
*    IF it_bsid IS NOT INITIAL.
*    IF r1 = abap_true. " Incomming
*
*      SELECT kunnr,
*             belnr,
*             shkzg,
*             gsber,
*             dmbtr  FROM "KUNNR BELNR SHKZG GSBER DMBTR
*      bsid INTO TABLE @DATA(it_bsid)
*      FOR ALL ENTRIES IN @it_bsis1 WHERE
*      belnr EQ @it_bsis1-belnr AND
*      budat IN @so_budat AND
*      bukrs EQ @p_bukrs. " AND BLART NE 'ZR'.
*
*******************To bring cleared item also**********************
*      SELECT kunnr,
*             belnr,
*             shkzg,
*             gsber,
*             dmbtr   FROM "KUNNR BELNR SHKZG GSBER DMBTR
*      bsad APPENDING CORRESPONDING FIELDS OF TABLE @it_bsid
*      FOR ALL ENTRIES IN @it_bsis1 WHERE belnr EQ @it_bsis1-belnr AND
*      budat IN @so_budat AND bukrs EQ @p_bukrs. " AND BLART NE 'ZR'.
********************************************************

    SELECT kunnr,
           name1 FROM kna1
           INTO TABLE @DATA(it_kna1) FOR ALL ENTRIES IN @it_bsis2
           WHERE kunnr EQ @it_bsis2-hkont.
*    ENDIF.
*    ELSE. " Outgoing
**    IF it_bsik IS NOT INITIAL.
*      SELECT lifnr,
*             belnr,
*             shkzg,
*             gsber,
*             dmbtr FROM bsik
*      INTO TABLE @DATA(it_bsik)
*      FOR ALL ENTRIES IN @it_bsis1 WHERE belnr EQ @it_bsis1-belnr
*      AND budat IN @so_budat AND bukrs EQ @p_bukrs. " AND BLART NE 'ZR'.
*
*******************To bring cleared item also**********************
*      SELECT lifnr,
*             belnr,
*             shkzg,
*             gsber,
*             dmbtr  FROM
*      bsak APPENDING CORRESPONDING FIELDS OF TABLE @it_bsik
*      FOR ALL ENTRIES IN @it_bsis1 WHERE belnr EQ @it_bsis1-belnr
*      AND budat IN @so_budat AND bukrs EQ @p_bukrs. " AND BLART NE 'ZR'.
********************************************************

    SELECT lifnr,
           name1 FROM lfa1 INTO TABLE @DATA(it_lfa1) FOR ALL ENTRIES IN @it_bsis2
           WHERE lifnr EQ @it_bsis2-hkont.
**    ENDIF.
*    ENDIF.
    SELECT saknr,
           txt50 FROM skat INTO TABLE @DATA(it_skat) FOR ALL ENTRIES IN @it_bsis2
           WHERE spras = @sy-langu AND saknr EQ @it_bsis2-hkont AND ktopl EQ 'YAIN'.

**************TO GET BUSINESS AREA DESCRIPTION*****************
*    SELECT gsber,
*           gtext
*           FROM tgsbt INTO TABLE @DATA(it_tgsbt)
*           FOR ALL ENTRIES IN @it_bsis2
*           WHERE gsber EQ @it_bsis2-gsber AND
*                 spras EQ @sy-langu.
*********************************************************************

  ENDIF.

  SORT it_lfa1 BY lifnr.
  SORT it_kna1 BY kunnr.
  SORT it_skat BY saknr.
  LOOP AT it_bsis2 INTO wa_bsis2.

    READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY belnr = wa_bsis2-belnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF r1 = abap_true. " Incoming
        wa_data-dmbtr = wa_bsis1-dmbtr.
      ELSE. " Outgoing
        IF wa_bsis2-dmbtr LT 0.
          wa_bsis2-dmbtr = wa_bsis2-dmbtr * -1.
        ELSE.
          wa_bsis2-dmbtr = wa_bsis2-dmbtr * -1.
        ENDIF.
      ENDIF.
      wa_data-belnr = wa_bsis2-belnr.
      wa_data-budat = wa_bsis2-budat.
      wa_data-hkont = wa_bsis2-hkont.
      READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_bsis2-hkont BINARY SEARCH.
      READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = wa_bsis2-hkont BINARY SEARCH.
      READ TABLE it_skat INTO DATA(wa_skat) WITH KEY saknr = wa_bsis2-hkont BINARY SEARCH.
*      IF sy-subrc = 0.
*        wa_data-gldesc = wa_skat-txt50.
*      ENDIF.

      IF wa_lfa1 IS NOT INITIAL.
        wa_data-gldesc = wa_lfa1-name1.
      ELSEIF wa_kna1 IS NOT INITIAL.
        wa_data-gldesc = wa_kna1-name1.
      ELSEIF wa_skat IS NOT INITIAL.
        wa_data-gldesc = wa_skat-txt50.
      ENDIF.
      " Business Area and Business Text
*    wa_data-gsber = wa_bsis2-gsber.
*    READ TABLE it_tgsbt INTO DATA(wa_tgsbt) WITH KEY gsber = wa_bsis2-gsber.
*
*    IF sy-subrc EQ 0.
*      wa_data-gtext = wa_tgsbt-gtext.
*    ENDIF.

      READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_bsis2-belnr
                                               gjahr = wa_bsis2-gjahr
                                               bukrs = wa_bsis2-bukrs BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_data-zuonr = wa_bseg-zuonr.
        wa_data-xblnr = wa_bseg-xblnr.
      ENDIF.
      wa_data-dmbtr = wa_bsis2-dmbtr.
*      READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY hkont = wa_bsis2-hkont belnr = wa_bsis2-belnr.
      wa_data-sgtxt = wa_bsis1-sgtxt.
      wa_data-gjahr = wa_bsis1-gjahr.

      wa_data-bukrs = wa_bsis2-bukrs.
        " House Bank and Bank Text
        READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY belnr = wa_bsis2-belnr.
        IF sy-subrc = 0.
          DATA(l_index) = sy-tabix.
          IF wa_bsis1-belnr = wa_bsis2-belnr AND
             wa_bsis1-hkont = wa_bsis2-hkont.
            READ TABLE it_bsis1 INTO wa_bsis1 INDEX l_index + 1.
            IF sy-subrc = 0 AND
               wa_bsis1-belnr = wa_bsis2-belnr AND
               wa_bsis1-hkont = wa_bsis2-hkont.
              CLEAR wa_bsis1.
            ELSE.
              IF r1 = abap_true.
                DATA(l_hkont)  = CONV num10( wa_bsis1-hkont - 1 ).
              ELSE.
                l_hkont = CONV num10( wa_bsis1-hkont - 2 ).
              ENDIF.
            ENDIF.
          ELSE.
            IF r1 = abap_true.
              l_hkont  = CONV num10( wa_bsis1-hkont - 1 ).
            ELSE.
              l_hkont = CONV num10( wa_bsis1-hkont - 2 ).
            ENDIF.
          ENDIF.
        ENDIF.
        READ TABLE lt_to12k INTO DATA(lw_to12k) WITH KEY hkont = l_hkont BINARY SEARCH.
        IF sy-subrc = 0.
          wa_data-hbkid = lw_to12k-hbkid.
*        wa_data-hktid = lw_to12k-hktid.
          wa_data-text1 = lw_to12k-banka.
          wa_data-bankn = lw_to12k-bankn.
        ENDIF.

        READ TABLE lt_to12k INTO lw_to12k WITH KEY hkont = l_hkont BINARY SEARCH.
        IF sy-subrc = 0.
          wa_data-hbkid = lw_to12k-hbkid.
*        wa_data-hktid = lw_to12k-hktid.
          wa_data-text1 = lw_to12k-banka.
          wa_data-bankn = lw_to12k-bankn.
        ENDIF.

      IF r1 = abap_true.
        APPEND wa_data TO lt_cust.
      ELSE.
        APPEND wa_data TO lt_vend.
      ENDIF.
    ENDIF.
    CLEAR: wa_data,
           wa_lfa1,
           wa_kna1,
           wa_skat,
*         wa_tgsbt ,
           wa_bseg.
  ENDLOOP.

  DATA: l_fields1 TYPE text30,
        l_fields2 TYPE text30.
  SORT lt_vend BY hbkid belnr.
  SORT lt_cust BY hbkid belnr.
  IF r_sumary = abap_true.
    l_fields1 = 'BANKN;GJAHR;ZUONR;BKTXT;BELNR'.
    l_fields2 = 'BUDAT;HKONT;GLDESC;XBLNR;SGTXT'.

    DATA lt_cust_temp LIKE lt_cust.
    CLEAR wa_data.
    LOOP AT lt_cust INTO wa_data.
      READ TABLE lt_cust_temp ASSIGNING FIELD-SYMBOL(<fs_sum>) WITH KEY hbkid = wa_data-hbkid BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_sum>-dmbtr = <fs_sum>-dmbtr + wa_data-dmbtr.
      ELSE.
        APPEND wa_data TO lt_cust_temp.
      ENDIF.
    ENDLOOP.
    CLEAR lt_cust. lt_cust = lt_cust_temp.
    DATA lt_vend_temp LIKE lt_vend.
    LOOP AT lt_vend INTO wa_data.
      READ TABLE lt_vend_temp ASSIGNING <fs_sum> WITH KEY hbkid = wa_data-hbkid BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_sum>-dmbtr = <fs_sum>-dmbtr + wa_data-dmbtr.
      ELSE.
        APPEND wa_data TO lt_vend_temp.
      ENDIF.
    ENDLOOP.
    CLEAR lt_vend. lt_vend = lt_vend_temp.
  ELSE.
    l_fields1 = 'BANKN;GJAHR;ZUONR;BKTXT'.
  ENDIF.
  IF r1 = abap_true.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
        i_repid            = sy-repid
        i_title            = 'Customer Payment Details'
        i_hyperlink_column = 'BELNR'
        i_hyperlink_data   = 'FB03'
        i_hide_column      = l_fields1
        i_hide_column_ext  = l_fields2
      TABLES
        it_alv             = lt_cust.
  ELSE.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
        i_repid            = sy-repid
        i_title            = 'Vendor Payment Details'
        i_hyperlink_column = 'BELNR'
        i_hyperlink_data   = 'FB03'
        i_hide_column      = l_fields1
        i_hide_column_ext  = l_fields2
      TABLES
        it_alv             = lt_vend.
    APPEND wa_data TO lt_vend.
  ENDIF.


********************TO TAKE OPENING BALANCE OF ALL THREE G/L***********************
*
*  SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsis INTO TABLE lt_data
*  WHERE  budat LT s_budat-low AND  "For calculating opening balance
*  bukrs EQ p_bukrs AND hkont = p_chqi.
*
*  SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsas  APPENDING CORRESPONDING FIELDS OF TABLE lt_data
*  WHERE bukrs EQ p_bukrs AND hkont = p_chqi AND
*  budat LT s_budat-low AND augdt GT s_budat-low.
*
*
*  SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsis  APPENDING CORRESPONDING FIELDS OF TABLE lt_data
*  WHERE  budat LT s_budat-low AND  "For calculating opening balance
*  bukrs EQ p_bukrs AND hkont = p_chqd.
*
*
*  SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsas  APPENDING CORRESPONDING FIELDS OF TABLE lt_data
*  WHERE bukrs EQ p_bukrs AND hkont = p_chqd AND
*  budat LT s_budat-low AND augdt GT s_budat-low.
