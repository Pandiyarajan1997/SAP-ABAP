FUNCTION zbapi_account_payable_cust.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FISCAL_YEAR) TYPE  GJAHR
*"     VALUE(MONTH) TYPE  ZMONTHS
*"  TABLES
*"      LT_BSIS STRUCTURE  ZSTR_BSIS
*********************************************************
*& Date Created - 17.05.2022
*& Created By   - KPABAP (shamsudeen)
*& Description  - Program to get the GL Account line items details from FBL3N
*& TR No: DEVK931702
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_bsis," Structure for Open item data
           bukrs TYPE bukrs,
           hkont TYPE hkont,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           xblnr TYPE xblnr1,
           monat TYPE monat,
           kostl TYPE kostl,
           werks TYPE werks_d,
           blart TYPE blart,
           gkont TYPE gkont,
           augbl TYPE augbl,
           dmbtr TYPE dmbtr,
           shkzg TYPE shkzg,
           sgtxt TYPE txt50,
           augdt TYPE augdt,
           zfbdt TYPE dzfbdt,
           gkart TYPE gkart,
           gsber TYPE gsber,
           ebeln TYPE ebeln,
         END OF lty_bsis,
         BEGIN OF lty_bsas, " Structure for Cleared item data
           bukrs TYPE bukrs,
           hkont TYPE hkont,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           augdt TYPE augdt,
           zfbdt TYPE dzfbdt,
           xblnr TYPE xblnr1,
           kostl TYPE kostl,
           monat TYPE monat,
           shkzg TYPE shkzg,
           blart TYPE blart,
           dmbtr TYPE dmbtr,
           augbl TYPE augbl,
           gkont TYPE gkont,
           werks TYPE werks_d,
           sgtxt TYPE txt50,
           ebeln TYPE ebeln,
         END OF lty_bsas,
         BEGIN OF lty_bsik, "For Vendor Open item
           lifnr TYPE lifnr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           hkont TYPE hkont,
         END OF lty_bsik,
         BEGIN OF lty_bsak,"For Vendor Cleared item
           lifnr TYPE lifnr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           hkont TYPE hkont,
         END OF lty_bsak,
         BEGIN OF lty_bsad, "For Customer Open item
           kunnr TYPE kunnr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           hkont TYPE hkont,
         END OF lty_bsad,
         BEGIN OF lty_bsid, "For Customer Cleared item
           kunnr TYPE kunnr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           hkont TYPE hkont,
         END OF lty_bsid,
         BEGIN OF lty_bkpf, "For entered Date and username
           belnr TYPE belnr_d,
           blart TYPE blart,
           bldat TYPE bldat,
           budat TYPE budat,
           monat TYPE monat,
           gjahr TYPE gjahr,
           xblnr TYPE xblnr1,
           usnam TYPE usnam,
           cpudt TYPE cpudt,
           awkey TYPE awkey,
         END OF lty_bkpf,
         BEGIN OF lty_skat, "GL Account Text
           saknr TYPE saknr,
           txt50 TYPE txt50_skat,
         END OF lty_skat,
         BEGIN OF lty_kna1, "Customer Name
           kunnr TYPE kunnr,
           name1 TYPE name1_gp,
         END OF lty_kna1,
         BEGIN OF lty_lfa1, "Vendor Name
           lifnr TYPE lifnr,
           name1 TYPE name1_gp,
         END OF lty_lfa1.

  DATA: ls_bsis  TYPE zstr_bsis,
        ls_bsis1 TYPE zstr_bsis.  " structure for GL Account open items

**************Internal Table Declaration*********************************
  DATA: it_bsis TYPE STANDARD TABLE OF lty_bsis,
        gs_bsis TYPE lty_bsis,
        it_bsas TYPE STANDARD TABLE OF lty_bsas,
        gs_bsas TYPE lty_bsas,
        it_bsik TYPE STANDARD TABLE OF lty_bsik,
        gs_bsik TYPE lty_bsik,
        it_bsak TYPE STANDARD TABLE OF lty_bsak,
        gs_bsak TYPE lty_bsak,
        it_bsid TYPE STANDARD TABLE OF lty_bsid,
        gs_bsid TYPE lty_bsid,
        it_bsad TYPE STANDARD TABLE OF lty_bsad,
        gs_bsad TYPE lty_bsad,
        it_bkpf TYPE STANDARD TABLE OF lty_bkpf,
        gs_bkpf TYPE lty_bkpf,
        it_skat TYPE STANDARD TABLE OF lty_skat,
        gs_skat TYPE lty_skat,
        it_kna1 TYPE STANDARD TABLE OF lty_kna1,
        gs_kna1 TYPE lty_kna1,
        it_lfa1 TYPE STANDARD TABLE OF lty_lfa1,
        gs_lfa1 TYPE lty_lfa1.


****************Month Conversion For Financial Month****************
  DATA: month1     TYPE zmonth,
        start_date TYPE sy-datum,
        end_date   TYPE sy-datum,
        final      TYPE spbup.

  CLEAR: month1,final,start_date,end_date.

  IF month EQ '01'.
    month1 = '04'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '02'.
    month1 = '05'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '03'.
    month1 = '06'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '04'.
    month1 = '07'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '05'.
    month1 = '08'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '06'.
    month1 = '09'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '07'.
    month1 = '10'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '08'.
    month1 = '11'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '09'.
    month1 = '12'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '10'.
    month1 = '01'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '11'.
    month1 = '02'.
    CONCATENATE fiscal_year month1 INTO final.
  ELSEIF month EQ '12'.
    month1 = '03'.
    CONCATENATE fiscal_year month1 INTO final.
  ENDIF.

****start date for every month conversion**************
  start_date = final.
  start_date+6(2) = '01'.

****End date for every month conversion**************
  IF final IS NOT INITIAL.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = start_date
      IMPORTING
        last_day_of_month = end_date.
  ENDIF.

  REFRESH: it_bsis,it_bsas,it_bsak,it_bsik,it_bsid,it_bsad,it_bkpf.
  CLEAR: gs_bsis,gs_bsas,gs_bsik,gs_bsak,gs_bsid,gs_bsad,gs_bkpf.

  IF start_date IS NOT INITIAL
     AND end_date IS NOT INITIAL.
********Fetching GL Account open items from bsis*******************
    SELECT bukrs
           hkont
           gjahr
           belnr
           budat
           bldat
           xblnr
           monat
           kostl
           werks
           blart
           gkont
           augbl
           dmbtr
           shkzg
           sgtxt
           augdt
           zfbdt
           gkart
           gsber
           ebeln FROM bsis
                 INTO TABLE it_bsis
                 WHERE ( budat BETWEEN start_date AND end_date ).
    IF sy-subrc EQ 0.
      SORT it_bsis[] BY bukrs hkont.
    ENDIF.
***********Fetching Cleared items from bsas*************
    SELECT bukrs
           hkont
           gjahr
           belnr
           budat
           bldat
           augdt
           zfbdt
           xblnr
           kostl
           monat
           shkzg
           blart
           dmbtr
           augbl
           gkont
           werks
           sgtxt
           ebeln FROM bsas
                 INTO TABLE it_bsas
                 WHERE ( budat BETWEEN start_date AND end_date ).
    IF sy-subrc EQ 0.
      SORT it_bsas[] BY bukrs hkont.
    ENDIF.
********Fetching Vendor open items from bsik*******************
    SELECT lifnr
           belnr
           budat
           bldat
           hkont FROM bsik
                 INTO TABLE it_bsik

                 WHERE ( budat BETWEEN start_date AND end_date ).
********Fetching Vendor Cleared items from bsak*******************
    SELECT lifnr
           belnr
           budat
           bldat
           hkont FROM bsak
                 INTO TABLE it_bsak
                 WHERE ( budat BETWEEN start_date AND end_date ).
********Fetching Customer open items from bsid*******************
    SELECT kunnr
           belnr
           budat
           bldat
           hkont FROM bsid
                 INTO TABLE it_bsid
                 WHERE ( budat BETWEEN start_date AND end_date ).
********Fetching Customer Cleared items from bsad*******************
    SELECT kunnr
           belnr
           budat
           bldat
           hkont FROM bsad
                 INTO TABLE it_bsad
                 WHERE ( budat BETWEEN start_date AND end_date ).
********Fetching Document Enterdate and Username from bkpf*******************
    SELECT belnr
           blart
           bldat
           budat
           monat
           gjahr
           xblnr
           usnam
           cpudt
           awkey FROM bkpf
                 INTO TABLE it_bkpf
                 WHERE gjahr EQ fiscal_year
                 AND monat EQ month
                 AND ( budat BETWEEN start_date AND end_date ).
***********Fetching GL Account Text From Skat*********************************
    SELECT saknr
           txt50 FROM skat
                 INTO TABLE it_skat
                 WHERE spras EQ sy-langu.
************Fetching Customer Name From Kna1**********************************
    SELECT kunnr
           name1 FROM kna1
                 INTO TABLE it_kna1.
************Fetching Vendor Name From Lfa1**********************************
    SELECT Lifnr
           name1 FROM lfa1
                 INTO TABLE it_Lfa1.


  ENDIF.

  DATA: jamon1 TYPE jamon_xpo.
  DATA: verzn1 TYPE verzn_cm.
  DATA: faedt1 TYPE faedt_fpos.

  LOOP AT it_bsis INTO gs_bsis.
    CLEAR ls_bsis.
    ls_bsis-gjahr = gs_bsis-gjahr.
    ls_bsis-monat = gs_bsis-monat.
    ls_bsis-bukrs = gs_bsis-bukrs.
    ls_bsis-belnr = gs_bsis-belnr.
    ls_bsis-budat = gs_bsis-budat.
    ls_bsis-bldat = gs_bsis-bldat.
    ls_bsis-hkont = gs_bsis-hkont.
    ls_bsis-xblnr = gs_bsis-xblnr.
    ls_bsis-rebzg = gs_bsis-belnr.
    ls_bsis-kostl = gs_bsis-kostl.
    ls_bsis-werks = gs_bsis-werks.
    ls_bsis-augbl = gs_bsis-augbl.
    ls_bsis-dmbtr = gs_bsis-dmbtr.
    ls_bsis-gkont = gs_bsis-gkont.
    ls_bsis-shkzg = gs_bsis-shkzg.
    ls_bsis-blart = gs_bsis-blart.
    ls_bsis-sgtxt = gs_bsis-sgtxt.
    ls_bsis-augdt = gs_bsis-augdt.
    ls_bsis-faedt = gs_bsis-zfbdt.
*********Fetching Enterdate AND Username From BKPF based on Document Number****************
    READ TABLE it_bkpf INTO gs_bkpf WITH KEY belnr = ls_bsis-belnr
                                             blart = ls_bsis-blart
                                             xblnr = ls_bsis-xblnr
                                             bldat = ls_bsis-bldat
                                             budat = ls_bsis-budat
                                             monat = ls_bsis-monat
                                             gjahr = ls_bsis-gjahr.
    IF sy-subrc EQ 0.
      ls_bsis-awkey = gs_bkpf-awkey.
      ls_bsis-u_cpudt = gs_bkpf-cpudt.
      ls_bsis-u_usnam = gs_bkpf-usnam.
    ENDIF.
    IF ls_bsis-faedt IS NOT INITIAL.
      CLEAR verzn1.
      CALL FUNCTION 'ITEM_OVERDUE_DAYS'
        EXPORTING
          key_date     = sy-datlo
          due_date     = ls_bsis-faedt
        IMPORTING
          overdue_days = verzn1.

      ls_bsis-verzn = verzn1.
    ELSEIF ls_bsis-faedt IS INITIAL.
      ls_bsis-verzn = '0'.
    ENDIF.

    CLEAR jamon1.
    CONCATENATE gs_bsis-gjahr '/' gs_bsis-monat  INTO jamon1.
    ls_bsis-jamon = jamon1.

****************Fetching LIFNR Number from BSIK and BSAK**********************
    READ TABLE it_bsik INTO gs_bsik WITH KEY belnr = gs_bsis-belnr
                                             bldat = gs_bsis-bldat
                                             budat = gs_bsis-budat.
    IF sy-subrc EQ 0.
      ls_bsis-lifnr_bte = gs_bsik-lifnr.
    ELSE.
      READ TABLE it_bsak INTO gs_bsak WITH KEY belnr = gs_bsis-belnr
                                               bldat = gs_bsis-bldat
                                               budat = gs_bsis-budat.
      ls_bsis-lifnr_bte = gs_bsak-lifnr.
    ENDIF.
****************Fetching KUNNR Number from BSID and BSAD**********************
    READ TABLE it_bsid INTO gs_bsid WITH KEY belnr = gs_bsis-belnr
                                             bldat = gs_bsis-bldat
                                             budat = gs_bsis-budat.
    IF sy-subrc EQ 0.
      ls_bsis-kunnr_bte = gs_bsid-kunnr.
    ELSE.
      READ TABLE it_bsad INTO gs_bsad WITH KEY belnr = gs_bsis-belnr
                                               bldat = gs_bsis-bldat
                                               budat = gs_bsis-budat.
      ls_bsis-kunnr_bte = gs_bsad-kunnr.
    ENDIF.
********Fetching name for KUNNR********************************
    READ TABLE it_kna1 INTO gs_kna1 WITH KEY kunnr = ls_bsis-kunnr_bte.
    IF sy-subrc EQ 0.
      ls_bsis-name1_bte = gs_kna1-name1.
    ENDIF.
**********Fetching name for LIFNR******************************************
    READ TABLE it_lfa1 INTO gs_lfa1 WITH KEY lifnr = ls_bsis-lifnr_bte.
    IF sy-subrc EQ 0.
      ls_bsis-name2_bte = gs_lfa1-name1.
    ENDIF.
    ls_bsis-gkart = gs_bsis-gkart.
    ls_bsis-gkont = gs_bsis-gkont.
    ls_bsis-zaldt = ls_bsis-faedt.
**************************************************************************
    ls_bsis-ebeln = gs_bsis-ebeln.
***********Fetching GL Account Text From Skat*********************************
    READ TABLE it_skat INTO gs_skat WITH KEY saknr = gs_bsis-hkont.
    IF sy-subrc EQ 0.
      ls_bsis-zgltext = gs_skat-txt50.
    ENDIF.
    APPEND ls_bsis TO lt_bsis.
  ENDLOOP.

  DATA: jamon2 TYPE jamon_xpo.
  DATA: verzn2 TYPE verzn_cm.

  LOOP AT it_bsas INTO gs_bsas.
    CLEAR ls_bsis1.
    ls_bsis1-gjahr = gs_bsas-gjahr.
    ls_bsis1-monat = gs_bsas-monat.
    ls_bsis1-bukrs = gs_bsas-bukrs.
    ls_bsis1-belnr = gs_bsas-belnr.
    ls_bsis1-budat = gs_bsas-budat.
    ls_bsis1-bldat = gs_bsas-bldat.
    ls_bsis1-blart = gs_bsas-blart.
    ls_bsis1-xblnr = gs_bsas-xblnr.
    ls_bsis1-kostl = gs_bsas-kostl.
    ls_bsis1-werks = gs_bsas-werks.
    ls_bsis1-hkont = gs_bsas-hkont.
    ls_bsis1-rebzg = gs_bsas-belnr.
    ls_bsis1-augbl = gs_bsas-augbl.
    ls_bsis1-dmbtr = gs_bsas-dmbtr.
    ls_bsis1-gkont = gs_bsas-gkont.
    ls_bsis1-faedt = gs_bsas-zfbdt.
**********Fetching Enterdate AND Username From BKPF based on Document Number****************
    READ TABLE it_bkpf INTO gs_bkpf WITH KEY belnr = ls_bsis1-belnr
                                             blart = ls_bsis1-blart
                                             xblnr = ls_bsis1-xblnr
                                             bldat = ls_bsis1-bldat
                                             budat = ls_bsis1-budat
                                             monat = ls_bsis1-monat
                                             gjahr = ls_bsis1-gjahr.
    IF sy-subrc EQ 0.
      ls_bsis1-awkey = gs_bkpf-awkey.
      ls_bsis1-u_cpudt = gs_bkpf-cpudt.
      ls_bsis1-u_usnam = gs_bkpf-usnam.
    ENDIF.

    IF ls_bsis1-faedt IS NOT INITIAL.
      CLEAR verzn1.
      CALL FUNCTION 'ITEM_OVERDUE_DAYS'
        EXPORTING
          key_date     = sy-datlo
          due_date     = ls_bsis1-faedt
        IMPORTING
          overdue_days = verzn1.

      ls_bsis1-verzn = verzn1.
    ELSEIF ls_bsis1-faedt IS INITIAL.
      ls_bsis1-verzn = '0'.
    ENDIF.
    CLEAR jamon2.
    CONCATENATE gs_bsis-gjahr '/' gs_bsis-monat  INTO jamon2.
    ls_bsis1-jamon = jamon2.
****************Fetching LIFNR Number from BSIK and BSAK**********************
    READ TABLE it_bsik INTO gs_bsik WITH KEY belnr = gs_bsas-belnr
                                             bldat = gs_bsas-bldat
                                             budat = gs_bsas-budat.
    IF sy-subrc EQ 0.
      ls_bsis1-lifnr_bte = gs_bsik-lifnr.
    ELSE.
      READ TABLE it_bsak INTO gs_bsak WITH KEY belnr = gs_bsas-belnr
                                               bldat = gs_bsas-bldat
                                               budat = gs_bsas-budat.
      IF sy-subrc EQ 0.
        ls_bsis1-lifnr_bte = gs_bsak-lifnr.
      ENDIF.
    ENDIF.
****************Fetching KUNNR Number from BSID and BSAD**********************
    READ TABLE it_bsid INTO gs_bsid WITH KEY belnr = gs_bsas-belnr
                                             bldat = gs_bsas-bldat
                                             budat = gs_bsas-budat.
    IF sy-subrc EQ 0.
      ls_bsis1-kunnr_bte = gs_bsid-kunnr.
    ELSE.
      READ TABLE it_bsad INTO gs_bsad WITH KEY belnr = gs_bsas-belnr
                                               bldat = gs_bsas-bldat
                                               budat = gs_bsas-budat.
      IF sy-subrc EQ 0.
        ls_bsis1-kunnr_bte = gs_bsad-kunnr.
      ENDIF.
    ENDIF.
*-----------------------------------------------------------------------------------------
    ls_bsis1-shkzg = gs_bsas-shkzg.
    ls_bsis1-sgtxt = gs_bsas-sgtxt.

********Fetching name for KUNNR********************************
    READ TABLE it_kna1 INTO gs_kna1 WITH KEY kunnr = ls_bsis1-kunnr_bte.
    IF sy-subrc EQ 0.
      ls_bsis1-name1_bte = gs_kna1-name1.
    ENDIF.
********Fetching name for LIFNR********************************
    READ TABLE it_lfa1 INTO gs_lfa1 WITH KEY lifnr = ls_bsis1-lifnr_bte.
    IF sy-subrc EQ 0.
      ls_bsis1-name2_bte = gs_lfa1-name1.
    ENDIF.
*****-------------------------------------------------------------------
    ls_bsis1-augdt = gs_bsas-augdt.
    ls_bsis1-gkont = gs_bsas-gkont.
    ls_bsis1-zaldt = ls_bsis1-faedt.
***********Fetching GL Account Text From Skat*********************************
    READ TABLE it_skat INTO gs_skat WITH KEY saknr = gs_bsas-hkont.
    IF sy-subrc EQ 0.
      ls_bsis1-zgltext = gs_skat-txt50.
    ENDIF.
    ls_bsis1-ebeln = gs_bsas-ebeln.
    APPEND ls_bsis1 TO lt_bsis.

  ENDLOOP.

  IF lt_bsis[] IS NOT INITIAL.
    IF sy-subrc  EQ 0.
      SORT lt_bsis[] by bukrs werks.
    ENDIF.

  ENDIF.


ENDFUNCTION.
