*&---------------------------------------------------------------------*
*& Include          ZMM_PO_FORM_EMAIL_FORMS
*&---------------------------------------------------------------------*
FORM actual_process.
** Fetching data from PO email logs table for sending Mail **
  REFRESH: gt_logs.
  SELECT * FROM zpo_email_log
           INTO TABLE gt_logs
           WHERE ebeln IN s_ebeln
           AND approval_date IN s_aedat
           AND email_flag NE 'X'
           AND ven_email NE ''.
  IF sy-subrc EQ 0.
    SORT gt_logs[] BY ebeln vendor_id.
** Tvarvc Variable for PO Document type
    SELECT * FROM tvarvc INTO TABLE @DATA(gt_tvarvc)
                         WHERE name = 'ZPO_EMAIL_VAR1'
                         AND type = 'S'.
    LOOP AT gt_tvarvc INTO DATA(gs_tvarvc).
      CLEAR gs_bsart.
      gs_bsart-sign = 'I'.
      gs_bsart-option = 'EQ'.
      gs_bsart-low = gs_tvarvc-low.
      APPEND gs_bsart TO gt_bsart.
    ENDLOOP.
** Based on log table fetching PO header Details **
    REFRESH: gt_ekko.
    SELECT ebeln
           aedat
           lifnr FROM ekko
                 INTO TABLE gt_ekko
                 FOR ALL ENTRIES IN gt_logs
                 WHERE ebeln = gt_logs-ebeln
                 AND bsart IN gt_bsart.
    IF sy-subrc EQ 0.
      SORT gt_ekko[] BY ebeln.
      SELECT ebeln, ebelp, loekz FROM ekpo INTO TABLE @DATA(gt_ekpo)
                                 FOR ALL ENTRIES IN @gt_ekko
                                 WHERE ebeln = @gt_ekko-ebeln.
      IF sy-subrc = 0.
        SORT gt_ekpo[] BY ebeln ebelp.
      ENDIF.
    ENDIF.
  ENDIF.

*---- Added by Samsudeen M on 03.03.2023 --------*
  LOOP AT gt_logs ASSIGNING FIELD-SYMBOL(<fs_logs>).
    DATA(lv_ebeln) = VALUE #( gt_ekpo[ ebeln = <fs_logs>-ebeln loekz = 'L' ] OPTIONAL ).
    IF lv_ebeln IS NOT INITIAL.
      DELETE gt_logs WHERE ebeln = lv_ebeln.
    ELSE.
      DATA(lv_ponumber) = VALUE #( gt_ekpo[ ebeln = <fs_logs>-ebeln loekz = 'S' ]-ebeln OPTIONAL ).
      IF lv_ponumber IS NOT INITIAL.
        DELETE gt_logs WHERE ebeln = lv_ponumber.
      ENDIF.
    ENDIF.
  ENDLOOP.
*------------------------------------------------*
  IF gt_logs[] IS NOT INITIAL.
    LOOP AT gt_logs INTO gs_logs.
      CLEAR gs_ekko.
      READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = gs_logs-ebeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        ekko-ebeln = gs_ekko-ebeln.
        ekko-aedat = gs_ekko-aedat.
      ENDIF.
*** Processing logic
      PERFORM entry_neu.

*** Form calling
      PERFORM form.
** Coverting PDF to attachment **
      PERFORM convert_to_pdf.
** Sending Email to respected vendor**
      PERFORM send_email.
      CLEAR gs_logs.
    ENDLOOP.
  ELSE.
    MESSAGE 'No Approved PO available in table' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM entry_neu .


  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.
  DATA: ent_retco  TYPE sy-subrc,
        xscreen,                         "Kz. POP-UP
        ent_screen.                         "Kz. Probeausgabe
  xscreen = ent_screen = 'X'.
  CLEAR ent_retco.
  l_druvo = '1'.

  gw_nast-mandt = sy-mandt.
  gw_nast-kappl = 'EF'.
  gw_nast-objky = ekko-ebeln.
  gw_nast-kschl = 'ZNEU'.
  gw_nast-spras = 'E'.
  gw_nast-parnr = ekko-lifnr.
  gw_nast-parvw = 'LF'.
  gw_nast-erdat = ekko-aedat.

  PERFORM processing_po USING gw_nast ls_xfz
                      CHANGING ent_retco ent_screen l_druvo.


ENDFORM.


FORM processing_po
                USING iv_nast TYPE nast
                      iv_xfz  TYPE char1
                CHANGING ent_retco
                         ent_screen
                         iv_druvo TYPE t166k-druvo.
  DATA: ls_print_data_to_read TYPE lbbil_print_data_to_read.
  DATA: ls_bil_invoice TYPE lbbil_invoice.
  DATA: lf_fm_name            TYPE rs38l_fnam.
  DATA: ls_control_param      TYPE ssfctrlop.
  DATA: ls_composer_param     TYPE ssfcompop.
  DATA: ls_recipient          TYPE swotobjid.
  DATA: ls_sender             TYPE swotobjid.
  DATA: lf_formname           TYPE tdsfname.
  DATA: ls_addr_key           LIKE addr_key.
  DATA: ls_job_info           TYPE ssfcrescl.
  DATA: l_spoolid             TYPE rspoid.

  DATA: l_from_memory,
        l_doc         TYPE meein_purchase_doc_print,
        l_nast        LIKE nast.
  DATA: neu            VALUE '1',                  "Neudruck
        aend           VALUE '2',                  "Änderungsdruck
        mahn           VALUE '3',                  "Mahnung
        absa           VALUE '4',                  "Absage
        lpet           VALUE '5',                  "Lieferplaneinteilung
        lpma           VALUE '6',                  "Mahnung Lieferplaneinteilung
        aufb           VALUE '7',                  "Auftragsbestätigung
        lpae           VALUE '8',                  "Änderung Lieferplaneinteilung
        lphe           VALUE '9',                  "Historisierte Einteilungen
        preisdruck,                      "Kz. Gesamtpreis drucken
        kontrakt_preis,                  "Kz. Kontraktpreise drucken
        we             VALUE 'E'.                  "Wareneingangswert


  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      ix_nast        = iv_nast
      ix_screen      = ent_screen
    IMPORTING
      ex_retco       = ent_retco
      ex_nast        = l_nast
      doc            = l_doc
    CHANGING
      cx_druvo       = iv_druvo
      cx_from_memory = l_from_memory.
  CHECK ent_retco EQ 0.

  l_nast-aende = iv_nast-aende.

***********************
  DATA lv_nast LIKE nast.
  IF iv_druvo EQ aend OR iv_druvo EQ lpae.
    SELECT datvr uhrvr INTO (lv_nast-datvr, lv_nast-uhrvr) FROM nast
             WHERE kappl EQ iv_nast-kappl
               AND kschl EQ iv_nast-kschl
               AND objky EQ iv_nast-objky
               AND vstat EQ '1'
       ORDER BY datvr DESCENDING uhrvr DESCENDING.
      EXIT.
    ENDSELECT.

    IF sy-subrc NE 0.                                   "Begin of 549924
      DATA: neu_messagetypes LIKE t161m OCCURS 0 WITH HEADER LINE.
      DATA: BEGIN OF time_tab OCCURS 0,
              datvr LIKE nast-datvr,
              uhrvr LIKE nast-uhrvr,
            END OF time_tab.

* Read all messagetypes that are allowed for NEU-Messages
      SELECT * FROM t161m INTO TABLE neu_messagetypes
        WHERE kvewe EQ 'B'
        AND   kappl EQ iv_nast-kappl
        AND   kschl NE iv_nast-kschl
        AND   druvo EQ neu.
      IF sy-subrc EQ 0.
        LOOP AT neu_messagetypes.
          SELECT datvr uhrvr INTO time_tab FROM nast
             WHERE kappl EQ iv_nast-kappl
               AND kschl EQ neu_messagetypes-kschl
               AND objky EQ iv_nast-objky
               AND vstat EQ '1'
             ORDER BY datvr DESCENDING uhrvr DESCENDING.
            EXIT.
          ENDSELECT.
          IF sy-subrc EQ 0.
            APPEND time_tab.
          ENDIF.
        ENDLOOP.
        SORT time_tab BY datvr DESCENDING uhrvr DESCENDING.
        READ TABLE time_tab INDEX 1.
        IF sy-subrc EQ 0.
          lv_nast-datvr = time_tab-datvr.
          lv_nast-uhrvr = time_tab-uhrvr.
        ENDIF.
      ENDIF.
    ENDIF.
    l_nast-datvr = lv_nast-datvr.
    l_nast-uhrvr = lv_nast-uhrvr.
  ENDIF.

* Fill up pricing condition table if calling from ME9F
  IF l_doc-xtkomv IS INITIAL.
    DATA : it_prcd_elements TYPE TABLE OF prcd_elements.
    SELECT * INTO TABLE it_prcd_elements FROM prcd_elements " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
                                     WHERE knumv = l_doc-xekko-knumv.
    MOVE-CORRESPONDING it_prcd_elements TO l_doc-xtkomv.

  ENDIF.
*=======================================================================================================================================
*          Below Codes Added by shanmugam
*=======================================================================================================================================
*  DATA :  IT_PO   TYPE TABLE OF ZMM_PO_STRUC WITH  HEADER LINE.
  DATA :  it_po   TYPE TABLE OF zmm_gst_po_struc WITH  HEADER LINE. "" Added by Bharath
*  DATA :  WA_PO   TYPE  ZMM_PO_STRUC .
  DATA :  wa_po   TYPE  zmm_gst_po_struc . "" Added by bharath
*==================================================
*       Added by Govind ON 03.04.2013
*===================================================
  DATA : wa_ekko TYPE ekko.
  DATA : wa_konv TYPE prcd_elements. " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
  DATA : wa_ekbe TYPE ekbe.
  DATA : wa_rg23d TYPE j_1irg23d.
  DATA : wa_excdtl TYPE j_1iexcdtl.
*====================================================
**  DATA :  it_ekpo TYPE TABLE OF ekpo  WITH HEADER LINE.
  DATA :  wa_konp TYPE konp.
  DATA :  gkwert  TYPE kwert.

  DATA :  dkwert  TYPE kwert.
  DATA :  b_duty  TYPE kwert.
  DATA :  b_duty1 TYPE kwert.
  DATA :  b_duty2 TYPE kwert.
  DATA :  b_duty3 TYPE kwert.
  DATA :  b_duty4 TYPE kwert.
  DATA :  b_duty5 TYPE kwert.
  DATA :  b_duty6 TYPE kwert.
  DATA :  bexdut  TYPE kwert.
  DATA :  gtotal  TYPE kwert.
  DATA :  ntotal TYPE kwert.
  DATA :  gpern   TYPE kwert.
*  DATA :  GPERN1   TYPE KWERT. " Added By Govind
  DATA :  gcalc   TYPE kwert.
  DATA :  vatcst  TYPE kwert.
  DATA :  vkwert  TYPE kwert.
*  DATA :  VKWERT1  TYPE KWERT. " Added By Govind
  DATA :  gnetwr  TYPE netwr.
  DATA :  gkschl  TYPE kschl.
  DATA :  wa_a363 TYPE a363.
  DATA :  wa_a969 TYPE a969.
  DATA : wa_a998 TYPE a998 .
  DATA : wa1_a998 TYPE a998 .
  DATA : wa2_a998 TYPE a998 .
  DATA :  wa_a359 TYPE a359.
  DATA :  wa1_a359 TYPE a359.
  DATA :  wa_makt TYPE makt.
  DATA :  sydat TYPE sy-datum.

  DATA :  b_duty11 TYPE kwert.
  DATA :  b_duty12 TYPE kwert.


  DATA :  wa_a003 TYPE a003.

  DATA :  b_duty7 TYPE kwert.           "ADDED BY MANI
  DATA :  b_duty8 TYPE kwert.           "ADDED BY MANI
  DATA:s1 TYPE kwert .                  "ADDED BY MANI

**************************BBK***********
*DATA: wa_A003 type a003.
****************************************

  it_ekpo[] = l_doc-xekpo[].

  "added on 8/6
  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N' OR sy-tcode EQ 'ME23N' OR sy-tcode EQ 'ME29N' .
    "IF SY-UCOMM EQ 'PREVOUTPUT'.
    IF l_doc-xekko-bsart EQ 'ZNB' OR l_doc-xekko-bsart EQ 'ZSR' OR l_doc-xekko-bsart EQ 'ZCA' OR l_doc-xekko-bsart EQ 'ZSC' OR l_doc-xekko-bsart EQ 'ZMK' OR
      l_doc-xekko-bsart EQ 'ZWK' OR l_doc-xekko-bsart EQ 'ZLP' OR l_doc-xekko-bsart EQ 'ZLPA' OR l_doc-xekko-bsart EQ 'ZDE' OR l_doc-xekko-bsart EQ 'ZVR' OR
      l_doc-xekko-bsart EQ 'YSCA' OR l_doc-xekko-bsart EQ 'YSIM' OR l_doc-xekko-bsart EQ 'YSNB' OR l_doc-xekko-bsart EQ 'YSSC' OR l_doc-xekko-bsart EQ 'YSSR' OR
      l_doc-xekko-bsart EQ 'YSVR' .
      IF l_doc-xekko-procstat NE '05'.
        " CLEAR : L_DOC-XEKPO[] .
        "CLEAR : IT_PO[].
        MESSAGE 'Purchase order not yet released' TYPE 'E' ." DISPLAY LIKE 'I'.
      ENDIF.
    ENDIF.
    " ENDIF.
  ENDIF.

*break-point .
  IF l_doc-xekko-bsart = 'ZIM'.
    CLEAR : gtotal.
    LOOP AT it_ekpo.
*      WA_PO-EBELN = IT_EKPO-EBELN.
      wa_po-uebpo  =  it_ekpo-ebelp.
      wa_po-matnr  =  it_ekpo-matnr.

      CLEAR :  wa_makt.
      "Commented by SPLABAP during code remediation
*      SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE  MATNR =  IT_EKPO-MATNR.
      "Added by SPLABAP during code remediation
      IF it_ekpo-mtart = 'ROH'.

        wa_po-maktx = it_ekpo-txz01.

      ELSE.
        SELECT * FROM makt INTO wa_makt UP TO 1 ROWS WHERE  matnr =  it_ekpo-matnr
        ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc = 0.
          wa_po-maktx = wa_makt-maktx.
        ELSEIF wa_po-matnr IS INITIAL.
          wa_po-maktx = it_ekpo-txz01.
        ENDIF.
      ENDIF.

      wa_po-menge  =  it_ekpo-menge.
      wa_po-meins  =  it_ekpo-meins.
      wa_po-netpr  =  it_ekpo-netpr.
      wa_po-ctot  = it_ekpo-netwr.

*GTOTAL =  GTOTAL  + IT_EKPO-NETWR . Hidded By Govind For Cumulate Amount
      gtotal =  it_ekpo-netwr .
      wa_po-ttamt =  gtotal.

      APPEND wa_po TO it_po.
      CLEAR : wa_po.
    ENDLOOP.


  ELSE.   "IF L_DOC-XEKKO-BSART = 'ZSR'.
*BREAK-POINT .
    DATA :  g_excise1 TYPE kwert.
    DATA :  g_excise2 TYPE kwert.
    DATA :  g_excise3 TYPE kwert.
    DATA :  g_excise4 TYPE kwert.
    DATA :  g_excise5 TYPE kwert.
    DATA :  g_excise6 TYPE kwert.
    DATA :  g_excise7 TYPE kwert. "Added by Govind

    LOOP AT it_ekpo.
      wa_po-uebpo  =  it_ekpo-ebelp.
      wa_po-matnr  =  it_ekpo-matnr.
      CLEAR :  wa_makt.

      IF it_ekpo-mtart = 'ROH'.

        wa_po-maktx = it_ekpo-txz01.

      ELSE.
        SELECT * FROM makt INTO wa_makt UP TO 1 ROWS WHERE  matnr =  it_ekpo-matnr
        ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc = 0.
          wa_po-maktx = wa_makt-maktx.
        ELSEIF wa_po-matnr IS INITIAL.
          wa_po-maktx = it_ekpo-txz01.
        ENDIF.
      ENDIF.

      wa_po-menge  =  it_ekpo-menge.
      wa_po-meins  =  it_ekpo-meins.
      wa_po-netpr  =  it_ekpo-netpr / it_ekpo-peinh.


      CLEAR wa_konv.
      SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln = it_ekpo-ebeln.

      IF sy-subrc = 0.
        SELECT * INTO wa_konv UP TO 1 ROWS FROM prcd_elements WHERE knumv = wa_ekko-knumv AND kposn = it_ekpo-ebelp AND kschl = 'P101'  " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          wa_po-netpr =  wa_konv-kbetr .
        ENDIF.
      ENDIF.

      wa_po-ctot  = it_ekpo-netwr.
      CLEAR wa_konv.
      SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln = it_ekpo-ebeln.

      IF sy-subrc = 0.
        SELECT * INTO wa_konv UP TO 1 ROWS FROM prcd_elements WHERE knumv = wa_ekko-knumv AND kposn = it_ekpo-ebelp AND kschl = 'P101'  " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          wa_po-ctot =  wa_konv-kbetr * wa_po-menge .
        ENDIF.
      ENDIF.

      IF l_doc-xekko-bsart = 'ZVR'.
        wa_po-ctot =  wa_po-ctot * ( -1 ) .
      ENDIF.

      DATA : lv_date LIKE a363-datab.
      DATA : lf_date LIKE a363-datab.
*      BREAK-POINT.
      lv_date = wa_ekko-bedat.
*
***************************************************************************************************************************************************************************
      CLEAR wa_konp.
      CLEAR : wa_a363, gkwert, dkwert, bexdut, g_excise1.
      CLEAR : wa_a003.

************************************Function module implimentaion******************************************************************

      DATA: it_taxcom TYPE taxcom,
            it_komv   TYPE komv OCCURS 20,
            wa_komv   TYPE komv,
            wa_ekko1  TYPE ekko,
            it_ekpo1  TYPE TABLE OF ekpo,
            wa_ekpo1  TYPE ekpo.

      it_taxcom-bukrs = it_ekpo-bukrs.""'1000'."ekpo-bukrs. 1000

      it_taxcom-budat = wa_ekko-aedat.""'20170609'."ekko-aedat. 20170609

      it_taxcom-bldat = wa_ekko-bedat.""'20170609'. "" ekko-bedat. 20170609

      it_taxcom-waers = wa_ekko-waers.""'INR'."'ekko-waers. INR

      it_taxcom-hwaer = wa_ekko-waers.""'INR'."ekko-waers. INR

      it_taxcom-kposn = it_ekpo-ebelp.""'10'.""ekpo-ebelp. 10

      it_taxcom-mwskz = it_ekpo-mwskz..""'Z2'."'Z1'.""ekpo-mwskz. Z1

      "Commented by SPLABAP during code remediation
*     it_taxcom-wrbtr = ( it_EKPO-NETPR / it_ekpo-PEINH ) * it_EKPO-MENGE. ""'1250'.""'500'."EKPO-NETPR * EKPO-MENGE. = 50*10
      it_taxcom-wrbtr = CONV wrbtr( ( it_ekpo-netpr / it_ekpo-peinh ) * it_ekpo-menge ). ""'1250'.""'500'."EKPO-NETPR * EKPO-MENGE. = 50*10
      "Added by SPLABAP during code remediation
      it_taxcom-xmwst = 'X'.

      it_taxcom-txjcd = it_ekpo-txjcd.""' '."ekpo-txjcd. = ?

      it_taxcom-lifnr = wa_ekko-lifnr.."''0010000000'."ekko-lifnr. = 0010000000

      it_taxcom-ekorg = wa_ekko-ekorg.""'1000'."ekko-ekorg. = 1000

      it_taxcom-matnr = it_ekpo-matnr."" '000000000010000000'.""ekpo-matnr. = 000000000010000000

      it_taxcom-werks = it_ekpo-werks.""'1000'."ekpo-werks. = 1000

      it_taxcom-matkl = it_ekpo-matkl.""'RMADD'.""ekpo-matkl. = RMADD

      it_taxcom-meins = it_ekpo-meins.""'KG'." ekpo-meins. = KG

      it_taxcom-mglme = it_ekpo-menge.""'25'."'10' ." ekpo-menge. = 10

      it_taxcom-mtart = it_ekpo-mtart.""'ROH'." ekpo-mtart. = ROH

      it_taxcom-land1 = wa_ekko-lands."" 'IN'.""ekko-lands. = IN


      CALL FUNCTION 'CALCULATE_TAX_ITEM'
        EXPORTING
*         ANZAHLUNG                 = ' '
*         DIALOG   = ' '
*         DISPLAY_ONLY              = ' '
*         INKLUSIVE                 = ' '
*         I_ANWTYP = ' '
*         I_DMBTR  = '0'
*         I_MWSTS  = '0'
          i_taxcom = it_taxcom
*         PRUEFEN  = ' '
*         RESET    = ' '
*         I_KTOSL  = ' '
* IMPORTING
*         E_NAVFW  =
*         E_TAXCOM =
*         E_XSTVR  =
*         NAV_ANTEIL                =
        TABLES
          t_xkomv  = it_komv
* EXCEPTIONS
*         MWSKZ_NOT_DEFINED         = 1
*         MWSKZ_NOT_FOUND           = 2
*         MWSKZ_NOT_VALID           = 3
*         STEUERBETRAG_FALSCH       = 4
*         COUNTRY_NOT_FOUND         = 5
*         TXJCD_NOT_VALID           = 6
*         OTHERS   = 7
        .
*endselect.
*
      DATA gcalc1 TYPE kwert.
      DATA perc_cgst TYPE kbetr.
      DATA perc_sgst TYPE kbetr.
      DATA perc_igst TYPE kbetr.
      CLEAR gcalc1.
      CLEAR b_duty1.
      CLEAR g_excise1.
      CLEAR: perc_cgst, perc_sgst, perc_igst.


      LOOP AT it_komv INTO wa_komv .
        IF wa_komv-kschl = 'JICG'.
          g_excise1 = wa_komv-kwert.
          perc_cgst = wa_komv-kbetr / 10.
        ELSEIF wa_komv-kschl = 'JISG'.
          b_duty1 = wa_komv-kwert.
          perc_sgst = wa_komv-kbetr / 10.
        ELSEIF wa_komv-kschl = 'JIIG'.
          gcalc1 = wa_komv-kwert.
          perc_igst = wa_komv-kbetr / 10.
        ENDIF.
      ENDLOOP.

      CLEAR b_duty.""*************** CGST
      CLEAR bexdut."""""""""""""""""SGST
      CLEAR gcalc. """""""""""""""""IGST
      IF g_excise1 <> 0.   """"" for CGST
        b_duty =   g_excise1.
        wa_po-taxpers_cgst = perc_cgst.
        CLEAR g_excise1.
        CLEAR perc_cgst.
      ENDIF.

      IF b_duty1 <> 0.       """"""""for SGST
        bexdut =   b_duty1.
        wa_po-taxpers_sgst = perc_sgst.
        CLEAR b_duty1.
        CLEAR perc_sgst.
      ENDIF.

      IF gcalc1 <> 0.       """"""""for IGST
        gcalc = gcalc1.
        wa_po-taxpers_igst = perc_igst.
        CLEAR gcalc1.
        CLEAR perc_igst.
      ENDIF.


      wa_po-kwert = b_duty.


      CLEAR: wa_konv,wa_ekbe,wa_rg23d,wa_excdtl.
      SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln = it_ekpo-ebeln.

      IF sy-subrc = 0.
        SELECT * FROM ekbe UP TO 1 ROWS INTO wa_ekbe WHERE ebeln = wa_ekko-ebeln AND ebelp = it_ekpo-ebelp AND bwart = '351'
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          SELECT  SINGLE * INTO wa_konv FROM prcd_elements WHERE knumv = wa_ekko-knumv AND kposn = it_ekpo-ebelp AND kschl = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
          IF sy-subrc = 0.
            SELECT  * FROM j_1irg23d UP TO 1 ROWS INTO wa_rg23d WHERE mblnr = wa_ekbe-belnr AND werks = wa_ekbe-werks
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc = 0.
              SELECT * FROM j_1iexcdtl UP TO 1 ROWS INTO wa_excdtl WHERE exnum = wa_rg23d-exnum AND zeile = wa_rg23d-zeile
                ORDER BY PRIMARY KEY.
              ENDSELECT.
              wa_po-kwert = wa_excdtl-exbed.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR wa_konp.
      CLEAR : wa_konp,gkwert, dkwert , b_duty1, b_duty2,  b_duty3,  b_duty4,  b_duty5, b_duty6.
      CLEAR wa_a003.


      wa_po-service = bexdut.

      IF l_doc-xekko-bsart = 'ZSR'.
        CLEAR : wa_konp,gkwert, dkwert , b_duty1, b_duty2,  b_duty3,  b_duty4,  b_duty5, b_duty6.
        CLEAR : wa_a359 , wa_a969  .
        CLEAR : s1.

        SELECT * INTO wa_a359 FROM a359 UP TO 1 ROWS  WHERE werks = it_ekpo-werks AND matkl = it_ekpo-matkl AND kschl = 'JSB5' AND datbi GE lv_date AND datab LE lv_date
          ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc = 0.
          SELECT  * INTO wa_konp UP TO 1 ROWS FROM konp WHERE knumh =  wa_a359-knumh  " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'.
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            gkwert = gkwert + wa_konp-kbetr.
          ENDIF.
        ENDIF.
        dkwert =  gkwert / 10.
        b_duty7 =   wa_po-ctot / 100  * dkwert.

        CLEAR : wa_konp ,  gkwert,dkwert.
        CLEAR : wa_a359 , wa_a969 .

        "Added by SPLABAP during code remediation
        SELECT  * INTO wa_a359 UP TO 1 ROWS FROM a359 WHERE werks = it_ekpo-werks  AND matkl = it_ekpo-matkl AND kschl = 'JSB6' AND datbi GE lv_date AND datab LE lv_date
          ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc = 0.
          "Added by SPLABAP during code remediation
          SELECT  * INTO wa_konp UP TO 1 ROWS FROM konp WHERE knumh =  wa_a359-knumh  " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'.
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            gkwert = gkwert + wa_konp-kbetr.
          ENDIF.
        ENDIF.
        dkwert =  gkwert / 10.
        b_duty8 =  wa_po-ctot / 100  * dkwert.

        s1 =   s1 + bexdut + b_duty7 + b_duty8.

        wa_po-service = s1 .

      ENDIF.

      CLEAR wa_konp.
      CLEAR wa_a003.


      IF l_doc-xekko-bsart = 'YSNB'.
        SELECT * INTO wa_a003 UP TO 1 ROWS FROM a003 WHERE kschl = 'JVLN' AND mwskz = it_ekpo-mwskz
          ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc = 0.
          SELECT * INTO wa_konp UP TO 1 ROWS FROM konp WHERE knumh =  wa_a003-knumh
            ORDER BY PRIMARY KEY.
          ENDSELECT.

          IF sy-subrc = 0.
            vkwert =  vkwert + wa_konp-kbetr .
            wa_po-kschl =  wa_konp-kschl.

          ENDIF.
        ENDIF.
      ENDIF.

      "ENDED BY RAM ON 16/9/161
      IF l_doc-xekko-bsart = 'ZVR'.
        it_ekpo-netwr = it_ekpo-netwr * ( -1 ) .
      ENDIF.
*      BREAK-POINT.
      CLEAR : gpern, gtotal.
      wa_po-kbetr = gcalc.
*      WA_PO-KBETR1 = GCALC * GPERN1.
      gtotal =  gtotal  + wa_po-kwert  +  wa_po-kbetr +  it_ekpo-netwr + wa_po-service.
      wa_po-ttamt =  gtotal.

      CLEAR : wa_konv,ntotal.
      SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln = it_ekpo-ebeln.

      IF sy-subrc = 0.
        SELECT  SINGLE * INTO wa_konv FROM prcd_elements WHERE knumv = wa_ekko-knumv AND kposn = it_ekpo-ebelp AND kschl = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        IF sy-subrc = 0.
          ntotal =  ntotal +  wa_po-kwert +  wa_po-ctot.
          wa_po-ttamt = ntotal.
        ENDIF.
      ENDIF.

      APPEND wa_po TO it_po.
      CLEAR : wa_po.
    ENDLOOP.
  ENDIF.

  IF it_po[] IS INITIAL.
    MESSAGE 'No line Items Found' TYPE 'I'.
  ENDIF.


*Set the print Parameters
  PERFORM set_print_param USING     ls_addr_key
                          CHANGING  ls_control_param
                                    ls_composer_param
                                    ls_recipient
                                    ls_sender
                                    ent_retco.

  lf_formname = 'ZMM_GST_PO_DOMESTIC'.
* Determine smartform function module for purchase document
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
*  error handling
    ent_retco = sy-subrc.
    IF sy-subrc = 1.
      MESSAGE e001(ssfcomposer).
    ENDIF.


* if it is faxed, changed its title to PO number
    IF ls_control_param-device = 'TELEFAX'.
      ls_composer_param-tdtitle = l_doc-xekko-ebeln.
    ENDIF.

*>>>>> Change of Parameters <<<<<<<<<<<<<<<<<<<<<<<
    CALL FUNCTION lf_fm_name
      EXPORTING
*       ARCHIVE_INDEX    = TOA_DARA
*       archive_parameters = arc_params
*       control_parameters = ls_control_param
        mail_recipient   = ls_recipient
        mail_sender      = ls_sender
        output_options   = ls_composer_param
        is_ekko          = l_doc-xekko
        user_settings    = ' '  "Disable User Printer
        is_pekko         = l_doc-xpekko
        is_nast          = l_nast
        iv_from_mem      = l_from_memory
        iv_druvo         = iv_druvo
        iv_xfz           = iv_xfz
      TABLES
        it_ekpo          = l_doc-xekpo[]
        it_ekpa          = l_doc-xekpa[]
        it_pekpo         = l_doc-xpekpo[]
        it_eket          = l_doc-xeket[]
        it_tkomv         = l_doc-xtkomv[]
        it_ekkn          = l_doc-xekkn[]
        it_ekek          = l_doc-xekek[]
*       IT_KOMK          = L_XKOMK[]
        it_tab           = it_po[]
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
*
  ENDIF.

  it_ekpo[] = l_doc-xekpo[].


  is_ekko = l_doc-xekko.
  is_pekko = l_doc-xpekko.
  is_nast = l_nast .
**  it_ekpo = l_doc-xekpo[].
  it_ekpa[] = l_doc-xekpa[].
  it_pekpo[] = l_doc-xpekpo[].
  it_eket[] = l_doc-xeket[].
  it_tkomv[] = l_doc-xtkomv[].
  it_ekkn[] = l_doc-xekkn[].
  it_ekek[] = l_doc-xekek[].
  it_komk[] = l_xkomk[].
  it_tab[] =  it_po[].



ENDFORM.                    "PROCESSING_PO

*&---------------------------------------------------------------------*

FORM set_print_param USING    is_addr_key LIKE addr_key
                     CHANGING cs_control_param TYPE ssfctrlop
                              cs_composer_param TYPE ssfcompop
                              cs_recipient TYPE  swotobjid
                              cs_sender TYPE  swotobjid
                              cf_retcode TYPE sy-subrc.

  DATA: ls_itcpo     TYPE itcpo.
  DATA: lf_repid     TYPE sy-repid.
  DATA: lf_device    TYPE tddevice.
  DATA: ls_recipient TYPE swotobjid.
  DATA: ls_sender    TYPE swotobjid.

  lf_repid = sy-repid.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      pi_nast       = gw_nast
      pi_addr_key   = is_addr_key
      pi_repid      = lf_repid
    IMPORTING
      pe_returncode = cf_retcode
      pe_itcpo      = ls_itcpo
      pe_device     = lf_device
      pe_recipient  = cs_recipient
      pe_sender     = cs_sender.

  IF cf_retcode = 0.
    MOVE-CORRESPONDING ls_itcpo TO cs_composer_param.
*    cs_composer_param-tdnoprint = 'X'.                     "Note 591576
    cs_control_param-device      = lf_device.
    cs_control_param-no_dialog   = 'X'.
*    CS_CONTROL_PARAM-PREVIEW     = XSCREEN.
    cs_control_param-getotf      = ls_itcpo-tdgetotf.
    cs_control_param-langu       = gw_nast-spras.
  ENDIF.
ENDFORM.                    "set_print_param

*&---------------------------------------------------------------------*
FORM form .


  CLEAR: ls_ekko, ls_ekpo,l_devtype.

  control-getotf        = abap_true.
  control-no_dialog     = abap_true.
  control-langu         = sy-langu.
  gw_ssfcompop-tdnoprev = abap_true.

*** Smartform
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZMM_GST_PO_DOMESTIC'
    IMPORTING
      fm_name            = v_fname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE 'No selection for Smartforms' TYPE 'I'.
  ENDIF.


  CALL FUNCTION v_fname
    EXPORTING
      control_parameters = control      "wa_ctrl
      output_options     = gw_ssfcompop "wa_ctrl_outopt
      user_settings      = 'X'
      is_ekko            = is_ekko
      is_pekko           = is_pekko
      is_nast            = is_nast
      iv_from_mem        = iv_from_mem
      iv_druvo           = iv_druvo
    IMPORTING
      job_output_info    = wa_return
      job_output_options = gs_job_output_options  "WA_RETURN
    TABLES
      it_ekpo            = it_ekpo
      it_ekpa            = it_ekpa
      it_pekpo           = it_pekpo
      it_eket            = it_eket
      it_tkomv           = it_tkomv
      it_ekkn            = it_ekkn
      it_ekek            = it_ekek
      it_komk            = it_komk
      it_tab             = it_tab.


ENDFORM.
*&---------------------------------------------------------------------*
FORM convert_to_pdf .

*/.. Get OTF data to convert to PDF
  REFRESH li_otf[].
  li_otf[] = wa_return-otfdata[].
  CLEAR : lv_bin_filesize.

*/.. Convert OTF data to binary
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format       = 'PDF'
    IMPORTING
      bin_filesize = lv_bin_filesize
      bin_file     = i_xstring
***      bin_file     = pdf_bin
    TABLES
      otf          = li_otf
      lines        = li_pdf_tab.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = i_xstring
    TABLES
      binary_tab = i_objbin[].

ENDFORM.

FORM send_email.
  DATA: lv_adrnr TYPE ad_addrnum.
  DATA: it_mailid1 TYPE TABLE OF ad_smtpadr.
  DATA: lv_venid(04) TYPE c.
  DATA: lv_venmail TYPE ad_smtpadr.
  DATA: lv_content TYPE string.
  DATA: lv_tot1 TYPE char50.

  REFRESH: lt_message_body.
  CONCATENATE 'Purchase order:' gs_ekko-ebeln INTO lv_tot1 SEPARATED BY space.
  lv_tot     = lv_tot1.
  "create send request
  lo_send_request = cl_bcs=>create_persistent( ).
  "create message body and subject
  salutation ='Dear Sir/Madam,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  body = 'Please find attached Purchase Order for your reference'.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
*  body = 'If you have any queries / doubts, kindly send us a mail to accountsreply@sheenlac.in'.
*  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  footer = 'Thanks & Best Regards,'.
  APPEND footer TO lt_message_body.
  footer = 'Sheenlac Paints Ltd'.
  APPEND footer TO lt_message_body.
  "put your text into the document
  lo_document = cl_document_bcs=>create_document(
  i_type = 'RAW'
  i_text = lt_message_body
  i_subject = lv_tot ).

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'PDF'
      i_attachment_subject = lv_tot1
      i_att_content_hex = i_objbin[] ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.
* Add attachment
* Pass the document to send request
  lo_send_request->set_document( lo_document ).
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc1) WHERE name = 'ZDEBIT_MEMO'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc1-low .
  lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ). "Sender Mail ID
  lo_send_request->set_sender( lo_sender ).

** Vendor Mail ID **
  CLEAR in_mailid.
  in_mailid = gs_logs-ven_email.
  "Create recipient to Mail ID
  lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
*set recipient
  lo_send_request->add_recipient(
     EXPORTING
     i_recipient = lo_recipient
      i_express = abap_true
     ).
* Send email
  lo_send_request->send(
  EXPORTING
  i_with_error_screen = abap_true
  RECEIVING
  result = lv_sent_to_all ).

  COMMIT WORK.
  CLEAR : in_mailid,in_mailid1,in_mailid2,in_mailid3.

** Alv Display Message Updation **
  CLEAR gs_alv.
  gs_alv-po_num = gs_logs-ebeln.
  gs_alv-ven_id = gs_logs-vendor_id.
  gs_alv-venmail = gs_logs-ven_email.
  gs_alv-msg = 'Mail Sent Successfully'.
  APPEND gs_alv TO gt_alv.

** Updating log table with flag and Remarks **
  "Locking the Table
  CALL FUNCTION 'ENQUEUE_EZZPOEMAIL_LOG'
    EXPORTING
      mode_zpo_email_log = 'E'
      mandt              = sy-mandt
      ebeln              = gs_logs-ebeln
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.

    UPDATE zpo_email_log SET email_flag = 'X'
                             mail_date = sy-datum
                             mail_time = sy-uzeit
                             remarks = 'Mail Sent'
                             WHERE ebeln = gs_logs-ebeln.

  ENDIF.
  "Unlocking the Table
  CALL FUNCTION 'DEQUEUE_EZZPOEMAIL_LOG'
    EXPORTING
      mode_zpo_email_log = 'E'
      mandt              = sy-mandt
      ebeln              = gs_logs-ebeln.

ENDFORM.
** ALV Display **
FORM alv_display.
  REFRESH: gt_fcat.

  gs_layout-colwidth_optimize = 'X'.
  PERFORM f_fieldcat USING 'PO_NUM'  'PO Number'   1 space.
  PERFORM f_fieldcat USING 'VEN_ID'  'Vendor ID'   2 space.
  PERFORM f_fieldcat USING 'VENMAIL' 'Vendor Mail' 3 space.
  PERFORM f_fieldcat USING 'MSG'     'Message'     5 space.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = gs_layout
      it_fieldcat   = gt_fcat[]
    TABLES
      t_outtab      = gt_alv[]
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  CLEAR gs_fcat.
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_l = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
