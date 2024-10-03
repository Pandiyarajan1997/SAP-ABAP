*&---------------------------------------------------------------------*
*& Report  ZNEW_GST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT znew_gst_purchase.

TYPE-POOLS:slis.

************** Table Used in Program*****************
TABLES: ekko,ekpo,lfa1,rbkp,rseg,bseg,bkpf,bset,makt,t007s,t001w,with_item.

TYPES: BEGIN OF ty_ekko,
         ebeln TYPE ekko-ebeln,  "purchase Number
         bsart TYPE ekko-bsart,  "Document Type
         lifnr TYPE ekko-lifnr,  "Vendor Code
       END OF ty_ekko.

DATA: it_ekko TYPE TABLE OF ty_ekko,
      wa_ekko TYPE ty_ekko.

TYPES: BEGIN OF ty1_ekko,
         ebeln TYPE ekko-ebeln,  "purchase Number
         bsart TYPE ekko-bsart,  "Document Type
         lifnr TYPE ekko-lifnr,  "Vendor Code
       END OF ty1_ekko.

DATA: it1_ekko TYPE TABLE OF ty1_ekko,
      wa1_ekko TYPE ty1_ekko.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr, "vendor code
         name1 TYPE lfa1-name1, "Vendor Name
         regio TYPE lfa1-regio,  "Division
         stcd3 TYPE lfa1-stcd3, "GST IN Of Vendor
       END OF ty_lfa1.

DATA: it_lfa1 TYPE TABLE OF ty_lfa1,
      wa_lfa1 TYPE ty_lfa1.

TYPES: BEGIN OF ty1_lfa1,
         lifnr TYPE lfa1-lifnr, "vendor code
         name1 TYPE lfa1-name1, "Vendor Name
         regio TYPE lfa1-regio,  "Division
         stcd3 TYPE lfa1-stcd3, "GST IN Of Vendor
       END OF ty1_lfa1.

DATA: it1_lfa1 TYPE TABLE OF ty1_lfa1,
      wa1_lfa1 TYPE ty1_lfa1.

TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ekpo-ebeln, ""Purchasing Doc.
         ebelp TYPE ekpo-ebelp, "Item
         matnr TYPE ekpo-matnr, "Material
       END OF ty_ekpo.

DATA: it_ekpo TYPE TABLE OF ty_ekpo,
      wa_ekpo TYPE ty_ekpo.

TYPES: BEGIN OF ty1_ekpo,
         ebeln TYPE ekpo-ebeln, ""Purchasing Doc.
         ebelp TYPE ekpo-ebelp, "Item
         matnr TYPE ekpo-matnr, "Material
       END OF ty1_ekpo.

DATA: it1_ekpo TYPE TABLE OF ty1_ekpo,
      wa1_ekpo TYPE ty1_ekpo.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr, " Material
         maktx TYPE makt-maktx, "Description
       END OF ty_makt.
DATA : it_makt TYPE TABLE OF  ty_makt,
       wa_makt TYPE ty_makt.

TYPES: BEGIN OF ty1_makt,
         matnr TYPE makt-matnr, " Material
         maktx TYPE makt-maktx, "Description
       END OF ty1_makt.
DATA : it1_makt TYPE TABLE OF  ty1_makt,
       wa1_makt TYPE ty1_makt.

TYPES: BEGIN OF ty_t007s,
         mwskz TYPE t007s-mwskz, "Tax code
         text1 TYPE t007s-text1, "Description
       END OF ty_t007s.

DATA:it_t007s TYPE TABLE OF ty_t007s,
     wa_t007s TYPE ty_t007s.

TYPES: BEGIN OF ty1_t007s,
         mwskz TYPE t007s-mwskz, "Tax code
         text1 TYPE t007s-text1, "Description
       END OF ty1_t007s.

DATA:it1_t007s TYPE TABLE OF ty1_t007s,
     wa1_t007s TYPE ty1_t007s.

TYPES: BEGIN OF ty_t001w, " ******************  Plants/Branches*******************
         werks TYPE t001w-werks, "Plant
         regio TYPE t001w-regio, "Division
       END OF ty_t001w.

DATA:it_t001w TYPE TABLE OF ty_t001w,
     wa_t001w TYPE ty_t001w.

TYPES: BEGIN OF ty1_t001w, " ******************  Plants/Branches*******************
         werks TYPE t001w-werks, "Plant
         regio TYPE t001w-regio, "Division
       END OF ty1_t001w.

DATA:it1_t001w TYPE TABLE OF ty1_t001w,
     wa1_t001w TYPE ty1_t001w.

DATA: months TYPE t247-ltx.
DATA: months1 TYPE t247-ltx.

TYPES: BEGIN OF ty_bseg,
         bukrs   TYPE bseg-bukrs,
         belnr   TYPE bseg-belnr,
         gjahr   TYPE bseg-gjahr,
         buzei   TYPE bseg-buzei,
         buzid   TYPE bseg-buzid,
         bschl   TYPE bseg-bschl,
         mwskz   TYPE bseg-mwskz,
         wrbtr   TYPE bseg-wrbtr,
         hkont   TYPE bseg-hkont,
         lifnr   TYPE bseg-lifnr,
         matnr   TYPE bseg-matnr,
         werks   TYPE bseg-werks,
         menge   TYPE bseg-menge,
         meins   TYPE bseg-meins,
         ebeln   TYPE bseg-ebeln,
         ebelp   TYPE bseg-ebelp,
         hsn_sac TYPE bseg-hsn_sac,
         tcode   TYPE bkpf-tcode,
         taxps   TYPE bseg-taxps,
         shkzg   TYPE bseg-shkzg,
       END OF ty_bseg.

DATA: it_bseg TYPE TABLE OF ty_bseg,
      wa_bseg TYPE ty_bseg.

TYPES: BEGIN OF ty_bseg1,
         bukrs   TYPE bseg-bukrs,
         belnr   TYPE bseg-belnr,
         gjahr   TYPE bseg-gjahr,
         buzei   TYPE bseg-buzei,
         buzid   TYPE bseg-buzid,
         bschl   TYPE bseg-bschl,
         mwskz   TYPE bseg-mwskz,
         wrbtr   TYPE bseg-wrbtr,
         hkont   TYPE bseg-hkont,
         lifnr   TYPE bseg-lifnr,
         matnr   TYPE bseg-matnr,
         werks   TYPE bseg-werks,
         menge   TYPE bseg-menge,
         meins   TYPE bseg-meins,
         ebeln   TYPE bseg-ebeln,
         ebelp   TYPE bseg-ebelp,
         hsn_sac TYPE bseg-hsn_sac,
         taxps   TYPE bseg-taxps,
         shkzg   TYPE bseg-shkzg,
         ktosl   TYPE bseg-ktosl,
         tcode   TYPE bkpf-tcode,
       END OF ty_bseg1.

DATA: it_bseg1 TYPE TABLE OF ty_bseg1,
      wa_bseg1 TYPE ty_bseg1.

DATA : it_bseg2 TYPE TABLE OF ty_bseg1,
       wa_bseg2 TYPE ty_bseg1.
DATA : it_bseg3 TYPE TABLE OF ty_bseg1,
       wa_bseg3 TYPE ty_bseg1.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         bldat TYPE bkpf-bldat,
         budat TYPE bkpf-budat,
         tcode TYPE bkpf-tcode,
         xblnr TYPE bkpf-xblnr,
         waers TYPE bkpf-waers,
         kursf TYPE bkpf-kursf, "Exchange rate
         awkey TYPE bkpf-awkey,
       END OF ty_bkpf.

DATA: it_bkpf TYPE TABLE OF ty_bkpf,
      wa_bkpf TYPE ty_bkpf.

DATA: it_bkpf2 TYPE TABLE OF ty_bkpf,
      wa_bkpf2 TYPE ty_bkpf.

TYPES: BEGIN OF ty_bkpf1,
         bukrs        TYPE bkpf-bukrs,
         belnr        TYPE bkpf-belnr,
         gjahr        TYPE bkpf-gjahr,
         bldat        TYPE bkpf-bldat,
         budat        TYPE bkpf-budat,
         tcode        TYPE bkpf-tcode,
         xblnr        TYPE bkpf-xblnr,
         waers        TYPE bkpf-waers,
         kursf        TYPE bkpf-kursf, "Exchange rate
         docno        TYPE bkpf-belnr,
         docyr        TYPE bkpf-gjahr,
         lifnr        TYPE bseg-lifnr,
         prepay_awkey TYPE rbkp-prepay_awkey,
       END OF ty_bkpf1.

DATA: it_bkpf1 TYPE TABLE OF ty_bkpf1,
      wa_bkpf1 TYPE ty_bkpf1.

DATA: it_rbkp1 TYPE TABLE OF ty_bkpf1,
      wa_rbkp1 TYPE ty_bkpf1.

TYPES: BEGIN OF ty_bset,
         bukrs TYPE bset-bukrs,
         belnr TYPE bset-belnr,
         gjahr TYPE bset-gjahr,
         buzei TYPE bset-buzei,
         shkzg TYPE bset-shkzg,
         hwbas TYPE bset-hwbas,
         fwste TYPE bset-fwste,
         kschl TYPE bset-kschl,
         kbetr TYPE bset-kbetr,
         taxps TYPE bset-taxps,
       END OF ty_bset.

DATA:it_bset TYPE TABLE OF ty_bset,
     wa_bset TYPE ty_bset.

TYPES: BEGIN OF ty_bset1,
         bukrs TYPE bset-bukrs,
         belnr TYPE bset-belnr,
         gjahr TYPE bset-gjahr,
         buzei TYPE bset-buzei,
         hwbas TYPE bset-hwbas,
         fwste TYPE bset-fwste,
         kschl TYPE bset-kschl,
         kbetr TYPE bset-kbetr,
         taxps TYPE bset-taxps,
       END OF ty_bset1.

DATA:it_bset1 TYPE TABLE OF ty_bset1,
     wa_bset1 TYPE ty_bset1.

TYPES: BEGIN OF ty_tds,
         bukrs     TYPE with_item-bukrs,
         belnr     TYPE with_item-belnr,
         gjahr     TYPE with_item-gjahr,
         wt_qbshh  TYPE with_item-wt_qbshh,
         j_1ibuzei TYPE with_item-j_1ibuzei,
       END OF ty_tds.

DATA :it_tds TYPE TABLE OF ty_tds,
      wa_tds TYPE ty_tds.

TYPES: BEGIN OF ty_skat,
         ktopl TYPE skat-ktopl,
         saknr TYPE skat-saknr,
         txt20 TYPE skat-txt20,
       END OF ty_skat.

DATA: it_skat TYPE TABLE OF ty_skat,
      wa_skat TYPE ty_skat.

TYPES: BEGIN OF ty_skat1,
         ktopl TYPE skat-ktopl,
         saknr TYPE skat-saknr,
         txt20 TYPE skat-txt20,
       END OF ty_skat1.

DATA: it_skat1 TYPE TABLE OF ty_skat1,
      wa_skat1 TYPE ty_skat1.


TYPES: BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-matnr,
         steuc TYPE marc-steuc,
       END OF ty_marc.
DATA: it_marc TYPE TABLE OF ty_marc,
      wa_marc TYPE ty_marc.

TYPES: BEGIN OF ty_marc1,
         matnr TYPE marc-matnr,
         werks TYPE marc-matnr,
         steuc TYPE marc-steuc,
       END OF ty_marc1.

DATA: it_marc1 TYPE TABLE OF ty_marc1,
      wa_marc1 TYPE ty_marc1.

TYPES:BEGIN OF ty_final1,
        bukrs         TYPE bseg-bukrs,
        belnr         TYPE bseg-belnr,
        gjahr         TYPE bseg-gjahr,
        buzei         TYPE bseg-buzei,
        bschl         TYPE bseg-bschl,
        mwskz         TYPE bseg-mwskz,
        wrbtr         TYPE bseg-wrbtr,
        wrbtr_h       TYPE bseg-wrbtr,
        wrbtr_s       TYPE bseg-wrbtr,
        hkont         TYPE bseg-hkont,
        seg_hkont     TYPE bseg-hkont,
        lifnr         TYPE bseg-lifnr,
        lv_lifnr      TYPE bseg-lifnr,
        matnr         TYPE bseg-matnr,
        werks         TYPE bseg-werks,
        menge         TYPE bseg-menge,
        meins         TYPE bseg-meins,
        ebeln         TYPE bseg-ebeln,
        ebelp         TYPE bseg-ebelp,
        hsn_sac       TYPE bseg-hsn_sac,
        taxps         TYPE bseg-taxps,
        shkzg         TYPE bseg-shkzg,

        bldat         TYPE bkpf-bldat,
        budat         TYPE bkpf-budat,
        tcode         TYPE bkpf-tcode,
        xblnr         TYPE bkpf-xblnr,
        waers         TYPE bkpf-waers,
        kursf         TYPE bkpf-kursf, "Exchange rate
        awkey         TYPE bkpf-awkey,

        hwbas         TYPE bset-hwbas,
        hwbas_gst     TYPE bset-hwbas,
        hwbas_tcs     TYPE bset-hwbas,
        fwste         TYPE bset-fwste,
        kschl         TYPE bset-kschl,
        kbetr         TYPE bset-kbetr,

        ktopl         TYPE skat-ktopl,
        saknr         TYPE skat-saknr,
        txt20         TYPE skat-txt20,

        months        TYPE t247-ltx, " month in words

        po_ebeln      TYPE ekko-ebeln,
        bsart         TYPE ekko-bsart,  "Document Type\
        po_lifnr      TYPE ekko-lifnr,

        maktx         TYPE makt-maktx,

*       VEN_LIFNR TYPE LFA1-LIFNR,
        name1         TYPE lfa1-name1,
        regio         TYPE lfa1-regio,
        stcd3         TYPE  lfa1-stcd3,

        text1         TYPE t007s-text1,

        bran_regio    TYPE lfa1-regio,

        docno         TYPE bkpf-belnr,
        docyr         TYPE bkpf-gjahr,

        val_sgst      TYPE bset-fwste,
        val_igst      TYPE bset-fwste,
        val_cgst      TYPE bset-fwste,
        val_ptcs(15)  TYPE p DECIMALS 3,
        val_total(15) TYPE p DECIMALS 3,

        per_igst      TYPE bset-kbetr,
        per_sgst      TYPE bset-kbetr,
        per_cgst      TYPE bset-kbetr,
        per_ptcs(15)  TYPE p DECIMALS 3,
        tot_gst(15)   TYPE p DECIMALS 3,
        tds           TYPE with_item-wt_qsshh,
        bset_taxps    TYPE bset-taxps,

        mar_werks     TYPE marc-werks,
        steuc         TYPE marc-steuc,

        t00_werks     TYPE t001w-werks, "Plant
        t00_regio     TYPE t001w-regio, "Division

        pur_ret       TYPE char30,
        gst_ret       TYPE char4,
        tax_dec       TYPE char20,
      END OF ty_final1.

DATA:it_final1 TYPE TABLE OF ty_final1,
     it_final  TYPE TABLE OF ty_final1,
     wa_final  TYPE ty_final1,
     wa_final1 TYPE ty_final1.

DATA:lv_lifnr TYPE bseg-lifnr.

************** Selection Screen Starts*****************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:s_bukrs FOR bseg-bukrs OBLIGATORY.                                "Vendor Code
*SELECT-OPTIONS:S_WERKS FOR BSEG-WERKS.   "Plant
  SELECT-OPTIONS:s_budat FOR bkpf-budat.  "Posting Date
*  SELECT-OPTIONS:s_belnr FOR bkpf-belnr.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND uc1 MODIF ID tb2,
               r2 RADIOBUTTON GROUP g1 MODIF ID tb2 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK a1.
************** Selection SCreen End*****************

START-OF-SELECTION.
  IF r1 = 'X'.
    PERFORM get_data_fv60.
    PERFORM read_data_fv60.
  ELSEIF r2 = 'X'.
    PERFORM get_data_miro.
    PERFORM read_data_miro.
  ENDIF.

END-OF-SELECTION.

************** ALv Declaraton starts *****************
  DATA: it_fieldcat TYPE  slis_t_fieldcat_alv,
        wa_fieldcat TYPE slis_fieldcat_alv,
        wa_layout   TYPE slis_layout_alv,
        it_repid    TYPE sy-repid VALUE sy-repid.

  PERFORM fieldcat.
  PERFORM layout.
  PERFORM display.

************** ALv Declaraton ends *****************

************** Start Of Selection Screen Ends *****************
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_miro .

  SELECT
    bukrs belnr gjahr
    bldat budat tcode
    xblnr waers kursf awkey
    FROM bkpf INTO TABLE it_bkpf
       WHERE budat IN s_budat  AND " belnr IN s_belnr AND
             tcode IN ( 'MIRO','MR8M','J_1IG_INV' ) AND "
             blart IN ( 'KG','KR','RE' ).

  IF it_bkpf IS NOT INITIAL.
    SELECT bukrs belnr gjahr buzei buzid bschl mwskz wrbtr hkont lifnr matnr werks menge meins ebeln ebelp hsn_sac taxps shkzg ktosl FROM bseg "#EC CI_DB_OPERATION_OK[2431747]"Added by SPLABAP during code remediation
      INTO TABLE it_bseg1
      FOR ALL ENTRIES IN it_bkpf
       WHERE bukrs = it_bkpf-bukrs
       AND belnr = it_bkpf-belnr
       AND gjahr = it_bkpf-gjahr
       AND bukrs IN s_bukrs.
  ENDIF.

  SELECT ebeln bsart lifnr FROM ekko
    INTO TABLE it_ekko
    FOR ALL ENTRIES IN it_bseg1
    WHERE ebeln = it_bseg1-ebeln.

  SELECT matnr maktx  FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_bseg1
     WHERE matnr = it_bseg1-matnr.

  SELECT matnr werks steuc FROM marc
    INTO TABLE it_marc
    FOR ALL ENTRIES IN it_bseg1
     WHERE matnr = it_bseg1-matnr.

  SELECT lifnr name1 regio stcd3 FROM lfa1
      INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_ekko
    WHERE lifnr = it_ekko-lifnr.

  SELECT lifnr name1 regio stcd3 FROM lfa1
       APPENDING TABLE it_lfa1
     FOR ALL ENTRIES IN it_bseg1
     WHERE lifnr = it_bseg1-lifnr.


  SELECT mwskz text1 FROM t007s "#EC CI_NOORDER  "Added by SPLABAP during code remediation
    INTO TABLE it_t007s
    FOR ALL ENTRIES IN it_bseg1
    WHERE mwskz = it_bseg1-mwskz
     AND spras EQ 'EN'
    AND kalsm EQ 'TAXINN' .

  SELECT  werks regio FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_bseg1
    WHERE werks = it_bseg1-werks.

  SELECT bukrs belnr gjahr wt_qbshh j_1ibuzei FROM with_item INTO TABLE it_tds FOR ALL ENTRIES IN it_bseg1 WHERE bukrs = it_bseg1-bukrs
   AND belnr = it_bseg1-belnr
   AND gjahr = it_bseg1-gjahr
    AND j_1ibuzei = it_bseg1-buzei.

  SELECT bukrs belnr gjahr buzei shkzg hwbas fwste kschl kbetr taxps FROM bset "#EC CI_NOORDER  "Added by SPLABAP during code remediation
    INTO TABLE it_bset
    FOR ALL ENTRIES IN it_bseg1
    WHERE belnr = it_bseg1-belnr
    AND   gjahr = it_bseg1-gjahr
    AND   taxps = it_bseg1-taxps.

  SELECT   ktopl saknr txt20  FROM skat
    INTO TABLE it_skat
    FOR ALL ENTRIES IN it_bseg1
    WHERE ktopl = 'YAIN'
    AND saknr = it_bseg1-hkont.

  APPEND LINES OF it_bseg1 TO it_bseg2.

  DELETE it_bseg2 WHERE lifnr EQ ' ' .
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_miro .

  SORT it_bkpf BY belnr gjahr.

  LOOP AT it_bseg1 INTO wa_bseg1.
    wa_bseg-belnr = wa_bseg1-belnr.
    wa_bseg-gjahr = wa_bseg1-gjahr.
    wa_bseg-buzid = wa_bseg1-buzid.
    wa_bseg-bschl = wa_bseg1-bschl.
    wa_bseg-mwskz = wa_bseg1-mwskz.
    wa_bseg-wrbtr = wa_bseg1-wrbtr.
    wa_bseg-lifnr = wa_bseg1-lifnr.

    IF wa_bseg-lifnr IS INITIAL.
      READ TABLE it_bseg2 INTO wa_bseg2 WITH KEY belnr = wa_bseg-belnr
                                                 gjahr = wa_bseg-gjahr .
      wa_bseg-lifnr = wa_bseg2-lifnr.

    ENDIF.

    wa_bseg-hkont = wa_bseg1-hkont.
    wa_bseg-matnr = wa_bseg1-matnr.
    wa_bseg-werks = wa_bseg1-werks.
    wa_bseg-menge = wa_bseg1-menge.
    wa_bseg-meins = wa_bseg1-meins.
    wa_bseg-ebeln = wa_bseg1-ebeln.
    wa_bseg-ebelp = wa_bseg1-ebelp.
    wa_bseg-hsn_sac = wa_bseg1-hsn_sac.
    wa_bseg-bukrs = wa_bseg1-bukrs.
    wa_bseg-taxps = wa_bseg1-taxps.
    wa_bseg-shkzg = wa_bseg1-shkzg.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bseg-belnr
                                             gjahr = wa_bseg-gjahr BINARY SEARCH.
    IF sy-subrc EQ 0 .
      wa_bseg-tcode = wa_bkpf-tcode.
    ENDIF.
    APPEND wa_bseg TO it_bseg.

  ENDLOOP.

  DELETE it_bseg WHERE tcode EQ 'MIRO' AND matnr EQ ' ' AND ebeln EQ ' ' .        " EBELN IS ADDED BY JESTOP ON 22.12.2020
  DELETE it_bseg WHERE tcode EQ 'MR8M' AND matnr EQ ' ' AND ebeln EQ ' ' .        " EBELN IS ADDED BY JESTOP ON 22.12.2020

  SORT it_bkpf BY belnr gjahr.
  SORT  it_marc BY matnr werks.
  SORT it_ekko BY ebeln .
  SORT it_makt BY matnr.
  SORT it_lfa1 BY lifnr.
  SORT it_t007s BY mwskz.
  SORT it_t001w BY werks.
  SORT it_skat BY saknr.
  SORT  it_bseg DESCENDING BY belnr taxps.
  SORT  it_bseg1 DESCENDING BY belnr taxps.
  DELETE it_bseg1 WHERE ktosl NE 'JIS' AND ktosl NE 'JIC' AND ktosl NE 'JII' AND ktosl NE 'JIM' AND ktosl NE 'JIU'.
  DELETE ADJACENT DUPLICATES FROM it_bseg1 COMPARING belnr wrbtr taxps.

  LOOP AT it_bseg INTO wa_bseg .

    wa_final1-belnr = wa_bseg-belnr.
    wa_final1-gjahr = wa_bseg-gjahr.
    wa_final1-mwskz = wa_bseg-mwskz.
    wa_final1-wrbtr = wa_bseg-wrbtr.
    wa_final1-shkzg = wa_bseg-shkzg.

    IF wa_bseg-shkzg = 'H'.
      wa_final1-wrbtr_h = wa_final1-wrbtr_h + wa_bseg-wrbtr.
    ELSEIF wa_bseg-shkzg = 'S'.
      wa_final1-wrbtr_s = wa_final1-wrbtr_s + wa_bseg-wrbtr.
    ENDIF.


    IF wa_bseg-tcode NE 'MIRO'  OR wa_bseg-tcode NE 'MR8M' OR wa_bseg-tcode NE 'FBVB'.
      wa_final1-lifnr = wa_bseg-lifnr.
      IF wa_bseg-lifnr = ' ' .
        wa_final1-lifnr = wa_bseg-hkont. "WA_BSEG-HKONT.
      ELSEIF
        wa_final1-lifnr = wa_bseg-lifnr.
      ENDIF.

    ENDIF.

    wa_final1-matnr = wa_bseg-matnr.
    wa_final1-werks = wa_bseg-werks.
    wa_final1-menge = wa_bseg-menge.
    wa_final1-meins = wa_bseg-meins.
    wa_final1-ebeln = wa_bseg-ebeln.
    wa_final1-ebelp = wa_bseg-ebelp.
    wa_final1-hsn_sac = wa_bseg-hsn_sac.
    wa_final1-bukrs = wa_bseg-bukrs.
    wa_final1-taxps = wa_bseg-taxps.

    IF wa_bseg-bschl EQ '89'.
      wa_final1-seg_hkont = wa_bseg-hkont.
    ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bseg-belnr
                                             gjahr = wa_bseg-gjahr BINARY SEARCH.
    IF wa_final1-mwskz = 'R*'.
      wa_final1-tax_dec = 'RCM '.
    ELSEIF wa_final1-mwskz NE 'R*' .
      wa_final1-tax_dec = 'TAXABLE – GST'.
    ENDIF.

    IF sy-subrc = 0.
      wa_final1-belnr = wa_bkpf-belnr.
      wa_final1-gjahr = wa_bkpf-gjahr.
      wa_final1-bldat = wa_bkpf-bldat.
      wa_final1-budat = wa_bkpf-budat.
      wa_final1-tcode = wa_bkpf-tcode.
      wa_final1-xblnr = wa_bkpf-xblnr.
      wa_final1-waers = wa_bkpf-waers.
      wa_final1-kursf = wa_bkpf-kursf.
      wa_final1-awkey = wa_bkpf-awkey.

**      CALL FUNCTION 'ISP_GET_MONTH_NAME'
**        EXPORTING
**          DATE         = WA_FINAL1-BLDAT
**          LANGUAGE     = SY-LANGU
***         MONTH_NUMBER = '00'
**        IMPORTING
***         LANGU_BACK   =
**          LONGTEXT     = WA_FINAL1-MONTHS
***         SHORTTEXT    =
**        EXCEPTIONS
**          CALENDAR_ID  = 1
**          DATE_ERROR   = 2
**          NOT_FOUND    = 3
**          WRONG_INPUT  = 4
**          OTHERS       = 5.
**      IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**      ENDIF.

      SELECT SINGLE ltx FROM t247
         INTO wa_final1-months
         WHERE spras EQ sy-langu AND mnr EQ wa_final1-bldat+4(2).

    ENDIF.

    IF wa_final1-tcode EQ 'MIRO' .
      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = 10
          text   = wa_final1-awkey
       "  AS_CHARACTER       =
        IMPORTING
          line   = wa_final1-docno
          rest   = wa_final1-docyr.
    ELSE.
      wa_final1-docno = wa_final1-belnr.
    ENDIF.
    IF wa_final1-tcode EQ 'MIRO' .
      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_bseg-ebeln . "BINARY SEARCH.

      wa_final1-lifnr = wa_ekko-lifnr. "WA_BSEG-HKONT.


      IF sy-subrc = 0.
        wa_final1-bsart = wa_ekko-bsart.
        IF wa_ekko-bsart EQ 'ZVR' .
          wa_final1-pur_ret = 'Return'.
        ELSE.                                   " WA_EKKO-BSART NE 'ZVR '.
          wa_final1-pur_ret = 'Purchase'.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final1-lifnr .

    IF sy-subrc = 0.
      wa_final1-name1 = wa_lfa1-name1.
      wa_final1-regio = wa_lfa1-regio.
      wa_final1-stcd3 = wa_lfa1-stcd3.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr =  wa_final1-matnr .
    IF sy-subrc = 0.
      wa_final1-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_marc INTO wa_marc WITH KEY matnr =  wa_bseg-matnr
                                             werks =  wa_bseg-werks BINARY SEARCH.

    IF sy-subrc = 0.
      wa_final1-steuc = wa_marc-steuc.
    ENDIF.

    READ TABLE it_skat INTO wa_skat WITH KEY  saknr = wa_bseg-hkont .

    IF wa_final1-name1 = ''.
      wa_final1-saknr = wa_skat-saknr.
      wa_final1-name1 = wa_skat-txt20.
    ENDIF.
    IF  sy-subrc = 0."WA_LFA1-NAME1 = ''.
      wa_final1-saknr = wa_skat-saknr.
      wa_final1-txt20 = wa_skat-txt20.
    ENDIF.

    READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final1-mwskz . "BINARY SEARCH.

    IF sy-subrc = 0.
      wa_final1-text1 = wa_t007s-text1.
    ENDIF.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_bseg-werks BINARY SEARCH.

    IF sy-subrc = 0.
      wa_final1-t00_regio = wa_t001w-regio.
    ENDIF.
*      ON CHANGE OF WA_BSEG-BELNR.
    DATA :tem_bset_belnr TYPE bseg-belnr,
          tem_bset_taxps TYPE bseg-taxps,
          tem_bseg_belnr TYPE bseg-belnr,
          tem_bseg_taxps TYPE bseg-taxps.

*    LOOP AT IT_BSET INTO WA_BSET WHERE BUKRS = WA_BSEG-BUKRS AND
*                                          BELNR = WA_BSEG-BELNR AND
*                                          GJAHR = WA_BSEG-GJAHR AND
*                                         TAXPS = WA_BSEG-TAXPS .
*
**   IF WA_BSET-BELNR <> TEM_BSET_BELNR.
**         IF WA_BSET-KSCHL = 'JIIG' OR WA_BSET-KSCHL = 'JICG'.
**            IF WA_BSET-SHKZG = 'H'.
**              WA_BSET-HWBAS = WA_BSET-HWBAS * -1.
**              WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
**            ELSEIF WA_BSET-SHKZG = 'S'.
**              WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
**            ENDIF.
**          ENDIF.
**     ENDIF.
**        IF WA_BSET-BELNR = TEM_BSET_BELNR AND WA_BSET-TAXPS <> TEM_BSET_TAXPS .
**    IF WA_BSET-TAXPS <> TEM_BSET_TAXPS.
*      IF WA_BSET-KSCHL = 'JIIG' OR WA_BSET-KSCHL = 'JICG'.
*        IF WA_BSET-SHKZG = 'H'.
*          WA_BSET-HWBAS = WA_BSET-HWBAS * -1.
*          WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
*        ELSEIF WA_BSET-SHKZG = 'S'.
*          WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
*        ENDIF.
*      ENDIF.
**        ENDIF.
*
**      CLEAR :TEM_BSET_BELNR,TEM_BSET_TAXPS.
**      TEM_BSET_BELNR = WA_BSET-BELNR.
**      TEM_BSET_TAXPS = WA_BSET-TAXPS.
*      CLEAR : WA_BSET.
*    ENDLOOP.
*ENDON.
*    LOOP AT IT_BSEG1 INTO WA_BSEG1 WHERE BUKRS = WA_BSEG-BUKRS AND
*                                          BELNR = WA_BSEG-BELNR.
**      ON CHANGE OF WA_BSEG1-BELNR.


    LOOP AT it_bset INTO wa_bset WHERE bukrs = wa_bseg-bukrs AND
                                        belnr = wa_bseg-belnr AND
                                        gjahr = wa_bseg-gjahr AND
                                        taxps = wa_bseg-taxps.
      IF wa_bset-kschl = 'JIIG' OR wa_bset-kschl = 'JICG'.
        IF wa_bset-shkzg = 'H'.
          wa_bset-hwbas = wa_bset-hwbas * -1.
          wa_final1-hwbas_gst = wa_final1-hwbas_gst + wa_bset-hwbas.
        ELSEIF wa_bset-shkzg = 'S'.
          wa_final1-hwbas_gst = wa_final1-hwbas_gst + wa_bset-hwbas.
        ENDIF.
      ENDIF.

      IF tem_bseg_taxps IS INITIAL OR wa_bseg1-taxps <> tem_bseg_taxps.
*        IF WA_BSEG-SHKZG <> 'H'.
*          WA_FINAL1-HWBAS = WA_BSET-HWBAS.

        IF wa_bset-kschl = 'JIIG'.
          IF wa_bset-shkzg = 'H'.
*            WA_BSET-HWBAS = WA_BSET-HWBAS * -1.
*            WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_bset-fwste = wa_bset-fwste * -1.
            wa_final1-val_igst = wa_final1-val_igst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
*            WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_final1-val_igst = wa_final1-val_igst + wa_bset-fwste .
          ENDIF.
          wa_final1-per_igst = wa_bset-kbetr / 10 .
        ENDIF.
        IF wa_bset-kschl = 'JICG'.
          IF wa_bset-shkzg = 'H'.
*              WA_BSET-HWBAS = WA_BSET-HWBAS * -1.
            wa_bset-fwste = wa_bset-fwste * -1.
*              WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_final1-val_cgst = wa_final1-val_cgst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
*            WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_final1-val_cgst = wa_final1-val_cgst + wa_bset-fwste .

          ENDIF.
          wa_final1-per_cgst = wa_bset-kbetr / 10 .
        ENDIF.
        IF wa_bset-kschl = 'JISG'.
          IF wa_bset-shkzg = 'H'.
*            WA_BSET-HWBAS = WA_BSET-HWBAS * -1.
            wa_bset-fwste = wa_bset-fwste * -1.
*            WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_final1-val_sgst = wa_final1-val_sgst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
*            WA_FINAL1-HWBAS_GST = WA_FINAL1-HWBAS_GST + WA_BSET-HWBAS.
            wa_final1-val_sgst = wa_final1-val_sgst + wa_bset-fwste .

          ENDIF.
          wa_final1-per_sgst = wa_bset-kbetr / 10 .
        ENDIF.
        IF wa_bset-kschl = 'PTCS'.
          wa_final1-hwbas_tcs = wa_bset-hwbas.
          wa_final1-val_ptcs = wa_bset-fwste .
          wa_final1-per_ptcs = wa_bset-kbetr / 10 .
        ENDIF.

        IF wa_final1-val_igst NE 0 .
          wa_final1-gst_ret = 'IGST' .
        ENDIF.
        IF wa_final1-val_igst = 0.
          wa_final1-gst_ret = 'GST' .
        ENDIF.

        CLEAR: wa_bset.
*        ENDIF.
        CLEAR : tem_bseg_belnr,tem_bset_taxps.
        tem_bseg_belnr = wa_bseg1-belnr.
        tem_bset_taxps = wa_bseg1-taxps.
      ENDIF.
    ENDLOOP.


*      ENDON.
    CLEAR : wa_bseg1.
*    ENDLOOP.


    wa_final1-val_total = wa_final1-val_igst + wa_final1-val_cgst + wa_final1-val_sgst." + WA_FINAL1-VAL_PTCS .
    wa_final1-tot_gst = wa_final1-per_igst + wa_final1-per_cgst + wa_final1-per_sgst." + WA_FINAL1-PER_PTCS .

    ON CHANGE OF wa_bseg-belnr.
      LOOP AT it_tds INTO wa_tds WHERE bukrs = wa_bseg-bukrs AND
                                            belnr = wa_bseg-belnr AND
                                            gjahr = wa_bseg-gjahr." AND
        " J_1IBUZEI = wa_BSEG-BUZEI.
        IF wa_bseg-shkzg <> 'H'.
          wa_final1-tds = wa_final1-tds + wa_tds-wt_qbshh.
        ELSEIF wa_bseg-shkzg = 'H'.
          wa_final1-tds = wa_final1-tds + wa_tds-wt_qbshh.
          wa_final1-tds = wa_final1-tds * -1.
        ENDIF.
        CLEAR: wa_tds.
      ENDLOOP.
    ENDON.
    SHIFT wa_final1-matnr LEFT DELETING LEADING '0' .
    IF wa_final1-hwbas_gst NE 0.
      APPEND wa_final1 TO it_final1.
    ENDIF.
    CLEAR :  wa_final1 , wa_bset,wa_bseg,wa_bseg1.
    CLEAR  wa_lfa1.
    CLEAR  wa_ekko.
    CLEAR:  wa_ekpo,wa_tds.

  ENDLOOP.
********************Final1 table select Ends ****************

  CLEAR: tem_bset_belnr.

  DATA : temp_amount_gst  TYPE bset-fwste,
         temp_amount_igst TYPE bset-fwste,
         temp_gst_amount  TYPE bset-fwste,
         i                TYPE i.
  SORT it_final1 BY belnr budat.

  LOOP AT it_final1 INTO wa_final1.

    IF wa_final1-belnr = tem_bset_belnr AND wa_final1-val_sgst = temp_amount_gst AND wa_final1-val_sgst <> 0.
      wa_final1-tot_gst = '0'.
      wa_final1-val_total = '0'.
      wa_final1-val_total = '0'.
      wa_final1-val_cgst = '0'.
      wa_final1-val_sgst = '0'.
      wa_final1-per_cgst = '0'.
      wa_final1-per_sgst = '0'.

      IF wa_final1-hwbas_gst = temp_gst_amount.
        wa_final1-hwbas_gst = '0'.
      ENDIF.

      i = 1.
      MODIFY TABLE it_final1 FROM wa_final1.

    ELSEIF wa_final1-belnr = tem_bset_belnr AND wa_final1-val_igst = temp_amount_igst AND wa_final1-val_igst <> 0.
      wa_final1-tot_gst = '0'.
      wa_final1-val_total = '0'.
      wa_final1-val_igst = '0'.
      wa_final1-per_igst = '0'.

      IF wa_final1-hwbas_gst = temp_gst_amount.
        wa_final1-hwbas_gst = '0'.
      ENDIF.

      i = 1.
      MODIFY TABLE it_final1 FROM wa_final1.
    ELSE.
      i = 0.
    ENDIF.

    IF i = 0.
      CLEAR : tem_bset_belnr,temp_amount_gst,temp_amount_igst,temp_gst_amount.
      tem_bset_belnr = wa_final1-belnr.
      temp_gst_amount = wa_final1-hwbas_gst.
      IF  wa_final1-val_sgst <> 0.
        temp_amount_gst = wa_final1-val_sgst.
      ENDIF.
      IF wa_final1-val_igst <> 0.
        temp_amount_igst = wa_final1-val_igst.
      ENDIF.
    ENDIF.

    CLEAR wa_final1.

  ENDLOOP.
  DELETE it_final1 WHERE hwbas_gst = 0.
  DATA l_gst_amt TYPE bset-hwbas.
  LOOP AT it_final1 INTO wa_final1.
    IF wa_final1-tot_gst LE 0 .
      CONTINUE.
    ENDIF.
    READ TABLE it_final ASSIGNING FIELD-SYMBOL(<fs1>)
    WITH KEY belnr = wa_final1-belnr
*             ebeln = wa_final1-ebeln
             per_igst = wa_final1-per_igst
             per_cgst = wa_final1-per_cgst
             per_sgst = wa_final1-per_sgst.
    IF sy-subrc = 0.
      <fs1>-wrbtr_h = <fs1>-wrbtr_h + wa_final1-wrbtr_h.
      <fs1>-wrbtr_s = <fs1>-wrbtr_s + wa_final1-wrbtr_s.

      <fs1>-hwbas_gst = <fs1>-hwbas_gst + wa_final1-hwbas_gst.
      <fs1>-val_igst = <fs1>-val_igst + wa_final1-val_igst.
      <fs1>-val_cgst = <fs1>-val_cgst + wa_final1-val_cgst.
      <fs1>-val_sgst = <fs1>-val_sgst + wa_final1-val_sgst.
      <fs1>-val_ptcs = <fs1>-val_ptcs + wa_final1-val_ptcs.
      <fs1>-tds = <fs1>-tds + wa_final1-tds.
      <fs1>-val_total = <fs1>-val_total + wa_final1-val_total.
    ELSE.
      MOVE-CORRESPONDING wa_final1 TO wa_final.
      CLEAR: wa_final-steuc,wa_final-ebeln, wa_final-ebelp,
             wa_final-maktx,wa_final-matnr,
             wa_final-menge,wa_final-meins.
      wa_final-tot_gst = wa_final1-per_igst + wa_final1-per_cgst + wa_final1-per_sgst.
      APPEND wa_final TO it_final.
      CLEAR wa_final.
    ENDIF.
  ENDLOOP.


  LOOP AT it_final ASSIGNING FIELD-SYMBOL(<fs>).
    IF <fs>-tcode = 'MIRO'.
      l_gst_amt = <fs>-hwbas_gst * -1.
      READ TABLE it_final ASSIGNING  <fs1>
                          WITH KEY lifnr = <fs>-lifnr
                                   per_igst = <fs>-per_igst
                                   per_cgst = <fs>-per_cgst
                                   per_sgst = <fs>-per_sgst
                                   hwbas_gst = l_gst_amt
                                   tcode = 'MR8M'.
      IF sy-subrc = 0 .
        <fs>-hwbas_gst = 0.
        <fs1>-hwbas_gst = 0.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE it_final WHERE hwbas_gst = 0.
  CLEAR it_final1[].
  it_final1 = it_final.
ENDFORM.                    " READ_DATA

************** Start Of Selection Screen Ends *****************
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FV60
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_fv60 .

  SELECT bukrs belnr gjahr bldat budat tcode xblnr waers kursf awkey FROM bkpf
       INTO TABLE it_bkpf
       WHERE budat IN s_budat AND
             tcode IN ( 'FB60','FB65','FV60','FV65','FBVB','ZFI_INV_POST' )  AND ( blart = 'KG' OR blart = 'KA' OR blart = 'KR' OR blart = 'RE' ).
*             bstat NOT IN ( 'Z','V' ) .


  IF it_bkpf IS NOT INITIAL.
    SELECT bukrs belnr gjahr buzei buzid bschl mwskz wrbtr hkont lifnr matnr werks menge meins ebeln ebelp hsn_sac taxps shkzg ktosl FROM bseg "#EC CI_DB_OPERATION_OK[2431747]"Added by SPLABAP during code remediation
      INTO TABLE it_bseg1
      FOR ALL ENTRIES IN it_bkpf
       WHERE bukrs = it_bkpf-bukrs
       AND belnr = it_bkpf-belnr
       AND gjahr = it_bkpf-gjahr
       AND bukrs IN s_bukrs AND ktosl NE 'JIS' AND ktosl NE 'JIC'
      AND ktosl NE 'JII' AND ktosl NE 'JIM' AND ktosl NE 'JIU'
      AND ktosl NE 'WIT' AND ktosl NE 'JRC' AND ktosl NE 'JRS' AND ktosl NE 'PTC'. "#EC CI_NOORDER  "Added by SPLABAP during code remediation
  ENDIF.
  IF it_bseg1 IS NOT INITIAL.
    SELECT matnr maktx  FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_bseg1
       WHERE matnr = it_bseg1-matnr.

    SELECT matnr werks steuc FROM marc
      INTO TABLE it_marc
      FOR ALL ENTRIES IN it_bseg1
       WHERE matnr = it_bseg1-matnr.

      SELECT lifnr name1 regio stcd3 FROM lfa1
          INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_bseg1
        WHERE lifnr = it_bseg1-lifnr.

        SELECT mwskz text1 FROM t007s "#EC CI_NOORDER  "Added by SPLABAP during code remediation
          INTO TABLE it_t007s
          FOR ALL ENTRIES IN it_bseg1
          WHERE mwskz = it_bseg1-mwskz
           AND spras EQ 'EN'
          AND kalsm EQ 'TAXINN' .

          SELECT  werks regio FROM t001w
            INTO TABLE it_t001w
            FOR ALL ENTRIES IN it_bseg1
            WHERE werks = it_bseg1-werks.

            SELECT bukrs belnr gjahr wt_qbshh j_1ibuzei FROM with_item INTO TABLE it_tds FOR ALL ENTRIES IN it_bseg1 WHERE bukrs = it_bseg1-bukrs
             AND belnr = it_bseg1-belnr
             AND gjahr = it_bseg1-gjahr.
              "  AND J_1IBUZEI = IT_BSEG1-BUZEI.

              SELECT bukrs belnr gjahr buzei shkzg hwbas fwste kschl kbetr taxps FROM bset "#EC CI_NOORDER  "Added by SPLABAP during code remediation
                INTO TABLE it_bset
                FOR ALL ENTRIES IN it_bseg1
                WHERE belnr = it_bseg1-belnr
                AND   gjahr = it_bseg1-gjahr
                AND   taxps = it_bseg1-taxps.

                SELECT   ktopl saknr txt20  FROM skat
                INTO TABLE it_skat
              FOR ALL ENTRIES IN it_bseg1
              WHERE ktopl = 'YAIN'
              AND   saknr = it_bseg1-hkont.

*  APPEND LINES OF IT_BSEG1 TO IT_BSEG2.
*
*  DELETE IT_BSEG2 WHERE LIFNR EQ ' ' .
                ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FV60
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fv60 .

  SORT it_bkpf BY belnr gjahr.

  LOOP AT it_bseg1 INTO wa_bseg1.
    wa_bseg-belnr = wa_bseg1-belnr.
    wa_bseg-gjahr = wa_bseg1-gjahr.
    wa_bseg-buzid = wa_bseg1-buzid.
    wa_bseg-bschl = wa_bseg1-bschl.
    wa_bseg-mwskz = wa_bseg1-mwskz.
    wa_bseg-wrbtr = wa_bseg1-wrbtr.
    wa_bseg-lifnr = wa_bseg1-lifnr.
    wa_bseg-hkont = wa_bseg1-hkont.
    wa_bseg-matnr = wa_bseg1-matnr.
    wa_bseg-werks = wa_bseg1-werks.
    wa_bseg-menge = wa_bseg1-menge.
    wa_bseg-meins = wa_bseg1-meins.
    wa_bseg-ebeln = wa_bseg1-ebeln.
    wa_bseg-ebelp = wa_bseg1-ebelp.
    wa_bseg-hsn_sac = wa_bseg1-hsn_sac.
    wa_bseg-bukrs = wa_bseg1-bukrs.
    wa_bseg-taxps = wa_bseg1-taxps.
    wa_bseg-shkzg = wa_bseg1-shkzg.

*    READ TABLE IT_LFA1  INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.
*    IF SY-SUBRC EQ 0 .
*      WA_BSEG-LIFNR = WA_LFA1-LIFNR.
*      ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bseg-belnr
                                             gjahr = wa_bseg-gjahr BINARY SEARCH."BUKRS = WA_BSEG-BUKRS BINARY SEARCH.
    IF sy-subrc EQ 0 .
      wa_bseg-tcode = wa_bkpf-tcode.
    ENDIF.
    APPEND wa_bseg TO it_bseg.
    CLEAR wa_bseg1.
    CLEAR wa_bseg.

  ENDLOOP.

  SORT it_bkpf BY belnr gjahr.
  SORT it_marc BY matnr werks.
  SORT it_ekko BY ebeln .
  SORT it_makt BY matnr.
  SORT it_lfa1 BY lifnr.
  SORT it_t007s BY mwskz.
  SORT it_t001w BY werks.
  SORT it_skat BY saknr.


  LOOP AT it_bseg INTO wa_bseg .

    wa_final1-belnr = wa_bseg-belnr.
    wa_final1-gjahr = wa_bseg-gjahr.
    wa_final1-mwskz = wa_bseg-mwskz.
    wa_final1-wrbtr = wa_bseg-wrbtr.

    wa_final1-shkzg = wa_bseg-shkzg.

    IF wa_bseg-shkzg = 'H'.
      wa_final1-wrbtr_h = wa_final1-wrbtr_h + wa_bseg-wrbtr.
    ELSEIF wa_bseg-shkzg = 'S'.
      wa_final1-wrbtr_s = wa_final1-wrbtr_s + wa_bseg-wrbtr.
    ENDIF.


*    IF WA_BSEG-TCODE EQ 'FV60'.

    IF wa_bseg-lifnr = ' '.
      wa_final1-name1 = wa_lfa1-name1.
        wa_final1-regio = wa_lfa1-regio.
        wa_final1-stcd3 = wa_lfa1-stcd3.
    SELECT SINGLE a~lifnr
                  b~name1
                  b~regio
                  b~stcd3 FROM bseg as a INNER JOIN lfa1 as b ON a~lifnr = b~lifnr
      INTO ( wa_final1-lifnr,
            wa_final1-name1,
            wa_final1-regio,
            wa_final1-stcd3 )
      WHERE  a~bukrs = wa_bseg-bukrs
         AND a~belnr = wa_bseg-belnr
         AND a~gjahr = wa_bseg-gjahr AND
             a~bschl = '31'.
    ELSE.
      wa_final1-lifnr = wa_bseg-lifnr.
    ENDIF.
*    ENDIF.

    wa_final1-matnr = wa_bseg-matnr.
    wa_final1-werks = wa_bseg-werks.
    wa_final1-menge = wa_bseg-menge.
    wa_final1-meins = wa_bseg-meins.
    wa_final1-ebeln = wa_bseg-ebeln.
    wa_final1-ebelp = wa_bseg-ebelp.
    wa_final1-hsn_sac = wa_bseg-hsn_sac.
    wa_final1-bukrs = wa_bseg-bukrs.
    wa_final1-taxps = wa_bseg-taxps.

    IF wa_bseg-bschl EQ '89'.
      wa_final1-seg_hkont = wa_bseg-hkont.
    ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bseg-belnr
                                             gjahr = wa_bseg-gjahr BINARY SEARCH.
    IF wa_final1-mwskz = 'R*'.
      wa_final1-tax_dec = 'RCM '.
    ELSEIF wa_final1-mwskz NE 'R*' .
      wa_final1-tax_dec = 'TAXABLE – GST'.
    ENDIF.

    IF sy-subrc = 0.
      wa_final1-belnr = wa_bkpf-belnr.
      wa_final1-gjahr = wa_bkpf-gjahr.
      wa_final1-bldat = wa_bkpf-bldat.
      wa_final1-budat = wa_bkpf-budat.
      wa_final1-tcode = wa_bkpf-tcode.
      wa_final1-xblnr = wa_bkpf-xblnr.
      wa_final1-waers = wa_bkpf-waers.
      wa_final1-kursf = wa_bkpf-kursf.
      wa_final1-awkey = wa_bkpf-awkey.

**      CALL FUNCTION 'ISP_GET_MONTH_NAME'
**        EXPORTING
**          DATE         = WA_FINAL1-BLDAT
**          LANGUAGE     = SY-LANGU
***         MONTH_NUMBER = '00'
**        IMPORTING
***         LANGU_BACK   =
**          LONGTEXT     = WA_FINAL1-MONTHS
***         SHORTTEXT    =
**        EXCEPTIONS
**          CALENDAR_ID  = 1
**          DATE_ERROR   = 2
**          NOT_FOUND    = 3
**          WRONG_INPUT  = 4
**          OTHERS       = 5.
**      IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**      ENDIF.
      SELECT SINGLE ltx FROM t247
          INTO wa_final1-months
          WHERE spras EQ sy-langu AND mnr EQ wa_final1-bldat+4(2).

      ENDIF.
      wa_final1-docno = wa_final1-belnr.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final1-lifnr .

      IF sy-subrc = 0.
        wa_final1-name1 = wa_lfa1-name1.
        wa_final1-regio = wa_lfa1-regio.
        wa_final1-stcd3 = wa_lfa1-stcd3.
      ENDIF.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr =  wa_final1-matnr .
      IF sy-subrc = 0.
        wa_final1-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_marc INTO wa_marc WITH KEY matnr =  wa_bseg-matnr werks =  wa_bseg-werks BINARY SEARCH.

      IF sy-subrc = 0.
        wa_final1-steuc = wa_marc-steuc.
      ENDIF.

      READ TABLE it_skat INTO wa_skat WITH KEY  saknr = wa_bseg-hkont .

      IF wa_final1-name1 = ''.
        wa_final1-saknr = wa_skat-saknr.
        wa_final1-name1 = wa_skat-txt20.
      ENDIF.
      IF  sy-subrc = 0."WA_LFA1-NAME1 = ''.
        wa_final1-saknr = wa_skat-saknr.
        wa_final1-txt20 = wa_skat-txt20.
      ENDIF.

      READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final1-mwskz . "BINARY SEARCH.

      IF sy-subrc = 0.
        wa_final1-text1 = wa_t007s-text1.
      ENDIF.

      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_bseg-werks BINARY SEARCH.

      IF sy-subrc = 0.
        wa_final1-t00_regio = wa_t001w-regio.
      ENDIF.

      LOOP AT it_bset INTO wa_bset WHERE bukrs = wa_bseg-bukrs AND
                                          belnr = wa_bseg-belnr AND
                                          gjahr = wa_bseg-gjahr AND
                                          taxps = wa_bseg-taxps .
*      IF WA_BSEG-SHKZG <> 'H'.
*
*      IF SY-SUBRC = '0' .
*        WA_FINAL1-HWBAS = WA_BSET-HWBAS.
        IF wa_bset-kschl = 'JIIG'.
          wa_final1-hwbas_gst = wa_bset-hwbas.
          IF wa_bset-shkzg = 'H'.
            wa_bset-fwste = wa_bset-fwste * -1.
            wa_final1-val_igst = wa_final1-val_igst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
            wa_final1-val_igst = wa_final1-val_igst + wa_bset-fwste .
          ENDIF.
          wa_final1-per_igst = wa_bset-kbetr / 10 .
        ENDIF.
        IF wa_bset-kschl = 'JICG' .
          IF wa_bset-shkzg = 'H'.
            wa_bset-fwste = wa_bset-fwste * -1.
            wa_final1-val_cgst = wa_final1-val_cgst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
            wa_final1-val_cgst = wa_final1-val_cgst + wa_bset-fwste .
          ENDIF.
          wa_final1-per_cgst = wa_bset-kbetr / 10 .
        ENDIF.
        IF wa_bset-kschl = 'JISG'.
          wa_final1-hwbas_gst = wa_bset-hwbas.
          IF wa_bset-shkzg = 'H'.
            wa_bset-fwste = wa_bset-fwste * -1.
            wa_final1-val_sgst = wa_final1-val_sgst + wa_bset-fwste .
          ELSEIF wa_bset-shkzg = 'S'.
            wa_final1-val_sgst = wa_final1-val_sgst + wa_bset-fwste .
          ENDIF.
          wa_final1-per_sgst = wa_bset-kbetr / 10 .
        ENDIF.

        IF wa_bset-kschl = 'PTCS'.
          wa_final1-hwbas_tcs = wa_bset-hwbas.
          wa_final1-val_ptcs = wa_bset-fwste .
          wa_final1-per_ptcs = wa_bset-kbetr / 10 .
        ENDIF.

        IF wa_final1-val_igst NE 0 .
          wa_final1-gst_ret = 'IGST' .
        ENDIF.
        IF wa_final1-val_igst = 0.
          wa_final1-gst_ret = 'GST' .
        ENDIF.
        CLEAR: wa_bset.
*      ENDIF.

      ENDLOOP.

      wa_final1-val_total = wa_final1-val_igst + wa_final1-val_cgst + wa_final1-val_sgst." + WA_FINAL1-VAL_PTCS.
      wa_final1-tot_gst = wa_final1-per_igst + wa_final1-per_cgst + wa_final1-per_sgst." + WA_FINAL1-PER_PTCS .

      ON CHANGE OF wa_bseg-belnr.
        LOOP AT it_tds INTO wa_tds WHERE bukrs = wa_bseg-bukrs AND
                                              belnr = wa_bseg-belnr AND
                                              gjahr = wa_bseg-gjahr." AND
          " J_1IBUZEI = WA_BSEG-BUZEI.
          IF wa_bseg-shkzg = 'H'.


            wa_final1-tds = wa_final1-tds + wa_tds-wt_qbshh.
*              WA_FINAL1-TDS = WA_FINAL1-TDS * -1.

          ENDIF.
          CLEAR: wa_tds.
        ENDLOOP.
      ENDON.

      SHIFT wa_final1-matnr LEFT DELETING LEADING '0' .

      IF wa_final1-hwbas_gst NE 0.
        APPEND wa_final1 TO it_final1.
      ENDIF.
      CLEAR  wa_final1.
      CLEAR  wa_bset.
      CLEAR  wa_bseg.
      CLEAR  wa_bseg1.
      CLEAR:  wa_lfa1,wa_tds.

    ENDLOOP.

    DELETE it_final1 WHERE hwbas_gst = 0.
    DATA l_gst_amt TYPE bset-hwbas.
    LOOP AT it_final1 INTO wa_final1.
      IF wa_final1-tot_gst LE 0 .
        CONTINUE.
      ENDIF.
      READ TABLE it_final ASSIGNING FIELD-SYMBOL(<fs1>)
      WITH KEY belnr = wa_final1-belnr
*             ebeln = wa_final1-ebeln
               per_igst = wa_final1-per_igst
               per_cgst = wa_final1-per_cgst
               per_sgst = wa_final1-per_sgst.
      IF sy-subrc = 0.
        <fs1>-wrbtr_h = <fs1>-wrbtr_h + wa_final1-wrbtr_h.
        <fs1>-wrbtr_s = <fs1>-wrbtr_s + wa_final1-wrbtr_s.

        <fs1>-hwbas_gst = <fs1>-hwbas_gst + wa_final1-hwbas_gst.
        <fs1>-val_igst = <fs1>-val_igst + wa_final1-val_igst.
        <fs1>-val_cgst = <fs1>-val_cgst + wa_final1-val_cgst.
        <fs1>-val_sgst = <fs1>-val_sgst + wa_final1-val_sgst.
        <fs1>-val_ptcs = <fs1>-val_ptcs + wa_final1-val_ptcs.
        <fs1>-tds = <fs1>-tds + wa_final1-tds.
        <fs1>-val_total = <fs1>-val_total + wa_final1-val_total.
      ELSE.
        MOVE-CORRESPONDING wa_final1 TO wa_final.
        CLEAR: wa_final-steuc,wa_final-ebeln, wa_final-ebelp,
               wa_final-maktx,wa_final-matnr,
               wa_final-menge,wa_final-meins.
        wa_final-tot_gst = wa_final1-per_igst + wa_final1-per_cgst + wa_final1-per_sgst.
        APPEND wa_final TO it_final.
        CLEAR wa_final.
      ENDIF.
    ENDLOOP.

    DELETE it_final WHERE hwbas_gst = 0.
    CLEAR it_final1[].
    it_final1 = it_final.
********************Final1 table select Ends ****************
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat .


  wa_fieldcat-fieldname ='MONTHS'. "BKPF-BLDAT
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Filing Month'.
  wa_fieldcat-seltext_l ='Filing Month'.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-outputlen = '10'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
*
  wa_fieldcat-fieldname ='TCODE'. "BKPF-TCODE,  "Transaction Code
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='T-Code'.
  wa_fieldcat-seltext_l ='T-Code'.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-outputlen = '10'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='BELNR'. "BSEG-BELNR,"Inv. Doc. No.
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Doc NO'.
  wa_fieldcat-seltext_l ='Document Number'.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='BUDAT'. "BKPF-BUDAT, "Posting Date
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Post Date'.
  wa_fieldcat-seltext_l ='Posting Date '.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='DOCNO'. "BKPF-BELNR
  wa_fieldcat-tabname ='IT_FINAL11'.
  wa_fieldcat-seltext_s ='Ac Doc'.
  wa_fieldcat-seltext_l ='Account Document'.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='STCD3'. "LFA1-STCD3
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='GST Vendor'.
  wa_fieldcat-seltext_l ='GST IN Of Vendor'.
  wa_fieldcat-col_pos   = 6.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='LIFNR'. "BSEG-LIFNR,  "Vendor Code
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Vendor No'.
  wa_fieldcat-seltext_l ='Vendor Number'.
  wa_fieldcat-col_pos   = 7.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.


  wa_fieldcat-fieldname ='NAME1'. "LFA1-NAME1, SKAT-TXT20 "Vendor Name
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Vendor'.
  wa_fieldcat-seltext_l ='Vendor Name'.
  wa_fieldcat-col_pos   = 8.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='XBLNR'. "BKPF-XBLNR,"Reference
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Vendor Invoice '.
  wa_fieldcat-seltext_l ='Vendor Invoice '.
  wa_fieldcat-col_pos   = 9.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='BLDAT'. "BKPF-BLDAT,"Document Date
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Doc Date'.
  wa_fieldcat-seltext_l ='Document Date'.
  wa_fieldcat-col_pos   = 10.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='STEUC'. "BSEG-HSN_SAC  MARC-STEUC
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='HSN/SAC'.
  wa_fieldcat-seltext_l ='HSN/SAC Code'.
  wa_fieldcat-col_pos   = 11.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME ='MAKTX'. "MAKT-MAKTX,"Description
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Material Desc'.
*  WA_FIELDCAT-SELTEXT_L ='Material Description'.
*  WA_FIELDCAT-COL_POS   = 12.
*  WA_FIELDCAT-OUTPUTLEN = '15'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

  wa_fieldcat-fieldname ='EBELN'. "BSEG-EBELN, "Purchasing Doc.
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='PO No'.
  wa_fieldcat-seltext_l ='PO No'.
  wa_fieldcat-col_pos   = 12.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='EBELP'. "BSEG-EBELp, "Purchasing item.
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='PO Item No'.
  wa_fieldcat-seltext_l ='PO Item No'.
  wa_fieldcat-col_pos   = 13.
  wa_fieldcat-outputlen = '10'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='MATNR'. "BSEG-MATNR, "Material
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Material No'.
  wa_fieldcat-seltext_l ='Material Number'.
  wa_fieldcat-col_pos   = 14.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='MAKTX'. "MAKT-MAKTX,"Description
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Material Desc'.
  wa_fieldcat-seltext_l ='Material Description'.
  wa_fieldcat-col_pos   = 15.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='MENGE'. " BSEG-MENGE, "Quantity
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Quantity'.
  wa_fieldcat-seltext_l ='PO Quantity'.
  wa_fieldcat-col_pos   = 16.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='MEINS'. "BSEG-MEINS, "Base Unit
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='UOM'.
  wa_fieldcat-seltext_l ='UOM'.
  wa_fieldcat-col_pos   = 17.
  wa_fieldcat-outputlen = '5'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME ='WRBTR'. "BSEG-WRBTR, "Amount
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Invoice Value'.
*  WA_FIELDCAT-SELTEXT_L ='Invoice Value'.
*  WA_FIELDCAT-COL_POS   = 18.
*  WA_FIELDCAT-OUTPUTLEN = '15'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

  wa_fieldcat-fieldname ='WRBTR_H'. "BSEG-WRBTR, "Amount
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Invoice CR Value'.
  wa_fieldcat-seltext_l ='Invoice CR Value'.
  wa_fieldcat-col_pos   = 19.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.


  wa_fieldcat-fieldname ='WRBTR_S'. "BSEG-WRBTR, "Amount
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Invoice DR Value'.
  wa_fieldcat-seltext_l ='Invoice DR Value'.
  wa_fieldcat-col_pos   = 20.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME ='HWBAS'. " BSET-HWBAS
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Taxable Value'.
*  WA_FIELDCAT-SELTEXT_L ='Taxable Value'.
*  WA_FIELDCAT-COL_POS   = 21.
*  WA_FIELDCAT-OUTPUTLEN = '15'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.


  wa_fieldcat-fieldname ='HWBAS_GST'. " BSET-HWBAS
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='GST Taxable Value'.
  wa_fieldcat-seltext_l ='GST Taxable Value'.
  wa_fieldcat-col_pos   = 21.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.


*  WA_FIELDCAT-FIELDNAME ='HWBAS_TCS'. " BSET-HWBAS
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='TCS Taxable Value'.
*  WA_FIELDCAT-SELTEXT_L ='TCS Taxable Value'.
*  WA_FIELDCAT-COL_POS   = 22.
*  WA_FIELDCAT-OUTPUTLEN = '15'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.
*
  wa_fieldcat-fieldname ='VAL_IGST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Value of IGST'.
  wa_fieldcat-seltext_l ='Value of IGST'.
  wa_fieldcat-col_pos   = 23.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='VAL_CGST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Value of CGST'.
  wa_fieldcat-seltext_l ='Value of CGST'.
  wa_fieldcat-col_pos   = 24.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='VAL_SGST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Value of SGST'.
  wa_fieldcat-seltext_l ='Value of SGST'.
  wa_fieldcat-col_pos   = 25.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='VAL_PTCS'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Value of TCS'.
  wa_fieldcat-seltext_l ='Value of TCS'.
  wa_fieldcat-col_pos   = 26.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='TDS'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Value of TDS'.
  wa_fieldcat-seltext_l ='Value of TDS'.
  wa_fieldcat-col_pos   = 27.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='VAL_TOTAL'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Total GST Value'.
  wa_fieldcat-seltext_l ='Total GST Value'.
  wa_fieldcat-col_pos   = 28.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='PER_IGST'. "
  wa_fieldcat-tabname ='IT_FINAL11'.
  wa_fieldcat-seltext_s ='Tax IGST'.
  wa_fieldcat-seltext_l ='Rate of Tax IGST'.
  wa_fieldcat-col_pos   = 29.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='PER_CGST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax CGST'.
  wa_fieldcat-seltext_l ='Rate of Tax CGST'.
  wa_fieldcat-col_pos   = 30.
  wa_fieldcat-outputlen = '12'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='PER_SGST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax SGST'.
  wa_fieldcat-seltext_l ='Rate of Tax SGST'.
  wa_fieldcat-col_pos   = 31.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='PER_PTCS'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax TCS'.
  wa_fieldcat-seltext_l ='Rate of Tax TCS'.
  wa_fieldcat-col_pos   = 32.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='TOT_GST'. "
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Total TAX %'.
  wa_fieldcat-seltext_l ='Total TAX %'.
  wa_fieldcat-col_pos   = 33.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
*
*  WA_FIELDCAT-FIELDNAME ='REWRT'. "
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Cess'.
*  WA_FIELDCAT-SELTEXT_L ='Cess'.
*  WA_FIELDCAT-COL_POS   = 28.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

  wa_fieldcat-fieldname ='MWSKZ '. "BSEG-MWSKZ, "Tax code
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax code'.
  wa_fieldcat-seltext_l ='Tax code'.
  wa_fieldcat-col_pos   = 34.
  wa_fieldcat-outputlen = '10'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='TEXT1'. "  T007S-TEXT1
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax Code Desc'.
  wa_fieldcat-seltext_l ='Tax Code Description'.
  wa_fieldcat-col_pos   = 35.
  wa_fieldcat-outputlen = '20'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='WAERS'. "BKPF-WAERS, "Currency
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Curr Key'.
  wa_fieldcat-seltext_l ='Currency Key'.
  wa_fieldcat-col_pos   = 36.
  wa_fieldcat-outputlen = '6'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='KURSF'. "BKPF-KURSF
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Exc Rate'.
  wa_fieldcat-seltext_l ='Exchange Rate'.
  wa_fieldcat-col_pos   = 37.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='BSART'. "EKKO-BSART,  "Document Type
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Doc Type'.
  wa_fieldcat-seltext_l ='Document Type'.
  wa_fieldcat-col_pos   = 38.
  wa_fieldcat-outputlen = '8'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='WERKS'. "BSEG-WERKS," plant
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Plant'.
  wa_fieldcat-seltext_l ='PO Plant'.
  wa_fieldcat-col_pos   = 39.
  wa_fieldcat-outputlen = '6'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='REGIO'. " LFA1-REGIO
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='State/Code'.
  wa_fieldcat-seltext_l ='State/Code'.
  wa_fieldcat-col_pos   = 40.
  wa_fieldcat-outputlen = '10'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME ='UF'. " MSEG-SAKTO
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Purch GL Name'.
*  WA_FIELDCAT-SELTEXT_L ='Purchase GL Name'.
*  WA_FIELDCAT-COL_POS   = 36.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME ='FITYP'. "LFA1-FITYP
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Tax type'.
*  WA_FIELDCAT-SELTEXT_L ='Tax type Desc'.
*  WA_FIELDCAT-COL_POS   = 37.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.
*
*

  wa_fieldcat-fieldname ='SEG_HKONT'. " BSEG-HKONT
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Purchase GL Name'.
  wa_fieldcat-seltext_l ='Purchase GL Name'.
  wa_fieldcat-col_pos   = 41.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname ='TAX_DEC'. "  base on Tax Code
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Tax Description'.
  wa_fieldcat-seltext_l ='Tax Description'.
  wa_fieldcat-col_pos   = 42.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.


  wa_fieldcat-fieldname ='T00_REGIO'. " T001W-REGIO
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='Division'.
  wa_fieldcat-seltext_l ='Division'.
  wa_fieldcat-col_pos   = 43.
  wa_fieldcat-outputlen = '6'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
*
*  WA_FIELDCAT-FIELDNAME ='TXBH2'. "BSEG-TXBH2
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='Tax Base'.
*  WA_FIELDCAT-SELTEXT_L ='Tax Base/Original Ta'.
*  WA_FIELDCAT-COL_POS   = 39.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME ='GST_PART'. "BSEG-GST_PART
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='GST Par'.
*  WA_FIELDCAT-SELTEXT_L ='GST Partner'.
*  WA_FIELDCAT-COL_POS   = 40.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME ='RETPO'. "EKPO-RETPO
*  WA_FIELDCAT-TABNAME ='IT_FINAL1'.
*  WA_FIELDCAT-SELTEXT_S ='PO or Return'.
*  WA_FIELDCAT-SELTEXT_L ='Purchase or Return'.
*  WA_FIELDCAT-COL_POS   = 41.
*  WA_FIELDCAT-OUTPUTLEN = '20'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

  wa_fieldcat-fieldname ='GST_RET'. " Base on the Igst Cgst and Sgat Value
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='GST'.
  wa_fieldcat-seltext_l ='GST '.
  wa_fieldcat-col_pos   = 44.
  wa_fieldcat-outputlen = '6'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.


  wa_fieldcat-fieldname ='PUR_RET'. " based on EKKO- bsart
  wa_fieldcat-tabname ='IT_FINAL1'.
  wa_fieldcat-seltext_s ='PO or Return'.
  wa_fieldcat-seltext_l ='Purchase or Return'.
  wa_fieldcat-col_pos   = 45.
  wa_fieldcat-outputlen = '15'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout .

  wa_layout-zebra = 'X'.

ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = it_repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = wa_layout
      it_fieldcat        = it_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = it_final1
* EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " DISPLAY
