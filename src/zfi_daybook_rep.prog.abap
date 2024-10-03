*&---------------------------------------------------------------------*
*& Report ZFI_DAYBOOK_REP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_daybook_rep.

TABLES: bsis,bsas,bkpf,kna1,lfa1.

DATA repid TYPE sy-repid.
TYPE-POOLS: cxtab, slis.
DATA: control_cols TYPE cxtab_column.

DATA : collectstr TYPE string.



TYPES: BEGIN OF ty_atta,
         instid_a TYPE srgbtbrel-instid_a,
         catid_b  TYPE srgbtbrel-catid_a,
       END OF ty_atta.
DATA: it_atta   TYPE TABLE OF ty_atta,
      wa_atta   TYPE ty_atta,
      it_atta_1 TYPE TABLE OF ty_atta,
      wa_atta_1 TYPE ty_atta.

*DATA alv TYPE REF TO cl_salv_table.
DATA: variant TYPE disvariant.     "<----  Here
variant-report = sy-repid.           "<----  Here
variant-username  = sy-uname.

TYPES: BEGIN OF ty_final,

         bukrs      TYPE bsis-bukrs,    " Company Code
         belnr      TYPE bsis-belnr,    " Document Number
         buzei      TYPE bsis-buzei,    " Line item
         xblnr      TYPE bsis-xblnr,    " Reference
         gjahr      TYPE bsis-gjahr,    " Fiscal Year
         blart      TYPE bsis-blart,    " Document Type
         monat      TYPE bsis-monat,    " Period
         bldat      TYPE bsis-bldat,    " Document Date
         budat      TYPE bsis-budat,    " Posting Date
         shkzg      TYPE bsis-shkzg,    " Debit/Credit
         gsber      TYPE bsis-gsber,    " Business Area
         dmbtr      TYPE bsis-dmbtr,    " Amount in LC
         pswsl      TYPE bsis-pswsl,    " G/L currency
         mwskz      TYPE bsis-mwskz,    " Tax code
         waers      TYPE bsis-waers,    " Currency
         zuonr      TYPE bsis-zuonr,    " Assignment
         sgtxt      TYPE bsis-sgtxt,    " Text
         kostl      TYPE bsis-kostl,    " Cost Center
         aufnr      TYPE bsis-aufnr,    " Order
         hkont      TYPE bsis-hkont,    " G/L
         bschl      TYPE bsis-bschl,    " Posting key
         augdt      TYPE bsis-augdt,    " Clearing
         augbl      TYPE bsis-augbl,    " Clrng doc.
         werks      TYPE bsis-werks,    " Plant
         fipos      TYPE bsis-fipos,    " Commitment Item
         cpudt      TYPE bkpf-cpudt,    " Entered on
         usnam      TYPE bkpf-usnam, " User name
         ppnam      TYPE bkpf-ppnam,  " Parked by"  Add by Viknesh on 22.10.2020 Park and Post added.
*ABTEI TYPE CSKS-ABTEI,
         txt20      TYPE skat-txt20,    " GL text
         ktext      TYPE cskt-ktext,    " Cost center text
*KOSTV TYPE AUFK-KOSTV,
         name1      TYPE kna1-name1,    " customer description
         vend2      TYPE lfa1-name1,    " vendor description.
         lifnr      TYPE bseg-lifnr,    "  VENDOR NUMBER
         kunnr      TYPE bseg-kunnr,    "  CUSTOMER NUMBER
         matnr      TYPE bseg-matnr,    " Materials code
         menge      TYPE bseg-menge,    " Qty
         meins      TYPE bseg-meins,    " UOm
         anln1      TYPE bseg-anln1,    " Asset
         anln2      TYPE bseg-anln2,    " Subnumber
         bpmng      TYPE bseg-bpmng,    " Qty in OPUn
         bprme      TYPE bseg-bprme,    " PO Price Unit
         ebeln      TYPE bseg-ebeln,    " Purchasing Doc.
         ebelp      TYPE bseg-ebelp,    " Item
         netwr      TYPE ekpo-netwr,    " Net Order Amount 29.11.2019
         batxt      TYPE t161t-batxt,   " Doc Type Description 10.01.2020.
         txt50      TYPE anla-txt50,    " Asset Description
         atta(20)   TYPE c,           " Attachment Indicator
         name_text  TYPE adrp-name_text, " SAP user Name
         name_text1 TYPE adrp-name_text, " SAP user Name of 'Parked by'  Add by Viknesh on 22.10.2020 Park and Post added.
         ltext      TYPE t003t-ltext,  " Document Type Description Add by Vikesh on 03.11.2020.
         text1      TYPE t007s-text1,  " Tax Code Description Add by Vikesh on 03.11.2020.
         witht      TYPE with_item-witht,                        "W/Tax Type
         wt_qsshb   TYPE with_item-wt_qsshb,                 " -W/Tax Base
         wt_qbshb   TYPE with_item-wt_qbshb,                 "- W/Tax Amt
         wt_qsshh   TYPE with_item-wt_qsshh,                             "- W/Tax Base LC
         wt_qbshh   TYPE with_item-wt_qbshh,                             "- W/Tax Amt LC  amount equal
       END OF ty_final.
TYPES: BEGIN OF ty_ekpo,
         netwr TYPE ekpo-netwr,  " Net order amount.
         bsart TYPE ekko-bsart,  " Document type         10.01.2020
         batxt TYPE t161t-batxt, " Doc Type Description. 10.01.2020
       END OF ty_ekpo.

TYPES: BEGIN OF ty_obj_id,
         archiv_id TYPE saeobjid,
       END OF ty_obj_id.
TYPES: BEGIN OF ty_obj_id_sel,
         archiv_id TYPE sibfboriid,
       END OF ty_obj_id_sel.
DATA lt_obj_id TYPE STANDARD TABLE OF ty_obj_id.
DATA lw_obj_id TYPE ty_obj_id.
DATA lt_obj_id_sel TYPE STANDARD TABLE OF ty_obj_id_sel.
DATA lw_obj_id_sel TYPE ty_obj_id_sel.
DATA: it_final  TYPE TABLE OF ty_final,
      wa_final  TYPE ty_final,
      it_final1 TYPE TABLE OF ty_final,
      wa_final1 TYPE ty_final,
      it_final2 TYPE TABLE OF ty_final,
      wa_final2 TYPE ty_final.
"it_ekpo   TYPE TABLE OF ty_ekpo,
"wa_ekpo   TYPE ty_ekpo.
DATA: idx TYPE sy-tabix.
TYPES: BEGIN OF ty_bkpf,
         belnr TYPE  bkpf-belnr,
         gjahr TYPE  bkpf-gjahr,
         cpudt TYPE  bkpf-cpudt,
         budat TYPE  bkpf-budat,
         usnam TYPE bkpf-usnam, " User name
       END OF ty_bkpf.
DATA: it_bkpf TYPE TABLE OF ty_bkpf,
      wa_bkpf TYPE ty_bkpf.

TYPES: BEGIN OF ty_un,
         bname      TYPE usr21-bname,
         persnumber TYPE usr21-persnumber,
         name_text  TYPE adrp-name_text,
       END OF ty_un.
DATA: it_un TYPE TABLE OF ty_un,
      wa_un TYPE ty_un.

TYPES: BEGIN OF ty_skat,
         txt20 TYPE  skat-txt20,
         saknr TYPE  skat-saknr,
       END OF ty_skat.

DATA: it_skat TYPE TABLE OF ty_skat,
      wa_skat TYPE ty_skat.

TYPES: BEGIN OF ty_t003t,
         blart TYPE t003t-blart,
         ltext TYPE t003t-ltext,
       END OF ty_t003t.

DATA: it_t003t TYPE TABLE OF ty_t003t,
      wa_t003t TYPE ty_t003t.

TYPES: BEGIN OF ty_t007s,
         mwskz TYPE t007s-mwskz,
         text1 TYPE t007s-text1,
       END OF ty_t007s.

DATA: it_t007s TYPE TABLE OF ty_t007s,
      wa_t007s TYPE ty_t007s.

TYPES: BEGIN OF ty_cskt,
*  ABTEI TYPE  CSKS-ABTEI,
         ktext TYPE  cskt-ktext,
         kostl TYPE  cskt-kostl,
       END OF ty_cskt.

DATA: it_cskt TYPE TABLE OF ty_cskt,
      wa_cskt TYPE ty_cskt.

TYPES: BEGIN OF ty_aufk,
*  AUFNR TYPE  AUFK-AUFNR,
         kostv TYPE  aufk-kostv,
       END OF ty_aufk.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         rcomp TYPE t001-rcomp,

       END OF ty_t001.

TYPES: BEGIN OF ty_anla,
         txt50 TYPE anla-txt50,
       END OF ty_anla.

TYPES: BEGIN OF ty_tax, " Business area in Tax table ( Modified on 22.08.2019)
         rbusa TYPE faglflexa-rbusa,
       END OF ty_tax.

DATA: "it_aufk TYPE TABLE OF ty_aufk,
  "wa_aufk TYPE ty_aufk,
  it_t001 TYPE TABLE OF ty_t001,
  wa_t001 TYPE ty_t001.
"it_anla TYPE TABLE OF ty_anla,
"wa_anla TYPE ty_anla.
*      it_tax  TYPE TABLE OF ty_tax,
*      wa_tax  TYPE ty_tax.


TYPES: BEGIN OF ty_bseg,
         belnr TYPE bseg-belnr,
         buzei TYPE bseg-buzei,    " Line item
         lifnr TYPE bseg-lifnr,
         kunnr TYPE bseg-kunnr,
         matnr TYPE bseg-matnr,    " Materials code
         menge TYPE bseg-menge,    " Qty
         meins TYPE bseg-meins,    " UOm
         anln1 TYPE bseg-anln1,    " Asset
         anln2 TYPE bseg-anln2,    " Subnumber
         bpmng TYPE bseg-bpmng,    " Qty in OPUn
         bprme TYPE bseg-bprme,    " PO Price Unit
         ebeln TYPE bseg-ebeln,    " Purchasing Doc.
         ebelp TYPE bseg-ebelp,    " Item
       END OF ty_bseg.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         bukrs TYPE knb1-bukrs,
       END OF ty_kna1.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         bukrs TYPE lfb1-bukrs,
       END OF ty_lfa1.

DATA: it_kna1 TYPE TABLE OF ty_kna1,
      wa_kna1 TYPE ty_kna1,
      it_lfa1 TYPE TABLE OF ty_lfa1,
      wa_lfa1 TYPE ty_lfa1.
"it_bseg TYPE TABLE OF ty_bseg,
"wa_bseg TYPE ty_bseg.

DATA: doc TYPE bseg-belnr,
      vno TYPE bseg-lifnr,
      vna TYPE kna1-name1,
      cno TYPE bseg-kunnr,
      cna TYPE lfa1-name1,
      ino TYPE i,
      dat TYPE t001-rcomp,
      s1  TYPE string,
      s2  TYPE string,
      s3  TYPE string.

TYPES: BEGIN OF ty_wtx,
         bukrs    TYPE with_item-bukrs,
         belnr    TYPE with_item-belnr,
         gjahr    TYPE with_item-gjahr,
         witht    TYPE with_item-witht,                        "W/Tax Type
         wt_qsshb TYPE with_item-wt_qsshb,                 " -W/Tax Base
         wt_qbshb TYPE with_item-wt_qbshb,                 "- W/Tax Amt
         wt_qsshh TYPE with_item-wt_qsshh,                             "- W/Tax Base LC
         wt_qbshh TYPE with_item-wt_qbshh,                             "- W/Tax Amt LC  amount equal
       END OF ty_wtx.

DATA: it_wtx TYPE TABLE OF ty_wtx,
      wa_wtx TYPE ty_wtx.

TYPES: BEGIN OF tyn_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey,
         ppnam TYPE bkpf-ppnam,  " parked
         usnam TYPE bkpf-usnam,  " Posted
         tcode TYPE bkpf-tcode,
       END OF tyn_bkpf.

DATA : it_tyn      TYPE STANDARD TABLE OF tyn_bkpf,
       wa_tyn      TYPE tyn_bkpf,
       wa_tyn_park TYPE tyn_bkpf,
       wa_tyn_post TYPE tyn_bkpf.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: rb_3 RADIOBUTTON GROUP rg2, " W ithout Vendor & costumer.
              rb_4 RADIOBUTTON GROUP rg2 DEFAULT 'X'. " With Venfor & Costumer.
SELECTION-SCREEN: END OF BLOCK b2.

***** Add by Viknedh on 22.10.2020 due to New method Parking and Posting.
*  SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE Text-003.
*    PARAMETERS:  RB_5 RADIOBUTTON GROUP rg3, " Without Parking & Posting.
*                 RB_6 RADIOBUTTON GROUP rg3 DEFAULT 'X'. " New method Parking & Posting.
*   SELECTION-SCREEN: END OF BLOCK b3.
***** Add by Viknedh on 22.10.2020 due to New method Parking and Posting.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*  SELECTION-SCREEN : SKIP 1.
  SELECT-OPTIONS: s_hkont FOR bsis-hkont,
                  s_kostl FOR bsis-kostl,
                  s_bukrs FOR bsis-bukrs OBLIGATORY,
                  s_budat FOR bsis-budat OBLIGATORY.
  PARAMETERS:     s_gjahr TYPE bsis-gjahr OBLIGATORY.
  PARAMETERS: rb_1 RADIOBUTTON GROUP rg1 DEFAULT 'X', "  Posting date
              rb_2 RADIOBUTTON GROUP rg1.             " Entry date.

SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN : SKIP 1.


START-OF-SELECTION.


*  IF s_budat-low NE s_budat-high .
*    IF s_hkont IS INITIAL AND s_kostl IS INITIAL.
*      MESSAGE 'Mandatory Inputs eigther G/L No or Coster center for different dates' TYPE 'S' DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.

  SELECT bukrs rcomp FROM t001 INTO TABLE it_t001 WHERE bukrs IN s_bukrs.
  READ TABLE it_t001 INTO wa_t001 INDEX 1.
  dat = wa_t001-rcomp.
  LOOP AT it_t001 INTO wa_t001.
    IF dat NE wa_t001-rcomp.
      ino = 1 + ino.
    ENDIF.
  ENDLOOP.




*perform get_data.
*
*perform processing_data.
*
*perform formatting_data.
*
*perform writing_data.
*
*perform get_data1.
*
*perform processing_data1.
*
*perform formatting_data1.
*
*perform writing_data1.
*
*perform writing_data2.
*
*Doc = 1.
***********************************************
*        call function 'SAPGUI_PROGRESS_INDICATOR'.
*********************************************
  SELECT blart ltext FROM t003t INTO TABLE it_t003t WHERE spras EQ 'EN'. " Add by Vikesh on 03.11.2020.
  SELECT mwskz text1 FROM t007s INTO TABLE it_t007s WHERE spras EQ 'EN' AND kalsm EQ 'TAXIN'. " (ZTAXIN  - Tax procedure for KPPL) Add by Vikesh on 03.11.2020.
  IF ino EQ 0.



    IF rb_1 IS NOT INITIAL . " Posting Date


      SELECT  b~bukrs b~belnr b~buzei b~xblnr b~gjahr b~blart b~monat b~bldat
              b~budat b~shkzg b~gsber b~dmbtr b~pswsl b~mwskz b~waers b~zuonr
              b~sgtxt b~kostl b~aufnr b~hkont b~bschl b~augdt b~augbl
              b~werks b~fipos k~cpudt k~usnam k~ppnam
        INTO TABLE it_final FROM  bsis AS b JOIN bkpf AS k ON k~belnr = b~belnr AND
                                                              b~gjahr = k~gjahr
        WHERE k~budat IN s_budat AND
              k~gjahr = s_gjahr AND
            ( b~bukrs IN s_bukrs  AND k~bukrs IN s_bukrs ).

      APPEND LINES OF it_final TO it_final1.
      CLEAR it_final.

      SELECT  b~bukrs b~belnr b~buzei b~xblnr b~gjahr b~blart b~monat
              b~bldat b~budat b~shkzg b~gsber b~dmbtr
              b~pswsl b~mwskz b~waers b~zuonr
              b~sgtxt b~kostl b~aufnr b~hkont b~bschl b~augdt
              b~augbl b~werks b~fipos k~cpudt k~usnam k~ppnam
            INTO TABLE it_final
            FROM bsas AS b JOIN bkpf AS k ON k~belnr = b~belnr
                                         AND b~gjahr = k~gjahr
            WHERE k~budat IN s_budat  AND
                  k~gjahr = s_gjahr   AND
                ( b~bukrs IN s_bukrs  AND k~bukrs IN s_bukrs ).

      APPEND LINES OF it_final TO it_final1.
      CLEAR it_final.
      it_final[] = it_final1[].

      CLEAR it_final1.
    ELSEIF rb_2 IS NOT INITIAL . " Entry Date.
*  Select BELNR GJAHR CPUDT FROM BKPF INTO TABLE IT_BKPF WHERE CPUDT IN s_BUDAT and GJAHR = s_GJAHR.

      SELECT  b~bukrs b~belnr b~buzei b~xblnr b~gjahr b~blart
              b~monat b~bldat b~budat b~shkzg b~gsber
              b~dmbtr b~pswsl b~mwskz b~waers b~zuonr
              b~sgtxt b~kostl b~aufnr b~hkont b~bschl b~augdt
              b~augbl b~werks b~fipos k~cpudt k~usnam k~ppnam
            INTO CORRESPONDING FIELDS OF TABLE it_final
            FROM bsis AS b INNER JOIN bkpf AS k ON k~belnr = b~belnr AND
                                      b~gjahr = k~gjahr
            WHERE k~cpudt IN s_budat AND
                  k~gjahr = s_gjahr AND
                ( b~bukrs IN s_bukrs AND k~bukrs IN s_bukrs ).

      APPEND LINES OF it_final TO it_final1.
      CLEAR it_final.

      SELECT  b~bukrs b~belnr b~buzei b~xblnr b~gjahr b~blart
              b~monat b~bldat b~budat b~shkzg b~gsber b~dmbtr
              b~pswsl b~mwskz b~waers b~zuonr b~sgtxt b~kostl
              b~aufnr b~hkont b~bschl b~augdt b~augbl b~werks
              b~fipos k~cpudt k~usnam k~ppnam
              INTO CORRESPONDING FIELDS OF TABLE it_final
              FROM  bsas AS b INNER JOIN bkpf AS k ON k~belnr = b~belnr
                                                  AND b~gjahr = k~gjahr
              WHERE k~cpudt IN s_budat
                AND k~gjahr = s_gjahr
                AND ( b~bukrs IN s_bukrs AND k~bukrs IN s_bukrs ).

      APPEND LINES OF it_final TO it_final1.
      CLEAR it_final.
      it_final[] = it_final1[].

      CLEAR it_final1.

    ENDIF.
    IF s_hkont IS NOT INITIAL.
      DELETE it_final WHERE hkont NOT IN s_hkont.
    ENDIF.
    IF s_kostl IS NOT INITIAL.
      DELETE it_final WHERE kostl NOT IN s_kostl.
    ENDIF.
    IF it_final IS INITIAL.
      MESSAGE 'No Data found' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT it_final BY belnr ASCENDING.

    SELECT txt20 saknr INTO TABLE it_skat FROM skat WHERE ktopl = 'YAIN'."dat. " 07.07.2017 add KTOPL = 'KPPL'
    SELECT ktext kostl INTO TABLE it_cskt FROM cskt WHERE kokrs = dat.
*    SELECT ktext kostl INTO TABLE it_cskt FROM cskt WHERE kokrs = dat.
    SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1.
    SELECT lifnr name1 FROM lfa1 INTO TABLE it_lfa1.
    SORT it_skat BY saknr.
    ino = 0.
    SELECT docnr,
           docln,
           rbukrs,
           ryear,
           rbusa FROM faglflexa INTO TABLE @DATA(it_tax)
      FOR ALL ENTRIES IN  @it_final
      WHERE   docnr   EQ @it_final-belnr
*                AND docln   EQ s3
          AND rbukrs  EQ @it_final-bukrs
          AND ryear   EQ @it_final-gjahr .

    SELECT aufnr,
           kostv
      INTO TABLE @DATA(it_aufk)
      FROM aufk
      FOR ALL ENTRIES IN @it_final
      WHERE aufk~aufnr = @it_final-aufnr.

    SELECT belnr,
           buzei,
           lifnr,
           kunnr,
           matnr,
           menge,
           meins,
           anln1,
           anln2,
           bpmng,
           bprme,
           ebeln,
           ebelp
           FROM bseg INTO TABLE @DATA(it_bseg)
           FOR ALL ENTRIES IN @it_final
           WHERE belnr = @it_final-belnr
             AND gjahr = @it_final-gjahr
             AND bukrs = @it_final-bukrs.

    SELECT a~ebeln,
           a~ebelp,
           a~netwr,
           b~bsart,
           c~batxt INTO TABLE @DATA(it_ekpo)
           FROM ekpo AS a INNER JOIN ekko AS b ON  a~ebeln EQ b~ebeln
                          INNER JOIN t161t AS c ON b~bsart EQ c~bsart
       FOR ALL ENTRIES IN @it_final
           WHERE  a~ebeln EQ @it_final-ebeln
              AND a~ebelp EQ @it_final-ebelp
              AND c~spras EQ @sy-langu.
    SELECT anln1,
           anln2,
           txt50 FROM anla INTO TABLE @DATA(it_anla)
       FOR ALL ENTRIES IN @it_final
      WHERE anln1 EQ @it_final-anln1
        AND anln2 EQ @it_final-anln2.

    LOOP AT it_final INTO wa_final.
      lw_obj_id-archiv_id = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.
      APPEND lw_obj_id TO lt_obj_id.
      lw_obj_id_sel-archiv_id = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.
      APPEND lw_obj_id_sel TO lt_obj_id_sel.
    ENDLOOP.

*================================================================
    " Attachments
    SELECT * FROM toa03 INTO TABLE @DATA(it_toa03)
      FOR ALL ENTRIES IN @lt_obj_id
      WHERE sap_object = 'BKPF' AND
            object_id = @lt_obj_id-archiv_id AND
            archiv_id	=	'ZS'.
    IF sy-subrc = 0.
      SORT it_toa03 BY object_id.
      SELECT
        arc_doc_id,
        filename,
        creator,
        descr,
        creatime
        FROM toaat INTO TABLE @DATA(it_toaat)
        FOR ALL ENTRIES IN @it_toa03
        WHERE arc_doc_id = @it_toa03-arc_doc_id .
    ENDIF.
    SELECT *
        FROM srgbtbrel
        INTO TABLE @DATA(it_srgbtbrel)
    FOR ALL ENTRIES IN @lt_obj_id_sel
      WHERE instid_a = @lt_obj_id_sel-archiv_id.
    SORT it_srgbtbrel BY instid_a.
*================================================================
    SORT it_anla BY anln1 anln2.
    SORT it_ekpo BY ebeln ebelp.
    SORT it_cskt BY kostl.
    SORT it_tax BY docnr.
    SORT it_final BY belnr.
    SORT it_aufk BY aufnr.
    SORT it_bseg BY belnr buzei.
    SORT it_kna1 BY kunnr.
    SORT it_lfa1 BY lifnr.
    IF s_hkont IS NOT INITIAL AND s_kostl IS INITIAL.  " Input data given only for GL and not cost center
      LOOP AT it_final INTO wa_final WHERE hkont IN s_hkont.

        IF wa_final-gsber IS INITIAL.
          s1 = wa_final-buzei.
          s2 = '000'.
          CONCATENATE s2 s1 INTO s3.
*          SELECT rbusa FROM faglflexa INTO TABLE it_tax
*            WHERE   docnr   EQ wa_final-belnr
*                AND docln   EQ s3
*                AND rbukrs  EQ wa_final-bukrs
*                AND ryear   EQ wa_final-gjahr .
          READ TABLE it_tax INTO DATA(wa_tax) WITH KEY  docnr   = wa_final-belnr
                                                        docln   = s3
                                                        rbukrs  = wa_final-bukrs
                                                        ryear   = wa_final-gjahr BINARY SEARCH.
          wa_final-gsber = wa_tax-rbusa.
          CLEAR: wa_tax,s1,s2,s3.
        ENDIF.

        idx = sy-tabix.
        IF wa_final-shkzg = 'H'.
          wa_final-dmbtr = wa_final-dmbtr * '-1'.
        ENDIF.
        READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_final-hkont BINARY SEARCH.
        wa_final-txt20 = wa_skat-txt20.
        IF wa_final-kostl = '' AND wa_final-aufnr = ''.
          wa_final-kostl = ''.
          wa_final-aufnr = ''.
        ELSEIF wa_final-kostl = ''.
*          SELECT kostv INTO TABLE it_aufk FROM aufk
*            WHERE aufk~aufnr = wa_final-aufnr.
          READ TABLE it_aufk INTO DATA(wa_aufk) WITH KEY aufnr = wa_final-aufnr BINARY SEARCH.
          wa_final-kostl = wa_aufk-kostv.
        ENDIF.
        READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_final-kostl BINARY SEARCH.
        wa_final-ktext = wa_cskt-ktext.
*      If RB_4 IS NOT INITIAL.



*        SELECT belnr,
*               buzei,
*               lifnr,
*               kunnr,
*               matnr,
*               menge,
*               meins,
*               anln1,
*               anln2,
*               bpmng,
*               bprme,
*               ebeln,
*               ebelp
*          FROM bseg INTO TABLE @DATA(it_bseg)
*          WHERE belnr = wa_final-belnr
*            AND gjahr = wa_final-gjahr
*            AND bukrs = wa_final-bukrs.
        READ TABLE it_bseg INTO DATA(wa_bseg) WITH KEY belnr = wa_final-belnr buzei = wa_final-buzei BINARY SEARCH.
        wa_final-matnr = wa_bseg-matnr.
        wa_final-menge = wa_bseg-menge.
        wa_final-meins = wa_bseg-meins.
        wa_final-anln1 = wa_bseg-anln1.
        wa_final-anln2 = wa_bseg-anln2.
********************************************** 29.11.2019
        wa_final-bpmng = wa_bseg-bpmng.
        wa_final-bprme = wa_bseg-bprme.
        wa_final-ebeln = wa_bseg-ebeln.
        wa_final-ebelp = wa_bseg-ebelp.
********************************************** 10.01.2020
*        SELECT a~netwr b~bsart c~batxt INTO TABLE it_ekpo
*          FROM ekpo AS a INNER JOIN ekko AS b ON  a~ebeln EQ b~ebeln
*                         INNER JOIN t161t AS c ON b~bsart EQ c~bsart
*           WHERE a~ebeln EQ wa_final-ebeln AND a~ebelp EQ wa_final-ebelp AND c~spras EQ 'EN'.
        READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH KEY ebeln = wa_final-ebeln
                                                       ebelp = wa_final-ebelp BINARY SEARCH.
        wa_final-netwr = wa_ekpo-netwr.
*              WA_FINAL-BSART = WA_EKPO-BSART.
        wa_final-batxt = wa_ekpo-batxt.
        CLEAR: wa_ekpo, it_ekpo.
********************************************** 10.01.2020
********************************************** 29.11.2019
***************************************** 16.07.2019
*        SELECT txt50 FROM anla INTO TABLE it_anla WHERE anln1 EQ wa_final-anln1 AND anln2 EQ wa_final-anln2.
        READ TABLE it_anla INTO DATA(wa_anla) WITH KEY anln1 = wa_final-anln1
                                                       anln2 = wa_final-anln2 BINARY SEARCH.
        wa_final-txt50 = wa_anla-txt50.
***************************************** 16.07.2019
        IF rb_4 IS NOT INITIAL.
          IF doc NE wa_final-belnr.
            CLEAR: vno,vna,cno,cna.

            DATA(it_bseg_t) = it_bseg .
            DELETE  it_bseg_t WHERE belnr NE wa_final-belnr.
            SORT it_bseg_t BY lifnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-lifnr =  wa_bseg-lifnr.
            vno =  wa_bseg-lifnr.
            SORT it_bseg_t BY kunnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-kunnr =  wa_bseg-kunnr.
            cno =  wa_bseg-kunnr.
            READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_final-kunnr BINARY SEARCH.
            wa_final-name1 = wa_kna1-name1.
            cna = wa_kna1-name1.
            READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr BINARY SEARCH.
            wa_final-vend2 = wa_lfa1-name1.
            vna = wa_lfa1-name1.
            doc = wa_final-belnr.
          ELSE.
            wa_final-lifnr = vno.
            wa_final-vend2 = vna.
            wa_final-kunnr = cno.
            wa_final-name1 = cna.
          ENDIF.

        ENDIF.
        PERFORM user_name.
*********************************** Attachment Update Start

*       Break Abap.

*       DATA : COLLECTSTR TYPE STRING.

*        CONCATENATE wa_final-bukrs wa_final-belnr wa_final-gjahr INTO collectstr.


*       TYPES: BEGIN OF TY_ATTA,
*              INSTID_A TYPE SRGBTBREL-INSTID_A,
*              CATID_B TYPE SRGBTBREL-CATID_A,
*             END OF TY_ATTA.
*             Data: IT_ATTA TYPE TABLE OF TY_ATTA,
*                   WA_ATTA TYPE TY_ATTA.


        IF wa_final-blart = 'CL' OR wa_final-blart = 'ZR' OR wa_final-blart = 'RD' OR wa_final-blart = 'AF'
                  OR wa_final-blart = 'DA' OR wa_final-blart = 'WA' OR wa_final-blart = 'WE' OR wa_final-blart = 'DR'
          OR wa_final-blart = 'DZ' OR wa_final-blart = 'DG' OR wa_final-blart = 'RF' OR wa_final-blart = 'SA' OR wa_final-blart = 'AB'.

          wa_final-atta = 'Not Required'.

        ELSEIF  wa_final-blart = 'SK' AND wa_final-hkont = 27000002 OR ( wa_final-blart = 'SK' AND wa_final-kunnr = 5600069 ).

          wa_final-atta = 'Not Required'.

        ELSE.

          DATA(l_obj_id) = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.

          READ TABLE it_toa03 INTO DATA(wa_toaat) WITH KEY object_id = l_obj_id BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-atta = 'Y'.
          ELSE.
            READ TABLE it_srgbtbrel INTO DATA(wa_srgbtbrel) WITH KEY instid_a = l_obj_id BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-atta = 'Y'.
            ELSE.
              wa_final-atta = 'N'.
            ENDIF.
          ENDIF.

*          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = collectstr.
*          IF sy-subrc = 0.
*            wa_final-atta = 'Y'.
*          ELSE.
********************************************************* Getting Refernce No From BKPF
*            SELECT  bukrs belnr gjahr awkey ppnam usnam tcode
*                      INTO TABLE it_tyn FROM bkpf
*               WHERE belnr = wa_final-belnr AND gjahr = s_gjahr." AND BUKRS = S_BUKRS.
*            IF sy-subrc = 0.
*              READ TABLE it_tyn INTO wa_tyn INDEX sy-tabix.
*              SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = wa_tyn-awkey.
*              IF sy-subrc = 0.
*                wa_final-atta = 'Y'.
*              ELSE.
*                wa_final-atta = 'N'.
*              ENDIF.
********************************************************************************
*            ENDIF.
*          ENDIF.
        ENDIF.
********************************** Attachment Update End
        READ TABLE it_t003t INTO wa_t003t WITH KEY blart = wa_final-blart.
        wa_final-ltext = wa_t003t-ltext.
        READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final-mwskz.
        wa_final-text1 = wa_t007s-text1.
        APPEND wa_final TO it_final1.
        CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, wa_anla, it_un, wa_un, wa_t007s, wa_t003t .
      ENDLOOP.
      CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, it_final, it_cskt, it_aufk, it_skat.
      it_final[] = it_final1[].
      CLEAR:wa_final1, it_final1.
    ENDIF.

    IF s_hkont IS INITIAL AND s_kostl IS NOT INITIAL.  " Input data given only for cost center
      LOOP AT it_final INTO wa_final.
        IF wa_final-gsber IS INITIAL.
          s1 = wa_final-buzei.
          s2 = '000'.
          CONCATENATE s2 s1 INTO s3.
*          SELECT rbusa FROM faglflexa
*            INTO TABLE it_tax
*            WHERE   docnr EQ wa_final-belnr
*                AND docln EQ s3
*                AND rbukrs EQ wa_final-bukrs
*                AND ryear EQ wa_final-gjahr .
          READ TABLE it_tax INTO wa_tax WITH KEY  docnr   = wa_final-belnr
                                                  docln   = s3
                                                  rbukrs  = wa_final-bukrs
                                                  ryear   = wa_final-gjahr .
          wa_final-gsber = wa_tax-rbusa.
          CLEAR: wa_tax,s1,s2,s3.
        ENDIF.

        idx = sy-tabix.
        IF wa_final-shkzg = 'H'.
          wa_final-dmbtr = wa_final-dmbtr * '-1'.
        ENDIF.
        READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_final-hkont  BINARY SEARCH..
        wa_final-txt20 = wa_skat-txt20.
        IF wa_final-kostl = '' AND wa_final-aufnr = ''.
          wa_final-kostl = ''.
          wa_final-aufnr = ''.
        ELSEIF wa_final-kostl = ''.
*          SELECT kostv INTO TABLE it_aufk FROM aufk WHERE aufk~aufnr = wa_final-aufnr.
          READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_final-aufnr BINARY SEARCH.
          wa_final-kostl = wa_aufk-kostv.
        ENDIF.
        READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_final-kostl BINARY SEARCH.
        wa_final-ktext = wa_cskt-ktext.

*        SELECT belnr buzei lifnr  kunnr matnr menge meins anln1 anln2 bpmng bprme ebeln ebelp FROM bseg INTO TABLE  " Add BPMNG BPRME EBELN EBELP 29.11.2019
*         it_bseg WHERE belnr = wa_final-belnr  AND gjahr = wa_final-gjahr AND bukrs = wa_final-bukrs. "BUKRS IN S_BUKRS (05.01.2021).
        READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_final-belnr
                                                 buzei = wa_final-buzei.
        wa_final-matnr = wa_bseg-matnr.
        wa_final-menge = wa_bseg-menge.
        wa_final-meins = wa_bseg-meins.
        wa_final-anln1 = wa_bseg-anln1.
        wa_final-anln2 = wa_bseg-anln2.
********************************************** 29.11.2019
        wa_final-bpmng = wa_bseg-bpmng.
        wa_final-bprme = wa_bseg-bprme.
        wa_final-ebeln = wa_bseg-ebeln.
        wa_final-ebelp = wa_bseg-ebelp.
********************************************** 10.01.2020
*        SELECT a~netwr b~bsart c~batxt INTO TABLE it_ekpo
*          FROM ekpo AS a INNER JOIN ekko AS b ON  a~ebeln EQ b~ebeln
*                         INNER JOIN t161t AS c ON b~bsart EQ c~bsart
*           WHERE a~ebeln EQ wa_final-ebeln AND a~ebelp EQ wa_final-ebelp AND c~spras EQ 'EN'.
        READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_final-ebeln
                                                 ebelp = wa_final-ebelp BINARY SEARCH.
        wa_final-netwr = wa_ekpo-netwr.
*              WA_FINAL-BSART = WA_EKPO-BSART.
        wa_final-batxt = wa_ekpo-batxt.
        CLEAR: wa_ekpo, it_ekpo.
********************************************** 10.01.2020
********************************************** 29.11.2019
***************************************** 16.07.2019
*        SELECT txt50 FROM anla INTO TABLE it_anla WHERE anln1 EQ wa_final-anln1 AND anln2 EQ wa_final-anln2.
        READ TABLE it_anla INTO wa_anla WITH KEY anln1 = wa_final-anln1
                                                 anln2 = wa_final-anln2 BINARY SEARCH.
        wa_final-txt50 = wa_anla-txt50.
***************************************** 16.07.2019
        IF rb_4 IS NOT INITIAL.
          IF doc NE wa_final-belnr.
            CLEAR: vno,vna,cno,cna.
            it_bseg_t = it_bseg .
            DELETE  it_bseg_t WHERE belnr NE wa_final-belnr.
            SORT it_bseg_t BY lifnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-lifnr =  wa_bseg-lifnr.
            vno =  wa_bseg-lifnr.
            SORT it_bseg_t BY kunnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-kunnr =  wa_bseg-kunnr.
            cno =  wa_bseg-kunnr.
            READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_final-kunnr BINARY SEARCH.
            wa_final-name1 = wa_kna1-name1.
            cna = wa_kna1-name1.
            READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr BINARY SEARCH.
            wa_final-vend2 = wa_lfa1-name1.
            vna = wa_lfa1-name1.
            doc = wa_final-belnr.
          ELSE.
            wa_final-lifnr = vno.
            wa_final-vend2 = vna.
            wa_final-kunnr = cno.
            wa_final-name1 = cna.

          ENDIF.

        ENDIF.
        PERFORM user_name.
*********************************** Attachment Update Start

*       Break Abap.

*       DATA : COLLECTSTR TYPE STRING.

        CONCATENATE wa_final-bukrs wa_final-belnr wa_final-gjahr INTO collectstr.

*       TYPES: BEGIN OF TY_ATTA,
*              INSTID_A TYPE SRGBTBREL-INSTID_A,
*              CATID_B TYPE SRGBTBREL-CATID_A,
*             END OF TY_ATTA.
*             Data: IT_ATTA TYPE TABLE OF TY_ATTA,
*                   WA_ATTA TYPE TY_ATTA.



        IF wa_final-blart = 'CL' OR wa_final-blart = 'ZR' OR wa_final-blart = 'RD' OR wa_final-blart = 'AF'
                OR wa_final-blart = 'DA' OR wa_final-blart = 'WA' OR wa_final-blart = 'WE' OR wa_final-blart = 'DR'
        OR wa_final-blart = 'DZ' OR wa_final-blart = 'DG' OR wa_final-blart = 'RF' OR wa_final-blart = 'SA' OR wa_final-blart = 'AB'.

          wa_final-atta = 'Not Required'.

        ELSEIF  wa_final-blart = 'SK' AND wa_final-hkont = 27000002 OR ( wa_final-blart = 'SK' AND wa_final-kunnr = 5600069 ).

          wa_final-atta = 'Not Required'.

        ELSE.


          l_obj_id = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.

          READ TABLE it_toa03 INTO wa_toaat WITH KEY object_id = l_obj_id BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-atta = 'Y'.
          ELSE.
            READ TABLE it_srgbtbrel INTO wa_srgbtbrel  WITH KEY instid_a = l_obj_id BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-atta = 'Y'.
            ELSE.
              wa_final-atta = 'N'.
            ENDIF.
          ENDIF.

*          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = collectstr.
*
*          IF sy-subrc = 0.
*            wa_final-atta = 'Y'.
*          ELSE.
********************************************************* Getting Refernce No From BKPF
*
*
*            SELECT  bukrs belnr gjahr awkey ppnam usnam tcode
*                      INTO TABLE it_tyn FROM bkpf
*               WHERE belnr = wa_final-belnr AND gjahr = s_gjahr." AND BUKRS = S_BUKRS.
*            IF sy-subrc = 0.
*
*              READ TABLE it_tyn INTO wa_tyn INDEX sy-tabix.
*
*              SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = wa_tyn-awkey.
*              IF sy-subrc = 0.
*
*                wa_final-atta = 'Y'.
*              ELSE.
*
*                wa_final-atta = 'N'.
*
*              ENDIF.
*            ENDIF.
*          ENDIF.

        ENDIF.



********************************** Attachment Update End
        READ TABLE it_t003t INTO wa_t003t WITH KEY blart = wa_final-blart.
        wa_final-ltext = wa_t003t-ltext.
        READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final-mwskz.
        wa_final-text1 = wa_t007s-text1.
        APPEND wa_final TO it_final1.
        CLEAR: idx , wa_final, wa_cskt, wa_aufk,  wa_skat, wa_kna1, wa_lfa1,  wa_anla, it_un, wa_un, wa_t007s, wa_t003t .
      ENDLOOP.
      CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, it_final, it_cskt, it_aufk, it_skat.
      it_final[] = it_final1[].
      CLEAR: wa_final1, it_final1.
      LOOP AT it_final INTO wa_final WHERE kostl IN s_kostl .
        APPEND wa_final TO it_final1.
      ENDLOOP.
      CLEAR it_final.
      it_final[] = it_final1[].
      CLEAR: wa_final1, it_final1.
    ENDIF.

    IF s_hkont IS NOT INITIAL AND s_kostl IS NOT INITIAL.  " Both Input data given cost center & GL
      LOOP AT it_final INTO wa_final WHERE hkont IN s_hkont.
        IF wa_final-gsber IS INITIAL.
          s1 = wa_final-buzei.
          s2 = '000'.
          CONCATENATE s2 s1 INTO s3.
*          SELECT rbusa FROM faglflexa
*              INTO TABLE it_tax
*            WHERE   docnr   EQ wa_final-belnr
*                AND docln   EQ s3
*                AND rbukrs  EQ wa_final-bukrs
*                AND ryear   EQ wa_final-gjahr .
          READ TABLE it_tax INTO wa_tax WITH KEY  docnr   = wa_final-belnr
                                                  docln   = s3
                                                  rbukrs  = wa_final-bukrs
                                                  ryear   = wa_final-gjahr .
          wa_final-gsber = wa_tax-rbusa.
          CLEAR: wa_tax,s1,s2,s3.
        ENDIF.
        idx = sy-tabix.
        IF wa_final-shkzg = 'H'.
          wa_final-dmbtr = wa_final-dmbtr * '-1'.
        ENDIF.
        READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_final-hkont  BINARY SEARCH..
        wa_final-txt20 = wa_skat-txt20.
        IF wa_final-kostl = '' AND wa_final-aufnr = ''.
          wa_final-kostl = ''.
          wa_final-aufnr = ''.
        ELSEIF wa_final-kostl = ''.
*          SELECT kostv INTO TABLE it_aufk FROM aufk WHERE aufk~aufnr = wa_final-aufnr.
          READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_final-aufnr BINARY SEARCH.
          wa_final-kostl = wa_aufk-kostv.
        ENDIF.
        READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_final-kostl BINARY SEARCH.
        wa_final-ktext = wa_cskt-ktext.
*      If RB_4 IS NOT INITIAL.
*        SELECT belnr buzei lifnr  kunnr matnr menge meins anln1 anln2 bpmng bprme ebeln ebelp FROM bseg INTO TABLE  " Add BPMNG BPRME EBELN EBELP 29.11.2019
*         it_bseg WHERE belnr = wa_final-belnr  AND gjahr = wa_final-gjahr AND bukrs = wa_final-bukrs. "BUKRS IN S_BUKRS (05.01.2021)
*        READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_final-belnr buzei = wa_final-buzei.
        READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_final-belnr buzei = wa_final-buzei BINARY SEARCH.
        wa_final-matnr = wa_bseg-matnr.
        wa_final-menge = wa_bseg-menge.
        wa_final-meins = wa_bseg-meins.
        wa_final-anln1 = wa_bseg-anln1.
        wa_final-anln2 = wa_bseg-anln2.
********************************************** 29.11.2019
        wa_final-bpmng = wa_bseg-bpmng.
        wa_final-bprme = wa_bseg-bprme.
        wa_final-ebeln = wa_bseg-ebeln.
        wa_final-ebelp = wa_bseg-ebelp.
********************************************** 10.01.2020
*        SELECT a~netwr b~bsart c~batxt INTO TABLE it_ekpo
*          FROM ekpo AS a INNER JOIN ekko AS b ON  a~ebeln EQ b~ebeln
*                         INNER JOIN t161t AS c ON b~bsart EQ c~bsart
*           WHERE a~ebeln EQ wa_final-ebeln AND a~ebelp EQ wa_final-ebelp AND c~spras EQ 'EN'.
        READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_final-ebeln
                                                 ebelp = wa_final-ebelp BINARY SEARCH.
        wa_final-netwr = wa_ekpo-netwr.
*              WA_FINAL-BSART = WA_EKPO-BSART.
        wa_final-batxt = wa_ekpo-batxt.
        CLEAR: wa_ekpo, it_ekpo.
********************************************** 10.01.2020
********************************************** 29.11.2019
***************************************** 16.07.2019
*        SELECT txt50 FROM anla INTO TABLE it_anla WHERE anln1 EQ wa_final-anln1 AND anln2 EQ wa_final-anln2.
        READ TABLE it_anla INTO wa_anla WITH KEY anln1 = wa_final-anln1
                                                 anln2 = wa_final-anln2 BINARY SEARCH.
        wa_final-txt50 = wa_anla-txt50.
***************************************** 16.07.2019
        IF rb_4 IS NOT INITIAL.
          IF doc NE wa_final-belnr.
            CLEAR: vno,vna,cno,cna.
            it_bseg_t = it_bseg .
            DELETE  it_bseg_t WHERE belnr NE wa_final-belnr.
            SORT it_bseg_t BY lifnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-lifnr =  wa_bseg-lifnr.
            vno =  wa_bseg-lifnr.
            SORT it_bseg_t BY kunnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-kunnr =  wa_bseg-kunnr.
            cno =  wa_bseg-kunnr.
            READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_final-kunnr BINARY SEARCH.
            wa_final-name1 = wa_kna1-name1.
            cna = wa_kna1-name1.
            READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr BINARY SEARCH.
            wa_final-vend2 = wa_lfa1-name1.
            vna = wa_lfa1-name1.
            doc = wa_final-belnr.
          ELSE.
            wa_final-lifnr = vno.
            wa_final-vend2 = vna.
            wa_final-kunnr = cno.
            wa_final-name1 = cna.
          ENDIF.
        ENDIF.
        PERFORM user_name.
*********************************** Attachment Update Start


        CONCATENATE wa_final-bukrs wa_final-belnr wa_final-gjahr INTO collectstr.





        IF wa_final-blart = 'CL' OR wa_final-blart = 'ZR' OR wa_final-blart = 'RD' OR wa_final-blart = 'AF'
                OR wa_final-blart = 'DA' OR wa_final-blart = 'WA' OR wa_final-blart = 'WE' OR wa_final-blart = 'DR'
        OR wa_final-blart = 'DZ' OR wa_final-blart = 'DG' OR wa_final-blart = 'RF' OR wa_final-blart = 'SA' OR wa_final-blart = 'AB'.

          wa_final-atta = 'Not Required'.

        ELSEIF  wa_final-blart = 'SK' AND wa_final-hkont = 27000002 OR ( wa_final-blart = 'SK' AND wa_final-kunnr = 5600069 ).

          wa_final-atta = 'Not Required'.

        ELSE.

          l_obj_id = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.

          READ TABLE it_toa03 INTO wa_toaat  WITH KEY object_id = l_obj_id BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-atta = 'Y'.
          ELSE.
            READ TABLE it_srgbtbrel INTO wa_srgbtbrel WITH KEY instid_a = l_obj_id BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-atta = 'Y'.
            ELSE.
              wa_final-atta = 'N'.
            ENDIF.
          ENDIF.
*          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = collectstr.
*
*          IF sy-subrc = 0.
*            wa_final-atta = 'Y'.
*          ELSE.
*
*            SELECT  bukrs belnr gjahr awkey ppnam usnam tcode
*                      INTO TABLE it_tyn FROM bkpf
*               WHERE belnr = wa_final-belnr AND gjahr = s_gjahr." AND BUKRS = S_BUKRS.
*            IF sy-subrc = 0.
*
*              READ TABLE it_tyn INTO wa_tyn INDEX sy-tabix.
*
*              SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = wa_tyn-awkey.
*              IF sy-subrc = 0.
*
*                wa_final-atta = 'Y'.
*              ELSE.
*
*                wa_final-atta = 'N'.
*
*              ENDIF.
*            ENDIF.
*
*
*          ENDIF.

        ENDIF.



********************************** Attachment Update End
        READ TABLE it_t003t INTO wa_t003t WITH KEY blart = wa_final-blart.
        wa_final-ltext = wa_t003t-ltext.
        READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final-mwskz.
        wa_final-text1 = wa_t007s-text1.
        APPEND wa_final TO it_final1.
        CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, wa_anla, it_un, wa_un, wa_t007s, wa_t003t .
      ENDLOOP.
      CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, it_final, it_cskt, it_aufk, it_skat,it_bseg_t.
      it_final[] = it_final1[].
      CLEAR: wa_final1, it_final1.
      LOOP AT it_final INTO wa_final WHERE kostl IN s_kostl.
        APPEND wa_final TO it_final1.
      ENDLOOP.
      CLEAR it_final.
      it_final[] = it_final1[].
      CLEAR: wa_final1, it_final1.
    ENDIF.

    IF s_hkont IS INITIAL AND s_kostl IS INITIAL.  " Not cost center & GL
      LOOP AT it_final INTO wa_final.
        IF wa_final-gsber IS INITIAL.
          s1 = wa_final-buzei.
          s2 = '000'.
          CONCATENATE s2 s1 INTO s3.
*          SELECT rbusa FROM faglflexa
*            INTO TABLE it_tax
*            WHERE   docnr EQ wa_final-belnr
*                AND docln EQ s3
*                AND rbukrs EQ wa_final-bukrs
*                AND ryear EQ wa_final-gjahr .
          READ TABLE it_tax INTO wa_tax WITH KEY  docnr   = wa_final-belnr
                                                  docln   = s3
                                                  rbukrs  = wa_final-bukrs
                                                  ryear   = wa_final-gjahr .
          wa_final-gsber = wa_tax-rbusa.
          CLEAR: wa_tax,s1,s2,s3.
        ENDIF.
        idx = sy-tabix.
        IF wa_final-shkzg = 'H'.
          wa_final-dmbtr = wa_final-dmbtr * '-1'.
        ENDIF.
        READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_final-hkont  BINARY SEARCH..
        wa_final-txt20 = wa_skat-txt20.
        IF wa_final-kostl = '' AND wa_final-aufnr = ''.
          wa_final-kostl = ''.
          wa_final-aufnr = ''.
        ELSEIF wa_final-kostl = ''.
*          SELECT kostv INTO TABLE it_aufk FROM aufk WHERE aufk~aufnr = wa_final-aufnr.
          READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_final-aufnr BINARY SEARCH.
          wa_final-kostl = wa_aufk-kostv.
        ENDIF.
        READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_final-kostl BINARY SEARCH.
        wa_final-ktext = wa_cskt-ktext.
*      If RB_4 IS NOT INITIAL.
*        SELECT belnr buzei lifnr  kunnr matnr menge meins anln1 anln2 bpmng bprme ebeln ebelp FROM bseg INTO TABLE  " Add BPMNG BPRME EBELN EBELP 29.11.2019
*         it_bseg WHERE belnr = wa_final-belnr  AND gjahr = wa_final-gjahr AND bukrs = wa_final-bukrs. "BUKRS IN S_BUKRS (05.01.2021)
*        READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_final-belnr buzei = wa_final-buzei.
        READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_final-belnr
                                                 buzei = wa_final-buzei BINARY SEARCH.
        wa_final-matnr = wa_bseg-matnr.
        wa_final-menge = wa_bseg-menge.
        wa_final-meins = wa_bseg-meins.
        wa_final-anln1 = wa_bseg-anln1.
        wa_final-anln2 = wa_bseg-anln2.
********************************************** 29.11.2019
        wa_final-bpmng = wa_bseg-bpmng.
        wa_final-bprme = wa_bseg-bprme.
        wa_final-ebeln = wa_bseg-ebeln.
        wa_final-ebelp = wa_bseg-ebelp.
*            SELECT NETWR FROM EKPO INTO TABLE IT_EKPO  WHERE EBELN EQ WA_FINAL-EBELN AND EBELP EQ WA_FINAL-EBELP.
********************************************** 10.01.2020
*        SELECT a~netwr b~bsart c~batxt INTO TABLE it_ekpo
*          FROM ekpo AS a INNER JOIN ekko AS b ON  a~ebeln EQ b~ebeln
*                         INNER JOIN t161t AS c ON b~bsart EQ c~bsart
*           WHERE a~ebeln EQ wa_final-ebeln AND a~ebelp EQ wa_final-ebelp AND c~spras EQ 'EN'.
        READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_final-ebeln
                                                 ebelp = wa_final-ebelp BINARY SEARCH.
        wa_final-netwr = wa_ekpo-netwr.
*              WA_FINAL-BSART = WA_EKPO-BSART.
        wa_final-batxt = wa_ekpo-batxt.
        CLEAR: wa_ekpo, it_ekpo.
********************************************** 10.01.2020
********************************************** 29.11.2019
***************************************** 16.07.2019
*        SELECT txt50 FROM anla INTO TABLE it_anla WHERE anln1 EQ wa_final-anln1 AND anln2 EQ wa_final-anln2.
        READ TABLE it_anla INTO wa_anla WITH KEY anln1 = wa_final-anln1
                                                 anln2 = wa_final-anln2 BINARY SEARCH.
        wa_final-txt50 = wa_anla-txt50.
***************************************** 16.07.2019
        IF rb_4 IS NOT INITIAL.
          IF doc NE wa_final-belnr.
            CLEAR: vno,vna,cno,cna.
            it_bseg_t = it_bseg .
            DELETE  it_bseg_t WHERE belnr NE wa_final-belnr.
            SORT it_bseg_t BY lifnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-lifnr =  wa_bseg-lifnr.
            vno =  wa_bseg-lifnr.
            SORT it_bseg_t BY kunnr DESCENDING.
            READ TABLE it_bseg_t INTO wa_bseg INDEX 1.
            wa_final-kunnr =  wa_bseg-kunnr.
            cno =  wa_bseg-kunnr.
            READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_final-kunnr BINARY SEARCH.
            wa_final-name1 = wa_kna1-name1.
            cna = wa_kna1-name1.
            READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr BINARY SEARCH.
            wa_final-vend2 = wa_lfa1-name1.
            vna = wa_lfa1-name1.
            doc = wa_final-belnr.
          ELSE.
            wa_final-lifnr = vno.
            wa_final-vend2 = vna.
            wa_final-kunnr = cno.
            wa_final-name1 = cna.
          ENDIF.

        ENDIF.
        PERFORM user_name.
*********************************** Attachment Update Start

*       Break Abap.

*       DATA : COLLECTSTR TYPE STRING.

        CONCATENATE wa_final-bukrs wa_final-belnr wa_final-gjahr INTO collectstr.

*       TYPES: BEGIN OF TY_ATTA,
*              INSTID_A TYPE SRGBTBREL-INSTID_A,
*              CATID_B TYPE SRGBTBREL-CATID_A,
*             END OF TY_ATTA.
*             Data: IT_ATTA TYPE TABLE OF TY_ATTA,
*                   WA_ATTA TYPE TY_ATTA.



        IF wa_final-blart = 'CL' OR wa_final-blart = 'ZR' OR wa_final-blart = 'RD' OR wa_final-blart = 'AF'
                OR wa_final-blart = 'DA' OR wa_final-blart = 'WA' OR wa_final-blart = 'WE' OR wa_final-blart = 'DR'
        OR wa_final-blart = 'DZ' OR wa_final-blart = 'DG' OR wa_final-blart = 'RF' OR wa_final-blart = 'SA' OR wa_final-blart = 'AB'.

          wa_final-atta = 'Not Required'.

        ELSEIF  wa_final-blart = 'SK' AND wa_final-hkont = 27000002 OR ( wa_final-blart = 'SK' AND wa_final-kunnr = 5600069 ).

          wa_final-atta = 'Not Required'.

        ELSE.
          l_obj_id = |{ wa_final-bukrs }{ wa_final-belnr }{ wa_final-gjahr }|.

          READ TABLE it_toa03 INTO wa_toaat WITH KEY object_id = l_obj_id BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-atta = 'Y'.
          ELSE.
            READ TABLE it_srgbtbrel INTO wa_srgbtbrel  WITH KEY instid_a = l_obj_id BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-atta = 'Y'.
            ELSE.
              wa_final-atta = 'N'.
            ENDIF.
          ENDIF.
*
*          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = collectstr.
*
*          IF sy-subrc = 0.
*            wa_final-atta = 'Y'.
*          ELSE.
*
*            SELECT  bukrs belnr gjahr awkey ppnam usnam tcode
*                      INTO TABLE it_tyn FROM bkpf
*               WHERE belnr = wa_final-belnr AND gjahr = s_gjahr." AND BUKRS = S_BUKRS.
*            IF sy-subrc = 0.
*
*              READ TABLE it_tyn INTO wa_tyn INDEX sy-tabix.
*
*              SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = wa_tyn-awkey.
*              IF sy-subrc = 0.
*
*                wa_final-atta = 'Y'.
*              ELSE.
*
*                wa_final-atta = 'N'.
*
*              ENDIF.
*            ENDIF.
*
*          ENDIF.

        ENDIF.



********************************** Attachment Update End
        READ TABLE it_t003t INTO wa_t003t WITH KEY blart = wa_final-blart.
        wa_final-ltext = wa_t003t-ltext.
        READ TABLE it_t007s INTO wa_t007s WITH KEY mwskz = wa_final-mwskz.
        wa_final-text1 = wa_t007s-text1.
        APPEND wa_final TO it_final1.
        CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1,  wa_anla, it_un, wa_un, wa_t007s, wa_t003t,it_bseg_t[] .
      ENDLOOP.
      CLEAR: idx , wa_final, wa_cskt, wa_aufk, wa_skat, wa_kna1, wa_lfa1, it_final, it_cskt, it_aufk, it_skat.
      it_final[] = it_final1[].
      CLEAR: wa_final1, it_final1.
    ENDIF.


************************  Getting And Updaating With holding tax
    LOOP AT it_final INTO  wa_final.

*  IF wa_final-BELNR = 5101000001.
*     Break Abap.
*     endif.

      SELECT
         bukrs
         belnr
         gjahr
         witht
         wt_qsshb
         wt_qbshb
         wt_qsshh
         wt_qbshh FROM with_item INTO TABLE it_wtx WHERE  bukrs = wa_final-bukrs AND belnr = wa_final-belnr
                                           AND gjahr = wa_final-gjahr AND wt_qbshh NE 0.

*    BREAK Abap.

      LOOP AT it_wtx INTO wa_wtx WHERE belnr = wa_final-belnr.


        wa_final-witht = wa_wtx-witht.
        wa_final-wt_qsshb = wa_wtx-wt_qsshb .
        wa_final-wt_qbshb = wa_wtx-wt_qbshb .
        wa_final-wt_qsshh = wa_wtx-wt_qsshh .
        wa_final-wt_qbshh = wa_wtx-wt_qbshh .



*     IF wa_final-BELNR = 5101000001.
*     Break Abap.
*     endif.



        MODIFY it_final FROM wa_final TRANSPORTING witht wt_qsshb wt_qbshb wt_qsshh wt_qbshh  WHERE bukrs = wa_final-bukrs AND belnr = wa_final-belnr
                                           AND gjahr = wa_final-gjahr AND buzei = 1.

        CLEAR : wa_final-bukrs,wa_final-bukrs,wa_final-bukrs,wa_wtx-witht,wa_final-wt_qsshb,wa_final-wt_qbshb
                 ,wa_final-wt_qsshh,wa_final-wt_qbshh.
      ENDLOOP.

    ENDLOOP.



****************************************************************


    " 5101001028
    SORT it_final BY belnr buzei ASCENDING .



    DATA: w_fcat                 TYPE slis_fieldcat_alv,
          i_fcat                 TYPE slis_t_fieldcat_alv,
          layout                 TYPE slis_layout_alv,
          g_top_of_page          TYPE slis_formname VALUE 'F_TOP_OF_PAGE', "for avl header.
          gt_callback_subroutine TYPE slis_formname VALUE 'USER_COMMAND'.
    IF rb_4 IS NOT INITIAL.

      SORT it_final BY belnr buzei.


      w_fcat-fieldname = 'BUKRS'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '1'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BELNR'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '2'.
      w_fcat-hotspot = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BUZEI'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '05'.
      w_fcat-col_pos = '3'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'XBLNR'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '16'.
      w_fcat-col_pos = '4'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.


      w_fcat-fieldname = 'GJAHR'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '5'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.



      w_fcat-fieldname = 'BLART'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '39'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BLDAT'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '7'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BUDAT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '8'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'CPUDT'.
      w_fcat-ref_tabname = 'BKPF'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '9'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'SHKZG'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '3'.
      w_fcat-col_pos = '10'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'DMBTR'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '15'.
      w_fcat-col_pos = '11'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'MWSKZ'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '2'.
      w_fcat-col_pos = '37'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.



      w_fcat-fieldname = 'SGTXT'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '50'.
      w_fcat-col_pos = '13'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'KOSTL'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '14'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'KTEXT'.
      w_fcat-ref_tabname = 'CSKT'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '15'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'AUFNR'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '16'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'HKONT'.
      w_fcat-ref_tabname = 'BSIS'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '17'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TXT20'.
      w_fcat-ref_tabname = 'SKAT'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '40'.
      w_fcat-col_pos = '18'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'MATNR'.
      w_fcat-ref_tabname = 'BSEG'.

*  W_FCAT-Seltext_M = 'Description'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '19'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'MENGE'.
      w_fcat-ref_tabname = 'BSEG'.

*  W_FCAT-Seltext_M = 'Description'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '8'.
      w_fcat-col_pos = '20'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'MEINS'.
      w_fcat-ref_tabname = 'BSEG'.

*  W_FCAT-Seltext_M = 'Description'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '21'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'LIFNR'.
      w_fcat-ref_tabname = 'BSEG'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '22'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'VEND2'.
*  W_FCAT-REF_TABNAME = 'lfa1'.

      w_fcat-seltext_s = 'Vendor Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '30'.
      w_fcat-col_pos = '23'.
*  W_FCAT-NO_OUT = 'X'.
*  W_FCAT-hotspot = ' '.
*  W_FCAT-ICON = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'KUNNR'.
      w_fcat-ref_tabname = 'BSEG'.

      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '24'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NAME1'.
      w_fcat-ref_tabname = 'KNA1'.

*  W_FCAT-Seltext_L = ' Customer Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '30'.
      w_fcat-col_pos = '25'.
*    W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ANLN1'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '26'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ANLN2'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '27'.
*    W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TXT50'.
      w_fcat-ref_tabname = 'ANLA'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '28'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'GSBER'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '6'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BPMNG'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '30'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BPRME'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '30'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'EBELN'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '30'.
      w_fcat-hotspot = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'EBELP'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '30'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NETWR'.
      w_fcat-ref_tabname = 'EKPO'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '30'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BATXT'.
      w_fcat-ref_tabname = 'T161T'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '41'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ZUONR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '18'.
      w_fcat-col_pos = '31'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ATTA'.
      w_fcat-seltext_m = 'Attachment'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '31'.
      w_fcat-hotspot = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

** IF  RB_5 EQ 'X'.
**  W_FCAT-FIELDNAME = 'USNAM'.
***  W_FCAT-REF_TABNAME = 'BKPF'.
**  W_FCAT-seltext_m = 'User ID'.
**  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '46'.
**  W_FCAT-COL_POS = '31'.
**  APPEND W_FCAT TO I_FCAT.
**  CLEAR W_FCAT.
**
**  W_FCAT-FIELDNAME = 'NAME_TEXT'.
***  W_FCAT-REF_TABNAME = 'ADRP'.
**  W_FCAT-seltext_m = 'User Name'.
**  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '46'.
**  W_FCAT-COL_POS = '32'.
**  APPEND W_FCAT TO I_FCAT.
**  CLEAR W_FCAT.
**ELSEIF RB_6 EQ 'X'.
      w_fcat-fieldname = 'USNAM'.
*  W_FCAT-REF_TABNAME = 'BKPF'.
      w_fcat-seltext_m = 'Post'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '33'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NAME_TEXT'.
*  W_FCAT-REF_TABNAME = 'ADRP'.
      w_fcat-seltext_m = 'User Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '34'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'PPNAM'.
*  W_FCAT-REF_TABNAME = 'BKPF'.
      w_fcat-seltext_m = 'Park By'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '31'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NAME_TEXT1'.
*  W_FCAT-REF_TABNAME = 'ADRP'.
      w_fcat-seltext_m = 'User Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '32'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'LTEXT'.
*  W_FCAT-REF_TABNAME = 'T003T'.
      w_fcat-seltext_m = 'Doc. Type Description'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '25'.
      w_fcat-col_pos = '40'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TEXT1'.
*  W_FCAT-REF_TABNAME = 'T007S'.
      w_fcat-seltext_m = 'Tax Code Description.'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '25'.
      w_fcat-col_pos = '38'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.
**ENDIF.

      layout-colwidth_optimize = 'X'.

      w_fcat-fieldname = 'WITHT'.
      w_fcat-seltext_m = 'W/Tax Type.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '41'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QSSHB'.
      w_fcat-seltext_m = 'W/Tax Base.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '42'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QBSHB'.
      w_fcat-seltext_m = 'W/Tax Amt.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '43'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QSSHH'.
      w_fcat-seltext_m = 'W/Tax Base LC.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '44'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QBSHH'.
      w_fcat-seltext_m = 'W/Tax Amt LC.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '45'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

*Break Abap.

*PERFORM initialize_alv.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*
          i_callback_program      = sy-repid
          i_callback_user_command = gt_callback_subroutine
*         I_CALLBACK_PF_STATUS_SET          = ' '
*         I_CALLBACK_USER_COMMAND = ' '
*         I_CALLBACK_TOP_OF_PAGE  = 'HEADER'
*         I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*         I_CALLBACK_HTML_END_OF_LIST       = ' '
*         I_STRUCTURE_NAME        =
*         I_BACKGROUND_ID         = ' '
          i_grid_title            = 'FBL3N with cost center'
*         I_GRID_SETTINGS         =
          is_layout               = layout
          it_fieldcat             = i_fcat
          is_variant              = variant     "<----  Here
          i_save                  = 'A'             "<----  Here
*         I_SAVE                  = 'X'
*         IS_VARIANT              = 'X'
        TABLES
          t_outtab                = it_final.
**************************************************
***call function 'SAPGUI_PROGRESS_INDICATOR'.
************************************************
*PERFORM enable_layout_settings.
    ELSE.

      SORT it_final BY belnr buzei.



      w_fcat-fieldname = 'BUKRS'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '1'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BELNR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '2'.
      w_fcat-hotspot = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BUZEI'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '05'.
      w_fcat-col_pos = '3'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'XBLNR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C311'.
      w_fcat-outputlen = '16'.
      w_fcat-col_pos = '4'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.


      w_fcat-fieldname = 'GJAHR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '5'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BLART'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '39'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'MONAT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '7'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BLDAT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '8'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BUDAT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '9'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'CPUDT'.
      w_fcat-ref_tabname = 'BKPF'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '10'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'SHKZG'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '3'.
      w_fcat-col_pos = '10'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.





      w_fcat-fieldname = 'DMBTR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '15'.
      w_fcat-col_pos = '12'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

*         W_FCAT-FIELDNAME = 'PSWSL'.
*  W_FCAT-REF_TABNAME = 'BSIS'.
*  W_FCAT-EMPHASIZE = 'C110'.
*  W_FCAT-OUTPUTLEN = '5'.
*  W_FCAT-COL_POS = '14'.
*  APPEND W_FCAT TO I_FCAT.
*  CLEAR W_FCAT.

      w_fcat-fieldname = 'MWSKZ'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '2'.
      w_fcat-col_pos = '37'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WAERS'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '14'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'DMBTR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '15'.
      w_fcat-col_pos = '12'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ZUONR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '18'.
      w_fcat-col_pos = '15'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'SGTXT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '50'.
      w_fcat-col_pos = '16'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'KOSTL'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '17'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'KTEXT'.
      w_fcat-ref_tabname = 'CSKT'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '18'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'AUFNR'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '19'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'HKONT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '20'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TXT20'.
      w_fcat-ref_tabname = 'SKAT'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '40'.
      w_fcat-col_pos = '21'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'BSCHL'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '4'.
      w_fcat-col_pos = '22'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'AUGDT'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '23'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'AUGBL'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '12'.
      w_fcat-col_pos = '24'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'FIPOS'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '18'.
      w_fcat-col_pos = '25'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ANLN1'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '26'.
*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ANLN2'.
      w_fcat-ref_tabname = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '27'.
*    W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TXT50'.
      w_fcat-ref_tabname = 'ANLA'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '28'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'GSBER'.
      w_fcat-ref_tabname = 'BSIS'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '5'.
      w_fcat-col_pos = '6'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'ATTA'.
      w_fcat-seltext_m = 'Attachment'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '6'.
      w_fcat-col_pos = '30'.
      w_fcat-hotspot = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.
**IF  RB_5 EQ 'X'.
**  W_FCAT-FIELDNAME = 'USNAM'.
***  W_FCAT-REF_TABNAME = 'BKPF'.
**  W_FCAT-seltext_m = 'User ID'.
**  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '46'.
**  W_FCAT-COL_POS = '31'.
**  APPEND W_FCAT TO I_FCAT.
**  CLEAR W_FCAT.
**
**  W_FCAT-FIELDNAME = 'NAME_TEXT'.
***  W_FCAT-REF_TABNAME = 'ADRP'.
**  W_FCAT-seltext_m = 'User Name'.
**  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '46'.
**  W_FCAT-COL_POS = '32'.
**  APPEND W_FCAT TO I_FCAT.
**  CLEAR W_FCAT.
**ELSEIF RB_6 EQ 'X'.
      w_fcat-fieldname = 'USNAM'.
*  W_FCAT-REF_TABNAME = 'BKPF'.
      w_fcat-seltext_m = 'Post'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '33'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NAME_TEXT'.
*  W_FCAT-REF_TABNAME = 'ADRP'.
      w_fcat-seltext_m = 'User Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '34'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'PPNAM'.
*  W_FCAT-REF_TABNAME = 'BKPF'.
      w_fcat-seltext_m = 'Park By'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '31'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'NAME_TEXT1'.
*  W_FCAT-REF_TABNAME = 'ADRP'.
      w_fcat-seltext_m = 'User Name'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '46'.
      w_fcat-col_pos = '32'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'LTEXT'.
*  W_FCAT-REF_TABNAME = 'T003T'.
      w_fcat-seltext_m = 'Doc. Type Description'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '25'.
      w_fcat-col_pos = '40'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'TEXT1'.
*  W_FCAT-REF_TABNAME = 'T007S'.
      w_fcat-seltext_m = 'Tax Code Description.'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '38'.
      w_fcat-col_pos = '12'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WITHT'.
      w_fcat-seltext_m = 'W/Tax Type.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '41'.

*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QSSHB'.
      w_fcat-seltext_m = 'W/Tax Base.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '42'.

*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QBSHB'.
      w_fcat-seltext_m = 'W/Tax Amt.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '43'.

*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QSSHH'.
      w_fcat-seltext_m = 'W/Tax Base LC.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '44'.

*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

      w_fcat-fieldname = 'WT_QBSHH'.
      w_fcat-seltext_m = 'W/Tax Amt LC.'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
      w_fcat-emphasize = 'C110'.
      w_fcat-outputlen = '10'.
      w_fcat-col_pos = '45'.

*  W_FCAT-NO_OUT = 'X'.
      APPEND w_fcat TO i_fcat.
      CLEAR w_fcat.

*ENDIF.
*  PERFORM initialize_alv.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*
          i_callback_program      = sy-repid
          i_callback_user_command = gt_callback_subroutine
*         I_CALLBACK_PF_STATUS_SET          = ' '
*         I_CALLBACK_USER_COMMAND = ' '
*         I_CALLBACK_TOP_OF_PAGE  = 'HEADER'
*         I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*         I_CALLBACK_HTML_END_OF_LIST       = ' '
*         I_STRUCTURE_NAME        =
*         I_BACKGROUND_ID         = ' '
          i_grid_title            = 'FBL3N with cost center'
*         I_GRID_SETTINGS         =
          is_layout               = layout
          it_fieldcat             = i_fcat
          is_variant              = variant     "<----  Here
          i_save                  = 'A'             "<----  Here
*         I_SAVE                  = 'X'
*         IS_VARIANT              = 'X'
        TABLES
          t_outtab                = it_final.
***********************************************
***call function 'SAPGUI_PROGRESS_INDICATOR'.
*********************************************

*    PERFORM enable_layout_settings.

    ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  ELSE.

    MESSAGE 'Company Code having Different Chart Of Account'  TYPE 'S'.
*leave to screen 1000.
    LEAVE LIST-PROCESSING.
  ENDIF.



FORM user_command  USING p_ucomm    LIKE sy-ucomm
                         p_selfield TYPE slis_selfield.

  "p_ucomm will hold user action like double click, clicking a button ,etc
  CASE p_ucomm.
    WHEN '&IC1'.        " SAP standard code for double-clicking
      CLEAR: wa_final.
      IF p_selfield-fieldname = 'BELNR'.
        READ TABLE it_final INTO wa_final INDEX p_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_final-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_final-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_final-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF p_selfield-fieldname = 'EBELN'.
        READ TABLE it_final INTO wa_final INDEX p_selfield-tabindex.
        SET PARAMETER ID 'BES' FIELD wa_final-ebeln.
        SET PARAMETER ID 'BSP' FIELD wa_final-ebelp.
        IF wa_final-ebeln NE ''.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      ELSEIF p_selfield-fieldname = 'ATTA'.

        READ TABLE it_final INTO wa_final INDEX p_selfield-tabindex.

        DATA : collectstr1 TYPE string.

        CONCATENATE wa_final-bukrs wa_final-belnr wa_final-gjahr INTO collectstr1.

        SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta_1 WHERE  instid_a = collectstr1.
        IF sy-subrc EQ 0.


          DATA: ls_object    TYPE sibflporb,
                save_request TYPE sgs_flag.
          ls_object-instid = collectstr1.
          ls_object-typeid = 'BKPF'.
          ls_object-catid = 'BO'.
          CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
            EXPORTING
              is_object       = ls_object
              ip_mode         = 'D' " Display mode
            IMPORTING
              ep_save_request = save_request.
          IF save_request = 'X'.
            COMMIT WORK.
          ENDIF.

          CLEAR : collectstr1.

        ELSE.

          SELECT  bukrs belnr gjahr awkey ppnam usnam tcode
                     INTO TABLE it_tyn FROM bkpf
              WHERE belnr = wa_final-belnr AND gjahr = wa_final-gjahr."  AND BUKRS = S_BUKRS.
          IF sy-subrc = 0.

            READ TABLE it_tyn INTO wa_tyn INDEX sy-tabix.


            DATA: ls_object1    TYPE sibflporb,
                  save_request1 TYPE sgs_flag.
            ls_object1-instid = wa_tyn-awkey.
            ls_object1-typeid = 'BUS2081'.
            ls_object1-catid = 'BO'.
            CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
              EXPORTING
                is_object       = ls_object1
                ip_mode         = 'D' " Display mode
              IMPORTING
                ep_save_request = save_request1.

            IF save_request1 = 'X'.
              COMMIT WORK.
            ENDIF.

          ENDIF.
          CLEAR : wa_final-bukrs,wa_final-belnr,wa_final-gjahr.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
FORM user_name.
  IF wa_final-usnam IS NOT INITIAL.
    SELECT
          a~bname
          a~persnumber
          b~name_text INTO TABLE it_un FROM usr21 AS a INNER JOIN adrp AS b ON a~persnumber = b~persnumber WHERE a~bname EQ wa_final-usnam.

    READ TABLE it_un INTO wa_un INDEX 1.

    wa_final-name_text = wa_un-name_text.
    CLEAR: it_un, wa_un.
  ENDIF.
  IF wa_final-ppnam IS NOT INITIAL.
    SELECT
          a~bname
          a~persnumber
          b~name_text INTO TABLE it_un FROM usr21 AS a INNER JOIN adrp AS b ON a~persnumber = b~persnumber WHERE a~bname EQ wa_final-ppnam.

    READ TABLE it_un INTO wa_un INDEX 1.

    wa_final-name_text1 = wa_un-name_text.

  ENDIF.

ENDFORM.

*FORM initialize_alv.
**&---------------------------------------------------------------------*
*  DATA message TYPE REF TO cx_salv_msg.
*
*  TRY.
*      cl_salv_table=>factory(
*      IMPORTING
*        r_salv_table = alv
*      CHANGING
*        t_table      = it_final ).
*
*      PERFORM enable_layout_settings.
*      " PERFORM setting_2.
*      " PERFORM setting_3.
*      " ...
*      " PERFORM setting_n.
*
*    CATCH cx_salv_msg INTO message.
*      " error handling
*  ENDTRY.
*ENDFORM.
*
*FORM enable_layout_settings.
*
**&---------------------------------------------------------------------*
*  DATA layout_settings TYPE REF TO cl_salv_layout.
*  DATA layout_key      TYPE salv_s_layout_key.
*
*  layout_settings = alv->get_layout( ).
*
*  layout_key-report = sy-repid.
*  layout_settings->set_key( layout_key ).
*
*  layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).
*ENDFORM.
