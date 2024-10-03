*=======================================================================
*  Author                    : T.Pandiarajan
*  Date                      : 28.02.2024
*  Requester Name            : Ramakrishnan
*  Business Logic            : Report for distributor wise invoice details - DMS*
*  Released on Date          :
*  Hardcoded                 : gv_bukrs = 'DMS1'.
*=======================================================================

REPORT zsd_rep_invoicecount_dms.

DATA: lv_bukrs    TYPE vbrk-bukrs,
      w_aux_fkdat TYPE vbrk-fkdat,
      w_fkart     TYPE vbrk-fkart,
      w_vbeln     TYPE vbrk-vbeln,
      w_kunag     TYPE vbrk-kunag,
      w_matnr     TYPE vbrp-matnr,
      w_name3     TYPE kna1-name3,                               "ADD BY SANTHOSHKUMAR
      it_fieldcat TYPE TABLE OF slis_fieldcat_alv,
      wa_fieldcat LIKE LINE OF it_fieldcat,
      w_kunnr     TYPE kna1-kunnr,
      w_vkbur     TYPE knvv-vkbur.                                " SALES OFFICE FOR ACCOUNT BALANCES

*--------------------------------------------------------------*
* TYPES DECLARATION
*--------------------------------------------------------------*


TYPES: BEGIN OF ty_vbrk_vbrp,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
         fkdat TYPE vbrk-fkdat,
         bukrs TYPE vbrk-bukrs,
         werks TYPE vbrp-werks,
         inco2 TYPE vbrk-inco2,
         erdat TYPE vbrp-erdat,
         erzet TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015
         spart TYPE vbrk-spart,
         vrkme TYPE vbrp-vrkme,
         vbeln TYPE vbrk-vbeln,
         fkart TYPE vbrk-fkart,
         kunag TYPE vbrk-kunag,
         kunrg TYPE vbrk-kunrg,
         vkorg TYPE vbrk-vkorg,
         vtweg TYPE vbrk-vtweg,
         netwr TYPE vbrk-netwr,
         sfakn TYPE vbrk-sfakn,
         mwsbk TYPE vbrk-mwsbk,
         fksto TYPE vbrk-fksto,
         knumv TYPE vbrk-knumv,
       END OF ty_vbrk_vbrp,

       BEGIN OF ty_tvkbt,
         vkbur TYPE tvkbt-vkbur,
         bezei TYPE tvkbt-bezei,
       END OF ty_tvkbt,

       BEGIN OF ty_tspat,
         spart TYPE tspat-spart,
         vtext TYPE tspat-vtext,
       END OF ty_tspat,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         name2 TYPE kna1-name2,
         name3 TYPE kna1-name3,                                           "ADD BY SANTHOSHKUMAR
       END OF ty_kna1,

*       BEGIN OF ty_marm,
*         matnr TYPE marm-matnr,
*         meinh TYPE marm-meinh,
*         umrez TYPE marm-umrez,
*         umren TYPE marm-umren,
*       END OF ty_marm,


       BEGIN OF ty_finovr,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
         fkdat     TYPE vbrk-fkdat,
         bukrs     TYPE vbrk-bukrs,
         werks     TYPE vbrp-werks,                              "ADDED BY JESTOP ON 30.10.2020
         name      TYPE  t001w-name1,                             "
         vkbur     TYPE vbrp-vkbur,
         kunrg     TYPE vbrk-kunag,
         spart     TYPE vbrk-spart,
         vbeln     TYPE vbrk-vbeln,
         fkart     TYPE vbrk-fkart,
         kunag     TYPE vbrk-kunag,
         vkorg     TYPE vbrk-vkorg,
         vtweg     TYPE vbrk-vtweg,
         netwr     TYPE vbrk-netwr,
         mwsbk     TYPE vbrk-mwsbk,
         fksto     TYPE vbrk-fksto,
         time      TYPE vbrk-time,
         btgew     TYPE likp-btgew,
         inv_val   TYPE netwr,
         posnr     TYPE vbrp-posnr,
         bezei     TYPE tvkbt-bezei,
         vtext     TYPE tspat-vtext,
         name1     TYPE kna1-name1,
         qty_ltr   TYPE vbrp-volum,
         aubel     TYPE vbrp-aubel,
         pernr     TYPE vbpa-pernr,
         sname     TYPE pa0001-sname,
         sm_no     TYPE vbpa-pernr,
         sm_name   TYPE pa0001-sname,
         dist_name TYPE kna1-name1,
         dist      TYPE kna1-kunnr,
       END OF ty_finovr.

TYPES: BEGIN OF ty_tvkbt2,
         vkbur TYPE tvkbt-vkbur,
         bezei TYPE tvkbt-bezei,
       END OF ty_tvkbt2,

       BEGIN OF ty_tspat2,
         spart TYPE tspat-spart,
         vtext TYPE tspat-vtext,
       END OF ty_tspat2,

       BEGIN OF ty_kna12,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         katr2 TYPE kna1-katr2,                                         "ADD BY SANTHOSHKUMAR
       END OF ty_kna12,

       BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         kunnr TYPE likp-kunnr,
         btgew TYPE likp-btgew,

       END OF ty_likp,

       BEGIN OF ty_vbrp, " Added by Govind
         erdat TYPE vbrp-erdat,
         erzet TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         vrkme TYPE vbrp-vrkme,
         brgew TYPE vbrp-brgew,
         volum TYPE vbrp-volum,
         matnr TYPE vbrp-matnr,
         aubel TYPE vbrp-aubel,
         werks TYPE vbrp-werks,
         vkbur TYPE vbrp-vkbur,
         netwr TYPE vbrp-netwr,
         mwsbp TYPE vbrp-mwsbp,
         arktx TYPE vbrp-arktx,
       END OF ty_vbrp,

       BEGIN OF ty_vbrp3, " Added by Govind
         erdat TYPE vbrp-erdat,
         erzet TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         volum TYPE vbrp-volum,
         matnr TYPE vbrp-matnr,
       END OF ty_vbrp3,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         volum TYPE mara-volum,
         voleh TYPE mara-voleh,
         spart TYPE mara-spart,   "ADD PR@$@TH
       END OF ty_mara,

       BEGIN OF ty_mara1,
         matnr TYPE mara-matnr,
         volum TYPE mara-volum,
         voleh TYPE mara-voleh,
         spart TYPE mara-spart,   "ADD PR@$@TH
       END OF ty_mara1,

       BEGIN OF ty_mara5,
         matnr TYPE mara-matnr,
         volum TYPE mara-volum,
         voleh TYPE mara-voleh,
         spart TYPE mara-spart,   "ADD PR@$@TH
       END OF ty_mara5,

       BEGIN OF ty_vdrk2,
         bukrs       TYPE vbrk-bukrs,
         werks       TYPE vbrp-werks,                          " ADDED BY JESTOP ON 30.10.2020
         fkdat       TYPE vbrk-fkdat,
         erdat       TYPE vbrp-erdat,
         erzet       TYPE vbrp-erzet,                          "CHANGED BY RAM ON 8/8/2015
         fkart       TYPE vbrk-fkart,
         vbeln       TYPE vbrp-vbeln,
         lposnr      TYPE lips-posnr,
         matnr       TYPE vbrp-matnr,
         fkimg       TYPE vbrp-fkimg,
         vgpos       TYPE vbrp-vgpos,
         volum       TYPE vbrp-volum,
         voleh       TYPE vbrp-voleh,
         pstyv       TYPE lips-pstyv,
         spart       TYPE  vbrk-spart,
         netwr       TYPE vbrk-netwr,
         kunag       TYPE vbrk-kunag,
         kunrg       TYPE vbrk-kunrg,
         vkorg       TYPE vbrk-vkorg,   "Added by Keerthi CLSS on 26.10.2016
         vtweg       TYPE vbrk-vtweg,   "Added by Keerthi CLSS on 26.10.2016
         sfakn       TYPE vbrk-sfakn,
         fksto       TYPE vbrk-fksto,
         posnr       TYPE vbrp-posnr,
         arktx       TYPE vbrp-arktx,
         vrkme       TYPE vbrp-vrkme,
         vgbel       TYPE vbrp-vgbel,
         vkbur       TYPE vbrp-vkbur,
         vnetwr      TYPE vbrp-netwr,
         lmatnr      TYPE lips-matnr,
         lvbeln      TYPE lips-vbeln,
         lfimg       TYPE lips-lfimg,
         charg       TYPE lips-charg,
         del_flag(1) TYPE c,
         proc_key(1) TYPE c,
         aubel       TYPE vbrp-aubel,                                                "  ADDED BY MANIKANDAN ON 28-08-2015
         vsnmr_v     TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
       END OF ty_vdrk2,


       BEGIN OF ty_finaldet,                              " FINAL TABLE TYPE FOR DETAIL REPORT
         fkdat     TYPE vbrk-fkdat,
         fkart     TYPE vbrk-fkart,
         bukrs     TYPE vbrk-bukrs,
         werks     TYPE vbrp-werks,                                " ADDED BY JESTOP ON 30.10.2020
         name      TYPE t001w-name1,
*         erdat       TYPE vbrp-erdat,
*         erzet       TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015
         vkbur     TYPE vbrp-vkbur,
         bezei     TYPE tvkbt-bezei,
         spart     TYPE vbrk-spart,
         vtext     TYPE tspat-vtext,
         vbeln     TYPE vbrp-vbeln,
*         lvbeln      TYPE lips-vbeln,
*         order_num   TYPE i,
**         charg       TYPE lips-charg,
*         sort_key(1) TYPE c,                               " SORT KEY is there so that the Header and will be shown first and items beneath it
*         sort_crit   TYPE posnr,
         candoc(1) TYPE c,
         posnr     TYPE vbrp-posnr,
         matnr     TYPE vbrp-matnr,
         arktx     TYPE vbrp-arktx,
         fkimg     TYPE vbrp-fkimg,
         volum     TYPE vbrp-volum,
         voleh     TYPE vbrp-voleh,
         m_volum   TYPE mara-volum,
         t_volum   TYPE mara-volum,
         m_spart   TYPE mara-spart,                          "ADDED BY PR@$@TH
         vrkme     TYPE vbrp-vrkme,
         vnetwr    TYPE vbrp-netwr,
         vmwsbp    TYPE vbrp-mwsbp,
         zpro      TYPE vbrp-netwr,
         disc      TYPE vbrp-netwr,
         zprb      TYPE vbrp-netwr,
         cgst      TYPE vbrp-netwr,
         sgst      TYPE vbrp-netwr,
         igst      TYPE vbrp-netwr,
         tcs       TYPE vbrp-netwr,
         roundoff  TYPE vbrp-netwr,
         total     TYPE vbrp-netwr,
*         netwr(10)   TYPE c,                                 " NETWR field has been declared as character field so that Net price fields for multiple batch split rows will be shown as '-'.
         kunag     TYPE vbrk-kunag,
         name1     TYPE kna1-name1,
         kunrg     TYPE vbrk-kunrg,
         name2     TYPE kna1-name1,
         ptype     TYPE tvk2t-vtext,
         name3     TYPE kna1-name1,                             "ADD BY SANTHOSHKUMAR
         sfakn     TYPE vbrk-sfakn,
         lfimg     TYPE lips-lfimg,
         aubel     TYPE vbrp-aubel,                                                "  ADDED BY MANIKANDAN ON 28-08-2015
         pernr     TYPE vbpa-pernr,
         sname     TYPE pa0001-sname,
*         vsnmr_v     TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
         sm_no     TYPE vbpa-pernr,          " Added by Keerthi CLSS on 24-08-2016.
         sm_name   TYPE pa0001-sname,      " Added by Keerthi CLSS on 24-08-2016.
         vkorg     TYPE vbrk-vkorg,          "Added by Keerthi CLSS on 26.10.2016
         vtweg     TYPE vbrk-vtweg,
         dist_name TYPE kna1-name1,
         dist      TYPE kna1-kunnr,
       END OF ty_finaldet.

*--------------------------------------------------------------*
*INTERNAL TABLE DECLARATIONS
*--------------------------------------------------------------*



DATA: it_vbrk     TYPE TABLE OF ty_vbrk_vbrp,
      it_tvkbt    TYPE TABLE OF ty_tvkbt,
      it_tspat    TYPE TABLE OF ty_tspat,
      it_kna1     TYPE TABLE OF ty_kna1,
      it_mara     TYPE TABLE OF ty_mara,
      it_kna12    TYPE TABLE OF ty_kna12,
      it_vbrp     TYPE TABLE OF ty_vbrp,
      it_vbrp_tmp TYPE TABLE OF ty_vbrp,
      it_finovr   TYPE TABLE OF ty_finovr,
      it_finaldet TYPE TABLE OF ty_finaldet.



DATA : layout TYPE slis_layout_alv.                   "Changed by Savariar S as on 08/01/2015.

*--------------------------------------------------------------*
*WORK AREA DECLARATIONS
*--------------------------------------------------------------*

DATA: wa_vbrk     TYPE ty_vbrk_vbrp,
      wa_tvkbt    TYPE  ty_tvkbt,
      wa_tspat    TYPE ty_tspat,
      wa_kna1     TYPE ty_kna1,
      wa_mara     TYPE ty_mara,
      wa_finaldet LIKE LINE OF it_finaldet,
      wa_finovr   LIKE LINE OF it_finovr,
      wa_vbrp     LIKE LINE OF it_vbrp.


TYPES : BEGIN OF ty_t001w,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
        END OF ty_t001w.

DATA: wa_t001w TYPE ty_t001w,
      it_t001w TYPE TABLE OF ty_t001w.



SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND uc1 MODIF ID tb2 ,          " SELECTION SCREEN FOR INVOICE

               r2 RADIOBUTTON GROUP g1 MODIF ID tb2 DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  SELECT-OPTIONS: so_bukrs FOR lv_bukrs DEFAULT 'DMS1' NO-EXTENSION NO INTERVALS,
                  so_fkdat FOR w_aux_fkdat MODIF ID tb2 DEFAULT sy-datum OBLIGATORY,        " SELECTION SCREEN ELEMENTS FOR INVOICE
                  so_fkart FOR w_fkart MODIF ID tb2,
                  so_vbeln FOR w_vbeln MODIF ID tb2,
                  so_dist  FOR w_kunnr MODIF ID tb2,
                  so_kunag FOR w_kunag MODIF ID tb2,
                  so_name3 FOR w_name3 MODIF ID tb2,                                        "NAME3 ADD "ADD BY SANTHOSHKUMAR
                  so_matnr FOR w_matnr MODIF ID tb2,
                  so_vkbu2 FOR w_vkbur MODIF ID tb2 NO-DISPLAY.
  SELECTION-SCREEN SKIP.

*  PARAMETERS: p_cb AS CHECKBOX USER-COMMAND cbc MODIF ID ab2.
*  PARAMETERS: p_sr AS CHECKBOX USER-COMMAND cbc MODIF ID ab3.
*  PARAMETERS: p_fs AS CHECKBOX USER-COMMAND cbc MODIF ID ab4 .
*  PARAMETERS: p_ic AS CHECKBOX USER-COMMAND cbc MODIF ID ib2.
*  PARAMETERS: p_st AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

INITIALIZATION.

START-OF-SELECTION.

  IF r1 = 'X'.

    PERFORM data_retrieval_overview.
    PERFORM build_fieldcatalog_overview.
    PERFORM build_alv_overview.

  ELSEIF r2 = 'X'.

    PERFORM data_retrieval_detail.
    PERFORM build_fieldcatalog_detail.
    PERFORM build_alv_detail.

  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_retrieval_overview .

***************fetch only dms distributor***************
  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1) FOR ALL ENTRIES IN @lt_t001k
                                       WHERE kunnr IN @so_dist
                                       AND   werks = @lt_t001k-bwkey.
    IF sy-subrc = 0.
      SORT : lt_kna1 BY werks.
      IF so_vkbu2[] IS INITIAL.
        DATA(so_vkbur) = VALUE rsdsselopt_t(
                         FOR ls_kna1 IN lt_kna1
                             ( sign   = if_fsbp_const_range=>sign_include
                               option = if_fsbp_const_range=>option_equal
                               low    = ls_kna1-werks ) ).
        so_vkbu2[] = so_vkbur[].
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT bukrs fkdat inco2 spart vbeln fkart kunag kunrg vkorg vtweg netwr sfakn mwsbk fksto
         INTO CORRESPONDING FIELDS OF TABLE it_vbrk
         FROM vbrk
         WHERE bukrs = 'DMS1' AND vkorg = 'SDMS' AND fkdat IN so_fkdat AND vbeln IN so_vbeln
           AND fkart IN so_fkart AND kunag IN so_kunag "AND vkbur IN so_vkbu2
           AND fksto <> 'X'  AND ( fkart <> 'S1' OR fkart <> 'S2' ).

*get the SO, SM pernr from VBPA based on the vbeln.
  SELECT vbeln, parvw, pernr FROM vbpa INTO TABLE @DATA(lt_vbpa)
    FOR ALL ENTRIES IN @it_vbrk WHERE vbeln = @it_vbrk-vbeln AND parvw IN ('L5','L3').
  IF sy-subrc = 0.
    SORT lt_vbpa BY vbeln parvw.
  ENDIF.

*get all pernr names from PA0001
  SELECT pernr, ename FROM pa0001 INTO TABLE @DATA(lt_pa0001) WHERE begda LE @sy-datum AND endda GE @sy-datum.
  IF sy-subrc = 0.
    SORT lt_pa0001 BY pernr.
  ENDIF.

  SELECT werks name1 FROM t001w INTO TABLE it_t001w.
  IF sy-subrc = 0.
    SORT it_t001w BY werks.
  ENDIF.

*get data from vbrp
  SELECT vbeln posnr fkimg brgew volum matnr aubel werks vkbur netwr mwsbp
    FROM vbrp INTO  CORRESPONDING FIELDS OF TABLE it_vbrp
    FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.

  IF so_dist[] IS NOT INITIAL.
    DELETE it_vbrp WHERE werks NOT IN so_vkbu2.
    IF it_vbrp[] IS NOT INITIAL.
      REFRESH it_vbrp_tmp.
      it_vbrp_tmp[] = it_vbrp[].
      SORT it_vbrp_tmp BY vbeln.
      DELETE ADJACENT DUPLICATES FROM it_vbrp_tmp COMPARING vbeln.
    ENDIF.
  ENDIF.

*get material data.
  SELECT matnr volum voleh spart
    FROM mara INTO CORRESPONDING FIELDS OF TABLE it_mara
    FOR ALL ENTRIES IN it_vbrp WHERE matnr = it_vbrp-matnr.


  TYPES: BEGIN OF ty_vbrk_temp,
           spart TYPE vbrk-spart,
           vkbur TYPE vbrp-vkbur,
           kunag TYPE vbrk-kunag,
           kunrg TYPE vbrk-kunrg,
         END OF ty_vbrk_temp.

*select sales office description
  SELECT vkbur bezei FROM tvkbt INTO TABLE it_tvkbt WHERE spras = sy-langu.
  IF sy-subrc = 0.
    SORT it_tvkbt BY vkbur.
  ENDIF.

  SELECT spart vtext FROM tspat INTO TABLE it_tspat WHERE spras = sy-langu.
  IF sy-subrc = 0.
    SORT it_tspat BY spart.
  ENDIF.

  SELECT kunnr name1 name2 name3 FROM kna1 INTO TABLE it_kna1.
  IF sy-subrc = 0.
    SORT it_kna1 BY kunnr.
  ENDIF.

  LOOP AT it_vbrk INTO wa_vbrk.

    IF so_dist[] IS NOT INITIAL.
      READ TABLE it_vbrp_tmp WITH KEY vbeln = wa_vbrk-vbeln TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.


    wa_finovr-fkdat = wa_vbrk-fkdat.
    wa_finovr-bukrs = wa_vbrk-bukrs.
    wa_finovr-spart = wa_vbrk-spart.
    wa_finovr-vbeln = wa_vbrk-vbeln.
    wa_finovr-fkart = wa_vbrk-fkart.
    wa_finovr-kunag = wa_vbrk-kunag.
    wa_finovr-kunrg = wa_vbrk-kunrg.
    wa_finovr-vkorg = wa_vbrk-vkorg.
    wa_finovr-vtweg = wa_vbrk-vtweg.
    wa_finovr-netwr = wa_vbrk-netwr.
*    wa_finovr-sfakn = wa_vbrk-sfakn.
    wa_finovr-mwsbk = wa_vbrk-mwsbk.

    READ TABLE lt_vbpa INTO DATA(ls_vbpa) WITH KEY vbeln = wa_vbrk-vbeln parvw = 'L5' BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_pa0001 INTO DATA(ls_pa0001) WITH KEY pernr = ls_vbpa-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finovr-pernr = ls_pa0001-pernr.
        wa_finovr-sname = ls_pa0001-ename.
      ENDIF.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = wa_vbrk-vbeln parvw = 'L3' BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_vbpa-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finovr-sm_no = ls_pa0001-pernr.
        wa_finovr-sm_name = ls_pa0001-ename.
      ENDIF.
    ENDIF.

    wa_finovr-inv_val = wa_vbrk-netwr + wa_vbrk-mwsbk.

    IF  wa_finovr-fkart = 'YRMS' .
      wa_finovr-netwr = - ( wa_finovr-netwr ).
      wa_finovr-mwsbk = - ( wa_finovr-mwsbk ) .
      wa_finovr-inv_val = - ( wa_finovr-inv_val ) .
    ENDIF .

*    wa_finovr-erzet = wa_vbrk-erzet .
*    wa_finovr-erdat = wa_vbrk-erdat .

    LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.

      wa_finovr-qty_ltr =  wa_finovr-qty_ltr + wa_vbrp-volum.
      wa_finovr-btgew = wa_finovr-btgew + wa_vbrp-brgew.

      IF wa_finovr-werks IS INITIAL.
        wa_finovr-werks = wa_vbrp-werks.
        CLEAR wa_t001w.
        READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_finovr-werks.
        IF sy-subrc = 0.
          wa_finovr-name = wa_t001w-name1.
        ENDIF.
      ENDIF.

      IF wa_finovr-vkbur IS INITIAL.
        wa_finovr-vkbur = wa_vbrp-vkbur.
        READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrp-vkbur BINARY SEARCH.
        IF sy-subrc = 0.
          wa_finovr-bezei = wa_tvkbt-bezei.
        ENDIF.
      ENDIF.

      IF wa_finovr-aubel IS INITIAL.
        wa_finovr-aubel = wa_vbrp-aubel .
      ENDIF.

    ENDLOOP.

    IF  wa_finovr-fkart = 'YRMS'.
      wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ) .
      wa_finovr-btgew   = - ( wa_finovr-btgew ).
    ENDIF .

    READ TABLE it_tspat INTO wa_tspat WITH KEY spart = wa_vbrk-spart BINARY SEARCH.
    IF sy-subrc = 0.
      wa_finovr-vtext = wa_tspat-vtext.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunag BINARY SEARCH.
    IF sy-subrc = 0.
      wa_finovr-name1 = wa_kna1-name1.
*      wa_finovr-name2 = wa_kna1-name1.
*      wa_finovr-name3 = wa_kna1-name3. "ADD BY SANTHOSHKUMAR
    ENDIF.

************for dms add**********
    READ TABLE lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY werks = wa_finovr-vkbur BINARY SEARCH.
    IF sy-subrc = 0.
      wa_finovr-dist      = <fs_kna1>-kunnr.
      wa_finovr-dist_name = <fs_kna1>-name1.
    ENDIF.

    APPEND wa_finovr TO it_finovr.
    CLEAR : wa_finovr. " , wa_kna1_1 , wa_likp .", wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
  ENDLOOP.


*&--------End of Keerthi CLSS on 26.10.2016---------------*&
  IF it_finovr IS INITIAL.
    MESSAGE 'No Data Exists for the Input' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    "DATA_RETRIEVAL_OVERVIEW

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_overview .

************Added for DMS *************

  wa_fieldcat-fieldname   = 'BUKRS'.
  wa_fieldcat-seltext_m   = 'COMPANY CODE'.
  wa_fieldcat-col_pos     = 1.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'DIST'.
  wa_fieldcat-seltext_m = 'DISTRIBUTOR NO'.
  wa_fieldcat-col_pos   = 2.
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname = 'DIST_NAME'.
  wa_fieldcat-seltext_m = 'DISTRIBUTOR NAME'.
  wa_fieldcat-col_pos   = 3.
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname   = 'WERKS'.
  wa_fieldcat-seltext_m   = 'PLANT'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'NAME'.
  wa_fieldcat-seltext_m   = 'PLANT NAME'.
  wa_fieldcat-col_pos     = 5.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  ENDIF.


  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DATE'.
  wa_fieldcat-col_pos     = 6.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'BEZEI'.
  wa_fieldcat-seltext_m   = 'SALES DESCRIPTION'.
  wa_fieldcat-col_pos     = 8.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  ENDIF.

  wa_fieldcat-fieldname   = 'VKORG'.
  wa_fieldcat-seltext_m   = 'SALES ORGANIZATION'.
  wa_fieldcat-col_pos     = 9.
  wa_fieldcat-cfieldname  = 'VRORG'.
  wa_fieldcat-ctabname    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VBELN'.
  wa_fieldcat-tabname     = 'IT_FINOVR'.
  wa_fieldcat-seltext_m   = 'BILLING DOCUMENT'.
  wa_fieldcat-col_pos     = 10.
  wa_fieldcat-hotspot     = 'X'.
*  wa_fieldcat-cfieldname  = 'VBELN'.
*  wa_fieldcat-ctabname    = 'VBRK'.
  wa_fieldcat-ref_fieldname  = 'VBELN'.
  wa_fieldcat-ref_tabname    = 'VBRK'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'FKART'.
  wa_fieldcat-seltext_m   = 'BILLING TYPE'.
  wa_fieldcat-col_pos     = 11.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-col_pos     = 13.
*  wa_fieldcat-cfieldname   = 'KUNRG'.
*  wa_fieldcat-ctabname    = 'VBRK'.
  wa_fieldcat-ref_fieldname  = 'KUNRG'.
  wa_fieldcat-ref_tabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'NAME1'.
  wa_fieldcat-seltext_m   = 'PAYER NAME'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'QTY_LTR'. "  Added By Govind On 08/01/2015
  wa_fieldcat-seltext_m   = 'TOTAL IN LITES/KGS'.
  wa_fieldcat-col_pos     = 15.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*
  wa_fieldcat-fieldname   = 'BTGEW'.
  wa_fieldcat-seltext_m   = 'TOTAL WEIGHT'.
  wa_fieldcat-col_pos     = 16.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'NETWR'.
  wa_fieldcat-seltext_m   = 'NET VALUE'.
  wa_fieldcat-col_pos     = 17.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'MWSBK'.
  wa_fieldcat-seltext_m   = 'TAX VALUE'.
  wa_fieldcat-col_pos     = 18.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'INV_VAL'.
  wa_fieldcat-seltext_m   = 'INVOICE VALUE'.
  wa_fieldcat-col_pos     = 19.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AUBEL'.               "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 22.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'PERNR'.               "  ADDED BY RAM ON  5/7/2016
  wa_fieldcat-seltext_m   = 'S.O ID'.
  wa_fieldcat-col_pos     = 25.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.               "  ADDED BY RAM ON  5/7/2016
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-col_pos     = 26.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NO'.
  wa_fieldcat-seltext_m   = 'S.M ID'.
  wa_fieldcat-col_pos     = 27.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-col_pos     = 28.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

ENDFORM.                    "BUILD_FIELDCATALOG_OVERVIEW

" BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_alv_overview .

  layout-colwidth_optimize = 'X'.                          "ADDED BY SAVARIAR S AS ON 08/01/2015

  layout-zebra = 'X'.


  SORT it_finovr BY dist fkdat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_GET'
      i_callback_user_command  = 'MY_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = layout
      it_fieldcat              = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     it_sort                  = it_sort
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_finovr
* EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    "BUILD_ALV_OVERVIEW

*&---------------------------------------------------------------------*
*&      Form  MY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_status_get USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'MY_STATUS'.

ENDFORM.                    "MY_STATUS

*&---------------------------------------------------------------------*
*&      Form  MY_USER_COMMAND
*&---------------------------------------------------------------------*
*      FOR SENDING EMAIL WITH EXCEL ATTACHMENT
*----------------------------------------------------------------------*
*      -->R_UCOMM    text
*----------------------------------------------------------------------*
FORM my_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.

*    WHEN '&MAI'.
*
**      PERFORM build_xls_data_table.
**
**      PERFORM send_mail.

    WHEN '&IC1'.

      IF rs_selfield-fieldname = 'VBELN'.

        SET PARAMETER ID 'VF' FIELD rs_selfield-value.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.
ENDFORM.                    "MY_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_retrieval_detail .

  TYPES: BEGIN OF ty_prcdelem,
           vbeln TYPE vbrk-vbeln,
           knumv TYPE prcd_elements-knumv,
           kposn TYPE prcd_elements-kposn,
           kschl TYPE prcd_elements-kschl,
           kwert TYPE prcd_elements-kwert,
         END OF ty_prcdelem.
*
  DATA: lt_prcd TYPE TABLE OF ty_prcdelem.


***************fetch only dms distributor***************
  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1) FOR ALL ENTRIES IN @lt_t001k
                                       WHERE kunnr IN @so_dist
                                       AND   werks = @lt_t001k-bwkey.
    IF sy-subrc = 0.
      SORT : lt_kna1 BY werks.
      IF so_vkbu2[] IS INITIAL.
        DATA(so_vkbur) = VALUE rsdsselopt_t(
                         FOR ls_kna1 IN lt_kna1
                             ( sign   = if_fsbp_const_range=>sign_include
                               option = if_fsbp_const_range=>option_equal
                               low    = ls_kna1-werks ) ).
        so_vkbu2[] = so_vkbur[].
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT bukrs fkdat inco2 spart vbeln fkart kunag kunrg vkorg vtweg netwr sfakn mwsbk fksto knumv
         INTO CORRESPONDING FIELDS OF TABLE it_vbrk FROM vbrk
         WHERE bukrs = 'DMS1' AND vkorg = 'SDMS' AND fkdat IN so_fkdat AND vbeln IN so_vbeln
           AND fkart IN so_fkart AND kunag IN so_kunag "AND vkbur IN so_vkbu2
           AND fksto <> 'X'  AND fkart <> 'S1'.

  SELECT werks name1 FROM t001w INTO TABLE it_t001w WHERE ekorg = 'DMS1'.

  SELECT vkbur bezei FROM tvkbt INTO TABLE it_tvkbt WHERE spras = sy-langu.

  SELECT spart vtext FROM tspat INTO TABLE it_tspat WHERE spras = sy-langu.

  SELECT kunnr name1 katr2 FROM kna1 INTO TABLE it_kna12.

*get the SO, SM pernr from VBPA based on the vbeln.
  SELECT vbeln, parvw, pernr FROM vbpa INTO TABLE @DATA(lt_vbpa)
    FOR ALL ENTRIES IN @it_vbrk WHERE vbeln = @it_vbrk-vbeln AND parvw IN ('L5','L3').
  IF sy-subrc = 0.
    SORT lt_vbpa BY vbeln parvw.
  ENDIF.

*get all pernr names from PA0001
  SELECT pernr, ename FROM pa0001 INTO TABLE @DATA(lt_pa0001) WHERE begda LE @sy-datum AND endda GE @sy-datum.
  IF sy-subrc = 0.
    SORT lt_pa0001 BY pernr.
  ENDIF.

  SELECT werks name1 FROM t001w INTO TABLE it_t001w.
  IF sy-subrc = 0.
    SORT it_t001w BY werks.
  ENDIF.

  SELECT * FROM tvk2t INTO TABLE @DATA(lt_tvk2t) WHERE spras EQ  @sy-langu.

*get data from vbrp
  IF it_vbrk[] IS NOT INITIAL.
    SELECT vbeln posnr fkimg vrkme brgew volum matnr aubel werks vkbur netwr mwsbp arktx
      FROM vbrp INTO  CORRESPONDING FIELDS OF TABLE it_vbrp
      FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.
    IF sy-subrc = 0.
*get material data.
      SELECT matnr volum voleh spart FROM mara
        INTO CORRESPONDING FIELDS OF TABLE it_mara
        FOR ALL ENTRIES IN it_vbrp WHERE matnr = it_vbrp-matnr.
      IF sy-subrc = 0.
        SORT it_mara BY matnr.
      ENDIF.

      SORT it_vbrp BY vbeln posnr.
    ENDIF.

    SELECT knumv kposn kschl kwert FROM prcd_elements INTO CORRESPONDING FIELDS OF TABLE lt_prcd
      FOR ALL ENTRIES IN it_vbrk
      WHERE knumv = it_vbrk-knumv
      AND   kschl IN ('ZPR0','Z013','Z014','Z015','Z016','JOCG','JOSG','JOIG','JTC1','ZPRB','YDIF').
    IF sy-subrc = 0.
      SORT it_vbrk BY knumv.
      LOOP AT lt_prcd ASSIGNING FIELD-SYMBOL(<fs_prcd>).
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY knumv = <fs_prcd>-knumv BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_prcd>-vbeln = wa_vbrk-vbeln.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SORT it_vbrk BY vbeln.
  SORT it_vbrp BY vbeln posnr.
  SORT lt_prcd BY vbeln knumv kposn.

  IF so_dist[] IS NOT INITIAL.
    DELETE it_vbrp WHERE werks NOT IN so_vkbu2.
    IF it_vbrp[] IS NOT INITIAL.
      REFRESH it_vbrp_tmp.
      it_vbrp_tmp[] = it_vbrp[].
      SORT it_vbrp_tmp BY vbeln.
      DELETE ADJACENT DUPLICATES FROM it_vbrp_tmp COMPARING vbeln.
    ENDIF.
  ENDIF.

  DATA: lv_vbrp_tabix TYPE sy-tabix,
        lv_prcd_tabix TYPE sy-tabix.

  LOOP AT it_vbrk INTO wa_vbrk.

    IF so_dist[] IS NOT INITIAL.
      READ TABLE it_vbrp_tmp WITH KEY vbeln = wa_vbrk-vbeln TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    wa_finaldet-fkdat = wa_vbrk-fkdat.
    wa_finaldet-bukrs = wa_vbrk-bukrs.
    wa_finaldet-spart = wa_vbrk-spart.
    wa_finaldet-vbeln = wa_vbrk-vbeln.
    wa_finaldet-fkart = wa_vbrk-fkart.
    wa_finaldet-kunag = wa_vbrk-kunag.
    wa_finaldet-kunrg = wa_vbrk-kunrg.
    wa_finaldet-vkorg = wa_vbrk-vkorg.
    wa_finaldet-vtweg = wa_vbrk-vtweg.
    wa_finaldet-sfakn = wa_vbrk-sfakn.

    READ TABLE it_kna12 INTO DATA(ls_kna12) WITH KEY kunnr = wa_vbrk-kunag.
    IF sy-subrc = 0.
      wa_finaldet-name2 = ls_kna12-name1.
      READ TABLE lt_tvk2t INTO DATA(ls_tvk2t) WITH KEY katr2 = ls_kna12-katr2.
      IF sy-subrc = 0.
        wa_finaldet-ptype = ls_tvk2t-vtext.
      ENDIF.

    ENDIF.


    READ TABLE lt_vbpa INTO DATA(ls_vbpa) WITH KEY vbeln = wa_vbrk-vbeln parvw = 'L5' BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_pa0001 INTO DATA(ls_pa0001) WITH KEY pernr = ls_vbpa-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-pernr = ls_pa0001-pernr.
        wa_finaldet-sname = ls_pa0001-ename.
      ENDIF.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = wa_vbrk-vbeln parvw = 'L3' BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_vbpa-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-sm_no   = ls_pa0001-pernr.
        wa_finaldet-sm_name = ls_pa0001-ename.
      ENDIF.
    ENDIF.

    CLEAR lv_vbrp_tabix.
    READ TABLE it_vbrp TRANSPORTING NO FIELDS WITH KEY vbeln = wa_vbrk-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      lv_vbrp_tabix = sy-tabix.
    ENDIF.

    LOOP AT it_vbrp INTO wa_vbrp FROM lv_vbrp_tabix."WHERE vbeln = wa_vbrk-vbeln.

      IF wa_vbrp-vbeln <> wa_vbrk-vbeln.
        CLEAR lv_vbrp_tabix.
        EXIT.
      ENDIF.

      wa_finaldet-posnr = wa_vbrp-posnr.
      wa_finaldet-volum = wa_vbrp-volum.
      wa_finaldet-fkimg = wa_vbrp-fkimg.
      wa_finaldet-vrkme = wa_vbrp-vrkme.
*      wa_finaldet-qty_ltr =  wa_vbrp-volum.
*      wa_finaldet-btgew = wa_vbrp-brgew.


      READ TABLE lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY werks = wa_vbrp-vkbur BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-dist      = <fs_kna1>-kunnr.
        wa_finaldet-dist_name = <fs_kna1>-name1.
      ENDIF.

      CLEAR wa_t001w.
      wa_finaldet-werks = wa_vbrp-werks.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_vbrp-werks BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-name = wa_t001w-name1.
      ENDIF.

      wa_finaldet-vkbur = wa_vbrp-vkbur.
      READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrp-vkbur BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt-bezei.
      ENDIF.

      wa_finaldet-aubel = wa_vbrp-aubel .

      wa_finaldet-matnr = wa_vbrp-matnr.
      wa_finaldet-arktx = wa_vbrp-arktx.

      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbrp-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-m_spart = wa_mara-spart.
        wa_finaldet-spart = wa_mara-spart.
        wa_finaldet-m_volum = wa_mara-volum.
        wa_finaldet-voleh   = wa_mara-voleh.
        READ TABLE it_tspat INTO wa_tspat WITH KEY spart = wa_mara-spart BINARY SEARCH.
        IF sy-subrc = 0.
          wa_finaldet-vtext = wa_tspat-vtext.
        ENDIF.
        wa_finaldet-t_volum = wa_mara-volum * wa_vbrp-fkimg.
      ENDIF.

      IF wa_finaldet-fkart = 'YRMS'.
        wa_finaldet-fkimg = wa_finaldet-fkimg * -1.
        wa_finaldet-t_volum = wa_finaldet-t_volum * -1.
        wa_finaldet-total = wa_vbrp-netwr + wa_vbrp-mwsbp.
        wa_finaldet-vnetwr = wa_vbrp-netwr * -1.
        wa_finaldet-vmwsbp = wa_vbrp-mwsbp * -1.
        wa_finaldet-total = wa_finaldet-total * -1.
*        wa_finaldet = - ( wa_finovr-inv_val ) .
      ELSE.
        wa_finaldet-total = wa_vbrp-netwr + wa_vbrp-mwsbp.
        wa_finaldet-vnetwr = wa_vbrp-netwr.
        wa_finaldet-vmwsbp = wa_vbrp-mwsbp.
      ENDIF.

      DATA l_total TYPE prcd_elements-kwert.

      CLEAR l_total.
      CLEAR: wa_finaldet-zpro, wa_finaldet-disc, wa_finaldet-zprb, wa_finaldet-cgst, wa_finaldet-sgst, wa_finaldet-igst,
             wa_finaldet-tcs, wa_finaldet-roundoff.

      CLEAR lv_prcd_tabix.
      READ TABLE lt_prcd TRANSPORTING NO FIELDS
            WITH KEY vbeln = wa_vbrk-vbeln
                     knumv = wa_vbrk-knumv
                     kposn = wa_vbrp-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        lv_prcd_tabix = sy-tabix.
      ENDIF.

      LOOP AT lt_prcd INTO DATA(lw_prcd) FROM lv_prcd_tabix."WHERE knumv = wa_vbrk-knumv AND kposn = wa_vbrp-posnr.

        IF lw_prcd-vbeln NE wa_vbrk-vbeln OR lw_prcd-knumv NE wa_vbrk-knumv OR lw_prcd-kposn NE wa_vbrp-posnr.
          CLEAR lv_prcd_tabix.
          EXIT.
        ENDIF.

        CASE lw_prcd-kschl.
          WHEN 'ZPR0'.
            wa_finaldet-zpro = lw_prcd-kwert.
          WHEN 'Z013' OR 'Z014' OR 'Z015' OR 'Z016' .
            wa_finaldet-disc = wa_finaldet-disc + lw_prcd-kwert.
          WHEN 'ZPRB'.
            wa_finaldet-zprb = lw_prcd-kwert.
          WHEN 'JOCG'.
            wa_finaldet-cgst = lw_prcd-kwert.
          WHEN 'JOSG'.
            wa_finaldet-sgst = lw_prcd-kwert.
          WHEN 'JOIG'.
            wa_finaldet-igst = lw_prcd-kwert.
          WHEN 'JTC1'.
            wa_finaldet-tcs = lw_prcd-kwert.
          WHEN 'YDIF'.
            wa_finaldet-roundoff = lw_prcd-kwert.
          WHEN OTHERS.
        ENDCASE.
*        l_total = l_total + lw_prcd-kwert.
      ENDLOOP.

      IF wa_vbrk-fkart = 'YRMS'.

*        wa_finaldet-total =  wa_finaldet-zpro - wa_finaldet-disc +
*                             wa_finaldet-zprb + wa_finaldet-cgst +
*                             wa_finaldet-sgst + wa_finaldet-igst +
*                             wa_finaldet-tcs + wa_finaldet-roundoff.

        wa_finaldet-zpro = wa_finaldet-zpro * -1.
        IF wa_finaldet-disc > 0 .
          wa_finaldet-disc = wa_finaldet-disc * -1.
        ENDIF.
        wa_finaldet-zprb = wa_finaldet-zprb * -1.
        wa_finaldet-cgst = wa_finaldet-cgst * -1.
        wa_finaldet-sgst = wa_finaldet-sgst * -1.
        wa_finaldet-igst = wa_finaldet-igst * -1.
        wa_finaldet-tcs = wa_finaldet-tcs * -1.
        wa_finaldet-roundoff = wa_finaldet-roundoff * -1.
*        wa_finaldet-total = wa_finaldet-total * -1.
      ELSE.
        wa_finaldet-disc = wa_finaldet-disc * -1.
*        wa_finaldet-total = l_total.
*        wa_finaldet-total =  wa_finaldet-zpro - wa_finaldet-disc +
*                             wa_finaldet-zprb + wa_finaldet-cgst +
*                             wa_finaldet-sgst + wa_finaldet-igst +
*                             wa_finaldet-tcs + wa_finaldet-roundoff.
      ENDIF.

      APPEND wa_finaldet TO it_finaldet.
    ENDLOOP.
    CLEAR wa_finaldet.
  ENDLOOP.

ENDFORM.                    "DATA_RETRIEVAL_DETAIL

" DATA_RETRIEVAL_DETAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_detail .

************Added for DMS *************

  wa_fieldcat-fieldname   = 'BUKRS'.
  wa_fieldcat-tabname   = 'IT_FINOVR'.
  wa_fieldcat-seltext_m   = 'COMPANY CODE'.
  wa_fieldcat-col_pos     = 1.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname = 'DIST'.
  wa_fieldcat-seltext_m = 'DISTRIBUTOR NO'.
  wa_fieldcat-col_pos   = 2.
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname = 'DIST_NAME'.
  wa_fieldcat-seltext_m = 'DISTRIBUTOR NAME'.
  wa_fieldcat-col_pos   = 3.
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname   = 'FKART'.
  wa_fieldcat-seltext_m   = 'BILLING TYPE'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'WERKS'.
  wa_fieldcat-seltext_m   = 'PLANT'.
  wa_fieldcat-col_pos     = 5.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'NAME'.
  wa_fieldcat-seltext_m   = 'PLANT NAME'.
  wa_fieldcat-col_pos     = 6.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DATE'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname   = 'VKBUR'.
  wa_fieldcat-seltext_m   = 'SALES OFFICE'.
  wa_fieldcat-col_pos     = 8.
  wa_fieldcat-cfieldname   = 'VKBUR'.
  wa_fieldcat-ctabname    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.


  wa_fieldcat-fieldname   = 'BEZEI'.
  wa_fieldcat-seltext_m   = 'SALES OFFICE DESCRIPTION'.
  wa_fieldcat-col_pos     = 9.
  wa_fieldcat-cfieldname   = 'BEZEI'.
  wa_fieldcat-ctabname    = 'TVKBT'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.


  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION'.
  wa_fieldcat-cfieldname   = 'SPART'.
  wa_fieldcat-ctabname    = 'VBRK'.
  wa_fieldcat-col_pos     = 10.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.


  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION DESCRIPTION'.
  wa_fieldcat-col_pos     = 11.
  wa_fieldcat-cfieldname   = 'VTEXT'.
  wa_fieldcat-ctabname    = 'TSPAT'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.


  wa_fieldcat-fieldname   = 'NAME3'.                                "ADD BY SANTHOSHKUMAR
  wa_fieldcat-seltext_m   = 'SHIP'.
  wa_fieldcat-col_pos     = 11.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-col_pos     = 12.
  wa_fieldcat-cfieldname   = 'KUNRG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'NAME2'.
  wa_fieldcat-seltext_m   = 'PAYER NAME'.
  wa_fieldcat-col_pos     = 13.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'PTYPE'.
  wa_fieldcat-seltext_m   = 'CUST TYPE'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VBELN'.
  wa_fieldcat-seltext_m = 'BILLING DOC'.
  wa_fieldcat-col_pos     = 15.
  wa_fieldcat-hotspot     = 'X'.
  wa_fieldcat-cfieldname   = 'VBELN'.
  wa_fieldcat-ctabname    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'MATNR'.
  wa_fieldcat-seltext_m   = 'MATERIAL NUMBER'.
  wa_fieldcat-col_pos     = 16.
  wa_fieldcat-tabname     = 'MARA'.
  wa_fieldcat-datatype    = 'NUMC'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ARKTX'.
  wa_fieldcat-seltext_m   = 'MATERIAL DESCRIPTION'.
  wa_fieldcat-col_pos     = 17.
  wa_fieldcat-cfieldname   = 'ARKTX'.
  wa_fieldcat-ctabname    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  IF p_st IS NOT INITIAL.

  wa_fieldcat-fieldname   = 'M_SPART'.
  wa_fieldcat-seltext_m   = 'MATERIAL DIVISION'.
  wa_fieldcat-col_pos     = 18.
*  WA_FIELDCAT-CFIELDNAME   = ''.
  wa_fieldcat-ctabname    = 'MARA'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'FKIMG'.
  wa_fieldcat-seltext_m   = 'QUANTITY'.
  wa_fieldcat-col_pos     = 19.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VRKME'.
  wa_fieldcat-seltext_m   = 'SALES UNIT'.
  wa_fieldcat-ctabname     = 'VBRP'.
  wa_fieldcat-cfieldname  = 'VRKME'.
  wa_fieldcat-col_pos     = 20.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'M_VOLUM'.
  wa_fieldcat-seltext_m   = 'LTRS'.
  wa_fieldcat-ctabname     = 'MARA'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 21.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOLEH'.
  wa_fieldcat-seltext_m   = 'UNIT'.
  wa_fieldcat-ctabname     = 'MARA'.
  wa_fieldcat-col_pos     = 22.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'T_VOLUM'.
  wa_fieldcat-seltext_m   = 'TOTAL LTRS/KGS'.
  wa_fieldcat-ctabname     = 'MARA'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 23.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VNETWR'.
  wa_fieldcat-seltext_m   = 'NET VALUE'.
*  WA_FIELDCAT-DATATYPE    = 'CURR'.
*  WA_FIELDCAT-DATATYPE    = 'CUCY'.
*  WA_FIELDCAT-TECH        = 'X'.
  wa_fieldcat-col_pos     = 24.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ZPRO'.
  wa_fieldcat-seltext_m   = 'Before Discount'.
  wa_fieldcat-col_pos     = 25.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'DISC'.
  wa_fieldcat-seltext_m   = 'Discount'.
  wa_fieldcat-col_pos     = 26.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ZPRB'.
  wa_fieldcat-seltext_m   = 'PROD REBATE'.
  wa_fieldcat-col_pos     = 27.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'CGST'.
  wa_fieldcat-seltext_m   = 'CGST'.
  wa_fieldcat-col_pos     = 28.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SGST'.
  wa_fieldcat-seltext_m   = 'SGST'.
  wa_fieldcat-col_pos     = 29.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'IGST'.
  wa_fieldcat-seltext_m   = 'IGST'.
  wa_fieldcat-col_pos     = 30.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TCS'.
  wa_fieldcat-seltext_m   = 'TCS'.
  wa_fieldcat-col_pos     = 31.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ROUNDOFF'.
  wa_fieldcat-seltext_m   = 'Roundff'.
  wa_fieldcat-col_pos     = 32.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TOTAL'.
  wa_fieldcat-seltext_m   = 'TOTAL'.
  wa_fieldcat-col_pos     = 33.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  wa_fieldcat-fieldname   = 'ERDAT'.
*  wa_fieldcat-seltext_m   = 'INV.CREATED DATE'.
*  wa_fieldcat-ctabname     = 'VBRP'.
**  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
*  wa_fieldcat-col_pos     = 26.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.
*
*  wa_fieldcat-fieldname   = 'ERZET'.
*  wa_fieldcat-seltext_m   = 'TIME'.
*  wa_fieldcat-ctabname     = 'VBRP'.
**  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
*  wa_fieldcat-col_pos     = 27.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.

*
*  WA_SORT-FIELDNAME = 'NETWR'. " Modified by Govind on 02/08/2014 Time 12:01 PM
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.



  wa_fieldcat-fieldname   = 'AUBEL'.                                "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 34.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*
*  wa_fieldcat-fieldname   = 'ERDAT1'.                                  "  ADDED BY MANIKANDAN ON 28-08-2015
*  wa_fieldcat-seltext_m   = 'Date'.
*  wa_fieldcat-ctabname    = 'VBAK'.
*  wa_fieldcat-col_pos     = 29.
**  WA_FIELDCAT-DO_SUM      = 'X'.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.
*
*
*  wa_fieldcat-fieldname   = 'ERZET1'.                                   "  ADDED BY MANIKANDAN ON 28-08-2015
*  wa_fieldcat-seltext_m   = 'Time'.
*  wa_fieldcat-ctabname    = 'VBAK'.
*  wa_fieldcat-col_pos     = 30.
**  WA_FIELDCAT-DO_SUM      = 'X'.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'PERNR'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.O NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 35.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 36.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
  wa_fieldcat-fieldname   = 'SM_NO'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.M NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 37.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 38.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
ENDFORM.                    " BUILD_FIELDCATALOG_DETAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_alv_detail .


  layout-colwidth_optimize = 'X'.                             " ADDED BY SAVARIAR AS ON 08/01/2015
  layout-zebra = 'X'.


  SORT it_finaldet BY dist kunag vbeln.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_GET_DETAIL'
      i_callback_user_command  = 'MY_USER_COMMAND_DETAIL'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = layout
      it_fieldcat              = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     it_sort                  = it_sort
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_finaldet
* EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFORM.                    " BUILD_ALV_DETAIL


*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_status_get_detail USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'MY_STATUS'.

ENDFORM.                    "PF_STATUS_GET_DETAIL

*&---------------------------------------------------------------------*
*&      Form  MY_USER_COMMAND_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM my_user_command_detail USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  IF rs_selfield-fieldname = 'VBELN'.

    SET PARAMETER ID 'VF' FIELD rs_selfield-value.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "MY_USER_COMMAND_DETAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_XLS_DATA_TABLE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM build_xls_data_table_detail .

*  CLASS cl_abap_char_utilities DEFINITION LOAD.
*  CONSTANTS:
*    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
*    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.
*
*  CONCATENATE 'BILLING DOCUMENT DATE'
*               'SALES OFFICE'
*               'SALES OFFICE DESCRIPTION'
*               'SALES DIVISION'
*               'SALES DIVISION DESCRPTION'
*               'BILLING DOCUMENT NO'
*               'BILLING ITEM'
*               'MATERIAL NUMBER'
*               'MATERIAL DESCRIPTION'
*               'BATCH NO'
*               'QUANTITY'
*               'SALES UNIT'
*               'NET VALUE'
**               'SOLD TO PARTY'
**               'SP NAME'
*               'PAYER'
*               'PAYER NAME'
*  INTO  wa_attachment SEPARATED BY  con_tab.
*
*  CONCATENATE con_cret
*  wa_attachment
*  INTO wa_attachment.
*
*  APPEND wa_attachment TO it_attachment.
*  CLEAR  wa_attachment.
*
*  DATA: lv_string_det       TYPE string,
*        lv_string_det_netwr TYPE string.
*
*
*  LOOP AT it_finaldet INTO wa_finaldet.
*
*
*    lv_string_det = wa_finaldet-fkimg.
**    lv_string_det_netwr = wa_finaldet-netwr.
*
*    CONCATENATE  wa_finaldet-fkdat
*                 wa_finaldet-vkbur
*                 wa_finaldet-bezei
*                 wa_finaldet-spart
*                 wa_finaldet-vtext
*                 wa_finaldet-vbeln
*                 wa_finaldet-posnr
*                 wa_finaldet-matnr
*                 wa_finaldet-arktx
*                 wa_finaldet-charg
*                 lv_string_det
*                 wa_finaldet-vrkme
*                 lv_string_det_netwr
*                 wa_finaldet-kunag
*                 wa_finaldet-name1
*                 wa_finaldet-kunrg
*                 wa_finaldet-name2
*  INTO wa_attachment SEPARATED BY con_tab. "#EC CI_FLDEXT_OK[2215424] "Added by SPLABAP during code remediation
*    " FINAL TABLE TYPE FOR DETAIL REPORT
*
*    CONCATENATE con_cret wa_attachment
*    INTO wa_attachment.
*    APPEND wa_attachment TO it_attachment.
*    CLEAR wa_attachment.
*  ENDLOOP.

*ENDFORM.                    "BUILD_XLS_DATA_TABLE_DETAIL
*" BUILD_XLS_DATA_TABLE_DETAIL
**&---------------------------------------------------------------------*
**&      Form  SEND_MAIL_DETAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM send_mail_detail .
*
*  wa_docdata-obj_name = 'MAIL WITH EXCEL ATTACHMENT'.
*  wa_docdata-obj_descr = 'SAP Reports'.
*
*  PERFORM body_of_mail USING: space, 'Hello ',
*  'Please find the attached excel sheet'.
*
*  DESCRIBE TABLE it_body_msg LINES g_tab_lines.
*  wa_packlist-head_start = 1.
*  wa_packlist-head_num   = 0.
*  wa_packlist-body_start = 1.
*  wa_packlist-body_num   = g_tab_lines.
*  wa_packlist-doc_type   = 'RAW'.
*  APPEND wa_packlist TO it_packlist.
*  CLEAR  wa_packlist.
*
*  "Write Packing List for Attachment
*  wa_packlist-transf_bin = space.
*  wa_packlist-head_start = 1.
*  wa_packlist-head_num   = 1.
*  wa_packlist-body_start = g_tab_lines + 1.
*  DESCRIBE TABLE it_attachment LINES wa_packlist-body_num.
*  wa_packlist-doc_type   = 'XLS'.
*  wa_packlist-obj_descr  = 'Excell Attachment'.
*  wa_packlist-obj_name   = 'XLS_ATTACHMENT'.
*  wa_packlist-doc_size   = wa_packlist-body_num * 255.
*  APPEND wa_packlist TO it_packlist.
*  CLEAR  wa_packlist.
*
*  APPEND LINES OF it_attachment TO it_body_msg.
*  "Fill the document data and get size of attachment
*  wa_docdata-obj_langu  = sy-langu.
*  READ TABLE it_body_msg INTO wa_body_msg INDEX g_tab_lines.
*  wa_docdata-doc_size = ( g_tab_lines - 1 ) * 255 + strlen( wa_body_msg ).
*
*  "Receivers List.
*  wa_receivers-rec_type   = 'U'.  "Internet address
*  wa_receivers-receiver   = 'no-reply@sheenlac.in'.
*  wa_receivers-com_type   = 'INT'.
*  wa_receivers-notif_del  = 'X'.
*  wa_receivers-notif_ndel = 'X'.
*  APPEND wa_receivers TO it_receivers .
*  CLEAR:wa_receivers.
*
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'                                 " FUNCTION MODULE FOR MAIL SENDING
*    EXPORTING
*      document_data              = wa_docdata
*      put_in_outbox              = 'X'
*      commit_work                = 'X'
*    IMPORTING
*      sent_to_all                = g_sent_to_all
**     NEW_OBJECT_ID              =
*    TABLES
*      packing_list               = it_packlist
**     OBJECT_HEADER              =
**     CONTENTS_BIN               =
*      contents_txt               = it_body_msg
**     CONTENTS_HEX               =
**     OBJECT_PARA                =
**     OBJECT_PARB                =
*      receivers                  = it_receivers
*    EXCEPTIONS
*      too_many_receivers         = 1
*      document_not_sent          = 2
*      document_type_not_exist    = 3
*      operation_no_authorization = 4
*      parameter_error            = 5
*      x_error                    = 6
*      enqueue_error              = 7
*      OTHERS                     = 8.
*  IF sy-subrc <> 0.
*
*    WRITE: 'FAILURE'.
*
*  ELSE.
*
*    WAIT UP TO 2 SECONDS.
*
*    SUBMIT rsconn01 WITH mode = 'INT'
*        WITH output = 'X'
*        AND RETURN.
*  ENDIF.
*ENDFORM.                " SEND_MAIL_DETAILENDFORM.                    " DATA_RETRIEVAL_DETAIL
**&---------------------------------------------------------------------*
**&      Form  BUILD_GRAPH_DATA_TABLE_DETAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM build_graph_data_table_detail .
*
*ENDFORM.                    " BUILD_GRAPH_DATA_TABLE_DETAIL
