*&---------------------------------------------------------------------*
*& Report  ZSD_REP_INVOICECOUNT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsd_rep_invoicecount_vi.

DATA: lv_bukrs              TYPE vbrk-bukrs,
      w_aux_fkdat           TYPE vbrk-fkdat,
      w_fkart               TYPE vbrk-fkart,
      w_vbeln               TYPE vbrk-vbeln,
      w_kunag               TYPE vbrk-kunag,
      w_matnr               TYPE vbrp-matnr,
      w_charg               TYPE lips-charg,                               " BATCH FOR INVOICE DETAILS
      w_inv_vkbur           TYPE vbrp-vkbur,                           " SALES OFFICE FOR INVOICE
      w_spart               TYPE vbrk-spart,
      w_name3               TYPE kna1-name3,                               "ADD BY SANTHOSHKUMAR
      counter               TYPE i VALUE '0',
      gt_list               TYPE vrm_values,
      gwa_list              TYPE vrm_value,
      gt_values             TYPE TABLE OF dynpread,                     " INTERNAL TABLE FOR LIST BOX
      gwa_values            TYPE dynpread,                              " WORK AREA FOR LIST BOX
      gv_selected_value(10) TYPE c,
      it_fieldcat           TYPE TABLE OF slis_fieldcat_alv,
      wa_fieldcat           LIKE LINE OF it_fieldcat,
      it_sort               TYPE slis_t_sortinfo_alv,
      wa_sort               LIKE LINE OF it_sort,
      g_sent_to_all         TYPE sonv-flag,
      g_tab_lines           TYPE i,

      w_kunnr               TYPE knc1-kunnr,
      w_bukrs               TYPE knc1-bukrs,                                " VARIABLE DECLARATIONS FOR ACCOUNT BALANCES
      w_vkbur               TYPE knvv-vkbur,                                " SALES OFFICE FOR ACCOUNT BALANCES
*      W_GJAHR TYPE KNC1-GJAHR,
      w_re_period           TYPE bsid-monat,

      w_s_matnr             TYPE mard-matnr,                               " VARIABLE DECLARATIONS FOR STOCK
      w_s_bukrs             TYPE t001-bukrs,
      w_s_hkont             TYPE bseg-hkont,
      w_s_werks             TYPE t001w-werks,
      w_s_lgort             TYPE t001l-lgort,
      w_s_charg             TYPE mchb-charg,
      w_s_bwtar             TYPE mbew-bwtar,
      w_s_bwart             TYPE mseg-bwart,
      w_s_datum             TYPE mkpf-budat.

*--------------------------------------------------------------*
* TYPES DECLARATION
*--------------------------------------------------------------*


TYPES: BEGIN OF ty_vbrk_vbrp,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
         fkdat            TYPE vbrk-fkdat,
         bukrs            TYPE vbrk-bukrs,
         werks            TYPE vbrp-werks,
         inco2            TYPE vbrk-inco2,
         erdat            TYPE vbrp-erdat,
         erzet            TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015

         spart            TYPE vbrk-spart,
         vrkme            TYPE vbrp-vrkme,
         vbeln            TYPE vbrk-vbeln,

         fkart            TYPE vbrk-fkart,
         kunag            TYPE vbrk-kunag,
         kunrg            TYPE vbrk-kunrg,
         vkorg            TYPE vbrk-vkorg,
         vtweg            TYPE vbrk-vtweg,
         netwr            TYPE vbrk-netwr,
         sfakn            TYPE vbrk-sfakn,
         knkli            TYPE vbrk-knkli,
         mwsbk            TYPE vbrk-mwsbk,
         fksto            TYPE vbrk-fksto,
         date_of_delivery TYPE vbrk-date_of_delivery,
         time             TYPE vbrk-time,
         remarks          TYPE vbrk-remarks,
         posnr            TYPE vbrp-posnr,
         vkbur            TYPE vbrp-vkbur,
         matnr            TYPE vbrp-matnr,
         fkimg            TYPE vbrp-fkimg,
         volum            TYPE vbrp-volum,
         vgbel            TYPE vbrp-vgbel,
*       UMREN TYPE MARM-UMREN,
*       TOT_BAS TYPE VBRP-FKIMG,
*       BAS_UNIT(1) TYPE C,
*       TOTAL TYPE FKIMG,
*       TOT_QUA TYPE FKIMG,


         aubel            TYPE vbrp-aubel,                                             "  ADDED BY MANIKANDAN ON 28-08-2015



         invoiceno        TYPE ztrip_st-invoiceno,                                     "  ADDED BY MANIKANDAN ON 29-09-2015
         uniq1            TYPE ztrip_st-uniq1,                                             "  ADDED BY MANIKANDAN ON 29-09-2015
         ttdate           TYPE ztrip_st-tdate ,                                           "  ADDED BY MANIKANDAN ON 29-09-2015
         ttime            TYPE ztrip_st-time,                                              "  ADDED BY MANIKANDAN ON 29-09-2015
         cartons          TYPE ztrip_st-cartons,                                         "  ADDED BY MANIKANDAN ON 15-10-2015
         city             TYPE ztrip_st-city,                                              "  ADDED BY MANIKANDAN ON 15-10-2015


         vsnmr_v          TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
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

       BEGIN OF ty_marm,
         matnr TYPE marm-matnr,
         meinh TYPE marm-meinh,
         umrez TYPE marm-umrez,
         umren TYPE marm-umren,
       END OF ty_marm,


       BEGIN OF ty_finovr,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
         fkdat            TYPE vbrk-fkdat,
         bukrs            TYPE vbrk-bukrs,
         werks            TYPE vbrp-werks,                              "ADDED BY JESTOP ON 30.10.2020
         name             TYPE  t001w-name1,                             "

         erdat            TYPE vbrp-erdat,
         erzet            TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015

         vkbur            TYPE vbrp-vkbur,
         kunrg            TYPE vbrk-kunag,
         spart            TYPE vbrk-spart,
         vbeln            TYPE vbrk-vbeln,
         candoc(1)        TYPE c,
         fkart            TYPE vbrk-fkart,
         kunag            TYPE vbrk-kunag,
*       KUNRG TYPE VBRK-KUNRG,
         vkorg            TYPE vbrk-vkorg,
         vtweg            TYPE vbrk-vtweg,
         netwr            TYPE vbrk-netwr,
         sfakn            TYPE vbrk-sfakn,
         knkli            TYPE vbrk-knkli,
         mwsbk            TYPE vbrk-mwsbk,
         fksto            TYPE vbrk-fksto,
         date_of_delivery TYPE vbrk-date_of_delivery,
         time             TYPE vbrk-time,
         remarks          TYPE vbrk-remarks,
         btgew            TYPE likp-btgew, " Commended by Govind on 12.05.2014
         inv_val          TYPE netwr,
         posnr            TYPE vbrp-posnr,

         bezei            TYPE tvkbt-bezei,
         vtext            TYPE tspat-vtext,
         name1            TYPE kna1-name1,
         name2            TYPE kna1-name2,
         name3            TYPE kna1-name3,                                     "ADD BY SANTHOSHKUMAR
         m_volum          TYPE mara-volum,
         qty_ltr          TYPE vbrp-volum,
         qty_ltr1         TYPE vbrp-volum,
         sum_cust         TYPE kbetr,                             " SUM CUSTOMER WISE
         sum_off          TYPE kbetr,                              " SUM SALES OFFICE WISE
         v_count          TYPE i,
         flag(1)          TYPE c,


         aubel            TYPE vbrp-aubel,                                                "  ADDED BY MANIKANDAN ON 28-08-2015
         erdat1           TYPE vbak-erdat,                                              "  ADDED BY MANIKANDAN ON 28-08-2015
         erzet1           TYPE vbak-erzet,                                              "  ADDED BY MANIKANDAN ON 28-08-2015


         invoiceno        TYPE ztrip_st-invoiceno,                                     "  ADDED BY MANIKANDAN ON 29-09-2015
         uniq1            TYPE ztrip_st-uniq1,                                             "  ADDED BY MANIKANDAN ON 29-09-2015
         ttdate           TYPE ztrip_st-tdate ,                                           "  ADDED BY MANIKANDAN ON 29-09-2015
         ttime            TYPE ztrip_st-time,                                              "  ADDED BY MANIKANDAN ON 29-09-2015
         cartons          TYPE ztrip_st-cartons,                                         "  ADDED BY MANIKANDAN ON 15-10-2015
         city             TYPE ztrip_st-city,                                               "  ADDED BY MANIKANDAN ON 15-10-2015

         pernr            TYPE vbpa-pernr,
         sname            TYPE pa0001-sname,

         vsnmr_v          TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
         sm_no            TYPE vbpa-pernr,     " Added by Keerthi CLSS on 24-08-2016
         sm_name          TYPE pa0001-sname, " Added by Keerthi CLSS on 24-08-2016
         tr_name          TYPE ztrip_st-transporternamelrno,
         v_no             TYPE ztrip_st-vehiclenodrivername,
         to_city          TYPE ztrip_st-city,
         stp_no           TYPE kna1-kunnr,
         stp_nam          TYPE kna1-name1,
*      LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
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
         name2 TYPE kna1-name2,
         name3 TYPE kna1-name3,                                          "ADD BY SANTHOSHKUMAR
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
         brgew TYPE vbrp-brgew,
         volum TYPE vbrp-volum,
         matnr TYPE vbrp-matnr,
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
         invoiceno   TYPE ztrip_st-invoiceno,                                     "  ADDED BY MANIKANDAN ON 29-09-2015
         uniq1       TYPE ztrip_st-uniq1,                                             "  ADDED BY MANIKANDAN ON 29-09-2015
         ttdate      TYPE ztrip_st-tdate ,                                           "  ADDED BY MANIKANDAN ON 29-09-2015
         ttime       TYPE ztrip_st-time,                                              "  ADDED BY MANIKANDAN ON 29-09-2015
         cartons     TYPE ztrip_st-cartons,                                              "  ADDED BY MANIKANDAN ON 15-10-2015
         city        TYPE ztrip_st-city,                                              "  ADDED BY MANIKANDAN ON 15-10-2015
         vsnmr_v     TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
       END OF ty_vdrk2,


       BEGIN OF ty_finaldet,                              " FINAL TABLE TYPE FOR DETAIL REPORT
         fkdat       TYPE vbrk-fkdat,
         fkart       TYPE vbrk-fkart,
         bukrs       TYPE vbrk-bukrs,

         werks       TYPE vbrp-werks,                                " ADDED BY JESTOP ON 30.10.2020
         name        TYPE t001w-name1,


         erdat       TYPE vbrp-erdat,
         erzet       TYPE vbrp-erzet,                                "CHANGED BY RAM ON 8/8/2015

         vkbur       TYPE vbrp-vkbur,


         bezei       TYPE tvkbt-bezei,
         spart       TYPE vbrk-spart,
         vtext       TYPE tspat-vtext,
         vbeln       TYPE vbrp-vbeln,
         lvbeln      TYPE lips-vbeln,
         order_num   TYPE i,
         charg       TYPE lips-charg,
         sort_key(1) TYPE c,                               " SORT KEY is there so that the Header and will be shown first and items beneath it
         sort_crit   TYPE posnr,
         candoc(1)   TYPE c,
         posnr       TYPE vbrp-posnr,


         matnr       TYPE vbrp-matnr,
         arktx       TYPE vbrp-arktx,
*   CHARG TYPE LIPS-CHARG,
         fkimg       TYPE vbrp-fkimg,
         volum       TYPE vbrp-volum,
         voleh       TYPE vbrp-voleh,
         m_volum     TYPE mara-volum,
         t_volum     TYPE mara-volum,
         m_spart     TYPE mara-spart,                          "ADDED BY PR@$@TH
         vrkme       TYPE vbrp-vrkme,
         vnetwr      TYPE vbrp-netwr,
         netwr(10)   TYPE c,                                 " NETWR field has been declared as character field so that Net price fields for multiple batch split rows will be shown as '-'.
         kunag       TYPE vbrk-kunag,
         name1       TYPE kna1-name1,
         kunrg       TYPE vbrk-kunrg,
         name2       TYPE kna1-name1,
         name3       TYPE kna1-name1,                             "ADD BY SANTHOSHKUMAR
         sfakn       TYPE vbrk-sfakn,
         lfimg       TYPE lips-lfimg,
         flag2(2)    TYPE c,
         proc_key(1) TYPE c,                               " PROCESS KEY

         aubel       TYPE vbrp-aubel,                                                "  ADDED BY MANIKANDAN ON 28-08-2015

         erdat1      TYPE vbak-erdat,                                              "  ADDED BY MANIKANDAN ON 28-08-2015
         erzet1      TYPE vbak-erzet,                                              "  ADDED BY MANIKANDAN ON 28-08-2015




         invoiceno   TYPE ztrip_st-invoiceno,                                     "  ADDED BY MANIKANDAN ON 29-09-2015
         uniq1       TYPE ztrip_st-uniq1,                                             "  ADDED BY MANIKANDAN ON 29-09-2015
         ttdate      TYPE ztrip_st-tdate ,                                           "  ADDED BY MANIKANDAN ON 29-09-2015
         ttime       TYPE ztrip_st-time,                                              "  ADDED BY MANIKANDAN ON 29-09-2015
         cartons     TYPE ztrip_st-cartons,                                              "  ADDED BY MANIKANDAN ON 15-10-2015
         city        TYPE ztrip_st-city,                                              "  ADDED BY MANIKANDAN ON 15-10-2015

         pernr       TYPE vbpa-pernr,
         sname       TYPE pa0001-sname,

         vsnmr_v     TYPE vbak-vsnmr_v,                                              "  ADDED BY MANIKANDAN ON 26-09-2015
         sm_no       TYPE vbpa-pernr,          " Added by Keerthi CLSS on 24-08-2016.
         sm_name     TYPE pa0001-sname,      " Added by Keerthi CLSS on 24-08-2016.
         vkorg       TYPE vbrk-vkorg,          "Added by Keerthi CLSS on 26.10.2016
         vtweg       TYPE vbrk-vtweg,
         temp        TYPE int2,

         trname      TYPE zfreight_header-vendor_name,
         tocity      TYPE zfreight_header-to_loc,
         v_no        TYPE ztrip_st-vehiclenodrivername,
         stp_no      TYPE kna1-kunnr,
         stp_nam     TYPE kna1-name1,
         lr_no       TYPE zfreight_item-lr_no,

       END OF ty_finaldet.






TYPES:BEGIN OF str_vbak,                         "  ADDED BY MANIKANDAN ON 28-08-2015
        vbeln   TYPE vbak-vbeln,
        erdat   TYPE vbak-erdat,
        erzet   TYPE vbak-erzet,
        vsnmr_v TYPE vbak-vsnmr_v,
      END OF str_vbak.

*TYPES:BEGIN OF str_vbpa,                         "  ADDED BY RAM ON 05-07-2016
*      vbeln TYPE vbpa-vbeln,
*      parvw TYPE vbpa-parvw,
*      pernr TYPE vbpa-pernr,
*      END OF str_vbpa.

TYPES:BEGIN OF str_pa0001,                         "  ADDED BY RAM ON 05-07-2016
        pernr TYPE pa0001-pernr,
        endda TYPE pa0001-endda,
        begda TYPE pa0001-begda,
        sname TYPE pa0001-sname,
      END OF str_pa0001.

TYPES:BEGIN OF str_vbrp,                         "  ADDED BY MANIKANDAN ON 28-08-2015
        vbeln TYPE vbrp-vbeln,
        aubel TYPE vbrp-aubel,
      END OF str_vbrp.

DATA: "it_vbpa1 TYPE TABLE OF str_vbpa,           "Added by Keerthi CLSS on 24-08-2016
*     " wa_vbpa1 TYPE str_vbpa,
  it_pa0001_1 TYPE TABLE OF str_pa0001,
  wa_pa0001_1 TYPE str_pa0001.

DATA:waa_vbrp TYPE str_vbrp,                     "  ADDED BY MANIKANDAN ON 28-08-2015
     itt_vbrp TYPE  TABLE OF str_vbrp.

DATA:wa_vbak TYPE str_vbak,                       "  ADDED BY MANIKANDAN ON 28-08-2015
     it_vbak TYPE TABLE OF str_vbak.

*
*DATA:wa_vbpa TYPE str_vbpa,                     "  ADDED BY RAM ON 05-07-2016
*      it_vbpa TYPE  TABLE OF str_vbpa.

DATA:wa_pa0001 TYPE str_pa0001,                       "  ADDED BY RAM ON 05-07-2016
     it_pa0001 TYPE TABLE OF str_pa0001.


TYPES:BEGIN OF str_ztrip_st,
        invoiceno           TYPE ztrip_st-invoiceno,                       "  ADDED BY MANIKANDAN ON 29-09-2015
        transporternamelrno TYPE ztrip_st-transporternamelrno,
        vehiclenodrivername TYPE ztrip_st-vehiclenodrivername,
        uniq1               TYPE ztrip_st-uniq1,
        ttdate              TYPE ztrip_st-tdate,
        ttime               TYPE ztrip_st-time,
        cartons             TYPE ztrip_st-cartons,                           "  ADDED BY MANIKANDAN ON  15-10-2015
        city                TYPE ztrip_st-city,                                 "  ADDED BY MANIKANDAN ON  15-10-2015
      END OF str_ztrip_st.

TYPES: BEGIN OF ty_knvp,   "Added by Keerthi CLSS on 26.10.2016
         kunnr TYPE knvp-kunnr,
         vkorg TYPE knvp-vkorg,
         vtweg TYPE knvp-vtweg,
         spart TYPE knvp-spart,
         parvw TYPE knvp-parvw,
         parza TYPE knvp-parza,
         pernr TYPE knvp-pernr,
       END OF ty_knvp.


DATA: wa_ztrip_st TYPE str_ztrip_st,                                           "  ADDED BY MANIKANDAN ON 29-09-2015
      it_ztrip_st TYPE TABLE OF str_ztrip_st.

*&-----------------------------Added By PR@$@TH On 26/08/2019
*------------------------------------------------------------------------------------------------------------------------------------*
TYPES:BEGIN OF ty_zfrihed,
        trip_no        TYPE  zfreight_header-trip_no,
        status         TYPE  zfreight_header-status,
        vendor_code    TYPE  zfreight_header-vendor_code,
        vendor_name    TYPE  zfreight_header-vendor_name,
        from_code      TYPE  zfreight_header-from_code,
        from_loc       TYPE  zfreight_header-from_loc,
        city_id        TYPE  zfreight_header-city_id,
        to_loc         TYPE  zfreight_header-to_loc,
        truck_type     TYPE  zfreight_header-truck_type,
        truck_des      TYPE  zfreight_header-truck_des,
        filling_type   TYPE  zfreight_header-filling_type,
        filing_des     TYPE  zfreight_header-filing_des,
        freight_charge TYPE  zfreight_header-freight_charge,
        loding_charge  TYPE  zfreight_header-loding_charge,
        unload_charge  TYPE  zfreight_header-unload_charge,
        halt_charge    TYPE  zfreight_header-halt_charge,
        lr_charge      TYPE  zfreight_header-lr_charge,
        no_of_days     TYPE  zfreight_header-no_of_days,
        vechile_number TYPE  zfreight_header-vechile_number,
        crdate         TYPE  zfreight_header-crdate,
        crtime         TYPE  zfreight_header-crtime,
        created_by     TYPE  zfreight_header-created_by,
        trn_value      TYPE  zfreight_header-trn_value,
        apr_sta        TYPE  zfreight_header-apr_sta,
        remarks        TYPE  zfreight_header-remarks,
      END OF ty_zfrihed.

DATA: it_zfrihed TYPE TABLE OF ty_zfrihed,
      wa_zfrihed TYPE ty_zfrihed.

TYPES:BEGIN OF ty_zfritm,
        trip_no        TYPE  zfreight_item-trip_no,
        invoice_no     TYPE  zfreight_item-invoice_no,
        customer_name  TYPE  zfreight_item-customer_name,
        invoice_date   TYPE  zfreight_item-invoice_date,
        invoice_amount TYPE  zfreight_item-invoice_amount,
        weight         TYPE  zfreight_item-weight,
        company_code   TYPE  zfreight_item-company_code,
        location       TYPE  zfreight_item-location,
        lr_no          TYPE  zfreight_item-lr_no,
        status         TYPE  zfreight_item-status,
      END OF ty_zfritm.


DATA: it_zfritm TYPE TABLE OF ty_zfritm,
      wa_zfritm TYPE ty_zfritm.

*
*
*DATA: IT_ZFRITM TYPE TABLE OF ZFREIGHT_ITEM,
*      WA_ZFRITM TYPE ZFREIGHT_ITEM.
*
*DATA: IT_ZFRIHED TYPE TABLE OF ZFREIGHT_HEADER,
*      WA_ZFRIHED TYPE ZFREIGHT_HEADER.

TYPES:BEGIN OF ty_kna1_1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        name2 TYPE kna1-name2,
        name3 TYPE kna1-name3,                "ADD BY SANTHOSHKUMAR
      END OF ty_kna1_1.

DATA: it_kna1_1 TYPE TABLE OF ty_kna1_1,
      wa_kna1_1 TYPE ty_kna1_1.


TYPES:BEGIN OF ty_kna1_name3,             "ADD BY SANTHOSHKUMAR
        kunnr_name3 TYPE vbrk-kunag,
      END OF ty_kna1_name3.

DATA: it_kna1_name3 TYPE TABLE OF ty_kna1_name3,
      wa_kna1_name3 TYPE ty_kna1_name3.


*--------------------------------------------------------------*
*INTERNAL TABLE DECLARATIONS
*--------------------------------------------------------------*



DATA: it_vbrk       TYPE TABLE OF ty_vbrk_vbrp,
      it_tvkbt      TYPE TABLE OF ty_tvkbt,
      it_tspat      TYPE TABLE OF ty_tspat,
      it_kna1       TYPE TABLE OF ty_kna1,
      it_mara       TYPE TABLE OF ty_mara,
      it_mara1      TYPE TABLE OF ty_mara1,
      it_mara5      TYPE TABLE OF ty_mara,
      it_vbrk2      TYPE TABLE OF ty_vdrk2,
      it_tvkbt2     TYPE TABLE OF ty_tvkbt2,
      it_tspat2     TYPE TABLE OF ty_tspat2,
      it_kna12      TYPE TABLE OF ty_kna12,
      it_marm       TYPE TABLE OF ty_marm,
      it_likp       TYPE TABLE OF ty_likp,
      it_vbrp       TYPE TABLE OF ty_vbrp,
      it_vbrp3      TYPE TABLE OF ty_vbrp3,
      it_finovr     TYPE TABLE OF ty_finovr,
      it_finaldet   TYPE TABLE OF ty_finaldet,

      it_docdata    TYPE STANDARD TABLE OF sodocchgi1,
      it_packlist   TYPE STANDARD TABLE OF sopcklsti1,
      it_attachment TYPE STANDARD TABLE OF solisti1,
      it_body_msg   TYPE STANDARD TABLE OF solisti1,
      it_receivers  TYPE STANDARD TABLE OF somlreci1,
      it_rsparams   TYPE STANDARD TABLE OF rsparams.                                   " INTERNAL TABLE FOR TAKING SELECTION SCREEN VALUES FOR STOCK



DATA : layout TYPE slis_layout_alv.                   "Changed by Savariar S as on 08/01/2015.

*--------------------------------------------------------------*
*WORK AREA DECLARATIONS
*--------------------------------------------------------------*

DATA: wa_vbrk       TYPE ty_vbrk_vbrp,
      wa_tvkbt      TYPE  ty_tvkbt,
      wa_tspat      TYPE ty_tspat,
      wa_kna1       TYPE ty_kna1,
      wa_mara       TYPE ty_mara,
      wa_mara1      TYPE ty_mara1,
      wa_mara5      TYPE ty_mara5,
      wa_vbrk2      TYPE ty_vdrk2,
      wa_vbrktem    TYPE ty_vdrk2,
      wa_tvkbt2     TYPE  ty_tvkbt2,
      wa_tspat2     TYPE ty_tspat2,
      wa_kna12      TYPE ty_kna12,
      wa_finaldet   LIKE LINE OF it_finaldet,
      wa_tempdet    LIKE LINE OF it_finaldet,
      wa_finovr     LIKE LINE OF it_finovr,
      wa_marm       LIKE LINE OF it_marm,
      wa_likp       LIKE LINE OF it_likp,
      wa_vbrp       LIKE LINE OF it_vbrp,
      wa_vbrp3      LIKE LINE OF it_vbrp3,
*      WA_FINOVR1 LIKE LINE OF IT_FINOVR,
      wa_docdata    LIKE LINE OF it_docdata,
      wa_packlist   LIKE LINE OF it_packlist,
      wa_attachment LIKE LINE OF it_attachment,
      wa_body_msg   LIKE LINE OF it_body_msg,
      wa_receivers  LIKE LINE OF it_receivers,
      wa_rsparams   LIKE LINE OF it_rsparams.                                      " WORK AREA FOR TAKING SELECTION SCREEN VALUES FOR STOCK
DATA: it_knvp  TYPE TABLE OF ty_knvp,  "Added by Keerthi CLSS on 26.10.2016
      wa_knvp  TYPE ty_knvp,
      it_knvp1 TYPE TABLE OF ty_knvp,  "Added by Keerthi CLSS on 26.10.2016
      wa_knvp1 TYPE ty_knvp.



TYPES : BEGIN OF ty_t001w,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
        END OF ty_t001w.

DATA: wa_t001w TYPE ty_t001w,
      it_t001w TYPE TABLE OF ty_t001w.



SELECTION-SCREEN: BEGIN OF BLOCK b1.

  PARAMETERS: ps_parm AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND abc MODIF ID tb1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES

SELECTION-SCREEN: END OF BLOCK b1.



SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND uc1 MODIF ID tb2 ,          " SELECTION SCREEN FOR INVOICE

               r2 RADIOBUTTON GROUP g1 MODIF ID tb2 DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  SELECT-OPTIONS: so_bukrs FOR lv_bukrs,
                  so_fkdat FOR w_aux_fkdat MODIF ID tb2 DEFAULT sy-datum OBLIGATORY,        " SELECTION SCREEN ELEMENTS FOR INVOICE
                  so_fkart  FOR w_fkart MODIF ID tb2 DEFAULT 'YBFS',
                  so_vbeln FOR w_vbeln MODIF ID tb2,
                  so_kunag FOR w_kunag MODIF ID tb2,
                  so_name3 FOR w_name3 MODIF ID tb2,                                        "NAME3 ADD "ADD BY SANTHOSHKUMAR
                  so_matnr FOR w_matnr MODIF ID tb2,
                  so_charg FOR w_charg MODIF ID tb5,
                  so_vkbu2 FOR w_inv_vkbur MODIF ID lb2 OBLIGATORY,                                    " SELECT OPTIONS FOR SALES OFFICE IN invoice
                  so_spart FOR w_spart MODIF ID tb2 .

  SELECTION-SCREEN SKIP.

  PARAMETERS: p_cb AS CHECKBOX USER-COMMAND cbc MODIF ID ab2.
  PARAMETERS: p_sr AS CHECKBOX USER-COMMAND cbc MODIF ID ab3.
  PARAMETERS: p_fs AS CHECKBOX USER-COMMAND cbc MODIF ID ab4.
  PARAMETERS: p_ic AS CHECKBOX USER-COMMAND cbc MODIF ID ib2.
  PARAMETERS: p_st AS CHECKBOX .
SELECTION-SCREEN: END OF BLOCK b2.

INITIALIZATION.

  gwa_list-key = '1'.
  gwa_list-text = 'INVOICE'.
  APPEND gwa_list TO gt_list.
  CLEAR: gwa_list.

  gwa_list-key = '2'.
  gwa_list-text = 'ACCOUNTBALANCES'.
  APPEND gwa_list TO gt_list.
  CLEAR: gwa_list.

  gwa_list-key = '3'.
  gwa_list-text = 'STOCK'.
  APPEND gwa_list TO gt_list.
  CLEAR: gwa_list.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'PS_PARM'
      values          = gt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


  IF ps_parm IS INITIAL.                                                 " TO SET SET THE INITIAL VALUE SHOWN IN LIST BOX AS 'INVOICE'

    ps_parm = '1'.

  ENDIF.



  LOOP AT SCREEN.


    IF screen-name = 'PS_PARM'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.


*ENDIF.


AT SELECTION-SCREEN OUTPUT.

  IF r1 = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'SO_CHARG-LOW'.
        screen-active = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'SO_CHARG-HIGH'.
        screen-active = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'SO_MATNR-LOW'.
        screen-active = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'SO_MATNR-HIGH'.
        screen-active = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-group1 = 'TB1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.



  IF r2 = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'SO_MATNR'.
        screen-active = 1.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'SO_CHARG'.
        screen-active = 1.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'PS_PARM'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.


  LOOP AT SCREEN.


    IF p_cb  = 'X' AND screen-group1 = 'IB2'  .                                             " added by manikandan 07.03.2016

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.
    IF p_sr   = 'X' AND screen-group1 = 'IB2'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.

    IF p_fs   = 'X' AND screen-group1 = 'IB2'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.

    IF p_ic = 'X' AND screen-group1 = 'AB2'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.


    IF p_ic = 'X' AND screen-group1 = 'AB3'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.


    IF p_ic = 'X' AND screen-group1 = 'AB4'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.

    IF p_ic = 'X' AND screen-group1 = 'LB2'  .

      screen-input = 0.

      MODIFY SCREEN .

    ENDIF.


  ENDLOOP.








*--------------------------------------------------------------*
*At Selection Screen Ouput ON List Box PS_PARM
*--------------------------------------------------------------*


AT SELECTION-SCREEN ON ps_parm.
  CLEAR: gwa_values, gt_values.
  REFRESH gt_values.
  gwa_values-fieldname = 'PS_PARM'.
  APPEND gwa_values TO gt_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = gt_values.

  READ TABLE gt_values INDEX 1 INTO gwa_values.
  IF sy-subrc = 0 AND gwa_values-fieldvalue IS NOT INITIAL.
    READ TABLE gt_list INTO gwa_list
                      WITH KEY key = gwa_values-fieldvalue.
    IF sy-subrc = 0.
      gv_selected_value = gwa_list-text.
    ENDIF.
  ENDIF.


START-OF-SELECTION.

  IF r1 = 'X'.

    PERFORM data_retrieval_overview.
    PERFORM authcheck_overview.
    PERFORM build_fieldcatalog_overview.
    PERFORM build_alv_overview.

  ELSEIF r2 = 'X'.

    PERFORM data_retrieval_detail.
    PERFORM authcheck_detail.
    PERFORM build_fieldcatalog_detail.
    PERFORM build_alv_detail.


  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_retrieval_overview .
  IF so_name3 <> ''.
    SELECT kunnr  FROM kna1 INTO TABLE it_kna1_name3 WHERE name3 IN so_name3.  "ADD BY SANTHOSHKUMAR
  ENDIF.

  IF p_sr <> 'X' AND p_cb <> 'X' AND  p_ic <> 'X' AND p_st <> 'X'. " Added by Goivnd On 05/06/2014
    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.
        SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                vbrk~fkdat
                vbrk~inco2
                vbrp~erdat
                vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015
                vbrk~spart
                vbrk~vbeln
                vbrk~fkart
                vbrk~kunag
                vbrk~kunrg
                vbrk~vkorg
                vbrk~vtweg
                vbrk~netwr
                vbrk~sfakn
                vbrk~knkli
                vbrk~mwsbk
                vbrk~fksto
                vbrk~date_of_delivery
                vbrk~time
                vbrk~remarks
                vbrp~posnr
                vbrp~vrkme
                vbrp~vkbur
                vbrp~matnr
                vbrp~fkimg
                vbrp~vgbel
                vbrp~volum
                vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

                INTO CORRESPONDING FIELDS OF TABLE it_vbrk
                FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3 "#EC CI_DB_OPERATION_OK[2768887] "ADD BY SANTHOSHKUMARAdded by <IT-CAR Tool> during Code Remediation
                WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
        AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND  fkart <> 'YBRE'  AND fkart <> 'YIRE' AND  "FKART <> 'YRF2' AND
        fkart <> 'YFRE' AND fkart <> 'YLRE'  AND fkart <> 'IVS' AND fkart <> 'IG' AND  vbrk~kunag = it_kna1_name3-kunnr_name3 AND "ADD BY SANTHOSHKUMAR
        vbrk~fkart IN so_fkart AND fksto <> 'X' . " ADDED BY RAM ON 1/8/15
      ENDIF.
    ELSE.
      SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               vbrk~fkdat
               vbrk~inco2
               vbrp~erdat
               vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015
               vbrk~spart
               vbrk~vbeln
               vbrk~fkart
               vbrk~kunag
               vbrk~kunrg
               vbrk~vkorg
               vbrk~vtweg
               vbrk~netwr
               vbrk~sfakn
               vbrk~knkli
               vbrk~mwsbk
               vbrk~fksto
               vbrk~date_of_delivery
               vbrk~time
               vbrk~remarks
               vbrp~posnr
               vbrp~vrkme
               vbrp~vkbur
               vbrp~matnr
               vbrp~fkimg
               vbrp~vgbel
               vbrp~volum
               vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015
               INTO CORRESPONDING FIELDS OF TABLE it_vbrk
               FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
       AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND  fkart <> 'YBRE'  AND fkart <> 'YIRE' AND  "FKART <> 'YRF2' AND
       fkart <> 'YFRE' AND fkart <> 'YLRE'  AND fkart <> 'IVS' AND fkart <> 'IG' AND
       vbrk~fkart IN so_fkart AND fksto <> 'X'  . " ADDED BY RAM ON 1/8/15
    ENDIF.

    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ELSEIF p_cb = 'X'. " Added by Goivnd On 05/06/2014
    IF so_name3 <> ''.
      SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               vbrk~fkdat
               vbrk~inco2
               vbrk~spart
               vbrk~vbeln
               vbrk~fkart
               vbrk~kunag
               vbrk~kunrg
               vbrk~vkorg
               vbrk~vtweg
               vbrk~netwr
               vbrk~sfakn
               vbrk~knkli
               vbrk~mwsbk
               vbrk~fksto
               vbrk~date_of_delivery
               vbrk~time
               vbrk~remarks
               vbrp~posnr
               vbrp~vrkme
               vbrp~vkbur
               vbrp~matnr
               vbrp~fkimg

               vbrp~erdat
               vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015

               vbrp~vgbel
               vbrp~volum
               vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

               INTO CORRESPONDING FIELDS OF TABLE it_vbrk
               FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3 "#EC CI_DB_OPERATION_OK[2768887] "ADD BY SANTHOSHKUMARAdded by <IT-CAR Tool> during Code Remediation
               WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln AND vbrk~kunag = it_kna1_name3-kunnr_name3 "ADD BY SANTHOSHKUMAR
       AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND vbrk~fkart IN so_fkart AND fksto = 'X'.
    ELSE.
      SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                vbrk~fkdat
                vbrk~inco2
                vbrk~spart
                vbrk~vbeln
                vbrk~fkart
                vbrk~kunag
                vbrk~kunrg
                vbrk~vkorg
                vbrk~vtweg
                vbrk~netwr
                vbrk~sfakn
                vbrk~knkli
                vbrk~mwsbk
                vbrk~fksto
                vbrk~date_of_delivery
                vbrk~time
                vbrk~remarks
                vbrp~posnr
                vbrp~vrkme
                vbrp~vkbur
                vbrp~matnr
                vbrp~fkimg

                vbrp~erdat
                vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015

                vbrp~vgbel
                vbrp~volum
                vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

                INTO CORRESPONDING FIELDS OF TABLE it_vbrk
                FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
        AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND vbrk~fkart IN so_fkart AND fksto = 'X'.

    ENDIF.
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ELSEIF p_sr = 'X' . " Added by Goivnd On 05/06/2014

    IF so_name3 <> ''.
      SELECT  vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrk~fkdat
          vbrk~inco2
          vbrk~erdat
          vbrk~erzet                                 "CHANGED BY RAM ON 8/8/2015
          vbrk~spart
          vbrk~vbeln
          vbrk~fkart
          vbrk~kunag
          vbrk~kunrg
          vbrk~vkorg
          vbrk~vtweg
          vbrk~netwr
          vbrk~sfakn
          vbrk~knkli
          vbrk~mwsbk
          vbrk~fksto
           vbrk~date_of_delivery
           vbrk~time
           vbrk~remarks
          vbrp~posnr
          vbrp~vrkme
          vbrp~vkbur
          vbrp~matnr
          vbrp~fkimg
          vbrp~vgbel
          vbrp~volum

          vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

          INTO CORRESPONDING FIELDS OF TABLE it_vbrk
          FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3 "#EC CI_DB_OPERATION_OK[2768887] "ADD BY SANTHOSHKUMARAdded by <IT-CAR Tool> during Code Remediation
          WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln AND  vbrk~kunag = it_kna1_name3-kunnr_name3 "ADD BY SANTHOSHKUMAR
  AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE')
  AND fkart <> 'YRF2'
   AND vbrk~fkart IN so_fkart AND fksto <> 'X' . "ADDED BY RAM ON 1/8/15
    ELSE.

      SELECT  vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrk~fkdat
          vbrk~inco2
        vbrk~erdat
        vbrk~erzet                                 "CHANGED BY RAM ON 8/8/2015

          vbrk~spart
          vbrk~vbeln
          vbrk~fkart
          vbrk~kunag
          vbrk~kunrg
          vbrk~vkorg
          vbrk~vtweg
          vbrk~netwr
          vbrk~sfakn
          vbrk~knkli
          vbrk~mwsbk
          vbrk~fksto
           vbrk~date_of_delivery
           vbrk~time
           vbrk~remarks
          vbrp~posnr
          vbrp~vrkme
          vbrp~vkbur
          vbrp~matnr
          vbrp~fkimg
          vbrp~vgbel
          vbrp~volum

          vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

          INTO CORRESPONDING FIELDS OF TABLE it_vbrk
          FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
  AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE')
  AND fkart <> 'YRF2'
   AND vbrk~fkart IN so_fkart AND fksto <> 'X' . "ADDED BY RAM ON 1/8/15
      SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
      DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
    ENDIF.
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ENDIF.

  IF p_ic = 'X'.                                                                   " ADDED BY MANI 04.03.2016
    IF so_name3 <> ''.

      SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                vbrk~fkdat
                vbrk~inco2
                  vbrp~erdat
                  vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015

                  vbrk~spart
                  vbrk~vbeln
                  vbrk~fkart
                  vbrk~kunag
                  vbrk~kunrg
                  vbrk~vkorg
                  vbrk~vtweg
                  vbrk~netwr
                  vbrk~sfakn
                  vbrk~knkli
                  vbrk~mwsbk
                  vbrk~fksto
                  vbrk~date_of_delivery
                  vbrk~time
                  vbrk~remarks
                  vbrp~posnr
                  vbrp~vrkme
                  vbrp~vkbur
                  vbrp~matnr
                  vbrp~fkimg
                  vbrp~vgbel
                  vbrp~volum
                    vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015

                  INTO CORRESPONDING FIELDS OF TABLE it_vbrk
                  FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3 "#EC CI_DB_OPERATION_OK[2768887] "ADD BY SANTHOSHKUMARAdded by <IT-CAR Tool> during Code Remediation
                  WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln AND vbrk~kunag = it_kna1_name3-kunnr_name3 AND "ADD BY SANTHOSHKUMAR
                  vbrk~kunag IN so_kunag AND  vbrk~spart IN so_spart AND
                  vbrk~fkart IN so_fkart AND ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG' ) .                            " ADDED BY MANIKANDAN 05.03.2016
    ELSE.
      SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               vbrk~fkdat
               vbrk~inco2
               vbrp~erdat
               vbrp~erzet                                 "CHANGED BY RAM ON 8/8/2015
               vbrk~spart
               vbrk~vbeln
               vbrk~fkart
               vbrk~kunag
               vbrk~kunrg
               vbrk~vkorg
               vbrk~vtweg
               vbrk~netwr
               vbrk~sfakn
               vbrk~knkli
               vbrk~mwsbk
               vbrk~fksto
               vbrk~date_of_delivery
               vbrk~time
               vbrk~remarks
               vbrp~posnr
               vbrp~vrkme
               vbrp~vkbur
               vbrp~matnr
               vbrp~fkimg
               vbrp~vgbel
               vbrp~volum
               vbrp~aubel                                 "  ADDED BY MANIKANDAN ON 28-08-2015
               INTO CORRESPONDING FIELDS OF TABLE it_vbrk
               FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln AND
               vbrk~kunag IN so_kunag AND  vbrk~spart IN so_spart AND
               vbrk~fkart IN so_fkart AND ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG' ) .                            " ADDED BY MANIKANDAN 05.03.2016

    ENDIF.

    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .

  ENDIF.
  "added on 3/9
  IF p_st = 'X'.                         " Added By PR@$@TH on 26/08/2019  Desc:Stock Transfer Report Copy(ZKP_SD_SALES_REGISTER program)

    IF so_name3 <> ''.

      SELECT  vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              vbrp~werks
              vbrk~fkdat
              vbrk~inco2
              vbrp~erdat
              vbrp~erzet
              vbrk~spart
              vbrk~vbeln
              vbrk~fkart
              vbrk~kunag
              vbrk~kunrg
              vbrk~vkorg
              vbrk~vtweg
              vbrk~netwr
              vbrk~sfakn
              vbrk~knkli
              vbrk~mwsbk
              vbrk~fksto
              vbrk~date_of_delivery
              vbrk~time
              vbrk~remarks
              vbrp~posnr
              vbrp~vrkme
              vbrp~vkbur
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgbel
              vbrp~volum
              vbrp~aubel
              INTO CORRESPONDING FIELDS OF TABLE it_vbrk FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3 "ADD BY SANTHOSHKUMAR
              WHERE vbrk~bukrs IN so_bukrs
                AND vbrk~fkdat IN so_fkdat
                AND vbrk~vbeln IN so_vbeln
                AND vbrk~kunag IN so_kunag
                AND vbrp~werks IN so_vkbu2 AND  vbrk~kunag = it_kna1_name3-kunnr_name3 "ADD BY SANTHOSHKUMAR
*              AND VBRK~SPART IN SO_SPART
                AND vbrk~vtweg = '30' AND ( vbrk~fkart EQ 'YSTO' OR vbrk~fkart EQ 'S1') .
    ELSE.
      SELECT  vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                vbrp~werks
                vbrk~fkdat
                vbrk~inco2
                vbrp~erdat
                vbrp~erzet
                vbrk~spart
                vbrk~vbeln
                vbrk~fkart
                vbrk~kunag
                vbrk~kunrg
                vbrk~vkorg
                vbrk~vtweg
                vbrk~netwr
                vbrk~sfakn
                vbrk~knkli
                vbrk~mwsbk
                vbrk~fksto
                vbrk~date_of_delivery
                vbrk~time
                vbrk~remarks
                vbrp~posnr
                vbrp~vrkme
                vbrp~vkbur
                vbrp~matnr
                vbrp~fkimg
                vbrp~vgbel
                vbrp~volum
                vbrp~aubel
                INTO CORRESPONDING FIELDS OF TABLE it_vbrk FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                WHERE vbrk~bukrs IN so_bukrs
                  AND vbrk~fkdat IN so_fkdat
                  AND vbrk~vbeln IN so_vbeln
                  AND vbrk~kunag IN so_kunag
                  AND vbrp~werks IN so_vkbu2
*              AND VBRK~SPART IN SO_SPART
                  AND vbrk~vtweg = '30' AND ( vbrk~fkart EQ 'YSTO' OR vbrk~fkart EQ 'S1') .

    ENDIF.
    "DELETE IT_VBRK WHERE FKIMG EQ '0.00'.

    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .

  ENDIF.

  "ADDED BY JESTOP ON 30.10.2020

  SELECT werks
         name1 FROM t001w INTO TABLE it_t001w
               FOR ALL ENTRIES IN it_vbrk
               WHERE werks = it_vbrk-werks.


  "commended By Ram on 19/10/2015
  " IF SY-SUBRC = 0.
  SELECT vbeln posnr fkimg brgew volum matnr FROM vbrp INTO  CORRESPONDING FIELDS OF TABLE it_vbrp FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.
  " ENDIF.

  " SELECT MATNR VOLUM VOLEH FROM MARA INTO CORRESPONDING FIELDS OF TABLE IT_MARA FOR ALL ENTRIES IN IT_VBRP WHERE MATNR = IT_VBRP-MATNR.


  SELECT vbeln
         kunnr
         btgew FROM likp INTO TABLE it_likp FOR ALL ENTRIES IN it_vbrk
         WHERE vbeln = it_vbrk-vgbel.

  IF it_likp IS NOT INITIAL.
    SELECT kunnr
           name1
           name2
           name3 FROM kna1 INTO TABLE it_kna1_1 FOR ALL ENTRIES IN it_likp WHERE kunnr = it_likp-kunnr. "ADD BY SANTHOSHKUMAR
  ENDIF.

  SELECT vbeln                                                               "  ADDED BY MANIKANDAN ON 28-08-2015
          erdat
          erzet
          vsnmr_v FROM vbak INTO TABLE it_vbak                              "  ADDED BY MANIKANDAN ON 26-09-2015
          FOR ALL ENTRIES IN it_vbrk
          WHERE  vbeln = it_vbrk-aubel.

  SELECT invoiceno                                                             "  ADDED BY MANIKANDAN ON 29-09-2015
         transporternamelrno
         vehiclenodrivername
         uniq1
         tdate
         time
         cartons
         city FROM ztrip_st INTO TABLE it_ztrip_st                         "  ADDED BY MANIKANDAN ON  15-10-2015
         FOR ALL ENTRIES IN  it_vbrk
         WHERE  invoiceno = it_vbrk-vbeln .

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Added By PR@$@TH On 29/08/2019

  SELECT trip_no
         invoice_no
         customer_name
         invoice_date
         invoice_amount
         weight
         company_code
         location
         lr_no
         status FROM zfreight_item INTO TABLE it_zfritm FOR ALL ENTRIES IN it_vbrk WHERE invoice_no = it_vbrk-vbeln .

  IF it_zfritm IS NOT INITIAL.
    SELECT trip_no
           status
           vendor_code
           vendor_name
           from_code
           from_loc
           city_id
           to_loc
           truck_type
           truck_des
           filling_type
           filing_des
           freight_charge
           loding_charge
           unload_charge
           halt_charge
           lr_charge
           no_of_days
           vechile_number
           crdate
           crtime
           created_by
           trn_value
           apr_sta
           remarks FROM zfreight_header INTO TABLE it_zfrihed FOR ALL ENTRIES IN it_zfritm WHERE trip_no = it_zfritm-trip_no .
  ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Ended By PR@$@TH On 29/08/2019



*  IF it_vbak IS NOT INITIAL.
*    SELECT
*      vbeln
*      parvw
*      pernr
*      FROM vbpa INTO TABLE it_vbpa                              "  ADDED BY RAM ON 05-07-2016
*          FOR ALL ENTRIES IN it_vbak
*          WHERE  vbeln = it_vbak-vbeln AND parvw = 'L5'.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*    SELECT
*  vbeln
*  parvw
*  pernr
*  FROM vbpa INTO TABLE it_vbpa1
*      FOR ALL ENTRIES IN it_vbak
*      WHERE  vbeln = it_vbak-vbeln AND parvw = 'L3'.
*  ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*  IF it_vbpa IS NOT INITIAL.
*    SELECT
*      pernr
*      endda
*      begda
*      sname
*        FROM pa0001 INTO TABLE it_pa0001                              "  ADDED BY RAM ON 05-07-2016
*        FOR ALL ENTRIES IN it_vbpa
*        WHERE pernr = it_vbpa-pernr.
*  ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*  IF it_vbpa1 IS NOT INITIAL.
*    SELECT
*       pernr
*       endda
*       begda
*       sname
*         FROM pa0001 INTO TABLE it_pa0001_1
*         FOR ALL ENTRIES IN it_vbpa1
*         WHERE pernr = it_vbpa1-pernr.
*  ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*


  TYPES: BEGIN OF ty_vbrk_temp,
           spart TYPE vbrk-spart,
           vkbur TYPE vbrp-vkbur,
           kunag TYPE vbrk-kunag,
           kunrg TYPE vbrk-kunrg,
         END OF ty_vbrk_temp.

  DATA: it_vbrk_temp TYPE STANDARD TABLE OF ty_vbrk_temp,
        wa_vbrk_temp LIKE LINE OF it_vbrk_temp.

  LOOP AT it_vbrk INTO wa_vbrk.

    MOVE-CORRESPONDING wa_vbrk TO wa_vbrk_temp.
    APPEND wa_vbrk_temp TO it_vbrk_temp.
    CLEAR: wa_vbrk, wa_vbrk_temp.

  ENDLOOP.

  SORT it_vbrk_temp BY spart vkbur kunag kunrg.

  "DELETE ADJACENT DUPLICATES FROM IT_VBRK_TEMP COMPARING ALL FIELDS.

  IF it_vbrk_temp IS NOT INITIAL.

    SELECT vkbur
           bezei
           FROM tvkbt INTO TABLE it_tvkbt
           FOR ALL ENTRIES IN it_vbrk_temp
           WHERE vkbur = it_vbrk_temp-vkbur
           AND spras = 'EN'.

    SELECT spart
           vtext
           FROM tspat INTO TABLE it_tspat
           FOR ALL ENTRIES IN it_vbrk_temp
           WHERE spart = it_vbrk_temp-spart
           AND spras = 'EN'.

    SELECT kunnr
           name1
           name2
           name3
           FROM kna1 INTO TABLE it_kna1
           FOR ALL ENTRIES IN it_vbrk_temp
           WHERE kunnr = it_vbrk_temp-kunrg AND kunnr = it_vbrk_temp-kunag.

  ENDIF.


  LOOP AT it_vbrk INTO wa_vbrk.

    wa_finovr-fkdat = wa_vbrk-fkdat.
    wa_finovr-bukrs = wa_vbrk-bukrs.
    wa_finovr-werks = wa_vbrk-werks.
    CLEAR wa_t001w.
    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_finovr-werks.
    IF sy-subrc = 0.
      wa_finovr-name = wa_t001w-name1.
    ENDIF.
    wa_finovr-spart = wa_vbrk-spart.
    wa_finovr-vbeln = wa_vbrk-vbeln.
    wa_finovr-fkart = wa_vbrk-fkart.
    wa_finovr-kunag = wa_vbrk-kunag.
    wa_finovr-kunrg = wa_vbrk-kunrg.
    wa_finovr-vkorg = wa_vbrk-vkorg.
    wa_finovr-vtweg = wa_vbrk-vtweg.
    wa_finovr-netwr = wa_vbrk-netwr.
    wa_finovr-sfakn = wa_vbrk-sfakn.
*    wa_finovr-posnr = wa_vbrk-posnr.
    wa_finovr-vkbur = wa_vbrk-vkbur.
*    WA_FINOVR-MWSBK = WA_FINOVR-MWSBK. " Modified By Govind
    wa_finovr-mwsbk = wa_vbrk-mwsbk.
    wa_finovr-date_of_delivery = wa_vbrk-date_of_delivery.
    wa_finovr-time = wa_vbrk-time.
    wa_finovr-remarks = wa_vbrk-remarks.
    wa_finovr-city = wa_vbrk-inco2 .

    IF p_cb NE 'X' .
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG' .
        wa_finovr-netwr = - ( wa_finovr-netwr ).
        wa_finovr-mwsbk = - ( wa_finovr-mwsbk ) .

      ENDIF .
    ENDIF .

    wa_finovr-inv_val = wa_vbrk-netwr + wa_vbrk-mwsbk.

    IF p_cb NE 'X' .
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG' .

        wa_finovr-inv_val = - ( wa_finovr-inv_val ) .
      ENDIF .
    ENDIF .
    wa_finovr-erzet = wa_vbrk-erzet .                               "CHANGED BY RAM ON 8/8/2015
    wa_finovr-erdat = wa_vbrk-erdat .



    wa_finovr-aubel = wa_vbrk-aubel .                   "  ADDED BY MANIKANDAN ON 28-08-2015


    wa_finovr-invoiceno = wa_vbrk-invoiceno .           "  ADDED BY MANIKANDAN ON 29-09-2015
    wa_finovr-uniq1 = wa_vbrk-uniq1 .                   "  ADDED BY MANIKANDAN ON 29-09-2015
    wa_finovr-ttdate = wa_vbrk-ttdate .                 "  ADDED BY MANIKANDAN ON 29-08-2015
    wa_finovr-ttime = wa_vbrk-ttime .                   "  ADDED BY MANIKANDAN ON 29-08-2015
    wa_finovr-cartons = wa_vbrk-cartons .                    "  ADDED BY MANIKANDAN ON  15-10-2015
    " WA_FINOVR-CITY = WA_VBRK-CITY .                    "  ADDED BY MANIKANDAN ON  15-10-2015



    wa_finovr-vsnmr_v = wa_vbrk-vsnmr_v .                   "  ADDED BY MANIKANDAN ON 26-09-2015




    LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.

      wa_finovr-qty_ltr =  wa_finovr-qty_ltr + wa_vbrp-volum.
      wa_finovr-btgew = wa_finovr-btgew + wa_vbrp-brgew.
      "WA_FINOVR-BTGEW = WA_FINOVR-BTGEW + WA_VBRP- . "ADDED ON 29/5

*      IF p_cb NE 'X' .                                                                         " added by mani 24.02.2016
*        IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.     " added by mani 19.02.2016
*
*          wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ) .
*        ENDIF .
*      ENDIF .


    ENDLOOP.

    "STARTED BY RAM ON 19/10/2015 FOR VOLUM CORRECTION

*    IF WA_FINOVR-QTY_LTR = 0.
*
*        SELECT VBELN POSNR FKIMG VOLUM MATNR FROM VBRP INTO  CORRESPONDING FIELDS OF TABLE IT_VBRP3 FOR ALL ENTRIES IN IT_VBRK WHERE VBELN = IT_VBRK-VBELN.
*
*        SELECT MATNR VOLUM VOLEH FROM MARA INTO CORRESPONDING FIELDS OF TABLE IT_MARA5 FOR ALL ENTRIES IN IT_VBRP3 WHERE MATNR = IT_VBRP3-MATNR.
*
* LOOP AT IT_VBRP3 INTO WA_VBRP3 WHERE VBELN = WA_VBRK-VBELN .
*
*  LOOP AT IT_MARA5 INTO WA_MARA5 WHERE MATNR = WA_VBRP3-MATNR.
*
*     "   READ TABLE IT_MARA5 INTO WA_MARA5 WITH KEY MATNR = WA_VBR-MATNR .
*
*        WA_FINOVR-QTY_LTR1 = WA_MARA5-VOLUM * WA_VBRP3-FKIMG.
*
*        WA_FINOVR-QTY_LTR = WA_FINOVR-QTY_LTR + WA_FINOVR-QTY_LTR1.
*ENDLOOP.
*
*ENDLOOP .
*    ENDIF.

    "ENDED BY RAM ON 19/10/2015
*    READ TABLE IT_LIKP INTO WA_LIKP WITH KEY VBELN = WA_VBRK-VGBEL.
*
*    IF SY-SUBRC = 0.
*
*      WA_FINOVR-BTGEW = WA_LIKP-BTGEW.
*
*    ENDIF.

    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrk-vkbur.
    IF sy-subrc = 0.
      wa_finovr-bezei = wa_tvkbt-bezei.
    ENDIF.

    READ TABLE it_tspat INTO wa_tspat WITH KEY spart = wa_vbrk-spart.

    IF sy-subrc = 0.
      wa_finovr-vtext = wa_tspat-vtext.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunag .

    IF sy-subrc = 0.
      wa_finovr-name1 = wa_kna1-name1.
      wa_finovr-name2 = wa_kna1-name1.
      wa_finovr-name3 = wa_kna1-name3. "ADD BY SANTHOSHKUMAR
    ENDIF.


    READ TABLE it_vbak INTO wa_vbak                                "  ADDED BY MANIKANDAN ON 28-08-2015
       WITH KEY vbeln = wa_vbrk-aubel.
    IF sy-subrc = 0 .
      wa_finovr-erdat1 = wa_vbak-erdat.
      wa_finovr-erzet1 = wa_vbak-erzet.
      wa_finovr-vsnmr_v = wa_vbak-vsnmr_v.                              "  ADDED BY MANIKANDAN ON 26-09-2015
    ENDIF.

    READ TABLE it_ztrip_st INTO wa_ztrip_st
        WITH KEY invoiceno = wa_vbrk-vbeln.                             "  ADDED BY MANIKANDAN ON 29-09-2015

    IF sy-subrc = 0.
      wa_finovr-invoiceno = wa_ztrip_st-invoiceno.
      wa_finovr-uniq1 = wa_ztrip_st-uniq1.
      wa_finovr-ttdate = wa_ztrip_st-ttdate.
      wa_finovr-ttime = wa_ztrip_st-ttime.
      wa_finovr-cartons = wa_ztrip_st-cartons.                         "  ADDED BY MANIKANDAN ON  15-10-2015
      wa_finovr-to_city = wa_ztrip_st-city.
      wa_finovr-tr_name = wa_ztrip_st-transporternamelrno.
      wa_finovr-v_no = wa_ztrip_st-vehiclenodrivername.

      " WA_FINOVR-CITY = WA_ZTRIP_ST-CITY.                         "  ADDED BY MANIKANDAN ON  15-10-2015
    ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Added By PR@$@TH On 29/08/2019
    IF wa_finovr-tr_name IS INITIAL OR wa_finovr-to_city IS INITIAL OR wa_finovr-v_no IS INITIAL OR wa_finovr-uniq1 IS INITIAL
       OR wa_finovr-cartons IS INITIAL OR wa_finovr-ttdate IS INITIAL OR wa_finovr-ttime IS INITIAL .

      READ TABLE it_zfritm INTO wa_zfritm WITH KEY invoice_no = wa_vbrk-vbeln.
      READ TABLE it_zfrihed INTO wa_zfrihed WITH KEY trip_no = wa_zfritm-trip_no.

      IF wa_finovr-tr_name IS INITIAL.
        wa_finovr-tr_name = wa_zfrihed-vendor_name .
      ENDIF.

      IF wa_finovr-to_city IS INITIAL.
        wa_finovr-to_city = wa_zfrihed-to_loc .
      ENDIF.

      IF wa_finovr-v_no IS INITIAL.
        wa_finovr-v_no = wa_zfrihed-vechile_number .
      ENDIF.

      IF wa_finovr-uniq1 IS INITIAL.
        wa_finovr-uniq1 = wa_zfritm-trip_no.
      ENDIF.

      IF wa_finovr-cartons IS INITIAL.
        wa_finovr-cartons = wa_zfritm-lr_no .
      ENDIF.

      IF wa_finovr-ttdate IS INITIAL.
        wa_finovr-ttdate = wa_zfrihed-crdate .
      ENDIF.

      IF wa_finovr-ttime IS INITIAL.
        wa_finovr-ttime = wa_zfrihed-crtime .
      ENDIF.

    ENDIF.

    READ TABLE it_likp INTO wa_likp WITH KEY vbeln = wa_vbrk-vgbel.
    READ TABLE it_kna1_1 INTO wa_kna1_1 WITH KEY kunnr = wa_likp-kunnr.

    wa_finovr-stp_no = wa_kna1_1-kunnr .
    wa_finovr-stp_nam = wa_kna1_1-name1 .

    IF wa_finovr-stp_nam IS INITIAL.
      wa_finovr-stp_nam = wa_kna1_1-name2 .
    ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Ended By PR@$@TH On 29/08/2019

*    READ TABLE it_vbpa INTO wa_vbpa
*       WITH KEY vbeln = wa_vbak-vbeln.                             "  ADDED BY RAM ON 5/7/2016
*
*    IF sy-subrc = 0.
*      wa_finovr-pernr  = wa_vbpa-pernr .
*    ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*    READ TABLE it_vbpa1 INTO wa_vbpa1
*       WITH KEY vbeln = wa_vbak-vbeln.
*
*    IF sy-subrc = 0.
*      wa_finovr-sm_no  = wa_vbpa1-pernr.
*    ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*    IF wa_finovr-pernr IS NOT INITIAL .
*      READ TABLE it_pa0001 INTO wa_pa0001
*         WITH KEY pernr = wa_vbpa-pernr.                             "  ADDED BY RAM ON 5/7/2016
*
*      IF sy-subrc = 0.
*        wa_finovr-sname   = wa_pa0001-sname  .
*      ENDIF.
*    ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*    IF wa_finovr-sm_no IS NOT INITIAL .
*      READ TABLE it_pa0001_1 INTO wa_pa0001_1
*         WITH KEY pernr = wa_vbpa1-pernr.
*
*      IF sy-subrc = 0.
*        wa_finovr-sm_name   = wa_pa0001_1-sname  .
*      ENDIF.
*
**&-------End of Keerthi CLSS on 24-08-2016------*
*
*    ENDIF.
    APPEND wa_finovr TO it_finovr.
    CLEAR : wa_finovr , wa_ztrip_st , wa_zfritm , wa_zfrihed , wa_kna1_1 , wa_likp .", wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
  ENDLOOP.


  DATA: it_temp TYPE STANDARD TABLE OF ty_finovr,
        wa_temp LIKE LINE OF it_temp.


  DATA: cnt TYPE i VALUE '0'.


  IF p_cb EQ 'X'.

    LOOP AT it_finovr INTO wa_finovr.

      IF wa_finovr-sfakn IS NOT INITIAL.

        wa_finovr-candoc = 'X'.

        MODIFY it_finovr FROM wa_finovr TRANSPORTING candoc.

        APPEND wa_finovr TO it_temp.

        CLEAR wa_finovr.

      ENDIF.

    ENDLOOP.


    LOOP AT it_finovr INTO wa_finovr.

      READ TABLE it_temp INTO wa_temp WITH KEY sfakn = wa_finovr-vbeln.

      IF sy-subrc = 0.

        MODIFY it_finovr FROM wa_temp TRANSPORTING candoc.

      ENDIF.

    ENDLOOP.

  ENDIF.

  IF p_ic NE 'X'.                                                       " ADDED BY MANI 04.03.2016
    IF p_cb NE 'X'.

      LOOP AT it_finovr INTO wa_finovr.

        cnt = cnt + 1.
        IF wa_finovr-sfakn IS NOT INITIAL.

          wa_temp-sfakn = wa_finovr-sfakn.

          APPEND wa_temp TO it_temp.

          DELETE it_finovr INDEX cnt.

          cnt = cnt - 1.

        ENDIF.
      ENDLOOP.

      LOOP AT it_temp INTO wa_temp.

        DELETE it_finovr WHERE vbeln = wa_temp-sfakn.

      ENDLOOP.

      " DELETE IT_FINOVR WHERE VKBUR = ''.                        "ADDED BY RAM ON 2/2/16

    ENDIF.
  ENDIF.


  DATA: sum_cust TYPE netwr,
        sum_off  TYPE netwr.




  SORT it_finovr BY fkdat vkbur kunrg.

  LOOP AT it_finovr INTO wa_finovr.

    sum_cust = wa_finovr-netwr + sum_cust + wa_finovr-mwsbk.


    AT END OF kunrg.

      wa_finovr-sum_cust = sum_cust.

      MODIFY it_finovr FROM wa_finovr TRANSPORTING sum_cust.
      CLEAR: sum_cust.
    ENDAT.

  ENDLOOP.


  LOOP AT it_finovr INTO wa_finovr.




    counter = counter + 1.
    sum_off = wa_finovr-netwr + sum_off + wa_finovr-mwsbk.

    AT END OF vkbur.


      wa_finovr-v_count = counter.
      wa_finovr-sum_off = sum_off.

      MODIFY it_finovr FROM wa_finovr TRANSPORTING v_count sum_off.
      CLEAR: counter, sum_off.


    ENDAT.


  ENDLOOP.


*  LOOP AT IT_FINOVR INTO WA_FINOVR.
*    IF WA_FINOVR-V_COUNT IS INITIAL.
*      WA_FINOVR-V_COUNT = '-'.
*      MODIFY IT_FINOVR FROM WA_FINOVR TRANSPORTING V_COUNT.
*    ENDIF.
*    CLEAR:WA_FINOVR-V_COUNT.
*
*  ENDLOOP.


  LOOP AT it_finovr INTO wa_finovr.
    IF p_cb NE 'X' .                                                                         " added by mani 24.02.2016
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG'.


*  wa_finovr-QTY_LTR =  - ( wa_finovr-QTY_LTR ) .
        wa_finovr-btgew =  - ( wa_finovr-btgew ) .
        wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ) .

        MODIFY it_finovr FROM wa_finovr TRANSPORTING qty_ltr btgew.

      ENDIF.
    ENDIF.

  ENDLOOP.
*  break splabap.
*&--------Begin of Keerthi CLSS on 26.10.2016---------------*&
  CLEAR: it_pa0001, it_pa0001_1.
  IF it_finovr IS NOT INITIAL.
    SELECT kunnr
           vkorg
           vtweg
           spart
           parvw
           parza
           pernr
           FROM knvp INTO TABLE it_knvp FOR ALL ENTRIES IN it_finovr WHERE kunnr    = it_finovr-kunag
                                                                       AND vkorg    = it_finovr-vkorg
                                                                       AND vtweg    = it_finovr-vtweg
                                                                       AND spart    = it_finovr-spart
                                                                       AND parvw = 'L5'.   "L5 = SO      L3 = AS

    SELECT kunnr
       vkorg
       vtweg
       spart
       parvw
       parza
       pernr
       FROM knvp INTO TABLE it_knvp1 FOR ALL ENTRIES IN it_finovr WHERE kunnr    = it_finovr-kunag
                                                                   AND vkorg    = it_finovr-vkorg
                                                                   AND vtweg    = it_finovr-vtweg
                                                                   AND spart    = it_finovr-spart
                                                                   AND parvw = 'L3'.   "L5 = SO      L3 = AS
    SELECT pernr
           endda
           begda
           sname
           FROM pa0001 INTO TABLE it_pa0001 FOR ALL ENTRIES IN it_knvp WHERE pernr = it_knvp-pernr.

    SELECT pernr
        endda
        begda
        sname
        FROM pa0001 INTO TABLE it_pa0001_1 FOR ALL ENTRIES IN it_knvp1 WHERE pernr = it_knvp1-pernr.

  ENDIF.

  LOOP AT it_finovr INTO wa_finovr.

    READ TABLE it_knvp INTO wa_knvp WITH KEY kunnr = wa_finovr-kunag  vkorg = wa_finovr-vkorg vtweg = wa_finovr-vtweg spart = wa_finovr-spart parvw = 'L5'.
    IF sy-subrc = 0.
      wa_finovr-pernr = wa_knvp-pernr.
    ENDIF.
    READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_knvp-pernr.
    IF sy-subrc = 0.
      wa_finovr-sname = wa_pa0001-sname.
    ENDIF.
*&--------------------------------------------------------------------------------------------------------------------------------
    READ TABLE it_knvp1 INTO wa_knvp1 WITH KEY kunnr = wa_finovr-kunag  vkorg = wa_finovr-vkorg vtweg = wa_finovr-vtweg spart = wa_finovr-spart parvw = 'L3'.
    IF sy-subrc = 0.
      wa_finovr-sm_no = wa_knvp1-pernr.
    ENDIF.
    READ TABLE it_pa0001_1 INTO wa_pa0001_1 WITH KEY pernr =  wa_knvp1-pernr.
    IF sy-subrc = 0.
      wa_finovr-sm_name = wa_pa0001_1-sname.
    ENDIF.

    MODIFY it_finovr FROM wa_finovr TRANSPORTING pernr sname sm_no sm_name.

    CLEAR: wa_finovr, wa_knvp, wa_pa0001, wa_knvp1, wa_pa0001_1.
  ENDLOOP.
*&--------End of Keerthi CLSS on 26.10.2016---------------*&
  IF it_finovr IS INITIAL.


    MESSAGE 'No Data Exists for the Input' TYPE 'S' DISPLAY LIKE 'E'.

    STOP.

  ENDIF.

ENDFORM.                    "DATA_RETRIEVAL_OVERVIEW






" DATA_RETRIEVAL_OVERVIEW
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authcheck_overview .

*  LOOP AT IT_FINOVR INTO WA_FINOVR.
*    AUTHORITY-CHECK OBJECT 'ZINVOICE'
*    ID 'ZVKBUR' FIELD WA_FINOVR-VKBUR
*    ID 'ZSPART' FIELD WA_FINOVR-SPART
*   " ID 'ZSPART' DUMMY
*    ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC NE 0.
*      WA_FINOVR-FLAG = 'x' .
*      MODIFY IT_FINOVR FROM WA_FINOVR TRANSPORTING FLAG.
*      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*    ENDIF.
*     CLEAR: WA_FINOVR.
*ENDLOOP.
*    AUTHORITY-CHECK OBJECT 'ZINVOICE'
*    ID 'ZSPART' FIELD WA_FINOVR-SPART
*"    ID 'ZVKBUR' DUMMY
*    ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC NE 0.
*      WA_FINOVR-FLAG = 'x' .
*      MODIFY IT_FINOVR FROM WA_FINOVR TRANSPORTING FLAG.
*      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*    ENDIF.
*    CLEAR: WA_FINOVR.
*  ENDLOOP.

  " DELETE IT_FINOVR WHERE FLAG = 'x'.

  LOOP AT it_finovr INTO wa_finovr.
    AUTHORITY-CHECK OBJECT 'ZINVOICE'
    ID 'ZVKBUR' FIELD wa_finovr-vkbur
    ID 'ZSPART' FIELD wa_finovr-spart
   " ID 'ZSPART' DUMMY
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      wa_finovr-flag = 'x' .
      MODIFY it_finovr FROM wa_finovr TRANSPORTING flag.
      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.
    CLEAR: wa_finovr.
  ENDLOOP.

  DELETE it_finovr WHERE flag = 'x'.


ENDFORM.                    " AUTHCHECK_OVERVIEW



*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_overview .

  IF p_ic = 'X' .
    wa_fieldcat-fieldname   = 'BUKRS'.
    wa_fieldcat-seltext_m   = 'COMPANY CODE'.
    wa_fieldcat-col_pos     = 1.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

  ENDIF.

  IF p_st = 'X'.                                                          " ADDED BY JESTOP ON 30.10.2020

    wa_fieldcat-fieldname   = 'WERKS'.
    wa_fieldcat-seltext_m   = 'PLANT'.
    wa_fieldcat-col_pos     = 2.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

    wa_fieldcat-fieldname   = 'NAME'.
    wa_fieldcat-seltext_m   = 'PLANT NAME'.
    wa_fieldcat-col_pos     = 3.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

  ENDIF.


  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DATE'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'FKDAT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  IF p_st NE 'X'.

    wa_fieldcat-fieldname   = 'VKBUR'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 5.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

    wa_sort-fieldname = 'VKBUR'.
    wa_sort-up = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

    wa_fieldcat-fieldname   = 'BEZEI'.
    wa_fieldcat-seltext_m   = 'SALES DESCRIPTION'.
    wa_fieldcat-col_pos     = 6.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

  ENDIF.

  wa_fieldcat-fieldname   = 'VKORG'.
  wa_fieldcat-seltext_m   = 'SALES ORGANIZATION'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VTWEG'.
  wa_fieldcat-seltext_m   = 'DISTRIBUTION CHANNEL'.
  wa_fieldcat-col_pos     = 8.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION'.
  wa_fieldcat-col_pos     = 9.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION DESCRIPTION'.
  wa_fieldcat-col_pos     = 10.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VSNMR_V'.
  wa_fieldcat-seltext_m   = 'SALES REQ PLAN'.                                  "  ADDED BY MANIKANDAN ON 26-09-2015
  wa_fieldcat-col_pos     = 11.
*  WA_FIELDCAT-HOTSPOT     = 'X'.
*  WA_FIELDCAT-CFIELDNAME   = 'VBELN'.
*  WA_FIELDCAT-CTABNAME    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VBELN'.
  wa_fieldcat-seltext_m   = 'BILLING DOCUMENT'.
  wa_fieldcat-col_pos     = 12.
  wa_fieldcat-hotspot     = 'X'.
  wa_fieldcat-cfieldname   = 'VBELN'.
  wa_fieldcat-ctabname    = 'VBRP'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CANDOC'.
  wa_fieldcat-seltext_m   = 'CANCELLED DOCUMENT'.
  wa_fieldcat-col_pos     = 13.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'FKART'.
  wa_fieldcat-seltext_m   = 'BILLING TYPE'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*
*  WA_FIELDCAT-FIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-SELTEXT_M   = 'SOLD-TO-PARTY'.
*  WA_FIELDCAT-COL_POS     = 11.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-CTABNAME    = 'VBRK'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  wa_fieldcat-fieldname   = 'NAME3'.   "ADD BY SANTHOSHKUMAR
  wa_fieldcat-seltext_m   = 'NAME3'.
  wa_fieldcat-col_pos     = 15.
  wa_fieldcat-cfieldname   = 'NAME3'.
  wa_fieldcat-ctabname    = 'NAME3'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-col_pos     = 16.
  wa_fieldcat-cfieldname   = 'KUNRG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'NAME2'.
  wa_fieldcat-seltext_m   = 'PAYER NAME'.
  wa_fieldcat-col_pos     = 17.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CITY'.               "  ADDED BY MANIKANDAN ON  15-10-2015
  wa_fieldcat-seltext_m   = 'City'.
  wa_fieldcat-col_pos     = 18.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'QTY_LTR'. "  Added By Govind On 08/01/2015
  wa_fieldcat-seltext_m   = 'TOTAL IN LITES/KGS'.
  wa_fieldcat-col_pos     = 19.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*
  wa_fieldcat-fieldname   = 'BTGEW'.
  wa_fieldcat-seltext_m   = 'TOTAL WEIGHT'.
  wa_fieldcat-col_pos     = 20.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'NETWR'.
  wa_fieldcat-seltext_m   = 'NET VALUE'.
  wa_fieldcat-col_pos     = 21.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'MWSBK'.
  wa_fieldcat-seltext_m   = 'TAX VALUE'.
  wa_fieldcat-col_pos     = 22.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'INV_VAL'.
  wa_fieldcat-seltext_m   = 'INVOICE VALUE'.
  wa_fieldcat-col_pos     = 23.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERDAT'.               "ADDED BY RAM ON 8/8/2015
  wa_fieldcat-seltext_m   = 'INV.CREATED DATE'.
  wa_fieldcat-col_pos     = 24.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ERZET'.               "ADDED BY RAM ON 8/8/2015
  wa_fieldcat-seltext_m   = 'TIME'.
  wa_fieldcat-col_pos     = 25.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*
*  WA_FIELDCAT-FIELDNAME   = 'SUM_OFF'.
*  WA_FIELDCAT-SELTEXT_M   = ' SALES OFFICE SUM'.
*  WA_FIELDCAT-COL_POS     = 19.
*  WA_FIELDCAT-DO_SUM      = 'X'.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*
*  WA_FIELDCAT-FIELDNAME   = 'V_COUNT'.
*  WA_FIELDCAT-SELTEXT_M   = 'COUNTER'.
*  WA_FIELDCAT-COL_POS     = 20.
*  WA_FIELDCAT-DO_SUM      = 'X'.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.



  wa_fieldcat-fieldname   = 'AUBEL'.               "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 26.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname = 'STP_NO'.
  wa_fieldcat-seltext_m = 'Ship To Party No'.
  wa_fieldcat-no_zero = 'X' .
  wa_fieldcat-col_pos = 27 .
  APPEND: wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname = 'STP_NAM'.
  wa_fieldcat-seltext_m = 'Ship To Party Name'.
  wa_fieldcat-no_zero = 'X' .
  wa_fieldcat-col_pos = 28 .
  APPEND: wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .



  wa_fieldcat-fieldname   = 'ERDAT1'.               "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Date '.
  wa_fieldcat-col_pos     = 29.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'ERZET1'.               "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Time'.
  wa_fieldcat-col_pos     = 30.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.




  wa_fieldcat-fieldname   = 'UNIQ1'.               "  ADDED BY MANIKANDAN ON 29-09-2015
  wa_fieldcat-seltext_m   = 'Trip Number'.
  wa_fieldcat-col_pos     = 31.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'TTDATE'.               "  ADDED BY MANIKANDAN ON 29-09-2015
  wa_fieldcat-seltext_m   = 'Trip Date'.
  wa_fieldcat-col_pos     = 32.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'TTIME'.               "  ADDED BY MANIKANDAN ON 29-09-2015
  wa_fieldcat-seltext_m   = 'Trip Time'.
  wa_fieldcat-col_pos     = 33.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CARTONS'.               "  ADDED BY MANIKANDAN ON  15-10-2015
  wa_fieldcat-seltext_m   = 'LR Number'.
  wa_fieldcat-col_pos     = 34.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'PERNR'.               "  ADDED BY RAM ON  5/7/2016
  wa_fieldcat-seltext_m   = 'S.O ID'.
  wa_fieldcat-col_pos     = 35.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.               "  ADDED BY RAM ON  5/7/2016
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-col_pos     = 36.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
  wa_fieldcat-fieldname   = 'SM_NO'.
  wa_fieldcat-seltext_m   = 'S.M ID'.
  wa_fieldcat-col_pos     = 37.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-col_pos     = 38.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*&-------End of Keerthi CLSS on 24-08-2016------*

  wa_fieldcat-fieldname   = 'TIME'.
  wa_fieldcat-seltext_m   = 'Delivery Time'.
  wa_fieldcat-col_pos     = 39.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'DATE_OF_DELIVERY'.
  wa_fieldcat-seltext_m   = 'Date Of Delivery'.
  wa_fieldcat-col_pos     = 40.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'REMARKS'.
  wa_fieldcat-seltext_m   = 'Remarks'.
  wa_fieldcat-col_pos     = 41.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'TO_CITY'.
  wa_fieldcat-seltext_m = 'To City'.
  wa_fieldcat-col_pos = 42 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname = 'TR_NAME'.
  wa_fieldcat-seltext_m = 'Transporter Name'.
  wa_fieldcat-col_pos = 43 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .

  wa_fieldcat-fieldname = 'V_NO'.
  wa_fieldcat-seltext_m = 'VEHICLE NO'.
  wa_fieldcat-col_pos = 44 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR: wa_fieldcat .


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



  LOOP AT it_finovr TRANSPORTING NO FIELDS WHERE candoc = 'X'.

    EXIT.

  ENDLOOP.

  IF sy-subrc NE 0.

    READ TABLE it_fieldcat INTO wa_fieldcat WITH KEY fieldname = 'CANDOC'.

    IF sy-subrc = 0.

      wa_fieldcat-no_out = 'X'.
      wa_fieldcat-tech   = 'X'.

      MODIFY it_fieldcat FROM wa_fieldcat INDEX sy-tabix.

    ENDIF.

  ENDIF.
  IF gwa_values-fieldvalue = '1' AND it_finovr IS NOT INITIAL.
    SELECT * FROM zmm_automigo_log INTO TABLE @DATA(lt_migo_inv)
      FOR ALL ENTRIES IN @it_finovr
      WHERE vbeln = @it_finovr-vbeln.
     SORT lt_migo_inv by vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_migo_inv COMPARING vbeln.
    LOOP AT lt_migo_inv INTO DATA(lw_inv).
      READ TABLE it_finovr INTO DATA(lw_finovr) WITH KEY vbeln = lw_inv-vbeln.
      IF sy-subrc = 0.
        DELETE it_finovr WHERE vbeln = lw_inv-vbeln.
      ENDIF.
    ENDLOOP.
  ENDIF.
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
      it_sort                  = it_sort
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

    WHEN '&MAI'.

      PERFORM build_xls_data_table.

      PERFORM send_mail.

    WHEN '&IC1'.

      IF rs_selfield-fieldname = 'VBELN'.

        SET PARAMETER ID 'VF' FIELD rs_selfield-value.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.
ENDFORM.                    "MY_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  BUILD_XLS_DATA_TABLE
*&---------------------------------------------------------------------*
*       FOR BUILDING EXCEL DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_xls_data_table .

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'BILLING DATE '
               'SALES OFFICE'
               'SALES OFFICE DESCRIPTION'
               'SALES ORGANIZATION'
               'DISTRIBUTION CHANNEL'
               'SALES DIVISION'
               'SALES DIVISION DESCRIPTION'
               'BILLING DOCUMENT NO'
               'BILLING TYPE'
               'PAYER'
               'PAYER NAME'
               'TOT LTR'
               'NET VALUE'
               'TAX VALUE'
               'INVOICE VALUE'
               'CUSTOMER SUM'
               'SL OFFICE SUM'
               'COUNTER'
  INTO  wa_attachment SEPARATED BY  con_tab.

  CONCATENATE con_cret
  wa_attachment
  INTO wa_attachment.

  APPEND wa_attachment TO it_attachment.
  CLEAR  wa_attachment.

  DATA: lv_str_volum  TYPE string,
        lv_str_btgew  TYPE string,
        lv_string_ovr TYPE string,
        lv_mwsbk      TYPE string,
        lv_inv_val    TYPE string,
        lv_sum_cust   TYPE string,
        lv_sum_off    TYPE string,
        lv_count      TYPE string.
  LOOP AT it_finovr INTO wa_finovr.

    MOVE: wa_finovr-qty_ltr TO lv_str_volum,
           wa_finovr-btgew TO lv_str_btgew,
          wa_finovr-netwr TO lv_string_ovr,
          wa_finovr-mwsbk TO lv_mwsbk,
          wa_finovr-inv_val TO lv_inv_val,
          wa_finovr-sum_cust TO lv_sum_cust,
          wa_finovr-sum_off TO lv_sum_off,
          wa_finovr-v_count TO lv_count.

    CONCATENATE  wa_finovr-fkdat wa_finovr-vkbur wa_finovr-bezei wa_finovr-vkorg wa_finovr-vtweg wa_finovr-spart wa_finovr-vtext  wa_finovr-vbeln wa_finovr-fkart wa_finovr-erdat wa_finovr-erzet "ADDED BY RAM
    wa_finovr-kunrg wa_finovr-name2  lv_str_btgew  lv_string_ovr lv_mwsbk lv_inv_val
    lv_sum_cust lv_sum_off lv_count
  INTO wa_attachment SEPARATED BY con_tab.

    CONCATENATE con_cret wa_attachment
    INTO wa_attachment.
    APPEND wa_attachment TO it_attachment.
    CLEAR wa_attachment.

  ENDLOOP.

ENDFORM.                    " BUILD_XLS_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       FOR SENDING MAIL WITH EXCEL ATTACHMENT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail .

  wa_docdata-obj_name = 'MAIL WITH EXCEL ATTACHMENT'.
  wa_docdata-obj_descr = 'SAP Reports'.

  PERFORM body_of_mail USING: space, 'Hello ',
  'Please find the attached excel sheet'.

  DESCRIBE TABLE it_body_msg LINES g_tab_lines.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 0.
  wa_packlist-body_start = 1.
  wa_packlist-body_num   = g_tab_lines.
  wa_packlist-doc_type   = 'RAW'.
  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  "Write Packing List for Attachment
  wa_packlist-transf_bin = space.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 1.
  wa_packlist-body_start = g_tab_lines + 1.
  DESCRIBE TABLE it_attachment LINES wa_packlist-body_num.
  wa_packlist-doc_type   = 'XLS'.
  wa_packlist-obj_descr  = 'SAP Invoice Details'.
  wa_packlist-obj_name   = 'XLS_ATTACHMENT'.
  wa_packlist-doc_size   = wa_packlist-body_num * 255.
  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  APPEND LINES OF it_attachment TO it_body_msg.
  "Fill the document data and get size of attachment
  wa_docdata-obj_langu  = sy-langu.
  READ TABLE it_body_msg INTO wa_body_msg INDEX g_tab_lines.
  wa_docdata-doc_size = ( g_tab_lines - 1 ) * 255 + strlen( wa_body_msg ).

  "Receivers List.
  wa_receivers-rec_type   = 'U'.  "Internet address
  wa_receivers-receiver   = 'no-reply@sheenlac.in'.
  wa_receivers-com_type   = 'INT'.
  wa_receivers-notif_del  = 'X'.
  wa_receivers-notif_ndel = 'X'.
  APPEND wa_receivers TO it_receivers .
  CLEAR:wa_receivers.

*   WA_RECEIVERS-REC_TYPE   = 'U'.  "Internet address
*  WA_RECEIVERS-RECEIVER   = 'govindarajanm@sheenlac.in'.
*  WA_RECEIVERS-COM_TYPE   = 'INT'.
*  WA_RECEIVERS-NOTIF_DEL  = 'X'.
*  WA_RECEIVERS-NOTIF_NDEL = 'X'.
*  APPEND WA_RECEIVERS TO IT_RECEIVERS .
*  CLEAR:WA_RECEIVERS.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'                                 " FUNCTION MODULE FOR MAIL SENDING
    EXPORTING
      document_data              = wa_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = g_sent_to_all
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = it_packlist
*     OBJECT_HEADER              =
*     CONTENTS_BIN               =
      contents_txt               = it_body_msg
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.

    WRITE: 'FAILURE'.

  ELSE.

    WAIT UP TO 2 SECONDS.

    SUBMIT rsconn01 WITH mode = 'INT'
        WITH output = 'X'
        AND RETURN.
  ENDIF.
ENDFORM.                    " SEND_MAIL

*&---------------------------------------------------------------------*
*&      Form  BODY_OF_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM body_of_mail USING l_message.

  wa_body_msg = l_message.
  APPEND wa_body_msg TO it_body_msg.
  CLEAR  wa_body_msg.

ENDFORM.                    " BODY_OF_MAIL
*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_retrieval_detail .

  TYPES: BEGIN OF ty_lips,
           vbeln TYPE lips-vbeln,
           vkbur TYPE lips-vkbur,                             "SALES OFFICE FROM LIPS TABLE
           matnr TYPE lips-matnr,
           spart TYPE lips-spart,
           posnr TYPE lips-posnr,
           pstyv TYPE lips-pstyv,
           lfimg TYPE lips-lfimg,
           charg TYPE lips-charg,
           vrkme TYPE lips-vrkme,
         END OF ty_lips.

*
  DATA: it_lips TYPE STANDARD TABLE OF ty_lips,
        wa_lips LIKE LINE OF it_lips.

  TYPES: BEGIN OF ty_vbrp2,
           vbeln TYPE vbrp-vbeln,
           vgbel TYPE vbrp-vgbel,
           posnr TYPE vbrp-posnr,
           fkimg TYPE vbrp-fkimg,
           volum TYPE vbrp-volum,
           voleh TYPE vbrp-voleh,
           netwr TYPE vbrp-netwr,
         END OF ty_vbrp2.

  DATA: it_vbrp2 TYPE STANDARD TABLE OF ty_vbrp2,
        wa_vbrp2 LIKE LINE OF it_vbrp2.

  IF so_name3 <> ''.
    SELECT kunnr  FROM kna1 INTO TABLE it_kna1_name3 WHERE name3 IN so_name3.  "ADD BY SANTHOSHKUMAR
  ENDIF.


  IF p_sr <> 'X' AND p_cb <> 'X' AND p_ic <> 'X' AND p_st <> 'X'.

    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.

        SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               vbrk~bukrs
          vbrp~werks
               vbrk~fkdat
              vbrp~erdat  "ADDED BY RAM ON 18/8/15
              vbrp~erzet
               vbrk~fkart
              vbrp~vbeln
              lips~posnr
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgpos
               vbrp~volum
              vbrp~voleh
              lips~pstyv
              vbrk~spart
              vbrk~netwr
              vbrk~kunag
              vbrk~kunrg
              vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~sfakn
              vbrk~fksto
              vbrp~posnr
              vbrp~arktx
              vbrp~vrkme
              vbrp~vgbel
              vbrp~vkbur
              vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM

              lips~matnr
              lips~vbeln
              lips~lfimg
              lips~charg
              INTO TABLE it_vbrk2
              FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                        JOIN lips ON vbrp~vgbel = lips~vbeln
                       AND   vbrp~matnr = lips~matnr
                       JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3              "ADD BY SANTHOSHKUMAR
              WHERE     vbrk~bukrs IN so_bukrs
                    AND vbrk~fkdat IN so_fkdat
                    AND vbrp~matnr IN so_matnr
                    AND vbrp~vkbur IN so_vkbu2
                    AND vbrp~vbeln IN so_vbeln
                    AND vbrk~kunag IN so_kunag
                    AND vbrp~spart IN so_spart
                    AND vbrk~kunag = it_kna1_name3-kunnr_name3                                            "ADD BY SANTHOSHKUMAR
          AND vbrk~fksto <> 'X' AND  fkart <> 'YBRE' AND fkart <> 'YIRE' AND fkart <> 'IVS'  "AND FKART <> 'YRF2' "ADDED BY RAM ON 1/8/15
          AND fkart <> 'YFRE' AND fkart <> 'YLRE'  AND fkart <> 'IG'
          AND  vbrk~fkart IN so_fkart
                    AND lips~charg IN so_charg .
      ENDIF.
    ELSE.
      SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
      vbrk~bukrs
        vbrp~werks
      vbrk~fkdat
     vbrp~erdat  "ADDED BY RAM ON 18/8/15
     vbrp~erzet
      vbrk~fkart
     vbrp~vbeln
     lips~posnr
     vbrp~matnr
     vbrp~fkimg
     vbrp~vgpos
      vbrp~volum
     vbrp~voleh
     lips~pstyv
     vbrk~spart
     vbrk~netwr
     vbrk~kunag
     vbrk~kunrg
     vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
     vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
     vbrk~sfakn
     vbrk~fksto
     vbrp~posnr
     vbrp~arktx
     vbrp~vrkme
     vbrp~vgbel
     vbrp~vkbur
     vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM

     lips~matnr
     lips~vbeln
     lips~lfimg
     lips~charg
     INTO TABLE it_vbrk2
     FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               JOIN lips ON vbrp~vgbel = lips~vbeln
              AND   vbrp~matnr = lips~matnr
     WHERE     vbrk~bukrs IN so_bukrs
           AND vbrk~fkdat IN so_fkdat
           AND vbrp~matnr IN so_matnr
           AND vbrp~vkbur IN so_vkbu2
           AND vbrp~vbeln IN so_vbeln
           AND vbrk~kunag IN so_kunag
           AND vbrp~spart IN so_spart
 AND vbrk~fksto <> 'X' AND  fkart <> 'YBRE' AND fkart <> 'YIRE' AND fkart <> 'IVS'  "AND FKART <> 'YRF2' "ADDED BY RAM ON 1/8/15
 AND fkart <> 'YFRE' AND fkart <> 'YLRE'  AND fkart <> 'IG'
 AND  vbrk~fkart IN so_fkart
           AND lips~charg IN so_charg .

    ENDIF.

  ELSEIF p_cb = 'X'.
    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.
        SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrp~werks
           vbrk~fkdat
                vbrp~erdat  "ADDED BY RAM ON 18/8/15
              vbrp~erzet
               vbrk~fkart
              vbrp~vbeln
              lips~posnr
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgpos
               vbrp~volum
              vbrp~voleh
              lips~pstyv
              vbrk~spart
              vbrk~netwr
              vbrk~kunag
              vbrk~kunrg
              vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~sfakn
               vbrk~fksto
              vbrp~posnr
              vbrp~arktx
              vbrp~vrkme
              vbrp~vgbel
              vbrp~vkbur
              vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
              lips~matnr
              lips~vbeln
              lips~lfimg
              lips~charg

              INTO TABLE it_vbrk2
              FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                        JOIN lips ON vbrp~vgbel = lips~vbeln
                   AND   vbrp~matnr = lips~matnr
                   JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3                    "ADD BY SANTHOSHKUMAR
              WHERE     vbrk~bukrs IN so_bukrs
                    AND vbrk~fkdat IN so_fkdat
                    AND vbrp~matnr IN so_matnr
                    AND vbrp~vkbur IN so_vkbu2
                    AND vbrp~vbeln IN so_vbeln
                    AND vbrk~kunag IN so_kunag
                    AND vbrp~spart IN so_spart
                    AND vbrk~kunag = it_kna1_name3-kunnr_name3                                              "ADD BY SANTHOSHKUMAR
          AND vbrk~fksto = 'X'
          AND  vbrk~fkart IN so_fkart
                    AND lips~charg IN so_charg .
      ENDIF.
    ELSE.
      SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        vbrp~werks
  vbrk~fkdat
       vbrp~erdat  "ADDED BY RAM ON 18/8/15
     vbrp~erzet
      vbrk~fkart
     vbrp~vbeln
     lips~posnr
     vbrp~matnr
     vbrp~fkimg
     vbrp~vgpos
      vbrp~volum
     vbrp~voleh
     lips~pstyv
     vbrk~spart
     vbrk~netwr
     vbrk~kunag
     vbrk~kunrg
     vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
     vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
     vbrk~sfakn
      vbrk~fksto
     vbrp~posnr
     vbrp~arktx
     vbrp~vrkme
     vbrp~vgbel
     vbrp~vkbur
     vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
     lips~matnr
     lips~vbeln
     lips~lfimg
     lips~charg

     INTO TABLE it_vbrk2
     FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
               JOIN lips ON vbrp~vgbel = lips~vbeln
          AND   vbrp~matnr = lips~matnr
     WHERE     vbrk~bukrs IN so_bukrs
           AND vbrk~fkdat IN so_fkdat
           AND vbrp~matnr IN so_matnr
           AND vbrp~vkbur IN so_vkbu2
           AND vbrp~vbeln IN so_vbeln
           AND vbrk~kunag IN so_kunag
           AND vbrp~spart IN so_spart
 AND vbrk~fksto = 'X'
 AND  vbrk~fkart IN so_fkart
           AND lips~charg IN so_charg .

    ENDIF.

  ELSEIF p_sr = 'X' .
    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.
        SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrp~werks
               vbrk~fkdat
                vbrp~erdat  "ADDED BY RAM ON 18/8/15
              vbrp~erzet
               vbrk~fkart
              vbrp~vbeln
              lips~posnr
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgpos
               vbrp~volum
              vbrp~voleh
              lips~pstyv
              vbrk~spart
              vbrk~netwr
              vbrk~kunag
              vbrk~kunrg
              vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~sfakn
              vbrk~fksto
              vbrp~posnr
              vbrp~arktx
              vbrp~vrkme
              vbrp~vgbel
              vbrp~vkbur
              vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
              lips~matnr
              lips~vbeln
              lips~lfimg
              lips~charg

              INTO TABLE it_vbrk2
              FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                        JOIN lips ON vbrp~vgbel = lips~vbeln
                        AND   vbrp~matnr = lips~matnr
                        JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3               "ADD BY SANTHOSHKUMAR
              WHERE    vbrk~bukrs IN so_bukrs
                    AND  vbrk~fkdat IN so_fkdat
                    AND vbrk~kunag = it_kna1_name3-kunnr_name3                                            "ADD BY SANTHOSHKUMAR
                    AND vbrp~matnr IN so_matnr
                    AND vbrp~vkbur IN so_vkbu2
                    AND vbrp~vbeln IN so_vbeln
                    AND vbrk~kunag IN so_kunag
                    AND vbrp~spart IN so_spart
          AND vbrk~fksto <> 'X' AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE' OR fkart <> 'IG' )
          AND  vbrk~fkart IN so_fkart
                    AND lips~charg IN so_charg .
      ENDIF.
    ELSE.
      SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        vbrp~werks
     vbrk~fkdat
      vbrp~erdat  "ADDED BY RAM ON 18/8/15
    vbrp~erzet
     vbrk~fkart
    vbrp~vbeln
    lips~posnr
    vbrp~matnr
    vbrp~fkimg
    vbrp~vgpos
     vbrp~volum
    vbrp~voleh
    lips~pstyv
    vbrk~spart
    vbrk~netwr
    vbrk~kunag
    vbrk~kunrg
    vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
    vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
    vbrk~sfakn
    vbrk~fksto
    vbrp~posnr
    vbrp~arktx
    vbrp~vrkme
    vbrp~vgbel
    vbrp~vkbur
    vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
    lips~matnr
    lips~vbeln
    lips~lfimg
    lips~charg

    INTO TABLE it_vbrk2
    FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              JOIN lips ON vbrp~vgbel = lips~vbeln
**              AND   VBRP~VGPOS = LIPS~POSNR
              AND   vbrp~matnr = lips~matnr
    WHERE    vbrk~bukrs IN so_bukrs
          AND  vbrk~fkdat IN so_fkdat
          AND vbrp~matnr IN so_matnr
          AND vbrp~vkbur IN so_vkbu2
          AND vbrp~vbeln IN so_vbeln
          AND vbrk~kunag IN so_kunag
          AND vbrp~spart IN so_spart
AND vbrk~fksto <> 'X' AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE' OR fkart <> 'IG' )
AND  vbrk~fkart IN so_fkart
          AND lips~charg IN so_charg .
    ENDIF.

  ELSEIF p_ic = 'X' .

    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.

        SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrp~werks
               vbrk~fkdat
                vbrp~erdat  "ADDED BY RAM ON 18/8/15
              vbrp~erzet
               vbrk~fkart
              vbrp~vbeln
              lips~posnr
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgpos
               vbrp~volum
              vbrp~voleh
              lips~pstyv
              vbrk~spart
              vbrk~netwr
              vbrk~kunag
              vbrk~kunrg
              vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
              vbrk~sfakn
              vbrk~fksto
              vbrp~posnr
              vbrp~arktx
              vbrp~vrkme
              vbrp~vgbel
              vbrp~vkbur
              vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
              lips~matnr
              lips~vbeln
              lips~lfimg
              lips~charg

              INTO TABLE it_vbrk2
              FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                        JOIN lips ON vbrp~vgbel = lips~vbeln
                        AND   vbrp~matnr = lips~matnr
                        JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3             "ADD BY SANTHOSHKUMAR
              WHERE    vbrk~bukrs IN so_bukrs
                    AND  vbrk~fkdat IN so_fkdat
                    AND vbrp~matnr IN so_matnr
                    AND vbrk~kunag = it_kna1_name3-kunnr_name3                                            "ADD BY SANTHOSHKUMAR
*                AND VBRP~VKBUR IN SO_VKBU2
                    AND vbrp~vbeln IN so_vbeln
                    AND vbrk~kunag IN so_kunag
                    AND vbrp~spart IN so_spart
           AND ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG' )
          AND  vbrk~fkart IN so_fkart.
*                AND LIPS~CHARG IN SO_CHARG .
      ENDIF.
    ELSE.

      SELECT vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        vbrp~werks
     vbrk~fkdat
      vbrp~erdat  "ADDED BY RAM ON 18/8/15
    vbrp~erzet
     vbrk~fkart
    vbrp~vbeln
    lips~posnr
    vbrp~matnr
    vbrp~fkimg
    vbrp~vgpos
     vbrp~volum
    vbrp~voleh
    lips~pstyv
    vbrk~spart
    vbrk~netwr
    vbrk~kunag
    vbrk~kunrg
    vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
    vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
    vbrk~sfakn
    vbrk~fksto
    vbrp~posnr
    vbrp~arktx
    vbrp~vrkme
    vbrp~vgbel
    vbrp~vkbur
    vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
    lips~matnr
    lips~vbeln
    lips~lfimg
    lips~charg

    INTO TABLE it_vbrk2
    FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              JOIN lips ON vbrp~vgbel = lips~vbeln
              AND   vbrp~matnr = lips~matnr
    WHERE    vbrk~bukrs IN so_bukrs
          AND  vbrk~fkdat IN so_fkdat
          AND vbrp~matnr IN so_matnr
*                AND VBRP~VKBUR IN SO_VKBU2
          AND vbrp~vbeln IN so_vbeln
          AND vbrk~kunag IN so_kunag
          AND vbrp~spart IN so_spart
 AND ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG' )
AND  vbrk~fkart IN so_fkart.
*                AND LIPS~CHARG IN SO_CHARG .

    ENDIF.

  ELSEIF p_st = 'X'.

    IF so_name3 <> ''.
      IF it_kna1_name3 IS NOT INITIAL.                                         " Added By PR@$@TH on 26/08/2019  Desc:Stock Transfer Report Copy(ZKP_SD_SALES_REGISTER program)

        SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                   vbrk~bukrs
                   vbrp~werks
                   vbrk~fkdat
                  vbrp~erdat  "ADDED BY RAM ON 18/8/15
                  vbrp~erzet
                   vbrk~fkart
                  vbrp~vbeln
                  lips~posnr
                  vbrp~matnr
                  vbrp~fkimg
                  vbrp~vgpos
                   vbrp~volum
                  vbrp~voleh
                  lips~pstyv
                  vbrk~spart
                  vbrk~netwr
                  vbrk~kunag
                  vbrk~kunrg
                  vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
                  vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
                  vbrk~sfakn
                  vbrk~fksto
                  vbrp~posnr
                  vbrp~arktx
                  vbrp~vrkme
                  vbrp~vgbel
                  vbrp~vkbur
                  vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
                  lips~matnr
                  lips~vbeln
                  lips~lfimg
                  lips~charg
                  INTO TABLE it_vbrk2
                  FROM vbrp INNER JOIN vbrk AS vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                            INNER JOIN lips AS lips ON vbrp~vgbel = lips~vbeln
                            AND vbrp~matnr = lips~matnr
                            JOIN kna1 ON kna1~kunnr = vbrk~kunag FOR ALL ENTRIES IN it_kna1_name3             "ADD BY SANTHOSHKUMAR
                      WHERE vbrk~bukrs IN so_bukrs
                        AND vbrk~fkdat IN so_fkdat
                        AND vbrp~matnr IN so_matnr
                        AND vbrp~werks IN so_vkbu2
                        AND vbrp~vbeln IN so_vbeln
                        AND vbrk~kunag IN so_kunag
                        AND vbrp~spart IN so_spart
                        AND vbrk~fkart IN so_fkart
                        AND lips~charg IN so_charg
                        AND vbrk~kunag = it_kna1_name3-kunnr_name3                                            "ADD BY SANTHOSHKUMAR
                        AND vbrk~vtweg = '30' AND ( vbrk~fkart EQ 'YSTO' OR vbrk~fkart EQ 'S1').
      ENDIF.
    ELSE.
      SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             vbrk~bukrs
             vbrp~werks
             vbrk~fkdat
            vbrp~erdat  "ADDED BY RAM ON 18/8/15
            vbrp~erzet
             vbrk~fkart
            vbrp~vbeln
            lips~posnr
            vbrp~matnr
            vbrp~fkimg
            vbrp~vgpos
             vbrp~volum
            vbrp~voleh
            lips~pstyv
            vbrk~spart
            vbrk~netwr
            vbrk~kunag
            vbrk~kunrg
            vbrk~vkorg   " Added by Keerthi CLSS on 26.10.2016
            vbrk~vtweg   " Added by Keerthi CLSS on 26.10.2016
            vbrk~sfakn
            vbrk~fksto
            vbrp~posnr
            vbrp~arktx
            vbrp~vrkme
            vbrp~vgbel
            vbrp~vkbur
            vbrp~netwr " Modified by Govind on 02/08/2014 Time 12:01 PM
            lips~matnr
            lips~vbeln
            lips~lfimg
            lips~charg
            INTO TABLE it_vbrk2
            FROM vbrp INNER JOIN vbrk AS vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                      INNER JOIN lips AS lips ON vbrp~vgbel = lips~vbeln
                     AND vbrp~matnr = lips~matnr
                WHERE vbrk~bukrs IN so_bukrs
                  AND vbrk~fkdat IN so_fkdat
                  AND vbrp~matnr IN so_matnr
                  AND vbrp~werks IN so_vkbu2
                  AND vbrp~vbeln IN so_vbeln
                  AND vbrk~kunag IN so_kunag
                  AND vbrp~spart IN so_spart
                  AND vbrk~fkart IN so_fkart
                  AND lips~charg IN so_charg
                  AND vbrk~vtweg = '30' AND ( vbrk~fkart EQ 'YSTO' OR vbrk~fkart EQ 'S1').

    ENDIF.

  ENDIF.

*               AND VBRP-CHARG IN LIPS~CHARG.
*SORT IT_VBRK2  BY CHARG  .
*DELETE ADJACENT DUPLICATES FROM IT_VBRK2 COMPARING VBELN  MATNR.

  DATA: it_temp_vbrk2 TYPE STANDARD TABLE OF ty_vdrk2,
        wa_temp_vbrk2 LIKE LINE OF it_temp_vbrk2.

  MOVE it_vbrk2 TO it_temp_vbrk2.

  SORT it_temp_vbrk2 BY spart vkbur kunag.

  DELETE ADJACENT DUPLICATES FROM it_temp_vbrk2 COMPARING spart vkbur kunag.

  IF it_vbrk2 IS NOT INITIAL.

    REFRESH it_t001w.                                                                    "ADDED BY JESTOP ON 30.10.2020
    CLEAR wa_t001w.

    SELECT werks
           name1 FROM t001w INTO TABLE it_t001w
                 FOR ALL ENTRIES IN it_vbrk2
                 WHERE werks = it_vbrk2-werks.

    SELECT vkbur
           bezei
           FROM tvkbt INTO TABLE it_tvkbt2
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE vkbur = it_temp_vbrk2-vkbur
           AND spras = 'EN'.

    SELECT spart
           vtext
           FROM tspat INTO TABLE it_tspat2
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE spart = it_temp_vbrk2-spart
           AND spras = 'EN'.

    SELECT kunnr
           name1
           name2
           name3                                                "ADD BY SANTHOSHKUMAR
           FROM kna1 INTO TABLE it_kna12
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE kunnr = it_temp_vbrk2-kunag.
    SELECT matnr
         volum
         voleh
         spart FROM mara INTO TABLE it_mara1
         FOR ALL ENTRIES IN it_vbrk2 WHERE matnr = it_vbrk2-matnr.

    SELECT  vbeln
            aubel  FROM vbrp
            INTO TABLE itt_vbrp                                             "  ADDED BY MANIKANDAN ON 28-08-2015
            FOR ALL ENTRIES IN it_vbrk2
            WHERE vbeln = it_vbrk2-vbeln.



    IF sy-subrc = 0.

      SORT itt_vbrp BY vbeln.                                                "  ADDED BY MANIKANDAN ON 28-08-2015

    ENDIF.

    SELECT   vbeln
             erdat
             erzet
            vsnmr_v FROM  vbak        "  ADDED BY MANIKANDAN ON 26-09-2015
             INTO TABLE it_vbak                                             "  ADDED BY MANIKANDAN ON 28-08-2015
             FOR ALL ENTRIES IN itt_vbrp
              WHERE vbeln = itt_vbrp-aubel.

    IF sy-subrc = 0.

      SORT it_vbak BY vbeln .

    ENDIF.
*    IF it_vbak IS NOT INITIAL.
*
*      SELECT
*         vbeln
*         parvw
*         pernr
*         FROM vbpa INTO TABLE it_vbpa                              "  ADDED BY RAM ON 05-07-2016
*             FOR ALL ENTRIES IN it_vbak
*             WHERE  vbeln = it_vbak-vbeln AND parvw = 'L5'.
*
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*      SELECT
*    vbeln
*    parvw
*    pernr
*    FROM vbpa INTO TABLE it_vbpa1
*        FOR ALL ENTRIES IN it_vbak
*        WHERE  vbeln = it_vbak-vbeln AND parvw = 'L3'.
**&-------End of Keerthi CLSS on 24-08-2016------*
*    ENDIF.
*    IF it_vbpa IS NOT INITIAL.
*      SELECT
*           pernr
*           endda
*           begda
*           sname
*             FROM pa0001 INTO TABLE it_pa0001                              "  ADDED BY RAM ON 05-07-2016
*             FOR ALL ENTRIES IN it_vbpa
*             WHERE pernr = it_vbpa-pernr.
*    ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*    IF it_vbpa1 IS NOT INITIAL.
*      SELECT
*         pernr
*         endda
*         begda
*         sname
*           FROM pa0001 INTO TABLE it_pa0001_1
*           FOR ALL ENTRIES IN it_vbpa1
*           WHERE pernr = it_vbpa1-pernr.
*    ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ADDED PR@$@TH ON 28/08/19

    SELECT  invoiceno
            transporternamelrno
            vehiclenodrivername
            uniq1
            tdate
            time
            cartons
            city  FROM ztrip_st INTO TABLE it_ztrip_st
            FOR ALL ENTRIES IN  it_vbrk2  WHERE  invoiceno = it_vbrk2-vbeln .

    SELECT trip_no
           invoice_no
           customer_name
           invoice_date
           invoice_amount
           weight
           company_code
           location
           lr_no
           status FROM zfreight_item INTO TABLE it_zfritm FOR ALL ENTRIES IN it_vbrk2 WHERE invoice_no = it_vbrk2-vbeln .

    IF it_zfritm IS NOT INITIAL.
      SELECT trip_no
             status
             vendor_code
             vendor_name
             from_code
             from_loc
             city_id
             to_loc
             truck_type
             truck_des
             filling_type
             filing_des
             freight_charge
             loding_charge
             unload_charge
             halt_charge
             lr_charge
             no_of_days
             vechile_number
             crdate
             crtime
             created_by
             trn_value
             apr_sta
             remarks FROM zfreight_header INTO TABLE it_zfrihed FOR ALL ENTRIES IN it_zfritm WHERE trip_no = it_zfritm-trip_no .

    ENDIF.

    SELECT vbeln
           kunnr
           btgew FROM likp INTO TABLE it_likp FOR ALL ENTRIES IN it_vbrk2
           WHERE vbeln = it_vbrk2-vgbel.

    SELECT kunnr
           name1
           name2
           name3 FROM kna1 INTO TABLE it_kna1_1 FOR ALL ENTRIES IN it_likp WHERE kunnr = it_likp-kunnr.         "ADD BY SANTHOSHKUMAR

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ENDED PR@$@TH ON 28/08/19
  ENDIF.
*
  DATA: temp_pos  TYPE lips-charg,
        temp_pos1 TYPE lips-charg.

  DATA : it_read_vbrk2 TYPE STANDARD TABLE OF ty_vdrk2,
         wa_read_vbrk2 LIKE LINE OF it_read_vbrk2,
         temp_tabix    TYPE sy-tabix,
         temp_vgpos    TYPE vbrp-vgpos,
         temp_vgbel    TYPE vbrp-vgbel.

  SORT it_vbrk2 BY pstyv.

  "DELETE ADJACENT DUPLICATES FROM IT_VBRK2 COMPARING VBELN LPOSNR.

  LOOP AT it_vbrk2 INTO wa_vbrk2 WHERE pstyv = 'TAN'.

    " IF WA_VBRK2-FKART <> 'IV' OR WA_VBRK2-FKART <> 'IVS' . "ADDED BY RAM ON 2/2/16

    IF  wa_vbrk2-vgpos NE wa_vbrk2-lposnr.
      wa_vbrk2-del_flag = 'X'.

      MODIFY it_vbrk2 FROM wa_vbrk2 INDEX sy-tabix. "WHERE VBELN = WA_VBRK2-VBELN AND VGPOS = WA_VBRK2-VGPOS.

*DELETE IT_VBRK2 INDEX SY-TABIX.

      CLEAR: wa_vbrk2.

      "  ENDIF.

    ENDIF .

  ENDLOOP.

  DELETE it_vbrk2 WHERE del_flag = 'X'.

**  SORT IT_VBRK2 BY FKDAT VBELN LPOSNR FKIMG.   ""Commened by NTT
  SORT it_vbrk2 BY fkdat vbeln lposnr vgpos fkimg.  ""Added by NTT

  "ADDED BY RAM ON 23/11/2018
  DELETE it_vbrk2 WHERE fkimg = '0.00' .

**  DELETE ADJACENT DUPLICATES FROM IT_VBRK2 COMPARING FKDAT VBELN LPOSNR FKIMG LVBELN . ""Commened by NTT
  DELETE ADJACENT DUPLICATES FROM it_vbrk2 COMPARING fkdat vbeln lposnr vgpos fkimg lvbeln . ""Added by NTT

  IF p_sr EQ 'X'.
**    SORT IT_VBRK2 BY FKDAT VBELN POSNR MATNR FKIMG LPOSNR.  ""Commened by NTT
    SORT it_vbrk2 BY fkdat vbeln posnr matnr fkimg lposnr vgpos. ""Added by NTT
    DELETE ADJACENT DUPLICATES FROM it_vbrk2 COMPARING fkdat vbeln posnr matnr fkimg . " FKART EQ 'YBRE' .
  ENDIF.

  "ADDED ENDED BY RAM ON 23/11/2018

*DELETE IT_VBRK2 WHERE DEL_FLAG = 'X'.

  SORT it_tvkbt2 BY vkbur.
  SORT it_tspat2 BY spart.
  SORT it_kna12 BY kunnr.
  SORT it_mara1 BY matnr.

*  DELETE IT_VBRK2 WHERE LFIMG < 0 .

  LOOP AT it_vbrk2 INTO wa_vbrk2  .

    IF  wa_vbrk2-vgpos = wa_vbrk2-lposnr AND
        wa_vbrk2-vgbel = wa_vbrk2-lvbeln AND
        wa_vbrk2-matnr = wa_vbrk2-lmatnr
    AND        wa_vbrk2-fkimg = wa_vbrk2-lfimg.      "Manual Batch

      wa_finaldet-fkart = wa_vbrk2-fkart.                                 " ADDED BY MANI 24.02.2016
      wa_finaldet-bukrs = wa_vbrk2-bukrs.                                 " ADDED BY MANI 24.02.2016

      wa_finaldet-werks = wa_vbrk2-werks.
      CLEAR wa_t001w.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_vbrk2-werks.

      wa_finaldet-name = wa_t001w-name1.

      wa_finaldet-fkdat = wa_vbrk2-fkdat.
      wa_finaldet-spart = wa_vbrk2-spart.
      wa_finaldet-netwr = wa_vbrk2-netwr.
      wa_finaldet-kunag = wa_vbrk2-kunag.
      wa_finaldet-kunrg = wa_vbrk2-kunrg.
      wa_finaldet-sfakn = wa_vbrk2-sfakn.
      wa_finaldet-vbeln = wa_vbrk2-vbeln.
      wa_finaldet-lvbeln = wa_vbrk2-lvbeln.
*     wa_finaldet-erdat = wa_vbrk2-erdat.
      wa_finaldet-vkbur = wa_vbrk2-vkbur.
      wa_finaldet-posnr = wa_vbrk2-posnr.
      wa_finaldet-charg = wa_vbrk2-charg.
      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.
      wa_finaldet-fkimg = wa_vbrk2-fkimg.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
      wa_finaldet-vnetwr = wa_vbrk2-vnetwr. " Modified by Govind on 02/08/2014 Time 12:01 PM
      wa_finaldet-order_num = wa_vbrk2-posnr.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .   "ADDED BY RAM
      wa_finaldet-erzet = wa_vbrk2-erzet .

      wa_finaldet-t_volum = wa_vbrk2-volum .  "Added By Ram on 4/11/2015
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.

      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart BINARY SEARCH.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.
        wa_finaldet-name3 = wa_kna12-name3.                           "ADD BY SANTHOSHKUMAR
      ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Added By PR@$@TH On 29/08/2019
      READ TABLE it_ztrip_st INTO wa_ztrip_st WITH KEY invoiceno = wa_vbrk2-vbeln.
      IF sy-subrc = 0.
        wa_finaldet-trname = wa_ztrip_st-transporternamelrno .
        wa_finaldet-tocity = wa_ztrip_st-city .
        wa_finaldet-v_no = wa_ztrip_st-vehiclenodrivername.
        wa_finaldet-uniq1 = wa_ztrip_st-uniq1 .
        wa_finaldet-invoiceno = wa_ztrip_st-invoiceno .
        wa_finaldet-ttdate = wa_ztrip_st-ttdate.
        wa_finaldet-ttime = wa_ztrip_st-ttime.
        wa_finaldet-cartons = wa_ztrip_st-cartons.
      ENDIF.

      IF wa_finaldet-trname IS INITIAL OR wa_finaldet-tocity IS INITIAL OR wa_finaldet-v_no IS INITIAL OR wa_finaldet-uniq1 IS INITIAL
         OR wa_finaldet-ttdate IS INITIAL OR wa_finaldet-ttime IS INITIAL OR wa_finaldet-cartons IS INITIAL .

        READ TABLE it_zfritm INTO wa_zfritm WITH KEY invoice_no = wa_vbrk2-vbeln .
        READ TABLE it_zfrihed INTO wa_zfrihed WITH KEY trip_no = wa_zfritm-trip_no .

        IF wa_finaldet-trname IS INITIAL.
          wa_finaldet-trname = wa_zfrihed-vendor_name .  "Transporter Name
        ENDIF.

        IF wa_finaldet-tocity IS INITIAL.
          wa_finaldet-tocity = wa_zfrihed-to_loc .       " to city
        ENDIF.

        IF wa_finaldet-v_no IS INITIAL.                  "VECHILE NUMBER
          wa_finaldet-v_no = wa_zfrihed-vechile_number .
        ENDIF.

        IF ( wa_finaldet-uniq1 IS INITIAL OR wa_finaldet-uniq1 EQ '0' ).                  " TRIP NO
          wa_finaldet-uniq1 = wa_zfritm-trip_no.
        ENDIF.

        IF wa_finaldet-cartons IS INITIAL.
          wa_finaldet-cartons = wa_zfritm-lr_no .             "LR NO
        ENDIF.

        IF ( wa_finaldet-ttdate IS INITIAL OR wa_finaldet-ttdate EQ '00000000').
          wa_finaldet-ttdate = wa_zfrihed-crdate .            "TRIP DATE
        ENDIF.

        IF ( wa_finaldet-ttime IS INITIAL OR wa_finaldet-ttime EQ '000000') .
          wa_finaldet-ttime = wa_zfrihed-crtime .           " TRIP TIME
        ENDIF.

      ENDIF.

      READ TABLE it_likp INTO wa_likp WITH KEY vbeln = wa_vbrk2-lvbeln .
      READ TABLE it_kna1_1 INTO wa_kna1_1 WITH KEY kunnr = wa_likp-kunnr .

      wa_finaldet-stp_no = wa_kna1_1-kunnr.
      wa_finaldet-stp_nam = wa_kna1_1-name1 .
      wa_finaldet-name3 = wa_kna1_1-name3.
      IF  wa_finaldet-stp_nam IS INITIAL.
        wa_finaldet-stp_nam = wa_kna1_1-name2 .
      ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Ended By PR@$@TH On 29/08/2019

      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

      IF sy-subrc = 0.                                                                    "  ADDED BY MANIKANDAN ON 28-08-2015
        wa_finaldet-aubel  = waa_vbrp-aubel.
      ENDIF.

      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.


      IF sy-subrc = 0.                                                                   "  ADDED BY MANIKANDAN ON 28-08-2015

        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.
        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.           "  ADDED BY MANIKANDAN ON 26-09-2015

      ENDIF.

      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG' .

          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

        ENDIF.
      ENDIF.
      wa_finaldet-bukrs = wa_vbrk2-bukrs.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vkorg = wa_vbrk2-vkorg.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vtweg = wa_vbrk2-vtweg.   " Added by Keerthi CLSS on 26.10.2016

*      READ TABLE it_vbpa INTO wa_vbpa
*          WITH KEY vbeln = wa_vbak-vbeln.                             "  ADDED BY RAM ON 5/7/2016
*
*      IF sy-subrc = 0.
*        wa_finaldet-pernr  = wa_vbpa-pernr .
*      ENDIF.
*
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*      READ TABLE it_vbpa1 INTO wa_vbpa1
*         WITH KEY vbeln = wa_vbak-vbeln.
*
*      IF sy-subrc = 0.
*        wa_finaldet-sm_no  = wa_vbpa1-pernr.
*      ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*
*      IF wa_finaldet-pernr IS NOT INITIAL .
*        READ TABLE it_pa0001 INTO wa_pa0001
*           WITH KEY pernr = wa_vbpa-pernr.                             "  ADDED BY RAM ON 5/7/2016
*
*        IF sy-subrc = 0.
*          wa_finaldet-sname   = wa_pa0001-sname.
*        ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*
*        READ TABLE it_pa0001_1 INTO wa_pa0001_1
*           WITH KEY pernr = wa_vbpa1-pernr.
*
*        IF sy-subrc = 0.
*          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
*        ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*      ENDIF.


      APPEND wa_finaldet TO it_finaldet.
      CLEAR : wa_finaldet , wa_zfrihed , wa_zfritm , wa_ztrip_st , wa_kna1_1 , wa_likp  .", wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
      CONTINUE.
    ENDIF.

    IF wa_vbrk2-vgpos = wa_vbrk2-lposnr AND
       wa_vbrk2-vgbel = wa_vbrk2-lvbeln AND
       wa_vbrk2-matnr = wa_vbrk2-lmatnr
      AND             wa_vbrk2-pstyv = 'TAN' OR wa_vbrk2-pstyv = 'NLC' OR wa_vbrk2-pstyv = 'YBEN' OR wa_vbrk2-pstyv = 'YBLN' .                         "    Automatic Batch

      wa_finaldet-fkart = wa_vbrk2-fkart.                                 " ADDED BY MANI 24.02.2016

      wa_finaldet-fkdat = wa_vbrk2-fkdat.
      wa_finaldet-spart = wa_vbrk2-spart.
      wa_finaldet-netwr = wa_vbrk2-netwr.
      wa_finaldet-kunag = wa_vbrk2-kunag.
      wa_finaldet-kunrg = wa_vbrk2-kunrg.
      wa_finaldet-sfakn = wa_vbrk2-sfakn.
      wa_finaldet-vbeln = wa_vbrk2-vbeln.
      wa_finaldet-sort_key = ''.
      wa_finaldet-lvbeln = wa_vbrk2-lvbeln.
*    wa_finaldet-erdat = wa_vbrk2-erdat.
      wa_finaldet-vkbur = wa_vbrk2-vkbur.
      wa_finaldet-posnr = wa_vbrk2-posnr.
      wa_finaldet-charg = wa_vbrk2-charg.

      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.
      wa_finaldet-fkimg = wa_vbrk2-fkimg.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
      wa_finaldet-vnetwr = wa_vbrk2-vnetwr. " Modified by Govind on 02/08/2014 Time 12:01 PM
      wa_finaldet-volum = wa_vbrk2-volum.
      wa_finaldet-voleh = wa_vbrk2-voleh.
      wa_finaldet-order_num = wa_vbrk2-posnr.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .   "ADDED BY RAM
      wa_finaldet-erzet = wa_vbrk2-erzet .

      wa_finaldet-t_volum = wa_vbrk2-volum .  "Added By Ram on 4/11/2015
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.

      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.
      IF sy-subrc = 0.
        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.
        wa_finaldet-name3 = wa_kna12-name3.                                    "ADD BY SANTHOSHKUMAR
      ENDIF.



      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.
      IF sy-subrc = 0.                                                                    "  ADDED BY MANIKANDAN ON 28-08-2015
        wa_finaldet-aubel  = waa_vbrp-aubel.
      ENDIF.


      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.
      IF sy-subrc = 0.                                                                   "  ADDED BY MANIKANDAN ON 28-08-2015
        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.
        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.      "  ADDED BY MANIKANDAN ON 26-09-2015
      ENDIF.

      wa_finaldet-bukrs = wa_vbrk2-bukrs.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vkorg = wa_vbrk2-vkorg.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vtweg = wa_vbrk2-vtweg.   " Added by Keerthi CLSS on 26.10.2016

*      READ TABLE it_vbpa INTO wa_vbpa
*      WITH KEY vbeln = wa_vbak-vbeln.                             "  ADDED BY RAM ON 5/7/2016
*
*      IF sy-subrc = 0.
*        wa_finaldet-pernr  = wa_vbpa-pernr .
*      ENDIF.
*
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*      READ TABLE it_vbpa1 INTO wa_vbpa1
*         WITH KEY vbeln = wa_vbak-vbeln.
*
*      IF sy-subrc = 0.
*        wa_finaldet-sm_no  = wa_vbpa1-pernr.
*      ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*
*      IF wa_finaldet-pernr IS NOT INITIAL .
*        READ TABLE it_pa0001 INTO wa_pa0001
*           WITH KEY pernr = wa_vbpa-pernr.                             "  ADDED BY RAM ON 5/7/2016
*
*        IF sy-subrc = 0.
*          wa_finaldet-sname   = wa_pa0001-sname.
*        ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*
*        READ TABLE it_pa0001_1 INTO wa_pa0001_1
*           WITH KEY pernr = wa_vbpa1-pernr.
*
*        IF sy-subrc = 0.
*          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
*        ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*      ENDIF.

      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG'.
          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .
        ENDIF.
      ENDIF.


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Added By PR@$@TH On 29/08/2019

      READ TABLE it_ztrip_st INTO wa_ztrip_st WITH KEY invoiceno = wa_vbrk2-vbeln.
      IF sy-subrc = 0.
        wa_finaldet-trname = wa_ztrip_st-transporternamelrno .
        wa_finaldet-tocity = wa_ztrip_st-city .
        wa_finaldet-v_no = wa_ztrip_st-vehiclenodrivername.
        wa_finaldet-uniq1 = wa_ztrip_st-uniq1 .
        wa_finaldet-invoiceno = wa_ztrip_st-invoiceno .
        wa_finaldet-ttdate = wa_ztrip_st-ttdate.
        wa_finaldet-ttime = wa_ztrip_st-ttime.
        wa_finaldet-cartons = wa_ztrip_st-cartons.
      ENDIF.

      IF wa_finaldet-trname IS INITIAL OR wa_finaldet-tocity IS INITIAL OR wa_finaldet-v_no IS INITIAL OR wa_finaldet-uniq1 IS INITIAL
         OR wa_finaldet-ttdate IS INITIAL OR wa_finaldet-ttime IS INITIAL OR wa_finaldet-cartons IS INITIAL .

        READ TABLE it_zfritm INTO wa_zfritm WITH KEY invoice_no = wa_vbrk2-vbeln .
        READ TABLE it_zfrihed INTO wa_zfrihed WITH KEY trip_no = wa_zfritm-trip_no .

        IF wa_finaldet-trname IS INITIAL.
          wa_finaldet-trname = wa_zfrihed-vendor_name .  "Transporter Name
        ENDIF.

        IF wa_finaldet-tocity IS INITIAL.
          wa_finaldet-tocity = wa_zfrihed-to_loc .       " to city
        ENDIF.

        IF wa_finaldet-v_no IS INITIAL.                  "VECHILE NUMBER
          wa_finaldet-v_no = wa_zfrihed-vechile_number .
        ENDIF.

        IF ( wa_finaldet-uniq1 IS INITIAL OR wa_finaldet-uniq1 EQ '0').                  " TRIP NO
          wa_finaldet-uniq1 = wa_zfritm-trip_no.
        ENDIF.

        IF wa_finaldet-cartons IS INITIAL.
          wa_finaldet-cartons = wa_zfritm-lr_no .             "LR NO
        ENDIF.

        IF ( wa_finaldet-ttdate IS INITIAL OR wa_finaldet-ttdate EQ '00000000' ).
          wa_finaldet-ttdate = wa_zfrihed-crdate .            "TRIP DATE
        ENDIF.

        IF ( wa_finaldet-ttime IS INITIAL OR wa_finaldet-ttime EQ '000000') .
          wa_finaldet-ttime = wa_zfrihed-crtime .           " TRIP TIME
        ENDIF.

      ENDIF.

      READ TABLE it_likp INTO wa_likp WITH KEY vbeln = wa_vbrk2-lvbeln .
      READ TABLE it_kna1_1 INTO wa_kna1_1 WITH KEY kunnr = wa_likp-kunnr .

      wa_finaldet-stp_no = wa_kna1_1-kunnr.
      wa_finaldet-stp_nam = wa_kna1_1-name1 .
      wa_finaldet-name3 = wa_kna1_1-name3.                                      "ADD BY SANTHOSHKUMAR
      IF  wa_finaldet-stp_nam IS INITIAL.
        wa_finaldet-stp_nam = wa_kna1_1-name2 .
      ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Ended By PR@$@TH On 29/08/2019

      APPEND wa_finaldet TO it_finaldet.
      CLEAR: wa_finaldet , wa_zfrihed , wa_zfritm , wa_ztrip_st ,wa_kna1_1 , wa_likp    .", wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
    ENDIF.

  ENDLOOP.

  DATA: temp_lfimg     TYPE lfimg,
        temp_fkimg     TYPE fkimg,
        temp_findet    TYPE STANDARD TABLE OF ty_finaldet,
        wa_temp_findet LIKE LINE OF temp_findet,
        count          TYPE i,
        lv_tabix       TYPE sy-tabix.


  LOOP AT it_finaldet INTO wa_finaldet WHERE proc_key NE 'X'.

    temp_fkimg = wa_finaldet-fkimg.

    LOOP AT it_vbrk2 INTO wa_vbrk2 WHERE vbeln = wa_finaldet-vbeln AND matnr = wa_finaldet-matnr AND pstyv = 'YB99'. "AND LFIMG NE WA_FINALDET-FKIMG.          ""AND DEL_FLAG = ''.

*    IF SY-SUBRC = 0.

*      TEMP_LFIMG = TEMP_LFIMG + WA_VBRK2-LFIMG. commened by Govind
      temp_lfimg =  wa_vbrk2-lfimg.

      IF temp_lfimg LE temp_fkimg.

        wa_finaldet-fkart = wa_vbrk2-fkart.                                 " ADDED BY MANI 24.02.2016

        wa_finaldet-fkdat = wa_vbrk2-fkdat.
        wa_finaldet-spart = wa_vbrk2-spart.
        wa_finaldet-netwr = wa_vbrk2-netwr.
        wa_finaldet-kunag = wa_vbrk2-kunag.
        wa_finaldet-kunrg = wa_vbrk2-kunrg.
        wa_finaldet-sfakn = wa_vbrk2-sfakn.
        wa_finaldet-vbeln = wa_vbrk2-vbeln.
        wa_finaldet-sort_key = 'A'.
        wa_finaldet-lvbeln = wa_vbrk2-lvbeln.
*    wa_finaldet-erdat = wa_vbrk2-erdat.
        wa_finaldet-vkbur = wa_vbrk2-vkbur.
        wa_finaldet-posnr = ''.                                       "WA_VBRK2-POSNR.
        wa_finaldet-charg = wa_vbrk2-charg.
        wa_finaldet-matnr = wa_vbrk2-matnr.
        wa_finaldet-arktx = wa_vbrk2-arktx.
*        WA_FINALDET-FKIMG = WA_VBRK2-FKIMG.
        wa_finaldet-vrkme = wa_vbrk2-vrkme.
*        WA_FINALDET-VNETWR = WA_VBRK2-VNETWR. " Modified by Govind on 02/08/2014 Time 12:01 PM
        wa_finaldet-volum = wa_vbrk2-volum.
        wa_finaldet-voleh = wa_vbrk2-voleh.
        wa_finaldet-order_num = wa_vbrk2-lposnr.
        wa_finaldet-lfimg = wa_vbrk2-lfimg.

        wa_finaldet-erdat = wa_vbrk2-erdat .   "ADDED BY RAM
        wa_finaldet-erzet = wa_vbrk2-erzet .

        wa_finaldet-t_volum = wa_vbrk2-volum .  "Added By Ram on 4/11/2015
        wa_finaldet-proc_key = 'X'.
        READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
        IF sy-subrc = 0.
          wa_finaldet-bezei = wa_tvkbt2-bezei.
        ENDIF.

        READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

        IF sy-subrc = 0.
          wa_finaldet-vtext = wa_tspat2-vtext.
        ENDIF.

        READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.

        IF sy-subrc = 0.
          wa_finaldet-name1 = wa_kna12-name1.
          wa_finaldet-name2 = wa_kna12-name1.
          wa_finaldet-name3 = wa_kna12-name3.                                             "ADD BY SANTHOSHKUMAR
        ENDIF.

*        READ TABLE IT_MARA1 INTO WA_MARA1 WITH KEY MATNR = WA_VBRK2-MATNR BINARY SEARCH.
*        IF SY-SUBRC = 0 .
*          WA_FINALDET-M_VOLUM = WA_MARA1-VOLUM.
*        ENDIF.
* WA_FINALDET-T_VOLUM = WA_MARA1-VOLUM * WA_FINALDET-LFIMG.

        READ TABLE itt_vbrp INTO waa_vbrp
           WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

        IF sy-subrc = 0.                                                                    "  ADDED BY MANIKANDAN ON 28-08-2015
          wa_finaldet-aubel  = waa_vbrp-aubel.


        ENDIF.

        READ TABLE it_vbak INTO wa_vbak
        WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.


        IF sy-subrc = 0.                                                                   "  ADDED BY MANIKANDAN ON 28-08-2015

          wa_finaldet-erdat1  = wa_vbak-erdat.
          wa_finaldet-erzet1  = wa_vbak-erzet.

          wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.      "  ADDED BY MANIKANDAN ON 26-09-2015
        ENDIF.

        IF p_cb NE 'X'.
          IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE'.     " added by mani 19.02.2016

            wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
            wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
            wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

          ENDIF.
        ENDIF.

        wa_finaldet-bukrs = wa_vbrk2-bukrs.   " Added by Keerthi CLSS on 26.10.2016
        wa_finaldet-vkorg = wa_vbrk2-vkorg.   " Added by Keerthi CLSS on 26.10.2016
        wa_finaldet-vtweg = wa_vbrk2-vtweg.   " Added by Keerthi CLSS on 26.10.2016
**&--------------Begin of Keerthi CLSS on 29.08.2016---------------------*
*        READ TABLE it_vbpa INTO wa_vbpa
*        WITH KEY vbeln = wa_vbak-vbeln.                             "  ADDED BY RAM ON 5/7/2016
*
*        IF sy-subrc = 0.
*          wa_finaldet-pernr  = wa_vbpa-pernr .
*        ENDIF.
*
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*        READ TABLE it_vbpa1 INTO wa_vbpa1
*           WITH KEY vbeln = wa_vbak-vbeln.
*
*        IF sy-subrc = 0.
*          wa_finaldet-sm_no  = wa_vbpa1-pernr.
*        ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*
*        IF wa_finaldet-pernr IS NOT INITIAL .
*          READ TABLE it_pa0001 INTO wa_pa0001
*             WITH KEY pernr = wa_vbpa-pernr.                             "  ADDED BY RAM ON 5/7/2016
*
*          IF sy-subrc = 0.
*            wa_finaldet-sname   = wa_pa0001-sname.
*          ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*
*          READ TABLE it_pa0001_1 INTO wa_pa0001_1
*             WITH KEY pernr = wa_vbpa1-pernr.
*
*          IF sy-subrc = 0.
*            wa_finaldet-sm_name   = wa_pa0001_1-sname  .
*          ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*        ENDIF.
**&---------------End of Keerthi CLSS on 29.08.2016----------------------*
        APPEND wa_finaldet TO it_finaldet.
        CLEAR: wa_finaldet.", wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
      ENDIF.
    ENDLOOP.

    READ TABLE it_vbrk2 INTO wa_vbrk2 WITH KEY
    vbeln = wa_finaldet-vbeln matnr = wa_finaldet-matnr pstyv = 'YB99' proc_key = '' . "LFIMG = WA_FINALDET-FKIMG .



    IF sy-subrc = 0 .
      lv_tabix = sy-tabix.                                                " Variable to Denote already processed records

      wa_finaldet-fkart = wa_vbrk2-fkart.                                 " ADDED BY MANI 24.02.2016

      wa_finaldet-fkdat = wa_vbrk2-fkdat.
      wa_finaldet-spart = wa_vbrk2-spart.
      wa_finaldet-netwr = wa_vbrk2-netwr.
      wa_finaldet-kunag = wa_vbrk2-kunag.
      wa_finaldet-kunrg = wa_vbrk2-kunrg.
      wa_finaldet-sfakn = wa_vbrk2-sfakn.
      wa_finaldet-vbeln = wa_vbrk2-vbeln.
*    WA_FINALDET-SORT_KEY = 'A'.
      wa_finaldet-lvbeln = wa_vbrk2-lvbeln.
*    wa_finaldet-erdat = wa_vbrk2-erdat.
      wa_finaldet-vkbur = wa_vbrk2-vkbur.
      wa_finaldet-posnr = ''.                               "WA_VBRK2-POSNR.
      wa_finaldet-charg = wa_vbrk2-charg.
      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.

*      WA_FINALDET-FKIMG = WA_VBRK2-FKIMG.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
*      WA_FINALDET-VNETWR = WA_VBRK2-VNETWR. " Modified by Govind on 02/08/2014 Time 12:01 PM
      wa_finaldet-volum = wa_vbrk2-volum.
      wa_finaldet-voleh = wa_vbrk2-voleh.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .   "ADDED BY RAM
      wa_finaldet-erzet = wa_vbrk2-erzet .
*    WA_FINALDET-ORDER_NUM = WA_VBRK2-POSNR + 1.

      wa_finaldet-t_volum = wa_vbrk2-volum .  "Added By Ram on 4/11/2015

      wa_finaldet-proc_key = 'X'.
*    WA_FINALDET-ORDER_NUM =
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.

      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.

      IF sy-subrc = 0.

        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.
        wa_finaldet-name3 = wa_kna12-name3.

      ENDIF.

*      READ TABLE IT_MARA1 INTO WA_MARA1 WITH KEY MATNR = WA_VBRP-MATNR BINARY SEARCH.
*      IF SY-SUBRC = 0 .
*        WA_FINALDET-M_VOLUM = WA_MARA1-VOLUM.
*
*      ENDIF.
*        WA_FINALDET-T_VOLUM = WA_MARA1-VOLUM * WA_FINALDET-LFIMG.
*   WA_TEMP_FINDET-ORDER_NUM = SY-TABIX.

      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

      IF sy-subrc = 0.                                                                    "  ADDED BY MANIKANDAN ON 28-08-2015
        wa_finaldet-aubel  = waa_vbrp-aubel.

      ENDIF.

      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.

      IF sy-subrc = 0.                                                                   "  ADDED BY MANIKANDAN ON 28-08-2015

        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.

        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.      "  ADDED BY MANIKANDAN ON 26-09-2015

      ENDIF.

      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE'.     " added by mani 19.02.2016

          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

        ENDIF.
      ENDIF.

      wa_finaldet-bukrs = wa_vbrk2-bukrs.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vkorg = wa_vbrk2-vkorg.   " Added by Keerthi CLSS on 26.10.2016
      wa_finaldet-vtweg = wa_vbrk2-vtweg.   " Added by Keerthi CLSS on 26.10.2016
**&--------------Begin of Keerthi CLSS on 29.08.2016---------------------*
*      READ TABLE it_vbpa INTO wa_vbpa
*      WITH KEY vbeln = wa_vbak-vbeln.                             "  ADDED BY RAM ON 5/7/2016
*
*      IF sy-subrc = 0.
*        wa_finaldet-pernr  = wa_vbpa-pernr .
*      ENDIF.
*
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*      READ TABLE it_vbpa1 INTO wa_vbpa1
*         WITH KEY vbeln = wa_vbak-vbeln.
*
*      IF sy-subrc = 0.
*        wa_finaldet-sm_no  = wa_vbpa1-pernr.
*      ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*
*      IF wa_finaldet-pernr IS NOT INITIAL .
*        READ TABLE it_pa0001 INTO wa_pa0001
*           WITH KEY pernr = wa_vbpa-pernr.                             "  ADDED BY RAM ON 5/7/2016
*
*        IF sy-subrc = 0.
*          wa_finaldet-sname   = wa_pa0001-sname.
*        ENDIF.
**&------Begin of Keerthi CLSS on 24-08-2016-----*
*
*        READ TABLE it_pa0001_1 INTO wa_pa0001_1
*           WITH KEY pernr = wa_vbpa1-pernr.
*
*        IF sy-subrc = 0.
*          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
*        ENDIF.
**&-------End of Keerthi CLSS on 24-08-2016------*
*      ENDIF.
**&---------------End of Keerthi CLSS on 29.08.2016----------------------*
      APPEND wa_finaldet TO it_finaldet.
*    CLEAR WA_TEMP_FINDET.

      wa_vbrk2-proc_key = 'X'.
      MODIFY it_vbrk2 FROM wa_vbrk2 INDEX lv_tabix TRANSPORTING proc_key.

    ENDIF.

    CLEAR: temp_lfimg, temp_fkimg, lv_tabix.
*    CLEAR: wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.

  ENDLOOP.


  DATA: cnt2 TYPE i VALUE '0'.


  DATA: it_temp2 TYPE STANDARD TABLE OF ty_finaldet,
        wa_temp2 LIKE LINE OF it_temp2.



  IF p_cb EQ 'X'.

    LOOP AT it_finaldet INTO wa_finaldet.

      IF wa_finaldet-sfakn IS NOT INITIAL.

        wa_finaldet-candoc = 'X'.

        MODIFY it_finaldet FROM wa_finaldet TRANSPORTING candoc.

        APPEND wa_finaldet TO it_temp2.

        CLEAR wa_finaldet.

      ENDIF.

    ENDLOOP.


    LOOP AT it_finaldet INTO wa_finaldet.

      READ TABLE it_temp2 INTO wa_temp2 WITH KEY sfakn = wa_finaldet-vbeln.

      IF sy-subrc = 0.

        MODIFY it_finaldet FROM wa_temp2 TRANSPORTING  candoc.

        CLEAR: wa_temp2.

      ENDIF.

    ENDLOOP.



*   SORT IT_FINALDET BY ORDER_NUM.
    SORT it_finaldet BY fkdat vkbur vbeln order_num charg.

  ENDIF.

  IF p_ic NE 'X'.

    IF p_cb NE 'X'.

      LOOP AT it_finaldet INTO wa_finaldet.

        cnt2 = cnt2 + 1.
        IF wa_finaldet-sfakn IS NOT INITIAL.

          wa_temp2-sfakn = wa_finaldet-sfakn.

          APPEND wa_temp2 TO it_temp2.

          DELETE it_finaldet INDEX cnt2.

          cnt2 = cnt2 - 1.

        ENDIF.
      ENDLOOP.

      LOOP AT it_temp2 INTO wa_temp2.

        DELETE it_finaldet WHERE vbeln = wa_temp2-sfakn.

      ENDLOOP.

*    SORT IT_FINALDET BY SORT_CRIT.
*    SORT IT_FINALDET BY SORT_KEY.

      SORT it_finaldet BY fkdat vkbur vbeln order_num charg.

      " DELETE IT_FINALDET WHERE VKBUR = ''.

    ENDIF.
  ENDIF.

  LOOP AT it_finaldet INTO wa_finaldet.
    IF wa_finaldet-posnr = ' '.
      wa_finaldet-fkimg = '0' .
*  APPEND WA_FINALDET-FKIMG TO IT_FINALDET-FKIMG.
    ENDIF.
  ENDLOOP.

*BREAK splabap.
*&--------Begin of Keerthi CLSS on 26.10.2016---------------*&
  CLEAR: it_pa0001, it_pa0001_1, it_knvp, it_knvp1.

  IF it_finaldet IS NOT INITIAL.
    SELECT kunnr
           vkorg
           vtweg
           spart
           parvw
           parza
           pernr
           FROM knvp INTO TABLE it_knvp FOR ALL ENTRIES IN it_finaldet WHERE kunnr    = it_finaldet-kunag
                                                                       AND vkorg    = it_finaldet-vkorg
                                                                       AND vtweg    = it_finaldet-vtweg
                                                                       AND spart    = it_finaldet-spart
                                                                       AND parvw = 'L5'.   "L5 = SO      L3 = AS

    SELECT kunnr
       vkorg
       vtweg
       spart
       parvw
       parza
       pernr
       FROM knvp INTO TABLE it_knvp1 FOR ALL ENTRIES IN it_finaldet WHERE kunnr    = it_finaldet-kunag
                                                                   AND vkorg    = it_finaldet-vkorg
                                                                   AND vtweg    = it_finaldet-vtweg
                                                                   AND spart    = it_finaldet-spart
                                                                   AND parvw = 'L3'.   "L5 = SO      L3 = AS
    SELECT pernr
           endda
           begda
           sname
           FROM pa0001 INTO TABLE it_pa0001 FOR ALL ENTRIES IN it_knvp WHERE pernr = it_knvp-pernr.

    SELECT pernr
        endda
        begda
        sname
        FROM pa0001 INTO TABLE it_pa0001_1 FOR ALL ENTRIES IN it_knvp1 WHERE pernr = it_knvp1-pernr.

  ENDIF.

  LOOP AT it_finaldet INTO wa_finaldet.

    READ TABLE it_knvp INTO wa_knvp WITH KEY kunnr = wa_finaldet-kunag  vkorg = wa_finaldet-vkorg vtweg = wa_finaldet-vtweg spart = wa_finaldet-spart parvw = 'L5'.
    IF sy-subrc = 0.
      wa_finaldet-pernr = wa_knvp-pernr.
    ENDIF.
    READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_knvp-pernr.
    IF sy-subrc = 0.
      wa_finaldet-sname = wa_pa0001-sname.
    ENDIF.
*&--------------------------------------------------------------------------------------------------------------------------------
    READ TABLE it_knvp1 INTO wa_knvp1 WITH KEY kunnr = wa_finaldet-kunag  vkorg = wa_finaldet-vkorg vtweg = wa_finaldet-vtweg spart = wa_finaldet-spart parvw = 'L3'.
    IF sy-subrc = 0.
      wa_finaldet-sm_no = wa_knvp1-pernr.
    ENDIF.
    READ TABLE it_pa0001_1 INTO wa_pa0001_1 WITH KEY pernr =  wa_knvp1-pernr.
    IF sy-subrc = 0.
      wa_finaldet-sm_name = wa_pa0001_1-sname.
    ENDIF.

    MODIFY it_finaldet FROM wa_finaldet TRANSPORTING pernr sname sm_no sm_name.

    CLEAR: wa_finaldet, wa_knvp, wa_pa0001, wa_knvp1, wa_pa0001_1.
  ENDLOOP.
*&--------End of Keerthi CLSS on 26.10.2016---------------*&

  IF it_finaldet IS INITIAL.

    MESSAGE 'No Data Exists for the Input' TYPE 'S' DISPLAY LIKE 'E'.

    STOP.
  ENDIF.



  LOOP AT it_finaldet INTO wa_finaldet.

    READ TABLE it_mara1 INTO wa_mara1 WITH  KEY matnr = wa_finaldet-matnr BINARY SEARCH.

    wa_finaldet-m_volum = wa_mara1-volum.
    wa_finaldet-voleh = wa_mara1-voleh.
    wa_finaldet-m_spart = wa_mara1-spart.

    READ TABLE it_vbrk2 INTO wa_vbrk2 WITH KEY vbeln = wa_finaldet-matnr .
    wa_finaldet-bukrs = wa_vbrk2-bukrs.


    MODIFY it_finaldet FROM wa_finaldet TRANSPORTING m_volum voleh  bukrs m_spart.
    CLEAR wa_finaldet.
  ENDLOOP.

  DATA : lv_temp TYPE int2.
  "LV_TEMP = 1 .


  "LOOP AT IT_FINALDET INTO WA_TEMPDET WHERE VBELN = '0101000001' AND MATNR = 'NCTHT022A1' .
  "IF WA_TEMPDET-ORDER_NUM = '5' OR WA_TEMPDET-ORDER_NUM = '7' .
  LOOP AT it_finaldet INTO wa_tempdet WHERE vbeln = '0127006137' AND ( matnr = 'PUAFT539E4' OR matnr = 'PUAFT537E4' ).
    IF wa_tempdet-order_num = '320' OR wa_tempdet-order_num = '330' OR wa_tempdet-order_num = '350'OR wa_tempdet-order_num = '360' .
      wa_tempdet-temp = lv_temp + 1    .
      lv_temp = wa_tempdet-temp.
    ENDIF.
    MODIFY it_finaldet FROM wa_tempdet TRANSPORTING temp .
  ENDLOOP.

  DELETE it_finaldet WHERE ( temp = '2' OR temp = '4' OR temp = '6' OR temp = '8' ) .

*  LOOP AT IT_FINALDET INTO WA_FINALDET.
*    WA_FINALDET-T_VOLUM = WA_FINALDET-M_VOLUM * WA_FINALDET-FKIMG.
*    MODIFY IT_FINALDET FROM WA_FINALDET TRANSPORTING T_VOLUM.
*    CLEAR WA_FINALDET.
*  ENDLOOP.


ENDFORM.                    "DATA_RETRIEVAL_DETAIL
*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authcheck_detail .

*  LOOP AT IT_FINALDET INTO WA_FINALDET.
*    AUTHORITY-CHECK OBJECT 'ZINVOICE'
*    ID 'ZVKBUR' FIELD WA_FINALDET-VKBUR
*    ID 'ZSPART' FIELD WA_FINALDET-SPART
*   " ID 'ZSPART' DUMMY
*    ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC NE 0.
*      WA_FINALDET-FLAG2 = 'x' .
*      MODIFY IT_FINALDET FROM WA_FINALDET TRANSPORTING FLAG2.
*      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*    ENDIF.
*ENDLOOP.
*    AUTHORITY-CHECK OBJECT 'ZINVOICE'
*   ID 'ZVKBUR' DUMMY
*   ID 'ZSPART' FIELD WA_FINALDET-SPART
*   ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC NE 0.
*      WA_FINALDET-FLAG2 = 'x' .
*      MODIFY IT_FINALDET FROM WA_FINALDET TRANSPORTING FLAG2.
*      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*    ENDIF.
*    CLEAR: WA_FINALDET.
*  ENDLOOP.

  LOOP AT it_finaldet INTO wa_finaldet.
    AUTHORITY-CHECK OBJECT 'ZINVOICE'
    ID 'ZVKBUR' FIELD wa_finaldet-vkbur
    ID 'ZSPART' FIELD wa_finaldet-spart
   " ID 'ZSPART' DUMMY
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      wa_finaldet-flag2 = 'x' .
      MODIFY it_finaldet FROM wa_finaldet TRANSPORTING flag2.
      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.
  ENDLOOP.
  DELETE it_finaldet WHERE flag2 = 'x'.


ENDFORM.                    " AUTHCHECK_DETAIL
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

  IF p_ic = 'X'.
    wa_fieldcat-fieldname   = 'BUKRS'.
    wa_fieldcat-tabname   = 'IT_FINOVR'.
    wa_fieldcat-seltext_m   = 'COMPANY CODE'.
    wa_fieldcat-col_pos     = 1.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  IF p_st = 'X'.                                                          " ADDED BY JESTOP ON 30.10.2020

    wa_fieldcat-fieldname   = 'WERKS'.
    wa_fieldcat-seltext_m   = 'PLANT'.
    wa_fieldcat-col_pos     = 2.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

    wa_fieldcat-fieldname   = 'NAME'.
    wa_fieldcat-seltext_m   = 'PLANT NAME'.
    wa_fieldcat-col_pos     = 3.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

  ENDIF.


  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DOCUMENT DATE'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_sort-fieldname = 'FKDAT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  IF p_st NE 'X'.

    wa_fieldcat-fieldname   = 'VKBUR'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 5.
    wa_fieldcat-cfieldname   = 'VKBUR'.
    wa_fieldcat-ctabname    = 'VBRP'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_sort-fieldname = 'VKBUR'.
* WA_SORT-UP = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

    wa_fieldcat-fieldname   = 'BEZEI'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE DESCRIPTION'.
    wa_fieldcat-col_pos     = 6.
    wa_fieldcat-cfieldname   = 'BEZEI'.
    wa_fieldcat-ctabname    = 'TVKBT'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
*
    wa_sort-fieldname = 'BEZEI'.
*  WA_SORT-UP = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

  ENDIF.


  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION'.
  wa_fieldcat-cfieldname   = 'SPART'.
  wa_fieldcat-ctabname    = 'VBRK'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_sort-fieldname = 'SPART'.
*  WA_SORT-UP = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.


  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION DESCRIPTION'.
  wa_fieldcat-col_pos     = 8.
  wa_fieldcat-cfieldname   = 'VTEXT'.
  wa_fieldcat-ctabname    = 'TSPAT'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

*  WA_SORT-FIELDNAME = 'VTEXT'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'VSNMR_V'.                                   "  ADDED BY MANIKANDAN ON 26-09-2015
  wa_fieldcat-seltext_m   = 'SALES REQ PLAN'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 9.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



*  WA_FIELDCAT-FIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-SELTEXT_M   = 'SOLD TO PARTY'.
*  WA_FIELDCAT-COL_POS     = 8.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-CTABNAME    = 'VBRK'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*  WA_SORT-FIELDNAME = 'KUNAG'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'NAME3'.                                "ADD BY SANTHOSHKUMAR
  wa_fieldcat-seltext_m   = 'NAME3'.
  wa_fieldcat-col_pos     = 10.
  wa_fieldcat-cfieldname   = 'NAME3'.
  wa_fieldcat-ctabname    = 'NAME3'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*

*  WA_SORT-FIELDNAME = 'NAME1'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-col_pos     = 11.
  wa_fieldcat-cfieldname   = 'KUNRG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  WA_SORT-FIELDNAME = 'KUNRG'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.



  wa_fieldcat-fieldname   = 'NAME2'.
  wa_fieldcat-seltext_m   = 'PAYER NAME'.
  wa_fieldcat-col_pos     = 12.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  WA_SORT-FIELDNAME = 'NAME2'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'VBELN'.
  wa_fieldcat-seltext_m = 'BILLING DOC NO'.
  wa_fieldcat-col_pos     = 13.
  wa_fieldcat-hotspot     = 'X'.
  wa_fieldcat-cfieldname   = 'VBELN'.
  wa_fieldcat-ctabname    = 'VBRP'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'VBELN'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

*  WA_FIELDCAT-FIELDNAME   = 'CANDOC'.
*  WA_FIELDCAT-SELTEXT_M = 'CANCELLED DOCUMENT'.
*  WA_FIELDCAT-COL_POS     = 7.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

*
*  WA_FIELDCAT-FIELDNAME   = 'POSNR'.
*  WA_FIELDCAT-SELTEXT_M   = 'BILLING ITEM'.
*  WA_FIELDCAT-COL_POS     = 8.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_SORT-FIELDNAME = 'POSNR'.
*  WA_SORT-DOWN = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ADDED BY PR@$@TH  ON 30/08/19

  wa_fieldcat-fieldname   = 'STP_NO'.
  wa_fieldcat-seltext_m = 'SHIP TO PARTY NO'.
  wa_fieldcat-col_pos     = 14.
  wa_fieldcat-ctabname    = wa_finaldet. "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'STP_NAM'.
  wa_fieldcat-seltext_m = 'SHIP TO PARTY NAME'.
  wa_fieldcat-col_pos     = 15.
  wa_fieldcat-ctabname    = wa_finaldet. "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ENDED BY PR@$@TH ON 30/08/19




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

  IF p_st IS NOT INITIAL.

    wa_fieldcat-fieldname   = 'M_SPART'.
    wa_fieldcat-seltext_m   = 'MATERIAL DIVISION'.
    wa_fieldcat-col_pos     = 18.
*  WA_FIELDCAT-CFIELDNAME   = ''.
    wa_fieldcat-ctabname    = 'MARA'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

  ENDIF.




*
*  WA_SORT-FIELDNAME = 'MATNR'.
* WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


*
*  WA_FIELDCAT-FIELDNAME   = 'CHARG'.
*  WA_FIELDCAT-SELTEXT_M   = 'BATCH NUMBER'.
*  WA_FIELDCAT-COL_POS     = 11.
*  WA_FIELDCAT-CFIELDNAME   = 'CHARG'.
*  WA_FIELDCAT-CTABNAME    = 'LIPS'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*
  wa_fieldcat-fieldname   = 'FKIMG'.
  wa_fieldcat-seltext_m   = 'QUANTITY'.
  wa_fieldcat-col_pos     = 19.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME   = 'LFIMG'.
*  WA_FIELDCAT-SELTEXT_M   = 'QUANTITY'.
*  WA_FIELDCAT-CTABNAME    = 'LIPS'.
*  WA_FIELDCAT-COL_POS     = 13.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_SORT-FIELDNAME = 'FKIMG'.
*  WA_SORT-DOWN = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.



  wa_fieldcat-fieldname   = 'VRKME'.
  wa_fieldcat-seltext_m   = 'SALES UNIT'.
  wa_fieldcat-ctabname     = 'VBRP'.
  wa_fieldcat-cfieldname  = 'VRKME'.
  wa_fieldcat-col_pos     = 20.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  WA_FIELDCAT-FIELDNAME   = 'VOLUM'.
*  WA_FIELDCAT-SELTEXT_M   = 'QTY IN LTRS/KGS'.
*  WA_FIELDCAT-CTABNAME     = 'VBRP'.
**  WA_FIELDCAT-CFIELDNAME  = 'LFIMG'.
*  WA_FIELDCAT-COL_POS     = 23.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.



  wa_fieldcat-fieldname   = 'M_VOLUM'.
  wa_fieldcat-seltext_m   = 'LTRS/KGS'.
  wa_fieldcat-ctabname     = 'MARA'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 21.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOLEH'.
  wa_fieldcat-seltext_m   = 'LTRS/KGS UNIT'.
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



  wa_fieldcat-fieldname   = 'ERDAT'.
  wa_fieldcat-seltext_m   = 'INV.CREATED DATE'.
  wa_fieldcat-ctabname     = 'VBRP'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 25.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ERZET'.
  wa_fieldcat-seltext_m   = 'TIME'.
  wa_fieldcat-ctabname     = 'VBRP'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 26.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*
*  WA_SORT-FIELDNAME = 'NETWR'. " Modified by Govind on 02/08/2014 Time 12:01 PM
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.



  wa_fieldcat-fieldname   = 'AUBEL'.                                "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 27.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERDAT1'.                                  "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Date'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 28.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERZET1'.                                   "  ADDED BY MANIKANDAN ON 28-08-2015
  wa_fieldcat-seltext_m   = 'Time'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 29.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'PERNR'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.O NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 30.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 31.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
  wa_fieldcat-fieldname   = 'SM_NO'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.M NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 32.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.                                   "  ADDED BY RAM ON 5/7/2016
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 33.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*&-------End of Keerthi CLSS on 24-08-2016------*

*~~~~~~~~~~~~~~~~Added By PR@$@TH ON 29/08/2019

  wa_fieldcat-fieldname = 'UNIQ1'.
  wa_fieldcat-seltext_m = 'TRIP NUMBER'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 34 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

  wa_fieldcat-fieldname = 'TTDATE'.
  wa_fieldcat-seltext_m = 'TRIP DATE'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 35 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .


  wa_fieldcat-fieldname = 'TTIME'.
  wa_fieldcat-seltext_m = 'TRIP TIME'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 36 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

  wa_fieldcat-fieldname = 'CARTONS'.
  wa_fieldcat-seltext_m = 'LR NUMBER'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 37 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

  wa_fieldcat-fieldname = 'TRNAME'.
  wa_fieldcat-seltext_m = 'Transporter Name'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 38 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

  wa_fieldcat-fieldname = 'TOCITY'.
  wa_fieldcat-seltext_m = 'To City'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 39 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

  wa_fieldcat-fieldname = 'V_NO'.
  wa_fieldcat-seltext_m = 'VEHICLE NO'.
  wa_fieldcat-ctabname = wa_finaldet . "#EC CI_FLDEXT_OK[2215424]  "Added by SPLABAP during code remediation
  wa_fieldcat-col_pos = 40 .
  APPEND wa_fieldcat TO it_fieldcat .
  CLEAR wa_fieldcat .

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




  LOOP AT it_finaldet TRANSPORTING NO FIELDS WHERE candoc = 'X'.

    EXIT.

  ENDLOOP.

  IF sy-subrc NE 0.

    READ TABLE it_fieldcat INTO wa_fieldcat WITH KEY fieldname = 'CANDOC'.

    IF sy-subrc = 0.

      wa_fieldcat-no_out = 'X'.
      wa_fieldcat-tech   = 'X'.

      MODIFY it_fieldcat FROM wa_fieldcat INDEX sy-tabix.

    ENDIF.

  ENDIF.
  "SORT it_finaldet BY charg vbeln.

  SORT it_finaldet BY vbeln.
  DELETE it_finaldet WHERE posnr = ' '.


  IF p_ic <> 'X'.

    IF p_fs <> 'X'.
      DELETE it_finaldet WHERE vnetwr = 0.
    ENDIF.
  ENDIF.

  IF gwa_values-fieldvalue = '1' AND it_finaldet IS NOT INITIAL.
    SELECT * FROM zmm_automigo_log INTO TABLE @DATA(lt_migo_inv)
      FOR ALL ENTRIES IN @it_finaldet
      WHERE vbeln = @it_finaldet-vbeln.
    SORT lt_migo_inv by vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_migo_inv COMPARING vbeln.
    LOOP AT lt_migo_inv INTO DATA(lw_inv).
      READ TABLE it_finaldet INTO DATA(lw_findet) WITH KEY vbeln = lw_inv-vbeln.
      IF sy-subrc = 0.
        DELETE it_finaldet WHERE vbeln = lw_inv-vbeln.
      ENDIF.
    ENDLOOP.
  ENDIF.
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
      it_sort                  = it_sort
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

  CASE r_ucomm.

    WHEN '&MAI'.

      PERFORM build_xls_data_table_detail.

      PERFORM send_mail_detail.

    WHEN '&GRH'.
      PERFORM build_graph_data_table_detail.

  ENDCASE.

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
FORM build_xls_data_table_detail .

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'BILLING DOCUMENT DATE'
               'SALES OFFICE'
               'SALES OFFICE DESCRIPTION'
               'SALES DIVISION'
               'SALES DIVISION DESCRPTION'
               'BILLING DOCUMENT NO'
               'BILLING ITEM'
               'MATERIAL NUMBER'
               'MATERIAL DESCRIPTION'
               'BATCH NO'
               'QUANTITY'
               'SALES UNIT'
               'NET VALUE'
*               'SOLD TO PARTY'
*               'SP NAME'
               'PAYER'
               'PAYER NAME'
  INTO  wa_attachment SEPARATED BY  con_tab.

  CONCATENATE con_cret
  wa_attachment
  INTO wa_attachment.

  APPEND wa_attachment TO it_attachment.
  CLEAR  wa_attachment.

  DATA: lv_string_det       TYPE string,
        lv_string_det_netwr TYPE string.


  LOOP AT it_finaldet INTO wa_finaldet.


    lv_string_det = wa_finaldet-fkimg.
    lv_string_det_netwr = wa_finaldet-netwr.

    CONCATENATE  wa_finaldet-fkdat
                 wa_finaldet-vkbur
                 wa_finaldet-bezei
                 wa_finaldet-spart
                 wa_finaldet-vtext
                 wa_finaldet-vbeln
                 wa_finaldet-posnr
                 wa_finaldet-matnr
                 wa_finaldet-arktx
                 wa_finaldet-charg
                 lv_string_det
                 wa_finaldet-vrkme
                 lv_string_det_netwr
                 wa_finaldet-kunag
                 wa_finaldet-name1
                 wa_finaldet-kunrg
                 wa_finaldet-name2
  INTO wa_attachment SEPARATED BY con_tab. "#EC CI_FLDEXT_OK[2215424] "Added by SPLABAP during code remediation
    " FINAL TABLE TYPE FOR DETAIL REPORT

    CONCATENATE con_cret wa_attachment
    INTO wa_attachment.
    APPEND wa_attachment TO it_attachment.
    CLEAR wa_attachment.
  ENDLOOP.

ENDFORM.                    "BUILD_XLS_DATA_TABLE_DETAIL
" BUILD_XLS_DATA_TABLE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail_detail .

  wa_docdata-obj_name = 'MAIL WITH EXCEL ATTACHMENT'.
  wa_docdata-obj_descr = 'SAP Reports'.

  PERFORM body_of_mail USING: space, 'Hello ',
  'Please find the attached excel sheet'.

  DESCRIBE TABLE it_body_msg LINES g_tab_lines.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 0.
  wa_packlist-body_start = 1.
  wa_packlist-body_num   = g_tab_lines.
  wa_packlist-doc_type   = 'RAW'.
  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  "Write Packing List for Attachment
  wa_packlist-transf_bin = space.
  wa_packlist-head_start = 1.
  wa_packlist-head_num   = 1.
  wa_packlist-body_start = g_tab_lines + 1.
  DESCRIBE TABLE it_attachment LINES wa_packlist-body_num.
  wa_packlist-doc_type   = 'XLS'.
  wa_packlist-obj_descr  = 'Excell Attachment'.
  wa_packlist-obj_name   = 'XLS_ATTACHMENT'.
  wa_packlist-doc_size   = wa_packlist-body_num * 255.
  APPEND wa_packlist TO it_packlist.
  CLEAR  wa_packlist.

  APPEND LINES OF it_attachment TO it_body_msg.
  "Fill the document data and get size of attachment
  wa_docdata-obj_langu  = sy-langu.
  READ TABLE it_body_msg INTO wa_body_msg INDEX g_tab_lines.
  wa_docdata-doc_size = ( g_tab_lines - 1 ) * 255 + strlen( wa_body_msg ).

  "Receivers List.
  wa_receivers-rec_type   = 'U'.  "Internet address
  wa_receivers-receiver   = 'no-reply@sheenlac.in'.
  wa_receivers-com_type   = 'INT'.
  wa_receivers-notif_del  = 'X'.
  wa_receivers-notif_ndel = 'X'.
  APPEND wa_receivers TO it_receivers .
  CLEAR:wa_receivers.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'                                 " FUNCTION MODULE FOR MAIL SENDING
    EXPORTING
      document_data              = wa_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = g_sent_to_all
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = it_packlist
*     OBJECT_HEADER              =
*     CONTENTS_BIN               =
      contents_txt               = it_body_msg
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.

    WRITE: 'FAILURE'.

  ELSE.

    WAIT UP TO 2 SECONDS.

    SUBMIT rsconn01 WITH mode = 'INT'
        WITH output = 'X'
        AND RETURN.
  ENDIF.
ENDFORM.                " SEND_MAIL_DETAILENDFORM.                    " DATA_RETRIEVAL_DETAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_GRAPH_DATA_TABLE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_graph_data_table_detail .

ENDFORM.                    " BUILD_GRAPH_DATA_TABLE_DETAIL
