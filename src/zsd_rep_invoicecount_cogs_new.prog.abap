*&---------------------------------------------------------------------*
*& Report  ZSD_REP_INVOICECOUNT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsd_rep_invoicecount_cogs_new.
**** Data Declarations ***
INCLUDe  zsd_rep_invoicecount_cogs_top.
*** Selection screen Design ***
INCLUDE zsd_rep_invoicecount_cogs_sel.
**** Subroutines ****
INCLUDE zsd_rep_invoicecount_cogs_forms.

 PERFORM f_screen_adjustments.
*--------------------------------------------------------------*
* TYPES DECLARATION
*--------------------------------------------------------------*


TYPES: BEGIN OF ty_vbrk_vbrp,
         knumv     TYPE vbrk-knumv,
         fkdat     TYPE vbrk-fkdat,
         bukrs     TYPE vbrk-bukrs,

         erdat     TYPE vbrp-erdat,
         erzet     TYPE vbrp-erzet,

         spart     TYPE vbrk-spart,
         vrkme     TYPE vbrp-vrkme,
         vbeln     TYPE vbrk-vbeln,

         fkart     TYPE vbrk-fkart,
         kunag     TYPE vbrk-kunag,
         kunrg     TYPE vbrk-kunrg,
         vkorg     TYPE vbrk-vkorg,
         vtweg     TYPE vbrk-vtweg,
         kurrf     TYPE vbrk-kurrf,
         netwr     TYPE vbrk-netwr,
         sfakn     TYPE vbrk-sfakn,
         knkli     TYPE vbrk-knkli,
         mwsbk     TYPE vbrk-mwsbk,
         fksto     TYPE vbrk-fksto,
         posnr     TYPE vbrp-posnr,
         vkbur     TYPE vbrp-vkbur,
         matnr     TYPE vbrp-matnr,
         fkimg     TYPE vbrp-fkimg,
         volum     TYPE vbrp-volum,
         vgbel     TYPE vbrp-vgbel,
*       UMREN TYPE MARM-UMREN,
*       TOT_BAS TYPE VBRP-FKIMG,
*       BAS_UNIT(1) TYPE C,
*       TOTAL TYPE FKIMG,
*       TOT_QUA TYPE FKIMG,


         aubel     TYPE vbrp-aubel,



         invoiceno TYPE ztrip_st-invoiceno,
         uniq1     TYPE ztrip_st-uniq1,
         ttdate    TYPE ztrip_st-tdate,
         ttime     TYPE ztrip_st-time,
         cartons   TYPE ztrip_st-cartons,
         city      TYPE ztrip_st-city,


         vsnmr_v   TYPE vbak-vsnmr_v,


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
       END OF ty_kna1,

       BEGIN OF ty_marm,
         matnr TYPE marm-matnr,
         meinh TYPE marm-meinh,
         umrez TYPE marm-umrez,
         umren TYPE marm-umren,
       END OF ty_marm,


       BEGIN OF ty_finovr,
         knumv     TYPE vbrk-knumv,
         fkdat     TYPE vbrk-fkdat,
         bukrs     TYPE vbrk-bukrs,

         erdat     TYPE vbrp-erdat,
         erzet     TYPE vbrp-erzet,

         vkbur     TYPE vbrp-vkbur,
         kunrg     TYPE vbrk-kunag,
         spart     TYPE vbrk-spart,
         vbeln     TYPE vbrk-vbeln,
         candoc(1) TYPE c,
         fkart     TYPE vbrk-fkart,
         kunag     TYPE vbrk-kunag,
*       KUNRG TYPE VBRK-KUNRG,
         vkorg     TYPE vbrk-vkorg,
         vtweg     TYPE vbrk-vtweg,
         kurrf     TYPE vbrk-kurrf,
         netwr     TYPE vbrk-netwr,
         sfakn     TYPE vbrk-sfakn,
         knkli     TYPE vbrk-knkli,
         mwsbk     TYPE vbrk-mwsbk,
         fksto     TYPE vbrk-fksto,
         btgew     TYPE likp-btgew,
         inv_val   TYPE netwr,
         posnr     TYPE vbrp-posnr,

         bezei     TYPE tvkbt-bezei,
         vtext     TYPE tspat-vtext,
         name1     TYPE kna1-name1,
         name2     TYPE kna1-name2,
         m_volum   TYPE mara-volum,
         qty_ltr   TYPE vbrp-volum,
         qty_ltr1  TYPE vbrp-volum,

         int_code  TYPE vbrp-werks,

         sum_cust  TYPE kbetr,
         sum_off   TYPE kbetr,
         v_count   TYPE i,
         flag(1)   TYPE c,


         aubel     TYPE vbrp-aubel,
         erdat1    TYPE vbak-erdat,
         erzet1    TYPE vbak-erzet,


         invoiceno TYPE ztrip_st-invoiceno,
         uniq1     TYPE ztrip_st-uniq1,
         ttdate    TYPE ztrip_st-tdate,
         ttime     TYPE ztrip_st-time,
         cartons   TYPE ztrip_st-cartons,
         city      TYPE ztrip_st-city,

         pernr     TYPE vbpa-pernr,
         sname     TYPE pa0001-sname,

         vsnmr_v   TYPE vbak-vsnmr_v,
         sm_no     TYPE vbpa-pernr,
         sm_name   TYPE pa0001-sname,

         kposn     TYPE konv-kposn,   "13/10
         kschl     TYPE konv-kschl,
         kbetr     TYPE konv-kbetr,
         kwert     TYPE konv-kwert,
         inbill    TYPE konv-kwert,
         tot_y004  TYPE konv-kwert,
         tot_y008  TYPE konv-kwert,

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
       END OF ty_kna12,

       BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         btgew TYPE likp-btgew,
       END OF ty_likp,

       BEGIN OF ty_vbrp,

         erdat TYPE vbrp-erdat,
         erzet TYPE vbrp-erzet,

         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         volum TYPE vbrp-volum,
         matnr TYPE vbrp-matnr,
         werks TYPE vbrp-werks,
       END OF ty_vbrp,

       BEGIN OF ty_vbrp3,

         erdat TYPE vbrp-erdat,
         erzet TYPE vbrp-erzet,

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
       END OF ty_mara,

       BEGIN OF ty_mara1,
         matnr TYPE mara-matnr,
         volum TYPE mara-volum,
         voleh TYPE mara-voleh,
       END OF ty_mara1,

       BEGIN OF ty_mara5,
         matnr TYPE mara-matnr,
         volum TYPE mara-volum,
         voleh TYPE mara-voleh,
       END OF ty_mara5,

       BEGIN OF ty_vdrk2,
         knumv       TYPE vbrk-knumv,
         bukrs       TYPE vbrk-bukrs,
         fkdat       TYPE vbrk-fkdat,

         erdat       TYPE vbrp-erdat,
         erzet       TYPE vbrp-erzet,


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
         sfakn       TYPE vbrk-sfakn,
         fksto       TYPE vbrk-fksto,
         posnr       TYPE vbrp-posnr,
         arktx       TYPE vbrp-arktx,
         vrkme       TYPE vbrp-vrkme,
         vgbel       TYPE vbrp-vgbel,
         vkbur       TYPE vbrp-vkbur,
         kursk       TYPE vbrp-kursk,
         vnetwr      TYPE vbrp-netwr,
         werks       TYPE vbrp-werks,
         lmatnr      TYPE lips-matnr,
         lvbeln      TYPE ekbe-xblnr,  "LIPS-VBELN,
         lfimg       TYPE lips-lfimg,
         charg       TYPE lips-charg,

         aubel1      TYPE vbrp-aubel,
         aupos       TYPE vbrp-aupos,

         del_flag(1) TYPE c,
         proc_key(1) TYPE c,
         aubel       TYPE vbrp-aubel,



         invoiceno   TYPE ztrip_st-invoiceno,
         uniq1       TYPE ztrip_st-uniq1,
         ttdate      TYPE ztrip_st-tdate,
         ttime       TYPE ztrip_st-time,
         cartons     TYPE ztrip_st-cartons,
         city        TYPE ztrip_st-city,

         vsnmr_v     TYPE vbak-vsnmr_v,
       END OF ty_vdrk2,


       BEGIN OF ty_finaldet,
         knumv       TYPE vbrk-knumv,
         fkdat       TYPE vbrk-fkdat,
         fkart       TYPE vbrk-fkart,
         bukrs       TYPE vbrk-bukrs,

         erdat       TYPE vbrp-erdat,
         erzet       TYPE vbrp-erzet,

         vkbur       TYPE vbrp-vkbur,


         bezei       TYPE tvkbt-bezei,
         spart       TYPE vbrk-spart,
         vtext       TYPE tspat-vtext,
         vbeln       TYPE vbrp-vbeln,
         lvbeln      TYPE lips-vbeln,
         order_num   TYPE i,
         charg       TYPE lips-charg,
         sort_key(1) TYPE c,
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
         vrkme       TYPE vbrp-vrkme,
         kursk       TYPE vbrp-kursk,
         vnetwr      TYPE vbrp-netwr,
         werks1      TYPE vbrp-werks,
         netwr(10)   TYPE c,
         kunag       TYPE vbrk-kunag,
         name1       TYPE kna1-name1,
         kunrg       TYPE vbrk-kunrg,
         name2       TYPE kna1-name1,
         sfakn       TYPE vbrk-sfakn,
         lfimg       TYPE lips-lfimg,
         flag2(2)    TYPE c,
         proc_key(1) TYPE c,

         aubel       TYPE vbrp-aubel,
         aubel1      TYPE vbrp-aubel,
         aupos       TYPE vbrp-aupos,

         erdat1      TYPE vbak-erdat,
         erzet1      TYPE vbak-erzet,




         invoiceno   TYPE ztrip_st-invoiceno,
         uniq1       TYPE ztrip_st-uniq1,
         ttdate      TYPE ztrip_st-tdate,
         ttime       TYPE ztrip_st-time,
         cartons     TYPE ztrip_st-cartons,
         city        TYPE ztrip_st-city,

         pernr       TYPE vbpa-pernr,
         sname       TYPE pa0001-sname,

         vsnmr_v     TYPE vbak-vsnmr_v,
         sm_no       TYPE vbpa-pernr,
         sm_name     TYPE pa0001-sname,


         kposn       TYPE konv-kposn,          "13/10
         kschl       TYPE konv-kschl,
         kbetr       TYPE konv-kbetr,
         kwert       TYPE konv-kwert,
         inbill1     TYPE konv-kwert,
         tot_y004    TYPE konv-kwert,
         tot_y008    TYPE konv-kwert,



       END OF ty_finaldet.



TYPES:BEGIN OF str_vbak,
        vbeln   TYPE vbak-vbeln,
        erdat   TYPE vbak-erdat,
        erzet   TYPE vbak-erzet,
        vsnmr_v TYPE vbak-vsnmr_v,
      END OF str_vbak.

TYPES:BEGIN OF str_vbpa,
        vbeln TYPE vbpa-vbeln,
        parvw TYPE vbpa-parvw,
        pernr TYPE vbpa-pernr,
      END OF str_vbpa.

TYPES:BEGIN OF str_pa0001,
        pernr TYPE pa0001-pernr,
        endda TYPE pa0001-endda,
        begda TYPE pa0001-begda,
        sname TYPE pa0001-sname,
      END OF str_pa0001.

TYPES:BEGIN OF str_vbrp,
        vbeln TYPE vbrp-vbeln,
        aubel TYPE vbrp-aubel,
      END OF str_vbrp.

TYPES : BEGIN OF str_konv,
          knumv TYPE konv-knumv,
          kposn TYPE konv-kposn,
          kschl TYPE konv-kschl,
          kbetr TYPE konv-kbetr,
          kwert TYPE konv-kwert,
        END OF str_konv .

TYPES : BEGIN OF str_ekbe,
          ebeln TYPE ekbe-ebeln,
          ebelp TYPE ekbe-ebeln,
          bwart TYPE ekbe-bwart,
          dmbtr TYPE ekbe-dmbtr,
          xblnr TYPE ekbe-xblnr,
        END OF str_ekbe.


DATA: it_vbpa1    TYPE TABLE OF str_vbpa,
      wa_vbpa1    TYPE str_vbpa,
      it_pa0001_1 TYPE TABLE OF str_pa0001,
      wa_pa0001_1 TYPE str_pa0001.

DATA:waa_vbrp TYPE str_vbrp,
     itt_vbrp TYPE  TABLE OF str_vbrp.

DATA:wa_vbak TYPE str_vbak,
     it_vbak TYPE TABLE OF str_vbak.


DATA:wa_vbpa TYPE str_vbpa,
     it_vbpa TYPE  TABLE OF str_vbpa.

DATA:wa_pa0001 TYPE str_pa0001,
     it_pa0001 TYPE TABLE OF str_pa0001.

DATA :it_konv TYPE TABLE OF str_konv,
      wa_konv TYPE str_konv.

DATA :it1_konv TYPE TABLE OF str_konv,
      wa1_konv TYPE str_konv.

DATA : it_ekbe TYPE TABLE OF str_ekbe,
       wa_ekbe TYPE str_ekbe.

TYPES:BEGIN OF str_ztrip_st,
        invoiceno TYPE ztrip_st-invoiceno,
        uniq1     TYPE ztrip_st-uniq1,
        ttdate    TYPE ztrip_st-tdate,
        ttime     TYPE ztrip_st-time,
        cartons   TYPE ztrip_st-cartons,
        city      TYPE ztrip_st-city,
      END OF str_ztrip_st.



DATA:wa_ztrip_st TYPE str_ztrip_st,
     it_ztrip_st TYPE TABLE OF str_ztrip_st.


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



DATA : layout TYPE slis_layout_alv.

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
      wa_tvkbt2     TYPE  ty_tvkbt2,
      wa_tspat2     TYPE ty_tspat2,
      wa_kna12      TYPE ty_kna12,
      wa_finaldet   LIKE LINE OF it_finaldet,
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



*SELECTION-SCREEN: BEGIN OF BLOCK b1.
*
*PARAMETERS: ps_parm AS LISTBOX VISIBLE LENGTH 20 MODIF ID tb1 no-display.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES
*
*SELECTION-SCREEN: END OF BLOCK b1.




*INITIALIZATION.
*
*  gwa_list-key = '1'.
*  gwa_list-text = 'INVOICE'.
*  APPEND gwa_list TO gt_list.
*  CLEAR: gwa_list.
*
*  gwa_list-key = '2'.
*  gwa_list-text = 'ACCOUNTBALANCES'.
*  APPEND gwa_list TO gt_list.
*  CLEAR: gwa_list.
*
*  gwa_list-key = '3'.
*  gwa_list-text = 'STOCK'.
*  APPEND gwa_list TO gt_list.
*  CLEAR: gwa_list.


CALL FUNCTION 'VRM_SET_VALUES'
  EXPORTING
    id              = 'PS_PARM'
    values          = gt_list
  EXCEPTIONS
    id_illegal_name = 1
    OTHERS          = 2.


*  IF ps_parm IS INITIAL.                                                 " TO SET SET THE INITIAL VALUE SHOWN IN LIST BOX AS 'INVOICE'
*
*    ps_parm = '1'.
*
*  ENDIF.



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


    IF p_cb  = 'X' AND screen-group1 = 'IB2'  .

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

    "COMMENTED BY SRI ON 10/2
*    IF p_ic = 'X' AND screen-group1 = 'LB2'  .
*
*      screen-input = 0.
*
*      MODIFY SCREEN .
*
*    ENDIF.


  ENDLOOP.

**********Added by samsudeen on (23.06.2022)
  LOOP AT SCREEN.
    IF screen-group1 = 'BP1' .
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*********************************************






*--------------------------------------------------------------*
*At Selection Screen Ouput ON List Box PS_PARM
*--------------------------------------------------------------*


*AT SELECTION-SCREEN ON ps_parm.
*  CLEAR: gwa_values, gt_values.
*  REFRESH gt_values.
*  gwa_values-fieldname = 'PS_PARM'.
*  APPEND gwa_values TO gt_values.
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname             = sy-cprog
*      dynumb             = sy-dynnr
*      translate_to_upper = 'X'
*    TABLES
*      dynpfields         = gt_values.

*  READ TABLE gt_values INDEX 1 INTO gwa_values.
*  IF sy-subrc = 0 AND gwa_values-fieldvalue IS NOT INITIAL.
*    READ TABLE gt_list INTO gwa_list
*                      WITH KEY key = gwa_values-fieldvalue.
*    IF sy-subrc = 0.
*      gv_selected_value = gwa_list-text.
*    ENDIF.
*  ENDIF.

START-OF-SELECTION.

  IF r1 = 'X'.

    PERFORM data_retrieval_overview.
    PERFORM authcheck_overview.
    PERFORM build_fieldcatalog_overview.
    PERFORM build_alv_overview.

  ELSEIF r2 = 'X'.
    IF p_bp NE 'X'.
      PERFORM data_retrieval_detail.
      PERFORM authcheck_detail.
      PERFORM build_fieldcatalog_detail.
      PERFORM build_alv_detail.
    ELSE.
      PERFORM data_retrieval_detail.    " Added by  Samsudeen 24/06/2022
    ENDIF.

  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_retrieval_overview .

  IF p_sr <> 'X' AND p_cb <> 'X' AND  p_ic <> 'X' " Added by Goivnd On 05/06/2014
                                 AND p_bp = 'X'.  " Added by Samsudeen 24/06/2022

    SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             vbrk~knumv
              vbrk~fkdat
                vbrp~erdat
                vbrp~erzet

                vbrk~spart
                vbrk~vbeln
                vbrk~fkart
                vbrk~kunag
                vbrk~kunrg
                vbrk~vkorg
                vbrk~vtweg
                vbrk~kurrf    "added on 1/2
                vbrk~netwr
                vbrk~sfakn
                vbrk~knkli
                vbrk~mwsbk
                vbrk~fksto
                vbrp~posnr
                vbrp~vrkme
                vbrp~vkbur
                vbrp~matnr
                vbrp~fkimg
                vbrp~vgbel
                vbrp~volum
                  vbrp~aubel

                INTO CORRESPONDING FIELDS OF TABLE it_vbrk
                FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
        AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND  fkart <> 'YBRE'  AND fkart <> 'YIRE' AND fkart <> 'YRF2' AND
        fkart <> 'YFRE' AND fkart <> 'YLRE'  AND fkart <> 'IVS' AND fkart <> 'IG' AND
        vbrk~fkart IN so_fkart AND fksto <> 'X' .
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ELSEIF p_cb = 'X'.

    SELECT    vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             vbrk~knumv
             vbrk~fkdat

             vbrk~spart
             vbrk~vbeln
             vbrk~fkart
             vbrk~kunag
             vbrk~kunrg
             vbrk~vkorg
             vbrk~vtweg
             vbrk~kurrf    "added on 1/2
             vbrk~netwr
             vbrk~sfakn
             vbrk~knkli
             vbrk~mwsbk
             vbrk~fksto
             vbrp~posnr
             vbrp~vrkme
             vbrp~vkbur
             vbrp~matnr
             vbrp~fkimg

             vbrp~erdat
             vbrp~erzet

             vbrp~vgbel
             vbrp~volum
             vbrp~aubel

             INTO CORRESPONDING FIELDS OF TABLE it_vbrk
             FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
     AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND vbrk~fkart IN so_fkart AND fksto = 'X'.

    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .


  ELSEIF p_sr = 'X' .


    SELECT  vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        vbrk~knumv
        vbrk~fkdat

      vbrk~erdat
      vbrk~erzet

        vbrk~spart
        vbrk~vbeln
        vbrk~fkart
        vbrk~kunag
        vbrk~kunrg
        vbrk~vkorg
        vbrk~vtweg
        vbrk~kurrf    "added on 1/2
        vbrk~netwr
        vbrk~sfakn
        vbrk~knkli
        vbrk~mwsbk
        vbrk~fksto
        vbrp~posnr
        vbrp~vrkme
        vbrp~vkbur
        vbrp~matnr
        vbrp~fkimg
        vbrp~vgbel
        vbrp~volum

        vbrp~aubel

        INTO CORRESPONDING FIELDS OF TABLE it_vbrk
        FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE'  )
AND fkart <> 'YRF2'
 AND vbrk~fkart IN so_fkart AND fksto <> 'X' .
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ENDIF.

  IF p_ic = 'X'.

    SELECT   vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             vbrk~knumv
              vbrk~fkdat
                vbrp~erdat
                vbrp~erzet

                vbrk~spart
                vbrk~vbeln
                vbrk~fkart
                vbrk~kunag
                vbrk~kunrg
                vbrk~vkorg
                vbrk~vtweg
                vbrk~kurrf    "added on 1/2
                vbrk~netwr
                vbrk~sfakn
                vbrk~knkli
                vbrk~mwsbk
                vbrk~fksto
                vbrp~posnr
                vbrp~vrkme
                vbrp~vkbur
                vbrp~matnr
                vbrp~fkimg
                vbrp~vgbel
                vbrp~volum
                vbrp~aubel

                INTO CORRESPONDING FIELDS OF TABLE it_vbrk
                FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                WHERE vbrk~bukrs IN so_bukrs AND vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln AND
                vbrk~kunag IN so_kunag AND  vbrk~spart IN so_spart AND
               " AND VBRP~VKBUR IN so_vkbu2 AND
               " VBRP~WERKS IN so_vkbu2 AND
                vbrk~fkart IN so_fkart AND
                ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG') .

    SELECT
      ebeln
      ebelp
      bwart
      dmbtr
      xblnr
           FROM ekbe INTO TABLE it_ekbe FOR ALL ENTRIES IN it_vbrk WHERE ebeln = it_vbrk-aubel AND bwart = '643' . "AND XBLNR = IT_VBRK-VGBEL .

    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    SORT it_vbrk BY vbeln . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln  .


  ENDIF.


  " IF SY-SUBRC = 0.
  SELECT vbeln posnr fkimg volum matnr werks FROM vbrp INTO CORRESPONDING FIELDS OF TABLE it_vbrp FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.
  " ENDIF.

  SELECT      knumv                                         "13/10
              kposn
              kschl
              kbetr
              kwert
          FROM prcd_elements " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
              INTO TABLE it_konv
              FOR ALL ENTRIES IN it_vbrk
              WHERE knumv = it_vbrk-knumv AND ( kschl EQ 'VPRS' OR kschl EQ 'YBSD' OR kschl EQ 'Y005' OR kschl EQ 'Y006' OR kschl EQ 'Y029' OR
                                               kschl EQ 'Y007' OR kschl EQ 'Y008' OR kschl EQ 'ZPRB' OR kschl EQ 'BO01' OR kschl EQ 'ZCAD' OR kschl EQ 'Y004' OR kschl EQ 'YBRD') .


  SELECT vbeln
         btgew FROM likp INTO TABLE it_likp FOR ALL ENTRIES IN it_vbrk
         WHERE vbeln = it_vbrk-vgbel.



  SELECT vbeln
          erdat
          erzet
          vsnmr_v FROM vbak INTO TABLE it_vbak
          FOR ALL ENTRIES IN it_vbrk
          WHERE  vbeln = it_vbrk-aubel.


  SELECT invoiceno
         uniq1
         tdate
         time
         cartons
         city      FROM ztrip_st INTO TABLE it_ztrip_st
         FOR ALL ENTRIES IN  it_vbrk
            WHERE  invoiceno = it_vbrk-vbeln .

  IF it_vbak IS NOT INITIAL.
    SELECT
      vbeln
      parvw
      pernr
      FROM vbpa INTO TABLE it_vbpa
          FOR ALL ENTRIES IN it_vbak
          WHERE  vbeln = it_vbak-vbeln AND parvw = 'L5'.
*&------Begin of Keerthi CLSS on 24-08-2016-----*
    SELECT
  vbeln
  parvw
  pernr
  FROM vbpa INTO TABLE it_vbpa1
      FOR ALL ENTRIES IN it_vbak
      WHERE  vbeln = it_vbak-vbeln AND parvw = 'L3'.
  ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
  IF it_vbpa IS NOT INITIAL.
    SELECT
      pernr
      endda
      begda
      sname
        FROM pa0001 INTO TABLE it_pa0001
        FOR ALL ENTRIES IN it_vbpa
        WHERE pernr = it_vbpa-pernr.
  ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*
  IF it_vbpa1 IS NOT INITIAL.
    SELECT
       pernr
       endda
       begda
       sname
         FROM pa0001 INTO TABLE it_pa0001_1
         FOR ALL ENTRIES IN it_vbpa1
         WHERE pernr = it_vbpa1-pernr.
  ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*


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

  DELETE ADJACENT DUPLICATES FROM it_vbrk_temp COMPARING ALL FIELDS.

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
           FROM kna1 INTO TABLE it_kna1
           FOR ALL ENTRIES IN it_vbrk_temp
           WHERE kunnr = it_vbrk_temp-kunrg AND kunnr = it_vbrk_temp-kunag.

  ENDIF.

  IF p_ic = 'X'.

    TYPES: BEGIN OF ty_vbrp_temp,
             vbeln TYPE vbrp-vbeln,
             posnr TYPE vbrp-posnr,
             kunag TYPE vbrp-fkimg,
             matnr TYPE vbrp-matnr,
             volum TYPE vbrp-volum,
             werks TYPE vbrp-werks,
           END OF ty_vbrp_temp.

    DATA: it_vbrp_temp TYPE STANDARD TABLE OF ty_vbrp_temp,
          wa_vbrp_temp LIKE LINE OF it_vbrp_temp.

    LOOP AT it_vbrp INTO wa_vbrp.

      MOVE-CORRESPONDING wa_vbrp TO wa_vbrp_temp.
      APPEND wa_vbrp_temp TO it_vbrp_temp.
      CLEAR: wa_vbrp, wa_vbrp_temp.

    ENDLOOP.

    "SORT it_vbrk_temp BY spart vkbur kunag kunrg.

    SORT it_vbrp_temp  . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrp_temp COMPARING ALL FIELDS.

    IF it_vbrp_temp IS NOT INITIAL.

      SELECT vkbur
             bezei
             FROM tvkbt INTO TABLE it_tvkbt
             FOR ALL ENTRIES IN it_vbrp_temp
             WHERE vkbur = it_vbrp_temp-werks
             AND spras = 'EN'.



*    SELECT spart
*           vtext
*           FROM tspat INTO TABLE it_tspat
*           FOR ALL ENTRIES IN it_vbrk_temp
*           WHERE spart = it_vbrk_temp-spart
*           AND spras = 'EN'.

*    SELECT kunnr
*           name1
*           name2
*           FROM kna1 INTO TABLE it_kna1
*           FOR ALL ENTRIES IN it_vbrk_temp
*           WHERE kunnr = it_vbrk_temp-kunrg AND kunnr = it_vbrk_temp-kunag.

    ENDIF.

  ENDIF.



  LOOP AT it_vbrk INTO wa_vbrk.
    wa_finovr-knumv = wa_vbrk-knumv.
    wa_finovr-fkdat = wa_vbrk-fkdat.
    wa_finovr-bukrs = wa_vbrk-bukrs.


    wa_finovr-spart = wa_vbrk-spart.
    wa_finovr-vbeln = wa_vbrk-vbeln.
    wa_finovr-fkart = wa_vbrk-fkart.
    wa_finovr-kunag = wa_vbrk-kunag.
    wa_finovr-kunrg = wa_vbrk-kunrg.
    wa_finovr-vkorg = wa_vbrk-vkorg.
    wa_finovr-vtweg = wa_vbrk-vtweg.
    wa_finovr-kurrf = wa_vbrk-kurrf.      "added on 1/2
    wa_finovr-netwr = wa_vbrk-netwr.
    wa_finovr-sfakn = wa_vbrk-sfakn.
*    wa_finovr-posnr = wa_vbrk-posnr.
    wa_finovr-vkbur = wa_vbrk-vkbur.
*    WA_FINOVR-MWSBK = WA_FINOVR-MWSBK.
    wa_finovr-mwsbk = wa_vbrk-mwsbk.

    IF p_cb NE 'X' .
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG'.
        wa_finovr-netwr = - ( wa_finovr-netwr ).
        wa_finovr-mwsbk = - ( wa_finovr-mwsbk ) .

      ENDIF .
    ENDIF .

    wa_finovr-inv_val = wa_vbrk-netwr + wa_vbrk-mwsbk.

    IF p_cb NE 'X' .
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG'.

        wa_finovr-inv_val = - ( wa_finovr-inv_val ) .
      ENDIF .
    ENDIF .
    wa_finovr-erzet = wa_vbrk-erzet .
    wa_finovr-erdat = wa_vbrk-erdat .



    wa_finovr-aubel = wa_vbrk-aubel .


    wa_finovr-invoiceno = wa_vbrk-invoiceno .
    wa_finovr-uniq1 = wa_vbrk-uniq1 .
    wa_finovr-ttdate = wa_vbrk-ttdate .
    wa_finovr-ttime = wa_vbrk-ttime .
    wa_finovr-cartons = wa_vbrk-cartons .
    wa_finovr-city = wa_vbrk-city .



    wa_finovr-vsnmr_v = wa_vbrk-vsnmr_v .




    LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.

      wa_finovr-qty_ltr =  wa_finovr-qty_ltr + wa_vbrp-volum.

      wa_finovr-int_code = wa_vbrp-werks .

      IF p_cb NE 'X' .
        IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG'.

          "  wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ) . "COMMENTED ON 8/2
        ENDIF .
      ENDIF .


    ENDLOOP.


    READ TABLE it_likp INTO wa_likp WITH KEY vbeln = wa_vbrk-vgbel.

    IF sy-subrc = 0.

      wa_finovr-btgew = wa_likp-btgew.

    ENDIF.

    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrk-vkbur.
    IF sy-subrc = 0.
      wa_finovr-bezei = wa_tvkbt-bezei.
    ENDIF.

    IF p_ic = 'X'.
      READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrp-werks.
      IF sy-subrc = 0.
        wa_finovr-bezei = wa_tvkbt-bezei.
      ENDIF.
    ENDIF.

    READ TABLE it_tspat INTO wa_tspat WITH KEY spart = wa_vbrk-spart.

    IF sy-subrc = 0.
      wa_finovr-vtext = wa_tspat-vtext.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunag .

    IF sy-subrc = 0.
      wa_finovr-name1 = wa_kna1-name1.
      wa_finovr-name2 = wa_kna1-name1.
    ENDIF.




    READ TABLE it_vbak INTO wa_vbak
       WITH KEY vbeln = wa_vbrk-aubel.

    IF sy-subrc = 0 .

      wa_finovr-erdat1 = wa_vbak-erdat.
      wa_finovr-erzet1 = wa_vbak-erzet.


      wa_finovr-vsnmr_v = wa_vbak-vsnmr_v.


    ENDIF.

    READ TABLE it_ztrip_st INTO wa_ztrip_st
        WITH KEY invoiceno = wa_vbrk-vbeln.

    IF sy-subrc = 0.

      wa_finovr-invoiceno = wa_ztrip_st-invoiceno.
      wa_finovr-uniq1 = wa_ztrip_st-uniq1.
      wa_finovr-ttdate = wa_ztrip_st-ttdate.
      wa_finovr-ttime = wa_ztrip_st-ttime.
      wa_finovr-cartons = wa_ztrip_st-cartons.
      wa_finovr-city = wa_ztrip_st-city.
    ENDIF.


    READ TABLE it_vbpa INTO wa_vbpa
       WITH KEY vbeln = wa_vbak-vbeln.

    IF sy-subrc = 0.
      wa_finovr-pernr  = wa_vbpa-pernr .
    ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*
    READ TABLE it_vbpa1 INTO wa_vbpa1
       WITH KEY vbeln = wa_vbak-vbeln.

    IF sy-subrc = 0.
      wa_finovr-sm_no  = wa_vbpa1-pernr.
    ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
    IF wa_finovr-pernr IS NOT INITIAL .
      READ TABLE it_pa0001 INTO wa_pa0001
         WITH KEY pernr = wa_vbpa-pernr.

      IF sy-subrc = 0.
        wa_finovr-sname   = wa_pa0001-sname  .
      ENDIF.
    ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*
    IF wa_finovr-sm_no IS NOT INITIAL .
      READ TABLE it_pa0001_1 INTO wa_pa0001_1
         WITH KEY pernr = wa_vbpa1-pernr.

      IF sy-subrc = 0.
        wa_finovr-sm_name   = wa_pa0001_1-sname  .
      ENDIF.

*&-------End of Keerthi CLSS on 24-08-2016------*
    ENDIF.


    LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk-knumv AND kschl EQ 'VPRS'.
      wa_finovr-kwert = wa_finovr-kwert + wa_konv-kwert .
    ENDLOOP .

    LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk-knumv AND ( kschl EQ 'YBSD' OR kschl EQ 'Y005' OR kschl EQ 'Y006' OR kschl EQ 'Y029' OR
                                              kschl EQ 'Y007' OR kschl EQ 'ZPRB' OR kschl EQ 'BO01' OR kschl EQ 'ZCAD' OR kschl EQ 'YBRD'  ) .
      wa_finovr-inbill =  wa_finovr-inbill + wa_konv-kwert .
    ENDLOOP .

    LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk-knumv AND kschl EQ 'Y004'.
      wa_finovr-tot_y004 = wa_finovr-tot_y004 + wa_konv-kwert .
    ENDLOOP .

    LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk-knumv AND kschl EQ 'Y008'.
      wa_finovr-tot_y008 = wa_finovr-tot_y008 + wa_konv-kwert .
    ENDLOOP .

    APPEND wa_finovr TO it_finovr.
    CLEAR : wa_finovr, wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001,wa_konv.
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

  IF p_ic NE 'X'.
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

      " DELETE IT_FINOVR WHERE VKBUR = ''.

    ENDIF.
  ENDIF.


  IF p_ic EQ 'X'.

    LOOP AT it_finovr INTO wa_finovr .

      IF wa_finovr-kwert EQ '0.00'.

        LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_finovr-aubel .

          wa_finovr-kwert = wa_ekbe-dmbtr.



          MODIFY it_finovr FROM wa_finovr TRANSPORTING kwert.

        ENDLOOP.

      ENDIF.

    ENDLOOP.



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



  LOOP AT it_finovr INTO wa_finovr.
    IF p_cb NE 'X' .
      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE' OR wa_finovr-fkart = 'IG'.     " added by mani 19.02.2016

        wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ) . "ADDED ON 8/2
*  wa_finovr-QTY_LTR =  - ( wa_finovr-QTY_LTR ) .
        wa_finovr-btgew =  - ( wa_finovr-btgew ) .
        wa_finovr-kwert = - ( wa_finovr-kwert ) .

        MODIFY it_finovr FROM wa_finovr TRANSPORTING qty_ltr btgew kwert.

      ENDIF.
    ENDIF.

  ENDLOOP.

  LOOP AT it_finovr INTO wa_finovr .

    IF wa_finovr-fkart = 'YBBR' OR wa_finovr-fkart = 'YBDP' OR wa_finovr-fkart = 'YBFS' OR wa_finovr-fkart = 'YRF2' OR wa_finovr-fkart = 'YBEO' OR wa_finovr-fkart = 'IG' .

      wa_finovr-inbill = - ( wa_finovr-inbill ) .
      wa_finovr-tot_y004 = - ( wa_finovr-tot_y004 ).


      MODIFY it_finovr FROM wa_finovr TRANSPORTING inbill tot_y004 .


    ENDIF.

  ENDLOOP.


  LOOP AT it_finovr INTO wa_finovr .

    IF wa_finovr-fkart = 'YBEO'  .

      wa_finovr-netwr =  wa_finovr-kurrf * wa_finovr-netwr  .
      wa_finovr-kwert = wa_finovr-kurrf * wa_finovr-kwert .
      wa_finovr-inv_val = wa_finovr-kurrf * wa_finovr-inv_val .


      MODIFY it_finovr FROM wa_finovr TRANSPORTING netwr kwert inv_val.

    ENDIF.

  ENDLOOP.


  LOOP AT it_finovr INTO wa_finovr WHERE int_code NOT IN so_vkbu2 . " and INT_CODE ne SO_VKBU2-HIGH.

    IF p_ic EQ 'X'.

      DELETE it_finovr  .

    ENDIF.

  ENDLOOP.


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

  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DATE'.
  wa_fieldcat-col_pos     = 2.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'FKDAT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.
  IF p_ic <> 'X' .
    wa_fieldcat-fieldname   = 'VKBUR'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 3.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

    wa_sort-fieldname = 'VKBUR'.
    wa_sort-up = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

  ENDIF.

  IF p_ic = 'X' .
    wa_fieldcat-fieldname   = 'INT_CODE'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 3.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.

    wa_sort-fieldname = 'INT_CODE'.
    wa_sort-up = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.
  ENDIF.

  wa_fieldcat-fieldname   = 'BEZEI'.
  wa_fieldcat-seltext_m   = 'SALES DESCRIPTION'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VKORG'.
  wa_fieldcat-seltext_m   = 'SALES ORGANIZATION'.
  wa_fieldcat-col_pos     = 5.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VTWEG'.
  wa_fieldcat-seltext_m   = 'DISTRIBUTION CHANNEL'.
  wa_fieldcat-col_pos     = 6.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION DESCRIPTION'.
  wa_fieldcat-col_pos     = 8.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VSNMR_V'.
  wa_fieldcat-seltext_m   = 'SALES REQ PLAN'.                                  "  ADDED BY MANIKANDAN ON 26-09-2015
  wa_fieldcat-col_pos     = 9.
*  WA_FIELDCAT-HOTSPOT     = 'X'.
*  WA_FIELDCAT-CFIELDNAME   = 'VBELN'.
*  WA_FIELDCAT-CTABNAME    = 'VBRP'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.




  wa_fieldcat-fieldname   = 'VBELN'.
  wa_fieldcat-seltext_m   = 'BILLING DOCUMENT'.
  wa_fieldcat-col_pos     = 10.
  wa_fieldcat-hotspot     = 'X'.
  wa_fieldcat-cfieldname   = 'VBELN'.
  wa_fieldcat-ctabname    = 'VBRP'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CANDOC'.
  wa_fieldcat-seltext_m   = 'CANCELLED DOCUMENT'.
  wa_fieldcat-col_pos     = 11.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'FKART'.
  wa_fieldcat-seltext_m   = 'BILLING TYPE'.
  wa_fieldcat-col_pos     = 12.
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

*  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
*  WA_FIELDCAT-SELTEXT_M   = 'SP NAME'.
*  WA_FIELDCAT-COL_POS     = 10.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  wa_fieldcat-col_pos     = 13.
  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-outputlen = 20.
  wa_fieldcat-lowercase = 'X'.
  wa_fieldcat-cfieldname   = 'KUNRG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'NAME2'.
  wa_fieldcat-seltext_m   = 'PAYER NAME'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CITY'.
  wa_fieldcat-seltext_m   = 'City'.
  wa_fieldcat-col_pos     = 15.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'QTY_LTR'.
  wa_fieldcat-seltext_m   = 'TOTAL IN LITES/KGS'.
  wa_fieldcat-col_pos     = 16.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*
  wa_fieldcat-fieldname   = 'BTGEW'.
  wa_fieldcat-seltext_m   = 'TOTAL WEIGHT'.
  wa_fieldcat-col_pos     = 17.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


*  wa_fieldcat-fieldname   = 'KNUMV'.
*  wa_fieldcat-seltext_m   = 'DOC.CON'.
*  wa_fieldcat-col_pos     = 18.
*  wa_fieldcat-do_sum      = 'X'.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'KWERT'.
  wa_fieldcat-seltext_m   = 'COGS'.
  wa_fieldcat-col_pos     = 19.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'NETWR'.
  wa_fieldcat-seltext_m   = 'NET VALUE'.
  wa_fieldcat-col_pos     = 20.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'INBILL'.
  wa_fieldcat-seltext_m   = 'Total Inbill Rebate'.
  wa_fieldcat-col_pos     = 21.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TOT_Y004'.
  wa_fieldcat-seltext_m   = 'Total Cash Discount'.
  wa_fieldcat-col_pos     = 22.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TOT_Y008'.
  wa_fieldcat-seltext_m   = 'Rebate SKU'.
  wa_fieldcat-col_pos     = 23.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'MWSBK'.
  wa_fieldcat-seltext_m   = 'TAX VALUE'.
  wa_fieldcat-col_pos     = 24.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'INV_VAL'.
  wa_fieldcat-seltext_m   = 'INVOICE VALUE'.
  wa_fieldcat-col_pos     = 25.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERDAT'.
  wa_fieldcat-seltext_m   = 'INV.CREATED DATE'.
  wa_fieldcat-col_pos     = 26.
  wa_fieldcat-do_sum      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ERZET'.
  wa_fieldcat-seltext_m   = 'TIME'.
  wa_fieldcat-col_pos     = 27.
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



  wa_fieldcat-fieldname   = 'AUBEL'.
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 28.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.





  wa_fieldcat-fieldname   = 'ERDAT1'.
  wa_fieldcat-seltext_m   = 'Date '.
  wa_fieldcat-col_pos     = 29.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



  wa_fieldcat-fieldname   = 'ERZET1'.
  wa_fieldcat-seltext_m   = 'Time'.
  wa_fieldcat-col_pos     = 30.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.




  wa_fieldcat-fieldname   = 'UNIQ1'.
  wa_fieldcat-seltext_m   = 'Trip Number'.
  wa_fieldcat-col_pos     = 31.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'TTDATE'.
  wa_fieldcat-seltext_m   = 'Trip Date'.
  wa_fieldcat-col_pos     = 32.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'TTIME'.
  wa_fieldcat-seltext_m   = 'Trip Time'.
  wa_fieldcat-col_pos     = 33.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'CARTONS'.
  wa_fieldcat-seltext_m   = 'LR Number'.
  wa_fieldcat-col_pos     = 34.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'PERNR'.
  wa_fieldcat-seltext_m   = 'S.O ID'.
  wa_fieldcat-col_pos     = 35.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.
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

  layout-colwidth_optimize = 'X'.

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

    CONCATENATE  wa_finovr-knumv wa_finovr-fkdat wa_finovr-vkbur wa_finovr-bezei wa_finovr-vkorg wa_finovr-vtweg wa_finovr-spart wa_finovr-vtext  wa_finovr-vbeln wa_finovr-fkart wa_finovr-erdat wa_finovr-erzet "ADDED BY RAM
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
           vkbur TYPE lips-vkbur,
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


  IF p_sr <> 'X' AND p_cb <> 'X' AND p_ic <> 'X' AND p_bp EQ 'X'.

    SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
          vbrk~knumv
          vbrk~bukrs
          vbrk~fkdat
          vbrp~erdat
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
          vbrk~sfakn
          vbrk~fksto
          vbrp~posnr
          vbrp~arktx
          vbrp~vrkme
          vbrp~vgbel
          vbrp~vkbur
          vbrp~kursk
          vbrp~netwr
          vbrp~werks
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
      AND vbrk~fksto <> 'X' AND  fkart <> 'YBRE' AND fkart <> 'YIRE' AND fkart <> 'IVS' AND fkart <> 'YRF2'
      AND fkart <> 'YFRE' AND fkart <> 'YLRE' AND fkart <> 'IG'
      AND  vbrk~fkart IN so_fkart
                AND lips~charg IN so_charg .

  ELSEIF p_cb = 'X'.
    SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        vbrk~knumv
        vbrk~bukrs
       vbrk~fkdat
            vbrp~erdat
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
          vbrk~sfakn
           vbrk~fksto
          vbrp~posnr
          vbrp~arktx
          vbrp~vrkme
          vbrp~vgbel
          vbrp~vkbur
          vbrp~kursk
          vbrp~netwr
          vbrp~werks
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

  ELSEIF p_sr = 'X' .


    SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           vbrk~knumv
           vbrk~bukrs
           vbrk~fkdat
            vbrp~erdat
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
          vbrk~sfakn
          vbrk~fksto
          vbrp~posnr
          vbrp~arktx
          vbrp~vrkme
          vbrp~vgbel
          vbrp~vkbur
          vbrp~kursk
          vbrp~netwr
          vbrp~werks
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
                AND vbrp~vkbur IN so_vkbu2
                AND vbrp~vbeln IN so_vbeln
                AND vbrk~kunag IN so_kunag
                AND vbrp~spart IN so_spart
      AND vbrk~fksto <> 'X' AND ( fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE' OR fkart = 'IVS' OR fkart <> 'YLRE' OR fkart <> 'IG' )
      AND  vbrk~fkart IN so_fkart
                AND lips~charg IN so_charg .


  ELSEIF p_ic = 'X' .


    SELECT vbrk~knumv "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           vbrk~bukrs
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
          vbrk~sfakn
          vbrk~fksto
          vbrp~posnr
          vbrp~arktx
          vbrp~vrkme
          vbrp~vgbel
          vbrp~vkbur
          vbrp~kursk
          vbrp~netwr
          vbrp~werks
          lips~matnr
          lips~vbeln
          lips~lfimg
          lips~charg
          vbrp~aubel
          vbrp~aupos

          INTO TABLE it_vbrk2
          FROM vbrp JOIN vbrk ON vbrp~vbeln = vbrk~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
                    JOIN lips ON vbrp~vgbel = lips~vbeln
                    AND   vbrp~matnr = lips~matnr
          WHERE    vbrk~bukrs IN so_bukrs
                AND  vbrk~fkdat IN so_fkdat
                AND vbrp~matnr IN so_matnr
                "AND VBRP~VKBUR IN SO_VKBU2
                AND vbrp~vbeln IN so_vbeln
                AND vbrk~kunag IN so_kunag
                AND vbrp~spart IN so_spart
       AND ( fkart = 'IV' OR fkart = 'IVS' OR fkart = 'IG' )
      AND  vbrk~fkart IN so_fkart.
*                AND LIPS~CHARG IN SO_CHARG .

    SELECT
          ebeln
          ebelp
          bwart
          dmbtr
          xblnr
               INTO TABLE it_ekbe FROM ekbe FOR ALL ENTRIES IN it_vbrk2 WHERE ebeln = it_vbrk2-aubel1 AND xblnr = it_vbrk2-lvbeln AND bwart = '643' ." AND EBELP = IT_VBRK2-POSNR_VA.


    SORT it_vbrk2 BY vbeln posnr matnr. " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk2 COMPARING vbeln posnr matnr.

  ENDIF.



  DATA: it_temp_vbrk2 TYPE STANDARD TABLE OF ty_vdrk2,
        wa_temp_vbrk2 LIKE LINE OF it_temp_vbrk2.

  MOVE it_vbrk2 TO it_temp_vbrk2.

  SORT it_temp_vbrk2 BY spart vkbur kunag.

  DELETE ADJACENT DUPLICATES FROM it_temp_vbrk2 COMPARING spart vkbur kunag.

  IF it_vbrk2 IS NOT INITIAL.

    SELECT vkbur
           bezei
           FROM tvkbt INTO TABLE it_tvkbt2
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE vkbur = it_temp_vbrk2-vkbur
           AND spras = 'EN'.

    IF p_ic = 'X'.

      SELECT vkbur
      bezei
      FROM tvkbt INTO TABLE it_tvkbt2
      FOR ALL ENTRIES IN it_temp_vbrk2
      WHERE vkbur = it_temp_vbrk2-werks
      AND spras = 'EN'.


    ENDIF.


    SELECT spart
           vtext
           FROM tspat INTO TABLE it_tspat2
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE spart = it_temp_vbrk2-spart
           AND spras = 'EN'.

    SELECT kunnr
           name1
           name2
           FROM kna1 INTO TABLE it_kna12
           FOR ALL ENTRIES IN it_temp_vbrk2
           WHERE kunnr = it_temp_vbrk2-kunag.
    SELECT matnr
         volum
         voleh FROM mara INTO TABLE it_mara1
         FOR ALL ENTRIES IN it_vbrk2 WHERE matnr = it_vbrk2-matnr.


    SELECT  vbeln
            aubel  FROM vbrp
            INTO TABLE itt_vbrp
            FOR ALL ENTRIES IN it_vbrk2
            WHERE vbeln = it_vbrk2-vbeln.


    SELECT  knumv                                         "13/10
            kposn
            kschl
            kbetr
            kwert
        FROM prcd_elements " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
            INTO TABLE it_konv
            FOR ALL ENTRIES IN it_vbrk2
            WHERE knumv = it_vbrk2-knumv AND kposn = it_vbrk2-posnr AND ( kschl EQ 'VPRS' OR kschl EQ 'YBSD' OR kschl EQ 'Y005' OR kschl EQ 'Y006' OR kschl EQ 'Y029' OR
                                            kschl EQ 'Y007' OR kschl EQ 'Y008' OR  kschl EQ 'ZPRB' OR kschl EQ 'BO01' OR kschl EQ 'ZCAD' OR kschl EQ 'Y004' OR kschl EQ 'YBRD' ) .



    IF sy-subrc = 0.

      SORT itt_vbrp BY vbeln.

    ENDIF.

    SELECT   vbeln
             erdat
             erzet
            vsnmr_v FROM  vbak
             INTO TABLE it_vbak
             FOR ALL ENTRIES IN itt_vbrp
              WHERE vbeln = itt_vbrp-aubel.

    IF sy-subrc = 0.

      SORT it_vbak BY vbeln .

    ENDIF.
    IF it_vbak IS NOT INITIAL.

      SELECT
         vbeln
         parvw
         pernr
         FROM vbpa INTO TABLE it_vbpa
             FOR ALL ENTRIES IN it_vbak
             WHERE  vbeln = it_vbak-vbeln AND parvw = 'L5'.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
      SELECT
    vbeln
    parvw
    pernr
    FROM vbpa INTO TABLE it_vbpa1
        FOR ALL ENTRIES IN it_vbak
        WHERE  vbeln = it_vbak-vbeln AND parvw = 'L3'.
*&-------End of Keerthi CLSS on 24-08-2016------*
    ENDIF.
    IF it_vbpa IS NOT INITIAL.
      SELECT
           pernr
           endda
           begda
           sname
             FROM pa0001 INTO TABLE it_pa0001
             FOR ALL ENTRIES IN it_vbpa
             WHERE pernr = it_vbpa-pernr.
    ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*
    IF it_vbpa1 IS NOT INITIAL.
      SELECT
         pernr
         endda
         begda
         sname
           FROM pa0001 INTO TABLE it_pa0001_1
           FOR ALL ENTRIES IN it_vbpa1
           WHERE pernr = it_vbpa1-pernr.
    ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*


  ENDIF.



  DATA: temp_pos  TYPE lips-charg,
        temp_pos1 TYPE lips-charg.


  DATA : it_read_vbrk2 TYPE STANDARD TABLE OF ty_vdrk2,
         wa_read_vbrk2 LIKE LINE OF it_read_vbrk2,
         temp_tabix    TYPE sy-tabix,
         temp_vgpos    TYPE vbrp-vgpos,
         temp_vgbel    TYPE vbrp-vgbel.

  SORT it_vbrk2 BY pstyv.

  LOOP AT it_vbrk2 INTO wa_vbrk2 WHERE pstyv = 'TAN'.



    IF  wa_vbrk2-vgpos NE wa_vbrk2-lposnr.
      wa_vbrk2-del_flag = 'X'.


      MODIFY it_vbrk2 FROM wa_vbrk2 INDEX sy-tabix.



      CLEAR: wa_vbrk2.


    ENDIF .

  ENDLOOP.


  DELETE it_vbrk2 WHERE del_flag = 'X'.


  SORT it_vbrk2 BY fkdat vbeln lposnr.

  SORT it_tvkbt2 BY vkbur.
  SORT it_tspat2 BY spart.
  SORT it_kna12 BY kunnr.
  SORT it_mara1 BY matnr.


  LOOP AT it_vbrk2 INTO wa_vbrk2  .

    IF  wa_vbrk2-vgpos = wa_vbrk2-lposnr AND
        wa_vbrk2-vgbel = wa_vbrk2-lvbeln AND
        wa_vbrk2-matnr = wa_vbrk2-lmatnr
    AND        wa_vbrk2-fkimg = wa_vbrk2-lfimg.

      wa_finaldet-fkart = wa_vbrk2-fkart.
      wa_finaldet-bukrs = wa_vbrk2-bukrs.
      wa_finaldet-knumv = wa_vbrk2-knumv.

      wa_finaldet-fkdat = wa_vbrk2-fkdat.
      wa_finaldet-spart = wa_vbrk2-spart.
      wa_finaldet-netwr = wa_vbrk2-netwr.
      wa_finaldet-kunag = wa_vbrk2-kunag.
      wa_finaldet-kunrg = wa_vbrk2-kunrg.
      wa_finaldet-sfakn = wa_vbrk2-sfakn.
      wa_finaldet-vbeln = wa_vbrk2-vbeln.
      wa_finaldet-lvbeln = wa_vbrk2-lvbeln.
*    wa_finaldet-erdat = wa_vbrk2-erdat.
      wa_finaldet-vkbur = wa_vbrk2-vkbur.
      wa_finaldet-posnr = wa_vbrk2-posnr.
      wa_finaldet-charg = wa_vbrk2-charg.
      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.
      wa_finaldet-fkimg = wa_vbrk2-fkimg.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
      wa_finaldet-vnetwr = wa_vbrk2-vnetwr.
      wa_finaldet-kursk = wa_vbrk2-kursk. "ADDED ON 1/2

      wa_finaldet-werks1 = wa_vbrk2-werks.

      wa_finaldet-order_num = wa_vbrk2-posnr.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .
      wa_finaldet-erzet = wa_vbrk2-erzet .

      wa_finaldet-t_volum = wa_vbrk2-volum .

      wa_finaldet-aubel = wa_vbrk2-aubel.
      wa_finaldet-aupos = wa_vbrk2-aupos.
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.

      IF p_ic = 'X'.
        READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_finaldet-werks1.
        IF sy-subrc = 0.
          wa_finaldet-bezei = wa_tvkbt2-bezei.
        ENDIF.
      ENDIF.

      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart BINARY SEARCH.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag BINARY SEARCH.
      IF sy-subrc = 0.
        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.
      ENDIF.


      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

      IF sy-subrc = 0.
        wa_finaldet-aubel  = waa_vbrp-aubel.


      ENDIF.

      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.


      IF sy-subrc = 0.

        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.
        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.

      ENDIF.

      LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND kschl EQ 'VPRS' . "13/10
        "  IF sy-subrc = 0.
        wa_finaldet-kwert = wa_konv-kwert .
        " ENDIF.
      ENDLOOP.


      LOOP AT it_konv INTO wa1_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND ( kschl EQ 'YBSD' OR kschl EQ 'Y005' OR kschl EQ 'Y006' OR kschl EQ 'Y029' OR
                                           kschl EQ 'Y007' OR kschl EQ 'ZPRB' OR kschl EQ 'BO01' OR kschl EQ 'ZCAD' OR kschl EQ 'YBRD') .
        "  IF sy-subrc = 0.
        wa_finaldet-inbill1 = wa_finaldet-inbill1 + wa1_konv-kwert .
        " ENDIF.
      ENDLOOP.

      LOOP AT it_konv INTO wa1_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND  kschl EQ 'Y004'  .
        "  IF sy-subrc = 0.
        wa_finaldet-tot_y004 = wa1_konv-kwert .
        " ENDIF.
      ENDLOOP.

      LOOP AT it_konv INTO wa1_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND  kschl EQ 'Y008'  .
        "  IF sy-subrc = 0.
        wa_finaldet-tot_y008 = wa1_konv-kwert .
        " ENDIF.
      ENDLOOP.


      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG' .

          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .
          wa_finaldet-kwert = - ( wa_finaldet-kwert ) .

        ENDIF.
      ENDIF.
      wa_finaldet-bukrs = wa_vbrk2-bukrs.

      READ TABLE it_vbpa INTO wa_vbpa
          WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-pernr  = wa_vbpa-pernr .
      ENDIF.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
      READ TABLE it_vbpa1 INTO wa_vbpa1
         WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-sm_no  = wa_vbpa1-pernr.
      ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*

      IF wa_finaldet-pernr IS NOT INITIAL .
        READ TABLE it_pa0001 INTO wa_pa0001
           WITH KEY pernr = wa_vbpa-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sname   = wa_pa0001-sname.
        ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*

        READ TABLE it_pa0001_1 INTO wa_pa0001_1
           WITH KEY pernr = wa_vbpa1-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
        ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
      ENDIF.



      APPEND wa_finaldet TO it_finaldet.
      CLEAR : wa_finaldet, wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
      CONTINUE.
    ENDIF.



    IF wa_vbrk2-vgpos = wa_vbrk2-lposnr AND
       wa_vbrk2-vgbel = wa_vbrk2-lvbeln AND
       wa_vbrk2-matnr = wa_vbrk2-lmatnr AND
       wa_vbrk2-pstyv = 'TAN' OR wa_vbrk2-pstyv = 'NLC'.

      wa_finaldet-fkart = wa_vbrk2-fkart.
      wa_finaldet-knumv = wa_vbrk2-knumv .
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
      wa_finaldet-kursk = wa_vbrk2-kursk.  "ADDED ON 1/2
      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.
      wa_finaldet-fkimg = wa_vbrk2-fkimg.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
      wa_finaldet-vnetwr = wa_vbrk2-vnetwr.

      wa_finaldet-werks1 = wa_vbrk2-werks.

      wa_finaldet-volum = wa_vbrk2-volum.
      wa_finaldet-voleh = wa_vbrk2-voleh.
      wa_finaldet-order_num = wa_vbrk2-posnr.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .
      wa_finaldet-erzet = wa_vbrk2-erzet .

      wa_finaldet-t_volum = wa_vbrk2-volum .
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.

      IF p_ic = 'X'.
        READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_finaldet-werks1.
        IF sy-subrc = 0.
          wa_finaldet-bezei = wa_tvkbt2-bezei.
        ENDIF.
      ENDIF.


      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.
      IF sy-subrc = 0.
        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.
      ENDIF.



      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

      IF sy-subrc = 0.
        wa_finaldet-aubel  = waa_vbrp-aubel.


      ENDIF.

      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.


      IF sy-subrc = 0.

        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.
        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.

      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa
      WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-pernr  = wa_vbpa-pernr .
      ENDIF.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
      READ TABLE it_vbpa1 INTO wa_vbpa1
         WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-sm_no  = wa_vbpa1-pernr.
      ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*

      IF wa_finaldet-pernr IS NOT INITIAL .
        READ TABLE it_pa0001 INTO wa_pa0001
           WITH KEY pernr = wa_vbpa-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sname   = wa_pa0001-sname.
        ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*

        READ TABLE it_pa0001_1 INTO wa_pa0001_1
           WITH KEY pernr = wa_vbpa1-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
        ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
      ENDIF.

      "   READ TABLE IT_KONV INTO WA_KONV WITH KEY KNUMV = WA_vbrk2-KNUMV KPOSN = WA_VBRK2-POSNR .    "13/10
      LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND kschl EQ 'VPRS' .
        "  IF sy-subrc = 0.
        wa_finaldet-kwert = wa_konv-kwert .
        " ENDIF.
      ENDLOOP.


      LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND kschl EQ 'Y004' .
        "  IF sy-subrc = 0.
        wa_finaldet-tot_y004 = wa_konv-kwert .
        " ENDIF.
      ENDLOOP.

      LOOP AT it_konv INTO wa_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND kschl EQ 'Y008' .
        "  IF sy-subrc = 0.
        wa_finaldet-tot_y008 = wa_konv-kwert .
        " ENDIF.
      ENDLOOP.


      LOOP AT it_konv INTO wa1_konv WHERE knumv = wa_vbrk2-knumv AND kposn = wa_vbrk2-posnr AND ( kschl EQ 'YBSD' OR kschl EQ 'Y005' OR kschl EQ 'Y006' OR kschl EQ 'Y029' OR
                                           kschl EQ 'Y007' OR kschl EQ 'ZPRB' OR kschl EQ 'BO01' OR kschl EQ 'ZCAD' OR kschl EQ 'YBRD' ) .
        "  IF sy-subrc = 0.
        wa_finaldet-inbill1 = wa_finaldet-inbill1 + wa1_konv-kwert . "TESTING
        " ENDIF.
      ENDLOOP.


      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG'.

          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

          wa_finaldet-kwert = - ( wa_finaldet-kwert ) .

        ENDIF.
      ENDIF.



      APPEND wa_finaldet TO it_finaldet.
      CLEAR: wa_finaldet, wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
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

    LOOP AT it_vbrk2 INTO wa_vbrk2 WHERE vbeln = wa_finaldet-vbeln AND matnr = wa_finaldet-matnr AND pstyv = 'YB99'.


      temp_lfimg =  wa_vbrk2-lfimg.

      IF temp_lfimg LE temp_fkimg.

        wa_finaldet-fkart = wa_vbrk2-fkart.
        wa_finaldet-knumv = wa_vbrk2-knumv .
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
        wa_finaldet-posnr = ''.
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
        wa_finaldet-kursk = wa_vbrk2-kursk.  "ADDED ON 1/2
        wa_finaldet-erdat = wa_vbrk2-erdat .
        wa_finaldet-erzet = wa_vbrk2-erzet .

        wa_finaldet-werks1 = wa_vbrk2-werks.

        wa_finaldet-t_volum = wa_vbrk2-volum .
        wa_finaldet-proc_key = 'X'.
        READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
        IF sy-subrc = 0.
          wa_finaldet-bezei = wa_tvkbt2-bezei.
        ENDIF.


        IF p_ic = 'X'.
          READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_finaldet-werks1.
          IF sy-subrc = 0.
            wa_finaldet-bezei = wa_tvkbt2-bezei.
          ENDIF.
        ENDIF.

        READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

        IF sy-subrc = 0.
          wa_finaldet-vtext = wa_tspat2-vtext.
        ENDIF.

        READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.

        IF sy-subrc = 0.
          wa_finaldet-name1 = wa_kna12-name1.
          wa_finaldet-name2 = wa_kna12-name1.
        ENDIF.


        READ TABLE itt_vbrp INTO waa_vbrp
           WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

        IF sy-subrc = 0.
          wa_finaldet-aubel  = waa_vbrp-aubel.


        ENDIF.

        READ TABLE it_vbak INTO wa_vbak
        WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.


        IF sy-subrc = 0.

          wa_finaldet-erdat1  = wa_vbak-erdat.
          wa_finaldet-erzet1  = wa_vbak-erzet.

          wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.
        ENDIF.

        IF p_cb NE 'X'.
          IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG'.

            wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
            wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
            wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

            wa_finaldet-kwert = - ( wa_finaldet-kwert ) .

          ENDIF.
        ENDIF.
*&--------------Begin of Keerthi CLSS on 29.08.2016---------------------*
        READ TABLE it_vbpa INTO wa_vbpa
        WITH KEY vbeln = wa_vbak-vbeln.

        IF sy-subrc = 0.
          wa_finaldet-pernr  = wa_vbpa-pernr .
        ENDIF.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
        READ TABLE it_vbpa1 INTO wa_vbpa1
           WITH KEY vbeln = wa_vbak-vbeln.

        IF sy-subrc = 0.
          wa_finaldet-sm_no  = wa_vbpa1-pernr.
        ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*

        IF wa_finaldet-pernr IS NOT INITIAL .
          READ TABLE it_pa0001 INTO wa_pa0001
             WITH KEY pernr = wa_vbpa-pernr.

          IF sy-subrc = 0.
            wa_finaldet-sname   = wa_pa0001-sname.
          ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*

          READ TABLE it_pa0001_1 INTO wa_pa0001_1
             WITH KEY pernr = wa_vbpa1-pernr.

          IF sy-subrc = 0.
            wa_finaldet-sm_name   = wa_pa0001_1-sname  .
          ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
        ENDIF.
*&---------------End of Keerthi CLSS on 29.08.2016----------------------*
        "   READ TABLE IT_KONV INTO WA_KONV WITH KEY KNUMV = WA_vbrk2-KNUMV KPOSN = WA_VBRK2-POSNR .    "13/10
*        LOOP AT IT_KONV INTO WA_KONV WHERE KNUMV = WA_VBRK2-KNUMV AND KPOSN = WA_VBRK2-POSNR .
*        "  IF sy-subrc = 0.
*            wa_finaldet-KWERT = WA_KONV-KWERT .
*         " ENDIF.
*        ENDLOOP.
*"BREAK-POINT. "RAM
*      LOOP AT IT_KONV INTO WA_KONV WHERE KNUMV = WA_VBRK2-KNUMV AND KPOSN = WA_VBRK2-POSNR AND KSCHL EQ 'VPRS'.
*        "  IF sy-subrc = 0.
*            wa_finaldet-KWERT = WA_KONV-KWERT .
*         " ENDIF.
*       ENDLOOP.

*         LOOP AT IT_KONV INTO WA1_KONV WHERE KNUMV = WA_VBRK2-KNUMV AND KPOSN = WA_VBRK2-POSNR AND ( KSCHL EQ 'YBSD' OR KSCHL EQ 'Y005' OR KSCHL EQ 'Y029' OR
*                                               KSCHL EQ 'Y007' OR KSCHL EQ 'ZPRB' OR KSCHL EQ 'BO01' OR KSCHL EQ 'ZCAD' ) .
*        "  IF sy-subrc = 0.
*            wa_finaldet-INBILL1 =  wa_finaldet-INBILL1 + WA1_KONV-KWERT .
*         " ENDIF.
*        ENDLOOP.

        APPEND wa_finaldet TO it_finaldet.
        CLEAR: wa_finaldet, wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.
      ENDIF.
    ENDLOOP.

    READ TABLE it_vbrk2 INTO wa_vbrk2 WITH KEY
    vbeln = wa_finaldet-vbeln matnr = wa_finaldet-matnr pstyv = 'YB99' proc_key = '' .



    IF sy-subrc = 0 .
      lv_tabix = sy-tabix.

      wa_finaldet-fkart = wa_vbrk2-fkart.
      wa_finaldet-knumv = wa_vbrk2-knumv.
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
      wa_finaldet-posnr = ''.
      wa_finaldet-charg = wa_vbrk2-charg.
      wa_finaldet-matnr = wa_vbrk2-matnr.
      wa_finaldet-arktx = wa_vbrk2-arktx.

*      WA_FINALDET-FKIMG = WA_VBRK2-FKIMG.
      wa_finaldet-vrkme = wa_vbrk2-vrkme.
*      WA_FINALDET-VNETWR = WA_VBRK2-VNETWR.

      wa_finaldet-werks1 = wa_vbrk2-werks.
      wa_finaldet-kursk = wa_vbrk2-kursk.       "ADDED ON 1/2
      wa_finaldet-volum = wa_vbrk2-volum.
      wa_finaldet-voleh = wa_vbrk2-voleh.
      wa_finaldet-lfimg = wa_vbrk2-lfimg.

      wa_finaldet-erdat = wa_vbrk2-erdat .
      wa_finaldet-erzet = wa_vbrk2-erzet .
*    WA_FINALDET-ORDER_NUM = WA_VBRK2-POSNR + 1.

      wa_finaldet-t_volum = wa_vbrk2-volum .

      wa_finaldet-proc_key = 'X'.
*    WA_FINALDET-ORDER_NUM =
      READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_vbrk2-vkbur.
      IF sy-subrc = 0.
        wa_finaldet-bezei = wa_tvkbt2-bezei.
      ENDIF.


      IF p_ic = 'X'.
        READ TABLE it_tvkbt2 INTO wa_tvkbt2 WITH KEY vkbur = wa_finaldet-werks1.
        IF sy-subrc = 0.
          wa_finaldet-bezei = wa_tvkbt2-bezei.
        ENDIF.
      ENDIF.

      READ TABLE it_tspat2 INTO wa_tspat2 WITH KEY spart = wa_vbrk2-spart.

      IF sy-subrc = 0.
        wa_finaldet-vtext = wa_tspat2-vtext.
      ENDIF.

      READ TABLE it_kna12 INTO wa_kna12 WITH KEY kunnr = wa_vbrk2-kunag.

      IF sy-subrc = 0.

        wa_finaldet-name1 = wa_kna12-name1.
        wa_finaldet-name2 = wa_kna12-name1.

      ENDIF.


      READ TABLE itt_vbrp INTO waa_vbrp
         WITH KEY vbeln = wa_vbrk2-vbeln BINARY SEARCH.

      IF sy-subrc = 0.
        wa_finaldet-aubel  = waa_vbrp-aubel.

      ENDIF.

      READ TABLE it_vbak INTO wa_vbak
      WITH KEY vbeln = waa_vbrp-aubel BINARY SEARCH.

      IF sy-subrc = 0.

        wa_finaldet-erdat1  = wa_vbak-erdat.
        wa_finaldet-erzet1  = wa_vbak-erzet.

        wa_finaldet-vsnmr_v  = wa_vbak-vsnmr_v.

      ENDIF.

      IF p_cb NE 'X'.
        IF  wa_finaldet-fkart = 'YLRE' OR wa_finaldet-fkart = 'YIRE' OR wa_finaldet-fkart = 'YFRE' OR wa_finaldet-fkart = 'IVS' OR wa_finaldet-fkart = 'YBRE' OR wa_finaldet-fkart = 'IG'.

          wa_finaldet-fkimg = - ( wa_finaldet-fkimg ) .
          wa_finaldet-t_volum   = - ( wa_finaldet-t_volum   ) .
          wa_finaldet-vnetwr   = - ( wa_finaldet-vnetwr  ) .

          wa_finaldet-kwert = - ( wa_finaldet-kwert ) .

        ENDIF.
      ENDIF.
*&--------------Begin of Keerthi CLSS on 29.08.2016---------------------*
      READ TABLE it_vbpa INTO wa_vbpa
      WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-pernr  = wa_vbpa-pernr .
      ENDIF.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
      READ TABLE it_vbpa1 INTO wa_vbpa1
         WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        wa_finaldet-sm_no  = wa_vbpa1-pernr.
      ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*

      IF wa_finaldet-pernr IS NOT INITIAL .
        READ TABLE it_pa0001 INTO wa_pa0001
           WITH KEY pernr = wa_vbpa-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sname   = wa_pa0001-sname.
        ENDIF.
*&------Begin of Keerthi CLSS on 24-08-2016-----*

        READ TABLE it_pa0001_1 INTO wa_pa0001_1
           WITH KEY pernr = wa_vbpa1-pernr.

        IF sy-subrc = 0.
          wa_finaldet-sm_name   = wa_pa0001_1-sname  .
        ENDIF.
*&-------End of Keerthi CLSS on 24-08-2016------*
      ENDIF.
*&---------------End of Keerthi CLSS on 29.08.2016----------------------*
      APPEND wa_finaldet TO it_finaldet.
*    CLEAR WA_TEMP_FINDET.

      wa_vbrk2-proc_key = 'X'.
      MODIFY it_vbrk2 FROM wa_vbrk2 INDEX lv_tabix TRANSPORTING proc_key.

    ENDIF.

    CLEAR: temp_lfimg, temp_fkimg, lv_tabix.
    CLEAR: wa_vbpa1, wa_pa0001_1, wa_vbpa, wa_pa0001.

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

  IF p_ic EQ 'X' .
    LOOP AT it_finaldet INTO wa_finaldet.


      IF wa_finaldet-kwert EQ '0.00'.

        LOOP AT it_ekbe INTO wa_ekbe WHERE xblnr = wa_finaldet-lvbeln .

          wa_finaldet-kwert = wa_ekbe-dmbtr.

          MODIFY it_finaldet FROM wa_finaldet TRANSPORTING kwert.

        ENDLOOP.

      ENDIF.

    ENDLOOP.


  ENDIF.

  LOOP AT it_finaldet INTO wa_finaldet.
    IF wa_finaldet-posnr = ' '.
      wa_finaldet-fkimg = '0' .
*  APPEND WA_FINALDET-FKIMG TO IT_FINALDET-FKIMG.
    ENDIF.
  ENDLOOP.



  IF it_finaldet IS INITIAL.

    MESSAGE 'No Data Exists for the Input' TYPE 'S' DISPLAY LIKE 'E'.

    STOP.
  ENDIF.

  DELETE it_finaldet WHERE fkimg = '0.00' .
*LOOP AT IT_FINALDET INTO WA_FINALDET .
*
*  DELETE wa_finaldet WHERE wa_finaldet-fkimg = '0'.
*
*  MODIFY WA_FINALDET FROM IT_FINALDET .
*  CLEAR WA_FINALDET .
*ENDLOOP.
*LOOP AT IT_FINALDET INTO WA_FINALDET .
*  IF WA_FINALDET-FKIMG = '0' .
*
*
*
*    ENDIF
*
*ENDLOOP.

  LOOP AT it_finaldet INTO wa_finaldet.

    READ TABLE it_mara1 INTO wa_mara1 WITH  KEY matnr = wa_finaldet-matnr BINARY SEARCH.

    wa_finaldet-m_volum = wa_mara1-volum.
    wa_finaldet-voleh = wa_mara1-voleh.

    READ TABLE it_vbrk2 INTO wa_vbrk2 WITH KEY vbeln = wa_finaldet-matnr .
    wa_finaldet-bukrs = wa_vbrk2-bukrs.


    MODIFY it_finaldet FROM wa_finaldet TRANSPORTING m_volum voleh  bukrs.
    CLEAR wa_finaldet.
  ENDLOOP.


  LOOP AT it_finaldet INTO wa_finaldet .

    IF wa_finaldet-fkart = 'YBBR' OR wa_finaldet-fkart = 'YBDP' OR wa_finaldet-fkart = 'YBFS' OR wa_finaldet-fkart = 'YRF2' OR wa_finaldet-fkart = 'YBEO'  .

      wa_finaldet-inbill1 = - ( wa_finaldet-inbill1 ) .
      wa_finaldet-tot_y004 = - ( wa_finaldet-tot_y004 ).


      MODIFY it_finaldet FROM wa_finaldet TRANSPORTING inbill1 tot_y004 .

    ENDIF.

  ENDLOOP.


  LOOP AT it_finaldet INTO wa_finaldet .

    IF wa_finaldet-fkart = 'YBEO'  .

      wa_finaldet-kwert =  wa_finaldet-kursk * wa_finaldet-kwert  .
      wa_finaldet-vnetwr = wa_finaldet-kursk * wa_finaldet-vnetwr .

      MODIFY it_finaldet FROM wa_finaldet TRANSPORTING kwert vnetwr .

    ENDIF.

  ENDLOOP.



  LOOP AT it_finaldet INTO wa_finaldet WHERE werks1 NOT IN so_vkbu2 . " and INT_CODE ne SO_VKBU2-HIGH.

    IF p_ic EQ 'X'.

      DELETE it_finaldet .

    ENDIF.

  ENDLOOP.

  "Added by Samsudeen 24/06/2022
  IF p_bp EQ 'X'.
    EXPORT it_finaldet FROM it_finaldet TO MEMORY ID 'CTAB'.
  ENDIF.

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

  wa_fieldcat-fieldname   = 'FKDAT'.
  wa_fieldcat-seltext_m   = 'BILLING DOCUMENT DATE'.
  wa_fieldcat-col_pos     = 2.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_sort-fieldname = 'FKDAT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  IF p_ic <> 'X'.
    wa_fieldcat-fieldname   = 'VKBUR'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 3.
    wa_fieldcat-cfieldname   = 'VKBUR'.
    wa_fieldcat-ctabname    = 'VBRP'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_sort-fieldname = 'VKBUR'.
*  WA_SORT-UP = 'X'.
    wa_sort-group = 'X'.
    APPEND wa_sort TO it_sort.
    CLEAR wa_sort.

    wa_fieldcat-fieldname   = 'BEZEI'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE DESCRIPTION'.
    wa_fieldcat-col_pos     = 4.
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


  IF p_ic = 'X'.

    wa_fieldcat-fieldname   = 'WERKS1'.
    wa_fieldcat-tabname   = 'IT_FINOVR'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE'.
    wa_fieldcat-col_pos     = 3.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.


    wa_fieldcat-fieldname   = 'BEZEI'.
    wa_fieldcat-seltext_m   = 'SALES OFFICE DESCRIPTION'.
    wa_fieldcat-col_pos     = 4.
    wa_fieldcat-cfieldname   = 'BEZEI'.
    wa_fieldcat-ctabname    = 'TVKBT'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.


  ENDIF.


  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION'.
  wa_fieldcat-cfieldname   = 'SPART'.
  wa_fieldcat-ctabname    = 'VBRK'.
  wa_fieldcat-col_pos     = 5.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

  wa_sort-fieldname = 'SPART'.
*  WA_SORT-UP = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.


  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'SALES DIVISION DESCRIPTION'.
  wa_fieldcat-col_pos     = 6.
  wa_fieldcat-cfieldname   = 'VTEXT'.
  wa_fieldcat-ctabname    = 'TSPAT'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR: wa_fieldcat.

*  WA_SORT-FIELDNAME = 'VTEXT'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'VSNMR_V'.
  wa_fieldcat-seltext_m   = 'SALES REQ PLAN'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 7.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*
*
*  WA_FIELDCAT-FIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-SELTEXT_M   = 'SOLD TO PARTY'.
*  WA_FIELDCAT-COL_POS     = 8.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
*  WA_FIELDCAT-CTABNAME    = 'VBRK'.

*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*  WA_SORT-FIELDNAME = 'KUNAG'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

*
*  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
*  WA_FIELDCAT-SELTEXT_M   = 'SP NAME'.
*  WA_FIELDCAT-COL_POS     = 9.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*  WA_SORT-FIELDNAME = 'NAME1'.
**  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'PAYER'.
  wa_fieldcat-col_pos     = 10.
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
  wa_fieldcat-seltext_m   = 'PAYER NAME'.  "Changed by sri 04.03.17
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
  wa_fieldcat-col_pos     = 14.
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



  wa_fieldcat-fieldname   = 'MATNR'.
  wa_fieldcat-seltext_m   = 'MATERIAL NUMBER'.
  wa_fieldcat-col_pos     = 16.
  wa_fieldcat-tabname     = 'MARA'.
  wa_fieldcat-datatype    = 'NUMC'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ARKTX'.
  wa_fieldcat-seltext_m   = 'MATERIAL DESCRIPTION'.
  wa_fieldcat-col_pos     = 18.
  wa_fieldcat-cfieldname   = 'ARKTX'.
  wa_fieldcat-ctabname    = 'VBRP'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.



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
  wa_fieldcat-col_pos     = 20.
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
  wa_fieldcat-col_pos     = 22.
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
  wa_fieldcat-col_pos     = 23.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOLEH'.
  wa_fieldcat-seltext_m   = 'LTRS/KGS UNIT'.
  wa_fieldcat-ctabname     = 'MARA'.
  wa_fieldcat-col_pos     = 24.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'T_VOLUM'.
  wa_fieldcat-seltext_m   = 'TOTAL LTRS/KGS'.
  wa_fieldcat-ctabname     = 'MARA'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 25.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*  wa_fieldcat-fieldname   = 'KNUMV'.
*  wa_fieldcat-seltext_m   = 'DOC.CON'.
*  wa_fieldcat-ctabname     = 'VBRK'.
**  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
*  wa_fieldcat-col_pos     = 26.
*  APPEND wa_fieldcat TO it_fieldcat.
*  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'KWERT'.  "13/10
  wa_fieldcat-seltext_m   = 'COGS'.
  wa_fieldcat-ctabname     = 'KONV'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 27.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VNETWR'.
  wa_fieldcat-seltext_m   = 'NET VALUE'.
*  WA_FIELDCAT-DATATYPE    = 'CURR'.
*  WA_FIELDCAT-DATATYPE    = 'CUCY'.
*  WA_FIELDCAT-TECH        = 'X'.
  wa_fieldcat-col_pos     = 28.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'INBILL1'.  "13/10
  wa_fieldcat-seltext_m   = 'Total InBill Rebate'.
  wa_fieldcat-ctabname     = 'KONV'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 29.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TOT_Y004'.  "13/10
  wa_fieldcat-seltext_m   = 'Total Cash Discount'.
  wa_fieldcat-ctabname     = 'KONV'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 30.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'TOT_Y008'.  "13/10
  wa_fieldcat-seltext_m   = 'Rebate SKU'.
  wa_fieldcat-ctabname     = 'KONV'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 31.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERDAT'.
  wa_fieldcat-seltext_m   = 'INV.CREATED DATE'.
  wa_fieldcat-ctabname     = 'VBRP'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 32.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'ERZET'.
  wa_fieldcat-seltext_m   = 'TIME'.
  wa_fieldcat-ctabname     = 'VBRP'.
*  WA_FIELDCAT-CFIELDNAME  = 'VOLUM'.
  wa_fieldcat-col_pos     = 33.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*
*  WA_SORT-FIELDNAME = 'NETWR'. " Modified by Govind on 02/08/2014 Time 12:01 PM
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.



  wa_fieldcat-fieldname   = 'AUBEL'.
  wa_fieldcat-seltext_m   = 'Sales Document'.
  wa_fieldcat-no_zero   = 'X'.
  wa_fieldcat-col_pos     = 34.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERDAT1'.
  wa_fieldcat-seltext_m   = 'Date'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 35.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'ERZET1'.
  wa_fieldcat-seltext_m   = 'Time'.
  wa_fieldcat-ctabname    = 'VBAK'.
  wa_fieldcat-col_pos     = 36.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'PERNR'.
  wa_fieldcat-seltext_m   = 'S.O NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 37.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SNAME'.
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 38.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*&------Begin of Keerthi CLSS on 24-08-2016-----*
  wa_fieldcat-fieldname   = 'SM_NO'.
  wa_fieldcat-seltext_m   = 'S.M NO'.
  wa_fieldcat-ctabname    = 'VBPA'.
  wa_fieldcat-col_pos     = 39.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-ctabname    = 'PA0001'.
  wa_fieldcat-col_pos     = 40.
*  WA_FIELDCAT-DO_SUM      = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*&-------End of Keerthi CLSS on 24-08-2016------*

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


  layout-colwidth_optimize = 'X'.
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
  " SORT it_finaldet BY charg vbeln.
  SORT it_finaldet BY vbeln.
  " DELETE it_finaldet WHERE charg = ' '.
  DELETE it_finaldet WHERE posnr = ' '.


  IF p_ic <> 'X'.

    IF p_fs <> 'X'.
      DELETE it_finaldet WHERE vnetwr = 0.
    ENDIF.
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
  INTO wa_attachment SEPARATED BY con_tab. "#EC CI_FLDEXT_OK[2215424]    "Added by SPLABAP during code remediation
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
