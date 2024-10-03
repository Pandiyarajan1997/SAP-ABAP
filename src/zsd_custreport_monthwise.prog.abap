*&---------------------------------------------------------------------*
*& Report   ZSD_CUSTREPORT_MONTHWISE
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Customer Register Month wise          *
*& Report Name                 : ZSD_CUSTREPORT_MONTHWISE             *
*& Development Id              : kpabap                                *
*& Related Information         : Customer Register Month wise          *
*&---------------------------------------------------------------------*
REPORT zsd_custreport_monthwise_old.

DATA:
      w_aux_fkdat TYPE vbrk-fkdat,
      w_fkart TYPE vbrk-fkart,
      w_vbeln TYPE vbrk-vbeln,
      w_kunag TYPE vbrk-kunag,
      w_matnr TYPE vbrp-matnr,
      w_charg TYPE lips-charg,                               " BATCH FOR INVOICE DETAILS
      w_inv_vkbur TYPE vbrp-vkbur,                           " SALES OFFICE FOR INVOICE
      w_spart TYPE vbrk-spart,

      counter TYPE i VALUE '0',
      gt_list     TYPE vrm_values,
      gwa_list    TYPE vrm_value,
      gt_values   TYPE TABLE OF dynpread,                     " INTERNAL TABLE FOR LIST BOX
      gwa_values  TYPE dynpread,                              " WORK AREA FOR LIST BOX
      gv_selected_value(10) TYPE c,
      it_fieldcat  TYPE TABLE OF slis_fieldcat_alv,
      wa_fieldcat  LIKE LINE OF it_fieldcat,
      it_sort TYPE slis_t_sortinfo_alv,
      wa_sort LIKE LINE OF it_sort,
      g_sent_to_all TYPE sonv-flag,
      g_tab_lines   TYPE i,


      w_kunnr TYPE knc1-kunnr,
      w_bukrs TYPE knc1-bukrs,                                " VARIABLE DECLARATIONS FOR ACCOUNT BALANCES
      w_vkbur TYPE knvv-vkbur,                                " SALES OFFICE FOR ACCOUNT BALANCES
*      W_GJAHR TYPE KNC1-GJAHR,
      w_re_period TYPE bsid-monat,

      w_s_matnr TYPE mard-matnr,                               " VARIABLE DECLARATIONS FOR STOCK
      w_s_bukrs TYPE t001-bukrs,
      w_s_hkont TYPE bseg-hkont,
      w_s_werks TYPE t001w-werks,
      w_s_lgort TYPE t001l-lgort,
      w_s_charg TYPE mchb-charg,
      w_s_bwtar TYPE mbew-bwtar,
      w_s_bwart TYPE mseg-bwart,
            w_s_datum TYPE mkpf-budat.


DATA : layout TYPE slis_layout_alv.


*--------------------------------------------------------------*
* TYPES DECLARATION
*--------------------------------------------------------------*


TYPES: BEGIN OF ty_vbrk_vbrp,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
       bukrs TYPE vbrk-bukrs,
       fkdat TYPE vbrk-fkdat,
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
       knkli TYPE vbrk-knkli,
       mwsbk TYPE vbrk-mwsbk,
       fksto TYPE vbrk-fksto,
       posnr TYPE vbrp-posnr,
       vkbur TYPE vbrp-vkbur,
       matnr TYPE vbrp-matnr,
       fkimg TYPE vbrp-fkimg,
       volum TYPE vbrp-volum,
       vgbel TYPE vbrp-vgbel,
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
       regio TYPE kna1-regio,
       END OF ty_kna1,

       BEGIN OF ty_knvv,                             " ADDED BY RAM ON 21/11/2014
       kunnr TYPE knvv-kunnr,
       vkorg TYPE knvv-vkorg,
       kdgrp TYPE knvv-kdgrp,
       spart TYPE knvv-spart, " Added By Govind On 06/12/2014
       END OF ty_knvv,

       BEGIN OF ty_t151t,
       spras TYPE t151t-spras,
       kdgrp TYPE t151t-kdgrp,
       ktext TYPE t151t-ktext,
       END OF ty_t151t,

       BEGIN OF ty_marm,
       matnr TYPE marm-matnr,
       meinh TYPE marm-meinh,
       umrez TYPE marm-umrez,
       umren TYPE marm-umren,
       END OF ty_marm,

 BEGIN OF gs_t005u ,
       bland TYPE t005u-bland ,                                                                                                           "TABLE 6
       bezei TYPE t005u-bezei ,                                  "REGION
       END OF gs_t005u ,

       BEGIN OF ty_finovr,                           "FINAL TABLE TYPE FOR OVERVIEW REPORT
       fkdat TYPE vbrk-fkdat,
       vkbur TYPE vbrp-vkbur,
       kunrg TYPE vbrk-kunag,
       spart TYPE vbrk-spart,
       vbeln TYPE vbrk-vbeln,
       candoc(1) TYPE c,
       fkart TYPE vbrk-fkart,
       kunag TYPE vbrk-kunag,
*       KUNRG TYPE VBRK-KUNRG,
       vkorg TYPE vbrk-vkorg,
       vtweg TYPE vbrk-vtweg,
       netwr TYPE vbrk-netwr,
       sfakn TYPE vbrk-sfakn,
       knkli TYPE vbrk-knkli,
       mwsbk TYPE vbrk-mwsbk,
       fksto TYPE vbrk-fksto,
       btgew TYPE likp-btgew, " Commended by Govind on 12.05.2014
       inv_val TYPE netwr,
       posnr TYPE vbrp-posnr,
       fkimg TYPE vbrp-fkimg,
       vnetwr TYPE vbrp-netwr,
       bezei TYPE tvkbt-bezei,
       vtext TYPE tspat-vtext,
       name1 TYPE kna1-name1,
       name2 TYPE kna1-name2,
       kdgrp TYPE knvv-kdgrp,              "ADDED BY RAM ON 21/11/2014
       spras TYPE t151t-spras,
       ktext TYPE t151t-ktext,
       m_volum TYPE mara-volum,
       qty_ltr TYPE p DECIMALS 2,
       t_volum TYPE p DECIMALS 2,
       v_count TYPE i,
       flag(1) TYPE c,
       n_month TYPE  t009b-poper,
       vol0 TYPE p DECIMALS 2,     " APR
       vol1 TYPE p DECIMALS 2,        "MAY
       vol2 TYPE p   DECIMALS 2,   " JUNE
       vol3 TYPE p DECIMALS 2,  " JULY
       vol4 TYPE p DECIMALS 2,     " AUG
       vol5 TYPE p DECIMALS 2,        "SEP
       vol6 TYPE p   DECIMALS 2,   " OCT
       vol7 TYPE p DECIMALS 2,  " NOV
       vol8 TYPE p DECIMALS 2,     " DEC
       vol9 TYPE p DECIMALS 2,        "JAN
       vol10 TYPE p   DECIMALS 2,   " FEB
       vol11 TYPE p DECIMALS 2,  " MAR
       amt0 TYPE p DECIMALS 2,     " APR
       amt1 TYPE p DECIMALS 2,        "MAY
       amt2 TYPE p   DECIMALS 2,   " JUNE
       amt3 TYPE p DECIMALS 2,  " JULY
       amt4 TYPE p DECIMALS 2,     " AUG
       amt5 TYPE p DECIMALS 2,        "SEP
       amt6 TYPE p   DECIMALS 2,   " OCT
       amt7 TYPE p DECIMALS 2,  " NOV
       amt8 TYPE p DECIMALS 2,     " DEC
       amt9 TYPE p DECIMALS 2,        "JAN
       amt10 TYPE p   DECIMALS 2,   " FEB
       amt11 TYPE p DECIMALS 2,  " MAR
*&-----Begin of Keerthi CLSS on 24-08-2016-----*
       so_no TYPE vbpa-pernr,       "Sales officer number
       so_name TYPE pa0001-sname,   "Sales officer name
       sm_no TYPE vbpa-pernr,       "Sales manager number
       sm_name TYPE pa0001-sname,   "Sales manager name
       END OF ty_finovr.

TYPES:BEGIN OF ty_knvp,                         "  ADDED BY RAM ON 05-07-2016
       kunnr TYPE knvp-kunnr,    "26.10.2016
       vkorg TYPE knvp-vkorg,
       vtweg TYPE knvp-vtweg,
       spart TYPE knvp-spart,
       parvw TYPE knvp-parvw,
       parza TYPE knvp-parza,
       pernr TYPE knvp-pernr,
      END OF ty_knvp.
TYPES:BEGIN OF str_pa0001,                         "  ADDED BY RAM ON 05-07-2016
      pernr TYPE pa0001-pernr,
      endda TYPE pa0001-endda,
      begda TYPE pa0001-begda,
      sname TYPE pa0001-sname,
      END OF str_pa0001.
*&------End of Keerthi CLSS on 24-08-2016------*
TYPES: BEGIN OF ty_tvkbt2,
    vkbur TYPE tvkbt-vkbur,
    bezei TYPE tvkbt-bezei,
    END OF ty_tvkbt2,

    BEGIN OF ty_tspat2,
    spart TYPE tspat-spart,
    vtext TYPE tspat-vtext,
    END OF ty_tspat2,


    BEGIN OF ty_vbrp, " Added by Govind
       vbeln TYPE vbrp-vbeln,
      posnr TYPE vbrp-posnr,
      fkimg TYPE vbrp-fkimg,
      volum TYPE vbrp-volum,
      matnr TYPE vbrp-matnr,
      netwr TYPE vbrp-netwr,
      END OF ty_vbrp,

BEGIN OF ty_mara,
  matnr TYPE mara-matnr,
  volum TYPE mara-volum,
  voleh TYPE mara-voleh,
  END OF ty_mara,


BEGIN OF ty_vdrk2,
fkdat TYPE vbrk-fkdat,
vbeln TYPE vbrp-vbeln,
lposnr TYPE lips-posnr,
matnr TYPE vbrp-matnr,
fkimg TYPE vbrp-fkimg,
vgpos TYPE vbrp-vgpos,
volum TYPE vbrp-volum,
voleh TYPE vbrp-voleh,
pstyv TYPE lips-pstyv,
spart TYPE  vbrk-spart,
netwr TYPE vbrk-netwr,
kunag TYPE vbrk-kunag,
kunrg TYPE vbrk-kunrg,
sfakn TYPE vbrk-sfakn,
fksto TYPE vbrk-fksto,
posnr TYPE vbrp-posnr,
arktx TYPE vbrp-arktx,
vrkme TYPE vbrp-vrkme,
vgbel TYPE vbrp-vgbel,
vkbur TYPE vbrp-vkbur,
lmatnr TYPE lips-matnr,
lvbeln TYPE lips-vbeln,
lfimg TYPE lips-lfimg,
charg TYPE lips-charg,
del_flag(1) TYPE c,
proc_key(1) TYPE c,

END OF ty_vdrk2.





*--------------------------------------------------------------*
*INTERNAL TABLE DECLARATIONS
*--------------------------------------------------------------*

*&-----Begin of Keerthi CLSS on 24-08-2016-----*
DATA: it_knvp TYPE TABLE OF ty_knvp,
      wa_knvp TYPE ty_knvp,
      it_knvp1 TYPE TABLE OF ty_knvp,
      wa_knvp1 TYPE ty_knvp,
      it_pa0001 TYPE TABLE OF str_pa0001,
      wa_pa0001 TYPE str_pa0001,
      it_pa0001_1 TYPE TABLE OF str_pa0001,
      wa_pa0001_1 TYPE str_pa0001.
*&------End of Keerthi CLSS on 24-08-2016------*

DATA: it_vbrk TYPE TABLE OF ty_vbrk_vbrp,
it_tvkbt TYPE TABLE OF ty_tvkbt,
it_tspat  TYPE TABLE OF ty_tspat,
gt_kna1 TYPE TABLE OF ty_kna1,
gt_knvv TYPE TABLE OF ty_knvv,   "ADDED BY RAM ON 21/11/2014
gt_t151t  TYPE TABLE OF ty_t151t ,
it_mara TYPE TABLE OF ty_mara,
 gt_t005u TYPE TABLE OF gs_t005u ,
it_vbrk2 TYPE TABLE OF ty_vdrk2,
it_tvkbt2 TYPE TABLE OF ty_tvkbt2,
it_tspat2  TYPE TABLE OF ty_tspat2,
it_marm TYPE TABLE OF ty_marm,
it_vbrp TYPE TABLE OF ty_vbrp,
it_finovr TYPE TABLE OF ty_finovr.



*--------------------------------------------------------------*
*WORK AREA DECLARATIONS
*--------------------------------------------------------------*

DATA: wa_vbrk TYPE ty_vbrk_vbrp,
      wa_tvkbt TYPE  ty_tvkbt,
      wa_tspat  TYPE ty_tspat,
      wa_kna1 TYPE ty_kna1,
      wa_knvv TYPE ty_knvv,      "ADDED BY RAM ON 21/11/2014
      wa_t151t TYPE ty_t151t,
      wa_mara TYPE ty_mara,
      wa_t005u TYPE gs_t005u,
      wa_vbrk2 TYPE ty_vdrk2,
      wa_tvkbt2 TYPE  ty_tvkbt2,
      wa_tspat2  TYPE ty_tspat2,

      wa_finovr LIKE LINE OF it_finovr,
      wa_marm LIKE LINE OF it_marm,
           wa_vbrp LIKE LINE OF it_vbrp.


DATA: no_month TYPE          t009b-poper,
  d_year  TYPE             t009b-bdatj.

DATA : lv_bukrs TYPE vbrk-bukrs.

DATA l LIKE sy-tabix.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-t01.



SELECT-OPTIONS: so_bukrs FOR lv_bukrs MODIF ID tb2 OBLIGATORY,
                so_fkdat FOR w_aux_fkdat MODIF ID tb2 DEFAULT sy-datum OBLIGATORY,        " SELECTION SCREEN ELEMENTS FOR INVOICE
                so_fkart  FOR w_fkart MODIF ID tb2,
                so_vbeln FOR w_vbeln MODIF ID tb2,
                so_kunag FOR w_kunag MODIF ID tb2,
                so_matnr FOR w_matnr MODIF ID tb2 NO-DISPLAY,
                so_charg FOR w_charg MODIF ID tb5 NO-DISPLAY,
                so_vkbu2 FOR w_inv_vkbur MODIF ID tb2 OBLIGATORY,                                    " SELECT OPTIONS FOR SALES OFFICE IN invoice
                so_spart FOR w_spart MODIF ID tb2.

SELECTION-SCREEN SKIP.

PARAMETERS: p_cb AS CHECKBOX USER-COMMAND cbc MODIF ID tb2.
PARAMETERS: p_sr AS CHECKBOX USER-COMMAND cbc MODIF ID tb2.

SELECTION-SCREEN: END OF BLOCK b2.
*

START-OF-SELECTION.



  PERFORM data_retrieval_overview.
  PERFORM authcheck_overview.
  PERFORM build_fieldcatalog_overview.
  PERFORM build_alv_overview.





*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_retrieval_overview .

  IF p_sr <> 'X' AND p_cb <> 'X'. " Added by Goivnd On 05/06/2014

    SELECT    vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              vbrk~fkdat
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
              vbrp~posnr
              vbrp~vrkme
              vbrp~vkbur
              vbrp~matnr
              vbrp~fkimg
              vbrp~vgbel
              vbrp~volum
              vbrp~netwr
              INTO CORRESPONDING FIELDS OF TABLE it_vbrk
              FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              WHERE vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
      AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~spart IN so_spart AND vbrk~fkart IN so_fkart AND fksto <> 'X' AND vbrk~bukrs IN so_bukrs
       AND  vbrk~fkart NOT IN ('S1','YBRE','S2','JEX','F1','F2', 'YLRE','YIRE').
SORT IT_VBRK BY VBELN. " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln.

  ELSEIF p_cb = 'X'. " Added by Goivnd On 05/06/2014
    SELECT     vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
              vbrk~fkdat
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
             vbrp~posnr
             vbrp~vrkme
             vbrp~vkbur
             vbrp~matnr
             vbrp~fkimg
             vbrp~vgbel
             vbrp~volum
             vbrp~netwr
             INTO CORRESPONDING FIELDS OF TABLE it_vbrk
             FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             WHERE vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
     AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~bukrs IN so_bukrs  AND vbrk~spart IN so_spart AND vbrk~fkart IN so_fkart AND fksto = 'X'  .
SORT IT_VBRK BY VBELN . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
*
*    SELECT     VBRK~BUKRS
*               VBRK~FKDAT
*                VBRK~SPART
*                VBRK~VBELN
*                VBRK~FKART
*                VBRK~KUNAG
*                VBRK~KUNRG
*                VBRK~VKORG
*                VBRK~VTWEG
*                VBRK~NETWR
*                VBRK~SFAKN
*                VBRK~KNKLI
*                VBRK~MWSBK
*                VBRK~FKSTO
*                VBRP~POSNR
*                VBRP~VRKME
*                VBRP~VKBUR
*                VBRP~MATNR
*                VBRP~FKIMG
*                VBRP~VGBEL
*                VBRP~VOLUM
*                INTO CORRESPONDING FIELDS OF TABLE IT_VBRK
*                FROM VBRK JOIN VBRP ON VBRK~VBELN = VBRP~VBELN
*                WHERE VBRK~FKDAT IN SO_FKDAT AND VBRK~VBELN IN SO_VBELN
*        AND VBRK~KUNAG IN SO_KUNAG AND VBRP~VKBUR IN SO_VKBU2 AND VBRK~BUKRS IN SO_BUKRS AND VBRK~SPART IN SO_SPART AND VBRK~FKART NOT IN ('S1','YBRE','S2','JEX','F1','F2') AND VBRK~FKART IN SO_FKART AND FKSTO <> 'X'  .
*    DELETE ADJACENT DUPLICATES FROM IT_VBRK COMPARING VBELN.
  ELSEIF p_sr = 'X' . " Added by Goivnd On 05/06/2014
    SELECT    vbrk~bukrs "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
         vbrk~fkdat
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
        vbrp~posnr
        vbrp~vrkme
        vbrp~vkbur
        vbrp~matnr
        vbrp~fkimg
        vbrp~vgbel
        vbrp~volum
       vbrp~netwr
        INTO CORRESPONDING FIELDS OF TABLE it_vbrk
        FROM vbrk JOIN vbrp ON vbrk~vbeln = vbrp~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        WHERE vbrk~fkdat IN so_fkdat AND vbrk~vbeln IN so_vbeln
AND vbrk~kunag IN so_kunag AND vbrp~vkbur IN so_vkbu2 AND vbrk~bukrs IN so_bukrs AND vbrk~spart IN so_spart   AND fksto <> 'X' AND
      ( fkart = 'YBBR' OR fkart = 'YBRE' OR fkart = 'YIRE' OR fkart = 'YFRE'
      OR fkart = 'IVS' OR fkart <> 'YLRE' ) AND vbrk~fkart NOT IN ('S1','S2','JEX','F1','F2') AND vbrk~fkart IN so_fkart.

SORT IT_VBRK BY VBELN . " Added by <IT-CAR Tool> during Code Remediation
    DELETE ADJACENT DUPLICATES FROM it_vbrk COMPARING vbeln .
  ENDIF.

*BREAK-POINT.
  IF sy-subrc = 0.
    SELECT vbeln posnr fkimg volum matnr netwr
      FROM vbrp INTO  CORRESPONDING FIELDS OF TABLE it_vbrp
       FOR ALL ENTRIES IN it_vbrk WHERE vbeln = it_vbrk-vbeln.
  ENDIF.

  SELECT matnr volum voleh
    FROM mara INTO CORRESPONDING FIELDS OF TABLE it_mara
     FOR ALL ENTRIES IN it_vbrp WHERE matnr = it_vbrp-matnr.

* SELECT KUNNR KDGRP SPART
*   FROM KNVV INTO CORRESPONDING FIELDS OF TABLE GT_KNVV
*   FOR ALL ENTRIES IN GT_KNA1 WHERE KUNNR = GT_KNA1-KUNNR.  "ADDED BY RAM ON 21/11/2014

  SELECT kunnr vkorg kdgrp spart
    FROM knvv INTO CORRESPONDING FIELDS OF TABLE gt_knvv
    FOR ALL ENTRIES IN it_vbrk WHERE kunnr = it_vbrk-kunrg AND spart = it_vbrk-spart AND vkorg = it_vbrk-vkorg AND  kunnr IN so_kunag.  "Added By Govind On 06/12/2014.

  SELECT spras kdgrp ktext
    FROM t151t INTO CORRESPONDING FIELDS OF TABLE gt_t151t
     FOR ALL ENTRIES IN gt_knvv WHERE  kdgrp = gt_knvv-kdgrp AND spras = 'EN' .



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
           FROM kna1 INTO TABLE gt_kna1
           FOR ALL ENTRIES IN it_vbrk
           WHERE kunnr = it_vbrk-kunrg.



    SELECT bland
           bezei
           FROM t005u INTO TABLE gt_t005u
           FOR ALL ENTRIES IN gt_kna1
           WHERE bland = gt_kna1-regio AND spras = 'EN' AND land1 ='IN'.
  ENDIF.



  LOOP AT  gt_knvv INTO wa_knvv.
*  LOOP AT GT_KNA1  INTO WA_KNA1. " Hidded By Govind On 06/12/2014
*    MOVE-CORRESPONDING WA_KNA1 TO WA_FINOVR.  " Hidded By Govind On 06/12/2014
    LOOP AT it_vbrk INTO wa_vbrk WHERE kunrg  = wa_knvv-kunnr AND spart = wa_knvv-spart.
      IF sy-subrc = 0.
*    WA_FINOVR-FKDAT = WA_VBRK-FKDAT.
        wa_finovr-spart = wa_vbrk-spart.
*    WA_FINOVR-VBELN = WA_VBRK-VBELN.
        wa_finovr-fkart = wa_vbrk-fkart.
        wa_finovr-kunag = wa_vbrk-kunag.
        wa_finovr-kunrg = wa_vbrk-kunrg.
        wa_finovr-vkorg = wa_vbrk-vkorg.
        wa_finovr-vtweg = wa_vbrk-vtweg.
        wa_finovr-netwr = wa_finovr-netwr +  wa_vbrk-netwr.
        wa_finovr-sfakn = wa_vbrk-sfakn.
*    wa_finovr-posnr = wa_vbrk-posnr.
        wa_finovr-vkbur = wa_vbrk-vkbur.
*    WA_FINOVR-MWSBK = WA_FINOVR-MWSBK. " Modified By Govind
*    WA_FINOVR-MWSBK = WA_VBRK-MWSBK.
*    WA_FINOVR-INV_VAL = WA_VBRK-NETWR + WA_VBRK-MWSBK.
      ENDIF.

      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
        EXPORTING
          i_date         = wa_vbrk-fkdat
*         I_MONMIT       = 00
          i_periv        = 'K4'
        IMPORTING
          e_buper        = no_month
          e_gjahr        = d_year
        EXCEPTIONS
          input_false    = 1
          t009_notfound  = 2
          t009b_notfound = 3
          OTHERS         = 4.


      LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.
        IF sy-subrc = 0.
*          wa_finovr-fkart =  wa_vbrk-fkart.

          IF wa_finovr-fkart <> 'YLRE' AND wa_finovr-fkart <> 'YIRE' AND wa_finovr-fkart <> 'YFRE' AND wa_finovr-fkart <> 'IVS' AND wa_finovr-fkart <> 'YBRE'.   " added by mani 19.02.2016
            wa_finovr-vnetwr = wa_finovr-vnetwr + wa_vbrp-netwr.
            wa_finovr-fkimg  = wa_finovr-fkimg + wa_vbrp-fkimg.
            wa_finovr-qty_ltr =  wa_finovr-qty_ltr + wa_vbrp-volum.
          ENDIF.

          IF p_cb = 'X'.
            IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.        " added by mani 19.02.2016

              wa_finovr-vnetwr = wa_finovr-vnetwr + wa_vbrp-netwr.
              wa_finovr-fkimg  = wa_finovr-fkimg + wa_vbrp-fkimg.
              wa_finovr-qty_ltr =  wa_finovr-qty_ltr + wa_vbrp-volum.

            ENDIF.
          ENDIF.

          IF p_cb NE 'X'.
            IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.        " added by mani 19.02.2016
              wa_finovr-vnetwr = wa_finovr-vnetwr - wa_vbrp-netwr.
              wa_finovr-fkimg  = wa_finovr-fkimg - wa_vbrp-fkimg.
              wa_finovr-qty_ltr =  ( wa_finovr-qty_ltr + wa_vbrp-volum ).

            ENDIF.
          ENDIF.

        ENDIF.

        IF wa_finovr-fkart <> 'YLRE' AND wa_finovr-fkart <> 'YIRE' AND wa_finovr-fkart <> 'YFRE' AND wa_finovr-fkart <> 'IVS' AND wa_finovr-fkart <> 'YBRE'.   " added by mani 19.02.2016

          IF no_month = '004' .
            wa_finovr-vol0 =  wa_finovr-vol0 +  wa_vbrp-volum .
            wa_finovr-amt0 =  wa_finovr-amt0 + wa_vbrp-netwr.
          ELSEIF no_month = '005'.
            wa_finovr-vol1 = wa_finovr-vol1 + wa_vbrp-volum .
            wa_finovr-amt1 =  wa_finovr-amt1 + wa_vbrp-netwr.
          ELSEIF no_month = '006'.
            wa_finovr-vol2 = wa_finovr-vol2 + wa_vbrp-volum  .
            wa_finovr-amt2 =  wa_finovr-amt2 + wa_vbrp-netwr.
          ELSEIF no_month = '007'.
            wa_finovr-vol3 = wa_finovr-vol3 + wa_vbrp-volum .
            wa_finovr-amt3 =  wa_finovr-amt3 + wa_vbrp-netwr.
          ELSEIF no_month = '008'.
            wa_finovr-vol4 = wa_finovr-vol4 + wa_vbrp-volum.
            wa_finovr-amt4 = wa_finovr-amt4 +  wa_vbrp-netwr.
          ELSEIF no_month = '009'.
            wa_finovr-vol5 = wa_finovr-vol5 + wa_vbrp-volum .
            wa_finovr-amt5 =  wa_finovr-amt5 +   wa_vbrp-netwr.
          ELSEIF no_month = '010'.
            wa_finovr-vol6 = wa_finovr-vol6 + wa_vbrp-volum.
            wa_finovr-amt6 =  wa_finovr-amt6 +  wa_vbrp-netwr.
          ELSEIF no_month = '011'.
            wa_finovr-vol7 = wa_finovr-vol7 + wa_vbrp-volum.
            wa_finovr-amt7 =  wa_finovr-amt7 + wa_vbrp-netwr.
          ELSEIF no_month = '012'.
            wa_finovr-vol8 = wa_finovr-vol8 +  wa_vbrp-volum.
            wa_finovr-amt8 =  wa_finovr-amt8 + wa_vbrp-netwr.
          ELSEIF no_month = '001'.
            wa_finovr-vol9 = wa_finovr-vol9 +  wa_vbrp-volum.
            wa_finovr-amt9 = wa_finovr-amt9 + wa_vbrp-netwr.
          ELSEIF no_month = '002'.
            wa_finovr-vol10 = wa_finovr-vol10 + wa_vbrp-volum .
            wa_finovr-amt10 =  wa_finovr-amt10 + wa_vbrp-netwr.
          ELSEIF no_month = '003'.
            wa_finovr-vol11 = wa_finovr-vol11 + wa_vbrp-volum .
            wa_finovr-amt11 =  wa_finovr-amt11 + wa_vbrp-netwr.

          ENDIF.
        ENDIF.
        IF p_cb = 'X'.
          IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.     " added by mani 19.02.2016


            IF no_month = '004' .
              wa_finovr-vol0 =  wa_finovr-vol0 +  wa_vbrp-volum .
              wa_finovr-amt0 =  wa_finovr-amt0 + wa_vbrp-netwr.
            ELSEIF no_month = '005'.
              wa_finovr-vol1 = wa_finovr-vol1 + wa_vbrp-volum .
              wa_finovr-amt1 =  wa_finovr-amt1 + wa_vbrp-netwr.
            ELSEIF no_month = '006'.
              wa_finovr-vol2 = wa_finovr-vol2 + wa_vbrp-volum  .
              wa_finovr-amt2 =  wa_finovr-amt2 + wa_vbrp-netwr.
            ELSEIF no_month = '007'.
              wa_finovr-vol3 = wa_finovr-vol3 + wa_vbrp-volum .
              wa_finovr-amt3 =  wa_finovr-amt3 + wa_vbrp-netwr.
            ELSEIF no_month = '008'.
              wa_finovr-vol4 = wa_finovr-vol4 + wa_vbrp-volum.
              wa_finovr-amt4 = wa_finovr-amt4 +  wa_vbrp-netwr.
            ELSEIF no_month = '009'.
              wa_finovr-vol5 = wa_finovr-vol5 + wa_vbrp-volum .
              wa_finovr-amt5 =  wa_finovr-amt5 +   wa_vbrp-netwr.
            ELSEIF no_month = '010'.
              wa_finovr-vol6 = wa_finovr-vol6 + wa_vbrp-volum.
              wa_finovr-amt6 =  wa_finovr-amt6 +  wa_vbrp-netwr.
            ELSEIF no_month = '011'.
              wa_finovr-vol7 = wa_finovr-vol7 + wa_vbrp-volum.
              wa_finovr-amt7 =  wa_finovr-amt7 + wa_vbrp-netwr.
            ELSEIF no_month = '012'.
              wa_finovr-vol8 = wa_finovr-vol8 +  wa_vbrp-volum.
              wa_finovr-amt8 =  wa_finovr-amt8 + wa_vbrp-netwr.
            ELSEIF no_month = '001'.
              wa_finovr-vol9 = wa_finovr-vol9 +  wa_vbrp-volum.
              wa_finovr-amt9 = wa_finovr-amt9 + wa_vbrp-netwr.
            ELSEIF no_month = '002'.
              wa_finovr-vol10 = wa_finovr-vol10 + wa_vbrp-volum .
              wa_finovr-amt10 =  wa_finovr-amt10 + wa_vbrp-netwr.
            ELSEIF no_month = '003'.
              wa_finovr-vol11 = wa_finovr-vol11 + wa_vbrp-volum .
              wa_finovr-amt11 =  wa_finovr-amt11 + wa_vbrp-netwr.

            ENDIF.

          ENDIF.
        ENDIF.
        IF p_cb NE 'X'.
          IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.     " added by mani 19.02.2016

            IF no_month = '004' .
              wa_finovr-vol0 =  wa_finovr-vol0 +  wa_vbrp-volum .
              wa_finovr-amt0 =  wa_finovr-amt0 - wa_vbrp-netwr.
            ELSEIF no_month = '005'.
              wa_finovr-vol1 = wa_finovr-vol1 + wa_vbrp-volum .
              wa_finovr-amt1 =  wa_finovr-amt1 - wa_vbrp-netwr.
            ELSEIF no_month = '006'.
              wa_finovr-vol2 = wa_finovr-vol2 + wa_vbrp-volum  .
              wa_finovr-amt2 =  wa_finovr-amt2 - wa_vbrp-netwr.
            ELSEIF no_month = '007'.
              wa_finovr-vol3 = wa_finovr-vol3 + wa_vbrp-volum .
              wa_finovr-amt3 =  wa_finovr-amt3 - wa_vbrp-netwr.
            ELSEIF no_month = '008'.
              wa_finovr-vol4 = wa_finovr-vol4 + wa_vbrp-volum.
              wa_finovr-amt4 = wa_finovr-amt4 -  wa_vbrp-netwr.
            ELSEIF no_month = '009'.
              wa_finovr-vol5 = wa_finovr-vol5 + wa_vbrp-volum .
              wa_finovr-amt5 =  wa_finovr-amt5 -   wa_vbrp-netwr.
            ELSEIF no_month = '010'.
              wa_finovr-vol6 = wa_finovr-vol6 + wa_vbrp-volum.
              wa_finovr-amt6 =  wa_finovr-amt6 -  wa_vbrp-netwr.
            ELSEIF no_month = '011'.
              wa_finovr-vol7 = wa_finovr-vol7 + wa_vbrp-volum.
              wa_finovr-amt7 =  wa_finovr-amt7 - wa_vbrp-netwr.
            ELSEIF no_month = '012'.
              wa_finovr-vol8 = wa_finovr-vol8 +  wa_vbrp-volum.
              wa_finovr-amt8 =  wa_finovr-amt8 - wa_vbrp-netwr.
            ELSEIF no_month = '001'.
              wa_finovr-vol9 = wa_finovr-vol9 +  wa_vbrp-volum.
              wa_finovr-amt9 = wa_finovr-amt9 - wa_vbrp-netwr.
            ELSEIF no_month = '002'.
              wa_finovr-vol10 = wa_finovr-vol10 + wa_vbrp-volum .
              wa_finovr-amt10 =  wa_finovr-amt10 - wa_vbrp-netwr.
            ELSEIF no_month = '003'.
              wa_finovr-vol11 = wa_finovr-vol11 +  wa_vbrp-volum .
              wa_finovr-amt11 =  wa_finovr-amt11 - wa_vbrp-netwr.

            ENDIF.

          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.




    READ TABLE it_mara INTO wa_mara WITH  KEY matnr = wa_vbrp-matnr .

    wa_finovr-m_volum = wa_mara-volum.


    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbrk-vkbur .
    IF sy-subrc = 0.
      wa_finovr-bezei = wa_tvkbt-bezei.
    ENDIF.

    READ TABLE it_tspat INTO wa_tspat WITH KEY spart = wa_vbrk-spart .

    IF sy-subrc = 0.
      wa_finovr-vtext = wa_tspat-vtext.
    ENDIF.

    READ TABLE gt_knvv INTO wa_knvv WITH  KEY kunnr = wa_knvv-kunnr  .   " ADDED BY RAM ON 21/11/2014
    IF sy-subrc = 0 .
      wa_finovr-kdgrp = wa_knvv-kdgrp.
    ENDIF.

    READ TABLE gt_t151t INTO wa_t151t WITH  KEY kdgrp = wa_knvv-kdgrp .
    IF sy-subrc = 0 .
      wa_finovr-ktext = wa_t151t-ktext.
    ENDIF.

    APPEND wa_finovr TO it_finovr.
    CLEAR wa_finovr.
  ENDLOOP.


  LOOP AT it_finovr INTO wa_finovr. " Added By Govind on 06/12/2014
    READ TABLE gt_kna1 INTO wa_kna1 WITH  KEY kunnr = wa_finovr-kunrg.
    wa_finovr-name1 = wa_kna1-name1.


    MODIFY it_finovr FROM wa_finovr TRANSPORTING name1 .
    CLEAR wa_finovr.
  ENDLOOP.

****************************ADDED BY RAM ON 28/10/2015

  DELETE ADJACENT DUPLICATES FROM it_finovr COMPARING vkbur kunrg spart qty_ltr vnetwr vol0 vol1 .

*  SORT IT_FINOVR ASCENDING BY VKBUR .
*  DESCRIBE TABLE IT_FINOVR LINES L.
*
*  DELETE IT_FINOVR INDEX L .

*****************************ENDED BY RAM ON 28/10/2015

  LOOP AT it_finovr INTO wa_finovr .

    IF p_cb NE 'X'.

      IF  wa_finovr-fkart = 'YLRE' OR wa_finovr-fkart = 'YIRE' OR wa_finovr-fkart = 'YFRE' OR wa_finovr-fkart = 'IVS' OR wa_finovr-fkart = 'YBRE'.        " ADDED BY MANI 25.02.2016

        wa_finovr-qty_ltr = - ( wa_finovr-qty_ltr ).
        wa_finovr-vol0 = - ( wa_finovr-vol0 ) .
        wa_finovr-vol1 = - ( wa_finovr-vol1 ) .
        wa_finovr-vol2 = - ( wa_finovr-vol2 ) .
        wa_finovr-vol3 = - ( wa_finovr-vol3 ) .
        wa_finovr-vol4 = - ( wa_finovr-vol4 ) .
        wa_finovr-vol5 = - ( wa_finovr-vol5 ) .
        wa_finovr-vol6 = - ( wa_finovr-vol6 ) .
        wa_finovr-vol7 = - ( wa_finovr-vol7 ) .
        wa_finovr-vol8 = - ( wa_finovr-vol8 ) .
        wa_finovr-vol9 = - ( wa_finovr-vol9 ) .
        wa_finovr-vol10 = - ( wa_finovr-vol10 ) .
        wa_finovr-vol11 = - ( wa_finovr-vol11 ) .

        MODIFY it_finovr FROM wa_finovr TRANSPORTING  qty_ltr vol0 vol1 vol2 vol3 vol4 vol5 vol6 vol7 vol8 vol9 vol10 vol11 .

      ENDIF.
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
  "  commented started by ram on 17/2/16
  LOOP AT it_finovr INTO wa_finovr.
    AUTHORITY-CHECK OBJECT 'ZINVOICE'
    ID 'ZVKBUR' FIELD wa_finovr-vkbur
    ID 'ZSPART' DUMMY
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      wa_finovr-flag = 'x' .
      MODIFY it_finovr FROM wa_finovr TRANSPORTING flag.
      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZINVOICE'
    ID 'ZVKBUR' DUMMY
    ID 'ZSPART' FIELD wa_finovr-spart
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      wa_finovr-flag = 'x' .
      MODIFY it_finovr FROM wa_finovr TRANSPORTING flag.
      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.
    CLEAR: wa_finovr.
  ENDLOOP.

  DELETE it_finovr WHERE flag = 'x'.
  "commented ended by ram on 17/2/16

*   LOOP AT IT_FINOVR INTO WA_FINOVR.
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
*     ENDLOOP.
*
*     DELETE IT_FINOVR WHERE FLAG = 'X' .

ENDFORM.                    " AUTHCHECK_OVERVIEW



*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_overview .

  wa_fieldcat-fieldname   = 'VKBUR'.
  wa_fieldcat-seltext_m   = 'Sales Office'.
  wa_fieldcat-col_pos     = 2.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'VKBUR'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

*  WA_SORT-FIELDNAME = 'VKBUR'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.
*
*   WA_SORT-FIELDNAME = 'VKBUR'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

  wa_fieldcat-fieldname   = 'BEZEI'.
  wa_fieldcat-seltext_m   = 'Sales Desc.'.
  wa_fieldcat-col_pos     = 3.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'BEZEI'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'VKORG'.
  wa_fieldcat-seltext_m   = 'Sales Organization'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'VKORG'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'SPART'.
  wa_fieldcat-seltext_m   = 'Sales Division'.
  wa_fieldcat-col_pos     = 6.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'SPART'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

*  WA_FIELDCAT-FIELDNAME   = 'FKART'.
*  WA_FIELDCAT-SELTEXT_M   = 'BILLING TYPE'.
*  WA_FIELDCAT-COL_POS     = 10.
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*

  wa_fieldcat-fieldname   = 'VTEXT'.
  wa_fieldcat-seltext_m   = 'Sales Division Desc.'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'VTEXT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'KDGRP'.
  wa_fieldcat-seltext_m   = 'Customer Group Code'.
  wa_fieldcat-col_pos     = 8.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'KDGRP'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'KTEXT'.
  wa_fieldcat-seltext_m   = 'Customer Group Desc.'.
  wa_fieldcat-col_pos     = 9.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'KTEXT'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'KUNRG'.
  wa_fieldcat-seltext_m   = 'Customer Code'.
  wa_fieldcat-col_pos     = 11.
  wa_fieldcat-cfieldname   = 'KUNRG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_sort-fieldname = 'KUNRG'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'NAME1'.
  wa_fieldcat-seltext_m   = 'Customer Name'.
  wa_fieldcat-col_pos     = 12.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_sort-fieldname = 'NAME1'.
  wa_sort-up = 'X'.
  wa_sort-group = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_fieldcat-fieldname   = 'FKART'.                                                            " ADDED BY MANI 23.02.2016
  wa_fieldcat-seltext_m   = 'Billing Type'.
  wa_fieldcat-col_pos     = 13.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'FKIMG'.
  wa_fieldcat-seltext_m   = 'Total Inv.Qty'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'QTY_LTR'.
  wa_fieldcat-seltext_m   = 'Total in Liter/Kgs'.
  wa_fieldcat-col_pos     = 15.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VNETWR'.
  wa_fieldcat-seltext_m   = 'Net Value'.
  wa_fieldcat-col_pos     = 16.
  wa_fieldcat-do_sum      = 'X'.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL0'.
  wa_fieldcat-seltext_m   = 'April Volume'.
  wa_fieldcat-col_pos     = 17.
  wa_fieldcat-no_zero     = 'X'.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT0'.
  wa_fieldcat-seltext_m   = 'April Value'.
  wa_fieldcat-col_pos     = 28.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL1'.
  wa_fieldcat-seltext_m   = 'May Volume'.
  wa_fieldcat-col_pos     = 18.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
  wa_fieldcat-no_zero     = 'X'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT1'.
  wa_fieldcat-seltext_m   = 'May Value'.
  wa_fieldcat-col_pos     = 29.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL2'.
  wa_fieldcat-seltext_m   = 'June Volume'.
  wa_fieldcat-col_pos     = 19.
  wa_fieldcat-no_zero     = 'X'.
*  WA_FIELDCAT-CFIELDNAME   = 'KUNAG'.
  wa_fieldcat-ctabname    = 'VBRK'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT2'.
  wa_fieldcat-seltext_m   = 'June Value'.
  wa_fieldcat-col_pos     = 30.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'AMT3'.
  wa_fieldcat-seltext_m   = 'July Value'.
  wa_fieldcat-col_pos     = 31.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL3'.
  wa_fieldcat-seltext_m   = 'July Volume'.
  wa_fieldcat-col_pos     = 20.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL4'.  "Changes on 07/08/2014 my savariar s VOL4
  wa_fieldcat-seltext_m   = 'Augest Volume'.
  wa_fieldcat-col_pos     = 21.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT4'.  "Changes on 07/08/2014 my savariar s AMT4
  wa_fieldcat-seltext_m   = 'Augest Value'.
  wa_fieldcat-col_pos     = 32.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VOL5'.
  wa_fieldcat-seltext_m   = 'September Volume'.
  wa_fieldcat-col_pos     = 22.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT5'.
  wa_fieldcat-seltext_m   = 'September Value'.
  wa_fieldcat-col_pos     = 33.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
  wa_fieldcat-fieldname   = 'VOL6'.
  wa_fieldcat-seltext_m   = 'October Volume'.
  wa_fieldcat-col_pos     = 23.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT6'.
  wa_fieldcat-seltext_m   = 'October Value'.
  wa_fieldcat-col_pos     = 34.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VOL7'.
  wa_fieldcat-seltext_m   = 'November Volume'.
  wa_fieldcat-col_pos     = 24.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT7'.
  wa_fieldcat-seltext_m   = 'November  Value'.
  wa_fieldcat-col_pos     = 35.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL8'.
  wa_fieldcat-seltext_m   = 'December Volume'.
  wa_fieldcat-col_pos     = 25.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT8'.
  wa_fieldcat-seltext_m   = 'December  Value'.
  wa_fieldcat-col_pos     = 36.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname   = 'VOL9'.
  wa_fieldcat-seltext_m   = 'January Volume'.
  wa_fieldcat-col_pos     = 26.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT9'.
  wa_fieldcat-seltext_m   = 'January Value'.
  wa_fieldcat-col_pos     = 37.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL10'.
  wa_fieldcat-seltext_m   = 'Febrary Volume'.
  wa_fieldcat-col_pos     = 27.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT10'.
  wa_fieldcat-seltext_m   = 'Febrary Value'.
  wa_fieldcat-col_pos     = 38.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'VOL11'.
  wa_fieldcat-seltext_m   = 'March Volume'.
  wa_fieldcat-col_pos     = 28.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'AMT11'.
  wa_fieldcat-seltext_m   = 'March Value'.
  wa_fieldcat-col_pos     = 39.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

*&-----Begin of Keerthi CLSS on 24-08-2016-----*
  wa_fieldcat-fieldname   = 'SO_NO'.
  wa_fieldcat-seltext_m   = 'S.O NO'.
  wa_fieldcat-col_pos     = 40.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SO_NAME'.
  wa_fieldcat-seltext_m   = 'S.O Name'.
  wa_fieldcat-col_pos     = 41.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NO'.
  wa_fieldcat-seltext_m   = 'S.M NO'.
  wa_fieldcat-col_pos     = 42.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname   = 'SM_NAME'.
  wa_fieldcat-seltext_m   = 'S.M Name'.
  wa_fieldcat-col_pos     = 43.
  wa_fieldcat-no_zero     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.
*&------End of Keerthi CLSS on 24-08-2016------*

*  WA_SORT-FIELDNAME = 'VKBUR'.
*  WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.


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


  layout-colwidth_optimize = 'X'.                            "Added by Savariar S as on 21/10/2014.
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

*&-------Begin of Keerthi CLSS on 26-10-2016--------*

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
                                                                       AND parvw    =  'L5'.", 'L3').   "L5 = SO      L3 = AS

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
      wa_finovr-so_no = wa_knvp-pernr.
    ENDIF.
    READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_knvp-pernr.
    IF sy-subrc = 0.
      wa_finovr-so_name = wa_pa0001-sname.
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

    MODIFY it_finovr FROM wa_finovr TRANSPORTING so_no so_name sm_no sm_name.

    CLEAR: wa_finovr, wa_knvp, wa_pa0001, wa_knvp1, wa_pa0001_1.
  ENDLOOP.
*&--------End of Keerthi CLSS on 26-10-2016---------*

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
          i_callback_program                = sy-repid
*      I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS_GET'
*      I_CALLBACK_USER_COMMAND           = 'MY_USER_COMMAND'
        i_callback_top_of_page            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
       is_layout                          = layout
          it_fieldcat                       = it_fieldcat
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
        it_sort                           = it_sort
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
        TABLES
          t_outtab                          = it_finovr
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


    ENDFORM.                    "BUILD_ALV_OVERVIEW


*---------------------------------------------------
*&      Form  ALV_CATALOG_HEADER
*&-------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------
FORM alv_catalog_header.
  DATA : lit_header TYPE  slis_t_listheader,
       ls_line TYPE slis_listheader.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-key = ' '.
  ls_line-info = 'Customer Sales Month Wise Report' .
  APPEND ls_line TO lit_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lit_header
      i_logo             = 'ZLOGO'.
*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BUDAT_MKPF
*    IMPORTING
*      OUTPUT = LV_BUDAT_MKPF.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
