*&---------------------------------------------------------------------*
*& Include          ZSD_REP_INVOICECOUNT_COGS_TOP
*&---------------------------------------------------------------------*
*** variable for calculations ***
DATA: lv_bukrs              TYPE vbrk-bukrs,
      w_aux_fkdat           TYPE vbrk-fkdat,
      w_fkart               TYPE vbrk-fkart,
      w_vbeln               TYPE vbrk-vbeln,
      w_kunag               TYPE vbrk-kunag,
      w_matnr               TYPE vbrp-matnr,
      w_charg               TYPE lips-charg,
      w_inv_vkbur           TYPE vbrp-vkbur,
      w_spart               TYPE vbrk-spart,

      counter               TYPE i VALUE '0',
      gt_list               TYPE vrm_values,
      gwa_list              TYPE vrm_value,
      gt_values             TYPE TABLE OF dynpread,
      gwa_values            TYPE dynpread,
      gv_selected_value(10) TYPE c,
      it_fieldcat           TYPE TABLE OF slis_fieldcat_alv,
      wa_fieldcat           LIKE LINE OF it_fieldcat,
      it_sort               TYPE slis_t_sortinfo_alv,
      wa_sort               LIKE LINE OF it_sort,
      g_sent_to_all         TYPE sonv-flag,
      g_tab_lines           TYPE i,


      w_kunnr               TYPE knc1-kunnr,
      w_bukrs               TYPE knc1-bukrs,
      w_vkbur               TYPE knvv-vkbur,
*      W_GJAHR TYPE KNC1-GJAHR,
      w_re_period           TYPE bsid-monat,

      w_s_matnr             TYPE mard-matnr,
      w_s_bukrs             TYPE t001-bukrs,
      w_s_hkont             TYPE bseg-hkont,
      w_s_werks             TYPE t001w-werks,
      w_s_lgort             TYPE t001l-lgort,
      w_s_charg             TYPE mchb-charg,
      w_s_bwtar             TYPE mbew-bwtar,
      w_s_bwart             TYPE mseg-bwart,
      w_s_datum             TYPE mkpf-budat.
