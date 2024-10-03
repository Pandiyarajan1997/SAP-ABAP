*&---------------------------------------------------------------------*
*& Include          ZKNVP_UPLOAD_TOP
*&---------------------------------------------------------------------*

TABLES: knvp.

TYPES: BEGIN OF ty_upload,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
         spart TYPE spart,
         parvw TYPE parvw,
         kunn2 TYPE kunnr,
         lifnr TYPE lifnr,
         pernr TYPE pernr_d,
       END OF ty_upload.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
       END OF ty_kna1.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lifnr,
         name1 TYPE name1_gp,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_pa0000,
         pernr TYPE pernr_d,
         stat2 TYPE stat2,
       END OF ty_pa0000.

TYPES: BEGIN OF ty_pa0002,
         pernr TYPE pernr_d,
         vorna TYPE pad_vorna,
       END OF ty_pa0002.

TYPES: BEGIN OF ty_tvko,
         vkorg TYPE vkorg,
       END OF ty_tvko.

TYPES: BEGIN OF ty_tvkov,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
       END OF ty_tvkov.

TYPES: BEGIN OF ty_tvkos,
         vkorg TYPE vkorg,
         spart TYPE spart,
       END OF ty_tvkos.

TYPES: BEGIN OF ty_display,
         kunnr        TYPE kunnr,
         name1        TYPE name1_gp,
         vkorg        TYPE vkorg,
         vtweg        TYPE vtweg,
         spart        TYPE spart,
         parvw        TYPE parvw,
         kunn2        TYPE kunnr,
         dist_name    TYPE name1_gp,
         lifnr        TYPE lifnr,
         venname      TYPE name1_gp,
         pernr        TYPE pernr_d,
         empname      TYPE name1_gp,
         message(150) TYPE c,
       END OF ty_display.

* ----------------------------------------- *
*  Internal Table and Work Area Declaration *
DATA: lt_upload TYPE TABLE OF ty_upload,
      ls_upload TYPE ty_upload.

DATA: lt_type      TYPE truxs_t_text_data.

DATA: lt_knvp TYPE STANDARD TABLE OF knvp,
      ls_knvp TYPE knvp.

DATA: lt_pa0000 TYPE TABLE OF ty_pa0000,
      ls_pa0000 TYPE ty_pa0000.

DATA: lt_pa0002 TYPE TABLE OF ty_pa0002,
      ls_pa0002 TYPE ty_pa0002.

DATA: lt_kna1 TYPE TABLE OF ty_kna1,
      ls_kna1 TYPE ty_kna1.

DATA: lt_lfa1 TYPE TABLE OF ty_lfa1,
      ls_lfa1 TYPE ty_lfa1.

DATA: lt_tpaum TYPE STANDARD TABLE OF tpaum,
      ls_tpaum TYPE tpaum.

DATA: lt_tvko TYPE TABLE OF ty_tvko,
      ls_tvko TYPE ty_tvko.

DATA: lt_tvkov TYPE TABLE OF ty_tvkov,
      ls_tvkov TYPE ty_tvkov.

DATA: lt_tvkos TYPE TABLE OF ty_tvkos,
      ls_tvkos TYPE ty_tvkos.

DATA: lt_fm_knvp TYPE STANDARD TABLE OF fknvp,
      ls_fm_knvp TYPE fknvp.

DATA: lt_disp TYPE TABLE OF ty_display,
      ls_disp TYPE ty_display.

DATA: lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      ls_layout TYPE lvc_s_layo.

DATA: lo_grid TYPE REF TO cl_salv_table.
*----------------------------------------------- *
