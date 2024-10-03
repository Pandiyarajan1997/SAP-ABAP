*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_TOP
*&---------------------------------------------------------------------*
*TABLES:vbrk.

TYPES: BEGIN OF ty_einvoice,
         status     TYPE icon_d,
         vbeln      TYPE vbeln_vf,
         fkart      TYPE fkart,
         fkdat      TYPE fkdat,
         bukrs      TYPE bukrs,
         erdat      TYPE erdat,
         distrb     TYPE kunnr,
         distrb_t   TYPE name1_gp,
         kunag      TYPE kunag,
         kunag_t    TYPE name1_gp,
         werks      TYPE werks_d,
         irn        TYPE j_1ig_irn,
         ack_no     TYPE j_1ig_ack_no,
         ack_date   TYPE j_1ig_ack_date,
         irn_status TYPE j_1ig_irn_status,
         ebillno    TYPE j_1ig_ebillno,
         celltab    TYPE lvc_t_styl,
         coltab     TYPE lvc_t_scol,
         can_status TYPE char30,
       END OF ty_einvoice.

TYPES: BEGIN OF ty_vbrk,
         vbeln      TYPE vbeln_vf,
         fkart      TYPE fkart,
         fkdat      TYPE fkdat,
         bukrs      TYPE bukrs,
         erdat      TYPE erdat,
         kunag      TYPE kunag,
         distrb     TYPE kunnr,
         irn        TYPE  j_1ig_irn,
         irn_status TYPE j_1ig_irn_status,
         ack_no     TYPE j_1ig_ack_no,
         ack_date   TYPE j_1ig_ack_date,
         ebillno    TYPE j_1ig_ebillno,
         vdfmdate   TYPE j_1ig_vdfmdate,
         vdfmtime   TYPE j_1ig_vdfmtime,
         status     TYPE j_1ig_stat,
       END OF ty_vbrk.

DATA : l_tab_vbrk TYPE TABLE OF ty_vbrk,
       l_wrk_vbrk TYPE ty_vbrk.
DATA: lt_einv_final TYPE TABLE OF ty_einvoice,
      lw_einv_final TYPE ty_einvoice.

DATA :gs_exclude TYPE ui_func.

DATA : gv_custom_container TYPE REF TO cl_gui_custom_container,
       gs_variant          TYPE disvariant,
*       gv_event_receiver   TYPE REF TO lcl_event_receiver,
       gs_fieldcat         TYPE lvc_s_fcat,
       gt_fieldcat         TYPE lvc_t_fcat,
       gt_exclude          TYPE ui_functions,
       gv_pointer          TYPE REF TO data,
       gv_container        TYPE scrfname VALUE 'CONT_0100',
       gv_grid             TYPE REF TO cl_gui_alv_grid,
       gs_layout           TYPE lvc_s_layo.
DATA save_ok TYPE sy-ucomm.
DATA : ok_code TYPE sy-ucomm.
FIELD-SYMBOLS:
  <fcat>    TYPE lvc_t_fcat,
  <celltab> TYPE lvc_t_styl.
