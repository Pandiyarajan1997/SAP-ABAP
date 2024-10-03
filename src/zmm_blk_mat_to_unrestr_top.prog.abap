*&---------------------------------------------------------------------*
*& Include zmm_blk_mat_to_unrestr_top
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_file_mm,
        matnr           TYPE matnr, "material
        plant           TYPE werks_d, "plant
        stor_loc        TYPE lgort_d, "Storage Location
        Batch           TYPE charg_d, "Batch.
        sled            TYPE char10, "Shelf life expiration date,
        qty             TYPE menge_d, "quantity
        mat_slip        TYPE mtsnr1, "material slip
        doc_header_text TYPE bktxt, "Document Header Text
        uom             TYPE meins,
        post_date       TYPE budat,
        doc_date        TYPE bldat,
        err_flag        TYPE flag,
      END OF ty_file_mm,
      BEGIN OF ty_file_tp,
        matnr    TYPE matnr, "material
        plant    TYPE werks_d, "plant
        stor_loc TYPE lgort_d, "Storage Location
        Batch    TYPE charg_d, "Batch.
        qty      TYPE menge_d, "quantity
        uom      TYPE meins,
      END OF ty_file_tp,
      BEGIN OF ty_logs,
        msgtype TYPE icon_d,
        mblnr   TYPE mblnr,
        mjahr   TYPE mjahr,
        msg     TYPE  bapiret1-message,
      END OF ty_logs,
      BEGIN OF ty_sled_logs,
        check     TYPE xfeld,
        msgtype   TYPE icon_d,
        matnr     TYPE matnr,
        plant     TYPE werks_d,
        lgort     TYPE lgort_d,
        charg     TYPE charg_d,
        curr_sled TYPE vfdat,
        act_sled  TYPE vfdat,
        msg       TYPE bapiret1-message,
      END OF ty_sled_logs,
      BEGIN OF ty_tp343,
        check type xfeld,
        msgtype TYPE icon_d,
        matnr   TYPE matnr,
        plant   TYPE werks_d,
        lgort   TYPE lgort_d,
        charg   TYPE charg_d,
        qty     TYPE menge_d,
        msg     TYPE bapiret1-message,
      END OF ty_tp343,
      BEGIN OF ty_err_msc2n,
        matnr    TYPE matnr, "material
        plant    TYPE werks_d, "plant
        stor_loc TYPE lgort_d, "Storage Location
        Batch    TYPE charg_d, "Batch
      END OF ty_err_msc2n.

DATA: gt_upload    TYPE STANDARD TABLE OF ty_file_mm,
      lt_upload    TYPE STANDARD TABLE OF ty_file_tp,
      gt_logs      TYPE STANDARD TABLE OF ty_logs,
      gt_sled_logs TYPE STANDARD TABLE OF ty_sled_logs,
      lt_sled_logs TYPE STANDARD TABLE OF ty_sled_logs,
      gt_trpost    TYPE STANDARD TABLE OF ty_tp343,
      gt_err_msc2n TYPE STANDARD TABLE OF ty_err_msc2n,
      gt_log_table TYPE STANDARD TABLE OF zmm_sled_343_log,
      gt_log_itm   TYPE STANDARD TABLE OF zmm_sled_343_log,
      gt_return    TYPE TABLE OF bapiret2,
      gs_return    TYPE bapiret2,
      gt_gsmvt_itm TYPE TABLE OF bapi2017_gm_item_create.
DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
      lo_gr_functions TYPE REF TO cl_salv_functions_list.

DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
      lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
      lo_content     TYPE REF TO cl_salv_form_element,
      lv_title       TYPE string,
      lv_rows        TYPE string.

DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
      lv_key    TYPE salv_s_layout_key.

DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
      lo_columns    TYPE REF TO cl_salv_columns_table,
      lo_column     TYPE REF TO cl_salv_column_table,
      lo_col_icon  TYPE REF TO cl_salv_column,
      lo_icon      TYPE REF TO cl_salv_column_table,
      lt_icon       TYPE TABLE OF icon,
      ls_icon       TYPE icon.

DATA lt_rows   TYPE salv_t_row.
DATA ls_row TYPE i .
DATA: lv_refno TYPE numc10.
DATA(lt_msg_tab) = VALUE esp1_message_tab_type( ).
DATA(wa_msg_tab) = VALUE esp1_message_wa_type(  ).
DATA(gmsvt_ret) = VALUE bapi2017_gm_head_ret(  ).
DATA(matnr) = VALUE matnr18(  ).
DATA(gt_batch_details) = VALUE bapibatchatt(  ).
DATA(mblpo) = VALUE mblpo(  ).
DATA(iv_sled) = VALUE vfdat(  ).

"For Excel Format Download
DATA: lr_excel_structure      TYPE REF TO data,
      lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
      lo_table_row_descripter TYPE REF TO cl_abap_structdescr,
      lv_content              TYPE xstring,
      lt_binary_tab           TYPE TABLE OF sdokcntasc,
      lv_filename1            TYPE string,
      lv_path                 TYPE string,
      lv_length               TYPE i,
      lv_fullpath             TYPE string,
      dummy                   TYPE i,
      flag type xfeld.
