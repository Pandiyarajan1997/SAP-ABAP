*&---------------------------------------------------------------------*
*& Include          ZFB50_GL_LINEITEM_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_input,
         sno            TYPE i,
         refdoc         TYPE xblnr1,
         pdate          TYPE budat,
         key            TYPE newbs,
         gl_account     TYPE saknr,
         gl_account_txt TYPE txt50_skat,
         amt            TYPE wrbtr,
         item_txt       TYPE sgtxt,
         costcenter     TYPE kostl,
         plant          TYPE werks_d,
       END OF ty_input.
TYPES: BEGIN OF ty_alv,
         sno     TYPE i,
         type    TYPE bapi_mtype,
         docno   TYPE belnr_d,
         d_amt    TYPE wrbtr,
         c_amt    TYPE wrbtr,
         message TYPE string,
       END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv.
DATA: lv_fund TYPE bapiacgl09-fund.
DATA: lt_type  TYPE truxs_t_text_data.
DATA: lt_excel TYPE TABLE OF ty_input.
DATA: lt_glaccount TYPE TABLE OF bapiacgl09.
DATA: lt_curramnt TYPE TABLE OF bapiaccr09.
DATA: lv_amount  TYPE char25,
      lv_amount1 TYPE char25.
DATA: lv_belnr TYPE belnr_d,
      lv_msg   TYPE string.
DATA: lv_objtyp TYPE bapiache09-obj_type.
DATA: lv_objkey TYPE bapiache09-obj_key.
DATA: lv_objsys TYPE bapiache09-obj_sys.
DATA: return TYPE STANDARD TABLE OF bapiret2
                                     WITH HEADER LINE.
DATA: lv_difference TYPE wrbtr,
      lv_debit      TYPE wrbtr,
      lv_credit     TYPE wrbtr.
DATA: lr_excel_structure      TYPE REF TO data,
      lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
      lo_table_row_descripter TYPE REF TO cl_abap_structdescr,
      lv_content              TYPE xstring,
      lt_binary_tab           TYPE TABLE OF sdokcntasc,
      lv_filename1            TYPE string,
      lv_path                 TYPE string,
      lv_length               TYPE i,
      lv_fullpath             TYPE string.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA: ls_header TYPE bapiache09.
TYPE-POOLS: esp1.

DATA: lt_tab TYPE esp1_message_tab_type.

DATA: ls_tab TYPE esp1_message_wa_type.
DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
      lv_month   TYPE bapi0002_4-fiscal_period,
      l_return   TYPE bapireturn1.
DATA: gt_skat TYPE STANDARD TABLE OF skat.
