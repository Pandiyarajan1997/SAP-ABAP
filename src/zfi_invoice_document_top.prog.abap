*&---------------------------------------------------------------------*
*& Include          ZFI_INVOICE_DOCUMENT_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel,
         vendor     TYPE lifnr,
         inv_date   TYPE bldat,
         refdoc     TYPE xblnr1,
         budat      TYPE budat,
         amount     TYPE wrbtr,
         taxcode    TYPE mwskz,
         bplace     TYPE bupla,
         text       TYPE sgtxt,
         glacc      TYPE saknr,
         costcenter TYPE kostl,
         plant      TYPE werks_d,
         tds(03)        TYPE c,
       END OF ty_excel.
TYPES: BEGIN OF ty_withtx,
         lifnr     TYPE lfbw-lifnr,
         witht     TYPE lfbw-witht,
         wt_withcd TYPE lfbw-wt_withcd,
         qproz     TYPE t059z-qproz,
         qsatz     TYPE t059z-qsatz,
         konth     TYPE t030-konth,
       END OF ty_withtx.
TYPES: BEGIN OF ty_konp,
         kschl TYPE kschl,
         kbetr TYPE kbetr_kond,
         mwsk1 TYPE mwskz,
       END OF ty_konp.
TYPES: BEGIN OF ty_csks,
         kostl TYPE kostl,
       END OF ty_csks.
TYPES: BEGIN OF ty_final,
         vendor     TYPE lifnr,
         venname    TYPE name1_gp,
         bukrs      TYPE bukrs,
         gjahr      TYPE gjahr,
         erdat      TYPE erdat,
         budat      TYPE budat,
         xblnr      TYPE xblnr1,
         blart      TYPE blart,
         bplace     TYPE bupla,
         text       TYPE sgtxt,
         saknr      TYPE saknr,
         gltxt      TYPE txt50,
         tax        TYPE mwskz,
         plant      TYPE werks_d,
         costcntr   TYPE kostl,
*         fund        TYPE char07,
         vend_amnt  TYPE wrbtr,
         net_amnt   TYPE wrbtr,
         icon       TYPE kschl,
         icond      TYPE ktosl,
         igst       TYPE wrbtr,
         irate      TYPE msatz_f05l,
         igl        TYPE saknr,
         scon       TYPE kschl,
         scond      TYPE ktosl,
         sgst       TYPE wrbtr,
         srate      TYPE msatz_f05l,
         sgl        TYPE saknr,
         ccon       TYPE kschl,
         ccond      TYPE ktosl,
         cgst       TYPE wrbtr,
         crate      TYPE msatz_f05l,
         cgl        TYPE saknr,
         ventax     TYPE witht,
         ventax1    TYPE wt_withcd,
         tds_gl     TYPE saknr,
         tds        TYPE wrbtr,
         tds_amt    TYPE wrbtr,
         flag(01)   TYPE c,
         msgtyp(01) TYPE c,
         docno      TYPE belnr_d,
         message    TYPE string,
       END OF ty_final.

DATA: gt_excel TYPE TABLE OF ty_excel.
DATA: gt_excel1 TYPE TABLE OF ty_excel.
DATA: gt_final TYPE TABLE OF ty_final,
      gs_final TYPE ty_final.
DATA: gt_konp TYPE TABLE OF ty_konp.
DATA: gt_kostl TYPE TABLE OF ty_csks.
DATA: gt_type  TYPE truxs_t_text_data.
DATA: gt_withtax TYPE TABLE OF ty_withtx.
DATA: gt_glget TYPE STANDARD TABLE OF j_1it030k.
DATA: gt_gldes TYPE STANDARD TABLE OF skat.
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
DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
      lv_month   TYPE bapi0002_4-fiscal_period,
      l_return   TYPE bapireturn1.
DATA: lv_fund TYPE char10.
DATA: lv_amount TYPE wrbtr.
DATA:e_fwnav TYPE bset-fwste,
     e_fwnvv TYPE bset-fwste,
     e_fwste TYPE bset-fwste,
     e_fwast TYPE bset-fwste.
DATA: lt_caltax TYPE TABLE OF rtax1u15.
DATA: return TYPE bapiret2_t.
.
TYPE-POOLS: esp1.
DATA: lt_tab TYPE esp1_message_tab_type.
DATA: ls_tab TYPE esp1_message_wa_type.
DATA: lv_objtyp TYPE bapiache09-obj_type.
DATA: lv_objkey TYPE bapiache09-obj_key.
DATA: lv_objsys TYPE bapiache09-obj_sys.
DATA: lv_gross TYPE wrbtr,
      lv_net   TYPE wrbtr.
DATA: lt_payable   TYPE TABLE OF bapiacap09,
      lt_accountgl TYPE TABLE OF bapiacgl09,
      lt_tax       TYPE TABLE OF bapiactx09,
      ls_tax       TYPE bapiactx09,
      lt_currency  TYPE TABLE OF bapiaccr09.
DATA: bapiret2_t TYPE bapiret2_t.
DATA: lt_withtax TYPE TABLE OF  bapiacwt09,
      ls_withtax TYPE bapiacwt09.
DATA: gv_tds_famnt TYPE wrbtr.
FIELD-SYMBOLS <fs_final> TYPE ty_final.
