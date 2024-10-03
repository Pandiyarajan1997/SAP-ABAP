*=======================================================================
*  Author                    : Ramakrishnan
*  Date                      : 01.05.2024
*  Requester Name            : Ramakrishnan
*  Business Logic            : Create missed Auto CD for subdealers
*  Released on Date          :
*=======================================================================
REPORT zdms_crautocd_miss.

TYPES: BEGIN OF alv,
         checkbox  TYPE char1,
         bukrs     TYPE bukrs,
         refid     TYPE zref_id,
         distr     TYPE zdist,
         disname   TYPE name1,
         kunnr     TYPE z_deal_code,
         retname   TYPE name1,
         acdno     TYPE belnr_d,
         acdyr     TYPE gjahr,
         dgdoc     TYPE belnr_d,
         dgdyr     TYPE gjahr,
         invno     TYPE vbeln,
         inamt     TYPE wrbtr,
         totamt    TYPE dmbtr,
         xblnr     TYPE xblnr1,
         indat     TYPE budat,
         paydt     TYPE betdt,
         payamt    TYPE wrbtr,
         dgamt     TYPE wrbtr,
         hkont     TYPE hkont,
         pdays     TYPE zdmsdays,
         dgper     TYPE zperzt,
         crdat     TYPE crdat,
         crnam     TYPE crnam,
         msgtyp    TYPE bapi_mtype,
         message   TYPE bapi_msg,
         crflag    TYPE char1,
         gsber     TYPE gsber,
         invflag   TYPE char1,
         postdate  TYPE budat,
         new_docno TYPE bktxt,
       END OF alv.

TYPES : BEGIN OF ty_excel,
          distributor TYPE zdist,
          retailer    TYPE kunnr,
          ref_no      TYPE vbeln,
          percentage  TYPE char3,
          payref      TYPE xblnr,
          invflag     TYPE char1,
          postdate    TYPE datum,
        END OF ty_excel.


DATA : gt_alv  TYPE TABLE OF alv.
DATA : lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA : gv_date  TYPE budat.
DATA : gv_bukrs TYPE bukrs.
DATA : gv_kunnr TYPE kna1-kunnr.
DATA : gv_doc   TYPE bkpf-blart.

DATA: ls_excel TYPE ty_excel.


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND u1 DEFAULT 'X', "upload excel dr/cr note  Posting Process
               r4 RADIOBUTTON GROUP g1.          "zdms_deal_dr_cr table display
SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : so_kunnr FOR gv_kunnr MODIF ID a,        "distributor
                   so_retai FOR gv_kunnr MODIF ID a.        "retailer
*                   so_pdate FOR sy-datum MODIF ID a,        "posting date
*                   so_docty FOR gv_doc   MODIF ID a.        "doc type
*                   so_statu FOR gv_statu MODIF ID a NO INTERVALS.        "posting date
  PARAMETERS : p_fname TYPE rlgrap-filename MODIF ID a.     "for excel file upload
  PARAMETERS : p_chk3 AS CHECKBOX MODIF ID bl3.
SELECTION-SCREEN : END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.
***********set the default values*******
INITIALIZATION.
  gv_bukrs = 'DMS1'.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen_adj.
**********Make the file path as mandatory********
AT SELECTION-SCREEN.

  IF r1 = 'X' AND p_fname IS INITIAL AND sy-ucomm = 'ONLI'.
    MESSAGE : 'Please Fill the File Path' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  IF r1 = abap_true.                    "Display the posting data
    PERFORM : process.
    PERFORM : alv_display.
  ELSEIF r4 = abap_true.                "zdms_deal_dr_cr table display
    PERFORM : display.

  ENDIF.
*&---------------------------------------------------------------------*
*& Form process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process .

  TYPES: BEGIN OF ty_vbrp,
           vbeln TYPE vbrk-vbeln,
           fkart TYPE vbrk-fkart,
           vkorg TYPE vbrk-vkorg,
           fkdat TYPE vbrk-fkdat,
           belnr TYPE vbrk-belnr,
           gjahr TYPE vbrk-gjahr,
           netwr TYPE vbrk-netwr,
           mwsbk TYPE vbrk-mwsbk,
           kunag TYPE vbrk-kunag,
           fksto TYPE vbrk-fksto,
           vkbur TYPE vbrp-vkbur,
         END OF ty_vbrp.

  DATA : lv_fisyear TYPE bapi0002_4-fiscal_year,
         lv_month   TYPE bapi0002_4-fiscal_period,
         l_return   TYPE bapireturn1.

  DATA: lt_vbrp TYPE TABLE OF ty_vbrp,
        ls_vbrp TYPE ty_vbrp.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         ls_excel TYPE ty_excel,
         lt_raw   TYPE truxs_t_text_data.

  DATA: t_file TYPE STANDARD TABLE OF alsmex_tabline.



  CONSTANTS :c_begin_col TYPE i VALUE '1',
             c_begin_row TYPE i VALUE '1',
             c_end_col   TYPE i VALUE '7',
             c_end_row   TYPE i VALUE '65535'.

  DATA: lt_filetable TYPE filetable,
        lx_filetable TYPE file_table,
        lv_rc        TYPE i.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = c_begin_col
      i_begin_row             = c_begin_row
      i_end_col               = c_end_col
      i_end_row               = c_end_row
    TABLES
      intern                  = t_file
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc = 0.

* Implement suitable error handling here
    REFRESH lt_excel.
    LOOP AT t_file ASSIGNING FIELD-SYMBOL(<fs_file>).
      IF <fs_file>-row = '0001'.  "header roe
        CONTINUE.
      ENDIF.
      CASE <fs_file>-col.
        WHEN '0001'.
          ls_excel-distributor = <fs_file>-value.
        WHEN '0002'.
          ls_excel-retailer = <fs_file>-value.
        WHEN '0003'.
          ls_excel-ref_no = <fs_file>-value.
        WHEN '0004'.
          ls_excel-percentage = <fs_file>-value.
        WHEN '0005'.
          ls_excel-payref = <fs_file>-value.
        WHEN '0006'.
          ls_excel-invflag = <fs_file>-value.
        WHEN '0007'.
          ls_excel-postdate = <fs_file>-value.
        WHEN OTHERS.
      ENDCASE.

      AT END OF row.
        APPEND ls_excel TO lt_excel.
        CLEAR ls_excel.
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF lt_excel IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_excel BY distributor retailer ref_no.

    DATA: lv_tabix TYPE sy-tabix.

    LOOP AT lt_excel INTO ls_excel.
      CLEAR lv_tabix.
      lv_tabix = sy-tabix.
      ls_excel-distributor  = |{ ls_excel-distributor ALPHA = IN }|.
      ls_excel-retailer     = |{ ls_excel-retailer    ALPHA = IN }|.
*      ls_excel-gl_acnt      = |{ ls_excel-gl_acnt     ALPHA = IN }|.
      MODIFY lt_excel FROM ls_excel INDEX lv_tabix.
    ENDLOOP.

* select from cust block table for distributor data
    DATA: lt_blk TYPE TABLE OF zcust_blk_chk,
          ls_blk TYPE zcust_blk_chk.

    SELECT * FROM zcust_blk_chk
      INTO TABLE lt_blk
      FOR ALL ENTRIES IN lt_excel
      WHERE bukrs = '1000'
        AND vkorg = '1000'
        AND kunnr = lt_excel-distributor.

* select from cust block table for dealer data
    SELECT * FROM zcust_blk_chk
     APPENDING TABLE lt_blk
     FOR ALL ENTRIES IN lt_excel
     WHERE bukrs = 'DMS1'
       AND vkorg = 'SDMS'
       AND kunnr = lt_excel-retailer.

    IF lt_blk[] IS NOT INITIAL.
      SORT lt_blk BY bukrs vkorg kunnr.
    ENDIF.

*select details from old invoice table
    DATA: lt_dmsretacnt TYPE TABLE OF zdms_retail_acnt,
          ls_dmsretacnt TYPE zdms_retail_acnt.

    SELECT * FROM zdms_retail_acnt
      INTO TABLE lt_dmsretacnt
      FOR ALL ENTRIES IN lt_excel
     WHERE distributor = lt_excel-distributor
       AND retailer    = lt_excel-retailer
       AND doc_type    = 'RV'.
    IF sy-subrc = 0.
      SORT lt_dmsretacnt BY distributor retailer.
    ENDIF.

*Get the document details from VBRK
    SELECT a~vbeln a~fkart a~vkorg a~fkdat
           a~belnr a~gjahr a~netwr a~mwsbk a~kunag
           a~fksto b~vkbur
       INTO TABLE lt_vbrp
       FROM vbrk AS a INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
       FOR ALL ENTRIES IN lt_excel
      WHERE a~vbeln = lt_excel-ref_no
        AND a~vkorg = 'SDMS'
        AND a~kunag = lt_excel-retailer
        AND b~posnr = '000001'.

*to get the existing cr note log
    DATA: lt_dmscrnotelog TYPE TABLE OF zdms_crnote_log,
          ls_dmscrnotelog TYPE zdms_crnote_log.

    SELECT * FROM zdms_crnote_log
      INTO TABLE lt_dmscrnotelog
     FOR ALL ENTRIES IN lt_excel
     WHERE bukrs = 'DMS1'
       AND distr = lt_excel-distributor
       AND kunnr = lt_excel-retailer.
    IF sy-subrc = 0.
      SORT lt_dmscrnotelog BY distr kunnr acdyr acdno.
    ENDIF.

    DATA: lt_dmscustpay TYPE TABLE OF zfi_dms_cust_pay,
          ls_dmscustpay TYPE zfi_dms_cust_pay.

*to get the payment dates and details
    SELECT * FROM zfi_dms_cust_pay
      INTO TABLE lt_dmscustpay
      FOR ALL ENTRIES IN lt_excel
     WHERE bukrs = 'DMS1'
       AND kunnr = lt_excel-retailer
       AND xblnr = lt_excel-payref
       AND distributor = lt_excel-distributor.
    IF sy-subrc = 0.
      SORT lt_dmscustpay BY distributor kunnr xblnr.
    ENDIF.

  ENDIF.

  DATA: lv_gsber    TYPE gsber,
        lv_msg      TYPE bapi_msg,
        lv_acdyr    TYPE gjahr,
        lv_belnr    TYPE belnr_d,
        lv_disname  TYPE name1,
        lv_retname  TYPE name1,
        lv_indat    TYPE budat,
        lv_intamt   TYPE dmbtr,
        lv_totamt   TYPE dmbtr,
        lv_postdate TYPE budat,
        lv_payamt   TYPE dmbtr,
        lv_paydt    TYPE begda,
        lv_roundoff TYPE i,
        lv_hknot    TYPE hkont,
        lv_pdays    TYPE zdmsdays,
        lv_dgper    TYPE zperzt,
        lv_dgamt    TYPE wrbtr,
        lv_refid    TYPE zref_id.


  LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_dealer>).

    CLEAR: lv_gsber, lv_msg, lv_belnr, lv_acdyr, lv_intamt, lv_indat, lv_dgamt, lv_disname, lv_retname,
           lv_totamt, lv_payamt, lv_paydt, lv_hknot, lv_pdays, lv_dgper, lv_refid, lv_postdate.

    <fs_dealer>-distributor  = |{ <fs_dealer>-distributor ALPHA = IN }|.
    <fs_dealer>-retailer     = |{ <fs_dealer>-retailer    ALPHA = IN }|.

*check distributor codes
    CLEAR ls_blk.
    READ TABLE lt_blk INTO ls_blk WITH KEY bukrs = '1000' vkorg = '1000' kunnr = <fs_dealer>-distributor.
    IF sy-subrc = 0.
      IF ls_blk-block = abap_true.
        lv_msg = | Distributor is blocked , { lv_msg } |.
      ELSE.
        lv_gsber = ls_blk-werks.
        lv_disname = ls_blk-name1.
      ENDIF.
    ELSE.
      lv_msg = |Distributor code is incorrect , { lv_msg }|.
    ENDIF.

*check distributor codes
    CLEAR ls_blk.
    READ TABLE lt_blk INTO ls_blk WITH KEY bukrs = 'DMS1' vkorg = 'SDMS' kunnr = <fs_dealer>-retailer.
    IF sy-subrc = 0.
      IF ls_blk-block = abap_true.
        lv_msg = | Retailer is blocked , { lv_msg } |.
      ELSE.
        lv_retname = ls_blk-name1.
      ENDIF.
    ELSE.
      lv_msg = |Retailer code is incorrect , { lv_msg }|.
    ENDIF.

    CLEAR ls_vbrp.
    READ TABLE lt_vbrp INTO ls_vbrp WITH KEY vbeln = <fs_dealer>-ref_no.
    IF sy-subrc = 0.
      IF ls_vbrp-fksto = abap_true.
        lv_msg = |Invoice Already Cancel , { lv_msg }|.
      ELSEIF ls_vbrp-kunag NE <fs_dealer>-retailer.
        lv_msg = |Invoice Not Belong to Retailer , { lv_msg }|.
      ELSEIF ls_vbrp-vkbur NE lv_gsber.
        lv_msg = |Invoice Not Belong to Distributor , { lv_msg }|.
      ELSE.
        lv_belnr = ls_vbrp-belnr.
        lv_acdyr = ls_vbrp-gjahr.
        lv_indat = ls_vbrp-fkdat.
        lv_intamt = ls_vbrp-netwr.
        CLEAR lv_roundoff.
        lv_roundoff = lv_intamt.
        lv_intamt = lv_roundoff.

        lv_totamt = ls_vbrp-netwr + ls_vbrp-mwsbk.
      ENDIF.
    ELSE.
*      do nothing need to check another table
    ENDIF.

    IF lv_belnr IS INITIAL.
      READ TABLE lt_dmsretacnt INTO ls_dmsretacnt WITH KEY distributor = <fs_dealer>-distributor
                                                           retailer    = <fs_dealer>-retailer
                                                           invoice     = <fs_dealer>-ref_no.
      IF sy-subrc = 0.
        lv_belnr = ls_dmsretacnt-doc_no.
        lv_acdyr = '2023'.
        lv_indat = ls_dmsretacnt-pos_date.

        lv_intamt = ls_dmsretacnt-net_amt.
        CLEAR lv_roundoff.
        lv_roundoff = lv_intamt.
        lv_intamt = lv_roundoff.

        lv_totamt = ls_dmsretacnt-total_amt.
        CLEAR lv_roundoff.
        lv_roundoff = lv_totamt.
        lv_totamt = lv_roundoff.
      ELSE.
        lv_msg = |Invoice No Incorrect , { lv_msg }|.
      ENDIF.
    ENDIF.

    IF lv_belnr IS NOT INITIAL.
      READ TABLE lt_dmscrnotelog INTO ls_dmscrnotelog WITH KEY distr = <fs_dealer>-distributor
                                                               kunnr = <fs_dealer>-retailer
                                                               acdno = lv_belnr
                                                               acdyr = lv_acdyr.
      IF sy-subrc = 0.
        lv_msg = |CD Already paid for the invoice , { lv_msg }|.
      ENDIF.
    ENDIF.

    IF <fs_dealer>-invflag = 'I'. "To create based on invoice for existign credit balance
      IF <fs_dealer>-payref IS NOT INITIAL.
        lv_msg = |Payref Should not Be Provided , { lv_msg }|.
      ELSE.
        lv_payamt = 0.
        lv_paydt  = lv_indat.
      ENDIF.
    ELSEIF <fs_dealer>-invflag = 'P'. "to create based on payments where CD is missed
      CLEAR ls_dmscustpay.
      READ TABLE lt_dmscustpay INTO ls_dmscustpay WITH KEY distributor = <fs_dealer>-distributor
                                                           kunnr       = <fs_dealer>-retailer
                                                           xblnr       = <fs_dealer>-payref.
      IF sy-subrc = 0.
        lv_payamt = ls_dmscustpay-amount.
        lv_paydt = ls_dmscustpay-budat.
        lv_refid = ls_dmscustpay-reference_id.
      ENDIF.
    ELSE.
      lv_msg = |Incorrect Inv Flag type , { lv_msg }|.
    ENDIF.

    IF <fs_dealer>-percentage = '1.5'.
      lv_hknot = '0042002013'.
      lv_dgper = <fs_dealer>-percentage.
    ELSEIF <fs_dealer>-percentage = '2.5'.
      lv_hknot = '0042002012'.
      lv_dgper = <fs_dealer>-percentage.
    ELSE.
      lv_msg = |Incorrect Percentage type , { lv_msg }|.
    ENDIF.

    IF lv_dgper IS NOT INITIAL.
      CLEAR lv_roundoff.
      lv_roundoff = lv_intamt.
      lv_intamt = lv_roundoff.
*Calculate % based on date
      lv_dgamt = lv_intamt * ( lv_dgper / 100 ).

      CLEAR lv_roundoff.
      lv_roundoff = lv_dgamt.
      lv_dgamt = lv_roundoff.
    ENDIF.

    lv_pdays = lv_paydt - lv_indat.
    lv_pdays = lv_pdays + 1.

    IF <fs_dealer>-postdate IS NOT INITIAL.
      lv_postdate = <fs_dealer>-postdate.
    ELSE.
      lv_postdate = lv_paydt.
    ENDIF.

*** function module to get fiscal year ***
    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = 'DMS1'
        posting_date  = lv_postdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

    IF lv_postdate+4(2) LE 3.
      lv_fisyear = lv_postdate(4) - 1.
    ELSE.
      lv_fisyear = lv_postdate(4).
    ENDIF.

    IF lv_msg IS INITIAL.
      APPEND VALUE #( bukrs     = 'DMS1'
                      refid     = lv_refid
                      distr     = <fs_dealer>-distributor
                      disname   = lv_disname
                      kunnr     = <fs_dealer>-retailer
                      retname   = lv_retname
                      acdno     = lv_belnr
                      acdyr     = lv_acdyr
                      dgdyr     = lv_fisyear
                      invno     = <fs_dealer>-ref_no
                      inamt     = lv_intamt
                      totamt    = lv_totamt
                      xblnr     = <fs_dealer>-payref
                      indat     = lv_indat
                      paydt     = lv_paydt
                      payamt    = lv_payamt
                      dgamt     = lv_dgamt
                      hkont     = lv_hknot
                      pdays     = lv_pdays
                      dgper     = lv_dgper
                      msgtyp    = 'S'
                      message   = |No errror in lineitems|
                      gsber     = lv_gsber
                      invflag   = <fs_dealer>-invflag
                      postdate  = lv_postdate ) TO gt_alv.
    ELSE.
      APPEND VALUE #( bukrs     = 'DMS1'
                      refid     = lv_refid
                      distr     = <fs_dealer>-distributor
                      disname   = lv_disname
                      kunnr     = <fs_dealer>-retailer
                      retname   = lv_retname
                      acdno     = lv_belnr
                      acdyr     = lv_acdyr
                      dgdyr     = lv_fisyear
                      invno     = <fs_dealer>-ref_no
                      inamt     = lv_intamt
                      totamt    = lv_totamt
                      xblnr     = <fs_dealer>-payref
                      indat     = lv_indat
                      paydt     = lv_paydt
                      payamt    = lv_payamt
                      dgamt     = lv_dgamt
                      hkont     = lv_hknot
                      pdays     = lv_pdays
                      dgper     = lv_dgper
                      msgtyp    = 'E'
                      message   = lv_msg
                      gsber     = lv_gsber
                      invflag   = <fs_dealer>-invflag
                      postdate  = lv_postdate ) TO gt_alv.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Include          ZDMS_OPENING_BALANCE_POST_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function, "Event for User command
      on_link_click FOR EVENT link_click  OF cl_salv_events_table
        IMPORTING
          row,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
  METHOD on_double_click.

    IF column = 'DOCNO'.
      READ TABLE gt_alv INTO DATA(ls_alv) INDEX row.
      IF ls_alv-dgdoc IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_alv-dgdoc.
        SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_alv-dgdyr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.
* Implementation
  METHOD on_link_click.
    READ TABLE gt_alv INTO DATA(ls_alv) INDEX row.
    IF ls_alv-checkbox = abap_true.
      ls_alv-checkbox = abap_false.
    ELSE.
      ls_alv-checkbox = abap_true.
    ENDIF.
    MODIFY gt_alv FROM ls_alv INDEX row TRANSPORTING checkbox.
* refresh ALV
    CALL METHOD lo_gr_alv->refresh.
  ENDMETHOD.
ENDCLASS.
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  DATA : ls_alv TYPE alv.
  CASE i_ucomm.
    WHEN '&INV_POST'.
      PERFORM f_posting.
    WHEN 'ALL'.
      ls_alv-checkbox = abap_true.
      MODIFY gt_alv FROM ls_alv TRANSPORTING checkbox WHERE checkbox = abap_false.
    WHEN 'DALL'.
      ls_alv-checkbox = abap_false.
      MODIFY gt_alv FROM ls_alv TRANSPORTING checkbox WHERE checkbox = abap_true.
  ENDCASE.
* refresh alv
  CALL METHOD lo_gr_alv->refresh.
ENDFORM.
FORM f_posting.

  DATA: lv_msg_text TYPE string.
  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
        lt_payable    TYPE TABLE OF bapiacap09,
        lt_recievable TYPE TABLE OF bapiacar09,
        lt_curramnt   TYPE TABLE OF bapiaccr09,
        lt_return     TYPE TABLE OF bapiret2.
  DATA: lv_belnr  TYPE belnr_d,
        ls_dealer TYPE zdms_crnote_log,
        lv_bktxt  TYPE bktxt.


  DATA: lv_referenceid TYPE num10.
  DATA: l_msg TYPE string.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S' AND checkbox = abap_true.

*********************get the number range*******************
    SELECT SINGLE * FROM zfi_dms_no_serie INTO @DATA(ls_series)
                                          WHERE distributor = @<fs_alv>-distr
                                          AND   plant       = @<fs_alv>-gsber
                                          AND   gjahr       = @<fs_alv>-dgdyr
                                          AND   doc_type    = 'CN'.
    IF sy-subrc <> 0.
      ls_series-distributor = <fs_alv>-distr.
      ls_series-gjahr       = <fs_alv>-dgdyr.
      ls_series-mandt       = sy-mandt.
      ls_series-plant       = <fs_alv>-gsber.
      ls_series-doc_type    = 'CN'.
    ENDIF.
    ls_series-num_range   = ls_series-num_range + 1.
************number series**************
    lv_bktxt = |{ ls_series-plant }{ ls_series-gjahr }{ ls_series-doc_type }{ ls_series-num_range }|.

******************header data filling*****************
    DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = 'DMS1'
                                        doc_date   = <fs_alv>-postdate
                                        pstng_date = <fs_alv>-postdate
                                        ref_doc_no = <fs_alv>-invno
                                        fisc_year  = <fs_alv>-dgdyr
                                        doc_type   = 'DG'
                                        header_txt = lv_bktxt ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return,lt_recievable.

    lt_recievable = VALUE #( ( itemno_acc = '1'
                               customer   = <fs_alv>-kunnr
                               item_text  = |{ <fs_alv>-invno }-Credit Note@ { <fs_alv>-dgper }%|
                               bus_area   = <fs_alv>-gsber  ) ).

    lt_glaccount = VALUE #( ( itemno_acc = '2'
                              gl_account = <fs_alv>-hkont
                              item_text  = |{ <fs_alv>-invno }-Credit Note@ { <fs_alv>-dgper }%|
                              bus_area   = <fs_alv>-gsber ) ).

    lt_curramnt = VALUE #( ( itemno_acc = '1'
                         currency   = 'INR'
                         amt_doccur = ( <fs_alv>-dgamt ) * -1 )
                       ( itemno_acc = '2'
                         currency   = 'INR'
                         amt_doccur = <fs_alv>-dgamt  ) ).

*      *** Document Check Before posting ***
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader    = l_headers
      TABLES
        accountgl         = lt_glaccount
        accountreceivable = lt_recievable
*       accountpayable    = lt_payable
        currencyamount    = lt_curramnt
        return            = lt_return.
    READ TABLE lt_return INTO DATA(lw_ret) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CLEAR lv_msg_text.
      LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
        lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
      ENDLOOP.
      <fs_alv>-msgtyp = 'E'.
      <fs_alv>-message = lv_msg_text.
    ELSE.
      IF p_chk3 EQ abap_true.
        <fs_alv>-msgtyp  = 'P'.
        <fs_alv>-message = |Document Ready to Post|.
      ELSE.

*need to generate a new reference number
        IF <fs_alv>-invflag = 'I'.
          PERFORM f_getrefno CHANGING <fs_alv>-refid.
        ENDIF.

        IF <fs_alv>-refid IS INITIAL.
          <fs_alv>-msgtyp  = 'E'.
          <fs_alv>-message = |Reference ID not generated Posting not possible|.
          CONTINUE.
        ENDIF.

        REFRESH: lt_return.
        CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Credit note ***
        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader    = l_headers
          IMPORTING
            obj_type          = lv_objtyp
            obj_key           = lv_objkey
            obj_sys           = lv_objsys
          TABLES
            accountgl         = lt_glaccount
            accountreceivable = lt_recievable
*           accountpayable    = lt_payable
            currencyamount    = lt_curramnt
            return            = lt_return.
        COMMIT WORK AND WAIT.
        READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          CLEAR lv_msg_text.
          LOOP AT lt_return INTO l_ret WHERE ( type = 'E' OR type = 'A' ).
            lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
          ENDLOOP.
          <fs_alv>-msgtyp  = 'E'.
          <fs_alv>-message = lv_msg_text.
        ELSE.
          COMMIT WORK AND WAIT.
          CLEAR lv_belnr.

          lv_belnr         = lv_objkey+0(10).
          <fs_alv>-dgdoc   = lv_belnr.
          <fs_alv>-msgtyp  = 'S'.
          <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
          <fs_alv>-crdat   = sy-datum.
          <fs_alv>-crnam   = sy-uname.
          <fs_alv>-crflag  = 'X'.
          <fs_alv>-new_docno = lv_bktxt.

******************update the next number range***********
          MODIFY zfi_dms_no_serie FROM ls_series.
          IF sy-subrc = 0.
            COMMIT WORK.
          ENDIF.
*****************add to the credit note log table************
          CLEAR ls_dealer.
          MOVE-CORRESPONDING <fs_alv> TO ls_dealer.
          INSERT zdms_crnote_log FROM ls_dealer.
          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
            <fs_alv>-msgtyp  = 'E'.
            <fs_alv>-message = |Error inserting data into log table|.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE : 'Please select any one of without errror lineitem' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_getrefno
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <FS_ALV>_REFID
*&---------------------------------------------------------------------*
FORM f_getrefno  CHANGING p_refid TYPE zref_id.

  DATA: lv_referenceid TYPE num10.
  DATA: l_msg TYPE string.

  CLEAR: lv_referenceid, l_msg.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCT_MANCD'
    IMPORTING
      number                  = lv_referenceid
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        l_msg = |interval_not_found|.
      WHEN '2'.
        l_msg = |number_range_not_intern|.
      WHEN '3'.
        l_msg = |object_not_found|.
      WHEN '4'.
        l_msg = |quantity_is_0|.
      WHEN '5'.
        l_msg = |quantity_is_not_1|.
      WHEN '6'.
        l_msg = |interval_overflow|.
      WHEN '7'.
        l_msg = |buffer_overflow|.
      WHEN '8'.
        l_msg = |Others|.
    ENDCASE.
  ELSE.
    p_refid = lv_referenceid.
  ENDIF.
ENDFORM.

FORM alv_display.

  IF gt_alv IS NOT INITIAL.

    DATA: lo_gr_functions  TYPE REF TO cl_salv_functions_list.
    DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.
    DATA: lo_display       TYPE REF TO cl_salv_display_settings. " Variable for layout settings
    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_table.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.

    lo_gr_alv->set_screen_status(
        pfstatus      =  'POSTING'
        report        =  sy-repid
        set_functions = lo_gr_alv->c_functions_all ).


* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_user_command FOR lo_events.
    SET HANDLER lo_event_handler->on_link_click FOR lo_events.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.


    TRY.
        lo_column ?= lo_columns->get_column( 'CHECKBOX' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Checkbox' ).
        lo_column->set_medium_text( 'Checkbox' ).
        lo_column->set_short_text( 'Checkbox' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>checkbox_hotspot ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REFID' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'PayRef SID' ).
        lo_column->set_medium_text( 'PayRef SID' ).
        lo_column->set_short_text( 'PayRef SID' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REFID' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'PayRef SID' ).
        lo_column->set_medium_text( 'PayRef SID' ).
        lo_column->set_short_text( 'PayRef SID' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DISTR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Distributor' ).
        lo_column->set_medium_text( 'Distributor' ).
        lo_column->set_short_text( 'Distr' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DISNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Dist Name' ).
        lo_column->set_medium_text( 'Dist Name' ).
        lo_column->set_short_text( 'Dist Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'KUNNR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Dealer' ).
        lo_column->set_medium_text( 'Dealer' ).
        lo_column->set_short_text( 'Dealer' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'RETNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Deal Name' ).
        lo_column->set_medium_text( 'Deal Name' ).
        lo_column->set_short_text( 'Deal Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'ACDNO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'INV ACDOC No' ).
        lo_column->set_medium_text( 'INV ACDOC No' ).
        lo_column->set_short_text( 'ACDOC No' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ACDYR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'ACDOC YR' ).
        lo_column->set_medium_text( 'ACDOC YR' ).
        lo_column->set_short_text( 'ACDOC YR' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DGDOC' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'DG Doc No' ).
        lo_column->set_medium_text( 'DG Doc No' ).
        lo_column->set_short_text( 'DG Doc No' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DGDYR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'DG Doc Yr' ).
        lo_column->set_medium_text( 'DG Doc Yr' ).
        lo_column->set_short_text( 'DG Doc Yr' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INVNO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Invoice No' ).
        lo_column->set_medium_text( 'Invoice No' ).
        lo_column->set_short_text( 'Invoice No' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INAMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Inv Amt' ).
        lo_column->set_medium_text( 'Inv Amt' ).
        lo_column->set_short_text( 'Inv Amt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'XBLNR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'MIS Pay Ref' ).
        lo_column->set_medium_text( 'MIS Pay Ref' ).
        lo_column->set_short_text( 'MIS Ref' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Inv Dt' ).
        lo_column->set_medium_text( 'Inv Dt' ).
        lo_column->set_short_text( 'Inv Dt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'PAYDT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Pay Date' ).
        lo_column->set_medium_text( 'Pay Date' ).
        lo_column->set_short_text( 'Pay Date' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'POSTDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Posting Date' ).
        lo_column->set_medium_text( 'Posting Date' ).
        lo_column->set_short_text( 'Post Date' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'PAYAMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Paid Amt' ).
        lo_column->set_medium_text( 'Paid Amt' ).
        lo_column->set_short_text( 'Paid Amt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DGAMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CD Amt' ).
        lo_column->set_medium_text( 'CD Amt' ).
        lo_column->set_short_text( 'CD Amt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'HKONT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Acct' ).
        lo_column->set_medium_text( 'GL Acct' ).
        lo_column->set_short_text( 'GL Acct' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'PDAYS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'PDAYS' ).
        lo_column->set_medium_text( 'PDAYS' ).
        lo_column->set_short_text( 'PDAYS' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'DGPER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CD Per' ).
        lo_column->set_medium_text( 'CD Per' ).
        lo_column->set_short_text( 'CD Per' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CRDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'CRT Date' ).
        lo_column->set_medium_text( 'CRT Date' ).
        lo_column->set_short_text( 'CRT Date' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CRNAM' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'CRT Name' ).
        lo_column->set_medium_text( 'CRT Name' ).
        lo_column->set_short_text( 'CRT Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSGTYP' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Type' ).
        lo_column->set_medium_text( 'Type' ).
        lo_column->set_short_text( 'Type' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CRFLAG' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'CRD Flag' ).
        lo_column->set_medium_text( 'CRD Flag' ).
        lo_column->set_short_text( 'CRD Flag' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GSBER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'B.Area' ).
        lo_column->set_medium_text( 'B.Area' ).
        lo_column->set_short_text( 'B.Area' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INVFLAG' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CD Type' ).
        lo_column->set_medium_text( 'CD Type' ).
        lo_column->set_short_text( 'CD Type' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

***********Call the alv screen************
    lo_gr_alv->display( ).

  ENDIF.
ENDFORM.

FORM screen_adj.
**********screen adjustment**************
  LOOP AT SCREEN.

    IF r4 = abap_false AND ( screen-name CS 'SO_RETAI' OR screen-name CS 'SO_PDATE' OR screen-name CS 'SO_DOCTY' ).
      screen-active = 0.
    ENDIF.
    IF r1 = abap_false AND ( screen-name CS 'P_CHK3' ).
      screen-active = 0.
    ENDIF.
    IF r1 = abap_false AND screen-name CS 'P_FNAME'.
      screen-active = 0.
    ENDIF.
    IF r1 = abap_true AND screen-name CS 'SO_KUNNR'.  "disable distributor fields
      screen-active = 0.
    ENDIF.
    IF r4 = abap_false AND ( screen-name CS 'SO_STATU' ).
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

FORM display .

************fetch the credit note details from log table*********
  SELECT *  FROM zdms_crnote_log
                  INTO TABLE @DATA(lt_retail)
                  WHERE distr IN @so_kunnr
                  AND   kunnr IN @so_retai.
  IF sy-subrc = 0.

    DATA: lo_gr_functions  TYPE REF TO cl_salv_functions_list.
    DATA: lo_display       TYPE REF TO cl_salv_display_settings. " Variable for layout settings
    DATA: lo_columns TYPE REF TO cl_salv_columns.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = lt_retail.
      CATCH cx_salv_msg.
    ENDTRY.

* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
***********call the alv screen************
    lo_gr_alv->display( ).
  ELSE.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
