
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 19.03.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Retailer debit credit note upload program
*
*  Released on Date          :
* Hardcoded                  : gv_bukrs = 'DMS1'.
*=======================================================================
REPORT zdms_retailer_dr_cr_upl_pro.
**************Alv sctructure*********
TYPES: BEGIN OF alv,
         checkbox    TYPE char1,
         distributor TYPE zdist,
         dist_name   TYPE zdist_name,
         gsber       TYPE gsber,
         account(10) TYPE c,        "dealer
         reta_name   TYPE zret_name,
         acctyp      TYPE koart,
         blart       TYPE blart,
         doc_typetxt TYPE char20,
         refdoc      TYPE xblnr1,   "ref dealer
         fisyear     TYPE gjahr,
         period      TYPE monat,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         total       TYPE zdms_retail_acnt-total_amt,
         text        TYPE sgtxt,
         docno       TYPE belnr_d,
         bktxt       TYPE bktxt,
         msgtyp      TYPE bapi_mtype,
         message     TYPE string,
       END OF alv.

TYPES : BEGIN OF ty_excel,
          distributor TYPE zdist,
          retailer    TYPE kunnr,
          ref_no      TYPE xblnr1,
          doc_date    TYPE bldat,
          pos_date    TYPE budat,
          doc_type    TYPE blart,
          gl_acnt     TYPE saknr,
          total_amt   TYPE ztotal,
          text        TYPE sgtxt,
        END OF ty_excel.
****************Global data dec*******************
DATA : gt_alv  TYPE TABLE OF alv.
DATA : lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA : gv_date  TYPE budat.
DATA : gv_bukrs TYPE bukrs.
DATA : gv_kunnr TYPE kna1-kunnr.
DATA : gv_doc   TYPE bkpf-blart.

DATA: ls_excel TYPE ty_excel.
*DATA : gv_statu TYPE zdms_retail_acnt-status.
*******************Selection screen design******************

*SELECTION-SCREEN

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

***********Download option for excel format*************
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 1(14) downn USER-COMMAND down MODIF ID b.

  SELECTION-SCREEN SKIP 1.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND u1 DEFAULT 'X', "upload excel dr/cr note  Posting Process
*               r2 RADIOBUTTON GROUP g1,          "Posting Process
*               r3 RADIOBUTTON GROUP g1,          "business area update
               r4 RADIOBUTTON GROUP g1.          "zdms_deal_dr_cr table display
SELECTION-SCREEN : END OF BLOCK b1.
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : so_kunnr FOR gv_kunnr MODIF ID a,        "distributor
                   so_retai FOR gv_kunnr MODIF ID a,        "retailer
                   so_pdate FOR sy-datum MODIF ID a,        "posting date
                   so_docty FOR gv_doc   MODIF ID a.        "doc type
*                   so_statu FOR gv_statu MODIF ID a NO INTERVALS.        "posting date
  PARAMETERS : p_fname TYPE rlgrap-filename MODIF ID a.     "for excel file upload
  PARAMETERS : p_chk3 AS CHECKBOX MODIF ID bl3.
SELECTION-SCREEN : END OF BLOCK b2.

**********Events**********
START-OF-SELECTION.
  IF r1 = abap_true.                    "Display the posting data
    PERFORM : process.
    PERFORM : alv_display.
*  ELSEIF r3 = abap_true.                "business area update
*    PERFORM : barea_update.
  ELSEIF r4 = abap_true.                "zdms_deal_dr_cr table display
    PERFORM : display.
*  ELSEIF r1 = abap_true.               "upload the past cr/dr note excel file
*    PERFORM : credit_debit_upload.
  ENDIF.

  "f4 functionality to file path

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
  downn    = 'Excel Format'.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen_adj.
**********Make the file path as mandatory********
AT SELECTION-SCREEN.

  IF r1 = 'X' AND p_fname IS INITIAL AND sy-ucomm = 'ONLI'.

    MESSAGE : 'Please Fill the File Path' TYPE 'E'.

  ENDIF.

***********download excel format***********
  IF sy-ucomm = 'DOWN'.
    PERFORM : down_exc.
  ENDIF.

END-OF-SELECTION.

FORM process.

  DATA : lv_fisyear TYPE bapi0002_4-fiscal_year,
         lv_month   TYPE bapi0002_4-fiscal_period,
         l_return   TYPE bapireturn1.
************loc variable dec**********
  DATA : lv_msg  TYPE  string,
         lv_gl   TYPE  hkont,
         lv_text TYPE  string.

*************customer block check***********
  DATA : lo_block TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_block.
*  DATA : lt_block      TYPE TABLE OF zsd_st_cust_block,
*         lt_retail_blk TYPE TABLE OF zsd_st_cust_block.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         lt_raw   TYPE truxs_t_text_data.
  DATA : ls_dealer TYPE zdms_deal_dr_cr.
****************excel to it**************
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
    EXPORTING
      i_tab_raw_data       = lt_raw
      i_filename           = p_fname
      i_line_header        = abap_true
    TABLES
      i_tab_converted_data = lt_excel.

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
      ls_excel-gl_acnt      = |{ ls_excel-gl_acnt    ALPHA = IN }|.
      MODIFY lt_excel FROM ls_excel INDEX lv_tabix.
    ENDLOOP.
*    DELETE ADJACENT DUPLICATES FROM lt_excel COMPARING distributor retailer ref_no.
*    IF sy-subrc = 0.
*      MESSAGE : 'Duplicate Reference Number , Please Check The Excel File' TYPE 'S' DISPLAY LIKE 'E'.
*    ELSE.
***********fetch the datas from zdms_retail_acnt for dublicate entry check***********
    SELECT distributor,retailer,ref_doc FROM zdms_deal_dr_cr
      INTO TABLE @DATA(lt_check)
      FOR ALL ENTRIES IN @lt_excel
      WHERE distributor = @lt_excel-distributor
        AND retailer    = @lt_excel-retailer.
    IF sy-subrc = 0.
      SORT lt_check BY distributor retailer ref_doc.
    ENDIF.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1
      INTO TABLE @DATA(lt_kna1)
      FOR ALL ENTRIES IN @lt_excel
      WHERE kunnr = @lt_excel-distributor
        AND werks NE @abap_false.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.

****************fetch the retailer sales details************
    SELECT a~kunnr,a~name1,a~werks,b~loevm,b~vkorg,b~vtweg,b~spart
      FROM kna1 AS a
      INNER JOIN knvv AS b ON a~kunnr = b~kunnr
      INTO TABLE @DATA(lt_kna2)
      FOR ALL ENTRIES IN @lt_excel
      WHERE  a~kunnr = @lt_excel-retailer
        AND  b~vkorg = 'SDMS'.
    IF sy-subrc = 0.
      SORT lt_kna2 BY kunnr vkorg vtweg spart.
    ENDIF.

    REFRESH gt_alv.
****************fetch the GL ACNT details************
    SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat).
    IF sy-subrc = 0.
      SORT lt_skat BY saknr ktopl.
    ENDIF.

****************fetch the incomming GL details************
    SELECT kunnr,werks,incgl FROM zdms_distb_gl INTO TABLE @DATA(lt_glact).
    IF sy-subrc = 0.
      SORT lt_glact BY kunnr werks.
    ENDIF.

**************data process**************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_dealer>).
*      <fs_dealer>-distributor  = |{ <fs_dealer>-distributor ALPHA = IN }|.
*      <fs_dealer>-retailer     = |{ <fs_dealer>-retailer    ALPHA = IN }|.
      CONDENSE <fs_dealer>-ref_no NO-GAPS.
*****************check the dublicate entry****************
      READ TABLE lt_check TRANSPORTING NO FIELDS WITH KEY distributor = <fs_dealer>-distributor
                                                          retailer    = <fs_dealer>-retailer
                                                          ref_doc     = <fs_dealer>-ref_no BINARY SEARCH.
      IF sy-subrc = 0.
        lv_msg = |Already Document with same reference Available , { lv_msg }|.
      ENDIF.

****************check the dealer code****************
      READ TABLE lt_kna2 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_dealer>-retailer BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Retailer code is incorrect , { lv_msg }|.
      ELSE.
        ls_dealer-reta_name = ls_kna1-name1.
      ENDIF.
****************check the dealer code****************
      READ TABLE lt_kna2 INTO ls_kna1 WITH KEY kunnr = <fs_dealer>-retailer
                                               vkorg = 'SDMS'
                                               vtweg = '20'
                                               spart = '10' BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Retailer code is not maintained in SDMS , { lv_msg }|.
      ELSE.
      ENDIF.
*****************check retailer block or not*******************
      SELECT SINGLE bukrs FROM zcust_blk_chk INTO @DATA(ls_blk) WHERE bukrs = 'DMS1'
                                                                AND   vkorg = 'SDMS'
                                                                AND   kunnr = @<fs_dealer>-retailer
                                                                AND   block = @abap_true.
      IF sy-subrc = 0.
        lv_msg = | Retailer is blocked , { lv_msg } |.
      ENDIF.
***********check the amount***********
      IF <fs_dealer>-total_amt LT 0.
        lv_msg = |Amount is zero , { lv_msg }|.
      ENDIF.
*****************check distributor block or not*******************
      SELECT SINGLE bukrs FROM zcust_blk_chk INTO ls_blk WHERE bukrs = '1000'
                                                         AND   vkorg = '1000'
                                                         AND   kunnr = <fs_dealer>-distributor
                                                         AND   block = abap_true.
      IF sy-subrc = 0.
        lv_msg = | Distributor is blocked , { lv_msg } |.
      ENDIF.
****************distributor code business area update****************
      READ TABLE lt_kna1 INTO DATA(ls_kna2) WITH KEY kunnr = <fs_dealer>-distributor BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_dealer-b_area    = ls_kna2-werks.
        ls_dealer-dist_name = ls_kna2-name1.
      ELSE.
        lv_msg = |Distributor code is incorrect , { lv_msg }|.
      ENDIF.
      CLEAR : ls_kna1.
      "Business Area checks
      IF ls_dealer-b_area IS INITIAL.
        lv_msg = |Business area is missing , { lv_msg }|.
      ENDIF.
**********check the doc type********
      IF <fs_dealer>-doc_type = 'DR'.          "debit
        ls_dealer-doc_type = 'DR'.
        lv_text            = 'Debit Note'.
      ELSEIF <fs_dealer>-doc_type = 'CR' OR <fs_dealer>-doc_type = 'DG'.      "credit
        ls_dealer-doc_type = 'DG'.
        lv_text            = 'Credit Note'.
*      ELSEIF <fs_dealer>-doc_type = 'DA'.      "
*        ls_dealer-doc_type = 'DG'.
*        lv_text            = 'Reversal Payment'.
      ENDIF.

**************only for DA document*************
*      IF <fs_dealer>-doc_type = 'DA'.
*        READ TABLE lt_glact INTO DATA(ls_glact) WITH KEY kunnr = <fs_dealer>-distributor
*                                                         werks = ls_dealer-b_area BINARY SEARCH.
*        IF sy-subrc = 0.
*          <fs_dealer>-gl_acnt = ls_glact-incgl.
*          <fs_dealer>-gl_acnt = |{ <fs_dealer>-gl_acnt ALPHA = IN }|.
*        ELSE.
*          lv_msg = |GL Account not maintained in zdms_distb_gl , { lv_msg }|.
*        ENDIF.
*      ENDIF.
*      "GL Account existence checks
      READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = <fs_dealer>-gl_acnt
                                                     ktopl = 'SDMS' BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Incorrect GL Account Number , { lv_msg }|.
      ENDIF.
      IF <fs_dealer>-gl_acnt IS INITIAL.
        lv_msg = |GL account is missing , { lv_msg }|.
      ENDIF.
*************Doc type checking************
      IF ls_dealer-doc_type IS INITIAL.
        lv_msg = |Document type is missing , { lv_msg }|.
      ENDIF.
******************posting date check****************
      IF <fs_dealer>-pos_date IS INITIAL.
        lv_msg = |Posting date is missing , { lv_msg }|.
      ELSEIF <fs_dealer>-pos_date GT sy-datum.
        lv_msg = |Can't Post Future Date , { lv_msg }|.
      ELSEIF <fs_dealer>-pos_date+4(2) LT sy-datum+4(2).
        lv_msg = |Can't Post Past Month, { lv_msg }|.
      ENDIF.
****************TEXT CHECK**********
      IF <fs_dealer>-text IS INITIAL.
        lv_msg = |Text is missing , { lv_msg }|.
      ENDIF.
*** function module to get fiscal year ***
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = gv_bukrs
          posting_date  = <fs_dealer>-pos_date
        IMPORTING
          fiscal_year   = lv_fisyear
          fiscal_period = lv_month
          return        = l_return.

      IF <fs_dealer>-pos_date+4(2) LE 3.
        lv_fisyear = <fs_dealer>-pos_date(4) - 1.
      ELSE.
        lv_fisyear = <fs_dealer>-pos_date(4).
      ENDIF.

      IF lv_msg IS INITIAL.
        "Adding the Success Msg
        APPEND VALUE #( distributor = <fs_dealer>-distributor
                        account     = <fs_dealer>-retailer
                        dist_name   = ls_dealer-dist_name
                        reta_name   = ls_dealer-reta_name
                        acctyp      = 'D'
                        blart       = ls_dealer-doc_type
                        refdoc      = <fs_dealer>-ref_no
                        fisyear     = lv_fisyear
                        period      = lv_month
                        doc_typetxt = lv_text
                        docdate     = <fs_dealer>-doc_date
                        pdate       = <fs_dealer>-pos_date
                        gl_account  = <fs_dealer>-gl_acnt
                        glacc_txt   = ls_skat-txt50
                        total       = <fs_dealer>-total_amt
                        gsber       = ls_dealer-b_area
                        text        = <fs_dealer>-text
                        msgtyp      = 'S'
                        message     = |No errror in lineitems| ) TO gt_alv.
      ELSE.
        "Adding the Error Msg
        APPEND VALUE #( distributor = <fs_dealer>-distributor
                        account     = <fs_dealer>-retailer
                        dist_name   = ls_dealer-dist_name
                        reta_name   = ls_dealer-reta_name
                        acctyp      = 'D'
                        blart       = ls_dealer-doc_type
                        refdoc      = <fs_dealer>-ref_no
                        fisyear     = lv_fisyear
                        period      = lv_month
                        doc_typetxt = lv_text
                        docdate     = <fs_dealer>-doc_date
                        pdate       = <fs_dealer>-pos_date
                        gl_account  = <fs_dealer>-gl_acnt
                        glacc_txt   = ls_skat-txt50
                        total       = <fs_dealer>-total_amt
                        gsber       = ls_dealer-b_area
                        text        = <fs_dealer>-text
                        msgtyp      = 'E'
                        message     = lv_msg ) TO gt_alv.
      ENDIF.
      CLEAR : ls_dealer,ls_kna1,ls_skat,lv_msg,lv_gl,lv_text.
    ENDLOOP.
*    ENDIF.
  ENDIF.
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
      IF ls_alv-docno IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_alv-docno.
        SET PARAMETER ID 'BUK' FIELD gv_bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_alv-fisyear.
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

FORM alv_display.
  IF gt_alv IS NOT INITIAL.

    SORT gt_alv BY distributor account pdate.

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
        lo_column ?= lo_columns->get_column( 'ACCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Retailer code' ).
        lo_column->set_medium_text( 'Retailer' ).
        lo_column->set_short_text( 'Retailer' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'NAME1' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Name' ).
        lo_column->set_medium_text( 'Name' ).
        lo_column->set_short_text( 'Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ACCTYP' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Account type' ).
        lo_column->set_medium_text( 'Account type' ).
        lo_column->set_short_text( 'Acctyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DOC_TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document Type' ).
        lo_column->set_medium_text( 'Document Type' ).
        lo_column->set_short_text( 'Doc_Type' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REFDOC' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Reference Doc' ).
        lo_column->set_medium_text( 'Reference' ).
        lo_column->set_short_text( 'Reference' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DOCDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document date' ).
        lo_column->set_medium_text( 'Document date' ).
        lo_column->set_short_text( 'Docdate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'PDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Posting Date' ).
        lo_column->set_medium_text( 'Posting Date' ).
        lo_column->set_short_text( 'Postdate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BLART' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document Type' ).
        lo_column->set_medium_text( 'Document Type' ).
        lo_column->set_short_text( 'Doctype' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'FISYEAR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Fiscal Year' ).
        lo_column->set_medium_text( 'Fiscal Year' ).
        lo_column->set_short_text( 'Fisyear' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GL_ACCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Account' ).
        lo_column->set_medium_text( 'GL Account' ).
        lo_column->set_short_text( 'GL Account' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GLACC_TXT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Acctext' ).
        lo_column->set_medium_text( 'GL Acctext' ).
        lo_column->set_short_text( 'GL Acctext' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'AMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Amount' ).
        lo_column->set_medium_text( 'Amount' ).
        lo_column->set_short_text( 'Amount' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSGTYP' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Msgtyp' ).
        lo_column->set_medium_text( 'Msgtyp' ).
        lo_column->set_short_text( 'Msgtyp' ).
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
***********Call the alv screen************
    lo_gr_alv->display( ).

  ENDIF.
ENDFORM.


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
        ls_dealer TYPE zdms_deal_dr_cr,
        lv_bktxt  TYPE bktxt,
        lv_blart  TYPE blart.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S'
                                                  AND checkbox = abap_true.

    CLEAR : lv_bktxt,lv_blart.
****************check the document type************
    IF <fs_alv>-blart = 'DG'.
      lv_blart = 'CN'.
    ELSEIF <fs_alv>-blart = 'DR'.
      lv_blart = 'DN'.
    ENDIF.
**********************get the number range*****************
    SELECT SINGLE * FROM zfi_dms_no_serie INTO @DATA(ls_series)
                                          WHERE distributor = @<fs_alv>-distributor
                                          AND   plant       = @<fs_alv>-gsber
                                          AND   gjahr       = @<fs_alv>-fisyear
                                          AND   doc_type    = @lv_blart.
    IF sy-subrc <> 0.
      ls_series-distributor = <fs_alv>-distributor.
      ls_series-gjahr       = <fs_alv>-fisyear.
      ls_series-mandt       = sy-mandt.
      ls_series-plant       = <fs_alv>-gsber.
      ls_series-doc_type    = lv_blart.
    ENDIF.
    ls_series-num_range   = ls_series-num_range + 1.
************number series**************
    lv_bktxt = |{ ls_series-plant }{ ls_series-gjahr }{ ls_series-doc_type }{ ls_series-num_range }|.

************************header data filling********************
    DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = gv_bukrs
                                        doc_date   = <fs_alv>-docdate
                                        pstng_date = <fs_alv>-pdate
                                        ref_doc_no = <fs_alv>-refdoc
                                        fisc_year  = <fs_alv>-fisyear
                                        doc_type   = <fs_alv>-blart
                                        header_txt = lv_bktxt ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return,lt_recievable.

    lt_recievable = VALUE #( ( itemno_acc = '1'
                               customer   = <fs_alv>-account
                               item_text  = <fs_alv>-text
                               bus_area   = <fs_alv>-gsber  ) ).

    lt_glaccount = VALUE #( ( itemno_acc = '2'
                              gl_account = <fs_alv>-gl_account
                              item_text  = <fs_alv>-text
                              bus_area   = <fs_alv>-gsber ) ).

    IF <fs_alv>-blart = 'DG'.            "credit note or incomming payment

      lt_curramnt = VALUE #( ( itemno_acc = '1'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-total ) * -1 )
                             ( itemno_acc = '2'
                               currency   = 'INR'
                               amt_doccur = <fs_alv>-total  ) ).

    ELSEIF <fs_alv>-blart = 'DR'. "OR <fs_alv>-blart = 'DA'.          "debit note or DA

      lt_curramnt = VALUE #( ( itemno_acc = '1'
                               currency   = 'INR'
                               amt_doccur = <fs_alv>-total )
                             ( itemno_acc = '2'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-total ) * -1 ) ).

    ENDIF.
    IF p_chk3 EQ abap_true.
      REFRESH : lt_return.
      CLEAR   : lv_objkey,lv_objsys,lv_objtyp.
*** Document Check Before posting ***
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = l_headers
        TABLES
          accountgl         = lt_glaccount
          accountreceivable = lt_recievable
          accountpayable    = lt_payable
          currencyamount    = lt_curramnt
          return            = lt_return.
      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CLEAR lv_msg_text.
        LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
          lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
        ENDLOOP.
        <fs_alv>-msgtyp  = 'E'.
        <fs_alv>-message = lv_msg_text.
      ELSE.
        <fs_alv>-msgtyp  = 'P'.
        <fs_alv>-message = |Document Ready to Post|.
      ENDIF.

    ELSE.

      REFRESH : lt_return.
      CLEAR   : lv_objkey,lv_objsys,lv_objtyp.

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
          accountpayable    = lt_payable
          currencyamount    = lt_curramnt
          return            = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
      IF sy-subrc <> 0.
        CLEAR lv_msg_text.
        LOOP AT lt_return INTO l_ret WHERE ( type = 'E' OR type = 'A' ).
          lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
        ENDLOOP.
        <fs_alv>-msgtyp  = 'E'.
        <fs_alv>-message = lv_msg_text.
      ELSE.
        COMMIT WORK AND WAIT.
******************update the next number range***********
        MODIFY zfi_dms_no_serie FROM ls_series.

        CLEAR lv_belnr.
        lv_belnr         = lv_objkey+0(10).
        <fs_alv>-docno   = lv_belnr.
        <fs_alv>-msgtyp  = 'P'.
        <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
        <fs_alv>-bktxt   = lv_bktxt.
*****************log table******************
        ls_dealer-distributor = <fs_alv>-distributor.
        ls_dealer-retailer    = <fs_alv>-account.
        ls_dealer-ref_doc     = <fs_alv>-refdoc.
        ls_dealer-dist_name   = <fs_alv>-dist_name.
        ls_dealer-reta_name   = <fs_alv>-reta_name.
        ls_dealer-b_area      = <fs_alv>-gsber.
        ls_dealer-total_amt   = <fs_alv>-total.
        ls_dealer-doc_type    = <fs_alv>-blart.
        ls_dealer-pos_date    = <fs_alv>-pdate.
        ls_dealer-doc_date    = <fs_alv>-docdate.
        ls_dealer-erdat       = sy-datum.
        ls_dealer-ernam       = sy-uname.
        ls_dealer-uzeit       = sy-uzeit.
        ls_dealer-saknr       = <fs_alv>-gl_account.
        ls_dealer-sgtxt       = <fs_alv>-text.
        ls_dealer-doc_no      = lv_belnr.
        ls_dealer-doc_yr      = lv_objkey+14(4).
        ls_dealer-status      = 'S'.
        ls_dealer-message     = <fs_alv>-message.
        ls_dealer-new_docno   = lv_bktxt.
        MODIFY zdms_deal_dr_cr FROM ls_dealer.
        CLEAR : ls_dealer.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR : ls_series.

  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE : 'Please select any one of without errror lineitem' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*FORM barea_update.
*
*  SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
*  IF sy-subrc = 0.
*
*    SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1)
*                       FOR ALL ENTRIES IN @lt_t001k
*                       WHERE werks =  @lt_t001k-bwkey
*                       AND   kunnr IN @so_kunnr.
*
*    LOOP AT lt_kna1 INTO DATA(ls_kna2).
*      UPDATE zdms_deal_dr_cr SET b_area = ls_kna2-werks WHERE distributor = ls_kna2-kunnr.
*    ENDLOOP.
*    IF sy-subrc = 0.
*      MESSAGE : 'Business Area Updated successfully' TYPE 'S'.
*    ELSE.
*      MESSAGE : 'Business Area Not Updated' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
FORM display .


  IF so_docty-low = 'CR'.
    so_docty-low = 'DG'.
  ENDIF.

************fetch the debit & credit note details from log table*********
  SELECT a~distributor,
         a~retailer,
         a~ref_doc,
         a~dist_name,
         a~reta_name,
         a~b_area,
         a~doc_type,
         a~doc_date,
         a~pos_date,
         a~total_amt,
         a~saknr,
         b~txt50,
         a~sgtxt,
         a~doc_no,
         a~status,
         a~message,
         a~erdat,
         a~uzeit,
         a~ernam  FROM zdms_deal_dr_cr AS a
                  INNER JOIN skat AS b ON a~saknr = b~saknr
                  INTO TABLE @DATA(lt_retail)
                  WHERE distributor IN @so_kunnr
                  AND   retailer    IN @so_retai
                  AND   pos_date    IN @so_pdate
                  AND   doc_type    IN @so_docty.
*                  AND   status      IN @so_statu.
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


FORM credit_debit_upload.


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

FORM down_exc .

  DATA : lt_sample TYPE TABLE OF ty_excel,
         p_file    TYPE rlgrap-filename,
         lv_string TYPE string.

  DATA : BEGIN OF it_heading OCCURS 0,
           text(15),
         END OF it_heading.

  it_heading-text = 'Distributor'.
  APPEND it_heading.
  it_heading-text = 'Retailer'.
  APPEND it_heading.
  it_heading-text = 'Reference No'.
  APPEND it_heading.
  it_heading-text = 'Doc Date'.
  APPEND it_heading.
  it_heading-text = 'Pos Date'.
  APPEND it_heading.
  it_heading-text = 'Doc Type'.
  APPEND it_heading.
  it_heading-text = 'GL Acnt'.
  APPEND it_heading.
  it_heading-text = 'Amount'.
  APPEND it_heading.
  it_heading-text = 'Text'.
  APPEND it_heading.

  APPEND VALUE #( ref_no      =   '1'
                  distributor =   '8664644644'
                  retailer    =   '4646464565'
                  doc_date    =   '20240413'
                  pos_date    =   '20240413'
                  doc_type    =   'DR'
                  gl_acnt     =   '2314861'
                  total_amt   =   '10000'
                  text        =   'Debit note sample' ) TO lt_sample.

  APPEND VALUE #( ref_no      =   '2'
                  distributor =   '8664644644'
                  retailer    =   '4646464565'
                  doc_date    =   '20240413'
                  pos_date    =   '20240413'
                  doc_type    =   'CR'
                  gl_acnt     =   '2314861'
                  total_amt   =   '5000'
                  text        =   'Credit note sample' ) TO lt_sample.

*************File path********
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_file.

  lv_string = p_file.
***************download**************
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename   = lv_string
*     write_field_separator = 'X'
      filetype   = 'DAT'
    TABLES
      data_tab   = lt_sample
      fieldnames = it_heading.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
