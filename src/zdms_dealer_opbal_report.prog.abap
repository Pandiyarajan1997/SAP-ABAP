
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 09.01.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Customer Ledger - Sub Dealer (retailer) Opening balance apr1 - ZDMS_DEAL_OPBAL
*
*  Released on Date          :
* Hardcoded                  : gv_bukrs = 'DMS1'.
*                              gv_date = '20230401'.
*=======================================================================
REPORT zdms_dealer_opbal_report.
**************Alv sctructure*********
TYPES: BEGIN OF alv,
         checkbox    TYPE char1,
         distributor TYPE zdist,
         gsber       TYPE gsber,
         account(10) TYPE c,        "dealer
         name1       TYPE name1_gp,
         acctyp      TYPE koart,
         blart       TYPE blart,
         refdoc      TYPE xblnr1,   "ref dealer
         fisyear     TYPE gjahr,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         amt         TYPE wrbtr,
         item_txt    TYPE sgtxt,
         docno       TYPE belnr_d,
         msgtyp      TYPE bapi_mtype,
         message     TYPE string,
       END OF alv.
****************Global data dec*******************
DATA : gt_alv  TYPE TABLE OF alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA : gv_date  TYPE budat.
DATA : gv_bukrs TYPE bukrs.
DATA : gv_kunnr TYPE kna1-kunnr.
gv_bukrs = 'DMS1'.
gv_date = '20230401'.
*******************Selection screen design******************

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : so_kunnr FOR gv_kunnr MODIF ID a.
  PARAMETERS : p_fname TYPE rlgrap-filename MODIF ID a.     "for excel file upload
  PARAMETERS : p_chk3 AS CHECKBOX MODIF ID bl3.
  PARAMETERS : r3 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,         "upload the excel to zdms_deal_opbal
               r1 RADIOBUTTON GROUP g1,                     "customer posting
               r2 RADIOBUTTON GROUP g1,                     "business area update
               r4 RADIOBUTTON GROUP g1.                     "zdms_deal_opbal table display
SELECTION-SCREEN : END OF BLOCK b1.

**********Events**********
START-OF-SELECTION.
  IF r1 = abap_true.                    "upload the excel to zdms_deal_opbal
    PERFORM : process.
    PERFORM : alv_display.
  ELSEIF r2 = abap_true.                "customer posting
    PERFORM : barea_update.
  ELSEIF r3 = abap_true.                "business area update
    PERFORM : excel_upload.
  ELSEIF r4 = abap_true.                "zdms_deal_opbal table display
    PERFORM : display.
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

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF r4 = 'X' AND screen-name CS 'SO_KUNNR'.        "enable distributor fields
      screen-active = 1.
    ELSEIF r4 = abap_false AND screen-name CS 'SO_KUNNR' AND r2 = abap_false.         "disable distributor fields
      screen-active = 0.
    ELSEIF r3 = 'X' AND screen-name CS 'P_FNAME'.
      screen-active = 1.
    ELSEIF r3 = abap_false AND screen-name CS 'P_FNAME'.
      screen-active = 0.
    ELSEIF r2 = 'X' AND screen-name CS 'SO_KUNNR'.
      screen-active = 1.
    ELSEIF r1 = 'X' AND screen-name CS 'P_CHK3'.
      screen-active = 1.
    ELSEIF r1 = abap_false AND screen-name CS 'P_CHK3'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

END-OF-SELECTION.
*----------------------------------------------------------------------*
FORM excel_upload.
  TYPES : BEGIN OF ty_excel,
            dist   TYPE zdist,
            dealer TYPE kunnr,
            amnt   TYPE bsad-dmbtr,
            type   TYPE char2,
          END OF ty_excel.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         lt_raw   TYPE truxs_t_text_data.
  DATA : ls_dealer TYPE zdms_deal_opbal.
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
    SORT lt_excel BY dist dealer.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
**************data process**************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_exc>).
****************alpha input conversion****************
      <fs_exc>-dealer = |{ <fs_exc>-dealer ALPHA = IN }|.
      <fs_exc>-dist   = |{ <fs_exc>-dist ALPHA = IN }|.
      ls_dealer-zdate = gv_date.
      ls_dealer-dealer = <fs_exc>-dealer.
************check the Amount**************
      ls_dealer-dms_amt = <fs_exc>-amnt.
      IF <fs_exc>-amnt LT 0.
        ls_dealer-amt = <fs_exc>-amnt * -1 .
      ELSE.
        ls_dealer-amt = <fs_exc>-amnt.
      ENDIF.
****************check the distributor code****************
      READ TABLE lt_kna1 INTO DATA(ls_kna2) WITH KEY kunnr = <fs_exc>-dist BINARY SEARCH.
      ls_dealer-b_area      = ls_kna2-werks.
      ls_dealer-distributor = <fs_exc>-dist.
      ls_dealer-type        = <fs_exc>-type.
************check the document type**************
      IF <fs_exc>-type = 'DR'.
        ls_dealer-doc_type = 'RV'.
      ELSEIF <fs_exc>-type = 'CR'.
        ls_dealer-doc_type = 'DZ'.
      ENDIF.

      IF ls_dealer-distributor IS NOT INITIAL AND ls_dealer-dealer IS NOT INITIAL.
        MODIFY zdms_deal_opbal FROM ls_dealer.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer,ls_kna2.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Data uploaded successfully' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.
FORM process.
  DATA : ls_dealer TYPE zdms_deal_opbal.
  DATA : lv_fisyear TYPE bapi0002_4-fiscal_year,
         lv_month   TYPE bapi0002_4-fiscal_period,
         l_return   TYPE bapireturn1.

  DATA : lv_msg TYPE string,
         lv_gl  TYPE  hkont.
*************customer block check***********
  DATA : lo_block TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_block.
  DATA : lt_block TYPE TABLE OF zsd_st_cust_block.

  REFRESH gt_alv.
  SELECT * FROM zdms_deal_opbal INTO TABLE @DATA(lt_dealer) WHERE status IN ( ' ' , 'E' ).
  IF sy-subrc NE 0.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_dealer BY distributor dealer status.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
****************fetch the incomming GL details************
    SELECT kunnr,werks,incgl FROM zdms_distb_gl INTO TABLE @DATA(lt_glact).
    IF sy-subrc = 0.
      SORT lt_glact BY kunnr werks.
    ENDIF.
****************fetch the GL ACNT details************
    SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat).
    IF sy-subrc = 0.
      SORT lt_skat BY saknr ktopl.
    ENDIF.
**************call customer check*********
    REFRESH : lt_block.
    lo_block->customer_block_check(
      EXPORTING
        bukrs                     = '1000'      " Company Code
        vkorg                     = '1000'      " Sales Organization
      CHANGING
        cust_tab                  = lt_block    " Table Type for Customer Block Check
        ).
    IF lt_block IS NOT INITIAL.
      SORT : lt_block BY kunnr block.
    ENDIF.
*****************fetch the retailer sales details************
*    SELECT a~kunnr,a~name1,a~werks,b~loevm,b~vkorg,b~vtweg,b~spart FROM kna1 AS a
*                                           INNER JOIN knvv AS b ON a~kunnr = b~kunnr
*                                           INTO TABLE @DATA(lt_kna2)
*                                           FOR ALL ENTRIES IN @lt_dealer
*                                           WHERE a~kunnr = @lt_dealer-dealer.
*    IF sy-subrc = 0.
*      SORT lt_kna2 BY kunnr vkorg vtweg spart.
*    ENDIF.
**************data process**************
    LOOP AT lt_dealer ASSIGNING FIELD-SYMBOL(<fs_exc>).

*****************check distributor block or not*******************
      IF lt_block IS NOT INITIAL.
        READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_exc>-distributor
                                                            block = abap_true BINARY SEARCH.
        IF sy-subrc = 0.
          lv_msg = | Distributor is blocked , { lv_msg } |.
        ENDIF.
      ENDIF.
****************check the dealer code****************
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_exc>-dealer BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Dealer code is incorrect , { lv_msg }|.
      ENDIF.
      IF <fs_exc>-amt IS INITIAL.
        lv_msg = |Amount is zero , { lv_msg }|.
      ENDIF.
****************check the distributor code****************
      READ TABLE lt_kna1 TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_exc>-distributor BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Distributor code is incorrect , { lv_msg }|.
      ENDIF.
      "Business Area checks
      IF <fs_exc>-b_area IS INITIAL.
        lv_msg = |Business area is missing , { lv_msg }|.
      ENDIF.
      IF <fs_exc>-doc_type IS INITIAL.
        lv_msg = |Doc type is Incorrect , { lv_msg }|.
      ENDIF.
*****************check the GL account***************
      IF <fs_exc>-doc_type = 'DZ'.
        READ TABLE lt_glact INTO DATA(ls_glact) WITH KEY kunnr = <fs_exc>-distributor
                                                         werks = <fs_exc>-b_area BINARY SEARCH.
        IF sy-subrc = 0.
          lv_gl = ls_glact-incgl.
          else.
            lv_msg = |GL Account not maintained in zdms_distb_gl , { lv_msg }|.
        ENDIF.
      ELSEIF <fs_exc>-doc_type = 'RV'.
        lv_gl = '31100001'.
      ENDIF.
      lv_gl = |{ lv_gl ALPHA = IN }|.
*      "GL Account existence checks
      READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = lv_gl
                                                     ktopl = 'SDMS' BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Incorrect GL Account Number , { lv_msg }|.
      ENDIF.
*** function module to get fiscal year ***
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = gv_bukrs
          posting_date  = gv_date
        IMPORTING
          fiscal_year   = lv_fisyear
          fiscal_period = lv_month
          return        = l_return.
      "Reference Document Checks
      IF <fs_exc>-doc_no IS NOT INITIAL AND <fs_exc>-status = 'S'.
        lv_msg = |Already Document with same reference Available , { lv_msg }|.
      ENDIF.

      IF lv_msg IS INITIAL.
        "Adding the Success Msg
        APPEND VALUE #( distributor = <fs_exc>-distributor
                        account     = <fs_exc>-dealer
                        name1       = ls_kna1-name1
                        acctyp      = 'D'
                        blart       = <fs_exc>-doc_type
                        refdoc      = <fs_exc>-dealer
                        fisyear     = lv_fisyear
                        docdate     = <fs_exc>-zdate
                        pdate       = <fs_exc>-zdate
                        gl_account  = lv_gl
                        glacc_txt   = ls_skat-txt50
                        amt         = <fs_exc>-dms_amt
                        item_txt    = |opening outstanding of dealer - { <fs_exc>-dealer } |
                        gsber       = <fs_exc>-b_area
                        msgtyp      = 'S'
                        message     = |No errror in lineitems| ) TO gt_alv.
      ELSE.
        "Adding the Error Msg
        APPEND VALUE #( distributor = <fs_exc>-distributor
                        account     = <fs_exc>-dealer
                        name1       = ls_kna1-name1
                        acctyp      = 'D'
                        blart       = <fs_exc>-doc_type
                        refdoc      = <fs_exc>-dealer
                        fisyear     = lv_fisyear
                        docdate     = <fs_exc>-zdate
                        pdate       = <fs_exc>-zdate
                        gl_account  = lv_gl
                        glacc_txt   = ls_skat-txt50
                        amt         = <fs_exc>-dms_amt
                        item_txt    = |opening outstanding of dealer - { <fs_exc>-dealer } |
                        gsber       = <fs_exc>-b_area
                        msgtyp      = 'E'
                        message     = lv_msg ) TO gt_alv.
***********update the error status in table*************
        IF <fs_exc>-doc_no IS NOT INITIAL OR <fs_exc>-amt = 0.
          UPDATE zdms_deal_opbal SET status = 'S' message = lv_msg
                                 WHERE distributor = <fs_exc>-distributor
                                 AND   dealer      = <fs_exc>-dealer.
        ELSE.
          UPDATE zdms_deal_opbal SET status = 'E' message = lv_msg
                                 WHERE distributor = <fs_exc>-distributor
                                 AND   dealer      = <fs_exc>-dealer.
        ENDIF.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer,ls_kna1,ls_glact,ls_skat,lv_msg,lv_gl.
    ENDLOOP.
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
          row.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
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

    TRY.
        lo_column ?= lo_columns->get_column( 'CHECKBOX' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Checkbox' ).
        lo_column->set_medium_text( 'Checkbox' ).
        lo_column->set_short_text( 'Checkbox' ).
*        lo_column->set
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>checkbox_hotspot ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ACCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Account code' ).
        lo_column->set_medium_text( 'Account' ).
        lo_column->set_short_text( 'Account' ).
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
  DATA: lv_belnr TYPE belnr_d.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S' AND checkbox = abap_true.
    DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = gv_bukrs
                                        doc_date   = <fs_alv>-docdate
                                        pstng_date = <fs_alv>-pdate
                                        ref_doc_no = <fs_alv>-refdoc
                                        fisc_year  = <fs_alv>-fisyear
                                        doc_type   = <fs_alv>-blart ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return.

    lt_recievable = VALUE #( ( itemno_acc = '1'
                               customer   = <fs_alv>-account
                               item_text  = <fs_alv>-item_txt
                               bus_area   = <fs_alv>-gsber  ) ).

    lt_glaccount = VALUE #( ( itemno_acc = '2'
                              gl_account = <fs_alv>-gl_account
                              item_text  = <fs_alv>-item_txt
                              bus_area   = <fs_alv>-gsber ) ).

    lt_curramnt = VALUE #( ( itemno_acc = '1'
                             currency   = 'INR'
                             amt_doccur = <fs_alv>-amt )
                           ( itemno_acc = '2'
                             currency   = 'INR'
                             amt_doccur = ( <fs_alv>-amt ) * -1 ) ).

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
    READ TABLE lt_return INTO DATA(lw_ret) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CLEAR lv_msg_text.
      LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
        lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
      ENDLOOP.
      <fs_alv>-msgtyp  = 'E'.
      <fs_alv>-message = lv_msg_text.
      UPDATE zdms_deal_opbal SET   status      = 'E' message = <fs_alv>-message
                             WHERE distributor = <fs_alv>-distributor
                             AND   dealer      = <fs_alv>-account.
    ELSE.
      IF p_chk3 NE abap_true.
        REFRESH: lt_return.
        CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
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
        COMMIT WORK AND WAIT.
        CLEAR lv_belnr.
        lv_belnr         = lv_objkey+0(10).
        <fs_alv>-docno   = lv_belnr.
        <fs_alv>-msgtyp  = 'S'.
        <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
        UPDATE zdms_deal_opbal SET   status      = 'S' message = <fs_alv>-message doc_no = lv_belnr
                               WHERE distributor = <fs_alv>-distributor
                               AND   dealer      = <fs_alv>-account.
      ELSE.
        <fs_alv>-msgtyp  = 'S'.
        <fs_alv>-message = |Document Ready to Post|.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE : 'Please select any one checkbox' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM barea_update.

  SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.

    SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1)
                       FOR ALL ENTRIES IN @lt_t001k
    WHERE werks = @lt_t001k-bwkey
    AND   kunnr IN @so_kunnr.

    LOOP AT lt_kna1 INTO DATA(ls_kna2).
      UPDATE zdms_deal_opbal SET b_area = ls_kna2-werks WHERE distributor = ls_kna2-kunnr.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Business Area Updated successfully' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.
FORM display .

  SELECT * FROM zdms_deal_opbal INTO TABLE @DATA(lt_dealop) WHERE distributor IN @so_kunnr.
  IF sy-subrc = 0.

    DATA : ls_layo TYPE slis_layout_alv.
    ls_layo-colwidth_optimize = abap_true.
***********call the alv screen************
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_structure_name = 'ZDMS_DEAL_OPBAL'
        is_layout        = ls_layo
*       IT_FIELDCAT      =
      TABLES
        t_outtab         = lt_dealop.
  ELSE.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
