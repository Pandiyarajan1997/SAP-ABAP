*&---------------------------------------------------------------------*
*& Include          ZDMS_OPENING_BALANCE_POST_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function. "Event for User command

    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display


ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.

  METHOD on_double_click.
    IF column EQ 'DOCNO'.
      READ TABLE gt_alv INTO DATA(wa_alv) INDEX  row.
      SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs EQ @p_bukrs
                                                  AND belnr EQ @wa_alv-docno
                                                  AND gjahr EQ @wa_alv-fisyear.
      IF sy-subrc = 0. " Exists?
        SET PARAMETER ID 'BLN' FIELD wa_alv-docno. "Document Number

        SET PARAMETER ID 'BUK' FIELD p_bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_alv-fisyear. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_posting DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING l_bukrs TYPE bukrs
                                   l_fname TYPE rlgrap-filename,
      excel_conversion,
      actual_process,
      alv_display.

    DATA: gv_bukrs TYPE bukrs,
          gv_fname TYPE rlgrap-filename.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_input,
             account(10) TYPE c,
             doctyp      TYPE blart,
             busarea     TYPE gsber,
             refdoc      TYPE xblnr1,
             docdate     TYPE bldat,
             pdate       TYPE budat,
             gl_account  TYPE saknr,
             amt         TYPE wrbtr,
             item_txt    TYPE sgtxt,
           END OF ty_input.
    DATA: gt_excel TYPE TABLE OF ty_input.
    DATA: gt_type TYPE truxs_t_text_data.
ENDCLASS.

CLASS lcl_posting IMPLEMENTATION.

  METHOD constructor.
    CLEAR: gv_bukrs,gv_fname.
    gv_bukrs = l_bukrs.
    gv_fname = l_fname.
  ENDMETHOD.

  METHOD excel_conversion.
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = gt_type
        i_filename           = gv_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD actual_process.

    DATA: l_lifnr   TYPE elifn,
          l_kunnr   TYPE kunnr,
          l_venname TYPE name1_gp,
          l_cusname TYPE name1_gp.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.

    REFRESH gt_alv.

    LOOP AT me->gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
      CLEAR: l_lifnr,l_kunnr,l_venname,l_cusname.
      IF <fs_excel>-account IS NOT INITIAL.
        DATA(l_account) = |{ <fs_excel>-account ALPHA = IN }|.
      ENDIF.
      DATA(l_glacc) = |{ <fs_excel>-gl_account ALPHA = IN }|.
*** function module to get fiscal year ***
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = gv_bukrs
          posting_date  = <fs_excel>-docdate
        IMPORTING
          fiscal_year   = lv_fisyear
          fiscal_period = lv_month
          return        = l_return.

      IF p_rad1 = abap_true. "Vendor Posting
        "Checking Vendor Number
        SELECT SINGLE lifnr, name1 FROM lfa1 INTO ( @l_lifnr , @l_venname ) WHERE lifnr = @l_account.
        IF sy-subrc NE 0.
          APPEND VALUE #( account = l_account
                          acctyp = 'S'
                          msgtyp = 'E'
                          message = |Incorrect Vendor Number| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "GL Account existence checks
        SELECT SINGLE saknr, txt50 FROM skat INTO @DATA(l_glacc_dtls) WHERE saknr = @l_glacc.
        IF sy-subrc NE 0.
          APPEND VALUE #( account = l_account
                          name1   = l_venname
                          acctyp  = 'K'
                          refdoc  = <fs_excel>-refdoc
                          msgtyp  = 'E'
                          message = |Incorrect GL Account Number| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "Reference Document Checks
        SELECT SINGLE * FROM bsip INTO @DATA(ls_bsip) WHERE bukrs = @gv_bukrs AND lifnr = @l_lifnr
                                                      AND gjahr = @lv_fisyear
                                                      AND xblnr = @<fs_excel>-refdoc.
        IF sy-subrc = 0.
          APPEND VALUE #( account = l_account
                          name1   = l_venname
                          acctyp  = 'K'
                          refdoc  = <fs_excel>-refdoc
                          msgtyp  = 'E'
                          message = |Already Document with same reference Available| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "Business Area checks
        IF <fs_excel>-busarea IS NOT INITIAL.
          SELECT SINGLE * FROM tgsbt INTO @DATA(l_busarea) WHERE spras = @sy-langu
                                                           AND gsber = @<fs_excel>-busarea.
          IF sy-subrc NE 0.
            APPEND VALUE #( account = l_account
                            name1   = l_venname
                            acctyp  = 'K'
                            refdoc  = <fs_excel>-refdoc
                            gl_account = l_glacc_dtls-saknr
                            glacc_txt = l_glacc_dtls-txt50
                            msgtyp  = 'E'
                            message = |Incorrect Business Area| ) TO gt_alv.
            CONTINUE.
          ENDIF.
        ENDIF.
        "Adding the Success Msg
        APPEND VALUE #( account = l_lifnr
                        name1 = l_venname
                        acctyp = 'K'
                        blart = <fs_excel>-doctyp
                        refdoc = <fs_excel>-refdoc
                        fisyear = lv_fisyear
                        docdate = <fs_excel>-docdate
                        pdate = <fs_excel>-pdate
                        gl_account = l_glacc_dtls-saknr
                        glacc_txt = l_glacc_dtls-txt50
                        amt = <fs_excel>-amt
                        item_txt = <fs_excel>-item_txt
                        msgtyp = 'S'
                        message = |No errror in lineitems| ) TO gt_alv.

      ELSE.
        "Checking Customer Number
        SELECT SINGLE kunnr, name1 FROM kna1 INTO ( @l_kunnr, @l_cusname ) WHERE kunnr = @l_account.
        IF sy-subrc NE 0.
          APPEND VALUE #( account = l_account
                          acctyp = 'S'
                          msgtyp = 'E'
                          message = |Incorrect Customer Number| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "GL Account existence checks
        SELECT SINGLE saknr, txt50 FROM skat INTO @l_glacc_dtls WHERE saknr = @l_glacc.
        IF sy-subrc NE 0.
          APPEND VALUE #( account = l_account
                          name1   = l_cusname
                          acctyp  = 'S'
                          refdoc  = <fs_excel>-refdoc
                          msgtyp  = 'E'
                          message = |Incorrect GL Account Number| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "Reference Document Checks
        SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs = @gv_bukrs
                                                      AND gjahr = @lv_fisyear
                                                      AND xblnr = @<fs_excel>-refdoc.
        IF sy-subrc = 0.
          APPEND VALUE #( account = l_account
                          name1   = l_cusname
                          acctyp  = 'K'
                          refdoc  = <fs_excel>-refdoc
                          msgtyp  = 'E'
                          message = |Already Document with same reference Available| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        IF <fs_excel>-busarea IS NOT INITIAL.
          "Business Area checks
          SELECT SINGLE * FROM tgsbt INTO @l_busarea WHERE spras = @sy-langu
                                                     AND gsber = @<fs_excel>-busarea.
          IF sy-subrc NE 0.
            APPEND VALUE #( account = l_account
                            name1   = l_cusname
                            acctyp  = 'K'
                            refdoc  = <fs_excel>-refdoc
                            gl_account = l_glacc_dtls-saknr
                            glacc_txt = l_glacc_dtls-txt50
                            msgtyp  = 'E'
                            message = |Incorrect Business Area| ) TO gt_alv.
            CONTINUE.
          ENDIF.
        ENDIF.
        "Adding the Success Msg
        APPEND VALUE #( account = l_kunnr
                        name1 = l_cusname
                        acctyp = 'S'
                        blart = <fs_excel>-doctyp
                        refdoc = <fs_excel>-refdoc
                        docdate = <fs_excel>-docdate
                        pdate = <fs_excel>-pdate
                        gl_account = l_glacc_dtls-saknr
                        glacc_txt = l_glacc_dtls-txt50
                        amt = <fs_excel>-amt
                        item_txt = <fs_excel>-item_txt
                        msgtyp = 'S'
                        message = |No errror in lineitems| ) TO gt_alv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD alv_display.
    DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_content     TYPE REF TO cl_salv_form_element.

    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.
    DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
    DATA: lr_groups TYPE REF TO cl_salv_sorts .
    DATA: toolbar TYPE REF TO cl_salv_functions_list .
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
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.

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

    lo_gr_alv->display( ).
  ENDMETHOD.

ENDCLASS.
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN '&INV_POST'.
      PERFORM f_posting.
      lo_gr_alv->refresh( ).
  ENDCASE.
ENDFORM.
FORM f_posting.
  DATA: lv_fund TYPE bapiacgl09-fund.
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

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S'.
    DATA(l_headers) = VALUE bapiache09( bus_act = 'RFBU'
                                        username = sy-uname
                                        comp_code = p_bukrs
                                        doc_date = <fs_alv>-docdate
                                        pstng_date = <fs_alv>-pdate
                                        ref_doc_no = <fs_alv>-refdoc
                                        fisc_year = <fs_alv>-fisyear
                                        doc_type = <fs_alv>-blart ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return.

    IF p_rad1 = abap_true.
      lt_payable = VALUE #( ( itemno_acc = '1'
                              vendor_no = <fs_alv>-account
                              item_text = <fs_alv>-item_txt
                              bus_area = <fs_alv>-gsber  ) ).

      lt_glaccount = VALUE #( ( itemno_acc = '2'
                                gl_account = <fs_alv>-gl_account
                                item_text = <fs_alv>-item_txt
                                bus_area = <fs_alv>-gsber ) ).

      lt_curramnt = VALUE #( ( itemno_acc = '1'
                               currency = 'INR'
                               amt_doccur = ( <fs_alv>-amt * -1 ) )
                             ( itemno_acc = '2'
                               currency = 'INR'
                               amt_doccur = <fs_alv>-amt ) ).

    ELSE.
      lt_recievable = VALUE #( ( itemno_acc = '1'
                                 customer = <fs_alv>-account
                                 item_text = <fs_alv>-item_txt
                                 bus_area = <fs_alv>-gsber  ) ).

      lt_glaccount  = VALUE #( ( itemno_acc = '2'
                                 gl_account = <fs_alv>-gl_account
                                 item_text = <fs_alv>-item_txt
                                 bus_area = <fs_alv>-gsber ) ).

      lt_curramnt = VALUE #( (   itemno_acc = '1'
                                 currency = 'INR'
                                 amt_doccur =  <fs_alv>-amt )
                             (   itemno_acc = '2'
                                 currency = 'INR'
                                 amt_doccur = ( <fs_alv>-amt * -1 ) ) ).
    ENDIF.

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
      <fs_alv>-msgtyp = 'E'.
      <fs_alv>-message = lv_msg_text.
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
        lv_belnr = lv_objkey+0(10).
        <fs_alv>-docno = lv_belnr.
        <fs_alv>-msgtyp = 'S'.
        <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
      ELSE.
        <fs_alv>-msgtyp = 'S'.
        <fs_alv>-message = |Document Ready to Post|.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form screen_adjust
*&---------------------------------------------------------------------*
FORM screen_adjust .
  IF sy-tcode = 'ZCUST_DBT_NOTE'.
    LOOP AT SCREEN.
      IF screen-group1 = 'BL1'.
        screen-input = 0.
        screen-invisible = 1.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
