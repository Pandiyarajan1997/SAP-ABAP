*&---------------------------------------------------------------------*
*& Include          ZSD_BP_CORRECTION_CLS
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS: f4_help, convert_excel_to_sap, update_process, build_alv.

  PRIVATE SECTION.
*Excel Structure
    TYPES: BEGIN OF ty_excel,
             bpartner TYPE bu_partner,
             f1       TYPE string,
             f2       TYPE string,
             f3       TYPE string,
             f4       TYPE string,
           END OF ty_excel.
    DATA: gt_exc_data TYPE TABLE OF ty_excel.
    DATA: gt_type  TYPE truxs_t_text_data.

    TYPES: BEGIN OF ty_alv,
             bpartner TYPE bu_partner,
             type(1)  TYPE c,
             msg(150) TYPE c,
           END OF ty_alv.
    DATA: gt_log_report TYPE TABLE OF ty_alv,
          gs_log_report TYPE ty_alv.

ENDCLASS.
CLASS lcl_main IMPLEMENTATION.
*F4 File help
  METHOD f4_help.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        program_name  = syst-repid
        dynpro_number = syst-dynnr
        field_name    = ' '
        static        = ' '
        mask          = ' '
      CHANGING
        file_name     = p_fname.
  ENDMETHOD.
*Excel Conversion to SAP
  METHOD convert_excel_to_sap.
    REFRESH: me->gt_exc_data.
    IF p_fname IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
*         I_FIELD_SEPERATOR    =
          i_line_header        = 'X'
          i_tab_raw_data       = gt_type
          i_filename           = p_fname
        TABLES
          i_tab_converted_data = me->gt_exc_data
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Conversion of excel failed' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'File is Mandatory' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
*Update Process
  METHOD update_process.

    DATA: lt_error_tab TYPE bapiretm,
          lt_main_data TYPE cvis_ei_extern_t,
          ls_main_data TYPE cvis_ei_extern,
          l_header     TYPE bus_ei_header,
          l_msg        TYPE string,
          l_grpfeature TYPE bapi_str_bupa_fs_treasury,
          lt_return    TYPE TABLE OF bapiret2,
          lt_mail      TYPE TABLE OF bapiadsmtp,
          lt_mailx     TYPE TABLE OF bapiadsmtx,
          ls_mail      TYPE bapiadsmtp,
          ls_mailx     TYPE bapiadsmtx,
          l_address    TYPE bapibus1006_address,
          ls_tel       TYPE bapiadtel,
          lt_tel       TYPE TABLE OF bapiadtel,
          ls_telx      TYPE bapiadtelx,
          lt_telx      TYPE TABLE OF bapiadtelx.

    REFRESH: gt_log_report.
    LOOP AT me->gt_exc_data ASSIGNING FIELD-SYMBOL(<fs_exc_data>).
      CLEAR: gs_log_report.
*Business Partner Number Missing
      IF <fs_exc_data>-bpartner IS INITIAL.
        gs_log_report-bpartner = <fs_exc_data>-bpartner.
        gs_log_report-type     = 'E'.
        gs_log_report-msg      = TEXT-002.
        APPEND gs_log_report TO gt_log_report.
        CONTINUE.
      ENDIF.

      IF p_r1 = abap_true.
*Vendor to Business Partner Number Check
        DATA(lv_vendor) = <fs_exc_data>-bpartner.
        lv_vendor = |{ lv_vendor  ALPHA = IN }|.
        SELECT SINGLE * FROM but000
          INTO @DATA(l_bpartner)
          WHERE partner = @lv_vendor.
        IF sy-subrc NE 0.
          gs_log_report-bpartner = lv_vendor.
          gs_log_report-type     = 'E'.
          gs_log_report-msg      = TEXT-003.
          APPEND gs_log_report TO gt_log_report.
          CONTINUE.
        ENDIF.
        DATA(lv_grpfeature) = CONV bp_group_feature( <fs_exc_data>-f1 ).
        SELECT SINGLE group_feature FROM tp24
          INTO @DATA(l_groupfeature)
          WHERE group_feature = @lv_grpfeature.
        IF sy-subrc NE 0.
          gs_log_report-bpartner = l_bpartner-partner.
          gs_log_report-type     = 'E'.
          gs_log_report-msg      = TEXT-004.
          APPEND gs_log_report TO gt_log_report.
          CONTINUE.
        ELSE.
          CLEAR: l_grpfeature,lt_return.
          CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_GET'
            EXPORTING
              businesspartner = l_bpartner-partner
            IMPORTING
              fsattributes    = l_grpfeature
            TABLES
              return          = lt_return.
          LOOP AT lt_return INTO DATA(lw_return) WHERE ( type = 'E' OR type = 'A' ).

          ENDLOOP.
          IF sy-subrc NE 0.
            l_grpfeature-group_feature = l_groupfeature.
            DATA(l_grpfeaturex) = VALUE bapi_str_bupa_fs_treasury2_x( group_feature = abap_true ).
            CLEAR lt_return.
            CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_SET'
              EXPORTING
                businesspartner = l_bpartner-partner
                attributes      = l_grpfeature
                attributesx     = l_grpfeaturex
              TABLES
                return          = lt_return.
            CLEAR l_msg.
            LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
              l_msg = |{ l_msg }/{ lw_return-message }|.
            ENDLOOP.
            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              gs_log_report-bpartner = l_bpartner-partner.
              gs_log_report-type     = 'S'.
              gs_log_report-msg      = |Grouping Feature Updated for business Partner|.
              APPEND gs_log_report TO gt_log_report.
            ENDIF.
            IF l_msg IS NOT INITIAL.
              gs_log_report-bpartner = l_bpartner-partner.
              gs_log_report-type     = 'E'.
              gs_log_report-msg      = l_msg.
              APPEND gs_log_report TO gt_log_report.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
*GST Number Updation
      ELSEIF p_r2 = abap_true.
        DATA(lv_partner) = <fs_exc_data>-bpartner.
        lv_partner = |{ lv_partner ALPHA = IN }|.
        SELECT SINGLE * FROM dfkkbptaxnum
          INTO @DATA(l_gst)
          WHERE partner = @lv_partner.
        IF sy-subrc NE 0.
          CLEAR lt_return.
          CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
            EXPORTING
              businesspartner = lv_partner
              taxtype         = 'IN3'
              taxnumber       = CONV bapibus1006tax-taxnumber( <fs_exc_data>-f1 )
            TABLES
              return          = lt_return.
          CLEAR l_msg.
          LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
            l_msg = |{ l_msg }/{ lw_return-message }|.
          ENDLOOP.
          IF sy-subrc NE 0.
            COMMIT WORK.
            WAIT UP TO 2 SECONDS.
            gs_log_report-bpartner = lv_partner.
            gs_log_report-type     = 'S'.
            gs_log_report-msg      = |GST Number added for business Partner|.
            APPEND gs_log_report TO gt_log_report.
          ENDIF.
          IF l_msg IS NOT INITIAL.
            gs_log_report-bpartner = l_bpartner-partner.
            gs_log_report-type     = 'E'.
            gs_log_report-msg      = l_msg.
            APPEND gs_log_report TO gt_log_report.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR lt_return.
          CALL FUNCTION 'BAPI_BUPA_TAX_CHANGE'
            EXPORTING
              businesspartner = lv_partner
              taxtype         = 'IN3'
              taxnumber       = CONV bapibus1006tax-taxnumber( <fs_exc_data>-f1 )
            TABLES
              return          = lt_return.
          CLEAR l_msg.
          LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
            l_msg = |{ l_msg }/{ lw_return-message }|.
          ENDLOOP.
          IF sy-subrc NE 0.
            COMMIT WORK.
            WAIT UP TO 5 SECONDS.
            gs_log_report-bpartner = lv_partner.
            gs_log_report-type     = 'S'.
            gs_log_report-msg      = |GST Number Updated for business Partner|.
            APPEND gs_log_report TO gt_log_report.
          ENDIF.
          IF l_msg IS NOT INITIAL.
            gs_log_report-bpartner = l_bpartner-partner.
            gs_log_report-type     = 'E'.
            gs_log_report-msg      = l_msg.
            APPEND gs_log_report TO gt_log_report.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        lv_partner = <fs_exc_data>-bpartner.
        lv_partner = |{ lv_partner ALPHA = IN }|.
        SELECT SINGLE partner FROM but000
          INTO @DATA(l_buspartner)
          WHERE partner = @lv_partner.
        IF sy-subrc NE 0.
          gs_log_report-bpartner = l_buspartner.
          gs_log_report-type     = 'E'.
          gs_log_report-msg      = |Incorrect Business Partner Number|.
          APPEND gs_log_report TO gt_log_report.
          CONTINUE.
        ELSE.
          CLEAR : lt_mail,lt_mailx,ls_mail,ls_mailx.
          ls_mail-e_mail = CONV ad_smtpadr( <fs_exc_data>-f1 ).
*          ls_mail-std_no = abap_true.
*          ls_mail-std_recip = abap_true.
          APPEND ls_mail TO lt_mail.
          ls_mailx-e_mail = abap_true.
*          ls_mailx-std_no = abap_true.
*          ls_mailx-std_recip = abap_true.
*          ls_mailx-updateflag = 'I'.
*          APPEND ls_mailx TO lt_mailx.
*          ls_telx-telephone = 'X'.
*          ls_telx-updateflag = 'I'.
*          APPEND ls_telx TO lt_telx.
          CLEAR l_address.
          CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
            EXPORTING
              businesspartner = l_buspartner
            IMPORTING
              addressdata     = l_address
            TABLES
              return          = lt_return
              bapiadtel       = lt_tel.

          CLEAR lt_return.
          CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
            EXPORTING
              businesspartner        = l_buspartner
              addressdata            = l_address
              duplicate_message_type = 'W'
            TABLES
              bapiadsmtp             = lt_mail
              bapiadtel              = lt_tel
              return                 = lt_return.
          CLEAR l_msg.
          LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
            l_msg = |{ l_msg }/{ lw_return-message }|.
          ENDLOOP.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            CLEAR lt_return.
*            CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
*              EXPORTING
*                businesspartner       = l_buspartner
*              TABLES
*                e_maildatanonaddress  = lt_mail
*                e_maildatanonaddressx = lt_mailx
**               telefondatanonaddress = lt_tel
**               telefondatanonaddressx = lt_telx
*                return                = lt_return.
*            CLEAR l_msg.
*            LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
*              l_msg = |{ l_msg }/{ lw_return-message }|.
*            ENDLOOP.
*            IF sy-subrc NE 0.
*              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                EXPORTING
*                  wait = 'X'.
              gs_log_report-bpartner = lv_partner.
              gs_log_report-type     = 'S'.
              gs_log_report-msg      = |Email ID Updated for business Partner|.
              APPEND gs_log_report TO gt_log_report.
*            ENDIF.
*            IF l_msg IS NOT INITIAL.
*              gs_log_report-bpartner = lv_partner.
*              gs_log_report-type     = 'E'.
*              gs_log_report-msg      = l_msg.
*              APPEND gs_log_report TO gt_log_report.
*              CONTINUE.
*            ENDIF.
          ENDIF.
          IF l_msg IS NOT INITIAL.
            gs_log_report-bpartner = lv_partner.
            gs_log_report-type     = 'E'.
            gs_log_report-msg      = l_msg.
            APPEND gs_log_report TO gt_log_report.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: lv_partner.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_display      TYPE REF TO cl_salv_display_settings,
          lo_columns      TYPE REF TO cl_salv_columns,
          lo_column       TYPE REF TO cl_salv_column_table,
          lo_gr_functions TYPE REF TO cl_salv_functions_list.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = me->gt_log_report.
      CATCH cx_salv_msg.
    ENDTRY.

    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'BPARTNER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Business Partner' ).
        lo_column->set_medium_text( 'Bpartner' ).
        lo_column->set_short_text( 'BusPartner' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Msg type' ).
        lo_column->set_medium_text( 'Msg type' ).
        lo_column->set_short_text( 'Msgtyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSG' ).
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
