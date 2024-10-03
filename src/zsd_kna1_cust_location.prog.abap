*&---------------------------------------------------------------------*
*& Report ZMM_SERVICE_PR_CREATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*changed by : T.Pandiarajan
*changed on : 05.10.2023
*Ref By     : Ramakrishnan & Praveen
**************************************************************************

REPORT zsd_kna1_cust_location.
INCLUDE zsd_kna1_cust_location_cls.
*INCLUDE zmm_service_pr_creation_cls.
TYPES: BEGIN OF ty_file,

         kunnr      TYPE kunnr,
         zlatitude  TYPE zhrlatit,
         zlongitude TYPE zhrlong,
         zdays      TYPE zdays,
         zcapacity  TYPE zcapacity,
         message    TYPE bapi_msg,
       END OF ty_file.
TYPES: BEGIN OF ty_output,
         msgtype TYPE msgty,
         message TYPE bapi_msg,
       END OF ty_output.
DATA : lt_up_file    TYPE TABLE OF ty_file,
       ls_up_file    TYPE ty_file,
       lt_popup_file TYPE TABLE OF ty_output,
       ls_popup_file TYPE ty_output.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  PARAMETERS: fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = fname.

START-OF-SELECTION.

  PERFORM f_read_uploadfile.

  PERFORM f_display_output.
*&---------------------------------------------------------------------*
*& Form f_read_uploadfile
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_read_uploadfile .
  DATA: mtable TYPE TABLE OF alsmex_tabline WITH HEADER LINE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename    = fname
      i_begin_col = 1
      i_begin_row = 2
      i_end_col   = 5
      i_end_row   = 99999
    TABLES
      intern      = mtable.


  LOOP AT mtable.
    CASE mtable-col.
      WHEN 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = mtable-value
          IMPORTING
            output = ls_up_file-kunnr.
      WHEN 2.
        ls_up_file-zlatitude = mtable-value.
      WHEN 3.
        ls_up_file-zlongitude = mtable-value.
      WHEN 4.

        ls_up_file-zdays = mtable-value.

      WHEN 5.

        ls_up_file-zcapacity = mtable-value.

      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      APPEND ls_up_file TO lt_up_file.
      CLEAR :ls_up_file,mtable.
    ENDAT.
  ENDLOOP.
  IF lt_up_file[] IS INITIAL.
    DATA(lv_err) = 'No data found'.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT lt_up_file ASSIGNING FIELD-SYMBOL(<fs_file>).

*-----------------------------------------------------------------------------------------
*Contains all validations

      IF <fs_file>-zlatitude IS INITIAL AND <fs_file>-zlongitude IS INITIAL AND <fs_file>-zdays IS INITIAL AND <fs_file>-zcapacity IS INITIAL.
        ls_popup_file-msgtype = 'E'.
        ls_popup_file-message = |Line no.{ sy-tabix } - { <fs_file>-kunnr } Latitude / Langitude / days is empty|.
        APPEND ls_popup_file TO lt_popup_file.
      ENDIF.

      IF <fs_file>-zlatitude IS INITIAL AND <fs_file>-zlongitude IS NOT INITIAL.

        ls_popup_file-msgtype = 'E'.
        ls_popup_file-message = |Line no.{ sy-tabix } - { <fs_file>-kunnr } Latitude / Langitude is empty|.
        APPEND ls_popup_file TO lt_popup_file.

      ENDIF.

      IF <fs_file>-zlatitude IS NOT INITIAL AND <fs_file>-zlongitude IS INITIAL.

        ls_popup_file-msgtype = 'E'.
        ls_popup_file-message = |Line no.{ sy-tabix } - { <fs_file>-kunnr } Latitude / Langitude is empty|.
        APPEND ls_popup_file TO lt_popup_file.

      ENDIF.

      IF <fs_file>-zcapacity IS INITIAL.

      ELSEIF <fs_file>-zcapacity = '0'.

      ELSEIF <fs_file>-zcapacity LT '101' OR <fs_file>-zcapacity GT '106'.

        ls_popup_file-msgtype = 'E'.
        ls_popup_file-message = |Line no.{ sy-tabix } - { <fs_file>-kunnr } Invalid Capacity|.
        APPEND ls_popup_file TO lt_popup_file.

      ENDIF.

*      IF <fs_file>-zdays CA '.,-'.
*
*        ls_popup_file-msgtype = 'E'.
*        ls_popup_file-message = |Line no.{ sy-tabix } - { <fs_file>-kunnr } Invalid Days|.
*        APPEND ls_popup_file TO lt_popup_file.
*
*      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_output .
  DATA: "lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,

        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = lt_up_file.
    CATCH cx_salv_msg.
  ENDTRY.

  lo_gr_alv->set_screen_status(
    pfstatus      =  'SPR_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

  lr_aggregations->clear( ).
  lr_groups = lo_gr_alv->get_sorts( ) .
  lr_groups->clear( ).


* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE lt_up_file LINES lv_rows.
  CONCATENATE 'Number of records: ' lv_rows INTO lv_title SEPARATED BY space.

  CREATE OBJECT lo_grid.
  CREATE OBJECT lo_layout_logo.
  lo_grid->create_label( row = 1 column = 1 text = lv_title tooltip = lv_title ).
  lo_layout_logo->set_left_content( lo_grid ).
  lo_content = lo_layout_logo.
  lo_gr_alv->set_top_of_list( lo_content ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lo_layout = lo_gr_alv->get_layout( ).
  lo_layout->set_key( lv_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.

* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  lo_gr_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM handle_user_command  USING i_ucomm TYPE  salv_de_function.

  CASE i_ucomm.
    WHEN '&SAL_POST'.
      IF lt_popup_file IS INITIAL.
        PERFORM f_customer_update_kna1.
      ENDIF.
      IF lt_popup_file IS NOT INITIAL.
        CALL FUNCTION 'Z_POPUP_ALV'
          EXPORTING
            i_start_column      = 25
            i_start_line        = 6
            i_end_column        = 90
            i_end_line          = 20
            i_title             = 'File Status Display'
            i_status_field_name = 'MSGTYPE'
            i_popup             = 'X'
          TABLES
            it_alv              = lt_popup_file.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_purchase_req
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_customer_update_kna1 .
*&---------------------------------------------------------------------*
*& Report ZRAM_ABAP_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  DATA  lw_bapiret2  TYPE bapiret2.
  CHECK lt_up_file IS NOT INITIAL.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.

  DATA: ls_nkna1 TYPE kna1,
        ls_okna1 TYPE kna1.

  SELECT * FROM kna1
    INTO TABLE @DATA(lt_kna1)
    FOR ALL ENTRIES IN @lt_up_file
    WHERE kunnr = @lt_up_file-kunnr.
  IF sy-subrc = 0.
    LOOP AT lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).

      READ TABLE lt_up_file ASSIGNING FIELD-SYMBOL(<fs_file>) WITH KEY kunnr = <fs_kna1>-kunnr.
*                                                                       TYpe = 'S'.
      IF sy-subrc = 0.

        CLEAR: ls_okna1, ls_nkna1.
        ls_okna1 = <fs_kna1>.
        ls_nkna1 = <fs_kna1>.

        IF <fs_file>-zlatitude IS INITIAL.    "if there is no data in excel modify with old datas
          <fs_file>-zlatitude  = <fs_kna1>-zlatitude.
        ENDIF.

        IF <fs_file>-zlongitude IS INITIAL.    "if there is no data in excel modify with old datas
          <fs_file>-zlongitude = <fs_kna1>-zlongitude.
        ENDIF.

        IF <fs_file>-zdays IS INITIAL.
          <fs_file>-zdays = <fs_kna1>-zdays.
        ENDIF.

        IF <fs_file>-zcapacity IS INITIAL.
          <fs_file>-zcapacity = <fs_kna1>-zcapacity.
        ENDIF.


        IF <fs_file>-zlatitude IS NOT INITIAL OR <fs_file>-zlongitude IS NOT INITIAL OR <fs_file>-zdays IS NOT INITIAL.
* Latitude, Longitude,capacity days update
          UPDATE kna1
             SET zlatitude  =   @<fs_file>-zlatitude,
                 zlongitude =   @<fs_file>-zlongitude,
                 zdays      =   @<fs_file>-zdays,
                 zcapacity  =   @<fs_file>-zcapacity
             WHERE kunnr    =   @<fs_kna1>-kunnr.

          IF sy-subrc = 0.
            <fs_file>-message = |Customer data is updated successfully|.
            COMMIT WORK AND WAIT.

            ls_nkna1-zlatitude  = <fs_file>-zlatitude.
            ls_nkna1-zlongitude = <fs_file>-zlongitude.
            ls_nkna1-zdays      = <fs_file>-zdays.
            ls_nkna1-zcapacity  = <fs_file>-zcapacity.

* Call the FM to write entry to CDHDR and CDPOS for therespective update
            CLEAR lv_objectid.
            lv_objectid = <fs_kna1>-kunnr.

*Write the Change Document
            CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
              EXPORTING
                objectid = lv_objectid
                tcode    = 'XD02'
                utime    = sy-uzeit
                udate    = sy-datum
                username = sy-uname
                n_kna1   = ls_nkna1
                o_ykna1  = ls_okna1
                upd_kna1 = 'U'.
          ELSE.
            <fs_file>-message = |Customer data is not updated|.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  lo_gr_alv->refresh( ).
ENDFORM.
