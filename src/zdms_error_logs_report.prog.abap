
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 02.01.2024
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934902
*
*  Business Logic            : Report for DMS error log
*
*  Released on Date          :
*=======================================================================
REPORT zdms_error_log_report.

*************Global data dec & selection screen design**************
DATA : gv_type        TYPE zdms_com_err_log-type,
       gv_status      TYPE zdms_com_err_log-status,
       gv_distributor TYPE kna1-kunnr,
       gv_ordid       TYPE zdms_com_err_log-dms_orderid.
SELECT-OPTIONS : so_type  FOR gv_type,
                 so_stat  FOR gv_status DEFAULT '10',
                 so_dist  FOR gv_distributor,
                 so_retai FOR gv_distributor,
                 so_ordid FOR gv_ordid,
                 so_date  FOR sy-datum OBLIGATORY DEFAULT sy-datum.
*&---------------------------------------------------------------------*
*class definition
*&---------------------------------------------------------------------*
CLASS lcl_error DEFINITION FINAL.

  PUBLIC SECTION.
*********structure
    TYPES : BEGIN OF ty_final,
              refid       TYPE zdms_refid,
              type        TYPE ztype,
              desc        TYPE char100,
              status      TYPE zdms_com_err_log-status,
              dms_orderid TYPE zorder_id,
              distributor TYPE zdist,
              plant       TYPE werks_d,
              dealer      TYPE kunag,
              msg         TYPE string,
              erdat       TYPE erdat,
              erzet       TYPE erzet,
              ernam       TYPE ernam,
              material    TYPE matnr,
              cbox        TYPE char1,
              status_desc TYPE char20,
            END OF ty_final.
    TYPES: BEGIN OF ty_text,
             value(200000) TYPE  c,
           END OF ty_text.
*****data dec
    DATA : gt_final TYPE TABLE OF ty_final,
           gv_alv   TYPE REF TO cl_gui_alv_grid. "for alv object
*****methods dec
    METHODS : fetch,   "fetching
      alv,     "mat master alv
      update,  "update process
      select_all. "select all check box
*methods for event handlers
    METHODS: "change          FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column.

ENDCLASS.
*&---------------------------------------------------------------------*
*class IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_error IMPLEMENTATION.

  METHOD fetch.
    DATA: lt_value TYPE TABLE OF  dd07v.
****************fetch from error log table***********
    SELECT * FROM zdms_com_err_log INTO CORRESPONDING FIELDS OF TABLE gt_final
                                   WHERE type        IN so_type
                                   AND   status      IN so_stat
                                   AND   dms_orderid IN so_ordid
                                   AND   distributor IN so_dist
                                   AND   dealer      IN so_retai
                                   AND   erdat       IN so_date.
    IF sy-subrc = 0.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZTYPE'   "<-- Your Domain Here
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_value
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        SORT lt_value BY domvalue_l.
        LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
          READ TABLE lt_value INTO DATA(ls_value) WITH KEY domvalue_l = <fs_final>-type BINARY SEARCH.
          IF sy-subrc = 0.
            <fs_final>-desc = ls_value-ddtext.
            CLEAR ls_value.
          ENDIF.
          IF <fs_final>-status = '10'.
            <fs_final>-status_desc = '10 - Error'.
          ELSE.
            <fs_final>-status_desc = '20 - Cleared'.
          ENDIF.
        ENDLOOP.
      ENDIF.
**************Call the ALV SCreen**************
      alv( ).
      CALL SCREEN 9000.
    ELSE.
      MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD alv.
    DATA : lt_fcat    TYPE lvc_t_fcat,   "data dec for fieldcat
           lt_exclude TYPE TABLE OF ui_func.      "exclude the toolbar it
    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true ).   "data dec for layout design
    IF gv_alv IS INITIAL.
**create the object for alv
      CREATE OBJECT gv_alv
        EXPORTING
          i_parent = cl_gui_container=>screen0.
    ENDIF.
****************fill internal table***************
    lt_fcat = VALUE #(
    ( col_pos = 1  fieldname = 'CBOX'        scrtext_m = 'CHECKBOX'     checkbox = abap_true edit = abap_true )
    ( col_pos = 2  fieldname = 'REFID'       ref_field = 'REFID'        ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 3  fieldname = 'TYPE'        ref_field = 'TYPE'         ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 4  fieldname = 'DESC'        scrtext_m = 'Program Name'                                )
    ( col_pos = 5  fieldname = 'STATUS_DESC' scrtext_m = 'Status Desc'                                 )
    ( col_pos = 6  fieldname = 'DMS_ORDERID' ref_field = 'DMS_ORDERID'  ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 7  fieldname = 'DISTRIBUTOR' ref_field = 'DISTRIBUTOR'  ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 8  fieldname = 'PLANT'       ref_field = 'PLANT'        ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 9  fieldname = 'DEALER'      ref_field = 'DEALER'       ref_table = 'ZDMS_COM_ERR_LOG' scrtext_m = 'Dealer' )
    ( col_pos = 10 fieldname = 'MSG'         ref_field = 'MSG'          ref_table = 'ZDMS_COM_ERR_LOG' scrtext_m = 'Message' )
    ( col_pos = 11 fieldname = 'ERDAT'       ref_field = 'ERDAT'        ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 12 fieldname = 'ERZET'       ref_field = 'ERZET'        ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 13 fieldname = 'ERNAM'       ref_field = 'ERNAM'        ref_table = 'ZDMS_COM_ERR_LOG' )
    ( col_pos = 14 fieldname = 'MATERIAL'    ref_field = 'MATERIAL'     ref_table = 'ZDMS_COM_ERR_LOG' ) ).
***********exclude the standard toolbar***************
    lt_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_undo )
                          ( cl_gui_alv_grid=>mc_fc_refresh )
                          ( cl_gui_alv_grid=>mc_fc_loc_cut )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_copy  ) ).

    SET HANDLER on_double_click FOR gv_alv.
*display the alv
    gv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
        it_toolbar_excluding          = lt_exclude
      CHANGING
        it_outtab                     = gt_final    "Output Table
        it_fieldcatalog               = lt_fcat     "Field Catalog
    ).
    REFRESH lt_fcat.        "refresh fieldcat

  ENDMETHOD.

  METHOD update.
*    "enable the changed data
    DATA : w_x    TYPE char1 VALUE abap_false.
*
    gv_alv->check_changed_data(
                        CHANGING
                           c_refresh = w_x ).
*************changing the status**********
    READ TABLE gt_final TRANSPORTING NO FIELDS WITH KEY cbox = abap_true.
    IF sy-subrc = 0.
      LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>) WHERE cbox = abap_true.
        UPDATE zdms_com_err_log SET status = '20' WHERE refid = <fs_final>-refid.
        IF sy-subrc = 0.
          COMMIT WORK.
          <fs_final>-status_desc = '20 - Cleared'.
          CLEAR : <fs_final>-cbox.
        ELSE.
          <fs_final>-status_desc = '10 - Error'.
          ROLLBACK WORK.
        ENDIF.
      ENDLOOP.
************refresh the alv screen**********
      gv_alv->refresh_table_display( ).
    ELSE.
      MESSAGE : 'please select any checkbox' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

*  METHOD change.
*  ENDMETHOD.

  METHOD on_double_click.
****************Read the text**************
    DATA : lv_string_in TYPE string,
           lt_text      TYPE TABLE OF ty_text.
    DATA: lv_string TYPE string,
          lt_split  TYPE TABLE OF char40,
          lt_char72 TYPE TABLE OF char72,
          lv_wa_str TYPE string,
          lv_len    TYPE int4,
          lr_split  TYPE REF TO char40,
          lr_char72 TYPE REF TO char72.

    READ TABLE gt_final ASSIGNING FIELD-SYMBOL(<fs_logs>) INDEX e_row.
    IF sy-subrc = 0.
      CLEAR: lv_string_in,lt_split,lt_char72.
      lv_string_in = <fs_logs>-msg.
*************************split the text into internal table***************
      SPLIT lv_string_in AT space INTO TABLE lt_split.

      LOOP AT lt_split REFERENCE INTO lr_split.
        lv_len = strlen( lv_wa_str ) + strlen( lr_split->* ).
        IF lv_len LT 72.
          CONCATENATE lv_wa_str lr_split->* INTO lv_wa_str SEPARATED BY space.
        ELSE.
          APPEND lv_wa_str TO lt_char72.
          lv_wa_str = lr_split->*.
        ENDIF.
      ENDLOOP.
      APPEND lv_wa_str TO lt_char72.
****************call the text editor**************
      IF lt_char72  IS NOT INITIAL.

        CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
          EXPORTING
            edit_mode = ' '
          TABLES
            t_txwnote = lt_char72.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD select_all.
    DATA : ls_final  LIKE LINE OF gt_final.

    ls_final-cbox = abap_true.
    MODIFY gt_final  FROM ls_final TRANSPORTING cbox WHERE cbox = abap_false.

    IF sy-subrc = 0.
      gv_alv->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA lobj_error TYPE REF TO lcl_error.
  CREATE OBJECT lobj_error. "create the object for local class

START-OF-SELECTION.
  lobj_error->fetch( ).

MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR lobj_error.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'UPDATE'.
      lobj_error->update( ).
    WHEN 'SELECT_ALL'.
      lobj_error->select_all( ).
  ENDCASE.

ENDMODULE.

MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZERROR'.
  SET TITLEBAR 'ZERROR'.
ENDMODULE.
