*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_CLS
*&---------------------------------------------------------------------*
*CLASS : lcl_event_receiver DEFINITION DEFERRED.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    DATA gv_object_text    TYPE char30.

    METHODS:
      constructor
        IMPORTING e_object_text TYPE char30.

    METHODS : handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING sender e_object e_interactive.

    METHODS : handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS : handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    METHODS : handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column.

    METHODS : handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.
    METHODS : handle_onf4
      FOR EVENT onf4  OF cl_gui_alv_grid
      IMPORTING sender
                e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

    METHODS :  handle_button_click
      FOR EVENT button_click  OF cl_gui_alv_grid
      IMPORTING
        es_col_id
        es_row_no.

ENDCLASS. "(LCL_EVENT_RECEIVER DEFINITION)
DATA gv_event_receiver       TYPE REF TO lcl_event_receiver.
*----------------------------------------------------------------------*
* LOCAL CLASSES: Implementation
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor.
    gv_object_text = e_object_text.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD handle_toolbar.
    PERFORM set_toolbar USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM set_command USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_button_click.
*    PERFORM data_button_click  USING    es_col_id es_row_no.
  ENDMETHOD.                    "handle_data_changed

*-- Double Click
  METHOD handle_double_click.
    PERFORM double_click  USING gv_object_text e_row e_column.
  ENDMETHOD.                    "handle_double_click

  METHOD handle_hotspot_click.
*    PERFORM hotspot_click USING gv_object_text
*                                 e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD handle_onf4.
    PERFORM onf4 USING e_fieldname
                       e_fieldvalue
                       es_row_no
                       er_event_data
                       et_bad_cells
                       e_display.
  ENDMETHOD.                    "Handle_OnF4
ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM set_toolbar  USING p_e_object TYPE REF TO cl_alv_event_toolbar_set
                       p_e_interactive.


  DATA: ls_toolbar  TYPE stb_button.
* Append seperator to the normal toolbar
  MOVE 3            TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
  CLEAR  ls_toolbar.

  IF p_rad1 IS NOT INITIAL.
    PERFORM add_function USING p_e_object 'ZNEW'.
  ELSEIF p_rad2 IS NOT INITIAL.
    PERFORM add_function USING p_e_object 'CANEWY'.

    AUTHORITY-CHECK OBJECT 'ZCANCELIRN' ID 'ACTVT' FIELD '01'.

    IF sy-subrc EQ 0.
*    IF sy-uname = '264380' OR  sy-uname = '304377'.     "Added Two Users for cancelling the IRN
      PERFORM add_function USING p_e_object 'CANIRN'.
    ENDIF.

  ELSEIF p_rad4 IS NOT INITIAL.
    PERFORM add_function USING p_e_object 'CREWAY'.
  ENDIF.

ENDFORM.                    "set_toolbar
*&---------------------------------------------------------------------*
*&      Form  SET_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM set_command   USING    p_ucomm.
  DATA:
* Internal table
    gi_index_rows TYPE lvc_t_row,
    lw_index      TYPE lvc_s_row.
  DATA          l_answer.

  TYPES: BEGIN OF ty_msg,
           message    TYPE bapiret2-message,
           message_v1 TYPE bapiret2-message,
           message_v2 TYPE bapiret2-message,
           message_v3 TYPE bapiret2-message,
           message_v4 TYPE bapiret2-message,
         END OF ty_msg.
  DATA:   lt_tab TYPE STANDARD TABLE OF ty_msg.
  DATA:   ls_tab TYPE ty_msg.
*  DATA:   lt_tab TYPE esp1_message_tab_type.
*  DATA:   ls_tab TYPE esp1_message_wa_type.
  DATA : im_einv_out  TYPE zst_irn_out,
         im_eway_out  TYPE zdms_invoice_ewb,
         response     TYPE string,
         et_error_msg TYPE bapiret2_t,
         l_wrk_msg    TYPE bapiret2.
*  FIELD-SYMBOLS: <lfs_einv_final> TYPE any.
  FIELD-SYMBOLS: <lfs_einv_final> TYPE ty_einvoice.

  DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
        lv_month   TYPE bapi0002_4-fiscal_period,
        l_return   TYPE bapireturn1.

  CASE p_ucomm.
    WHEN 'ZNEW'.                       "Create IRN
* Read index of selected rows
      CALL METHOD gv_grid->get_selected_rows
        IMPORTING
          et_index_rows = gi_index_rows.
*      READ TABLE gi_index_rows INTO DATA(lw_index) INDEX 1.
      READ TABLE gi_index_rows INTO lw_index INDEX 1.
      IF sy-subrc IS INITIAL.
*        READ TABLE lt_einv_final ASSIGNING FIELD-SYMBOL(<lfs_einv_final>) INDEX lw_index-index.
        READ TABLE lt_einv_final ASSIGNING  <lfs_einv_final> INDEX lw_index-index.
        IF sy-subrc = 0.
*
*          CALL FUNCTION 'ZSD_FM_IRN_CREATE'
*            EXPORTING
*              im_vbeln  = <lfs_einv_final>-vbeln
*            IMPORTING
*              einv_out  = im_einv_out
*              eway_out  = im_eway_out
*            TABLES
*              error_msg = et_error_msg.
          SELECT SINGLE bukrs,gjahr,kunag
            FROM vbrk INTO @DATA(l_vbrk)
            WHERE vbeln = @<lfs_einv_final>-vbeln.
          IF sy-subrc = 0.
            SELECT SINGLE werks
              FROM vbrp INTO @DATA(l_werks)
              WHERE vbeln = @<lfs_einv_final>-vbeln.
            SELECT SINGLE kunnr
              FROM kna1 INTO @DATA(l_kunnr)
              WHERE werks = @l_werks.

            DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
            CREATE OBJECT lo_main1.
            CALL METHOD lo_main1->create_irn_qrcode
              EXPORTING
                distributor_code = l_kunnr
                vbeln            = <lfs_einv_final>-vbeln
                bukrs            = l_vbrk-bukrs
                gjahr            = l_vbrk-gjahr
*               DATE             = <lw_header>
              IMPORTING
                irn              = DATA(irn)
                signed_inv       = DATA(signed_inv)
                signed_qrcode    = DATA(signed_qrcode)
*                pdf              = DATA(pdf)
                return           = DATA(return).

            IF return IS INITIAL AND irn IS NOT INITIAL.
              l_wrk_msg-type = 'S'.
              l_wrk_msg-message = |E-invoice IRN Create Process Completed|.
              <lfs_einv_final>-irn = irn.
              SELECT SINGLE ack_no
                            ack_date  FROM zdms_invoice_irn
                INTO (<lfs_einv_final>-ack_no,
                      <lfs_einv_final>-ack_date)
                WHERE docno = <lfs_einv_final>-vbeln AND irn = irn.
*              <lfs_einv_final>-ack_no = ''."im_einv_out-ack_no.
*              <lfs_einv_final>-ack_date = ''."im_einv_out-ack_date.
              <lfs_einv_final>-irn_status = 'ACT'.
              <lfs_einv_final>-status = icon_led_green.
              MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
              APPEND ls_tab TO lt_tab.
            ELSE.

              l_wrk_msg-type = 'E'.
              l_wrk_msg-message = return.
              MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
              APPEND ls_tab TO lt_tab.
              CALL FUNCTION 'Z_POPUP_ALV'
                EXPORTING
*                 I_START_COLUMN       = 25
*                 I_START_LINE         = 6
*                 I_END_COLUMN         = 200
*                 I_END_LINE           = 20
                  i_title = 'Errors in Create IRN'
                  i_popup = 'X'
                TABLES
                  it_alv  = lt_tab.
*
*            CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*              TABLES
*                i_message_tab = lt_tab.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'CANEWY'.
*
** Read index of selected rows
*      CALL METHOD gv_grid->get_selected_rows
*        IMPORTING
*          et_index_rows = gi_index_rows.
*      READ TABLE gi_index_rows INTO lw_index INDEX 1.
**      READ TABLE lt_einv_final INTO lw_einv_final INDEX lw_index-index.
*      READ TABLE lt_einv_final ASSIGNING <lfs_einv_final> INDEX lw_index-index.
*      IF sy-subrc = 0.
*
*        CALL FUNCTION 'ZSD_FM_EWAYBILL_CANCEL'
*          EXPORTING
*            im_vbeln    = <lfs_einv_final>-vbeln
*            im_ewbillno = <lfs_einv_final>-ebillno
*          TABLES
*            error_msg   = et_error_msg.
*
*        IF et_error_msg[] IS INITIAL.
*          <lfs_einv_final>-status      = icon_system_cancel.
*          <lfs_einv_final>-can_status  = 'Cancelled'.
**          MODIFY lt_einv_final FROM lw_einv_final TRANSPORTING status can_status INDEX lw_index-index.
*        ELSE.
*          CLEAR: lv_line.
*          LOOP AT et_error_msg INTO l_wrk_msg.
**            CONCATENATE l_wrk_msg-message l_wrk_msg-message_v1 INTO  l_wrk_msg-message SEPARATED BY '-'.
**            ls_tab-msgty  = 'E'.
**            ls_tab-msgid = 'ZEINVOICE'.
**            ls_tab-msgno = '002'.
**            ls_tab-msgv1  = l_wrk_msg-message.
***            ls_tab-msgv2  = l_wrk_msg-message.
**            ls_tab-lineno = lv_line + 1.
*            MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
*            APPEND ls_tab TO lt_tab.
*          ENDLOOP.
*          CALL FUNCTION 'Z_POPUP_ALV'
*            EXPORTING
**             I_START_COLUMN       = 25
**             I_START_LINE         = 6
**             I_END_COLUMN         = 200
**             I_END_LINE           = 20
*              i_title = 'Errors in Cancel Eway Bill'
*              i_popup = 'X'
*            TABLES
*              it_alv  = lt_tab.
**          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
**            TABLES
**              i_message_tab = lt_tab.
*        ENDIF.
*
*      ENDIF.
    WHEN 'CANIRN'.

      DATA : lv_type TYPE bapi_mtype,
             lv_sec  TYPE int4.

* Read index of selected rows
      CALL METHOD gv_grid->get_selected_rows
        IMPORTING
          et_index_rows = gi_index_rows.

      READ TABLE gi_index_rows INTO lw_index INDEX 1.
      IF sy-subrc IS INITIAL.
*        READ TABLE lt_einv_final ASSIGNING <lfs_einv_final> INDEX lw_index-index.
        READ TABLE lt_einv_final ASSIGNING <lfs_einv_final> INDEX lw_index-index.

        IF <lfs_einv_final> IS ASSIGNED.

          SELECT SINGLE bukrs,gjahr,kunag
            FROM vbrk INTO @l_vbrk
            WHERE vbeln = @<lfs_einv_final>-vbeln.
          IF sy-subrc = 0.
            SELECT SINGLE werks
              FROM vbrp INTO @l_werks
              WHERE vbeln = @<lfs_einv_final>-vbeln.
            SELECT SINGLE kunnr
              FROM kna1 INTO @l_kunnr
              WHERE werks = @l_werks.
**********fetch the E-invoice details***********
            SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(ls_einv) WHERE docno  = @<lfs_einv_final>-vbeln
                                                                      AND   distrb = @l_kunnr
                                                                      AND   dealer = @l_vbrk-kunag.

************check the 24 hours**************
            CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
              EXPORTING
                date_1  = ls_einv-erdat
                time_1  = ls_einv-erzet
                date_2  = sy-datum
                time_2  = sy-uzeit
              IMPORTING
                seconds = lv_sec.

            IF lv_sec GT '84600'.
              l_wrk_msg-type    = 'E'.
              l_wrk_msg-message = | E-Invoice Cancellation Time Period Exceed |.
**********************call the alv******************
              DATA lv_line TYPE char03 .
              MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
              APPEND ls_tab TO lt_tab.
              CALL FUNCTION 'Z_POPUP_ALV'
                EXPORTING
*                 I_START_COLUMN       = 25
*                 I_START_LINE         = 6
*                 I_END_COLUMN         = 200
*                 I_END_LINE           = 20
                  i_title = 'Errors in Cancel IRN'
                  i_popup = 'X'
                TABLES
                  it_alv  = lt_tab.
            ELSE.
              DATA : lo_main2 TYPE REF TO zcl_dms_einvoice_process.
              CREATE OBJECT lo_main2.
              lo_main2->cancel_irn_qrcode(
                EXPORTING
                  distributor_code = l_kunnr                   " Disctributor Number
                  vbeln            = <lfs_einv_final>-vbeln    " Billing Document
                  bukrs            = 'DMS1'                    " Company Code
                  gjahr            = l_vbrk-gjahr          " Fiscal Year
                  irn              = ls_einv-irn               " Invoice Reference Number
                IMPORTING
                  return           = return
                  type             = lv_type ).
              IF lv_type = 'S'.
                l_wrk_msg-type = 'S'.
                l_wrk_msg-message = |E-invoice IRN Cancel Process Completed|.
                <lfs_einv_final>-irn = irn.
*              <lfs_einv_final>-ack_no = ''."im_einv_out-ack_no.
*              <lfs_einv_final>-ack_date = ''."im_einv_out-ack_date.
                <lfs_einv_final>-irn_status = 'CNL'.
                <lfs_einv_final>-status = icon_delete.
                MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
                APPEND ls_tab TO lt_tab.
              ELSE.

                l_wrk_msg-type = 'E'.
                l_wrk_msg-message = return.
                MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
                APPEND ls_tab TO lt_tab.
                CALL FUNCTION 'Z_POPUP_ALV'
                  EXPORTING
*                   I_START_COLUMN       = 25
*                   I_START_LINE         = 6
*                   I_END_COLUMN         = 200
*                   I_END_LINE           = 20
                    i_title = 'Errors in Cancel IRN'
                    i_popup = 'X'
                  TABLES
                    it_alv  = lt_tab.
*
*            CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*              TABLES
*                i_message_tab = lt_tab.

              ENDIF.
            ENDIF.
          ENDIF.
*          DATA :im_cancel_out TYPE zsd_irn_cout .
*
*          CALL FUNCTION 'ZSD_FM_IRN_CANCEL'
*            EXPORTING
*              im_irn        = <lfs_einv_final>-irn
*              im_vbeln      = <lfs_einv_final>-vbeln
*            IMPORTING
**             ex_error      =
*              ex_cancel_out = im_cancel_out
*            TABLES
*              t_error_msg   = et_error_msg
*            EXCEPTIONS
*              update_error  = 1
*              OTHERS        = 2.
*
*          IF et_error_msg[] IS INITIAL.
*            <lfs_einv_final>-status = icon_delete.
*            <lfs_einv_final>-irn_status = im_cancel_out-irn_status.
*          ELSE.
*            LOOP AT et_error_msg INTO l_wrk_msg.
**            CONCATENATE l_wrk_msg-message l_wrk_msg-message_v1 INTO  l_wrk_msg-message SEPARATED BY '-'.
**            ls_tab-msgty  = 'E'.
**            ls_tab-msgid = 'ZEINVOICE'.
**            ls_tab-msgno = '002'.
**            ls_tab-msgv1  = l_wrk_msg-message.
***            ls_tab-msgv2  = l_wrk_msg-message.
**            ls_tab-lineno = lv_line + 1.
*              MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
*              APPEND ls_tab TO lt_tab.
*            ENDLOOP.
*
*            CALL FUNCTION 'Z_POPUP_ALV'
*              EXPORTING
**               I_START_COLUMN       = 25
**               I_START_LINE         = 6
**               I_END_COLUMN         = 200
**               I_END_LINE           = 20
*                i_title = 'Errors in Cancel IRN'
*                i_popup = 'X'
*              TABLES
*                it_alv  = lt_tab.
**          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
**            TABLES
**              i_message_tab = lt_tab.
*          ENDIF.
        ENDIF.
      ENDIF.


    WHEN 'CREWAY'.                       "Create E-Way Bill BY IRN details
** Read index of selected rows
*      DATA: wa_veh TYPE zst_ewaybill.
*
*      CALL METHOD gv_grid->get_selected_rows
*        IMPORTING
*          et_index_rows = gi_index_rows.
*
*      READ TABLE gi_index_rows INTO lw_index INDEX 1.
*      IF sy-subrc IS INITIAL.
*        READ TABLE lt_einv_final ASSIGNING <lfs_einv_final> INDEX lw_index-index.
*
*        IF sy-subrc = 0.
*          CONCATENATE '"' <lfs_einv_final>-irn '"' INTO wa_veh-irn.
*
*          IF p_vehno IS NOT INITIAL.
*            CONCATENATE '"' p_vehno '"' INTO wa_veh-vehno.
*          ELSE.
*            MOVE 'null' TO wa_veh-vehno.
*          ENDIF.
*
*          IF p_tid IS NOT INITIAL.
*            CONCATENATE '"' p_tid '"'   INTO wa_veh-transid.
*          ELSE.
*            MOVE 'null' TO wa_veh-transid.
*          ENDIF.
*
*          IF p_tnm IS NOT INITIAL.
*            CONCATENATE '"' p_tnm '"'   INTO wa_veh-transname.
*          ELSE.
*            MOVE 'null' TO wa_veh-transname.
*          ENDIF.
*
*          IF p_docno IS NOT INITIAL.
*            CONCATENATE '"' p_docno '"' INTO wa_veh-transdoc.
*          ELSE.
*            MOVE 'null' TO wa_veh-transdoc.
*          ENDIF.
*
*          IF p_docdt IS NOT INITIAL.
*            CONCATENATE '"' p_docdt+6(2) '/' p_docdt+4(2) '/'  p_docdt+0(4) '"' INTO wa_veh-transdt.
*          ELSE.
*            MOVE 'null' TO wa_veh-transdt.
*          ENDIF.
*
*          IF p_dis IS NOT INITIAL.
*            MOVE p_dis                    TO wa_veh-distance.      "Distance
*          ENDIF.
*
*          MOVE <lfs_einv_final>-vbeln          TO wa_veh-vbeln.
*          MOVE <lfs_einv_final>-bukrs          TO wa_veh-bukrs.
*          MOVE <lfs_einv_final>-fkart          TO wa_veh-doctyp.
*
**Fisyear Derive based on Posting Date
*          DATA: lv_postdate TYPE budat.
*
*          CLEAR: lv_fisyear,lv_month,l_return.
*          lv_postdate = <lfs_einv_final>-fkdat.
*          CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
*            EXPORTING
*              companycodeid = '1000'
*              posting_date  = lv_postdate
*            IMPORTING
*              fiscal_year   = lv_fisyear
*              fiscal_period = lv_month
*              return        = l_return.
*
**        MOVE <lfs_einv_final>-fkdat(4)       TO wa_veh-gjahr.
*          wa_veh-gjahr = lv_fisyear.
*
*          CONCATENATE     '"' 'R' '"'       INTO wa_veh-vehtyp.      "Regular
*          CONCATENATE     '"' '1' '"'       INTO wa_veh-transmode.   "1 - Road , 2-Rail , Air-3 , Ship-4
*
*          CALL FUNCTION 'ZSD_FM_EWAYBILL_CREATE'
*            EXPORTING
*              im_ewb    = wa_veh
*            IMPORTING
*              response  = response
*              eway_out  = im_eway_out
*            TABLES
*              error_msg = et_error_msg.
*
*          IF et_error_msg[] IS INITIAL AND im_eway_out-ebillno IS NOT INITIAL.
*            <lfs_einv_final>-status     = icon_led_green.
*            <lfs_einv_final>-ebillno    = im_eway_out-ebillno.
**            <lfs_einv_final>-VDFMDATE   = im_eway_out-VDFMDATE.
**            <lfs_einv_final>-VDFMTIME   = im_eway_out-VDFMTIME.
*          ELSE.
*            CLEAR: lv_line.
*            LOOP AT et_error_msg INTO l_wrk_msg.
**            CONCATENATE l_wrk_msg-message l_wrk_msg-message_v1 INTO  l_wrk_msg-message SEPARATED BY '-'.
**            ls_tab-msgty  = 'E'.
**            ls_tab-msgid = 'ZEINVOICE'.
**            ls_tab-msgno = '004'.
**            ls_tab-msgv1  = l_wrk_msg-message.
***            ls_tab-msgv2  = l_wrk_msg-message.
**            ls_tab-lineno = lv_line + 1.
*              MOVE-CORRESPONDING l_wrk_msg TO ls_tab.
*              APPEND ls_tab TO lt_tab.
*            ENDLOOP.
*            CALL FUNCTION 'Z_POPUP_ALV'
*              EXPORTING
**               I_START_COLUMN       = 25
**               I_START_LINE         = 6
**               I_END_COLUMN         = 200
**               I_END_LINE           = 20
*                i_title = 'Errors in Cancel IRN'
*                i_popup = 'X'
*              TABLES
*                it_alv  = lt_tab.
**          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
**            TABLES
**              i_message_tab = lt_tab.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.

  ENDCASE.


ENDFORM.                    "set_command
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed  USING p_data_changed TYPE REF TO
                                         cl_alv_changed_data_protocol.



ENDFORM.                    "data_changed
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_OBJECT_TEXT  text
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*----------------------------------------------------------------------*
FORM double_click  USING    p_gv_object_text
                            p_e_row
                            p_e_column.



ENDFORM.                    "double_click
*&---------------------------------------------------------------------*
*&      Form  ONF4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM onf4  USING    p_fieldname
                    p_fieldvalue
                    ps_row_no     TYPE lvc_s_roid
                    pr_event_data TYPE REF TO cl_alv_event_data
                    pt_bad_cells
                    p_display.

ENDFORM.                    "onf4
