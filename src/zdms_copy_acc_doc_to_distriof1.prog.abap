**KR https://answers.sap.com/questions/10462823/how-to-post-the-withholding-tax-using-function-mod.html
**KG https://content1438.rssing.com/chan-7561109/article5674.html
**KA
*
*DATA: gs_documentheader LIKE bapiache09.
*DATA: gs_accountgl LIKE bapiacgl09.
*DATA: BEGIN OF gt_accountgl OCCURS 0.
*        INCLUDE STRUCTURE bapiacgl09.
*DATA: END OF gt_accountgl.
*
*DATA: gs_accountpayable LIKE bapiacap09.
*DATA: BEGIN OF gt_accountpayable OCCURS 0.
*        INCLUDE STRUCTURE bapiacap09.
*DATA: END OF gt_accountpayable.
*
*DATA: gs_currencyamount LIKE bapiaccr09.
*DATA: BEGIN OF gt_currencyamount OCCURS 0.
*        INCLUDE STRUCTURE bapiaccr09.
*DATA: END OF gt_currencyamount.
*
*DATA: BEGIN OF gt_return OCCURS 0.
*        INCLUDE STRUCTURE bapiret2.
*DATA: END OF gt_return.
*
*DATA: gv_obj_type LIKE bapiache09-obj_type,
*
*      gv_obj_key  LIKE bapiache09-obj_key,
*      gv_obj_sys  LIKE bapiache09-obj_sys.
*gs_documentheader-bus_act = 'RFBU'.
*gs_documentheader-username = sy-uname.
*gs_documentheader-header_txt = 'test'.
*gs_documentheader-comp_code = 'YOUR COMPANY CODE'.
*gs_documentheader-doc_date = 'WRITE UR DOC.DATE'.
*gs_documentheader-pstng_date = 'WRITE UR DOC. DATE'.
*
*gs_documentheader-doc_type = 'KA'.
*gs_accountpayable-itemno_acc = '2'.
*gs_accountpayable-vendor_no = '0060001439'.
*APPEND gs_accountpayable TO gt_accountpayable.
*
*gs_accountgl-itemno_acc = '1'.
*gs_accountgl-gl_account = '0000825100'.
*gs_accountgl-costcenter = '0000010500'.
*APPEND gs_accountgl TO gt_accountgl.
*
*gs_currencyamount-itemno_acc = '1'.
*gs_currencyamount-currency = 'USD'.
*gs_currencyamount-amt_doccur = '350.00'. "i_tab-repval.
*APPEND gs_currencyamount TO gt_currencyamount.
*
*gs_currencyamount-itemno_acc = '2'.
*gs_currencyamount-currency = 'USD'.
*gs_currencyamount-amt_doccur = '-350.00'. "i_tab-repval.
*APPEND gs_currencyamount TO gt_currencyamount.
*
*CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*  EXPORTING
*    documentheader = gs_documentheader
*  IMPORTING
*    obj_type       = gv_obj_type
*    obj_key        = gv_obj_key
*    obj_sys        = gv_obj_sys
*  TABLES
*    accountgl      = gt_accountgl
*    accountpayable = gt_accountpayable
*    currencyamount = gt_currencyamount
*    return         = gt_return.

**----------------------------------------------------------------------*
****INCLUDE ZDMS_COPY_ACC_DOC_TO_DISTRIOPB.
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**& Module STATUS_9001 OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE status_9001 OUTPUT.
*  SET PF-STATUS 'SALV_STANDARD'.
*  gr_main_cls->build_alv( ).
** SET TITLEBAR 'xxx'.
**  PERFORM f_display_alv.
*ENDMODULE.
***&---------------------------------------------------------------------*
***& Form f_display_alv
***&---------------------------------------------------------------------*
***& text
***&---------------------------------------------------------------------*
***& -->  p1        text
***& <--  p2        text
***&---------------------------------------------------------------------*
**FORM f_display_alv .
**
**  DATA: lr_alv TYPE REF TO cl_salv_table.
**  DATA: lr_alv2 TYPE REF TO cl_salv_table.
**  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
**  DATA: lr_groups TYPE REF TO cl_salv_sorts .
**  DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
**        go_alv_custom TYPE REF TO cl_gui_custom_container.
**
**
**  CALL METHOD cl_salv_table=>factory
**    EXPORTING
**      r_container  = cl_gui_container=>default_screen "go_alv_custom " <== type ref to cl_gui_container
**    IMPORTING
**      r_salv_table = lr_alv
**    CHANGING
**      t_table      = gt_final_tab. "lt_final.
**
**  DATA  "gr_salv_func TYPE REF TO cl_salv_functions_list.
**        gr_salv_func TYPE REF TO cl_salv_functions.
**
**
*** Let's show all default buttons of ALV
**  gr_salv_func = lr_alv->get_functions( ).
**  gr_salv_func->set_all( ).
**
**  INCLUDE <icon>.
**  TRY.
**      gr_salv_func->add_function(
**        name     = 'POST'
**        icon     = CONV string( icon_system_save )
**        text     = `Post Document`
**        tooltip  = `Post Document`
**        position = if_salv_c_function_position=>right_of_salv_functions ).
**    CATCH cx_salv_existing cx_salv_wrong_call.
**  ENDTRY.
**
*** Register events
**
***... §5 object for handling the events of cl_salv_table
**  DATA: gr_events TYPE REF TO lcl_handle_events.
**  DATA: lr_events  TYPE REF TO cl_salv_events_table.
**
**  lr_events  = lr_alv->get_event( ).
**  CREATE OBJECT gr_events.
**
**  SET HANDLER gr_events->on_user_command FOR lr_events.
**
**  lr_columns = lr_alv->get_columns( ).
**  lr_columns->set_optimize( 'X' ).
**  DATA not_found TYPE REF TO cx_salv_not_found.
**  TRY.
**      DATA(lr_column) = lr_columns->get_column( 'KSCHL' ).
**      lr_column->set_visible( abap_false ).
**      lr_column = lr_columns->get_column( 'KWERT' ).
**      lr_column->set_visible( abap_false ).
**      lr_column = lr_columns->get_column( 'JOSG' ).
**      lr_column->set_short_text( 'Tax-SGST' ).
**      lr_column = lr_columns->get_column( 'JOCG' ).
**      lr_column->set_short_text( 'Tax-CGST' ).
**      lr_column = lr_columns->get_column( 'JOIG' ).
**      lr_column->set_short_text( 'Tax-IGST' ).
**      lr_column = lr_columns->get_column( 'JTC1' ).
**      lr_column->set_short_text( 'Tax-TCS' ).
**      lr_column = lr_columns->get_column( 'REMARKS' ).
**      lr_column->set_short_text( 'Message' ).
**    CATCH cx_salv_not_found INTO not_found.
**      " error handling
**  ENDTRY.
**
*** display ALV
**  lr_alv->display( ).
***  lr_alv2->display( ).
***&---------------------------------------------------------------------*
***& Form show_function_info
***&---------------------------------------------------------------------*
***& text
***&---------------------------------------------------------------------*
***&      --> E_SALV_FUNCTION
***&      --> TEXT_I08
***&---------------------------------------------------------------------*
**ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_9001  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE user_command_9001 INPUT.
*
*  CASE sy-ucomm.
*    WHEN 'SAVE'.
**      IF p_status NE '99'.
**        MESSAGE 'Records status contains other than 99 . Rerun the program with status 99 to update'
**        TYPE 'I' DISPLAY LIKE 'E'.
**      ELSE.
**        lobj_cust->update_10( ).
**        MESSAGE 'Records has been updated successfully'
**        TYPE 'I' DISPLAY LIKE 'S'.
**        LEAVE TO SCREEN 0.
**      ENDIF.
*    WHEN '&F03'. "BACK
*      LEAVE TO SCREEN 0.
*    WHEN '&F15' OR '&F12'. "EXIT CANCEL
*      LEAVE PROGRAM.
*  ENDCASE.
*ENDMODULE.
