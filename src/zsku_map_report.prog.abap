*&---------------------------------------------------------------------*
*& Report Zsku_map_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsku_map_report.

tables: kna1.

select-options: s_kunnr for kna1-kunnr,
                s_regio for kna1-regio.

types: begin of lty_kna1,
         kunnr type kunnr,
         name1 type name1_gp,
         regio type regio,
       end of lty_kna1,

       begin of lty_knvv,
         kunnr type kunnr,
         vkorg type vkorg,
         kdgrp type kdgrp,
       end of lty_knvv,

       begin of lty_knvp,
         kunnr type kunnr,
         vkorg type vkorg,
         kunn2 type kunn2,
         parvw type parvw,
       end of lty_knvp,


       begin of lty_disp,
         kunnr  type kunnr,
         name1  type name1_gp,
         kunn2  type kunn2,
         name2  type name1_gp,
         kunn3  type kunn2,
         name3  type name1_gp,
         kunn4  type kunn2,
         name4  type name1_gp,
         status type zupdate,
       end of lty_disp,
       begin of lty_fields,
         fieldname type fieldname,
         rollname  type rollname,
         position  type tabfdpos,
         datatype  type datatype_d,
         leng      type ddleng,
       end of lty_fields.

data: lt_kna1    TYPE STANDARD TABLE OF lty_kna1,
      ls_kna1    TYPE lty_kna1,
      lt_disp    type standard table of lty_disp,
      lt_disp1   type standard table of lty_disp,
      lt_disp2   type standard table of lty_disp,
      lt_disp3   type standard table of lty_disp,
      ls_disp    type lty_disp,
      ls_disp1   type lty_disp,
      lt_knvp    type standard table of lty_knvp,
      ls_knvp    type lty_knvp,
      lt_knvv    type standard table of lty_knvv,
      ls_knvv    type lty_knvv,
      lt_fields  type standard table of lty_fields,
      ls_fields  type lty_fields,
      lt_fcat    type lvc_t_fcat,
      ls_fcat    type lvc_s_fcat,
      lt_exclude type ui_functions,
      ls_excl    type ui_func,
      ls_layout  type lvc_s_layo,
      lt_sku     type standard table of zsku_table,
      ls_sku     type zsku_table.

DATA: lv_container TYPE scrfname VALUE 'ALV_GRID',
      lv_line      TYPE i.

CLASS lcl_handle_events DEFINITION DEFERRED.

DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_custom_container.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   type lvc_s_modi,
          lv_index type i.
    CLEAR:ls_disp,ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'KUNN3'.
    IF sy-subrc = 0 AND ls_row-value IS NOT INITIAL.
      READ TABLE lt_disp3 INTO ls_disp INDEX ls_row-row_id.
      ls_disp-kunn3 = ls_row-value.
      SELECT SINGLE name1 FROM kna1 INTO ls_disp-name3 WHERE kunnr = ls_disp-kunn3.
      IF ls_disp-kunn3 IS NOT INITIAL.
        ls_disp-status = 'Updated'.
      ELSEIF ls_disp-kunn3 NE ls_sku-distributor_2.
        ls_disp-status = ''(002).
      ENDIF.
      MODIFY lt_disp3 FROM ls_disp INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:ls_disp,ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'KUNN4'.
    IF sy-subrc = 0 AND ls_row-value IS NOT INITIAL.
      READ TABLE lt_disp3 INTO ls_disp INDEX ls_row-row_id.
      ls_disp-kunn4 = ls_row-value.
      SELECT SINGLE name1 FROM kna1 INTO ls_disp-name4 WHERE kunnr = ls_disp-kunn4.
      IF ls_disp-kunn4 IS NOT INITIAL.
        ls_disp-status = 'Updated'.
       ELSEIF  ls_disp-kunn3 NE ls_sku-distributor_2.
        ls_disp-status = ''.
      ENDIF.
      MODIFY lt_disp3 FROM ls_disp INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  REFRESH:lt_kna1,lt_disp,lt_knvp,lt_knvv.

  SELECT kunnr name1 regio FROM kna1 INTO TABLE lt_kna1
                                      WHERE kunnr IN s_kunnr AND
                                            regio IN s_regio.


  IF lt_kna1 IS NOT INITIAL.
    SELECT kunnr vkorg kdgrp FROM knvv INTO TABLE lt_knvv
                                             FOR ALL ENTRIES IN lt_kna1
                                             WHERE kunnr = lt_kna1-kunnr AND
                                                  vkorg EQ '6000' AND kdgrp EQ '10'.
    IF lt_knvv IS NOT INITIAL.
      SELECT kunnr vkorg kunn2 parvw FROM knvp INTO TABLE lt_knvp
                                                FOR ALL ENTRIES IN lt_knvv
                                                WHERE kunnr = lt_knvv-kunnr AND
                                                vkorg EQ '6000' AND
                                                parvw EQ 'SK'.


      REFRESH lt_sku.
      SELECT * FROM zsku_table INTO TABLE lt_sku
                          FOR ALL ENTRIES IN lt_knvv
                                                WHERE Customer_no = lt_knvv-kunnr.
    ENDIF.

    LOOP AT lt_knvv INTO ls_knvv.

      CLEAR ls_knvp.
      READ TABLE lt_knvp INTO ls_knvp WITH KEY Kunnr = ls_knvv-kunnr.
      ls_disp-kunnr = ls_knvp-kunnr.
      ls_disp-kunn2 = ls_knvp-kunn2.
      SELECT SINGLE name1 INTO ls_disp-name2 FROM Kna1 WHERE kunnr = ls_disp-kunn2.
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_knvp-kunnr.
      ls_disp-name1 = ls_kna1-name1.

      IF sy-subrc = 0 AND ls_disp-kunn3 IS INITIAL.
       ls_disp-status = 'New'.
      ENDIF.


      CLEAR ls_sku.
      READ TABLE lt_sku INTO ls_sku WITH KEY customer_no = ls_knvv-kunnr.
      IF sy-subrc = 0.

        ls_disp-kunn3 = ls_sku-distributor_2.
        ls_disp-name3 = ls_sku-distributor_nme2.
        ls_disp-kunn4 = ls_sku-distributor_3.
        ls_disp-name4 = ls_sku-distributor_nme3.
        ls_disp-status = ls_sku-status.
      ENDIF.

      APPEND ls_disp TO lt_disp.

    ENDLOOP.

  ENDIF.

End-of-selection.

  REFRESH: lt_disp1[],lt_disp2[],lt_disp3[].
  lt_disp1[] = lt_disp[].

  SORT lt_disp1 BY Kunnr Name1.
  DELETE ADJACENT DUPLICATES FROM lt_disp1 COMPARING kunnr name1.

  LOOP AT lt_disp1 INTO ls_disp.
    REFRESH lt_disp2[].
    lt_disp2[] = lt_disp[].
    SORT lt_disp2 BY Kunnr Name1.
    DELETE lt_disp2[] WHERE Kunnr NE ls_disp-kunnr.
    DELETE lt_disp2[] WHERE Name1 NE ls_disp-name1.
    SORT lt_disp2 DESCENDING BY kunn2.
    CLEAR Lv_line.
    DESCRIBE TABLE lt_disp2 LINES lv_line.
    IF lv_line = 1.
      CLEAR ls_disp1.
      READ TABLE lt_disp2 INTO ls_disp INDEX 1.
    ELSE.
      READ TABLE lt_disp2 INTO ls_disp INDEX 2.
    ENDIF.
    APPEND ls_disp TO lt_disp3.
  ENDLOOP.

  REFRESH lt_fcat.

  PERFORM f_fieldcat USING  'KUNNR' 'Customer No' 1 Space.
  PERFORM f_fieldcat USING  'NAME1' 'Customer Nme' 2 Space.
  PERFORM f_fieldcat USING  'KUNN2' 'Distributor_1' 3 Space .
  PERFORM f_fieldcat USING  'NAME2' 'Distributor Nme' 4 Space.
  PERFORM f_fieldcat USING  'KUNN3' 'Distributor_2' 5 'X'.
  PERFORM f_fieldcat USING  'NAME3' 'Distributor Nme' 6 Space.
  PERFORM f_fieldcat USING  'KUNN4' 'Distributor_3' 7 'X'.
  PERFORM f_fieldcat USING  'NAME4' 'Distributor Nme' 8 Space.
  PERFORM f_fieldcat USING 'STATUS' 'Updated Status' 9 Space.

  CALL SCREEN 100.


FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module Status_0100 Output
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE Status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS'.
  SET TITLEBAR 'ZSKU'.
  IF lo_container IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = lv_container.
    CREATE OBJECT lo_grid
      EXPORTING
        I_parent = Lo_container.

    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.

    ls_layout-cwidth_opt = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = lt_disp3
        it_fieldcatalog      = lt_fcat.

    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        Error      = 1
        OTHERS     = 2.
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  User_command_0100  Input
*&---------------------------------------------------------------------*
*       Text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      REFRESH lt_sku.
      LOOP AT lt_disp3 INTO ls_disp WHERE kunn3 IS NOT INITIAL.
        CLEAR: ls_sku.
        ls_sku-customer_no = ls_disp-kunnr.
        ls_sku-distributor_1 =  ls_disp-kunn2.
        ls_sku-distributor_nme1 = ls_disp-name2.
        ls_sku-distributor_2  = ls_disp-kunn3.
        ls_sku-distributor_nme2 = ls_disp-name3.
        ls_sku-distributor_3  = ls_disp-kunn4.
        ls_sku-distributor_nme3 = ls_disp-name4.
        ls_sku-status = ls_disp-status.
        ls_sku-create_on  = sy-datum.
        ls_sku-created_by  = sy-uname.
        ls_sku-changed_by  = sy-uname.
        ls_sku-changed_on = sy-datum.
        APPEND ls_sku TO lt_sku.
      ENDLOOP.
      IF lt_sku[] IS NOT INITIAL.
        MODIFY zsku_table FROM TABLE lt_sku.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          MESSAGE TEXT-001 TYPE 'S'.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.
