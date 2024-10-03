*&---------------------------------------------------------------------*
*& Report ZMM_INFO_RECORD_LIST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_info_record_list.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: matnr  TYPE marc-matnr,
              plant  TYPE marc-werks OBLIGATORY,
              f_date TYPE a017-datab OBLIGATORY,
              t_date TYPE a017-datbi OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

DATA: lt_output TYPE STANDARD TABLE OF zstr_source_list.
DATA: lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      ls_layout TYPE lvc_s_layo.

DATA: lo_grid  TYPE REF TO cl_gui_alv_grid.

START-OF-SELECTION.
  REFRESH: lt_output.

  CALL FUNCTION 'ZBAPI_INFO_RECORD_LIST'
    EXPORTING
      matnr     = matnr
      plant     = plant
      begda     = f_date
      endda     = t_date
    TABLES
      it_source = lt_output.

  IF sy-subrc EQ 0 .
    SORT lt_output[] BY matnr.
  ENDIF.

END-OF-SELECTION.

**************** ALV Display *****************************
  PERFORM alv_display.

FORM alv_display.
  REFRESH lt_fcat.
  PERFORM f_fieldcat USING  'MATNR'      'Material Number'       1 space.
  PERFORM f_fieldcat USING  'MAKTX'      'material Description'  2 space.
  PERFORM f_fieldcat USING  'MTART'      'Material type'         3 space.
  PERFORM f_fieldcat USING  'EKORG'      'Purchase Organization' 4 space.
  PERFORM f_fieldcat USING  'WERKS'      'Plant'                 5 space.
  PERFORM f_fieldcat USING  'INFNR'      'Info Record number'    6 space.
  PERFORM f_fieldcat USING  'LIFNR'      'Supplier'              7 space.
  PERFORM f_fieldcat USING  'NAME1'      'Supplier Name'         8 space.
  PERFORM f_fieldcat USING  'KSCHL'      'Condition type'        9 space.
  PERFORM f_fieldcat USING  'KNUMH'      'Condition Record No'  10 space.
  PERFORM f_fieldcat USING  'KBTER'      'Amount Value'         11 space.
  PERFORM f_fieldcat USING  'FREIGHT'    'Freight Cost'         12 space.
  PERFORM f_fieldcat USING  'ADDED_COST' 'Added Csost'          13 space.
  PERFORM f_fieldcat USING  'DATAB'      'Valid From'           14 space.
  PERFORM f_fieldcat USING  'DATBI'      'Valid To'             15 space.

  CALL SCREEN 100.
ENDFORM.
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZMM_IRL'.
  SET TITLEBAR 'ZIRL'.
  IF lo_grid IS INITIAL .

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    ls_layout-cwidth_opt = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = lt_output
        it_fieldcatalog = lt_fcat.

  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.

*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  CLEAR: ls_fcat.
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
