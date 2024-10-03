*&---------------------------------------------------------------------*
*& Report ZAPI_LOGS_DISPLAY
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 15.06.2023
*&Purpose: To Display the API Logs input and Output
*&Reference: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zapi_logs_display.

TYPES: BEGIN OF ty_text,
         value(200000) TYPE  c,
       END OF ty_text.
DATA: gt_text  TYPE TABLE OF ty_text,
      gt_text1 TYPE TABLE OF ty_text.
DATA: geditor_container1 TYPE REF TO cl_gui_custom_container,
      geditor_container2 TYPE REF TO cl_gui_custom_container,
      gd_mode            TYPE i  VALUE cl_gui_textedit=>false,
      gtext_editor1      TYPE REF TO cl_gui_textedit,
      gtext_editor2      TYPE REF TO cl_gui_textedit.

DATA: lv_refid   TYPE zapi_refid,
      lv_apiname TYPE zapiname,
      lv_erdat   TYPE erdat,
      lv_dist    TYPE kunnr.

DATA: gv_string_in TYPE string,
      gv_string_ot TYPE string.
DATA: gt_api_logs TYPE STANDARD TABLE OF zapi_logs.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_refid FOR lv_refid,
                  s_name FOR lv_apiname,
                  s_dist FOR lv_dist,
                  s_reta FOR lv_dist,
                  s_erdat FOR lv_erdat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zapi_logs_display_cls.

DATA: lo_main TYPE REF TO lcl_logdisplay.

INITIALIZATION.
  CREATE OBJECT lo_main.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
  lo_main->f4_shlp( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-high.
  lo_main->f4_shlp( ).

START-OF-SELECTION.

  lo_main->data_population( ).

  lo_main->build_alv( ).

END-OF-SELECTION.
