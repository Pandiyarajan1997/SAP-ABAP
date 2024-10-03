*&---------------------------------------------------------------------*
*& Report ZDMS_OPENING_BALANCE_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_postion_pernr_mapping.

TYPES: BEGIN OF alv,
         plans   TYPE plans,
         begda   TYPE begda,
         endda   TYPE endda,
         pernr   TYPE pernr_d,
         msgtyp  TYPE bapi_mtype,
         message TYPE string,
       END OF alv.
TYPES: BEGIN OF ty_input,
         plans TYPE plans,
         begda TYPE begda,
         endda TYPE endda,
         pernr TYPE pernr_d,
       END OF ty_input.
DATA: gt_excel TYPE TABLE OF ty_input.
DATA: gt_type TYPE truxs_t_text_data.
DATA: gt_alv  TYPE TABLE OF alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
*************************************************************************
************ Selection Screen Design ************************************
PARAMETERS: p_fname TYPE rlgrap-filename.

***************************************************************************


INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.


START-OF-SELECTION.


  IF p_fname IS NOT INITIAL.

    "Converting Data from excel to SAP
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = gt_type
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.

    DATA : gt_zhr_pos_pernr TYPE STANDARD TABLE OF zhr_pos_pernr.
    DATA : gw_tab TYPE zhr_pos_pernr.
    DATA : gw_alv LIKE LINE OF  gt_alv.
    DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.
    LOOP AT gt_excel INTO DATA(lw_excel).
      MOVE-CORRESPONDING lw_excel TO gw_alv.
      gw_alv-msgtyp  = 'S'.
      gw_alv-message = 'Uploaded susscessfully'.
      APPEND gw_alv TO gt_alv.
      MOVE-CORRESPONDING lw_excel TO gw_tab.
      APPEND gw_tab TO gt_zhr_pos_pernr.
    ENDLOOP.
    IF gt_zhr_pos_pernr IS NOT INITIAL.
      MODIFY zhr_pos_pernr FROM TABLE gt_zhr_pos_pernr.
    ENDIF.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

    lo_gr_alv->display( ).

  ELSE.
    MESSAGE 'Filename is Mandatory' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
