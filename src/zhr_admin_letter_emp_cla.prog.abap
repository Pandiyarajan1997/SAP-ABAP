*&---------------------------------------------------------------------*
*& Include          ZHR_ADMIN_LETTER_EMP_CLA
*&---------------------------------------------------------------------*
*MODULE status_9001 OUTPUT.
*  SET PF-STATUS 'HO_STATUS'.
*  SET TITLEBAR 'HO_TITLE'.
*  IF gt_list IS INITIAL.
*    gv_name = 'G_LETTER_TYP'.
*    gw_list-key = '1'.
*    gw_list-text = 'Appointment Letter'.
*    APPEND gw_list TO gt_list.
*
*    gw_list-key = '2'.
*    gw_list-text = 'Confirmation Letter'.
*    APPEND gw_list TO gt_list.
*  ENDIF.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = gv_name
*      values = gt_list.
*
*  go_main->process_data( ).
*
*  go_main->build_alv( ).
*ENDMODULE.
