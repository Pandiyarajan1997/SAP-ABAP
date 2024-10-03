*&---------------------------------------------------------------------*
*& Include          ZHR_ADMIN_LETTER_EMP_PBO
*&---------------------------------------------------------------------*

MODULE status_9001 OUTPUT.
  SET PF-STATUS 'HO_STATUS'.
  SET TITLEBAR 'HO_TITLE'.
  IF gt_list IS INITIAL.
    gv_name = 'G_LETTER_TYP'.
    SELECT domvalue_l AS key
           ddtext AS text
           FROM dd07t INTO TABLE gt_list
           WHERE domname = 'ZHR_D_LETTER_TYP' AND
                 as4local = 'A' AND
                 ddlanguage = 'E'.
    SORT gt_list BY key.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = gv_name
        values = gt_list.
    g_letter_typ = '11'.
  ENDIF.

  g_date = sy-datum.
  g_time = sy-uzeit.


  CASE g_letter_typ.
    WHEN '11'.
      g_ssn = '0101'. " Appoinment
      SELECT SINGLE ddtext AS text
             FROM dd07t INTO zhr_st_appt_letter-lstatus
             WHERE domname = 'ZLSTATUS' AND
                   as4local = 'A' AND
                   ddlanguage = 'E' AND
                   domvalue_l = gw_emp_letter-lstatus.
    WHEN '12'. g_ssn = '0102'. " Confirmation
    WHEN '13'. g_ssn = '0103'. " Transfer
    WHEN '14'. g_ssn = '0104'. " Position/Department Change
    WHEN OTHERS.
  ENDCASE.
*  go_main->process_data( ).
ENDMODULE.
