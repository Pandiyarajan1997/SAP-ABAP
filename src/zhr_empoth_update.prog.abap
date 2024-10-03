*&---------------------------------------------------------------------*
*& Report zhr_empoth_update
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_empoth_update.
INCLUDE zhr_greythr_integration_top.
DATA:gt_emp_data TYPE TABLE OF zempoth_update,
     gs_emp_data LIKE LINE OF gt_emp_data,
     lv_pernr type pa0001-pernr.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS: s_pernr for lv_pernr NO INTERVALS.
SELECTION-SCREEN:END OF BLOCK b1.
INCLUDE zhr_greythr_empoth_update.

START-OF-SELECTION.
  SELECT FROM zempoth_update
  FIELDS *
  where pernr in @s_pernr
  INTO TABLE @gt_emp_data.
  IF sy-subrc = 0 .
   delete gt_emp_data where kostl_upd = abap_true and greyt_data_upd = abap_true
                        and greyt_manager_upd = abap_true and vend_code_upd = abap_true.
  IF gt_emp_data is NOT INITIAL.
    PERFORM f_greytipid_get.
    PERFORM f_get_hiredate.
    "Cost Centre update
    PERFORM f_update_costcenter_9001 CHANGING gt_emp_data .

    "Employee greythr Data
    PERFORM f_employee_work_details CHANGING gt_emp_data.

    "Manager Details Updation
    PERFORM f_get_manager_details CHANGING gt_emp_data.

    "Vendor code creation by Synchronizing BP
    PERFORM f_sync_bupa_emp_single CHANGING gt_emp_data.

    MODIFY zempoth_update FROM TABLE gt_emp_data .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.
*  ELSE.
*  Write:/ `No Data to Display!!!`.
  ENDIF.

END-OF-SELECTION.

  PERFORM alv_display."ALV_Display for 0105
