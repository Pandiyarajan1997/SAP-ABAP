*&---------------------------------------------------------------------*
*& Report ZHR_GREYTHR_INTEGRATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_greythr_integration.
"Data Declarations
INCLUDE zhr_greythr_integration_top.

**** Selection Screen Design **********
*DATA: lv_emp TYPE pa0001-pernr.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_empno FOR pa0001-pernr MATCHCODE OBJECT prem.
  SELECT-OPTIONS: s_bukrs FOR pa0001-bukrs.
*  PARAMETERS: p_empno TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_rad1  RADIOBUTTON GROUP rad USER-COMMAND grp1,
              p_rad2  RADIOBUTTON GROUP rad,
              p_rad3  RADIOBUTTON GROUP rad,
              p_rad4  RADIOBUTTON GROUP rad,
              p_rad5  RADIOBUTTON GROUP rad,
              p_rad6  RADIOBUTTON GROUP rad,
              p_rad7  RADIOBUTTON GROUP rad,
*---- Added by Samsudeen M on 03.03.2023 ----*
              p_rad8  RADIOBUTTON GROUP rad,
*---- Added by Samsudeen M on 01.04.2023 ----*
              p_rad9  RADIOBUTTON GROUP rad,
              p_rad10 RADIOBUTTON GROUP rad,
*----- Added by samsudeeb M on 11.04.2023 -----*
              p_rad11 RADIOBUTTON GROUP rad.
SELECTION-SCREEN END OF BLOCK b2.
*********************************************************

"Subroutines
INCLUDE zhr_greythr_integration_pforms.

START-OF-SELECTION.
******Greytip ID From Employee Lookup API*************
  REFRESH: gt_pernr.
*---- Changes by Samsudeen ON 03.03.2023 --------*
  "Personnel Number Selections

  IF s_empno[] IS INITIAL.
    SELECT
      pa0001~pernr
      INTO TABLE gt_pernr
      FROM pa0000 INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
      WHERE pa0000~pernr IN s_empno
        AND pa0000~stat2 EQ '3'
        AND pa0001~begda LE sy-datum
        AND pa0000~endda GE sy-datum
        AND pa0001~bukrs in s_bukrs.
    IF sy-subrc = 0.
      SORT gt_pernr[] BY pernr.
      DELETE ADJACENT DUPLICATES FROM gt_pernr[] COMPARING pernr.
    ENDIF.
  ELSE.
    SELECT pernr
      FROM pa0001
      INTO TABLE gt_pernr
     WHERE pernr in s_empno
       and begda le sy-datum
       and endda ge sy-datum
       and bukrs in s_bukrs
       and ( persg = 'C' or persg = 'F' or persg = 'E' or persg = 'G' ).
    IF sy-subrc = 0.
      SORT gt_pernr[] BY pernr.
      DELETE ADJACENT DUPLICATES FROM gt_pernr[] COMPARING pernr.
    ENDIF.
  ENDIF.

*----- End of Changes on 03.03.2023 --------*
  IF gt_pernr[] IS NOT INITIAL.

    PERFORM f_get_hiredate.

    PERFORM f_greytipid_get.

******Fetching State desc and Bloodgrp Desc based on greythr *****************
    SELECT * FROM zgreythr_tab INTO TABLE lt_greythr.
    "To identify the bank and branch From API
    SELECT * FROM zhr_greythr_lov
             INTO TABLE lt_lov.

******** Update Temporary ID Details *************
    IF p_rad1 = 'X'.
      PERFORM call_employee_lookup_api.
    ENDIF.

******** Update Bank Details *************
    IF p_rad2 = 'X'.
      "for checking existing branchcode
      SELECT banks
             bankl FROM bnka
                   INTO TABLE lt_bank.
      PERFORM call_employee_bank_api.
    ENDIF.

******** Update Address Details *************
    IF p_rad3 = 'X'.
      PERFORM call_employee_address_api.
    ENDIF.

******** Update GOVT ID Details *************
    IF p_rad4 = 'X'.
      PERFORM call_employee_identity_details.
    ENDIF.

******** Update Qualification Details *************
    IF p_rad5 = 'X'.
      PERFORM call_employee_qualification.
    ENDIF.

******** Update Family Details *************
    IF p_rad6 = 'X'.
      PERFORM call_employee_family_details.
    ENDIF.
*---- Added by Samsudeen M on 03.03.2023 ------*
    IF p_rad8 = abap_true.
      PERFORM update_costcenter_to_greyhr.
    ENDIF.
*---- Added by Samsudeen M on 01.04.2023 ------*
    IF p_rad9 = abap_true.
      "Manager Details Updation
      PERFORM f_get_manager_details.
    ENDIF.

    IF p_rad10 = abap_true.
      "Employee greythr Data
      PERFORM f_employee_work_details.
    ENDIF.
*--- End of Changes on 01.04.2023
*-- Start of addition on 11.04.2023
    IF p_rad11 = abap_true.
      PERFORM f_update_costcenter_9001.
    ENDIF.
*--- End of changes on 11.04.2023
  ENDIF.

END-OF-SELECTION.

  PERFORM alv_display."ALV_Display for 0105
