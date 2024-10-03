*&---------------------------------------------------------------------*
*& Report ZHR_EMPLOYEE_HIRING_PROGRAM
REPORT zhr_employee_hiring_program.


TYPE-POOLS  sscr.

TYPES: BEGIN OF ty_employee,      "Structure for Employee greytip Id API
         employeeid TYPE string,
         name       TYPE string,
         employeeno TYPE string,
         joindate   TYPE string,
         leftorg    TYPE string,
         email      TYPE string,
       END OF ty_employee.

TYPES: BEGIN OF ty_get_emp,
         employeeid       TYPE string,
         name             TYPE string,
         email            TYPE string,
         employeeno       TYPE string,
         dateofjoin       TYPE string,
         leavingdate      TYPE string,
         originalhiredate TYPE string,
         leftorg          TYPE string,
         lastmodified     TYPE string,
         status           TYPE string,
         dateofbirth      TYPE string,
         gender           TYPE string,
         probationperiod  TYPE string,
         personalemail2   TYPE string,
         personalemail3   TYPE string,
         mobile           TYPE string,
       END OF ty_get_emp.

TYPES: BEGIN OF ty_emp_dob,                    "structure for employee personal details
         employeeid     TYPE string,
         bloodgroup     TYPE string,
         maritalstatus  TYPE string,
         marriagedate   TYPE string,
         spousebirthday TYPE string,
         spousename     TYPE string,
         actualdob      TYPE string,
       END OF ty_emp_dob.

TYPES: BEGIN OF ty_emp_id,        "Structure for Employee Identity details API
         id            TYPE string,
         idcode        TYPE string,
         employeeid    TYPE string,
         documentno    TYPE string,
         nameasperdoc  TYPE string,
         ifsccode      TYPE string,
         expirydate    TYPE string,
         verified      TYPE string,
         verifieddate  TYPE string,
         aadharappno   TYPE string,
         enablemasking TYPE string,
         guid          TYPE string,
       END OF ty_emp_id.

TYPES: BEGIN OF ty_emp_address,    "Structure for Employee Address API
         employeeid  TYPE string,
         address1    TYPE string,
         address2    TYPE string,
         address3    TYPE string,
         city        TYPE string,
         state       TYPE string,
         country     TYPE string,
         pin         TYPE string,
         phone1      TYPE string,
         phone2      TYPE string,
         extnno      TYPE string,
         fax         TYPE string,
         mobile      TYPE string,
         email       TYPE string,
         name        TYPE string,
         addresstype TYPE string,
         companyid   TYPE string,
         recordid    TYPE string,
       END OF ty_emp_address.
TYPES: BEGIN OF ty_bank,
         id                TYPE string,
         bankaccountnumber TYPE string,
         accounttype       TYPE string,
         bankname          TYPE string,
         bankbranch        TYPE string,
         branchcode        TYPE string,
         salarypaymentmode TYPE string,
         ddpayableat       TYPE string,
         nameasperbank     TYPE string,
       END OF ty_bank.

TYPES: BEGIN OF ty_api_error, "structure for handling API error
         employee_id(8) TYPE c,
         api_name(50)   TYPE c,
         error(50)      TYPE c,
       END OF ty_api_error.

******Internal table for Handling API Error*********
DATA: gt_api_error TYPE TABLE OF ty_api_error,
      gs_api_error TYPE ty_api_error.

TYPES: BEGIN OF ty_url,           "Structure for API URL
         url1 TYPE string,
       END OF ty_url.

DATA: gt_url TYPE TABLE OF ty_url,
      gs_url TYPE ty_url.

TYPES: BEGIN OF ty_alv1,          "Structure For First ALV
         candidate_no(8) TYPE c,
         name            TYPE emnam,
         message(150)    TYPE c,
       END OF ty_alv1.

*******Internal Table for First RadioButton*********
DATA: gt_alv1 TYPE TABLE OF ty_alv1,
      gs_alv1 TYPE ty_alv1.

TYPES: BEGIN OF ty_alv,
         sel            TYPE char1, "ALV Display
         status_des(20) TYPE c,
         emp_grptxt(30) TYPE c,
         emp_subtxt(20) TYPE c,
         pos_name(40)   TYPE c,
         orgunitid      TYPE num08,
         orgunitname    TYPE char40,
         btrtl_des(15)  TYPE c,
         massg_txt(30)  TYPE c,
         persa_txt(30)  TYPE c,
         type(1)        TYPE c,
         lcolor(4)      TYPE c,
         msg(150)       TYPE c.
         INCLUDE STRUCTURE zhr_hiring_tab.
TYPES:       END OF ty_alv.

*****Internal Table for ALV Display**********
DATA: gt_display  TYPE STANDARD TABLE OF zhr_hiring_tab,
      gt_display1 TYPE TABLE OF ty_alv,
      gt_display2 TYPE TABLE OF ty_alv,
      gt_display3 TYPE TABLE OF ty_alv,
      gs_display  TYPE zhr_hiring_tab,
      gs_display1 TYPE ty_alv,
      gs_display2 TYPE ty_alv,
      gs_display3 TYPE ty_alv,
      lt_display  TYPE TABLE OF ty_alv,
      ls_display  TYPE ty_alv.

DATA ls_exclude TYPE ui_func.
DATA: lt_exclude TYPE ui_functions.


TYPES: BEGIN OF ty_state,
         land1 TYPE land1_gp,
         bland TYPE regio,
         bezei TYPE bezei20,
       END OF ty_state.

***********Internal table for state code*********
DATA: lt_state TYPE TABLE OF ty_state,
      ls_state TYPE ty_state.

TYPES: BEGIN OF ty_output,  "Structure for infotype creation status"
         pernr     TYPE pernr_d,
         infty     TYPE infty,
         subty     TYPE sbttx,
         status(1) TYPE c,
         message   TYPE char100,
       END OF ty_output.

DATA: gt_reason TYPE TABLE OF t530t,
      gs_reason TYPE t530t.

TYPES: BEGIN OF ty_persa,
         persa TYPE persa,
         name1 TYPE pbtxt,
         molga TYPE molga,
       END OF ty_persa.

DATA: gt_persa TYPE TABLE OF ty_persa,
      gs_persa TYPE ty_persa.

DATA: gt_persg TYPE TABLE OF t501t,
      gs_persg TYPE t501t.

DATA: gt_persk TYPE TABLE OF t503t,
      gs_persk TYPE t503t.

TYPES: BEGIN OF ty_persk,
         persg TYPE persg,
         persk TYPE persk,
       END OF ty_persk.
DATA: gt_persk1 TYPE STANDARD TABLE OF ty_persk,
      gs_persk1 TYPE ty_persk.

TYPES: BEGIN OF ty_btrtl,
         werks TYPE persa,
         btrtl TYPE btrtl,
         btext TYPE btrtx,
         molga TYPE molga,
       END OF ty_btrtl.

TYPES: BEGIN OF ty_werks,
         persa TYPE persa,
         name1 TYPE pbtxt,
         bukrs TYPE bukrs,
       END OF ty_werks.

DATA: gt_werks TYPE TABLE OF ty_werks,
      gs_werks TYPE ty_werks.


DATA: gt_btrtl     TYPE TABLE OF ty_btrtl,
      gt_btrtl_hlp TYPE TABLE OF ty_btrtl,
      gs_btrtl     TYPE ty_btrtl.

DATA: gt_position TYPE STANDARD TABLE OF hrp1000,
      gs_position TYPE hrp1000.

DATA: gt_position1 TYPE STANDARD TABLE OF hrp1001,
      gs_position1 TYPE hrp1001.

DATA: gt_mar_stat TYPE STANDARD TABLE OF t502t,
      gs_mar_stat TYPE t502t.
*****Internal Table For Error Display in infotype 0000 Creation*******
DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

**********Internal table for Greytip ID API***************
DATA: gt_employee TYPE TABLE OF ty_employee,
      gs_employee TYPE ty_employee.

**********Internal table for Get employee details  API***************
DATA: gt_emp_details TYPE TABLE OF ty_get_emp,
      gs_emp_details TYPE ty_get_emp.

**********Internal table for Employee personal details API**************
DATA: gt_personal TYPE TABLE OF ty_emp_dob,
      gs_personal TYPE ty_emp_dob.

**********Internal table for Address Details API***************
DATA: gt_address TYPE TABLE OF ty_emp_address,
      gs_address TYPE ty_emp_address.

**********Internal table for ID details API***************
DATA: gt_id_details TYPE TABLE OF ty_emp_id,
      gs_id_details TYPE ty_emp_id.

**********Internal table for Employee Bank Details From API***************
DATA: gt_bank TYPE TABLE OF ty_bank,
      gs_bank TYPE ty_bank.

DATA: lt_lov TYPE STANDARD TABLE OF zhr_greythr_lov,
      ls_lov TYPE zhr_greythr_lov.

DATA: ls_restrict TYPE sscr_restrict,
      ls_opt_list TYPE sscr_opt_list,
      ls_ass      TYPE sscr_ass,
      gv_class    TYPE klasse_d,
      lv_sobid    TYPE hrp1001-sobid.

DATA: str1       TYPE string,  "Access Token From Both Table and FM"
      str2       TYPE string,
      token      TYPE zaccess, "Function Module Access Token"
      create_url TYPE string,  "URL of API"
      emp_id(8)  TYPE c. "Actual input of API

DATA: lo_http_client TYPE REF TO if_http_client.

*********Internal Table and Work area for URL for all API***********
DATA: gs_tvarvc  TYPE tvarvc,
      gs_tvarvc1 TYPE tvarvc.

DATA: lv_url TYPE string.
DATA: lv_response   TYPE string, "API Response
      lv_codes      TYPE i,      "STATUS Code
      lv_http_error TYPE string. "STATUS Description

* Internal Table and Work Area Declaration for FieldCatlog
DATA :gt_fcat    TYPE lvc_t_fcat,
      gs_fcat    TYPE lvc_s_fcat,
      gt_exclude TYPE ui_functions,
      gs_excl    TYPE ui_func,
      gs_layout  TYPE lvc_s_layo.

CLASS lcl_handle_events DEFINITION DEFERRED.

DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_container.

*DATA: lv_container TYPE scrfname VALUE 'ALV_GRID_CONT'.

DATA: lo_grid1 TYPE REF TO cl_salv_table.

DATA: lv_msg(150) TYPE c.


DATA: state_desc(30) TYPE c. "To get State code

***Internal Table For hiring Table***********
DATA: lt_hiring  TYPE STANDARD TABLE OF zhr_hiring_tab,
      ls_hiring  TYPE zhr_hiring_tab,
      lt_hiring1 TYPE STANDARD TABLE OF zhr_hiring_tab,
      ls_hiring1 TYPE zhr_hiring_tab.

DATA: gv_pernr TYPE pernr_d,
      gv_ok    TYPE boole_d.
DATA: gc_endda(10) TYPE c.
DATA: l_log TYPE zhr_hiring_log.

TYPES: BEGIN OF ty_shlp,
         temp_empno   TYPE ztemp_no,
         name         TYPE emnam,
         joining_date TYPE datum,
       END OF ty_shlp.
DATA: gt_shlp TYPE STANDARD TABLE OF ty_shlp.

*******SELECTION SCREEN DESIGN**************************
DATA:     lv_tempno(8) TYPE c.
DATA: lv_pernr TYPE pa0001-pernr,
      lv_date  TYPE pa0041-dat01,
      lv_stat  TYPE zhr_hiring_tab-status.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_rad1 RADIOBUTTON GROUP rad USER-COMMAND grp1 MODIF ID bl1,
              p_rad2 RADIOBUTTON GROUP rad MODIF ID bl1,
              p_rad5 RADIOBUTTON GROUP rad MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_rad3 RADIOBUTTON GROUP rad1 USER-COMMAND grp1 MODIF ID bl4,
              p_rad4 RADIOBUTTON GROUP rad1 MODIF ID bl4.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_tempno FOR lv_tempno MODIF ID bl2 NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: s_pernr FOR lv_pernr MODIF ID bl3,
                  s_date  FOR lv_date MODIF ID bl3,
                  s_stat  FOR lv_stat MODIF ID bl3.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  PERFORM f_restrict_option USING 'S_TEMPNO'.


***********Value help ********************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tempno-low.
  SELECT temp_empno name joining_date FROM zhr_hiring_tab INTO TABLE gt_shlp.     "#EC CI_NOWHERE

  IF sy-subrc EQ 0.

    SORT gt_shlp BY temp_empno.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'TEMP_EMPNO'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'S_TEMPNO '
        value_org   = 'S'
      TABLES
        value_tab   = gt_shlp.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_screen_adjustments.



CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

    METHODS:
      handle_on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no
                  er_event_data.

ENDCLASS.

********Editable Field in ALV Display****************
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   TYPE lvc_s_modi,
          lv_index TYPE i.
** Editable Employee Group Field in Propose Screen ***
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERSK'.
    IF sy-subrc = 0.
      CLEAR gs_display2.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-persk = ls_row-value.
        IF gs_display2-persk IS NOT INITIAL.
          READ TABLE gt_persk INTO gs_persk WITH KEY persk = gs_display2-persk.
          IF sy-subrc EQ 0.
            gs_display2-emp_subtxt = gs_persk-ptext.
          ELSE.
            MESSAGE 'Employee sub group is incorrect' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR gs_display2-emp_subtxt.
        ENDIF.
        MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
*** Editable Personal Area field in Propose Screen ***
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'WERKS'.
    IF sy-subrc = 0.
      CLEAR gs_display2.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-werks = ls_row-value.
        IF gs_display2-werks IS NOT INITIAL.
          READ TABLE gt_persa INTO gs_persa WITH KEY persa = gs_display2-werks.
          IF sy-subrc EQ 0.
            gs_display2-persa_txt = gs_persa-name1.
          ELSE.
            MESSAGE 'Personal Area is incorrect' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR: gs_display2-persa_txt, gs_display2-btrtl, gs_display2-btrtl_des.
        ENDIF.
        MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
***Employee Personal Subarea editable field in Propose Screen **
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'BTRTL'.
    IF sy-subrc = 0.
      CLEAR gs_display2.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-btrtl = ls_row-value.
        IF gs_display2-btrtl IS NOT INITIAL.
          CLEAR gs_btrtl.
          READ TABLE gt_btrtl INTO gs_btrtl WITH KEY werks = gs_display2-werks
                                                     btrtl = gs_display2-btrtl.
          IF sy-subrc EQ 0.
            gs_display2-btrtl_des = gs_btrtl-btext.
          ELSE.
            DATA(e_msg) = |Please Check Personal Area and Sub Area|.
            MESSAGE e_msg TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR gs_display2-btrtl_des.
        ENDIF.
        MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
** Employee Position editable in Propose Screen **
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PLANS'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-plans = ls_row-value.
        IF gs_display2-plans IS NOT INITIAL.
*-- Added by Samsudeen on 02.03.2023 -------*
          SELECT SINGLE sobid FROM hrp1001
           INTO  gs_display2-costcenter
           WHERE otype EQ 'S' AND
           objid EQ gs_display2-plans
           AND plvar EQ '01' AND
            rsign EQ 'A' AND relat EQ '011'
            AND istat EQ '1' AND
           sclas EQ 'K' AND
           begda LE sy-datum AND endda GE sy-datum.
          IF sy-subrc NE 0.
            CLEAR lv_msg.
            lv_msg = |Costcenter is Missing for Position { gs_display2-plans }|.
            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
          READ TABLE gt_position INTO gs_position WITH KEY objid = gs_display2-plans.
          IF sy-subrc EQ 0.
            gs_display2-pos_name = gs_position-mc_stext.
          ELSE.
            MESSAGE 'Position is incorrect' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR gs_display2-pos_name.
        ENDIF.
        MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
** Employee Reason For Hiring Editable Field in Propose Screen **
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'MASSG'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-massg = ls_row-value.
        IF gs_display2-massg IS NOT INITIAL.
          READ TABLE gt_reason INTO gs_reason WITH KEY massg = gs_display2-massg.
          IF sy-subrc EQ 0.
            gs_display2-massg_txt = gs_reason-mgtxt.
          ELSE.
            MESSAGE 'Reason for Hiring is incorrect' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR gs_display2-massg_txt.
        ENDIF.
        MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
** Employee Address editable in Propose Screen **
    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_ADDRESS1'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_address1 = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_ADDRESS2'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_address2 = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_ADDRESS3'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_address3 = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_CITY'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_city = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_STATE'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_state = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_COUNTRY'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_country = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'PERM_POSTAL_CODE'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-perm_postal_code = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'MOBILE_NO'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-mobile_no = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'BANKN'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-bankn = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

    CLEAR:gs_display2, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'BANKL'.
    IF sy-subrc = 0.
      READ  TABLE gt_display2 INTO gs_display2 INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display2-bankl = ls_row-value.
      ENDIF.
      MODIFY gt_display2 FROM gs_display2 INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.
  METHOD handle_on_f4.
    PERFORM handle_on_f4
     USING e_fieldname
           es_row_no
           er_event_data.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  PERFORM f_initial_selections.


*&------- Employee Data Extraction from API Radio button ----

  IF ( p_rad1 = 'X' AND s_tempno[] IS NOT INITIAL ).
************Fetching Hiring table for Record Existence ******************
    SELECT * FROM zhr_hiring_tab "#EC CI_NOWHERE
             INTO TABLE @DATA(gt_hiring).

    REFRESH: gt_alv1.
    LOOP AT s_tempno.
      READ TABLE gt_hiring INTO DATA(gs_hiring) WITH KEY temp_empno = s_tempno-low.
      IF sy-subrc EQ 0.
        CLEAR gs_alv1.
        gs_alv1-candidate_no = gs_hiring-temp_empno.
        gs_alv1-name =  gs_hiring-name.
        gs_alv1-message = |Candidate Details Already Exists in Table|.
        APPEND gs_alv1 TO gt_alv1.
      ELSE.
        PERFORM data_extraction.  "API Data EXtraaction
      ENDIF.
    ENDLOOP.


    IF gt_alv1[] IS NOT INITIAL.
      PERFORM alv_display. "API ALV Display
    ENDIF.

*&-------Propose Employee Creation Radio button ----&
  ELSEIF p_rad2 = 'X'.

    REFRESH gt_display.

    SELECT *  FROM zhr_hiring_tab
              INTO TABLE gt_display
              WHERE temp_empno IN s_tempno
                AND status = '01'.

    IF gt_display[] IS NOT INITIAL.
      MOVE-CORRESPONDING gt_display TO gt_display2.
      IF gt_display2[] IS NOT INITIAL.
        LOOP AT gt_display2 INTO gs_display2.
          CLEAR gs_display2-status_des.
          IF gs_display2-status = '01'.
            gs_display2-status_des = 'Initiated'.
          ELSEIF gs_display2-status = '02'.
            gs_display2-status_des = 'Proposed'.
          ELSEIF gs_display2-status = '03'.
            gs_display2-status_des = 'Approved'.
          ELSEIF gs_display2-status = '04'.
            gs_display2-status_des = 'Created'.
          ELSEIF gs_display2-status = '05'.
            gs_display2-status_des = 'Greythr Updated'.
          ENDIF.

          READ TABLE gt_persg INTO gs_persg WITH KEY persg = gs_display2-persg.
          IF sy-subrc EQ 0.
            gs_display2-emp_grptxt = gs_persg-ptext.
          ENDIF.

          READ TABLE gt_position INTO gs_position WITH KEY objid = gs_display2-plans.
          IF sy-subrc EQ 0.
            gs_display2-pos_name = gs_position-mc_stext.

            PERFORM f_get_orgdetail USING gs_display2-plans
                                    CHANGING gs_display2-orgunitid
                                             gs_display2-orgunitname.


          ENDIF.
          READ TABLE gt_reason INTO gs_reason WITH KEY massg = gs_display2-massg.
          IF sy-subrc EQ 0.
            gs_display2-massg_txt = gs_reason-mgtxt.
          ENDIF.
          READ TABLE gt_persk INTO gs_persk WITH KEY persk = gs_display2-persk.
          IF sy-subrc EQ 0.
            gs_display2-emp_subtxt = gs_persk-ptext.
          ENDIF.

          READ TABLE gt_persa INTO gs_persa WITH KEY persa = gs_display2-werks.
          IF sy-subrc EQ 0.
            gs_display2-persa_txt = gs_persa-name1.
          ENDIF.

          READ TABLE gt_btrtl INTO gs_btrtl WITH KEY werks = gs_display2-werks btrtl = gs_display2-btrtl.
          IF sy-subrc EQ 0.
            gs_display2-btrtl_des = gs_btrtl-btext.
          ENDIF.

          MODIFY gt_display2 FROM gs_display2.
        ENDLOOP.
        PERFORM propose_alv.
      ELSE.
        MESSAGE 'No Candidate available for Hiring Proposal' TYPE 'E'.
      ENDIF.
    ENDIF.
*&-------Approve Employee Creation Radio button ----&
  ELSEIF p_rad3 = 'X'.

    REFRESH: gt_display.

    SELECT * FROM zhr_hiring_tab
             INTO TABLE gt_display
             WHERE status = '02'
               AND temp_empno IN s_tempno.

    IF gt_display[] IS NOT INITIAL.
      MOVE-CORRESPONDING gt_display TO gt_display1.
      IF gt_display1[] IS NOT INITIAL.
        LOOP AT gt_display1 INTO gs_display1.
          CLEAR gs_display1-status_des.
          IF gs_display1-status = '01'.
            gs_display1-status_des = 'Initiated'.
          ELSEIF gs_display1-status = '02'.
            gs_display1-status_des = 'Proposed'.
          ELSEIF gs_display1-status = '03'.
            gs_display1-status_des = 'Approved'.
          ELSEIF gs_display1-status = '04'.
            gs_display1-status_des = 'Created'.
          ELSEIF gs_display1-status = '05'.
            gs_display1-status_des = 'Greythr Updated'.
          ENDIF.


          READ TABLE gt_position INTO gs_position WITH KEY objid = gs_display1-plans.
          IF sy-subrc EQ 0.
            gs_display1-pos_name = gs_position-mc_stext.

            PERFORM f_get_orgdetail USING gs_display1-plans
                                    CHANGING gs_display1-orgunitid
                                             gs_display1-orgunitname.
          ENDIF.

          READ TABLE gt_reason INTO gs_reason WITH KEY massg = gs_display1-massg.
          IF sy-subrc EQ 0.
            gs_display1-massg_txt = gs_reason-mgtxt.
          ENDIF.

          READ TABLE gt_persg INTO gs_persg WITH KEY persg = gs_display1-persg.
          IF sy-subrc EQ 0.
            gs_display1-emp_grptxt = gs_persg-ptext.
          ENDIF.

          READ TABLE gt_persk INTO gs_persk WITH KEY persk = gs_display1-persk.
          IF sy-subrc EQ 0.
            gs_display1-emp_subtxt = gs_persk-ptext.
          ENDIF.

          READ TABLE gt_persa INTO gs_persa WITH KEY persa = gs_display1-werks.
          IF sy-subrc EQ 0.
            gs_display1-persa_txt = gs_persa-name1.
          ENDIF.

          READ TABLE gt_btrtl INTO gs_btrtl WITH KEY werks = gs_display1-werks btrtl = gs_display1-btrtl.
          IF sy-subrc EQ 0.
            gs_display1-btrtl_des = gs_btrtl-btext.
          ENDIF.

          MODIFY gt_display1 FROM gs_display1.
        ENDLOOP.
        PERFORM approve_alv.
      ELSE.
        MESSAGE 'No Candidate is in Status 02' TYPE 'E'.
      ENDIF.
    ENDIF.
*&-------Display Employee Details Radio button ----&
  ELSEIF p_rad5 = 'X'.
    REFRESH gt_display.

    SELECT * FROM zhr_hiring_tab
             INTO TABLE gt_display
             WHERE temp_empno IN s_tempno
              AND  status IN s_stat
              AND  pernr IN s_pernr
              AND  joining_date IN s_date.

    IF gt_display[] IS NOT INITIAL.
      MOVE-CORRESPONDING gt_display TO lt_display.
      IF lt_display[] IS NOT INITIAL.
        LOOP AT lt_display INTO ls_display.
          CLEAR ls_display-status_des.
          IF ls_display-status = '01'.
            ls_display-status_des = 'Initiated'.
          ELSEIF ls_display-status = '02'.
            ls_display-status_des = 'Proposed'.
          ELSEIF ls_display-status = '03'.
            ls_display-status_des = 'Approved'.
          ELSEIF ls_display-status = '04'.
            ls_display-status_des = 'Created'.
          ELSEIF ls_display-status = '05'.
            ls_display-status_des = 'Greythr Updated'.
          ENDIF.
          READ TABLE gt_position INTO gs_position WITH KEY objid = ls_display-plans.
          IF sy-subrc EQ 0.
            ls_display-pos_name = gs_position-mc_stext.
            PERFORM f_get_orgdetail USING ls_display-plans
                                    CHANGING ls_display-orgunitid
                                             ls_display-orgunitname.
          ENDIF.
          READ TABLE gt_reason INTO gs_reason WITH KEY massg = ls_display-massg.
          IF sy-subrc EQ 0.
            ls_display-massg_txt = gs_reason-mgtxt.
          ENDIF.

          READ TABLE gt_persg INTO gs_persg WITH KEY persg = ls_display-persg.
          IF sy-subrc EQ 0.
            ls_display-emp_grptxt = gs_persg-ptext.
          ENDIF.

          READ TABLE gt_persk INTO gs_persk WITH KEY persk = ls_display-persk.
          IF sy-subrc EQ 0.
            ls_display-emp_subtxt = gs_persk-ptext.
          ENDIF.

          READ TABLE gt_persa INTO gs_persa WITH KEY persa = ls_display-werks.
          IF sy-subrc EQ 0.
            ls_display-persa_txt = gs_persa-name1.
          ENDIF.

          READ TABLE gt_btrtl INTO gs_btrtl WITH KEY werks = gs_display1-werks btrtl = ls_display-btrtl.
          IF sy-subrc EQ 0.
            ls_display-btrtl_des = gs_btrtl-btext.
          ENDIF.

          MODIFY lt_display FROM ls_display.
        ENDLOOP.
      ENDIF.
      PERFORM display_alv.
    ENDIF.
  ELSEIF p_rad4 = 'X'.
    REFRESH gt_display.

    SELECT * FROM zhr_hiring_tab
             INTO TABLE gt_display
             WHERE temp_empno IN s_tempno
              AND  status IN s_stat
              AND  pernr IN s_pernr
              AND  joining_date IN s_date.

    IF gt_display[] IS NOT INITIAL.
      MOVE-CORRESPONDING gt_display TO lt_display.
      IF lt_display[] IS NOT INITIAL.
        LOOP AT lt_display INTO ls_display.
          CLEAR ls_display-status_des.
          IF ls_display-status = '01'.
            ls_display-status_des = 'Initiated'.
          ELSEIF ls_display-status = '02'.
            ls_display-status_des = 'Proposed'.
          ELSEIF ls_display-status = '03'.
            ls_display-status_des = 'Approved'.
          ELSEIF ls_display-status = '04'.
            ls_display-status_des = 'Created'.
          ELSEIF ls_display-status = '05'.
            ls_display-status_des = 'Greythr Updated'.
          ENDIF.
          READ TABLE gt_position INTO gs_position WITH KEY objid = ls_display-plans.
          IF sy-subrc EQ 0.
            CLEAR gs_display1-pos_name.
            ls_display-pos_name = gs_position-mc_stext.
            PERFORM f_get_orgdetail USING ls_display-plans
                                    CHANGING ls_display-orgunitid
                                             ls_display-orgunitname.
          ENDIF.
          READ TABLE gt_reason INTO gs_reason WITH KEY massg = ls_display-massg.
          IF sy-subrc EQ 0.
            ls_display-massg_txt = gs_reason-mgtxt.
          ENDIF.

          READ TABLE gt_persg INTO gs_persg WITH KEY persg = ls_display-persg.
          IF sy-subrc EQ 0.
            ls_display-emp_grptxt = gs_persg-ptext.
          ENDIF.

          READ TABLE gt_persk INTO gs_persk WITH KEY persk = ls_display-persk.
          IF sy-subrc EQ 0.
            ls_display-emp_subtxt = gs_persk-ptext.
          ENDIF.

          READ TABLE gt_persa INTO gs_persa WITH KEY persa = ls_display-werks.
          IF sy-subrc EQ 0.
            ls_display-persa_txt = gs_persa-name1.
          ENDIF.

          READ TABLE gt_btrtl INTO gs_btrtl WITH KEY btrtl = ls_display-btrtl.
          IF sy-subrc EQ 0.
            ls_display-btrtl_des = gs_btrtl-btext.
          ENDIF.

          MODIFY lt_display FROM ls_display.
        ENDLOOP.
      ENDIF.
      PERFORM display_alv.
    ENDIF.
  ENDIF.


*END-OF-SELECTION.
*
*  PERFORM F_Pernr_bp_job.


*&--------Data Extraction from API -------&
FORM data_extraction.
***getting temp_id FOR TEMPORARY employee number*******
  REFRESH:gt_employee,gt_emp_details,gt_personal,gt_address,
           gt_id_details,gt_bank.
  REFRESH gt_api_error.
  PERFORM employee_lookup_api.
  IF gt_api_error[] IS NOT INITIAL.
*      cl_demo_output=>display( gt_api_error ).
    CLEAR gs_alv1.
    gs_alv1-candidate_no = s_tempno-low.
    gs_alv1-message = 'candidate details deos not exist'.
    APPEND gs_alv1 TO gt_alv1.
*    CONTINUE.
  ENDIF.

  PERFORM get_employee_details.  "Get Employee API

  PERFORM employee_personal_details. "Get Employee Personal API

  PERFORM employee_address_api. "Get Employee Address API

  PERFORM employee_identity_details. "Get Employee Identity API


  CLEAR gs_display.
  gs_display-temp_empno = s_tempno-low.
  gs_display-status = '01'.
  gs_display-persg = 'C'.

  LOOP AT gt_employee INTO gs_employee.
    gs_display-temp_empid = gs_employee-employeeid.
    gs_display-name = gs_employee-name.
    gs_display-e_mail = gs_employee-email.
  ENDLOOP.

  CLEAR gs_bank.
  PERFORM employee_bank_details_api USING  gs_display-temp_empid CHANGING gs_bank.

  READ TABLE gt_emp_details INTO gs_emp_details WITH KEY employeeid = gs_employee-employeeid.
  IF sy-subrc EQ 0.
    gs_display-joining_date = gs_emp_details-dateofjoin.
    gs_display-mobile_no = gs_emp_details-mobile.
    gs_display-dob = gs_emp_details-dateofbirth.
    gs_display-gender = gs_emp_details-gender.
  ENDIF.

*    READ TABLE lt_lov INTO ls_lov WITH KEY field_code = '11'
*                                           id = gs_personal-bloodgroup.
*    IF sy-subrc EQ 0.
*      gs_display-blood_grp = ls_lov-value.
*    ENDIF.

  READ TABLE gt_personal INTO gs_personal WITH KEY  employeeid  = gs_employee-employeeid.
  IF sy-subrc EQ 0.
    READ TABLE lt_lov INTO ls_lov WITH KEY  field_code = '12'
                                          id = gs_personal-maritalstatus.
    IF sy-subrc EQ 0.
      CASE ls_lov-value.
        WHEN 'Married'.
          gs_display-famst = '1'.  "Married
        WHEN 'Single'.
          gs_display-famst = '0'.  "Single
        WHEN 'Widowed'.
          gs_display-famst = '2'.  "Widowed
        WHEN 'Seperated'.
          gs_display-famst = '5'.  "Separated
      ENDCASE.
    ELSE.
      gs_display-famst = '1'.  "Unknown
    ENDIF.
    gs_display-famdt = gs_personal-marriagedate.
  ENDIF.


  LOOP AT gt_address INTO gs_address WHERE employeeid = gs_employee-employeeid.
    IF gs_address-addresstype = 'permanentaddress'.
      gs_display-perm_address = gs_address-addresstype.
      gs_display-perm_postal_code = gs_address-pin.
      gs_display-perm_address1 = gs_address-address1.
      gs_display-perm_address2 = gs_address-address2.
      gs_display-perm_address3 = gs_address-address3.
      gs_display-perm_city = gs_address-city.
      gs_display-p_phone1 = gs_address-phone1.
      gs_display-p_phone2 = gs_address-phone2.
    ELSEIF gs_address-addresstype = 'presentaddress'.
      gs_display-pres_address = gs_address-addresstype.
      gs_display-pres_postal_code = gs_address-pin.
      gs_display-pres_address1 = gs_address-address1.
      gs_display-pres_address2 = gs_address-address2.
      gs_display-pres_address3 = gs_address-address3.
      gs_display-pres_city = gs_address-city.
      gs_display-pr_phone1 = gs_address-phone1.
      gs_display-pr_phone2 = gs_address-phone2.
*** Added on 15.11.2022 by Samsudeen M ***
    ELSEIF gs_address-addresstype = 'emergencyaddress'.
      gs_display-care_of = gs_address-name.
      gs_display-em_contact = gs_address-mobile.
    ENDIF.
    READ TABLE lt_lov INTO ls_lov WITH KEY field_code = '13'
                                         id = gs_address-country.
    IF ( sy-subrc EQ 0 AND ls_lov-value EQ 'Indian' ).
      gs_display-natio = 'IN'.
      gs_display-perm_country = 'IN'.
      gs_display-pres_country = 'IN'.
    ENDIF.
    READ TABLE lt_lov INTO ls_lov WITH KEY field_code = '17'
                                         id = gs_address-state.
    IF sy-subrc EQ 0.
      CLEAR state_desc.
      state_desc = ls_lov-value.
    ENDIF.

    READ TABLE lt_state INTO ls_state WITH KEY bezei = state_desc.
    IF sy-subrc EQ 0.
      gs_display-perm_state = ls_state-bland.
      gs_display-pres_state = ls_state-bland.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_id_details INTO gs_id_details WHERE employeeid = gs_employee-employeeid.
    IF gs_id_details-idcode = 'AADHAR'.
      gs_display-aadhar_no = gs_id_details-documentno.
      gs_display-aadhar_name = gs_id_details-nameasperdoc.
    ELSEIF gs_id_details-idcode = 'PAN'.
      gs_display-pancard = gs_id_details-documentno.
      gs_display-pan_name = gs_id_details-nameasperdoc.
    ELSEIF gs_id_details-idcode = 'PASSPORT'.
      gs_display-passport = gs_id_details-documentno.
      gs_display-passport_name = gs_id_details-nameasperdoc.
      gs_display-expid = gs_id_details-expirydate.
    ENDIF.
  ENDLOOP.

  gs_display-bankn = gs_bank-bankaccountnumber.
  gs_display-bankl = gs_bank-branchcode.
  gs_display-bankcode = gs_bank-bankname.
*  READ TABLE gt_bank ASSIGNING FIELD-SYMBOL(<fs_bank>) WITH KEY id = gs_employee-employeeid.
*  IF sy-subrc = 0.
*    gs_display-bankn = <fs_bank>-bankaccountnumber.
*    gs_display-bankl = <fs_bank>-branchcode.
*  ENDIF.

  IF gs_display-temp_empid IS NOT INITIAL.
    MODIFY zhr_hiring_tab FROM gs_display.
    APPEND gs_display TO gt_display.
    CLEAR gs_alv1.
    gs_alv1-candidate_no = gs_display-temp_empno.
    gs_alv1-name =  gs_display-name.
    gs_alv1-message = 'candidate Downloaded Successfully and Stored'.
    APPEND gs_alv1 TO gt_alv1.
  ENDIF.

*  PERFORM update_table. "Updating Table (ZHR_HIRING_TAB)


ENDFORM.

******Get ACCESS TOKEN API
FORM get_access_token.

  CALL FUNCTION 'ZHR_GET_ACCESS_TOKEN'
    IMPORTING
      access_token          = token
    EXCEPTIONS
      communication_failure = 1
      status_failure        = 2
      OTHERS                = 3.

  CLEAR str1.
  str1 = token.

ENDFORM.

****GETTING TEMP_ID FOR TEMPORARY EMPLOYEE NUMBER*******
FORM employee_lookup_api .

  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMP_LOOKUP' AND type = 'S'.

  CLEAR create_url.
  create_url = gs_tvarvc-low.
  CONCATENATE create_url s_tempno-low INTO create_url.
  CONDENSE create_url NO-GAPS.

  CLEAR lv_response.
  "Actual API_CALL
  PERFORM api_call.


  IF lv_response IS INITIAL.
    REFRESH gt_api_error.
    CLEAR gs_api_error.
    gs_api_error-employee_id = s_tempno-low.
    gs_api_error-api_name = 'Employee_Lookup_API'.
    gs_api_error-error = lv_http_error.
    APPEND gs_api_error TO gt_api_error.
  ELSE.
    CLEAR gs_employee.
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_employee ).

    APPEND gs_employee TO gt_employee.

    LOOP AT gt_employee INTO gs_employee.
      CLEAR emp_id.
      emp_id = gs_employee-employeeid.
    ENDLOOP.

  ENDIF.
ENDFORM.

*&------ Employee Basic information API Extraction ----&
FORM get_employee_details.


  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
  AND type = 'P'.
  CLEAR lv_url.
  lv_url = gs_tvarvc-low.

  CLEAR create_url.
  CONCATENATE lv_url emp_id INTO create_url.
  CONDENSE create_url NO-GAPS.

  CLEAR lv_response.
  "Actual API CALL
  PERFORM api_call.

  IF lv_response IS INITIAL.
    REFRESH gt_api_error.
    CLEAR gs_api_error.
    gs_api_error-employee_id = emp_id.
    gs_api_error-api_name = 'Get_Employee_API'.
    gs_api_error-error = lv_http_error.
    APPEND gs_api_error TO gt_api_error.
  ELSE.
    CLEAR gs_emp_details.
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_emp_details ).

    REPLACE ALL OCCURRENCES OF '-' IN gs_emp_details-dateofjoin WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN gs_emp_details-dateofbirth WITH ''.
    APPEND gs_emp_details TO gt_emp_details.
  ENDIF.

ENDFORM.

**&------ Employee Personal Details API Extraction -----&
FORM employee_personal_details.

  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
  AND type = 'P'.
  CLEAR lv_url.
  lv_url = gs_tvarvc-low.

  CLEAR create_url.
  CONCATENATE lv_url emp_id '/personal' INTO create_url.
  CONDENSE create_url NO-GAPS.

  CLEAR lv_response.
  "Actual API CALL
  PERFORM api_call.

  IF lv_response IS INITIAL.
    REFRESH gt_api_error.
    CLEAR gs_api_error.
    gs_api_error-employee_id = emp_id.
    gs_api_error-api_name = 'Get_Employee_Personal_API'.
    gs_api_error-error = lv_http_error.
    APPEND gs_api_error TO gt_api_error.
  ELSE.
    CLEAR gs_personal.
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_personal ).
    REPLACE ALL OCCURRENCES OF '-' IN gs_personal-marriagedate WITH ''.
    APPEND gs_personal TO gt_personal.
  ENDIF.

ENDFORM.

*&-- Employee Address API Extrraction ---*
FORM employee_address_api .

  DATA: create_url1 TYPE string,
        create_url2 TYPE string,
        create_url3 TYPE string. "added by Samsudeen on 15.11.2022

  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
  AND type = 'P'.

  CLEAR lv_url.
  lv_url = gs_tvarvc-low.

  REFRESH : gt_url.
  CLEAR: create_url1,create_url2,create_url3 .
  CONCATENATE lv_url emp_id '/addresses/permanentaddress' INTO create_url1.
  CONDENSE create_url1 NO-GAPS.

  CLEAR gs_url.
  gs_url-url1 = create_url1.
  APPEND gs_url TO gt_url.

  CONCATENATE lv_url emp_id '/addresses/presentaddress' INTO create_url2.
  CONDENSE create_url2 NO-GAPS.

  CLEAR gs_url.
  gs_url-url1 = create_url2.
  APPEND gs_url TO gt_url.

*** Added by samsudeen 0n 15.11.2022 for Emergency address ***
  CONCATENATE lv_url emp_id '/addresses/emergencyaddress' INTO create_url3.
  CONDENSE create_url2 NO-GAPS.

  CLEAR gs_url.
  gs_url-url1 = create_url3.
  APPEND gs_url TO gt_url.

  LOOP AT gt_url INTO gs_url.
    CLEAR create_url.
    create_url = gs_url-url1.

    CLEAR lv_response.
    PERFORM api_call.

    IF lv_response IS INITIAL.
      REFRESH gt_api_error.
      CLEAR gs_api_error.
      gs_api_error-employee_id = emp_id.
      gs_api_error-api_name = 'Get_Employee_Address_API'.
      gs_api_error-error = lv_http_error.
      APPEND gs_api_error TO gt_api_error.
    ELSE.
      CLEAR gs_address.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gs_address ).

      APPEND gs_address TO gt_address.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---- Employee Identity API Extraction -----------*
FORM employee_identity_details .
  DATA: create_url1 TYPE string,
        create_url2 TYPE string,
        create_url3 TYPE string.


  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMP_ID_DETAILS' AND type = 'S'.

  REFRESH:gt_url.

  CLEAR lv_url.
  lv_url = gs_tvarvc-low.

  CLEAR create_url.
  CONCATENATE lv_url emp_id '/identities' INTO create_url.
  CONDENSE create_url NO-GAPS.

  CLEAR lv_response.
  PERFORM api_call.

  IF lv_response IS INITIAL.
    REFRESH gt_api_error.
    CLEAR gs_api_error.
    gs_api_error-employee_id = emp_id.
    gs_api_error-api_name = 'Get_Employee_Identity_API'.
    gs_api_error-error = lv_http_error.
    APPEND gs_api_error TO gt_api_error.
  ELSE.
    CLEAR gs_id_details.

    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_id_details ).

    LOOP AT gt_id_details INTO gs_id_details.
      REPLACE ALL OCCURRENCES OF '-' IN gs_id_details-expirydate WITH ''.
      MODIFY gt_id_details FROM gs_id_details.
    ENDLOOP.
  ENDIF.

ENDFORM.
*** Bank Details API for data fetch ****
FORM employee_bank_details_api USING p_empid CHANGING p_bankdtls TYPE ty_bank.
  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
                                             AND type = 'P'.
  CLEAR lv_url.
  lv_url = gs_tvarvc-low.
  CLEAR create_url.
  CONCATENATE lv_url p_empid '/bank' INTO create_url.
  CONDENSE create_url NO-GAPS.

  CLEAR lv_response.
  PERFORM api_call.

  IF lv_response IS INITIAL.
    REFRESH gt_api_error.
    CLEAR gs_api_error.
    gs_api_error-employee_id = emp_id.
    gs_api_error-api_name = 'Get_Employee_Bank_API'.
    gs_api_error-error = lv_http_error.
    APPEND gs_api_error TO gt_api_error.
  ELSE.
    CLEAR gs_bank.
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = p_bankdtls ).
    APPEND p_bankdtls TO gt_bank.
  ENDIF.
ENDFORM.
*&------- Actual API CALL ------------*
FORM api_call .
  cl_http_client=>create_by_url(
    EXPORTING
    url = create_url
    IMPORTING
    client = lo_http_client
    EXCEPTIONS
    argument_not_found = 1
    plugin_not_active = 2
    internal_error = 3
    OTHERS = 4 ).

  CHECK lo_http_client IS BOUND.
  lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.


  lo_http_client->request->set_method(
   EXPORTING
   method = if_http_entity=>co_request_method_get ).


  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  CLEAR gs_tvarvc1.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc1 WHERE name = 'URL_LOV_API'
  AND type = 'P'.

  CLEAR str2.
  str2 = gs_tvarvc1-low." Value for authorization

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = str2 ).


*  lo_http_client->request->set_cdata(
*   EXPORTING
*   data = '{ JSON_Payload }' ).


  lo_http_client->send(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2 ).


  CHECK sy-subrc = 0.
  lo_http_client->receive(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2
   http_processing_failed = 3 ).


  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).

  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
    lv_response = lo_http_client->response->get_cdata( ).
  ENDIF.
ENDFORM.


***********API Data Download ALV Message*******************
FORM alv_display.

  REFRESH gt_fcat.

  PERFORM f_fieldcat USING 'CANDIDATE_NO' 'Candidate No' 1 space space space space space space.
  PERFORM f_fieldcat USING 'NAME'         'Name'         2 space space space space space space.
  PERFORM f_fieldcat USING 'MESSAGE'      'Message'      3 space space space space space space.

  CALL SCREEN 200.
ENDFORM.

*************Propose ALV Screen***********************
FORM propose_alv.

  REFRESH gt_fcat.

  PERFORM f_fieldcat USING  'SEL'               'Select'             1 'X' space  space     space   space 'X'.
  PERFORM f_fieldcat USING  'TEMP_EMPNO'        'Candidate No'       2 space space  space     space   space space .
  PERFORM f_fieldcat USING  'TEMP_EMPID'        'Candidate ID'       3 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS'            'Status'             4 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS_DES'        'Status Des'         5 space space  'GT_DISPLAY2'     'STATUS_DES'   space space.
  PERFORM f_fieldcat USING  'NAME'              'EmpName'            6 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERNR'             'Personnel No'       7 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PERSG'             'EmpGroup'           8 space space  space     space   space space.
  PERFORM f_fieldcat USING  'EMP_GRPTXT'        'EmpGroup'           9 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERSK'             'EmpSubgroup'       10 'X'   'X'    space     space   space  space.
  PERFORM f_fieldcat USING  'EMP_SUBTXT'        'EmpSubgroup'       11 space space  space     space   space space.
  PERFORM f_fieldcat USING  'WERKS'             'Pers Area'         12 'X'   'X'    'PA0001'  'WERKS' space  space.
  PERFORM f_fieldcat USING  'PERSA_TXT'          'Pers Area Text'    13 space space  space     space   space space.
  PERFORM f_fieldcat USING  'BTRTL'             'Pers Subarea'      14 'X'   'X'    'PA0001'  'BTRTL' space  space.
  PERFORM f_fieldcat USING  'BTRTL_DES'         'Pers Subarea Text' 15 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PLANS'             'Position'          16 'X'   'X'    'T528B'  'PLANS' space  space.
  PERFORM f_fieldcat USING  'POS_NAME'          'Position desc'     17 space space  space     space   space space.
  PERFORM f_fieldcat USING  'MASSG'             'Hiring Reason'     18 'X'   'X'    'T530T'    'MASSG' space  space.
  PERFORM f_fieldcat USING  'MASSG_TXT'         'HR Des'            19 space space  space     space   space space.
  PERFORM f_fieldcat USING  'DOB'               'DOB'               20 'X'   space  space     space   space  space.
  PERFORM f_fieldcat USING  'JOINING_DATE'      'Join Date'         21 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'E_MAIL'            'Email'             22 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'MOBILE_NO'         'Mobile no'         23 'X'   space  space     space   space  space.
  PERFORM f_fieldcat USING  'BLOOD_GRP'         'Bl_Grp'            24 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'FAMST'             'MaritalStatus'     25 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'FAMDT'             'Marital Date'      26 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'NATIO'             'Nationality'       27 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'AADHAR_NO'         'Aadhar No'         28 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PANCARD'           'Pancard No '       29 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PASSPORT'          'Passport No '      30 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'EXPID'             'ID expiry date'    31 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS'      'Addrtyp'           32 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS1'     'Addr1'             33 'X'   space  'ZHR_HIRING_TAB' 'PERM_ADDRESS1'   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS2'     'Addr2'             34 'X'   space  'ZHR_HIRING_TAB' 'PERM_ADDRESS2'   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS3'     'Addr3'             35 'X'   space  'ZHR_HIRING_TAB' 'PERM_ADDRESS3'   space space.
  PERFORM f_fieldcat USING  'PERM_CITY'         'City'              36 'X'   space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_STATE'        'Region'            37 'X'   space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_COUNTRY'      'Country'           38 'X'   space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_POSTAL_CODE'  'Postal Code'       39 'X'   space  space     space   space space.
  PERFORM f_fieldcat USING  'P_PHONE1'          'Mobile No'         40 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'P_PHONE2'          'Mobile No'         41 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS'      'Addrtyp'           42 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'COSTCENTER'        'Costcenter'        43 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS1'     'Addr1'             43 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS2'     'Addr2'             44 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS3'     'Addr3'             45 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_CITY'         'City'              46 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_STATE'        'Region'            47 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_COUNTRY'      'Country'           48 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_POSTAL_CODE'  'Postal Code'       49 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE1'         'Mobile No'         50 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE2'         'Mobile No'         51 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'CREATED_BY'        'Created by'        52 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'CREATED_ON'        'Created On'        53 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'APPROVED_BY'       'Approved by'       54 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'APPROVED_ON'       'Approved on'       55 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'BANKN'             'Account No'        56 'X'   space  'PA0009'  'BANKN'    space     space.
  PERFORM f_fieldcat USING  'BANKL'             'Ifsc Code'         57 'X'   space  'PA0009'  'BANKL'  space     space.
  PERFORM f_fieldcat USING  'MSG'               'Message'           58 space space  space     space space     space.


  CALL SCREEN 100.


ENDFORM.

*************Approve ALV Screen***********************
FORM approve_alv.

  REFRESH gt_fcat.
  .

  PERFORM f_fieldcat USING  'SEL'               'Select'             1 'X' space  space     space   space 'X'.
  PERFORM f_fieldcat USING  'TEMP_EMPNO'        'Candidate No'       2 space space  space     space   space space.
  PERFORM f_fieldcat USING  'TEMP_EMPID'        'Candidate ID'       3 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS'            'Status'             4 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS_DES'        'Status Des'         5 space space  'GT_DISPLAY1'     'STATUS_DES'   space space.
  PERFORM f_fieldcat USING  'NAME'              'EmpName'            6 space space  space     space   space space .
  PERFORM f_fieldcat USING  'PERNR'             'Personnel No'       7 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERSG'             'EmpGroup'           8 space space  space     space   space space.
  PERFORM f_fieldcat USING  'EMP_GRPTXT'        'EmpGroup'           9 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERSK'             'EmpSubgroup'       10 space   'X'    'PA0001'  'PERSK' space  space.
  PERFORM f_fieldcat USING  'EMP_SUBTXT'        'EmpSubgroup'       11 space space  space     space   space space.
  PERFORM f_fieldcat USING  'WERKS'             'Pers Area'         12 space   'X'    'PA0001'  'WERKS' space  space.
  PERFORM f_fieldcat USING  'PERSA_TXT'         'Pers Area Text'    13 space space  space     space   space space.
  PERFORM f_fieldcat USING  'BTRTL'             'Pers Subarea'      14 space   'X'    'PA0001'  'BTRTL' space  space.
  PERFORM f_fieldcat USING  'BTRTL_DES'         'Pers Subarea Text' 15 space space  space     space   space space.
  PERFORM f_fieldcat USING  'ORGUNITID'         'Org Id'            16 space space  space     space   space space.
  PERFORM f_fieldcat USING  'ORGUNITNAME'       'Org Unit Name'     17 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PLANS'             'Position'          18 space   'X'    'PA0001'   'PLANS' space  space.
  PERFORM f_fieldcat USING  'POS_NAME'          'Position desc'     19 space space  space     space   space space.
  PERFORM f_fieldcat USING  'MASSG'             'Hiring Reason'     20 space   'X'    'T530'    'MASSG' space  space.
  PERFORM f_fieldcat USING  'MASSG_TXT'         'HR Des'            21 space space  space     space   space space.
  PERFORM f_fieldcat USING  'JOINING_DATE'      'Join Date'         22 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'E_MAIL'            'Email'             23 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'MOBILE_NO'         'Mobile no'         24 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'DOB'               'DOB'               25 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'BLOOD_GRP'         'Bl_Grp'            26 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'FAMST'             'MaritalStatus'     27 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'FAMDT'             'Marital Date'      28 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'NATIO'             'Nationality'       29 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'AADHAR_NO'         'Aadhar No'         30 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PANCARD'           'Pancard No '       31 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PASSPORT'          'Passport No '      32 space space  space     space   space space.
  PERFORM f_fieldcat USING  'EXPID'             'ID expiry date'    33 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS'      'Addrtyp'           34 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS1'     'Addr1'             35 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS2'     'Addr2'             36 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS3'     'Addr3'             37 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_CITY'         'City'              38 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_STATE'        'Region'            39 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_COUNTRY'      'Country'           40 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_POSTAL_CODE'  'Postal Code'       41 space space  space     space   space space.
  PERFORM f_fieldcat USING  'P_PHONE1'          'Mobile No'         42 space space  space     space   space space.
  PERFORM f_fieldcat USING  'P_PHONE2'          'Mobile No'         43 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS'      'Addrtyp'           44 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS1'     'Addr1'             45 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS2'     'Addr2'             46 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS3'     'Addr3'             47 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_CITY'         'City'              48 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_STATE'        'Region'            49 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_COUNTRY'      'Country'           50 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_POSTAL_CODE'  'Postal Code'       51 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE1'         'Mobile No'         52 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE2'         'Mobile No'         53 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'MSG'               'Message'           54 space space  space     space abap_false space.
*  PERFORM f_fieldcat USING  'CREATED_BY'        'Created by'        52 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'CREATED_ON'        'Created On'        53 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'APPROVED_BY'       'Approved by'       54 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'APPROVED_ON'       'Approved on'       55 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'IT0000'            'IT0000'            56 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0001'            'IT0001'            57 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0002'            'IT0002'            58 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB3'       'IT0105_S3'         59 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB1'       'IT0105_S1'         60 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB2'       'IT0105_S2'         61 space space  space space  abap_true  space.
*  PERFORM f_fieldcat USING  'IT0009'            'IT0009'            62 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB1'       'IT0185_S1'         63 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB2'       'IT0185_S2'         64 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB3'       'IT0185_S3'         65 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0041'            'IT0041'            66 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0006_SUB1'       'IT0006_S1'         67 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0006_SUB2'       'IT0006_S2'         68 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'HRP1001'           'HRP1001'           69 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'PERNR_VENDOR'      'PERNR_VENDOR'      70 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0009'            'IT0009'            71 space space  space space  abap_true space.


  CALL SCREEN 300.

ENDFORM.

**********Display Table Contents in ALV***************
FORM display_alv.

  REFRESH gt_fcat.

  PERFORM f_fieldcat USING  'SEL'               'Select'             1 'X' space  space     space   abap_true 'X'.
  PERFORM f_fieldcat USING  'TEMP_EMPNO'        'Candidate No'       2 space space  space     space   space space .
  PERFORM f_fieldcat USING  'TEMP_EMPID'        'Candidate ID'       3 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS'            'Status'             4 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'STATUS_DES'        'Status Des'         5 space space  'GT_DISPLAY1'     'STATUS_DES'   space space.
  PERFORM f_fieldcat USING  'NAME'              'EmpName'            6 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERNR'             'Personnel No'       7 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERSG'             'EmpGroup'           8 space space  space     space   space space.
  PERFORM f_fieldcat USING  'EMP_GRPTXT'        'EmpGroup'           9 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERSK'             'EmpSubgroup'       10 space   'X'    'PA0001'  'PERSK' space  space.
  PERFORM f_fieldcat USING  'EMP_SUBTXT'        'EmpSubgroup'       11 space space  space     space   space space.
  PERFORM f_fieldcat USING  'WERKS'             'Pers Area'         12 space   'X'    'PA0001'  'WERKS' space  space.
  PERFORM f_fieldcat USING  'PERSA_TXT'         'Pers Area Text'    13 space space  space     space   space space.
  PERFORM f_fieldcat USING  'BTRTL'             'Pers Subarea'      14 space   'X'    'PA0001'  'BTRTL' space  space.
  PERFORM f_fieldcat USING  'BTRTL_DES'         'Pers Subarea Text' 15 space space  space     space   space space.
  PERFORM f_fieldcat USING  'ORGUNITID'         'Org Id'            16 space space  space     space   space space.
  PERFORM f_fieldcat USING  'ORGUNITNAME'       'Org Unit Name'     17 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PLANS'             'Position'          18 space   'X'    'PA0001'   'PLANS' space  space.
  PERFORM f_fieldcat USING  'POS_NAME'          'Position desc'     19 space space  space     space   space space.
  PERFORM f_fieldcat USING  'MASSG'             'Hiring Reason'     20 space   'X'    'T530'    'MASSG' space  space.
  PERFORM f_fieldcat USING  'MASSG_TXT'         'HR Des'            21 space space  space     space   space space.
  PERFORM f_fieldcat USING  'JOINING_DATE'      'Join Date'         22 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'E_MAIL'            'Email'             23 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'MOBILE_NO'         'Mobile no'         24 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'DOB'               'DOB'               25 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'BLOOD_GRP'         'Bl_Grp'            26 space space  space     space   space  space.
  PERFORM f_fieldcat USING  'FAMST'             'MaritalStatus'     27 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'FAMDT'             'Marital Date'      28 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'NATIO'             'Nationality'       29 space space  space     space   abap_true space.
  PERFORM f_fieldcat USING  'AADHAR_NO'         'Aadhar No'         30 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PANCARD'           'Pancard No '       31 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PASSPORT'          'Passport No '      32 space space  space     space   space space.
  PERFORM f_fieldcat USING  'EXPID'             'ID expiry date'    33 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS'      'Addrtyp'           34 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS1'     'Addr1'             35 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS2'     'Addr2'             36 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_ADDRESS3'     'Addr3'             37 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_CITY'         'City'              38 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_STATE'        'Region'            39 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_COUNTRY'      'Country'           40 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PERM_POSTAL_CODE'  'Postal Code'       41 space space  space     space   space space.
  PERFORM f_fieldcat USING  'P_PHONE1'          'Mobile No'         42 space space  space     space   space space.
  PERFORM f_fieldcat USING  'P_PHONE2'          'Mobile No'         43 space space  space     space   space space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS'      'Addrtyp'           44 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS1'     'Addr1'             45 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS2'     'Addr2'             46 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_ADDRESS3'     'Addr3'             47 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_CITY'         'City'              48 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_STATE'        'Region'            49 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_COUNTRY'      'Country'           50 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PRES_POSTAL_CODE'  'Postal Code'       51 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE1'         'Mobile No'         52 space space  space     space abap_true space.
  PERFORM f_fieldcat USING  'PR_PHONE2'         'Mobile No'         53 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'CREATED_BY'        'Created by'        52 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'CREATED_ON'        'Created On'        53 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'APPROVED_BY'       'Approved by'       54 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'APPROVED_ON'       'Approved on'       55 space space  space     space abap_true space.
*  PERFORM f_fieldcat USING  'IT0000'            'IT0000'            56 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0001'            'IT0001'            57 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0002'            'IT0002'            58 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB3'       'IT0105_S3'         59 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB1'       'IT0105_S1'         60 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0105_SUB2'       'IT0105_S2'         61 space space  space space  abap_true  space.
*  PERFORM f_fieldcat USING  'IT0009'            'IT0009'            62 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB1'       'IT0185_S1'         63 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB2'       'IT0185_S2'         64 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0185_SUB3'       'IT0185_S3'         65 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0041'            'IT0041'            66 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0006_SUB1'       'IT0006_S1'         67 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0006_SUB2'       'IT0006_S2'         68 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'HRP1001'           'HRP1001'           69 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'PERNR_VENDOR'      'PERNR_VENDOR'      70 space space  space space  abap_true space.
*  PERFORM f_fieldcat USING  'IT0009'            'IT0009'            71 space space  space space  abap_true space.
  CALL SCREEN 400.
ENDFORM.

FORM alv_error_display.

  REFRESH gt_fcat.
  PERFORM f_fieldcat USING  'PERNR'        'Personnel No' 1 space space  space     space   space space.
  PERFORM f_fieldcat USING  'INFTY'        'Infotype'     2 space space  space     space   space space.
  PERFORM f_fieldcat USING  'SUBTY'        'Subtype'      3 space space  space     space   space space.
  PERFORM f_fieldcat USING  'STATUS'       'Status'       4 space space  space     space   space space.
  PERFORM f_fieldcat USING  'MESSAGE'      'Message'      5 space space  space     space   space space.

*********Error Display*****************
  CALL SCREEN 500.
ENDFORM.

**********ALV Display For Propose ****************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZHR_PROP'.
  SET TITLEBAR 'ZTITLE'.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO lt_exclude.

  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.
    SET HANDLER: lo_events->handle_on_f4 FOR lo_grid.

*    PERFORM register_f4_fields.

    gs_layout-cwidth_opt = 'X'.
    gs_layout-info_fname = 'LCOLOR'.
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_display2
        it_fieldcatalog      = gt_fcat.

** Register for EDIT Fields and Events
    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.

    REFRESH lt_f4.
    CLEAR lt_f4.
    lt_f4-fieldname = 'PLANS'.
    lt_f4-register  = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'WERKS'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'BTRTL'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'PERSK'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'MASSG'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CALL METHOD lo_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.

MODULE user_command_0100 INPUT.

  DATA: ls_msg(150) TYPE c.
  DATA: ls_color               TYPE lvc_s_scol.
*********For Propose ALV Screen*********************************
  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

      "Changed by: Samsudeen M  "changed on 17.12.2022
      "new button sel all desel all ***
**** Select all and deselect all button in alv ****
    WHEN 'SEL_ALL'.
      LOOP AT gt_display2 ASSIGNING FIELD-SYMBOL(<fs_display>).
        <fs_display>-sel = 'X'.
        MODIFY gt_display2 FROM <fs_display>.
      ENDLOOP.
*** Deselect Button in alv display ***
    WHEN 'DESEL_ALL'.
      LOOP AT gt_display2 ASSIGNING FIELD-SYMBOL(<fs_display1>).
        <fs_display1>-sel = ''.
        MODIFY gt_display2 FROM <fs_display1>.
      ENDLOOP.
***** End of changes on 17.12.2022 *******************
*********SAVE button in ALV screen**************
    WHEN 'SAVE'.
      LOOP AT gt_display2 INTO gs_display2. " WHERE sel = 'X'.
        " Lock Table for update
        CALL FUNCTION 'ENQUEUE_EZZHRHIRING'
          EXPORTING
            mode_zhr_hiring_tab = 'E'
            mandt               = sy-mandt
            temp_empno          = gs_display2-temp_empno
            temp_empid          = gs_display2-temp_empid
          EXCEPTIONS
            foreign_lock        = 1
            system_failure      = 2
            OTHERS              = 3.
        IF sy-subrc <> 0.
          MESSAGE 'Object is locked by other user' TYPE 'S'.
        ELSE.
          UPDATE zhr_hiring_tab SET persk = gs_display2-persk
                                  werks = gs_display2-werks
                                  btrtl = gs_display2-btrtl
                                  plans = gs_display2-plans
                                  massg = gs_display2-massg
                                  dob   = gs_display2-dob
                                  perm_address1 = gs_display2-perm_address1
                                  perm_address2 = gs_display2-perm_address2
                                  perm_address3 = gs_display2-perm_address3
                                  perm_city = gs_display2-perm_city
                                  perm_state = gs_display2-perm_state
                                  perm_country = gs_display2-perm_country
                                  perm_postal_code = gs_display2-perm_postal_code
                                  WHERE temp_empno = gs_display2-temp_empno
                                  AND temp_empid = gs_display2-temp_empid.
          IF sy-subrc EQ 0.
            CLEAR ls_msg.
            CONCATENATE 'Candidate' gs_display2-temp_empno 'is Updated in table' INTO ls_msg.
            MESSAGE ls_msg TYPE 'S'.
          ENDIF.
          " Release Lock for  Table
          CALL FUNCTION 'DEQUEUE_EZZHRHIRING'
            EXPORTING
              mode_zhr_hiring_tab = 'E'
              mandt               = sy-mandt
              temp_empno          = gs_display2-temp_empno
              temp_empid          = gs_display2-temp_empid.
        ENDIF.
        CLEAR gs_display2.
      ENDLOOP.


******** PROPOSE Button Trigger in ALV display **************
    WHEN 'PROP'.
      gt_display3[] = gt_display2.
      DELETE gt_display3 WHERE sel NE 'X'.
      CLEAR gs_display2.
      LOOP AT gt_display2 INTO gs_display2 WHERE sel = 'X'.
        CLEAR: gs_position1,gs_display2-type,gs_display2-msg.
        SELECT SINGLE * FROM hrp1001
         INTO  gs_position1
         WHERE otype EQ 'S' AND
         objid EQ gs_display2-plans
         AND plvar EQ '01' AND
          rsign EQ 'A' AND relat EQ '008'
          AND istat EQ '1' AND
         sclas EQ 'P' AND
         begda <= '99991231' AND endda GE gs_display2-joining_date .
        IF sy-subrc EQ 0.
          gs_display2-type = 'E'.
          gs_display2-msg = |Position Already Assigned to Employee { gs_position1-sobid }|.
        ENDIF.
*----- Costcenter checks -----*
*-- Added by Samsudeen on 02.03.2023 -------*
        SELECT SINGLE sobid FROM hrp1001
         INTO  gs_display2-costcenter
         WHERE otype EQ 'S' AND
         objid EQ gs_display2-plans
         AND plvar EQ '01' AND
          rsign EQ 'A' AND relat EQ '011'
          AND istat EQ '1' AND
         sclas EQ 'K' AND
         begda LE sy-datum AND endda GE sy-datum.
        IF sy-subrc NE 0.
          gs_display2-type = 'E'.
          gs_display2-msg = |Costcenter is Missing for Position { gs_display2-plans }|.
        ELSE.
          "Added by Samsudeen M on 05.05.2023
          SELECT SINGLE costcenter FROM zhr_greyhr_cstcr INTO @DATA(l_cstcr) WHERE costcenter = @gs_display2-costcenter.
          IF sy-subrc NE 0.
            gs_display2-type = 'E'.
            gs_display2-msg = |Employee Costcenter Not Available in Greythr|.
          ENDIF.
        ENDIF.
        IF gs_display2-msg IS INITIAL.
******************Check For Employee Details Message While PROPOSE Button  ***************
          IF gs_display2-name IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Employee Name is Missing|.

          ELSEIF gs_display2-dob IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |DOB is Missing|.

          ELSEIF gs_display2-joining_date IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Joining date is Missing|.

          ELSEIF gs_display2-persk IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Emp Group is Missing|.

          ELSEIF gs_display2-persg IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Emp Sub Grp is Missing|.

          ELSEIF gs_display2-werks IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Pers. Area is Missing|.

          ELSEIF gs_display2-btrtl IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Pers. Sub Area is Missing|.

          ELSEIF gs_display2-plans IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Position is Missing|.

          ELSEIF gs_display2-massg IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Hiring reason is Missing|.

          ELSEIF gs_display2-e_mail IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Email is Missing|.

          ELSEIF gs_display2-mobile_no IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Mobile No. is Missing|.

          ELSEIF ( gs_display2-perm_address1 IS INITIAL  AND gs_display2-perm_country IS INITIAL
                   AND gs_display2-perm_address2 IS INITIAL  AND gs_display2-perm_postal_code IS INITIAL ).
            gs_display2-type = 'E'.
            gs_display2-msg = |'Address is Missing|.

          ELSEIF gs_display2-perm_state IS INITIAL .
            gs_display2-type = 'E'.
            gs_display2-msg = |State is Missing|.

          ELSEIF gs_display2-costcenter IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |CostCenter is Missing for Position|.
*Added by Samsudeen M 0n 05.05.2023
          ELSEIF gs_display2-bankn IS INITIAL. "Bank Account Check
            gs_display2-type = 'E'.
            gs_display2-msg = |Bank Account Number Missing|.
*Bank key Check
          ELSEIF gs_display2-bankl IS INITIAL.
            gs_display2-type = 'E'.
            gs_display2-msg = |Bank Ifsc Code Missing|.
          ENDIF.
        ENDIF.
        "Error Means Updating the table and stop the Process
        IF gs_display2-type = 'E'.
          gs_display2-lcolor = 'C610'.
          MODIFY gt_display2 FROM gs_display2.
          CONTINUE.
        ELSE.
          CLEAR gs_display2-lcolor.
        ENDIF.

        MODIFY gt_display2 FROM gs_display2.
**********Final Proposal to employeee ******************************
        IF  gs_display2-status EQ '01'.
          " Lock Table for update
          CALL FUNCTION 'ENQUEUE_EZZHRHIRING'
            EXPORTING
              mode_zhr_hiring_tab = 'E'
              mandt               = sy-mandt
              temp_empno          = gs_display2-temp_empno
              temp_empid          = gs_display2-temp_empid
            EXCEPTIONS
              foreign_lock        = 1
              system_failure      = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
            MESSAGE 'Object is locked by other user' TYPE 'S'.
          ELSE.
            UPDATE zhr_hiring_tab SET persk = gs_display2-persk
                                      werks = gs_display2-werks
                                      btrtl = gs_display2-btrtl
                                      plans = gs_display2-plans
                                      massg = gs_display2-massg
                                      dob   = gs_display2-dob
                                      status = '02'
                                      bankn = gs_display2-bankn
                                      bankl = gs_display2-bankl
                                      created_by = sy-uname
                                      created_on = sy-datum
                                      costcenter = gs_display2-costcenter
                                      WHERE temp_empno = gs_display2-temp_empno
                                      AND temp_empid = gs_display2-temp_empid.
            gs_display2-status_des = |Proposed|.
            gs_display2-msg = |Proposed|.
            MODIFY gt_display2 FROM gs_display2.
          ENDIF.
          " Release Lock for  Table
          CALL FUNCTION 'DEQUEUE_EZZHRHIRING'
            EXPORTING
              mode_zhr_hiring_tab = 'E'
              mandt               = sy-mandt
              temp_empno          = gs_display2-temp_empno
              temp_empid          = gs_display2-temp_empid.
        ENDIF.
        CLEAR gs_display2.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).
      "If Atleast one Employee Proposal Happen then only Mail triggers
      DATA(lv_proposed) = VALUE #( gt_display2[ msg = |Proposed| ] OPTIONAL ).
      IF lv_proposed IS NOT INITIAL.
        PERFORM propose_email.   "Propose EMAIL
      ENDIF.

*&------RE-EXTRACTION Button In ALV Display-------&
    WHEN 'REEX'.
      DATA popup_return TYPE c.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation'
          text_question         = 'Selected entries will be deleted from table kindly re-extract candidate details again using get candidate details'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF popup_return EQ '1'.
        LOOP AT gt_display2 ASSIGNING FIELD-SYMBOL(<fs_display2>) WHERE sel = 'X'.
          DELETE FROM zhr_hiring_tab WHERE temp_empid = <fs_display2>-temp_empid
                                     AND temp_empno = <fs_display2>-temp_empno.
          DELETE gt_display2 WHERE temp_empid = <fs_display2>-temp_empid
                             AND temp_empno = <fs_display2>-temp_empno.
        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDMODULE.
*----------Handling F4 Help for field ********************************************
FORM handle_on_f4
  USING e_fieldname   TYPE lvc_fname
        es_row_no     TYPE lvc_s_roid
        er_event_data TYPE REF TO cl_alv_event_data.

  DATA: lt_map    TYPE TABLE OF dselc,
        ls_map    TYPE dselc,
        lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        ls_stable TYPE lvc_s_stbl.

  DATA: ls_selec TYPE objec,
        lt_selec TYPE TABLE OF objec,
        ls_plvar TYPE plogi-plvar,
        ls_otype TYPE plogi-otype.
  FIELD-SYMBOLS: <l_out> TYPE ty_alv. " alv table line

  REFRESH : lt_return.
  CASE e_fieldname.

    WHEN 'PLANS'.   " Search help for Position
      READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar           = '01'
            otype           = 'S'
            dynpro_repid    = sy-repid
            dynpro_dynnr    = sy-dynnr
          IMPORTING
            sel_plvar       = ls_plvar
            sel_otype       = ls_otype
            sel_object      = ls_selec
          TABLES
            sel_objects     = lt_selec
          EXCEPTIONS
            cancelled       = 1
            wrong_condition = 2
            nothing_found   = 3
            internal_error  = 4
            illegal_mode    = 5
            OTHERS          = 6.
        <l_out>-plans = ls_selec-objid.
        <l_out>-pos_name = ls_selec-stext.

        PERFORM f_get_orgdetail USING <l_out>-plans
                                CHANGING <l_out>-orgunitid
                                         <l_out>-orgunitname.

      ENDIF.


    WHEN 'WERKS'.  " Search help for personal Area
      REFRESH : lt_return.
      CLEAR: ls_map,ls_return.
      ls_map-dyfldname = 'WERKS'.
      ls_map-fldname = 'F0002'.
      APPEND ls_map TO lt_map.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'PERSA'
          value_org       = 'S'
        TABLES
          value_tab       = gt_persa
          dynpfld_mapping = lt_map
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      CLEAR gs_display2.
      READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        CLEAR ls_return.
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF sy-subrc EQ 0.
          <l_out>-werks = ls_return-fieldval.
        ENDIF.
        IF <l_out>-werks IS NOT INITIAL.
          CLEAR gs_persa.
          READ TABLE gt_persa  INTO gs_persa WITH KEY persa = <l_out>-werks.
          IF sy-subrc EQ 0.
            <l_out>-persa_txt = gs_persa-name1.
          ENDIF.
        ELSE.
          CLEAR gs_display2-persa_txt.
          CLEAR <l_out>-persa_txt.
        ENDIF.
      ENDIF.


    WHEN 'PERSK'.  "Search help for Employee Subgrp
      REFRESH : lt_return.
      CLEAR ls_map.
      ls_map-dyfldname = 'PERSK'.
      ls_map-fldname = 'F0003'.
      APPEND ls_map TO lt_map.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'PERSK'
          value_org       = 'S'
        TABLES
          value_tab       = gt_persk
          dynpfld_mapping = lt_map
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0003'.
        IF sy-subrc EQ 0.
          <l_out>-persk = ls_return-fieldval.
        ENDIF.
        READ TABLE gt_persk INTO gs_persk WITH KEY persk = <l_out>-persk.
        IF sy-subrc EQ 0.
          <l_out>-emp_subtxt = gs_persk-ptext.
        ENDIF.
      ENDIF.

    WHEN 'BTRTL'.  "Search help for Employee personal Sub Area

      READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc = 0.
        IF <l_out>-werks IS NOT INITIAL.
          REFRESH gt_btrtl_hlp.
          gt_btrtl_hlp[] = gt_btrtl[].

          DELETE gt_btrtl_hlp WHERE werks NE <l_out>-werks.

          REFRESH : lt_return.
          CLEAR ls_map.
          ls_map-dyfldname = 'BTRTL'.
          ls_map-fldname = 'F0001'.
          APPEND ls_map TO lt_map.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'BTRTL'
              value_org       = 'S'
            TABLES
              value_tab       = gt_btrtl_hlp
              dynpfld_mapping = lt_map
              return_tab      = lt_return
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.

          READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
          IF sy-subrc EQ 0.
            READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0002'.
            IF sy-subrc EQ 0.
              <l_out>-btrtl = ls_return-fieldval.
            ELSE.
              CLEAR gs_display2-btrtl_des.
            ENDIF.
            READ TABLE gt_btrtl INTO gs_btrtl WITH KEY werks = <l_out>-werks btrtl = <l_out>-btrtl.
            IF sy-subrc EQ 0.
              <l_out>-btrtl_des = gs_btrtl-btext.
            ENDIF.
          ENDIF.

        ELSE.
*          MESSAGE 'Select the Personal Area First' TYPE 'E' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.



    WHEN 'MASSG'.   " Search help for Reason for Hiring
      REFRESH : lt_return.
      REFRESH lt_map.
      CLEAR: ls_map,ls_return.
      ls_map-dyfldname = 'MASSG'.
      ls_map-fldname = 'F0005'.
      APPEND ls_map TO lt_map.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'MASSG'
          value_org       = 'S'
        TABLES
          value_tab       = gt_reason
          return_tab      = lt_return
          dynpfld_mapping = lt_map
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      READ TABLE gt_display2 ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0004'.
        IF sy-subrc EQ 0.
          <l_out>-massg = ls_return-fieldval.
        ENDIF.
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0005'.
        IF sy-subrc EQ 0.
          <l_out>-massg_txt = ls_return-fieldval.
        ENDIF.
      ENDIF.
  ENDCASE.
  ls_stable = 'XX'. " set stable refresh for row and column

  " alv refresh
  CALL METHOD lo_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stable
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.
  er_event_data->m_event_handled = 'X'.



ENDFORM.

**********ALV for API Display*********
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ZSTANDARD'.
  SET TITLEBAR 'ZTITLE'.

  IF lo_grid IS INITIAL .

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    gs_layout-cwidth_opt = 'X'.

    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab            = gt_alv1
        it_fieldcatalog      = gt_fcat.

  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

**********ALV for Approve Display*********
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'ZAPPROVE'.
  SET TITLEBAR 'ZTITLE'.

**  DATA ls_exclude TYPE ui_func.
*  DATA: lt_exclude TYPE ui_functions.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO lt_exclude.


  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    gs_layout-cwidth_opt = 'X'.
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_display1
        it_fieldcatalog      = gt_fcat.

** Register for EDIT Fields and Events
    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.

DATA:lv_errflag TYPE flag.

MODULE user_command_0300 INPUT.

  TYPES: BEGIN OF ty_pernr,
           pernr TYPE pernr_d,
         END OF ty_pernr.
  DATA: lt_pernr TYPE TABLE OF ty_pernr.
  DATA: lv_flag TYPE flag.
  DATA: l_user TYPE sy-uname.

  DATA:  ls_stable TYPE lvc_s_stbl.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

*** Changes on 17.12.2022  "Changes by: Samsudeen M   ****
    WHEN 'SEL_ALL'.
      LOOP AT gt_display1 ASSIGNING FIELD-SYMBOL(<fs_disp>).
        <fs_disp>-sel = 'X'.
        MODIFY gt_display1 FROM <fs_disp>.
      ENDLOOP.
    WHEN 'DESEL_ALL'.
      LOOP AT gt_display1 ASSIGNING FIELD-SYMBOL(<fs_disp1>).
        <fs_disp1>-sel = ''.
        MODIFY gt_display1 FROM <fs_disp1>.
      ENDLOOP.
**** End of changes 17.12.2022 ********************

**** Approve Buttonin Approve ALV screen*************
    WHEN 'APPR'.
      REFRESH:gt_display3.
      gt_display3[] = gt_display1[].
      DELETE gt_display3[] WHERE sel NE 'X'.
      LOOP AT gt_display1 INTO gs_display1 WHERE sel = 'X'.
        REFRESH gt_output.
        CLEAR l_user.
        CALL FUNCTION 'HR_ENQUEUE_OBJECT'
          EXPORTING
            plvar            = '01'
            otype            = 'S'
            objid            = gs_display1-plans
          IMPORTING
            lock_user        = l_user
          EXCEPTIONS
            enqueue_failed   = 1
            objid_is_initial = 2
            illegal_otype    = 3
            internal_error   = 4
            OTHERS           = 5.
        IF sy-subrc = 0.
        ENDIF.
        IF l_user IS INITIAL.
****** Employee Infotype 0000 Creation************
          IF gs_display1-pernr IS INITIAL.

************Infotype 0000 Creation************
            PERFORM f_hire_employee CHANGING lv_errflag gs_display1-pernr.
            IF gv_pernr IS NOT INITIAL.
              UPDATE zhr_hiring_tab SET pernr = gv_pernr
                                 it0000 = 'X'
                                 WHERE temp_empno = gs_display1-temp_empno
                                 AND temp_empid = gs_display1-temp_empid.
              APPEND VALUE #( pernr = gv_pernr ) TO lt_pernr.
************Infotype 0001 Creation************
              CLEAR lv_flag.
              PERFORM f_create_it0001 CHANGING lv_flag.
              IF lv_flag = abap_true.
                UPDATE zhr_hiring_tab SET it0001 = 'X'
                                      WHERE temp_empno = gs_display1-temp_empno
                                      AND temp_empid = gs_display1-temp_empid.
              ELSE.
                CONTINUE.
              ENDIF.

*********Position Assignment in HRP1001**********
              PERFORM f_position_assignment .

************Infotype 0002 Creation************
              PERFORM f_create_it0002.

********** Update Employee Costcenter **********
              IF sy-sysid EQ 'PRD'.
                "Added on Samsudeen M 0n 02.03.2023
                PERFORM update_costcentr USING gs_display1-temp_empid gs_display1-costcenter gs_display1-joining_date.
              ENDIF.
************Infotype 0041 Creation************
              PERFORM f_create_it0041.
************Update Pernr into a ztable to perform other infotype operation*************************** begin  added by zakir
       data(ls_emp_upd) = VALUE ZEMPOTH_UPDATE(  pernr = gv_pernr  ).
       MODIFY ZEMPOTH_UPDATE from ls_emp_upd.
"End
************Infotype 0105 Creation************
              PERFORM f_create_it0105 USING '0002'.

              PERFORM f_create_it0105 USING '0030'.

              PERFORM f_create_it0105 USING 'CELL'.

************ infotype 0185 creation ***************
              PERFORM f_create_it0185 USING '02'. "Pan

*        ELSEIF ( gs_display1-aadhar_no IS NOT INITIAL AND gs_display1-it0185_sub2 NE 'X' ).
              PERFORM f_create_it0185 USING '06'.  "AAdhar

              PERFORM f_create_it0185 USING '09'. "Passport

***********Infotype 0006 Creation****************
              PERFORM f_create_it0006 USING '1'.
              PERFORM f_create_it0006 USING '3'.
              PERFORM f_create_it0006 USING '4'.

************Infotype 0009 Creation************
              PERFORM f_create_it0009 USING '0'.

*************Update Greythr From SAP************
              IF sy-sysid EQ 'PRD'.
                PERFORM update_greythr.
              ENDIF.
*************PERNR_VENDOR BP LINKING **************
*          PERFORM pernr_vendor.

              " Lock Table for update
              CALL FUNCTION 'ENQUEUE_EZZHRHIRING'
                EXPORTING
                  mode_zhr_hiring_tab = 'E'
                  mandt               = sy-mandt
                  temp_empno          = gs_display1-temp_empno
                  temp_empid          = gs_display1-temp_empid
                EXCEPTIONS
                  foreign_lock        = 1
                  system_failure      = 2
                  OTHERS              = 3.
              IF sy-subrc EQ 0.
                gs_display1-status_des = 'Created'.
                UPDATE zhr_hiring_tab SET status = '04'
                                          approved_by = sy-uname
                                          approved_on = sy-datum
                                          WHERE temp_empno = gs_display1-temp_empno
                                          AND temp_empid = gs_display1-temp_empid.
*            MESSAGE 'Pernr created successfully' TYPE 'S'.
              ELSE.
                MESSAGE 'Table is locked already' TYPE 'S'.
              ENDIF.
              " Release Lock for  Table
              CALL FUNCTION 'DEQUEUE_EZZHRHIRING'
                EXPORTING
                  mode_zhr_hiring_tab = 'E'
                  mandt               = sy-mandt
                  temp_empno          = gs_display2-temp_empno
                  temp_empid          = gs_display2-temp_empid.
            ENDIF.
          ELSE.
            gs_display1-msg = |Pernr already assigned to this candidate_no { gs_display1-temp_empno }|.
          ENDIF.
        ELSE.
          gs_display1-msg = |Position ID { gs_display1-plans } is locked by { l_user }|.
        ENDIF.
        MODIFY gt_display1 FROM gs_display1.
        CLEAR gs_display1.
        CALL METHOD lo_grid->refresh_table_display( ).
      ENDLOOP.

      IF lt_pernr[] IS NOT INITIAL.
        PERFORM approve_email.   " EMAIL for Approval
      ENDIF.

***********Rejecting Employee Creation *********************
    WHEN 'REJE'.
      LOOP AT gt_display1 INTO gs_display1 WHERE sel = 'X'.
        " Lock Table for update
        CALL FUNCTION 'ENQUEUE_EZZHRHIRING'
          EXPORTING
            mode_zhr_hiring_tab = 'E'
            mandt               = sy-mandt
            temp_empno          = gs_display1-temp_empno
            temp_empid          = gs_display1-temp_empid
          EXCEPTIONS
            foreign_lock        = 1
            system_failure      = 2
            OTHERS              = 3.
        IF sy-subrc EQ 0.
          UPDATE zhr_hiring_tab SET status = '01'
                                   WHERE temp_empno = gs_display1-temp_empno
                                   AND temp_empid = gs_display1-temp_empid.
          MESSAGE 'Candidate ID Rejected from creation' TYPE 'S'.
        ENDIF.
        " Release Lock for  Table
        CALL FUNCTION 'DEQUEUE_EZZHRHIRING'
          EXPORTING
            mode_zhr_hiring_tab = 'E'
            mandt               = sy-mandt
            temp_empno          = gs_display2-temp_empno
            temp_empid          = gs_display2-temp_empid.

      ENDLOOP.
      DELETE gt_display1 WHERE sel = 'X'.
  ENDCASE.

ENDMODULE.

**********Final Display Radiobutton ALV Display*************
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'ZSTANDARD'.
  SET TITLEBAR 'ZTITLE'.

  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    gs_layout-cwidth_opt = 'X'.
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
*       it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab       = lt_display
        it_fieldcatalog = gt_fcat.
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.

*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'ZALV_ERROR'.
  SET TITLEBAR 'ZERROR'.
  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    gs_layout-cwidth_opt = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab            = gt_output[]
        it_fieldcatalog      = gt_fcat.
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*************Excel build and email intimation for proposed status *************************
FORM propose_email.

  DATA: lt_propose TYPE STANDARD TABLE OF zhr_hiring_tab,
        ls_propose TYPE zhr_hiring_tab.

  DATA: lt_attachment TYPE TABLE OF solisti1,
        ls_attachment LIKE LINE OF lt_attachment.

  DATA: lv_data TYPE string.
  DATA: lv_xstring TYPE xstring.
  DATA: lt_bin TYPE solix_tab.
  DATA : lv_sub TYPE sood-objdes.
  DATA: it_body_msg   TYPE STANDARD TABLE OF solisti1.
  DATA: salutation TYPE string.
  DATA: body TYPE string.
  DATA: footer TYPE string.
  DATA: lv_date(10) TYPE c.
  DATA: lv_date1(10) TYPE c.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_sender       TYPE REF TO if_sender_bcs,
        sender_mail     TYPE  adr6-smtp_addr,
        lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
        lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
        lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lx_document_bcs TYPE REF TO cx_document_bcs,
        lv_sent_to_all  TYPE os_boolean.

  DATA: lv_body(300) TYPE c.
  DATA: lv_tot   TYPE char50.
  DATA: lt_mail TYPE TABLE OF zhr_hiring_email,
        ls_mail TYPE zhr_hiring_email.

  DATA: ls_tvarvc TYPE tvarvc.
  DATA: in_mailid  TYPE ad_smtpadr,
        in_mailid1 TYPE ad_smtpadr,
        in_mailid2 TYPE ad_smtpadr,
        in_mailid3 TYPE ad_smtpadr.
  DATA: it_mailid  TYPE TABLE OF ad_smtpadr,
        it_mailid1 TYPE TABLE OF ad_smtpadr.
  DATA: lv_position TYPE hr_mcstext.
  DATA: lv_orgname TYPE hr_mcstext,
        lv_objid   TYPE hrp1001-objid.

  REFRESH: lt_propose.

  SELECT * FROM zhr_hiring_tab
           INTO TABLE lt_propose
           FOR ALL ENTRIES IN gt_display3
           WHERE temp_empno EQ gt_display3-temp_empno
           AND temp_empid EQ gt_display3-temp_empid.

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Temp_empno'
              'Temp_empid'
              'Name'
              'DOB'
              'Joining_date'
              'Position ID'
              'Position Name'
              'Orgunit ID'
              'Orgunit Name'
              INTO ls_attachment SEPARATED BY con_tab.


  APPEND ls_attachment TO lt_attachment.
  CLEAR  ls_attachment.

  CONCATENATE  ls_attachment lv_data
     INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

  LOOP AT lt_propose INTO ls_propose.

    CLEAR lv_date.
    WRITE ls_propose-joining_date TO lv_date DD/MM/YYYY.

    CLEAR lv_date1.
    WRITE ls_propose-dob TO lv_date1 DD/MM/YYYY.

    CLEAR lv_position.
    READ TABLE gt_position INTO gs_position WITH KEY objid = ls_propose-plans.
    IF sy-subrc EQ 0.
      lv_position = gs_position-mc_stext.
    ENDIF.

    CLEAR: lv_objid,lv_orgname.
    READ TABLE gt_display2 INTO gs_display2 WITH KEY temp_empno = ls_propose-temp_empno
                                                     temp_empid = ls_propose-temp_empid.
    IF sy-subrc EQ 0.
      lv_objid = gs_display2-orgunitid.
      lv_orgname = gs_display2-orgunitname.
    ENDIF.

    CONCATENATE ls_propose-temp_empno
                ls_propose-temp_empid
                ls_propose-name
                lv_date1
                lv_date
                ls_propose-plans
                lv_position
                lv_objid
                lv_orgname
                INTO ls_attachment SEPARATED BY con_tab.

    CONCATENATE con_cret ls_attachment
    INTO ls_attachment.
    APPEND ls_attachment TO lt_attachment.
    CLEAR ls_attachment.

  ENDLOOP.

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = lt_attachment
      binary_tab = lt_bin.


  CLEAR lv_date.
  WRITE sy-datum TO lv_date DD/MM/YYYY.

  "create send request
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
    CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
  ENDTRY.

  salutation ='Dear Sir/Madam ,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  CLEAR: lv_body.
  CONCATENATE 'Candidate is Proposed for hiring on' lv_date INTO lv_body SEPARATED BY space.
  CONDENSE lv_body.
  body = lv_body.
  CONDENSE body.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  footer = 'Thanks & Best Regards,'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  footer = 'Sheenlac Paints Ltd'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  TRY.
      lo_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_tot ).
    CATCH cx_document_bcs INTO DATA(lc_document_bcs1).
  ENDTRY.

  APPEND LINES OF lt_attachment TO it_body_msg.


  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'XLS'
      i_attachment_subject = 'Candidate for hiring'
      i_att_content_text = lt_attachment ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.

* Pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
  ENDTRY.

  "Sender Mail ID set in TVARVC
  CLEAR ls_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZHR_HIRING_PROP'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc-low .

  "Sender Mail ID
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
    CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
  ENDTRY.


  TRY.
      lo_send_request->set_sender( lo_sender ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
  ENDTRY.


  REFRESH: lt_mail.
  SELECT * FROM zhr_hiring_email
           INTO TABLE lt_mail
           WHERE id_val = '01'.

** TO Mail Recipients **
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_to>) WHERE e_type = 'TO'.
    in_mailid2 = <ls_mail_to>-email.
    APPEND in_mailid2 TO it_mailid.
    CLEAR in_mailid2.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_cc>) WHERE e_type = 'CC'.
    in_mailid3 = <ls_mail_cc>-email.
    APPEND in_mailid3 TO it_mailid1.
    CLEAR in_mailid3.
  ENDLOOP.


** TO Mail Recipients **
  LOOP AT it_mailid INTO in_mailid.
    TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
    ENDTRY.
*set recipient
    TRY.
        lo_send_request->add_recipient(
           EXPORTING
           i_recipient = lo_recipient
            i_express = abap_true
           ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
    ENDTRY.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT it_mailid1 INTO in_mailid1.

    TRY.
        lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
    ENDTRY.
    "Set recipient
    TRY.
        lo_send_request->add_recipient(
         EXPORTING
         i_recipient = lo_recipient1
         i_copy = 'X'
         i_express = abap_true
         ).
        in_mailid2 = in_mailid1.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
    ENDTRY.
  ENDLOOP.

* Send email
  TRY.
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = abap_true
      RECEIVING
      result = lv_sent_to_all ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
  ENDTRY.

  COMMIT WORK.

  CLEAR : in_mailid,in_mailid1,in_mailid2,in_mailid3.

ENDFORM.

*********** Excel build and Mail intimation for Approval *******
FORM approve_email.

  DATA: lt_approve TYPE STANDARD TABLE OF zhr_hiring_tab,
        ls_approve TYPE zhr_hiring_tab.

  DATA: lt_attachment TYPE STANDARD TABLE OF solisti1,
        ls_attachment LIKE LINE OF lt_attachment.

  DATA: lv_data TYPE string.
  DATA: lv_xstring TYPE xstring.
  DATA: lt_bin TYPE solix_tab.
  DATA : lv_sub TYPE sood-objdes.
  DATA: it_body_msg   TYPE STANDARD TABLE OF solisti1.
  DATA: salutation TYPE string.
  DATA: body TYPE string.
  DATA: footer TYPE string.
  DATA: lv_date(10) TYPE c.
  DATA: lv_date1(10) TYPE c.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_sender       TYPE REF TO if_sender_bcs,
        sender_mail     TYPE  adr6-smtp_addr,
        lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
        lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
        lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lx_document_bcs TYPE REF TO cx_document_bcs,
        lv_sent_to_all  TYPE os_boolean.

  DATA: lv_body(300) TYPE c.
  DATA: lv_tot   TYPE char50.
  DATA: lt_mail TYPE TABLE OF zhr_hiring_email,
        ls_mail TYPE zhr_hiring_email.

  DATA: ls_tvarvc TYPE tvarvc.
  DATA: in_mailid  TYPE ad_smtpadr,
        in_mailid1 TYPE ad_smtpadr,
        in_mailid2 TYPE ad_smtpadr,
        in_mailid3 TYPE ad_smtpadr.
  DATA: it_mailid  TYPE TABLE OF ad_smtpadr,
        it_mailid1 TYPE TABLE OF ad_smtpadr.
  DATA: lv_position TYPE hr_mcstext.

  DATA: lv_orgname TYPE hr_mcstext,
        lv_objid   TYPE hrp1001-objid.

  REFRESH: lt_approve.
  SELECT * FROM zhr_hiring_tab
           INTO TABLE lt_approve
           FOR ALL ENTRIES IN gt_display3
           WHERE temp_empno EQ gt_display3-temp_empno
           AND temp_empid EQ gt_display3-temp_empid.
  IF sy-subrc = 0.
    DELETE lt_approve[] WHERE pernr IS INITIAL.
  ENDIF.

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Temp_empno'
              'Temp_empid'
              'Personal Number'
              'Name'
              'DOB'
              'Joining_date'
              'Position ID'
              'Position Name'
              'Orgunit ID'
              'Orgunit Name'
              INTO ls_attachment SEPARATED BY con_tab.

  APPEND ls_attachment TO lt_attachment.
  CLEAR  ls_attachment.

  CONCATENATE  ls_attachment lv_data
     INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

  LOOP AT lt_approve INTO ls_approve.

    CLEAR lv_date.
    WRITE ls_approve-joining_date TO lv_date DD/MM/YYYY.

    CLEAR lv_date1.
    WRITE ls_approve-dob TO lv_date1 DD/MM/YYYY.

    CLEAR lv_position.
    READ TABLE gt_position INTO gs_position WITH KEY objid = ls_approve-plans.
    IF sy-subrc EQ 0.
      lv_position = gs_position-mc_stext.
    ENDIF.

    CLEAR: lv_objid,lv_orgname.
    READ TABLE gt_display1 INTO gs_display1 WITH KEY temp_empno = ls_approve-temp_empno
                                                     temp_empid = ls_approve-temp_empid.
    IF sy-subrc EQ 0.
      lv_objid = gs_display1-orgunitid.
      lv_orgname = gs_display1-orgunitname.
    ENDIF.

    CONCATENATE ls_approve-temp_empno
                ls_approve-temp_empid
                ls_approve-pernr
                ls_approve-name
                lv_date1
                lv_date
                ls_approve-plans
                lv_position
                lv_objid
                lv_orgname
                INTO ls_attachment SEPARATED BY con_tab.

    CONCATENATE con_cret ls_attachment
    INTO ls_attachment.
    APPEND ls_attachment TO lt_attachment.
    CLEAR ls_attachment.

  ENDLOOP.

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = lt_attachment
      binary_tab = lt_bin.


  "create send request
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
    CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
  ENDTRY.

  salutation ='Dear Sir/Madam ,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  CLEAR: lv_body,lv_date.
  WRITE sy-datum TO lv_date DD/MM/YYYY.
  CONCATENATE 'Employees Hired on Today' lv_date INTO lv_body SEPARATED BY space.
  CONDENSE lv_body.
  body = lv_body.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  footer = 'Thanks & Best Regards,'.
  APPEND footer TO lt_message_body.

  footer = 'Sheenlac Paints Ltd'.
  APPEND footer TO lt_message_body.

  lv_tot = 'New Employee Hired'.
  "put your text into the document
  TRY.
      lo_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_tot ).
    CATCH cx_document_bcs INTO DATA(lc_document_bcs1).
  ENDTRY.

  APPEND LINES OF lt_attachment TO it_body_msg.


  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'XLS'
      i_attachment_subject = 'Employees Hired'
      i_att_content_text = lt_attachment ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.

* Pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
  ENDTRY.


  "Sender Mail ID set in TVARVC
  CLEAR ls_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZHR_HIRING_APPR'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc-low .

  "Sender Mail ID
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
    CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
  ENDTRY.

  TRY.
      lo_send_request->set_sender( lo_sender ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
  ENDTRY.

  REFRESH: lt_mail.
  SELECT * FROM zhr_hiring_email
           INTO TABLE lt_mail
           WHERE id_val = '02'.

** TO Mail recipients **
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_to>) WHERE e_type = 'TO'.
    in_mailid2 = <ls_mail_to>-email.
    APPEND in_mailid2 TO it_mailid.
    CLEAR in_mailid2.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_cc>) WHERE e_type = 'CC'.
    in_mailid3 = <ls_mail_cc>-email.
    APPEND in_mailid3 TO it_mailid1.
    CLEAR in_mailid3.
  ENDLOOP.


** TO Mail Recipients **
  LOOP AT it_mailid INTO in_mailid.
    TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
    ENDTRY.
*set recipient
    TRY.
        lo_send_request->add_recipient(
           EXPORTING
           i_recipient = lo_recipient
            i_express = abap_true
           ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
    ENDTRY.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT it_mailid1 INTO in_mailid1.

    TRY.
        lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
    ENDTRY.
    "Set recipient
    TRY.
        lo_send_request->add_recipient(
         EXPORTING
         i_recipient = lo_recipient1
         i_copy = 'X'
         i_express = abap_true
         ).
        in_mailid2 = in_mailid1.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
    ENDTRY.
  ENDLOOP.

* Send email
  TRY.
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = abap_true
      RECEIVING
      result = lv_sent_to_all ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
  ENDTRY.

  COMMIT WORK.

  CLEAR : in_mailid,in_mailid1,in_mailid2,in_mailid3.
ENDFORM.

************Hire Employee infotype 0000 Creation*********
FORM f_hire_employee CHANGING p_flag p_pernr.
  CONSTANTS: lc_update TYPE c VALUE 'S',
             lc_mode   TYPE c VALUE 'N',
             lc_msge   TYPE c VALUE 'E',
             lc_msga   TYPE c VALUE 'A'.


  DATA: lt_bdc TYPE STANDARD TABLE OF bdcdata,
        lt_msg TYPE STANDARD TABLE OF bdcmsgcoll,
        ls_msg TYPE bdcmsgcoll.
  DATA: lv_begda(10)    TYPE c,
        lv_highdate(10) TYPE c.

  gc_endda = '31.12.9999'.
  WRITE gs_display1-joining_date TO lv_begda.
  WRITE gc_endda TO lv_highdate.
  PERFORM gen_bdc_rec USING:
            'X' 'SAPMP50A'          '1000'                          CHANGING lt_bdc[] ,
            ' ' 'RP50G-PERNR'       ' '                             CHANGING lt_bdc[] ,
            ' ' 'RP50G-BEGDA'      lv_begda                         CHANGING lt_bdc[] ,
            ' ' 'RP50G-ENDDA'      lv_highdate                      CHANGING lt_bdc[] ,
            ' ' 'RP50G-TIMR6'       'X'                             CHANGING lt_bdc[] ,
            ' ' 'RP50G-CHOIC'       '0000'                          CHANGING lt_bdc[] ,
            ' ' 'RP50G-SUBTY'       '01'                            CHANGING lt_bdc[] ,
            ' ' 'BDC_OKCODE'        '=INS'                          CHANGING lt_bdc[] ,

            'X' 'MP000000'          '2000'                          CHANGING lt_bdc[] ,
            ' ' 'P0000-BEGDA'       lv_begda                        CHANGING lt_bdc[] ,
            ' ' 'P0000-MASSN'       '01'                            CHANGING lt_bdc[] ,
            ' ' 'P0000-MASSG'       gs_display1-massg               CHANGING lt_bdc[] ,
            ' ' 'PSPAR-PLANS'       gs_display1-plans               CHANGING lt_bdc[] ,
            ' ' 'PSPAR-WERKS'       gs_display1-werks               CHANGING lt_bdc[] ,
            ' ' 'PSPAR-PERSG'       gs_display1-persg               CHANGING lt_bdc[] ,
            ' ' 'PSPAR-PERSK'       gs_display1-persk               CHANGING lt_bdc[] ,
            ' ' 'BDC_OKCODE'        '=UPD'                          CHANGING lt_bdc[] ,

            'X' 'MP000100'          '2000'                          CHANGING lt_bdc[] ,
            ' ' 'BDC_OKCODE'        '/EBCK'                         CHANGING lt_bdc[] ,

            'X' 'SAPLSPO1'          '0200'                          CHANGING lt_bdc[] ,
            ' ' 'BDC_OKCODE'        '=YES'                          CHANGING lt_bdc[] .

  REFRESH lt_msg.
  CALL TRANSACTION 'PA30'
                 USING lt_bdc
                 MODE lc_mode
                 UPDATE lc_update
                 MESSAGES INTO lt_msg.
  LOOP AT lt_msg INTO ls_msg WHERE ( msgtyp = lc_msge OR msgtyp = lc_msga ).
    CLEAR gs_output-message.
    gs_output-infty = '0000'.
    gs_output-status = 'E'.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = ls_msg-msgid
        msgnr               = ls_msg-msgnr
        msgv1               = ls_msg-msgv1
        msgv2               = ls_msg-msgv2
        msgv3               = ls_msg-msgv3
        msgv4               = ls_msg-msgv4
      IMPORTING
        message_text_output = gs_output-message.

*    MESSAGE ID ls_msg-msgid TYPE ls_msg-msgtyp NUMBER
*               ls_msg-msgnr WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
*                              INTO gs_output-message.
    APPEND gs_output TO gt_output.
    p_flag = 'X'.
    ROLLBACK WORK.
  ENDLOOP.
  IF sy-subrc <> 0.
    GET PARAMETER ID 'PER' FIELD p_pernr.
    gs_output-pernr = p_pernr.
    gs_output-infty = '0000'.
    gs_output-subty = '0000'.
    gs_output-status = 'S'.
    gs_output-message =  TEXT-009.
    APPEND gs_output TO gt_output.
    CLEAR: gv_pernr.
    gv_pernr = p_pernr.

    CLEAR l_log.
    l_log-temp_empno = gs_display1-temp_empno.
    l_log-temp_empid = gs_display1-temp_empid.
    l_log-pernr = gs_output-pernr.
    l_log-infty = gs_output-infty.
    l_log-subty = gs_output-subty.
    l_log-executed_on = sy-datum.
    l_log-executed_time = sy-uzeit.
    l_log-status = gs_output-status.
    l_log-message = gs_output-message.
    MODIFY zhr_hiring_log FROM l_log.

  ENDIF.
ENDFORM.
FORM gen_bdc_rec     USING    VALUE(dynbegin) LIKE bdcdata-dynbegin
                              VALUE(fnam)     LIKE bdcdata-fnam
                              VALUE(fval)
                     CHANGING lt_bdc          TYPE table.
  CONSTANTS: lc_x TYPE c VALUE 'X'.
  DATA: ls_bdc TYPE bdcdata.
  IF dynbegin = lc_x.
    MOVE: fnam  TO ls_bdc-program,
          fval  TO ls_bdc-dynpro,
          lc_x  TO ls_bdc-dynbegin.
  ELSE.
    MOVE: fnam  TO ls_bdc-fnam,
          fval  TO ls_bdc-fval.
  ENDIF.
  APPEND ls_bdc TO lt_bdc.
ENDFORM.                    " GEN_BDC_REC " F_HIRE_EMPLOYEE
*************Create infotype 0001 *****************************
FORM f_create_it0001 CHANGING p_flag TYPE flag.

  DATA: ls_0001 TYPE p0001.
  DATA: ls_ret TYPE bapireturn1.

  DATA: ls_hrp1001 TYPE hrp1001.
  DATA: lv_objid TYPE hrp1001-objid.

  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.
  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0001, ls_ret.
    ls_0001-pernr = gs_display1-pernr.
    ls_0001-begda = gs_display1-joining_date.
    ls_0001-endda = '99991231'.
    ls_0001-werks = gs_display1-werks.
    ls_0001-persk = gs_display1-persk.
    ls_0001-btrtl = gs_display1-btrtl.
    ls_0001-persg = gs_display1-persg.
    ls_0001-plans = gs_display1-plans.

    CLEAR gs_werks.
    READ TABLE gt_werks INTO gs_werks WITH KEY persa = gs_display1-werks.
    IF sy-subrc = 0.
      ls_0001-bukrs = gs_werks-bukrs.
    ENDIF.

* get orgunit
    CLEAR ls_hrp1001.
    SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001
      WHERE otype = 'S'
        AND objid = gs_display1-plans
        AND plvar = '01'
        AND rsign = 'A'
        AND relat = '003'
        AND istat = '1'
        AND begda LE sy-datum
        AND endda GE sy-datum
        AND sclas = 'O'.
    IF sy-subrc = 0.
      ls_0001-orgeh = ls_hrp1001-sobid+0(8).

      CLEAR lv_objid.
      lv_objid = ls_hrp1001-sobid+0(8).
*Getting the costcenter
      CLEAR ls_hrp1001.
      SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001
        WHERE otype = 'O'
          AND objid = lv_objid
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '011'
          AND istat = '1'
          AND begda LE sy-datum
          AND endda GE sy-datum
          AND sclas = 'K'.
      IF sy-subrc = 0.
        ls_0001-kostl = ls_hrp1001-sobid+0(10).
        CONDENSE ls_0001-kostl.
      ENDIF.
    ENDIF.

*getting job key
    CLEAR ls_hrp1001.
    SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001
      WHERE otype = 'S'
        AND objid = gs_display1-plans
        AND plvar = '01'
        AND rsign = 'B'
        AND relat = '007'
        AND istat = '1'
        AND begda LE sy-datum
        AND endda GE sy-datum
        AND sclas = 'C'.
    IF sy-subrc = 0.
      ls_0001-stell = ls_hrp1001-sobid+0(8).
    ENDIF.

    ls_0001-abkrs = 'CE'.

    ls_0001-infty = '0001'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0001'
        number        = ls_0001-pernr
        subtype       = ls_0001-subty
        validityend   = ls_0001-endda
        validitybegin = ls_0001-begda
        record        = ls_0001
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.

    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
      p_flag = abap_true.
      CLEAR gs_output.
      gs_output-pernr = ls_0001-pernr.
      gs_output-subty = ls_0001-subty.
      gs_output-infty = ls_0001-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0001-pernr.
      gs_output-subty = ls_0001-subty.
      gs_output-infty = ls_0001-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0001-pernr.
    gs_output-subty = ls_0001-subty.
    gs_output-infty = ls_0001-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.

  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.

FORM f_position_assignment .
  CONSTANTS: lc_fcode      TYPE hrrhap-fcode  VALUE 'INSE',
             lc_vtask      TYPE hrrhap-vtask  VALUE 'D',
             lc_commit_flg TYPE c         VALUE 'X',
             lc_rsign      TYPE hri1001-rsign VALUE 'A',
             lc_relat      TYPE hri1001-relat VALUE '008',
             lc_infty      TYPE hri1001-infty VALUE '1001',
             lc_sclas      TYPE hri1001-sclas VALUE 'P',
             lc_istat      TYPE hri1001-istat VALUE '1',
             lc_prozt      TYPE hri1001-prozt VALUE '100.00'.

  DATA: ls_hri1001 TYPE hri1001,
        lt_hri1001 TYPE STANDARD TABLE OF hri1001.
  REFRESH lt_hri1001.
  CLEAR ls_hri1001.
  ls_hri1001-plvar = '01'.
  ls_hri1001-otype = 'S'.
  ls_hri1001-objid = gs_display1-plans.
  ls_hri1001-rsign = lc_rsign.
  ls_hri1001-relat = lc_relat.
  ls_hri1001-begda = gs_display1-joining_date.
  ls_hri1001-endda = '99991231'.
  ls_hri1001-infty = lc_infty.
  ls_hri1001-sclas = lc_sclas.
  ls_hri1001-istat = lc_istat.
  ls_hri1001-prozt = lc_prozt.
  ls_hri1001-sobid = gs_display1-pernr.

  APPEND ls_hri1001 TO lt_hri1001.

  CALL FUNCTION 'RH_INSERT_INFTY_1001_EXT'
    EXPORTING
      fcode                   = lc_fcode
      vtask                   = lc_vtask
      commit_flg              = lc_commit_flg
      authy                   = ' '
    TABLES
      innnn                   = lt_hri1001
    EXCEPTIONS
      no_authorization        = 1
      error_during_insert     = 2
      relation_not_reversible = 3
      corr_exit               = 4
      begda_greater_endda     = 5
      OTHERS                  = 6.

  IF lt_hri1001 IS NOT INITIAL.
    UPDATE zhr_hiring_tab SET hrp1001 = 'X'
                          WHERE temp_empno = gs_display1-temp_empno
                          AND temp_empid = gs_display1-temp_empid.
    CLEAR gs_output.
    gs_output-pernr = ls_hri1001-sobid.
    gs_output-subty = ls_hri1001-otype.
    gs_output-infty = ls_hri1001-infty.
    gs_output-status = 'S'.
    gs_output-message   = 'Employee position created (HRP1001) in SAP'.
    APPEND gs_output TO gt_output.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_hri1001-sobid.
    gs_output-subty = ls_hri1001-otype.
    gs_output-infty = ls_hri1001-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee position creating (HRP1001) Failed'.
    APPEND gs_output TO gt_output.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.                    " F_POSITION_ASSIGNMENT
***********Infotype 0002 creation***********
FORM f_create_it0002.

  DATA: ls_0002 TYPE p0002.
  DATA: ls_ret TYPE bapireturn1.
  DATA: lv_len TYPE i.

  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.
  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0002, ls_ret.
    ls_0002-pernr = gs_display1-pernr.
    ls_0002-begda = gs_display1-joining_date.
    ls_0002-endda = '99991231'.

    lv_len = strlen( gs_display1-name ).
    IF lv_len LE 40.
      ls_0002-vorna = gs_display1-name.
    ELSE.
      SPLIT gs_display1-name AT ' ' INTO DATA(lv_string1) DATA(lv_string2) DATA(lv_string3) DATA(lv_string4).

      CONCATENATE lv_string1 lv_string2 INTO lv_string1 SEPARATED BY space.
      CONDENSE lv_string1.
      ls_0002-vorna = lv_string1.

      IF lv_string3 IS NOT INITIAL.
        CONCATENATE lv_string3 lv_string4 INTO lv_string3 SEPARATED BY space.
        CONDENSE lv_string3.
        ls_0002-nachn = lv_string3.
      ENDIF.
    ENDIF.


    ls_0002-gbdat = gs_display1-dob.
    ls_0002-natio = gs_display1-natio.

    ls_0002-famst = gs_display1-famst.
*    ls_0002-zbgrup = gs_display1-blood_grp.
    ls_0002-sprsl = 'EN'.

    IF gs_display1-gender EQ 'M' or gs_display1-gender eq '1'.
      ls_0002-gesch = '1'.
      ls_0002-anred = '1'.
    ELSEIF gs_display1-gender EQ 'F' or gs_display1-gender eq '2'.
      ls_0002-gesch = '2'.
      ls_0002-anred = '4'.
    ENDIF.

    ls_0002-infty = '0002'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0002'
        number        = ls_0002-pernr
        subtype       = ls_0002-subty
        validityend   = ls_0002-endda
        validitybegin = ls_0002-begda
        record        = ls_0002
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.

    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
      UPDATE zhr_hiring_tab SET it0002 = 'X'
                      WHERE temp_empno = gs_display1-temp_empno
                      AND temp_empid = gs_display1-temp_empid.
      CLEAR gs_output.
      gs_output-pernr = ls_0002-pernr.
      gs_output-subty = ls_0002-subty.
      gs_output-infty = ls_0002-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0002-pernr.
      gs_output-subty = ls_0002-subty.
      gs_output-infty = ls_0002-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0002-pernr.
    gs_output-subty = ls_0002-subty.
    gs_output-infty = ls_0002-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.

  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
**********Update Employee Number From SAP to Greythr **********
FORM update_greythr.
  DATA: ls_pernr(10) TYPE c.
  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
  AND type = 'S'.
  CLEAR lv_url.
  lv_url = gs_tvarvc-low.

  CLEAR create_url.
  CONCATENATE lv_url gs_display1-temp_empid INTO create_url.
  CONDENSE create_url NO-GAPS.

  cl_http_client=>create_by_url(
   EXPORTING
   url = create_url
   IMPORTING
   client = lo_http_client
   EXCEPTIONS
   argument_not_found = 1
   plugin_not_active = 2
   internal_error = 3
   OTHERS = 4 ).

  CHECK lo_http_client IS BOUND.
  lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

  lo_http_client->request->set_method( 'PUT' ).

  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  SELECT SINGLE *  FROM tvarvc INTO gs_tvarvc1 WHERE name = 'URL_LOV_API'
                                              AND type = 'P'.

  CLEAR str2.
  str2 = gs_tvarvc1-low.

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = str2 ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  CLEAR ls_pernr.
  ls_pernr = gs_display1-pernr.
  SHIFT ls_pernr LEFT DELETING LEADING '0'.
  CONDENSE ls_pernr NO-GAPS.

  CONCATENATE '{'
              '"employeeNo":"' ls_pernr'"'
              '}' INTO v_jsonload.
  lo_http_client->request->set_cdata(
 EXPORTING
   data = v_jsonload ).

  lo_http_client->send(
  EXCEPTIONS
  http_communication_failure = 1
  http_invalid_state = 2 ).


  CHECK sy-subrc = 0.
  lo_http_client->receive(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2
   http_processing_failed = 3 ).


  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).

  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
    lv_response = lo_http_client->response->get_cdata( ).
    UPDATE zhr_hiring_tab SET greythr_upd = 'X'
                          WHERE temp_empno = gs_display1-temp_empno
                          AND temp_empid = gs_display1-temp_empid.

    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = 'greythr_upd'.
    gs_output-infty = 'PUT_METHOD'.
    gs_output-status = 'S'.
    gs_output-message   = 'Employee Update From SAP to Greythr'.
    APPEND gs_output TO gt_output.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = 'greythr_upd'.
    gs_output-infty = 'PUT_METHOD'.
    gs_output-status = 'E'.
    gs_output-message   = lv_response.
    APPEND gs_output TO gt_output.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
*------ Coscenter Updation --------*
FORM update_costcentr USING p_empid
                            p_kostl TYPE kostl
                            p_date.

  IF sy-subrc = 0.
    DATA(lv_datum) = |{ p_date+0(4) }-{ p_date+4(2) }-{ p_date+6(2) }|.
  ENDIF.

  CLEAR gs_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO gs_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
                                             AND type = 'P'.
  IF sy-subrc = 0.
    CLEAR lv_url.
    lv_url = gs_tvarvc-low.
  ENDIF.

  CLEAR create_url.
  create_url = |{ lv_url }{ p_empid }/categories|.

  cl_http_client=>create_by_url(
   EXPORTING
   url = create_url
   IMPORTING
   client = lo_http_client
   EXCEPTIONS
   argument_not_found = 1
   plugin_not_active = 2
   internal_error = 3
   OTHERS = 4 ).

  CHECK lo_http_client IS BOUND.
  lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

  lo_http_client->request->set_method( 'POST' ).

  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  SELECT SINGLE *  FROM tvarvc INTO gs_tvarvc1 WHERE name = 'URL_LOV_API'
                                              AND type = 'P'.

  CLEAR str2.
  str2 = gs_tvarvc1-low.

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = str2 ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  SELECT SINGLE cc_id FROM zhr_greyhr_cstcr INTO @DATA(lv_ccid) WHERE costcenter = @p_kostl.
  IF sy-subrc = 0.

  ENDIF.
  CLEAR v_jsonload.
  CONCATENATE '{'
     '"list":['
        '{'
           '"category":5,'
           '"value":' lv_ccid','
           '"effectiveDate":"' lv_datum'"'
       ' }'
     ']'
  '}' INTO v_jsonload.
  lo_http_client->request->set_cdata(
 EXPORTING
   data = v_jsonload ).

  lo_http_client->send(
  EXCEPTIONS
  http_communication_failure = 1
  http_invalid_state = 2 ).


  CHECK sy-subrc = 0.
  lo_http_client->receive(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2
   http_processing_failed = 3 ).


  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).

  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 201.
    lv_response = lo_http_client->response->get_cdata( ).
    UPDATE zhr_hiring_tab SET greythr_upd = 'X'
                          WHERE temp_empno = gs_display1-temp_empno
                          AND temp_empid = gs_display1-temp_empid.

    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = 'greythr_upd'.
    gs_output-infty = 'POST'.
    gs_output-status = 'S'.
    gs_output-message   = 'Employee Costcenter Update From SAP to Greythr'.
    APPEND gs_output TO gt_output.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = 'greythr_upd'.
    gs_output-infty = 'POST'.
    gs_output-status = 'E'.
    gs_output-message   = lv_http_error.
    APPEND gs_output TO gt_output.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
***********Infotype 0041 creation***********
FORM f_create_it0041 .

  DATA: ls_0041 TYPE p0041.
  DATA: ls_ret TYPE bapireturn1.

  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.
  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0041, ls_ret.
    ls_0041-pernr = gs_display1-pernr.
    ls_0041-begda = gs_display1-joining_date.
    ls_0041-endda = '99991231'.
    ls_0041-dar01 = 'S1'.
    ls_0041-dat01 = gs_display1-joining_date.

    ls_0041-infty = '0041'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0041'
        number        = ls_0041-pernr
        subtype       = ls_0041-subty
        validityend   = ls_0041-endda
        validitybegin = ls_0041-begda
        record        = ls_0041
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.

    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
      UPDATE zhr_hiring_tab SET it0041 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      CLEAR gs_output.
      gs_output-pernr = ls_0041-pernr.
      gs_output-subty = ls_0041-subty.
      gs_output-infty = ls_0041-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0041-pernr.
      gs_output-subty = ls_0041-subty.
      gs_output-infty = ls_0041-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.

  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0041-pernr.
    gs_output-subty = ls_0041-subty.
    gs_output-infty = ls_0041-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
***********Infotype 0105 Creation*************
FORM f_create_it0105 USING p_subty TYPE subty .
  DATA: ls_ret  TYPE  bapireturn1,  "Structure for Messages"
        ls_0105 TYPE p0105.

  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.
  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0105, ls_ret.
    ls_0105-pernr = gs_display1-pernr.

    ls_0105-begda = gs_display1-joining_date.
    ls_0105-endda = '99991231'.
    IF p_subty = '0002'.
      ls_0105-subty  = '0002'.
      ls_0105-usrty  = '0002'.
      ls_0105-usrid =  gs_display1-temp_empid.
    ELSEIF p_subty = '0030'.
      ls_0105-subty  = '0030'.
      ls_0105-usrty  = '0030'.
      ls_0105-usrid_long = gs_display1-e_mail.
    ELSEIF  p_subty = 'CELL'.
      ls_0105-subty  = 'CELL'.
      ls_0105-usrty  = 'CELL'.
      ls_0105-usrid =  gs_display1-mobile_no.
    ENDIF.

    ls_0105-infty = '0105'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0105'
        number        = ls_0105-pernr
        subtype       = ls_0105-subty
        validityend   = ls_0105-endda
        validitybegin = ls_0105-begda
        record        = ls_0105
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.


    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

      IF ls_0105-subty EQ '0002'.
        UPDATE zhr_hiring_tab SET it0105_sub1 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.

      ELSEIF ls_0105-subty EQ '0030'.
        UPDATE zhr_hiring_tab SET it0105_sub2 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ELSEIF ls_0105-subty EQ 'CELL'.
        UPDATE zhr_hiring_tab SET it0105_sub3 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ENDIF.

      CLEAR gs_output.
      gs_output-pernr = ls_0105-pernr.
      gs_output-subty = ls_0105-subty.
      gs_output-infty = ls_0105-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0105-pernr.
      gs_output-subty = ls_0105-subty.
      gs_output-infty = ls_0105-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0105-pernr.
    gs_output-subty = ls_0105-subty.
    gs_output-infty = ls_0105-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
*****Infotype 0006 Creation **************
FORM f_create_it0006 USING p_subty TYPE subty .

  DATA: ls_0006 TYPE p0006,
        ls_ret  TYPE bapireturn1.

  IF p_subty = '3' AND gs_display1-pres_address1 IS INITIAL.
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = p_subty.
    gs_output-infty = '0006'.
    gs_output-status = 'S'.
    gs_output-message   = 'No Present Address Data to update'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.

  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.

  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0006, ls_ret.
    ls_0006-pernr = gs_display1-pernr.
    ls_0006-begda = gs_display1-joining_date.
    ls_0006-endda = '99991231'.

    IF p_subty = '1'.
      ls_0006-subty  = '1'.
    ELSEIF p_subty = '3'.
      ls_0006-subty  = '3'.
    ELSEIF p_subty = '4'.
      ls_0006-subty = '4'.
    ENDIF.

    IF p_subty = '1'.
      ls_0006-name2 = gs_display1-name.
      ls_0006-stras = gs_display1-perm_address1.
      ls_0006-locat = gs_display1-perm_address2.
      ls_0006-pstlz = gs_display1-perm_postal_code.
      ls_0006-ort01 = gs_display1-perm_city.
      ls_0006-land1 = gs_display1-perm_country.
      ls_0006-state = gs_display1-perm_state.
      ls_0006-telnr = gs_display1-p_phone1.
      ls_0006-com01 = 'CELL'.
      ls_0006-num01 = gs_display1-p_phone2.
      ls_0006-com02 = 'CELL'.
      ls_0006-num02 = gs_display1-mobile_no.
    ELSEIF p_subty = '3'.
      ls_0006-name2 = gs_display1-name.
      ls_0006-stras = gs_display1-pres_address1.
      ls_0006-locat = gs_display1-pres_address2.
      ls_0006-pstlz = gs_display1-pres_postal_code.
      ls_0006-ort01 = gs_display1-pres_city.
      ls_0006-land1 = gs_display1-pres_country.
      ls_0006-state = gs_display1-pres_state.
      ls_0006-telnr = gs_display1-pr_phone1.
      ls_0006-com01 = 'CELL'.
      ls_0006-num01 = gs_display1-pr_phone2.
      ls_0006-com02 = 'CELL'.
      ls_0006-num02 = gs_display1-mobile_no.
    ELSEIF p_subty = '4'.
      ls_0006-name2 = gs_display1-care_of.
      ls_0006-telnr = gs_display1-em_contact.
      ls_0006-land1 = 'IN'.
      ls_0006-ort01 = 'DEFAULT'.
    ENDIF.

    ls_0006-infty = '0006'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0006'
        number        = ls_0006-pernr
        subtype       = ls_0006-subty
        validityend   = ls_0006-endda
        validitybegin = ls_0006-begda
        record        = ls_0006
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.

    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

      IF ls_0006-subty EQ '1'.
        UPDATE zhr_hiring_tab SET it0006_sub1 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ELSEIF ls_0006-subty EQ '3'.
        UPDATE zhr_hiring_tab SET it0006_sub2 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ENDIF.

      CLEAR gs_output.
      gs_output-pernr = ls_0006-pernr.
      gs_output-subty = ls_0006-subty.
      gs_output-infty = ls_0006-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0006-pernr.
      gs_output-subty = ls_0006-subty.
      gs_output-infty = ls_0006-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.

    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0006-pernr.
    gs_output-subty = ls_0006-subty.
    gs_output-infty = ls_0006-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
***********Infotype 0185 Creation**************
FORM f_create_it0185 USING p_subty TYPE subty.

  DATA: ls_ret  TYPE  bapireturn1,  "Structure for Messages"
        ls_0185 TYPE p0185.         "Structure for Infotype 0185"

  IF p_subty = '02' AND gs_display1-pancard IS INITIAL. "PAN
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = p_subty.
    gs_output-infty = '0185'.
    gs_output-status = 'S'.
    gs_output-message   = 'No Pancard Data to update'.
    APPEND gs_output TO gt_output.
    EXIT.
  ELSEIF p_subty = '06' AND gs_display1-aadhar_no IS INITIAL . "AADHAR
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = p_subty.
    gs_output-infty = '0185'.
    gs_output-status = 'S'.
    gs_output-message   = 'No AAdhar Card Data to update'.
    APPEND gs_output TO gt_output.
    EXIT.
  ELSEIF p_subty = '09' AND gs_display1-passport IS INITIAL . "PASSPORT
    CLEAR gs_output.
    gs_output-pernr = gs_display1-pernr.
    gs_output-subty = p_subty.
    gs_output-infty = '0185'.
    gs_output-status = 'S'.
    gs_output-message   = 'No Passport Data to update'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.



  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.
  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0185, ls_ret.
    ls_0185-pernr = gs_display1-pernr.
    ls_0185-begda = gs_display1-joining_date.
    ls_0185-endda = '99991231'.
    IF p_subty = '02' . "PAN
      ls_0185-subty = '02'.
      ls_0185-ictyp  = '02'.
      ls_0185-icnum = gs_display1-pancard.
      ls_0185-ename = gs_display1-pan_name.
    ELSEIF p_subty = '06' . "AADHAR
      ls_0185-subty = '06'.
      ls_0185-ictyp  = '06'.
      ls_0185-icnum = gs_display1-aadhar_no.
      ls_0185-ename = gs_display1-aadhar_name.
    ELSEIF p_subty = '09' . "PASSPORT
      ls_0185-subty = '09'.
      ls_0185-ictyp  = '09'.
      ls_0185-icnum = gs_display1-passport.
      ls_0185-ename = gs_display1-passport_name.
      ls_0185-expid = gs_display1-expid.
    ENDIF.


    ls_0185-infty = '0185'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0185'
        number        = ls_0185-pernr
        subtype       = ls_0185-subty
        validityend   = ls_0185-endda
        validitybegin = ls_0185-begda
        record        = ls_0185
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.


    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
      IF ls_0185-subty EQ '02'.
        UPDATE zhr_hiring_tab SET it0185_sub1 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ELSEIF ls_0185-subty EQ '06'.
        UPDATE zhr_hiring_tab SET it0185_sub2 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ELSEIF ls_0185-subty EQ '09'.
        UPDATE zhr_hiring_tab SET it0185_sub3 = 'X'
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      ENDIF.
      CLEAR gs_output.
      gs_output-pernr = ls_0185-pernr.
      gs_output-subty = ls_0185-subty.
      gs_output-infty = ls_0185-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0185-pernr.
      gs_output-subty = ls_0185-subty.
      gs_output-infty = ls_0185-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.

    ENDIF.
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0185-pernr.
    gs_output-subty = ls_0185-subty.
    gs_output-infty = ls_0185-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
***********Infotype 0009 Creation**************
FORM f_create_it0009 USING p_subty TYPE subty.

  DATA: bankcountry TYPE bapi1011_key-bank_ctry,
        bankkey     TYPE bapi1011_key-bank_key.


  IF gs_display1-bankl IS NOT INITIAL.
    DATA(l_ifsccode) =  gs_display1-bankl.
  ENDIF.

  DATA: ls_ret  TYPE bapireturn1,
        l_ret   TYPE bapiret2,
        ls_0009 TYPE p0009.
  CLEAR ls_ret.
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_display1-pernr
    IMPORTING
      return = ls_ret.

  IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
    CLEAR:ls_0009, ls_ret.
    ls_0009-pernr = gs_display1-pernr.
    ls_0009-begda = gs_display1-joining_date.
    ls_0009-endda = '99991231'.

    ls_0009-subty = '0'.
    IF gs_display1-bankn IS NOT INITIAL .
      ls_0009-bankn = gs_display1-bankn.
*    ELSE.
*      ls_0009-bankn = '1234567891234'.
    ENDIF.

    IF l_ifsccode IS NOT INITIAL.
      SELECT SINGLE banks, bankl FROM bnka INTO @DATA(l_banks) WHERE bankl = @l_ifsccode.
      IF sy-subrc = 0.
        ls_0009-bankl = l_banks-bankl.
        ls_0009-banks = l_banks-banks.
      ELSE.
        SELECT SINGLE value FROM zhr_greythr_lov INTO @DATA(l_bankname) WHERE id = @gs_display1-bankcode.
        IF sy-subrc = 0.
          DATA(bankname) = CONV banka( l_bankname ).
          DATA(bank_ctry) = CONV banks( 'IN' ).
          DATA(bank_key) =  l_ifsccode.
          DATA(ls_add) = VALUE bapi1011_address( bank_name = bankname ).
          "Creating bank key
          CLEAR: l_ret,bankcountry,bankkey.
          CALL FUNCTION 'BAPI_BANK_CREATE'
            EXPORTING
              bank_ctry    = bank_ctry
              bank_key     = bank_key
              bank_address = ls_add
              i_xupdate    = 'X'
            IMPORTING
              return       = l_ret
              bankcountry  = bankcountry
              bankkey      = bankkey.
          IF l_ret IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            ls_0009-bankl = CONV bankl( bankkey ).
            ls_0009-banks = CONV banks( bankcountry ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*    IF ls_0009-bankl IS INITIAL.
*      ls_0009-bankl = '913020026408306'.
*    ENDIF.
    ls_0009-emftx = gs_display1-name.
    ls_0009-zlsch = 'N'.

    IF ls_0009-banks IS INITIAL.
      ls_0009-banks = 'IN'.
    ENDIF.

    ls_0009-waers = 'INR'.
    ls_0009-bkont = '11'.
    ls_0009-infty = '0009'.

    CLEAR: ls_ret.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '0009'
        number        = ls_0009-pernr
        subtype       = ls_0009-subty
        validityend   = ls_0009-endda
        validitybegin = ls_0009-begda
        record        = ls_0009
        operation     = 'INS'
      IMPORTING
        return        = ls_ret.

    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
      UPDATE zhr_hiring_tab SET it0009 = 'X'
                                bankn = ls_0009-bankn
                                bankl = ls_0009-bankl
                        WHERE temp_empno = gs_display1-temp_empno
                        AND temp_empid = gs_display1-temp_empid.
      CLEAR gs_output.
      gs_output-pernr = ls_0009-pernr.
      gs_output-subty = ls_0009-subty.
      gs_output-infty = ls_0009-infty.
      gs_output-status = 'S'.
      gs_output-message   = 'Data Inserted successfully'.
      APPEND gs_output TO gt_output.
    ELSE.
      CLEAR gs_output.
      gs_output-pernr = ls_0009-pernr.
      gs_output-subty = ls_0009-subty.
      gs_output-infty = ls_0009-infty.
      gs_output-status = 'E'.
      gs_output-message   = ls_ret-message.
      APPEND gs_output TO gt_output.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_display1-pernr
      IMPORTING
        return = ls_ret.
  ELSE.
    CLEAR gs_output.
    gs_output-pernr = ls_0009-pernr.
    gs_output-subty = ls_0009-subty.
    gs_output-infty = ls_0009-infty.
    gs_output-status = 'E'.
    gs_output-message   = 'Employee id already locked in SAP'.
    APPEND gs_output TO gt_output.
    EXIT.
  ENDIF.
  CLEAR l_log.
  l_log-temp_empno = gs_display1-temp_empno.
  l_log-temp_empid = gs_display1-temp_empid.
  l_log-pernr = gs_output-pernr.
  l_log-infty = gs_output-infty.
  l_log-subty = gs_output-subty.
  l_log-executed_on = sy-datum.
  l_log-executed_time = sy-uzeit.
  l_log-status = gs_output-status.
  l_log-message = gs_output-message.
  MODIFY zhr_hiring_log FROM l_log.
ENDFORM.
*********PERNR VENDOR LINKING FOR BP***********
FORM pernr_vendor.

*  DATA: lt_seltab TYPE TABLE OF rsparams,
*        ls_seltab LIKE LINE OF lt_seltab.
*
*  REFRESH: lt_seltab.
*  CLEAR ls_seltab.
*  ls_seltab-selname = 'PNPTIMR1'.          " Name of parameter on submitted program
*  ls_seltab-kind    = 'P'.
*  ls_seltab-sign    = 'I'.
*  ls_seltab-option  = 'EQ'.
*  ls_seltab-low     = 'X'.
*  APPEND ls_seltab TO lt_seltab.
*
*  CLEAR ls_seltab.
*  ls_seltab-selname = 'PNPPERNR'.          " Name of parameter on submitted program
*  ls_seltab-kind    = 'S'.
*  ls_seltab-sign    = 'I'.
*  ls_seltab-option  = 'EQ'.
*  ls_seltab-low     = gs_display1-pernr.
*  APPEND ls_seltab TO lt_seltab.
*
*  SUBMIT /shcm/rh_sync_bupa_empl_single
*         WITH SELECTION-TABLE lt_seltab
*         EXPORTING LIST TO MEMORY
*         AND RETURN.


*  DATA: lt_pernr_tab TYPE /shcm/t_pernr_change,
*        ls_pernr_tab TYPE /shcm/s_pernr_change.
*  DATA: lc_ven    TYPE xfeld,
*        lc_bank   TYPE xfeld,
*        lc_wpdata TYPE xfeld,
*        lc_add    TYPE /shcm/ovr_with_default_address,
*        gs_bank   TYPE /shcm/ovr_with_default_bank,
*        lc_role   TYPE /shcm/ovr_with_default_roles.
*
*  DATA lv_is_ok  TYPE boole_d.
*  DATA lo_message_handler TYPE REF TO cl_hrpa_message_list.
**  lc_ven = 'X'.
**  lc_bank = 'X'.
**  lc_wpdata = 'X'.
**  lc_add = abap_true.
**  gs_bank = abap_true.
**  lc_role = abap_true.
*
*  REFRESH: lt_pernr_tab.
*  CLEAR ls_pernr_tab.
*  ls_pernr_tab-pernr = gs_display1-pernr.
*  APPEND ls_pernr_tab TO lt_pernr_tab.
*
*  CREATE OBJECT lo_message_handler.
*
*  CLEAR lv_is_ok.
*  IF lt_pernr_tab[] IS NOT INITIAL.
*
*    "Lock the employee
*    CALL METHOD cl_hrpa_masterdata_enq_deq=>enqueue_by_pernr
*      EXPORTING
*        tclas           = 'A'
*        pernr           = ls_pernr_tab-pernr
*        message_handler = lo_message_handler
*      IMPORTING
*        is_ok           = lv_is_ok.
*
*    IF lv_is_ok = abap_true.
*      CALL FUNCTION '/SHCM/TRIGGER_BUPA_SYNC' DESTINATION 'NONE'
*        EXPORTING
*          it_pernr_tab          = lt_pernr_tab
*          iv_suppress_log       = abap_false
*          iv_migration_check    = abap_true
*          iv_override_wpdata    = lc_wpdata
*          iv_override_data      = lc_bank
*          iv_vendor_ovrdata     = lc_ven
*          iv_defaulting_address = lc_add
*          iv_defaulting_bank    = gs_bank
*          iv_defaulting_roles   = lc_role.
*      IF sy-subrc EQ 0.
*        WAIT UP TO 1 SECONDS.
*        COMMIT WORK.
*      ENDIF.
*      "Release the employee
*      CALL METHOD cl_hrpa_masterdata_enq_deq=>dequeue_by_pernr
*        EXPORTING
*          tclas = 'A'
*          pernr = ls_pernr_tab-pernr.
*
*    ENDIF.
*  ENDIF.
*
*  DATA: lt_hrp1001 TYPE STANDARD TABLE OF hrp1001,
*        ls_hrp1001 TYPE hrp1001,
*        gs_hrp1001 TYPE hrp1001.
*
*  REFRESH lt_hrp1001.
*  CLEAR: ls_hrp1001,gs_hrp1001.
*
*  SELECT SINGLE * FROM hrp1001
*           INTO  ls_hrp1001
*           WHERE otype = 'P'
*             AND objid EQ gs_display1-pernr
*             AND plvar = '01'
*             AND rsign = 'A'
*             AND relat = '209'
*             AND begda LE '99991231'
*             AND endda GE gs_display1-joining_date
*             AND sclas EQ 'CP'.
*  IF sy-subrc EQ 0.
*    SELECT SINGLE * FROM hrp1001
*                    INTO gs_hrp1001
*                    WHERE otype EQ 'CP'
*                      AND objid EQ ls_hrp1001-sobid
*                      AND plvar = '01'
*                      AND rsign = 'B'
*                      AND relat = '207'
*                      AND sclas EQ 'BP'.
*
*    IF sy-subrc EQ 0.
*      UPDATE zhr_hiring_tab SET pernr_vendor = 'X'
*                           WHERE temp_empno = gs_display1-temp_empno
*                           AND temp_empid = gs_display1-temp_empid.
*      CLEAR gs_output.
*      gs_output-pernr = gs_display1-pernr.
*      gs_output-subty = 'BP'.
*      gs_output-infty = 'EMBP'.
*      gs_output-status = 'S'.
*      CONCATENATE 'Employee linked to BP' gs_hrp1001-sobid INTO gs_output-message SEPARATED BY space.
*      APPEND gs_output TO gt_output.
*    ELSE.
*      CLEAR gs_output.
*      gs_output-pernr = gs_display1-pernr.
*      gs_output-subty = 'BP'.
*      gs_output-infty = 'HRP1001'.
*      gs_output-status = 'E'.
*      gs_output-message   = 'Employee BP linking Failed'.
*      APPEND gs_output TO gt_output.
*      EXIT.
*
*    ENDIF.
*  ELSE.
*  ENDIF.

ENDFORM.

**************ALV DISPLAY**********************
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 f_var5 f_var6 f_var7 f_var8 f_var9.
  gs_fcat-fieldname = f_var1.
  gs_fcat-coltext = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  gs_fcat-f4availabl = f_var5.
  gs_fcat-ref_table = f_var6.
  gs_fcat-ref_field = f_var7.
  gs_fcat-no_out = f_var8.
  gs_fcat-checkbox = f_var9.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_Pernr_bp_job
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pernr_bp_job .
*to submit job for the pernr_BP creation
  DATA: jobname LIKE tbtcjob-jobname VALUE
                               'EMPLOYEEBP'.
  DATA: jobcount LIKE tbtcjob-jobcount,
        host     LIKE msxxlist-host.
  DATA: BEGIN OF starttime.
          INCLUDE STRUCTURE tbtcstrt.
  DATA: END OF starttime.

  DATA: lv_uname TYPE sy-uname VALUE 'SPLIT'.

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  DATA: starttimeimmediate LIKE btch0000-char1 VALUE 'X'.


  DATA: gs_hiring1 TYPE zhr_hiring_tab.


  SELECT SINGLE * FROM zhr_hiring_tab"#EC CI_NOFIELD
  INTO gs_hiring1
  WHERE status = '04'
    AND pernr_vendor NE 'X'.

  IF sy-subrc = 0.

    CONCATENATE jobname sy-datum sy-uzeit INTO jobname.

* Job open
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        delanfrep        = ' '
        jobgroup         = ' '
        jobname          = jobname
        sdlstrtdt        = sy-datum
        sdlstrttm        = sy-uzeit
      IMPORTING
        jobcount         = jobcount
      EXCEPTIONS
        cant_create_job  = 01
        invalid_job_data = 02
        jobname_missing  = 03.
    IF sy-subrc NE 0.
      "error processing
    ENDIF.

    REFRESH: lt_seltab.
    CLEAR ls_seltab.
    ls_seltab-selname = 'P_RAD1'.          " Name of parameter on submitted program
    ls_seltab-kind    = 'P'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = 'X'.
    APPEND ls_seltab TO lt_seltab.

    SUBMIT zhr_emp_bp_synch AND RETURN
         WITH SELECTION-TABLE lt_seltab
         USER lv_uname
         VIA JOB jobname
         NUMBER jobcount.
    IF sy-subrc > 0.
      "error processing
    ENDIF.

*Close job
    starttime-sdlstrtdt = sy-datum + 1.
    starttime-sdlstrttm = '220000'.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = jobcount
        jobname              = jobname
        strtimmed            = starttimeimmediate
      EXCEPTIONS
        cant_start_immediate = 01
        invalid_startdate    = 02
        jobname_missing      = 03
        job_close_failed     = 04
        job_nosteps          = 05
        job_notex            = 06
        lock_failed          = 07
        OTHERS               = 99.
    IF sy-subrc EQ 0.
      "error processing
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_restrict_option
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_restrict_option USING p_name TYPE rsrestrict-objectname.

  CLEAR: ls_restrict,
         ls_opt_list,
         ls_ass.

  ls_opt_list-name       = 'EQUAL'.
  ls_opt_list-options-eq = 'X'.   " Restrict to equal
  APPEND ls_opt_list TO ls_restrict-opt_list_tab.

  ls_ass-kind            = 'S'.
  ls_ass-name            = p_name. " select option which you want
  "to restrict
  ls_ass-sg_main         = 'I'.
  ls_ass-sg_addy         = ''.
  ls_ass-op_main         = 'EQUAL'.
  APPEND ls_ass TO ls_restrict-ass_tab.
*** To apply restrictions to select option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = ls_restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.

  IF sy-subrc NE 0.
    WRITE:'error'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_screen_adjustments
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_screen_adjustments .
  LOOP AT SCREEN.
    IF sy-tcode EQ 'ZHR_EMP_PROP'.
      IF screen-group1 = 'BL4'.
        screen-input = 0.
        screen-invisible = '1'.
      ENDIF.

******Radio button 1 *****************
      IF p_rad2 IS INITIAL.
        IF screen-name = 'S_TEMPNO-LOW'.
          screen-input = 1.
          screen-value_help = 0.
        ENDIF.
      ENDIF.

      IF p_rad5 IS NOT INITIAL.
        IF screen-group1 = 'BL3'.
          screen-input = '1'.           "<----to enable display mode
        ENDIF.
      ELSE.
        IF screen-group1 = 'BL3'.
          screen-input = '1'.           "<----to enable display mode
          screen-invisible = '1'.
          screen-input = '0'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF sy-tcode EQ 'ZHR_EMP_APPR'.

      IF screen-group1 = 'BL1'.
        screen-input = 0.
        screen-invisible = '1'.
      ENDIF.

      IF p_rad3 IS NOT INITIAL.
        IF screen-group1 = 'BL2'.
          screen-input = '1'.           "<----to enable display mode
        ENDIF.
      ENDIF.

      IF p_rad4 IS INITIAL.
        IF screen-group1 = 'BL3'.
          screen-input = 0.
          screen-invisible = '1'.       "<----to enable display mode
        ENDIF.
      ENDIF.

      IF p_rad4 IS NOT INITIAL.
        IF screen-group1 = 'BL3'.
          screen-input = 1.
          screen-invisible = '0'.       "<----to enable display mode
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_initial_selections
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_initial_selections .
  REFRESH: lt_lov,gt_employee,gt_emp_details,gt_personal,gt_address,
           gt_id_details,gt_display,gt_display1,gt_alv1,lt_hiring,
           gt_position1,gt_mar_stat,gt_bank.

  SELECT * FROM zhr_greythr_lov
  INTO TABLE lt_lov.

  REFRESH lt_state.
  SELECT land1
         bland
         bezei FROM t005u
               INTO TABLE lt_state
               WHERE land1 = 'IN'
                 AND spras EQ sy-langu.

  REFRESH: gt_reason,gt_persk,gt_persg,gt_btrtl,gt_persa,gt_position.

  SELECT * FROM t530t
           INTO TABLE gt_reason
           WHERE sprsl EQ sy-langu
           AND massn EQ '01'.

  SELECT persg
         persk FROM t503z
         INTO TABLE gt_persk1
         WHERE persg = 'C'
         AND molga = '40'.

  IF gt_persk1[] IS NOT INITIAL.
    SELECT * FROM t503t
             INTO TABLE gt_persk
             FOR ALL ENTRIES IN gt_persk1
             WHERE persk = gt_persk1-persk
             AND sprsl EQ sy-langu.

  ENDIF.

  SELECT * FROM t501t
           INTO TABLE gt_persg
           WHERE sprsl EQ sy-langu.


  SELECT persa name1 bukrs
    FROM t500p
    INTO TABLE gt_werks
   WHERE molga = '40'
     AND ( bukrs NE 'IN01' AND bukrs NE '0001' ).


  SELECT werks
         btrtl
         btext
         molga FROM t001p
               INTO TABLE gt_btrtl
               WHERE molga EQ '40'.

  SELECT persa
         name1
         molga FROM t500p
               INTO TABLE gt_persa
               WHERE molga EQ '40'.

  SELECT * FROM hrp1000
           INTO TABLE gt_position
           WHERE otype EQ 'S'.

  SELECT * FROM t502t
         INTO TABLE gt_mar_stat
         WHERE sprsl EQ sy-langu.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_orgdetail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY2_PLANS
*&      <-- GS_DISPLAY2_ORGUNITID
*&      <-- GS_DISPLAY2_ORGUNITNAME
*&---------------------------------------------------------------------*
FORM f_get_orgdetail  USING    p_plans TYPE numc08
                      CHANGING p_orgunitid TYPE num08
                               p_orgunitname TYPE char40.

  DATA: lv_sobid TYPE hrp1001-sobid,
        lv_objid TYPE hrp1001-objid.

  IF p_plans IS NOT INITIAL.
    SELECT SINGLE sobid
      FROM hrp1001
      INTO lv_sobid
     WHERE otype = 'S'
       AND objid = p_plans
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '003'
       AND begda LE sy-datum
       AND endda GE sy-datum
       AND sclas = 'O'.
    IF sy-subrc = 0.
      p_orgunitid = lv_objid = lv_sobid+0(8).
      SELECT SINGLE stext
        FROM hrp1000
        INTO p_orgunitname
       WHERE otype = 'O'
         AND objid = lv_objid
         AND begda LE sy-datum
         AND endda GE sy-datum.
    ENDIF.
  ENDIF.


ENDFORM.
