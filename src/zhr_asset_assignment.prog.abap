*&---------------------------------------------------------------------*
*& Report ZHR_ASSET_ASSIGNMENT

* Date Created - 29.03.2022
*& Created By - KPABAP (Shamsudeen)
*& Description - Program for the asset assignment
*                for employee maintainence
*****
*& TR NO: DEVK931551
*&---------------------------------------------------------------------*
REPORT zhr_asset_assignment.

TABLES: zhr_asset_master.

**************DATA DECLARATION***********
DATA: filename TYPE rlgrap-filename.

TYPES: BEGIN OF lty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF lty_fields,
       BEGIN OF lty_disp, "ACTUAL ALV OUTPUT STRUCTURE"
         box(10)         TYPE c,
         asset_type      TYPE zasset,
         asset_type_des  TYPE zasset_des,
         comp_code       TYPE bukrs,
         plant           TYPE werks_d,
         asset_id        TYPE anln1,
         serial_no       TYPE zserial_no,
         asset_tag       TYPE zasset_tag,
         asset_des       TYPE zasset_des,
         product_no      TYPE zprod_no,
         asset_st        TYPE zhr_status,
         asset_st_des    TYPE zhr_st_des,
         asset_date      TYPE zasset_date,
         asset_remarks   TYPE zasset_rem,
         location        TYPE zlocation,
         department      TYPE zdepart,
         model           TYPE zmodel,
         costcenter      TYPE kostl,
         operating_sys   TYPE zop_sys,
         configuration   TYPE zconfig,
         warranty        TYPE zwarranty,
         warranty_des(3) TYPE c,
         warranty_end    TYPE zend_date,
         amc             TYPE zamc,
         amc_des(3)      TYPE c,
         amc_company     TYPE zamc_comp,
         amc_contact     TYPE zamc_contact,
         invoice_no      TYPE zinvoice_no,
         invoice_date    TYPE zinv_date,
         po_number       TYPE ebeln,
         vendor_id       TYPE lifnr,
         vendor_name     TYPE name1_gp,
         non_emp_assign  TYPE znon_emp_ass,
         non_emp_des(3)  TYPE c,
         assign_to_name  TYPE zassign_name,
         assign_cont_no  TYPE zassign_con,
         assigned_email  TYPE zassign_email,
         accessory_1     TYPE zaccess_1,
         accessory_1_no  TYPE zaccess_no1,
         accessory_2     TYPE zaccess_2,
         accessory_2_no  TYPE zaccess_no2,
         accessory_3     TYPE zaccess_3,
         accessory_3_no  TYPE zaccess_no3,
         accessory_4     TYPE zaccess_4,
         accessory_4_no  TYPE zaccess_no4,
         accessory_5     TYPE zaccess_5,
         accessory_5_no  TYPE zaccess_no5,
         wifi_mac_id     TYPE zwifi_mac,
         ip_address      TYPE zip_addr,
       END OF lty_disp,
       BEGIN OF ty_asset_id,
         asset_id TYPE zhr_asset_master-asset_id,
       END OF ty_asset_id.
TYPES: BEGIN OF ty_anla,
         anln1 TYPE anln1,
       END OF ty_anla,
       BEGIN OF ty_csks,
         kostl TYPE kostl,
       END OF ty_csks.
DATA: gt_anla TYPE TABLE OF ty_anla,
      gs_anla TYPE ty_anla,
      gt_csks TYPE TABLE OF ty_csks,
      gs_csks TYPE ty_csks.

DATA: lo_grid      TYPE REF TO cl_gui_alv_grid.

DATA: v_flag TYPE i.
DATA: g_flag TYPE i.

DATA: lt_asset       TYPE STANDARD TABLE OF zhr_asset_master,
      ls_asset       TYPE zhr_asset_master,
      lt_fields      TYPE STANDARD TABLE OF lty_fields,
      ls_fields      TYPE lty_fields,
      lt_disp        TYPE STANDARD TABLE OF lty_disp,
      ls_disp        TYPE lty_disp,
      lt_fcat        TYPE lvc_t_fcat,
      ls_fcat        TYPE lvc_s_fcat,
      ls_grid        TYPE lvc_s_glay,
      lt_exclude     TYPE ui_functions,
      ls_excl        TYPE ui_func,
      ls_layout      TYPE lvc_s_layo,
      lt_asset_id    TYPE TABLE OF ty_asset_id,
      lt_header      TYPE slis_t_listheader,
      ls_header      TYPE slis_listheader,
      today_date(10),
      today_time(10).


DATA: asset_type_des(20) TYPE c,
      asset_st_des(6)    TYPE c,
      warranty_des(6)    TYPE c,
      amc_des(6)         TYPE c,
      non_emp_des(6)     TYPE c.

DATA: gt_asset_type TYPE STANDARD TABLE OF zhr_asset_type,
      gs_asset_type TYPE zhr_asset_type.

DATA: gt_status TYPE STANDARD TABLE OF zhr_asset_status,
      gs_status TYPE zhr_asset_status.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TBSTR'
CONSTANTS: BEGIN OF c_tbstr,
             tab1 LIKE sy-ucomm VALUE 'TBSTR_FC1',
             tab2 LIKE sy-ucomm VALUE 'TBSTR_FC2',
             tab3 LIKE sy-ucomm VALUE 'TBSTR_FC3',
             tab4 LIKE sy-ucomm VALUE 'TBSTR_FC4',
             tab5 LIKE sy-ucomm VALUE 'TBSTR_FC5',
           END OF c_tbstr.
*&SPWIZARD: DATA FOR TABSTRIP 'TBSTR'
CONTROLS:  tbstr TYPE TABSTRIP.
DATA: BEGIN OF g_tbstr,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZHR_ASSET_ASSIGNMENT',
        pressed_tab LIKE sy-ucomm VALUE c_tbstr-tab1,
      END OF g_tbstr.
DATA:      ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TBSTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tbstr_active_tab_set OUTPUT.
  tbstr-activetab = g_tbstr-pressed_tab.
  CASE g_tbstr-pressed_tab.
    WHEN c_tbstr-tab1.
      g_tbstr-subscreen = '0801'.
    WHEN c_tbstr-tab2.
      g_tbstr-subscreen = '0802'.
    WHEN c_tbstr-tab3.
      g_tbstr-subscreen = '0803'.
    WHEN c_tbstr-tab4.
      g_tbstr-subscreen = '0804'.
    WHEN c_tbstr-tab5.
      g_tbstr-subscreen = '0805'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TBSTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tbstr_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tbstr-tab1.
      g_tbstr-pressed_tab = c_tbstr-tab1.
    WHEN c_tbstr-tab2.
      g_tbstr-pressed_tab = c_tbstr-tab2.
    WHEN c_tbstr-tab3.
      g_tbstr-pressed_tab = c_tbstr-tab3.
    WHEN c_tbstr-tab4.
      g_tbstr-pressed_tab = c_tbstr-tab4.
    WHEN c_tbstr-tab5.
      g_tbstr-pressed_tab = c_tbstr-tab5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TBSTR1'
CONSTANTS: BEGIN OF c_tbstr1,
             tab1 LIKE sy-ucomm VALUE 'TBSTR1_FC1',
             tab2 LIKE sy-ucomm VALUE 'TBSTR1_FC2',
             tab3 LIKE sy-ucomm VALUE 'TBSTR1_FC3',
             tab4 LIKE sy-ucomm VALUE 'TBSTR1_FC4',
             tab5 LIKE sy-ucomm VALUE 'TBSTR1_FC5',
           END OF c_tbstr1.
*&SPWIZARD: DATA FOR TABSTRIP 'TBSTR1'
CONTROLS:  tbstr1 TYPE TABSTRIP.
DATA: BEGIN OF g_tbstr1,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZHR_ASSET_ASSIGNMENT',
        pressed_tab LIKE sy-ucomm VALUE c_tbstr1-tab1,
      END OF g_tbstr1.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TBSTR1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tbstr1_active_tab_set OUTPUT.
  tbstr1-activetab = g_tbstr1-pressed_tab.
  CASE g_tbstr1-pressed_tab.
    WHEN c_tbstr1-tab1.
      g_tbstr1-subscreen = '0901'.
    WHEN c_tbstr1-tab2.
      g_tbstr1-subscreen = '0902'.
    WHEN c_tbstr1-tab3.
      g_tbstr1-subscreen = '0903'.
    WHEN c_tbstr1-tab4.
      g_tbstr1-subscreen = '0904'.
    WHEN c_tbstr1-tab5.
      g_tbstr1-subscreen = '0905'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TBSTR1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tbstr1_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tbstr1-tab1.
      g_tbstr1-pressed_tab = c_tbstr1-tab1.
    WHEN c_tbstr1-tab2.
      g_tbstr1-pressed_tab = c_tbstr1-tab2.
    WHEN c_tbstr1-tab3.
      g_tbstr1-pressed_tab = c_tbstr1-tab3.
    WHEN c_tbstr1-tab4.
      g_tbstr1-pressed_tab = c_tbstr1-tab4.
    WHEN c_tbstr1-tab5.
      g_tbstr1-pressed_tab = c_tbstr1-tab5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

************************SELECTION-SCREEN PARAMETER*******************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR zhr_asset_master-comp_code,
                  s_werks FOR zhr_asset_master-plant,
                  s_as1 FOR zhr_asset_master-asset_type,
                  s_as2 FOR zhr_asset_master-asset_id,
                  s_as3 FOR zhr_asset_master-asset_tag,
                  s_as4 FOR zhr_asset_master-asset_st,
                  warranty FOR zhr_asset_master-warranty.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN: FUNCTION KEY 1.


** AT SELECTION SCREEN ACTION****************
AT SELECTION-SCREEN.
  CASE sy-ucomm.

********MODAL DIALOG SCREEN FOR CREATE ASSET**************
    WHEN 'CREA'.
      CALL SCREEN 200 STARTING AT 6  10
                      ENDING AT 70  20.


  ENDCASE.

*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR ASSET_ID.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_as2-low.
  SELECT anln1 FROM anla INTO TABLE lt_asset_id.
  SORT lt_asset_id BY asset_id.
  IF sy-subrc = 0.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ASSET_ID'
        dynpprog    = 'ZHR_ASSET_ASSIGNMENT'
        dynpnr      = sy-dynnr
        dynprofield = 'S_AS2'
        value_org   = 'S'
      TABLES
        value_tab   = lt_asset_id.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_as2-high.
  SELECT anln1 FROM anla INTO TABLE lt_asset_id.
  SORT lt_asset_id BY asset_id.
  IF sy-subrc = 0.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ASSET_ID'
        dynpprog    = 'ZHR_ASSET_ASSIGNMENT'
        dynpnr      = sy-dynnr
        dynprofield = 'S_AS2'
        value_org   = 'S'
      TABLES
        value_tab   = lt_asset_id.
  ENDIF.
*******CREATE ASSET BUTTON IN SELECTION SCREEN*********
AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZPF_STATUS2'.

START-OF-SELECTION.

  SELECT * FROM zhr_asset_type
           INTO TABLE gt_asset_type.

  SELECT * FROM zhr_asset_status
           INTO TABLE gt_status.

  SELECT anln1 FROM anla
               INTO TABLE gt_anla.

  SELECT kostl FROM csks
               INTO TABLE gt_csks.

*FETCHING ALL DATA FROM TABLE BASED ON ASSET_ID AND ASSET_TYPE*
  SELECT * FROM zhr_asset_master
           INTO TABLE lt_asset
           WHERE comp_code IN s_bukrs
           AND plant IN s_werks
           AND asset_id IN s_as2
           AND asset_type IN s_as1
           AND asset_st IN s_as4.

  MOVE-CORRESPONDING lt_asset TO lt_disp.

  CLEAR ls_disp.
  LOOP AT lt_disp INTO ls_disp.

    CLEAR gs_asset_type.
    READ TABLE gt_asset_type INTO gs_asset_type WITH KEY asset_type = ls_disp-asset_type.
    IF sy-subrc EQ 0.
      ls_disp-asset_type_des = gs_asset_type-asset_des.
    ENDIF.

* ASSET_STATUS  (DESCRIPTION)***********
    CLEAR gs_status.
    READ TABLE gt_status INTO gs_status WITH KEY asset_st = ls_disp-asset_st.
    IF sy-subrc EQ 0.
      ls_disp-asset_st_des = gs_status-asset_st_des.
    ENDIF.



*****WARRANTY  (DESCRIPTION)***********
    IF ls_disp-warranty = '01'.
      ls_disp-warranty_des = 'YES'.
    ELSEIF ls_disp-warranty = '02'.
      ls_disp-warranty_des = 'NO'.
    ENDIF.
******** AMC  (DESCRIPTION)***********
    IF ls_disp-amc = '01'.
      ls_disp-amc_des = 'YES'.
    ELSEIF ls_disp-amc = '02'.
      ls_disp-amc_des = 'NO'.
    ENDIF.
******** NON_EMP_ASSIGN (DESCRIPTION)***********
    IF ls_disp-non_emp_assign = '01'.
      ls_disp-non_emp_des = 'YES'.
    ELSEIF ls_disp-non_emp_assign = '02'.
      ls_disp-non_emp_des = 'NO'.
    ENDIF.

    MODIFY lt_disp FROM ls_disp.

  ENDLOOP.

END-OF-SELECTION.

  REFRESH lt_fcat.
  ls_layout-cwidth_opt = 'X'.
*********** ALV FIELDCATALOG FOR ALV DISPLAY******************************
  PERFORM f_fieldcat USING  'BOX'            'BOX'                     1 'X' 'X' space.
  PERFORM f_fieldcat USING  'ASSET_TYPE'     'Asset_type'              2 space space space. "space.
  PERFORM f_fieldcat USING  'ASSET_TYPE_DES' 'Asset Type Desc'         3 space space space. "space.
  PERFORM f_fieldcat USING  'COMP_CODE'      'Company Code'            4 space space space. "space.
  PERFORM f_fieldcat USING  'PLANT'          'Plant'                   5 space space space. "space.
  PERFORM f_fieldcat USING  'ASSET_ID'       'Main_asset_number'       6 space space space. "space
  PERFORM f_fieldcat USING  'SERIAL_NO'      'Serial Number'           7 space space space ."space.
  PERFORM f_fieldcat USING  'ASSET_TAG'      'Asset Tag'               8 space space space. "space.
  PERFORM f_fieldcat USING  'ASSET_DES'      'Asset type description'  9 space space space. "space.
  PERFORM f_fieldcat USING  'PRODUCT_NO'     'Product Number'         10 space space space. "space.
  PERFORM f_fieldcat USING  'ASSET_ST'       'Asset status'           11 space space space. "space.
  PERFORM f_fieldcat USING  'ASSET_ST_DES'   'Asset status Desc'      12 space space space. "space
  PERFORM f_fieldcat USING  'ASSET_DATE'     'Asset date'             13 space space space. "space
  PERFORM f_fieldcat USING  'ASSET_REMARKS'  'Asset Remarks'          14 space space space. "space
  PERFORM f_fieldcat USING  'LOCATION'       'Location'               15 space space space. "space
  PERFORM f_fieldcat USING  'DEPARTMENT'     'Department'             16 space space space. "space
  PERFORM f_fieldcat USING  'MODEL'          'Model'                  17 space space space. "space.
  PERFORM f_fieldcat USING  'COSTCENTER'     'Cost Center'            18 space space space."space
  PERFORM f_fieldcat USING  'OPERATING_SYS'  'Operating System'       19 space space space. "space
  PERFORM f_fieldcat USING  'CONFIGURATION'  'Configuration'          20 space space space. "space
  PERFORM f_fieldcat USING  'WARRANTY'       'Warranty'               21 space space space. "space
  PERFORM f_fieldcat USING  'WARRANTY_DES'   'Warranty_Des'           22 space space space. "space
  PERFORM f_fieldcat USING  'WARRANTY_END'   'Warranty End Date'      23 space space space. "space
  PERFORM f_fieldcat USING  'AMC'            'Amc'                    24 space space space. "space
  PERFORM f_fieldcat USING  'AMC_DES'        'Amc Des'                25 space space space. "space
  PERFORM f_fieldcat USING  'AMC_COMPANY'    'AMC Company'            26 space space space. "space
  PERFORM f_fieldcat USING  'AMC_CONTACT'    'AMC Contact'            27 space space space. "space
  PERFORM f_fieldcat USING  'INVOICE_NO'     'Invoice Number'         28 space space space. "space
  PERFORM f_fieldcat USING  'INVOICE_DATE'   'Invoice Date'           29 space space space. "space
  PERFORM f_fieldcat USING  'PO_NUMBER'      'Purchasing Doc Number'  30 space space space. "space
  PERFORM f_fieldcat USING  'VENDOR_ID'      'Account Number Vendor'  31 space space space. "space
  PERFORM f_fieldcat USING  'VENDOR_NAME'    'Name 1'                 32 space space space. "space"
  PERFORM f_fieldcat USING  'NON_EMP_ASSIGN' 'Non emp assignment'     33 space space space. "space
  PERFORM f_fieldcat USING  'NON_EMP_DES'    'Non employee Desc'      34 space space space. "space
  PERFORM f_fieldcat USING  'ASSIGN_TO_NAME' 'Assigned to Name'       35 space space space. "space
  PERFORM f_fieldcat USING  'ASSIGN_CONT_NO' 'Assign Contact number'  36 space space space. "space
  PERFORM f_fieldcat USING  'ASSIGNED_EMAIL' 'Assigned EMAIL'         37 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_1'    'Accessory 1'            38 space space space. "space.
  PERFORM f_fieldcat USING  'ACCESSORY_1_NO' 'Accessory No1'          39 space space space. "space.
  PERFORM f_fieldcat USING  'ACCESSORY_2'    'Accessory 2'            40 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_2_NO' 'Accessory No2'          41 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_3'    'Accessory 3'            42 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_3_NO' 'Accessory No3'          43 space space space. "space.
  PERFORM f_fieldcat USING  'ACCESSORY_4'    'Accessory 4'            44 space space space. "space.
  PERFORM f_fieldcat USING  'ACCESSORY_4_NO' 'Accessory No4'          45 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_5'    'Accessory 5'            46 space space space. "space
  PERFORM f_fieldcat USING  'ACCESSORY_5_NO' 'Accessory No5'          47 space space space. "space
  PERFORM f_fieldcat USING  'WIFI_MAC_ID'    'WIFI Mac ID'            48 space space space. "space
  PERFORM f_fieldcat USING  'IP_ADDRESS'     'IP Address'             49 space space space. "space

  IF lt_disp[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_default                = 'X'
        is_layout_lvc            = ls_layout
        it_fieldcat_lvc          = lt_fcat[]
      TABLES
        t_outtab                 = lt_disp[].
  ENDIF.

FORM top_of_page.
  REFRESH lt_header.
  ls_header-typ = 'H'.
  ls_header-info = 'ASSET ASSIGNMENT DETAILS'.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  WRITE sy-datum TO today_date.
  ls_header-typ = 'S'.
  ls_header-key = 'REPORT RUN DATE'.
  ls_header-info = today_date.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  WRITE sy-uzeit TO today_time.
  ls_header-typ = 'S'.
  ls_header-key = 'REPORT RUN TIME'.
  ls_header-info = today_time.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.


FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 f_var5 f_var6 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  ls_fcat-checkbox = f_var5.
  ls_fcat-key = f_var6.
*  ls_fcat-hotspot = f_var7.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.

FORM pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZPF'.
ENDFORM.
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&F03' OR '&F12'.
      SET SCREEN 0.
    WHEN '&IC1'.
      CLEAR ls_disp.
      READ TABLE lt_disp INTO ls_disp INDEX rs_selfield-tabindex.
      IF sy-subrc EQ 0.
        SET PARAMETER ID 'AN1' FIELD ls_disp-asset_id.
        SELECT SINGLE * FROM zhr_asset_master
                                 INTO CORRESPONDING FIELDS OF zhr_asset_master
                                 WHERE asset_id = ls_disp-asset_id.
        CALL SCREEN 700.
      ENDIF.

    WHEN 'MOD'.
      DATA var TYPE sy-tabix.
      DATA: lo_grid TYPE REF TO cl_gui_alv_grid.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_grid.
      CALL METHOD lo_grid->check_changed_data.

      READ TABLE lt_disp INTO ls_disp INDEX rs_selfield-tabindex.
      IF r_ucomm = 'MOD'.
        LOOP AT lt_disp INTO ls_disp WHERE box = 'X'.
          var = var + 1.
          IF var > 1.
            MESSAGE 'Select single check box' TYPE 'E'.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF ls_disp-box NE 'X'.
        MESSAGE 'click atleast one checkbox' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        IF ls_disp-asset_id IS NOT INITIAL.
          SET PARAMETER ID 'AN1' FIELD ls_disp-asset_id.
          SELECT SINGLE * FROM zhr_asset_master
                                 INTO CORRESPONDING FIELDS OF zhr_asset_master
                                 WHERE asset_id = ls_disp-asset_id.
          CALL SCREEN 900.
        ENDIF.
      ENDIF.

    WHEN 'AS1'.
      DATA: lo_grid1 TYPE REF TO cl_gui_alv_grid.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_grid1.
      CALL METHOD lo_grid1->check_changed_data.
      READ TABLE lt_disp INTO ls_disp INDEX rs_selfield-tabindex.
      IF ls_disp-box NE 'X'.
        MESSAGE 'click atleast one checkbox' TYPE 'I'.
      ELSEIF ls_disp-box EQ 'X'.
        CALL SCREEN 300 STARTING AT 5  10
                       ENDING AT 50  20.
      ENDIF.

    WHEN 'AS2'.
      DATA: lo_grid2 TYPE REF TO cl_gui_alv_grid.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_grid2.
      CALL METHOD lo_grid2->check_changed_data.
      READ TABLE lt_disp INTO ls_disp INDEX rs_selfield-tabindex.
      IF ls_disp-box NE 'X'.
        MESSAGE 'click atleast one checkbox' TYPE 'I'.
      ELSEIF ls_disp-box EQ 'X'.
        CALL SCREEN 400 STARTING AT 5  10
                       ENDING AT 50  20.
      ENDIF.


  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*** OUTPUT SCREEN OF MODAL DIALOG SCREEN***********************
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF_STATUS1'.
  SET TITLEBAR 'ZMOD'.
  DATA: radio1,
        radio2.
  IF v_flag EQ 0.
    PERFORM invisible.
  ELSEIF v_flag EQ 1.
    PERFORM visible.
  ELSEIF v_flag EQ 2.
    PERFORM invisible.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
******USER COMMAND FOR MODAL DIALOG SCREEN ***************
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'CH1'.
      IF radio2 = 'X'.
        IF sy-subrc EQ 0.
          v_flag = 1.
        ENDIF.
      ELSEIF radio1 = 'X'.
        IF sy-subrc EQ 0.
          v_flag = 2.
        ENDIF.
      ENDIF.
    WHEN 'UC1'.
      IF radio1 = 'X' AND g_flag EQ 0.
        CLEAR zhr_asset_master.
        CALL SCREEN 800.
      ENDIF.
    WHEN 'UC2'.
      IF radio2 = 'X'.
****** SUBROUTINE FOR EXCEL UPLOAD**************************************
        PERFORM data_upload.
      ENDIF.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*** INVISIBLE FIELD IN RADIO BUTTON *********
FORM invisible .
  LOOP AT SCREEN.
    IF screen-name = 'FILENAME'.
      screen-invisible = '1'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
**** VISIBLE FIELD IN RADIO BUTTON************
FORM visible.
  LOOP AT SCREEN.
    IF screen-name = 'FILENAME'.
      screen-invisible = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
** INPUT HELP FOR DIALOG BOX FIELD FILENAME***********
MODULE f4_bu INPUT.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'FILENAME '
    IMPORTING
      file_name     = filename.

ENDMODULE.

MODULE status_0800 OUTPUT.
  SET PF-STATUS 'ZPF_1'.
  SET TITLEBAR 'ZPF1'.
ENDMODULE.

**************CREATE SCREEN LOGIC*****************
MODULE user_command_0800 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR'CANC'.
      SET SCREEN 0.
**********SAVE Button in 'CREATE SCREEN' logic*********************
    WHEN 'SAVE'.

      CLEAR gs_anla.
      READ TABLE gt_anla INTO gs_anla WITH KEY anln1 = zhr_asset_master-asset_id.
      IF sy-subrc NE 0 AND gs_anla-anln1 NE zhr_asset_master-asset_id.
        MESSAGE 'Asset_id not Found' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

      CLEAR gs_csks.
      READ TABLE gt_csks INTO gs_csks WITH KEY kostl = zhr_asset_master-costcenter.
      IF sy-subrc NE 0 AND gs_csks-kostl NE zhr_asset_master-costcenter.
        MESSAGE 'Costcenter is incorrect' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

***************Warranty Date Error message when warranty is 'YES'******************************
      IF zhr_asset_master-warranty EQ '01' AND zhr_asset_master-warranty_end IS INITIAL.
        MESSAGE 'Warranty_Date is Required' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.
***********Everything is correct it modify Table**********************
      IF zhr_asset_master-asset_type IS NOT INITIAL
         AND zhr_asset_master-asset_id IS NOT INITIAL
         AND zhr_asset_master-serial_no IS NOT INITIAL
         AND gs_anla-anln1 EQ zhr_asset_master-asset_id
         AND gs_csks-kostl EQ zhr_asset_master-costcenter.

        MODIFY zhr_asset_master FROM zhr_asset_master.
        COMMIT WORK AND WAIT.
        IF sy-subrc = 0.
          MESSAGE 'Asset Created Successfully' TYPE 'S'.
        ENDIF.
********Error mesage for fill required fields in create screen********************************
      ELSE.
        MESSAGE 'Fill In All Required Fields' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.


**********GET DETAILS Button in 'CREATE SCREEN' logic*********************
    WHEN 'PC3'.
      IF zhr_asset_master-asset_id IS NOT INITIAL.
        SELECT SINGLE txa50 FROM anla
                            INTO zhr_asset_master-asset_des
                            WHERE anln1 = zhr_asset_master-asset_id.
        SELECT SINGLE zugdt FROM anla
                            INTO zhr_asset_master-asset_date
                            WHERE anln1 = zhr_asset_master-asset_id.
        SELECT SINGLE kostl FROM anlz
                            INTO zhr_asset_master-costcenter
                            WHERE anln1 = zhr_asset_master-asset_id.
        SELECT SINGLE lifnr FROM anla
                            INTO zhr_asset_master-vendor_id
                            WHERE anln1 = zhr_asset_master-asset_id.
        IF zhr_asset_master-vendor_id IS NOT INITIAL .
          SELECT SINGLE name1 FROM lfa1
                              INTO zhr_asset_master-vendor_name
                              WHERE lifnr = zhr_asset_master-vendor_id.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0900 OUTPUT

MODULE status_0900 OUTPUT.
  SET PF-STATUS 'ZPF_STATUS'.
*  SET TITLEBAR 'ZMODIFY'.
ENDMODULE.
*&---------------------------------------------------------------------*

MODULE user_command_0900 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      SET SCREEN 0.
**********SAVE Button in 'MODIFY SCREEN' logic*********************
    WHEN 'SAVE'.

      READ TABLE gt_csks INTO gs_csks WITH KEY kostl = zhr_asset_master-costcenter.
      IF sy-subrc NE 0.
        MESSAGE 'Costcenter not Found' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.


      IF gs_csks-kostl EQ zhr_asset_master-costcenter.
        MODIFY zhr_asset_master FROM zhr_asset_master.
        COMMIT WORK AND WAIT.
        IF sy-subrc = 0.
          MESSAGE 'Exisisting Asset_id updated Successfully' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'Fill in Correct details' TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.
ENDMODULE.
*******************************EXCEL UPLOAD PROGRAM ***************************
FORM data_upload .
  TYPES: BEGIN OF ty_asset,
           asset_type        TYPE zasset,
           comp_code         TYPE bukrs,
           plant             TYPE werks_d,
           asset_id          TYPE anln1,
           serial_no         TYPE zserial_no,
           asset_tag         TYPE zasset_tag,
           asset_des         TYPE zasset_des,
           product_no        TYPE zprod_no,
           asset_st(2)       TYPE n,
           asset_date(8)     TYPE n,
           asset_remarks     TYPE zasset_rem,
           location          TYPE zlocation,
           department        TYPE zdepart,
           model             TYPE zmodel,
           costcenter        TYPE kostl,
           operating_sys     TYPE zop_sys,
           configuration     TYPE zconfig,
           warranty          TYPE zwarranty,
           warranty_end(8)   TYPE n,
           amc(2)            TYPE n,
           amc_company       TYPE zamc_comp,
           amc_contact       TYPE zamc_contact,
           invoice_no        TYPE zinvoice_no,
           invoice_date(8)   TYPE n,
           po_number         TYPE ebeln,
           vendor_id         TYPE lifnr,
           vendor_name       TYPE name1_gp,
           non_emp_assign(2) TYPE n,
           assign_to_name    TYPE zassign_name,
           assign_cont_no    TYPE zassign_con,
           assigned_email    TYPE zassign_email,
           accessory_1       TYPE zaccess_1,
           accessory_1_no    TYPE zaccess_no1,
           accessory_2       TYPE zaccess_2,
           accessory_2_no    TYPE zaccess_no2,
           accessory_3       TYPE zaccess_3,
           accessory_3_no    TYPE zaccess_no3,
           accessory_4       TYPE zaccess_4,
           accessory_4_no    TYPE zaccess_no4,
           accessory_5       TYPE zaccess_5,
           accessory_5_no    TYPE zaccess_no5,
           wifi_mac_id       TYPE zwifi_mac,
           ip_address        TYPE zip_addr,
         END OF ty_asset,
         BEGIN OF ty_comp,
           bukrs TYPE bukrs,
           butxt TYPE butxt,
         END OF ty_comp,
         BEGIN OF ty_plant,
           werks TYPE werks_d,
           name1 TYPE name1_gp,
         END OF ty_plant,
         BEGIN OF lty_fields,
           fieldname TYPE fieldname,
           rollname  TYPE rollname,
           position  TYPE tabfdpos,
           datatype  TYPE datatype_d,
           leng      TYPE ddleng,
         END OF lty_fields,
         BEGIN OF lty_disp,
           asset_type          TYPE zasset,
           asset_type_desc(20) TYPE c,
           comp_code           TYPE bukrs,
           compcode_des        TYPE butxt,
           plant               TYPE werks_d,
           plant_des           TYPE name1_gp,
           asset_id            TYPE anln1,
           serial_no           TYPE zserial_no,
           asset_tag           TYPE zasset_tag,
           asset_des           TYPE zasset_des,
           product_no          TYPE zprod_no,
           asset_st            TYPE zhr_status,
           asset_st_desc(40)   TYPE c,
           asset_date          TYPE zasset_date,
           asset_remarks       TYPE zasset_rem,
           location            TYPE zlocation,
           department          TYPE zdepart,
           model               TYPE zmodel,
           costcenter          TYPE kostl,
           operating_sys       TYPE zop_sys,
           configuration       TYPE zconfig,
           warranty            TYPE zwarranty,
           warranty_desc(6)    TYPE c,
           warranty_end        TYPE zend_date,
           amc                 TYPE zamc,
           amc_desc(6)         TYPE c,
           amc_company         TYPE zamc_comp,
           amc_contact         TYPE zamc_contact,
           invoice_no          TYPE zinvoice_no,
           invoice_date        TYPE zinv_date,
           po_number           TYPE ebeln,
           vendor_id           TYPE lifnr,
           vendor_name         TYPE name1_gp,
           non_emp_assign      TYPE znon_emp_ass,
           non_emp_desc(6)     TYPE c,
           assign_to_name      TYPE zassign_name,
           assign_cont_no      TYPE zassign_con,
           assigned_email      TYPE zassign_email,
           accessory_1         TYPE zaccess_1,
           accessory_1_no      TYPE zaccess_no1,
           accessory_2         TYPE zaccess_2,
           accessory_2_no      TYPE zaccess_no2,
           accessory_3         TYPE zaccess_3,
           accessory_3_no      TYPE zaccess_no3,
           accessory_4         TYPE zaccess_4,
           accessory_4_no      TYPE zaccess_no4,
           accessory_5         TYPE zaccess_5,
           accessory_5_no      TYPE zaccess_no5,
           wifi_mac_id         TYPE zwifi_mac,
           ip_address          TYPE zip_addr,
           message(100)        TYPE c,
         END OF lty_disp.


  DATA : lt_type      TYPE truxs_t_text_data,
         lt_excel     TYPE TABLE OF ty_asset,
         ls_excel     TYPE ty_asset,
         lt_excel_tmp TYPE TABLE OF ty_asset,
         ls_excel_tmp TYPE ty_asset,
         lt_asset_typ TYPE STANDARD TABLE OF zhr_asset_type,
         ls_asset_typ TYPE zhr_asset_type,
         lt_asset     TYPE STANDARD TABLE OF zhr_asset_master,
         ls_asset     TYPE zhr_asset_master,
         lt_disp      TYPE TABLE OF lty_disp,
         ls_disp      TYPE lty_disp,
         lt_fields    TYPE STANDARD TABLE OF lty_fields,
         ls_fields    TYPE lty_fields,
         lt_fcat      TYPE lvc_t_fcat,
         ls_fcat      TYPE lvc_s_fcat,
         ls_layout    TYPE lvc_s_layo.

  DATA: lt_comp   TYPE TABLE OF ty_comp,
        ls_comp   TYPE ty_comp,
        lt_plant  TYPE TABLE OF ty_plant,
        ls_plant  TYPE ty_plant,
        lt_anla   TYPE TABLE OF ty_anla,
        ls_anla   TYPE ty_anla,
        lt_csks   TYPE TABLE OF ty_csks,
        ls_csks   TYPE ty_csks,
        lt_status TYPE STANDARD TABLE OF zhr_asset_status,
        ls_status TYPE zhr_asset_status.

  DATA: lo_grid TYPE REF TO cl_salv_table.

  REFRESH: lt_excel,lt_asset.
****Taking data from (ZHR_ASSET_Master) TO update and create new record*********
  SELECT * FROM zhr_asset_master
  INTO TABLE lt_asset.


  IF filename IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = lt_type
        i_filename           = filename
      TABLES
        i_tab_converted_data = lt_excel.
  ENDIF.

**** Company code plant Checks************
  SELECT bukrs
         butxt FROM t001
               INTO TABLE lt_comp.
  IF sy-subrc EQ 0.
    SORT lt_comp[] BY bukrs.
  ENDIF.

  SELECT werks
         name1 FROM t001w
               INTO TABLE lt_plant.
  IF sy-subrc EQ 0.
    SORT lt_plant[] BY werks.
  ENDIF.

***Asset Type check ******
  SELECT * FROM zhr_asset_type
           INTO TABLE lt_asset_typ.

**** Asset ID checks*********
  SELECT anln1 FROM anla
               INTO TABLE lt_anla.

*****Costcenter Checks *******
  SELECT kostl FROM csks
               INTO TABLE lt_csks.

** Asset Status Checks*****
  SELECT * FROM zhr_asset_status
           INTO TABLE lt_status.

***Asset
*Convert Customer No
  LOOP AT lt_excel INTO ls_excel.
    IF ls_excel-asset_id IS NOT INITIAL.
* to convert to KUNNR format to proper format
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-asset_id
        IMPORTING
          output = ls_excel-asset_id.

      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

**************IT HOLDS DESCRIPTION OF SOME ID IN ALV DISPLAY*********
  DATA: asset_type_desc(20) TYPE c,
        asset_st_desc(6)    TYPE c,
        warranty_desc(6)    TYPE c,
        amc_desc(6)         TYPE c,
        non_emp_desc(6)     TYPE c.

  DATA: lv_asset TYPE num10.

  LOOP AT lt_excel INTO ls_excel.
******Company code checks**************
    IF ls_excel-comp_code IS NOT INITIAL.
      READ TABLE lt_comp INTO ls_comp WITH KEY bukrs = ls_excel-comp_code.
      IF sy-subrc NE 0.
        ls_disp-asset_type = ls_excel-asset_type.
        ls_disp-comp_code  = ls_excel-comp_code.
        ls_disp-plant  = ls_excel-plant.
        ls_disp-asset_id  = ls_excel-asset_id.
        ls_disp-serial_no = ls_excel-serial_no.
        ls_disp-asset_tag  = ls_excel-asset_tag.
        ls_disp-asset_des  = ls_excel-asset_des.
        ls_disp-product_no = ls_excel-product_no.
        ls_disp-asset_st  = ls_excel-asset_st.
        ls_disp-asset_date = ls_excel-asset_date.
        ls_disp-asset_remarks = ls_excel-asset_remarks.
        ls_disp-location = ls_excel-location.
        ls_disp-department = ls_excel-department.
        ls_disp-model = ls_excel-model.
        ls_disp-costcenter = ls_excel-costcenter.
        ls_disp-operating_sys = ls_excel-operating_sys.
        ls_disp-configuration = ls_excel-configuration.
        ls_disp-warranty = ls_excel-warranty.
        ls_disp-warranty_end = ls_excel-warranty_end.
        ls_disp-amc = ls_excel-amc.
        ls_disp-amc_company = ls_excel-amc_company.
        ls_disp-amc_contact = ls_excel-amc_contact.
        ls_disp-invoice_no = ls_excel-invoice_no.
        ls_disp-invoice_date = ls_excel-invoice_date.
        ls_disp-po_number = ls_excel-po_number.
        ls_disp-vendor_id = ls_excel-vendor_id.
        ls_disp-vendor_name = ls_excel-vendor_name.
        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
        ls_disp-assign_to_name = ls_excel-assign_to_name.
        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
        ls_disp-assigned_email = ls_excel-assigned_email.
        ls_disp-accessory_1 = ls_excel-accessory_1.
        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
        ls_disp-accessory_2 = ls_excel-accessory_2.
        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
        ls_disp-accessory_3 = ls_excel-accessory_3.
        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
        ls_disp-accessory_4 = ls_excel-accessory_4.
        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
        ls_disp-accessory_5 = ls_excel-accessory_5.
        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
        ls_disp-ip_address = ls_excel-ip_address.
        ls_disp-message  = 'Incorrect Company Code'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.

******Plant checks from excel*********
    IF ls_excel-plant IS NOT INITIAL.
      READ TABLE lt_plant INTO ls_plant WITH KEY werks = ls_excel-plant.
      IF sy-subrc NE 0.
        ls_disp-asset_type = ls_excel-asset_type.
        ls_disp-comp_code  = ls_excel-comp_code.
        ls_disp-plant  = ls_excel-plant.
        ls_disp-asset_id  = ls_excel-asset_id.
        ls_disp-serial_no = ls_excel-serial_no.
        ls_disp-asset_tag  = ls_excel-asset_tag.
        ls_disp-asset_des  = ls_excel-asset_des.
        ls_disp-product_no = ls_excel-product_no.
        ls_disp-asset_st  = ls_excel-asset_st.
        ls_disp-asset_date = ls_excel-asset_date.
        ls_disp-asset_remarks = ls_excel-asset_remarks.
        ls_disp-location = ls_excel-location.
        ls_disp-department = ls_excel-department.
        ls_disp-model = ls_excel-model.
        ls_disp-costcenter = ls_excel-costcenter.
        ls_disp-operating_sys = ls_excel-operating_sys.
        ls_disp-configuration = ls_excel-configuration.
        ls_disp-warranty = ls_excel-warranty.
        ls_disp-warranty_end = ls_excel-warranty_end.
        ls_disp-amc = ls_excel-amc.
        ls_disp-amc_company = ls_excel-amc_company.
        ls_disp-amc_contact = ls_excel-amc_contact.
        ls_disp-invoice_no = ls_excel-invoice_no.
        ls_disp-invoice_date = ls_excel-invoice_date.
        ls_disp-po_number = ls_excel-po_number.
        ls_disp-vendor_id = ls_excel-vendor_id.
        ls_disp-vendor_name = ls_excel-vendor_name.
        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
        ls_disp-assign_to_name = ls_excel-assign_to_name.
        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
        ls_disp-assigned_email = ls_excel-assigned_email.
        ls_disp-accessory_1 = ls_excel-accessory_1.
        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
        ls_disp-accessory_2 = ls_excel-accessory_2.
        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
        ls_disp-accessory_3 = ls_excel-accessory_3.
        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
        ls_disp-accessory_4 = ls_excel-accessory_4.
        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
        ls_disp-accessory_5 = ls_excel-accessory_5.
        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
        ls_disp-ip_address = ls_excel-ip_address.
        ls_disp-message  = 'Incorrect Plant'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.
******PUTS CHECK FOR ASSET_TYPE IN ZHR_ASSET_TYPE*************
    IF ls_excel-asset_type IS NOT INITIAL.
      READ TABLE lt_asset_typ  INTO ls_asset_typ WITH KEY asset_type = ls_excel-asset_type.
      IF sy-subrc NE 0.
        ls_disp-asset_type = ls_excel-asset_type.
        ls_disp-comp_code  = ls_excel-comp_code.
        ls_disp-plant  = ls_excel-plant.
        ls_disp-asset_id  = ls_excel-asset_id.
        ls_disp-serial_no = ls_excel-serial_no.
        ls_disp-asset_tag  = ls_excel-asset_tag.
        ls_disp-asset_des  = ls_excel-asset_des.
        ls_disp-product_no = ls_excel-product_no.
        ls_disp-asset_st  = ls_excel-asset_st.
        ls_disp-asset_date = ls_excel-asset_date.
        ls_disp-asset_remarks = ls_excel-asset_remarks.
        ls_disp-location = ls_excel-location.
        ls_disp-department = ls_excel-department.
        ls_disp-model = ls_excel-model.
        ls_disp-costcenter = ls_excel-costcenter.
        ls_disp-operating_sys = ls_excel-operating_sys.
        ls_disp-configuration = ls_excel-configuration.
        ls_disp-warranty = ls_excel-warranty.
        ls_disp-warranty_end = ls_excel-warranty_end.
        ls_disp-amc = ls_excel-amc.
        ls_disp-amc_company = ls_excel-amc_company.
        ls_disp-amc_contact = ls_excel-amc_contact.
        ls_disp-invoice_no = ls_excel-invoice_no.
        ls_disp-invoice_date = ls_excel-invoice_date.
        ls_disp-po_number = ls_excel-po_number.
        ls_disp-vendor_id = ls_excel-vendor_id.
        ls_disp-vendor_name = ls_excel-vendor_name.
        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
        ls_disp-assign_to_name = ls_excel-assign_to_name.
        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
        ls_disp-assigned_email = ls_excel-assigned_email.
        ls_disp-accessory_1 = ls_excel-accessory_1.
        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
        ls_disp-accessory_2 = ls_excel-accessory_2.
        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
        ls_disp-accessory_3 = ls_excel-accessory_3.
        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
        ls_disp-accessory_4 = ls_excel-accessory_4.
        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
        ls_disp-accessory_5 = ls_excel-accessory_5.
        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
        ls_disp-ip_address = ls_excel-ip_address.
        ls_disp-message  = 'Asset_type Not Matched'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.


********PUTS CHECK FOR ASSET_ID IN ANLA ****************
*    IF ls_excel-asset_id IS NOT INITIAL.
*      READ TABLE lt_anla INTO ls_anla WITH KEY anln1 = ls_excel-asset_id.
*      IF sy-subrc NE 0.
*        ls_disp-asset_type = ls_excel-asset_type.
*        ls_disp-comp_code  = ls_excel-comp_code.
*        ls_disp-plant  = ls_excel-plant.
*        ls_disp-asset_id  = ls_excel-asset_id.
*        ls_disp-serial_no = ls_excel-serial_no.
*        ls_disp-asset_tag  = ls_excel-asset_tag.
*        ls_disp-asset_des  = ls_excel-asset_des.
*        ls_disp-product_no = ls_excel-product_no.
*        ls_disp-asset_st  = ls_excel-asset_st.
*        ls_disp-asset_date = ls_excel-asset_date.
*        ls_disp-asset_remarks = ls_excel-asset_remarks.
*        ls_disp-location = ls_excel-location.
*        ls_disp-department = ls_excel-department.
*        ls_disp-model = ls_excel-model.
*        ls_disp-costcenter = ls_excel-costcenter.
*        ls_disp-operating_sys = ls_excel-operating_sys.
*        ls_disp-configuration = ls_excel-configuration.
*        ls_disp-warranty = ls_excel-warranty.
*        ls_disp-warranty_end = ls_excel-warranty_end.
*        ls_disp-amc = ls_excel-amc.
*        ls_disp-amc_company = ls_excel-amc_company.
*        ls_disp-amc_contact = ls_excel-amc_contact.
*        ls_disp-invoice_no = ls_excel-invoice_no.
*        ls_disp-invoice_date = ls_excel-invoice_date.
*        ls_disp-po_number = ls_excel-po_number.
*        ls_disp-vendor_id = ls_excel-vendor_id.
*        ls_disp-vendor_name = ls_excel-vendor_name.
*        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
*        ls_disp-assign_to_name = ls_excel-assign_to_name.
*        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
*        ls_disp-assigned_email = ls_excel-assigned_email.
*        ls_disp-accessory_1 = ls_excel-accessory_1.
*        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
*        ls_disp-accessory_2 = ls_excel-accessory_2.
*        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
*        ls_disp-accessory_3 = ls_excel-accessory_3.
*        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
*        ls_disp-accessory_4 = ls_excel-accessory_4.
*        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
*        ls_disp-accessory_5 = ls_excel-accessory_5.
*        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
*        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
*        ls_disp-ip_address = ls_excel-ip_address.
*        ls_disp-message  = 'Asset_ID Not Matched'.
*        APPEND ls_disp TO lt_disp.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

******PUTS CHECK FOR COSTCENTER******************************
    IF ls_excel-costcenter IS NOT INITIAL.
      READ TABLE lt_csks INTO ls_csks WITH KEY kostl = ls_excel-costcenter.
      IF sy-subrc NE 0.
        ls_disp-asset_type = ls_excel-asset_type.
        ls_disp-comp_code  = ls_excel-comp_code.
        ls_disp-plant  = ls_excel-plant.
        ls_disp-asset_id  = ls_excel-asset_id.
        ls_disp-serial_no = ls_excel-serial_no.
        ls_disp-asset_tag  = ls_excel-asset_tag.
        ls_disp-asset_des  = ls_excel-asset_des.
        ls_disp-product_no = ls_excel-product_no.
        ls_disp-asset_st  = ls_excel-asset_st.
        ls_disp-asset_date = ls_excel-asset_date.
        ls_disp-asset_remarks = ls_excel-asset_remarks.
        ls_disp-location = ls_excel-location.
        ls_disp-department = ls_excel-department.
        ls_disp-model = ls_excel-model.
        ls_disp-costcenter = ls_excel-costcenter.
        ls_disp-operating_sys = ls_excel-operating_sys.
        ls_disp-configuration = ls_excel-configuration.
        ls_disp-warranty = ls_excel-warranty.
        ls_disp-warranty_end = ls_excel-warranty_end.
        ls_disp-amc = ls_excel-amc.
        ls_disp-amc_company = ls_excel-amc_company.
        ls_disp-amc_contact = ls_excel-amc_contact.
        ls_disp-invoice_no = ls_excel-invoice_no.
        ls_disp-invoice_date = ls_excel-invoice_date.
        ls_disp-po_number = ls_excel-po_number.
        ls_disp-vendor_id = ls_excel-vendor_id.
        ls_disp-vendor_name = ls_excel-vendor_name.
        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
        ls_disp-assign_to_name = ls_excel-assign_to_name.
        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
        ls_disp-assigned_email = ls_excel-assigned_email.
        ls_disp-accessory_1 = ls_excel-accessory_1.
        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
        ls_disp-accessory_2 = ls_excel-accessory_2.
        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
        ls_disp-accessory_3 = ls_excel-accessory_3.
        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
        ls_disp-accessory_4 = ls_excel-accessory_4.
        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
        ls_disp-accessory_5 = ls_excel-accessory_5.
        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
        ls_disp-ip_address = ls_excel-ip_address.
        ls_disp-message  = 'Costcenter Not Matched'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.
*****PUTS CHECK FOR ASSET_STATUS************************
    IF ls_excel-asset_st NOT BETWEEN '01' AND '08'.
      CLEAR ls_disp.
      ls_disp-asset_type = ls_excel-asset_type.
      ls_disp-comp_code  = ls_excel-comp_code.
      ls_disp-plant  = ls_excel-plant.
      ls_disp-asset_id  = ls_excel-asset_id.
      ls_disp-serial_no = ls_excel-serial_no.
      ls_disp-asset_tag  = ls_excel-asset_tag.
      ls_disp-asset_des  = ls_excel-asset_des.
      ls_disp-product_no = ls_excel-product_no.
      ls_disp-asset_st  = ls_excel-asset_st.
      ls_disp-asset_date = ls_excel-asset_date.
      ls_disp-asset_remarks = ls_excel-asset_remarks.
      ls_disp-location = ls_excel-location.
      ls_disp-department = ls_excel-department.
      ls_disp-model = ls_excel-model.
      ls_disp-costcenter = ls_excel-costcenter.
      ls_disp-operating_sys = ls_excel-operating_sys.
      ls_disp-configuration = ls_excel-configuration.
      ls_disp-warranty = ls_excel-warranty.
      ls_disp-warranty_end = ls_excel-warranty_end.
      ls_disp-amc = ls_excel-amc.
      ls_disp-amc_company = ls_excel-amc_company.
      ls_disp-amc_contact = ls_excel-amc_contact.
      ls_disp-invoice_no = ls_excel-invoice_no.
      ls_disp-invoice_date = ls_excel-invoice_date.
      ls_disp-po_number = ls_excel-po_number.
      ls_disp-vendor_id = ls_excel-vendor_id.
      ls_disp-vendor_name = ls_excel-vendor_name.
      ls_disp-non_emp_assign = ls_excel-non_emp_assign.
      ls_disp-assign_to_name = ls_excel-assign_to_name.
      ls_disp-assign_cont_no = ls_excel-assign_cont_no.
      ls_disp-assigned_email = ls_excel-assigned_email.
      ls_disp-accessory_1 = ls_excel-accessory_1.
      ls_disp-accessory_1_no = ls_excel-accessory_1_no.
      ls_disp-accessory_2 = ls_excel-accessory_2.
      ls_disp-accessory_2_no = ls_excel-accessory_2_no.
      ls_disp-accessory_3 = ls_excel-accessory_3.
      ls_disp-accessory_3_no = ls_excel-accessory_3_no.
      ls_disp-accessory_4 = ls_excel-accessory_4.
      ls_disp-accessory_4_no = ls_excel-accessory_4_no.
      ls_disp-accessory_5 = ls_excel-accessory_5.
      ls_disp-accessory_5_no = ls_excel-accessory_5_no.
      ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_disp-ip_address = ls_excel-ip_address.
      ls_disp-message = 'Asset status Incorrect'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.


******PUTS CHECK FOR WARRANTY **************
    IF ls_excel-warranty NOT BETWEEN '01' AND '02'.
      CLEAR ls_disp.
      ls_disp-asset_type = ls_excel-asset_type.
      ls_disp-comp_code  = ls_excel-comp_code.
      ls_disp-plant  = ls_excel-plant.
      ls_disp-asset_id  = ls_excel-asset_id.
      ls_disp-serial_no = ls_excel-serial_no.
      ls_disp-asset_tag  = ls_excel-asset_tag.
      ls_disp-asset_des  = ls_excel-asset_des.
      ls_disp-product_no = ls_excel-product_no.
      ls_disp-asset_st  = ls_excel-asset_st.
      ls_disp-asset_date = ls_excel-asset_date.
      ls_disp-asset_remarks = ls_excel-asset_remarks.
      ls_disp-location = ls_excel-location.
      ls_disp-department = ls_excel-department.
      ls_disp-model = ls_excel-model.
      ls_disp-costcenter = ls_excel-costcenter.
      ls_disp-operating_sys = ls_excel-operating_sys.
      ls_disp-configuration = ls_excel-configuration.
      ls_disp-warranty = ls_excel-warranty.
      ls_disp-warranty_end = ls_excel-warranty_end.
      ls_disp-amc = ls_excel-amc.
      ls_disp-amc_company = ls_excel-amc_company.
      ls_disp-amc_contact = ls_excel-amc_contact.
      ls_disp-invoice_no = ls_excel-invoice_no.
      ls_disp-invoice_date = ls_excel-invoice_date.
      ls_disp-po_number = ls_excel-po_number.
      ls_disp-vendor_id = ls_excel-vendor_id.
      ls_disp-vendor_name = ls_excel-vendor_name.
      ls_disp-non_emp_assign = ls_excel-non_emp_assign.
      ls_disp-assign_to_name = ls_excel-assign_to_name.
      ls_disp-assign_cont_no = ls_excel-assign_cont_no.
      ls_disp-assigned_email = ls_excel-assigned_email.
      ls_disp-accessory_1 = ls_excel-accessory_1.
      ls_disp-accessory_1_no = ls_excel-accessory_1_no.
      ls_disp-accessory_2 = ls_excel-accessory_2.
      ls_disp-accessory_2_no = ls_excel-accessory_2_no.
      ls_disp-accessory_3 = ls_excel-accessory_3.
      ls_disp-accessory_3_no = ls_excel-accessory_3_no.
      ls_disp-accessory_4 = ls_excel-accessory_4.
      ls_disp-accessory_4_no = ls_excel-accessory_4_no.
      ls_disp-accessory_5 = ls_excel-accessory_5.
      ls_disp-accessory_5_no = ls_excel-accessory_5_no.
      ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_disp-ip_address = ls_excel-ip_address.
      ls_disp-message = 'Warranty Incorrect'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

******PUTS CHECK FOR NON EMP ASSIGN **************
    IF ls_excel-non_emp_assign IS NOT INITIAL.
      IF ls_excel-non_emp_assign NOT BETWEEN '01' AND '02'.
        CLEAR ls_disp.
        ls_disp-asset_type = ls_excel-asset_type.
        ls_disp-comp_code  = ls_excel-comp_code.
        ls_disp-plant  = ls_excel-plant.
        ls_disp-asset_id  = ls_excel-asset_id.
        ls_disp-serial_no = ls_excel-serial_no.
        ls_disp-asset_tag  = ls_excel-asset_tag.
        ls_disp-asset_des  = ls_excel-asset_des.
        ls_disp-product_no = ls_excel-product_no.
        ls_disp-asset_st  = ls_excel-asset_st.
        ls_disp-asset_date = ls_excel-asset_date.
        ls_disp-asset_remarks = ls_excel-asset_remarks.
        ls_disp-location = ls_excel-location.
        ls_disp-department = ls_excel-department.
        ls_disp-model = ls_excel-model.
        ls_disp-costcenter = ls_excel-costcenter.
        ls_disp-operating_sys = ls_excel-operating_sys.
        ls_disp-configuration = ls_excel-configuration.
        ls_disp-warranty = ls_excel-warranty.
        ls_disp-warranty_end = ls_excel-warranty_end.
        ls_disp-amc = ls_excel-amc.
        ls_disp-amc_company = ls_excel-amc_company.
        ls_disp-amc_contact = ls_excel-amc_contact.
        ls_disp-invoice_no = ls_excel-invoice_no.
        ls_disp-invoice_date = ls_excel-invoice_date.
        ls_disp-po_number = ls_excel-po_number.
        ls_disp-vendor_id = ls_excel-vendor_id.
        ls_disp-vendor_name = ls_excel-vendor_name.
        ls_disp-non_emp_assign = ls_excel-non_emp_assign.
        ls_disp-assign_to_name = ls_excel-assign_to_name.
        ls_disp-assign_cont_no = ls_excel-assign_cont_no.
        ls_disp-assigned_email = ls_excel-assigned_email.
        ls_disp-accessory_1 = ls_excel-accessory_1.
        ls_disp-accessory_1_no = ls_excel-accessory_1_no.
        ls_disp-accessory_2 = ls_excel-accessory_2.
        ls_disp-accessory_2_no = ls_excel-accessory_2_no.
        ls_disp-accessory_3 = ls_excel-accessory_3.
        ls_disp-accessory_3_no = ls_excel-accessory_3_no.
        ls_disp-accessory_4 = ls_excel-accessory_4.
        ls_disp-accessory_4_no = ls_excel-accessory_4_no.
        ls_disp-accessory_5 = ls_excel-accessory_5.
        ls_disp-accessory_5_no = ls_excel-accessory_5_no.
        ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
        ls_disp-ip_address = ls_excel-ip_address.
        ls_disp-message = 'Non Emp Assignment Incorrect'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.


***************Updating Existing data in table (ZHR_ASSET_MASTER)******
    READ TABLE lt_asset INTO ls_asset WITH KEY asset_id = ls_excel-asset_id
                                               asset_type = ls_excel-asset_type
                                               comp_code = ls_excel-comp_code
                                               plant     = ls_excel-plant
                                               serial_no = ls_excel-serial_no.
    IF sy-subrc = 0.
      ls_asset-asset_tag  = ls_excel-asset_tag.
      ls_asset-asset_des  = ls_excel-asset_des.
      ls_asset-product_no = ls_excel-product_no.
      ls_asset-asset_st  = ls_excel-asset_st.
      ls_asset-asset_date = ls_excel-asset_date.
      ls_asset-asset_remarks = ls_excel-asset_remarks.
      ls_asset-location = ls_excel-location.
      ls_asset-department = ls_excel-department.
      ls_asset-model = ls_excel-model.
      ls_asset-costcenter = ls_excel-costcenter.
      ls_asset-operating_sys = ls_excel-operating_sys.
      ls_asset-configuration = ls_excel-configuration.
      ls_asset-warranty = ls_excel-warranty.
      ls_asset-warranty_end = ls_excel-warranty_end.
      ls_asset-amc = ls_excel-amc.
      ls_asset-amc_company = ls_excel-amc_company.
      ls_asset-amc_contact = ls_excel-amc_contact.
      ls_asset-invoice_no = ls_excel-invoice_no.
      ls_asset-invoice_date = ls_excel-invoice_date.
      ls_asset-po_number = ls_excel-po_number.
      ls_asset-vendor_id = ls_excel-vendor_id.
      ls_asset-vendor_name = ls_excel-vendor_name.
      ls_asset-non_emp_assign = ls_excel-non_emp_assign.
      ls_asset-assign_to_name = ls_excel-assign_to_name.
      ls_asset-assign_cont_no = ls_excel-assign_cont_no.
      ls_asset-assigned_email = ls_excel-assigned_email.
      ls_asset-accessory_1 = ls_excel-accessory_1.
      ls_asset-accessory_1_no = ls_excel-accessory_1_no.
      ls_asset-accessory_2 = ls_excel-accessory_2.
      ls_asset-accessory_2_no = ls_excel-accessory_2_no.
      ls_asset-accessory_3 = ls_excel-accessory_3.
      ls_asset-accessory_3_no = ls_excel-accessory_3_no.
      ls_asset-accessory_4 = ls_excel-accessory_4.
      ls_asset-accessory_4_no = ls_excel-accessory_4_no.
      ls_asset-accessory_5 = ls_excel-accessory_5.
      ls_asset-accessory_5_no = ls_excel-accessory_5_no.
      ls_asset-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_asset-ip_address = ls_excel-ip_address.
      MODIFY zhr_asset_master FROM ls_asset.
      COMMIT WORK AND WAIT.

***Error Message ALV Display****
      ls_disp-asset_type = ls_excel-asset_type.
****Asset type Desc
      CLEAR ls_asset_typ.
      READ TABLE lt_asset_typ INTO ls_asset_typ WITH KEY asset_type = ls_disp-asset_type.
      IF sy-subrc EQ 0.
        ls_disp-asset_type_desc = ls_asset_typ-asset_des.
      ENDIF.

      ls_disp-comp_code  = ls_excel-comp_code.
****Company code Desc**
      CLEAR ls_comp.
      READ TABLE lt_comp INTO ls_comp WITH KEY bukrs = ls_disp-comp_code.
      IF sy-subrc EQ 0.
        ls_disp-compcode_des = ls_comp-butxt.
      ENDIF.

      ls_disp-plant  = ls_excel-plant.
***Plant code Desc**
      CLEAR ls_plant.
      READ TABLE lt_plant INTO ls_plant WITH KEY werks = ls_disp-plant.
      IF sy-subrc EQ 0.
        ls_disp-plant_des = ls_plant-name1.
      ENDIF.

      ls_disp-asset_id  = ls_excel-asset_id.
      ls_disp-serial_no = ls_excel-serial_no.
      ls_disp-asset_tag  = ls_excel-asset_tag.
      ls_disp-asset_des  = ls_excel-asset_des.
      ls_disp-product_no = ls_excel-product_no.

      ls_disp-asset_st  = ls_excel-asset_st.
* ASSET_STATUS  (DESCRIPTION)***********
      CLEAR gs_status.
      READ TABLE lt_status INTO ls_status WITH KEY asset_st = ls_disp-asset_st.
      IF sy-subrc EQ 0.
        ls_disp-asset_st_desc = ls_status-asset_st_des.
      ENDIF.

      ls_disp-asset_date = ls_excel-asset_date.
      ls_disp-asset_remarks = ls_excel-asset_remarks.
      ls_disp-location = ls_excel-location.
      ls_disp-department = ls_excel-department.
      ls_disp-model = ls_excel-model.
      ls_disp-costcenter = ls_excel-costcenter.
      ls_disp-operating_sys = ls_excel-operating_sys.
      ls_disp-configuration = ls_excel-configuration.
      ls_disp-warranty = ls_excel-warranty.
*****WARRANTY  (DESCRIPTION)***********
      IF ls_disp-warranty = '01'.
        ls_disp-warranty_desc = 'YES'.
      ELSEIF ls_disp-warranty = '02'.
        ls_disp-warranty_desc = 'NO'.
      ENDIF.

      ls_disp-warranty_end = ls_excel-warranty_end.
      ls_disp-amc = ls_excel-amc.
******** AMC  (DESCRIPTION)***********
      IF ls_disp-amc = '01'.
        ls_disp-amc_desc = 'YES'.
      ELSEIF ls_disp-amc = '02'.
        ls_disp-amc_desc = 'NO'.
      ENDIF.

      ls_disp-amc_company = ls_excel-amc_company.
      ls_disp-amc_contact = ls_excel-amc_contact.
      ls_disp-invoice_no = ls_excel-invoice_no.
      ls_disp-invoice_date = ls_excel-invoice_date.
      ls_disp-po_number = ls_excel-po_number.
      ls_disp-vendor_id = ls_excel-vendor_id.
      ls_disp-vendor_name = ls_excel-vendor_name.
      ls_disp-non_emp_assign = ls_excel-non_emp_assign.
******** NON_EMP_ASSIGN (DESCRIPTION)***********
      IF ls_disp-non_emp_assign = '01'.
        ls_disp-non_emp_desc = 'YES'.
      ELSEIF ls_disp-non_emp_assign = '02'.
        ls_disp-non_emp_desc = 'NO'.
      ENDIF.

      ls_disp-assign_to_name = ls_excel-assign_to_name.
      ls_disp-assign_cont_no = ls_excel-assign_cont_no.
      ls_disp-assigned_email = ls_excel-assigned_email.
      ls_disp-accessory_1 = ls_excel-accessory_1.
      ls_disp-accessory_1_no = ls_excel-accessory_1_no.
      ls_disp-accessory_2 = ls_excel-accessory_2.
      ls_disp-accessory_2_no = ls_excel-accessory_2_no.
      ls_disp-accessory_3 = ls_excel-accessory_3.
      ls_disp-accessory_3_no = ls_excel-accessory_3_no.
      ls_disp-accessory_4 = ls_excel-accessory_4.
      ls_disp-accessory_4_no = ls_excel-accessory_4_no.
      ls_disp-accessory_5 = ls_excel-accessory_5.
      ls_disp-accessory_5_no = ls_excel-accessory_5_no.
      ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_disp-ip_address = ls_excel-ip_address.
      ls_disp-message = 'Existing Asset_id Updated Successfully' .
      APPEND ls_disp TO lt_disp.

    ELSE.
*****Creating New records in Table***********************
      ls_asset-asset_type = ls_excel-asset_type.
      ls_asset-comp_code  = ls_excel-comp_code.
      ls_asset-plant  = ls_excel-plant.

      IF ls_excel-asset_id IS NOT INITIAL.
        ls_asset-asset_id   = ls_excel-asset_id.
***If asset ID is not present Creating Custom Number***
      ELSE.
        CLEAR lv_asset.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZCUSTOM_AS'
*           QUANTITY                = '1'
*           SUBOBJECT               = ' '
*           TOYEAR                  = '0000'
*           IGNORE_BUFFER           = ' '
          IMPORTING
            number                  = lv_asset
*           QUANTITY                =
*           RETURNCODE              =
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        ls_asset-asset_id = lv_asset.
      ENDIF.

      ls_asset-serial_no  = ls_excel-serial_no.
      ls_asset-asset_tag  = ls_excel-asset_tag.
      ls_asset-asset_des  = ls_excel-asset_des.
      ls_asset-product_no = ls_excel-product_no.
      ls_asset-asset_st  = ls_excel-asset_st.
      ls_asset-asset_date = ls_excel-asset_date.
      ls_asset-asset_remarks = ls_excel-asset_remarks.
      ls_asset-location = ls_excel-location.
      ls_asset-department = ls_excel-department.
      ls_asset-model = ls_excel-model.
      ls_asset-costcenter = ls_excel-costcenter.
      ls_asset-operating_sys = ls_excel-operating_sys.
      ls_asset-configuration = ls_excel-configuration.
      ls_asset-warranty = ls_excel-warranty.
      ls_asset-warranty_end = ls_excel-warranty_end.
      ls_asset-amc = ls_excel-amc.
      ls_asset-amc_company = ls_excel-amc_company.
      ls_asset-amc_contact = ls_excel-amc_contact.
      ls_asset-invoice_no = ls_excel-invoice_no.
      ls_asset-invoice_date = ls_excel-invoice_date.
      ls_asset-po_number = ls_excel-po_number.
      ls_asset-vendor_id = ls_excel-vendor_id.
      ls_asset-vendor_name = ls_excel-vendor_name.
      ls_asset-non_emp_assign = ls_excel-non_emp_assign.
      ls_asset-assign_to_name = ls_excel-assign_to_name.
      ls_asset-assign_cont_no = ls_excel-assign_cont_no.
      ls_asset-assigned_email = ls_excel-assigned_email.
      ls_asset-accessory_1 = ls_excel-accessory_1.
      ls_asset-accessory_1_no = ls_excel-accessory_1_no.
      ls_asset-accessory_2 = ls_excel-accessory_2.
      ls_asset-accessory_2_no = ls_excel-accessory_2_no.
      ls_asset-accessory_3 = ls_excel-accessory_3.
      ls_asset-accessory_3_no = ls_excel-accessory_3_no.
      ls_asset-accessory_4 = ls_excel-accessory_4.
      ls_asset-accessory_4_no = ls_excel-accessory_4_no.
      ls_asset-accessory_5 = ls_excel-accessory_5.
      ls_asset-accessory_5_no = ls_excel-accessory_5_no.
      ls_asset-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_asset-ip_address = ls_excel-ip_address.
      MODIFY zhr_asset_master FROM ls_asset.
      COMMIT WORK AND WAIT.

***Error Message ALV Display****
      ls_disp-asset_type = ls_excel-asset_type.
****Asset type Desc
      CLEAR ls_asset_typ.
      READ TABLE lt_asset_typ INTO ls_asset_typ WITH KEY asset_type = ls_disp-asset_type.
      IF sy-subrc EQ 0.
        ls_disp-asset_type_desc = ls_asset_typ-asset_des.
      ENDIF.
      ls_disp-comp_code  = ls_excel-comp_code.
****Company code Desc**
      CLEAR ls_comp.
      READ TABLE lt_comp INTO ls_comp WITH KEY bukrs = ls_disp-comp_code.
      IF sy-subrc EQ 0.
        ls_disp-compcode_des = ls_comp-butxt.
      ENDIF.

      ls_disp-plant  = ls_excel-plant.
***Plant code Desc**
      CLEAR ls_plant.
      READ TABLE lt_plant INTO ls_plant WITH KEY werks = ls_disp-plant.
      IF sy-subrc EQ 0.
        ls_disp-plant_des = ls_plant-name1.
      ENDIF.

      ls_disp-asset_id  = lv_asset.
      ls_disp-serial_no = ls_excel-serial_no.
      ls_disp-asset_tag  = ls_excel-asset_tag.
      ls_disp-asset_des  = ls_excel-asset_des.
      ls_disp-product_no = ls_excel-product_no.

      ls_disp-asset_st  = ls_excel-asset_st.
* ASSET_STATUS  (DESCRIPTION)***********
      CLEAR gs_status.
      READ TABLE lt_status INTO ls_status WITH KEY asset_st = ls_disp-asset_st.
      IF sy-subrc EQ 0.
        ls_disp-asset_st_desc = ls_status-asset_st_des.
      ENDIF.

      ls_disp-asset_date = ls_excel-asset_date.
      ls_disp-asset_remarks = ls_excel-asset_remarks.
      ls_disp-location = ls_excel-location.
      ls_disp-department = ls_excel-department.
      ls_disp-model = ls_excel-model.
      ls_disp-costcenter = ls_excel-costcenter.
      ls_disp-operating_sys = ls_excel-operating_sys.
      ls_disp-configuration = ls_excel-configuration.
      ls_disp-warranty = ls_excel-warranty.
*****WARRANTY  (DESCRIPTION)***********
      IF ls_disp-warranty = '01'.
        ls_disp-warranty_desc = 'YES'.
      ELSEIF ls_disp-warranty = '02'.
        ls_disp-warranty_desc = 'NO'.
      ENDIF.

      ls_disp-warranty_end = ls_excel-warranty_end.
      ls_disp-amc = ls_excel-amc.
******** AMC  (DESCRIPTION)***********
      IF ls_disp-amc = '01'.
        ls_disp-amc_desc = 'YES'.
      ELSEIF ls_disp-amc = '02'.
        ls_disp-amc_desc = 'NO'.
      ENDIF.

      ls_disp-amc_company = ls_excel-amc_company.
      ls_disp-amc_contact = ls_excel-amc_contact.
      ls_disp-invoice_no = ls_excel-invoice_no.
      ls_disp-invoice_date = ls_excel-invoice_date.
      ls_disp-po_number = ls_excel-po_number.
      ls_disp-vendor_id = ls_excel-vendor_id.
      ls_disp-vendor_name = ls_excel-vendor_name.
      ls_disp-non_emp_assign = ls_excel-non_emp_assign.
******** NON_EMP_ASSIGN (DESCRIPTION)***********
      IF ls_disp-non_emp_assign = '01'.
        ls_disp-non_emp_desc = 'YES'.
      ELSEIF ls_disp-non_emp_assign = '02'.
        ls_disp-non_emp_desc = 'NO'.
      ENDIF.

      ls_disp-assign_to_name = ls_excel-assign_to_name.
      ls_disp-assign_cont_no = ls_excel-assign_cont_no.
      ls_disp-assigned_email = ls_excel-assigned_email.
      ls_disp-accessory_1 = ls_excel-accessory_1.
      ls_disp-accessory_1_no = ls_excel-accessory_1_no.
      ls_disp-accessory_2 = ls_excel-accessory_2.
      ls_disp-accessory_2_no = ls_excel-accessory_2_no.
      ls_disp-accessory_3 = ls_excel-accessory_3.
      ls_disp-accessory_3_no = ls_excel-accessory_3_no.
      ls_disp-accessory_4 = ls_excel-accessory_4.
      ls_disp-accessory_4_no = ls_excel-accessory_4_no.
      ls_disp-accessory_5 = ls_excel-accessory_5.
      ls_disp-accessory_5_no = ls_excel-accessory_5_no.
      ls_disp-wifi_mac_id = ls_excel-wifi_mac_id.
      ls_disp-ip_address = ls_excel-ip_address.
      ls_disp-message = 'New Asset_id Created Successfully' .
      APPEND ls_disp TO lt_disp.
    ENDIF.
  ENDLOOP.

  REFRESH lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_TYPE'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  1.
  ls_fcat-scrtext_l  = 'Asset_type'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_TYPE_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  2.
  ls_fcat-scrtext_l   = 'Asset_type description'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'COMP_CODE'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  3.
  ls_fcat-scrtext_l   = 'Company Code'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'COMPCODE_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  4.
  ls_fcat-scrtext_l   = 'Compcode Desc'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'PLANT'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  4.
  ls_fcat-scrtext_l   = 'Plant'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'PLANT_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  5.
  ls_fcat-scrtext_l   = 'Plant Desc'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_ID'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  6.
  ls_fcat-scrtext_l   = 'Asset_ID'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'SERIAL_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  7.
  ls_fcat-scrtext_l   = 'Serial number'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_TAG'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  8.
  ls_fcat-scrtext_l   = 'ASSET_TAG'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  9.
  ls_fcat-scrtext_l   = 'ASSET_DES'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'PRODUCT_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  10.
  ls_fcat-scrtext_l   = 'PRODUCT_NO'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_ST'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  11.
  ls_fcat-scrtext_l = 'Asset status'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_ST_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  12.
  ls_fcat-scrtext_l   = 'ASSET_STATUS_DES'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_DATE'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  13.
  ls_fcat-scrtext_l   = 'Asset date'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSET_REMARKS'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  14.
  ls_fcat-scrtext_l   = 'Asset remarks'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'LOCATION'.
  ls_fcat-tabname   = 'ZHR_ASSET_MASTER'.
  ls_fcat-col_pos   =  15.
  ls_fcat-scrtext_l   = 'LOCATION'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'DEPARTMENT'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  16.
  ls_fcat-scrtext_l   = 'DEPARTMENT'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MODEL'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  17.
  ls_fcat-scrtext_l   = 'MODEL'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'COSTCENTER'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  18.
  ls_fcat-scrtext_l   = 'COSTCENTER'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OPERATING_SYS'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  19.
  ls_fcat-scrtext_l   = 'OPERATING_SYS'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'CONFIGURATION'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  20.
  ls_fcat-scrtext_l  = 'CONFIGURATION'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'WARRANTY'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  21.
  ls_fcat-scrtext_l  = 'WARRANTY'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'WARRANTY_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  22.
  ls_fcat-scrtext_l   = 'WARRANTY_DES'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'WARRANTY_END'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  23.
  ls_fcat-scrtext_l  = 'WARRANTY_END'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'AMC'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  24.
  ls_fcat-scrtext_l   = 'AMC'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'AMC_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  25.
  ls_fcat-scrtext_l   = 'AMC_DES'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'AMC_COMPANY'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  26.
  ls_fcat-scrtext_l   = 'AMC_COMPANY'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'AMC_CONTACT'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  27.
  ls_fcat-scrtext_l  = 'AMC_CONTACT'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'INVOICE_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  28.
  ls_fcat-scrtext_l   = 'INVOICE_NO'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'INVOICE_DATE'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  29.
  ls_fcat-scrtext_l   = 'INVOICE_DATE'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'PO_NUMBER'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  30.
  ls_fcat-scrtext_l   = 'PO_NUMBER'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'VENDOR_ID'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  31.
  ls_fcat-scrtext_l   = 'VENDOR_ID'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'VENDOR_NAME'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  32.
  ls_fcat-scrtext_l   = 'VENDOR_NAME'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NON_EMP_ASSIGN'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  33.
  ls_fcat-scrtext_l   = 'NON_EMP_ASSIGN'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NON_EMP_DES'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  34.
  ls_fcat-scrtext_l   = 'NON_EMP_DES'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSIGN_TO_NAME'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  35.
  ls_fcat-scrtext_l   = 'ASSIGN_TO_NAME'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSIGN_CONT_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  36.
  ls_fcat-scrtext_l   = 'ASSIGN_CONT_NO'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ASSIGNED_EMAIL'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  37.
  ls_fcat-scrtext_l  = 'ASSIGNED_EMAIL'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_1'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  38.
  ls_fcat-scrtext_l   = 'ACCESSORY_1'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_1_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  39.
  ls_fcat-scrtext_l   = 'ACCESSORY_NO1'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_2'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  40.
  ls_fcat-scrtext_l   = 'ACCESSORY_2'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_2_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  41.
  ls_fcat-scrtext_l   = 'ACCESSORY_NO2'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_3'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  42.
  ls_fcat-scrtext_l   = 'ACCESSORY_3'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_3_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  43.
  ls_fcat-scrtext_l   = 'ACCESSORY_NO3'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_4'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  44.
  ls_fcat-scrtext_l   = 'ACCESSORY_4'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_4_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  45.
  ls_fcat-scrtext_l   = 'ACCESSORY_NO4'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_5'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  46.
  ls_fcat-scrtext_l   = 'ACCESSORY_5'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ACCESSORY_5_NO'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  47.
  ls_fcat-scrtext_l   = 'ACCESSORY_NO5'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'WIFI_MAC_ID'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  48.
  ls_fcat-scrtext_l   = 'WIFI_MAC_ID'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'IP_ADDRESS'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  49.
  ls_fcat-scrtext_l   = 'IP_Address'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MESSAGE'.
  ls_fcat-tabname   = 'LT_DISP'.
  ls_fcat-col_pos   =  50.
  ls_fcat-txt_field   = 'Messages'.
  APPEND ls_fcat TO lt_fcat.

  IF lt_disp[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_grid
          CHANGING
            t_table      = lt_disp.

        CALL METHOD lo_grid->display.

      CATCH cx_salv_msg.

    ENDTRY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ZASSIGN'.
  SET TITLEBAR 'ZASSIGN'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
MODULE user_command_0300 INPUT.
  DATA: v_pernr TYPE pernr_d,
        v_begda TYPE datum,
        ls_ret  TYPE  bapireturn1,
        ls_0040 TYPE p0040.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'PC4'.
      SET SCREEN 0.
    WHEN 'PC3'.
      IF v_pernr IS INITIAL.
        MESSAGE 'Please key in Personnel Number' TYPE 'I' DISPLAY LIKE 'E'.
*        SET SCREEN 0.
      ELSEIF v_begda IS INITIAL.
        MESSAGE 'Please key in Start Date' TYPE 'I' DISPLAY LIKE 'E'.
*        SET SCREEN 0.
      ELSE.
        LOOP AT lt_disp INTO ls_disp WHERE box = 'X' .

          CLEAR ls_ret.
          CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
            EXPORTING
              number = v_pernr
            IMPORTING
              return = ls_ret.
          IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
            CLEAR:ls_0040, ls_ret.
            ls_0040-pernr = v_pernr.
            ls_0040-subty  = 'NA'.
            ls_0040-begda = v_begda.
            ls_0040-endda = '99991231'.
            ls_0040-lobnr =  ls_disp-asset_id .
            ls_0040-serial_no = ls_disp-serial_no  .

            CALL FUNCTION 'HR_INFOTYPE_OPERATION'
              EXPORTING
                infty         = '0040'
                number        = ls_0040-pernr
                subtype       = ls_0040-subty
                validityend   = ls_0040-endda
                validitybegin = ls_0040-begda
                record        = ls_0040
                operation     = 'INS'
              IMPORTING
                return        = ls_ret.

            IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
              MESSAGE 'IT0040 Created Successfully' TYPE 'S'.
            ELSE.
              MESSAGE ls_ret-message TYPE ls_ret-type.
            ENDIF.
            CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
              EXPORTING
                number = v_pernr
              IMPORTING
                return = ls_ret.
          ELSE.
            MESSAGE ls_ret-message TYPE ls_ret-type.
          ENDIF.

        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDMODULE.

*& Module STATUS_0400 OUTPUT
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'ZUNASSIGN'.
  SET TITLEBAR 'ZUNASSIGN'.
ENDMODULE.

*&      Module  USER_COMMAND_0400  INPUT
MODULE user_command_0400 INPUT.
  DATA: g_pernr  TYPE pernr_d,
        g_begda  TYPE datum,
        ls_ret1  TYPE  bapireturn1,
        ls_infty TYPE p0040,
        lt_infty TYPE TABLE OF p0040.

  CASE sy-ucomm.
    WHEN 'PC6'.
      SET SCREEN 0.
    WHEN 'PC5'.
      IF g_pernr IS INITIAL.
        MESSAGE 'Please key in Personnel Number' TYPE 'I' DISPLAY LIKE 'E'.

      ELSEIF g_begda IS INITIAL.
        MESSAGE 'Please key in End Date' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        SELECT SINGLE * FROM pa0040 INTO CORRESPONDING FIELDS OF ls_infty WHERE pernr = g_pernr
                                                                          AND endda = '99991231'.
        LOOP AT lt_disp INTO ls_disp WHERE box = 'X' .

          CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
            EXPORTING
              number = g_pernr
            IMPORTING
              return = ls_ret1.

          IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
            CLEAR:ls_ret1.
            ls_infty-pernr = g_pernr.
            ls_infty-infty = '0040'.
            ls_infty-endda = g_begda.
            ls_infty-leihg = 'NA'.
            ls_infty-lobnr =  ''.
            ls_infty-serial_no = ''.
            CALL FUNCTION 'HR_INFOTYPE_OPERATION'
              EXPORTING
                infty         = ls_infty-infty
                number        = ls_infty-pernr
                subtype       = ls_infty-leihg
                validityend   = ls_infty-endda
                validitybegin = ls_infty-begda
                record        = ls_infty
                recordnumber  = ls_infty-seqnr
                operation     = 'INS'
*               dialog_mode   = '2'
              IMPORTING
                return        = ls_ret1.

            IF ( ls_ret1-type NE 'E' AND ls_ret1-type NE 'A' ).
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
              MESSAGE 'IT0040 Created Successfully' TYPE 'S'.
            ELSE.
              MESSAGE ls_ret1-message TYPE ls_ret1-type.
            ENDIF.
            CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
              EXPORTING
                number = g_pernr
              IMPORTING
                return = ls_ret1.
          ELSE.
            MESSAGE ls_ret1-message TYPE ls_ret1-type.
          ENDIF.

        ENDLOOP.
      ENDIF.

  ENDCASE.

ENDMODULE.
