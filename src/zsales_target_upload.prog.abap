*&---------------------------------------------------------------------*
*& Report ZSALES_TARGET_UPLOAD
*&---------------------------------------------------------------------*
*&Date Created - 02.05.2022
*& Created By - KPABAP (Shamsudeen)
*& Description - Program for excel upload in tbale (ZSALES_MONTH_TGT) customer
*                 sales target upload  based on Employee Number.
*& TR No    - DEVK931660.
*===========================================================================
*changed_on - 07.06.2022
*changed_by - KPABAP
*Description - Adding Four fields in Upload template and
"in table (ZSALES_MONTH_TGT)
*TR NO - DEVK931760
*&---------------------------------------------------------------------*
REPORT zsales_target_upload.

* INPUT PARAMETER IN SELECTION_SCREEN***************
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk.

***********EXCEL INPUT TEMPLATE STRUCTURE***********
TYPES: BEGIN OF lty_sales,
         employ_code   TYPE pernr_d, "Excel structure
         mon(2)        TYPE n,
         yr(4)         TYPE n,
         cx            TYPE wertv8,
         cy            TYPE wertv8,
         deco          TYPE wertv8,
         snc           TYPE wertv8,
         project       TYPE wertv8,
         dmi_target    TYPE wertv8,
*****Newly Added on 07.06.2022  ****************
         active_dealer TYPE zactive,
         ppi           TYPE zppi,
         focus_prdt1   TYPE zfocus,
         focus_prdt2   TYPE zfocus2,
         dmi_target1   TYPE wertv8,
         dmi_target2   TYPE wertv8,
         dmi_target3   TYPE wertv8,
         dmi_target4   TYPE wertv8,
       END OF lty_sales,
       BEGIN OF lty_disp,
         employ_code    TYPE pernr_d,
         emp_name       TYPE emnam,
         mon            TYPE n,
         zmonth_des(12) TYPE c,
         yr             TYPE n,
         cx             TYPE wertv8,
         cy             TYPE wertv8,
         deco           TYPE wertv8,
         snc            TYPE wertv8,
         project        TYPE wertv8,
         dmi_target     TYPE wertv8,
         cky            TYPE currency,
         active_dealer  TYPE zactive,
         ppi            TYPE zppi,
         focus_prdt1    TYPE zfocus,
         focus_prdt2    TYPE zfocus2,
         dmi_target1    TYPE wertv8,
         dmi_target2    TYPE wertv8,
         dmi_target3    TYPE wertv8,
         dmi_target4    TYPE wertv8,
         message(100)   TYPE c,
       END OF lty_disp,
       BEGIN OF lty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF lty_fields,

       BEGIN OF lty_pa0001,
         pernr TYPE pernr_d,
         ename TYPE emnam,
       END OF lty_pa0001.

DATA: lt_type      TYPE truxs_t_text_data,
      lt_excel     TYPE TABLE OF lty_sales,
      lt_excel_tmp TYPE TABLE OF lty_sales,
      ls_excel     TYPE lty_sales,
      lt_disp      TYPE STANDARD TABLE OF lty_disp,
      ls_disp      TYPE lty_disp,
      lt_pa0001    TYPE STANDARD TABLE OF lty_pa0001,
      ls_pa0001    TYPE lty_pa0001,
      lt_fields    TYPE STANDARD TABLE OF lty_fields,
      ls_fields    TYPE lty_fields,
      lt_fcat      TYPE lvc_t_fcat,
      ls_fcat      TYPE lvc_s_fcat,
      ls_layout    TYPE lvc_s_layo.
DATA: lt_sales TYPE STANDARD TABLE OF zsales_month_tgt,
      ls_sales TYPE zsales_month_tgt.

DATA: lo_grid TYPE REF TO cl_salv_table.



****** F4 help for fetching excel file
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'p_file'
    IMPORTING
      file_name     = p_file.
  .

START-OF-SELECTION.
  SELECT * FROM zsales_month_tgt
  INTO TABLE lt_sales.


  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = lt_type
      i_filename           = p_file
    TABLES
      i_tab_converted_data = lt_excel
*   EXCEPTIONS
*     CONVERSION_FAILED    = 1
*     OTHERS               = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

END-OF-SELECTION.

  IF lt_excel[] IS NOT INITIAL.
    lt_excel_tmp[] = lt_excel.
    SORT  lt_excel_tmp BY employ_code.
    DELETE ADJACENT DUPLICATES FROM lt_excel_tmp COMPARING employ_code.


*Select all from the Employee number based on the Excel***********
    SELECT pernr
           ename FROM pa0001
              INTO TABLE lt_pa0001
              FOR ALL ENTRIES IN lt_excel_tmp
              WHERE pernr = lt_excel_tmp-employ_code.
  ENDIF.

  LOOP AT lt_excel INTO ls_excel.

    IF ls_excel-employ_code IS  INITIAL.
*put error here
      CLEAR ls_disp.
      ls_disp-employ_code   = ls_excel-employ_code.
      ls_disp-mon   = ls_excel-mon.
      ls_disp-yr   = ls_excel-yr.
      ls_disp-cx   = ls_excel-cx.
      ls_disp-cy = ls_excel-cy.
      ls_disp-deco = ls_excel-deco.
      ls_disp-snc = ls_excel-snc.
      ls_disp-project = ls_excel-project.
      ls_disp-dmi_target = ls_excel-dmi_target.
      ls_disp-cky = 'INR'.
      ls_disp-active_dealer = ls_excel-active_dealer.
      ls_disp-ppi = ls_excel-ppi.
      ls_disp-focus_prdt1 = ls_excel-focus_prdt1.
      ls_disp-focus_prdt2 = ls_excel-focus_prdt2.
      ls_disp-dmi_target1 = ls_excel-dmi_target1.
      ls_disp-dmi_target2 = ls_excel-dmi_target2.
      ls_disp-dmi_target3 = ls_excel-dmi_target3.
      ls_disp-dmi_target4 = ls_excel-dmi_target4.
      ls_disp-message = 'Invalid Employee No'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

    READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr =  ls_excel-employ_code.
    IF sy-subrc NE 0.
      ls_disp-employ_code   = ls_excel-employ_code.
      ls_disp-emp_name       = ls_pa0001-ename.
      ls_disp-mon   = ls_excel-mon.
      ls_disp-yr   = ls_excel-yr.
      ls_disp-cx   = ls_excel-cx.
      ls_disp-cy = ls_excel-cy.
      ls_disp-deco = ls_excel-deco.
      ls_disp-snc = ls_excel-snc.
      ls_disp-project = ls_excel-project.
      ls_disp-dmi_target = ls_excel-dmi_target.
      ls_disp-cky = 'INR'.
      ls_disp-active_dealer = ls_excel-active_dealer.
      ls_disp-ppi = ls_excel-ppi.
      ls_disp-focus_prdt1 = ls_excel-focus_prdt1.
      ls_disp-focus_prdt2 = ls_excel-focus_prdt2.
      ls_disp-dmi_target1 = ls_excel-dmi_target1.
      ls_disp-dmi_target2 = ls_excel-dmi_target2.
      ls_disp-dmi_target3 = ls_excel-dmi_target3.
      ls_disp-dmi_target4 = ls_excel-dmi_target4.
      ls_disp-message = 'Invalid Employee No'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

    IF ls_excel-mon NOT BETWEEN '01' AND '12'.
      CLEAR ls_disp.
      ls_disp-employ_code   = ls_excel-employ_code.
      ls_disp-emp_name = ls_pa0001-ename.
      ls_disp-mon   = ls_excel-mon.
      ls_disp-yr   = ls_excel-yr.
      ls_disp-cx   = ls_excel-cx.
      ls_disp-cy = ls_excel-cy.
      ls_disp-deco = ls_excel-deco.
      ls_disp-snc = ls_excel-snc.
      ls_disp-project = ls_excel-project.
      ls_disp-dmi_target = ls_excel-dmi_target.
      ls_disp-cky = 'INR'.
      ls_disp-active_dealer = ls_excel-active_dealer.
      ls_disp-ppi = ls_excel-ppi.
      ls_disp-focus_prdt1 = ls_excel-focus_prdt1.
      ls_disp-focus_prdt2 = ls_excel-focus_prdt2.
      ls_disp-dmi_target1 = ls_excel-dmi_target1.
      ls_disp-dmi_target2 = ls_excel-dmi_target2.
      ls_disp-dmi_target3 = ls_excel-dmi_target3.
      ls_disp-dmi_target4 = ls_excel-dmi_target4.
      ls_disp-message = 'Invalid Month'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.


** Updating already available Employee record from excel********
    READ TABLE lt_sales INTO ls_sales WITH KEY employ_code = ls_excel-employ_code
                                               mon = ls_excel-mon
                                               yr = ls_excel-yr.

    IF sy-subrc = 0.
      ls_sales-cx = ls_excel-cx.
      ls_sales-cy = ls_excel-cy.
      ls_sales-deco = ls_excel-deco.
      ls_sales-snc = ls_excel-snc.
      ls_sales-project = ls_excel-project.
      ls_sales-dmi_target = ls_excel-dmi_target.
      ls_sales-active_dealer = ls_excel-active_dealer.
      ls_sales-ppi = ls_excel-ppi.
      ls_sales-focus_prdt1 = ls_excel-focus_prdt1.
      ls_sales-focus_prdt2 = ls_excel-focus_prdt2.
      ls_sales-dmi_target1 = ls_excel-dmi_target1.
      ls_sales-dmi_target2 = ls_excel-dmi_target2.
      ls_sales-dmi_target3 = ls_excel-dmi_target3.
      ls_sales-dmi_target4 = ls_excel-dmi_target4.
      MODIFY zsales_month_tgt FROM ls_sales.
      COMMIT WORK AND WAIT.

      CLEAR ls_disp.
      ls_disp-employ_code   = ls_excel-employ_code.
      ls_disp-emp_name      = ls_sales-emp_name.
      ls_disp-mon   = ls_excel-mon.
      ls_disp-yr   = ls_excel-yr.
      ls_disp-cx   = ls_excel-cx.
      ls_disp-cy = ls_excel-cy.
      ls_disp-deco = ls_excel-deco.
      ls_disp-snc = ls_excel-snc.
      ls_disp-project = ls_excel-project.
      ls_disp-dmi_target = ls_excel-dmi_target.
      ls_disp-cky = ls_sales-cky.
      ls_disp-active_dealer = ls_excel-active_dealer.
      ls_disp-ppi = ls_excel-ppi.
      ls_disp-focus_prdt1 = ls_excel-focus_prdt1.
      ls_disp-focus_prdt2 = ls_excel-focus_prdt2.
      ls_disp-dmi_target1 = ls_excel-dmi_target1.
      ls_disp-dmi_target2 = ls_excel-dmi_target2.
      ls_disp-dmi_target3 = ls_excel-dmi_target3.
      ls_disp-dmi_target4 = ls_excel-dmi_target4.
      ls_disp-message = 'Existing Employee Target updated Successfully'.
      APPEND ls_disp TO lt_disp.

    ELSE.
** Creating new record in Table from excel file.
      ls_sales-employ_code = ls_excel-employ_code.
      ls_sales-emp_name =  ls_pa0001-ename.
      ls_sales-mon  = ls_excel-mon.
      ls_sales-yr   = ls_excel-yr.
      ls_sales-cx = ls_excel-cx.
      ls_sales-cy = ls_excel-cy.
      ls_sales-deco = ls_excel-deco.
      ls_sales-snc = ls_excel-snc.
      ls_sales-project = ls_excel-project.
      ls_sales-dmi_target = ls_excel-dmi_target.
      ls_sales-cky = 'INR'.
      ls_sales-active_dealer = ls_excel-active_dealer.
      ls_sales-ppi = ls_excel-ppi.
      ls_sales-focus_prdt1 = ls_excel-focus_prdt1.
      ls_sales-focus_prdt2 = ls_excel-focus_prdt2.
      ls_sales-dmi_target1 = ls_excel-dmi_target1.
      ls_sales-dmi_target2 = ls_excel-dmi_target2.
      ls_sales-dmi_target3 = ls_excel-dmi_target3.
      ls_sales-dmi_target4 = ls_excel-dmi_target4.
      MODIFY zsales_month_tgt FROM ls_sales.
      COMMIT WORK AND WAIT.

      CLEAR ls_disp.
      ls_disp-employ_code   = ls_excel-employ_code.
      ls_disp-emp_name      = ls_pa0001-ename.
      ls_disp-mon   = ls_excel-mon.
      ls_disp-yr   = ls_excel-yr.
      ls_disp-cx   = ls_excel-cx.
      ls_disp-cy = ls_excel-cy.
      ls_disp-deco = ls_excel-deco.
      ls_disp-snc = ls_excel-snc.
      ls_disp-project = ls_excel-project.
      ls_disp-dmi_target = ls_excel-dmi_target.
      ls_disp-cky = ls_sales-cky.
      ls_disp-active_dealer = ls_excel-active_dealer.
      ls_disp-ppi = ls_excel-ppi.
      ls_disp-focus_prdt1 = ls_excel-focus_prdt1.
      ls_disp-focus_prdt2 = ls_excel-focus_prdt2.
      ls_disp-dmi_target1 = ls_excel-dmi_target1.
      ls_disp-dmi_target2 = ls_excel-dmi_target2.
      ls_disp-dmi_target3 = ls_excel-dmi_target3.
      ls_disp-dmi_target4 = ls_excel-dmi_target4.
      ls_disp-message = 'New Employee Target Created Successfully'.
      APPEND ls_disp TO lt_disp.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_disp INTO ls_disp.
    IF ls_disp-mon = '01'.
      ls_disp-zmonth_des = 'April'.
    ELSEIF ls_disp-mon = '02'.
      ls_disp-zmonth_des = 'May'.
    ELSEIF ls_disp-mon = '03'.
      ls_disp-zmonth_des = 'June'.
    ELSEIF ls_disp-mon = '04'.
      ls_disp-zmonth_des = 'July'.
    ELSEIF ls_disp-mon = '05'.
      ls_disp-zmonth_des = 'August'.
    ELSEIF ls_disp-mon = '06'.
      ls_disp-zmonth_des = 'September'.
    ELSEIF ls_disp-mon = '07'.
      ls_disp-zmonth_des = 'October'.
    ELSEIF ls_disp-mon = '08'.
      ls_disp-zmonth_des = 'November'.
    ELSEIF ls_disp-mon = '09'.
      ls_disp-zmonth_des = 'December'.
    ELSEIF ls_disp-mon = '10'.
      ls_disp-zmonth_des = 'Janauary'.
    ELSEIF ls_disp-mon = '11'.
      ls_disp-zmonth_des = 'Febraury'.
    ELSEIF ls_disp-mon = '12'.
      ls_disp-zmonth_des = 'March'.
    ENDIF.
    IF sy-subrc EQ 0.
      MODIFY lt_disp FROM ls_disp.
    ENDIF.
  ENDLOOP.

  REFRESH lt_fcat.

  PERFORM f_fieldcat USING  'EMPLOY_CODE'    'Employee Number' 1 space.
  PERFORM f_fieldcat USING  'EMP_NAME'       'Employee Name'   2 space.
  PERFORM f_fieldcat USING  'MON'            'Month'           3 space.
  PERFORM f_fieldcat USING  'ZMONTH_DES'     'Month Desc'      4 space.
  PERFORM f_fieldcat USING  'YR'             'Fiscal year'     5 space.
  PERFORM f_fieldcat USING  'CX'             'Target CX'       6 space.
  PERFORM f_fieldcat USING  'CY'             'Target Cy'       7 space.
  PERFORM f_fieldcat USING  'DECO'           'Target DECO'     8 space.
  PERFORM f_fieldcat USING  'SNC'            'Target SNC '     9 space.
  PERFORM f_fieldcat USING  'PROJECT'        'Project'        10 space.
  PERFORM f_fieldcat USING  'DMI_TARGET'     'Target1'        11 space.
  PERFORM f_fieldcat USING  'CKY'            'Currency Key'   12 space.
  PERFORM f_fieldcat USING  'ACTIVE_DEALERS' 'Active_dealers' 13 space.
  PERFORM f_fieldcat USING  'PPI'            'PPI Value'      14 space.
  PERFORM f_fieldcat USING  'FOCUS_PRDT1'    'Focus_product1' 15 space.
  PERFORM f_fieldcat USING  'FOCUS_PRDT2'    'Focus_product2' 16 space.
  PERFORM f_fieldcat USING  'DMI_TARGET1'    'Target2'        17 space.
  PERFORM f_fieldcat USING  'DMI_TARGET2'    'Target3'        18 space.
  PERFORM f_fieldcat USING  'DMI_TARGET3'    'Target4'        19 space.
  PERFORM f_fieldcat USING  'DMI_TARGET4'    'Target5'        20 space.

  IF lt_disp[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_grid
          CHANGING
            t_table      = lt_disp.
      CATCH cx_salv_msg.

    ENDTRY.

    CALL METHOD lo_grid->display.

  ENDIF.
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
