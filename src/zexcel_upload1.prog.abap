*&---------------------------------------------------------------------*
*& Report ZEXCEL_UPLOAD1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zexcel_upload1.


TYPES: BEGIN OF ty_excel,               " Structure For Excel Upload
         customer_no   TYPE kunnr,
         distributor_1 TYPE kunn2,
         distributor_2 TYPE kunn2,
         distributor_3 TYPE kunn2,
       END OF ty_excel.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
       END OF ty_kna1.

TYPES: BEGIN OF ty_disp,             " Structure for ALV display
         customer_no       TYPE kunnr,
         customer_name     TYPE name1_gp,
         distributor1      TYPE kunn2,
         distributor_name1 TYPE name1_gp,
         distributor2      TYPE kunn2,
         distributor_name2 TYPE name1_gp,
         distributor3      TYPE kunn2,
         distributor_name3 TYPE name1_gp,
         message           TYPE /aif/msg_text,
       END OF ty_disp.

DATA: lt_excel TYPE TABLE OF ty_excel,
      ls_excel TYPE ty_excel.

DATA: lt_type  TYPE truxs_t_text_data.

***** Internal table Declaration ********************
DATA:lt_disp TYPE STANDARD TABLE OF ty_disp,
     ls_disp TYPE ty_disp,
     lt_kna1 TYPE STANDARD TABLE OF ty_kna1,
     ls_kna1 TYPE ty_kna1.

DATA: lt_excel_temp TYPE STANDARD TABLE OF zsku_table,
      ls_excel_temp LIKE LINE OF lt_excel_temp.

********** Internal Table for ALV **********************
DATA: lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      ls_layout TYPE lvc_s_layo.

DATA: lo_grid TYPE REF TO cl_salv_table.

* INPUT PARAMETER IN SELECTION_SCREEN***************
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk.

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

  REFRESH: lt_excel_temp,lt_excel,lt_kna1,lt_disp.
  CLEAR: ls_excel,ls_excel_temp,ls_kna1,ls_disp.

******* Fetching Data from ZSKU_TABLE *************
  SELECT * FROM zsku_table
                INTO TABLE lt_excel_temp.
  IF sy-subrc EQ 0.
    SORT lt_excel_temp[] BY customer_no.
  ENDIF.

**** Fetching Customer Master For Checks ********
  SELECT kunnr
         name1 FROM kna1
               INTO TABLE lt_kna1.

  IF sy-subrc EQ 0.
    SORT lt_kna1[] BY kunnr.
  ENDIF.

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



  LOOP AT lt_excel INTO ls_excel.
*********Conversion for Customer Number *************
    IF ls_excel-customer_no IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-customer_no
        IMPORTING
          output = ls_excel-customer_no.
      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.

    ENDIF.
*********Conversion for Distributor_1 *****************
    IF ls_excel-distributor_1 IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-distributor_1
        IMPORTING
          output = ls_excel-distributor_1.
      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.

    ENDIF.
*********Conversion for Distributor_2 *****************
    IF ls_excel-distributor_2 IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-distributor_2
        IMPORTING
          output = ls_excel-distributor_2.
      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.

    ENDIF.
*********Conversion for Distributor_3 *****************
    IF ls_excel-distributor_3 IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-distributor_3
        IMPORTING
          output = ls_excel-distributor_3.
      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.

    ENDIF.

*****Check for customer_no **********
    IF ls_excel-customer_no IS INITIAL.
      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.
      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-message = 'Invalid Customer Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*****Check for customer_no **********
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-customer_no.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.
      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-message = 'Invalid Customer Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*****Check for Distributor 1 code **********
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-distributor_1.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.

      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-customer_no.
      IF sy-subrc EQ 0.
        ls_disp-customer_name = ls_kna1-name1.
      ENDIF.

      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-message = 'Invalid distributor_1 Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*****Check for Distributor 2 code **********
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-distributor_2.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.
      ls_disp-customer_name = ls_kna1-name1.
      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-message = 'Invalid distributor_2 Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*****Check for Distributor 3 code **********
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-distributor_3.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.
      ls_disp-customer_name = ls_kna1-name1.
      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-message = 'Invalid distributor_3 Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*******Updating Availabe Customer Data in ZSKU_TABLE **********
    CLEAR ls_excel_temp.
    READ TABLE lt_excel_temp INTO ls_excel_temp WITH KEY customer_no = ls_excel-customer_no.

    IF sy-subrc EQ 0 .

      ls_excel_temp-distributor_1 = ls_excel-distributor_1. " Distributor 1
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_1.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme1 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-distributor_2 = ls_excel-distributor_2.  " Distributor 2
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_2.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme2 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-distributor_3 = ls_excel-distributor_3.  " Distributor 3
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_3.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme3 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-created_by = sy-uname.
      ls_excel_temp-create_on = sy-datum.
      ls_excel_temp-changed_by = sy-uname.
      ls_excel_temp-changed_on = sy-datum.
      MODIFY zsku_table FROM ls_excel_temp.
      COMMIT WORK AND WAIT.

      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.
      ls_disp-customer_name = ls_kna1-name1.
      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor_name1 =  ls_excel_temp-distributor_nme1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor_name2 =  ls_excel_temp-distributor_nme2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-distributor_name3 =  ls_excel_temp-distributor_nme3.
      ls_disp-message = 'Existing Customer Number updated'.
      APPEND ls_disp TO lt_disp.

***********Creating New Customer Data in ZKU_TABLE ****************
    ELSE.
      ls_excel_temp-customer_no = ls_excel-customer_no.
      ls_excel_temp-distributor_1 = ls_excel-distributor_1. " Distributor 1
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_1.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme1 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-distributor_2 = ls_excel-distributor_2.  " Distributor 2
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_2.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme2 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-distributor_3 = ls_excel-distributor_3.  " Distributor 3
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY  kunnr = ls_excel-distributor_3.
      IF sy-subrc EQ 0.
        ls_excel_temp-distributor_nme3 = ls_kna1-name1.
      ENDIF.

      ls_excel_temp-created_by = sy-uname.
      ls_excel_temp-create_on = sy-datum.
      ls_excel_temp-changed_by = sy-uname.
      ls_excel_temp-changed_on = sy-datum.
      MODIFY zsku_table FROM ls_excel_temp.
      COMMIT WORK AND WAIT.

      CLEAR ls_disp.
      ls_disp-customer_no = ls_excel-customer_no.

      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-customer_no.
      IF sy-subrc EQ 0.
        ls_disp-customer_name = ls_kna1-name1.
      ENDIF.

      ls_disp-distributor1 = ls_excel-distributor_1.
      ls_disp-distributor_name1 =  ls_excel_temp-distributor_nme1.
      ls_disp-distributor2 = ls_excel-distributor_2.
      ls_disp-distributor_name2 =  ls_excel_temp-distributor_nme2.
      ls_disp-distributor3 = ls_excel-distributor_3.
      ls_disp-distributor_name3 =  ls_excel_temp-distributor_nme3.
      ls_disp-message = 'New Customer Number Created'.
      APPEND ls_disp TO lt_disp.

    ENDIF.

  ENDLOOP.

************ ALV Display*********************************

  REFRESH: lt_fcat.

  PERFORM f_fieldcat USING 'CUSTOMER_NO'  'Customer Number' 1 space.
  PERFORM f_fieldcat USING 'DISTRIBUTOR1' 'Distributor_1'   2 space.
  PERFORM f_fieldcat USING 'DISTRIBUTOR2' 'Distributor_2'   3 space.
  PERFORM f_fieldcat USING 'DISTRIBUTOR3' 'Distributor_3'   4 space.
  PERFORM f_fieldcat USING 'MESSAGE'      'Message'         5 space.

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
