*&---------------------------------------------------------------------*
*& Report ZEXCEL_UPLOAD
*Date Created - 14.03.2022
*& Created By - KPABAP (Shamsudeen)
*& Description - Program for excel upload in tbale (ZSALES_TABLE) customer
*                 commitment upload  based on Region and customer type.
*&
*
*  TR No    -     DEVK931587, DEVK931608
*&---------------------------------------------------------------------*
* Change Date   - 21.04.2022
* TR NO         - DEVK931608
* Changed By    - Shamsudeen (KPABAP)
* Reference     - Discussion with Ramakrishnan
* Description   - Add (ZMONTH AND FISCAL YEAR) Field in upload template for commitment upload
*                 month on month basis
*&---------------------------------------------------------------------*
REPORT zexcel_upload NO STANDARD PAGE HEADING.


* INPUT PARAMETER IN SELECTION_SCREEN***************
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk.

***********EXCEL INPUT TEMPLATE STRUCTURE***********
TYPES: BEGIN OF ty_str,              "Excel input template structure
         kunnr          TYPE kunnr,
         matkl          TYPE matkl,
         gjahr(4)       TYPE n,
         zmonths(2)     TYPE n,
*         zeinr          TYPE dzeinr,
         commitment(10) TYPE n,
       END OF ty_str,

       BEGIN OF lty_disp,               " upload status message structure
         kunnr          TYPE kunnr,
         name1          TYPE name1_gp,
         matkl          TYPE matkl,
         wgbez60        TYPE wgbez60,
         gjahr          TYPE gjahr,
         zmonths        TYPE zmonths,
         zmonth_des(12) TYPE c,
*         zeinr          TYPE dzeinr,
         regio          TYPE regio,
         katr6          TYPE katr6,
         commitment     TYPE zcommitment,
         message(100)   TYPE c,
       END OF lty_disp,
       BEGIN OF lty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         regio TYPE regio,
         katr6 TYPE katr6,
       END OF lty_kna1,
       BEGIN OF lty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF lty_fields.

DATA: lt_type      TYPE truxs_t_text_data,
      lt_excel     TYPE TABLE OF ty_str,
      lt_excel_tmp TYPE TABLE OF ty_str,
      ls_excel     TYPE ty_str,
      lt_disp      TYPE STANDARD TABLE OF lty_disp,
      ls_disp      TYPE lty_disp,
      lt_kna1      TYPE STANDARD TABLE OF lty_kna1,
      ls_kna1      TYPE lty_kna1,
      lt_t023t     TYPE STANDARD TABLE OF t023t,
      ls_t023t     TYPE t023t,
      lt_fields    TYPE STANDARD TABLE OF lty_fields,
      ls_fields    TYPE lty_fields,
      lt_fcat      TYPE lvc_t_fcat,
      ls_fcat      TYPE lvc_s_fcat,
      ls_layout    TYPE lvc_s_layo.
DATA: lt_sales      TYPE STANDARD TABLE OF zsales_table,
      ls_sales      TYPE zsales_table.
*      lt_zmm_zeinr  TYPE STANDARD TABLE OF zmm_zeinr,
*      ls_zmm_zeinr  TYPE zmm_zeinr.


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

****************
  SELECT * FROM zsales_table
           INTO TABLE lt_sales.

*  SELECT * FROM zmm_zeinr INTO TABLE lt_zmm_zeinr.


******updating custom table********



***********SELECT MATERIAL GRP AND MATERIAL GRP DESC*****************
  SELECT * FROM t023t
           INTO TABLE lt_t023t
           WHERE spras = sy-langu.



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


*Convert Customer No
  LOOP AT lt_excel INTO ls_excel.
    IF ls_excel-kunnr IS NOT INITIAL.
* to convert to KUNNR format to proper format
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_excel-kunnr
        IMPORTING
          output = ls_excel-kunnr.

      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  IF lt_excel[] IS NOT INITIAL.
    lt_excel_tmp[] = lt_excel.
    SORT  lt_excel_tmp BY kunnr.
    DELETE ADJACENT DUPLICATES FROM LT_EXCEL_tmp COMPARING kunnr.

* Select all from the customer master based on the
    SELECT kunnr name1 regio katr6 FROM kna1
      INTO TABLE lt_kna1
      FOR ALL ENTRIES IN lt_excel_tmp
      WHERE kunnr = lt_excel_tmp-kunnr.

    SELECT * FROM zsales_table
      INTO TABLE lt_sales
      FOR ALL ENTRIES IN lt_excel_tmp
      WHERE kunnr = lt_excel_tmp-kunnr.
    ENDIF.


****** Update database table
DATA: zmonth_des(12) TYPE c.

    LOOP AT lt_excel INTO ls_excel.

      IF ls_excel-kunnr IS  INITIAL.
*put error here
        CLEAR ls_disp.
        ls_disp-kunnr   = ls_excel-kunnr.
        ls_disp-matkl   = ls_excel-matkl.
        ls_disp-gjahr   = ls_excel-gjahr.
        ls_disp-zmonths = ls_excel-zmonths.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'Invalid Customer No'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.


      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_excel-kunnr.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr   = ls_excel-kunnr.
        ls_disp-matkl   = ls_excel-matkl.
        ls_disp-gjahr   = ls_excel-gjahr.
        ls_disp-zmonths = ls_excel-zmonths.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'Invalid Customer No'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.

      READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_excel-matkl.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr   = ls_excel-kunnr.
        ls_disp-name1 = ls_kna1-name1.
        ls_disp-matkl   = ls_excel-matkl.
        ls_disp-gjahr   = ls_excel-gjahr.
        ls_disp-zmonths = ls_excel-zmonths.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-regio = ls_kna1-regio.
        ls_disp-katr6 = ls_kna1-katr6.
        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'Invalid Material Group'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.

*      READ TABLE lt_zmm_zeinr INTO ls_zmm_zeinr WITH KEY zeinr = ls_excel-zeinr.
*      IF sy-subrc ne 0.
*        CLEAR ls_disp.
*        ls_disp-kunnr   = ls_excel-kunnr.
*        ls_disp-name1 = ls_kna1-name1.
*        ls_disp-matkl   = ls_excel-matkl.
*        ls_disp-gjahr   = ls_excel-gjahr.
*        ls_disp-zmonths = ls_excel-zmonths.
*        ls_disp-zeinr   = ls_excel-zeinr.
*        ls_disp-regio = ls_kna1-regio.
*        ls_disp-katr6 = ls_kna1-katr6.
*        ls_disp-commitment = ls_excel-commitment.
*        ls_disp-message = 'Invalid Document'.
*        APPEND ls_disp TO lt_disp.
*        CONTINUE.
*      ENDIF.

      IF ls_excel-zmonths NOT BETWEEN '01' AND '12'.
        CLEAR ls_disp.
        ls_disp-kunnr   = ls_excel-kunnr.
        ls_disp-name1 = ls_kna1-name1.
        ls_disp-matkl   = ls_excel-matkl.
        ls_disp-gjahr   = ls_excel-gjahr.
        ls_disp-zmonths = ls_excel-zmonths.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-regio = ls_kna1-regio.
        ls_disp-katr6 = ls_kna1-katr6.
        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'Month Value Incorrect'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.

      IF ls_excel-zmonths = '01'.
        zmonth_des = 'April'.
      ELSEIF ls_excel-zmonths = '02'.
        zmonth_des = 'May'.
      ELSEIF ls_excel-zmonths = '03'.
        zmonth_des = 'June'.
      ELSEIF ls_excel-zmonths = '04'.
        zmonth_des = 'July'.
      ELSEIF ls_excel-zmonths = '05'.
        zmonth_des = 'August'.
      ELSEIF ls_excel-zmonths = '06'.
        zmonth_des = 'September'.
      ELSEIF ls_excel-zmonths = '07'.
        zmonth_des = 'October'.
      ELSEIF ls_excel-zmonths = '08'.
        zmonth_des = 'November'.
      ELSEIF ls_excel-zmonths = '09'.
        zmonth_des = 'December'.
      ELSEIF ls_excel-zmonths = '10'.
        zmonth_des = 'Janauary'.
      ELSEIF ls_excel-zmonths = '11'.
        zmonth_des = 'Febraury'.
      ELSEIF ls_excel-zmonths = '12'.
        zmonth_des = 'March'.
      ENDIF.


*ls_disp-zmonth_des = 'APRIL'.
** Updating already available customer record from excel********
      READ TABLE lt_sales INTO ls_sales WITH KEY kunnr = ls_excel-kunnr
                                                 matkl = ls_excel-matkl
                                                 gjahr = ls_excel-gjahr
                                                 zmonths = ls_excel-zmonths.
      IF sy-subrc = 0.
*        ls_sales-zeinr = ls_excel-zeinr.
        ls_sales-commitment = ls_excel-commitment.
        ls_sales-aenam = sy-uname.
        ls_sales-aedat = sy-datum.
        MODIFY zsales_table FROM ls_sales.
        COMMIT WORK AND WAIT.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_sales-kunnr.
        ls_disp-name1 = ls_kna1-name1.
        ls_disp-matkl = ls_sales-matkl.
        ls_disp-wgbez60 = ls_t023t-wgbez60.
        ls_disp-gjahr = ls_sales-gjahr.
        ls_disp-zmonths = ls_sales-zmonths.
        ls_disp-zmonth_des = zmonth_des.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-regio = ls_kna1-regio.
        ls_disp-katr6 = ls_kna1-katr6.

        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'Existing Customer Data updated Successfully'.
        APPEND ls_disp TO lt_disp.
      ELSE.
** Creating new record in Table from excel file.
        ls_sales-kunnr = ls_excel-kunnr.
        ls_sales-matkl = ls_excel-matkl.
        ls_sales-gjahr = ls_excel-gjahr.
        ls_sales-zmonths = ls_excel-zmonths.
        ls_sales-regio = ls_kna1-regio.
        ls_sales-katr6 = ls_kna1-katr6.
*        ls_sales-zeinr = ls_excel-zeinr.
        ls_sales-commitment = ls_excel-commitment.
        ls_sales-ernam = sy-uname.
        ls_sales-erdat = sy-datum.
        ls_sales-aenam = sy-uname.
        ls_sales-aedat = sy-datum.
        MODIFY zsales_table FROM ls_sales.
        COMMIT WORK AND WAIT.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_excel-kunnr.
        ls_disp-name1 = ls_kna1-name1.
        ls_disp-matkl = ls_excel-matkl.
        ls_disp-wgbez60 = ls_t023t-wgbez60.
        ls_disp-gjahr = ls_excel-gjahr.
        ls_disp-zmonths = ls_excel-zmonths.
        ls_disp-zmonth_des = zmonth_des.
*        ls_disp-zeinr   = ls_excel-zeinr.
        ls_disp-regio = ls_kna1-regio.
        ls_disp-katr6 = ls_kna1-katr6.
        ls_disp-commitment = ls_excel-commitment.
        ls_disp-message = 'New Record Uploaded Successfully'.
        APPEND ls_disp TO lt_disp.
      ENDIF.

    ENDLOOP.


    REFRESH lt_fcat.

    PERFORM f_fieldcat USING  'KUNNR' 'Customer Number' 1 space.
    PERFORM f_fieldcat USING  'NAME1' 'Customer Name' 2 space.
    PERFORM f_fieldcat USING  'MATKL' 'Material Grp' 3 space.
    PERFORM f_fieldcat USING  'WGBEZ60' 'Material Grp Desc' 4 space.
    PERFORM f_fieldcat USING  'GJAHR' 'Fiscal year' 5 space.
    PERFORM f_fieldcat USING  'ZMONTHS' 'Month' 6 space.
    PERFORM f_fieldcat USING  'ZMONTH_DES' 'Month desc' 7 space.
    PERFORM f_fieldcat USING  'REGIO' 'Region' 8 space.
    PERFORM f_fieldcat USING  'KATR6' 'Custom Region' 9 space.
    PERFORM f_fieldcat USING  'COMMITMENT' 'User Target' 10 space.
    PERFORM f_fieldcat USING  'MESSAGE' ' Status Message' 11 space.

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
