*&---------------------------------------------------------------------*
*& Report ZSKU_DISTRIBUTOR_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*          Program to upload Partner Function Mapping
* With change Document History for Automatic Mapping through ZSKU_DIST_MAP_DAILY
" Created By : Samsudeen M
" Created_on : 09.09.2022
*-----------------------------------------------------------------------------*
REPORT zsku_distributor_upload.

INCLUDE zknvp_upload_top.

*  Selection Screen Input *
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"Help for File name in Selection Screen

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_help.

START-OF-SELECTION.
  PERFORM convert_data.
  PERFORM data_selection.
  PERFORM data_updation.
  PERFORM alv_display.

*&---------------------------------------------------------------------*
*& Form f4_help
*&---------------------------------------------------------------------*
FORM f4_help .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'p_file'
    IMPORTING
      file_name     = p_file.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_data
*&---------------------------------------------------------------------*
FORM convert_data .
  REFRESH: lt_upload.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = lt_type
      i_filename           = p_file
    TABLES
      i_tab_converted_data = lt_upload.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form data_selection
*&---------------------------------------------------------------------*

FORM data_selection .
  REFRESH: lt_kna1,lt_knvp.
* Customer Master Selection  *
  SELECT kunnr
         name1 FROM kna1
               INTO TABLE lt_kna1.
  IF sy-subrc EQ 0.
    SORT lt_kna1[] BY kunnr.
  ENDIF.

* Pernr Selection For checks *
  SELECT pernr FROM pa0000
               INTO TABLE lt_pa0000
               WHERE stat2 = '3'.
  IF sy-subrc EQ 0.
    SORT lt_pa0000[] BY pernr.
* Pernr Name Selection for ALV display *
    SELECT pernr
           vorna FROM pa0002
                 INTO TABLE lt_pa0002
                 FOR ALL ENTRIES IN lt_pa0000
                 WHERE pernr = lt_pa0000-pernr.
    IF sy-subrc EQ 0.
      SORT lt_pa0002[] BY pernr.
    ENDIF.
  ENDIF.
* Data selection from LFA1 for checks *
  SELECT lifnr
         name1 FROM lfa1
               INTO TABLE lt_lfa1.
  IF sy-subrc EQ 0.
    SORT lt_lfa1[] BY lifnr.
  ENDIF.
* Data Selection From Knvp for checks *
  SELECT * FROM knvp
           INTO TABLE lt_knvp.
  IF sy-subrc EQ 0.
    SORT lt_knvp[] BY kunnr vkorg vtweg spart parvw.
  ENDIF.
* Partner function selections *
  SELECT * FROM tpaum
           INTO TABLE lt_tpaum
           WHERE spras EQ sy-langu.
  IF sy-subrc EQ 0.
    SORT lt_tpaum[] BY parvw.
  ENDIF.
* Sales Organization selection *
  SELECT vkorg FROM tvko
               INTO TABLE lt_tvko.
  IF sy-subrc EQ 0.
    SORT lt_tvko[] BY vkorg.
* Sales Org Distribution Channel *
    SELECT vkorg
           vtweg FROM tvkov
                 INTO TABLE lt_tvkov
                 FOR ALL ENTRIES IN lt_tvko
                 WHERE vkorg = lt_tvko-vkorg.
    IF sy-subrc EQ 0.
      SORT lt_tvkov[] BY vkorg vtweg.
    ENDIF.
* Sales Org Division *
    SELECT vkorg
           spart FROM tvkos
                 INTO TABLE lt_tvkos
                 FOR ALL ENTRIES IN lt_tvko
    WHERE vkorg = lt_tvko-vkorg.
    IF sy-subrc EQ 0.
      SORT lt_tvkos[] BY vkorg spart.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form data_updation
*&---------------------------------------------------------------------*
FORM data_updation.

*Convert Customer No
  LOOP AT lt_upload INTO ls_upload.

    IF ls_upload-kunnr IS NOT INITIAL.
* to convert to KUNNR format to proper format
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_upload-kunnr
        IMPORTING
          output = ls_upload-kunnr.
    ENDIF.

    IF ls_upload-kunn2 IS NOT INITIAL.
* to convert to KUNN2 format to proper format
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_upload-kunn2
        IMPORTING
          output = ls_upload-kunn2.
    ENDIF.

    IF ls_upload-lifnr IS NOT INITIAL.
* to convert to LIFNR format to proper format
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_upload-lifnr
        IMPORTING
          output = ls_upload-lifnr.
    ENDIF.

    MODIFY lt_upload FROM ls_upload INDEX sy-tabix.
  ENDLOOP.

  CLEAR ls_upload.
  LOOP AT lt_upload INTO ls_upload.
    CLEAR ls_knvp.
** Customer Number Checks if not present
    IF ls_upload-kunnr IS INITIAL.

      CLEAR ls_disp.
      ls_disp-kunnr = ls_upload-kunnr.
      ls_disp-vkorg = ls_upload-vkorg.
      ls_disp-vtweg = ls_upload-vtweg.
      ls_disp-spart = ls_upload-spart.
      ls_disp-parvw = ls_upload-parvw.
      ls_disp-kunn2 = ls_upload-kunn2.
      ls_disp-message = 'Invalid Customer Number'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.

    ELSE.
** Customer Number Checks if present
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-kunn2 = ls_upload-kunn2.
        ls_disp-message = 'Invalid Customer Number'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.

** Checks For Sales Org **
    CLEAR ls_tvko.
    READ TABLE lt_tvko INTO ls_tvko WITH KEY vkorg = ls_upload-vkorg BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-kunnr = ls_upload-kunnr.
      ls_disp-vkorg = ls_upload-vkorg.
      ls_disp-vtweg = ls_upload-vtweg.
      ls_disp-spart = ls_upload-spart.
      ls_disp-parvw = ls_upload-parvw.
      ls_disp-kunn2 = ls_upload-kunn2.
      ls_disp-message = 'No Sales Org Available in SAP'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

**Checks for Sales Org Distribution Channel **
    CLEAR ls_tvkov.
    READ TABLE lt_tvkov INTO ls_tvkov WITH KEY vkorg = ls_upload-vkorg
                                               vtweg = ls_upload-vtweg BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-kunnr = ls_upload-kunnr.
      ls_disp-vkorg = ls_upload-vkorg.
      ls_disp-vtweg = ls_upload-vtweg.
      ls_disp-spart = ls_upload-spart.
      ls_disp-parvw = ls_upload-parvw.
      ls_disp-kunn2 = ls_upload-kunn2.
      ls_disp-message = 'No Dist Channel under Sales Org in SAP'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

** Checks for Sales Org Division **
    CLEAR ls_tvkos.
    READ TABLE lt_tvkos INTO ls_tvkos WITH KEY vkorg = ls_upload-vkorg
                                               spart = ls_upload-spart BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-kunnr = ls_upload-kunnr.
      ls_disp-vkorg = ls_upload-vkorg.
      ls_disp-vtweg = ls_upload-vtweg.
      ls_disp-spart = ls_upload-spart.
      ls_disp-parvw = ls_upload-parvw.
      ls_disp-kunn2 = ls_upload-kunn2.
      ls_disp-message = 'No Division under Sales Org in SAP'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

**Checks for Partner Function **
    CLEAR ls_tpaum.
    READ TABLE lt_tpaum INTO ls_tpaum WITH KEY parvw = ls_upload-parvw BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_disp.
      ls_disp-kunnr = ls_upload-kunnr.
      ls_disp-vkorg = ls_upload-vkorg.
      ls_disp-vtweg = ls_upload-vtweg.
      ls_disp-spart = ls_upload-spart.
      ls_disp-parvw = ls_upload-parvw.
      ls_disp-kunn2 = ls_upload-kunn2.
      ls_disp-message = 'Invalid Partner Function'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.
** Checks for KUNN2 if presents **
    IF ls_upload-kunn2 IS NOT INITIAL.
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunn2 BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-kunn2 = ls_upload-kunn2.
        ls_disp-message = 'Invalid Distributor Number'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.

** Checks for pernr if Presents *
    IF ls_upload-pernr IS NOT INITIAL.
      CLEAR ls_pa0000.
      READ TABLE lt_pa0000 INTO ls_pa0000 WITH KEY pernr = ls_upload-pernr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-pernr = ls_upload-pernr.
        ls_disp-message = 'Invalid Employee Number'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.

* Checks for Vendor if Presents *
    IF ls_upload-lifnr IS NOT INITIAL.
      CLEAR ls_lfa1.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_upload-lifnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-pernr = ls_upload-pernr.
        ls_disp-message = 'Invalid Vendor Number'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.
    ENDIF.

*  Updating through Function Module with Change Document  *
    CLEAR ls_knvp.
    READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_upload-kunnr
                                             vkorg = ls_upload-vkorg
                                             vtweg = ls_upload-vtweg
                                             spart = ls_upload-spart
                                             parvw = ls_upload-parvw BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ls_upload-kunn2 IS NOT INITIAL.
***Distributor Updation ***
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_knvp-kunnr.
        ls_fm_knvp-vkorg = ls_knvp-vkorg.
        ls_fm_knvp-vtweg = ls_knvp-vtweg.
        ls_fm_knvp-spart = ls_knvp-spart.
        ls_fm_knvp-parvw = ls_knvp-parvw.
        ls_fm_knvp-kunn2 = ls_upload-kunn2.
        ls_fm_knvp-kz = 'U'.  " Update Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-kunn2 = ls_upload-kunn2.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunn2 BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-dist_name = ls_kna1-name1.
        ENDIF.
        ls_disp-message = 'Existing Distributor Mapping Updated'.
        APPEND ls_disp TO lt_disp.

      ELSEIF ls_upload-pernr IS NOT INITIAL.
**Pernr Updation**
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_knvp-kunnr.
        ls_fm_knvp-vkorg = ls_knvp-vkorg.
        ls_fm_knvp-vtweg = ls_knvp-vtweg.
        ls_fm_knvp-spart = ls_knvp-spart.
        ls_fm_knvp-parvw = ls_knvp-parvw.
        ls_fm_knvp-pernr = ls_upload-pernr.
        ls_fm_knvp-kz = 'U'.  " Update Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-pernr = ls_upload-pernr.
        CLEAR ls_pa0002.
        READ TABLE lt_pa0002 INTO ls_pa0002 WITH KEY pernr = ls_upload-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-empname = ls_pa0002-vorna.
        ENDIF.
        ls_disp-message = 'Existing Employee Mapping Updated'.
        APPEND ls_disp TO lt_disp.

      ELSEIF ls_upload-lifnr IS NOT INITIAL.
**Vendor Updation **
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_knvp-kunnr.
        ls_fm_knvp-vkorg = ls_knvp-vkorg.
        ls_fm_knvp-vtweg = ls_knvp-vtweg.
        ls_fm_knvp-spart = ls_knvp-spart.
        ls_fm_knvp-parvw = ls_knvp-parvw.
        ls_fm_knvp-lifnr = ls_upload-lifnr.
        ls_fm_knvp-kz = 'U'.  " Update Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.


        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-lifnr = ls_upload-lifnr.
        CLEAR ls_lfa1.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_upload-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-venname = ls_lfa1-name1.
        ENDIF.
        ls_disp-message = 'Existing Vendor Mapping Updated'.
        APPEND ls_disp TO lt_disp.

      ENDIF.
***Function module to update KNVP Mapping in BP
      CALL FUNCTION 'CUSTOMER_UPDATE_SALES_AREA'
        TABLES
          t_xknvp = lt_fm_knvp[]
          t_yknvp = lt_fm_knvp[].

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.


    ELSE.

* Creating New record
      IF ls_upload-kunn2 IS NOT INITIAL.
**Distributor New Record Creation
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_upload-kunnr.
        ls_fm_knvp-vkorg = ls_upload-vkorg.
        ls_fm_knvp-vtweg = ls_upload-vtweg.
        ls_fm_knvp-spart = ls_upload-spart.
        ls_fm_knvp-parvw = ls_upload-parvw.
        ls_fm_knvp-kunn2 = ls_upload-kunn2.
        ls_fm_knvp-kz = 'I'. "Insert Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-kunn2 = ls_upload-kunn2.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunn2 BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-dist_name = ls_kna1-name1.
        ENDIF.
        ls_disp-message = 'New Distributor Mapping Created'.
        APPEND ls_disp TO lt_disp.

      ELSEIF ls_upload-pernr IS NOT INITIAL.
**Pernr Mapping New Record Creation **
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_upload-kunnr.
        ls_fm_knvp-vkorg = ls_upload-vkorg.
        ls_fm_knvp-vtweg = ls_upload-vtweg.
        ls_fm_knvp-spart = ls_upload-spart.
        ls_fm_knvp-parvw = ls_upload-parvw.
        ls_fm_knvp-pernr = ls_upload-pernr.
        ls_fm_knvp-kz = 'I'. "Insert Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-pernr = ls_upload-pernr.
        CLEAR ls_pa0002.
        READ TABLE lt_pa0002 INTO ls_pa0002 WITH KEY pernr = ls_upload-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-empname = ls_pa0002-vorna.
        ENDIF.
        ls_disp-message = 'New Pernr Mapping Created'.
        APPEND ls_disp TO lt_disp.

      ELSEIF ls_upload-lifnr IS NOT INITIAL.
**Vendor New mapping Record Creation **
        REFRESH lt_fm_knvp.
        CLEAR ls_fm_knvp.
        ls_fm_knvp-kunnr = ls_upload-kunnr.
        ls_fm_knvp-vkorg = ls_upload-vkorg.
        ls_fm_knvp-vtweg = ls_upload-vtweg.
        ls_fm_knvp-spart = ls_upload-spart.
        ls_fm_knvp-parvw = ls_upload-parvw.
        ls_fm_knvp-lifnr = ls_upload-lifnr.
        ls_fm_knvp-kz = 'I'. "Insert Indication
        APPEND ls_fm_knvp TO lt_fm_knvp.

        CLEAR ls_disp.
        ls_disp-kunnr = ls_upload-kunnr.
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_upload-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-name1 = ls_kna1-name1.
        ENDIF.
        ls_disp-vkorg = ls_upload-vkorg.
        ls_disp-vtweg = ls_upload-vtweg.
        ls_disp-spart = ls_upload-spart.
        ls_disp-parvw = ls_upload-parvw.
        ls_disp-lifnr = ls_upload-lifnr.
        CLEAR ls_lfa1.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_upload-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_disp-venname = ls_lfa1-name1.
        ENDIF.
        ls_disp-message = 'New Vendor Mapping Created'.
        APPEND ls_disp TO lt_disp.

      ENDIF.
**Function Module Create new Sales Org Mapping Record
      CALL FUNCTION 'CUSTOMER_UPDATE_SALES_AREA'
        TABLES
          t_xknvp = lt_fm_knvp[]
          t_yknvp = lt_fm_knvp[].

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
FORM alv_display .

  REFRESH lt_fcat.
  PERFORM f_fieldcat USING  'KUNNR'     'Customer Number'  1 space.
  PERFORM f_fieldcat USING  'NAME1'     'Customer Name'    2 space.
  PERFORM f_fieldcat USING  'VKORG'     'Sales Org'        3 space.
  PERFORM f_fieldcat USING  'VTWEG'     'Distr Chan'       4 space.
  PERFORM f_fieldcat USING  'SPART'     'Division'         5 space.
  PERFORM f_fieldcat USING  'PARVW'     'Partner Func'     6 space.
  PERFORM f_fieldcat USING  'KUNN2'     'Distributor'      7 space.
  PERFORM f_fieldcat USING  'DIST_NAME' 'Dist Name'        8 space.
  PERFORM f_fieldcat USING  'LIFNR'     'Vendor Number'    9 space.
  PERFORM f_fieldcat USING  'VENNAME'   'Vendor Name'     10 space.
  PERFORM f_fieldcat USING  'PERNR'     'Employee No'     11 space.
  PERFORM f_fieldcat USING  'EMPNAME'   'Employee Name'   12 space.
  PERFORM f_fieldcat USING  'MESSAGE'   'Message'         13 space.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_grid
        CHANGING
          t_table      = lt_disp[].
    CATCH cx_salv_msg.

  ENDTRY.
  "For Menus In ALV Display
  lo_grid->set_screen_status(
          pfstatus      =  'STANDARD_FULLSCREEN'
          report        =  'SAPLKKBL'
          set_functions = lo_grid->c_functions_all ).

  CALL METHOD lo_grid->display.

ENDFORM.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
