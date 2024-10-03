*&---------------------------------------------------------------------*
*& Report ZUPDATE_BP_PF
*&---------------------------------------------------------------------*
*&Program to update the Partner functions. This program is used to directly
* update the KNVP table using modify statement

* Mutiple checks have been entered in the program to validate the data
* also code to write the change docuemnt to CDHDR and CDPOS has been also
* entered
*&---------------------------------------------------------------------*
*	  DEVK934428 - SAP SD: Partner function update program - changes for CDHDR
*	  DEVK934439 - SAP SD: Partner function update program - chngs for CDHDR 2

REPORT zupdate_bp_pf_tmpwoval.

TYPE-POOLS : truxs.

TABLES : knvp.

TYPES: BEGIN OF ty_excel,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
         spart TYPE spart,
         parvw TYPE parvw,
         kunn2 TYPE kunnr,
         lifnr TYPE lifnr,
         pernr TYPE pernr_d,
       END OF ty_excel,

       BEGIN OF lty_kna1,
         kunnr TYPE kunnr,
       END OF lty_kna1,

       BEGIN OF lty_pa0001,
         pernr TYPE persno,
         ename TYPE ename,
       END OF lty_pa0001,

       BEGIN OF lty_lfa1,
         lifnr TYPE lifnr,
       END OF lty_lfa1,

       BEGIN OF lty_disp,
         kunnr        TYPE kunnr,
         vkorg        TYPE vkorg,
         vtweg        TYPE vtweg,
         spart        TYPE spart,
         parvw        TYPE parvw,
         kunn2        TYPE kunnr,
         lifnr        TYPE lifnr,
         pernr        TYPE pernr_d,
         message(100) TYPE c,
       END OF lty_disp.

DATA: lt_excel     TYPE TABLE OF ty_excel,
      ls_excel     TYPE ty_excel,
      lt_excel_tmp TYPE TABLE OF ty_excel,
      lt_kna1      TYPE STANDARD TABLE OF lty_kna1,
      ls_kna1      TYPE lty_kna1,
      lt_pa0001    TYPE STANDARD TABLE OF lty_pa0001,
      ls_pa0001    TYPE lty_pa0001,
      lt_lfa1      TYPE STANDARD TABLE OF lty_lfa1,
      ls_lfa1      TYPE lty_lfa1,
      lt_tpart     TYPE STANDARD TABLE OF tpaum,
      ls_tpart     TYPE tpaum,
      lt_disp      TYPE STANDARD TABLE OF lty_disp,
      ls_disp      TYPE lty_disp,
      lt_fcat      TYPE lvc_t_fcat,
      ls_fcat      TYPE lvc_s_fcat,
      ls_layout    TYPE lvc_s_layo.

DATA : lt_rawdata TYPE truxs_t_text_data.
DATA: lt_knvp TYPE STANDARD TABLE OF knvp,
      ls_knvp TYPE knvp.

DATA: lt_yknvp TYPE TABLE OF fknvp,
      lt_Xknvp TYPE TABLE OF fknvp,
      ls_xknvp TYPE fknvp,
      ls_yknvp TYPE fknvp.

DATA: lv_OBJECTID TYPE cdhdr-objectid.

DATA: lt_tpar TYPE TABLE OF tpar,
      ls_tpar TYPE tpar.

DATA: lo_grid TYPE REF TO cl_salv_table.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS : file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'FILE'
    IMPORTING
      file_name     = file.

START-OF-SELECTION.


  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = lt_rawdata
      i_filename           = file
    TABLES
      i_tab_converted_data = lt_excel.



END-OF-SELECTION.

*  LOOP AT lt_excel INTO ls_excel.
*    IF ls_excel-kunnr IS NOT INITIAL.
** to convert to KUNNR format to proper format
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ls_excel-kunnr
*        IMPORTING
*          output = ls_excel-kunnr.
*
*      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.
*    ENDIF.
*
*    IF ls_excel-kunn2 IS NOT INITIAL.
** to convert to KUNNR format to proper format
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ls_excel-kunn2
*        IMPORTING
*          output = ls_excel-kunn2.
*
*      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.
*    ENDIF.
*
*    IF ls_excel-lifnr IS NOT INITIAL.
** to convert to LIFNR format to proper format
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ls_excel-lifnr
*        IMPORTING
*          output = ls_excel-lifnr.
*
*      MODIFY lt_excel FROM ls_excel INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.

  IF lt_excel[] IS NOT INITIAL.



    REFRESH lt_excel_tmp.
    lt_excel_tmp[] = lt_excel.
    SORT  lt_excel_tmp BY kunnr.
    DELETE ADJACENT DUPLICATES FROM LT_EXCEL_tmp COMPARING kunnr.

* Select all from the customer master based on the provided customer number
    SELECT kunnr FROM kna1
      INTO TABLE lt_kna1 FOR ALL ENTRIES IN lt_excel_tmp WHERE kunnr = lt_excel_tmp-kunnr.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.


    REFRESH lt_excel_tmp.
    lt_excel_tmp[] = lt_excel.
    SORT  lt_excel_tmp BY kunnr vkorg vtweg spart.
    DELETE ADJACENT DUPLICATES FROM LT_EXCEL_tmp COMPARING kunnr vkorg vtweg spart.


    SELECT * FROM knvp
      INTO TABLE lt_knvp
      FOR ALL ENTRIES IN lt_excel_tmp
      WHERE kunnr = lt_excel_tmp-kunnr
        AND vkorg = lt_excel_tmp-vkorg
        AND vtweg = lt_excel_tmp-vtweg
        AND spart = lt_excel_tmp-spart.
    IF sy-subrc = 0.
      SORT lt_knvp BY kunnr vkorg vtweg spart parvw.
    ENDIF.

  ENDIF.


  SORT lt_excel BY kunnr vkorg vtweg spart parvw.


  LOOP AT lt_excel INTO ls_excel.

    CLEAR ls_disp.
    ls_disp-kunnr   = ls_excel-kunnr.
    ls_disp-vkorg   = ls_excel-vkorg.
    ls_disp-vtweg   = ls_excel-vtweg.
    ls_disp-spart   = ls_excel-spart.
    ls_disp-parvw   = ls_excel-parvw.
    ls_disp-kunn2   = ls_excel-kunn2.
    ls_disp-lifnr   = ls_excel-lifnr.
    ls_disp-pernr   = ls_excel-pernr.

    IF ls_excel-kunnr IS  INITIAL.
*********Customer number check***************
      ls_disp-message = 'Invalid Customer No'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.

*********Customer number check***************
    READ TABLE lt_kna1 WITH KEY kunnr = ls_excel-kunnr BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_disp-message = 'Invalid Customer No'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.


********partner function check***********************
    IF ls_excel-parvw IS NOT INITIAL.

    ELSE.
      ls_disp-message = 'Partner Function cannot be empty'.
      APPEND ls_disp TO lt_disp.
      CONTINUE.
    ENDIF.


*Check if record is already there in the KNVP table for the entered records
    CLEAR ls_knvp.
    READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_excel-kunnr
                                             vkorg = ls_excel-vkorg
                                             vtweg = ls_excel-vtweg
                                             spart = ls_excel-spart
                                             parvw = ls_excel-parvw BINARY SEARCH.
    IF sy-subrc = 0.
* fill the before structure
      CLEAR: ls_yknvp. REFRESH: lt_yknvp.
      MOVE-CORRESPONDING ls_knvp TO ls_yknvp.
      ls_yknvp-mandt = sy-mandt.
      ls_yknvp-kz = 'U'.

      ls_knvp-kunn2 = ls_excel-kunn2.
      ls_knvp-lifnr = ls_excel-lifnr.
      ls_knvp-pernr = ls_excel-pernr.

      IF ls_yknvp-pernr = ls_knvp-pernr AND ls_yknvp-lifnr = ls_knvp-lifnr AND ls_yknvp-kunn2 = ls_knvp-kunn2.
*skip as the values are same
        ls_disp-message = 'No Changes as values are same'.
        APPEND ls_disp TO lt_disp.
        CONTINUE.
      ENDIF.

*fill the after structure
      CLEAR: ls_xknvp. REFRESH: lt_Xknvp.
      MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
      ls_xknvp-mandt = sy-mandt.
      ls_xknvp-kz = 'U'.

*Table modified directly
      MODIFY knvp FROM ls_knvp.
      COMMIT WORK AND WAIT.


      APPEND ls_yknvp TO lt_yknvp.
      APPEND ls_Xknvp TO lt_Xknvp.

* Call the FM to write entry to CDHDR and CDPOS for therespective update
*      CLEAR lv_objectid.
*      lv_objectid = ls_knvp-kunnr.
*
*      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
*        EXPORTING
*          objectid = lv_objectid
*          tcode    = 'XD02'
*          utime    = sy-uzeit
*          udate    = sy-datum
*          username = sy-uname
*          upd_knvp = 'U'
*        TABLES
*          xknvp    = lt_xknvp
*          yknvp    = lt_yknvp.

      CLEAR ls_disp.
      ls_disp-kunnr = ls_excel-kunnr.
      ls_disp-vkorg = ls_excel-vkorg.
      ls_disp-vtweg = ls_excel-vtweg.
      ls_disp-spart = ls_excel-spart.
      ls_disp-parvw = ls_excel-parvw.
      ls_disp-kunn2 = ls_excel-kunn2.
      ls_disp-lifnr = ls_excel-lifnr.
      ls_disp-pernr = ls_excel-pernr.
      ls_disp-message = 'Existing Customer Updated'.
      APPEND ls_disp TO lt_disp.
    ELSE.
* for New partner record inserting
      ls_knvp-kunnr = ls_excel-kunnr.
      ls_knvp-vkorg = ls_excel-vkorg.
      ls_knvp-vtweg = ls_excel-vtweg.
      ls_knvp-spart = ls_excel-spart.
      ls_knvp-parvw = ls_excel-parvw.

** fill the before structure
*      CLEAR: ls_yknvp. REFRESH: lt_yknvp.
*      MOVE-CORRESPONDING ls_knvp TO ls_yknvp.
*      ls_yknvp-mandt = sy-mandt.
*      ls_yknvp-kz = 'I'.
*      APPEND ls_yknvp TO lt_yknvp.

      ls_knvp-kunn2 = ls_excel-kunn2.
      ls_knvp-lifnr = ls_excel-lifnr.
      ls_knvp-pernr = ls_excel-pernr.


      MODIFY knvp FROM ls_knvp.
      COMMIT WORK AND WAIT.

**fill the after structure
*      CLEAR: ls_xknvp. REFRESH: lt_Xknvp.
*      MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
*      ls_xknvp-mandt = sy-mandt.
*      ls_xknvp-kz = 'I'.
*      APPEND ls_Xknvp TO lt_Xknvp.

* Call the FM to write entry to CDHDR and CDPOS for therespective update
*      CLEAR lv_objectid.
*      lv_objectid = ls_knvp-kunnr.

*      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
*        EXPORTING
*          objectid = lv_objectid
*          tcode    = 'XD02'
*          utime    = sy-uzeit
*          udate    = sy-datum
*          username = sy-uname
*          upd_knvp = 'I'
*        TABLES
*          xknvp    = lt_xknvp
*          yknvp    = lt_yknvp.

      CLEAR ls_disp.
      ls_disp-kunnr = ls_excel-kunnr.
      ls_disp-vkorg = ls_excel-vkorg.
      ls_disp-vtweg = ls_excel-vtweg.
      ls_disp-spart = ls_excel-spart.
      ls_disp-parvw = ls_excel-parvw.
      ls_disp-kunn2 = ls_excel-kunn2.
      ls_disp-lifnr = ls_excel-lifnr.
      ls_disp-pernr = ls_excel-pernr.
      ls_disp-message = 'New Customer created'.
      APPEND ls_disp TO lt_disp.
    ENDIF.
  ENDLOOP.

  REFRESH lt_fcat.

  PERFORM f_fieldcat USING 'KUNNR'   'Customer No'          1 space.
  PERFORM f_fieldcat USING 'VKORG'   'Sales Organisation'   2 space.
  PERFORM f_fieldcat USING 'VTWEG'   'Distribution Channel' 3 space.
  PERFORM f_fieldcat USING 'SPART'   'Division'             4 space.
  PERFORM f_fieldcat USING 'PARVW'   'Partner Function'     5 space.
  PERFORM f_fieldcat USING 'KUNN2'   'Customer Partner'     6 space.
  PERFORM f_fieldcat USING 'LIFNR'   'Vendor No'            7 space.
  PERFORM f_fieldcat USING 'PERNR'   'Employee No'          8 space.
  PERFORM f_fieldcat USING 'MESSAGE' 'Messages'             9 space.

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
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
FORM f_fieldcat  USING  p1 p2 p3 p4.
  CLEAR ls_fcat.
  ls_fcat-fieldname = p1.
  ls_fcat-coltext = p2.
  ls_fcat-col_pos = p3.
  ls_fcat-edit = p4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
