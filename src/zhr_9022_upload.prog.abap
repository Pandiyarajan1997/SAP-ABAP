*&---------------------------------------------------------------------*
*& Report ZHR_0185_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_9022_upload.

TYPES: BEGIN OF ty_upload,
         pernr       TYPE string,
         begda       TYPE string,
         grosspay    TYPE string,
         variablepay TYPE string,
       END OF ty_upload.

TYPES:BEGIN OF ty_log_report,
        pernr  TYPE pa0001-pernr,
        status TYPE c,
        text   TYPE string,
      END OF ty_log_report.

DATA: gt_upload     TYPE TABLE OF ty_upload,
      gs_upload     TYPE ty_upload,
      i_raw         TYPE truxs_t_text_data,
      gs_return     TYPE bapireturn1,
      gs_9022       TYPE p9022,
      gt_log_report TYPE TABLE OF ty_log_report,
      gs_log_report TYPE ty_log_report,
      gv_pernr      TYPE pa0001-pernr.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM sub_get_f4.

START-OF-SELECTION.
  PERFORM sub_upload_file.
  PERFORM update_0185.
  PERFORM display_log.

FORM sub_upload_file.
  DATA : l_file TYPE string.
  IF p_fname CS '.xls' OR p_fname CS '.XLS'.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
*       i_line_header        = 'X'
        i_tab_raw_data       = i_raw
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = gt_upload
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    DELETE gt_upload INDEX 1.
  ENDIF.
ENDFORM.

FORM sub_get_f4 .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name = syst-cprog
*     DYNPRO_NUMBER       = SYST-DYNNR
*     FIELD_NAME   = ' '
    IMPORTING
      file_name    = p_fname.
ENDFORM.
FORM update_0185.

  LOOP AT gt_upload INTO gs_upload.
    CLEAR gv_pernr.
    gv_pernr = gs_upload-pernr.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = gv_pernr
      IMPORTING
        return = gs_return.
    IF gs_return-type <> 'E' OR gs_return-type <> 'A'.
      CLEAR gs_return.

      gs_9022-pernr       = gv_pernr.
      gs_9022-grosspay    = gs_upload-grosspay.
      gs_9022-variablepay = gs_upload-variablepay.
      CONCATENATE gs_upload-begda+6(4) gs_upload-begda+3(2) gs_upload-begda+0(2) INTO gs_9022-begda.
      gs_9022-endda = '99991231'.
***********
      SELECT SINGLE a~dat01 FROM pa0041 AS a
                            INNER JOIN pa0000 AS b ON a~pernr = b~pernr
                            WHERE a~pernr = @gv_pernr
                            AND   a~dar01 = 'S1'
                            AND   b~stat2 = '3'
                            AND   b~begda LE @sy-datum
                            AND   b~endda GE @sy-datum INTO @DATA(lv_hiredate).
      IF sy-subrc NE 0.
        gs_log_report-pernr = gv_pernr.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'Inactive Employee'.
        APPEND gs_log_report TO gt_log_report.
        CLEAR gs_log_report.
        CONTINUE.
      ELSE.
        IF gs_9022-begda LT lv_hiredate.
          gs_9022-begda = lv_hiredate.
        ENDIF.
      ENDIF.
      CLEAR : lv_hiredate.
******************8call the bapi****************
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty     = '9022'
          number    = gv_pernr
*         SUBTYPE   =
*         OBJECTID  =
*         LOCKINDICATOR          =
*         VALIDITYEND            =
*         VALIDITYBEGIN          =
*         RECORDNUMBER           =
          record    = gs_9022
          operation = 'INS'
*         TCLAS     = 'A'
*         DIALOG_MODE            = '0'
*         NOCOMMIT  =
*         VIEW_IDENTIFIER        =
*         SECONDARY_RECORD       =
        IMPORTING
          return    = gs_return
*         KEY       =
        .


      IF gs_return-type <> 'E' OR gs_return-type <> 'A'.
        gs_log_report-pernr = gv_pernr.
        gs_log_report-status = 'S'.
        gs_log_report-text = 'Employee successfully updated'.

        APPEND gs_log_report TO gt_log_report.
        CLEAR gs_log_report.
      ELSE.
        gs_log_report-pernr = gv_pernr.
        gs_log_report-status = 'E'.
        gs_log_report-text = gs_return-message.

        APPEND gs_log_report TO gt_log_report.
        CLEAR gs_log_report.
      ENDIF.
    ELSE.
      gs_log_report-pernr = gv_pernr.
      gs_log_report-status = 'E'.
      gs_log_report-text = 'Employee cannot be locked'.

      APPEND gs_log_report TO gt_log_report.
      CLEAR gs_log_report.
    ENDIF.

    CLEAR gs_return.
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gv_pernr
      IMPORTING
        return = gs_return.
  ENDLOOP.

ENDFORM.
FORM display_log.
  REFRESH gt_fcat.
  PERFORM sub_get_fcat USING : 'PERNR' 'Employee Number',
                               'STATUS' 'Status',
                               'TEXT' 'Message'.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = gt_log_report.
ENDFORM.
FORM sub_get_fcat  USING p_fnam TYPE any
                         p_text  TYPE any.

  gs_fcat-fieldname = p_fnam.
  gs_fcat-seltext_l = p_text.
  APPEND gs_fcat TO gt_fcat.
  CLEAR gs_fcat.


ENDFORM.
