*&---------------------------------------------------------------------*
*& Report ZHR_0185_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_9003_baseloc_upload.

TYPES: BEGIN OF ty_upload,
         f1 TYPE string, " XLS Field 1
         f2 TYPE string, " XLS Field 2
         f3 TYPE string, " XLS Field 3
         f4 TYPE string, " XLS Field 4
         f5 TYPE string, " XLS Field 5
       END OF ty_upload.

TYPES:BEGIN OF ty_log_report,
        f1     TYPE string, " XLS Field 1
        f2     TYPE string, " XLS Field 2
        f3     TYPE string, " XLS Field 3
        f4     TYPE string, " XLS Field 4
        f5     TYPE string, " XLS Field 5
        status TYPE c,
        text   TYPE string,
      END OF ty_log_report.

DATA: gt_upload     TYPE TABLE OF ty_upload,
      gs_upload     TYPE ty_upload,
      i_raw         TYPE truxs_t_text_data,
      gs_return     TYPE bapireturn1,
      gt_log_report TYPE TABLE OF ty_log_report,
      gs_log_report TYPE ty_log_report,
      gv_pernr      TYPE pa0001-pernr.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_infty TYPE i AS LISTBOX VISIBLE LENGTH 20 DEFAULT 1 OBLIGATORY.
  PARAMETERS : p_fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN OUTPUT.
  DATA:
    name  TYPE vrm_id,
    list  TYPE vrm_values,
    value TYPE vrm_value.
  name = 'P_INFTY'. " Name should be in UPPER CASE

  value-key = '1'.
  value-text = 'Base Location(9011)'.
  APPEND value TO list.
  value-key = '2'.
  value-text = 'Travel Category(9013)'.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = CONV vrm_id( 'P_INFTY' )
*      values = VALUE vrm_values(
*                 FOR i = 1 UNTIL i > 4
*                 ( key = i text = |IT { i + 9010 }| ) ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM sub_get_f4.

START-OF-SELECTION.

  PERFORM sub_upload_file.

  CASE p_infty.
    WHEN 1." IT9011 Base Location
    PERFORM update_9011.
    WHEN 2." IT9013 Travel Allowed
    PERFORM update_9013.
    WHEN OTHERS.
  ENDCASE.

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
FORM update_9011.
  DATA ls_p9011 TYPE p9011.

  DATA lt_p9011 TYPE TABLE OF p9011.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.
  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.

  LOOP AT gt_upload INTO gs_upload.

    CLEAR ls_p9011.
    ls_p9011-objid = gs_upload-f1.
    ls_p9011-otype = 'S'.
    ls_p9011-istat = '1'.
    ls_p9011-plvar = '01'.
    ls_p9011-infty = '9011'.
    ls_p9011-endda = '99991231'.
    ls_p9011-latitude = gs_upload-f2.
    ls_p9011-longitude = gs_upload-f3.

    gs_log_report-f1 = ls_p9011-objid.
    gs_log_report-f2 = ls_p9011-begda.
    gs_log_report-f3 = ls_p9011-latitude.
    gs_log_report-f4 = ls_p9011-longitude.

    SELECT SINGLE begda FROM hrp1000 INTO ls_p9011-begda WHERE plvar = '01' AND
                                                               otype = 'S' AND
    objid = ls_p9011-objid.
    IF sy-subrc <> 0.
      gs_log_report-status = 'E'.
      gs_log_report-text = 'Position not found in HRP1000'.
      APPEND gs_log_report TO gt_log_report.
      CLEAR gs_log_report.
      CONTINUE.
    ENDIF.
    APPEND ls_p9011 TO lt_p9011.

    CLEAR ls_enque_tab.
    REFRESH: lt_enque_tab.

    ls_enque_tab-plvar = ls_p9011-plvar.
    ls_enque_tab-otype = 'S'.
    ls_enque_tab-objid = ls_p9011-objid.
    APPEND ls_enque_tab TO lt_enque_tab.

    CALL FUNCTION 'RH_ENQUEUE_LIST'
      TABLES
        enq_tab = lt_enque_tab
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty
              NUMBER sy-msgno
              INTO gs_log_report-text
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    gs_log_report-f1 = ls_p9011-objid.
    gs_log_report-f2 = ls_p9011-begda.
    gs_log_report-f3 = ls_p9011-latitude.
    gs_log_report-f4 = ls_p9011-longitude.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p9011
      EXCEPTIONS
        corr_exit           = 01
        no_authorization    = 08
        error_during_insert = 04
        OTHERS              = 04.

    CASE sy-subrc.
      WHEN 0.
        CALL FUNCTION 'RH_UPDATE_DATABASE'
          EXPORTING
            vtask     = 'D'
          EXCEPTIONS
            corr_exit = 1
            OTHERS    = 2.
        IF sy-subrc = 0.
          gs_log_report-status = 'S'.
          gs_log_report-text = 'Reporting Manager Changed Successfully'.
        ELSE.
          gs_log_report-status = 'E'.
          gs_log_report-text = 'Error during updating the database'.
        ENDIF.
      WHEN 1 OR 4.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'Error during creating the record'.
      WHEN 8.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'No Authorization to Create Relationship record'.
      WHEN OTHERS.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'Error during creating the record'.
    ENDCASE.

    APPEND gs_log_report TO gt_log_report.
    CLEAR gs_log_report.


    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.

  ENDLOOP.

ENDFORM.
FORM display_log.
  REFRESH gt_fcat.
  PERFORM sub_get_fcat USING : 'F1' 'Position',
                               'F2' 'StartDate',
                               'F3' 'Field1',
                               'F4' 'Field2',
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
*&---------------------------------------------------------------------*
*& Form update_9013
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_9013 .

  DATA ls_p9013 TYPE p9013.

  DATA lt_p9013 TYPE TABLE OF p9013.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.
  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.

  LOOP AT gt_upload INTO gs_upload.

    CLEAR ls_p9013.
    ls_p9013-objid = gs_upload-f1.
    ls_p9013-otype = 'S'.
    ls_p9013-istat = '1'.
    ls_p9013-plvar = '01'.
    ls_p9013-infty = '9013'.
    ls_p9013-endda = '99991231'.
    ls_p9013-travel = gs_upload-f2.
    ls_p9013-category = gs_upload-f3.

    gs_log_report-f1 = ls_p9013-objid.
    gs_log_report-f2 = ls_p9013-begda.
    gs_log_report-f3 = ls_p9013-travel.
    gs_log_report-f4 = ls_p9013-category.

    SELECT SINGLE begda
      FROM hrp1000 INTO ls_p9013-begda
      WHERE plvar = '01' AND
            otype = 'S' AND
            objid = ls_p9013-objid.
    IF sy-subrc <> 0.
      gs_log_report-status = 'E'.
      gs_log_report-text = 'Position not found in HRP1000'.
      APPEND gs_log_report TO gt_log_report.
      CLEAR gs_log_report.
      CONTINUE.
    ENDIF.
    APPEND ls_p9013 TO lt_p9013.

    CLEAR ls_enque_tab.
    REFRESH: lt_enque_tab.

    ls_enque_tab-plvar = ls_p9013-plvar.
    ls_enque_tab-otype = 'S'.
    ls_enque_tab-objid = ls_p9013-objid.
    APPEND ls_enque_tab TO lt_enque_tab.

    CALL FUNCTION 'RH_ENQUEUE_LIST'
      TABLES
        enq_tab = lt_enque_tab
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty
              NUMBER sy-msgno
              INTO gs_log_report-text
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    gs_log_report-f1 = ls_p9013-objid.
    gs_log_report-f2 = ls_p9013-begda.
    gs_log_report-f3 = ls_p9013-travel.
    gs_log_report-f4 = ls_p9013-category.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p9013
      EXCEPTIONS
        corr_exit           = 01
        no_authorization    = 08
        error_during_insert = 04
        OTHERS              = 04.

    CASE sy-subrc.
      WHEN 0.
        CALL FUNCTION 'RH_UPDATE_DATABASE'
          EXPORTING
            vtask     = 'D'
          EXCEPTIONS
            corr_exit = 1
            OTHERS    = 2.
        IF sy-subrc = 0.
          gs_log_report-status = 'S'.
          gs_log_report-text = 'Reporting Manager Changed Successfully'.
        ELSE.
          gs_log_report-status = 'E'.
          gs_log_report-text = 'Error during updating the database'.
        ENDIF.
      WHEN 1 OR 4.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'Error during creating the record'.
      WHEN 8.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'No Authorization to Create Relationship record'.
      WHEN OTHERS.
        gs_log_report-status = 'E'.
        gs_log_report-text = 'Error during creating the record'.
    ENDCASE.

    APPEND gs_log_report TO gt_log_report.
    CLEAR gs_log_report.


    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.
  ENDLOOP.
ENDFORM.
