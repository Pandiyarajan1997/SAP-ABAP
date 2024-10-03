*----------------------------------------------------------------------*
***INCLUDE ZHR_ADMIN_LETTER_EMP_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  DATA: l_txt      TYPE text100,
        lv_answer  TYPE c,
        l_err_flag TYPE c.
  CLEAR l_txt.
  CASE sy-ucomm.
    WHEN 'LTYPE'.
      CLEAR: gw_emp_letter, gt_emp_letter[].
      SELECT * FROM zhr_emp_letters
        INTO TABLE gt_emp_letter
        WHERE pernr = zhr_st_appt_letter-empno AND
              ltype = g_letter_typ.
      IF sy-subrc = 0.
        SORT gt_emp_letter BY ldate DESCENDING.
        gw_emp_letter = gt_emp_letter[ 1 ].
        PERFORM f_preview_letter.
      ELSE.
        IF NOT g_html_control IS INITIAL.
          CALL METHOD g_html_control->free.
          FREE g_html_control.
        ENDIF.
        IF NOT g_html_container IS INITIAL.
          CALL METHOD g_html_container->free.
          FREE g_html_container.
        ENDIF.
        CREATE OBJECT g_html_container
          EXPORTING
            container_name = 'PDF'.
        CREATE OBJECT g_html_control
          EXPORTING
            parent = g_html_container.
      ENDIF.

    WHEN 'NEW'.
      CLEAR: lv_answer, l_err_flag.
      IF zhr_st_appt_letter-empno IS INITIAL OR
         zhr_st_appt_letter-empname IS INITIAL.
        l_txt = 'Employee No or Name is empty'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ELSEIF zhr_st_appt_letter-address1 IS INITIAL OR
         zhr_st_appt_letter-address2 IS INITIAL OR
         zhr_st_appt_letter-addresscity IS INITIAL OR
         zhr_st_appt_letter-addresspin IS INITIAL.
        l_txt = 'Employee Address field is empty'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ELSEIF zhr_st_appt_letter-joindate IS INITIAL.
        l_txt = 'Employee Join Date field is empty'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ELSEIF zhr_st_appt_letter-designation IS INITIAL OR
         zhr_st_appt_letter-department IS INITIAL OR
         zhr_st_appt_letter-location IS INITIAL.
        l_txt = 'Employee Information is missing from Infotype 9020'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ELSEIF zhr_st_appt_letter-repmanager IS INITIAL.
        l_txt = 'Employee Information is missing from Infotype 9021'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.

      ELSEIF zhr_st_appt_letter-sign_email IS INITIAL.
        l_txt = 'Letter Singatory Email is missing(Table-ZHR_LETTERS_SIGN)'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ELSEIF ( r1 = abap_true AND zhr_st_appt_letter-off_email IS INITIAL ) OR
             ( r2 = abap_true AND zhr_st_appt_letter-emp_email  IS INITIAL ).
        l_txt = 'Employee Email is missing'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.

      ELSEIF zhr_st_appt_letter-grosspay IS INITIAL.
*         zhr_st_appt_letter-variablepay IS INITIAL.
        l_txt = 'Employee Information is missing from Infotype 9022'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = abap_true.
      ENDIF.
      IF l_err_flag = abap_true.
        EXIT.
      ELSE.
        IF gw_emp_letter IS NOT INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Employees Letter''s'
              text_question         = 'Do you want to overwrite the old and re-generate new form ?'
              text_button_1         = 'Confirm'(101)
              text_button_2         = 'No'(102)
              default_button        = '1'
              display_cancel_button = ''
              start_column          = 25
              start_row             = 6
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
        ENDIF.
        IF ( gw_emp_letter IS INITIAL OR lv_answer = '1' ) AND
             zhr_st_appt_letter-empno IS NOT INITIAL.

          PERFORM f_generate_appt_letter.
*          CLEAR gw_emp_letter.
*          l_txt = 'Letter is successfully generated'.
*          MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'S'.
        ELSE.
          IF gw_emp_letter IS NOT INITIAL AND zhr_st_appt_letter-empno IS NOT INITIAL.
            PERFORM f_preview_letter.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'MAIL'.
      IF gw_emp_letter IS INITIAL.
        l_txt = 'Please Enter Employee''s Letter Information'.
      ELSE.
        CASE gw_emp_letter-lstatus.
          WHEN ''. l_txt = 'Employee Letter is not generated'.
          WHEN '11'.  PERFORM f_send_mail.
          WHEN '12'. l_txt = 'Employee Letter is already send for Signature'.
          WHEN '13'. l_txt = 'Employee Letter process is completed'.
        ENDCASE.
      ENDIF.
      IF l_txt IS NOT INITIAL.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'BACK' OR
         'EXIT'. LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_EMP_DETAILS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_emp_details INPUT.
  IF sy-ucomm = 'MAIL' OR  sy-ucomm = 'LTYPE'.
    EXIT.
  ENDIF.
  CLEAR zhr_st_appt_letter.
  CLEAR: gw_emp_letter, gt_emp_letter[].

  IF NOT g_html_control IS INITIAL.
    CALL METHOD g_html_control->free.
    FREE g_html_control.
  ENDIF.
  IF NOT g_html_container IS INITIAL.
    CALL METHOD g_html_container->free.
    FREE g_html_container.
  ENDIF.

  CREATE OBJECT g_html_container
    EXPORTING
      container_name = 'PDF'.

  CREATE OBJECT g_html_control
    EXPORTING
      parent = g_html_container.

  IF g_pernr IS NOT INITIAL.
    zhr_st_appt_letter-empno = g_pernr.
    SELECT SINGLE
            a~pernr    AS empno,
           CASE WHEN f~gesch = '1' THEN 'Mr'
                WHEN f~gesch = '2' THEN 'Ms'
           END AS title,
           a~ename    AS empname,
*             a~plans,
           b~design   AS designation,
*             a~orgeh,
           b~department,
           b~location,
*             c~greytmngid,
           d~ename    AS repmanager,
           e~stras    AS address1,
           e~locat    AS address2,
           e~ort01    AS addresscity,
           e~ort02    AS addressdist,
           e~pstlz    AS addresspin,
           CASE WHEN g~dar01 = 'S1' THEN g~dat01
                WHEN g~dar02 = 'S1' THEN g~dat02
                WHEN g~dar03 = 'S1' THEN g~dat03
                WHEN g~dar04 = 'S1' THEN g~dat04
                WHEN g~dar05 = 'S1' THEN g~dat05
                WHEN g~dar06 = 'S1' THEN g~dat06
           END AS joindate,

                h~grosspay AS grosspay,
                h~variablepay AS variablepay,
                i~usrid_long AS off_email
                INTO CORRESPONDING FIELDS OF @zhr_st_appt_letter
                FROM pa0001 AS a
                                 LEFT OUTER JOIN pa9020 AS b ON b~pernr  =   a~pernr     AND
                                                           b~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa9021 AS c ON c~pernr  =   a~pernr     AND
                                                           c~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa0001 AS d ON d~pernr  =   c~greytmngid AND
                                                           d~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa0006 AS e ON e~pernr  =   a~pernr AND
                                                           e~subty  =   '1'     AND
                                                           e~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa0002 AS f ON f~pernr  =   a~pernr AND
                                                           f~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa0041 AS g ON g~pernr  =   a~pernr AND
                                                           g~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa9022 AS h ON h~pernr  =   a~pernr AND
                                                           h~endda  >=  @sy-datum
                                 LEFT OUTER JOIN pa0105 AS i ON i~pernr = a~pernr AND
                                                                i~subty = '0010'
                WHERE a~pernr = @zhr_st_appt_letter-empno AND
                      a~endda = '99991231'.

    SELECT SINGLE usrid_long
    FROM pa0105 INTO zhr_st_appt_letter-emp_email
      WHERE pernr = zhr_st_appt_letter-empno AND
            subty = '0030'.

    SELECT SINGLE bukrs FROM pa0001
      INTO @DATA(l_ccode)
      WHERE pernr = @zhr_st_appt_letter-empno.

    SELECT SINGLE emnam email
    FROM zhr_letters_sign INTO ( zhr_st_appt_letter-sign_name,zhr_st_appt_letter-sign_email )
      WHERE bukrs = l_ccode AND ltype = g_letter_typ AND lsrno = 1.
*
*    CLEAR: gw_emp_letter, gt_emp_letter[].
    SELECT * FROM zhr_emp_letters
      INTO TABLE gt_emp_letter
      WHERE pernr = zhr_st_appt_letter-empno AND
            ltype = g_letter_typ.
    IF sy-subrc = 0.
      SORT gt_emp_letter BY ldate DESCENDING.
      gw_emp_letter = gt_emp_letter[ 1 ].
      DATA(l_domvalue_l) = gw_emp_letter-lstatus.
      IF l_domvalue_l IS INITIAL.
        l_domvalue_l = '11'.
      ENDIF.
      IF l_domvalue_l = '12'.
        CALL METHOD go_adobe->get_status
          EXPORTING
            pernr       = gw_emp_letter-pernr
            ltype       = gw_emp_letter-ltype
            ldate       = gw_emp_letter-ldate
            transientid = gw_emp_letter-transientid
            agreementid = gw_emp_letter-agreementid
          IMPORTING
            status      = DATA(l_status).
        IF l_status = 'SIGNED'.
          gw_emp_letter-lstatus = '13'.
          CALL METHOD go_adobe->get_signed_document
            EXPORTING
              pernr       = gw_emp_letter-pernr
              ltype       = gw_emp_letter-ltype
              ldate       = gw_emp_letter-ldate
              transientid = gw_emp_letter-transientid
              agreementid = gw_emp_letter-agreementid
            IMPORTING
              pdf         = DATA(l_pdf).
          gw_emp_letter-pdf = l_pdf.
          MODIFY zhr_emp_letters FROM gw_emp_letter.
        ENDIF.
      ENDIF.

      SELECT SINGLE ddtext AS text
             FROM dd07t INTO zhr_st_appt_letter-lstatus
             WHERE domname = 'ZLSTATUS' AND
                   as4local = 'A' AND
                   ddlanguage = 'E' AND
                   domvalue_l = l_domvalue_l.
      IF sy-ucomm EQ 'NEW' OR  sy-ucomm EQ 'LTYPE'.
      ELSE.
        PERFORM f_preview_letter.
      ENDIF.
    ELSE.
      zhr_st_appt_letter-lstatus = 'Letter is not Generated'.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_preview_letter
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_preview_letter .

  DATA: lt_data             TYPE tsfixml.

*CONVERT xstring to binary table to pass to the load_data METHOD

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = gw_emp_letter-pdf
    TABLES
      binary_tab = lt_data.

*LOAD the html
  DATA:    lv_url       TYPE /sapcnd/det_analysis_docuurl.
  CALL METHOD g_html_control->load_data(
    EXPORTING
      type                 = 'application'
      subtype              = 'pdf'
    IMPORTING
      assigned_url         = lv_url
    CHANGING
      data_table           = lt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).

  g_html_control->show_url( url = lv_url in_place = 'X' ).

ENDFORM.
FORM f_generate_appt_letter.


  " Generate Document.
  CLEAR : gw_emp_letter , gt_emp_letter.

  SELECT * FROM zhr_emp_letters
  INTO TABLE gt_emp_letter
  WHERE pernr = zhr_st_appt_letter-empno AND
        ltype = g_letter_typ.
  IF sy-subrc = 0.
    SORT gt_emp_letter BY ldate DESCENDING.
    gw_emp_letter = gt_emp_letter[ 1 ].
    DATA(ldate) = gw_emp_letter-ldate.
  ELSE.
    ldate = sy-datum.
  ENDIF.

  CALL METHOD go_adobe->generate_letter
    EXPORTING
      hr_screen_struct = zhr_st_appt_letter
      pernr            = zhr_st_appt_letter-empno
      ltype            = g_letter_typ
      ldate            = ldate
*     sign_link        = abap_true
*     sign_email       =
*     emp_email        =
    IMPORTING
      out_pdf          = gw_emp_letter-pdf
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  gw_emp_letter-pernr = zhr_st_appt_letter-empno.
  gw_emp_letter-ltype = g_letter_typ.
  gw_emp_letter-lstatus = '11'.
  gw_emp_letter-ldate = ldate.
  gw_emp_letter-emnam = zhr_st_appt_letter-empname.
  gw_emp_letter-pdf  = gw_emp_letter-pdf.
  gw_emp_letter-usrid  = sy-uname.
  gw_emp_letter-cdate = sy-datum.
  gw_emp_letter-ctime = sy-uzeit.
  MODIFY zhr_emp_letters FROM gw_emp_letter.

  DATA: lt_data             TYPE tsfixml.


*CONVERT xstring to binary table to pass to the load_data METHOD

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = gw_emp_letter-pdf
    TABLES
      binary_tab = lt_data.

*LOAD the html
  DATA:    lv_url       TYPE /sapcnd/det_analysis_docuurl.
  CALL METHOD g_html_control->load_data(
    EXPORTING
      type                 = 'application'
      subtype              = 'pdf'
    IMPORTING
      assigned_url         = lv_url
    CHANGING
      data_table           = lt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).

  g_html_control->show_url( url = lv_url in_place = 'X' ).

  PERFORM f_download_file.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_download_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_download_file .
  DATA: lv_dummy    TYPE c,
        lv_clen     TYPE i,
        lv_line(72) TYPE c,
        lv_len      TYPE i,
        lv_size     TYPE i,
        lv_hex(144) TYPE x.
  DATA: lt_otf_data TYPE tsfotf,
        w_otf_data  TYPE itcoo.
  FIELD-SYMBOLS: <l_fs> TYPE c.
*
*    CALL METHOD go_adobe->generate_letter
*      EXPORTING
*        hr_screen_struct = zhr_st_appt_letter
*        pernr            = gw_emp_letter-pernr
*        ltype            = gw_emp_letter-ltype
*        ldate            = gw_emp_letter-ldate
*        sign_link        = abap_true
**       sign_email       =
**       emp_email        =
*      IMPORTING
*        out_pdf          = gw_emp_letter-pdf
*      EXCEPTIONS
*        not_found        = 1
*        OTHERS           = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

  DATA(lv_pdf) = gw_emp_letter-pdf.

  lv_size = 72.
  DESCRIBE FIELD lv_dummy LENGTH lv_clen IN BYTE MODE.
  lv_size = lv_size * lv_clen.
  lv_len = xstrlen( lv_pdf ).
  WHILE lv_len > lv_size.
    lv_hex(lv_size) = lv_pdf(lv_size).
    ASSIGN lv_hex(lv_size) TO <l_fs> CASTING.
    lv_line = <l_fs>.
    w_otf_data = lv_line.
    APPEND w_otf_data TO lt_otf_data.
    SHIFT lv_pdf LEFT BY lv_size PLACES IN BYTE MODE.
    lv_len = xstrlen( lv_pdf ).
  ENDWHILE.
  IF lv_len >= 0.
    CLEAR: lv_hex, w_otf_data, lv_line.
    lv_hex = lv_pdf(lv_len).
    ASSIGN lv_hex(lv_size) TO <l_fs> CASTING.
    lv_line = <l_fs>.
    w_otf_data = lv_line.
    APPEND w_otf_data TO lt_otf_data.
  ENDIF.

  CLEAR:  lv_dummy,
          lv_clen,
          lv_line,
          lv_len,
          lv_size,
          lv_hex.
  UNASSIGN : <l_fs>.
  CLEAR: t_tline[],w_tline.
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_size
*     bin_file              =
    TABLES
      otf                   = lt_otf_data
      lines                 = t_tline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  CASE gw_emp_letter-ltype.
    WHEN '11'. " Appointment Letter
*  and then to download the pdf use this .
      CONCATENATE 'C:\HR Letters\Appt Letter\'
                  gw_emp_letter-pernr '.pdf'
                  INTO g_filename.
      DATA(l_txt) = 'Letter is generated and Download to C:\HR Letters\Appt Letter\'.
    WHEN '12'. " Confirmation Letter
*  and then to download the pdf use this .
      CONCATENATE 'C:\HR Letters\Confirm Letter\'
                  gw_emp_letter-pernr '.pdf'
                  INTO g_filename.
      l_txt = 'Letter is generated and Download to C:\HR Letters\Confirm Letter\'.
    WHEN OTHERS.
  ENDCASE.

*  l_filename = |C:\HR Letters\Appt Letter\ { gw_emp_letter-pernr }.pdf|.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize = lv_size
      filename     = g_filename
      filetype     = 'BIN'
    TABLES
      data_tab     = t_tline.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_send_mail .
  DATA: l_transientid TYPE string,
        l_agreementid TYPE string,
        l_status      TYPE text50,
        l_emp_email   TYPE zassign_email.

  IF r1 = abap_true.
    l_emp_email   = zhr_st_appt_letter-off_email.
    DATA(l_txt) = 'Employee Official Email is not found'.
  ELSE.
    l_emp_email   = zhr_st_appt_letter-emp_email.
    l_txt = 'Employee Personal Email is not found'.
  ENDIF.
  IF l_emp_email IS INITIAL.
    MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    " Get Document ID and Generate Document.
    CALL METHOD go_adobe->get_documentid
      EXPORTING
        hr_screen_struct = zhr_st_appt_letter
        pernr            = gw_emp_letter-pernr
        ltype            = gw_emp_letter-ltype
        ldate            = gw_emp_letter-ldate
        sign_link        = abap_true
      IMPORTING
        transientid      = l_transientid.
    IF l_transientid IS NOT INITIAL.
      " Post Documebt and Send mail
      CALL METHOD go_adobe->post_dcumentid
        EXPORTING
          pernr       = gw_emp_letter-pernr
          ltype       = gw_emp_letter-ltype
          ldate       = gw_emp_letter-ldate
          sign_email  = zhr_st_appt_letter-sign_email
          emp_email   = l_emp_email
          transientid = l_transientid
        IMPORTING
          agreementid = l_agreementid.
      IF l_agreementid IS NOT INITIAL.
        gw_emp_letter-transientid = l_transientid.
        gw_emp_letter-agreementid = l_agreementid.
        gw_emp_letter-mailid = l_emp_email.
        gw_emp_letter-lstatus = '12'.

        MODIFY zhr_emp_letters FROM gw_emp_letter.
        l_txt = 'Letter''s mail sent for e-signature'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'S'.
      ELSE.
        l_txt = 'Mail is not sent for e-signature. Error in AgreementID.'.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      l_txt = 'Transient ID is not generated'.
      MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
