*&---------------------------------------------------------------------*
*& Report ZFB50_GL_LINEITEMS_POSTING
*&---------------------------------------------------------------------*
*&Created_by: Samsudeen M
*&Created On: 02.12.2022
*&Purpose: To post the G/L Account lineitems in FB50
*&Reference by: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zfb50_gl_lineitems_posting.
*---- Data Declaration ----------*
INCLUDE zfb50_gl_lineitem_top.
*----- Selection Screen ---------*
INCLUDE zfb50_gl_lineitems_sel.
*------ Subroutines -------------*
INCLUDE zfb50_gl_lineitem_forms.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZSEL_STAT'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM f_excel_download.
  ENDCASE.
*&---------------------------------------------------------------------*
*** Search Help to filename in ss **
AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = fname.

START-OF-SELECTION.

  IF fname IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = lt_type
        i_filename           = fname
      TABLES
        i_tab_converted_data = lt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'File is Mandatory' TYPE 'E'.
  ENDIF.

  IF lt_excel[] IS NOT INITIAL.
    REFRESH gt_skat.
    SELECT * FROM skat INTO TABLE gt_skat
                       WHERE spras = sy-langu
                       AND ktopl = 'YAIN'.
    IF sy-subrc = 0.
      SORT gt_skat[] BY saknr.
    ENDIF.
  ENDIF.

  LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fls_excel>).
*** Conversion of G/L account **
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fls_excel>-gl_account
      IMPORTING
        output = <fls_excel>-gl_account.

    <fls_excel>-gl_account_txt = VALUE #( gt_skat[ saknr = <fls_excel>-gl_account ]-txt50 OPTIONAL ).
*** Conversion of G/L account **
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fls_excel>-costcenter
      IMPORTING
        output = <fls_excel>-costcenter.
    TRANSLATE <fls_excel>-costcenter TO UPPER CASE.
*** If post key is 50 it should be in negative **
    IF <fls_excel>-key EQ '50'.
      <fls_excel>-amt = <fls_excel>-amt * -1.
    ENDIF.
    MODIFY lt_excel FROM <fls_excel>.
  ENDLOOP.
  "Actual Posting Of G/L Lineitems
  PERFORM f_jv_posting.
  "ALV Display
  PERFORM f_alv_display.


*
