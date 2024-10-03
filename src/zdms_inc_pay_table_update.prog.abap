*&---------------------------------------------------------------------*
*& Report ZDMS_INC_PAY_TABLE_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_inc_pay_table_update.

TYPES : BEGIN OF ty_excel,
          misautoid TYPE xblnr1,
          retailer  TYPE kunnr,
*          distributor TYPE kunnr,
*          saporderid  TYPE zref_id,
*          postingdate TYPE budat,
*          amount      TYPE wrbtr,
          misdoc_no TYPE char30,
        END OF ty_excel.
DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
       lt_raw   TYPE truxs_t_text_data.

PARAMETERS : p_fname TYPE rlgrap-filename.     "for excel file upload

"f4 functionality to file path

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.

START-OF-SELECTION.
****************excel to it**************
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
    EXPORTING
      i_tab_raw_data       = lt_raw
      i_filename           = p_fname
      i_line_header        = abap_true
    TABLES
      i_tab_converted_data = lt_excel.


  LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
*    <ls_excel>-distributor  = |{ <ls_excel>-distributor ALPHA = IN }|.
    CONDENSE : <ls_excel>-retailer.
    CONDENSE : <ls_excel>-misautoid.
    CONDENSE : <ls_excel>-misdoc_no.
    <ls_excel>-retailer = |{ <ls_excel>-retailer ALPHA = IN }|.

*******************check the misdoc_no***********
    IF <ls_excel>-misdoc_no(1) = '_'.
      REPLACE '_' IN <ls_excel>-misdoc_no WITH space.
      CONDENSE : <ls_excel>-misdoc_no.
    ENDIF.

    UPDATE zfi_dms_cust_pay SET   new_ref   = <ls_excel>-misdoc_no
                            WHERE bukrs     = 'DMS1'
                            AND   kunnr     = <ls_excel>-retailer
                            AND   xblnr     = <ls_excel>-misautoid.
*                            AND   reference_id = <ls_excel>-saporderid.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE : 'Data updated' TYPE 'S'.
  ENDIF.
