*&---------------------------------------------------------------------*
*& Include          ZSD_UPDATE_CUSPOS_E01
*&---------------------------------------------------------------------*

INITIALIZATION.

  DATA lobj_cust TYPE REF TO lcl_cust.
  CREATE OBJECT lobj_cust. "create the object for local class

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.       "f4 functionality to file path

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

  lobj_cust->exc_convert( ).      "for excel convert
  lobj_cust->alv( ).              "display alv
  CALL SCREEN 9001.
