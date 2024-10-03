*----------------------------------------------------------------------*
***INCLUDE LZFI_DOWNPAYF01.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.

  DATA: bdcdata TYPE bdcdata.

  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata TO lt_bdcdata.
  CLEAR bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  DATA:bdcdata TYPE bdcdata.

  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata TO lt_bdcdata.
  CLEAR bdcdata.

ENDFORM.
