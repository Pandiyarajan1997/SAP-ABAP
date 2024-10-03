*----------------------------------------------------------------------*
***INCLUDE LZFG_TRIP_TABLEF01.
*----------------------------------------------------------------------*

FORM ZCITY_ROWID.

      DATA : W_COUNT TYPE ZFREIGHT_CITY-CITY_ROW,
         W_RANGE TYPE INRI-NRRANGENR  VALUE '01' .


  DATA : NUM(7) TYPE C ,
        NUM1(2) TYPE C VALUE 'DT' .

    CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = W_RANGE
      OBJECT                  = 'ZCITY_NO'
*     QUANTITY                = ‘1’
*     SUBOBJECT               = ‘ ‘
*     TOYEAR                  = ‘0000’
*     IGNORE_BUFFER           = ‘ ‘
    IMPORTING
      NUMBER                  = W_COUNT
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  "CONCATENATE NUM1 w_count INTO NUM .
  IF  SY-SUBRC = 0 .
    ZFREIGHT_CITY-CITY_ROW = W_COUNT .
  ENDIF.
*IF sy–subrc = 0.
** Implement suitable error handling here
*Tablename-fieldname = NUM .
*ENDIF.

ENDFORM.
