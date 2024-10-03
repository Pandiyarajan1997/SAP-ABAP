*----------------------------------------------------------------------*
***INCLUDE LZWATER_BASEF01.
*----------------------------------------------------------------------*

FORM ZWATER_ROWID.
* break-point.
   DATA : ROW_COUNT TYPE ZWATBASE_TABLE-SL_NO,
           ROW_RANGE TYPE INRI-NRRANGENR VALUE '01' .

   DATA : NUM(7) TYPE C,
          NUM1(2) TYPE C VALUE 'DT' .


   CALL FUNCTION 'NUMBER_GET_NEXT'
     EXPORTING
       NR_RANGE_NR                   = ROW_RANGE
       OBJECT                        = 'ZWATER_NO'
*      QUANTITY                      = '1'
*      SUBOBJECT                     = ' '
*      TOYEAR                        = '0000'
*      IGNORE_BUFFER                 = ' '
    IMPORTING
      NUMBER                        = ROW_COUNT
*      QUANTITY                      =
*      RETURNCODE                    =
*    EXCEPTIONS
*      INTERVAL_NOT_FOUND            = 1
*      NUMBER_RANGE_NOT_INTERN       = 2
*      OBJECT_NOT_FOUND              = 3
*      QUANTITY_IS_0                 = 4
*      QUANTITY_IS_NOT_1             = 5
*      INTERVAL_OVERFLOW             = 6
*      BUFFER_OVERFLOW               = 7
*      OTHERS                        = 8
             .
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.

 ZWATBASE_TABLE-SL_NO = ROW_COUNT.

ENDFORM.                    "ZWATER_ROWID
