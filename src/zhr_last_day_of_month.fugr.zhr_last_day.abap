FUNCTION ZHR_LAST_DAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  D
*"  EXPORTING
*"     VALUE(E_LAST_DAY) TYPE  D
*"----------------------------------------------------------------------

  DATA: BEGIN OF W_DATE_DS,
          YEAR(4)    TYPE N,
          MONTH(2)   TYPE N,
          DAY(2)     TYPE N,
        END OF W_DATE_DS.

  W_DATE_DS = I_DATE.
  IF W_DATE_DS-MONTH = '12'.
    W_DATE_DS-DAY = 31.
  E_LAST_DAY = W_DATE_DS.
  ELSE.
    ADD 1 TO W_DATE_DS-MONTH.
    W_DATE_DS-DAY = '01'.
  E_LAST_DAY = W_DATE_DS.
    SUBTRACT 1 FROM E_LAST_DAY.

  ENDIF.


ENDFUNCTION.
