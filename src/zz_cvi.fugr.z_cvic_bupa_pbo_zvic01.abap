FUNCTION z_cvic_bupa_pbo_zvic01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  DATA:
    lt_kna1         TYPE TABLE OF kna1,
    table_name_kna1 TYPE fsbp_table_name VALUE 'KNA1'.
   " gs_kna1 TYPE kna1.

* step 1: request data from xo for dynpro structure
  cvi_bdt_adapter=>data_pbo(
    EXPORTING
      i_table_name = table_name_kna1
    IMPORTING
      e_data_table = lt_kna1[]
  ).
  IF lt_kna1[] IS INITIAL.
    CLEAR gs_kna1.
  ELSE.
    READ TABLE lt_kna1 INTO gs_kna1 INDEX 1.
  ENDIF.




ENDFUNCTION.
