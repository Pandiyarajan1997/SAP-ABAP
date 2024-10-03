FUNCTION z_cvic_bupa_pai_cvic12.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  DATA: false           TYPE boole-boole  VALUE ' ',
        lt_kna1         TYPE TABLE OF kna1,
        table_name_kna1 TYPE fsbp_table_name VALUE 'KNA1'.

  FIELD-SYMBOLS:  <kna1>  LIKE LINE OF lt_kna1.

  CHECK cvi_bdt_adapter=>is_direct_input_active( ) = false.
* step 1: update xo memory from technical screen structure
  cvi_bdt_adapter=>get_current_bp_data(
    EXPORTING
      i_table_name = table_name_kna1
    IMPORTING
      e_data_table = lt_kna1[]
  ).

  IF lt_kna1[] IS INITIAL.
    IF gs_kna1 IS NOT INITIAL.
      gs_kna1-kunnr = cvi_bdt_adapter=>get_current_customer( ).
      APPEND gs_kna1 TO lt_kna1.
    ENDIF.
  ELSE.
    READ TABLE lt_kna1 ASSIGNING <kna1> INDEX 1.
    <kna1>-zlatitude = gs_kna1-zlatitude.
    <kna1>-zlongitude = gs_kna1-zlongitude.
    <kna1>-zfincode   = gs_kna1-zfincode.
    <kna1>-zcustype   = gs_kna1-zcustype.
  ENDIF.

  cvi_bdt_adapter=>data_pai(
   i_table_name = table_name_kna1
   i_data_new   = lt_kna1[]
   i_validate   = false
  ).

* step 2: check fields
  CHECK cvi_bdt_adapter=>get_activity( ) <> cvi_bdt_adapter=>activity_display.

*   SPACE to implement own PAI checks





ENDFUNCTION.
