class Z_CL_SALV_TABLE definition
  public
  final
  create public .

public section.
protected section.
private section.

  methods ALV_DISPLAY
    importing
      !I_START_COLUMN type I default 25
      !I_START_LINE type I default 6
      !I_END_COLUMN type I default 200
      !I_END_LINE type I default 20
      !I_TITLE type STRING default 'ALV'
      !I_POPUP type FLAG optional
    exporting
      !IT_ALV type STANDARD TABLE .
ENDCLASS.



CLASS Z_CL_SALV_TABLE IMPLEMENTATION.


  METHOD ALV_DISPLAY.

    DATA go_alv TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = it_alv[] ).

      CATCH cx_salv_msg.
    ENDTRY.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    lr_functions = go_alv->get_functions( ).
    lr_functions->set_all( 'X' ).

    IF go_alv IS BOUND.
      IF i_popup = 'X'.
        go_alv->set_screen_popup(
          start_column = i_start_column
          end_column  = i_end_column
          start_line  = i_start_line
          end_line    = i_end_line ).
      ENDIF.

      go_alv->display( ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
