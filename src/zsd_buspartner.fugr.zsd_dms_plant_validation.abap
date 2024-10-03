FUNCTION zsd_dms_plant_validation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  DATA: lt_kna1 TYPE STANDARD TABLE OF kna1.

* step 1: update xo memory from dypro structure
  cvi_bdt_adapter=>get_current_bp_data(
    EXPORTING
      i_table_name = 'KNA1'
    IMPORTING
      e_data_table = lt_kna1[]
  ).

  DATA(lv_kunnr) = VALUE #( lt_kna1[ 1 ]-kunnr OPTIONAL ).
  DATA(lv_plant) = VALUE #( lt_kna1[ 1 ]-werks OPTIONAL ).
  IF lv_plant IS NOT INITIAL.
    SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE werks = @lv_plant
                                                  AND kunnr NE @lv_kunnr.
    IF sy-subrc = 0.
      CALL FUNCTION 'BUS_MESSAGE_STORE'
        EXPORTING
          arbgb = 'ZBP_ERROR'
          msgty = 'E'
          txtnr = 000.
    ENDIF.
  ENDIF.



ENDFUNCTION.
