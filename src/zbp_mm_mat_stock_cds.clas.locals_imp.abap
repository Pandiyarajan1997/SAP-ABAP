CLASS lhc_zmm_mat_stock_cds DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_authorizations FOR AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zmm_mat_stock_cds RESULT result.

ENDCLASS.

CLASS lhc_zmm_mat_stock_cds IMPLEMENTATION.

  METHOD get_authorizations.
  ENDMETHOD.

ENDCLASS.
