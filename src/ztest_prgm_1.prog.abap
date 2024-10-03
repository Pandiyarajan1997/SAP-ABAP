*&---------------------------------------------------------------------*
*& Report ztest_prgm_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_prgm_1.

DATA: lo_json  TYPE REF TO /ui2/cl_json,
      gt_data type zst_cust_create_input.

      START-OF-SELECTION.

     /ui2/cl_json=>serialize(
        EXPORTING
          data             = gt_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
        RECEIVING
          r_json           = data(ls_json)
      ).

      cl_demo_output=>display(
        EXPORTING
          data = ls_json
*          name =
      ).
