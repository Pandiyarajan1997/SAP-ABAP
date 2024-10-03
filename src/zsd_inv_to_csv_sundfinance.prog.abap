*&---------------------------------------------------------------------*
*& Report ZSD_INV_TO_CSV_SUNDFINANCE
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 07.08.2023
*&Purpose: Sending Invoice Details in CSV file Format to Appl Server
*&         for Sundaram Finance
*& Reference By: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zsd_inv_to_csv_sundfinance.

INCLUDE zsd_inv_to_csv_cls.

INITIALIZATION.
  DATA: lo_cls_main TYPE REF TO lcl_inv_to_csv_format.
  CREATE OBJECT lo_cls_main.

START-OF-SELECTION.

  IF p_r1 EQ abap_true and p_r3 is INITIAL.

    IF p_ctype = 'SF'. "Sundaram Finance
      "Fetching Active Invoices and Uploading the file in App server
      lo_cls_main->active_invoice_fetch( ).
      "SFTP Script execution
      lo_cls_main->script_execution( ).

    ELSEIF p_ctype = 'RF'.    "Rupifi

      CLEAR gv_token.
      lo_cls_main->rf_get_token( EXPORTING im_custype = p_ctype IMPORTING ex_token = gv_token ).

      IF gv_token IS NOT INITIAL.
        lo_cls_main->rf_active_inv_fetch( ).
      ELSE.
        MESSAGE : 'Rupifi Token not generated' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.


  ELSEIF p_r2 EQ abap_true and p_r3  is INITIAL.

    "Fetching Cancelled Invoices and Uploading the fie in App server
    lo_cls_main->cancel_invoice_fetch( ).
    "SFTP Script execution
    lo_cls_main->script_execution( ).

  ELSEIF p_r3 is NOT INITIAL and p_r3 = 1 .
         CLEAR gv_token.
      lo_cls_main->rf_get_token( EXPORTING im_custype = p_ctype IMPORTING ex_token = gv_token ).
    lo_cls_main->rf_get_pament_status( ).

  ELSEIF p_r3 is NOT INITIAL and p_r3 = 2 .
           CLEAR gv_token.
      lo_cls_main->rf_get_token( EXPORTING im_custype = p_ctype IMPORTING ex_token = gv_token ).
      lo_cls_main->rf_get_pament_status( ).
  ENDIF.

  "ALV Display
  lo_cls_main->build_alv( ).
