*&---------------------------------------------------------------------*
*& Report ZMM_INFOREC_TO_MR21_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_incoming_payment_new.
*** Data Declaration include **
INCLUDE zfi_incoming_payment_new_top.
*** Selection Screen Design ***
INCLUDE zfi_incoming_payment_new_sel.
*** Subriutines for Actual Process ***
INCLUDE zfi_incoming_payment_new_f01.

AT SELECTION-SCREEN.

  PERFORM f_header_from_selection.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'BLART'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  blart = 'DZ'.
  IF budat EQ 0.
    budat = sy-datum.
  ENDIF.

*** Actaul process starts *****
START-OF-SELECTION.
**** Initial Selections ****
*  PERFORM f_popup_confirm.
*
*  PERFORM f_process_from_selection.

  PERFORM f_document_post.


END-OF-SELECTION.
  CHECK sy-batch NE abap_true.
  IF gt_message[] IS NOT INITIAL.
    PERFORM f_display_output.
  ELSE.
    DATA(lv_err) = `No data found for ` && kunnr && ` Customer.`.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
