*&---------------------------------------------------------------------*
*& Report ZMM_INFOREC_TO_MR21_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_fert_price_update_me12.
*** Data Declaration include **
INCLUDE zmm_fert_price_update_me12_top.
*** Selection Screen Design ***
INCLUDE zmm_fert_price_update_me12_sel.
*** Subriutines for Actual Process ***
INCLUDE zmm_fert_price_update_me12_f01.

AT SELECTION-SCREEN.
*
*  PERFORM f_initial_screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_initial_screen.

*** Actaul process starts *****
START-OF-SELECTION.
*** Initial Selections ****
  PERFORM f_popup_confirm.
  IF r1 = abap_true. " Purchase Org Plant to Plant Copy
    PERFORM f_process_for_purchase_org.
  ELSE.  " Sales Org Material Price Plant to Plant Copy
    PERFORM f_process_for_sales_org.
  ENDIF.

END-OF-SELECTION.
  IF gt_output[] IS NOT INITIAL .
*     gt_output_vk11 IS NOT INITIAL.
    PERFORM f_display_output.
  ELSE.
    DATA(lv_err) = `No data found for ` && p_werks && ` plant.`.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
