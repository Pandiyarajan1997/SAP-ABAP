*&---------------------------------------------------------------------*
*& Report ZMM_INFOREC_TO_MR21_UPDATE
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created on: 14.11.2022
*&Reference by: Ramakrishnan J
*&Purpose: For Updating Material Price in MR21 based on latest PIR ()
*           Purchase Info Record
*           For Purchase team
*----------------------------------------------------------------------------
** Changed On: 16.12.2022
** Changed By: Samsudeen M
** full Logic Changes based on current stock and
** open po qty and values for from plant material and doing average
** after calculation the price is updating in MR21 for material of to plant
*&---------------------------------------------------------------------*
REPORT zmm_inforec_to_mr21_update.
*** Data Declaration include **
INCLUDE zmm_inforec_to_mr21_upd_top.
*** Selection Screen Design ***
INCLUDE zmm_inforec_to_mr21_upd_sel.
*** Subriutines for Actual Process ***
INCLUDE zmm_inforec_to_mr21_upd_forms.
*** Actaul process starts *****
START-OF-SELECTION.
*** Getting Open PO for From Plant ***
  PERFORM f_get_open_po USING p_from.
*** Initial Selections ****
  PERFORM f_initial_selection.
*** Actual Process for Material in From Plant for Price Updation ****
  REFRESH: gt_cal_price.
  LOOP AT gt_marc_fr ASSIGNING FIELD-SYMBOL(<fs_mat>).
    DATA(lv_matnr) = VALUE #( gt_marc_to[ matnr = <fs_mat>-matnr ]-matnr OPTIONAL ).
**** Material is not Present in TO Plant ****
    IF lv_matnr IS INITIAL.
      APPEND VALUE #( matnr = <fs_mat>-matnr
                      matdes = VALUE #( gt_makt[ matnr = <fs_mat>-matnr ]-maktx OPTIONAL )
                      f_werks = p_from
                      t_werks = p_to
                      message = | { 'Material Not extended in' } { p_to } | ) TO gt_cal_price.
      CONTINUE.
    ELSE.
*** If Material is presents in to Material  means actual Process ***
      PERFORM f_price_calculation USING <fs_mat>-matnr.
    ENDIF.
  ENDLOOP.
*** Price updation or Display ***
  PERFORM f_price_upd_or_disp USING p_disp.
