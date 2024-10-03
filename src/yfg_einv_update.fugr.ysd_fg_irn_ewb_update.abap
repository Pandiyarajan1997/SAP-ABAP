FUNCTION YSD_FG_IRN_EWB_UPDATE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_IRN_DTEL) TYPE  J_1IG_INVREFNUM
*"     REFERENCE(IM_EWB_DETL) TYPE  J_1IG_EWAYBILL
*"--------------------------------------------------------------------

  "IRN details Updation
  IF im_irn_dtel IS NOT INITIAL.
    MODIFY j_1ig_invrefnum FROM im_irn_dtel.
  ENDIF.

  "EWB details Updation
  IF im_ewb_detl-ebillno IS NOT INITIAL.
    MODIFY j_1ig_ewaybill FROM im_ewb_detl.
  ENDIF.



ENDFUNCTION.
