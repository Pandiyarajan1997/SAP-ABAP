"Name: \PR:SAPFV45E\FO:VBEP-WMENG_PRUEFEN_FOLGEBELEG\SE:END\EI
ENHANCEMENT 0 ZSALESRETURN.

*Commented Based on discussion with Rajendran Balasubramaniam
* IF vbak-auart eq 'YBRE'."  AUART    "Added by Savariar as on 20/07/2015 requirment by Mr.ragu
*
*   DATA : LV_QTY TYPE VBRP-FKIMG.
*
**  IF vbak-vbtyp CN vbtyp_lp.
*  IF vbak-vbtyp CN 'EF'.
*
**if cl_sd_doc_category_util=>is_any_Sched_agreement = vbak-vbtyp.
*      IF vbap-kzfme IS INITIAL.
*        da_mengev = xvbapf-hmenge  * quan_1 * us_umvkn / us_umvkz.
*      ELSE.
*        da_mengev = xvbapf-hmengev * quan_1.
*      ENDIF.
*      IF vbak-vbtyp = charh.
*        da_mengev = da_mengev - us_kwmeng.
*      ENDIF.
*      da_lsmeng = us_kwmeng.
**      IF us_kwmeng < da_mengev.
*       IF cvbrp-fkimg <= da_mengev.
**      IF da_mengev > 0.
**        WRITE da_mengev TO da_mengec UNIT vbap-vrkme.
**        SET CURSOR FIELD us_feldname LINE sy-stepl.
**        MESSAGE ID 'V1' TYPE da_dia NUMBER 499
**                WITH vbap-posnr da_mengec us_vrkme.
*
*        MESSAGE 'There are already Returns for the item ' TYPE 'I' DISPLAY LIKE 'E'." WITH vbap-posnr da_mengec us_vrkme.
*        LEAVE TO CURRENT TRANSACTION.
*
*      ENDIF.
*    ENDIF.
*
*ENDIF.


ENDENHANCEMENT.
