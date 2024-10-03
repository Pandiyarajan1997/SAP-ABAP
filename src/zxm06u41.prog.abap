*&---------------------------------------------------------------------*
*&  Include           ZXM06U41
*&---------------------------------------------------------------------*
*Requirment by : N.Umapathy ABAPer - RAM
*
*IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N'.
*
*DATA:LV_MAT TYPE MARA-MATNR.
*
*    SELECT SINGLE MATERIAL FROM ZMM_MAT_BLOCK INTO LV_MAT WHERE MATERIAL = I_EKPO-MATNR.
*      IF LV_MAT IS NOT INITIAL.
*
*      IF I_EKPO-PSTYP EQ '0' OR 'I_EKPO-PSTYP ' EQ ''.
*
*MESSAGE E001(ZMB).
*LEAVE TO CURRENT TRANSACTION.
*
*ENDIF.
*ENDIF.
*ENDIF.


DATA: fs_ekpo TYPE ekpo_ci.
DATA: lv_remark TYPE ekpo-zremarks.

gw_aktyp = i_aktyp.

zremarks = i_ci_ekpo-zremarks.
