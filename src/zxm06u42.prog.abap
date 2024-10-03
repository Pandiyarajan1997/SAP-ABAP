*&---------------------------------------------------------------------*
*&  Include           ZXM06U42
*&---------------------------------------------------------------------*
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

*      ENDIF.
*--------------------------------------------------------*
*Material Info record validity check - ME31L
*--------------------------------------------------------*
IF SY-TCODE EQ 'ME31L'.

  DATA : LS_EINE TYPE EINE,
        LS_EINA TYPE EINA,
        IT_EKOMD TYPE TABLE OF EKOMD,
        LS_EKOMD TYPE EKOMD.
*--------------------------------------------------------*
* Material Info record tables
*--------------------------------------------------------*
  SELECT SINGLE * FROM EINA INTO LS_EINA WHERE INFNR = I_EKPO-INFNR.
  SELECT SINGLE *  FROM EINE INTO LS_EINE WHERE INFNR = I_EKPO-INFNR.

*--------------------------------------------------------*
*Fm to get Info record conditions - Dates
*--------------------------------------------------------*
  IF LS_EINA-LIFNR IS NOT  INITIAL.
    CALL FUNCTION 'ME_GET_INFORECORD_CONDITIONS'
      EXPORTING
        I_EKORG = LS_EINE-EKORG
        I_ESOKZ = LS_EINE-ESOKZ
        I_INFNR = I_EKPO-INFNR
        I_LIFNR = LS_EINA-LIFNR
        I_MATKL = I_EKPO-MATKL
        I_MATNR = I_EKPO-MATNR
        I_WERKS = I_EKPO-WERKS
*       I_VABME =
      TABLES
        TEKOMD  = IT_EKOMD.
    DELETE IT_EKOMD[] WHERE DATBI LT SY-DATUM.
    IF IT_EKOMD[] IS INITIAL.
      MESSAGE 'Material info record is not valid' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDIF.

*IF SY-TCODE EQ 'ME21N' OR SY-TCODE EQ 'ME22N' OR SY-TCODE EQ 'ME23N'.
*  if SY-UCOMM ne 'YES'.
*  DATA : LV_MAT TYPE ZPLANT_MAT_AGE-MATERIAL .
*  DATA : LV_FLAG TYPE ZPLANT_MAT_AGE-FLAG.
*  SELECT SINGLE MATERIAL INTO LV_MAT FROM ZPLANT_MAT_AGE WHERE MATERIAL = I_EKPO-MATNR.
*  IF LV_MAT IS NOT INITIAL.
*    SELECT SINGLE FLAG INTO LV_FLAG FROM ZPLANT_MAT_AGE WHERE PLANT = I_EKPO-WERKS AND MATERIAL = I_EKPO-MATNR.
*    IF LV_FLAG EQ 'Y' or LV_FLAG EQ ' '  .
*      IF LV_FLAG EQ 'Y' .
*     UPDATE ZPLANT_MAT_AGE SET FLAG = 'N' WHERE PLANT = I_EKPO-WERKS AND MATERIAL = I_EKPO-MATNR.
*     endif.
*    ELSE.
*     MESSAGE 'Material stock age exceeds more than allowed days' TYPE 'E'.
*    ENDIF.
*  ENDIF.
*  CLEAR : LV_MAT,LV_FLAG.
*  ENDIF.
*ENDIF.
