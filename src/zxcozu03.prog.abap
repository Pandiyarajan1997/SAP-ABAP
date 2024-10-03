*&---------------------------------------------------------------------*
*&  Include           ZXCOZU03
*&---------------------------------------------------------------------*
*BREAK-POINT.

  DATA : LV_STRGR TYPE MARC-STRGR.

  IF SY-TCODE = 'COR1'.

  CLEAR : LV_STRGR.
  IF CAUFVD_IMP-MATNR IS NOT INITIAL AND CAUFVD_IMP-WERKS IS  NOT INITIAL.

    SELECT SINGLE STRGR FROM MARC
      INTO LV_STRGR
      WHERE MATNR = CAUFVD_IMP-MATNR
      AND   WERKS = CAUFVD_IMP-WERKS.
*
    IF LV_STRGR IS NOT INITIAL.
*
      ELSE.
*
        MESSAGE 'This material blocked for production so recheck the material code' type 'E'.
*
     ENDIF.
*
*
  ENDIF.
*
  ENDIF.
*  CLEAR : LV_STRGR.
