*&---------------------------------------------------------------------*
*& Include          ZXQAAU01
*&---------------------------------------------------------------------*
IF ( sy-tcode = 'ZAUTO_MIGO' OR
     sy-tcode = 'ZASN_MIGO' OR
     sy-tcode = 'ZSTO_MIGO') AND
   i_zgtyp = 'ZCRT'.

  DATA: lv_cert_active(1),
        lv_mbgmcr_idoc(1),
        lv_answer(1)      VALUE 'J',
        ls_qcert_migo     TYPE qcert_migo.
  CALL FUNCTION 'QBCK_MIGO_CERT_REL_CHECK'
    EXPORTING
      i_tq05_zgtyp  = i_zgtyp
*     I_WEMNG       = 0
*     I_XBLNR       = ' '
    IMPORTING
      e_cert_active = lv_cert_active
    EXCEPTIONS
      no_tq05       = 1
      OTHERS        = 2.
  IF sy-subrc = 0 AND lv_cert_active IS NOT INITIAL.
    ls_qcert_migo-global_counter = i_ebelp / 10.
    ls_qcert_migo-answer = lv_answer.
    ls_qcert_migo-zgtyp = i_zgtyp.
    CALL FUNCTION 'QBCK_MIGO_LINE_TO_QM'
      EXPORTING
        i_qcert_migo = ls_qcert_migo.
    CLEAR ls_qcert_migo.
  ENDIF.
ENDIF.
e_tq32 = i_tq32.
