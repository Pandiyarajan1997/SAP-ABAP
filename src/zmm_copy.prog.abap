*&---------------------------------------------------------------------*
*& Report ZMM_COPY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_COPY.

DATA:
  lv_matnr TYPE matnr,
  bdcdata  LIKE bdcdata    OCCURS 0 WITH HEADER LINE.


SELECT-OPTIONS:
 s_matnr FOR lv_matnr NO INTERVALS.

LOOP AT s_matnr INTO DATA(ls_matnr).
  PERFORM bdc_dynpro      USING 'ZRBDMATCC_R' '1000'.
  PERFORM bdc_field       USING 'P_MATNR'
                                ls_matnr-low.
  PERFORM bdc_field       USING 'P_NUMMAT'
                                '001'.
  PERFORM bdc_field       USING 'P_TEST'
                                ' '.
  PERFORM bdc_field       USING 'P_NOCHAN'
                                abap_true.
  PERFORM bdc_field       USING 'P_MARA1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MARC1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MARD1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MVKE1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MLGN1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MLGT1'
                                abap_true.
  PERFORM bdc_field       USING 'P_MBEW1'
                                abap_true.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=ONLI'.
  PERFORM bdc_dynpro      USING 'ZRBDMATCC_R' '0400'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=CRET'.
  PERFORM bdc_dynpro      USING 'ZRBDMATCC_R' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/EE'.
  PERFORM bdc_transaction USING 'ZMMCC_R'.
ENDLOOP.


MESSAGE 'Completed!' TYPE 'I'.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: lx_auth_check TYPE REF TO cx_root.
  DATA: l_auth_check_text TYPE string.
* batch input session
  DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: ctu_parameters TYPE ctu_params.
  ctu_parameters-dismode = 'N'.
  ctu_parameters-updmode = 'A'.
  ctu_parameters-racommit = 'X'. "No abortion by COMMIT WORK

  REFRESH messtab.
  TRY.
      CALL TRANSACTION tcode USING bdcdata OPTIONS  FROM ctu_parameters
                                           MESSAGES INTO messtab.
    CATCH cx_sy_authorization_error INTO lx_auth_check.
*     Authorization missing for user when executing transaction
      l_auth_check_text = lx_auth_check->get_text( ).
      sy-subrc = 99.
  ENDTRY.
  l_subrc = sy-subrc.
  REFRESH bdcdata.
ENDFORM.
