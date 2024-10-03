*&---------------------------------------------------------------------*
*& Report  MP9010BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9010bi.

TABLES: p9010.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9010.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9010 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9010-SALESTYPE'
  p9010-salestype.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9010-DUMMY' P9010-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
