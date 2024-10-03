*&---------------------------------------------------------------------*
*& Report  MP9017BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9017bi.

TABLES: p9017.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9017.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9017 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9017-SRNO'
  p9017-srno.
  PERFORM fill_field(rhaltd00) USING
  'P9017-IDNO'
  p9017-idno.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9017-DUMMY' P9017-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
