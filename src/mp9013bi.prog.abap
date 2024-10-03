*&---------------------------------------------------------------------*
*& Report  MP9013BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9013bi.

TABLES: p9013.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9013.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9013 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9013-CATEGORY'
  p9013-category.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9013-DUMMY' P9013-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
