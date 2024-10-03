*&---------------------------------------------------------------------*
*& Report  MP9011BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9011bi.

TABLES: p9011.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9011.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9011 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9011-LONGITUDE'
  p9011-longitude.
  PERFORM fill_field(rhaltd00) USING
  'P9011-LATITUDE'
  p9011-latitude.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9011-DUMMY' P9011-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
