*&---------------------------------------------------------------------*
*&  Include           ZXM06U17
*&--------------------------------------------------------------------
  IF SY-TCODE EQ 'ME21N' OR SY-TCODE EQ 'ME22N'.
    IF NEKKO-BSART EQ 'YUB'.
      IF NEKKO-VERKF EQ ' '.
        MESSAGE E010(ZME21N).
      ENDIF.
    ENDIF.
  ENDIF.
