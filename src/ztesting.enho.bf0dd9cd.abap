"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT\SE:END\EI
ENHANCEMENT 0 ZTESTING.
*
  IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .

  IF VBAK-AUART EQ 'YBBR'   .
  IF VBAP-PSTYV EQ 'TAN' AND  ( VBAP-MWSBP IS INITIAL or VBAP-MWSBP eq '0.00' ).
    MESSAGE 'GST is Mandatory for this Order ' TYPE 'E' .
    ENDIF.
*    leave to screen 0.
  ENDIF.

   if vbak-AUART eq 'YBDP'.
     if vbap-pstyv eq 'TAN' and vbap-mwsbp is initial.
    MESSAGE 'GST is Mandatory for this Order ' type 'E' .
    endif.
*    leave to  SCREEN 0.
    endif.

     if  vbak-AUART eq 'YBFS'.
       if vbap-pstyv eq 'TAN' and vbap-mwsbp is initial.
    MESSAGE 'GST is Mandatory for this Order ' type 'E' .
    endif.
    ENDIF.

   If vbak-AUART eq 'YIND'   .
  if vbap-pstyv eq 'TAN' and vbap-mwsbp is initial.
    MESSAGE 'GST is Mandatory for this Order ' type 'E' .
    endif.
*    leave to screen 0.
  endif.
 endif.


ENDENHANCEMENT.
