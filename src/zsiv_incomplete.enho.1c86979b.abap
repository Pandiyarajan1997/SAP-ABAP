"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EN:ZSALES_REQ4\SE:BEGIN\EI
ENHANCEMENT 0 ZSIV_INCOMPLETE.

IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02'.

if  vbak-vkorg eq '4000'.

if ( vbak-auart = 'YBBR' or vbak-auart = 'YBBE' ) and  vbkd-zterm = 'AP'.
    ELSEIF ( vbak-auart = 'YBBR' or vbak-auart = 'YBBE' ) and  vbkd-zterm = 'ND'.
    ELSEIF ( vbak-auart = 'YBBR' or vbak-auart = 'YBBE' ) and  vbkd-zterm = 'APC'.
    ELSEIF ( vbak-auart = 'YBBR' or vbak-auart = 'YBBE' ) and  vbkd-zterm = 'FP'.
    ELSEIF vbak-auart = 'YIND' and  vbkd-zterm = 'ND'.
  else.

if vbak-auart = 'YBRE' OR VBAK-AUART = 'YBFD' OR VBAK-AUART = 'YBTE' .
  else.
*Commented by Samsudeen M on 20.07.2023 refered by: Praveen Kumar
*    MESSAGE E000(ZSD).
    endif.
    endif.
else.
   IF ( vbak-auart = 'YBBR' OR VBAK-AUART = 'YBBE' ) and  vbkd-zterm = 'ND'.
   ELSEIF ( vbak-auart = 'YBBR' OR VBAK-AUART = 'YBBE' ) and  vbkd-zterm = 'AP'.
   ELSEIF ( vbak-auart = 'YBBR' OR VBAK-AUART = 'YBBE' ) and  vbkd-zterm = 'APC'.
   ELSEIF ( vbak-auart = 'YBBR' OR VBAK-AUART = 'YBBE' ) and  vbkd-zterm = 'FP'.
   ELSEIF vbak-auart = 'YIND' and  vbkd-zterm = 'ND'.
  else.
    endif.
  endif.
*payment term validation end
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

If vbak-AUART eq 'YBBR'   .
  if vbap-pstyv eq 'TAN' and vbap-mwsbp is initial.
    MESSAGE 'GST is Mandatory for this Order ' type 'E' .
    endif.
*    leave to screen 0.
  endif.

If vbak-AUART eq 'YIND'   .
  if vbap-pstyv eq 'TAN' and vbap-mwsbp is initial.
    MESSAGE 'GST is Mandatory for this Order ' type 'E' .
    endif.
*    leave to screen 0.
  endif.
     ENDIF.

*Commented by Samsudeen M on 20.07.2023 refered by: Praveen Kumar
*data : flag(1) type c.
* IF sy-tcode eq 'VA01' OR SY-TCODE EQ 'VA02' OR SY-TCODE EQ 'VA03'.
*    if vbak-AUART eq 'YBBR' and vbak-VKORG eq '4000' and vbak-VTWEG eq '20' and ( vbkd-zterm eq 'AP'  or   vbkd-zterm eq 'APC' ) .
*       IF VBAK-SCH_CODE IS INITIAL.
*           flag = 'E'.
*           MESSAGE 'Cheque Number is Compulsory - Maintain in "Additional Data tabB"' type 'I' DISPLAY LIKE 'E'.
*           export flag to MEMORY id 'SET'.
*           LEAVE to SCREEN '4001'.
*       ENDIF.
*   ENDIF.
* ENDIF.

ENDENHANCEMENT.
