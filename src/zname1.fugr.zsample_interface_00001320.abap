FUNCTION ZSAMPLE_INTERFACE_00001320.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_KUNNR) LIKE  KNA1-KUNNR
*"     VALUE(I_BUKRS) LIKE  KNB1-BUKRS OPTIONAL
*"     VALUE(I_VKORG) LIKE  KNVV-VKORG OPTIONAL
*"     VALUE(I_VTWEG) LIKE  KNVV-VTWEG OPTIONAL
*"     VALUE(I_SPART) LIKE  KNVV-SPART OPTIONAL
*"     VALUE(I_XVBUP) LIKE  OFIWA-XVBUP DEFAULT 'X'
*"     VALUE(I_AKTYP) LIKE  OFIWA-AKTYP DEFAULT 'V'
*"----------------------------------------------------------------------

IF SY-TCODE EQ 'XD01' .
 COMMIT WORK.
WAIT up to 2 seconds .
  UPDATE KNA1 SET NAME3 = I_KUNNR WHERE KUNNR = I_KUNNR . "#EC CI_DB_OPERATION_OK[2265093] " Added by <IT-CAR Tool> during Code Remediation
COMMIT WORK.
"Update Name3 via API/OData

"Pan Number Updation ----added by zakir
*{ BOC
*ELSE.
*data(ls_panno) = VALUE j_1ipanno( ).
*IMPORT ls_panno = ls_panno FROM MEMORY ID 'PAN'.
*Check ls_panno is NOT INITIAL.
*  UPDATE KNA1 SET j_1ipanno = ls_panno WHERE KUNNR = I_KUNNR .
* COMMIT WORK.
*   FREE MEMORY ID 'PAN'.
   ENDIF.
*  }EOC

ENDFUNCTION.
