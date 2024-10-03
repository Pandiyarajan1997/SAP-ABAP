FUNCTION ZBAPI_SKU_CN_RETURN_SPRO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  ZCN_RETURNWORK-BUKRS
*"     VALUE(P_GJAHR) TYPE  ZCN_RETURNWORK-GJAHR
*"     VALUE(P_VERTN) TYPE  ZCN_RETURNWORK-VERTN
*"  TABLES
*"      LT_FINAL TYPE  ZCN_TABLE
*"----------------------------------------------------------------------

*
*data :  lt_final type standard table of Zcn_table,
*        gs_final type  Zcn_table.
*
*data : p_bukrs  type string.


CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         =  p_bukrs
 IMPORTING
   OUTPUT        =  p_bukrs .


CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         = P_VERTN
 IMPORTING
   OUTPUT        =  P_VERTN.



select  BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
        BELNR
        GJAHR
        VERTN from bseg into table  lt_final where BUKRS = P_BUKRS and  GJAHR = p_GJAHR and VERTN = p_VERTN ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
break-point.

ENDFUNCTION.
