FUNCTION ZFM_ZCN_SEARCH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(P_)
*"  TABLES
*"      GT_T247 STRUCTURE  T247
*"----------------------------------------------------------------------


" SELECT * FROM T005S INTO GT_T005S WHERE LAND1 = 'IN' .

SELECT * UP TO 1 ROWS FROM T247 INTO GT_T247 WHERE SPRAS EQ 'EN' ORDER BY PRIMARY KEY.
ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation


ENDFUNCTION.
