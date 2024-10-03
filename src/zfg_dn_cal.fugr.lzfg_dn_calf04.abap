*----------------------------------------------------------------------*
***INCLUDE LZFG_DN_CALF04.
*----------------------------------------------------------------------*


FORM ZDN_CAL1.


data:wa_T247 type T247.

data:WA_T005U type T005U.

data:wa_TVKBT type TVKBT.

select single * from T247 into wa_T247 where MNR =  ZDN_CAL-D_N_MONTH  and spras eq 'EN'.

*  wa_T247-LTX = ZDN_CAL-MONTH_DESC.
  ZDN_CAL-MONTH_DESC = wa_T247-LTX.

SELECT * UP TO 1 ROWS FROM T005U into WA_T005U where BLAND =  ZDN_CAL-REGION  and LAND1 eq 'IN' ORDER BY PRIMARY KEY.
ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

*WA_T005U-BEZEI = ZDN_CAL-REGION_DESC.
ZDN_CAL-REGION_DESC = WA_T005U-BEZEI.


select single * from TVKBT into WA_TVKBT where VKBUR =  ZDN_CAL-SALES_OFFICE  and spras eq 'EN'.

*  WA_TVKBT-BEZEI = ZDN_CAL-SALES_OFFICE_NAME.
  ZDN_CAL-SALES_OFFICE_NAME = WA_TVKBT-BEZEI.





  ENDFORM.
