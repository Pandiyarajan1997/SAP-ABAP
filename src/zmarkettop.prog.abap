*&---------------------------------------------------------------------*
*& Include ZMARKETTOP                                       Module Pool      ZMARKETTOP
*&
*&---------------------------------------------------------------------*

PROGRAM : ZMARKETING_PGM.

TYPE-POOLS : VRM.


DATA: LV_KUNNR TYPE KUNNR,
      R_P_GP,
      R_P_H,
      R_P_E,
      R_P_HS,
      R_P_MI,
      RAD_PD VALUE 'X',
      RAD_NPD.
"F_CODE TYPE SY-UCOMM.

DATA: WA TYPE ZMKT_NPD,
      IT TYPE TABLE OF ZMKT_NPD,

      WA1 TYPE ZMKT_CMPT,
      IT1 TYPE TABLE OF ZMKT_CMPT.

DATA: "LD_COMP    TYPE VRM_ID ,
*      LD_COMP1    TYPE VRM_ID ,
*      LD_COMP2    TYPE VRM_ID ,
*      LD_COMP3    TYPE VRM_ID ,
*      LD_COMP4    TYPE VRM_ID ,
      ID_CLUB   TYPE VRM_ID,
      IT_ZCMPT_NAME  TYPE TABLE OF ZCMPT_NAME,
      WA_ZCMPT_NAME  TYPE ZCMPT_NAME,
      LBT_COMP  TYPE VRM_VALUES,
      LBT_CLUB  TYPE VRM_VALUES,
      LBS_COMP TYPE VRM_VALUE,
      LBS_CLUB TYPE VRM_VALUE.

DATA : I_KUNNR TYPE KNA1-KUNNR.

data : lv_grpnm TYPE char40.

data : i type i VALUE '1'.
