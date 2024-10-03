*&---------------------------------------------------------------------*
*&  Include           ZMARKETI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  IF RAD_PD = 'X'.

    CLEAR:R_P_GP  ,
          R_P_H  ,
          R_P_E  ,
          R_P_HS ,
          R_P_MI .

  ENDIF.

  CLEAR WA.
  IF SY-TCODE EQ 'XD02'  .
    IF SY-UCOMM EQ 'UPDA' .
      CLEAR : WA.
      "  GET PARAMETER ID 'CUST_NO' FIELD LV_KUNNR.
      IMPORT I_KUNNR TO I_KUNNR FROM MEMORY ID 'ZCUST'.
      WA-MANDT  = SY-MANDT .
      WA-KUNNR  = I_KUNNR .
      WA-GRPNM = LV_GRPNM.
      WA-P_GP = R_P_GP   .
      WA-P_H  = R_P_H    .
      WA-P_E  = R_P_E    .
      WA-P_HS = R_P_HS   .
      WA-P_MI = R_P_MI   .

      IF RAD_PD = 'X'.
        WA-CATG = 1.

      ELSEIF RAD_NPD = 'X'.
        WA-CATG = 2.

      ENDIF.




      MODIFY ZMKT_NPD FROM WA ."WHERE KUNNR = WA-KUNNR .
      CLEAR WA.
      CLEAR : R_P_GP , R_P_H , R_P_E , R_P_HS  , R_P_MI .

      WA1-KUNNR = I_KUNNR.

      IF WA1-CMPTNM1 IS NOT INITIAL.
        CLEAR : WA_ZCMPT_NAME.
        READ TABLE IT_ZCMPT_NAME INTO WA_ZCMPT_NAME WITH KEY ID = WA1-CMPTNM1.
        CLEAR : WA1-CMPTNM1, WA1-CMPTID1.
        WA1-CMPTID1 = WA_ZCMPT_NAME-ID.
        WA1-CMPTNM1 = WA_ZCMPT_NAME-NAME.
      ENDIF.

      IF WA1-CMPTNM2 IS NOT INITIAL.
        CLEAR : WA_ZCMPT_NAME.
        READ TABLE IT_ZCMPT_NAME INTO WA_ZCMPT_NAME WITH KEY ID = WA1-CMPTNM2.
        CLEAR : WA1-CMPTNM2, WA1-CMPTID2.
        WA1-CMPTID2 = WA_ZCMPT_NAME-ID.
        WA1-CMPTNM2 = WA_ZCMPT_NAME-NAME.
      ENDIF.

      IF WA1-CMPTNM3 IS NOT INITIAL.
        CLEAR : WA_ZCMPT_NAME.
        READ TABLE IT_ZCMPT_NAME INTO WA_ZCMPT_NAME WITH KEY ID = WA1-CMPTNM3.
        CLEAR : WA1-CMPTNM3, WA1-CMPTID3.
        WA1-CMPTID3 = WA_ZCMPT_NAME-ID.
        WA1-CMPTNM3 = WA_ZCMPT_NAME-NAME.
      ENDIF.

      IF WA1-CMPTNM4 IS NOT INITIAL.
        CLEAR : WA_ZCMPT_NAME.
        READ TABLE IT_ZCMPT_NAME INTO WA_ZCMPT_NAME WITH KEY ID = WA1-CMPTNM4.
        CLEAR : WA1-CMPTNM1, WA1-CMPTID1.
        WA1-CMPTID4 = WA_ZCMPT_NAME-ID.
        WA1-CMPTNM4 = WA_ZCMPT_NAME-NAME.
      ENDIF.

      IF WA1-OTHERSNM IS NOT INITIAL.
      WA1-OTHERSID = '123'.
      ENDIF.

      MODIFY ZMKT_CMPT FROM WA1.
      CLEAR WA1.

    ENDIF.
  ENDIF.










*WA-MANDT  = SY-MANDT .
*WA-KUNNR  = LV_KUNNR .
*WA-R_P_GP = R_P_GP   .
*WA-R_P_H  = R_P_H    .
*WA-R_P_E  = R_P_E    .
*WA-R_P_HS = R_P_HS   .
*WA-R_P_MI = R_P_MI   .
*
  "MODIFY ZJES FROM WA.
*
*IF SY-SUBRC = 0.
*MESSAGE 'SUCCESSFULLY SAVED......' TYPE 'S'.
*ENDIF.
* CLEAR WA.
*
*
*
*
*
*
*    WHEN 'F_SHOW'.
*
*      CLEAR : WA,
*              R_P_GP,
*              R_P_H ,
*              R_P_E ,
*              R_P_HS,
*              R_P_MI.
*
*      REFRESH IT.
*
*CONDENSE LV_KUNNR.
*
*      SELECT * FROM ZJES INTO TABLE IT WHERE KUNNR = LV_KUNNR.
*
*
*        IF SY-SUBRC = 0.
*READ TABLE IT INTO WA INDEX 1.
*
*          R_P_GP    = WA-R_P_GP .
*          R_P_H     = WA-R_P_H  .
*          R_P_E     = WA-R_P_E  .
*          R_P_HS    = WA-R_P_HS .
*          R_P_MI    = WA-R_P_MI .
*
*          CALL SCREEN 100.
*
*        ELSE.
*          MESSAGE 'NOT FOUND...' TYPE 'E'.
*
*        ENDIF.
*
*WHEN 'F_SAVE1'.
*
*WA1-MANDT = SY-MANDT.
*WA1-KUNNR = LV_KUNNR.
*
*MODIFY ZJES_C FROM WA1.
*
*IF SY-SUBRC = 0 .
*
*  MESSAGE 'SAVED' TYPE 'S'.
*
*ELSE.
*
*  MESSAGE 'NOT SAVED' TYPE 'S'.
*
*ENDIF.
*
*
*
*
*  ENDCASE.

  "ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  LIST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE LIST INPUT.
*
*
*
*
*
*
*
*ENDMODULE.                 " LIST  OUTPUT
*
**&---------------------------------------------------------------------*
**&      Module  LIST  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE LIST1 INPUT.
*
*
*
*
*
*
ENDMODULE.                 " LIST  OUTPUT
