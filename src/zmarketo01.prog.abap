*&---------------------------------------------------------------------*
*&  Include           ZMARKETO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

IF SY-TCODE = 'XD02'. "
REFRESH: LBT_COMP,
          LBT_CLUB.
  CLEAR : LBS_COMP,
          LBS_CLUB.


  SELECT MANDT ID NAME FROM ZCMPT_NAME INTO TABLE IT_ZCMPT_NAME  .


    LOOP AT IT_ZCMPT_NAME INTO WA_ZCMPT_NAME.
       LBS_COMP-KEY = WA_ZCMPT_NAME-ID ." i. "'A'.
       LBS_COMP-TEXT =  WA_ZCMPT_NAME-NAME . " 'A'.
       APPEND LBS_COMP TO LBT_COMP.
       CLEAR LBS_COMP.
       "i = i + 1.
    ENDLOOP.

    DATA: LD_COMP TYPE VRM_ID VALUE 'WA1-CMPTNM1',
          LD_COMP1 TYPE VRM_ID VALUE 'WA1-CMPTNM2',
         LD_COMP2 TYPE VRM_ID VALUE 'WA1-CMPTNM3',
         LD_COMP3 TYPE VRM_ID VALUE 'WA1-CMPTNM4'.

   " LD_COMP = 'WA1-CMPTNM1'.
    CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       ID                    = LD_COMP
       VALUES                = LBT_COMP
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS                = 2
             .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.




    "  LD_COMP2 = 'WA1-CMPTNM2'.

   CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID                    = LD_COMP1
      VALUES                = LBT_COMP
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS                = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  " LD_COMP3 = 'WA1-CMPTNM3'.
   CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID                    = LD_COMP2
      VALUES                = LBT_COMP
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS                = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

    " LD_COMP3 = 'WA1-CMPTNM1'.

     CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID                    = LD_COMP3
      VALUES                = LBT_COMP
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS                = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDIF.
IF I = 1.

IF SY-TCODE = 'XD03' OR SY-TCODE = 'XD02'.                                              " ADDED BY JESTOP

    CLEAR: WA, WA1, RAD_PD , RAD_NPD ,R_P_GP ,R_P_H ,R_P_E ,R_P_HS , R_P_MI , LV_GRPNM.

    IMPORT I_KUNNR TO I_KUNNR FROM MEMORY ID 'ZCUST'.

    SELECT * FROM ZMKT_NPD INTO TABLE IT WHERE KUNNR = I_KUNNR ORDER BY PRIMARY KEY.

    IF SY-SUBRC = 0.

      READ TABLE IT INTO WA INDEX 1. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation

      IF WA-CATG = 1.
        RAD_PD = 'X'.
      ELSEIF WA-CATG = 2.
        RAD_NPD = 'X'.
      ENDIF.

      R_P_GP = WA-P_GP .
      R_P_H  = WA-P_H  .
      R_P_E  = WA-P_E  .
      R_P_HS = WA-P_HS .
      R_P_MI = WA-P_MI .
      LV_GRPNM = WA-GRPNM.

      SELECT * FROM  ZMKT_CMPT INTO TABLE IT1 WHERE KUNNR = I_KUNNR ORDER BY PRIMARY KEY.

      IF SY-SUBRC = 0.

        READ TABLE IT1 INTO WA1 INDEX 1. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation


      ENDIF.


   ENDIF.



  ENDIF.

  I = 2.

  ENDIF.
  ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.

    IF RAD_PD = 'X'.

    LOOP AT SCREEN.

      IF SCREEN-GROUP1 = 'RAD'.

        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.

      ENDIF.
MODIFY SCREEN.
    ENDLOOP.

  ENDIF.


   IF SY-TCODE = 'XD03'.
  LOOP AT SCREEN.
   IF SCREEN-GROUP2 = 'DIS' .
      SCREEN-INPUT = '0' .
      MODIFY SCREEN.
      ENDIF.
  ENDLOOP.
ENDIF.


ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
