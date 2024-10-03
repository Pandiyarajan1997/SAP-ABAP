*&---------------------------------------------------------------------*
*&  Include           ZCUST_CRE_CON_DAILY_TOP
*&---------------------------------------------------------------------*

        TYPES : BEGIN OF GS_KNA1,
                KUNNR TYPE KNA1-KUNNR,
                LAND1 TYPE KNA1-LAND1,
                NAME1 TYPE KNA1-NAME1,
                NAME2 TYPE KNA1-NAME2,
                ORT01 TYPE KNA1-ORT01,
                PSTLZ TYPE KNA1-PSTLZ,
                REGIO TYPE KNA1-REGIO,
                SORTL TYPE KNA1-SORTL,
                LIFSD TYPE KNA1-LIFSD,
                LOEVM TYPE KNA1-LOEVM,
                WERKS TYPE KNA1-WERKS,
             END OF GS_KNA1.

        TYPES : BEGIN OF GS_KNB1,
                  KUNNR TYPE KNB1-KUNNR,
                  BUKRS TYPE KNB1-BUKRS,
                  ERDAT TYPE KNB1-ERDAT,
                  ERNAM TYPE KNB1-ERNAM,
                  SPERR TYPE KNB1-SPERR,
                  LOEVM TYPE KNB1-LOEVM,
                END OF GS_KNB1.

        TYPES : BEGIN OF GS1_ZCUST_CON,
                  COMPANY_CODE TYPE ZCUST_CONDITION-COMPANY_CODE,
                  CONDITION_ID TYPE ZCUST_CONDITION-CONDITION_ID,
                  CONDITION_DES TYPE ZCUST_CONDITION-CONDITION_DES,
                  TO_DAY TYPE ZCUST_CONDITION-TO_DAY,
                  PERCENTAGE TYPE ZCUST_CONDITION-PERCENTAGE,
                  ROW_COUNT TYPE I,
                END OF GS1_ZCUST_CON.

        TYPES : BEGIN OF GS_ZCUST_CON,
                   COMPANY_CODE TYPE ZCUST_CONDITION-COMPANY_CODE,
                   CONDITION_ID TYPE ZCUST_CONDITION-CONDITION_ID,
                   CONDITION_DES TYPE ZCUST_CONDITION-CONDITION_DES,
                   FROM_DAY TYPE ZCUST_CONDITION-TO_DAY,
                   TO_DAY TYPE ZCUST_CONDITION-TO_DAY,
                   PERCENTAGE TYPE ZCUST_CONDITION-PERCENTAGE,
        END OF GS_ZCUST_CON.

        TYPES : BEGIN OF GS_ZCUST_CRE_CON,
                   BUKRS TYPE ZCUST_CRE_CON-BUKRS,
                   KUNNR TYPE ZCUST_CRE_CON-KUNNR,
                   NAME1 TYPE ZCUST_CRE_CON-NAME1,
                   KLIMK TYPE ZCUST_CRE_CON-KLIMK,
                   KLIMK_CUR TYPE ZCUST_CRE_CON-KLIMK_CUR,
                   FBL5N_BAL TYPE ZCUST_CRE_CON-FBL5N_BAL,
                 END OF GS_ZCUST_CRE_CON.

      TYPES : BEGIN OF GS_ZCUST_ACC_TYPE,
              BUKRS TYPE ZCUST_ACC_TYPE-BUKRS,
              TYP_ID TYPE ZCUST_ACC_TYPE-TYP_ID,
              TYP_NAM TYPE ZCUST_ACC_TYPE-TYP_NAM,
              ACC_TYPE TYPE ZCUST_ACC_TYPE-ACC_TYPE,
              NO_OF_TIME TYPE ZCUST_ACC_TYPE-NO_OF_TIME,
              AMOUNT TYPE ZCUST_ACC_TYPE-AMOUNT,
            END OF GS_ZCUST_ACC_TYPE.

  TYPES : BEGIN OF GS_FINAL,
                     KUNNR TYPE KNB1-KUNNR,
                     BUKRS TYPE KNB1-BUKRS,
                     ERDAT TYPE KNB1-ERDAT,
                     ERNAM TYPE KNB1-ERNAM,
                     SPERR TYPE KNB1-SPERR,
                     LOEVM TYPE KNB1-LOEVM,
                     DAYS TYPE RFPOS-VERZN,
                     PER TYPE RFPOS-VERZN,
                     AFT_MINUS TYPE ZCUST_CRE_CON-KLIMK,
                  END OF GS_FINAL.

        DATA : IT_KNB1 TYPE TABLE OF GS_KNB1,
               WA_KNB1 TYPE GS_KNB1.

        DATA : IT1_KNB1 TYPE TABLE OF GS_KNB1,
               WA1_KNB1 TYPE GS_KNB1.

        DATA : IT1_ZCUST_CON TYPE TABLE OF GS1_ZCUST_CON,
               WA1_ZCUST_CON TYPE GS1_ZCUST_CON.

        DATA : IT2_ZCUST_CON TYPE TABLE OF GS1_ZCUST_CON,
               WA2_ZCUST_CON TYPE GS1_ZCUST_CON.

        DATA : IT_ZCUST_CON TYPE TABLE OF GS_ZCUST_CON,
               WA_ZCUST_CON TYPE GS_ZCUST_CON.

        DATA : GT_ZCUST_CRE_CON TYPE TABLE OF GS_ZCUST_CRE_CON,
               WA_ZCUST_CRE_CON TYPE GS_ZCUST_CRE_CON.

        DATA : GT_ZCUST_ACC_TYPE TYPE TABLE OF GS_ZCUST_ACC_TYPE,
               WA_ZCUST_ACC_TYPE TYPE GS_ZCUST_ACC_TYPE.

        DATA : IT_FINAL TYPE TABLE OF GS_FINAL,
               WA_FINAL TYPE GS_FINAL.

        DATA : IT1_FINAL TYPE TABLE OF GS_FINAL,
               WA1_FINAL TYPE GS_FINAL.

        DATA : NO_DAYS TYPE RFPOS-VERZN .

        DATA : LV_PER TYPE ZCUST_CRE_CON-KLIMK.

        DATA : LV_KUNNR TYPE KNA1-KUNNR .
