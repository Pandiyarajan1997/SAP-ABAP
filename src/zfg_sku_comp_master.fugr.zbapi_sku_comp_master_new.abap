FUNCTION ZBAPI_SKU_COMP_MASTER_NEW .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_COMP_CODE) TYPE  FLAG OPTIONAL
*"     VALUE(P_SKU_MAPPING) TYPE  FLAG OPTIONAL
*"     VALUE(P_DIST_CHAN) TYPE  FLAG OPTIONAL
*"     VALUE(P_REGION_MAS) TYPE  FLAG OPTIONAL
*"     VALUE(P_BILL_TYPE) TYPE  FLAG OPTIONAL
*"     VALUE(P_MOVE_TYPE) TYPE  FLAG OPTIONAL
*"     VALUE(P_PLANT_MAS) TYPE  FLAG OPTIONAL
*"     VALUE(P_PAYMENT_TERM_DESC) TYPE  FLAG OPTIONAL
*"     VALUE(P_SKU_MAS) TYPE  FLAG OPTIONAL
*"     VALUE(P_TAX_TYPE) TYPE  FLAG OPTIONAL
*"     VALUE(P_COND_TYPE) TYPE  FLAG OPTIONAL
*"     VALUE(P_GL01) TYPE  FLAG OPTIONAL
*"  TABLES
*"      IT_COMP_CODE TYPE  ZTT_CNF_SAL_ORG OPTIONAL
*"      IT_DIST_CHAN TYPE  ZTT_CNF_DIST_CHAN OPTIONAL
*"      IT_REGION_MAS TYPE  ZTT_CNF_REGION_MAS OPTIONAL
*"      IT_PLANT_MAS TYPE  ZTT_CNF_PLANT_MAS OPTIONAL
*"      IT_BILL_TYPE TYPE  ZTT_CNF_BILL_TYPE OPTIONAL
*"      IT_MOVE_TYPE TYPE  ZTT_CNF_MOVE_TYPE OPTIONAL
*"      IT_SKU_MAS TYPE  ZTT_CNF_SKU_MAS OPTIONAL
*"      IT_SKU_MAPPING STRUCTURE  ZSTR_CNF_SKU_MAPPING OPTIONAL
*"      IT_PAYMENT_TERM_DESC STRUCTURE  ZSTR_CNF_PAYMENT_TERM_DESC
*"       OPTIONAL
*"      IT_TAX_TYPE TYPE  ZTT_TAX_TYPE OPTIONAL
*"      IT_COND_TYPE TYPE  ZTT_COND_TYPE OPTIONAL
*"      IT_GL01 TYPE  ZTT_CNF_GL01 OPTIONAL
*"      RETURN TYPE  ZTT_BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
"
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&    AUTHOR          : VELRAJ
*&    CREATED ON      : 06-04-2015
*&    COMPANY         : CLSS
*&    TASK            : THIS FUNCTION MODULE CAN BE USED TO GET COMPANY RELATED DETAILS FOR THE SALES PRO BUSINESS
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
"test11
  IF P_COMP_CODE = 'X'.
    PERFORM GET_COMPANY_CODES         TABLES  IT_COMP_CODE
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  IF P_SKU_MAPPING = 'X'.
    PERFORM GET_SKU_MAPPING           TABLES  IT_SKU_MAPPING
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  IF P_DIST_CHAN = 'X'.
    PERFORM GET_DISTRIBUTION_CHANNNEL TABLES  IT_DIST_CHAN
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  IF P_REGION_MAS = 'X'.
    PERFORM GET_REGION                TABLES  IT_REGION_MAS
                                              RETURN.
  ENDIF.

  IF P_BILL_TYPE = 'X'.
    PERFORM GET_BILLING_TYPE          TABLES  IT_BILL_TYPE
                                              RETURN.
  ENDIF.

  IF P_MOVE_TYPE = 'X'.
    PERFORM GET_MOVEMENT_TYPE         TABLES  IT_MOVE_TYPE
                                              RETURN.
  ENDIF.

  IF P_PLANT_MAS = 'X'.
    PERFORM GET_PLANT_MASTER          TABLES  IT_PLANT_MAS
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  IF P_PAYMENT_TERM_DESC = 'X'.
    PERFORM GET_PAYTERM_DISCOUNT      TABLES  IT_PAYMENT_TERM_DESC
                                              RETURN.
  ENDIF.

  IF P_SKU_MAS = 'X'.

    PERFORM GET_SKU_MASTER            TABLES  IT_SKU_MAS
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  IF P_TAX_TYPE = 'X'.
    PERFORM GET_TAX_TYPE              TABLES  IT_TAX_TYPE
                                              RETURN.
  ENDIF.

  IF P_COND_TYPE = 'X'.
    PERFORM GET_COND_TYPE             TABLES  IT_COND_TYPE
                                              RETURN.
  ENDIF.

  IF P_GL01 = 'X'.
    PERFORM GET_GL01                  TABLES  IT_GL01
                                              RETURN
                                      USING   P_BUKRS.
  ENDIF.

  CLEAR: GV_INDEX_MSG, LV_INDEX.

ENDFUNCTION.
