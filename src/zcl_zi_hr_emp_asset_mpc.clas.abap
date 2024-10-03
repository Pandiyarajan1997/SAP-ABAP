class ZCL_ZI_HR_EMP_ASSET_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  interfaces IF_SADL_GW_MODEL_EXPOSURE_DATA .

  types:
    begin of TS_ZC_HR_EMPLOYEE_ASSET_0040TY.
      include type ZC_HR_EMPLOYEE_ASSET_0040.
  types:
      IS_EXIT type ZC_HR_EMPLOYEE_ASSET_0040-IS_EXIT,
      IS_ACTIVE type ZC_HR_EMPLOYEE_ASSET_0040-IS_ACTIVE,
    end of TS_ZC_HR_EMPLOYEE_ASSET_0040TY .
  types:
   TT_ZC_HR_EMPLOYEE_ASSET_0040TY type standard table of TS_ZC_HR_EMPLOYEE_ASSET_0040TY .
  types:
    begin of TS_ZI_PERNR_VALUE_HELPTYPE.
      include type ZI_PERNR_VALUE_HELP.
  types:
    end of TS_ZI_PERNR_VALUE_HELPTYPE .
  types:
   TT_ZI_PERNR_VALUE_HELPTYPE type standard table of TS_ZI_PERNR_VALUE_HELPTYPE .
  types:
    begin of TS_Z_C_SUBTYP_FIXEDVALUETYPE.
      include type Z_C_SUBTYP_FIXEDVALUE.
  types:
    end of TS_Z_C_SUBTYP_FIXEDVALUETYPE .
  types:
   TT_Z_C_SUBTYP_FIXEDVALUETYPE type standard table of TS_Z_C_SUBTYP_FIXEDVALUETYPE .

  constants GC_ZC_HR_EMPLOYEE_ASSET_0040TY type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZC_HR_Employee_Asset_0040Type' ##NO_TEXT.
  constants GC_ZI_PERNR_VALUE_HELPTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZI_Pernr_Value_helpType' ##NO_TEXT.
  constants GC_Z_C_SUBTYP_FIXEDVALUETYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Z_C_subtyp_FixedvalueType' ##NO_TEXT.

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  methods DEFINE_RDS_4
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GET_LAST_MODIFIED_RDS_4
    returning
      value(RV_LAST_MODIFIED_RDS) type TIMESTAMP .
ENDCLASS.



CLASS ZCL_ZI_HR_EMP_ASSET_MPC IMPLEMENTATION.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*

model->set_schema_namespace( 'ZI_HR_EMP_ASSET_SRV' ).

define_rds_4( ).
get_last_modified_rds_4( ).
  endmethod.


  method DEFINE_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
    TRY.
        if_sadl_gw_model_exposure_data~get_model_exposure( )->expose( model )->expose_vocabulary( vocab_anno_model ).
      CATCH cx_sadl_exposure_error INTO DATA(lx_sadl_exposure_error).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_med_exception
          EXPORTING
            previous = lx_sadl_exposure_error.
    ENDTRY.
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  CONSTANTS: lc_gen_date_time TYPE timestamp VALUE '20240726074228'.                  "#EC NOTEXT
 DATA: lv_rds_last_modified TYPE timestamp .
  rv_last_modified = super->get_last_modified( ).
  IF rv_last_modified LT lc_gen_date_time.
    rv_last_modified = lc_gen_date_time.
  ENDIF.
 lv_rds_last_modified =  GET_LAST_MODIFIED_RDS_4( ).
 IF rv_last_modified LT lv_rds_last_modified.
 rv_last_modified  = lv_rds_last_modified .
 ENDIF .
  endmethod.


  method GET_LAST_MODIFIED_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
*    @@TYPE_SWITCH:
    CONSTANTS: co_gen_date_time TYPE timestamp VALUE '20240726074228'.
    TRY.
        rv_last_modified_rds = CAST cl_sadl_gw_model_exposure( if_sadl_gw_model_exposure_data~get_model_exposure( ) )->get_last_modified( ).
      CATCH cx_root ##CATCH_ALL.
        rv_last_modified_rds = co_gen_date_time.
    ENDTRY.
    IF rv_last_modified_rds < co_gen_date_time.
      rv_last_modified_rds = co_gen_date_time.
    ENDIF.
  endmethod.


  method IF_SADL_GW_MODEL_EXPOSURE_DATA~GET_MODEL_EXPOSURE.
    CONSTANTS: co_gen_timestamp TYPE timestamp VALUE '20240726073532'.
    DATA(lv_sadl_xml) =
               |<?xml version="1.0" encoding="utf-16"?>|  &
               |<sadl:definition xmlns:sadl="http://sap.com/sap.nw.f.sadl" syntaxVersion="" >|  &
               | <sadl:dataSource type="CDS" name="ZC_HR_EMPLOYEE_ASSET_0040" binding="ZC_HR_EMPLOYEE_ASSET_0040" />|  &
               | <sadl:dataSource type="CDS" name="ZI_PERNR_VALUE_HELP" binding="ZI_PERNR_VALUE_HELP" />|  &
               | <sadl:dataSource type="CDS" name="Z_C_SUBTYP_FIXEDVALUE" binding="Z_C_SUBTYP_FIXEDVALUE" />|  &
               |<sadl:resultSet>|  &
               |<sadl:structure name="ZC_HR_Employee_Asset_0040" dataSource="ZC_HR_EMPLOYEE_ASSET_0040" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               | <sadl:association name="TO_SUBTYP" binding="_SUBTYP" target="Z_C_subtyp_Fixedvalue" cardinality="zeroToOne" />|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZI_Pernr_Value_help" dataSource="ZI_PERNR_VALUE_HELP" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="Z_C_subtyp_Fixedvalue" dataSource="Z_C_SUBTYP_FIXEDVALUE" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |</sadl:resultSet>|  &
               |</sadl:definition>| .

   ro_model_exposure = cl_sadl_gw_model_exposure=>get_exposure_xml( iv_uuid      = CONV #( 'ZI_HR_EMP_ASSET' )
                                                                    iv_timestamp = co_gen_timestamp
                                                                    iv_sadl_xml  = lv_sadl_xml ).
  endmethod.
ENDCLASS.
