class ZCL_ZI_HR_EMP_ASSET_01_MPC_EXT definition
  public
  inheriting from ZCL_ZI_HR_EMP_ASSET_01_MPC
  create public .

public section.
methods define REDEFINITION.
protected section.
private section.
METHODS add_Action IMPORTING iv_action_name type /iwbep/med_external_name.
ENDCLASS.



CLASS ZCL_ZI_HR_EMP_ASSET_01_MPC_EXT IMPLEMENTATION.
  METHOD ADD_ACTION.
 data: lv_fc_fieldvalue type /iwbep/med_annotation_value,
          lo_complex_type  type ref to /iwbep/if_mgw_odata_cmplx_type,
          lo_prop          type ref to /iwbep/if_mgw_odata_property.
    TRY.
    data(lo_action) = model->create_action( iv_action_name ).

    "set return parameter
    lo_action->set_return_entity_type( 'ZC_HR_Employee_Asset_0040Type' ) .
    lo_action->set_return_entity_set( 'ZC_HR_Employee_Asset_0040' ).

    lo_action->set_http_method( 'PUT' ).
    lo_action->set_return_multiplicity( /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1 ).
    "specify input parameters
    data(lo_parameter) = lo_action->create_input_parameter(
                                  iv_parameter_name = 'pernr'
                                  iv_abap_fieldname = 'PERNR' ).
    lo_parameter->/iwbep/if_mgw_odata_property~set_type_edm_string( ).
    lo_parameter->set_maxlength( iv_max_length = 8 ).

     "Is Action Active?
    concatenate 'Is' iv_action_name into data(lv_action_ac).

     data(lo_annotation) = lo_action->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
    lo_annotation->add( iv_key = 'action-for' iv_value = 'ZC_HR_Employee_Asset_0040Type' ).

     lo_annotation = lo_action->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
    lo_annotation->add( iv_key = 'applicable-path' iv_value = lv_action_ac ).
      catch /iwbep/cx_mgw_med_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD DEFINE.
   DATA lo_annotation TYPE REF TO /iwbep/if_mgw_odata_annotation.
    DATA  lo_property1 TYPE REF TO /iwbep/if_mgw_odata_property.
    DATA  lo_entity_set TYPE REF TO /iwbep/if_mgw_odata_entity_set.
super->define( ) .
data(lo_entity_type) = model->get_entity_type( iv_entity_name = 'FileUpload' ).

  IF lo_entity_type IS BOUND.
    data(lo_property) = lo_entity_type->get_property('Mimetype').
    lo_property->set_as_content_type( ).
  ENDIF.

    lo_entity_set = model->get_entity_set( 'ZC_HR_Employee_Asset_0040' ).
    lo_annotation = lo_entity_set->create_annotation( 'sap' ).
    lo_annotation->add( iv_key = 'semantics' iv_value = 'fixed-values').
    DATA(lo_entitytype) = model->get_entity_type( 'ZC_HR_Employee_Asset_0040Type' ).
    lo_entitytype->set_is_value_list( iv_is_value_list = abap_true ).
    data(lo_txt_property) = model->get_entity_type( iv_entity_name = 'ZC_HR_Employee_Asset_0040Type' )->get_property( iv_property_name = 'subty' ).

    lo_txt_property->set_value_list(
        iv_value_list_type = /iwbep/if_mgw_odata_property=>gcs_value_list_type_property-fixed_values
    ).

    data(lo_text_anno) = lo_txt_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
    lo_text_anno->add( iv_key = 'text' iv_value = 'to_subtyp/Stext ').

     lo_txt_property = model->get_entity_type( 'Z_C_subtyp_FixedvalueType' )->get_property( 'Subty' ).

     lo_txt_property->set_value_list( /iwbep/if_mgw_odata_property=>gcs_value_list_type_property-fixed_values ).

     lo_text_anno = lo_txt_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
    lo_text_anno->add( iv_key = 'text' iv_value = 'Stext').

*add_action( iv_action_name = 'Exits' ).
*add_action( iv_action_name = 'Active' ).
  ENDMETHOD.

ENDCLASS.
