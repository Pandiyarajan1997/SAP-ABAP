@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Employee Asset Infotype-40 '
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
@VDM.viewType: #CONSUMPTION
@OData.publish: true
@Search.searchable: true
@UI.headerInfo: { typeName: 'Employee Asset', typeNamePlural: 'Employee Assets' }
@UI.headerInfo.description: {
    type: #STANDARD,
    value: 'subtext'
    }
@UI.headerInfo.title: {
    type: #STANDARD,
    value: 'pernr'
}
@ObjectModel.action: [{enabled: true}]
@UI.presentationVariant: [{ sortOrder: [{by: 'last_changed_by'}] ,sortOrder: [{direction: #DESC}],
visualizations: [{ element: 'pernr' }],visualizations: [{type: #AS_LINEITEM}] }]
@ObjectModel.semanticKey: [ 'pernr' ]
define view entity ZC_HR_Employee_Asset_0040
  as select from ZI_HR_Employee_Asset_0040
{
    @UI.facet: [{ id: 'Collection',
                   position: 10,
                   purpose: #STANDARD,
                   type: #COLLECTION,
                   label: 'General Information' },
                   { id: 'Idr1',
                     parentId: 'Collection',
                     purpose: #STANDARD,
                     type: #IDENTIFICATION_REFERENCE,
                     targetQualifier: 'IDREF1',
                     label: ''  }]
      @EndUserText.label: 'Employee No'
      @Search.defaultSearchElement: true
      //      ,
      //      { type: #FOR_ACTION, invocationGrouping: #CHANGE_SET,position: 1,  dataAction: 'MPC_EXT:Exits', label: 'Exits' }
      //      { type: #FOR_ACTION,invocationGrouping: #CHANGE_SET,position: 1,  dataAction: 'MPC_EXT:Active', label: 'Active' }
      //]
      @UI.selectionField: [{ position: 10  }]
      @Consumption.valueHelpDefinition: [{ entity: {
          name: 'ZI_Pernr_Value_help',
          element: 'Personnel_Number'
      }, distinctValues: true }]
      @UI.lineItem: [{ position: 10 }]
  key pernr,
      @EndUserText.label: 'Serial No'
      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 60 }]
  key serial_no,
      @EndUserText.label: 'Subtype'
      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 20 }]
      @ObjectModel.text.element: [ 'subtext' ]
      @UI.textArrangement: #TEXT_ONLY
      @UI.selectionField: [{ position: 60 }]
      @Consumption.valueHelpDefinition: [{ entity: {
               name: 'Z_C_subtyp_Fixedvalue',
               element: 'Subty'
           } }]
      subty,
      @EndUserText.label: 'End date'
//      @UI.lineItem: [{ position: 100 }]
     @UI.identification: [{ position: 20, importance: #HIGH,qualifier: 'IDREF1' }]
      endda,
      @EndUserText.label: 'Start date'
      @Search.defaultSearchElement: true
      @UI.selectionField: [{ position: 20 }]
      @UI.lineItem: [{ position: 30 }]
      @Consumption.filter.selectionType: #INTERVAL
      @Consumption.filter.mandatory: true
      begda,
      @EndUserText.label: 'Asset Number'
      @UI.selectionField: [{ position: 30 }]
      @UI.lineItem: [{ position: 50 }]
       @Consumption.valueHelpDefinition: [{ entity: {
          name: 'ZI_lobnr_Value_help',
          element: 'lobnr'
      }, distinctValues: true }]
      lobnr,
      @EndUserText.label: 'Asset Tag'
      @UI.selectionField: [{ position: 40 }]
      @UI.lineItem: [{ position: 70 }]
            @Consumption.valueHelpDefinition: [{ entity: {
          name: 'ZI_AssetTag_Value_help',
          element: 'asset_tag'
      }, distinctValues: true }]
      asset_tag,
      @EndUserText.label: 'CoCode'
      @Search.defaultSearchElement: true
      @UI.selectionField: [{ position: 50 }]
      @UI.lineItem: [{ position: 80 }]
         @Consumption.valueHelpDefinition: [{ entity: {
          name: 'ZI_Company_Value_help',
          element: 'bukrs'
      }, distinctValues: true }]
      @UI.textArrangement: #TEXT_SEPARATE
      bukrs,
//            "ZI_Company_Value_help
      @EndUserText.label: 'Plant'
      //  @UI.selectionField: [{ position: 60 }]
      @UI.lineItem: [{ position: 90 }]
      werks,
      @EndUserText.label: 'Cost Center'
//      @UI.lineItem: [{ position: 40 }]
     @UI.identification: [{ position: 10, importance: #HIGH,qualifier: 'IDREF1' }]
      kostl,
      @UI.hidden: true
      _subtyp.Stext as subtext,
      @UI.hidden: true
      last_changed_by,
      //      @UI.hidden: true
      //      cast( 'X' as boole_d ) as Exits,
      //      cast( 'X' as boole_d ) as Active,
      _subtyp


}
