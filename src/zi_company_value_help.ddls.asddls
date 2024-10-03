@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Company code value help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZI_Company_Value_help as select from t001
{
@EndUserText.label: 'Company Code'
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8
@ObjectModel.text.element: [ 'butxt' ]
    key bukrs,
    @Semantics.text: true
    butxt
}
