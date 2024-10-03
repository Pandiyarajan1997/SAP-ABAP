@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Asset Tag value help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZI_AssetTag_Value_help as select from pa0040
{
@EndUserText.label: 'Asset Tag'
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.87
  key  asset_tag
}
group by asset_tag
having asset_tag <> ''
