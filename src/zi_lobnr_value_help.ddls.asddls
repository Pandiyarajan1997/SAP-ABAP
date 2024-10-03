@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Asset number value help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
define view entity ZI_lobnr_Value_help as select from pa0040
{
@Search.defaultSearchElement: true
@EndUserText.label: 'Asset Number'
@Search.fuzzinessThreshold: 0.87
   key lobnr
}
group by lobnr 
having lobnr <> ' '
