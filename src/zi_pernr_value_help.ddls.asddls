@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pernr Value help Definition'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MASTER
}
@Search.searchable: true
define view entity ZI_Pernr_Value_help
  as select from pa0001
{
      @Search.defaultSearchElement: true
      @EndUserText.label: 'Employee No'
       @Search.fuzzinessThreshold: 0.87
  key pernr as Personnel_Number,
        @EndUserText.label: 'Name'
        @Search.fuzzinessThreshold: 0.8
      ename as Name
}
group by pernr,ename
