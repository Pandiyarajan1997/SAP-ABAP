@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Herirarchy Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_HEIRARCHY_VIEW
  as select from zhierachy_table
 association[1..*] to ZI_HEIRARCHY_VIEW as _Mother on $projection.Mother = _Mother.ID
{
  key id         as ID,
      first_name as FIRSTNAME,
      last_name  as LASTNAME,
      mother     as Mother,
      _Mother

}
