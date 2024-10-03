@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer based on Sales Area'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CustomerSalesArea
  as select from knvv
{
  key kunnr ,
  key  vkorg,
  key  vtweg,
  key  spart,
       aufsd as OrderIsBlockedForCustomer    ,
       lifsd as DeliveryIsBlockedForCustomer ,
       faksd as BillingIsBlockedForCustomer ,
       cassd as SalesIsBlockedForCustomer ,
       loevm as DeletionflagSalesarea
}
