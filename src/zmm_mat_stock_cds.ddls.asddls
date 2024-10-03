@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Stock view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity zmm_mat_stock_cds
  as select from zmm_mat_stock
{
  key material     as Material,
  key plant        as Plant,
  key stloc        as Stloc,
  key batch        as Batch,
      safety_stock as SafetyStock,
      stock        as Stock,
      stkvalue     as Stkvalue,
      cdate        as Cdate
}
