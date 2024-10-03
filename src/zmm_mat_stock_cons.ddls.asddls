@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity zmm_mat_stock_cons
  as projection on zmm_mat_stock_cds
{
      @EndUserText.label: 'Material Number'
  key Material,
      @EndUserText.label: 'Plant'
  key Plant,
      @EndUserText.label: 'Storage Loc'
  key Stloc,
      @EndUserText.label: 'Batch Number'
  key Batch,
      @EndUserText.label: 'ROL Stock'
      SafetyStock,
      @EndUserText.label: 'Unrestk_qty'
      Stock,
      @EndUserText.label: 'Unrestk_value'
      Stkvalue,
      @EndUserText.label: 'Date'
      Cdate
}
