@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Batch stocks based on plant'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMS_MCHB_CV
//with parameters p_werks : werks_d
 as select from nsdm_e_mchb as batch
 association[1] to mara as _a on $projection.Material = _a.matnr
{
   key batch.matnr as Material,
         @UI.hidden: true
   key   batch.werks as plant,
   key batch.charg as Batch,
   @Semantics.quantity.unitOfMeasure: 'meins'
    batch.clabs as qty,
//  cast('EA' as meins) as UOM
    _a.mtart as mtart,
    _a.bismt as bismt,
    _a.meins as meins
}
//where werks = $parameters.p_werks 
group by matnr,werks,charg,clabs,_a.meins,_a.mtart, _a.bismt
having clabs <> 0.000
