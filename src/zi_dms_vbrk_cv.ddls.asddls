@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'VVBRK entity view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMS_VBRK_CV
  //  with parameters
  //    p_vbeln : vbeln
  as select from vbrk as a
  association [1..1] to kna1 as cust  on cust.kunnr = a.kunag
  association [1..1] to vbrp as _Item on $projection.vbeln = _Item.vbeln
  association [1..1] to vbfa as _docflow on  a.vbeln = _docflow.vbeln
                                                   and _docflow.vbtyp_n = 'M'
                                                   and _docflow.vbtyp_v = 'J' 
// association[1..1] to lips as _delivery on _delivery.vbeln = _docflow.vbe                                                                           
{
  key vbeln,
      cust.kunnr,
      _Item.posnr,
            cust.werks,
      _Item.matnr,
      @Semantics.quantity.unitOfMeasure: 'meins'
      _Item.fkimg,
      _Item.meins,
      _docflow
//      _docflow.vbelv as deliveryno
//      _delivery
}
//where vbeln = $parameters.p_vbeln
