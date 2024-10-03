@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material batch stocks availability view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMS_BATCH_STOCK
  with parameters
    p1_vbeln : vbeln,
    p2_werks : werks_d
  as select from ZI_DMS_MCHB_CV as a
  association [1..1] to ZI_DMS_VBRK_CV as c        on  c.vbeln    = $parameters.p1_vbeln
                                                   and a.plant    = $parameters.p2_werks
                                                   and a.Material = c.matnr
 {
  key  c[1:inner].vbeln as Invoice,
       //    key c._Item[1:inner].posnr,
       //    key a.Material as Material,
  key  c[1:inner].matnr as Material,
       a.Batch,
       c.kunnr as customer,
       a.plant,
       //     @Semantics.quantity.unitOfMeasure: 'meins'
       //       key c[1:inner].fkimg,
       //       c[1:inner].meins,
       @Semantics.quantity.unitOfMeasure: 'uom'
       a.qty,
       a.meins          as uom
//       _docflow[1:inner].posnv as Del_item,
//       _del
       //       _docflow
}
group by
  c[1:inner].vbeln,
  c[1:inner].matnr,
  Batch,
  plant,
  qty,
  meins,
   c.kunnr
