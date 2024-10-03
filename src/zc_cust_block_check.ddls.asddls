@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customr Block Check CDS Consumption'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity Zc_Cust_Block_Check
  with parameters
    p_bukrs : bukrs,
    p_vkorg : vkorg
  as select from kna1 as a
  association[1] to knb1 as _b on $projection.kunnr = _b.kunnr
  association[1] to knvv as _c on $projection.kunnr = _c.kunnr
{
  key _b.bukrs,
  key _c.vkorg,
  key a.kunnr,
      case when a.aufsd <> ' '  or a.faksd <> ' ' or a.lifsd <> ' '  or a.loevm <> ' '
              or a.sperr <> ' ' or a.cassd <> ' ' or a.nodel <> ' ' or _b.nodel <> ' '
              or _b.loevm <> ' ' or _c.loevm <> ' ' then 'X' else ' ' end as block,
  _b,
  _c            
}
where
      _b.bukrs = $parameters.p_bukrs
  and _c.vkorg = $parameters.p_vkorg
  and _c.vtweg = '20'
  and _c.spart = '10'
  
