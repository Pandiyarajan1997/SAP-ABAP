@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Employee Asset Infotype-40 Basic view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
@VDM.viewType: #BASIC
define view entity ZI_HR_Employee_Asset_0040
  as select from pa0001 as a
 association[1..1] to pa0040 as b on $projection.pernr = b.pernr
 association[1..1] to pa0001 as c on $projection.pernr = c.pernr
  association [0..1] to Z_C_subtyp_Fixedvalue     as _subtyp    on  $projection.subty = _subtyp.Subty
//                                                          and _subtyp.infty     = '0040'
{
  key   a.pernr     as pernr,
  key  b.serial_no as serial_no,
       b.subty     as subty,
        a.ename     as ename,
        b.endda     as endda,
        b.begda     as begda,
        b.lobnr     as lobnr,
        b.asset_tag as asset_tag,
        b.bukrs     as bukrs,
        b.werks     as werks,
        b.kostl     as kostl,
        b.aedtm     as last_changed_by,
        //         Association
     @ObjectModel.association.type: [ #TO_COMPOSITION_CHILD ]   
        _subtyp,
        c
}
where
      a.begda <= $session.user_date
  and a.endda >= $session.user_date
  and b.begda <= $session.user_date
  and b.endda >= $session.user_date
