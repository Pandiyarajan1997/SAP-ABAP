@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fixed values CDS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MASTER
}
@ObjectModel.resultSet.sizeCategory: #XS
define view entity Z_C_subtyp_Fixedvalue
  as select from t591s
{
  @ObjectModel.text.element: [ 'Stext' ]
  @UI.textArrangement: #TEXT_ONLY
  key subty as Subty,
  @Semantics.text: true
      stext as Stext
//      infty as infty
}
where sprsl = $session.system_language and infty = '0040'
