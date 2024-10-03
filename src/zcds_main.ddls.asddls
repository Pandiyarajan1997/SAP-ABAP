@AbapCatalog.sqlViewName: 'ZCDS_HEIRARCHY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'HEIRARCHY CDS MAIN'
define view Zcds_Main as select from zhierachy_table
 association[0..1] to ZI_HEIRARCHY_VIEW as _Mother on $projection.Mother = _Mother.ID
{
  key id         as ID,
      first_name as FIRSTNAME,
      last_name  as LASTNAME,
      mother     as Mother,
      _Mother
  
}
