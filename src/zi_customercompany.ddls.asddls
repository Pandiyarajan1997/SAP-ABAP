@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer based on Comapny Code'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CustomerCompany
 as select from I_CustomerCompany 
  
{
   key Customer as kunnr,
   key CompanyCode as bukrs,
    PhysicalInventoryBlockInd as sperr,
    DeletionIndicator as loevm,
    DeletionIsBlocked as nodel
    
}
