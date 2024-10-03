@AbapCatalog.sqlViewName: 'ZFI_VW_TALOG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel and Incentives log for employee vendors'
define view ZFI_TRA_INC_LOG as select from zfi_travel_log
{
    key employee_no as employee_no,
    key emp_vendor as emp_vendor,
    key xblnr as reference,
    amount as amount,
    bldat as docdate,
    budat as posdate,
    overall_kms as overall_kms,
    bukrs as compcode,
    gjahr as fisyear,
    belnr as documnet_no,
    erdat as entry_date,
    er_time as entry_time,
    invtype as Invtype,
    type as errtype,
    kostl as costcenter,
    saknr as glaccount,
    msg as message 
    
}
