@AbapCatalog.sqlViewAppendName: 'ZZMARC_V'
@EndUserText.label: 'Append in MARC'
//@AccessControl.authorizationCheck:#NOT_REQUIRED
extend view nsdm_e_marc with ZZMARC_E {
    supply_quantity,
    suplly_uom
}
