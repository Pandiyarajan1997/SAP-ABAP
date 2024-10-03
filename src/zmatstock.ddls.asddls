@AbapCatalog.sqlViewName: 'ZCURTSTK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Current Day Stock'
define view ZMATSTOCK
    with parameters matnr : matnr,
                    werks : werks_d
as select from marc as a 
inner join mard as b
on a.matnr = b.matnr
and a.werks = b.werks
inner join mbew as c
on a.matnr = c.matnr
and a.werks = c.bwkey
{
 key a.matnr as material,
 key a.werks as plant,
 a.eisbe as safetystk,
 b.labst as unrestrictedstk, 
 c.verpr as movavgprice,
 b.labst * c.verpr as unresvalue
} where a.matnr = $parameters.matnr and a.werks = $parameters.werks

group by
         a.matnr,
         a.werks,
         a.eisbe,
         b.labst,
         c.verpr
         
