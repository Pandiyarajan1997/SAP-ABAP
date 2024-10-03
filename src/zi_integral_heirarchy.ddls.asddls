define hierarchy Zi_Integral_Heirarchy
  as parent child hierarchy (
    source ZI_HEIRARCHY_VIEW
    child to parent association _Mother
    //start where Mother is initial
    siblings order by ID ascending
  )
{
   key ID,
//   FIRSTNAME,
//   LASTNAME,
    Mother
}
