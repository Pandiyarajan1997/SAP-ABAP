report ZPP_DEMAND.

include /1BCDWB/IQG000000000162DAT.

data %dtab type standard table of /1BCDWB/IQG000000000162 with header line.

data %subrc type sy-subrc.

include /1BCDWB/IQG000000000162SSCR.

include /1BCDWB/IQG000000000162SSCRAT.


start-of-selection.

 PERFORM AUTHCHECK_OVERVIEW.

  if %runmode-extr_on <> space.
    call function '/1BCDWB/IQG000000000162EXTR'
         tables     %selopt = %seloptions
                    %dtab   = %dtab
         changing   %rtmode = %runmode
         exceptions no_data = 1
                    others  = 2.
    %subrc = sy-subrc.
    call function 'RSAQRT_CHECK_EXTR'
         exporting extr_subrc = %subrc
         tables    dtab   = %dtab
         changing  rtmode = %runmode.
  endif.


end-of-selection.
  if %runmode-show_on <> space.
    call function '/1BCDWB/IQG000000000162SHOW'
         tables   %dtab   = %dtab
         changing %rtmode = %runmode.
  endif.


*----------------------------------------------------------------
*    special code for old API and BW extractor calls
*----------------------------------------------------------------

form %set_data changing p_lines type i.

  import ldata to %dtab from memory id 'AQLISTDATA'.
  describe table %dtab lines p_lines.
  free memory id 'AQLISTDATA'.

endform.

form %get_data tables p_dtab  structure %dtab
               using  p_first type i
                      p_last  type i.

  append lines of %dtab from p_first to p_last to p_dtab.

endform.

form %get_ref_to_table using p_lid   type aql_lid
                             p_ref   type ref to data
                             p_subrc type i.

  if p_lid = %iqid-lid.
    create data p_ref like %dtab[].
    p_subrc = 0.
  else.
    p_subrc = 4.
  endif.
endform.



*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTHCHECK_OVERVIEW .
LOOP AT %DTAB.
    AUTHORITY-CHECK OBJECT 'ZWEKS'
    ID 'ZWERKS' FIELD %DTAB-WERKS
        ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC NE 0.
            MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.
    ENDLOOP.
ENDFORM.                    " AUTHCHECK_OVERVIEW
