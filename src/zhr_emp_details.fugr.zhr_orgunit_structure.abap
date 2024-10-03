FUNCTION zhr_orgunit_structure.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJID) TYPE  HRP1000-OBJID OPTIONAL
*"  TABLES
*"      RESULT_OBJEC STRUCTURE  OBJEC
*"      RESULT_STRUC STRUCTURE  STRUC
*"----------------------------------------------------------------------
*&Created By: Samsudeen M
*&Created On: 16.08.2023
*&Purpose: Organization Unit Structure Hierarchy Get
*&Reference: Ramakrishnan J
**----------------------------------------------------------------------
  DATA: lv_objid TYPE objid.

  CLEAR: lv_objid.
  IF objid IS INITIAL.
    lv_objid = '40000000'.
  ELSE.
    lv_objid = objid.
  ENDIF.


  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = lv_objid
      act_wegid      = 'O-O-S-P'
      act_plvar      = '01'
      act_begda      = sy-datum
      act_endda      = sy-datum
    TABLES
      result_objec   = result_objec
      result_struc   = result_struc
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.
  IF sy-subrc = 0.

  ENDIF.

ENDFUNCTION.
