"Name: \PR:RQCAAP01\FO:MEDIUM_VRQ\SE:END\EI
ENHANCEMENT 0 ZAAP00F04.
*
  l_display_tab-NACHA = '8'.
  l_display_tab-TEXT  = 'PDF'.
  append l_display_tab.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      selectfield                  = 'NAST-NACHA'
      titel                        = %_p_medium_%_app_%-text
      no_pers_help_select          = c_kreuz
    IMPORTING
      ind                          = l_index
    TABLES
      fields                       = l_fields_tab
      full_table                   = l_display_tab
    EXCEPTIONS
      full_table_empty             = 1
      no_tablestructure_given      = 2
      no_tablefields_in_dictionary = 3
      more_then_one_selectfield    = 4
      no_selectfield               = 5
      OTHERS                       = 6.

  CHECK sy-subrc IS INITIAL.
  IF l_index IS INITIAL.
    MOVE c_rc04 TO p_subrc.
    EXIT.
  ENDIF.
*



ENDENHANCEMENT.
