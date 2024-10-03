*----------------------------------------------------------------------*
*   INCLUDE RVKREALV                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialisieren des FeldKatalogs
*----------------------------------------------------------------------*
FORM fieldcat_init USING u_kr_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

***********************************************************************
* FSCM: SAP Credit Management (since ERP 2005)
  STATICS: sv_cm_checked,
           sv_ukm_active,
           sv_ukm_erp2005.

  IF sv_cm_checked IS INITIAL.
    CALL FUNCTION 'UKM_IS_ACTIVE'
      IMPORTING
        e_active  = sv_ukm_active
        e_erp2005 = sv_ukm_erp2005.
    sv_cm_checked = 'X'.
  ENDIF.
***********************************************************************

  IF NOT flg_kkl IS INITIAL.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname    = 'VBELN'.
    ls_fieldcat-tabname      = k_tabname_item.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'STATUS_BEL'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-icon       = 'X'.
    ls_fieldcat-ddictxt     = 'S'.
    ls_fieldcat-outputlen  = 4.
    ls_fieldcat-rollname   = 'RVKRED_STATUS_ALV'.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname   = 'VBELN'.
    ls_fieldcat-key         = 'X'.
    ls_fieldcat-tabname     = k_tabname_header.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ABGRU_NEW'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 2.
    ls_fieldcat-rollname   = 'ABGRU_VA'.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ACTIV'.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'DUMMY'.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'DUMMY'.
    ls_fieldcat-tabname    = k_tabname_item.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname  = 'COL'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 3.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'SBGRP_NEW'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 3.
    ls_fieldcat-rollname   = 'SBGRP_CM'.
    ls_fieldcat-tabname    = k_tabname_header.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

*    *    ***    Added by anila
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'UNAME'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-tabname    = k_tabname_item.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'REGIO'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-tabname    = k_tabname_item.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ZWIDY'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-tabname    = k_tabname_item.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

***

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              i_program_name         = k_repid
              i_internal_tabname     = k_tabname_header
              i_structure_name       = 'VBKRED'
*           I_CLIENT_NEVER_DISPLAY = 'X'
         CHANGING
              ct_fieldcat            = u_kr_fieldcat[].

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              i_program_name         = k_repid
              i_internal_tabname     = k_tabname_item
              i_structure_name       = 'VBKREDET'
*           I_CLIENT_NEVER_DISPLAY = 'X'
         CHANGING
              ct_fieldcat            = u_kr_fieldcat[].

    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CMNGV'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'NAME1'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                   TRANSPORTING sp_group WHERE fieldname = 'VBELN'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                   TRANSPORTING sp_group WHERE fieldname = 'KWKKB'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'AWAER'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'KLPRZ'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'ZTERM'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CTLPC'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CMGST'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CSTAT'.
  ELSE.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'POSNR'.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'CMPRE'.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ACTIV'.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'DUMMY'.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'STATUS_BEL'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-icon       = 'X'.
    ls_fieldcat-ddictxt     = 'S'.
    ls_fieldcat-outputlen  = 4.
    ls_fieldcat-rollname   = 'RVKRED_STATUS_ALV'.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ABGRU_NEW'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 2.
    ls_fieldcat-rollname   = 'ABGRU_VA'.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'COL'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 3.
    ls_fieldcat-tech         = 'X'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'SBGRP_NEW'.
    ls_fieldcat-datatype   = 'CHAR'.
    ls_fieldcat-outputlen  = 3.
    ls_fieldcat-rollname   = 'SBGRP_CM'.
    ls_fieldcat-sp_group   = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

*    ***    Added by anila
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'UNAME'.
    ls_fieldcat-seltext_l  = 'User Name'.
    ls_fieldcat-seltext_m  = 'User Name'.
    ls_fieldcat-seltext_s  = 'User Name'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'REGIO'.
    ls_fieldcat-seltext_l  = 'Cutomer region'.
    ls_fieldcat-seltext_m  = 'Cutomer reg.'.
    ls_fieldcat-seltext_s  = 'Cust. reg.'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'ZWIDY'.
    ls_fieldcat-seltext_l  = 'With in days'.
    ls_fieldcat-seltext_m  = 'Within. days'.
    ls_fieldcat-seltext_s  = 'Wit.days.'.
    ls_fieldcat-tech         = 'X'.
    ls_fieldcat-sp_group = 'A'.
    APPEND ls_fieldcat TO u_kr_fieldcat.

***

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              i_program_name         = k_repid
*             i_internal_tabname     = k_tabname_header
              i_structure_name       = 'VBKRED'
*           I_CLIENT_NEVER_DISPLAY = 'X'
         CHANGING
              ct_fieldcat            = u_kr_fieldcat[].

    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CMNGV'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'NAME1'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                   TRANSPORTING sp_group WHERE fieldname = 'VBELN'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                   TRANSPORTING sp_group WHERE fieldname = 'KWKKB'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'AWAER'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'KLPRZ'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'ZTERM'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CTLPC'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CMGST'.
    CLEAR ls_fieldcat.
    ls_fieldcat-sp_group = 'A'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                        TRANSPORTING sp_group WHERE fieldname = 'CSTAT'.
  ENDIF.

***********************************************************************
* FSCM: SAP Credit Management
  IF NOT sv_ukm_erp2005 IS INITIAL.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'CMPS_CM'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat     " -> display this field!
                   TRANSPORTING no_out WHERE fieldname = 'CMPS_CM'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = 'CMPS_TE'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat     " -> display this field!
                   TRANSPORTING no_out WHERE fieldname = 'CMPS_TE'.
    CLEAR ls_fieldcat.
    ls_fieldcat-tech = 'X'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
              TRANSPORTING tech WHERE "fieldname = 'KNKLI' OR
*                                      fieldname = 'NAME1' OR
*                                      fieldname = 'ORT01' OR
*                                      fieldname = 'LAND'  OR
*                                      fieldname = 'ADRNR' OR
*                                      fieldname = 'GRUPP' OR
*                                      fieldname = 'SBGRP' OR
*                                      fieldname = 'CMNUP' OR
*                                      fieldname = 'CMTFG' OR
                                      fieldname = 'CMKUA' OR
                                      fieldname = 'AMTBL' OR
                                      fieldname = 'KLPRZ' OR
                                      fieldname = 'KWKKD' OR
                                      fieldname = 'KWKKE' OR
                                      fieldname = 'OEIKW' OR
                                      fieldname = 'OLIKW' OR
                                      fieldname = 'OFAKW' OR
                                      fieldname = 'WAERS' OR
                                      fieldname = 'AWAER' OR
*                                      fieldname = 'CSTAT' OR
                                      fieldname = 'CMPSA' OR
                                      fieldname = 'CMPSB' OR
                                      fieldname = 'CMPSE' OR
                                      fieldname = 'CMPSF' OR
                                      fieldname = 'CMPSG' OR
                                      fieldname = 'CMPSH' OR
*                                      fieldname = 'CMPSI' OR
*                                      fieldname = 'CMPSJ' OR
*                                      fieldname = 'CMPSK' OR
                                      fieldname = 'CMPSL' OR
                                      fieldname = 'CMPSM' OR
                                      fieldname = 'CMPS0' OR
                                      fieldname = 'CMPS1' OR
                                      fieldname = 'CMPS2' OR "ERP2004
                                      fieldname = 'SKFOR' OR
                                      fieldname = 'SSOBL' OR
                                      fieldname = 'CRBLB' OR
                                      fieldname = 'NXTRV' OR
                                      fieldname = 'KRAUS' OR
                                      fieldname = 'DBPAY' OR
                                      fieldname = 'DBRTG' OR
                                      fieldname = 'REVDB' OR
                                      fieldname = 'KLIMK' OR
                                      fieldname = 'TAGEF' OR
                                      fieldname = 'AUST1' OR
                                      fieldname = 'AUST2' OR
                                      fieldname = 'AUST3' OR
                                      fieldname = 'AUST5' OR
                                      fieldname = 'AUST1TXT' OR
                                      fieldname = 'AUST2TXT' OR
                                      fieldname = 'AUST3TXT' OR
                                      fieldname = 'AGNAME1'  OR
                                      fieldname = 'AGORT01'  OR
                                      fieldname = 'AGADRNR'  OR
                                      fieldname = 'VBKLA'
                                           .
  ELSE.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CMPS_CM'.
    ls_fieldcat-tech      = 'X'.
    MODIFY u_kr_fieldcat FROM ls_fieldcat
                   TRANSPORTING tech WHERE fieldname = 'CMPS_CM'.
  ENDIF.
***********************************************************************

ENDFORM.                               " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*      Ereignistabelle Bilden
*      EVENTS TABLE BUILD
*----------------------------------------------------------------------*
FORM eventtab_build USING u_kr_events TYPE slis_t_event.

  DATA: ks_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = u_kr_events.
  READ TABLE u_kr_events WITH KEY name = slis_ev_top_of_page
                           INTO ks_event.
  IF sy-subrc = 0.
    MOVE kr_formname_top_of_page TO ks_event-form.
    APPEND ks_event TO u_kr_events.
  ENDIF.
  READ TABLE u_kr_events WITH KEY name = slis_ev_foreign_top_of_page
                           INTO ks_event.
  IF sy-subrc = 0.
    MOVE 'PROTOKOLL_HEADER'  TO ks_event-form.
    APPEND ks_event TO u_kr_events.
  ENDIF.
  READ TABLE u_kr_events WITH KEY name = slis_ev_user_command
                           INTO ks_event.
  IF sy-subrc = 0.
    MOVE k_user_command TO ks_event-form.
    APPEND ks_event TO u_kr_events.
  ENDIF.
  READ TABLE u_kr_events WITH KEY name =  slis_ev_pf_status_set
                           INTO ks_event.
  IF sy-subrc = 0.
    MOVE k_status TO ks_event-form.
    APPEND ks_event TO u_kr_events.
  ENDIF.

ENDFORM.                               " EVENTTAB_BUILD

*&---------------------------------------------------------------------*
*&      Form  SP_GROUP_BUILD
*&---------------------------------------------------------------------*
*       Textzuordnung für die entsprechende Gruppe
*       textassign for the fieldgroup
*----------------------------------------------------------------------*
FORM sp_group_build USING    u_kr_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.

  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = text-150.
  APPEND ls_sp_group TO u_kr_sp_group.

ENDFORM.                               " SP_GROUP_BUILD
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Initialisieren der Anzeigevariante
*----------------------------------------------------------------------*
FORM variant_init.

  CLEAR k_variant.
  k_variant-report = k_repid.
  k_variant-log_group = code.

ENDFORM.                               " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*      Eingabe-Hilfe von Varianten
*      INPUT-HELP FOR VARIANTS
*----------------------------------------------------------------------*
FORM f4_for_variant USING    us_k_variant  LIKE  disvariant
                             us_k_save
                    CHANGING ch_kr_variant LIKE  disvariant .

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = us_k_variant
            i_save              = us_k_save
            i_tabname_header    = k_tabname_header
            i_tabname_item      = k_tabname_item
*           it_default_fieldcat =
       IMPORTING
            e_exit              = k_exit
            es_variant          = ch_kr_variant
       EXCEPTIONS
            not_found = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF k_exit = space.
      p_vari = ch_kr_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                               " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*         After Input for inputvariant
*----------------------------------------------------------------------*
FORM pai_of_selection_screen.

  IF NOT p_vari IS INITIAL.
    MOVE k_variant TO kr_variant.
    MOVE p_vari TO kr_variant-variant.
    MOVE code   TO kr_variant-log_group.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE' "Überprüfung der ex.
         EXPORTING                     " einer Anz.Var. auf DB.
              i_save     = k_save
         CHANGING
              cs_variant = kr_variant.
    k_variant = kr_variant.
  ELSE.
    PERFORM variant_init.
  ENDIF.

ENDFORM.                               " PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*           Form für die Anzeigeoptionen
*           form for output-option
*----------------------------------------------------------------------*
FORM layout_build USING    u_kr_layout TYPE slis_layout_alv.

  u_kr_layout-box_fieldname       = 'SELKZ'.  " Checkbox
  u_kr_layout-box_tabname         = 'POSTAB'.
  u_kr_layout-info_fieldname      = 'COL'.    "Zeilenfarbe
  u_kr_layout-confirmation_prompt = 'X'.       "Sicherheitsabfrage
*  u_kr_layout-f2code              =  'DETA' .    " Doppelklickfunktion
  u_kr_layout-key_hotspot         = 'X'.       "Schlüssel als Hotspot
  u_kr_layout-get_selinfos        = 'X'.

ENDFORM.                               " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_HIERSEQ_LIST_DISPLAY
*----------------------------------------------------------------------*
*       Die Ausgabe-Tabelle mit ALV ausgeben (Hierarchisch)
*       output the table with ALV technic. (2 steps)
*----------------------------------------------------------------------*
FORM reuse_alv_hierseq_list_display.

  PERFORM set_tooltip.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = k_repid
           i_callback_pf_status_set  = k_status
           i_callback_user_command   = k_user_command
            is_layout                = kr_layout
            it_fieldcat              = kr_fieldcat[]
*           IT_EXCLUDING             =
            it_special_groups        = kr_sp_group[]
*           IT_SORT                  =
*           IT_FILTER                =
*           IS_SEL_HIDE              =
*           I_SCREEN_START_COLUMN    = 0
*           I_SCREEN_START_LINE      = 0
*           I_SCREEN_END_COLUMN      = 0
*           I_SCREEN_END_LINE        = 0
*          I_DEFAULT                = K_DEFAULT
            i_save                   = k_save
            is_variant               = k_variant
            it_events                = kr_events[]
*           IT_EVENT_EXIT            =
            i_tabname_header         = k_tabname_header
            i_tabname_item           = k_tabname_item
            is_keyinfo               = kr_keyinfo
*           IS_PRINT                 =
            it_except_qinfo          = gt_exc
*      IMPORTING
*           E_EXIT_CAUSED_BY_CALLER  =
       TABLES
            t_outtab_header          = postab
            t_outtab_item            = xvbkredet.

ENDFORM.                               " REUSE_ALV_HIERSEQ_LIST_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_LIST_DISPLAY
*----------------------------------------------------------------------*
*       Die Ausgabe-Tabelle mit ALV ausgeben (einfache Liste)
*       output the table with ALV technic. (Normal list)
*----------------------------------------------------------------------*
FORM reuse_alv_list_display.

  PERFORM set_tooltip.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING

*            I_BUFFER_ACTIVE          = 'X'
*            I_INTERFACE_CHECK        = ' '
             i_callback_program       = k_repid
             i_callback_pf_status_set = k_status
             i_callback_user_command  = k_user_command
*            I_STRUCTURE_NAME         = 'VBUK'
             is_layout                = kr_layout
             it_fieldcat              = kr_fieldcat[]
*            IT_EXCLUDING             =
             it_special_groups        = kr_sp_group
*            IT_SORT                  =
*            IT_FILTER                =
*            IS_SEL_HIDE              =
*            I_DEFAULT                = 'X'
             i_save                   = k_save
             is_variant               = k_variant
             it_events                = kr_events[]
*            IT_EVENT_EXIT            =
*            IS_PRINT                 =
*            IS_REPREP_ID             =
*            I_SCREEN_START_COLUMN    = 0
*            I_SCREEN_START_LINE      = 0
*            I_SCREEN_END_COLUMN      = 0
*            I_SCREEN_END_LINE        = 0
             it_except_qinfo          = gt_exc
*     IMPORTING
*           E_EXIT_CAUSED_BY_CALLER   =
*           ES_EXIT_CAUSED_BY_USER    =
       TABLES
           t_outtab                  = postab.

ENDFORM.                               " REUSE_ALV_LIST_DISPLAY
*----------------------------------------------------------------------*
*    FORM PF_STATUS_SET
*----------------------------------------------------------------------*
*       Statussetzen
*       Status set
*----------------------------------------------------------------------*
*    --> EXTAB
*----------------------------------------------------------------------*
FORM standard_kr01 USING  extab TYPE slis_t_extab.

  DATA: extab1 LIKE extab WITH HEADER LINE.
  DATA: l_flag TYPE flag.
  extab1-fcode = 'AEND'.
  APPEND extab1 TO extab.
* deactivate if SAP Creditmanagement is active
  CALL FUNCTION 'UKM_IS_ACTIVE'
    IMPORTING
      e_active = l_flag.
  IF NOT l_flag IS INITIAL.
    APPEND 'KUS2' TO extab.
    APPEND 'KUS0' TO extab.
    APPEND 'KUS1' TO extab.
    APPEND 'VKM5' TO extab.
    APPEND 'F.31' TO extab.
  ENDIF.
*
  SET PF-STATUS 'STANDAKR' EXCLUDING extab.

ENDFORM.                    "STANDARD_KR01
*&---------------------------------------------------------------------*
*&      Form  set_tooltip
*&---------------------------------------------------------------------*
*       text Setzen Tooltips für die Icons
*----------------------------------------------------------------------*
FORM set_tooltip .

* Setzen Tooltips für die Icons
  DATA: tooltips TYPE REF TO cl_salv_tooltips,
        ls_exc TYPE alv_s_qinf.
  ls_exc-type        = cl_salv_tooltip=>c_type_icon.
  ls_exc-fieldname   = 'STATUS_BEL'.
  ls_exc-tabname     = 'POSTAB'.
  ls_exc-value       = icon_reject.
  ls_exc-text        = text-c01.
  APPEND ls_exc TO gt_exc.

  ls_exc-type        = cl_salv_tooltip=>c_type_icon.
  ls_exc-fieldname   = 'STATUS_BEL'.
  ls_exc-tabname     = 'POSTAB'.
  ls_exc-value       = icon_release.
  ls_exc-text        = text-c02.
  APPEND ls_exc TO gt_exc.

  ls_exc-type        = cl_salv_tooltip=>c_type_icon.
  ls_exc-fieldname   = 'STATUS_BEL'.
  ls_exc-tabname     = 'POSTAB'.
  ls_exc-value       = icon_check.
  ls_exc-text        = text-c03.
  APPEND ls_exc TO gt_exc.

  ls_exc-type        = cl_salv_tooltip=>c_type_icon.
  ls_exc-fieldname   = 'STATUS_BEL'.
  ls_exc-tabname     = 'POSTAB'.
  ls_exc-value       = icon_submit.
  ls_exc-text        = text-c04.
  APPEND ls_exc TO gt_exc.

ENDFORM.                    " set_tooltip
