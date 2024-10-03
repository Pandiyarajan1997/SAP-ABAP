FUNCTION ZC141_DELETE_DUPLICATES.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

* local data -----------------------------------------------------------
* Begin Correction 02.06.2004 0742156 **********************************
  DATA: l_fieldprop_wa   like DDSHFPROP.
  DATA: L_PARAM_TAB      like SEAHLPRES-STRING
                              occurs 0 with header line.
  DATA: L_PARAM_OLD      like SEAHLPRES-STRING.
  DATA: l_tabix          like sy-tabix,
        l_delcount       like sy-tabix.

  data:  begin of tabc_shlpstr,
       handle type i,
       startsubscreen type i,
       shlp_tab type SHLP_DESCR_TAB_T ,
       RECORD_TAB like SEAHLPRES occurs 0,
       shlp_curr type SHLP_DESCR_T,
       shlp_top type SHLP_DESCR_T,
       callcontrol like ddshf4ctrl,
       ocxinterface like ddshocxint,
       Dynpfields like Dynpread occurs 0,
       Dynpselect like Dselc occurs 0,
       dynp_info like help_info,
       tecinfo like ddshtecinf,
       end of TABC_SHLPSTR.

  data: tabc_shlptab like tabc_shlpstr occurs 0 with header line.
* End Correction 02.06.2004 0742156 ************************************

* function body --------------------------------------------------------

* EXIT immediately, if you do not want to handle this step
  IF CALLCONTROL-STEP <> 'SELONE' AND
     CALLCONTROL-STEP <> 'SELECT' AND
                                       " AND SO ON
     CALLCONTROL-STEP <> 'DISP'.
    EXIT.
  ENDIF.

* ----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
* ----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF CALLCONTROL-STEP = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

* ----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
* ----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF CALLCONTROL-STEP = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
* ----------------------------------------------------------------------
* STEP SELECT    (Select values)
* ----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step
  IF CALLCONTROL-STEP = 'SELECT'.
*   PERFORM STEP_SELECT TABLES RECORD_TAB USING SHLP CHANGING RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
* ----------------------------------------------------------------------
* STEP DISP     (Display values)
* ----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
  IF CALLCONTROL-STEP = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB USING SHLP.
* Begin Correction 02.06.2004 0742156 **********************************
     data: p_record_tab type string.
import p_record_tab = p_record_tab from memory id 'SHP'.
If p_record_tab is NOT INITIAL.
  data(len) = STRLEN( p_record_tab ).
  LOOP AT record_tab.
    CONDENSE record_tab-string.
IF record_tab-string+0(2) <> p_record_tab+0(2).
    DELETE record_tab INDEX sy-tabix.
ENDIF.
  ENDLOOP.

  ENDIF.
    READ TABLE SHLP-fieldprop INTO l_fieldprop_wa
      WITH KEY SHLPOUTPUT = ESP1_TRUE
               SHLPSELPOS = '01'.
    if sy-subrc <> 0.
      MESSAGE S125 WITH 'C140_DELETE_DUPLICATES' SY-SUBRC.
*     Interner Fehler in Funktion & (Ausnahme &)
          FREE MEMORY ID 'SHP'.
      EXIT.
    endif.
    SORT RECORD_TAB by string ASCENDING.
    DELETE RECORD_TAB WHERE string IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM record_tab COMPARING string.

    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        PARAMETER               = l_fieldprop_wa-fieldname
        FIELDNAME               = '*'
      TABLES
        SHLP_TAB                = SHLP_TAB
        RECORD_TAB              = RECORD_TAB
        RESULTS_TAB             = L_PARAM_TAB
      CHANGING
        SHLP                    = SHLP
        CALLCONTROL             = CALLCONTROL
      EXCEPTIONS
        PARAMETER_UNKNOWN       = 1
        OTHERS                  = 2.

    IF SY-SUBRC <> 0.
      MESSAGE S125 WITH 'F4UT_PARAMETER_VALUE_GET' SY-SUBRC.
*     Interner Fehler in Funktion & (Ausnahme &)
      EXIT.
    ENDIF.

    clear l_delcount. clear l_param_old.
*    SORT L_param_TAB ASCENDING.
**      DELETE ADJACENT DUPLICATES FROM L_param_TAB .
*    LOOP AT L_param_TAB.
*      IF L_PARAM_OLD = L_PARAM_TAB.
*        l_tabix  = sy-tabix - l_delcount.
*        DELETE record_tab index l_tabix.
*        l_delcount = l_delcount + 1.
*      else.
*        L_PARAM_OLD = L_PARAM_TAB.
*      endif.
*    endloop.
* End Correction 02.06.2004 0742156 ************************************
    EXIT.
  ENDIF.

ENDFUNCTION.
