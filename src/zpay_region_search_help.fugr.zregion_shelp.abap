FUNCTION ZREGION_SHELP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"--------------------------------------------------------------------
"a


TYPES : BEGIN OF GS_T005U,
          BLAND TYPE T005U-BLAND,
          BEZEI TYPE T005U-BEZEI,
        END OF GS_T005U.
*
DATA : IT_T005U TYPE TABLE OF GS_T005U,
       WA_T005U TYPE GS_T005U.
*
DATA : T_FIELDS LIKE TABLE OF SHLP_TAB-FIELDDESCR,
       W_FIELDS LIKE LINE OF SHLP_TAB-FIELDDESCR.
SELECT BLAND BEZEI FROM T005U INTO CORRESPONDING FIELDS OF TABLE IT_T005U WHERE LAND1 = 'IN'.
   DATA : l_fname TYPE dfies-lfieldname.
if CALLCONTROL-STEP = 'SELECT' .

  LOOP AT SHLP_TAB.
    LOOP AT shlp_tab-fielddescr INTO w_fields.

            l_fname = w_fields-fieldname.
CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
  EXPORTING
    PARAMETER               = w_fields-fieldname
*   OFF_SOURCE              = 0
*   LEN_SOURCE              = 0
*   VALUE                   =
   FIELDNAME               = l_fname
  TABLES
    SHLP_TAB                = SHLP_TAB
    RECORD_TAB              = RECORD_TAB
   SOURCE_TAB              = IT_T005U
  CHANGING
    SHLP                    = SHLP
    CALLCONTROL             = CALLCONTROL
 EXCEPTIONS
   PARAMETER_UNKNOWN       = 1
   OTHERS                  = 2
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.
*

ENDLOOP.
ENDLOOP.

IF SY-SUBRC EQ 0.
  CALLCONTROL-STEP = 'DISP'.
ELSE.
  CALLCONTROL-STEP = 'EXIT'.
  ENDIF.
  EXIT.
ENDIF.






ENDFUNCTION.
