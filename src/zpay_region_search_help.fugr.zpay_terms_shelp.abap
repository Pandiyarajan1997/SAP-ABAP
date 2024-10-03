FUNCTION ZPAY_TERMS_SHELP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

TYPES : BEGIN OF GS_T052,
          ZTERM TYPE T052-ZTERM,
          "TEXT1 TYPE TEXT1_052,
        END OF GS_T052.
*
DATA : IT_T052 TYPE TABLE OF GS_T052,
       WA_T052 TYPE GS_T052.

TYPES : BEGIN OF GS_T052U,
          ZTERM TYPE T052-ZTERM,
          TEXT1 TYPE T052U-TEXT1,
        END OF GS_T052U.

DATA : IT_T052U TYPE TABLE OF GS_T052U,
       WA_T052U TYPE GS_T052U.
*
DATA : T_FIELDS LIKE TABLE OF SHLP_TAB-FIELDDESCR,
       W_FIELDS LIKE LINE OF SHLP_TAB-FIELDDESCR.

SELECT ZTERM FROM T052 INTO CORRESPONDING FIELDS OF TABLE IT_T052 WHERE KOART NE 'K'.

SELECT ZTERM TEXT1 FROM T052u INTO TABLE IT_T052U FOR ALL ENTRIES IN IT_T052 WHERE ZTERM = IT_T052-ZTERM.

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
   SOURCE_TAB              = IT_T052u
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
