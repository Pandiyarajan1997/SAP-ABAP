*&---------------------------------------------------------------------*
*& Report ZCUST_DAYS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCUST_DAYS.

TABLES : kna1.    " this need to be defined for fetching data in line 15.

data : wa_fcat TYPE SLIS_FIELDCAT_ALV,            " it contains all filed information i.e line 64 to 90.
       it_fcat TYPE TABLE of SLIS_FIELDCAT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.   " to bring the layout in input screen

SELECT-OPTIONS : Cust for kna1-KUNNR,
State for kna1-REGIO,
Days for kna1-ZDAYS.

SELECTION-SCREEN : END OF BLOCK b1.

**layout
*
*SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-052.
*
*  PARAMETERS p_layo LIKE disvariant-variant.
*
*SELECTION-SCREEN : END OF BLOCK b2.


*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.      "layout option for alv
*
*      DATA: ls_variant TYPE disvariant.
*
*    ls_variant-report   = sy-repid.
*    ls_variant-username = sy-uname.
*
*    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*      EXPORTING
*        is_variant    = ls_variant
*        i_save        = 'A'
*      IMPORTING
*        es_variant    = ls_variant
*      EXCEPTIONS
*        not_found     = 1
*        program_error = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ELSE.
*      P_LAYO = ls_variant-variant.
*    ENDIF.

AT SELECTION-SCREEN.    "this content is for displaying error message in selection screen

select kunnr, NAME1, REGIO, ZDAYS from kna1 into TABLE @data(it_kna1) where kunnr in @cust and regio in @state and zdays in @days.

  if it_kna1 is INITIAL.


    MESSAGE : 'No Data Found' TYPE 'E'.

    ENDIF.

" below details is for displaying data inside the table.

WA_FCAT-COL_POS = 1.
wa_fcat-FIELDNAME = 'kunnr'.             " to confirm which field to be fetched from table
wa_fcat-EMPHASIZE = 'C7'.               " for colour of the field
wa_fcat-SELTEXT_M = 'Customer Code'. "For heading of the field data
APPENd wa_fcat TO it_fcat.
CLEAR wa_fcat.


WA_FCAT-COL_POS = 2.
wa_fcat-FIELDNAME = 'name1'.
wa_fcat-SELTEXT_M = 'Cust name'.
APPENd wa_fcat TO it_fcat.
CLEAR wa_fcat.


WA_FCAT-COL_POS = 3.
wa_fcat-FIELDNAME = 'Regio'.
wa_fcat-SELTEXT_M = 'State'.
APPENd wa_fcat TO it_fcat.
CLEAR wa_fcat.


WA_FCAT-COL_POS = 4.
wa_fcat-FIELDNAME = 'Zdays'.
wa_fcat-SELTEXT_M = 'Days'.
APPENd wa_fcat TO it_fcat.
CLEAR wa_fcat.

*    DATA(ls_layo) = VALUE SLIS_LAYOUT_ALV( COLWIDTH_OPTIMIZE = abap_true
*                                      ZEBRA = abap_true ).   "data dec for layout design

*    DATA(gs_variant) = VALUE disvariant( report   = sy-repid
*                                         username = sy-uname
*                                         variant  = p_layo ).


CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   IS_LAYOUT                         = LS_LAYO
   IT_FIELDCAT                       = it_fcat
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        = GS_VARIANT

  TABLES
    T_OUTTAB                          = it_kna1
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.
