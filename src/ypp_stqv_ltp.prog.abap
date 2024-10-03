REPORT YPP_STQV_LTP.

INCLUDE /1BCDWB/IQ000000000157DAT.

DATA %DTAB TYPE STANDARD TABLE OF /1BCDWB/IQ000000000157 WITH HEADER LINE.
" Added By Govind on 01-10-2014
DATA %MARD TYPE STANDARD TABLE OF  MARD WITH HEADER LINE.

DATA %SUBRC TYPE SY-SUBRC.

INCLUDE /1BCDWB/IQ000000000157SSCR.

INCLUDE /1BCDWB/IQ000000000157SSCRAT.


START-OF-SELECTION.
  IF %RUNMODE-EXTR_ON <> SPACE.
    CALL FUNCTION '/1BCDWB/IQ000000000157EXTR'
      TABLES
        %SELOPT = %SELOPTIONS
        %DTAB   = %DTAB
      CHANGING
        %RTMODE = %RUNMODE
      EXCEPTIONS
        NO_DATA = 1
        OTHERS  = 2.
    %SUBRC = SY-SUBRC.
    CALL FUNCTION 'RSAQRT_CHECK_EXTR'
      EXPORTING
        EXTR_SUBRC = %SUBRC
      TABLES
        DTAB       = %DTAB
      CHANGING
        RTMODE     = %RUNMODE.
  ENDIF.

*************************" Added By Govind on 01-10-2014 *************************
*break-point.

  DELETE %DTAB WHERE MNG02 = 0.
  SORT %DTAB BY WERKS MATNR LABST DESCENDING .
  SELECT * FROM MARD INTO TABLE %MARD FOR ALL ENTRIES IN %DTAB WHERE MATNR = %DTAB-MATNR  AND WERKS = %DTAB-WERKS .
  SORT %DTAB BY WERKS MATNR SPEME DESCENDING .

  DELETE %MARD[] WHERE SPEME = 0.

  LOOP AT %MARD .
    LOOP AT %DTAB  WHERE MATNR = %MARD-MATNR AND  WERKS = %MARD-WERKS.
      IF SY-SUBRC = 0.
        %DTAB-SPEME = %DTAB-SPEME + %MARD-SPEME .
      ENDIF.
      MODIFY %DTAB TRANSPORTING SPEME.
    ENDLOOP.
  ENDLOOP.

END-OF-SELECTION.
*DELETE %DTAB WHERE MNG02 = 0..
  SORT %DTAB BY WERKS MATNR LABST DESCENDING ."  ascending.
*BREAK-POINT.
  DELETE ADJACENT DUPLICATES FROM %DTAB COMPARING WERKS MATNR MAKTX MNG02.
************************************************************************************

  IF %RUNMODE-SHOW_ON <> SPACE.
    CALL FUNCTION '/1BCDWB/IQ000000000157SHOW'
      TABLES
        %DTAB   = %DTAB
      CHANGING
        %RTMODE = %RUNMODE.
  ENDIF.


*----------------------------------------------------------------
*    special code for old API and BW extractor calls
*----------------------------------------------------------------

FORM %SET_DATA CHANGING P_LINES TYPE I.

  IMPORT LDATA TO %DTAB FROM MEMORY ID 'AQLISTDATA'.
  DESCRIBE TABLE %DTAB LINES P_LINES.
  FREE MEMORY ID 'AQLISTDATA'.

ENDFORM.                    "%set_data

*&---------------------------------------------------------------------*
*&      Form  %get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DTAB     text
*      -->P_FIRST    text
*      -->P_LAST     text
*----------------------------------------------------------------------*
FORM %GET_DATA TABLES P_DTAB  STRUCTURE %DTAB
               USING  P_FIRST TYPE I
                      P_LAST  TYPE I.

  APPEND LINES OF %DTAB FROM P_FIRST TO P_LAST TO P_DTAB.

ENDFORM.                    "%get_data

*&---------------------------------------------------------------------*
*&      Form  %get_ref_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LID      text
*      -->P_REF      text
*----------------------------------------------------------------------*
FORM %GET_REF_TO_TABLE USING P_LID   TYPE AQL_LID
                             P_REF   TYPE REF TO DATA
                             P_SUBRC TYPE I.

  IF P_LID = %IQID-LID.
    CREATE DATA P_REF LIKE %DTAB[].
    P_SUBRC = 0.
  ELSE.
    P_SUBRC = 4.
  ENDIF.

ENDFORM.                    "%get_ref_to_table
