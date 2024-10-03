*&---------------------------------------------------------------------*
*& Report  ZMM_DELIVERY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT ZMM_DELIVERY.
INCLUDE /1BCDWB/IQ000000000196DAT.

DATA %DTAB TYPE STANDARD TABLE OF /1BCDWB/IQ000000000196 WITH HEADER LINE.

DATA %SUBRC TYPE SY-SUBRC.

INCLUDE /1BCDWB/IQ000000000196SSCR.

INCLUDE /1BCDWB/IQ000000000196SSCRAT.


START-OF-SELECTION.
  IF %RUNMODE-EXTR_ON <> SPACE.
    CALL FUNCTION '/1BCDWB/IQ000000000196EXTR'
         TABLES     %SELOPT = %SELOPTIONS
                    %DTAB   = %DTAB
         CHANGING   %RTMODE = %RUNMODE
         EXCEPTIONS NO_DATA = 1
                    OTHERS  = 2.
    %SUBRC = SY-SUBRC.
    CALL FUNCTION 'RSAQRT_CHECK_EXTR'
         EXPORTING EXTR_SUBRC = %SUBRC
         TABLES    DTAB   = %DTAB
         CHANGING  RTMODE = %RUNMODE.
  ENDIF.


END-OF-SELECTION.
  IF %RUNMODE-SHOW_ON <> SPACE.
    CALL FUNCTION '/1BCDWB/IQ000000000196SHOW'
         TABLES   %DTAB   = %DTAB
         CHANGING %RTMODE = %RUNMODE.
  ENDIF.


*----------------------------------------------------------------
*    special code for old API and BW extractor calls
*----------------------------------------------------------------

FORM %SET_DATA CHANGING P_LINES TYPE I.

  IMPORT LDATA TO %DTAB FROM MEMORY ID 'AQLISTDATA'.
  DESCRIBE TABLE %DTAB LINES P_LINES.
  FREE MEMORY ID 'AQLISTDATA'.

ENDFORM.

FORM %GET_DATA TABLES P_DTAB  STRUCTURE %DTAB
               USING  P_FIRST TYPE I
                      P_LAST  TYPE I.

  APPEND LINES OF %DTAB FROM P_FIRST TO P_LAST TO P_DTAB.

ENDFORM.

FORM %GET_REF_TO_TABLE USING P_LID   TYPE AQL_LID
                             P_REF   TYPE REF TO DATA
                             P_SUBRC TYPE I.

  IF P_LID = %IQID-LID.
    CREATE DATA P_REF LIKE %DTAB[].
    P_SUBRC = 0.
  ELSE.
    P_SUBRC = 4.
  ENDIF.

ENDFORM.
