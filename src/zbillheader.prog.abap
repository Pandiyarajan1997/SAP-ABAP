*&---------------------------------------------------------------------*
*& Report  ZBILLHEADER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
"added by ram
REPORT ZBILLHEADER.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE STATUS_0001 OUTPUT.
**  SET PF-STATUS 'xxxxxxxx'.
**  SET TITLEBAR 'xxx'.
*
*  IF SY-TCODE = 'VF03'.
*    LOOP AT SCREEN.
*      SCREEN-INPUT = '0'.
*      MODIFY SCREEN.
*      ENDLOOP.
*      ENDIF.
*
*ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  "IF VBRK-ENTRY_DATE EQ '1' .
  IF SY-TCODE = 'VF03'.
    LOOP AT SCREEN.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  "ENDIF.

*  TYPES : BEGIN OF GS_
*
*
"data : ENTRY_DATE type vbrk-ENTRY_DATE .
       "vbrk-ENTRY_DATE type vbrk-ENTRY_DATE .

*TYPES : BEGIN OF GS_VBRK,
*          ENTRY_DATE TYPE VBRK-ENTRY_DATE,
*        END OF GS_VBRK .
*DATA : GT_VBRK TYPE TABLE OF GS_VBRK,
*       WA_VBRK TYPE GS_VBRK .
*  IF SY-TCODE = 'VF02'.
*
*    DATA: V_WERKS LIKE LTAP-WERKS VALUE '1200'.

    LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'GR1'.

    "    LOOP AT GT_VBRK CURSOR  SY-DATUM TO ENTRY_DATE  .
    "    LOOP AT GT_VBRK WITH CONTROL TCvLAB CURSOR TCvLAB-CURRENT_LINE .
         "SCREEN-GROUP1-VBRK-ENTRY_DATE = SY-DATUM .
        "IF screen-NAME-VBRK-ENTRY_DATE IS INITIAL .

"ENDLOOP.

        "GR1-ENTRY_DATE = SY-DATUM .


       " MOVE SY-DATUM TO VBRK-ENTRY_DATE .


        SCREEN-INPUT = '0' .
       MODIFY SCREEN.
        "ENDIF.
    ENDIF.

  ENDLOOP.
  "ENDIF.

ENDMODULE.                 " STATUS_0002  OUTPUT
