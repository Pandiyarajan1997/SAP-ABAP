*&---------------------------------------------------------------------*
*& Include          ZINCL_CUST_COMMS_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK K1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_invkey for lv_invkey NO INTERVALS,
                s_cust for lv_cust NO INTERVALS.
SELECTION-SCREEN:END OF BLOCK K1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN: BEGIN OF BLOCK K2 WITH FRAME TITLE text-002.
PARAMETERS: p_mail AS CHECKBOX ,
            p_whats AS CHECKBOX.
SELECTION-SCREEN:END OF BLOCK K2.

AT SELECTION-SCREEN.
IF s_invkey is INITIAL and s_cust is INITIAL.
MESSAGE 'Enter atleast one data' TYPE 'E'.
ENDIF.
IF p_mail is INITIAL and p_whats is INITIAL.
MESSAGE 'Select a Checkbox' TYPE 'E'.
ENDIF.
