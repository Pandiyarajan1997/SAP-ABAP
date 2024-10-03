*&---------------------------------------------------------------------*
*&  Include           ZHSBC_REPORT_MAIN
*&---------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF P_VENDOR EQ ABAP_TRUE. " 'X'.

      IF SCREEN-GROUP1 = 'CUS' OR SCREEN-GROUP1 = 'GL'.
        SCREEN-ACTIVE = 0.
      ENDIF.

    ELSE.
      IF SCREEN-GROUP1 = 'CUS' OR SCREEN-GROUP1 = 'VEN'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.




AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_HBKID.
  PERFORM F4HELP_HBKID USING   'P_HBKID'
                       CHANGING P_HBKID.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_HKTID.
  PERFORM F4HELP_HKTID USING   'P_HKTID'
                       CHANGING P_HKTID.


START-OF-SELECTION.

PERFORM DATA_RETRIVAL.
