*&---------------------------------------------------------------------*
*&  Include           ZX60EU01
*&---------------------------------------------------------------------*

*BREAK-POINT.

IF FXPBPT-WERKS = '1101'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1101 plant' TYPE 'E'.
 ELSEIF  FXPBPT-WERKS = '1102'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1102 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1103'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1103 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1104'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1104 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1105'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1105 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1107'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1107 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1108'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1108 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1111'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1111 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1112'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1112 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1126'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1126 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1127'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1127 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1128'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1128 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1129'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1129 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1131'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1131 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1132'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1132 plant' TYPE 'E'.
  ELSEIF FXPBPT-WERKS = '1133'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1133 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1134'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1134 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1151'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1151 plant' TYPE 'E'.
 ELSEIF FXPBPT-WERKS = '1152'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1152 plant' TYPE 'E'.
ELSEIF  FXPBPT-WERKS = '1154'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1154 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1155'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1155 plant' TYPE 'E'.
ELSEIF FXPBPT-WERKS = '1177'  AND FXPBPT-VERSB <> 'P1'.
  MESSAGE 'This version not valid for 1177 plant' TYPE 'E'.
 ENDIF.

* IF FXPBPT-VERVS = 'X'.
*    MESSAGE 'Remove Tick Mark & then Save' TYPE 'E'.
*   ENDIF.
