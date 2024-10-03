*&---------------------------------------------------------------------*
*& Include          ZHR_POSITION_CREATION_SEL
*&---------------------------------------------------------------------*
DATA: lv_pernr TYPE pa0001-pernr.
DATA: lv_org      TYPE hrp1001-sobid,
      lv_job      TYPE hrp1001-sobid,
      lv_repman   TYPE hrp1000-objid,
      lv_begda    TYPE p0001-begda,
      lv_position TYPE hrp1000-objid.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_rad1 RADIOBUTTON GROUP rad USER-COMMAND grp1 MODIF ID bl1,  "Create Poition
              p_rad2 RADIOBUTTON GROUP rad MODIF ID bl1 DEFAULT 'X',                    "CHange Reporting Manager
              p_rad6 RADIOBUTTON GROUP rad MODIF ID bl1,                    "Position Reporting Manager Change
              p_rad5 RADIOBUTTON GROUP rad MODIF ID bl1,                    "Display
              p_rad7 RADIOBUTTON GROUP rad MODIF ID bl1. "Added on 21.06.2023" Copy Position
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_rad3 RADIOBUTTON GROUP rad1 USER-COMMAND grp1 MODIF ID bl4,   "Approve Creation
              p_rad4 RADIOBUTTON GROUP rad1 MODIF ID bl4.                     "Dsiplay
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_repman  FOR lv_pernr MODIF ID bl3 MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-100.
  SELECT-OPTIONS: s_pos  FOR lv_position MODIF ID bl6.
SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_pos(03) TYPE c MODIF ID bl2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: s_orgid FOR lv_org MODIF ID bl5,
                  s_jobid FOR lv_job MODIF ID bl5,
                  s_repmg FOR lv_repman MODIF ID bl5,
                  s_begda FOR lv_begda MODIF ID bl5.
SELECTION-SCREEN END OF BLOCK b5.

*SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
*  PARAMETERS: p_chk AS CHECKBOX USER-COMMAND chk MODIF ID ch1.
*SELECTION-SCREEN END OF BLOCK b6.
