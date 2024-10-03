*&---------------------------------------------------------------------*
*&  Include           ZWH_BRN_TOP
*&---------------------------------------------------------------------*

TABLES : ZWH_BRN.


TYPES : BEGIN OF GS_T001,
        BUKRS TYPE T001-BUKRS,
        BUTXT TYPE T001-BUTXT,
        END OF GS_T001.

DATA  : GT_T001 TYPE STANDARD TABLE OF GS_T001,
        WA_T001 TYPE GS_T001.


TYPES : BEGIN OF GS_T001W,
        WERKS   TYPE T001W-WERKS,
        NAME1   TYPE T001W-NAME1,
        WERKS1  TYPE T001W-WERKS,
        NAME1_1 TYPE T001W-NAME1,
        END OF GS_T001W.


DATA : GT_T001W TYPE STANDARD TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W.


TYPES : BEGIN OF GS_FINAL,
        BUKRS   TYPE T001-BUKRS,
        BUTXT   TYPE T001-BUTXT,
        WERKS   TYPE T001W-WERKS,
        NAME1   TYPE T001W-NAME1,
        WERKS1  TYPE T001W-WERKS,
        NAME1_1 TYPE T001W-NAME1,
        END OF GS_FINAL.

DATA : GT_FINAL TYPE STANDARD TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL,
       WA_FINAL1 TYPE GS_FINAL.


DATA : OK_CODE TYPE SY-UCOMM.

data : company_code type t001-bukrs,
       p_plant type t001w-werks,
       br_plant type t001w-werks.

CONTROLS : TAB TYPE TABLEVIEW USING SCREEN 9191.
