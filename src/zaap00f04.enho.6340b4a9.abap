"Name: \PR:RQCAAP01\IC:AAP00T01\SE:END\EI
ENHANCEMENT 0 ZAAP00F04.


  data : ls_qals like qals,
         lv_cname(30),
         lv_caddr(100),
         lv_phone(60),
         lv_line TYPE i,
         lv_sno TYPE i,
         LV_SNO1 TYPE CHAR10,
         lv_tp TYPE char30,
         lv_s TYPE char30,
         lv_r TYPE char30,
         LV_U TYPE CHAR20,
         lv_qty TYPE char20.

 TYPES: BEGIN OF TY_QAMV,
         PRUEFLOS   TYPE QAMV-PRUEFLOS,
         VORGLFNR   TYPE QAMV-VORGLFNR,
         MERKNR     TYPE QAMV-MERKNR,
         QPMK_WERKS TYPE QAMV-QPMK_WERKS,
         VERWMERKM  TYPE QAMV-VERWMERKM,
         MKVERSION  TYPE QAMV-MKVERSION,
         KATALGART1 TYPE QAMV-KATALGART1,
         AUSWMENGE1 TYPE QAMV-AUSWMENGE1,
         AUSWMGWRK1 TYPE QAMV-AUSWMGWRK1,
         MASSEINHSW TYPE QAMV-MASSEINHSW,
         TOLERANZOB TYPE QAMV-TOLERANZOB,
         TOLERANZUN TYPE QAMV-TOLERANZUN,
       END OF TY_QAMV.

DATA: GT_QAMV TYPE TABLE OF TY_QAMV,
      GS_QAMV TYPE TY_QAMV.

TYPES: BEGIN OF TY_QPAM,
        WERKS TYPE QPAM-WERKS,
        KATALOGART TYPE QPAM-KATALOGART,
        AUSWAHLMGE TYPE QPAM-AUSWAHLMGE,
        KTX01 TYPE QPAM-KTX01,
      END OF TY_QPAM.

 DATA: GT_QPAM TYPE TABLE OF TY_QPAM,
      GS_QPAM TYPE TY_QPAM.

  TYPES: BEGIN OF TY_QPMK,
          ZAEHLER TYPE QPMK-ZAEHLER,
          MKMNR TYPE QPMK-MKMNR,
          VERSION TYPE QPMK-VERSION,
          SORTFELD TYPE QPMK-SORTFELD,
        END OF TY_QPMK.

   DATA: GT_QPMK TYPE TABLE OF TY_QPMK,
      GS_QPMK TYPE TY_QPMK.



TYPES: BEGIN OF TY_QAMR,
        PRUEFLOS TYPE QAMR-PRUEFLOS,
        VORGLFNR TYPE QAMR-VORGLFNR,
        MERKNR TYPE QAMR-MERKNR,
        ORIGINAL_INPUT TYPE QAMR-ORIGINAL_INPUT,
        KATALGART1  type QAMR-KATALGART1,
        GRUPPE1     type QAMR-GRUPPE1,
        CODE1       type QAMR-CODE1,
        VERSION1    type QAMR-VERSION1,

      END OF TY_QAMR.

DATA: GT_QAMR TYPE TABLE OF TY_QAMR,
      GS_QAMR TYPE TY_QAMR.


TYPES: BEGIN OF TY_QPCT,
        KATALOGART TYPE QPCT-KATALOGART,
        CODEGRUPPE TYPE QPCT-CODEGRUPPE,
        CODE TYPE QPCT-CODE,
        VERSION TYPE QPCT-VERSION,
        KURZTEXT TYPE QPCT-KURZTEXT,
      END OF TY_QPCT.

DATA: GT_QPCT TYPE TABLE OF TY_QPCT,
      GS_QPCT TYPE TY_QPCT.

   TYPES: BEGIN OF TY_FINAL,
          PARA TYPE CHAR40,
          UNIT TYPE CHAR10,
          SPEC TYPE CHAR40,
          RESU TYPE CHAR40,

        END OF TY_FINAL.

 DATA: GT_FINAL TYPE TABLE OF TY_FINAL,
      GS_FINAL TYPE TY_FINAL.


TYPES: BEGIN OF TY_QASE,
        PRUEFLOS TYPE QASE-PRUEFLOS,
        VORGLFNR TYPE QASE-VORGLFNR,
        MERKNR     TYPE QASE-MERKNR,
        KATALGART1 TYPE QASE-KATALGART1,
        GRUPPE1    TYPE QASE-GRUPPE1,
        CODE1      TYPE QASE-CODE1,
        VERSION1   TYPE QASE-VERSION1,
        ORIGINAL_INPUT TYPE QASE-ORIGINAL_INPUT,
      END OF TY_QASE.

data: gs_qase TYPE ty_qase,
      gt_qase TYPE TABLE OF ty_qase.




ENDENHANCEMENT.
