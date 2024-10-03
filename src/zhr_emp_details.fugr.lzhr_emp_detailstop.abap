FUNCTION-POOL ZHR_EMP_DETAILS.              "MESSAGE-ID ..

* INCLUDE LZHR_EMP_DETAILSD...               " Local class definition


TYPES: BEGIN OF ty_hrp1001,
        OTYPE TYPE OTYPE,
        OBJID TYPE HROBJID,
        RSIGN TYPE RSIGN,
        RELAT TYPE RELAT,
        SCLAS TYPE SCLAS,
        SOBID TYPE SOBID,
       END OF ty_hrp1001.

TYPES: BEGIN OF ty_sobid,
        sobid TYPE sobid,
       END OF ty_sobid.

TYPES: BEGIN OF ty_hrp1000,
        OTYPE TYPE OTYPE,
        OBJID TYPE HROBJID,
        STEXT TYPE stext,
       END OF ty_hrp1000.

TYPES: BEGIN OF ty_empdet,
        pernr TYPE pernr_d,
        ename TYPE emnam,
       END OF ty_empdet.

DATA: GT_STOO  TYPE table of ty_hrp1001,
      GT_STOC  TYPE table of ty_hrp1001,
      GT_STOS  TYPE TABLE OF ty_hrp1001,
      GT_STOBP TYPE TABLE OF ty_hrp1001,
      GT_STOP  TYPE TABLE OF ty_hrp1001.

DATA: GT_JOBCODE TYPE TABLE OF zhr_jobcode.


DATA: GS_STOO  TYPE ty_hrp1001,
      GS_STOC  TYPE ty_hrp1001,
      GS_STOS  TYPE ty_hrp1001,
      GS_STOBP TYPE ty_hrp1001,
      GS_STOP  TYPE ty_hrp1001.

DATA: Gs_JOBCODE TYPE zhr_jobcode.

DATA: gt_sobid TYPE table of ty_sobid.

DATA: gt_plans TYPE HRPAD_T_PLANS.

DATA: gt_hrp1000 TYPE TABLE of ty_hrp1000.

DATA: gt_empdet TYPE table of ty_empdet.
