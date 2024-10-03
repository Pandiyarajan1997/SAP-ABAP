*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN2                                                   *
*----------------------------------------------------------------------*
*  Data declaration

DATA: BEGIN OF IT OCCURS 100.
        INCLUDE STRUCTURE PC207.
DATA: END OF IT.

DATA: BEGIN OF PRT OCCURS 10.
        INCLUDE STRUCTURE PC207.
DATA: END OF PRT.

DATA: BEGIN OF PEPF OCCURS 10.
        INCLUDE STRUCTURE PC2_IN07.
DATA: END OF PEPF.

DATA: BEGIN OF LEPF OCCURS 10.
        INCLUDE STRUCTURE PC2_IN07.
DATA: END OF LEPF.

DATA: BEGIN OF TEMP_RGDIR OCCURS 10.
        INCLUDE STRUCTURE PC261.
DATA: END OF TEMP_RGDIR.


DATA: LAST_MONTH_BEGDA LIKE PN-BEGDA,
      LAST_MONTH_ENDDA LIKE PN-ENDDA,
      LAST_MONTH(2),
      LAST_RESULT,
      NEXT_MONTH_BEGDA LIKE PN-BEGDA,
      TOGGLE_FLAG,
      NUM TYPE I,
      TITLE LIKE SY-TITLE,
      HEADLINE(20),
      RET_CD LIKE SY-SUBRC,
      EMP_NAME LIKE PERNR-ENAME,
      E_NAME LIKE PERNR-ENAME,
      FORMNAME(30),
      FORM5_TAB_ENTRY,
      FORM10_TAB_ENTRY,
      LANG LIKE SY-LANGU VALUE 'E',
      NBEGDA LIKE SY-DATUM,
      NENDDA LIKE SY-DATUM,
      EDLIPER(6) TYPE N,
      ACC_FACTOR LIKE T511K-KWERT.

DATA: PF_BASIS LIKE PC207-LGART VALUE '/3FB',
      EDLI_BASIS LIKE PC207-LGART VALUE '/3FL',
      PEN_BASIS LIKE PC207-LGART VALUE '/3FC'.

DATA: CHAR_DATE(9) TYPE C,
      PF_MONTH(3) TYPE C,
      PF_YEAR(4) TYPE C,
      PF_MON(2) TYPE C,
      PF_DAY(2) TYPE C,
      SEP(3) TYPE C VALUE ' - ',
      PF_MONTH_YEAR(10) TYPE C,
      FYBEGDA LIKE PN-BEGDA,
      FYENDDA LIKE PN-ENDDA,
      BEGDA1800 LIKE PN-BEGDA VALUE '18000101',
      ENDDA9999 LIKE PN-ENDDA VALUE '99991231'.

DATA: SNAME LIKE T596F-SNAME,
      ENDDATE LIKE SY-DATUM,
      SUBTY(4) TYPE C.

DATA: BEGIN OF MAIN_TAB OCCURS 10,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        PF_RATE LIKE PC2_IN07-EECTR,
        PF_EE_CONTR LIKE PC207-BETRG,
        PF_ER_CONTR LIKE PC207-BETRG,
        PF_ADMIN_CHGS LIKE PC207-BETRG,
        PEN_ER_CONTR LIKE PC207-BETRG,
        EDLI_ER_CONTR LIKE PC207-BETRG,
        EDLI_ADMIN_CHGS LIKE PC207-BETRG,
        EDLI_BASIS LIKE PC207-BETRG,
        PF_BASIS LIKE PC207-BETRG,
        PEN_BASIS LIKE PC207-BETRG,
        PFREF LIKE PC2_IN07-PFREF,
        PFRFN LIKE PC2_IN07-PFRFN,                          "PKT1152442
        PNFLG LIKE PC2_IN07-PNFLG,
        PAYED LIKE PC2_IN07-PAYED,
        LAST_MON_PNFLG LIKE PC2_IN07-PNFLG,
        LAST_MON_PAYED LIKE PC2_IN07-PAYED,
        MEMBER_LAST_MONTH TYPE I,
        NEW_THIS_MONTH TYPE I,
        LEFT_SERVICE TYPE I,
  gv_pens_contr TYPE pc207-betrg,
      END OF MAIN_TAB.

DATA: TOT_NO_EMP TYPE I.

DATA: BEGIN OF FINAL_TAB OCCURS 10,
        PF_RATE LIKE PC2_IN07-EECTR,

        TOT_PF_BASIS LIKE PC207-BETRG,
        TOT_PF_EE_CONTR LIKE PC207-BETRG,
        TOT_PF_ER_CONTR LIKE PC207-BETRG,
        TOT_PF_ADMIN_CHGS LIKE PC207-BETRG,

        TOT_PEN_BASIS LIKE PC207-BETRG,
        TOT_PEN_ER_CONTR LIKE PC207-BETRG,

        TOT_EDLI_BASIS LIKE PC207-BETRG,
        TOT_EDLI_ER_CONTR LIKE PC207-BETRG,
        TOT_EDLI_ADMIN_CHGS LIKE PC207-BETRG,
        PFREF LIKE PC2_IN07-PFREF,
        PFRFN LIKE PC2_IN07-PFRFN,                          "PKT1152442
        TOT_NO_EMP TYPE I,

        NO_LAST_MONTH TYPE I,
        NO_NEW_THIS_MONTH TYPE I,
        NO_LEFT_SERVICE TYPE I,
        TOTAL_MEMBERS TYPE I,

        NO_LAST_MONTH_PEN TYPE I,
        NO_NEW_THIS_MONTH_PEN TYPE I,
        NO_LEFT_SERVICE_PEN TYPE I,
        TOTAL_MEMBERS_PEN TYPE I,

        NO_LAST_MONTH_EDLI TYPE I,
        NO_NEW_THIS_MONTH_EDLI TYPE I,
        NO_LEFT_SERVICE_EDLI TYPE I,
        TOTAL_MEMBERS_EDLI TYPE I,

        TSTAD LIKE T7INF1-TSTAD,
    END OF FINAL_TAB.

DATA: BEGIN OF FORM5_TAB OCCURS 10,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        ENAME LIKE PERNR-ENAME,
        DOB LIKE P0002-GBDAT,
        DOJPF LIKE PC2_IN07-BEGDA,
        GENDER LIKE P0002-GESCH,
        TSTAD LIKE T7INF1-TSTAD,
        FATH_NAME LIKE PERNR-ENAME,
        PREV_SERVICE TYPE I,
        PFREF LIKE PC2_IN07-PFREF,
        PFRFN LIKE PC2_IN07-PFRFN,                          "PKT1152442
      END OF FORM5_TAB.

DATA: BEGIN OF FORM10_TAB OCCURS 10,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        ENAME LIKE PERNR-ENAME,
        DOL LIKE PC2_IN07-ENDDA,
        REASON_LEAVE(24) TYPE C,
        TSTAD LIKE T7INF1-TSTAD,
        FATH_NAME LIKE PERNR-ENAME,
        PFREF LIKE PC2_IN07-PFREF,
        PFRFN LIKE PC2_IN07-PFRFN,                          "PKT1152442
      END OF FORM10_TAB.

DATA: BEGIN OF HR_ERROR OCCURS 10.
        INCLUDE STRUCTURE HRERROR.
DATA: END OF HR_ERROR.

DATA: BEGIN OF FLD_NAM OCCURS 10,
            FIELD1(20),
            FIELD2(12),
            FIELD3(12),
      END OF FLD_NAM.

DATA : SERVICE_DAYS TYPE I,
       SERVICE_MONTHS TYPE P DECIMALS 2,
       REASON(24) TYPE C,
       DISP_FLG_LOT TYPE I VALUE 1,
       NEGTV TYPE I VALUE -1,
       FOUND.
DATA:  CONV_DATE(10).
DATA:  FORM_NAME(20) TYPE C.
TYPE-POOLS: SLIS.
DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      BEGIN OF G_ITAB_FCODE OCCURS 10,
       FCODE LIKE RSMPE-FUNC,
      END   OF G_ITAB_FCODE.
DATA: BEGIN OF MN_TAB OCCURS 0,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        PF_RATE LIKE PC2_IN07-EECTR,
        PF_EE_CONTR LIKE PC207-BETRG,
        PF_ER_CONTR LIKE PC207-BETRG,
        PF_ADMIN_CHGS LIKE PC207-BETRG,
        PEN_ER_CONTR LIKE PC207-BETRG,
        EDLI_ER_CONTR LIKE PC207-BETRG,
        EDLI_ADMIN_CHGS LIKE PC207-BETRG,
      END OF MN_TAB.
DATA: BEGIN OF FRM5_TAB OCCURS 10,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        ENAME LIKE PERNR-ENAME,
        DOB LIKE P0002-GBDAT,
        DOJPF LIKE PC2_IN07-BEGDA,
        GENDER LIKE P0002-GESCH,
      END OF FRM5_TAB.
DATA: BEGIN OF FRM10_TAB OCCURS 10,
        PERNR LIKE PERNR-PERNR,
        EEPFN LIKE PC2_IN07-EEPFM,
        ENAME LIKE PERNR-ENAME,
        DOL LIKE PC2_IN07-ENDDA,
        REASON_LEAVE(24) TYPE C,
      END OF FRM10_TAB.


***** START OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

***** DATA DECLARATION PART *****

DATA:   GV_FP_OUTPUTPARAMS    TYPE  SFPOUTPUTPARAMS,
        GV_FPNAME             TYPE  FPNAME,
        GV_FM_NAME            TYPE  FUNCNAME,
        GV_E_INTERFACE_TYPE   TYPE  FPINTERFACETYPE,
        GV_W_CX_ROOT TYPE REF TO CX_ROOT,
        GV_MESG TYPE STRING.


DATA:   GV_SLNO TYPE I,
        GV_PFMONTH TYPE PERNR-GEBER,
        GV_NAME1 TYPE SADR-NAME1,
        GV_STRAS TYPE SADR-STRAS,
        GV_ORT01 TYPE SADR-ORT01,
        GV_PSTLZ TYPE SADR-PSTLZ,
        GS_T500C  TYPE T500C,
*        gv_pfref type pc2_in07-pfref.
        GV_PFREF TYPE PC2_IN07-PFRFN.

DATA:   GT_OUTPUT_TAB TYPE STANDARD TABLE OF HCM_HINCEPF0_PDF."Internal
"table
DATA:   GS_OUTPUT_TAB TYPE HCM_HINCEPF0_PDF. " Structure

***** FOR PDF FORM 12A *****

DATA:   GT_OUTPUT12A_TAB TYPE STANDARD TABLE OF HCM_HINCEPF0_12A_PDF.

DATA:   CURR(2).

DATA:   GS_OUTPUT12A_TAB TYPE HCM_HINCEPF0_12A_PDF. "Structure

DATA:   GV_FYBEGDA TYPE PA0001-ENAME,
        GV_FYENDDA TYPE PA0001-ENAME,
        GV_BANKNAME TYPE RPCEPFIN-BKNAM,
        GV_BANKADDR TYPE RPCEPFIN-BKADD,
        GV_BANKCITY TYPE RPCEPFIN-BKCTY.
* Begin of AFY change - L4HK040454
DATA:
  AFY_SWITCH   TYPE PIN_REVAL,
  HTEXT_SWITCH TYPE C,
  PAYDATE      TYPE SY-DATUM,
  DATE         TYPE SY-DATUM.
* End of AFY change - L4HK040454

***** END OF CHANGES FOR PDF FORM BY C5061983 '30-12-2004' *****

* Start of changes for EPF efile
DATA : BEGIN OF FORM5_EFILE_TAB OCCURS 10,
        PERNR(7)        TYPE C,
        ENAME(85)       TYPE C,
        GENDER          LIKE P0002-GESCH,
        FATH_NAME(85)   TYPE C,
        DISABLED(1)     TYPE C,
        INT_WORK(1)     TYPE C,
        DOB(10)         TYPE C,
        DOJ(10)         TYPE C,
        MOB_NUM(15)     TYPE C,
        EMAIL(50)       TYPE C,
        PREV_SERVICE    TYPE C,
        HIGH_EPF(1)     TYPE C,
        HIGH_EPS(1)     TYPE C,
        BNK_ACCNM(30)   TYPE C,
        BNK_NAME(50)    TYPE C,
        BRANCH_NAME(50) TYPE C,
        ADD1(35)        TYPE C,
        ADD2(35)        TYPE C,
        CITY(35)        TYPE C,
        DIST(35)        TYPE C,
        STATE(33)       TYPE C,
        PIN_CODE(11)    TYPE N,
        MAR_STAT(1)     TYPE C,
        FATH_HUS(1)     TYPE C,
       END OF FORM5_EFILE_TAB,
       FORM5_HDR        LIKE FORM5_EFILE_TAB.

DATA:  BEGIN OF FORM10_EFILE_TAB OCCURS 10,
        EPERNR TYPE PERNR-PERNR,
        DOE_EPF(10)        TYPE    C,
        PERNR(7)        TYPE C,
        DOL(10)         TYPE C,
        REASON_LEAVE(1) TYPE C,
       END OF FORM10_EFILE_TAB,
       FORM10_HDR       LIKE FORM10_EFILE_TAB.

DATA:  BEGIN OF FORM12A_EFILE_TAB OCCURS 10,
        PERNR(7)        TYPE C,
        EPF_WGS(12)     TYPE C,
        EE_CONTR(12)    TYPE C,
        EE_REFUND(12)   TYPE C,
        NCP_DAYS(5)     TYPE C,
        DOL(10)         TYPE C,
        REASON_LEAVE(1) TYPE C,
        WAGE_ARR(12)    TYPE C,
        EE_EPFARR(12)   TYPE C,
        ER_EPFARR(12)   TYPE C,
        EPS_ARR(12)     TYPE C,
      END OF FORM12A_EFILE_TAB,
      FORM12A_HDR       LIKE FORM12A_EFILE_TAB.

DATA:  BEGIN OF ECR_EFILE_TAB OCCURS 10,
        EPERNR(10)            TYPE N,
        PERNR(7)             TYPE N,
        NAME(85)             TYPE C,
        EPF_WGS(10)          TYPE N,
        EPS_WGS(10)          TYPE N,
        EPF_CONT_DUE(10)     TYPE N,
        EPF_CONT_REM(10)     TYPE N,
        EPS_CONT_DUE(10)     TYPE N,
        EPS_CONT_REM(10)     TYPE N,
        DIFF_EPF_EPS_DUE(10) TYPE N,
        DIFF_EPF_EPS_REM(10) TYPE N,
        NCP_DAYS(2)          TYPE N,
        ADV_REF(10)          TYPE N,
        EPF_ARR_BASIS(10)    TYPE N,
        EPF_ARR_EE(10)       TYPE N,
        EPF_ARR_ER(10)       TYPE N,
        EPS_ARR_ER(10)       TYPE N,
        FATH_HUSB_NAME(85)   TYPE C,
        FATH_HUSB(1)         TYPE C,
        DOB(10)              TYPE C,
        GENDER(1)            TYPE C,
        DOJ_EPF(10)          TYPE C,
        DOJ_EPS(10)          TYPE C,
        DOL_EPF(10)          TYPE C,
        DOL_EPS(10)          TYPE C,
        DEX_EPF(10)          TYPE C,
        DEX_EPS(10)          TYPE C,
        REASON_LEAVE(1)      TYPE C,
       END OF ECR_EFILE_TAB,
       BEGIN OF ECR_HDR OCCURS 10,
        EPERNR            TYPE N,
        PERNR(7)             TYPE C,
        NAME(85)             TYPE C,
        EPF_WGS(10)          TYPE C,
        EPS_WGS(10)          TYPE C,
        EPF_CONT_DUE(10)     TYPE C,
        EPF_CONT_REM(10)     TYPE C,
        EPS_CONT_DUE(10)     TYPE C,
        EPS_CONT_REM(10)     TYPE C,
        DIFF_EPF_EPS_DUE(10) TYPE C,
        DIFF_EPF_EPS_REM(10) TYPE C,
        NCP_DAYS(2)          TYPE C,
        ADV_REF(10)          TYPE C,
        EPF_ARR_BASIS(10)    TYPE C,
        EPF_ARR_EE(10)       TYPE C,
        EPF_ARR_ER(10)       TYPE C,
        EPS_ARR_ER(10)       TYPE C,
        FATH_HUSB_NAME(85)   TYPE C,
        FATH_HUSB(1)         TYPE C,
        DOB(10)              TYPE C,
        GENDER(1)            TYPE C,
        DOJ_EPF(10)          TYPE C,
        DOJ_EPS(10)          TYPE C,
        DOL_EPF(10)          TYPE C,
        DOL_EPS(10)          TYPE C,
        REASON_LEAVE(1)      TYPE C,
       END OF ECR_HDR.

DATA: ARR_PF_BASIS   LIKE PC207-LGART VALUE '/ZFA',
      ARR_EE_PF_CONT LIKE PC207-LGART VALUE '/ZF5',
      ARR_ER_PF_CONT LIKE PC207-LGART VALUE '/ZF3',
      ARR_ER_PS_CONT LIKE PC207-LGART VALUE '/ZF4'.

DATA: BEGIN OF MODF OCCURS 0.                               "TKM1254275
        INCLUDE STRUCTURE PC214.
DATA: END OF MODF.

DATA: A TYPE I VALUE 1.

DATA : BEGIN OF ABSITAB OCCURS 0.
        INCLUDE STRUCTURE PC20I.
DATA : END OF ABSITAB.

DATA : BEGIN OF ITAB OCCURS 0,
          BEGDATE LIKE PC205-BEGDA,
          ENDDATE LIKE PC205-ENDDA,
          ACTV LIKE PC205-APZNR,
          VALRUL LIKE PC20I-KLBEW,
          PERIOD LIKE PC202-FPPER,
          MOD LIKE PC214-MOD0A,
       END OF ITAB.

DATA : BEGIN OF NEWITAB OCCURS 0.
        INCLUDE STRUCTURE PIN_PFNCD.
DATA : END OF NEWITAB.

DATA: BEGIN OF INDITAB OCCURS 0,
        INDI LIKE T554C-REF01,
        ABSDAYS LIKE PC20I-ABWTG,
        PERIOD LIKE PC202-FPPER,
      END OF INDITAB.
DATA : CURRPD LIKE PC202-FPPER.                             "TKM1254275

DATA: ABS01 TYPE PC20I-ABWTG,
      ABS02 TYPE PC20I-ABWTG,
      ABS03 TYPE PC20I-ABWTG,
      ABS04 TYPE PC20I-ABWTG,
      ABS05 TYPE PC20I-ABWTG,
      ABS06 TYPE PC20I-ABWTG,
      ABS07 TYPE PC20I-ABWTG,
      ABS08 TYPE PC20I-ABWTG,
      ABS09 TYPE PC20I-ABWTG,
      ABS10 TYPE PC20I-ABWTG,
      ABS11 TYPE PC20I-ABWTG,
      ABS12 TYPE PC20I-ABWTG.
DATA : PD01 LIKE PC202-FPPER,
       PD02 LIKE PC202-FPPER,
       PD03 LIKE PC202-FPPER,
       PD04 LIKE PC202-FPPER,
       PD05 LIKE PC202-FPPER,
       PD06 LIKE PC202-FPPER,
       PD07 LIKE PC202-FPPER,
       PD08 LIKE PC202-FPPER,
       PD09 LIKE PC202-FPPER,
       PD10 LIKE PC202-FPPER,
       PD11 LIKE PC202-FPPER,
       PD12 LIKE PC202-FPPER.

DATA : CRUN_SWITCH   TYPE PIN_REVAL.
DATA: BEGIN OF LRT OCCURS 10.
        INCLUDE STRUCTURE PC207.
DATA: END OF LRT.

*** Negative Arrears
DATA:  BEGIN OF NEG_TAB OCCURS 10,
        PERNR(8)             TYPE N,
        MEMID(7)             TYPE N,
        NAME(85)             TYPE C,
        EPF_ARR_BASIS        LIKE PC207-BETRG,
        EPF_ARR_EE           LIKE PC207-BETRG,
        EPF_ARR_ER           LIKE PC207-BETRG,
        EPS_ARR_ER           LIKE PC207-BETRG,
       END OF NEG_TAB.
 DATA: NEG_FLAG TYPE C.
*** Negative Arrears

* End of changes for EPF efile


"structure for KYC
 TYPES: BEGIN OF GST_KYC,
   UAN TYPE PSG_IDNUM,     "from infotype 0185. subty- 08
   DOC_TYPE(20) TYPE C,
   DOC_NO TYPE P25_ISSPL,
   IFSC_CODE TYPE  ISNUM,
   EXPIRY(15) TYPE C,
   EDU_QUAL(25) TYPE C,
   PH_HAND(20) TYPE C,
   PH_CAT TYPE P25_AUTH1,
   GENDER(6) TYPE C,   " from infotype 0002
   NAME(85) TYPE C,
   INT_WORK(20) TYPE C,
   MARITAL(20) TYPE C,
   EST_ID(15) TYPE C,
 END OF GST_KYC.
 DATA: IT_KYC_UAN TYPE TABLE OF GST_KYC,
       WA_KYC_UAN TYPE GST_KYC.

* structure to kyc error cases
 TYPES: BEGIN OF GST_KYC_ERROR,
   PER(30) TYPE C,
   NAME(85) TYPE C,
   DESC(35) TYPE C,
 END OF GST_KYC_ERROR.
 DATA: IT_KYC_UAN_ERROR TYPE TABLE OF GST_KYC_ERROR,
       WA_KYC_UAN_ERROR TYPE GST_KYC_ERROR.

 DATA: BEGIN OF GST_KYC_UAN,  "structure for changing parameter in BAdI
        DOC_TYPE(20) TYPE C,
        DOC_NO(20) TYPE C,
        IFSC_CODE(10) TYPE  C,
        EXPIRY(15) TYPE C,
        EDU_QUAL(25) TYPE C,
        PH_HAND(20) TYPE C,
        PH_CAT(30) TYPE C,
        GENDER(7) TYPE C,
        INT_WORK(20) TYPE C,
        MARITAL(20) TYPE C,
        EST_ID(16) TYPE C,
        END OF GST_KYC_UAN.

*DATA: gst_kyc_uan TYPE gst_kyc_uan.

**-Declaration for ECR 2.0
 DATA:  nom_basis type pc207-betrg,
        ptx_basis type pc207-betrg,
        diff_basis type p DECIMALS 5,
        days_in_month(2) type c,
        new_ncp_days type p.

**-type declaration for ECR 2.0
 DATA:  BEGIN OF ecr2_efile_tab OCCURS 10,
        pernr(8)             TYPE n,
        uan(30)              TYPE c,
        name(85)             TYPE c,
        grs_wgs(10)          TYPE n,
        epf_wgs(10)          TYPE n,
        eps_wgs(10)          TYPE n,
        edli_wgs(10)         TYPE n,
        epf_cont_rem(10)     TYPE n,
        eps_cont_rem(10)     TYPE n,
        diff_epf_eps_rem(10) TYPE n,
        ncp_days(2)          TYPE n,
        adv_ref(10)          TYPE n,
       END OF ecr2_efile_tab.

 DATA:  BEGIN OF ecr2_hdr OCCURS 10,
        pernr(8)             TYPE c,
        uan(30)              TYPE c,
        name(85)             TYPE c,
        grs_wgs(10)          TYPE c,
        epf_wgs(10)          TYPE c,
        eps_wgs(10)          TYPE c,
        edli_wgs(10)         TYPE c,
        epf_cont_rem(10)     TYPE c,
        eps_cont_rem(10)     TYPE c,
        diff_epf_eps_rem(10) TYPE c,
        ncp_days(2)          TYPE c,
        adv_ref(10)          TYPE c,
       END OF ecr2_hdr.

TYPES: BEGIN OF g_ecr_uan_error,
  seqno(10),
  pernr TYPE pernr,
  desc(40),
END OF g_ecr_uan_error.

DATA: it_ecr_uan_error TYPE TABLE OF g_ecr_uan_error,
      wa_ecr_uan_error TYPE g_ecr_uan_error.

**-type declaration ecr 2.0 arrear file
TYPES: BEGIN OF ty_arrear,
  pernr(8) TYPE n,
  uan(30) TYPE c,
  name(85) TYPE c,
  epf_arr_basis(11) TYPE n,
  arr_pen_wages(11) TYPE n,
  arr_edli_wages(11) TYPE n,
  arr_ee_pf_contr(11) TYPE n,
  arr_er_pf_contr(11) TYPE n,
  arr_er_pens_contr(11) TYPE n,
END OF ty_arrear.

TYPES: BEGIN OF ty_arr_hdr,
  pernr(8) TYPE c,
  uan(30) TYPE c,
  name(85) TYPE c,
  epf_arr_basis(11) TYPE c,
  arr_pen_wages(11) TYPE c,
  arr_edli_wages(11) TYPE c,
  arr_ee_pf_contr(11) TYPE c,
  arr_er_pf_contr(11) TYPE c,
  arr_er_pens_contr(11) TYPE c,
END OF ty_arr_hdr.

DATA : it_arrear TYPE TABLE OF ty_arrear,
       wa_arrear TYPE ty_arrear,
       arr_hdr TYPE ty_arr_hdr..

DATA : eps_arr_wgs TYPE pc207-lgart VALUE '/ZFC',
       edli_arr_wgs TYPE pc207-lgart VALUE '/ZFL',
       gov_pen_cont TYPE pc207-lgart VALUE '/3FN',
       arr_gov_pens_contr TYPE pc207-lgart VALUE '/ZFN'.

DATA: ncpexit_impl TYPE c.
