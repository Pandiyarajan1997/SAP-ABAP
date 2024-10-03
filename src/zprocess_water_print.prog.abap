*&---------------------------------------------------------------------*
*& Report  ZPROCESS_ORDER_DETAILS
*&
*&---------------------------------------------------------------------*
*&added by ram
*&
*&---------------------------------------------------------------------*

REPORT ZPROCESS_WATER_PRINT.

*INCLUDE ZPROCESS_ORDER_DETAILS_TOP.

*INCLUDE ZPROCESS_ORDER_DETAILS_SUB.


TABLES : AUFK.
TYPE-POOLS VRM.
*
TYPES : BEGIN OF TY_AUFK,
        AUFNR TYPE AUFK-AUFNR,
        WERKS TYPE AUFK-WERKS,
        END OF   TY_AUFK.


DATA : HEADER        TYPE TABLE OF BAPI_ORDER_HEADER1 ,
       WA_HEADER     TYPE          BAPI_ORDER_HEADER1,
       POSITION      TYPE TABLE OF BAPI_ORDER_ITEM,"  OCCURS 1,
       SEQUENCE      TYPE TABLE OF BAPI_ORDER_SEQUENCE,"   OCCURS 1,
       PHASE_BAPI         TYPE TABLE OF BAPI_ORDER_PHASE,"   OCCURS 1,
       PHASE         TYPE TABLE OF zphase,
       PROD_REL_TOOL TYPE TABLE OF BAPI_ORDER_PROD_REL_TOOLS."  OCCURS 1.

DATA : TRIGGER_POINT TYPE TABLE OF  BAPI_ORDER_TRIGGER_POINT,
       COMPONENT     TYPE TABLE OF  BAPI_ORDER_COMPONENT,
       WA_COMPONENT  TYPE BAPI_ORDER_COMPONENT,
       IT_COMPONENT  TYPE TABLE OF BAPI_ORDER_COMPONENT,
       WA_COMPONENT_1401  TYPE BAPI_ORDER_COMPONENT,
       IT_COMPONENT_1401  TYPE TABLE OF BAPI_ORDER_COMPONENT,
       IT1_COM_1401 TYPE TABLE OF BAPI_ORDER_COMPONENT.

DATA: QUN_1401 TYPE AFPO-PSMNG,
      BATCH_NO TYPE AFPO-CHARG.


DATA : WA_PHASE_BAPI      TYPE BAPI_ORDER_PHASE.  ""Commented NTT
DATA : WA_PHASE      TYPE ZPHASE.  ""ADDED NTT

DATA : ORDER_OBJECTS TYPE TABLE OF BAPI_PI_ORDER_OBJECTS WITH HEADER LINE.

DATA : IT_AUFK TYPE TABLE OF TY_AUFK.

DATA : IT_AFFV TYPE TABLE OF ZAFFV,
       WA_AFFV TYPE ZAFFV.

DATA : IT_AFFV1 TYPE TABLE OF ZAFFV,
       IT_AFFV2 TYPE TABLE OF ZAFFV,
       WA_AFFV1 TYPE ZAFFV.

DATA : IT_AFFT TYPE TABLE OF ZAFFT,
       WA_AFFT TYPE ZAFFT.

DATA : IT_AFFT1 TYPE TABLE OF ZAFFT,
       WA_AFFT1 TYPE ZAFFT.

DATA : FM_NAME TYPE RS38L_FNAM.

DATA : INSP_LOT     TYPE TABLE OF BAPI2045L1 ,
       WA_INSP_LOT  TYPE BAPI2045L1 .
DATA : INSP_LOT1  TYPE BAPI2045L1.
DATA : INSP_CHAR TYPE TABLE OF BAPI2045L2,
       WA_INSP_CHAR1 TYPE BAPI2045L2.
DATA : INSP_CHARLIST TYPE TABLE OF BAPI2045L3 WITH HEADER LINE.
DATA : INSP_LIST_TEMP TYPE TABLE OF BAPI2045L3,
       WA_INSP_CHAR2 TYPE BAPI2045L3.

DATA : IT_FINAL TYPE TABLE OF BAPI2045D1.

DATA : AFFV TYPE  AFFV.

DATA : REQUIREMENTS TYPE BAPI2045D1.
DATA : BAPIRETURN_MSG TYPE BAPIRETURN1.

DATA : WA1 TYPE BAPI2045L3.

DATA : WA_JOB_OUTPUT_INFO TYPE SSFCRESCL.

DATA : MI_BYTECOUNT TYPE I.

DATA : LINES LIKE TLINE OCCURS 100 WITH HEADER LINE.

DATA : PLANT TYPE AUFK-WERKS.

*-------------------------------------------------Added PR@$@@TH On 27/06/2019 Confurmation Quantity And Item Quantity Checking
TYPES: BEGIN OF TY_RESB,
   RSNUM TYPE RESB-RSNUM,
   RSPOS TYPE RESB-RSPOS,
   RSART TYPE RESB-RSART,
   XFEHL TYPE RESB-XFEHL,
   BDMNG TYPE RESB-BDMNG,
   AUFNR TYPE RESB-AUFNR,
   VMENG TYPE RESB-VMENG,
   END OF TY_RESB.

DATA: IT_RESB TYPE TABLE OF TY_RESB,
      WA_RESB TYPE TY_RESB.

*DATA: MP TYPE I VALUE 0 . " Missing Part

DATA: QC(1) TYPE C VALUE 'X'. "Quality Checking
DATA: MP(1) TYPE C VALUE 'X'. "Missing Part

*-------------------------------------------------Ended PR@$@TH

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS     : AUFNR TYPE AUFK-AUFNR OBLIGATORY.
*  PARAMETERS     : WERKS TYPE AUFK-WERKS.

SELECTION-SCREEN : END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-004.
" DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) TEXT-002 .
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS : P_PDF TYPE  C AS CHECKBOX USER-COMMAND FLAG.
PARAMETERS: P_FILE TYPE IBIPPARMS-PATH.
SELECTION-SCREEN END OF LINE.
DATA : FILE TYPE STRING.
SELECTION-SCREEN END OF BLOCK B2.

DATA : ET_CHA_DATA TYPE TABLE OF CHA_CLASS_DATA,
       ET_HDR_DATA TYPE TABLE OF TSK_CLASS_DATA,
       ET_PAC_DATA TYPE TABLE OF PAC_CLASS_DATA,
       I_PLNTY_SOURCE TYPE TSK_CLASS_DATA-PLNTY,
       I_PLNNR_SOURCE TYPE TSK_CLASS_DATA-PLNNR,
       I_PLNAL_SOURCE TYPE TSK_CLASS_DATA-PLNAL.

TYPES : BEGIN OF TY_MAPL,
        MATNR TYPE MATNR,
        WERKS TYPE WERKS_D,
        PLNTY TYPE PLNTY,
        PLNNR TYPE PLNNR,
        PLNAL TYPE PLNAL,
        LOEKZ TYPE LOEKZ,
        END OF TY_MAPL.

DATA : IT_MAPL TYPE TABLE OF TY_MAPL,
       WA_MAPL TYPE TY_MAPL.



DATA : FLAG TYPE C LENGTH 1.

*BREAK hari.



AT SELECTION-SCREEN OUTPUT.


  IF FLAG = 1.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'AUFNR'.
        SCREEN-INPUT = 0.
        SCREEN-OUTPUT = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

*          ELSEIF aufnr IS NOT INITIAL." AND werks IS NOT INITIAL.
**    AND p_qplos IS NOT INITIAL.
*    .
*    LOOP AT SCREEN.
*
*      IF screen-name = 'P_FILE'.
*        screen-active = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.


  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      FILE_NAME = P_FILE.



AT SELECTION-SCREEN ON P_PDF.
*
  IF P_PDF = 'X' AND P_FILE IS INITIAL.

    MESSAGE 'Give Valid Path' TYPE 'S' DISPLAY LIKE 'I'.

  ENDIF.

  IF SY-UCOMM = 'ONLI'.


    SELECT AUFNR
*          WERKS
           FROM AUFK
           INTO CORRESPONDING FIELDS OF TABLE IT_AUFK
           WHERE AUFNR = AUFNR.
*          AND   WERKS = WERKS.

    IF SY-SUBRC <> 0.
      MESSAGE : 'No values found' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.



    CALL FUNCTION 'CO_SF_AFFV_READ'
      EXPORTING
        AUFNR_IMP = AUFNR
        AFVZL_IMP = 00000001
*       VSNMR_IMP =
      IMPORTING
        AFFV_EXP  = AFFV
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

*if affv is  INITIAL .
*
*

*  call TRANSACTION 'ZPO_DETVK'.
*

*
*  else.




    IF AUFNR IS NOT INITIAL OR P_FILE IS NOT INITIAL.
      CLEAR : ORDER_OBJECTS.
      REFRESH : ORDER_OBJECTS , HEADER , POSITION , SEQUENCE , PHASE , TRIGGER_POINT , COMPONENT , PROD_REL_TOOL.
*&---------------------------------------------------------------------*
*&  Include           ZPROCESS_ORDER_DETAILS_SUB
*&---------------------------------------------------------------------*
      ORDER_OBJECTS-HEADER              = 'X'.
      ORDER_OBJECTS-POSITIONS           = 'X'.
      ORDER_OBJECTS-SEQUENCES           = 'X'.
      ORDER_OBJECTS-PHASES              = 'X'.
      ORDER_OBJECTS-COMPONENTS          = 'X'.
      ORDER_OBJECTS-PROD_REL_TOOLS      = 'X'.
      ORDER_OBJECTS-TRIGGER_POINTS      = 'X'.
      ORDER_OBJECTS-SECONDARY_RESOURCES = 'X'.
      APPEND ORDER_OBJECTS.




      CALL FUNCTION 'BAPI_PROCORD_GET_DETAIL' "#EC CI_USAGE_OK[2438131] added by SPLABAP during code remedation
        EXPORTING
          NUMBER                 = AUFNR
*         COLLECTIVE_ORDER       =
          ORDER_OBJECTS          = ORDER_OBJECTS
*      IMPORTING
*         RETURN                 =
       TABLES
         HEADER                 = HEADER
         POSITION               = POSITION
         SEQUENCE               = SEQUENCE
         PHASE                  = PHASE_BAPI
         TRIGGER_POINT          = TRIGGER_POINT
         COMPONENT              = COMPONENT
         PROD_REL_TOOL          = PROD_REL_TOOL  .

      MOVE-CORRESPONDING PHASE_BAPI TO PHASE.
      REFRESH : INSP_LOT, INSP_CHAR , INSP_CHARLIST , INSP_LIST_TEMP , IT_FINAL.
      CLEAR : WA_INSP_LOT , INSP_LOT1 , WA_INSP_CHAR1 , INSP_CHARLIST , WA1 , WA_INSP_CHAR2 , REQUIREMENTS , BAPIRETURN_MSG.


      CLEAR : IT_MAPL.



      READ TABLE HEADER INTO WA_HEADER INDEX 1.

      PLANT = WA_HEADER-PRODUCTION_PLANT.

      IF SY-SUBRC = 0.

        SELECT MATNR
               WERKS
               PLNTY
               PLNNR
               PLNAL
               LOEKZ
               FROM MAPL
               INTO TABLE IT_MAPL
               WHERE MATNR = WA_HEADER-MATERIAL_LONG AND
                     WERKS = WA_HEADER-PRODUCTION_PLANT    AND
                     PLNTY = 'Q' AND LOEKZ <> 'X' .

      ENDIF.

      LOOP AT IT_MAPL INTO WA_MAPL.

        I_PLNTY_SOURCE = WA_MAPL-PLNTY.
        I_PLNNR_SOURCE = WA_MAPL-PLNNR.
        I_PLNAL_SOURCE = WA_MAPL-PLNAL.

      ENDLOOP.

      CALL FUNCTION 'RMXT412_GET_INSPECTION_PLANS'
        EXPORTING
         I_KEY_DATE_SOURCE              = SY-DATUM
          I_PLNTY_SOURCE                = I_PLNTY_SOURCE
          I_PLNNR_SOURCE                = I_PLNNR_SOURCE
          I_PLNAL_SOURCE                = I_PLNAL_SOURCE
        TABLES
          ET_PAC_DATA                    = ET_PAC_DATA
          ET_CHA_DATA                    = ET_CHA_DATA
          ET_HDR_DATA                    = ET_HDR_DATA
*      EXCEPTIONS
*        PATH_SOURCE_INCOMPLETE         = 1
*        NO_VALID_SOURCE_TASK           = 2
*        ECM_EVALUATION_NECESSARY       = 3
*        OTHERS                         = 4
                .
      IF SY-SUBRC <> 0.
*       Implement suitable error handling here
      ENDIF.

      IF IT_MAPL[] IS NOT INITIAL.

      ENDIF.

      SELECT AUFPL
             PLNNR
             FVLNR
             ATINN
             ATWRT
*            AFVZL
             AFTZL
             FROM AFFV
             INTO TABLE IT_AFFV
             WHERE AUFPL = AFFV-AUFPL
             AND   PLNNR = AFFV-PLNNR.


      SELECT AUFPL
             PLNNR
             FTRNR
             CSKTX
             AFTZL
             FROM AFFT
             INTO TABLE IT_AFFT
             WHERE AUFPL = AFFV-AUFPL AND
                   PLNNR = AFFV-PLNNR.



*
      TYPES : BEGIN OF TY_PHASE,
              OPERATION_NUMBER TYPE C LENGTH 4,"BAPI_ORDER_PHASE-OPERATION_NUMBER,
              DESCRIPTION      TYPE MAKT-MAKTX,"BAPI_ORDER_PHASE-description,
              FTRNR            TYPE AFFT-FTRNR,
              CSKTX            TYPE AFFT-CSKTX,
              FVLNR            TYPE AFFV-FVLNR,
              ATWRT            TYPE AFFV-ATWRT,
***              PHASE_NUM        TYPE CO_AUFPL,  Commented by NTT CF Team
              RESOURCE         TYPE ARBPL,
              RESOURCE_TEXT TYPE KTXT40,
*             charact          TYPE
*             opnum            TYPE BAPI_ORDER_PHASE-OPERATION_NUMBER,

              END OF TY_PHASE.


      DATA : ITF TYPE TABLE OF TY_PHASE,
             WAF TYPE TY_PHASE.

      DATA : COUNTER TYPE I VALUE 0011.

*BREAK-POINT.

      LOOP AT PHASE INTO WA_PHASE.

        WAF-OPERATION_NUMBER    = WA_PHASE-OPERATION_NUMBER.
        WAF-DESCRIPTION         = WA_PHASE-DESCRIPTION.
        WAF-RESOURCE            = WA_PHASE-RESOURCE.
        WAF-RESOURCE_TEXT           = WA_PHASE-RESOURCE_TEXT.
        IF COUNTER = WA_PHASE-OPERATION_NUMBER.

          IT_AFFT1 = IT_AFFT.

          SORT IT_AFFT1 BY FTRNR."AFTZL.
          DELETE ADJACENT DUPLICATES FROM IT_AFFT1 COMPARING FTRNR.

          LOOP AT IT_AFFT1 INTO WA_AFFT1.

            WAF-FTRNR = WA_AFFT1-FTRNR.
            WAF-CSKTX = WA_AFFT1-CSKTX.
*           waf-opnum = wa_phase-operation_number.
            APPEND WAF TO ITF.
            CLEAR  WAF.
            DELETE IT_AFFT WHERE FTRNR = WA_AFFT1-FTRNR AND
                                 CSKTX = WA_AFFT1-CSKTX AND
                                 AFTZL = WA_AFFT1-AFTZL.
*                                opnum = wa_afft1-opnum.
**
*loop at it_affv into wa_affv.
*
*  wa_affv-afvzl     = wa_affv-afvzl.
*  wa_affv-PHASE_NUM = wa_afft1-FTRNR.
*
*  modify it_affv FROM wa_affv.
*
*  endloop.

            IT_AFFV1 = IT_AFFV.


            IT_AFFV2 = IT_AFFV.
            SORT IT_AFFV1 BY FVLNR AFTZL.
*delete ADJACENT DUPLICATES FROM it_affv1 COMPARING FVLNR ATINN.

*delete ADJACENT DUPLICATES FROM it_affv1 COMPARING FVLNR afvzl." atinn.

            LOOP AT IT_AFFV INTO WA_AFFV.

              DELETE IT_AFFV1 WHERE AFTZL NE WA_AFFV-AFTZL.

*READ TABLE it_affv1 into wa_affv1 with key atinn = wa_affv-atinn.
*if sy-subrc = 0.

              LOOP AT IT_AFFV1 INTO WA_AFFV1.



                WAF-FVLNR     = WA_AFFV1-FVLNR.
                WAF-ATWRT     = WA_AFFV1-ATWRT.
*    waf-PHASE_NUM = wa_afft1-FTRNR.

*    waf-opnum    = wa_phase-operation_number.
*
                APPEND WAF TO ITF.
                DELETE IT_AFFV WHERE FVLNR = WA_AFFV1-FVLNR AND
                                     ATWRT = WA_AFFV1-ATWRT AND
                                     AFTZL = WA_AFFV1-AFTZL.
                CLEAR WAF.
              ENDLOOP.
*endif.



*      it_affv1 = it_affv2.
            ENDLOOP.


          ENDLOOP.



          CLEAR WAF.




          COUNTER = COUNTER + 0010.

        ENDIF.

        APPEND WAF TO ITF.
        CLEAR WAF.

      ENDLOOP.


*BREAK-POINT.


      CALL FUNCTION 'BAPI_INSPLOT_GETLIST' "#EC CI_USAGE_OK[2438131] added by SPLABAP during code remedation
       EXPORTING
*        MATERIAL              =
*        BATCH                 =
         ORDER                 = AUFNR
*        VERSION               =
*        RS_ORDER              =
*        VENDOR                =
*        CUSTOMER              =
*        MAX_ROWS              = 100
*        CREAT_DAT             =
*        PLANT                 =
*        PO_NUMBER             =
*        PO_ITEM               = 0000
*        MAT_DOC               =
*        MATDOC_ITM            = 0000
*        DELIV_NUMB            =
*        DELIV_ITEM            = 00000
*        MANUFACTURER          =
         STATUS_CREATED        = 'X'
         STATUS_RELEASED       = 'X'
         STATUS_UD             = 'X'
*        SELECTION_ID          = ' '
*        MATERIAL_EVG          =
*        IMPORTING
*        RETURN                =
        TABLES
          INSPLOT_LIST          = INSP_LOT.

      LOOP AT  INSP_LOT INTO WA_INSP_LOT.

        MOVE-CORRESPONDING WA_INSP_LOT TO INSP_LOT1.

      ENDLOOP.

      CALL FUNCTION 'BAPI_INSPLOT_GETOPERATIONS'
        EXPORTING
          NUMBER              = INSP_LOT1-INSPLOT
*       IMPORTING
*         RETURN              =
        TABLES
          INSPOPER_LIST       = INSP_CHAR.

*BREAK hari.

      REFRESH : INSP_CHARLIST , INSP_LIST_TEMP , IT_FINAL.
      CLEAR : WA_INSP_CHAR1 , INSP_CHARLIST , WA1 , WA_INSP_CHAR2 , REQUIREMENTS , BAPIRETURN_MSG.
      LOOP AT  INSP_CHAR INTO WA_INSP_CHAR1.

        CALL FUNCTION 'BAPI_INSPOPER_GETCHAR'
          EXPORTING
            INSPLOT                            = WA_INSP_CHAR1-INSPLOT
            INSPOPER                           = WA_INSP_CHAR1-INSPOPER
            CHAR_FILTER_NO                     = '1'
            CHAR_FILTER_TCODE                  = 'QE11'
*           RES_ORG                            = ' '
*           READ_CHARS_WITH_CLASSES            = ' '
*           READ_CHARS_WITHOUT_RECORDING       = ' '
*         IMPORTING
*           RETURN                             =
*           RETURN2                            =
          TABLES
            INSPCHAR_LIST                      = INSP_CHARLIST.
        LOOP AT  INSP_CHARLIST.
          MOVE-CORRESPONDING INSP_CHARLIST TO WA1.
        ENDLOOP.

        APPEND WA1 TO INSP_LIST_TEMP.
        CLEAR : WA_INSP_CHAR1 , WA1.
      ENDLOOP.

      DELETE INSP_LIST_TEMP WHERE INSPOPER = ' '.




      LOOP AT INSP_LIST_TEMP INTO WA_INSP_CHAR2.

        CALL FUNCTION 'BAPI_INSPCHAR_GETREQUIREMENTS'
          EXPORTING
            INSPLOT      = WA_INSP_CHAR2-INSPLOT
            INSPOPER     = WA_INSP_CHAR2-INSPOPER
            INSPCHAR     = WA_INSP_CHAR2-INSPCHAR
          IMPORTING
            REQUIREMENTS = REQUIREMENTS
            RETURN       = BAPIRETURN_MSG.

        APPEND REQUIREMENTS TO IT_FINAL.
        CLEAR : REQUIREMENTS.

      ENDLOOP.

      DATA : IT_SSFCTRLOP TYPE TABLE OF SSFCTRLOP.
      DATA : WA_SSFCTRLOP TYPE SSFCTRLOP.


      DATA : IT_OUTPUT_OPTIONS TYPE TABLE OF SSFCOMPOP.
      DATA : WA_OUTPUT_OPTIONS TYPE SSFCOMPOP.

      IF P_PDF = 'X' .
        WA_SSFCTRLOP-NO_DIALOG = 'X'.
        WA_SSFCTRLOP-GETOTF = 'X'.
        WA_SSFCTRLOP-PREVIEW = 'X'.

*        append wa_ssfctrlop to it_ssfctrlop.

        WA_OUTPUT_OPTIONS-TDNOPREV = 'X'.
        WA_OUTPUT_OPTIONS-TDTITLE = SY-TITLE.
        WA_OUTPUT_OPTIONS-TDNEWID = 'X'.
*        append wa_output_options to it_output_options.
      ENDIF.

*  DATA: gv_formname  TYPE  tdsfname,
*        gv_fm_name  TYPE  rs38l_fnam.

*  gv_formname = 'ZPROCESS_ORDER_DETAILS'.

*BREAK-POINT.
*
*data : T_STPO TYPE TABLE OF STPO_API02,
*       WA_TSTPO TYPE STPO_API02.
*
*CALL FUNCTION 'CSAP_MAT_BOM_READ'
*  EXPORTING
*    MATERIAL             = WA_COMPONENT-material
*    PLANT                = wa_header-PRODUCTION_PLANT
*    BOM_USAGE            = 1
**   ALTERNATIVE          =
**   VALID_FROM           =
**   VALID_TO             =
**   CHANGE_NO            =
**   REVISION_LEVEL       =
**   FL_DOC_LINKS         =
**   FL_DMU_TMX           =
** IMPORTING
**   FL_WARNING           =
* TABLES
*   T_STPO               = t_stpo
**   T_STKO               =
**   T_DEP_DATA           =
**   T_DEP_DESCR          =
**   T_DEP_ORDER          =
**   T_DEP_SOURCE         =
**   T_DEP_DOC            =
**   T_DOC_LINK           =
**   T_DMU_TMX            =
**   T_LTX_LINE           =
**   T_STPU               =
* EXCEPTIONS
*   ERROR                = 1
*   OTHERS               = 2
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*if affv is NOT INITIAL.

      DATA: TXT(1) TYPE C,
            TXT1(1) TYPE C.


      SELECT RSNUM RSPOS RSART XFEHL BDMNG AUFNR VMENG FROM RESB INTO TABLE IT_RESB WHERE AUFNR = AUFNR ."AND XFEHL = 'X' .

      IF AUFNR IS NOT INITIAL AND AFFV IS INITIAL.
        TXT =  'X' .
*        MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.

      IF WA_HEADER-PRODUCTION_PLANT <> 1401 AND WA_HEADER-PRODUCTION_PLANT <> 1003 .
        IF TXT = 'X'.
          MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            FORMNAME           = 'ZPROCESS_WATER_PRINT'
*           VARIANT            = ' '
*           DIRECT_CALL        = ' '
          IMPORTING
            FM_NAME            = FM_NAME
          EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.
        IF SY-SUBRC <> 0.
*         Implement suitable error handling here
        ENDIF.

        CALL FUNCTION FM_NAME"'/1BCDWB/SF00000125'
         EXPORTING
*          ARCHIVE_INDEX              =
*          ARCHIVE_INDEX_TAB          =
*          ARCHIVE_PARAMETERS         =
           CONTROL_PARAMETERS          = WA_SSFCTRLOP
*          MAIL_APPL_OBJ              =
*          MAIL_RECIPIENT             =
*          MAIL_SENDER                =
           OUTPUT_OPTIONS             = WA_OUTPUT_OPTIONS
           USER_SETTINGS              = 'X'
         IMPORTING
*          DOCUMENT_OUTPUT_INFO       =
           JOB_OUTPUT_INFO            = WA_JOB_OUTPUT_INFO
*          JOB_OUTPUT_OPTIONS         =
          TABLES
            IT_PHASE1                  = ITF
            IT_COMPONENT               = COMPONENT
            HEADER                     = HEADER
            POSITION                   = POSITION
            SEQUENCE                   = SEQUENCE
            TRIGGER_POINT              = TRIGGER_POINT
            PROD_REL_TOOL              = PROD_REL_TOOL
            IT_FINAL                   = IT_FINAL
            IT_AFFV                    = IT_AFFV
            IT_AFFT                    = IT_AFFT
            IT_PHASE                   = PHASE
            IT_ET_CHA                  = ET_CHA_DATA
         EXCEPTIONS
           FORMATTING_ERROR           = 1
           INTERNAL_ERROR             = 2
           SEND_ERROR                 = 3
           USER_CANCELED              = 4
           OTHERS                     = 5.
        IF SY-SUBRC <> 0.
*        Implement suitable error handling here
        ENDIF.
      ENDIF.

      IF WA_HEADER-PRODUCTION_PLANT = 1003 .
        IF TXT = 'X' AND TXT1 <> 'X'.
          MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            FORMNAME           = 'ZPROCESS_WATER_PRINT_NEW'
*           VARIANT            = ' '
*           DIRECT_CALL        = ' '
          IMPORTING
            FM_NAME            = FM_NAME
          EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.
        IF SY-SUBRC <> 0.
*          Implement suitable error handling here
        ENDIF.

        CALL FUNCTION FM_NAME"'/1BCDWB/SF00000125'
         EXPORTING
*          ARCHIVE_INDEX              =
*          ARCHIVE_INDEX_TAB          =
*          ARCHIVE_PARAMETERS         =
          CONTROL_PARAMETERS          = WA_SSFCTRLOP
*          MAIL_APPL_OBJ              =
*          MAIL_RECIPIENT             =
*          MAIL_SENDER                =
           OUTPUT_OPTIONS             = WA_OUTPUT_OPTIONS
           USER_SETTINGS              = 'X'
         IMPORTING
*          DOCUMENT_OUTPUT_INFO       =
           JOB_OUTPUT_INFO            = WA_JOB_OUTPUT_INFO
*          JOB_OUTPUT_OPTIONS         =
          TABLES
            IT_PHASE1                  = ITF
            IT_COMPONENT               = COMPONENT
            HEADER                     = HEADER
            POSITION                   = POSITION
            SEQUENCE                   = SEQUENCE
            TRIGGER_POINT              = TRIGGER_POINT
            PROD_REL_TOOL              = PROD_REL_TOOL
            IT_FINAL                   = IT_FINAL
            IT_AFFV                    = IT_AFFV
            IT_AFFT                    = IT_AFFT
            IT_PHASE                   = PHASE
            IT_ET_CHA                  = ET_CHA_DATA
         EXCEPTIONS
           FORMATTING_ERROR           = 1
           INTERNAL_ERROR             = 2
           SEND_ERROR                 = 3
           USER_CANCELED              = 4
           OTHERS                     = 5 .
        IF SY-SUBRC <> 0.
*         Implement suitable error handling here
        ENDIF.
      ENDIF.

      IF WA_HEADER-PRODUCTION_PLANT = 1401 .

        LOOP AT IT_RESB INTO WA_RESB.
          IF WA_RESB-XFEHL = 'X'.
            CLEAR: MP .
          ENDIF.
          IF WA_RESB-BDMNG <> WA_RESB-VMENG.
            CLEAR: QC .
          ENDIF.
        ENDLOOP.

        IF WA_HEADER-CONFIRMED_QUANTITY <> '0.000' AND MP = 'X' AND QC = 'X'.

          IF TXT = 'X'.
            MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.

          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              FORMNAME           = 'ZPROCESS_WATER_PRINT'
*             VARIANT            = ' '
*             DIRECT_CALL        = ' '
            IMPORTING
              FM_NAME            = FM_NAME
            EXCEPTIONS
              NO_FORM            = 1
              NO_FUNCTION_MODULE = 2
              OTHERS             = 3.
          IF SY-SUBRC <> 0.
*         Implement suitable error handling here
          ENDIF.

          CALL FUNCTION FM_NAME"'/1BCDWB/SF00000125'
           EXPORTING
*          ARCHIVE_INDEX              =
*          ARCHIVE_INDEX_TAB          =
*          ARCHIVE_PARAMETERS         =
             CONTROL_PARAMETERS          = WA_SSFCTRLOP
*          MAIL_APPL_OBJ              =
*          MAIL_RECIPIENT             =
*          MAIL_SENDER                =
             OUTPUT_OPTIONS             = WA_OUTPUT_OPTIONS
             USER_SETTINGS              = 'X'
           IMPORTING
*          DOCUMENT_OUTPUT_INFO       =
             JOB_OUTPUT_INFO            = WA_JOB_OUTPUT_INFO
*          JOB_OUTPUT_OPTIONS         =
            TABLES
              IT_PHASE1                  = ITF
              IT_COMPONENT               = COMPONENT
              HEADER                     = HEADER
              POSITION                   = POSITION
              SEQUENCE                   = SEQUENCE
              TRIGGER_POINT              = TRIGGER_POINT
              PROD_REL_TOOL              = PROD_REL_TOOL
              IT_FINAL                   = IT_FINAL
              IT_AFFV                    = IT_AFFV
              IT_AFFT                    = IT_AFFT
              IT_PHASE                   = PHASE
              IT_ET_CHA                  = ET_CHA_DATA
           EXCEPTIONS
             FORMATTING_ERROR           = 1
             INTERNAL_ERROR             = 2
             SEND_ERROR                 = 3
             USER_CANCELED              = 4
             OTHERS                     = 5.
          IF SY-SUBRC <> 0.
*          Implement suitable error handling here
          ENDIF.
        ELSE.
          IF TXT = 'X'.
*            MESSAGE 'Process Order print not possible due to in-sufficient RM stock & PI not maintained' TYPE 'I' DISPLAY LIKE 'E'.

            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
              EXPORTING
                TITEL        = 'Alert'
                TEXTLINE1    = '1.Process Order print not possible due to in-sufficient RM stock'
                TEXTLINE2    = '2.PI not maintained'
                START_COLUMN = 25
                START_ROW    = 10.

          ELSE.
*            MESSAGE 'Process Order print not possible due to in-sufficient RM stock' TYPE 'S' DISPLAY LIKE 'E'.

            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
              EXPORTING
                TITEL        = 'Alert'
                TEXTLINE1    = 'Process Order print not possible due to in-sufficient RM stock'
*               TEXTLINE2    = '2.PI not maintained'
                START_COLUMN = 25
                START_ROW    = 10.
          ENDIF.
        ENDIF.
      ENDIF.

*      IF TXT = 'X' AND TXT1 <> 'X'.
*        MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.
*      ELSE.
*        MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.
*      IF AUFNR IS NOT INITIAL AND AFFV IS INITIAL.
*        MESSAGE 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.


      IF P_PDF IS NOT INITIAL AND P_FILE IS NOT INITIAL.
        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            FORMAT                = 'PDF'
*           MAX_LINEWIDTH         = 132
*           ARCHIVE_INDEX         = ' '
*           COPYNUMBER            = 0
*           ASCII_BIDI_VIS2LOG    = ' '
*           PDF_DELETE_OTFTAB     = ' '
          IMPORTING
            BIN_FILESIZE          = MI_BYTECOUNT
*           BIN_FILE              =
          TABLES
            OTF                   = WA_JOB_OUTPUT_INFO-OTFDATA
            LINES                 = LINES
          EXCEPTIONS
            ERR_MAX_LINEWIDTH     = 1
            ERR_FORMAT            = 2
            ERR_CONV_NOT_POSSIBLE = 3
            ERR_BAD_OTF           = 4
            OTHERS                = 5.
        IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          MESSAGE 'CONVERT ERROR ' TYPE 'S'.
        ELSE.
          CONCATENATE P_FILE '.pdf' INTO FILE.
*      file = p_file.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              BIN_FILESIZE = MI_BYTECOUNT
              FILENAME     = FILE
              FILETYPE     = 'BIN'
            TABLES
              DATA_TAB     = LINES[].

          IF SY-SUBRC = 0.
            MESSAGE 'Downloaded Successfully' TYPE 'S'.
            CLEAR :P_PDF,P_FILE,FILE.
*            CALL TRANSACTION 'ZPO_WATERBASE'.
            CLEAR : AUFNR , p_file.
            REFRESH : INSP_CHAR , INSP_CHARLIST , INSP_LIST_TEMP , IT_FINAL.
*            CLEAR : AUFNR.
          ENDIF.


        ENDIF.
      ENDIF.

    ENDIF.
*ELSEIF aufnr is NOT INITIAL and affv is INITIAL.
*    message 'PI not maintained' TYPE 'S' DISPLAY LIKE 'W'.
*endif.
*ENDIF.
  ENDIF.

*  LEAVE TO TRANSACTION 'ZPO_DETVK'.
