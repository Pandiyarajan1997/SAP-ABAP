FUNCTION ZBAPI_SKU_GST_TAX_MASTER_SIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_MATNR) TYPE  MARA-MATNR OPTIONAL
*"     VALUE(P_KUNNR) TYPE  KNA1-KUNNR OPTIONAL
*"     VALUE(P_VKBUR) TYPE  KNVV-VKBUR OPTIONAL
*"     VALUE(P_KSCHL) TYPE  KONH-KSCHL OPTIONAL
*"  TABLES
*"      IT_TAX_MASTER TYPE  ZTT_CNF_TAX_MASTER
*"      IT_TAX_TYPE TYPE  ZTT_TAX_TYPE
*"      IT_COND_TYPE TYPE  ZTT_COND_TYPE
*"      IT_GL01 TYPE  ZTT_CNF_GL01
*"      P_REGIO STRUCTURE  ZSTR_REGION
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE
*"      S_MTART TYPE  ZTT_CNF_MTART
*"      RETURN STRUCTURE  BAPIRET2
*"      S_TIME STRUCTURE  ZTT_STRU_UTIME_CDHDR
*"----------------------------------------------------------------------
  "TEST
*&-------------------------------------- DECLARATIONS - BEGIN -----------------------------------------------
  TYPES: BEGIN OF T_T001KW,
          BWKEY TYPE T001K-BWKEY,
          BUKRS TYPE T001K-BUKRS,
          WERKS TYPE T001W-WERKS,
          REGIO TYPE T001W-REGIO,
         END OF T_T001KW,

         BEGIN OF T_MARC,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
         END OF T_MARC,

         BEGIN OF TY_TVKO,
          VKORG TYPE TVKO-VKORG,
          BUKRS TYPE TVKO-BUKRS,
         END OF TY_TVKO,

*         BEGIN OF TY_MARA, " Commented for test
*           MATNR TYPE MARA-MATNR,
*           MTART TYPE MARA-MTART,
*         END OF TY_MARA,

         BEGIN OF TY_TVKBZ,
          VKORG TYPE TVKBZ-VKORG,
          VKBUR TYPE TVKBZ-VKBUR,
         END OF TY_TVKBZ.

  "Started By Ram on 1/7 " Commented for test
*         BEGIN OF TY_A709,
*          KAPPL     TYPE     A709-KAPPL,
*          KSCHL     TYPE     A709-KSCHL,
*          WKREG     TYPE     A709-WKREG,
*          "LAND1     TYPE     A709-LAND1,
*          REGIO     TYPE     A709-REGIO,
*          TAXK1     TYPE     A709-TAXK1,
*          MATNR     TYPE     A709-MATNR,
*          KFRST     TYPE     A709-KFRST,
*          DATBI     TYPE     A709-DATBI,
*          DATAB     TYPE     A709-DATAB,
*          KBSTAT     TYPE    A709-KBSTAT,
*          KNUMH     TYPE     A709-KNUMH,
*          ERDAT     TYPE     KONH-ERDAT,
*          KOPOS     TYPE     KONP-KOPOS,
**          STFKZ     TYPE     KONP-STFKZ,
*          KBETR     TYPE     KONP-KBETR,
*          KONWA     TYPE     KONP-KONWA,
*          KPEIN     TYPE     KONP-KPEIN,
*          KMEIN     TYPE     KONP-KMEIN,
*          LOEVM_KO  TYPE     KONP-LOEVM_KO,
*         END OF TY_A709,

*        BEGIN OF TY_A710, " Commented for test
*          KAPPL     TYPE     A710-KAPPL,
*          KSCHL     TYPE     A710-KSCHL,
*          "WKREG     TYPE     A710-WKREG,
*          LAND1     TYPE     A710-LAND1,
*          VKORG     TYPE     A710-VKORG,
*          WKREG     TYPE     A710-WKREG,
*          TAXK1     TYPE     A710-TAXK1,
*          REGIO     TYPE     A710-REGIO,
*          KFRST     TYPE     A710-KFRST,
*          DATBI     TYPE     A710-DATBI,
*          DATAB     TYPE     A710-DATAB,
*          KBSTAT     TYPE    A710-KBSTAT,
*          KNUMH     TYPE     A710-KNUMH,
*          ERDAT     TYPE     KONH-ERDAT,
*          KOPOS     TYPE     KONP-KOPOS,
**          STFKZ     TYPE     KONP-STFKZ,
*          KBETR     TYPE     KONP-KBETR,
*          KONWA     TYPE     KONP-KONWA,
*          KPEIN     TYPE     KONP-KPEIN,
*          KMEIN     TYPE     KONP-KMEIN,
*          LOEVM_KO  TYPE     KONP-LOEVM_KO,
*         END OF TY_A710.

  TYPES: BEGIN OF TY_SKB1,
          BUKRS TYPE SKB1-BUKRS,
          SAKNR TYPE SKB1-SAKNR,
         END OF TY_SKB1,

         BEGIN OF TY_SKAT,
           SAKNR TYPE SKAT-SAKNR,
           TXT20 TYPE SKAT-TXT20,
         END OF TY_SKAT.

  TYPES:  BEGIN OF TY_T001,
            BUKRS TYPE T001-BUKRS,
            BUTXT TYPE T001-BUTXT,
          END OF TY_T001.


  DATA:
*        IT_T001KW   TYPE TABLE OF T_T001KW, " Commented for check
*        WA_T001KW   TYPE          T_T001KW, " Commented for check
*        IT_MARC     TYPE TABLE OF T_MARC,   " Commented for check
*        WA_MARC     TYPE          T_MARC,   " Commented for check
*        IT_MARA     TYPE TABLE OF TY_MARA,  " Commented for check
*        WA_MARA     TYPE          TY_MARA,  " Commented for check
        IT_TVKO     TYPE TABLE OF TY_TVKO,
        WA_TVKO     TYPE          TY_TVKO,
        WA_TAX_MASTER   TYPE      ZSTR_CNF_TAX_MASTER,
        WA_GL01         TYPE      ZSTR_CNF_GL01,
        IT_SKB1     TYPE TABLE OF TY_SKB1,
        WA_SKB1     TYPE          TY_SKB1,
        IT_SKAT     TYPE TABLE OF TY_SKAT,
        WA_SKAT     TYPE          TY_SKAT,
        IT_VKBUR    TYPE TABLE OF TY_TVKBZ,
        WA_VKBUR    TYPE          TY_TVKBZ,
        WA_RETURN   TYPE          BAPIRET2,
        IT_T001     TYPE TABLE OF TY_T001.


*  DATA:   IT_A709    TYPE TABLE OF TY_A709, " Commented for check
*          WA_A709    TYPE          TY_A709, " Commented for check
*          IT_A710    TYPE TABLE OF TY_A710, " Commented for check
*          WA_A710    TYPE          TY_A710. " Commented for check

******************************************* IT_TAX_MASTER _ Full Check Start****************************************************
  TYPES: BEGIN OF TY_T001K,
          BWKEY TYPE T001K-BWKEY,
          BUKRS TYPE T001K-BUKRS,
        END OF TY_T001K,

      BEGIN OF TY_T001W,
        WERKS TYPE T001W-WERKS,
        BWKEY TYPE T001W-BWKEY,
        REGIO TYPE T001W-REGIO,
      END OF TY_T001W,

      BEGIN OF TY_MARC,
              MATNR TYPE MARC-MATNR,
              WERKS TYPE MARC-WERKS,
            END OF TY_MARC,
 BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
      END OF TY_MARA,

 BEGIN OF TY_A709,
        KAPPL TYPE A709-KAPPL,
        KSCHL TYPE A709-KSCHL,
        WKREG TYPE A709-WKREG,
        REGIO TYPE A709-REGIO,
        TAXK1 TYPE A709-TAXK1,
        MATNR TYPE A709-MATNR,
        KFRST TYPE A709-KFRST,
        DATBI TYPE A709-DATBI,
        DATAB TYPE A709-DATAB,
        KBSTAT TYPE A709-KBSTAT,
        KNUMH TYPE A709-KNUMH,
      END OF TY_A709,

 BEGIN OF TY_A710,
        KAPPL TYPE A710-KAPPL,
        KSCHL TYPE A710-KSCHL,
        LAND1 TYPE A710-LAND1,
        VKORG TYPE A710-VKORG,
        WKREG TYPE A710-WKREG,
        TAXK1 TYPE A710-TAXK1,
        REGIO TYPE A710-REGIO,
        KFRST TYPE A710-KFRST,
        DATBI TYPE A710-DATBI,
        DATAB TYPE A710-DATAB,
        KBSTAT TYPE A710-KBSTAT,
        KNUMH TYPE A710-KNUMH,
      END OF TY_A710,


 BEGIN OF TY_KONP,
        KNUMH TYPE KONP-KNUMH,
        KOPOS TYPE KONP-KOPOS,
        KSCHL TYPE KONP-KSCHL,
        KBETR TYPE KONP-KBETR,
        KONWA TYPE KONP-KONWA,
        KPEIN TYPE KONP-KPEIN,
        KMEIN TYPE KONP-KMEIN,
        LOEVM_KO TYPE KONP-LOEVM_KO,
      END OF TY_KONP,

 BEGIN OF TY_KONH,
        KNUMH TYPE KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
        ERDAT TYPE KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
        KSCHL TYPE KONH-KSCHL, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
      END OF TY_KONH.



  DATA: GT_T001K TYPE TABLE OF TY_T001K,
        GS_T001K TYPE TY_T001K,
        GT_T001W TYPE TABLE OF TY_T001W,
        GS_T001W TYPE TY_T001W,
        GT_MARC TYPE TABLE OF TY_MARC,
        GS_MARC TYPE TY_MARC,
        GT_MARA TYPE TABLE OF TY_MARA,
        GS_MARA TYPE TY_MARA,
        GT_A709 TYPE TABLE OF TY_A709,
        GS_A709 TYPE TY_A709,
        GT_A709O TYPE TABLE OF TY_A709,
        GS_A709O TYPE TY_A709,
        GT_A709M TYPE TABLE OF TY_A709,
        GS_A709M TYPE TY_A709,
        GT_A710 TYPE TABLE OF TY_A710,
        GS_A710 TYPE TY_A710,
        GT_KONP TYPE TABLE OF TY_KONP,
        GS_KONP TYPE TY_KONP,
        GT_KONH TYPE TABLE OF TY_KONH,
        GS_KONH TYPE TY_KONH,
        GT_KONP1 TYPE TABLE OF TY_KONP,
        GS_KONP1 TYPE TY_KONP,
        GT_KONP2 TYPE TABLE OF TY_KONP,
        GS_KONP2 TYPE TY_KONP,
        GT_KONH1 TYPE TABLE OF TY_KONH,
        GS_KONH1 TYPE TY_KONH,
        LV_WH_MATNR TYPE STRING,
        LV_WH_KUNNR TYPE STRING,
        LV_WH_VKBUR TYPE STRING,
        LV_WH_KSCHL_A709 TYPE STRING,
        LV_WH_CDHDR_A709 TYPE STRING,
        LV_WH_KSCHL_A710 TYPE STRING,
        LV_WH_REGIO TYPE STRING.

  TYPES: BEGIN OF TY_CDHDR,
        OBJECTCLAS TYPE CDHDR-OBJECTCLAS,
        OBJECTID TYPE CDHDR-OBJECTID,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,
        TCODE TYPE CDHDR-TCODE,
      END OF TY_CDHDR.

  TYPES: BEGIN OF TY_MATOBJ,
          OBJECTID TYPE CDHDR-OBJECTID,
          MATNR TYPE MARC-MATNR,
         END OF TY_MATOBJ.

  DATA: LT_CDHDR TYPE TABLE OF TY_CDHDR,
        LS_CDHDR TYPE TY_CDHDR,
        LV_TABIX TYPE I,
        GT_MATOBJ TYPE TABLE OF TY_MATOBJ,
        GS_MATOBJ TYPE TY_MATOBJ.

  TYPES: BEGIN OF TY_OBJID,
        KNUMH TYPE A709-KNUMH,
         END OF TY_OBJID.

  DATA: GT_OBJID TYPE TABLE OF TY_OBJID,
        GS_OBJID TYPE TY_OBJID.

  DATA: LV_LINES(4)  TYPE N.

  FIELD-SYMBOLS: <FS_MATOBJ> TYPE TY_MATOBJ .


  IF S_DATE IS INITIAL.

    S_DATE-SIGN   = 'I'.
    S_DATE-OPTION = 'EQ'.
    S_DATE-LOW    = SY-DATUM.
    APPEND S_DATE TO S_DATE.

    CLEAR: S_DATE.

  ENDIF.

  SELECT OBJECTCLAS
         OBJECTID
         UDATE
         UTIME
         TCODE
         FROM CDHDR
         INTO TABLE  LT_CDHDR
         WHERE OBJECTCLAS EQ 'COND_A'
         AND UDATE IN S_DATE AND TCODE IN ('VK11' , 'VK12')
         AND UTIME IN S_TIME.

  LOOP AT LT_CDHDR INTO LS_CDHDR.

    GS_OBJID-KNUMH = LS_CDHDR-OBJECTID.
    APPEND GS_OBJID TO GT_OBJID.

  ENDLOOP.

  IF GT_OBJID IS NOT INITIAL.

    IF P_KSCHL IS NOT INITIAL AND P_BUKRS IS NOT INITIAL AND P_REGIO IS NOT INITIAL.

      CONCATENATE 'KAPPL =' '''V''' 'AND REGIO IN P_REGIO' 'AND KNUMH EQ GT_OBJID-KNUMH' 'AND KSCHL EQ P_KSCHL AND VKORG EQ P_BUKRS' INTO LV_WH_KSCHL_A710 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS INITIAL AND P_BUKRS IS NOT INITIAL AND P_REGIO IS INITIAL.

      CONCATENATE 'KAPPL =' '''V''' 'AND' 'KNUMH EQ GT_OBJID-KNUMH AND' 'VKORG EQ P_BUKRS AND' '(' 'KSCHL EQ' '''JOCG''' 'OR KSCHL EQ'
         '''JOIG''' 'OR KSCHL EQ' '''JOSG''' 'OR KSCHL EQ' '''JOUG''' ')' INTO LV_WH_KSCHL_A710 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS INITIAL AND P_BUKRS IS NOT INITIAL AND P_REGIO IS NOT INITIAL.

      CONCATENATE 'KAPPL =' '''V''' 'AND REGIO IN P_REGIO' 'AND KNUMH EQ GT_OBJID-KNUMH' 'AND VKORG EQ P_BUKRS' INTO LV_WH_KSCHL_A710 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS NOT INITIAL AND P_BUKRS IS NOT INITIAL AND P_REGIO IS INITIAL.

      CONCATENATE 'KAPPL =' '''V''' 'AND KNUMH EQ GT_OBJID-KNUMH' 'AND KSCHL EQ P_KSCHL AND VKORG EQ P_BUKRS' INTO LV_WH_KSCHL_A710 SEPARATED BY SPACE.


    ENDIF.


    IF P_KSCHL IS NOT INITIAL AND P_REGIO IS NOT INITIAL.

      CONCATENATE 'KNUMH EQ GT_OBJID-KNUMH AND KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL' 'AND REGIO IN P_REGIO' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS INITIAL AND P_REGIO IS INITIAL.

      CONCATENATE 'KNUMH EQ GT_OBJID-KNUMH AND KAPPL =' '''V''' 'AND' '(' 'KSCHL EQ' '''JOCG''' 'OR KSCHL EQ'
       '''JOIG''' 'OR KSCHL EQ' '''JOSG''' 'OR KSCHL EQ' '''JOUG''' ')' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS NOT INITIAL AND P_REGIO IS INITIAL.

      CONCATENATE 'KNUMH EQ GT_OBJID-KNUMH AND KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.

    ELSEIF P_KSCHL IS INITIAL AND P_REGIO IS NOT INITIAL.

      CONCATENATE 'KNUMH EQ GT_OBJID-KNUMH AND KAPPL =' '''V''' 'AND REGIO IN P_REGIO' 'AND' '(' 'KSCHL EQ' '''JOCG''' 'OR KSCHL EQ'
      '''JOIG''' 'OR KSCHL EQ' '''JOSG''' 'OR KSCHL EQ' '''JOUG''' ')' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.

    ENDIF.

    SELECT KAPPL
           KSCHL
           WKREG
           REGIO
           TAXK1
           MATNR
           KFRST
           DATBI
           DATAB
           KBSTAT
           KNUMH
           FROM A709
           INTO TABLE GT_A709O
           FOR ALL ENTRIES IN GT_OBJID
           WHERE (LV_WH_CDHDR_A709).

*      KNUMH EQ LT_CDHDR-OBJECTID AND KAPPL = 'V' AND ( KSCHL EQ 'JOCG' OR KSCHL EQ 'JOIG' OR KSCHL EQ 'JOSG' OR KSCHL EQ 'JOUG' ).


    CLEAR LV_WH_CDHDR_A709.

*  ELSEIF LT_CDHDR IS INITIAL.
*
*    IF P_KSCHL IS NOT INITIAL.
*
*      CONCATENATE 'KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.
*
*    ELSEIF P_KSCHL IS INITIAL.
*
*      CONCATENATE 'KAPPL =' '''V''' 'AND' '('  'KSCHL EQ' '''JOCG''' 'OR KSCHL EQ'
*       '''JOIG''' 'OR KSCHL EQ' '''JOSG''' 'OR KSCHL EQ' '''JOUG''' ')' INTO LV_WH_CDHDR_A709 SEPARATED BY SPACE.
*
*    ENDIF.
*
*    SELECT KAPPL
*           KSCHL
*           WKREG
*           REGIO
*           TAXK1
*           MATNR
*           KFRST
*           DATBI
*           DATAB
*           KBSTAT
*           KNUMH
*           FROM A709
*           INTO TABLE GT_A709O
*           WHERE (LV_WH_CDHDR_A709).
*
*    CLEAR LV_WH_CDHDR_A709.



    IF P_MATNR IS NOT INITIAL.


      IF P_MATNR IS NOT INITIAL.

        CONCATENATE 'WERKS EQ GT_T001W-WERKS AND MATNR EQ P_MATNR AND MATNR NE' ''' ''' INTO LV_WH_MATNR SEPARATED BY SPACE.

      ELSE.

        LV_WH_MATNR = 'WERKS EQ GT_T001W-WERKS'.

      ENDIF.


      IF P_REGIO IS NOT INITIAL.

        LV_WH_REGIO = 'BWKEY EQ GT_T001K-BWKEY AND REGIO IN P_REGIO'.

      ELSE.

        LV_WH_REGIO = 'BWKEY EQ GT_T001K-BWKEY'.

      ENDIF.


      SELECT BWKEY
             BUKRS
             FROM T001K
             INTO TABLE GT_T001K
             WHERE BUKRS EQ P_BUKRS.

      IF GT_T001K IS NOT INITIAL.

        SELECT WERKS
               BWKEY
               REGIO
               FROM T001W
               INTO TABLE GT_T001W
               FOR ALL ENTRIES IN GT_T001K
               WHERE (LV_WH_REGIO). " BWKEY EQ GT_T001K-BWKEY.

      ENDIF.

      IF GT_T001W IS NOT INITIAL.


        SELECT MATNR
               WERKS
               FROM MARC
               INTO TABLE GT_MARC
               FOR ALL ENTRIES IN GT_T001W
               WHERE (LV_WH_MATNR).  " WERKS EQ GT_T001W-WERKS.

      ENDIF.


      IF GT_MARC IS NOT INITIAL.


        SELECT MATNR
               MTART
               FROM MARA
               INTO TABLE GT_MARA
               FOR ALL ENTRIES IN GT_MARC
               WHERE MATNR EQ GT_MARC-MATNR AND
               MTART IN R_MTART.


      ELSEIF GT_MARC IS INITIAL.


        SELECT MATNR
               MTART
               FROM MARA
               INTO TABLE GT_MARA
               WHERE MTART IN R_MTART.

      ENDIF.

      IF GT_MARA IS NOT INITIAL.

        IF P_KSCHL IS NOT INITIAL AND P_REGIO IS NOT INITIAL.

          CONCATENATE 'KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL AND MATNR EQ GT_MARA-MATNR' 'AND REGIO IN P_REGIO' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.

        ELSEIF P_KSCHL IS INITIAL AND P_REGIO IS INITIAL.

          CONCATENATE 'KAPPL =' '''V''' 'AND MATNR EQ GT_MARA-MATNR' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.

        ELSEIF P_KSCHL IS NOT INITIAL AND P_REGIO IS INITIAL.

          CONCATENATE 'KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL AND MATNR EQ GT_MARA-MATNR' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.

        ELSEIF P_KSCHL IS INITIAL AND P_REGIO IS NOT INITIAL.

          CONCATENATE 'KAPPL =' '''V''' 'AND MATNR EQ GT_MARA-MATNR' 'AND REGIO IN P_REGIO' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.

        ENDIF.

        SELECT KAPPL
               KSCHL
               WKREG
               REGIO
               TAXK1
               MATNR
               KFRST
               DATBI
               DATAB
               KBSTAT
               KNUMH
               FROM A709
               INTO TABLE GT_A709M
               FOR ALL ENTRIES IN GT_MARA
               WHERE (LV_WH_KSCHL_A709).

        CLEAR LV_WH_KSCHL_A709.


*  ELSEIF GT_MARA IS INITIAL.
*
*
*    IF P_KSCHL IS NOT INITIAL.
*
*      CONCATENATE 'KAPPL =' '''V''' 'AND KSCHL EQ P_KSCHL' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.
*
*    ELSEIF P_KSCHL IS INITIAL.
*
*      CONCATENATE 'KAPPL =' '''V''' INTO LV_WH_KSCHL_A709 SEPARATED BY SPACE.
*
*    ENDIF.
*
*    SELECT KAPPL
*           KSCHL
*           WKREG
*           REGIO
*           TAXK1
*           MATNR
*           KFRST
*           DATBI
*           DATAB
*           KBSTAT
*           KNUMH
*           FROM A709
*           INTO TABLE GT_A709M
*           WHERE (LV_WH_KSCHL_A709).
*
*    CLEAR LV_WH_KSCHL_A709.


      ENDIF.

*  LV_TABIX = 1.


*KAPPL KSCHL WKREG REGIO TAXK1 MATNR KFRST DATBI DATAB KBSTAT KNUMH

*  SORT: GT_A709M ASCENDING BY KAPPL KSCHL WKREG REGIO TAXK1 MATNR KFRST DATBI.

*KAPPL = KSCHL
*                                               WKREG
*                                               REGIO
*                                               TAXK1
*                                               MATNR
*                                               KFRST
*                                               DATBI.
*    READ TABLE GT_A709M INTO GS_A709M WITH KEY KNUMH = GS_A709O BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*
*
*
*    ENDIF.


      SORT: GT_A709M ASCENDING BY KNUMH,
            GT_A709O ASCENDING BY KNUMH.

      LV_TABIX = 1.
      IF P_MATNR IS NOT INITIAL.

        LOOP AT GT_A709O INTO GS_A709O.

          LOOP AT GT_A709M INTO GS_A709M FROM LV_TABIX.

            IF GS_A709O-KNUMH <> GS_A709M-KNUMH.

              LV_TABIX = SY-TABIX.
              EXIT.

            ELSE.


              GS_A709-KNUMH = GS_A709M-KNUMH.
              GS_A709-KAPPL = GS_A709M-KAPPL.
              GS_A709-KSCHL = GS_A709M-KSCHL.
              GS_A709-WKREG = GS_A709M-WKREG.
              GS_A709-MATNR = GS_A709M-MATNR.
              GS_A709-KFRST = GS_A709M-KFRST.
              GS_A709-DATBI = GS_A709M-DATBI.
              GS_A709-DATAB = GS_A709M-DATAB.
              GS_A709-REGIO = GS_A709M-REGIO.

              APPEND GS_A709 TO GT_A709.

            ENDIF.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

    ELSE.

      APPEND LINES OF GT_A709O TO GT_A709.

    ENDIF.

    IF GT_A709 IS NOT INITIAL.

      SELECT KNUMH
             ERDAT
             KSCHL
             FROM KONH "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
             INTO TABLE GT_KONH
             FOR ALL ENTRIES IN GT_A709
             WHERE KNUMH EQ GT_A709-KNUMH AND
             KSCHL EQ GT_A709-KSCHL.

    ENDIF.

    IF GT_KONH IS NOT INITIAL.

      SELECT KNUMH
             KOPOS
             KSCHL
             KBETR
             KONWA
             KPEIN
             KMEIN
             LOEVM_KO
             FROM KONP
             INTO TABLE GT_KONP
             FOR ALL ENTRIES IN GT_KONH
             WHERE KNUMH EQ GT_KONH-KNUMH AND
             KSCHL EQ GT_KONH-KSCHL.

    ENDIF.


    IF P_MATNR IS INITIAL.


      SELECT KAPPL
             KSCHL
             LAND1
             VKORG
             WKREG
             TAXK1
             REGIO
             KFRST
             DATBI
             DATAB
             KBSTAT
             KNUMH
             FROM A710
             INTO TABLE GT_A710
             FOR ALL ENTRIES IN GT_OBJID
             WHERE (LV_WH_KSCHL_A710).
      " KAPPL = 'V' AND KSCHL EQ P_KSCHL.

      IF GT_A710 IS NOT INITIAL.

        SELECT KNUMH
               ERDAT
               KSCHL
               FROM KONH "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
               INTO TABLE GT_KONH1
               FOR ALL ENTRIES IN GT_A710
               WHERE KNUMH EQ GT_A710-KNUMH AND
               KSCHL EQ GT_A710-KSCHL.

      ENDIF.


      IF GT_KONH IS NOT INITIAL.

        SELECT KNUMH
               KOPOS
               KSCHL
               KBETR
               KONWA
               KPEIN
               KMEIN
               LOEVM_KO
               FROM KONP
               INTO TABLE GT_KONP1
               FOR ALL ENTRIES IN GT_KONH1
               WHERE KNUMH EQ GT_KONH1-KNUMH AND
               KSCHL EQ GT_KONH1-KSCHL.

      ENDIF.


      IF GT_KONH IS INITIAL.

        SELECT KNUMH
               KOPOS
               KSCHL
               KBETR
               KONWA
               KPEIN
               KMEIN
               LOEVM_KO
               FROM KONP
               INTO TABLE GT_KONP2
               FOR ALL ENTRIES IN GT_KONH1
               WHERE KNUMH EQ GT_KONH1-KNUMH AND
               KSCHL EQ GT_KONH1-KSCHL.

      ENDIF.

    ENDIF.

    SORT: GT_KONH BY KNUMH KSCHL,
          GT_KONP BY KNUMH KSCHL.

*A~TAXK1
*A~KBSTAT


    LOOP AT GT_A709 INTO GS_A709.

      WA_TAX_MASTER-TABLE_NAME = 'A709'.
      WA_TAX_MASTER-KNUMH = GS_A709-KNUMH.
      WA_TAX_MASTER-KAPPL = GS_A709-KAPPL.
      WA_TAX_MASTER-KSCHL = GS_A709-KSCHL.
      WA_TAX_MASTER-WKREG = GS_A709-WKREG.
      WA_TAX_MASTER-MATNR = GS_A709-MATNR.
      WA_TAX_MASTER-KFRST = GS_A709-KFRST.
      WA_TAX_MASTER-DATBI = GS_A709-DATBI.
      WA_TAX_MASTER-DATAB = GS_A709-DATAB.
      WA_TAX_MASTER-REGIO = GS_A709-REGIO.

      READ TABLE GT_KONH INTO GS_KONH WITH KEY KNUMH = GS_A709-KNUMH KSCHL = GS_A709-KSCHL BINARY SEARCH.
      IF SY-SUBRC EQ 0.

        WA_TAX_MASTER-ERDAT = GS_KONH-ERDAT.

      ENDIF.


      READ TABLE GT_KONP INTO GS_KONP WITH KEY KNUMH = GS_KONH-KNUMH KSCHL = GS_KONH-KSCHL BINARY SEARCH.
      IF SY-SUBRC EQ 0.

        WA_TAX_MASTER-KOPOS = GS_KONP-KOPOS.
        WA_TAX_MASTER-KBETR = GS_KONP-KBETR.
        WA_TAX_MASTER-KONWA = GS_KONP-KONWA.
        WA_TAX_MASTER-KPEIN = GS_KONP-KPEIN.
        WA_TAX_MASTER-KMEIN = GS_KONP-KMEIN.
        WA_TAX_MASTER-LOEVM_KO = GS_KONP-LOEVM_KO.

      ENDIF.

      APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
      CLEAR: WA_TAX_MASTER ,GS_A709,GS_KONH,GS_KONP.
    ENDLOOP.


    IF P_MATNR IS INITIAL.


      SORT: GT_KONH1 BY KNUMH KSCHL,
            GT_KONP1 BY KNUMH KSCHL.


      LOOP AT GT_A710 INTO GS_A710.

        WA_TAX_MASTER-TABLE_NAME = 'A710'.
        WA_TAX_MASTER-KSCHL = GS_A710-KSCHL.
        WA_TAX_MASTER-KAPPL = GS_A710-KAPPL.
        WA_TAX_MASTER-LAND1 = GS_A710-LAND1.
        WA_TAX_MASTER-VKORG = GS_A710-VKORG.
        WA_TAX_MASTER-WKREG = GS_A710-WKREG.
        WA_TAX_MASTER-REGIO = GS_A710-REGIO.
        WA_TAX_MASTER-KFRST = GS_A710-KFRST.
        WA_TAX_MASTER-DATBI = GS_A710-DATBI.
        WA_TAX_MASTER-DATAB = GS_A710-DATAB.
        WA_TAX_MASTER-KNUMH = GS_A710-KNUMH.

        READ TABLE GT_KONH1 INTO GS_KONH1 WITH KEY KNUMH = GS_A710-KNUMH KSCHL = GS_A710-KSCHL BINARY SEARCH.
        IF SY-SUBRC EQ 0.

          WA_TAX_MASTER-ERDAT = GS_KONH1-ERDAT.

        ENDIF.

        READ TABLE GT_KONP1 INTO GS_KONP1 WITH KEY KNUMH = GS_KONH1-KNUMH KSCHL = GS_KONH1-KSCHL BINARY SEARCH.
        IF SY-SUBRC EQ 0.

          WA_TAX_MASTER-KOPOS = GS_KONP1-KOPOS.
          WA_TAX_MASTER-KBETR = GS_KONP1-KBETR.
          WA_TAX_MASTER-KONWA = GS_KONP1-KONWA.
          WA_TAX_MASTER-KPEIN = GS_KONP1-KPEIN.
          WA_TAX_MASTER-KMEIN = GS_KONP1-KMEIN.
          WA_TAX_MASTER-LOEVM_KO = GS_KONP1-LOEVM_KO.

        ENDIF.

        READ TABLE GT_KONP2 INTO GS_KONP2 WITH KEY KNUMH = GS_KONH1-KNUMH KSCHL = GS_KONH1-KSCHL BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
        IF SY-SUBRC EQ 0.

          WA_TAX_MASTER-KOPOS = GS_KONP2-KOPOS.
          WA_TAX_MASTER-KBETR = GS_KONP2-KBETR.
          WA_TAX_MASTER-KONWA = GS_KONP2-KONWA.
          WA_TAX_MASTER-KPEIN = GS_KONP2-KPEIN.
          WA_TAX_MASTER-KMEIN = GS_KONP2-KMEIN.
          WA_TAX_MASTER-LOEVM_KO = GS_KONP2-LOEVM_KO.

        ENDIF.

        APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
        CLEAR: WA_TAX_MASTER , GS_A710,GS_KONH1,GS_KONP1.
      ENDLOOP.

    ENDIF.
  ENDIF.
*        KAPPL        -KNUMH
*       KSCHL*       -KAPPL
*       LAND1*        -KSCHL
*       VKORG*        -WKREG
* WKREG*        -VKORG
*       TAXK1        -LAND1
*       REGIO*        -REGIO
*       KFRST*        -VKBUR
*       DATBI*        -KUNNR
*         DATAB*         -TAXK6
*       KBSTAT*
*      KNUMH*

*    TAXK7
*    TAXK8
*    MATNR
*    KFRST
*    DATBI
*    DATAB
*    ERDAT
*    KOPOS
*    KBETR
*    KONWA
*    KPEIN
*    KMEIN
*    LOEVM





*            A~KSCHL
*            A~LAND1
*            A~VKORG
*            A~WKREG
*            A~TAXK1
*            A~REGIO
*            A~KFRST
*            A~DATBI
*            A~DATAB
*            A~KBSTAT
*            A~KNUMH
*            C~ERDAT
*            B~KOPOS
**                B~STFKZ
*            B~KBETR
*            B~KONWA
*            B~KPEIN
*            B~KMEIN
*            B~LOEVM_KO









*    WA_TAX_MASTER-VKORG =
*    WA_TAX_MASTER-LAND1 =
*    WA_TAX_MASTER-VKBUR =
*    WA_TAX_MASTER-KUNNR =
*    WA_TAX_MASTER-TAXK6 =
*    WA_TAX_MASTER-TAXK7 =
*    WA_TAX_MASTER-TAXK8 =
*    WA_TAX_MASTER-KOPOS = GS_KONP-KOPOS.
*    WA_TAX_MASTER-KBETR = GS_KONP-KBETR.
*    WA_TAX_MASTER-KONWA = GS_KONP-KONWA.
*    WA_TAX_MASTER-KPEIN = GS_KONP-KPEIN.
*    WA_TAX_MASTER-KMEIN = GS_KONP-KMEIN.







*
*      ENDLOOP.

*            B~KOPOS
**                B~STFKZ
*            B~KBETR
*            B~KONWA
*            B~KPEIN
*            B~KMEIN
*            B~LOEVM_KO

*            B~KOPOS
**                B~STFKZ
*            B~KBETR
*            B~KONWA
*            B~KPEIN
*            B~KMEIN
*            B~LOEVM_KO

*C~KNUMH
*C~KSCHL



******************************************* IT_TAX_MASTER _ Full Check End******************************************************




*&-------------------------------------- DECLARATIONS - END---------------------------------------------------


*&-------------------------------------- Data Retrieval - BEGIN -----------------------------------------------
*&------------------------------------------------ Check If the given company code exists
  SELECT BUKRS
         BUTXT
    FROM T001
    INTO TABLE IT_T001
    WHERE BUKRS = P_BUKRS.

  IF SY-SUBRC <> 0.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Company Code does not Exist!!! Check Your Entries!!!'.
    CLEAR: GV_INDEX_MSG.
    EXIT.
  ENDIF.

*&------------------------------------------------Get GL Account List
  SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
         SAKNR
    FROM SKB1
    INTO TABLE IT_SKB1
    WHERE BUKRS = P_BUKRS.

  IF SY-SUBRC = 0.
    SORT IT_SKB1 BY SAKNR.

    SELECT SAKNR
           TXT20
      FROM SKAT
      INTO TABLE IT_SKAT
      FOR ALL ENTRIES IN IT_SKB1
      WHERE SAKNR = IT_SKB1-SAKNR
      AND   SPRAS = SY-LANGU
      AND   KTOPL = 'YAIN'.
  ENDIF.

*&------------------------------------------------ Get Tax Types
  SELECT TAXKM
         VTEXT
    FROM TSKMT
    INTO TABLE IT_TAX_TYPE
    WHERE SPRAS = SY-LANGU.

  IF SY-SUBRC = 0.
    SORT IT_TAX_TYPE BY TAXKM VTEXT DESCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_TAX_TYPE COMPARING TAXKM.
  ENDIF.

*&------------------------------------------------ Get Condition Types
  SELECT KVEWE
         KAPPL
         KSCHL
         VTEXT
    FROM T685T
    INTO TABLE IT_COND_TYPE
    WHERE KAPPL = 'V'
    AND   KSCHL IN ('ZIVP' , 'ZIVC', 'JIVA', 'ZVSR')
    AND   SPRAS = SY-LANGU.

*******************************************
  RANGES: R_MTART FOR MARA-MTART.

  LOOP AT S_MTART.
    R_MTART-SIGN = 'I'.
    R_MTART-OPTION = 'EQ'.
    R_MTART-LOW = S_MTART-MTART.
    APPEND R_MTART.
  ENDLOOP.
  SORT: R_MTART.
  DELETE ADJACENT DUPLICATES FROM R_MTART.
********************************************
  "test
*&----------------------------------------- Get Plant List for a given Company code

  " Commented for check

*  SELECT A~BWKEY
*         A~BUKRS
*         B~WERKS
*         B~REGIO
*    FROM T001K AS A
*    JOIN T001W AS B
*    ON A~BWKEY = B~BWKEY
*    INTO TABLE IT_T001KW
*    WHERE A~BUKRS = P_BUKRS.
*
*  IF SY-SUBRC = 0.
*    SORT IT_T001KW BY WERKS ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM IT_T001KW COMPARING WERKS.
*
**        IF P_WERKS IS NOT INITIAL.
**          DELETE IT_T001KW WHERE WERKS <> P_WERKS.
**        ENDIF.
*  ENDIF.



*&---------------------------------------- Get Material List for the plant
*  IF NOT IT_T001KW IS INITIAL.
*    SORT: IT_T001KW BY WERKS.
*
*    SELECT MATNR
*           WERKS
*      FROM MARC
*      INTO TABLE IT_MARC
*      FOR ALL ENTRIES IN IT_T001KW
*      WHERE WERKS = IT_T001KW-WERKS.
*  ENDIF.



*&-------------------------------------- Get Material Details For a Material
*  IF NOT IT_MARC IS INITIAL.
*    SORT IT_MARC BY MATNR.
*
*    SELECT MATNR
*           MTART
*      FROM MARA
*      INTO TABLE IT_MARA
*      FOR ALL ENTRIES IN IT_MARC
*      WHERE  MATNR = IT_MARC-MATNR AND
*             MTART IN R_MTART.
*
*    IF SY-SUBRC = 0.
*      SORT IT_MARA BY MATNR.
*    ELSE.
*      CLEAR IT_MARA.
*    ENDIF.
*  ENDIF.

  " Commented for check

*&-------------------------------------- Get VKORG from TVKO for the given Company code
  SELECT VKORG
         BUKRS
    FROM TVKO
    INTO TABLE IT_TVKO
    WHERE BUKRS = P_BUKRS.

  IF SY-SUBRC = 0.
    RANGES: R_VKORG FOR TVKO-VKORG.

    LOOP AT IT_TVKO INTO WA_TVKO.
      R_VKORG-SIGN = 'I'.
      R_VKORG-OPTION = 'EQ'.
      R_VKORG-LOW = WA_TVKO-VKORG.
      APPEND R_VKORG.
    ENDLOOP.
    SORT R_VKORG.
    DELETE ADJACENT DUPLICATES FROM R_VKORG.
  ENDIF.

*&-------------------------------------- get sales office details for a particular sales org. as it should be an input in the table
  SELECT VKORG
         VKBUR
    FROM TVKBZ
    INTO TABLE IT_VKBUR
    WHERE VKORG IN R_VKORG.

  IF SY-SUBRC = 0.
    SORT IT_VKBUR BY VKBUR ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_VKBUR COMPARING VKBUR.

    RANGES: R_VKBUR FOR TVKBZ-VKBUR.

    LOOP AT IT_VKBUR INTO WA_VKBUR.
      R_VKBUR-SIGN = 'I'.
      R_VKBUR-OPTION = 'EQ'.
      R_VKBUR-LOW = WA_VKBUR-VKBUR.
      APPEND R_VKBUR.
    ENDLOOP.
*        SORT R_VKBUR.
*        DELETE ADJACENT DUPLICATES FROM R_VKBUR.
  ENDIF.


*&------------------------------------------------------------- tax data retrieval
  "    STARTED BY RAM ON 1/7
  "IF NOT IT_MARA IS INITIAL.

  " Commented for check

*    SELECT  A~KAPPL
*            A~KSCHL
*            A~WKREG
*            A~REGIO
*            A~TAXK1
*            A~MATNR
*            A~KFRST
*            A~DATBI
*            A~DATAB
*            A~KBSTAT
*            A~KNUMH
*            C~ERDAT
*            B~KOPOS
**                B~STFKZ
*            B~KBETR
*            B~KONWA
*            B~KPEIN
*            B~KMEIN
*            B~LOEVM_KO
*       INTO TABLE IT_A709
*       FROM       A709 AS A
*      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH  AND A~KSCHL = C~KSCHL  )
*       INNER JOIN KONP AS B
*       ON         C~KNUMH = B~KNUMH
*       AND        C~KSCHL = B~KSCHL
*       FOR ALL ENTRIES IN IT_MARA
*       WHERE      MATNR = IT_MARA-MATNR
*       AND        A~KAPPL = 'V'
*       AND       ( A~KSCHL = 'JOSG' OR A~KSCHL = 'JOCG' OR A~KSCHL = 'JOIG' OR A~KSCHL = 'JOUG' ).
*       "AND        A~VKBUR IN R_VKBUR
*    "   AND A~REGIO = P_REGIO .
**     AND    C~ERDAT IN S_DATE.



*          SELECT  A~KAPPL
*            A~KSCHL
*            A~LAND1
*            A~VKORG
*            A~WKREG
*            A~TAXK1
*            A~REGIO
*            A~KFRST
*            A~DATBI
*            A~DATAB
*            A~KBSTAT
*            A~KNUMH
*            C~ERDAT
*            B~KOPOS
**                B~STFKZ
*            B~KBETR
*            B~KONWA
*            B~KPEIN
*            B~KMEIN
*            B~LOEVM_KO
*       INTO TABLE IT_A710
*       FROM       A710 AS A
*      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH  AND A~KSCHL = C~KSCHL  )
*       INNER JOIN KONP AS B
*       ON         C~KNUMH = B~KNUMH
*       AND        C~KSCHL = B~KSCHL
*
*      " FOR ALL ENTRIES IN IT_MARA
*      " WHERE      MATNR = IT_MARA-MATNR
*       WHERE      A~KAPPL = 'V'
*       AND       ( A~KSCHL = 'JOSG' OR A~KSCHL = 'JOCG' OR A~KSCHL = 'JOIG' OR A~KSCHL = 'JOUG' ).
*      "AND        A~VKBUR IN R_VKBUR
*     " AND A~REGIO = P_REGIO .
**     AND    C~ERDAT IN S_DATE.


  " ENDIF.

  " Commented for check

*&-------------------------------------- Data Retrieval - END -----------------------------------------------

*&------------------------------------- Data Processing - BEGIN -----------------------------------------------
*&-------------------------------------- Get GL account details
  SORT: IT_SKAT BY SAKNR.

  LOOP AT IT_SKB1 INTO WA_SKB1.
    WA_GL01-BUKRS = WA_SKB1-BUKRS.
    WA_GL01-SAKNR = WA_SKB1-SAKNR.

    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_SKB1-SAKNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_GL01-TXT20 = WA_SKAT-TXT20.
    ENDIF.
    APPEND WA_GL01 TO IT_GL01.
    CLEAR WA_GL01.
  ENDLOOP.

  " Commented for check
*  LOOP AT IT_A709 INTO WA_A709.
*    WA_TAX_MASTER-TABLE_NAME = 'A709'.
*    MOVE-CORRESPONDING WA_A709 TO WA_TAX_MASTER.
*    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
*    CLEAR: WA_TAX_MASTER, WA_A709.
*  ENDLOOP.

*    LOOP AT IT_A710 INTO WA_A710.
*    WA_TAX_MASTER-TABLE_NAME = 'A710'.
*    MOVE-CORRESPONDING WA_A710 TO WA_TAX_MASTER.
*    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
*    CLEAR: WA_TAX_MASTER, WA_A710.
*  ENDLOOP.

  SORT: IT_TAX_MASTER BY VKBUR KSCHL KNUMH.

*  IF NOT P_MATNR IS INITIAL.
*    DELETE IT_TAX_MASTER WHERE MATNR <> P_MATNR.
*  ENDIF.
*  IF NOT P_KUNNR IS INITIAL.
*    DELETE IT_TAX_MASTER WHERE KUNNR <> P_KUNNR.
*  ENDIF.
*  IF NOT P_VKBUR IS INITIAL.
*    DELETE IT_TAX_MASTER WHERE VKBUR <> P_VKBUR.
*  ENDIF.
*  IF NOT P_KSCHL IS INITIAL.
*    DELETE IT_TAX_MASTER WHERE KSCHL <> P_KSCHL.
*  ENDIF.
*  IF NOT P_REGIO IS INITIAL.
*    DELETE IT_TAX_MASTER WHERE REGIO NOT IN P_REGIO.
*  ENDIF.

*  IF NOT S_DATE[] IS INITIAL.
*  PERFORM GET_CDHDR_DETAILS   TABLES S_DATE
*                                     R_KNUMH
*                                     RETURN.
*    IF R_KNUMH[] IS NOT INITIAL.
*      DELETE IT_TAX_MASTER WHERE KNUMH NOT IN R_KNUMH.
*    ENDIF.
*  ELSE.
*
*    S_DATE-SIGN   = 'I'.
*    S_DATE-OPTION = 'EQ'.
*    S_DATE-LOW    = SY-DATUM.
*    APPEND S_DATE TO S_DATE.
*
*    CLEAR: S_DATE.
*
*    PERFORM GET_CDHDR_DETAILS   TABLES S_DATE
*                                   R_KNUMH
*                                   RETURN.
*    IF R_KNUMH[] IS NOT INITIAL.
*      DELETE IT_TAX_MASTER WHERE KNUMH NOT IN R_KNUMH.
*    ENDIF.
*  ENDIF.
*&------------------------------------- Data Processing - END -----------------------------------------------


*&-------------------------------------- Return Messages -------------------------------------------------

  PERFORM BAPI_MESSAGES TABLES RETURN
                               IT_TAX_MASTER
                               IT_TAX_TYPE
                               IT_COND_TYPE
                               IT_GL01.

  CLEAR: GV_INDEX_MSG, R_KNUMH[].

ENDFUNCTION.
