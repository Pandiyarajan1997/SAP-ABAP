FUNCTION ZBAPI_SKU_DISC_CN_MASTER .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_MATNR) TYPE  MARA-MATNR OPTIONAL
*"     VALUE(P_KUNNR) TYPE  KNA1-KUNNR OPTIONAL
*"     VALUE(P_VKBUR) TYPE  KNVV-VKBUR OPTIONAL
*"     VALUE(P_KSCHL) TYPE  KONH-KSCHL OPTIONAL
*"  TABLES
*"      IT_DISC_MASTER STRUCTURE  ZSTR_CNF_DISC_MASTER OPTIONAL
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE OPTIONAL
*"      S_MTART TYPE  ZTT_CNF_MTART OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------


********************************************************************************************************************************
*&----------------------------------------------- Discount Condtion --------------------------------------------------------
* Z004, Z005, ZPRB, ZBSD
* Z004 - Cash Discount-Mtl      - A005, A837, A832
* Z005 - Cash Discount-Slab     - A837, A832, A005
* Z006 - Cash Discount-Slab-Qty - A837, A832, A005
* ZPRB - Product Rebate         - A005, A837, A832
* ZBSD - Special Discount       - A005, A837, A832
********************************************************************************************************************************

*&-------------------------------------- DECLARATIONS - BEGIN -----------------------------------------------
  TYPES: BEGIN OF T_T001KW,
            BWKEY TYPE T001K-BWKEY,
            BUKRS TYPE T001K-BUKRS,
            WERKS TYPE T001W-WERKS,
           END OF T_T001KW.

  TYPES: BEGIN OF T_MARC,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
         END OF T_MARC.

  TYPES: BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
        END OF TY_MARA.

  TYPES: BEGIN OF TY_TVKO,
          VKORG TYPE TVKO-VKORG,
          BUKRS TYPE TVKO-BUKRS,
         END OF TY_TVKO.

  TYPES:   BEGIN OF TY_A005,
             KAPPL     TYPE     A005-KAPPL,
             KSCHL     TYPE     A005-KSCHL,
             VKORG     TYPE     A005-VKORG,
             VTWEG     TYPE     A005-VTWEG,
             KUNNR     TYPE     A005-KUNNR,
             MATNR     TYPE     A005-MATNR,
             DATBI     TYPE     A005-DATBI,
             DATAB     TYPE     A005-DATAB,
             KNUMH     TYPE     A005-KNUMH,
              ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
             KOPOS     TYPE     KONP-KOPOS,
             STFKZ     TYPE     KONP-STFKZ,
             KONMS     TYPE     KONP-KONMS,
             KONWS     TYPE     KONP-KONWS,
             KBETR     TYPE     KONP-KBETR,
             KONWA     TYPE     KONP-KONWA,

             KPEIN     TYPE     KONP-KPEIN,
             KMEIN     TYPE     KONP-KMEIN,
             MEINS     TYPE     KONP-MEINS,
             LOEVM_KO  TYPE     KONP-LOEVM_KO,
            END OF TY_A005,

            BEGIN OF TY_A832,
             KAPPL     TYPE     A832-KAPPL,
             KSCHL     TYPE     A832-KSCHL,
             REGIO     TYPE     A832-REGIO,
             MATNR     TYPE     A832-MATNR,
             KFRST     TYPE     A832-KFRST,
             DATBI     TYPE     A832-DATBI,
             DATAB     TYPE     A832-DATAB,
             KNUMH     TYPE     A832-KNUMH,
              ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
             KOPOS     TYPE     KONP-KOPOS,
             STFKZ     TYPE     KONP-STFKZ,
             KONMS     TYPE     KONP-KONMS,
             KONWS     TYPE     KONP-KONWS,
             KBETR     TYPE     KONP-KBETR,
             KONWA     TYPE     KONP-KONWA,
             KPEIN     TYPE     KONP-KPEIN,
             KMEIN     TYPE     KONP-KMEIN,
             MEINS     TYPE     KONP-MEINS,
             LOEVM_KO  TYPE     KONP-LOEVM_KO,
            END OF TY_A832,

            BEGIN OF TY_A837,
             KAPPL     TYPE     A837-KAPPL,
             KSCHL     TYPE     A837-KSCHL,
             VKBUR     TYPE     A837-VKBUR,
             MATNR     TYPE     A837-MATNR,
             KFRST     TYPE     A837-KFRST,
             DATBI     TYPE     A837-DATBI,
             DATAB     TYPE     A837-DATAB,
             KNUMH     TYPE     A837-KNUMH,
              ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
             KOPOS     TYPE     KONP-KOPOS,
             STFKZ     TYPE     KONP-STFKZ,
             KONMS     TYPE     KONP-KONMS,
             KONWS     TYPE     KONP-KONWS,
             KBETR     TYPE     KONP-KBETR,
             KONWA     TYPE     KONP-KONWA,
             KPEIN     TYPE     KONP-KPEIN,
             KMEIN     TYPE     KONP-KMEIN,
             MEINS     TYPE     KONP-MEINS,
             LOEVM_KO  TYPE     KONP-LOEVM_KO,
            END OF TY_A837,

            BEGIN OF TY_KONM,
             KNUMH     TYPE     KONM-KNUMH,
             KOPOS     TYPE     KONM-KOPOS,
             KLFN1     TYPE     KONM-KLFN1,
             KSTBM     TYPE     KONM-KSTBM,
             KBETR    TYPE     KONM-KBETR,
            END OF TY_KONM,

            BEGIN OF TY_KONW,
             KNUMH     TYPE     KONW-KNUMH,
             KOPOS     TYPE     KONW-KOPOS,
             KLFN1     TYPE     KONW-KLFN1,
             KSTBW     TYPE     KONW-KSTBW,
             KBETR     TYPE     KONW-KBETR,
            END OF TY_KONW,

           BEGIN OF TY1_KONP,
             KNUMH     TYPE     KONP-KNUMH,
             KOPOS     TYPE     KONP-KOPOS,
             "KLFN1     TYPE     KON-KLFN1,
             KSTBW     TYPE     KONP-KSTBW,
             KBETR     TYPE     KONP-KBETR,
            END OF TY1_KONP.

*            BEGIN OF TY_KONP,
*             KNUMH     TYPE     KONP-KNUMH,
*             KOPOS     TYPE     KONP-KOPOS,
*           "  KLFN1     TYPE     KONP-KLFN1,
*             KSTBW     TYPE     KONP-KSTBW,
*             KBETR     TYPE     KONP-KBETR,
*            END OF TY_KONP.

  TYPES: BEGIN OF TY_TVKBZ,
          VKORG TYPE TVKBZ-VKORG,
          VKBUR TYPE TVKBZ-VKBUR,
         END OF TY_TVKBZ.

  TYPES:  BEGIN OF TY_T001,
            BUKRS TYPE T001-BUKRS,
            BUTXT TYPE T001-BUTXT,
          END OF TY_T001.

  DATA: IT_T001KW TYPE TABLE OF T_T001KW,
        WA_T001KW TYPE T_T001KW.

  DATA: IT_MARC TYPE TABLE OF T_MARC,
        WA_MARC TYPE T_MARC.

  DATA: IT_MARA TYPE TABLE OF TY_MARA,
        WA_MARA TYPE TY_MARA.

  DATA: IT_TVKO TYPE TABLE OF TY_TVKO,
        WA_TVKO TYPE TY_TVKO.

  DATA : IT_VKBUR TYPE TABLE OF TY_TVKBZ,
           WA_VKBUR TYPE TY_TVKBZ.

  DATA : IT_A005    TYPE TABLE OF TY_A005,
         WA_A005    TYPE          TY_A005,
         IT_A832    TYPE TABLE OF TY_A832,
         WA_A832    TYPE          TY_A832,
         IT_A837    TYPE TABLE OF TY_A837,
         WA_A837    TYPE          TY_A837,
         IT_KONM  TYPE TABLE OF TY_KONM,
         WA_KONM   TYPE          TY_KONM,
         IT_KONW TYPE TABLE OF TY_KONW,
         WA_KONW  TYPE         TY_KONW,
         IT1_KONP TYPE TABLE OF TY1_KONP,
         WA1_KONP  TYPE         TY1_KONP.

  DATA: WA_RETURN TYPE BAPIRET2,
        WA_DISC_MASTER TYPE ZSTR_CNF_DISC_MASTER,
        IT_DISC_TEMP TYPE TABLE OF ZSTR_CNF_DISC_MASTER,
        WA_DISC_TEMP TYPE ZSTR_CNF_DISC_MASTER,
        LV_COUNT TYPE I,
        IT_T001     TYPE TABLE OF TY_T001.
*&-------------------------------------- DECLARATIONS - END -------------------------------------------------


*&----------------------------------------- FETCH Data - BEGIN ------------------------------------------------
*&-------------------------------------- Check If the given company code exists
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

*&-------------------------------------- Get MTART in a single internal table
  RANGES: R_MTART FOR MARA-MTART.

  LOOP AT S_MTART.
    R_MTART-SIGN = 'I'.
    R_MTART-OPTION = 'EQ'.
    R_MTART-LOW = S_MTART-MTART.
    APPEND R_MTART.
  ENDLOOP.

*&------------------------------------ Get Plant List for a given Company code
  SELECT A~BWKEY
         A~BUKRS
         B~WERKS
    FROM T001K AS A
    JOIN T001W AS B
    ON A~BWKEY = B~BWKEY
    INTO TABLE IT_T001KW
    WHERE A~BUKRS = P_BUKRS.

*  IF P_WERKS IS NOT INITIAL.
*    DELETE IT_T001KW WHERE WERKS NE P_WERKS.
*  ENDIF.

*&------------------------------------ Get Material List for the plant
  IF NOT IT_T001KW IS INITIAL.
    SORT: IT_T001KW BY WERKS.

    SELECT MATNR
           WERKS
      FROM MARC
      INTO TABLE IT_MARC
      FOR ALL ENTRIES IN IT_T001KW
      WHERE WERKS = IT_T001KW-WERKS.

    IF NOT P_MATNR IS INITIAL.
      DELETE IT_MARC WHERE MATNR <> P_MATNR.
    ENDIF.
  ENDIF.

*&------------------------------------ Get Material Details For a Material
  IF NOT IT_MARC IS INITIAL.
    SORT IT_MARC BY MATNR.

    SELECT MATNR
           MTART
      FROM MARA
      INTO TABLE IT_MARA
      FOR ALL ENTRIES IN IT_MARC
      WHERE  MATNR = IT_MARC-MATNR AND
             MTART IN R_MTART.

    IF SY-SUBRC = 0.
      SORT IT_MARA BY MATNR.
    ELSE.
      CLEAR IT_MARA.
    ENDIF.
  ENDIF.

*&------------------------------------ Get VKORG from TVKO for the given Company code
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


*&-------------------------------------------------------- discount master (A005, A832, A837)
*--------------------------------------- Fetch A005 - Customer/Material
  IF IT_MARA  IS NOT INITIAL.
    TYPES: BEGIN OF TY_KONP,           "A005 is a pooled table. so join wont work. have to use for all entries and then join the tables KONP with a005
              KNUMH TYPE KONP-KNUMH,
              KSCHL TYPE KONP-KSCHL,
              KOPOS TYPE KONP-KOPOS,
              STFKZ TYPE KONP-STFKZ,
            "  KSTBM TYPE KONP-KSTBM,
              KONMS TYPE KONP-KONMS,
              KONWS TYPE KONP-KONWS,
              "KSTBW TYPE KONP-KSTBW,
              KBETR TYPE KONP-KBETR,
              KONWA TYPE KONP-KONWA,
              KPEIN TYPE KONP-KPEIN,
              KMEIN TYPE KONP-KMEIN,
              MEINS TYPE KONP-MEINS,
              LOEVM_KO TYPE KONP-LOEVM_KO,
                ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
           END OF TY_KONP,

           BEGIN OF TY_A005_TEMP,
               KAPPL     TYPE     A005-KAPPL,
               KSCHL     TYPE     A005-KSCHL,
               VKORG     TYPE     A005-VKORG,
               VTWEG     TYPE     A005-VTWEG,
               KUNNR     TYPE     A005-KUNNR,
               MATNR     TYPE     A005-MATNR,
               DATBI     TYPE     A005-DATBI,
               DATAB     TYPE     A005-DATAB,
               KNUMH     TYPE     A005-KNUMH,
           END OF TY_A005_TEMP.

    DATA:  IT_KONP TYPE TABLE OF TY_KONP,
           WA_KONP TYPE TY_KONP,
           IT_A005_TEMP TYPE TABLE OF TY_A005_TEMP,
           IT_A005_TEMP1 TYPE TABLE OF TY_A005_TEMP,
           WA_A005_TEMP TYPE  TY_A005_TEMP.

    SELECT KAPPL
           KSCHL
           VKORG
           VTWEG
           KUNNR
           MATNR
           DATBI
           DATAB
           KNUMH
        INTO CORRESPONDING FIELDS OF TABLE IT_A005_TEMP1
        FROM A005
*        FOR ALL ENTRIES IN IT_MARA
        WHERE KAPPL = 'V'
        AND   KSCHL IN ('ZPRV' , 'ZPRP' )
        AND   VKORG IN R_VKORG ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
*        AND   MATNR  = IT_MARA-MATNR.

    IF SY-SUBRC = 0.

      SORT IT_A005_TEMP1 BY MATNR.
      SORT IT_MARA BY MATNR.

      IF P_MATNR IS NOT INITIAL.
        DELETE IT_A005_TEMP1 WHERE MATNR <> P_MATNR.
      ENDIF.

      LOOP AT IT_A005_TEMP1 INTO WA_A005_TEMP.
        READ TABLE IT_MARA TRANSPORTING NO FIELDS WITH KEY MATNR = WA_A005_TEMP-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          APPEND WA_A005_TEMP TO IT_A005_TEMP.
        ENDIF.
      ENDLOOP.

      IF IT_A005_TEMP[] IS NOT INITIAL.
        SELECT A~KNUMH
               A~KSCHL
               A~KOPOS
               A~STFKZ
               A~KONMS
               A~KONWS
               A~KBETR
               A~KONWA
               A~KPEIN
               A~KMEIN
               A~MEINS
               A~LOEVM_KO
                B~ERDAT
          FROM KONP AS A
          INNER JOIN KONH AS B "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          ON A~KNUMH = B~KNUMH
          INTO TABLE IT_KONP
          FOR ALL ENTRIES IN IT_A005_TEMP
          WHERE A~KNUMH = IT_A005_TEMP-KNUMH
          AND   A~KSCHL = IT_A005_TEMP-KSCHL.
*          AND    B~ERDAT IN S_DATE.
      ENDIF.
    ENDIF.

    "*** get sales office list for a particular sales org. as it should be an input in the table
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
      SORT R_VKBUR.
      DELETE ADJACENT DUPLICATES FROM R_VKBUR.
    ENDIF.

*--------------------------------------- Fetch A832 - Cust.Region/Material
    SELECT A~KAPPL
           A~KSCHL
           A~REGIO
           A~MATNR
           A~KFRST
           A~DATBI
           A~DATAB
           A~KNUMH
            C~ERDAT
           B~KOPOS
           B~STFKZ
           B~KONMS
           B~KONWS
           B~KBETR
           B~KONWA
           B~KPEIN
           B~KMEIN
           B~MEINS
           B~LOEVM_KO
      INTO CORRESPONDING FIELDS  OF TABLE IT_A832
      FROM A832 AS A
      INNER JOIN KONH AS C ON A~KNUMH = C~KNUMH "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
      INNER JOIN KONP AS B
      ON C~KNUMH = B~KNUMH
      AND C~KSCHL = B~KSCHL

      FOR ALL ENTRIES IN IT_MARA
      WHERE A~KAPPL = 'V'
     "  AND  A~KSCHL IN ('Z004', 'Z005', 'Z006', 'ZPRB', 'ZBSD', 'ZPRV','ZPRP' )
      AND  A~KSCHL IN ('ZPRV','ZPRP' )
      AND   A~MATNR  = IT_MARA-MATNR.
*      AND    C~ERDAT IN S_DATE.

*--------------------------------------- Fetch A837 - Sales Office/Material
    SELECT A~KAPPL
           A~KSCHL
           A~VKBUR
           A~MATNR
           A~KFRST
           A~DATBI
           A~DATAB
           A~KNUMH
            C~ERDAT
           B~KOPOS
           B~STFKZ
           B~KONMS
           B~KONWS
           B~KBETR
           B~KONWA
           B~KPEIN
           B~KMEIN
           B~MEINS
           B~LOEVM_KO
      INTO CORRESPONDING FIELDS  OF TABLE IT_A837
      FROM A837 AS A
      INNER JOIN KONH AS C ON A~KNUMH = C~KNUMH "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
      INNER JOIN KONP AS B
      ON C~KNUMH = B~KNUMH
      AND C~KSCHL = B~KSCHL

      FOR ALL ENTRIES IN IT_MARA
      WHERE A~KAPPL = 'V'
    "   AND  A~KSCHL IN ('Z004', 'Z005', 'Z006', 'ZPRB', 'ZBSD' , 'ZPRV' , 'ZPRP')
      AND  A~KSCHL IN ('ZPRV' , 'ZPRP')
      AND   A~MATNR  = IT_MARA-MATNR
      AND   A~VKBUR IN R_VKBUR.
*      AND    C~ERDAT IN S_DATE.

  ENDIF.
*&----------------------------------------- FETCH Data - END ------------------------------------------------


*&------------------------------------------ DATA AGGREGATION- BEGIN ------------------------------------------

  SORT IT_A005_TEMP BY KNUMH.
  SORT IT_KONP BY KNUMH.

  LOOP AT IT_A005_TEMP INTO WA_A005_TEMP.
    READ TABLE IT_KONP TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_A005_TEMP-KNUMH BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.

      LOOP AT IT_KONP INTO WA_KONP FROM LV_INDEX.
        IF WA_KONP-KNUMH = WA_A005_TEMP-KNUMH.
          MOVE-CORRESPONDING: WA_A005_TEMP TO WA_A005,
                              WA_KONP TO WA_A005.
          APPEND WA_A005 TO IT_A005.
          CLEAR: WA_A005, WA_KONP.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDLOOP.


  LOOP AT IT_A005 INTO WA_A005.
    CASE WA_A005-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING WA_A005 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING WA_A005 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'ZPRV'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING WA_A005 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'ZPRP'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING WA_A005 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING WA_A005 TO WA_DISC_MASTER.
        APPEND: WA_DISC_MASTER TO IT_DISC_MASTER.

    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, WA_A005.
  ENDLOOP.


  LOOP AT IT_A832 INTO WA_A832.

    CASE WA_A832-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING WA_A832 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING WA_A832 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'ZPRV'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING WA_A832 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'ZPRP'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING WA_A832 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING WA_A832 TO WA_DISC_MASTER.
        APPEND: WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, WA_A832.
  ENDLOOP.

  LOOP AT IT_A837 INTO WA_A837.

    CASE WA_A837-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING WA_A837 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING WA_A837 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
     WHEN 'ZPRV'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING WA_A837 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'ZPRP'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING WA_A837 TO WA_DISC_TEMP.
        APPEND WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING WA_A837 TO WA_DISC_MASTER.
        APPEND: WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, WA_A837.
  ENDLOOP.

  READ TABLE IT_DISC_TEMP TRANSPORTING NO FIELDS INDEX 1.
  IF SY-SUBRC = 0.
    SORT: IT_DISC_TEMP BY KNUMH.

    SELECT KNUMH
           KOPOS
           KLFN1
           KSTBM
           KBETR
           FROM KONM
           INTO TABLE IT_KONM
           FOR ALL ENTRIES IN IT_DISC_TEMP
           WHERE KNUMH = IT_DISC_TEMP-KNUMH.
    IF SY-SUBRC = 0.
      SORT IT_KONM BY KNUMH.
    ELSE.
      REFRESH IT_KONM.
    ENDIF.

    SELECT KNUMH
           KOPOS
           KLFN1
           KSTBW
           KBETR
           FROM KONW
           INTO TABLE IT_KONW
           FOR ALL ENTRIES IN IT_DISC_TEMP
           WHERE KNUMH = IT_DISC_TEMP-KNUMH.
    IF SY-SUBRC = 0.
      SORT IT_KONW BY KNUMH.
    ELSE.
      REFRESH IT_KONW.

    ENDIF.


        SELECT KNUMH
           KOPOS
        "   KLFN1
           KSTBW
           KBETR
           FROM KONP
           INTO TABLE IT1_KONP
           FOR ALL ENTRIES IN IT_DISC_TEMP
           WHERE KNUMH = IT_DISC_TEMP-KNUMH AND KONMS EQ ' ' .
    IF SY-SUBRC = 0.
      SORT IT_KONP BY KNUMH.
    ELSE.
      REFRESH IT_KONP.
    ENDIF.

  ENDIF.

  LOOP AT IT_DISC_TEMP INTO WA_DISC_TEMP.

    MOVE-CORRESPONDING WA_DISC_TEMP TO WA_DISC_MASTER.

    READ TABLE IT_KONM TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      CLEAR LV_COUNT.

      LOOP AT IT_KONM INTO WA_KONM FROM LV_INDEX.
        IF WA_KONM-KNUMH = WA_DISC_TEMP-KNUMH.
          LV_COUNT = LV_COUNT + 1.

          WA_DISC_MASTER-KNUMH    = WA_KONM-KNUMH.
          WA_DISC_MASTER-KOPOS    = WA_KONM-KOPOS.
          WA_DISC_MASTER-KBETR    = WA_KONM-KBETR.              " condition value
          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
          WA_DISC_MASTER-SCALE_VALUE = WA_KONM-KSTBM.           " quantity scale
        "  WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONMS.      " quantity unit
           WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KMEIN .
          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE IT_KONW TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      CLEAR LV_COUNT.

      LOOP AT IT_KONW INTO WA_KONW FROM LV_INDEX.
        IF WA_KONW-KNUMH = WA_DISC_TEMP-KNUMH.
          LV_COUNT = LV_COUNT + 1.

          WA_DISC_MASTER-KNUMH    = WA_KONW-KNUMH.
          WA_DISC_MASTER-KOPOS    = WA_KONW-KOPOS.
          WA_DISC_MASTER-KBETR    = WA_KONW-KBETR.              " condition value
          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
          WA_DISC_MASTER-SCALE_VALUE = WA_KONW-KSTBW.           " quantity scale
          WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONWS.      " quantity unit

          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.


        READ TABLE IT1_KONP TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      CLEAR LV_COUNT.

      LOOP AT IT1_KONP INTO WA1_KONP FROM LV_INDEX. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
        IF WA1_KONP-KNUMH = WA_DISC_TEMP-KNUMH.
          LV_COUNT = LV_COUNT + 1.

          WA_DISC_MASTER-KNUMH    = WA1_KONP-KNUMH.
          WA_DISC_MASTER-KOPOS    = WA1_KONP-KOPOS.
          WA_DISC_MASTER-KBETR    = WA1_KONP-KBETR.              " condition value
          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
          WA_DISC_MASTER-SCALE_VALUE = WA1_KONP-KSTBW.           " quantity scale
          WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONWS.      " quantity unit

          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
        ELSE.
          EXIT. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

 " DELETE ADJACENT DUPLICATES FROM IT_DISC_MASTER COMPARING KNUMH KBETR.
*BREAK-POINT.
*  LOOP AT IT_DISC_TEMP INTO WA_DISC_TEMP.
*
*    MOVE-CORRESPONDING WA_DISC_TEMP TO WA_DISC_MASTER.
*
*    READ TABLE IT_KONP TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*      CLEAR LV_COUNT.
*
*      LOOP AT IT_KONP INTO WA_KONP FROM LV_INDEX.
*        IF WA_KONP-KNUMH = WA_DISC_TEMP-KNUMH.
*          LV_COUNT = LV_COUNT + 1.
*
*          WA_DISC_MASTER-KNUMH    = WA_KONP-KNUMH.
*          WA_DISC_MASTER-KOPOS    = WA_KONP-KOPOS.
*          WA_DISC_MASTER-KBETR    = WA_KONP-KBETR.              " condition value
*          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
*          WA_DISC_MASTER-SCALE_VALUE = WA_KONP-KSTBM.           " quantity scale
*        "  WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONMS.      " quantity unit
*           WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KMEIN .
*          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    READ TABLE IT_KONP TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*      CLEAR LV_COUNT.
*
*      LOOP AT IT_KONP INTO WA_KONP FROM LV_INDEX.
*        IF WA_KONP-KNUMH = WA_DISC_TEMP-KNUMH.
*          LV_COUNT = LV_COUNT + 1.
*
*          WA_DISC_MASTER-KNUMH    = WA_KONP-KNUMH.
*          WA_DISC_MASTER-KOPOS    = WA_KONP-KOPOS.
*          WA_DISC_MASTER-KBETR    = WA_KONP-KBETR.              " condition value
*          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
*          WA_DISC_MASTER-SCALE_VALUE = WA_KONP-KSTBW.           " quantity scale
*          WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONWS.      " quantity unit
*
*          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*  ENDLOOP.

  IF P_KUNNR IS NOT INITIAL.
    DELETE IT_DISC_MASTER WHERE KUNNR <> P_KUNNR.
  ENDIF.
  IF P_VKBUR IS NOT INITIAL.
    DELETE IT_DISC_MASTER WHERE VKBUR <> P_VKBUR.
  ENDIF.
  IF P_KSCHL IS NOT INITIAL.
    DELETE IT_DISC_MASTER WHERE KSCHL <> P_KSCHL.
  ENDIF.
  IF NOT S_DATE[] IS INITIAL.
    PERFORM GET_CDHDR_DETAILS   TABLES S_DATE
                                       R_KNUMH
                                       RETURN.
    IF R_KNUMH[] IS NOT INITIAL.
      DELETE IT_DISC_MASTER WHERE KNUMH NOT IN R_KNUMH.
    ENDIF.
  ENDIF.

*&------------------------------------------ DATA AGGREGATION- END ------------------------------------------


*&------------------------------------ BAPI Return Messages - BEGIN --------------------------------------------

  READ TABLE IT_DISC_MASTER TRANSPORTING NO FIELDS INDEX 1.
  IF SY-SUBRC = 0.
    SORT IT_DISC_MASTER BY KSCHL MATNR KNUMH.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'S'   'Success!!! - Discount master is updated!!!'.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Discount master is not updated!!!'.
  ENDIF.
*&------------------------------------ BAPI Return Messages - BEGIN --------------------------------------------

  CLEAR: GV_INDEX_MSG, LV_INDEX, R_KNUMH[].


ENDFUNCTION.
