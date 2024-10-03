FUNCTION ZBAPI_SKU_GST_N_TAX_MASTER_SIS .
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
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE
*"      S_MTART TYPE  ZTT_CNF_MTART
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
"TEST
***************************************************************************************************
*&---------------------Condition Types and tables
*ZIVP - VAT - A843, A846
*ZIVC - CST - A844, A847
*JIVA - Additional Tax - A834, A831, A835
*ZVSR - VAT Surcharge - A845, A848


* A831 - Sales Off./Material
* A834 - Sales Off./Customer
* A835 - Sales Off.
* A843 - Sales Off./Country/Region/TaxCl6Cust
* A844 - Sales Off./Country/Region/TaxCl7Cust
* A845 - Sales Off./Country/Region/TaxCl8Cust
* A846 - Sales Off./Country/Region/TaxCl6Cust/Material
* A847 - Sales Off./Country/Region/TaxCl7Cust/Material
* A848 - Sales Off./Country/Region/TaxCl8Cust/Material
*****************************************************************************************************

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

         BEGIN OF TY_MARA,
           MATNR TYPE MARA-MATNR,
           MTART TYPE MARA-MTART,
         END OF TY_MARA,

         BEGIN OF TY_TVKBZ,
          VKORG TYPE TVKBZ-VKORG,
          VKBUR TYPE TVKBZ-VKBUR,
         END OF TY_TVKBZ,

      "Started By Ram on 1/7
         BEGIN OF TY_A709,
          KAPPL     TYPE     A709-KAPPL,
          KSCHL     TYPE     A709-KSCHL,
          WKREG     TYPE     A709-WKREG,
          "LAND1     TYPE     A709-LAND1,
          REGIO     TYPE     A709-REGIO,
          TAXK1     TYPE     A709-TAXK1,
          MATNR     TYPE     A709-MATNR,
          KFRST     TYPE     A709-KFRST,
          DATBI     TYPE     A709-DATBI,
          DATAB     TYPE     A709-DATAB,
          KBSTAT     TYPE    A709-KBSTAT,
          KNUMH     TYPE     A709-KNUMH,
          ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A709,

        BEGIN OF TY_A710,
          KAPPL     TYPE     A710-KAPPL,
          KSCHL     TYPE     A710-KSCHL,
          "WKREG     TYPE     A710-WKREG,
          LAND1     TYPE     A710-LAND1,
          VKORG     TYPE     A710-VKORG,
          WKREG     TYPE     A710-WKREG,
          TAXK1     TYPE     A710-TAXK1,
          REGIO     TYPE     A710-REGIO,
          KFRST     TYPE     A710-KFRST,
          DATBI     TYPE     A710-DATBI,
          DATAB     TYPE     A710-DATAB,
          KBSTAT     TYPE    A710-KBSTAT,
          KNUMH     TYPE     A710-KNUMH,
          ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A710.

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

  DATA: IT_T001KW   TYPE TABLE OF T_T001KW,
        WA_T001KW   TYPE          T_T001KW,
        IT_MARC     TYPE TABLE OF T_MARC,
        WA_MARC     TYPE          T_MARC,
        IT_MARA     TYPE TABLE OF TY_MARA,
        WA_MARA     TYPE          TY_MARA,
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


  DATA:   IT_A709    TYPE TABLE OF TY_A709,
          WA_A709    TYPE          TY_A709,
          IT_A710    TYPE TABLE OF TY_A710,
          WA_A710    TYPE          TY_A710.


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

*&----------------------------------------- Get Plant List for a given Company code
  SELECT A~BWKEY
         A~BUKRS
         B~WERKS
         B~REGIO
    FROM T001K AS A
    JOIN T001W AS B
    ON A~BWKEY = B~BWKEY
    INTO TABLE IT_T001KW
    WHERE A~BUKRS = P_BUKRS.

  IF SY-SUBRC = 0.
    SORT IT_T001KW BY WERKS ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_T001KW COMPARING WERKS.

*        IF P_WERKS IS NOT INITIAL.
*          DELETE IT_T001KW WHERE WERKS <> P_WERKS.
*        ENDIF.
  ENDIF.

*&---------------------------------------- Get Material List for the plant
  IF NOT IT_T001KW IS INITIAL.
    SORT: IT_T001KW BY WERKS.

    SELECT MATNR
           WERKS
      FROM MARC
      INTO TABLE IT_MARC
      FOR ALL ENTRIES IN IT_T001KW
      WHERE WERKS = IT_T001KW-WERKS.
  ENDIF.

*&-------------------------------------- Get Material Details For a Material
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
*&----------------------------------- Fetch A831 - Sales Off./Material
 SELECT  A~KAPPL
            A~KSCHL
            A~WKREG
            A~REGIO
            A~TAXK1
            A~MATNR
            A~KFRST
            A~DATBI
            A~DATAB
            A~KBSTAT
            A~KNUMH
            C~ERDAT
            B~KOPOS
*                B~STFKZ
            B~KBETR
            B~KONWA
            B~KPEIN
            B~KMEIN
            B~LOEVM_KO
       INTO TABLE IT_A709
       FROM       A709 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
       FOR ALL ENTRIES IN IT_MARA
       WHERE      MATNR = IT_MARA-MATNR
       AND        A~KAPPL = 'V'
       AND       ( A~KSCHL = 'JOSG' OR A~KSCHL = 'JOCG' OR A~KSCHL = 'JOIG' OR A~KSCHL = 'JOUG' ) .
      " AND        A~VKBUR IN R_VKBUR.
*     AND    C~ERDAT IN S_DATE.



          SELECT  A~KAPPL
            A~KSCHL
            A~LAND1
            A~VKORG
            A~WKREG
            A~TAXK1
            A~REGIO
            A~KFRST
            A~DATBI
            A~DATAB
            A~KBSTAT
            A~KNUMH
            C~ERDAT
            B~KOPOS
*                B~STFKZ
            B~KBETR
            B~KONWA
            B~KPEIN
            B~KMEIN
            B~LOEVM_KO
       INTO TABLE IT_A710
       FROM       A710 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
      " FOR ALL ENTRIES IN IT_MARA
      " WHERE      MATNR = IT_MARA-MATNR
       WHERE      A~KAPPL = 'V'
       AND       ( A~KSCHL = 'JOSG' OR A~KSCHL = 'JOCG' OR A~KSCHL = 'JOIG' OR A~KSCHL = 'JOUG' ) .
    "   AND        A~VKBUR IN R_VKBUR.
*     AND    C~ERDAT IN S_DATE.



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


  LOOP AT IT_A709 INTO WA_A709.
    WA_TAX_MASTER-TABLE_NAME = 'A709'.
    MOVE-CORRESPONDING WA_A709 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A709.
  ENDLOOP.

    LOOP AT IT_A710 INTO WA_A710.
    WA_TAX_MASTER-TABLE_NAME = 'A710'.
    MOVE-CORRESPONDING WA_A710 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A710.
  ENDLOOP.

  SORT: IT_TAX_MASTER BY VKBUR KSCHL KNUMH.

  IF NOT P_MATNR IS INITIAL.
    DELETE IT_TAX_MASTER WHERE MATNR <> P_MATNR.
  ENDIF.
  IF NOT P_KUNNR IS INITIAL.
    DELETE IT_TAX_MASTER WHERE KUNNR <> P_KUNNR.
  ENDIF.
  IF NOT P_VKBUR IS INITIAL.
    DELETE IT_TAX_MASTER WHERE VKBUR <> P_VKBUR.
  ENDIF.
  IF NOT P_KSCHL IS INITIAL.
    DELETE IT_TAX_MASTER WHERE KSCHL <> P_KSCHL.
  ENDIF.
  IF NOT S_DATE[] IS INITIAL.
    PERFORM GET_CDHDR_DETAILS   TABLES S_DATE
                                       R_KNUMH
                                       RETURN.
    IF R_KNUMH[] IS NOT INITIAL.
      DELETE IT_TAX_MASTER WHERE KNUMH NOT IN R_KNUMH.
    ENDIF.
  ELSE.

    S_DATE-SIGN   = 'I'.
    S_DATE-OPTION = 'EQ'.
    S_DATE-LOW    = sy-datum.
    APPEND S_DATE TO S_DATE.

    CLEAR: S_DATE.

        PERFORM GET_CDHDR_DETAILS   TABLES S_DATE
                                       R_KNUMH
                                       RETURN.
    IF R_KNUMH[] IS NOT INITIAL.
      DELETE IT_TAX_MASTER WHERE KNUMH NOT IN R_KNUMH.
    ENDIF.
  ENDIF.
*&------------------------------------- Data Processing - END -----------------------------------------------


*&-------------------------------------- Return Messages -------------------------------------------------

  PERFORM BAPI_MESSAGES TABLES RETURN
                               IT_TAX_MASTER
                               IT_TAX_TYPE
                               IT_COND_TYPE
                               IT_GL01.

  CLEAR: GV_INDEX_MSG, R_KNUMH[].

ENDFUNCTION.
