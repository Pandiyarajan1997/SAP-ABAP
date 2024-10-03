FUNCTION ZBAPI_SKU_TAX_MASTER .
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

         BEGIN OF TY_A831,
*          KAPPL     TYPE     A831-KAPPL,
*          KSCHL     TYPE     A831-KSCHL,
*          VKBUR     TYPE     A831-VKBUR,
*          MATNR     TYPE     A831-MATNR,
*          KFRST     TYPE     A831-KFRST,
*          DATBI     TYPE     A831-DATBI,
*          DATAB     TYPE     A831-DATAB,
*          KNUMH     TYPE     A831-KNUMH,
          KAPPL     TYPE     A929-KAPPL,
          KSCHL     TYPE     A929-KSCHL,
          LAND1    TYPE      A929-ALAND,
          VKBUR     TYPE     A929-VKBUR,
          MATNR     TYPE     A929-MATNR,
          KFRST     TYPE     A929-KFRST,
          DATBI     TYPE     A929-DATBI,
          DATAB     TYPE     A929-DATAB,
          KNUMH     TYPE     A929-KNUMH,
            ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A831,

         BEGIN OF TY_A834,
*          KAPPL     TYPE     A834-KAPPL,
*          KSCHL     TYPE     A834-KSCHL,
*          VKBUR     TYPE     A834-VKBUR,
*          KUNNR     TYPE     A834-KUNNR,
*          KFRST     TYPE     A834-KFRST,
*          DATBI     TYPE     A834-DATBI,
*          DATAB     TYPE     A834-DATAB,
*          KNUMH     TYPE     A834-KNUMH,

          KAPPL     TYPE     A928-KAPPL,
          KSCHL     TYPE     A928-KSCHL,
          LAND1     TYPE    A928-ALAND,
          VKBUR     TYPE     A928-VKBUR,
          KUNNR     TYPE     A928-KUNNR,
          KFRST     TYPE     A928-KFRST,
          DATBI     TYPE     A928-DATBI,
          DATAB     TYPE     A928-DATAB,
          KNUMH     TYPE     A928-KNUMH,

           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A834,

         BEGIN OF TY_A835,
*          KAPPL     TYPE     A835-KAPPL,
*          KSCHL     TYPE     A835-KSCHL,
*          VKBUR     TYPE     A835-VKBUR,
*          KFRST     TYPE     A835-KFRST,
*          DATBI     TYPE     A835-DATBI,
*          DATAB     TYPE     A835-DATAB,
*          KNUMH     TYPE     A835-KNUMH,

          KAPPL     TYPE     A930-KAPPL,
          KSCHL     TYPE     A930-KSCHL,
          LAND1 TYPE       A930-ALAND,
          VKBUR     TYPE     A930-VKBUR,
          KFRST     TYPE     A930-KFRST,
          DATBI     TYPE     A930-DATBI,
          DATAB     TYPE     A930-DATAB,
          KNUMH     TYPE     A930-KNUMH,

           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A835,

         BEGIN OF TY_A843,
          KAPPL     TYPE     A843-KAPPL,
          KSCHL     TYPE     A843-KSCHL,
          VKBUR     TYPE     A843-VKBUR,
          LAND1     TYPE     A843-LAND1,
          REGIO     TYPE     A843-REGIO,
          TAXK6     TYPE     A843-TAXK6,
          KFRST     TYPE     A843-KFRST,
          DATBI     TYPE     A843-DATBI,
          DATAB     TYPE     A843-DATAB,
          KNUMH     TYPE     A843-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A843,

         BEGIN OF TY_A844,
          KAPPL     TYPE     A844-KAPPL,
          KSCHL     TYPE     A844-KSCHL,
          VKBUR     TYPE     A844-VKBUR,
          LAND1     TYPE     A844-LAND1,
          REGIO     TYPE     A844-REGIO,
          TAXK7     TYPE     A844-TAXK7,
          KFRST     TYPE     A844-KFRST,
          DATBI     TYPE     A844-DATBI,
          DATAB     TYPE     A844-DATAB,
          KNUMH     TYPE     A844-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A844,

         BEGIN OF TY_A845,
          KAPPL     TYPE     A845-KAPPL,
          KSCHL     TYPE     A845-KSCHL,
          VKBUR     TYPE     A845-VKBUR,
          LAND1     TYPE     A845-LAND1,
          REGIO     TYPE     A845-REGIO,
          TAXK8     TYPE     A845-TAXK8,
          KFRST     TYPE     A845-KFRST,
          DATBI     TYPE     A845-DATBI,
          DATAB     TYPE     A845-DATAB,
          KNUMH     TYPE     A845-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A845,

         BEGIN OF TY_A846,
          KAPPL     TYPE     A846-KAPPL,
          KSCHL     TYPE     A846-KSCHL,
          VKBUR     TYPE     A846-VKBUR,
          LAND1     TYPE     A846-LAND1,
          REGIO     TYPE     A846-REGIO,
          TAXK6     TYPE     A846-TAXK6,
          MATNR     TYPE     A846-MATNR,
          KFRST     TYPE     A846-KFRST,
          DATBI     TYPE     A846-DATBI,
          DATAB     TYPE     A846-DATAB,
          KNUMH     TYPE     A846-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A846,

         BEGIN OF TY_A847,
          KAPPL     TYPE     A847-KAPPL,
          KSCHL     TYPE     A847-KSCHL,
          VKBUR     TYPE     A847-VKBUR,
          LAND1     TYPE     A847-LAND1,
          REGIO     TYPE     A847-REGIO,
          TAXK7     TYPE     A847-TAXK7,
          MATNR     TYPE     A847-MATNR,
          KFRST     TYPE     A847-KFRST,
          DATBI     TYPE     A847-DATBI,
          DATAB     TYPE     A847-DATAB,
          KNUMH     TYPE     A847-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A847,

         BEGIN OF TY_A848,
          KAPPL     TYPE     A848-KAPPL,
          KSCHL     TYPE     A848-KSCHL,
          VKBUR     TYPE     A848-VKBUR,
          LAND1     TYPE     A848-LAND1,
          REGIO     TYPE     A848-REGIO,
          TAXK8     TYPE     A848-TAXK8,
          MATNR     TYPE     A848-MATNR,
          KFRST     TYPE     A848-KFRST,
          DATBI     TYPE     A848-DATBI,
          DATAB     TYPE     A848-DATAB,
          KNUMH     TYPE     A848-KNUMH,
           ERDAT     TYPE     KONH-ERDAT, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
          KOPOS     TYPE     KONP-KOPOS,
*          STFKZ     TYPE     KONP-STFKZ,
          KBETR     TYPE     KONP-KBETR,
          KONWA     TYPE     KONP-KONWA,
          KPEIN     TYPE     KONP-KPEIN,
          KMEIN     TYPE     KONP-KMEIN,
          LOEVM_KO  TYPE     KONP-LOEVM_KO,
         END OF TY_A848.

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


  DATA:   IT_A831    TYPE TABLE OF TY_A831,
          WA_A831    TYPE          TY_A831,
          IT_A834    TYPE TABLE OF TY_A834,
          WA_A834    TYPE          TY_A834,
          IT_A835    TYPE TABLE OF TY_A835,
          WA_A835    TYPE          TY_A835,
          IT_A843    TYPE TABLE OF TY_A843,
          WA_A843    TYPE          TY_A843,
          IT_A844    TYPE TABLE OF TY_A844,
          WA_A844    TYPE          TY_A844,
          IT_A845    TYPE TABLE OF TY_A845,
          WA_A845    TYPE          TY_A845,
          IT_A846    TYPE TABLE OF TY_A846,
          WA_A846    TYPE          TY_A846,
          IT_A847    TYPE TABLE OF TY_A847,
          WA_A847    TYPE          TY_A847,
          IT_A848    TYPE TABLE OF TY_A848,
          WA_A848    TYPE          TY_A848.
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
  IF NOT IT_MARA IS INITIAL.
    SELECT A~KAPPL
           A~KSCHL
           A~ALAND
           A~VKBUR
           A~MATNR
           A~KFRST
           A~DATBI
           A~DATAB
           A~KNUMH
            C~ERDAT
           B~KOPOS
*               B~STFKZ
           B~KBETR
           B~KONWA
           B~KPEIN
           B~KMEIN
           B~LOEVM_KO
       INTO TABLE IT_A831
     "  FROM       A831 AS A
        FROM       A929 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
       FOR ALL ENTRIES IN IT_MARA
       WHERE      A~MATNR = IT_MARA-MATNR
       AND        A~KAPPL = 'V'
       AND        A~KSCHL = 'JIVA'
       AND        A~VKBUR IN R_VKBUR.
*      AND    C~ERDAT IN S_DATE.
  ENDIF.

*&----------------------------------- Fetch A834 - Sales Off./Customer
  SELECT A~KAPPL
         A~KSCHL
         A~ALAND
         A~VKBUR
         A~KUNNR
         A~KFRST
         A~DATBI
         A~DATAB
         A~KNUMH
          C~ERDAT
         B~KOPOS
*               B~STFKZ
         B~KBETR
         B~KONWA
         B~KPEIN
         B~KMEIN
         B~LOEVM_KO
    " INTO TABLE IT_A834
     INTO TABLE IT_A834
     FROM       A928 AS A
    INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
     INNER JOIN KONP AS B
     ON         C~KNUMH = B~KNUMH
     AND        C~KSCHL = B~KSCHL
     WHERE      A~KAPPL = 'V'
     AND        A~KSCHL IN ('JIVA', 'ZIVP', 'ZIVC')
     AND        A~VKBUR IN R_VKBUR.
*    AND    C~ERDAT IN S_DATE.


*&----------------------------------- Fetch A835 - Sales Off.
  SELECT  A~KAPPL
          A~KSCHL
          A~ALAND
          A~VKBUR
          A~KFRST
          A~DATBI
          A~DATAB
          A~KNUMH
            C~ERDAT
          B~KOPOS
*                B~STFKZ
          B~KBETR
          B~KONWA
          B~KPEIN
          B~KMEIN
          B~LOEVM_KO
     INTO TABLE IT_A835
    " FROM       A835 AS A
    FROM       A930 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
     INNER JOIN KONP AS B
     ON         C~KNUMH = B~KNUMH
     AND        C~KSCHL = B~KSCHL
     WHERE      A~KAPPL = 'V'
     AND        A~KSCHL = 'JIVA'
     AND        A~VKBUR IN R_VKBUR.
*    AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A843 - Sales Off./Country/Region/TaxCl6Cust
  SELECT  A~KAPPL
          A~KSCHL
          A~VKBUR
          A~LAND1
          A~REGIO
          A~TAXK6
          A~KFRST
          A~DATBI
          A~DATAB
          A~KNUMH
            C~ERDAT
          B~KOPOS
*                B~STFKZ
          B~KBETR
          B~KONWA
          B~KPEIN
          B~KMEIN
          B~LOEVM_KO
     INTO TABLE IT_A843
     FROM       A843 AS A
    INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
     INNER JOIN KONP AS B
     ON         C~KNUMH = B~KNUMH
     AND        C~KSCHL = B~KSCHL
     WHERE      A~KAPPL = 'V'
     AND        A~KSCHL = 'ZIVP'
     AND        A~VKBUR IN R_VKBUR.
*    AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A844 - Sales Off./Country/Region/TaxCl7Cust
  SELECT  A~KAPPL
          A~KSCHL
          A~VKBUR
          A~LAND1
          A~REGIO
          A~TAXK7
          A~KFRST
          A~DATBI
          A~DATAB
          A~KNUMH
            C~ERDAT
          B~KOPOS
*                B~STFKZ
          B~KBETR
          B~KONWA
          B~KPEIN
          B~KMEIN
          B~LOEVM_KO
     INTO TABLE IT_A844
     FROM       A844 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
     INNER JOIN KONP AS B
     ON         C~KNUMH = B~KNUMH
     AND        C~KSCHL = B~KSCHL
     WHERE      A~KAPPL = 'V'
     AND        A~KSCHL = 'ZIVC'
     AND        A~VKBUR IN R_VKBUR.
*    AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A845 - Sales Off./Country/Region/TaxCl8Cust
  SELECT  A~KAPPL
          A~KSCHL
          A~VKBUR
          A~LAND1
          A~REGIO
          A~TAXK8
          A~KFRST
          A~DATBI
          A~DATAB
          A~KNUMH
            C~ERDAT
          B~KOPOS
*                B~STFKZ
          B~KBETR
          B~KONWA
          B~KPEIN
          B~KMEIN
          B~LOEVM_KO
     INTO TABLE IT_A845
     FROM       A845 AS A
    INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
     INNER JOIN KONP AS B
     ON         C~KNUMH = B~KNUMH
     AND        C~KSCHL = B~KSCHL
     WHERE      A~KAPPL = 'V'
     AND        A~KSCHL = 'ZVSR'
     AND        A~VKBUR IN R_VKBUR.
*    AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A846 - Sales Off./Country/Region/TaxCl6Cust/Material
  IF NOT IT_MARA IS INITIAL.
    SELECT  A~KAPPL
            A~KSCHL
            A~VKBUR
            A~LAND1
            A~REGIO
            A~TAXK6
            A~MATNR
            A~KFRST
            A~DATBI
            A~DATAB
            A~KNUMH
              C~ERDAT
            B~KOPOS
*                B~STFKZ
            B~KBETR
            B~KONWA
            B~KPEIN
            B~KMEIN
            B~LOEVM_KO
       INTO TABLE IT_A846
       FROM       A846 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
       FOR ALL ENTRIES IN IT_MARA
       WHERE      MATNR = IT_MARA-MATNR
       AND        A~KAPPL = 'V'
       AND        A~KSCHL = 'ZIVP'
       AND        A~VKBUR IN R_VKBUR.
*      AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A847 - Sales Off./Country/Region/TaxCl7Cust/Material
    SELECT  A~KAPPL
            A~KSCHL
            A~VKBUR
            A~LAND1
            A~REGIO
            A~TAXK7
            A~MATNR
            A~KFRST
            A~DATBI
            A~DATAB
            A~KNUMH
              C~ERDAT
            B~KOPOS
*                B~STFKZ
            B~KBETR
            B~KONWA
            B~KPEIN
            B~KMEIN
            B~LOEVM_KO
       INTO TABLE IT_A847
       FROM       A847 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
       FOR ALL ENTRIES IN IT_MARA
       WHERE      MATNR = IT_MARA-MATNR
       AND        A~KAPPL = 'V'
       AND        A~KSCHL = 'ZIVC'
       AND        A~VKBUR IN R_VKBUR.
*      AND    C~ERDAT IN S_DATE.

*&----------------------------------- Fetch A848 - Sales Off./Country/Region/TaxCl8Cust/Material
    SELECT  A~KAPPL
            A~KSCHL
            A~VKBUR
            A~LAND1
            A~REGIO
            A~TAXK8
            A~MATNR
            A~KFRST
            A~DATBI
            A~DATAB
            A~KNUMH
              C~ERDAT
            B~KOPOS
*                B~STFKZ
            B~KBETR
            B~KONWA
            B~KPEIN
            B~KMEIN
            B~LOEVM_KO
       INTO TABLE IT_A848
       FROM       A848 AS A
      INNER JOIN KONH AS C ON  ( A~KNUMH = C~KNUMH ) "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       INNER JOIN KONP AS B
       ON         C~KNUMH = B~KNUMH
       AND        C~KSCHL = B~KSCHL
       FOR ALL ENTRIES IN IT_MARA
       WHERE      MATNR = IT_MARA-MATNR
       AND        A~KAPPL = 'V'
       AND        A~KSCHL = 'ZVSR'
       AND        A~VKBUR IN R_VKBUR.
*     AND    C~ERDAT IN S_DATE.
  ENDIF.
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


  LOOP AT IT_A831 INTO WA_A831.
    WA_TAX_MASTER-TABLE_NAME = 'A831'.
    MOVE-CORRESPONDING WA_A831 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A831.
  ENDLOOP.

  LOOP AT IT_A834 INTO WA_A834.
    WA_TAX_MASTER-TABLE_NAME = 'A834'.
    MOVE-CORRESPONDING WA_A834 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A834.
  ENDLOOP.

  LOOP AT IT_A835 INTO WA_A835.
    WA_TAX_MASTER-TABLE_NAME = 'A835'.
    MOVE-CORRESPONDING WA_A835 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A835.
  ENDLOOP.

  LOOP AT IT_A843 INTO WA_A843.
    WA_TAX_MASTER-TABLE_NAME = 'A843'.
    MOVE-CORRESPONDING WA_A843 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A843.
  ENDLOOP.

  LOOP AT IT_A844 INTO WA_A844.
    WA_TAX_MASTER-TABLE_NAME = 'A844'.
    MOVE-CORRESPONDING WA_A844 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A844.
  ENDLOOP.

  LOOP AT IT_A845 INTO WA_A845.
    WA_TAX_MASTER-TABLE_NAME = 'A845'.
    MOVE-CORRESPONDING WA_A845 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A845.
  ENDLOOP.

  LOOP AT IT_A846 INTO WA_A846.
    WA_TAX_MASTER-TABLE_NAME = 'A846'.
    MOVE-CORRESPONDING WA_A846 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A846.
  ENDLOOP.

  LOOP AT IT_A847 INTO WA_A847.
    WA_TAX_MASTER-TABLE_NAME = 'A847'.
    MOVE-CORRESPONDING WA_A847 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A847.
  ENDLOOP.

  LOOP AT IT_A848 INTO WA_A848.
    WA_TAX_MASTER-TABLE_NAME = 'A848'.
    MOVE-CORRESPONDING WA_A848 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, WA_A848.
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
