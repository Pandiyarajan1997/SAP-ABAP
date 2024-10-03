FUNCTION ZBAPI_SKU_COMP_MASTER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"  TABLES
*"      IT_COMP_CODE TYPE  ZTT_CNF_SAL_ORG
*"      IT_DIST_CHAN TYPE  ZTT_CNF_DIST_CHAN
*"      IT_REGION_MAS TYPE  ZTT_CNF_REGION_MAS
*"      IT_PLANT_MAS TYPE  ZTT_CNF_PLANT_MAS
*"      IT_BILL_TYPE TYPE  ZTT_CNF_BILL_TYPE
*"      IT_MOVE_TYPE TYPE  ZTT_CNF_MOVE_TYPE
*"      IT_SKU_MAS TYPE  ZTT_CNF_SKU_MAS
*"      IT_SKU_MAPPING STRUCTURE  ZSTR_CNF_SKU_MAPPING
*"      IT_PAYMENT_TERM_DESC STRUCTURE  ZSTR_CNF_PAYMENT_TERM_DESC
*"      RETURN TYPE  ZTT_BAPIRET2
*"----------------------------------------------------------------------
""
*&------------------------------------------ DECLARATIONS - BEGIN ----------------------------------------------
  TYPES: BEGIN OF TY_TVBUR,
          VKBUR TYPE VKBUR,
          ADRNR TYPE ADRNR,
         END OF TY_TVBUR.

  TYPES: BEGIN OF TY_ADRC,
          ADDRNUMBER TYPE AD_ADDRNUM,
          NAME1 TYPE AD_NAME1,
          NAME2 TYPE KUNNR,
          CITY1 TYPE AD_CITY1,
          POST_CODE1 TYPE AD_PSTCD1,
          HOUSE_NUM1 TYPE AD_HSNM1,
          STREET TYPE AD_STREET,
          COUNTRY TYPE ADRC-COUNTRY,
          REGION TYPE REGIO,
          TEL_NUMBER TYPE AD_TLNMBR1,
          FAX_NUMBER TYPE AD_FXNMBR1,
          REMARK TYPE ADRCT-REMARK,
         END OF TY_ADRC.

  TYPES: BEGIN OF TY_ADR6,
          ADDRNUMBER TYPE AD_ADDRNUM,
          SMTP_ADDR TYPE AD_SMTPADR,
         END OF TY_ADR6.

  TYPES: BEGIN OF TY_TVKBT,
          VKBUR TYPE TVKBT-VKBUR,
          BEZEI TYPE TVKBT-BEZEI,
         END OF TY_TVKBT.

  TYPES: BEGIN OF TY_TVKO,
          BUKRS TYPE TVKO-BUKRS,
          VKORG TYPE TVKOV-VKORG,
          VTWEG TYPE TVKOV-VTWEG,
         END OF TY_TVKO.
  TYPES: BEGIN OF TY_TVTWT,
           VTWEG TYPE TVKOV-VTWEG,
           VTEXT TYPE TVTWT-VTEXT,
         END OF TY_TVTWT.


 TYPES: BEGIN OF TY_GST,
          KUNNR TYPE KUNNR,
          STCD3 TYPE STCD3,
         END OF TY_GST.

  DATA: WA_SKU_MAS TYPE ZSTR_CNF_SKU_MAS.
  DATA: IT_SKU_MAPPING1 TYPE TABLE OF ZSTR_CNF_SKU_MAPPING,
        WA_SKU_MAPPING1 TYPE ZSTR_CNF_SKU_MAPPING.

  DATA: IT_TVBUR TYPE STANDARD TABLE OF TY_TVBUR,
        WA_TVBUR TYPE TY_TVBUR.
  DATA: IT_ADRC TYPE STANDARD TABLE OF TY_ADRC,
        WA_ADRC TYPE TY_ADRC.
  DATA: IT_ADR6 TYPE STANDARD TABLE OF TY_ADR6,
        WA_ADR6 TYPE TY_ADR6.

  DATA : IT_TVKBT TYPE TABLE OF TY_TVKBT,
         WA_TVKBT TYPE TY_TVKBT.

  DATA: IT_TVKO TYPE TABLE OF TY_TVKO,
        WA_TVKO TYPE  TY_TVKO.
  DATA: IT_TVTWT TYPE TABLE OF TY_TVTWT,
        WA_TVTWT TYPE  TY_TVTWT.

  DATA: IT_GST TYPE TABLE OF TY_GST,
        WA_GST TYPE TY_GST.

  DATA: WA_DIST_CHAN LIKE LINE OF IT_DIST_CHAN.
  DATA: IT_ZSKU_BA TYPE TABLE OF ZSKU_BA,
        WA_ZSKU_BA TYPE ZSKU_BA.
  DATA: WA_RETURN TYPE BAPIRET2.
  DATA: IT_T052U TYPE TABLE OF T052U,
        WA_T052U TYPE T052U.
  DATA: IT_TVZBT TYPE TABLE OF TVZBT,
        WA_TVZBT TYPE TVZBT.
  DATA: WA_PAYMENT_TERM_DESC TYPE ZSTR_CNF_PAYMENT_TERM_DESC.
  DATA: IT_ADR2 TYPE TABLE OF ADR2.
  FIELD-SYMBOLS: <FS_ITAB> TYPE ANY TABLE.
  FIELD-SYMBOLS: <FS_ADR2> TYPE ADR2.
  DATA: WA_ADR2 TYPE ADR2.
  RANGES: R_VKORG FOR TVKO-VKORG.
  RANGES: R_ZTERM FOR TVZBT-ZTERM.
*&------------------------------------------ DECLARATIONS - END ------------------------------------------------


*&---------------------------------------- DATA RETRIEVAL - BEGIN ----------------------------------------------
*&------------------------------------ Get Company Code Master
  SELECT BUKRS
         BUTXT
    INTO TABLE IT_COMP_CODE
    FROM T001
    WHERE BUKRS = P_BUKRS.

  IF SY-SUBRC <> 0.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Company Code not Found!!! Check Your Entries!!!'.
    CLEAR: GV_INDEX_MSG.
    EXIT.
  ENDIF.

*&------------------------------------ Get sales organization, distribution channel for a company code
  SELECT T1~BUKRS
         T1~VKORG
         T2~VTWEG
    FROM TVKO AS T1
    JOIN TVKOV AS T2 ON T1~VKORG = T2~VKORG
    INTO TABLE IT_TVKO
    WHERE T1~BUKRS = P_BUKRS.

  IF SY-SUBRC = 0.
    LOOP AT IT_TVKO INTO WA_TVKO.
      R_VKORG-SIGN = 'I'.
      R_VKORG-OPTION = 'EQ'.
      R_VKORG-LOW = WA_TVKO-VKORG.
      APPEND R_VKORG.
    ENDLOOP.
    SORT R_VKORG.
    DELETE ADJACENT DUPLICATES FROM R_VKORG.

*&------------------------------------ Get distribution channel Master
    SELECT T1~VTWEG
           T2~VTEXT
      INTO TABLE IT_TVTWT
      FROM TVKOV AS T1
      JOIN TVTWT AS T2 ON T1~VTWEG = T2~VTWEG
      FOR ALL ENTRIES IN IT_TVKO
      WHERE T1~VTWEG = IT_TVKO-VTWEG
      AND   SPRAS = SY-LANGU.
  ENDIF.

*&-------------------------------------- Get Region Master
  SELECT LAND1
         BLAND
         BEZEI
    INTO TABLE IT_REGION_MAS
    FROM T005U
    WHERE SPRAS = SY-LANGU AND
          LAND1 IN ('IN' , 'LK' , 'NP' ).

*&-------------------------------------- Get Billing Type
  SELECT T1~FKART
         T2~VTEXT
    INTO TABLE IT_BILL_TYPE
    FROM TVFK AS T1
    JOIN TVFKT AS T2 ON T1~FKART = T2~FKART
    WHERE SPRAS = SY-LANGU.

*&--------------------------------------- Get Movement Type
  SELECT T1~BWART
         T2~SPRAS
         T2~SOBKZ
         T2~KZBEW
         T2~KZZUG
         T2~KZVBR
         T2~BTEXT
    INTO TABLE IT_MOVE_TYPE
    FROM T156 AS T1
    LEFT OUTER JOIN T156T AS T2 ON T1~BWART = T2~BWART.
*      where t2~spras = sy-langu.
  IF SY-SUBRC = 0.
    DELETE IT_MOVE_TYPE WHERE SPRAS NE SY-LANGU AND SPRAS IS NOT INITIAL.
  ENDIF.
*********************************************
  " BWART has many descriptions IN SAP. we have just cosidered the first one for time being
  SORT IT_MOVE_TYPE BY BWART.
  DELETE ADJACENT DUPLICATES FROM IT_MOVE_TYPE COMPARING BWART.
*********************************************

*&---------------------------------------------- Get Plant Master
  SELECT T1~BWKEY
         T1~BUKRS
         T2~WERKS
         T2~NAME1
         T2~REGIO
    INTO TABLE IT_PLANT_MAS
    FROM T001K AS T1
    JOIN T001W AS T2 ON T1~BWKEY = T2~BWKEY
    WHERE T1~BUKRS = P_BUKRS.

*&----------------------------------------------- Payment Terms Description
  SELECT *
    FROM TVZBT
    INTO TABLE IT_TVZBT
    WHERE SPRAS = SY-LANGU.

  SELECT *
    FROM T052U
    INTO TABLE IT_T052U
    WHERE SPRAS = SY-LANGU.

*&---------------------------------------------- Get SKU Mapping (List of Sales Offices for sales organization)
  SELECT VKORG
         VTWEG
         SPART
         VKBUR
    FROM TVKBZ
    INTO TABLE IT_SKU_MAPPING
    WHERE VKORG IN R_VKORG.

  IF SY-SUBRC = 0 .
    IT_SKU_MAPPING1[] = IT_SKU_MAPPING[].

    SORT IT_SKU_MAPPING1 BY VKBUR.
    DELETE ADJACENT DUPLICATES FROM IT_SKU_MAPPING1 COMPARING VKBUR.

*&---------------------------- Get name of the sales offices
    SELECT VKBUR
           BEZEI
      FROM TVKBT
      INTO TABLE IT_TVKBT
      FOR ALL ENTRIES IN IT_SKU_MAPPING1
      WHERE VKBUR = IT_SKU_MAPPING1-VKBUR
      AND SPRAS = SY-LANGU.

*&---------------------------- Get Business area for the sales offices
    SELECT *
      FROM ZSKU_BA
      INTO TABLE IT_ZSKU_BA
      FOR ALL ENTRIES IN IT_SKU_MAPPING1
      WHERE VKORG = IT_SKU_MAPPING1-VKORG
      AND VKBUR = IT_SKU_MAPPING1-VKBUR.

*&----------------------------- Get ADRNR for a sales office from TVBUR
    SELECT VKBUR
           ADRNR
    FROM  TVBUR
    INTO CORRESPONDING FIELDS OF TABLE IT_TVBUR
    FOR ALL ENTRIES IN IT_SKU_MAPPING1
    WHERE VKBUR = IT_SKU_MAPPING1-VKBUR.

*&---------------------------- Get Contact Details (Address) for the sales offices
    IF SY-SUBRC = 0.
      SELECT A~ADDRNUMBER
             A~NAME1
             A~NAME2
             A~CITY1
             A~POST_CODE1
             A~HOUSE_NUM1
             A~STREET
             A~COUNTRY
             A~REGION
             A~TEL_NUMBER
             A~FAX_NUMBER
             B~REMARK
        FROM ADRC AS A
        LEFT OUTER JOIN ADRCT AS B ON A~ADDRNUMBER = B~ADDRNUMBER
        INTO TABLE IT_ADRC
        FOR ALL ENTRIES IN IT_TVBUR
        WHERE A~ADDRNUMBER = IT_TVBUR-ADRNR.

*&---------------------------- Get Email Address for the sales offices
      SELECT ADDRNUMBER
             SMTP_ADDR
        FROM ADR6
        INTO TABLE IT_ADR6
        FOR ALL ENTRIES IN IT_ADRC
        WHERE ADDRNUMBER = IT_ADRC-ADDRNUMBER.

*&----------------------------- Get GST no

        SELECT
     KUNNR
     STCD3
        FROM KNA1
        INTO TABLE IT_GST .
*        FOR ALL ENTRIES IN IT_ADRC
*        WHERE KUNNR EQ IT_ADRC-NAME2 .

*&---------------------------- Get Mobile phone number for the sales offices
      SELECT *
        FROM ADR2
        INTO TABLE IT_ADR2
        FOR ALL ENTRIES IN IT_ADRC
        WHERE ADDRNUMBER = IT_ADRC-ADDRNUMBER.
      IF SY-SUBRC = 0.
        SORT IT_ADR2 BY ADDRNUMBER ASCENDING R3_USER DESCENDING.
      ENDIF.
    ENDIF.
  ENDIF.
*&---------------------------------------- DATA RETRIEVAL - END ----------------------------------------------


*&--------------------------------------- DATA PROCESSING - BEGIN ----------------------------------------------
*&------------------------------------- Fill Distribution Channel Master - BEGIN -------------------------------
  IF NOT IT_TVKO IS INITIAL.
    SORT: IT_TVKO BY VTWEG,
          IT_TVTWT BY VTWEG.

    LOOP AT IT_TVKO INTO WA_TVKO.
      READ TABLE IT_TVTWT TRANSPORTING NO FIELDS WITH KEY VTWEG = WA_TVKO-VTWEG BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_INDEX = SY-TABIX.

        LOOP AT  IT_TVTWT INTO WA_TVTWT FROM LV_INDEX.
          IF WA_TVTWT-VTWEG = WA_TVKO-VTWEG.
            WA_DIST_CHAN-BUKRS = WA_TVKO-BUKRS.
            WA_DIST_CHAN-VKORG = WA_TVKO-VKORG.
            WA_DIST_CHAN-VTWEG = WA_TVTWT-VTWEG.
            WA_DIST_CHAN-VTEXT = WA_TVTWT-VTEXT.

            APPEND WA_DIST_CHAN TO IT_DIST_CHAN.
            CLEAR: WA_DIST_CHAN, WA_TVKO, WA_TVTWT.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
*&------------------------------------- Fill Distribution Channel Master - END -------------------------------


*&------------------------------------- Fill Payment term Descriptions - BEGIN --------------------------------
  SORT IT_T052U BY ZTERM.
  SORT IT_TVZBT BY ZTERM.
  DELETE ADJACENT DUPLICATES FROM IT_T052U COMPARING ZTERM.

  LOOP AT IT_TVZBT INTO WA_TVZBT.
    R_ZTERM-SIGN = 'I'.
    R_ZTERM-OPTION = 'EQ'.
    R_ZTERM-LOW = WA_TVZBT-ZTERM.

    APPEND R_ZTERM.
  ENDLOOP.

  DELETE IT_T052U WHERE ZTERM IN R_ZTERM.

  LOOP AT IT_TVZBT INTO WA_TVZBT.
    MOVE-CORRESPONDING WA_TVZBT TO WA_PAYMENT_TERM_DESC.
    APPEND WA_PAYMENT_TERM_DESC TO IT_PAYMENT_TERM_DESC.
    CLEAR: WA_PAYMENT_TERM_DESC.
  ENDLOOP.

  LOOP AT IT_T052U INTO WA_T052U.
    MOVE-CORRESPONDING WA_T052U TO WA_PAYMENT_TERM_DESC.
    APPEND WA_PAYMENT_TERM_DESC TO IT_PAYMENT_TERM_DESC.
    CLEAR: WA_PAYMENT_TERM_DESC.
  ENDLOOP.

  SORT: IT_PAYMENT_TERM_DESC BY ZTERM.
*&------------------------------------- Fill Payment term Descriptions - END ----------------------------------


*&-------------------------------------- Fill SKU Master - BEGIN ----------------------------------------------
  SORT: IT_SKU_MAPPING1 BY VKBUR,
        IT_TVKBT BY VKBUR,
        IT_TVBUR BY VKBUR,
        IT_ZSKU_BA BY VKBUR,
        IT_ADRC BY ADDRNUMBER,
        IT_ADR6 BY ADDRNUMBER,
        IT_ADR2 BY ADDRNUMBER R3_USER,
        IT_GST BY KUNNR.

 LOOP AT IT_GST INTO WA_GST .
   SHIFT WA_GST-KUNNR LEFT DELETING LEADING '0'.
   MODIFY IT_GST FROM WA_GST TRANSPORTING KUNNR.
   CLEAR WA_GST.
 ENDLOOP.

  ASSIGN WA_ADR2 TO <FS_ADR2>.

  LOOP AT IT_SKU_MAPPING1 INTO WA_SKU_MAPPING1.

    WA_SKU_MAS-VKORG = WA_SKU_MAPPING1-VKORG.
    WA_SKU_MAS-VKBUR = WA_SKU_MAPPING1-VKBUR.

    READ TABLE IT_ZSKU_BA INTO WA_ZSKU_BA WITH KEY VKBUR = WA_SKU_MAPPING1-VKBUR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SKU_MAS-GSBER     = WA_ZSKU_BA-GSBER.
      WA_SKU_MAS-J_1ILSTNO = WA_ZSKU_BA-J_1ILSTNO.
      WA_SKU_MAS-J_1ICSTNO = WA_ZSKU_BA-J_1ICSTNO.
      WA_SKU_MAS-J_1IPANNO = WA_ZSKU_BA-J_1IPANNO.
    ENDIF.

    READ TABLE IT_TVKBT INTO WA_TVKBT WITH KEY VKBUR = WA_SKU_MAPPING1-VKBUR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SKU_MAS-BEZEI = WA_TVKBT-BEZEI.
    ENDIF.

    READ TABLE IT_TVBUR INTO WA_TVBUR WITH KEY VKBUR = WA_SKU_MAPPING1-VKBUR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SKU_MAS-ADRNR = WA_TVBUR-ADRNR.

      READ TABLE IT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_TVBUR-ADRNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SKU_MAS-NAME1 = WA_ADRC-NAME1.
        WA_SKU_MAS-NAME2 = WA_ADRC-NAME2.
        WA_SKU_MAS-CITY1 = WA_ADRC-CITY1.
        WA_SKU_MAS-POST_CODE1 = WA_ADRC-POST_CODE1.
        WA_SKU_MAS-HOUSE_NUM1 = WA_ADRC-HOUSE_NUM1.
        WA_SKU_MAS-STREET = WA_ADRC-STREET.
        WA_SKU_MAS-COUNTRY = WA_ADRC-COUNTRY.
        WA_SKU_MAS-REGION = WA_ADRC-REGION.
        WA_SKU_MAS-TEL_NUMBER = WA_ADRC-TEL_NUMBER.
        WA_SKU_MAS-FAX_NUMBER = WA_ADRC-FAX_NUMBER.
        WA_SKU_MAS-REMARK     = WA_ADRC-REMARK.
        CONCATENATE WA_SKU_MAS-HOUSE_NUM1 SPACE WA_SKU_MAS-STREET ',' SPACE WA_SKU_MAS-CITY1 '-' SPACE WA_SKU_MAS-POST_CODE1 '.' INTO WA_SKU_MAS-ADDRESS.
      ENDIF.

      READ TABLE IT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_TVBUR-ADRNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SKU_MAS-SMTP_ADDR = WA_ADR6-SMTP_ADDR.
      ENDIF.
"   LOOP AT IT_GST INTO WA_GST WHERE KUNNR = WA_SKU_MAS-NAME2 .
    READ TABLE IT_GST INTO WA_GST WITH KEY KUNNR = WA_SKU_MAS-NAME2 BINARY SEARCH .
           IF SY-SUBRC = 0.
          WA_SKU_MAS-STCD3    = WA_GST-STCD3 .
        ENDIF.
    "    ENDLOOP.

      LOOP AT IT_ADR2 ASSIGNING <FS_ADR2> WHERE ADDRNUMBER = WA_TVBUR-ADRNR AND R3_USER = '3'.
        READ TABLE IT_ADR2 ASSIGNING <FS_ADR2> WITH KEY ADDRNUMBER = <FS_ADR2>-ADDRNUMBER
                                                        R3_USER    = '3' BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SKU_MAS-TELNR_LONG   = <FS_ADR2>-TELNR_LONG.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    APPEND WA_SKU_MAS TO IT_SKU_MAS.
    CLEAR: WA_SKU_MAS, WA_ADRC, WA_ADR6,WA_GST, WA_TVBUR, WA_TVKBT, <FS_ADR2>, WA_ZSKU_BA.
  ENDLOOP.
*&-------------------------------------- Fill SKU Master - END ----------------------------------------------
*&--------------------------------------- DATA PROCESSING - END ----------------------------------------------


*&----------------------------------------- BAPI Return Messages - BEGIN -------------------------------------
  PERFORM BAPI_MESSAGES TABLES RETURN
                               IT_COMP_CODE
                               IT_DIST_CHAN
                               IT_REGION_MAS
                               IT_BILL_TYPE
                               IT_MOVE_TYPE
                               IT_PLANT_MAS
                               IT_PAYMENT_TERM_DESC
                               IT_SKU_MAS
                               IT_SKU_MAPPING.
*&----------------------------------------- BAPI Return Messages - END -------------------------------------

  DELETE IT_PAYMENT_TERM_DESC WHERE ZTERM IS INITIAL.

  CLEAR: GV_INDEX_MSG, LV_INDEX.




ENDFUNCTION.
