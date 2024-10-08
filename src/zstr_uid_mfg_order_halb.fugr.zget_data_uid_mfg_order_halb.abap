FUNCTION ZGET_DATA_UID_MFG_ORDER_HALB.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_MFG_DATE) TYPE  CO_STRMP
*"  TABLES
*"      IT_MFG_ORDER_HALB STRUCTURE  ZSTR_UID_MFG_ORDER
*"----------------------------------------------------------------------

 TYPES : BEGIN OF TY_AFKO,
          AUFNR TYPE  AFPO-AUFNR,
          GETRI TYPE  AFKO-GETRI,
          END OF TY_AFKO.
  DATA: IT_AFKO TYPE TABLE OF TY_AFKO,
        WA_AFKO TYPE TY_AFKO.

  TYPES : BEGIN OF TY_MARA,
          MATNR TYPE MARA-MATNR,
          MTART TYPE MARA-MTART,
          END OF TY_MARA.
  DATA :IT_MARA TYPE TABLE OF TY_MARA,
        WA_MARA TYPE TY_MARA.

  TYPES :BEGIN OF TY_MARM,
        MATNR TYPE MARM-MATNR,
        UMREZ TYPE MARM-UMREZ,
        UMREN TYPE MARM-UMREN,
        MEINH TYPE MARM-MEINH,
    END OF TY_MARM.

  DATA :IT_MARM TYPE TABLE OF TY_MARM,
        WA_MARM TYPE TY_MARM.

  TYPES : BEGIN OF TY_AFPO,
      AUFNR TYPE  AFPO-AUFNR,
      PSMNG TYPE  AFPO-PSMNG,
      WEMNG TYPE  AFPO-WEMNG,
      PSAMG TYPE  AFPO-PSAMG,
      AMEIN TYPE  AFPO-AMEIN,
      MATNR TYPE  AFPO-MATNR,
      ELIKZ TYPE  AFPO-ELIKZ,
      DWERK TYPE  AFPO-DWERK,
      DGLTP TYPE  AFPO-DGLTP,
      CHARG TYPE  AFPO-CHARG,
      UMREZ TYPE  AFPO-UMREZ,
      UMREN TYPE  AFPO-UMREN,
    END OF TY_AFPO..

  DATA: IT_AFPO TYPE TABLE OF TY_AFPO,
        WA_AFPO TYPE TY_AFPO.

  TYPES : BEGIN OF TY_MAKT,
          MATNR TYPE MAKT-MATNR,
      MAKTX  TYPE MAKT-MAKTX,
    END OF TY_MAKT.

  DATA: IT_MAKT TYPE TABLE OF TY_MAKT,
        WA_MAKT TYPE TY_MAKT.
  TYPES :BEGIN OF TY_AUFK,
    AUFNR TYPE AUFK-AUFNR,
    END OF TY_AUFK.

  DATA: IT_AUFK TYPE TABLE OF TY_AUFK,
        WA_AUFK TYPE TY_AUFK.

  TYPES : BEGIN OF TY_AFPO_FI,
      AUFNR TYPE  AFPO-AUFNR,
      PSMNG TYPE  AFPO-PSMNG,
      WEMNG TYPE  AFPO-WEMNG,
      PSAMG TYPE  AFPO-PSAMG,
      AMEIN TYPE  AFPO-AMEIN,
      MATNR TYPE  AFPO-MATNR,
      ELIKZ TYPE  AFPO-ELIKZ,
      DWERK TYPE  AFPO-DWERK,
      DGLTP TYPE  AFPO-DGLTP,
      CHARG TYPE  AFPO-CHARG,
      MAKTX TYPE  MAKT-MAKTX,
      UMREZ TYPE  AFPO-UMREZ,
      UMREN TYPE  AFPO-UMREN,
    END OF TY_AFPO_FI.

  DATA:IT_AFPO_FI TYPE TABLE OF TY_AFPO_FI,
       WA_AFPO_FI TYPE TY_AFPO_FI,
       WA_AFPO_TAP TYPE ZSTR_UID_MFG_ORDER.



  SELECT
    A~AUFNR
    A~PSMNG
    A~WEMNG
    A~PSAMG
    A~AMEIN
    A~MATNR
    A~ELIKZ
    A~DWERK
    A~DGLTP
    A~CHARG
    A~UMREZ
    A~UMREN
      INTO TABLE IT_AFPO FROM ( ( AFPO AS A INNER JOIN AFKO AS B ON A~AUFNR = B~AUFNR AND B~GETRI EQ IT_MFG_DATE )
      INNER JOIN MARA AS C ON A~MATNR = C~MATNR AND C~MTART EQ 'HALB' ) .

  IF IT_AFPO IS NOT INITIAL .

    SELECT
      AUFNR
      INTO TABLE IT_AUFK FROM AUFK FOR ALL ENTRIES IN IT_AFPO WHERE AUFNR = IT_AFPO-AUFNR AND AUART IN ('PK00','PK01','PK02','PK03','PK04','PK05','PK08',
      'PK09','PK11','PK12','PK22','PK34','PK41','PK54','PK99','RW01').

    SELECT
      MATNR
      UMREZ
      UMREN
      MEINH
      INTO TABLE IT_MARM FROM MARM FOR ALL ENTRIES IN IT_AFPO WHERE MATNR = IT_AFPO-MATNR AND MEINH IN ('BOT') .

    SELECT
      MATNR
      MAKTX
      INTO TABLE IT_MAKT FROM MAKT FOR ALL ENTRIES IN IT_AFPO WHERE MATNR  EQ IT_AFPO-MATNR.
    SORT IT_AFPO ASCENDING BY AUFNR.
    SORT IT_AUFK ASCENDING BY AUFNR.

    LOOP AT IT_AUFK INTO WA_AUFK.
    LOOP AT IT_AFPO INTO WA_AFPO WHERE AUFNR = WA_AUFK-AUFNR.
      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_AFPO-MATNR.
      WA_AFPO_FI-AUFNR = WA_AFPO-AUFNR.
      WA_AFPO_FI-PSMNG = WA_AFPO-PSMNG.
      WA_AFPO_FI-WEMNG = WA_AFPO-WEMNG.
      WA_AFPO_FI-PSAMG = WA_AFPO-PSAMG.
      WA_AFPO_FI-AMEIN = WA_AFPO-AMEIN.
      WA_AFPO_FI-MATNR = WA_AFPO-MATNR.
      WA_AFPO_FI-ELIKZ = WA_AFPO-ELIKZ.
      WA_AFPO_FI-DWERK = WA_AFPO-DWERK.
      WA_AFPO_FI-DGLTP = WA_AFPO-DGLTP.
      WA_AFPO_FI-CHARG = WA_AFPO-CHARG.
      WA_AFPO_FI-MAKTX = WA_MAKT-MAKTX.
      READ TABLE IT_MARM INTO WA_MARM WITH KEY MATNR = WA_AFPO-MATNR.
      WA_AFPO_FI-UMREZ = WA_MARM-UMREZ.
      WA_AFPO_FI-UMREN = WA_MARM-UMREN.
      APPEND WA_AFPO_FI TO IT_AFPO_FI.
      CLEAR :WA_AFPO_FI,WA_AFPO,WA_MAKT.
    ENDLOOP.

    ENDLOOP.

    LOOP AT IT_AFPO_FI INTO WA_AFPO_FI.
      MOVE-CORRESPONDING WA_AFPO_FI TO WA_AFPO_TAP.
      APPEND WA_AFPO_TAP TO IT_MFG_ORDER_HALB.
      CLEAR :WA_AFPO_TAP,WA_AFPO_FI.
    ENDLOOP.

  ENDIF.





ENDFUNCTION.
