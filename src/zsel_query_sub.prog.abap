*&---------------------------------------------------------------------*
*&  Include           ZSEL_QUERY_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_QUERY_T001W
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_QUERY_T001W ."

  IF P_BUKRS = '1000' OR P_BUKRS = '2000' OR P_BUKRS = '4000' OR P_BUKRS = '5000'.


    IF P_BUKRS IS NOT INITIAL.

      AUTHORITY-CHECK OBJECT 'ZTRANSIT'
      ID 'BUKRS' FIELD P_BUKRS
      ID 'ACTVT' FIELD '03'.
      IF SY-SUBRC EQ 0.
        LT_DATE = SY-DATUM.

        REFRESH GT_FINAL.
        REFRESH GT_T001.

        CLEAR P_KEY.
        CLEAR LV_FLAG5.


        IF P_KEY = ' '.
          WA_FINAL-CHECK1 = ' '.
        ENDIF.


        SELECT BUKRS
               BUTXT FROM T001 INTO TABLE GT_T001 WHERE BUKRS = P_BUKRS.
        REFRESH GT_T001W.


        SELECT  WERKS
                NAME1
                IWERK
                VKORG
                FROM T001W INTO TABLE GT_T001W
                FOR ALL ENTRIES IN GT_T001 WHERE VKORG = GT_T001-BUKRS AND  IWERK = ' ' .


        REFRESH GT_MDUB.
        IF GT_T001W[] IS NOT INITIAL.
          SELECT MATNR
                 WERKS
                 EBELN
                 EBELP
                 WAMNG
                 WEMNG
                 FROM MDUB INTO TABLE GT_MDUB
                 FOR ALL ENTRIES IN GT_T001W
                 WHERE WERKS = GT_T001W-WERKS.

        ENDIF.
        SORT GT_MDUB BY WERKS ASCENDING.
        DELETE GT_MDUB WHERE WAMNG EQ 0 AND WEMNG EQ 0  .


        LOOP AT GT_MDUB INTO WA_MDUB.
          IF WA_MDUB-WAMNG  EQ WA_MDUB-WEMNG.
            DELETE GT_MDUB .
          ENDIF.
        ENDLOOP.



        REFRESH GT_MSEG.
        IF GT_MDUB[] IS  NOT INITIAL.
          SELECT MBLNR
                 ZEILE
                 BWART
                 MATNR
                 WERKS
                 EBELN
                 BUDAT_MKPF
                 SMBLN
                 SMBLP
                FROM MSEG INTO TABLE GT_MSEG
                 FOR ALL ENTRIES IN GT_MDUB
                 WHERE ( MATNR = GT_MDUB-MATNR
                 AND WERKS = GT_MDUB-WERKS
                 AND EBELN = GT_MDUB-EBELN )
                 AND BWART <> '101' .
        ENDIF.
        SORT GT_MSEG BY WERKS ASCENDING.
        APPEND LINES OF GT_MSEG TO GT_MSEG1.
        APPEND LINES OF GT_MSEG TO GT_MSEG2.


        LOOP AT GT_MSEG1 INTO WA_MSEG1 .
          LOOP AT GT_MSEG2 INTO WA_MSEG2 WHERE MBLNR = WA_MSEG1-SMBLN AND ZEILE = WA_MSEG1-SMBLP.
            DELETE GT_MSEG2.
          ENDLOOP.
        ENDLOOP.
        DELETE  GT_MSEG2  WHERE MBLNR NE 0 AND SMBLN NE 0.

        REFRESH GT_MSEG.
        APPEND LINES OF GT_MSEG2 TO GT_MSEG.

        SELECT  WERKS
                NAME1
                ST_DAYS
                MN_DAYS1
                STATUS
                STATUS1
                FROM ZTRAM_LOG_TABLE INTO TABLE GT_CUS_TABLE.


        LOOP AT GT_MSEG INTO WA_MSEG.
          CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
            EXPORTING
              BEGDA = WA_MSEG-BUDAT_MKPF
              ENDDA = LT_DATE
            IMPORTING
              DAYS  = LV_DAYS.
          WA_MSEG-TOT_DAYS = LV_DAYS.
          MODIFY GT_MSEG FROM WA_MSEG TRANSPORTING TOT_DAYS.
        ENDLOOP.

        SORT GT_MSEG BY WERKS TOT_DAYS DESCENDING.
        DELETE ADJACENT DUPLICATES FROM GT_MSEG COMPARING WERKS.


        LOOP AT GT_T001W INTO WA_T001W.
          WA_FINAL-WERKS = WA_T001W-WERKS.
          WA_FINAL-NAME1  = WA_T001W-NAME1.
          WA_FINAL-ST_DAYS = ''  .
          APPEND WA_FINAL TO GT_FINAL.
        ENDLOOP.



        LOOP AT GT_FINAL INTO WA_FINAL.
          LOOP AT GT_MSEG INTO WA_MSEG WHERE WERKS = WA_FINAL-WERKS.
            WA_FINAL-ST_DAYS = WA_MSEG-TOT_DAYS.
            MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING ST_DAYS.
            CLEAR WA_FINAL-ST_DAYS.
          ENDLOOP.
        ENDLOOP.


        LOOP AT GT_FINAL INTO WA_FINAL.
          LOOP AT GT_CUS_TABLE INTO WA_CUS_TABLE WHERE WERKS = WA_FINAL-WERKS.
            WA_FINAL-MN_DAYS1 = WA_CUS_TABLE-MN_DAYS1.
            IF WA_MSEG-TOT_DAYS GE WA_CUS_TABLE-MN_DAYS1.
              WA_FINAL-CHECK = ' '.
            ELSEIF WA_MSEG-TOT_DAYS LE WA_CUS_TABLE-MN_DAYS1.
              WA_FINAL-CHECK = 'X'.
            ENDIF.

            IF WA_CUS_TABLE-STATUS1 = 'YES'.
              WA_FINAL-CHECK1 = 'X'.
            ELSEIF WA_CUS_TABLE-STATUS1 = 'NO'.
              WA_FINAL-CHECK1 = ' '.
            ENDIF.

          ENDLOOP.
          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  MN_DAYS1  CHECK CHECK1 .

        ENDLOOP.
      ELSE.
        MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
      ENDIF.

    ELSE.
      REFRESH GT_FINAL.
      CLEAR P_KEY.
    ENDIF.

  ELSE.
    MESSAGE 'INVALIED COMPANY CODE' TYPE 'S'.
    CLEAR P_BUKRS.
    REFRESH GT_FINAL.
  ENDIF.






ENDFORM.                    " SELECT_QUERY_T001W
