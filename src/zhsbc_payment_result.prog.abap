*
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPDATE_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TABLE_UPDATE_XML .

  DATA : C TYPE STRING.

  clear : HEAD_XML.

  IF STATUS_XML IS INITIAL.

    IF BACK_SCREEN = '101' OR BACK_SCREEN = '103'.

      HEAD_XML-MANDT = SY-MANDT.
      HEAD_XML-BATCH = BATCH.
      HEAD_XML-XMLNA = FILE_NAME_XML.
      HEAD_XML-ERNAM = SY-UNAME.
      HEAD_XML-ERZET = SY-UZEIT.
      HEAD_XML-ERDAT = SY-DATUM.
      HEAD_XML-TRNUM = XML_NO.
      HEAD_XML-TOAMT = XML_TOTAL.
      HEAD_XML-PSWSL = CKY.
      IF BACK_SCREEN = '101' .
        HEAD_XML-METHOD = 'VE'.
      ELSEIF BACK_SCREEN = '103' .
        HEAD_XML-METHOD = 'GL'.
      ENDIF.

      CONDENSE TEMP_STATUS_XML.
      INSERT INTO ZHSBC_HEAD VALUES HEAD_XML.

      LOOP AT IT_XML INTO WA_XML.
        CLEAR C.
        C = TEMP_STATUS_XML.
        WA_XML-DOWNLOAD_STATUS =  TEMP_STATUS_XML(119).
        SHIFT C BY 119 PLACES.
        WA_XML-DOWNLOAD_STATUS1 =  C.

        IF BACK_SCREEN = '101' .
          WA_XML-METHOD = 'VE'.
        ELSEIF BACK_SCREEN = '103' .
          WA_XML-METHOD = 'GL'.
        ENDIF.

        INSERT INTO  ZHSBC_ITEMS VALUES WA_XML.
        CLEAR WA_XML.

      ENDLOOP.

    ELSEIF BACK_SCREEN = '102' OR BACK_SCREEN = '104'.

      HEAD_XML-MANDT = SY-MANDT.
      HEAD_XML-BATCH = BATCH.
      HEAD_XML-XMLNA = FILE_NAME_XML.
      HEAD_XML-ERNAM = SY-UNAME.
      HEAD_XML-ERZET = SY-UZEIT.
      HEAD_XML-ERDAT = SY-DATUM.
      HEAD_XML-TRNUM = XML_NO.
      HEAD_XML-TOAMT = XML_TOTAL.
      HEAD_XML-PSWSL = CKY.

      IF BACK_SCREEN = '102' .
        HEAD_XML-METHOD = 'VE'.
      ELSEIF BACK_SCREEN = '104' .
        HEAD_XML-METHOD = 'GL'.
      ENDIF.


      CONDENSE TEMP_STATUS_XML.
      INSERT INTO ZHSBC_HEAD VALUES HEAD_XML.

      LOOP AT IT_XML INTO WA_XML.
        CLEAR C.
        C = TEMP_STATUS_XML.
        WA_XML-DOWNLOAD_STATUS =  TEMP_STATUS_XML(119).
        SHIFT C BY 119 PLACES.
        WA_XML-DOWNLOAD_STATUS1 =  C.

        READ TABLE IT_HSBC_ITEMS INTO WA_HSBC_ITEMS WITH KEY BELNR = WA_XML-BELNR.

        IF SY-SUBRC = 0.

          WA_HSBC_ITEMS-REX_BATCH =  BATCH.
          WA_HSBC_ITEMS-REX_XMLNA =  FILE_NAME_XML.
          WA_HSBC_ITEMS-REX_DATE = SY-DATUM.
          WA_HSBC_ITEMS-REX_TIME = SY-UZEIT.
          WA_HSBC_ITEMS-REX_USER = SY-UNAME.

          UPDATE ZHSBC_ITEMS FROM WA_HSBC_ITEMS.

        ENDIF.

        WA_XML-REX_NO = WA_HSBC_ITEMS-REX_NO + 1.

        IF BACK_SCREEN = '102' .
          WA_XML-METHOD = 'VE'.
        ELSEIF BACK_SCREEN = '104'.
          WA_XML-METHOD = 'GL'.
        ENDIF.


        INSERT INTO  ZHSBC_ITEMS VALUES WA_XML.
        CLEAR WA_XML.
        CLEAR WA_HSBC_ITEMS.
      ENDLOOP.


    ENDIF.

  ENDIF.

ENDFORM.                    " TABLE_UPDATE_XML
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPDATE_IMPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TABLE_UPDATE_IMPS .

  DATA D TYPE STRING.

  IF STATUS_IMPS IS INITIAL.

    IF BACK_SCREEN = '101' OR BACK_SCREEN = '103'.

      HEAD_IMPS-MANDT = SY-MANDT.
      HEAD_IMPS-BATCH = BATCH.
      HEAD_IMPS-XMLNA = FILE_NAME_IMPS.
      HEAD_IMPS-ERNAM = SY-UNAME.
      HEAD_IMPS-ERZET = SY-UZEIT.
      HEAD_IMPS-ERDAT = SY-DATUM.
      HEAD_IMPS-TRNUM = IMPS_NO.
      HEAD_IMPS-TOAMT = IMPS_TOTAL.
      HEAD_IMPS-PSWSL = CKY.

      IF BACK_SCREEN = '101' .
        HEAD_IMPS-METHOD = 'VE'.
      ELSEIF BACK_SCREEN = '103' .
        HEAD_IMPS-METHOD = 'GL'.
      ENDIF.

      INSERT INTO ZHSBC_HEAD VALUES HEAD_IMPS.
      CONDENSE TEMP_STATUS_IMPS.
      LOOP AT IT_IMPS INTO WA_IMPS.

*        READ TABLE IT_HSBC_ITEMS INTO WA_HSBC_ITEMS WITH KEY BELNR = WA_IMPS-BELNR.
*
*        IF SY-SUBRC = 0.
*          WA_HSBC_ITEMS-REX_NO = WA_HSBC_ITEMS-REX_NO + 1.
*          WA_HSBC_ITEMS-REX_BATCH =  BATCH.
*          WA_HSBC_ITEMS-REX_XMLNA =  FILE_NAME_IMPS.
*          WA_HSBC_ITEMS-REX_DATE = SY-DATUM.
*          WA_HSBC_ITEMS-REX_TIME = SY-UZEIT.
*          WA_HSBC_ITEMS-REX_USER = SY-UNAME.
*
*          UPDATE ZHSBC_ITEMS FROM WA_HSBC_ITEMS.
*          CLEAR WA_HSBC_ITEMS.
*        ENDIF.

        WA_IMPS-DOWNLOAD_STATUS = TEMP_STATUS_IMPS(120).
        CLEAR D.
        D = TEMP_STATUS_IMPS.
        SHIFT D BY 120 PLACES.
        WA_IMPS-DOWNLOAD_STATUS1 = D.

        IF BACK_SCREEN = '101' .
          WA_IMPS-METHOD = 'VE'.
        ELSEIF BACK_SCREEN = '103' .
          WA_IMPS-METHOD = 'GL'.
        ENDIF.

        INSERT INTO ZHSBC_ITEMS VALUES WA_IMPS.
        CLEAR WA_IMPS.

      ENDLOOP.

    ELSEIF BACK_SCREEN = '102' OR BACK_SCREEN = '104'.

      HEAD_IMPS-MANDT = SY-MANDT.
      HEAD_IMPS-BATCH = BATCH.
      HEAD_IMPS-XMLNA = FILE_NAME_IMPS.
      HEAD_IMPS-ERNAM = SY-UNAME.
      HEAD_IMPS-ERZET = SY-UZEIT.
      HEAD_IMPS-ERDAT = SY-DATUM.
      HEAD_IMPS-TRNUM = IMPS_NO.
      HEAD_IMPS-TOAMT = IMPS_TOTAL.
      HEAD_IMPS-PSWSL = 'INR'.

      IF BACK_SCREEN = '102' .
        HEAD_IMPS-METHOD = 'VE'.
      ELSEIF BACK_SCREEN = '104' .
        HEAD_IMPS-METHOD = 'GL'.
      ENDIF.


      CONDENSE TEMP_STATUS_IMPS.
      INSERT INTO ZHSBC_HEAD VALUES HEAD_IMPS.

      LOOP AT IT_IMPS INTO WA_IMPS.
        WA_IMPS-DOWNLOAD_STATUS = TEMP_STATUS_IMPS(120).
        CLEAR D.
        D = TEMP_STATUS_IMPS.
        SHIFT D BY 120 PLACES.
        WA_IMPS-DOWNLOAD_STATUS1 = D.

        READ TABLE IT_HSBC_ITEMS INTO WA_HSBC_ITEMS WITH KEY BELNR = WA_IMPS-BELNR.

        IF SY-SUBRC = 0.

          WA_HSBC_ITEMS-REX_BATCH =  BATCH.
          WA_HSBC_ITEMS-REX_XMLNA =  FILE_NAME_IMPS.
          WA_HSBC_ITEMS-REX_DATE = SY-DATUM.
          WA_HSBC_ITEMS-REX_TIME = SY-UZEIT.
          WA_HSBC_ITEMS-REX_USER = SY-UNAME.

          UPDATE ZHSBC_ITEMS FROM WA_HSBC_ITEMS.

        ENDIF.

        WA_IMPS-REX_NO = WA_HSBC_ITEMS-REX_NO + 1.

        IF BACK_SCREEN = '102' .
          WA_IMPS-METHOD = 'VE'.
        ELSEIF BACK_SCREEN = '104'.
          WA_IMPS-METHOD = 'GL'.
        ENDIF.

        INSERT INTO  ZHSBC_ITEMS VALUES WA_IMPS.
        CLEAR WA_XML.
        CLEAR WA_HSBC_ITEMS.
      ENDLOOP.


    ENDIF.

  ENDIF.

ENDFORM.                    " TABLE_UPDATE_IMPS
