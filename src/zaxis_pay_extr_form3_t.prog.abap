*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTR_FORM3
*======================================================================*
*       text          EXTRACTION SUB FORMS
*======================================================================*


FORM ZAXIS_036_HDR USING L_FIELD
                         BSAK   STRUCTURE BSAK
                         REGUH  STRUCTURE REGUH
                         PAYR   STRUCTURE PAYR
                         LFA1   STRUCTURE LFA1
                         LFB1   STRUCTURE LFB1
                         ADRC   STRUCTURE ADRC
                         T001   STRUCTURE T001
                         BNKA   STRUCTURE BNKA
                         LFBK   STRUCTURE LFBK
                         T012   STRUCTURE T012
                         T012K  STRUCTURE T012K
                         BSEC   STRUCTURE BSEC
                         BSEG   STRUCTURE BSEG
                         ZAXIS_TAB_PAYLOC STRUCTURE ZAXIS_TAB_PAYLOC
                        ZAXIS_STR_HEADER STRUCTURE ZAXIS_STR_HEADER.

  DATA : L_FUN_FIELD(250),
         LV_GJAHR TYPE GJAHR.
  DATA : WA_T005U TYPE T005U.
* PLACE A BANK CODE IN A FIELD SYMBOL.

  CLEAR : WA_ZAXIS_TAB_MAP , LV_GJAHR.

  LOOP AT G_TAB_ZAXIS_TAB_MAP INTO WA_ZAXIS_TAB_MAP
                              WHERE FIELDNAME = L_FIELD
                                AND RECORD_TYPE = 'H'.

    IF WA_ZAXIS_TAB_MAP-SOURCE_TYPE = 'TABLE' .

      SPLIT WA_ZAXIS_TAB_MAP-SOURCE_VALUE AT '-' INTO G_TABLENAME G_TABLE_FIELDNAME.

* FIELD-SYMBOLS: FOR DYNAMIC FIELD ALLOCATION.
* ASSIGN THE TABLE NAME ALONE TO A FIELD SYMBOL .

      ASSIGN (G_TABLENAME) TO <FS_TABLE_NAME>.

      IF SY-SUBRC = 0.

* ASSIGN THE FIELD NAME WITH THE STRUCTURE TO A FIELD SYMBOL.

        ASSIGN ZAXIS_STR_HEADER TO <FS_ZAXIS_STR_DETAIL>.

        IF SY-SUBRC = 0.

          ASSIGN COMPONENT G_TABLE_FIELDNAME OF STRUCTURE <FS_TABLE_NAME> TO <FS_TABLE_FIELD>.

          IF SY-SUBRC = 0.

            CLEAR G_FIELD_VALUE.
            ASSIGN COMPONENT L_FIELD OF STRUCTURE <FS_ZAXIS_STR_DETAIL> TO <FS_FIELD_VALUE>.
            G_FIELD_VALUE  = <FS_TABLE_FIELD>.

            IF G_FIELD_VALUE IS NOT INITIAL.
              MOVE G_FIELD_VALUE TO <FS_FIELD_VALUE>.
            ENDIF.

          ENDIF.
* ASSIGN THE HEADER STRUCTURE TO A FIELD SYMBOL

          ASSIGN ZAXIS_STR_HEADER TO <FS_ZAXIS_STR_DETAIL>.

          IF SY-SUBRC = 0.

* DATE CONVERSION BASED ON USER NEEDS (DD/MM/YYYY)

            PERFORM FORMAT_CONVERSION USING
                                            L_FIELD
                                            G_TABLENAME
                                            G_TABLE_FIELDNAME.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF WA_ZAXIS_TAB_MAP-SOURCE_TYPE = 'FUNCTION'.

**        ASSIGN THE HEADER STRUCTURE TO A FIELD SYMBOL

      ASSIGN ZAXIS_STR_HEADER TO <FS_ZAXIS_STR_DETAIL>.

* ASSIGN THE HEADER STRUCTURE  field TO A FIELD SYMBOL

      ASSIGN COMPONENT L_FIELD OF STRUCTURE <FS_ZAXIS_STR_DETAIL> TO <FS_FIELD_VALUE>.

      IF SY-SUBRC = 0.

        CASE WA_ZAXIS_TAB_MAP-SOURCE_VALUE.

*FUNCTION: CALCULATE THE CUSTOMER REFERENCE

          WHEN 'CL_CUST_REFRENCE'.

            CLEAR : L_FUN_FIELD , LV_GJAHR.

            IF BSAK-GJAHR IS INITIAL.
              CALL FUNCTION 'FI_PERIOD_DETERMINE'
                EXPORTING
                  I_BUDAT        = BKPF-BUDAT
                  I_BUKRS        = BSAK-BUKRS
                  I_PERIV        = T001-PERIV
                IMPORTING
                  E_GJAHR        = LV_GJAHR
                EXCEPTIONS
                  FISCAL_YEAR    = 1
                  PERIOD         = 2
                  PERIOD_VERSION = 3
                  POSTING_PERIOD = 4
                  SPECIAL_PERIOD = 5
                  VERSION        = 6
                  POSTING_DATE   = 7
                  OTHERS         = 8.
            ELSE.
              LV_GJAHR = BSAK-GJAHR.
            ENDIF.

            DATA : LV_BUKRS1(4)   TYPE C,
                   LV_STR_LEN1(2) TYPE N,
                   LV_TEMP1(2)    TYPE N.

            CLEAR : LV_BUKRS1,
                    LV_STR_LEN1,
                    LV_TEMP1.

            LV_BUKRS1    = BSAK-BUKRS.
            LV_STR_LEN1  = STRLEN( BSAK-BUKRS ).
            LV_TEMP1     = LV_STR_LEN1 - 4.

            IF LV_STR_LEN1 < 4.
              DO LV_TEMP1 TIMES.
                CONCATENATE '-' LV_BUKRS1 INTO LV_BUKRS1.
              ENDDO.
            ENDIF.
            CONCATENATE BSAK-BELNR LV_BUKRS1 LV_GJAHR INTO L_FUN_FIELD.


            IF L_FUN_FIELD IS NOT INITIAL.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
            ENDIF.

            CLEAR : LV_BUKRS1,
                    LV_STR_LEN1,
                    LV_TEMP1.

**** POPULATING USER ID:

          WHEN 'CL_USER_ID'.

            L_FUN_FIELD = SY-UNAME.

            IF L_FUN_FIELD IS NOT INITIAL.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
            ENDIF.



****** populating micr code"""""""

          WHEN 'CL_MICR_CODE'.

            L_FUN_FIELD = LV_MICR.

            IF L_FUN_FIELD IS NOT INITIAL.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
            ENDIF.

            CLEAR : LV_MICR.


*FUNCTION: GET THE EMAIL ID

          WHEN 'CL_GET_EMAIL_ID'.

            CLEAR L_FUN_FIELD.

* GET THE VENDOR EMAIL ID FROM ADR6 TABLE.

            PERFORM FILL_EMAIL_ID USING LFA1-ADRNR CHANGING L_FUN_FIELD.

            IF L_FUN_FIELD IS NOT INITIAL.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
              G_FIELD_CONCATENATE = L_FUN_FIELD.
            ENDIF.

          WHEN 'CL_GET_STATE'.
            SELECT * UP TO 1 ROWS FROM T005U INTO WA_T005U WHERE LAND1 = ADRC-COUNTRY
                                                     AND   BLAND = ADRC-REGION ORDER BY PRIMARY KEY.
            ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
            IF SY-SUBRC EQ 0.
              L_FUN_FIELD = WA_T005U-BEZEI.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
            ENDIF.

          WHEN 'CL_GET_CORPCODE'.
***            DATA : WA_ZAXIS_TAB_CONVER TYPE ZAXIS_TAB_CONVER,
***                   LV_CORPCODE         TYPE CHAR25.
***            CONCATENATE P_BUKRS S_HBKID-LOW INTO LV_CORPCODE.
***
***            SELECT SINGLE * FROM ZAXIS_TAB_CONVER INTO WA_ZAXIS_TAB_CONVER
***            WHERE FIELDNAME = 'CORPCODE' AND OLDVALUE = LV_CORPCODE.
***            IF SY-SUBRC EQ 0.
***              L_FUN_FIELD = WA_ZAXIS_TAB_CONVER-NEWVALUE.
***              <FS_FIELD_VALUE> = L_FUN_FIELD.
***            ENDIF.
***            CLEAR WA_ZAXIS_TAB_CONVER.


          WHEN 'CL_CORP_EMAIL'.
            CLEAR L_FUN_FIELD.
            CLEAR : WA_CONVERS.
            READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                                    BANK_CODE = C_BANK_CODE
                                                    RECORD_TYPE = 'H'
                                                    FIELDNAME = 'CORP_EMAIL'
                                                    OLDVALUE  = 'FIELD38'.
            IF SY-SUBRC = 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.
              CONDENSE WA_CONVERS-NEWVALUE NO-GAPS.
              L_FUN_FIELD = WA_CONVERS-NEWVALUE.
            ENDIF.

            IF L_FUN_FIELD IS NOT INITIAL.
              <FS_FIELD_VALUE> = L_FUN_FIELD.
            ENDIF.


        ENDCASE.
      ENDIF.

    ELSEIF WA_ZAXIS_TAB_MAP-SOURCE_TYPE IS INITIAL AND WA_ZAXIS_TAB_MAP-MANDATORY <> 'X'.

      ASSIGN COMPONENT L_FIELD OF STRUCTURE <FS_ZAXIS_STR_DETAIL> TO <FS_FIELD_VALUE>.

      IF SY-SUBRC = 0 AND WA_ZAXIS_TAB_MAP-CONSTANTVALUE IS NOT INITIAL.

        G_CONSTANT_VALUE = WA_ZAXIS_TAB_MAP-CONSTANTVALUE.

        IF G_FIELD_CONCATENATE IS NOT INITIAL.
          CONCATENATE G_FIELD_CONCATENATE G_CONSTANT_VALUE INTO G_FIELD_CONCATENATE SEPARATED BY SPACE.
        ELSE.
          G_FIELD_CONCATENATE = G_CONSTANT_VALUE.
        ENDIF.

        MOVE G_FIELD_CONCATENATE TO <FS_FIELD_VALUE>.

      ENDIF.

    ENDIF.

*ADD PREFIX TO THE FIELD GIVEN

    IF WA_ZAXIS_TAB_MAP-PREFIX IS NOT INITIAL AND <FS_FIELD_VALUE> IS NOT INITIAL.

      CLEAR G_FIELD_VALUE.
      G_FIELD_VALUE = <FS_FIELD_VALUE>.
      CONCATENATE WA_ZAXIS_TAB_MAP-PREFIX G_FIELD_VALUE INTO G_FIELD_VALUE SEPARATED BY SPACE.
      MOVE G_FIELD_VALUE TO <FS_FIELD_VALUE>.

    ENDIF.

*ADD THE SUFFIX FROM THE TABLE TO THE GIVEN FIELD

    IF WA_ZAXIS_TAB_MAP-SUFFIX IS NOT INITIAL  AND <FS_FIELD_VALUE> IS NOT INITIAL.

      CLEAR G_FIELD_VALUE.
      G_FIELD_VALUE = <FS_FIELD_VALUE>.
      CONCATENATE G_FIELD_VALUE WA_ZAXIS_TAB_MAP-PREFIX INTO G_FIELD_VALUE SEPARATED BY SPACE.
      MOVE G_FIELD_VALUE TO <FS_FIELD_VALUE>.

    ENDIF.

    UNASSIGN: <FS_FIELD_VALUE>.        "clear the field symbol

  ENDLOOP.

  CLEAR: G_FIELD_CONCATENATE, G_FIELD_CHECK.

ENDFORM.                    "Zaxis_036_HDR

*&---------------------------------------------------------------------*
*&      Form  zaxis_036_dtl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_INFO          text
*      -->L_FIELD          text
*      -->BSAK             text
*      -->REGUH            text
*      -->PAYR             text
*      -->LFA1             text
*      -->LFB1             text
*      -->ADRC             text
*      -->T001             text
*      -->BNKA             text
*      -->LFBK             text
*      -->T012             text
*      -->T012K            text
*      -->BSEC             text
*      -->BSEG             text
*      -->REGUP            text
*      -->Zaxis_STR_HEADER  text
*      -->Zaxis_STR_DETAIL  text
*----------------------------------------------------------------------*
FORM ZAXIS_036_DTL USING L_FIELD
                         BSAK   STRUCTURE BSAK
                         REGUH  STRUCTURE REGUH
                         PAYR   STRUCTURE PAYR
                         LFA1   STRUCTURE LFA1
                         LFB1   STRUCTURE LFB1
                         ADRC   STRUCTURE ADRC
                         T001   STRUCTURE T001
                         BNKA   STRUCTURE BNKA
                         LFBK   STRUCTURE LFBK
                         T012   STRUCTURE T012
                         T012K  STRUCTURE T012K
                         BSEC   STRUCTURE BSEC
                         BSEG   STRUCTURE BSEG
                         REGUP  STRUCTURE REGUP
                         *BSAK  STRUCTURE BSAK
                        ZAXIS_STR_HEADER STRUCTURE ZAXIS_STR_HEADER
                        ZAXIS_TAB_SEP     STRUCTURE  ZAXIS_TAB_SEP
                        ZAXIS_STR_DETAIL  STRUCTURE ZAXIS_STR_DETAIL.



  DATA: L_FUN_FIELD(30),
        L_TDS_VALUE LIKE BSAK-QBSHB,
        L_TDS_VALUE1(20),
        L_TOTAL_TDS LIKE WITH_ITEM-WT_QBSHH.


*PAYMENT DOCUMENT NO FOR THE PARTIAL PAYMENTS

  LOOP AT G_TAB_ZAXIS_TAB_MAP INTO WA_ZAXIS_TAB_MAP
                               WHERE FIELDNAME = L_FIELD
                                 AND RECORD_TYPE = 'D'.



    CLEAR G_TABLENAME.

    IF WA_ZAXIS_TAB_MAP-SOURCE_TYPE = 'TABLE'.

      SPLIT WA_ZAXIS_TAB_MAP-SOURCE_VALUE AT '-' INTO G_TABLENAME G_TABLE_FIELDNAME.

* ASSIGN THE TABLE NAME ALONE TO A FIELD SYMBOL.

      ASSIGN (G_TABLENAME) TO <FS_TABLE_NAME>.

      IF SY-SUBRC = 0.

* ASSIGN THE FIELD NAME WITH THE STRUCTURE TO A FIELD SYMBOL.

        ASSIGN COMPONENT G_TABLE_FIELDNAME OF STRUCTURE <FS_TABLE_NAME> TO <FS_TABLE_FIELD>.

        IF SY-SUBRC = 0.

          ASSIGN ZAXIS_STR_DETAIL TO <FS_ZAXIS_STR_DETAIL>.

          IF SY-SUBRC = 0.

            PERFORM FORMAT_CONVERSION
                                    USING
                                            L_FIELD
                                            G_TABLENAME
                                            G_TABLE_FIELDNAME.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF WA_ZAXIS_TAB_MAP-SOURCE_TYPE = 'FUNCTION'.

      ASSIGN ZAXIS_STR_DETAIL TO <FS_ZAXIS_STR_DETAIL>.

      IF SY-SUBRC = 0.

        ASSIGN COMPONENT L_FIELD OF STRUCTURE <FS_ZAXIS_STR_DETAIL> TO <FS_FIELD_VALUE>.

        IF SY-SUBRC = 0.

          CASE WA_ZAXIS_TAB_MAP-SOURCE_VALUE.

* FUNCTION: GET THE GROSS VALUE

            WHEN 'CL_GET_GROSS_VALUE'.

              CLEAR L_FUN_FIELD.

              PERFORM GET_GROSS_VALUE USING BSAK-DMBTR BSAK-QBSHB CHANGING L_FUN_FIELD.

              REPLACE ALL OCCURRENCES OF ',' IN L_FUN_FIELD WITH '.'.  "Code added for "." separation

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  VALUE = L_FUN_FIELD.

              IF L_FUN_FIELD IS NOT INITIAL.
                <FS_FIELD_VALUE> = L_FUN_FIELD.
              ENDIF.

****  populating Seperater

            WHEN 'CL_GET_SEP'.

              L_FUN_FIELD = '/'.

              IF L_FUN_FIELD IS NOT INITIAL.
                <FS_FIELD_VALUE> = L_FUN_FIELD.
              ENDIF.



* FUNCTION: GET THE NET VALUE

            WHEN 'CL_GET_NET_VALUE'.

              CLEAR L_FUN_FIELD.

              PERFORM GET_NET_VALUE USING BSAK-DMBTR BSAK-QBSHB CHANGING L_FUN_FIELD.

              REPLACE ALL OCCURRENCES OF ',' IN L_FUN_FIELD WITH '.'.  "Code added for "." separation

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  VALUE = L_FUN_FIELD.

              IF L_FUN_FIELD IS NOT INITIAL.
                <FS_FIELD_VALUE> = L_FUN_FIELD.
              ENDIF.


            WHEN 'TDS_VALUE'.
              CLEAR: L_FUN_FIELD, L_TOTAL_TDS,L_TDS_VALUE1,L_TDS_VALUE.

              LOOP AT G_TAB_WITH_ITEM INTO WA_TAB_WITH_ITEM
                  WHERE   BELNR = BSAK-BELNR AND
                          BUKRS = BSAK-BUKRS AND
                          GJAHR = BSAK-GJAHR.

                L_TOTAL_TDS = L_TOTAL_TDS + WA_TAB_WITH_ITEM-WT_QBSHH.
              ENDLOOP.

              IF BSAK1-QBSHB IS INITIAL.

                L_TDS_VALUE = L_TOTAL_TDS.
                L_TDS_VALUE1 = L_TDS_VALUE."#EC CI_FLDEXT_OK[2610650]
"Added by SPLABAP during code remediation

              ELSE.

                L_TDS_VALUE = BSAK-QBSHB.
                L_TDS_VALUE = L_TDS_VALUE * ( -1 ).
                L_TDS_VALUE1 = L_TDS_VALUE."#EC CI_FLDEXT_OK[2610650]
"Added by SPLABAP during code remediation

              ENDIF.

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  VALUE = L_TDS_VALUE1.

              L_FUN_FIELD =  L_TDS_VALUE1.
              REPLACE ALL OCCURRENCES OF ',' IN L_FUN_FIELD WITH '.'.  "Code added for "." separation

              IF L_FUN_FIELD IS NOT INITIAL.
                CONDENSE L_FUN_FIELD NO-GAPS.
                <FS_FIELD_VALUE> = L_FUN_FIELD.
              ENDIF.

              CLEAR: L_TOTAL_TDS,L_TDS_VALUE1,L_TDS_VALUE.

            WHEN 'CASH_DISCOUNT'.

              CLEAR: L_FUN_FIELD, L_TOTAL_TDS,L_TDS_VALUE1,L_TDS_VALUE.

              L_TDS_VALUE = BSAK-SKNTO.
              L_TDS_VALUE = L_TDS_VALUE * ( -1 ).
              L_FUN_FIELD = L_TDS_VALUE.

              REPLACE ALL OCCURRENCES OF ',' IN L_FUN_FIELD WITH '.'.  "Code added for "." separation

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  VALUE = L_FUN_FIELD.

              IF L_FUN_FIELD IS NOT INITIAL.
                CONDENSE L_FUN_FIELD NO-GAPS.
                <FS_FIELD_VALUE> = L_FUN_FIELD.
              ENDIF.

              CLEAR: L_TDS_VALUE1,L_TDS_VALUE.

          ENDCASE.

        ENDIF.

      ENDIF.

      UNASSIGN: <FS_FIELD_VALUE>.      "clear the field symbol
**      CLEAR WA_ZAXIS_TAB_MAP.           "clear the workarea

    ENDIF.

  ENDLOOP.

  CLEAR G_FIELD_CONCATENATE.

ENDFORM.                    "Zaxis_036_DTL

*&---------------------------------------------------------------------*
*&      Form  get_gross_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NET_AMT    text
*      -->TDS        text
*      -->GROSS_AMT  text
*----------------------------------------------------------------------*
FORM GET_GROSS_VALUE  USING  NET_AMT
                             TDS
                           CHANGING
                             GROSS_AMT.

  DATA :  L_GROSS TYPE DMBTR,
          L_SIGN_CHANGE1(20),
          LV_TDS TYPE QBSHB.

  LV_TDS = TDS.

  IF LV_TDS IS INITIAL.

    IF G_TAB_WITH_ITEM[] IS NOT INITIAL.

* FOR GETTING THE TDS VALUE.

      LOOP AT G_TAB_WITH_ITEM INTO WA_TAB_WITH_ITEM
                    WHERE   BELNR = BSAK-BELNR AND
                            BUKRS = BSAK-BUKRS AND
                            GJAHR = BSAK-GJAHR.

        LV_TDS = LV_TDS + WA_TAB_WITH_ITEM-WT_QBSHH.

      ENDLOOP.

      LV_TDS = LV_TDS * -1 .

    ENDIF.

  ENDIF.

  IF BSAK1-QBSHB IS INITIAL.

    L_GROSS   = NET_AMT + LV_TDS.

  ELSE.

    L_GROSS   = NET_AMT.

  ENDIF.

**  IF BSAK-REBZG <> ' '.
**    L_GROSS = 0.
**  ENDIF.

  IF BSAK-SHKZG  = 'S'.

    IF BSAK-AUGBL EQ BSAK-BELNR.

      WRITE L_GROSS TO GROSS_AMT CURRENCY 'INR' NO-SIGN NO-GROUPING LEFT-JUSTIFIED.

    ELSE.

      L_GROSS = L_GROSS * -1.
"Added by SPLABAP during code remediation
      L_SIGN_CHANGE1 = L_GROSS."#EC CI_FLDEXT_OK[2610650]
"Added by SPLABAP during code remediation

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = L_SIGN_CHANGE1.

      WRITE L_SIGN_CHANGE1 TO GROSS_AMT CURRENCY 'INR' NO-GROUPING LEFT-JUSTIFIED.

    ENDIF.

  ELSE.

    WRITE L_GROSS TO GROSS_AMT CURRENCY 'INR' NO-SIGN NO-GROUPING LEFT-JUSTIFIED.

  ENDIF.

  CLEAR : L_SIGN_CHANGE1 , L_GROSS.

ENDFORM.                    " get_net_value


*&---------------------------------------------------------------------*
*&      Form  date_convert_ddmmyy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Zaxis_STR_HEADER_FIELD10  text
*----------------------------------------------------------------------*
FORM DATE_CONVERT_DDMMYY  USING    DATE.

  DATA : BEGIN OF L_DATE,
          Y(4),
          M(2),
          D(2),
         END OF L_DATE.

  L_DATE = DATE.
  CLEAR DATE.

  IF L_DATE NE '00000000'.
    "CONCATENATE L_DATE-D L_DATE-M L_DATE-Y INTO DATE SEPARATED BY '/'.
    CONCATENATE  L_DATE-Y L_DATE-M  L_DATE-D INTO DATE SEPARATED BY '-'.
  ELSE.
    DATE = ' '.
  ENDIF.

ENDFORM.                    " date_convert_ddmmyy
*&---------------------------------------------------------------------*
*&      Form  fill_email_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFA1_ADRNR  text
*      <--P_Zaxis_STR_HEADER_FIELD63  text
*----------------------------------------------------------------------*
FORM FILL_EMAIL_ID  USING    ADRNR TYPE AD_ADDRNUM
                    CHANGING L_FUN_FIELD TYPE ANY.

  DATA : L_LINE TYPE I,
         L_LINE1 TYPE I,
         L_WA_ADR6 TYPE ADR6,
         L_TAB_ADR6 TYPE TABLE OF ADR6,
         L_TAB_CONVERS TYPE TABLE OF ZAXIS_TAB_CONVER,
         L_WA_CONVERS TYPE ZAXIS_TAB_CONVER,
         L_POS TYPE I,
         L_STRLEN TYPE I.

  DATA : WA_USR21 TYPE USR21,
         WA_ADR6  TYPE ADR6.
* FETCH THE EMAIL ID FROM ADR6

  IF G_FIELD_CONCATENATE IS INITIAL.
******-------------fetch corporate mail id-----------------------------------------*****
    CLEAR : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                            BANK_CODE = C_BANK_CODE
                                            RECORD_TYPE = 'H'
                                            FIELDNAME = 'CORP_EMAIL'
                                            OLDVALUE  = 'FIELD26'.
    IF SY-SUBRC = 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.

      CONDENSE WA_CONVERS-NEWVALUE NO-GAPS.

      IF L_FUN_FIELD IS INITIAL.
        CONCATENATE L_FUN_FIELD WA_CONVERS-NEWVALUE INTO L_FUN_FIELD.
      ELSE.
        CONCATENATE L_FUN_FIELD WA_CONVERS-NEWVALUE INTO L_FUN_FIELD SEPARATED BY '|'.
      ENDIF.

    ENDIF.

******-------------to fetch USer mail id-----------------------------------------*****

    CLEAR : WA_USR21.
    READ TABLE G_TAB_USR21 INTO WA_USR21 WITH KEY BNAME = BKPF-USNAM.

    IF SY-SUBRC = 0.
      READ TABLE G_TAB_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_USR21-ADDRNUMBER
                                            PERSNUMBER = WA_USR21-PERSNUMBER.
      IF SY-SUBRC = 0 AND WA_ADR6-SMTP_ADDR IS NOT INITIAL.

        CONDENSE WA_ADR6-SMTP_ADDR NO-GAPS.
        IF L_FUN_FIELD IS INITIAL.
          CONCATENATE L_FUN_FIELD WA_ADR6-SMTP_ADDR INTO L_FUN_FIELD.
        ELSE.
          CONCATENATE L_FUN_FIELD WA_ADR6-SMTP_ADDR INTO L_FUN_FIELD SEPARATED BY '|'.
        ENDIF.

      ENDIF.
    ENDIF.


******-------------to fetch Vendor mail id-----------------------------------------*****

    CLEAR : L_POS , L_STRLEN .

*    SELECT * FROM ADR6 INTO TABLE L_TAB_ADR6
*                            WHERE ADDRNUMBER = ADRNR.

    LOOP AT G_TAB_ADR6 INTO L_WA_ADR6 WHERE ADDRNUMBER = ADRNR.
      APPEND L_WA_ADR6 TO L_TAB_ADR6.
    ENDLOOP.

    DESCRIBE TABLE L_TAB_ADR6 LINES L_LINE.

    LOOP AT L_TAB_ADR6 INTO L_WA_ADR6.

      CLEAR : L_POS, L_STRLEN.
      L_STRLEN = STRLEN( L_WA_ADR6-SMTP_ADDR ).

      DO L_STRLEN TIMES.

        IF L_WA_ADR6-SMTP_ADDR+L_POS(1) CO ''''.
          L_WA_ADR6-SMTP_ADDR+L_POS(1) = ' '.
        ENDIF.
        L_POS = L_POS + 1.

      ENDDO.

      CONDENSE L_WA_ADR6-SMTP_ADDR NO-GAPS.

      IF L_FUN_FIELD IS INITIAL.
        CONCATENATE L_FUN_FIELD L_WA_ADR6-SMTP_ADDR INTO L_FUN_FIELD.
      ELSE.
        CONCATENATE L_FUN_FIELD L_WA_ADR6-SMTP_ADDR INTO L_FUN_FIELD SEPARATED BY '|'.
      ENDIF.

    ENDLOOP.

  ELSE.

    CONCATENATE G_FIELD_CONCATENATE G_FIELD_CHECK  INTO G_FIELD_CONCATENATE SEPARATED BY ','.
    MOVE G_FIELD_CONCATENATE TO <FS_FIELD_VALUE>.

  ENDIF.

ENDFORM.                    " fill_email_id

*&---------------------------------------------------------------------*
*&      Form  get_net_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_*BSAK_DMBTR  text
*      -->P_*BSAK_QBSHB  text
*      <--P_L_FUN_FIELD  text
*----------------------------------------------------------------------*
FORM GET_NET_VALUE  USING    L_NET_AMT
                             L_TDS
                    CHANGING NET_AMT.
  DATA: L_NET TYPE DMBTR,
        L_SIGN_CHANGE(20).

  IF BSAK1-QBSHB IS INITIAL.

    L_NET = L_NET_AMT.

  ELSE.

    L_NET = L_NET_AMT - L_TDS.

  ENDIF.

  IF BSAK-SHKZG  = 'S'.

    IF BSAK-AUGBL EQ BSAK-BELNR.

      WRITE L_NET TO NET_AMT CURRENCY 'INR' NO-SIGN NO-GROUPING LEFT-JUSTIFIED.

    ELSE.

      L_NET = L_NET * -1.
      L_SIGN_CHANGE = L_NET."#EC CI_FLDEXT_OK[2610650]
"Added by SPLABAP during code remediation

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = L_SIGN_CHANGE.

      WRITE L_SIGN_CHANGE TO NET_AMT CURRENCY 'INR' NO-GROUPING LEFT-JUSTIFIED.

    ENDIF.

  ELSE.

    WRITE L_NET TO NET_AMT CURRENCY 'INR' NO-SIGN NO-GROUPING LEFT-JUSTIFIED.

  ENDIF.

*&---------------------------------------------------------------------*
*       minus the Cash discount from net amount
*----------------------------------------------------------------------*
  IF BSAK-SKNTO IS NOT INITIAL.
    NET_AMT = NET_AMT - BSAK-SKNTO.
  ENDIF.

  CLEAR : L_SIGN_CHANGE , L_NET.

ENDFORM.                    " get_net_value
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_TABLENAME  text
*      -->P_G_TABLE_FIELDNAME  text
*----------------------------------------------------------------------*
FORM FORMAT_CONVERSION  USING
                               P_L_FIELD
                               P_G_TABLENAME
                               P_G_TABLE_FIELDNAME.


  CLEAR: WA_DFIES, G_FIELD_VALUE.
  REFRESH G_TAB_DFIES.

  G_FIELD_VALUE = <FS_TABLE_FIELD>.   "* PASS THE VALUE OF A CORRESPONDING FIELD TO A VARIABLE.

  CONDENSE G_FIELD_VALUE.
  ASSIGN COMPONENT P_L_FIELD OF STRUCTURE <FS_ZAXIS_STR_DETAIL> TO <FS_FIELD_VALUE>.

  IF SY-SUBRC = 0.

* FM: TO GET THE DATA TYPE OF A FIELD

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = P_G_TABLENAME
        FIELDNAME      = P_G_TABLE_FIELDNAME
      TABLES
        DFIES_TAB      = G_TAB_DFIES
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.



* IF THE DATA TYPE IS DATS (DATE) THEN PERFORM THE BELOW SUBROUTINE.
* NEED: CHANGE THE DATE FORMAT AS DD/MM/YYYY.

    LOOP AT G_TAB_DFIES INTO WA_DFIES.

      IF WA_DFIES-DATATYPE = 'DATS'.
        PERFORM DATE_CONVERT_DDMMYY USING G_FIELD_VALUE.        "to change the date format
        <FS_FIELD_VALUE> = G_FIELD_VALUE.

      ELSEIF G_FIELD_VALUE IS NOT INITIAL.

        <FS_FIELD_VALUE> = G_FIELD_VALUE.

      ENDIF.
    ENDLOOP.



    IF G_FIELD_VALUE IS NOT INITIAL AND WA_ZAXIS_TAB_MAP-RECORD_TYPE = 'H'.

      IF G_FIELD_VALUE IS NOT INITIAL.

        CONDENSE G_FIELD_VALUE .
        MOVE G_FIELD_VALUE TO <FS_FIELD_VALUE>.

      ELSE.

        UNASSIGN:  <FS_FIELD_VALUE>.
        MOVE '' TO <FS_OUT> .

      ENDIF.
    ENDIF.


* CONCATENATE: A FIELD CONTAINS MORE THAN ONE SEQUENCE NUMBER.
* THEN POPULATE ALL THE VALUE IN A FIRST FIELD.

    IF G_FIELD_CONCATENATE IS NOT INITIAL .

      CONCATENATE G_FIELD_CONCATENATE G_FIELD_VALUE  INTO G_FIELD_CONCATENATE SEPARATED BY SPACE.
      MOVE G_FIELD_CONCATENATE TO <FS_FIELD_VALUE>.
      CONDENSE G_FIELD_CONCATENATE.

    ELSEIF G_FIELD_VALUE IS INITIAL.

      G_FIELD_CONCATENATE = G_FIELD_CHECK.

    ELSE.

      G_FIELD_CONCATENATE = G_FIELD_VALUE.

    ENDIF.
  ENDIF.
ENDFORM.                    " date_conversion



*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  AUTO_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTO_PROCESS .


  DATA: WA_INDI TYPE ZAXIS_TAB_CONVER-NEWVALUE,
        WA_INDI2 TYPE ZAXIS_TAB_CONVER-NEWVALUE.

  SELECT NEWVALUE UP TO 1 ROWS FROM ZAXIS_TAB_CONVER INTO WA_INDI WHERE OLDVALUE = 'PUSH' ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  IF SY-SUBRC = 0.

    IF WA_INDI = 'Y'.

      SELECT NEWVALUE UP TO 1 ROWS FROM ZAXIS_TAB_CONVER INTO WA_INDI2 WHERE OLDVALUE = 'IND' ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      IF SY-SUBRC = 0.


        """"  processing automation process by calling submit progarm.

        SUBMIT ZAXIS_PROPERTY  WITH  P_IND   = WA_INDI2 AND RETURN.
*                               WITH  p_bukrs = p_bukrs  AND RETURN.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " AUTO_PROCESS
