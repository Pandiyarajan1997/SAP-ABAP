REPORT ZTESTING_SO_NEW3
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TABLES: T100.
TYPE-POOLS: TRUXS.

TYPES: BEGIN OF TY_KNVP,
        KUNNR TYPE KNVP-KUNNR,
        VKORG TYPE KNVP-VKORG,
        VTWEG TYPE KNVP-VTWEG,
        SPART TYPE KNVP-SPART,
        PARVW TYPE KNVP-PARVW,
        PERNR TYPE KNVP-PERNR,
      END OF TY_KNVP.

TYPES: BEGIN OF IG_SALE,
        KUNNR TYPE KNVP-KUNNR, " Customer Number
        VKORG TYPE KNVP-VKORG, " Sales Organization
        VTWEG TYPE KNVP-VTWEG, " Distribution Channel
        SPART TYPE KNVP-SPART, " Division
        PARVW TYPE KNVP-PARVW, " Partner Function
        KUNN2 TYPE KNVP-KUNN2, " Customer number of business partner
        PERNR TYPE KNVP-PERNR, " Personnel Number
      END OF IG_SALE.


DATA : IT_SALE TYPE TABLE OF IG_SALE,
       WA_SALE TYPE IG_SALE .

TYPES : BEGIN OF TY_XK02 ,
          KUNNR TYPE KNVP-KUNNR,
          BUKRS(4) TYPE C,
          VKORG(4) TYPE C,
          VTWEG(2) TYPE C,
          SPART(2) TYPE C,
          PERNR TYPE PA0001-PERNR,
          PARVW TYPE KNVP-PARVW,
        END OF TY_XK02.

TYPES: BEGIN OF TY_REP,
    RNO(2),
    KUNNR TYPE KNVP-KUNNR,
    BUKRS TYPE BUKRS,
    VKORG TYPE KNVP-VKORG,
    VTWEG TYPE KNVP-VTWEG,
    SPART TYPE KNVP-SPART,
    PERNR TYPE PA0001-PERNR,
    PARVW TYPE KNVP-PARVW,
    E TYPE STRING,
END OF TY_REP,

BEGIN OF TY_CHK,
          KUNNR TYPE KNVP-KUNNR," Customer Number
          BUKRS(4) TYPE C, " Company Code
          VKORG(4) TYPE C, " Sales Organization
          VTWEG(2) TYPE C, " Distribution Channel
          SPART(2) TYPE C, " Division
END OF TY_CHK.

TYPES: BEGIN OF TY_KNVP1,
        KUNNR TYPE KNVP-KUNNR,
        VKORG TYPE KNVP-VKORG,
        VTWEG TYPE KNVP-VTWEG,
        SPART TYPE KNVP-SPART,
        PARVW TYPE KNVP-PARVW,
      END OF TY_KNVP1.

DATA: LV_TAB TYPE SY-TABIX,
      LV_TAB1 TYPE SY-TABIX,
      LV_TABIX(2) TYPE N,
      LV_INDEX TYPE SY-TABIX.
DATA: C(4) VALUE '''',
  A(1) VALUE '(' ,
  B(1) VALUE ')'.


DATA : LV_KUNNR TYPE KNVP-KUNNR,
       LV_KUNNRO TYPE KNVP-KUNNR,
       GS_CHKUN TYPE TY_CHK.
DATA : LV_NUM TYPE STRING.
DATA : LV_NUM1 TYPE STRING.
DATA : LV_NUM2 TYPE STRING.
DATA : LV_TES(17),
       LV_MESS TYPE STRING,
       LV_MTEXT TYPE STRING,
       LV_ERR TYPE STRING VALUE 'Record failed to update. '.

DATA : IT_XK02 TYPE TABLE OF TY_XK02,
       WA_XK02 TYPE TY_XK02,
       IT_XK03 TYPE TABLE OF TY_XK02,
       WA_XK03 TYPE TY_XK02,
       GT_KNVP1 TYPE TABLE OF TY_KNVP1,
       GS_KNVP1 TYPE TY_KNVP1.

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA : IT_DYNPFIELDS 	TYPE STANDARD TABLE OF DYNPREAD .

DATA:   IT_BDCDATA LIKE BDCDATA  OCCURS 0 WITH HEADER LINE.

DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
     BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
      EXP_CMNMSG_05 TYPE C ,
       C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE ,
       LV_MSG TYPE STRING,
       LV_RNO TYPE SY-TABIX,
       LV_REC TYPE SY-TABIX,
       LV_NOS TYPE SY-TABIX,
       LV_DT TYPE I,
       LV_DT1 TYPE I,
       LV_CHK,
       GT_REP TYPE  TABLE OF TY_REP,
       GS_REP TYPE TY_REP,
       GT_ERR TYPE TABLE OF TY_XK02,
       GS_ERR TYPE TY_XK02,
       GT_KNVP TYPE TABLE OF TY_KNVP,
       GS_KNVP TYPE TY_KNVP,
       GT_SELC TYPE TABLE OF TY_KNVP,
       GS_SELC TYPE TY_KNVP,
       LR_COLS TYPE REF TO CL_SALV_COLUMNS,
       LR_COL TYPE REF TO CL_SALV_COLUMN,
       LV_REF TYPE  REF TO CL_SALV_TABLE,
       LR_LAY TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
       FULLPATH       TYPE                   STRING,
       FILENAME       TYPE                   STRING,
       PATH           TYPE                   STRING,
       LV_BDC.

DATA : LV_FILE TYPE RLGRAP-FILENAME.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS P_FILE TYPE IBIPPARMS-PATH OBLIGATORY.
PARAMETERS: C_MODE   LIKE CTU_PARAMS-DISMODE DEFAULT 'A'.

PARAMETERS : P_RAD1 RADIOBUTTON GROUP RB1, " Assnt. Manager L3
             P_RAD2 RADIOBUTTON GROUP RB1, " Sales Officer L5
             P_RAD3 RADIOBUTTON GROUP RB1, " Regional Sales Manager L2
             P_RAD4 RADIOBUTTON GROUP RB1, " National Sales Manager L1
             P_RAD5 RADIOBUTTON GROUP RB1, " CC Agent
             P_RAD6 RADIOBUTTON GROUP RB1, " SKU
             P_RAD7 RADIOBUTTON GROUP RB1, " ACT MGR
             P_RAD8 RADIOBUTTON GROUP RB1, " CCC MGR
             P_RAD9 RADIOBUTTON GROUP RB1, " ZSM/ZBM
             P_RAD10 RADIOBUTTON GROUP RB1, " ZSM/ZBM
             P_RAD11 RADIOBUTTON GROUP RB1. " All


SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.



START-OF-SELECTION.


  LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    = 'x'
      I_LINE_HEADER        = 'X'
      I_TAB_RAW_DATA       = IT_RAW
      I_FILENAME           = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA = IT_XK02[]
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.

  IF SY-SUBRC <> 0.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.


  APPEND LINES OF IT_XK02 TO IT_XK03.


*  LV_BDC = 'X'.



  DESCRIBE TABLE IT_XK02 LINES LV_DT.

  IF P_FILE IS NOT INITIAL AND C_MODE IS NOT INITIAL.

    IF IT_XK02 IS NOT INITIAL.

      LV_NOS = SY-TABIX.

      LOOP AT IT_XK02 INTO WA_XK02.

        CLEAR: LV_KUNNR.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_XK02-KUNNR
          IMPORTING
            OUTPUT = LV_KUNNR.

        SELECT KUNNR
               VKORG
               VTWEG
               SPART
               PARVW
               FROM KNVP
               INTO TABLE GT_KNVP1
               WHERE KUNNR EQ LV_KUNNR AND
               VKORG EQ WA_XK02-VKORG AND
               VTWEG EQ WA_XK02-VTWEG AND
               SPART EQ WA_XK02-SPART.


***************************************************************************************************************
        PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0101'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'RF02D-D0324'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM BDC_FIELD       USING 'RF02D-KUNNR'
                                      WA_XK02-KUNNR . " '10000007'.
        PERFORM BDC_FIELD       USING 'RF02D-BUKRS'
                                      WA_XK02-BUKRS . "'1700'.
        PERFORM BDC_FIELD       USING 'RF02D-VKORG'
                                      WA_XK02-VKORG . "'1700'.
        PERFORM BDC_FIELD       USING 'RF02D-VTWEG'
                                      WA_XK02-VTWEG.  "'20'..
        PERFORM BDC_FIELD       USING 'RF02D-SPART'
                                      WA_XK02-SPART. "'30'.
        PERFORM BDC_FIELD       USING 'RF02D-D0324'
                                      'X'.
        PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.


        IF P_RAD1 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'L3'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'AS'.

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'AS'. " L3

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.




        ELSEIF P_RAD2 = 'X'.



          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'L5'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'SO'. " L5

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'SO'. " L5

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.



        ELSEIF P_RAD3 = 'X'.



          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'L2'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'RS'. " L2

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'RS'. " L2

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.


        ELSEIF P_RAD4 = 'X'.


          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'L1'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.

            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'NS'. " L1

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'NS'. " L1

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

        ELSEIF P_RAD5 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'ZA'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'ZA'. " ZA

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'ZA'. " ZA

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

        ELSEIF P_RAD6 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'SK'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'SK'. " SK

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'SK'. " SK

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

        ELSEIF P_RAD7 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'AM'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'AM'. " AM

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'AM'. " AM

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

        ELSEIF P_RAD8 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'ZH'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'ZH'. " ZH

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'ZH'. " ZH

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

        ELSEIF P_RAD9 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'ZS'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'ZS'. " ZS

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'ZS'. " ZS

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.

           ELSEIF P_RAD10 = 'X'.

          READ TABLE GT_KNVP1 INTO GS_KNVP1 WITH KEY PARVW = 'ZC'.
          IF SY-SUBRC NE 0.


            DESCRIBE TABLE GT_KNVP1 LINES LV_TAB.
*            LV_TAB = SY-TABIX.
            LV_TAB = LV_TAB + 1.
            LV_TABIX = LV_TAB.

            CONCATENATE 'KNVP-PARVW' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.

            WA_XK02-PARVW = 'ZC'. " ZS

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(08)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PARVW.
*            PERFORM BDC_FIELD       USING 'KNVP-PARVW(08)'
*                                          'rs'.
            CLEAR:LV_NUM, LV_TES.


            CONCATENATE 'RF02D-KTONR' A LV_TABIX B INTO LV_NUM.
            LV_TES = LV_NUM.
            PERFORM BDC_FIELD       USING LV_TES
                                          WA_XK02-PERNR.

*            PERFORM BDC_FIELD       USING 'RF02D-KTONR(08)'
*                                          '2007'.
            CLEAR:LV_NUM, LV_TES.



          ELSE.

            WA_XK02-PARVW = 'ZC'. " ZS

            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                          WA_XK02-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.
            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK02-PERNR.

          ENDIF.


        ELSEIF P_RAD11 = 'X'.

          LOOP AT IT_XK03 INTO WA_XK03 WHERE KUNNR EQ WA_XK02-KUNNR." FROM LV_RNO.


            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          '*KNVP-PARVW'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING '*KNVP-PARVW'
                                    WA_XK03-PARVW.

            PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RF02D-KTONR(01)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=ENTR'.

            PERFORM BDC_FIELD       USING 'RF02D-KTONR(01)'
                                          WA_XK03-PERNR.

*            ENDIF.

          ENDLOOP.

        ENDIF.


        PERFORM BDC_DYNPRO      USING 'SAPMF02D' '0324'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'KNVP-PARVW(01)'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'          " Commented for test
                                      'KNVP-PARVW'.         " Commented for test


        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=UPDA'.

        CALL TRANSACTION 'XD02' USING IT_BDCDATA "#EC CI_USAGE_OK[2226131] " Added by <IT-CAR Tool> during Code Remediation
                                   MODE C_MODE "#EC CI_USAGE_OK[2265093] " Added by <IT-CAR Tool> during Code Remediation
                                   UPDATE 'S' MESSAGES INTO I_BDCMSG.

        REFRESH IT_BDCDATA.
        CLEAR: LV_KUNNR.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_XK02-KUNNR
          IMPORTING
            OUTPUT = LV_KUNNR.

        GS_SELC-KUNNR = LV_KUNNR.
        GS_SELC-VKORG = WA_XK02-VKORG.
        GS_SELC-VTWEG = WA_XK02-VTWEG.
        GS_SELC-SPART = WA_XK02-SPART.

        APPEND GS_SELC TO GT_SELC.

*        ENDAT.
      ENDLOOP.

    ENDIF.

* Report coding

    LOOP AT I_BDCMSG.

      LV_INDEX = SY-TABIX.


      LOOP AT IT_XK02 FROM LV_INDEX INTO WA_XK02.


        IF SY-TABIX GT LV_INDEX.

          EXIT.

        ELSE.

          IF I_BDCMSG-MSGTYP NE 'S'.

            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                MSGID               = I_BDCMSG-MSGID
                MSGNR               = I_BDCMSG-MSGNR
                MSGV1               = I_BDCMSG-MSGV1
*               MSGV2               = ' '
*               MSGV3               = ' '
*               MSGV4               = ' '
              IMPORTING
                MESSAGE_TEXT_OUTPUT = LV_MTEXT.

            CONCATENATE LV_ERR LV_MTEXT INTO LV_MESS.

            GS_REP-RNO = SY-TABIX.
            GS_REP-KUNNR = WA_XK02-KUNNR .
            GS_REP-BUKRS = WA_XK02-BUKRS .
            GS_REP-VKORG = WA_XK02-VKORG .
            GS_REP-VTWEG =  WA_XK02-VTWEG.
            GS_REP-SPART = WA_XK02-SPART.
            GS_REP-PERNR = WA_XK02-PERNR.
            GS_REP-E = LV_MESS.


            GS_ERR-KUNNR = WA_XK02-KUNNR .
            GS_ERR-BUKRS = WA_XK02-BUKRS .
            GS_ERR-VKORG = WA_XK02-VKORG .
            GS_ERR-VTWEG =  WA_XK02-VTWEG.
            GS_ERR-SPART = WA_XK02-SPART.
            GS_ERR-PERNR = WA_XK02-PERNR.
            APPEND GS_ERR TO GT_ERR.

            CLEAR LV_MESS.

*              LV_MSG = 'Mandatory fields not filled'.
*          WRITE LV_MSG.
*          SKIP.
          ELSEIF I_BDCMSG-MSGTYP EQ 'S'.

            GS_REP-RNO = SY-TABIX.
            GS_REP-KUNNR = WA_XK02-KUNNR .
            GS_REP-BUKRS = WA_XK02-BUKRS .
            GS_REP-VKORG = WA_XK02-VKORG .
            GS_REP-VTWEG =  WA_XK02-VTWEG.
            GS_REP-SPART = WA_XK02-SPART.
            GS_REP-PERNR = WA_XK02-PERNR.
            GS_REP-E = 'Record successfully updated'.

*          LV_MSG = 'Record successfully updated'.
*
*          WRITE LV_MSG.
*          SKIP.

          ENDIF.
          APPEND GS_REP TO GT_REP.
        ENDIF.


      ENDLOOP.


    ENDLOOP.


    SORT GT_REP ASCENDING BY RNO.


    TRY.
        CALL METHOD CL_SALV_TABLE=>FACTORY
          IMPORTING
            R_SALV_TABLE = LV_REF
          CHANGING
            T_TABLE      = GT_REP.
      CATCH CX_SALV_MSG .
    ENDTRY.

    CALL METHOD LV_REF->GET_COLUMNS
      RECEIVING
        VALUE = LR_COLS.


    CALL METHOD LR_COLS->GET_COLUMN
      EXPORTING
        COLUMNNAME = 'E'
      RECEIVING
        VALUE      = LR_COL.


    CALL METHOD LR_COL->SET_LONG_TEXT
      EXPORTING
        VALUE = 'Update Status'.

    CALL METHOD LV_REF->GET_DISPLAY_SETTINGS
      RECEIVING
        VALUE = LR_LAY.

    CALL METHOD LR_LAY->SET_STRIPED_PATTERN
      EXPORTING
        VALUE = CL_SALV_DISPLAY_SETTINGS=>TRUE.

    CALL METHOD LV_REF->DISPLAY.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO  USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM BDC_FIELD  USING FNAM FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_FIELD
