*&---------------------------------------------------------------------*
*& Report  J_1IPRNTARE                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  Y_1IPRNTARE MESSAGE-ID 8I.

****Tables
TABLES: J_1IEXCHDR,
        J_1IEXCDTL,
        J_1IWRKCUS,
        J_1IREGSET,
        J_1IBOND,
        T001W,
        TNAD7,
        TNAPR,
        T005,
        T005T,
        T043I,
        KNA1,
        J_1ILICHDR.


****Data Declaration
DATA: LS_EXCHDR LIKE J_1IEXCHDR.
DATA: LT_EXCDTL LIKE TABLE OF J_1IEXCDTL WITH HEADER LINE.
DATA: G_ADDR_EXC(120) TYPE C,
      G_ADDR_CUS(120) TYPE C,
      L_J1IREGID      TYPE J_1IREGID ,
      L_J1IEXCRN      TYPE J_1IEXRN  .
" l_j1iexcrn1      TYPE j_1iexrn  .

DATA : L_J1IEXCRN1 TYPE J_1IREGSET-J_1IEXCRN .


DATA : BEGIN OF GT_EXCDATE OCCURS 0,
         EXNUM  LIKE J_1IEXCHDR-EXNUM,
         EXYEAR LIKE J_1IEXCHDR-EXYEAR,
         EXDAT  LIKE J_1IEXCHDR-EXDAT,
       END OF GT_EXCDATE.

DATA : BEGIN OF OPTIONS.
        INCLUDE STRUCTURE ITCPO.
DATA : END OF OPTIONS.

DATA : BEGIN OF RESULT.
        INCLUDE STRUCTURE ITCPP.
DATA : END OF RESULT.

DATA : BEGIN OF THEAD OCCURS 10.
        INCLUDE STRUCTURE THEAD.
DATA : END OF THEAD.

DATA : BEGIN OF TLINES OCCURS 10.
        INCLUDE STRUCTURE TLINE.
DATA : END OF TLINES.

DATA : TEXT_ID(4) TYPE C,
       TEXT_NAME LIKE THEAD-TDNAME.

DATA: LAYOUT LIKE TNAPR-FONAM.

DATA: L_STPG TYPE C.

DATA : LV_L_J1IEXCRN(30)  TYPE C.

DATA: G_MODE   LIKE TLINES-TDLINE,
      G_EXP    LIKE TLINES-TDLINE,
      G_SEAL   LIKE TLINES-TDLINE,
      G_CONO   LIKE TLINES-TDLINE,
      G_EXDAT  LIKE J_1IEXCHDR-EXDAT,
      G_NOROWS TYPE I,
      G_PAGES  TYPE C,
      N_PAGES  TYPE C,
      G_TOT_PAGES TYPE C,
      G_ROWS   TYPE C.

DATA : GS_T001W LIKE T001W.
DATA : GS_T005T LIKE T005T.
DATA : G_TOT_OTHERS TYPE J_1IEXCDTL-EXBAS.
DATA : G_TOT_R_OTHERS TYPE J_1IEXCDTL-BEDRATE.
DATA : G_TOT_DUTY TYPE J_1IEXCDTL-EXBAS.

DATA : TOT_BED TYPE MAXBT.
DATA : TOT_BED3 TYPE  ZCURR-ZCUR.
DATA : TOT_VAL TYPE  MAXBT .
DATA : TOT_BED1 TYPE MAXBT.
DATA : TOT_WORDS TYPE C LENGTH 250.

"8/08/2016 Change by SRI
*DATA:TOT_BED1(16) TYPE P DECIMALS 5,
DATA :  TOT_BED2(16) TYPE P DECIMALS 0,
        L_ANDEC  LIKE T006-ANDEC VALUE 0.
DATA :   TOT_FL TYPE MAXBT.

DATA : TOT_VAL1(16) TYPE P DECIMALS 2.



****Form Call Begin
FORM ENTRY_ARE1
      TABLES LT_EXCDTL
      USING OUTPUT_TYPE
           L_FORM
           L_COPIES
           LS_EXCHDR LIKE J_1IEXCHDR.

****Code Begins
  PERFORM SELECT_PRINT_DATA USING OUTPUT_TYPE
                                  L_FORM
                                  L_COPIES
                                  LS_EXCHDR-WERKS
                                  LS_EXCHDR-SRGRP
                                  LS_EXCHDR-TRNTYP
                         CHANGING OPTIONS
                                  LAYOUT.

  PERFORM SELECT_DATA
                      TABLES LT_EXCDTL
                      USING  LS_EXCHDR.


  PERFORM OPEN_FORM USING OPTIONS
                          LAYOUT
                    CHANGING RESULT.

  PERFORM PRINT_LAYOUT
                      TABLES LT_EXCDTL
                      USING  LS_EXCHDR
                             OUTPUT_TYPE
                             LAYOUT.


  PERFORM CLOSE_FORM.
****Code Ends

ENDFORM.                                                    "entry_are1
****Form Call End


****Sub-routines
*&---------------------------------------------------------------------*
*&      Form  select_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OUTPUT_TYPE  text
*      -->P_L_copies  text
*      -->P_ls_exchdr_WERKS  text
*      <--P_OPTIONS  text
*      <--P_LAYOUT  text
*----------------------------------------------------------------------*
FORM SELECT_PRINT_DATA  USING    P_OUTPUT_TYPE
                                 P_L_FORM
                                 P_L_COPIES
                                 P_LS_EXCHDR_WERKS
                                 P_LS_EXCHDR_SRGRP
                                 P_LS_EXCHDR_TRNTYP
                        CHANGING P_OPTIONS LIKE ITCPO
                                 P_LAYOUT.

  DATA WA_TNAD7 LIKE TNAD7.
  DATA WA_TNAPR LIKE TNAPR.
  DATA LS_ARE_ATTRB TYPE J_1IARE_ATTRB.


*----number of copies
  SELECT SINGLE *
    INTO LS_ARE_ATTRB
    FROM J_1IARE_ATTRB
    WHERE SERGRP = P_LS_EXCHDR_SRGRP
    AND   TRNTYP = P_LS_EXCHDR_TRNTYP.

  P_OPTIONS-TDCOPIES = LS_ARE_ATTRB-NO_OF_COPIES.

  P_OPTIONS-TDIMMED = 'X'.
  P_OPTIONS-TDNEWID   = 'X' .
  P_OPTIONS-TDPROGRAM  = SY-REPID.

  P_LAYOUT  = P_L_FORM.

ENDFORM.                    " select_print_data


*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_lt_excdtl  text
*      -->P_ls_exchdr  text
*----------------------------------------------------------------------*
FORM SELECT_DATA  TABLES   P_LT_EXCDTL
                  USING    P_LS_EXCHDR LIKE J_1IEXCHDR.


  DATA: LT_BOND_ENTRY TYPE STANDARD TABLE OF J_1IBOND,
        LS_BOND_ENTRY TYPE J_1IBOND,
        L_ZEILE TYPE J_1IBOND-ZEILE,
        LS_EXCDTL TYPE J_1IEXCDTL.





  IF P_LT_EXCDTL[] IS INITIAL.
    SELECT * FROM J_1IEXCDTL INTO TABLE P_LT_EXCDTL "#EC CI_FLDEXT_OK[2215424]
      "Added by SPLABAP during code remediation
             WHERE TRNTYP = P_LS_EXCHDR-TRNTYP
             AND   DOCNO  = P_LS_EXCHDR-DOCNO
             AND   DOCYR  = P_LS_EXCHDR-DOCYR.
  ENDIF.


  LOOP AT P_LT_EXCDTL INTO J_1IEXCDTL."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation

    SELECT SINGLE * FROM J_1ILICHDR WHERE  LICYR = J_1IEXCDTL-LICYR
                                  AND LICNO = J_1IEXCDTL-LICNO.
    SELECT * FROM J_1IEXCHDR
           WHERE TRNTYP = 'DLFC'
           AND   EXNUM  = J_1IEXCDTL-RDOC2
           AND   EXYEAR = J_1IEXCDTL-RYEAR2
           AND   LIFNR NE ' ' .

      MOVE-CORRESPONDING J_1IEXCHDR TO GT_EXCDATE.
      APPEND GT_EXCDATE.
    ENDSELECT.
  ENDLOOP.

  "ADDED BY RAM ON 12/8/16
*  LOOP AT P_LT_EXCDTL INTO J_1IEXCDTL.
*
** TOT_BED = TOT_BED + J_1IEXCDTL-EXBED.
**    TOT_BED1 = TOT_BED.
*
*
*
*
**     TOT_VAL = TOT_VAL + J_1IEXCDTL-EXBAS.
*
**     BREAK-POINT.
**  clear tot_bed2.
**
*
**
*    TOT_BED = TOT_BED + J_1IEXCDTL-EXBED.
**   tot_val = tot_val1.
**   TOT_BED3 =  TOT_BED2.
**    TOT_BED =  TOT_BED2.
*    BREAK-POINT.
**
*  ENDLOOP.

"CHANGE BY RAM 12/09/2016
  LOOP AT P_LT_EXCDTL INTO J_1IEXCDTL ."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation

    CALL FUNCTION 'ROUND'
      EXPORTING
        DECIMALS      = L_ANDEC
        INPUT         = J_1IEXCDTL-EXBED
        SIGN          = '+'
      IMPORTING
        OUTPUT        = J_1IEXCDTL-EXBED
      EXCEPTIONS
        INPUT_INVALID = 1
        OVERFLOW      = 2
        TYPE_INVALID  = 3
        OTHERS        = 4.

    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    tot_bed3 =  tot_bed + J_1IEXCDTL-EXBed.
    tot_bed =  tot_bed + J_1IEXCDTL-EXBED.
    MODIFY P_LT_EXCDTL FROM J_1IEXCDTL .

     CALL FUNCTION 'ROUND'
      EXPORTING
        DECIMALS      = L_ANDEC
        INPUT         = J_1IEXCDTL-EXBAS
        SIGN          = '+'
      IMPORTING
        OUTPUT        = J_1IEXCDTL-EXBAS
      EXCEPTIONS
        INPUT_INVALID = 1
        OVERFLOW      = 2
        TYPE_INVALID  = 3
        OTHERS        = 4.

    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    tot_val =  tot_val + J_1IEXCDTL-EXBAS.
    MODIFY P_LT_EXCDTL FROM J_1IEXCDTL.

  ENDLOOP.



  CALL FUNCTION 'YHR_IN_CHG_INR_WRDS_2'
    EXPORTING
      AMT_IN_NUM         = TOT_BED
    IMPORTING
      AMT_IN_WORDS       = TOT_WORDS
    EXCEPTIONS
      DATA_TYPE_MISMATCH = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  "ENDED BY RAM ON 12/8/16


SORT GT_EXCDATE BY EXNUM EXYEAR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_EXCDATE COMPARING EXNUM EXYEAR.
  SELECT SINGLE * FROM T001W
                  WHERE WERKS = P_LS_EXCHDR-WERKS.



  SELECT SINGLE J_1IREGID FROM        J_1IEXGRPS                          " Note 1270637
                          INTO        L_J1IREGID
                          WHERE       J_1IWERKS  = P_LS_EXCHDR-WERKS
                          AND         J_1IEXCGRP = P_LS_EXCHDR-EXGRP.

  IF SY-SUBRC = 0.
    SELECT SINGLE        J_1IEXCRN FROM  J_1IREGSET
                        INTO    L_J1IEXCRN1
                         WHERE     J_1IREGID = L_J1IREGID.
  ENDIF.


  IF SY-SUBRC = 0.
    SELECT SINGLE        J_1IEXCRN FROM  J_1IREGSET
                        INTO      L_J1IEXCRN
                         WHERE     J_1IREGID = L_J1IREGID.
  ENDIF.

  SELECT SINGLE * FROM J_1IWRKCUS
                  WHERE J_1IWERKS = P_LS_EXCHDR-WERKS.
  SELECT SINGLE * FROM J_1IREGSET
                  WHERE J_1IREGID = J_1IWRKCUS-J_1IREGID.

  SELECT SINGLE * FROM KNA1
                  WHERE KUNNR = J_1IEXCHDR-KUNAG.
  SELECT SINGLE * FROM T005
                  WHERE LAND1 = KNA1-LAND1.

  SELECT * UP TO 1 ROWS FROM T005T
                  WHERE LAND1 = KNA1-LAND1 ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

*---get the bond amount for the respective ARE doc
  SELECT *
  INTO TABLE LT_BOND_ENTRY
  FROM J_1IBOND
  WHERE BONDYR = P_LS_EXCHDR-BONDYR
  AND  BONDNO = P_LS_EXCHDR-BONDNO.

  READ TABLE P_LT_EXCDTL "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
    INTO LS_EXCDTL"#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation
    INDEX 1.


  LOOP AT LT_BOND_ENTRY INTO LS_BOND_ENTRY.
    IF LS_BOND_ENTRY-ARE_DOCNO = LS_EXCDTL-EXNUM
       AND LS_BOND_ENTRY-ARE_DOCYR = LS_EXCDTL-EXYEAR
       AND LS_BOND_ENTRY-SHKZG = 'S'.
      L_ZEILE = LS_BOND_ENTRY-ZEILE.
    ENDIF.
  ENDLOOP.

  CLEAR: LS_BOND_ENTRY.
  REFRESH: LT_BOND_ENTRY.

  SELECT *
  INTO TABLE LT_BOND_ENTRY
  FROM J_1IBOND
  WHERE BONDYR = P_LS_EXCHDR-BONDYR
  AND  BONDNO = P_LS_EXCHDR-BONDNO
  AND ZEILE < L_ZEILE
  AND ARE_DOCNO = '          '
  AND ARE_DOCYR = '    '.

  SORT LT_BOND_ENTRY BY ZEILE DESCENDING.
  READ TABLE LT_BOND_ENTRY INTO LS_BOND_ENTRY INDEX 1.
  J_1IBOND-BONDAMT = LS_BOND_ENTRY-BONDAMT.






  PERFORM READ_ADDR_DATA USING P_LS_EXCHDR-EXC_ADDRESS_NO 'EXC'.

  PERFORM READ_ADDR_DATA USING P_LS_EXCHDR-CUS_ADDRESS_NO 'CUS'.

  DESCRIBE TABLE P_LT_EXCDTL LINES G_NOROWS.

  MOVE-CORRESPONDING P_LS_EXCHDR TO J_1IEXCHDR.

  MOVE-CORRESPONDING T001W TO GS_T001W.

  MOVE-CORRESPONDING T005T TO GS_T005T .

ENDFORM.                    " select_data

*&---------------------------------------------------------------------*
*&      Form  open_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OPTIONS  text
*      -->P_LAYOUT  text
*      <--P_RESULT  text
*----------------------------------------------------------------------*
FORM OPEN_FORM  USING    P_OPTIONS
                         P_LAYOUT
                CHANGING P_RESULT.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      APPLICATION                 = 'TX'
*     ARCHIVE_INDEX               =
*     ARCHIVE_PARAMS              =
      DEVICE                      = 'PRINTER'
      DIALOG                      = 'X'
      FORM                        = P_LAYOUT
      LANGUAGE                    = SY-LANGU
      OPTIONS                     = OPTIONS
*     MAIL_SENDER                 =
*     MAIL_RECIPIENT              =
*     MAIL_APPL_OBJECT            =
*     RAW_DATA_INTERFACE          = '*'
    IMPORTING
*     LANGUAGE                    =
*     NEW_ARCHIVE_PARAMS          =
      RESULT                      = RESULT
    EXCEPTIONS
      CANCELED                    = 1
      DEVICE                      = 2
      FORM                        = 3
      OPTIONS                     = 4
      UNCLOSED                    = 5
*     MAIL_OPTIONS                = 6
*     ARCHIVE_ERROR               = 7
*     INVALID_FAX_NUMBER          = 8
*     MORE_PARAMS_NEEDED_IN_BATCH = 9
      SPOOL_ERROR                 = 10
      CODEPAGE                    = 11
      OTHERS                      = 12.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " open_form

*&---------------------------------------------------------------------*
*&      Form  print_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LAYOUT TABLES LT_EXCDTL
                  USING P_LS_EXCHDR STRUCTURE J_1IEXCHDR
                        P_OUTPUT_TYPE
                        P_LAYOUT.


  DATA: L_REM    TYPE I,
        L_PAGES  TYPE I,
        L_NOROWS TYPE I,
        L_ONE_OVER.
  DATA : WA_EXCHDR TYPE J_1IEXCHDR.
  IF P_OUTPUT_TYPE = 'ARE1'.
    SELECT SINGLE * FROM J_1IBOND WHERE BONDNO  = P_LS_EXCHDR-BONDNO
                                      AND  BONDYR = P_LS_EXCHDR-BONDYR.

    G_ROWS   = G_NOROWS.
    L_NOROWS = G_NOROWS.
    IF L_NOROWS > 3.
      L_PAGES = 2.
      L_NOROWS = L_NOROWS - 3.
      L_PAGES  = L_PAGES + ( L_NOROWS DIV 4 ).
      L_REM    = L_NOROWS MOD 4.
      IF L_REM <> 0.
        L_PAGES = L_PAGES + 1.
      ENDIF.
    ELSE.
      L_PAGES = 1.
    ENDIF.

    G_PAGES = L_PAGES.
    L_PAGES = L_PAGES + 2.
    N_PAGES = G_PAGES + 1 .
    G_TOT_PAGES = L_PAGES.




    IF G_NOROWS <= 3.
      PERFORM START_FORM USING P_LAYOUT 'PAGE1'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER1'.

      CONCATENATE P_LS_EXCHDR-TRNTYP P_LS_EXCHDR-DOCNO P_LS_EXCHDR-DOCYR
       INTO THEAD-TDNAME.

      TEXT_ID = '0001'.
      PERFORM READ_TEXT.
      LOOP AT TLINES.
        IF TLINES-TDLINE NE SPACE.
          G_MODE = TLINES-TDLINE.
          EXIT.
        ENDIF.
      ENDLOOP.

      TEXT_ID = '0002'.
      PERFORM READ_TEXT.
      LOOP AT TLINES.
        IF TLINES-TDLINE NE SPACE.
          G_EXP = TLINES-TDLINE.
          EXIT.
        ENDIF.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'HEADER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER3'.
      PERFORM WRITE_FORM USING '' 'SET' 'SUBHEAD'.
      PERFORM WRITE_FORM USING '' 'SET' 'TITHEAD'.

      CLEAR G_TOT_DUTY.
      LOOP AT LT_EXCDTL INTO J_1IEXCDTL."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation
        CLEAR G_TOT_OTHERS.
        CLEAR G_TOT_R_OTHERS.
        G_TOT_DUTY  = G_TOT_DUTY +
               J_1IEXCDTL-NCCD +
               J_1IEXCDTL-EXAED +
               J_1IEXCDTL-EXSED +
               J_1IEXCDTL-EXBED +
               J_1IEXCDTL-ECS   +
               J_1IEXCDTL-EXADDTAX1.

        G_TOT_OTHERS  = J_1IEXCDTL-NCCD + J_1IEXCDTL-EXAED + J_1IEXCDTL-EXSED.

*      G_TOT_R_OTHERS = J_1IEXCDTL-NCCDRATE + J_1IEXCDTL-SEDRATE + J_1IEXCDTL-AEDRATE .
        IF SY-TABIX > 1.
          CLEAR T001W.
          CLEAR J_1IREGSET-J_1IEXCRN.
          CLEAR T005T .

        ENDIF.
        LOOP AT GT_EXCDATE WHERE EXNUM  = J_1IEXCDTL-RDOC2
                           AND   EXYEAR = J_1IEXCDTL-RYEAR2.
          PERFORM WRITE_FORM USING 'ITEMVALUES' 'APPEND' 'MAIN'.
        ENDLOOP.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER3'.
      PERFORM END_FORM.

    ELSE.   "g_norows <= 3.

      PERFORM START_FORM USING P_LAYOUT 'PAGE4'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER1'.

      CONCATENATE P_LS_EXCHDR-TRNTYP P_LS_EXCHDR-DOCNO P_LS_EXCHDR-DOCYR
        INTO THEAD-TDNAME.

      TEXT_ID = '0001'.
      PERFORM READ_TEXT.
      LOOP AT TLINES.
        IF TLINES-TDLINE NE SPACE.
          G_MODE = TLINES-TDLINE.
          EXIT.
        ENDIF.
      ENDLOOP.

      TEXT_ID = '0002'.
      PERFORM READ_TEXT.
      LOOP AT TLINES.
        IF TLINES-TDLINE NE SPACE.
          G_EXP = TLINES-TDLINE.
          EXIT.
        ENDIF.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'HEADER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER3'.
      PERFORM WRITE_FORM USING '' 'SET' 'SUBTIT'.
      PERFORM WRITE_FORM USING 'TITHEAD' 'SET' 'MAIN'. "changed by sri 03/08/2016

      LOOP AT LT_EXCDTL INTO J_1IEXCDTL."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation
        CLEAR G_TOT_OTHERS.
        CLEAR G_TOT_R_OTHERS.

        G_TOT_DUTY  = G_TOT_DUTY +
                    J_1IEXCDTL-NCCD +
                J_1IEXCDTL-EXAED +
                J_1IEXCDTL-EXSED +
                J_1IEXCDTL-EXBED +
                J_1IEXCDTL-ECS   +
                J_1IEXCDTL-EXADDTAX1.
        G_TOT_OTHERS  = J_1IEXCDTL-NCCD + J_1IEXCDTL-EXAED + J_1IEXCDTL-EXSED.

        IF SY-TABIX > 1.
          CLEAR T001W.
          CLEAR J_1IREGSET-J_1IEXCRN.

        ENDIF.
        LOOP AT GT_EXCDATE WHERE EXNUM  = J_1IEXCDTL-RDOC2
                           AND   EXYEAR = J_1IEXCDTL-RYEAR2.
          PERFORM WRITE_FORM USING 'ITEMVALUES' 'APPEND' 'MAIN'.
        ENDLOOP.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER3'.
      PERFORM END_FORM.
**
    ENDIF.    "g_norows <= 3.

    TEXT_ID = '0003'.
    PERFORM READ_TEXT.
    LOOP AT TLINES.
      IF TLINES-TDLINE NE SPACE.
        G_SEAL = TLINES-TDLINE.
        EXIT.
      ENDIF.
    ENDLOOP.

    TEXT_ID = '0004'.
    PERFORM READ_TEXT.
    LOOP AT TLINES.
      IF TLINES-TDLINE NE SPACE.
        G_CONO = TLINES-TDLINE.
        EXIT.
      ENDIF.
    ENDLOOP.

**** Calling Page 2 - Section B
    PERFORM START_FORM USING P_LAYOUT 'PAGE2'.
    PERFORM WRITE_FORM USING '' 'SET' 'PARTA'.
    PERFORM WRITE_FORM USING '' 'SET' 'PARTB'.
    PERFORM WRITE_FORM USING '' 'SET' 'PARTC'.
    PERFORM WRITE_FORM USING '' 'SET' 'PARTD'.
    PERFORM END_FORM.
**** Calling Page 3 - Annexure A
    IF G_NOROWS > 1.
      PERFORM START_FORM USING P_LAYOUT 'PAGE3'.
      PERFORM WRITE_FORM USING 'BOXITEM' 'SET' 'ANNEX'.

      LOOP AT LT_EXCDTL INTO J_1IEXCDTL."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation
        CLEAR G_TOT_OTHERS.
        CLEAR G_TOT_R_OTHERS.

        G_TOT_DUTY  = G_TOT_DUTY + J_1IEXCDTL-NCCD +
                J_1IEXCDTL-EXAED +
                J_1IEXCDTL-EXSED +
                J_1IEXCDTL-EXBED +
                J_1IEXCDTL-ECS   +
                J_1IEXCDTL-EXADDTAX1.
        G_TOT_OTHERS  = J_1IEXCDTL-NCCD + J_1IEXCDTL-EXAED + J_1IEXCDTL-EXSED.

        IF SY-TABIX = 1.
          L_ONE_OVER = 'X'.
        ENDIF.
        PERFORM WRITE_FORM USING 'ITEMS' 'APPEND' 'MAIN'.
        IF L_ONE_OVER = 'X'.
          CLEAR: G_CONO, G_SEAL.
        ENDIF.
      ENDLOOP.
      PERFORM END_FORM.
    ENDIF.

    PERFORM CLEAR_TAB.

  ELSEIF P_OUTPUT_TYPE = 'ARE3'.
    G_ROWS   = G_NOROWS.
    L_NOROWS = G_NOROWS.
    IF L_NOROWS > 3.
      L_PAGES = 1.
      L_NOROWS = L_NOROWS - 3.
      L_PAGES  = L_PAGES + ( L_NOROWS DIV 4 ).
      L_REM    = L_NOROWS MOD 4.
      IF L_REM <> 0.
        L_PAGES = L_PAGES + 1.
      ENDIF.
    ELSE.
      L_PAGES = 1.
    ENDIF.
    G_PAGES = L_PAGES.
    L_PAGES = L_PAGES + 1.
    N_PAGES = G_PAGES + 1 .
    G_TOT_PAGES = L_PAGES.
    IF G_NOROWS <= 3.

      PERFORM START_FORM USING P_LAYOUT 'PAGE4'.

      PERFORM WRITE_FORM USING '' 'SET' 'HEADER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER3'.
      PERFORM WRITE_FORM USING '' 'SET' 'SUBHEAD'.
      PERFORM WRITE_FORM USING '' 'SET' 'TITHEAD'. " change by sri 03/08/2016

      LOOP AT LT_EXCDTL INTO J_1IEXCDTL. "#EC CI_FLDEXT_OK[2215424]
        "Added by SPLABAP during code remediation
        IF SY-TABIX > 1.
          CLEAR T001W.
          CLEAR J_1IREGSET-J_1IEXCRN.

        ENDIF.
        LOOP AT GT_EXCDATE WHERE EXNUM  = J_1IEXCDTL-RDOC2
                           AND   EXYEAR = J_1IEXCDTL-RYEAR2.
          CLEAR G_TOT_OTHERS.
          CLEAR G_TOT_R_OTHERS.

          G_TOT_DUTY  = J_1IEXCDTL-NCCD +
                      J_1IEXCDTL-EXAED +
                      J_1IEXCDTL-EXSED +
                      J_1IEXCDTL-EXBED +
                      J_1IEXCDTL-ECS   +
                      J_1IEXCDTL-EXADDTAX1.
          G_TOT_OTHERS  = J_1IEXCDTL-NCCD + J_1IEXCDTL-EXAED + J_1IEXCDTL-EXSED.
          PERFORM WRITE_FORM USING 'ITEMVALUES' 'APPEND' 'MAIN'.
        ENDLOOP.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER2'.

      PERFORM END_FORM.

    ELSE.

      PERFORM START_FORM USING P_LAYOUT 'PAGE3'.

      PERFORM WRITE_FORM USING '' 'SET' 'HEADER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER2'.
      PERFORM WRITE_FORM USING '' 'SET' 'HEADER3'.
      PERFORM WRITE_FORM USING '' 'SET' 'SUBTIT'.
      PERFORM WRITE_FORM USING 'TITHEAD' 'SET' 'MAIN'.

      LOOP AT LT_EXCDTL INTO J_1IEXCDTL."#EC CI_FLDEXT_OK[2215424]
"Added by SPLABAP during code remediation
        IF SY-TABIX > 1.
          CLEAR T001W.
          CLEAR J_1IREGSET-J_1IEXCRN.
        ENDIF.
*        LOOP AT gt_excdate WHERE exnum  = j_1iexcdtl-rdoc2
*                           AND   exyear = j_1iexcdtl-ryear2.
*          PERFORM write_form USING 'ITEMVALUES' 'APPEND' 'MAIN'.
*        ENDLOOP.
      ENDLOOP.

      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER1'.
      PERFORM WRITE_FORM USING '' 'SET' 'FOOTER2'.

      PERFORM END_FORM.

    ENDIF.

*** Calling Page 3 - Certificate Printing
    PERFORM START_FORM USING P_LAYOUT 'PAGE3'.
    PERFORM WRITE_FORM USING '' 'SET' 'HEADER4'.
    PERFORM WRITE_FORM USING '' 'SET' 'SUBHEAD2'.
    PERFORM WRITE_FORM USING '' 'SET' 'TITHEAD2'.
    PERFORM WRITE_FORM USING '' 'SET' 'FOOTER1'.
    PERFORM WRITE_FORM USING '' 'SET' 'FOOTER2'.
    PERFORM END_FORM.

    PERFORM CLEAR_TAB.

  ENDIF.   "p_output_type = 'ARE1'

ENDFORM.                    " print_layout

*&---------------------------------------------------------------------*
*&      Form  close_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_FORM .

  CALL FUNCTION 'CLOSE_FORM'
   IMPORTING
     RESULT                         = RESULT
* TABLES
*   OTFDATA                        =
   EXCEPTIONS
     UNOPENED                       = 1
     OTHERS                         = 6
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " close_form

*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TEXT .

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = TEXT_ID
      LANGUAGE                = SY-LANGU
      NAME                    = THEAD-TDNAME
      OBJECT                  = 'J1IARE1'
      ARCHIVE_HANDLE          = 0
    IMPORTING
      HEADER                  = THEAD
    TABLES
      LINES                   = TLINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CLEAR TLINES.
    REFRESH TLINES.
  ENDIF.

ENDFORM.                    " READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  WRITE_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0319   text
*      -->P_0320   text
*      -->P_0321   text
*----------------------------------------------------------------------*
FORM WRITE_FORM  USING ELEMENT FNCTION WINDOW.

  CALL FUNCTION 'WRITE_FORM'
   EXPORTING
     ELEMENT                        = ELEMENT
     FUNCTION                       = FNCTION
     TYPE                           = 'BODY'
     WINDOW                         = WINDOW
* IMPORTING
*   PENDING_LINES                  =
   EXCEPTIONS
     ELEMENT                        = 1
     FUNCTION                       = 2
     TYPE                           = 3
     UNOPENED                       = 4
     UNSTARTED                      = 5
     WINDOW                         = 6
     BAD_PAGEFORMAT_FOR_PRINT       = 7
     SPOOL_ERROR                    = 8
     CODEPAGE                       = 9
     OTHERS                         = 10
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " WRITE_FORM
*&---------------------------------------------------------------------*
*&      Form  start_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM START_FORM USING L_LAYOUT L_STPG.

  CALL FUNCTION 'START_FORM'
   EXPORTING
*   ARCHIVE_INDEX          =
     FORM                  = L_LAYOUT
*     LANGUAGE             = ''
     STARTPAGE             = L_STPG
     PROGRAM               = SY-REPID
*   MAIL_APPL_OBJECT       =
* IMPORTING
*   LANGUAGE               =
   EXCEPTIONS
     FORM                   = 1
     FORMAT                 = 2
     UNENDED                = 3
     UNOPENED               = 4
     UNUSED                 = 5
     SPOOL_ERROR            = 6
     CODEPAGE               = 7
     OTHERS                 = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " start_form
*&---------------------------------------------------------------------*
*&      Form  end_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM END_FORM .

  CALL FUNCTION 'END_FORM'
    IMPORTING
      RESULT                   = RESULT
    EXCEPTIONS
      UNOPENED                 = 1
      BAD_PAGEFORMAT_FOR_PRINT = 2
      SPOOL_ERROR              = 3
      CODEPAGE                 = 4
      OTHERS                   = 5.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " end_form
*&---------------------------------------------------------------------*
*&      Form  CLEAR_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_TAB .

  CLEAR:J_1IEXCHDR,
        J_1IEXCDTL,
        J_1IWRKCUS,
        J_1IBOND,
        T001W,
        J_1IREGSET-J_1IEXCRN,
        TNAD7,
        TNAPR,
        T005,
        T005T,
        KNA1.

  CLEAR LT_EXCDTL.
  REFRESH LT_EXCDTL.

ENDFORM.                    " CLEAR_TAB
*&---------------------------------------------------------------------*
*&      Form  READ_ADDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ADDRESS_NO  text
*      -->P_STR         text
*----------------------------------------------------------------------*
FORM READ_ADDR_DATA  USING    P_ADDRESS_NO P_STR.

  DATA: L_ADDR1_SEL  LIKE ADDR1_SEL,
        L_SADR       LIKE SADR.
  DATA: L_CAM_HANDLE LIKE J_1IADDRES-CAM_HANDLE.

  SELECT SINGLE CAM_HANDLE INTO  L_CAM_HANDLE
                           FROM  J_1IADDRES
                           WHERE ADDRESS_NO = P_ADDRESS_NO.
  IF SY-SUBRC = 0.

    L_ADDR1_SEL-ADDRNUMBER = L_CAM_HANDLE.

    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        ADDRESS_SELECTION             = L_ADDR1_SEL
*   ADDRESS_GROUP                 =
*   READ_SADR_ONLY                = ' '
*   READ_TEXTS                    = ' '
 IMPORTING
*   ADDRESS_VALUE                 =
*   ADDRESS_ADDITIONAL_INFO       =
*   RETURNCODE                    =
*   ADDRESS_TEXT                  =
      SADR                            = L_SADR
* TABLES
*   ADDRESS_GROUPS                =
*   ERROR_TABLE                   =
*   VERSIONS                      =
* EXCEPTIONS
*   PARAMETER_ERROR               = 1
*   ADDRESS_NOT_EXIST             = 2
*   VERSION_NOT_EXIST             = 3
*   INTERNAL_ERROR                = 4
*   OTHERS                        = 5
              .
    IF SY-SUBRC = 0.
      IF P_STR = 'EXC'.
        CONCATENATE L_SADR-NAME1 L_SADR-NAME2 L_SADR-NAME3+0(20)
                    L_SADR-PSTLZ L_SADR-ORT01+0(20) INTO G_ADDR_EXC.
      ELSEIF P_STR = 'CUS'.
        CONCATENATE L_SADR-NAME1 L_SADR-NAME2 L_SADR-NAME3+0(20)
                    L_SADR-PSTLZ L_SADR-ORT01+0(20) INTO G_ADDR_CUS.
      ENDIF.
    ENDIF.

  ENDIF.



ENDFORM.                    " READ_ADDR_DATA
