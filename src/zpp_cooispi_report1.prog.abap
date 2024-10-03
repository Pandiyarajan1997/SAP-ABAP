*&---------------------------------------------------------------------*
*& Report  ZPP_COOISPI_REPORT1
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr. Umapathi N                         *
*& Developer                   : Mr. Manikandan T                       *
*& Modified  On                :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :  Mr. Umapathi                         *
*& Title                       : Process Order Information Report      *
*& Report Name                 : ZPP_COOISPI_REPORT                    *
*& Development Id              : kpabap                                *
*& Related Information         : Process Order Information System      *
*&---------------------------------------------------------------------*


REPORT ZPP_COOISPI_REPORT1.


TABLES:AFRU,AFKO,MARA,MAKT,AFPO,J_1IMTCHID.



*structure for AFRU

TYPES: BEGIN OF STR_AFRU,
        RUECK TYPE AFRU-RUECK,
        RMZHL TYPE AFRU-RMZHL,
        ERSDA TYPE AFRU-ERSDA,
        ERNAM TYPE AFRU-ERNAM,
        BUDAT TYPE AFRU-BUDAT,
        WERKS TYPE AFRU-WERKS,
        GMNGA TYPE AFRU-GMNGA,
        GMEIN TYPE AFRU-GMEIN,
        AUFNR TYPE AFRU-AUFNR,
        STOKZ TYPE AFRU-STOKZ,
        STZHL TYPE AFRU-STZHL,
      END OF STR_AFRU.

*structure for AFKO

TYPES: BEGIN OF STR_AFKO,
        AUFNR TYPE AFKO-AUFNR,
        PLNBEZ TYPE AFKO-PLNBEZ,
      END OF STR_AFKO.


*  structure for MARA

TYPES : BEGIN OF STR_MARA,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        VOLUM TYPE MARA-VOLUM,
      END OF STR_MARA.

*  structure for MAKT

TYPES: BEGIN OF STR_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END OF STR_MAKT.

*  structure for   AFPO

TYPES: BEGIN OF STR_AFPO,
        AUFNR TYPE AFPO-AUFNR,
        CHARG TYPE AFPO-CHARG,
      END OF STR_AFPO.

*  structure for J_1IMTCHID

TYPES: BEGIN OF STR_J_1IMTCHID,
        MATNR TYPE J_1IMTCHID-MATNR,
        WERKS TYPE J_1IMTCHID-WERKS,
        J_1ICHID TYPE J_1IMTCHID-J_1ICHID,
      END OF STR_J_1IMTCHID.

*  structure for FINAL

TYPES:BEGIN OF STR_FINAL,
   RUECK TYPE AFRU-RUECK,
        RMZHL TYPE AFRU-RMZHL,
        ERSDA TYPE AFRU-ERSDA,
        ERNAM TYPE AFRU-ERNAM,
        BUDAT TYPE AFRU-BUDAT,
        WERKS TYPE AFRU-WERKS,
        GMNGA TYPE AFRU-GMNGA,
        GMEIN TYPE AFRU-GMEIN,
        AUFNR TYPE AFRU-AUFNR,
        STOKZ TYPE AFRU-STOKZ,
        STZHL TYPE AFRU-STZHL,

         PLNBEZ TYPE AFKO-PLNBEZ,

   MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        VOLUM TYPE MARA-VOLUM,

   MAKTX TYPE MAKT-MAKTX,

  CHARG TYPE AFPO-CHARG,

  J_1ICHID TYPE J_1IMTCHID-J_1ICHID,

  LITR TYPE P DECIMALS 2,
  END OF STR_FINAL.



*  workarea & internal table decleration


DATA:WA_AFRU TYPE STR_AFRU,
      IT_AFRU TYPE TABLE OF STR_AFRU.

DATA:WA_AFKO TYPE STR_AFKO,
      IT_AFKO TYPE TABLE OF STR_AFKO.

DATA:WA_MARA TYPE STR_MARA,
      IT_MARA TYPE TABLE OF STR_MARA.

DATA:WA_MAKT TYPE STR_MAKT,
      IT_MAKT TYPE TABLE OF STR_MAKT.

DATA:WA_AFPO TYPE STR_AFPO,
      IT_AFPO TYPE TABLE OF STR_AFPO.

DATA:WA_J_1IMTCHID TYPE STR_J_1IMTCHID,
      IT_J_1IMTCHID TYPE TABLE OF STR_J_1IMTCHID.


DATA:WA_FINAL TYPE STR_FINAL,
      IT_FINAL TYPE TABLE OF STR_FINAL.

*fieldcatalog alv

DATA:WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

*input screen decleration

SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.


SELECT-OPTIONS:S_WERK FOR AFRU-WERKS,
               S_PLNBE FOR AFKO-PLNBEZ,
               s_mtart FOR mara-mtart,
               S_AUFN FOR AFRU-AUFNR NO-DISPLAY,
               S_BUDA FOR AFRU-BUDAT,
               S_CHA FOR J_1IMTCHID-J_1ICHID.
SELECTION-SCREEN END OF BLOCK A1.

DATA:DAT1 TYPE AFRU-BUDAT."sy-datum .

*data retrive from table

PERFORM DATA_RETRIVE.
PERFORM FIELD_CAT.
PERFORM ALV_DISPLAY.


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_RETRIVE.

  SELECT RUECK
         RMZHL
         ERSDA
         ERNAM
         BUDAT
         WERKS
         GMNGA
         GMEIN
         AUFNR
         STOKZ
         STZHL FROM AFRU
         INTO TABLE IT_AFRU
         WHERE WERKS IN S_WERK
         AND   AUFNR IN S_AUFN
         AND   BUDAT IN S_BUDA AND  STOKZ NE 'X' AND   STZHL EQ '0'.



  SELECT AUFNR
         PLNBEZ FROM AFKO
         INTO TABLE IT_AFKO
         FOR ALL ENTRIES IN IT_AFRU
         WHERE AUFNR = IT_AFRU-AUFNR." AND PLNBEZ IN S_PLNBEZ." AND PLNBEZ = 'FERT'.



  SELECT MATNR
         MTART
         VOLUM FROM MARA
         INTO TABLE IT_MARA
         FOR ALL ENTRIES IN IT_AFKO
         WHERE MATNR = IT_AFKO-PLNBEZ AND MATNR IN S_PLNBE and mtart in s_mtart.


  SELECT MATNR
         MAKTX FROM MAKT
         INTO TABLE IT_MAKT
         FOR ALL ENTRIES IN IT_MARA
         WHERE MATNR = IT_MARA-MATNR.

  SELECT AUFNR
         CHARG FROM AFPO
         INTO TABLE IT_AFPO
         FOR ALL ENTRIES IN IT_AFRU
         WHERE AUFNR = IT_AFRU-AUFNR.


  SELECT MATNR
         WERKS
         J_1ICHID FROM J_1IMTCHID
         INTO TABLE IT_J_1IMTCHID
         FOR ALL ENTRIES IN IT_AFKO
         WHERE MATNR = IT_AFKO-PLNBEZ AND J_1ICHID IN S_CHA.




  LOOP AT IT_AFRU INTO WA_AFRU.

    MOVE-CORRESPONDING WA_AFRU TO WA_FINAL.

    READ TABLE IT_AFKO INTO WA_AFKO
    WITH KEY AUFNR = WA_AFRU-AUFNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_AFKO TO WA_FINAL.
    ENDIF.

    READ TABLE IT_MARA INTO WA_MARA
    WITH KEY MATNR = WA_AFKO-PLNBEZ.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_MARA TO WA_FINAL.
    ENDIF.

    READ TABLE IT_MAKT INTO WA_MAKT
WITH KEY MATNR = WA_MARA-MATNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_MAKT TO WA_FINAL.
    ENDIF.

    READ TABLE IT_AFPO INTO WA_AFPO
    WITH KEY AUFNR = WA_AFRU-AUFNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_AFPO TO WA_FINAL.
    ENDIF.

    READ TABLE IT_J_1IMTCHID INTO WA_J_1IMTCHID
    WITH KEY MATNR = WA_AFKO-PLNBEZ WERKS = WA_AFRU-WERKS.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_J_1IMTCHID TO WA_FINAL.
    ENDIF.

    APPEND WA_FINAL TO IT_FINAL.
    CLEAR WA_FINAL.

  ENDLOOP.

  LOOP AT IT_FINAL INTO WA_FINAL.



      WA_FINAL-LITR = WA_FINAL-GMNGA * WA_FINAL-VOLUM .


    IF WA_FINAL-GMEIN = 'L' or WA_FINAL-GMEIN = 'KG'.

      WA_FINAL-LITR = WA_FINAL-GMNGA .
    ENDIF.

    MODIFY IT_FINAL FROM WA_FINAL TRANSPORTING LITR.

    CLEAR WA_FINAL.
  ENDLOOP .


  LOOP AT IT_FINAL INTO WA_FINAL.



    IF WA_FINAL-WERKS NE '1002' AND  WA_FINAL-WERKS NE '1001'.
      DELETE IT_FINAL WHERE MTART NE 'FERT'.
      IF S_CHA IS NOT INITIAL.
        DELETE IT_FINAL WHERE J_1ICHID EQ ''.
      ENDIF .
    ENDIF .


    IF WA_FINAL-WERKS = '1002'.

      DELETE IT_FINAL WHERE MTART NE 'FERT' AND MTART NE 'HALB' .

      IF S_CHA IS NOT INITIAL.
        DELETE IT_FINAL WHERE J_1ICHID EQ ''.
      ENDIF .

    ENDIF .


    IF WA_FINAL-WERKS = '1001'.                                       " added by mani 26.02.2016

      DELETE IT_FINAL WHERE MTART NE 'FERT' AND MTART NE 'HALB' .

      IF S_CHA IS NOT INITIAL.
        DELETE IT_FINAL WHERE J_1ICHID EQ ''.
      ENDIF .

    ENDIF .

  ENDLOOP.

*sort it_final by J_1ICHID.

ENDFORM.                    "DATA_RETRIVE




*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELD_CAT.

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'IT_FINAL' '' ''.
*  PERFORM ALV_LAYOUT USING 2 'Order' 'AUFNR' 'IT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 3 'Posting Date' 'BUDAT' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Number' 'PLNBEZ' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 5 'Material Description' 'MAKTX' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 6 'Chapter ID' 'J_1ICHID' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Material Type' 'MTART' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 8 'Volume' 'VOLUM' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 9 'Confirmed Qty' 'GMNGA' 'IT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 10 'Base Unit' 'GMEIN' 'IT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 11 'Confirmed Qty Ltrs' 'LITR' 'IT_FINAL' 'X' ''.
*  PERFORM ALV_LAYOUT USING 11 'Order' 'AUFNR' 'IT_FINAL' '' ''.

*  PERFORM ALV_LAYOUT USING 13 'Batch' 'CHARG' 'IT_FINAL' '' 'X'.
*  PERFORM ALV_LAYOUT USING 14 'Entered on' 'ERSDA' 'IT_FINAL' '' ''.
*  PERFORM ALV_LAYOUT USING 15 'User ID' 'ERNAM' 'IT_FINAL' '' ''.



  WA_SORT-FIELDNAME = 'J_1ICHID'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT .
  CLEAR WA_SORT.

*  WA_SORT-FIELDNAME = 'GMNGA'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT .
*  CLEAR WA_SORT.
*  WA_SORT-FIELDNAME = 'WERKS'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT .
*  CLEAR WA_SORT.
*
*  WA_SORT-FIELDNAME = 'BUDAT'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT .
*  CLEAR WA_SORT.


*  WA_SORT-FIELDNAME = 'LITR'.
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT .
*  CLEAR WA_SORT.



ENDFORM.                    "FIELD_CAT


*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1         text
*      -->P2         text
*      -->P3         text
*      -->P4         text
*      -->P5         text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5 P6.

  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  APPEND WA_FCAT TO IT_FCAT.

ENDFORM .                    "ALV_LAYOUT




*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY.


  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
   I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = WA_LAYOUT
     IT_FIELDCAT                       = IT_FCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
   IT_SORT                           =  IT_SORT
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "ALV_DISPLAY




*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE .

  DATA:WA_LISTHEADER TYPE SLIS_LISTHEADER,
        IT_LISTHEADER TYPE SLIS_T_LISTHEADER.


  DATA:HED1 TYPE CHAR100,
       HED2 TYPE CHAR100,
       HED3 TYPE CHAR100,
       HED4 TYPE CHAR100.

  DATA:HED5 TYPE SY-DATUM.


  IF S_WERK-HIGH IS NOT INITIAL .

    CONCATENATE 'Plant  :' S_WERK-LOW 'To' S_WERK-HIGH INTO HED1 SEPARATED BY SPACE.
  ELSEIF S_WERK-LOW IS NOT INITIAL.
    CONCATENATE 'Plant  :' S_WERK-LOW INTO HED1 SEPARATED BY SPACE.
  ENDIF .

  IF S_PLNBE-HIGH IS NOT INITIAL .

    CONCATENATE 'Material Number :' S_PLNBE-LOW 'To' S_PLNBE-HIGH INTO HED2 SEPARATED BY SPACE."#EC CI_FLDEXT_OK[2215424] "Added by SPLABAP during code remediation
  ELSEIF S_PLNBE-LOW IS NOT INITIAL.
    CONCATENATE 'Material Number :' S_PLNBE-LOW INTO HED2 SEPARATED BY SPACE .
  ENDIF .

  IF S_BUDA-HIGH IS NOT INITIAL .

    CONCATENATE 'Posting Date :'
                    S_BUDA-LOW+6(2) '-'
                    S_BUDA-LOW+4(2) '-'
                    S_BUDA-LOW+0(4)        ' / '

                     S_BUDA-HIGH+6(2) '-'
                    S_BUDA-HIGH+4(2) '-'
                    S_BUDA-HIGH+0(4)

                     INTO HED3 .

  ELSEIF S_BUDA-LOW IS NOT INITIAL.

    CONCATENATE 'Posting Date :'
                    S_BUDA-LOW+6(2) '-'
                    S_BUDA-LOW+4(2) '-'
                    S_BUDA-LOW+0(4)      INTO HED3.
  ENDIF .


  IF S_CHA-HIGH IS NOT INITIAL .

    CONCATENATE 'Chapter ID :' S_CHA-LOW 'To' S_CHA-HIGH INTO HED4 SEPARATED BY SPACE .
  ELSEIF S_CHA-LOW IS NOT INITIAL.
    CONCATENATE 'Chapter ID :' S_CHA-LOW INTO HED4 SEPARATED BY SPACE .

  ENDIF .




  WA_LISTHEADER-TYP = 'H'.
  WA_LISTHEADER-INFO = 'Process Order Information System'.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .



  WA_LISTHEADER-TYP = 'S'.
  WA_LISTHEADER-INFO = HED1.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .


  WA_LISTHEADER-TYP = 'S'.
  WA_LISTHEADER-INFO = HED2.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .

  WA_LISTHEADER-TYP = 'S'.
  WA_LISTHEADER-INFO = HED3.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .

  WA_LISTHEADER-TYP = 'S'.
  WA_LISTHEADER-INFO = HED4.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_LISTHEADER
      I_LOGO             = 'ZLOGO'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

ENDFORM .                    "TOP_OF_PAGE
