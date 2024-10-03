*&---------------------------------------------------------------------*
*& Report  ZSALES_OFF_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSALES_OFF_DETAILS.

TABLES : KNVP,KNVV.

TYPES : BEGIN OF GS_KNVP,
    KUNNR TYPE  KUNNR,
    VKORG TYPE  VKORG,
    VTWEG	TYPE VTWEG,
    SPART TYPE  SPART,
    PARVW	TYPE PARVW,
    PERNR	TYPE PERNR_D,
    KUNN2 TYPE KUNN2,
  END OF GS_KNVP.

DATA : GT_KNVP TYPE TABLE OF GS_KNVP,
       WA_KNVP TYPE GS_KNVP.

TYPES: BEGIN OF GS_PA0001,
       PERNR TYPE PA0001-PERNR,
       ENAME TYPE PA0001-ENAME,
     END OF GS_PA0001.

DATA : GT_PA0001 TYPE TABLE OF GS_PA0001,
       WA_PA0001 TYPE GS_PA0001.


TYPES : BEGIN OF GS_KNA1,
        KUNNR TYPE KNA1-KUNNR,
        NAME1 TYPE KNA1-NAME1,
      END OF GS_KNA1.

DATA : GT_KNA1 TYPE TABLE OF GS_KNA1,
       WA_KNA1 TYPE GS_KNA1,
       GT_KUNN2 TYPE TABLE OF GS_KNA1,
       WA_KUNN2 TYPE GS_KNA1.


TYPES :  BEGIN OF GS_TSPAT,
         SPART TYPE TSPAT-SPART,
         VTEXT TYPE TSPAT-VTEXT,
      END OF GS_TSPAT.

DATA : GT_TSPAT TYPE TABLE OF GS_TSPAT,
       WA_TSPAT TYPE GS_TSPAT.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.


*.............................................................." added by mani on 28.12.2015
TYPES: BEGIN OF STR_KNVV,
        KUNNR TYPE KNVV-KUNNR,
        VKORG TYPE KNVV-VKORG,
        SPART TYPE KNVV-SPART,
        VKBUR TYPE KNVV-VKBUR,
      END OF STR_KNVV.

DATA:WA_KNVV TYPE STR_KNVV,
      IT_KNVV TYPE TABLE OF STR_KNVV.

*.............................................................." added by mani on 28.12.2015

TYPES : BEGIN OF TY_TPAUM,
    SPRAS TYPE TPAUM-SPRAS,
    PARVW TYPE TPAUM-PARVW,
    PABEZ TYPE TPAUM-PABEZ,
  END OF TY_TPAUM.

DATA:WA_TPAUM TYPE TY_TPAUM,
     IT_TPAUM TYPE TABLE OF TY_TPAUM.

TYPES : BEGIN OF TY_TPART,
    SPRAS TYPE TPART-SPRAS,
    PARVW TYPE TPART-PARVW,
    VTEXT TYPE TPART-VTEXT,

  END OF TY_TPART.

DATA:WA_TPART TYPE TY_TPART,
     IT_TPART TYPE TABLE OF TY_TPART.

TYPES : BEGIN OF GS_FINAL,
      KUNNR TYPE  KNVP-KUNNR,
      VKORG TYPE  KNVP-VKORG,
      VTWEG	TYPE KNVP-VTWEG,
      SPART TYPE  KNVP-SPART,
      VTEXT TYPE TSPAT-VTEXT,
      PARVW	TYPE KNVP-PARVW,
      NAME1 TYPE KNA1-NAME1,
      PERNR	TYPE KNVP-PERNR,
      ENAME TYPE PA0001-ENAME,
      VKBUR TYPE KNVV-VKBUR,
      NAME_VTEXT TYPE TPART-VTEXT,
      FLAG(1) TYPE C,
      KUNN2 TYPE KNVP-KUNN2,
END OF  GS_FINAL.


DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL,
       GT_FIELDCAT TYPE TABLE OF SLIS_FIELDCAT_ALV,
       WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV.


DATA : LV_VKORG TYPE KNVP-VKORG,
       LV_SPART TYPE KNVP-SPART,
       LV_KUNNR TYPE KNVP-KUNNR,
       LV_PARVW TYPE KNVP-PARVW,
       LV_VKBUR TYPE KNVV-VKBUR.


SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME .
SELECT-OPTIONS : SO_VKORG FOR LV_VKORG,
                 S_VKBUR FOR KNVV-VKBUR,
                 SO_SPART FOR LV_SPART,
                 SO_KUNNR FOR LV_KUNNR,
                 SO_PARVW FOR LV_PARVW.

SELECTION-SCREEN : END OF BLOCK B1.


*----------------------------------------------------------------------*
*       CLASS SALEOFF DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS SALEOFF DEFINITION.
  PUBLIC SECTION .
    METHODS : GET_DATA,
              AUTH_CHECK,
           BUILD_FIELDCATALOG,
           DISPLAY_DATA.

ENDCLASS.                    "SALEOFF DEFINITION

*----------------------------------------------------------------------*
*       CLASS SALEOFF IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS SALEOFF IMPLEMENTATION.
  METHOD GET_DATA.
    PERFORM GET_DATA.
  ENDMETHOD.                    "GET_DATA

  METHOD AUTH_CHECK.
    PERFORM AUTH_CHECK.
  ENDMETHOD.                    "AUTHCHECK

  METHOD BUILD_FIELDCATALOG.
    PERFORM BUILD_FIELDCATALOG.
  ENDMETHOD.                    "BUILD_FIELDCATALOG

  METHOD  DISPLAY_DATA.
    PERFORM  DISPLAY_DATA.
  ENDMETHOD.                    "DISPLAY_DATA
ENDCLASS.                    "SALEOFF IMPLEMENTATION

START-OF-SELECTION.
  DATA : SALEOFF TYPE REF TO SALEOFF  .

  CREATE OBJECT SALEOFF.

  CALL METHOD SALEOFF->GET_DATA.
  CALL METHOD SALEOFF->BUILD_FIELDCATALOG.
  CALL METHOD SALEOFF->DISPLAY_DATA.

  "GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  IF S_VKBUR IS INITIAL .
    SELECT
      KUNNR
      VKORG
      VTWEG
      SPART
      PARVW
      PERNR
      KUNN2
    FROM KNVP INTO CORRESPONDING FIELDS OF TABLE GT_KNVP WHERE PARVW IN SO_PARVW AND KUNNR IN SO_KUNNR AND VKORG IN SO_VKORG AND SPART IN SO_SPART .

    SELECT
      SPRAS
      PARVW
      PABEZ
      FROM
      TPAUM INTO TABLE IT_TPAUM FOR ALL ENTRIES IN  GT_KNVP WHERE SPRAS = 'EN' AND PARVW = GT_KNVP-PARVW ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    SELECT
      SPRAS
      PARVW
      VTEXT
      FROM
      TPART INTO TABLE IT_TPART WHERE SPRAS = 'EN'." FOR ALL ENTRIES IN  IT_TPAUM WHERE SPRAS = 'EN' AND PARVW = IT_TPAUM-PARVW.

*  DELETE ADJACENT DUPLICATES FROM GT_KNVP COMPARING KUNNR  VKORG SPART PARVW.

    IF GT_KNVP IS NOT INITIAL .
      SELECT  PERNR
              ENAME FROM PA0001 INTO TABLE GT_PA0001 FOR ALL ENTRIES IN GT_KNVP WHERE PERNR = GT_KNVP-PERNR.
    ENDIF.

    SELECT KUNNR
           NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_KNVP WHERE KUNNR  = GT_KNVP-KUNNR.

       SELECT KUNNR
           NAME1 FROM KNA1 INTO TABLE GT_KUNN2 FOR ALL ENTRIES IN GT_KNVP WHERE KUNNR  = GT_KNVP-KUNN2.



    SELECT SPART
              VTEXT
              FROM TSPAT INTO TABLE GT_TSPAT
              FOR ALL ENTRIES IN GT_KNVP
              WHERE SPART = GT_KNVP-SPART
              AND SPRAS = 'EN'.

    SELECT KUNNR
           VKORG
           SPART
           VKBUR FROM KNVV
           INTO TABLE IT_KNVV
           FOR ALL ENTRIES IN GT_KNVP
           WHERE KUNNR = GT_KNVP-KUNNR AND VKBUR IN S_VKBUR .


    LOOP AT GT_KNVP INTO WA_KNVP ." where vkbur = S_VKBUR.
      MOVE-CORRESPONDING WA_KNVP TO WA_FINAL.
*      READ TABLE GT_PA0001 INTO WA_PA0001 WITH KEY PERNR = WA_KNVP-PERNR.
*      IF SY-SUBRC = 0.
*        WA_FINAL-ENAME = WA_PA0001-ENAME.
*      ENDIF.
      READ TABLE IT_TPART INTO WA_TPART WITH KEY PARVW = WA_KNVP-PARVW.
       IF WA_KNVP-PERNR NE 0.
        READ TABLE GT_PA0001 INTO WA_PA0001 WITH KEY PERNR = WA_KNVP-PERNR.

*        READ TABLE IT_TPAUM INTO WA_TPAUM WITH KEY PARVW = WA_KNVP-PARVW.
        IF SY-SUBRC = 0.
          WA_FINAL-ENAME = WA_PA0001-ENAME.
*          WA_FINAL-PARVW = WA_TPAUM-PABEZ.
          WA_FINAL-NAME_VTEXT = WA_TPART-VTEXT.
        ENDIF.
      ELSE.
        READ TABLE GT_KUNN2 INTO WA_KUNN2 WITH KEY KUNNR = WA_KNVP-KUNN2.
        IF SY-SUBRC = 0.
          WA_FINAL-PERNR = WA_KUNN2-KUNNR.
          WA_FINAL-ENAME = WA_KUNN2-NAME1.
          WA_FINAL-NAME_VTEXT = WA_TPART-VTEXT.
        ENDIF.
      ENDIF.

      READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_KNVP-KUNNR.
      IF SY-SUBRC = 0.
        WA_FINAL-NAME1 = WA_KNA1-NAME1.
      ENDIF.


      READ TABLE IT_KNVV INTO WA_KNVV
      WITH KEY KUNNR = WA_KNVP-KUNNR .
      IF SY-SUBRC = 0.
        WA_FINAL-VKBUR = WA_KNVV-VKBUR .
      ENDIF .


      READ TABLE GT_TSPAT INTO WA_TSPAT WITH KEY SPART = WA_KNVP-SPART.
      IF SY-SUBRC = 0.
        WA_FINAL-VTEXT = WA_TSPAT-VTEXT.
      ENDIF.


      APPEND WA_FINAL TO GT_FINAL.
      CLEAR WA_FINAL.
    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL .
         IF WA_FINAL-PARVW = 'L1' OR WA_FINAL-PARVW = 'L2' OR WA_FINAL-PARVW = 'L3' OR WA_FINAL-PARVW = 'L4' OR WA_FINAL-PARVW = 'L5' .
    READ TABLE IT_TPAUM INTO WA_TPAUM WITH KEY PARVW = WA_FINAL-PARVW.
      WA_FINAL-PARVW =  WA_TPAUM-PABEZ.
      ENDIF.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PARVW .
      CLEAR WA_FINAL.
    ENDLOOP.
    CLEAR: WA_FINAL,WA_TPAUM,WA_TPART,WA_KNA1,WA_KNVP,WA_KNVV,WA_KUNN2.

  ELSE.

    SELECT
      KUNNR
      VKORG
      VTWEG
      SPART
      PARVW
      PERNR
      KUNN2
    FROM KNVP INTO CORRESPONDING FIELDS OF TABLE GT_KNVP WHERE PARVW IN SO_PARVW AND KUNNR IN SO_KUNNR AND VKORG IN SO_VKORG AND SPART IN SO_SPART .

 SELECT
      SPRAS
      PARVW
      PABEZ
      FROM
      TPAUM INTO TABLE IT_TPAUM FOR ALL ENTRIES IN  GT_KNVP WHERE SPRAS = 'EN' AND PARVW = GT_KNVP-PARVW ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    SELECT
      SPRAS
      PARVW
      VTEXT
      FROM
      TPART INTO TABLE IT_TPART WHERE SPRAS = 'EN'." FOR ALL ENTRIES IN  IT_TPAUM WHERE SPRAS = 'EN' AND PARVW = IT_TPAUM-PARVW.

    IF GT_KNVP IS NOT INITIAL .
      SELECT  PERNR
              ENAME FROM PA0001 INTO TABLE GT_PA0001 FOR ALL ENTRIES IN GT_KNVP WHERE PERNR = GT_KNVP-PERNR.
    ENDIF.

    SELECT KUNNR
           NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_KNVP WHERE KUNNR  = GT_KNVP-KUNNR.
       SELECT KUNNR
           NAME1 FROM KNA1 INTO TABLE GT_KUNN2 FOR ALL ENTRIES IN GT_KNVP WHERE KUNNR  = GT_KNVP-KUNN2.

    SELECT SPART
              VTEXT
              FROM TSPAT INTO TABLE GT_TSPAT
              FOR ALL ENTRIES IN GT_KNVP
              WHERE SPART = GT_KNVP-SPART
              AND SPRAS = 'EN'.
    SELECT KUNNR
           VKORG
           SPART
           VKBUR FROM KNVV
           INTO TABLE IT_KNVV
           FOR ALL ENTRIES IN GT_KNVP
           WHERE KUNNR = GT_KNVP-KUNNR AND VKBUR IN S_VKBUR .

    LOOP AT GT_KNVP INTO WA_KNVP ." where vkbur = S_VKBUR.
      MOVE-CORRESPONDING WA_KNVP TO WA_FINAL.
*      READ TABLE GT_PA0001 INTO WA_PA0001 WITH KEY PERNR = WA_KNVP-PERNR.
*      IF SY-SUBRC = 0.
*        WA_FINAL-ENAME = WA_PA0001-ENAME.
*      ENDIF.
  READ TABLE IT_TPART INTO WA_TPART WITH KEY PARVW = WA_KNVP-PARVW.
      IF WA_KNVP-PERNR NE 0.
        READ TABLE GT_PA0001 INTO WA_PA0001 WITH KEY PERNR = WA_KNVP-PERNR.

*        READ TABLE IT_TPAUM INTO WA_TPAUM WITH KEY PARVW = WA_KNVP-PARVW.
        IF SY-SUBRC = 0.
          WA_FINAL-ENAME = WA_PA0001-ENAME.
*          WA_FINAL-PARVW = WA_TPAUM-PABEZ.
          WA_FINAL-NAME_VTEXT = WA_TPART-VTEXT.
        ENDIF.
      ELSE.
        READ TABLE GT_KUNN2 INTO WA_KUNN2 WITH KEY KUNNR = WA_KNVP-KUNN2.
        IF SY-SUBRC = 0.
          WA_FINAL-PERNR = WA_KUNN2-KUNNR.
          WA_FINAL-ENAME = WA_KUNN2-NAME1.
          WA_FINAL-NAME_VTEXT = WA_TPART-VTEXT.
        ENDIF.
      ENDIF.

      READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_KNVP-KUNNR.
      IF SY-SUBRC = 0.
        WA_FINAL-NAME1 = WA_KNA1-NAME1.
      ENDIF.


      READ TABLE IT_KNVV INTO WA_KNVV
      WITH KEY KUNNR = WA_KNVP-KUNNR .
      IF SY-SUBRC = 0.
        WA_FINAL-VKBUR = WA_KNVV-VKBUR .
      ENDIF .



      READ TABLE GT_TSPAT INTO WA_TSPAT WITH KEY SPART = WA_KNVP-SPART.
      IF SY-SUBRC = 0.
        WA_FINAL-VTEXT = WA_TSPAT-VTEXT.
      ENDIF.

      IF WA_FINAL-VKBUR IS NOT INITIAL .
        APPEND WA_FINAL TO GT_FINAL.
        CLEAR WA_FINAL.
      ENDIF.
    ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL .
    IF WA_FINAL-PARVW = 'L1' OR WA_FINAL-PARVW = 'L2' OR WA_FINAL-PARVW = 'L3' OR WA_FINAL-PARVW = 'L4' OR WA_FINAL-PARVW = 'L5' .
    READ TABLE IT_TPAUM INTO WA_TPAUM WITH KEY PARVW = WA_FINAL-PARVW.
      WA_FINAL-PARVW =  WA_TPAUM-PABEZ.
      ENDIF.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PARVW .
      CLEAR WA_FINAL.
    ENDLOOP.
  CLEAR: WA_FINAL,WA_TPAUM,WA_TPART,WA_KNA1,WA_KNVP,WA_KNVV,WA_KUNN2.
  ENDIF.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  WA_FIELDCAT-FIELDNAME   = 'VKORG'.
  WA_FIELDCAT-SELTEXT_M   = 'SALES ORGANIZATION'.
  WA_FIELDCAT-COL_POS     = 1.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'VKBUR'.                                  " added by mani on 28.12.2015
  WA_FIELDCAT-SELTEXT_M   = 'SALES OFFICE'.
  WA_FIELDCAT-COL_POS     = 2.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'SPART'.
  WA_FIELDCAT-SELTEXT_M   = 'DIVISION'.
  WA_FIELDCAT-COL_POS     = 3.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'VTEXT'.
  WA_FIELDCAT-SELTEXT_M   = 'DIVISION DESCRIPTION'.
  WA_FIELDCAT-COL_POS     = 4.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'KUNNR'.
  WA_FIELDCAT-SELTEXT_M   = 'CUSTOMER CODE'.
  WA_FIELDCAT-COL_POS     = 5.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
  WA_FIELDCAT-SELTEXT_M   = 'CUSTOMER NAME'.
  WA_FIELDCAT-COL_POS     = 7.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'PARVW'.
  WA_FIELDCAT-SELTEXT_M   = 'PARTNER FUNCTION'.
  WA_FIELDCAT-COL_POS     = 8.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'NAME_VTEXT'.
  WA_FIELDCAT-SELTEXT_M   = 'PARTNER DESC'.
  WA_FIELDCAT-COL_POS     = 9.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'PERNR'.
  WA_FIELDCAT-SELTEXT_M   = 'PERSONAL NUMBER'.
  WA_FIELDCAT-COL_POS     = 10.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ENAME'.
  WA_FIELDCAT-SELTEXT_M   = 'EMPLOYEE NAME'.
  WA_FIELDCAT-COL_POS     = 11.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.





*  WA_FIELDCAT-FIELDNAME   = 'KUNN2'.
*  WA_FIELDCAT-SELTEXT_M   = 'CUSTOMER'.
*  WA_FIELDCAT-COL_POS     = 3.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
   I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = GT_FIELDCAT
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = 'X'
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
      T_OUTTAB                          = GT_FINAL[]
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " DISPLAY_DATA








*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE .

  DATA:WA_LISTHEADER TYPE SLIS_LISTHEADER,
        IT_LISTHEADER TYPE SLIS_T_LISTHEADER.

  WA_LISTHEADER-TYP = 'H'.
  WA_LISTHEADER-INFO = 'CUSTOMER vs PARTNERS'.
  APPEND WA_LISTHEADER TO IT_LISTHEADER .
  CLEAR WA_LISTHEADER .

  WA_LISTHEADER-TYP = 'S'.
  CONCATENATE 'Report Run Date : '

           SY-DATUM+6(2) '-'
           SY-DATUM+4(2) '-'
           SY-DATUM+0(4) INTO WA_LISTHEADER-INFO .
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
*&---------------------------------------------------------------------*
*&      Form  AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTH_CHECK .

  LOOP AT GT_FINAL INTO WA_FINAL.
    AUTHORITY-CHECK OBJECT 'ZINVOICE'
    ID 'ZVKBUR' FIELD WA_FINAL-VKBUR
    ID 'ZSPART' FIELD WA_FINAL-SPART
   " ID 'ZSPART' DUMMY
    ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC NE 0.
      WA_FINAL-FLAG = 'x' .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FLAG.
      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
    ENDIF.
    CLEAR: WA_FINAL.
  ENDLOOP.

  DELETE GT_FINAL WHERE FLAG = 'X' .

ENDFORM.                    " AUTH_CHECK
