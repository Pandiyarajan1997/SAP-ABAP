*  * * * *  *  *&---------------------------------------------------------------------*
*& Module Pool       ZTRIP_SHEET
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE ZTRIPSHEETTOP                             .    " global Data

* INCLUDE ZTRIPSHEETO01                           .  " PBO-Modules
* INCLUDE ZTRIPSHEETI01                           .  " PAI-Modules
* INCLUDE ZTRIPSHEETF01                           .  " FORM-Routines

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'Z9000'.
  SET TITLEBAR 'Z9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE OKAY_CODE.

    WHEN 'BACK'.
      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'NEW'.
      CALL SCREEN '9001'.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'Z9001'.
  SET TITLEBAR 'Z9001'.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE  USER_COMMAND_9001 INPUT.

  CASE OKAY_CODE.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.


    WHEN 'CREATE'.
*

      SELECT * FROM ZTRIP_ST INTO TABLE  IT_ZTRIP_ST.

      SORT IT_ZTRIP_ST BY UNIQ1 .

      LOOP AT IT_ZTRIP_ST INTO WA_ZTRIP_ST .

        L_COUNT = WA_ZTRIP_ST-UNIQ1.

      ENDLOOP.

      L_COUNT = L_COUNT + 1.


      IF SY-SUBRC = 0.

        CLEAR: WA_ZTRIP_ST,WA_VBRP,LV_VBRP.

        REFRESH :IT_ZTRIP_ST.", LV_IT_ZTRIP_ST.
      ENDIF.




    WHEN 'RESET'.

      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP,WA_VBRK1.

      REFRESH :IT_ZTRIP_ST,IT_VBRK1.

    WHEN 'DIS'.
      CLEAR ZTRIP_ST.

      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.

      REFRESH :IT_ZTRIP_ST.", LV_IT_ZTRIP_ST.
      SUBMIT ZTRIPSHEET VIA SELECTION-SCREEN
           AND RETURN.



      CLEAR ZTRIP_ST.
    WHEN 'CHANGE'.
      REFRESH IT_ZTRIP_ST.
      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.
      CLEAR ZTRIP_ST.
      CLEAR ZLOG2.
      CALL SCREEN 9003.
      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.
*    WHEN 'SM'.
*      BREAK-POINT.
*
*
*      SELECT * FROM VBRP INTO TABLE IT_VBRP.
**        WHERE VBELN = WA_ZTRIP_ST-INVOICENO.
*
*      READ TABLE IT_VBRP INTO WA_VBRP
*      WITH KEY VBELN = ZTRIP_ST-INVOICENO.


    WHEN 'PRINT'.
      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.
      REFRESH IT_ZTRIP_ST.
      SUBMIT ZTRIPSHEET_SMARTFORM VIA SELECTION-SCREEN
             AND RETURN.

      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.

    WHEN OTHERS.


  ENDCASE.






ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  GO_OUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GO_OUT INPUT.
  CASE OKAY_CODE.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'PRINT'.
      SUBMIT ZTRIPSHEET_SMARTFORM VIA SELECTION-SCREEN
           AND RETURN.

    WHEN 'CHANGE'.
      CALL SCREEN 9003.
      CLEAR ZTRIP_ST.
      CLEAR ZLOG2.

    WHEN 'DIS'.
      CLEAR ZTRIP_ST.
      SUBMIT ZTRIPSHEET VIA SELECTION-SCREEN
           AND RETURN.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

  ENDCASE.


ENDMODULE.                 " GO_OUT  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'Z9002'.
  SET TITLEBAR 'Z9002'.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  CASE OKAY_CODE.
    WHEN 'BACK'.

      CLEAR ZLOG2.
      CLEAR ZTRIP_ST.
      CALL SCREEN 9003.
*      LEAVE PROGRAM.


    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SUBMIT'.

      SELECT  * FROM ZTRIP_ST
      INTO TABLE IT_ZTRIP_ST
      WHERE UNIQ1 = ZTRIP_ST-UNIQ1.
*      BREAK-POINT.
*
*      SELECT SINGLE * FROM ZTRIP_ST
*      INTO WA_ZTRIP_ST
*      WHERE UNIQ1 = ZTRIP_ST-UNIQ1." AND INVOICENO = ZTRIP_ST-INVOICENO.
*
*      SELECT * FROM VBRP INTO TABLE IT_VBRP.    "added by testet
**        WHERE VBELN = WA_ZTRIP_ST-INVOICENO.

*      READ TABLE IT_VBRP INTO WA_VBRP
*      WITH KEY VBELN = ZTRIP_ST-INVOICENO.

      IF SY-SUBRC = 0.
*        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
*        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
*      ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
*        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
*        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
*        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
*        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
*      ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
*      ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.
*
*
*      ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
*        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.
*
*
*      ZTRIP_ST-TDATE = SY-DATUM.
*      ZTRIP_ST-TIME

*
*
      ELSE.
*
        MESSAGE 'Record Not Found' TYPE 'E'.
      ENDIF.


    WHEN 'DD'.


      SELECT  * FROM ZTRIP_ST
           INTO TABLE IT_ZTRIP_ST
           WHERE UNIQ1 = ZTRIP_ST-UNIQ1.

*
*    WHEN 'CHG'.
*      WA_ZTRIP_ST-INVOICENO = ZTRIP_ST-INVOICENO  .
*      WA_ZTRIP_ST-INVOICEDATE = ZTRIP_ST-INVOICEDATE  .
*      WA_ZTRIP_ST-SKUDEALERNAME = ZTRIP_ST-SKUDEALERNAME  .
*      WA_ZTRIP_ST-QUANTITY = ZTRIP_ST-QUANTITY  .
*      " WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS = ZTRIP_ST-NOOFCARTONSPAILSDRUMS  .
*      WA_ZTRIP_ST-NETVOLUMEPERINVOICE = ZTRIP_ST-NETVOLUMEPERINVOICE  .
*      WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE = ZTRIP_ST-GROSSVOLUMEPERINVOICE  .
*      WA_ZTRIP_ST-TRANSPORTERNAMELRNO = ZTRIP_ST-TRANSPORTERNAMELRNO  .
*      WA_ZTRIP_ST-CARTONS = ZTRIP_ST-CARTONS  .
*      WA_ZTRIP_ST-VEHICLENODRIVERNAME = ZTRIP_ST-VEHICLENODRIVERNAME  .
*
*
*      WA_ZTRIP_ST-TDATE = SY-DATUM.
*      WA_ZTRIP_ST-TIME = SY-UZEIT.
*
*
*
*
*      MODIFY    ZTRIP_ST FROM WA_ZTRIP_ST.
*
*      IF SY-SUBRC = 0.
*        MESSAGE 'Record Changes Sucessfully' TYPE 'S'.
*
*      ELSE.
*        MESSAGE 'Enter A Valid Data' TYPE 'E'.
*      ENDIF.



    WHEN 'DELETE'.

      GET CURSOR LINE LI.

*      DELETE IT_ZTRIP_ST FROM WA_ZTRIP_ST TO LI.

      LI = LI + 1.

      DELETE IT_ZTRIP_ST FROM  LI.

      LOOP AT IT_ZTRIP_ST INTO WA_ZTRIP_ST.


      ENDLOOP.

      DELETE ZTRIP_ST FROM WA_ZTRIP_ST.



      IF SY-SUBRC = 0.
        MESSAGE 'Record Delete Sucessfully' TYPE 'S'.

      ELSE.
        MESSAGE 'Record Not Deleted' TYPE 'E'.
      ENDIF.
*      CLEAR ZTRIP_ST.



    WHEN 'REGG'.


      CLEAR: ZLOG2.
      SET SCREEN 9004.


    WHEN 'CL'.

      CLEAR ZTRIP_ST.


    WHEN OTHERS.
  ENDCASE.








ENDMODULE.                 " USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.
  SET PF-STATUS 'Z9003'.
  SET TITLEBAR 'Z9003'.

ENDMODULE.                 " STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003 INPUT.

  CASE OKAY_CODE.
    WHEN 'BACK'.

      CLEAR ZTRIP_ST.
      CLEAR WA_ZTRIP_ST.
      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.
*      LEAVE PROGRAM.
      SET SCREEN 9001.

      CLEAR ZTRIP_ST.
      CLEAR WA_ZTRIP_ST.
      CLEAR: ZTRIP_ST,WA_ZTRIP_ST,WA_VBRP,LV_VBRP.
      REFRESH IT_ZTRIP_ST.




    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'LOGIN'.
      SELECT SINGLE * FROM ZLOG2
        INTO WA_ZLOG2 WHERE UNAME = ZLOG2-UNAME
        AND PWORD = ZLOG2-PWORD.



      IF SY-SUBRC = 0.
*        SET SCREEN 9002.
        CLEAR ZTRIP_ST.
        CLEAR WA_ZTRIP_ST.
        CALL SCREEN 9002.
        MESSAGE 'Sucessfully Login' TYPE 'S'.
      ELSE.
        MESSAGE 'Enter A Correct Password' TYPE 'E'.
      ENDIF.








  ENDCASE.





ENDMODULE.                 " USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9004 OUTPUT.
  SET PF-STATUS 'Z9004'.
  SET TITLEBAR 'Z9004'.

ENDMODULE.                 " STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9004 INPUT.

  CASE OKAY_CODE.
    WHEN 'BACK'.
      CLEAR ZLOG2.
      CLEAR ZTRIP_ST.
      CLEAR WA_ZTRIP_ST.
      SET SCREEN 9003.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.


      WA_ZLOG2-PWORD = ZLOG2-PWORD.

      IF NAME1  EQ   WA_ZLOG2-PWORD.




        IF SY-SUBRC = 0.

          MODIFY ZLOG2 FROM WA_ZLOG2.

          MESSAGE 'Password Change sucessfully' TYPE 'S'.

        ELSE.

          MESSAGE 'Enter A Valid Data' TYPE 'E'.
        ENDIF.

      ELSE.

        MESSAGE 'Enter A Correct Password' TYPE 'E'.
      ENDIF.

  ENDCASE.



ENDMODULE.                 " USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY OUTPUT.

  IF SY-UCOMM = 'CREATE'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GG'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.



ENDMODULE.                 " MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIF  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIF OUTPUT.


  IF SY-UCOMM = 'SUBMIT'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GH'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.
*
*


  IF SY-UCOMM = 'CHG'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GH'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.

  IF SY-UCOMM = 'CHG'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GG'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.

  IF SY-UCOMM = 'DD'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GH'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.

  IF SY-UCOMM = 'DD'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GG'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.
  IF SY-UCOMM = 'DELETE'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GH'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.

  IF SY-UCOMM = 'DELETE'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GG'.
        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.

*
*
*
*  IF SY-UCOMM = 'CHG'.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ 'GG'.
*        SCREEN-INPUT = '0'.
**        SCREEN-INVISIBLE = '1'.
*      ENDIF.
*      MODIFY SCREEN.
*
*    ENDLOOP.
*
*
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ 'GH'.
*        SCREEN-INPUT = '0'.
**        SCREEN-INVISIBLE = '1'.
*      ENDIF.
*      MODIFY SCREEN.
*
*    ENDLOOP.
*  ENDIF.



ENDMODULE.                 " MODIF  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INSERT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE INSERT_DATA INPUT.
*
*
*ENDMODULE.                 " INSERT_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  INSERT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  INSERT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INSERT_DATA INPUT.

  CASE OKAY_CODE.

    WHEN 'CREATE'.

      SELECT VBELN FROM VBRK
                   INTO TABLE IT_VBRK1
                   WHERE VBELN = WA_ZTRIP_ST-INVOICENO.

      IF SY-SUBRC = 0.


        SELECT * FROM ZTRIP_ST INTO TABLE IT_ZTRIP_ST5 ORDER BY PRIMARY KEY.  "Added by SPLABAP during code remediation

*      READ TABLE IT_ZTRIP_ST INTO WA_ZTRIP_ST
*      WITH KEY INVOICENO = WA_ZTRIP_ST-INVOICENO.

        LOOP AT IT_ZTRIP_ST5 INTO WA_ZTRIP_ST
              WHERE INVOICENO = WA_ZTRIP_ST-INVOICENO.
          MESSAGE 'Already Created' TYPE 'E' .

          CALL SCREEN 9001.
        ENDLOOP.

*      IF  SY-SUBRC = 0.
*        MESSAGE 'Already Created' TYPE 'I' .
*        CALL SCREEN 9001.

*      ELSEIF  SY-SUBRC  <> 0 .



        WA_ZTRIP_ST-UNIQ1 =  L_COUNT .

        WA_ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO  .
        WA_ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE  .
        WA_ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY  .
        WA_ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE  .
        WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE  .

        WA_ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS  .

        WA_ZTRIP_ST-TRANSPORTERNAMELRNO = ZTRIP_ST-TRANSPORTERNAMELRNO  .
        WA_ZTRIP_ST-VEHICLENODRIVERNAME = ZTRIP_ST-VEHICLENODRIVERNAME  .
        WA_ZTRIP_ST-SKUDEALERNAME = ZTRIP_ST-SKUDEALERNAME  .

        WA_ZTRIP_ST-TDATE = SY-DATUM.
        WA_ZTRIP_ST-TIME = SY-UZEIT.



        WA_ZTRIP_ST-CITY = WA_ZTRIP_ST-CITY.
        WA_ZTRIP_ST-NAME = WA_ZTRIP_ST-NAME.






        APPEND WA_ZTRIP_ST TO IT_ZTRIP_ST .
        CLEAR WA_ZTRIP_ST  .



        MESSAGE 'Sucessfully Created' TYPE 'S'.
        ZTRIP_ST-UNIQ1 = L_COUNT.
        ZTRIP_ST-TDATE = SY-DATUM.
        ZTRIP_ST-TIME = SY-UZEIT.
*
*
*        SELECT SINGLE * FROM ZTRIP_ST INTO WA_ZTRIP_ST WHERE INVOICENO = WA_ZTRIP_ST-INVOICENO.
*        IF SY-SUBRC = 0.
*
*          ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO  .
*          ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME  .
*        ENDIF.
*      ENDIF.



      ELSE.

        MESSAGE 'Entry does not exit in VBRK (check entry)' TYPE 'E'.


      ENDIF.

  ENDCASE.
ENDMODULE.                 " INSERT_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAVE_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SAVE_DATA INPUT.


  CASE OKAY_CODE.

    WHEN 'CREATE'.

      MODIFY ZTRIP_ST FROM TABLE IT_ZTRIP_ST .

  ENDCASE.


ENDMODULE.                 " SAVE_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY OUTPUT.



  CASE OKAY_CODE.
*
*
    WHEN 'SM'.





      SELECT * FROM VBRP INTO TABLE IT_VBRP
        WHERE VBELN = WA_ZTRIP_ST-INVOICENO.

*
*
      READ TABLE IT_VBRP INTO WA_VBRP
      WITH KEY VBELN = ZTRIP_ST-INVOICENO.


      IF SY-SUBRC = 0.
**      VBRP-VBELN = WA_ZTRIP_ST-INVOICENO  .
*      VBRP-VOLUM = WA_ZTRIP_ST-NETVOLUMEPERINVOICE  .
*      VBRP-BRGEW = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE  .


*         WA_ZTRIP_ST-INVOICENO = ZTRIP_ST-INVOICENO  .
        VBRP-VBELN = WA_ZTRIP_ST-INVOICENO  .
*    WA_ZTRIP_ST-INVOICEDATE = ZTRIP_ST-INVOICEDATE  .
*    WA_ZTRIP_ST-QUANTITY = ZTRIP_ST-QUANTITY  .
*    WA_ZTRIP_ST-NETVOLUMEPERINVOICE = ZTRIP_ST-NETVOLUMEPERINVOICE  .
*    WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE = ZTRIP_ST-GROSSVOLUMEPERINVOICE  .
*    WA_ZTRIP_ST-TRANSPORTERNAMELRNO = ZTRIP_ST-TRANSPORTERNAMELRNO  .
*    WA_ZTRIP_ST-VEHICLENODRIVERNAME = ZTRIP_ST-VEHICLENODRIVERNAME  .
*    WA_ZTRIP_ST-CARTONS = ZTRIP_ST-CARTONS  .
      ENDIF.



  ENDCASE.

ENDMODULE.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4HELP  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE F4HELP INPUT.
**
**  TYPES:BEGIN OF STR,
**         VBELN TYPE VBRK-VBELN,
**         FKDAT TYPE VBRK-FKDAT,
***        FKIMG TYPE VBRP-FKIMG,
***        BRGEW TYPE VBRP-BRGEW,
**     END OF STR.
**
**  DATA:IT_VBRK1 TYPE  TABLE OF STR.
**  DATA:WA_VBRK1 TYPE   STR.
**
**
**  DATA: RETURN_TAB  TYPE TABLE OF DDSHRETVAL .
**  DATA:WA_TAB TYPE  DDSHRETVAL .
**
**  DATA:FM TYPE TABLE OF DSELC,
**        WA_FM TYPE DSELC.
**  DATA:MARK TYPE DDSHMARKS.
**
**
**
**
**  DATA:SHLP TYPE SHLP_DESCR,
**        CALLCONTROL TYPE DDSHF4CTRL,
**        SHLP_TAB TYPE TABLE OF SHLP_DESCR_TAB_T,
**        RECORD_TAB TYPE TABLE OF SEAHLPRES.
**
**
**
**  SELECT VBELN FKDAT FROM VBRK
**     INTO TABLE IT_VBRK1 .
**
**
**
***  WA_TAB-FIELDNAME = 'VBELN'.
***  WA_TAB-RETFIELD = 'INVOICENO'.
***  APPEND WA_TAB TO  RETURN_TAB .
***
***
***
***
***  WA_TAB-FIELDNAME = 'FKDAT'.
***  WA_TAB-RETFIELD = 'INVOICEDATE'.
***  APPEND WA_TAB TO  RETURN_TAB .
***
***
**  WA_FM-FLDNAME = 'VBELN'.
**  WA_FM-DYFLDNAME = 'INVOICENO'.
**  APPEND WA_FM TO FM.
**  CLEAR WA_FM.
**
**  WA_FM-FLDNAME = 'FKDAT'.
**  WA_FM-DYFLDNAME = 'INVOICEDATE'.
**  APPEND WA_FM TO FM.
**  CLEAR WA_FM.
**
**
**
**  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
**    EXPORTING
***   DDIC_STRUCTURE         = ''
**      RETFIELD               = 'VBELN'
***   PVALKEY                = ' '
**     DYNPPROG               = SY-REPID
**     DYNPNR                 = SY-DYNNR
**     DYNPROFIELD            = 'INVOICENO'       " MY ZTABLE FIELD
***   STEPL                  = 0
***   WINDOW_TITLE           =
***   VALUE                  = ' '
**     VALUE_ORG              = 'S'
***   MULTIPLE_CHOICE        = 'X'
***   DISPLAY                = 'X'
***   CALLBACK_PROGRAM       = ' '
***   CALLBACK_FORM          = ' '
**   MARK_TAB               =  MARK
*** IMPORTING
***   USER_RESET             =
**    TABLES
**      VALUE_TAB              = IT_VBRK1                     " MY INTERNAL TABLE
***   FIELD_TAB              =
**     RETURN_TAB             =  RETURN_TAB
**   DYNPFLD_MAPPING        = FM
** EXCEPTIONS
**   PARAMETER_ERROR        = 1
**   NO_VALUES_FOUND        = 2
**   OTHERS                 = 3
**            .
**  IF SY-SUBRC <> 0.
**    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**  ENDIF.
*
*
*
*  TABLES:VBRK.
*
*  DATA : FROM_F4.
**  PARAMETERS : VBELN TYPE VBRK-VBELN.",
**             CONNID TYPE SPFLI-CONNID,
**             FLDATE TYPE SFLIGHT-FLDATE.
*
*  DATA : ITAB TYPE TABLE OF VBRK WITH HEADER LINE.
*  DATA : FMAP TYPE TABLE OF DSELC WITH HEADER LINE.
*
*
*
**AT SELECTION-SCREEN ON VALUE-REQUEST FOR VBELN." If it is a selection Screen
***else in PROCESS ON VALUE REQUEST
*  FROM_F4 = 'X'. " This indicates the Value come from F4 only
*  SELECT * FROM VBRK INTO TABLE ITAB.
*  SORT ITAB BY VBELN FKDAT." CARRID CONNID FLDATE.
**  DELETE ADJACENT DUPLICATES FROM ITAB
**  COMPARING CARRID CONNID FLDATE.
*  FMAP-FLDNAME = 'VBELN'.
*  FMAP-DYFLDNAME = 'INVOICENO'.
*  APPEND FMAP.
*  FMAP-FLDNAME = 'FKDAT'.
*  FMAP-DYFLDNAME = 'INVOICEDATE'.
*  APPEND FMAP.
*
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**     DDIC_STRUCTURE         = 'VBRK'
*      RETFIELD               = 'VBELN'
**     PVALKEY                = ' '
*     DYNPPROG               = SY-REPID
*     DYNPNR                 = SY-DYNNR
*     DYNPROFIELD            = 'INVOICENO'
**     STEPL                  = 0
**     WINDOW_TITLE           =
**     VALUE                  = ' '
*     VALUE_ORG              = 'S'
**     MULTIPLE_CHOICE        = 'X'
**     DISPLAY                = ' '
**     CALLBACK_PROGRAM       = ' '
**     CALLBACK_FORM          = ' '
**     MARK_TAB               =
**   IMPORTING
**     USER_RESET             =
*    TABLES
*      VALUE_TAB              = ITAB
**     FIELD_TAB              =
**     RETURN_TAB             =
*     DYNPFLD_MAPPING        = FMAP
**   EXCEPTIONS
**     PARAMETER_ERROR        = 1
**     NO_VALUES_FOUND        = 2
**     OTHERS                 = 3
*            ." Just Execute this pilot program and verify
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*
*
*
*
*
*
*ENDMODULE.                    "F4HELP INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_HELP1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_DATA INPUT.


  CASE OKAY_CODE.




    WHEN 'SB'.

      SELECT VBELN
             FKIMG
             BRGEW
             VOLUM
             ERDAT FROM VBRP
            INTO TABLE  LV_IT_VBRP
        WHERE VBELN = WA_ZTRIP_ST-INVOICENO.

      IF SY-SUBRC = 0.

        LOOP AT LV_IT_VBRP INTO LV_VBRP.

          LV_QTY = LV_QTY + LV_VBRP-FKIMG.
          LV_VOLUM = LV_VOLUM + LV_VBRP-VOLUM.
          LV_GROSS = LV_GROSS + LV_VBRP-BRGEW.
        ENDLOOP.
*
        WA_ZTRIP_ST-QUANTITY = LV_QTY.
        WA_ZTRIP_ST-NETVOLUMEPERINVOICE = LV_VOLUM.
        WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE = LV_GROSS.
        WA_ZTRIP_ST-INVOICEDATE = LV_VBRP-ERDAT.

*    APPEND ZTRIP_ST-QUANTITY TO ZTRIP_ST.
*        APPEND WA_ZTRIP_ST TO IT_ZTRIP_ST.
      ENDIF.


      SELECT * FROM VBRK
               INTO TABLE LIT_VBRK
               WHERE VBELN = WA_ZTRIP_ST-INVOICENO.

      IF SY-SUBRC = 0.

        SELECT * FROM KNA1
                 INTO TABLE IT_KNA1
                 FOR ALL ENTRIES IN LIT_VBRK
                 WHERE KUNNR = LIT_VBRK-KUNRG.

      ENDIF.


      LOOP AT LIT_VBRK INTO LWA_VBRK.

        WA_F1-VBELN = LWA_VBRK-VBELN.
        WA_F1-KUNRG = LWA_VBRK-KUNRG.

        READ TABLE IT_KNA1 INTO WA_KNA1
        WITH KEY KUNNR = LWA_VBRK-KUNRG.

        IF SY-SUBRC = 0.

          WA_F1-KUNRG = WA_KNA1-KUNNR.
          WA_F1-NAME1 = WA_KNA1-NAME1.
          WA_F1-ORT01 = WA_KNA1-ORT01.

          APPEND WA_F1 TO IT_F1.

        ENDIF .
      ENDLOOP.

      WA_ZTRIP_ST-CUSTOMER = WA_F1-KUNRG.
      WA_ZTRIP_ST-NAME = WA_F1-NAME1.
      WA_ZTRIP_ST-CITY = WA_F1-ORT01.





      READ TABLE IT_ZTRIP_ST INTO LV_ZTRIP_ST12
      WITH KEY INVOICENO = WA_ZTRIP_ST-INVOICENO.
*
      IF SY-SUBRC <> 0.
        APPEND WA_ZTRIP_ST TO IT_ZTRIP_ST.
      ENDIF.

      CLEAR: WA_ZTRIP_ST, LV_QTY , LV_VOLUM ,LV_GROSS,LV_VBRP.



* APPEND WA_ZTRIP_ST TO IT_ZTRIP_ST.



  ENDCASE.
ENDMODULE.                 " CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  DIS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DIS OUTPUT.

  CASE OKAY_CODE.

    WHEN 'SUBMIT'.

      IF SY-SUBRC = 0.

        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
        ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
        ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
        ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.


        ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.
        ZTRIP_ST-CITY = WA_ZTRIP_ST-CITY.
        ZTRIP_ST-NAME = WA_ZTRIP_ST-NAME.


        ZTRIP_ST-TDATE = SY-DATUM.
        ZTRIP_ST-TIME = SY-UZEIT.


        LOOP AT IT_ZTRIP_ST INTO WA_ZTRIP_ST.
*
*        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
*        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
          ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
*        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
*        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
*        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
*        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
          ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
          ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.
*
*
          ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
*        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.
*
*
          ZTRIP_ST-TDATE = SY-DATUM.
          ZTRIP_ST-TIME = SY-UZEIT.



        ENDLOOP.



*
*
*      ELSE.
*
*        MESSAGE 'Record Not Found' TYPE 'W'.
      ENDIF.

    WHEN 'DD'.


      IF SY-SUBRC = 0.

        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
        ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
        ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
        ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.


        ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.

        ZTRIP_ST-CITY = WA_ZTRIP_ST-CITY.
        ZTRIP_ST-NAME = WA_ZTRIP_ST-NAME.


        ZTRIP_ST-TDATE = SY-DATUM.
        ZTRIP_ST-TIME = SY-UZEIT.


        LOOP AT IT_ZTRIP_ST INTO WA_ZTRIP_ST.
*
*        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
*        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
          ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
*        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
*        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
*        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
*        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
          ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
          ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.
*
*
          ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
*        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.
*
*
          ZTRIP_ST-TDATE = SY-DATUM.
          ZTRIP_ST-TIME = SY-UZEIT.



        ENDLOOP.



*
*
*      ELSE.
*
*        MESSAGE 'Record Not Found' TYPE 'W'.
      ENDIF.


*
*    WHEN 'DELETE'.
*
*
**      DELETE ZTRIP_ST FROM WA_ZTRIP_ST .
**      BREAK-POINT.
**
*      IF WA_ZTRIP_ST-INVOICENO = 1.
*        IF LI IS NOT INITIAL.
*          DELETE IT_ZTRIP_ST INDEX LI.
*          FREE LI.
*          FREE WA_ZTRIP_ST-INVOICENO.
*        ENDIF.
*      ENDIF.
*
*
*
*
  ENDCASE.
*
*


ENDMODULE.                 " DIS  OUTPUT
*&----------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHANGE_DATA INPUT.
  CASE OKAY_CODE.
    WHEN 'CHG'.
      WA_ZTRIP_ST-INVOICENO = ZTRIP_ST-INVOICENO  .
      WA_ZTRIP_ST-INVOICEDATE = ZTRIP_ST-INVOICEDATE  .
      WA_ZTRIP_ST-SKUDEALERNAME = ZTRIP_ST-SKUDEALERNAME  .
      WA_ZTRIP_ST-QUANTITY = ZTRIP_ST-QUANTITY  .
      " WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS = ZTRIP_ST-NOOFCARTONSPAILSDRUMS  .
      WA_ZTRIP_ST-NETVOLUMEPERINVOICE = ZTRIP_ST-NETVOLUMEPERINVOICE  .
      WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE = ZTRIP_ST-GROSSVOLUMEPERINVOICE  .
      WA_ZTRIP_ST-TRANSPORTERNAMELRNO = ZTRIP_ST-TRANSPORTERNAMELRNO  .
      WA_ZTRIP_ST-CARTONS = ZTRIP_ST-CARTONS  .
      WA_ZTRIP_ST-VEHICLENODRIVERNAME = ZTRIP_ST-VEHICLENODRIVERNAME  .


      WA_ZTRIP_ST-TDATE = SY-DATUM.
      WA_ZTRIP_ST-TIME = SY-UZEIT.


      WA_ZTRIP_ST-CITY = ZTRIP_ST-CITY.
      WA_ZTRIP_ST-NAME = ZTRIP_ST-NAME.



      UPDATE    ZTRIP_ST FROM WA_ZTRIP_ST.

      IF SY-SUBRC = 0.
        MESSAGE 'Record Changes Sucessfully' TYPE 'S'.

      ELSE.
        MESSAGE 'Enter A Valid Data' TYPE 'E'.
      ENDIF.





      IF SY-SUBRC = 0.

        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
        ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
        ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
        ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.


        ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.

        ZTRIP_ST-CITY = WA_ZTRIP_ST-CITY.
        ZTRIP_ST-NAME = WA_ZTRIP_ST-NAME.

        ZTRIP_ST-TDATE = SY-DATUM.
        ZTRIP_ST-TIME = SY-UZEIT.


        LOOP AT IT_ZTRIP_ST INTO WA_ZTRIP_ST.
*
*        ZTRIP_ST-INVOICENO = WA_ZTRIP_ST-INVOICENO.
*        ZTRIP_ST-INVOICEDATE = WA_ZTRIP_ST-INVOICEDATE.
          ZTRIP_ST-SKUDEALERNAME = WA_ZTRIP_ST-SKUDEALERNAME.
*        ZTRIP_ST-QUANTITY = WA_ZTRIP_ST-QUANTITY.
*        " ZTRIP_ST-NOOFCARTONSPAILSDRUMS = WA_ZTRIP_ST-NOOFCARTONSPAILSDRUMS.
*        ZTRIP_ST-NETVOLUMEPERINVOICE = WA_ZTRIP_ST-NETVOLUMEPERINVOICE.
*        ZTRIP_ST-GROSSVOLUMEPERINVOICE = WA_ZTRIP_ST-GROSSVOLUMEPERINVOICE.
          ZTRIP_ST-TRANSPORTERNAMELRNO = WA_ZTRIP_ST-TRANSPORTERNAMELRNO.
          ZTRIP_ST-VEHICLENODRIVERNAME = WA_ZTRIP_ST-VEHICLENODRIVERNAME.
*
*
          ZTRIP_ST-UNIQ1 = WA_ZTRIP_ST-UNIQ1.
*        ZTRIP_ST-CARTONS = WA_ZTRIP_ST-CARTONS.
*
*
          ZTRIP_ST-TDATE = SY-DATUM.
          ZTRIP_ST-TIME = SY-UZEIT.



        ENDLOOP.



*
*
*      ELSE.
*
*        MESSAGE 'Record Not Found' TYPE 'W'.
      ENDIF.







  ENDCASE.

ENDMODULE.                 " CHANGE_DATA  INPUT
