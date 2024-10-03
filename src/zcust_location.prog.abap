*&---------------------------------------------------------------------*
*& Module Pool       ZCUST_LOCATION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE ZCUSTTOP                                .    " global Data

* INCLUDE ZCUSTO01                                .  " PBO-Modules
* INCLUDE ZCUSTI01                                .  " PAI-Modules
* INCLUDE ZCUSTF01                                .  " FORM-Routines

*&---------------------------------------------------------------------*
*&      Module  STATUS_1111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1111 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

IF SY-TCODE = 'XD03'.
  LOOP AT SCREEN.
   IF SCREEN-GROUP1 = 'SL' .
      SCREEN-INPUT = '0' .
      MODIFY SCREEN.
      ENDIF.
  ENDLOOP.
ENDIF.
ENDMODULE.                 " STATUS_1111  OUTPUT
