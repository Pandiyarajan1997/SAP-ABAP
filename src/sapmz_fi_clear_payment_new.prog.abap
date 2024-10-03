*&---------------------------------------------------------------------*
*& Module Pool       SAPMZ_FI_INCOMING_PAYMENT_NEW
*&
*&---------------------------------------------------------------------*
* Program ID         :
* Date               : 7th SEP .2020
* Author             : Ramachandran.M
* Title              : Automated AR Collection Booking Process
* Tcode              : ZINCPAYN
* Function           : To automate incoming payments - Modified version of ZINCPAY
*&---------------------------------------------------------------------*
PROGRAM SAPMZ_FI_CLEAR_PAYMENT_NEW.
INCLUDE MZ_FI_CLEAR_PAYMENT_NEWTOP.
*INCLUDE MZ_FI_INCOMING_PAYMENT_NEWTOP." global Data
INCLUDE MZ_FI_CLEAR_PAYMENT_NEWO01.
*INCLUDE MZ_FI_INCOMING_PAYMENT_NEWO01. " PBO-Modules
INCLUDE MZ_FI_CLEAR_PAYMENT_NEWI01.
*INCLUDE MZ_FI_INCOMING_PAYMENT_NEWI01. " PAI-Modules
INCLUDE MZ_FI_CLEAR_PAYMENT_NEWF01.
*INCLUDE MZ_FI_INCOMING_PAYMENT_NEWF01. " FORM-Routines
INCLUDE mz_fi_clear_payment_new_f29f01.
*&---------------------------------------------------------------------*
*&      Module  IN_SH_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SH_SGL_9000 INPUT.

  PERFORM SEARCH_HELP_SPLGL_9000.

ENDMODULE.                 " IN_SH_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_SPLGL_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_HELP_SPLGL_9000 .

   SELECT KOART SHBKZ LTEXT FROM T074T INTO TABLE IT_T074T WHERE
              SPRAS EQ 'EN' AND KOART EQ 'D' .

  DATA: RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE.

   DATA: T_RETURN  TYPE STANDARD TABLE OF DDSHRETVAL.
  DATA: T_MAPPING TYPE STANDARD TABLE OF DSELC.

  DATA: S_RETURN  TYPE DDSHRETVAL.
  DATA: S_MAPPING TYPE DSELC.

  S_MAPPING-FLDNAME     = 'F0001'.
  S_MAPPING-DYFLDNAME   = 'IP_LTRCODE'.
  APPEND S_MAPPING TO T_MAPPING.
  CLEAR S_MAPPING.

  S_MAPPING-FLDNAME     = 'F0002'.
  S_MAPPING-DYFLDNAME   = 'GS_HEADER-SPLGL'.
  APPEND S_MAPPING TO T_MAPPING.
  CLEAR S_MAPPING.

  S_MAPPING-FLDNAME     = 'F0003'.
  S_MAPPING-DYFLDNAME   = 'IP_LTRANS'.
  APPEND S_MAPPING TO T_MAPPING.
  CLEAR S_MAPPING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'NAME1_GP'
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'GS_HEADER-SPLGL'
                                                              " window_title    = ‘Selection of Material with Description’
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_T074T
      RETURN_TAB      = T_RETURN
      DYNPFLD_MAPPING = T_MAPPING
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  IF SY-SUBRC <> 0.
** Implement suitable error handling here
  ENDIF.


ENDFORM.                    " SEARCH_HELP_SPLGL_9000
