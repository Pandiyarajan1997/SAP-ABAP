*----------------------------------------------------------------------*
***INCLUDE ZHSBC_PAYMENT_ALV.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0121  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0121 OUTPUT.
  SET PF-STATUS 'HSBC_121'.
  SET TITLEBAR 'HSBC_121'.

ENDMODULE.                 " STATUS_0121  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0121  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0121 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'DECLINE'.
      CALL SCREEN BACK_SCREEN.
    WHEN 'PROCEED'.
      PERFORM XML_DATA_CONVERS.
      PERFORM XML_FILE_GENERATION.

  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0121  INPUT
*&---------------------------------------------------------------------*
*&      Module  SELECTED_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECTED_DATA OUTPUT.

  CLEAR :XML_TOTAL, IMPS_TOTAL, XML_NO , IMPS_NO.

*  IF BACK_SCREEN = '103'.
*
*    data : lv_mail TYPE string.
*
*
*
*
*
*    LOOP AT IT_SELECT INTO WA_SELECT WHERE CHK = 'X' AND EMAIL IS INITIAL.
*      clear lv_mail.
*      CONCATENATE 'Kindly Maintain Email ID for' wa_select-HBKID ' House bank in ZHSBC( House Bank )' INTO lv_mail SEPARATED BY ' '.
*      CONDENSE lv_mail.
*      MESSAGE lv_mail TYPE 'W'.
*      call SCREEN back_screen.
*    ENDLOOP.
*
*  ENDIF.

DELETE ADJACENT DUPLICATES FROM it_select COMPARING ALL FIELDS.

DELETE it_select WHERE type is INITIAL or PSWBT is INITIAL or belnr is INITIAL .

  LOOP AT IT_SELECT INTO WA_SELECT WHERE CHK = 'X'.


    MOVE-CORRESPONDING WA_SELECT TO WA_FINAL.

    IF WA_SELECT-PPC IS NOT INITIAL.

      WA_FINAL-PPC = WA_SELECT-PPC(4).

    ENDIF.

    IF WA_SELECT-TYPE = 'NEFT' OR WA_SELECT-TYPE = 'RTGS'.

      WA_FINAL-TYPE_REF = '1'.

      XML_TOTAL = XML_TOTAL + WA_FINAL-PSWBT.
      XML_NO = XML_NO + 1.

    ELSEIF WA_SELECT-TYPE = 'IMPS'.

      WA_FINAL-TYPE_REF = '2'.

      IMPS_TOTAL = IMPS_TOTAL + WA_FINAL-PSWBT.
      IMPS_NO = IMPS_NO + 1.

    ENDIF.

    APPEND WA_FINAL TO IT_FINAL.

    CLEAR : WA_FINAL, WA_SELECT.



  ENDLOOP.


  IF IT_FINAL IS INITIAL.

    MESSAGE 'Select any one of the records' TYPE 'S'.

    CALL SCREEN '120'.

  ENDIF.

IF  xml_total = '0'.

message 'Select the Valid File.....' TYPE 'W'.

call screen '120'.


ENDIF.

delete ADJACENT DUPLICATES FROM it_final COMPARING ALL FIELDS.

ENDMODULE.                 " SELECTED_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV OUTPUT.

  CREATE OBJECT ALV_CONTAINER
  EXPORTING
*    PARENT                      =
    CONTAINER_NAME               = 'CC_ALV'
*    STYLE                       =
*    LIFETIME                    = lifetime_default
*    REPID                       =
*    DYNNR                       =
*    NO_AUTODEF_PROGID_DYNNR     =
*  EXCEPTIONS
*    CNTL_ERROR                  = 1
*    CNTL_SYSTEM_ERROR           = 2
*    CREATE_ERROR                = 3
*    LIFETIME_ERROR              = 4
*    LIFETIME_DYNPRO_DYNPRO_LINK = 5
*    OTHERS                      = 6
    .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CREATE OBJECT ALV_GRID
    EXPORTING
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
       I_PARENT          = ALV_CONTAINER
*    I_APPL_EVENTS     = space
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
*  EXCEPTIONS
*    ERROR_CNTL_CREATE = 1
*    ERROR_CNTL_INIT   = 2
*    ERROR_CNTL_LINK   = 3
*    ERROR_DP_CREATE   = 4
*    OTHERS            = 5
      .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM FCAT.

  SORT IT_FINAL BY TYPE_REF BELNR.

  PERFORM DISPLAY.


ENDMODULE.                 " ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*    I_BUFFER_ACTIVE               =
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
*    I_STRUCTURE_NAME              =
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
       IS_LAYOUT                     = WA_LAYOUT
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
*    IR_SALV_ADAPTER               =
    CHANGING
       IT_OUTTAB                     = IT_FINAL
       IT_FIELDCATALOG               = IT_FCAT
       IT_SORT                       = IT_SORT
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " DISPLAY
