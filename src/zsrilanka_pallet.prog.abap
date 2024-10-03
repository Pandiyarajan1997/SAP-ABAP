*&---------------------------------------------------------------------*
*& Module Pool       ZSRILANKA_PALLET
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
INCLUDE ZLANKA_PALLETTOP                        .    " global Data
* INCLUDE ZLANKA_PALLETO01                        .  " PBO-Modules
* INCLUDE ZLANKA_PALLETI01                        .  " PAI-Modules
* INCLUDE ZLANKA_PALLETF01                        .  " FORM-Routines
*&---------------------------------------------------------------------*
*&      Module  STATUS_1111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1111 OUTPUT.
  SET PF-STATUS 'GU1111'.
  SET TITLEBAR '1111'.
*break-point.
*data : i type i.
  TYPE-POOLS VRM.
  DATA VALUES TYPE VRM_VALUES WITH HEADER LINE.
IF SY-UCOMM EQ 'CREATE' .
IF IT_fINAL IS NOT INITIAL.
  loop at it_final into wa_final.
 IF WA_FINAL-VBELN NE LV_INV .
 MESSAGE 'One invoice can be addded' TYPE 'I'.
 ENDIF.
 endloop.
ENDIF.
ENDIF.
ENDMODULE.                    "STATUS_1111 OUTPUT
*  SELECT *
*        FROM ZPACKLIST
*    INTO TABLE IT_ZPACK.
*  DATA : I TYPE I.
*  LOOP AT IT_ZPACK INTO WA_ZPACK.
*    " wa_listbox-key = i + 1 .
*    WA_LISTBOX-TEXT = WA_ZPACK-SELECT_TYPE.
*    APPEND WA_LISTBOX TO IT_LISTBOX.
*  ENDLOOP.
*  CLEAR WA_LISTBOX.
*
**  wa_listbox = 'SELECT' .
**  append wa_listbox to it_listbox.
*
*  LD_FIELD = 'wa_final-sel_type'.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      ID     = LD_FIELD
*      VALUES = IT_LISTBOX.
"ENDMODULE.                 " STATUS_1111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1111 INPUT.
  USER = SY-UNAME .
  USER_DATE = SY-DATUM .
  USER_TIME = SY-UZEIT .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = INV_NO
    IMPORTING
      OUTPUT = LV_INV.
  IF SY-UCOMM EQ 'CREATE' .
    IF IT_FINAL IS INITIAL.
      SELECT VBELN UP TO 1 ROWS FROM ZPACK_PALLET INTO CUS_VBELN  WHERE VBELN = LV_INV  ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF CUS_VBELN IS INITIAL.
      SELECT VBELN POSNR MATNR FKIMG FROM VBRP INTO TABLE IT_VBRP WHERE VBELN = LV_INV .
      SELECT MATNR MAKTX FROM MAKT INTO TABLE IT_MAKT FOR ALL ENTRIES IN IT_VBRP
                     WHERE MATNR = IT_VBRP-MATNR.
      LOOP AT IT_VBRP INTO WA_VBRP.
        WA_FINAL-VBELN = WA_VBRP-VBELN.
        WA_FINAL-POSNR = WA_VBRP-POSNR.
        WA_FINAL-MATNR = WA_VBRP-MATNR.
        WA_FINAL-FKIMG = WA_VBRP-FKIMG.
*        WA_FINAL-SEL_TYPE = 'SELECT'.
     READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_VBRP-MATNR.
        IF SY-SUBRC EQ 0.
          WA_FINAL-MAKTX = WA_MAKT-MAKTX.
        ENDIF.
        APPEND WA_FINAL TO IT_FINAL.
        CLEAR WA_FINAL.
      ENDLOOP.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'G1'.
          SCREEN-INPUT = '1'.
          SCREEN-INVISIBLE = '0'.
        ENDIF.
      ENDLOOP.
    ENDIF.
IF CUS_VBELN IS NOT INITIAL.
   CLEAR : CUS_VBELN.
  MESSAGE 'Already Created.Kindly use Change' TYPE 'I'.
ENDIF.


ENDIF.
*BREAK-POINT.
  ELSEIF SY-UCOMM EQ 'CHANGE' .
    SELECT MANDT VBELN POSNR SL_NO MATNR MAKTX FKIMG SEL_TYPE PAL_NUMBER PACK_QTY
      GROSS_WEI NET_WEI CONTAIN_NO CREDATED_BY CREDATED_DATE CREDATED_TIME INTO
      TABLE IT_FINAL
       FROM ZPACK_PALLET WHERE VBELN = LV_INV .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT  = INV_NO
*    IMPORTING
*      OUTPUT = LOCAL_INV.

SORT : IT_FINAL BY SL_NO.

DATA : STA TYPE CHAR1 .
STA = 'X' .
      LOCAL_INV = LV_INV.

    SHIFT LOCAL_INV LEFT DELETING LEADING '0' .

   SELECT MANDT VBELN PORT PLACE FREIGHT INSURANCE INTO WA_HEADER
       FROM ZPALLET_HEADERS WHERE VBELN = LOCAL_INV .
     ENDSELECT.
     PORT_NO = WA_HEADER-PORT .
     PLACE = WA_HEADER-PLACE.
     FREIGHT = WA_HEADER-FREIGHT.
     INSU = WA_HEADER-INSURANCE.
  ELSEIF SY-UCOMM EQ 'BACK' .
    LEAVE PROGRAM.
*    SELECT
*      MATNR
*      MAKTX FROM MARA INTO TABLE IT_MARA WHERE MATNR = WA_FINAL-MATNR.
"  ELSEIF SY-UCOMM EQ 'ENTER' .
*    WA_FINAL-POSNR = WA_VBRP-POSNR .
*    WA_FINAL-MATNR = WA_VBRP-MATNR.
*    WA_FINAL-VBELN = WA_VBRP-VBELN.
*    WA_FINAL-FKIMG = WA_VBRP-FKIMG.
  ELSEIF SY-UCOMM EQ 'SAVE' .
    LV_COUNT = 0 .
       WA_HEADER-PORT = PORT_NO .
       WA_HEADER-PLACE = PLACE .
       WA_HEADER-FREIGHT = FREIGHT .
       WA_HEADER-INSURANCE = INSU .
       WA_HEADER-VBELN = INV_NO .

       "INSERT ZPALLET_HEADERS FROM WA_HEADER.



    IF IT_FINAL IS NOT INITIAL.
      LOOP AT IT_FINAL INTO WA_FINAL .
        WA_FINAL-MANDT = SY-MANDT .
        WA_FINAL-VBELN =  WA_FINAL-VBELN.
        WA_FINAL-POSNR = WA_FINAL-POSNR .
        WA_FINAL-SL_NO = LV_COUNT + 1.
        WA_FINAL-MATNR = WA_FINAL-MATNR.
        WA_FINAL-MAKTX = WA_FINAL-MAKTX.
        WA_FINAL-FKIMG = WA_FINAL-FKIMG.
        " WA_FINAL-MAKTX = WA_FINAL-MAKTX.
        WA_FINAL-SEL_TYPE = WA_FINAL-SEL_TYPE . "'Pallet'. " .
        WA_FINAL-PAL_NUMBER = WA_FINAL-PAL_NUMBER.
        WA_FINAL-PACK_QTY = WA_FINAL-PACK_QTY .
        WA_FINAL-NET_WEI = WA_FINAL-NET_WEI .
        WA_FINAL-GROSS_WEI = WA_FINAL-GROSS_WEI .
        WA_FINAL-CONTAIN_NO = WA_FINAL-CONTAIN_NO.
        WA_FINAL-CREDATED_BY = USER.
        WA_FINAL-CREDATED_DATE = USER_DATE.
        WA_FINAL-CREDATED_TIME = USER_TIME.
        "modify zPACK_PALLET from wa_final.
        "update ZPACK_PALLET from WA_FINAL.
        " APPEND WA_FINAL1 TO IT_FINAL1 .
        LV_COUNT = WA_FINAL-SL_NO.
        MODIFY IT_FINAL FROM WA_FINAL . "transporting .
        CLEAR : WA_FINAL.
      ENDLOOP.
      PERFORM FILL_VALIDATION.
      MODIFY ZPALLET_HEADERS FROM WA_HEADER . "it_final.
      MODIFY ZPACK_PALLET FROM TABLE it_final.
      "UPDATE ZPACK_PALLET FROM TABLE IT_FINAL .
      IF SY-SUBRC = 0.
        MESSAGE 'Record Created Sucessfully' TYPE 'I'.
       PERFORM REFRESH_1111 .
      ELSE.
        MESSAGE 'Record Not Created' TYPE 'E'.
      ENDIF.
    ENDIF.
    IF IT_FINAL IS INITIAL.
      MESSAGE 'No data available' TYPE 'I'.
    ENDIF.
  ENDIF.



  " ENDCASE.
ENDMODULE.                 " USER_COMMAND_1111  INPUT
*&---------------------------------------------------------------------*
*&      Module  SELECT_DROP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_DROP INPUT.

  LOOP AT IT_FINAL INTO WA_FINAL.
    WA_FINAL1-MANDT = WA_FINAL-MANDT .
    WA_FINAL1-POSNR = WA_FINAL-POSNR.
    WA_FINAL1-SL_NO = WA_FINAL-SL_NO.
    WA_FINAL1-MATNR = WA_FINAL-MATNR.
    WA_FINAL1-MAKTX = WA_FINAL-MAKTX.
    WA_FINAL1-FKIMG = WA_FINAL-FKIMG.
    WA_FINAL1-PACK_QTY = WA_FINAL-PACK_QTY .
    WA_FINAL1-NET_WEI = WA_FINAL-NET_WEI.
    WA_FINAL1-GROSS_WEI = WA_FINAL-GROSS_WEI.
    WA_FINAL1-CONTAIN_NO = WA_FINAL-CONTAIN_NO.
    WA_FINAL1-CREDATED_BY = WA_FINAL-CREDATED_BY.
    WA_FINAL1-CREDATED_DATE = WA_FINAL1-CREDATED_DATE .
    APPEND WA_FINAL1 TO IT_FINAL1.
    CLEAR WA_FINAL1.
  ENDLOOP.

  LOOP AT IT_FINAL INTO WA_FINAL.
     WA_FINAL1-MANDT = WA_FINAL-MANDT.
     WA_FINAL1-POSNR = WA_FINAL-POSNR.
     WA_FINAL1-MATNR = WA_FINAL-MATNR.
     WA_FINAL1-MAKTX = WA_FINAL-MAKTX.
     WA_FINAL1-FKIMG = WA_FINAL-FKIMG.
   ENDLOOP.

ENDMODULE.                 " SELECT_DROP  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SAVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SAVE INPUT.

  IF SY-UCOMM = 'SELL' .

    WA_VALUES-FIELDNAME = 'WA_FINAL-SEL_TYPE'.
    APPEND WA_VALUES TO GT_VALUES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME             = SY-CPROG
        DYNUMB             = SY-DYNNR
        TRANSLATE_TO_UPPER = 'X'
      TABLES
        DYNPFIELDS         = GT_VALUES.

    READ TABLE GT_VALUES INDEX 1 INTO WA_VALUES.
*
* DELETE VALUES WHERE key <> WA_VALUES-fieldvalue.
    READ TABLE VALUES WITH  KEY KEY = WA_VALUES-FIELDVALUE.

    WA_FINAL-SEL_TYPE = WA_VALUES-FIELDVALUE.
*    BREAK-POINT.
  ENDIF.

  MODIFY IT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE.

  IF SY-SUBRC <> 0.
    APPEND WA_FINAL TO IT_FINAL.
  ENDIF.


ENDMODULE.                 " USER_COMMAND_SAVE  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_9000 OUTPUT.

  IF SY-UCOMM = 'CREATE' .
    IF TAB-CURRENT_LINE LE LV_COUNT.
      IF WA_FINAL-MATNR IS NOT INITIAL.
        LOOP AT SCREEN.
          IF SCREEN-NAME = 'WA_FINAL-MATNR' .
            SCREEN-INPUT = '0'.
            MODIFY SCREEN.
          ELSEIF SCREEN-NAME = 'WA_FINAL-MAKTX'.
            SCREEN-INPUT = '0'.
            MODIFY SCREEN.
          ELSEIF SCREEN-NAME = 'WA_FINAL-FKIMG'.
            SCREEN-INPUT = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

*  CLEAR OK_CODE.
*  OK_CODE = SY-UCOMM.
*break-point.
*    CASE OK_CODE.
*
*      WHEN 'CREATE' .
*
*               IF SCREEN-GROUP1 = 'G1'.
*                 "IF SCREEN-
*            SCREEN-INPUT = '1'.
*            SCREEN-INVISIBLE = '1'.
*                 ENDIF.
*        ENDLOOP.
*
*      WHEN 'CHANGE' .
*
*     ENDCASE.

ENDMODULE.                 " DISPLAY_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LINE_COUNT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LINE_COUNT OUTPUT.

  DESCRIBE TABLE it_final LINES LV_COUNT . " to TAB LINES LV_COUNT.

  TAB-LINES = LV_COUNT + 18.

ENDMODULE.                 " LINE_COUNT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  VRM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VRM INPUT.

  LOOP AT IT_FINAL INTO WA_FINAL .
    IF WA_FINAL-MAKTX IS INITIAL
              AND WA_FINAL-POSNR IS NOT INITIAL
              AND WA_FINAL-MATNR IS NOT INITIAL.
      "SELECT SINGLE MAKTX INTO LV_MAKTX FROM ZPACK_PALLET WHERE MATNR = WA_FINAL-MATNR.
      SELECT SINGLE ARKTX INTO LV_MAKTX FROM vbrp WHERE VBELN = LV_INV AND MATNR = WA_FINAL-MATNR AND POSNR = WA_FINAL-POSNR .
      SELECT SINGLE FKIMG INTO LV_FKIMG FROM vbrp WHERE VBELN = LV_INV AND MATNR = WA_FINAL-MATNR AND POSNR = WA_FINAL-POSNR .
      WA_FINAL-VBELN = LV_INV.
      WA_FINAL-MAKTX = LV_MAKTX.
      WA_FINAL-FKIMG = LV_FKIMG.
      MODIFY IT_FINAL FROM WA_FINAL TRANSPORTING VBELN MAKTX FKIMG.
    ENDIF.
  ENDLOOP.

if it_final is initial.

  SELECT *
        FROM ZPACKLIST
    INTO TABLE IT_ZPACK.

   " SORT IT_ZPACK DESCENDING BY SELECT_TYPE .

  DATA : In TYPE I.
  LOOP AT IT_ZPACK INTO WA_ZPACK.
    wa_listbox-key = in + 1 .
    WA_LISTBOX-key = WA_ZPACK-SELECT_TYPE.
    APPEND WA_LISTBOX TO IT_LISTBOX.
  ENDLOOP.
  CLEAR WA_LISTBOX.

   CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
         id  = 'WA_FINAL-SEL_TYPE'
          VALUES = IT_LISTBOX.
   endif.

  IF SY-UCOMM NE 'SELL'.
    SELECT *
       FROM ZPACKLIST
   INTO TABLE IT_ZPACK.

    LOOP AT IT_ZPACK INTO WA_ZPACK.
      VALUES-KEY = WA_ZPACK-SELECT_TYPE.        "WA_ZPACK-ID.
*    VALUES-TEXT = WA_ZPACK-SELECT_TYPE.
      APPEND VALUES.
    ENDLOOP.
    SORT VALUES DESCENDING BY KEY.
    DELETE ADJACENT DUPLICATES FROM VALUES[] COMPARING KEY.
  ENDIF.
  "  VALUES-TEXT = ZPACKLIST-SELECT_TYPE .
  if sy-ucomm eq 'SELL'.
      SORT VALUES DESCENDING BY KEY.
   DELETE ADJACENT DUPLICATES FROM VALUES[] COMPARING KEY.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'WA_FINAL-SEL_TYPE'
      VALUES          = VALUES[]
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ENDIF.
ENDMODULE.                 " VRM  INPUT
*&---------------------------------------------------------------------*
*&      Module  IN_SH_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IN_SH_MATNR INPUT.

  PERFORM SEARCH_HELP_MATNR_1111.

ENDMODULE.                 " IN_SH_MATNR  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_MATNR_1111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_HELP_MATNR_1111 .

  RANGES: MATNR FOR WA_FINAL-MATNR.

  TYPES : BEGIN OF GS_MATNR ,
           MATNR TYPE VBRP-MATNR,
         END OF GS_MATNR .
  DATA : GT_MATNR TYPE TABLE OF GS_MATNR,
         WA_MATNR TYPE GS_MATNR.

  SELECT MATNR
         FROM VBRP
         INTO TABLE GT_MATNR WHERE VBELN = LV_INV.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
    RETFIELD               = 'VBELN_VF'
*   PVALKEY                = ' '
*   DYNPPROG               = ' '
*   DYNPNR                 = ' '
    DYNPROFIELD            = 'WA_FINAL-MATNR'
*   STEPL                  = 0
    WINDOW_TITLE           = 'Material Number'
*   VALUE                  = ' '
    VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
*   CALLBACK_METHOD        =
*   MARK_TAB               =
* IMPORTING
*   USER_RESET             =
    TABLES
    VALUE_TAB              = GT_MATNR
*   FIELD_TAB              =
       RETURN_TAB             = IT_RETURN
*   DYNPFLD_MAPPING        =
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE IT_RETURN INTO WA_RETURN INDEX 1." WITH KEY FIELDVAL.

    IF SY-SUBRC = 0.

    GS_DYFIELDS-FIELDNAME  = 'WA_FINAL-MATNR'.

    GS_DYFIELDS-FIELDVALUE = WA_RETURN-FIELDVAL .  "wa_ekpo-ebelp.
    APPEND GS_DYFIELDS TO IT_DYFIELDS.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME                     = SY-REPID
        DYNUMB                     = '1111'
      TABLES
        DYNPFIELDS                 = IT_DYFIELDS
* EXCEPTIONS
*   INVALID_ABAPWORKAREA       = 1
*   INVALID_DYNPROFIELD        = 2
*   INVALID_DYNPRONAME         = 3
*   INVALID_DYNPRONUMMER       = 4
*   INVALID_REQUEST            = 5
*   NO_FIELDDESCRIPTION        = 6
*   UNDEFIND_ERROR             = 7
*   OTHERS                     = 8
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " SEARCH_HELP_MATNR_1111
*&---------------------------------------------------------------------*
*&      Module  IN_SH_POSNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IN_SH_POSNR INPUT.
   PERFORM SEARCH_HELP_POSNR_1111.
ENDMODULE.                 " IN_SH_POSNR  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_POSNR_1111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_HELP_POSNR_1111 .

  RANGES: POSNR FOR WA_FINAL-POSNR.

  TYPES : BEGIN OF GS_POSNR ,
           POSNR TYPE VBRP-POSNR,
         END OF GS_POSNR .
  DATA : GT_POSNR TYPE TABLE OF GS_POSNR,
         WA_POSNR TYPE GS_POSNR.

  SELECT POSNR
         FROM VBRP
         INTO TABLE GT_POSNR WHERE VBELN = LV_INV.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
    RETFIELD               = 'VBELN_VF'
*   PVALKEY                = ' '
*   DYNPPROG               = ' '
*   DYNPNR                 = ' '
    DYNPROFIELD            = 'WA_FINAL-POSNR'
*   STEPL                  = 0
    WINDOW_TITLE           = 'Line Item'
*   VALUE                  = ' '
    VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
*   CALLBACK_METHOD        =
*   MARK_TAB               =
* IMPORTING
*   USER_RESET             =
    TABLES
    VALUE_TAB              = GT_POSNR
*   FIELD_TAB              =
       RETURN_TAB             = IT_RETURN
*   DYNPFLD_MAPPING        =
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE IT_RETURN INTO WA_RETURN INDEX 1." WITH KEY FIELDVAL.

    IF SY-SUBRC = 0.

    GS_DYFIELDS-FIELDNAME  = 'WA_FINAL-POSNR'.

    GS_DYFIELDS-FIELDVALUE = WA_RETURN-FIELDVAL .  "wa_ekpo-ebelp.
    APPEND GS_DYFIELDS TO IT_DYFIELDS.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME                     = SY-REPID
        DYNUMB                     = '1111'
      TABLES
        DYNPFIELDS                 = IT_DYFIELDS
* EXCEPTIONS
*   INVALID_ABAPWORKAREA       = 1
*   INVALID_DYNPROFIELD        = 2
*   INVALID_DYNPRONAME         = 3
*   INVALID_DYNPRONUMMER       = 4
*   INVALID_REQUEST            = 5
*   NO_FIELDDESCRIPTION        = 6
*   UNDEFIND_ERROR             = 7
*   OTHERS                     = 8
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.
ENDFORM.                    " SEARCH_HELP_POSNR_1111

*&---------------------------------------------------------------------*
*&      Module  IN_SH_SELTYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IN_SH_SELTYPE INPUT.

  SELECT *
        FROM ZPACKLIST
    INTO TABLE IT_ZPACK.
  DATA : IT TYPE I.
  LOOP AT IT_ZPACK INTO WA_ZPACK.
    WA_LISTBOX-KEY = IT + 1 ."wa_ZPACK-matnr.
    WA_LISTBOX-TEXT = WA_ZPACK-SELECT_TYPE.
    APPEND WA_LISTBOX TO IT_LISTBOX.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD  = 'wa_listbox-text'
      VALUE_ORG = 'S'
    TABLES
      VALUE_TAB = IT_LISTBOX.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*    CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*         id  = 'WA_FINAL-SEL_TYPE'
*          VALUES = IT_LISTBOX.

ENDMODULE.                 " IN_SH_SELTYPE  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_VALIDATION .


LOOP AT IT_FINAL INTO WA_FINAL.
  IF WA_FINAL-POSNR IS INITIAL.
    MESSAGE 'Please Give Line Item' TYPE 'E' .
  ELSEIF WA_FINAL-MATNR IS INITIAL.
    MESSAGE 'Please Give Material Number' TYPE 'E' .
  ELSEIF WA_FINAL-MAKTX IS INITIAL.
    MESSAGE 'Please Give Material Desc' TYPE 'E' .
  ELSEIF WA_FINAL-FKIMG IS INITIAL OR WA_FINAL-FKIMG EQ '0.00'.
    MESSAGE 'Please Give Billed Qty' TYPE 'E' .
  ELSEIF WA_FINAL-SEL_TYPE IS INITIAL OR WA_FINAL-SEL_TYPE EQ 'SELECT'.
    MESSAGE 'Please Select Valid Type' TYPE 'E' .
  ELSEIF WA_FINAL-PAL_NUMBER IS INITIAL OR WA_FINAL-PAL_NUMBER EQ '0' .
    MESSAGE 'Please Give Valid Pallet Number ' TYPE 'E' .
  ELSEIF WA_FINAL-PACK_QTY IS INITIAL OR WA_FINAL-PACK_QTY EQ '0.00'.
    MESSAGE 'Please Give Packet Qty' TYPE 'E' .
*  ELSEIF WA_FINAL-CONTAIN_NO IS INITIAL .
*    MESSAGE 'Please Give Packet Qty' TYPE 'E' .
  ENDIF.
ENDLOOP.

ENDFORM.                    " FILL_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  REFRESH_1111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_1111 .

  CLEAR : LV_INV ,
          INV_NO,
          CUS_VBELN,
          WA_FINAL,
          PORT_NO,
          PLACE,
          FREIGHT,
          INSU,
          WA_HEADER.
  REFRESH : IT_FINAL,
            VALUES ,
            IT_ZPACK,
            IT_LISTBOX.

  CALL SCREEN 1111.

ENDFORM.                    " REFRESH_1111
*&---------------------------------------------------------------------*
*&      Module  IN_SH_VBELN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IN_SH_VBELN INPUT.

  PERFORM SEARCH_HELP_VBELN_1111.

ENDMODULE.                 " IN_SH_VBELN  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_VBELN_1111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_HELP_VBELN_1111 .

  "RANGES: VBELN FOR WA_FINAL-POSNR.

  TYPES : BEGIN OF GS_VBELN ,
           FKDAT TYPE VBRK-FKDAT,
           VBELN TYPE VBRK-VBELN,
           END OF GS_VBELN .

  DATA : GT_VBELN TYPE TABLE OF GS_VBELN,
         WA_VBELN TYPE GS_VBELN.

  SELECT FKDAT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
         VBELN
         FROM VBRK
         INTO TABLE GT_VBELN WHERE FKART EQ 'YBEO'  . " WHERE VBELN = INV_NO.

    SORT : GT_VBELN DESCENDING BY FKDAT.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
    RETFIELD               = 'VBELN_VF'
*   PVALKEY                = ' '
*   DYNPPROG               = ' '
*   DYNPNR                 = ' '
    DYNPROFIELD            = 'INV_NO'
*   STEPL                  = 0
    WINDOW_TITLE           = 'Line Item'
*   VALUE                  = ' '
    VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
*   CALLBACK_METHOD        =
*   MARK_TAB               =
* IMPORTING
*   USER_RESET             =
    TABLES
    VALUE_TAB              = GT_VBELN
*   FIELD_TAB              =
       RETURN_TAB             = IT_RETURN
*   DYNPFLD_MAPPING        =
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE IT_RETURN INTO WA_RETURN INDEX 1." WITH KEY FIELDVAL.

    IF SY-SUBRC = 0.

    GS_DYFIELDS-FIELDNAME  = 'INV_NO'.

    GS_DYFIELDS-FIELDVALUE = WA_RETURN-FIELDVAL .  "wa_ekpo-ebelp.
    APPEND GS_DYFIELDS TO IT_DYFIELDS.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME                     = SY-REPID
        DYNUMB                     = '1111'
      TABLES
        DYNPFIELDS                 = IT_DYFIELDS
* EXCEPTIONS
*   INVALID_ABAPWORKAREA       = 1
*   INVALID_DYNPROFIELD        = 2
*   INVALID_DYNPRONAME         = 3
*   INVALID_DYNPRONUMMER       = 4
*   INVALID_REQUEST            = 5
*   NO_FIELDDESCRIPTION        = 6
*   UNDEFIND_ERROR             = 7
*   OTHERS                     = 8
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " SEARCH_HELP_VBELN_1111
*&---------------------------------------------------------------------*
*&      Module  TEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TEST INPUT.

ENDMODULE.                 " TEST  INPUT
*&---------------------------------------------------------------------*
*&      Module  TAB_CONT_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_CONT_MODIFY INPUT.

  MODIFY iT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE.

ENDMODULE.                 " TAB_CONT_MODIFY  INPUT
