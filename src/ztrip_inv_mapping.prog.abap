*&---------------------------------------------------------------------*
*& Module Pool       ZTRIP_INV_MAPPING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM ZTRIP_INV_MAPPING.

TYPES : BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TY_LFA1.

TYPES : BEGIN OF GS_HEADER,
       MANDT TYPE ZFREIGHT_HEADER-MANDT,
       TRIP_NO TYPE ZFREIGHT_HEADER-TRIP_NO,
       STATUS TYPE ZFREIGHT_HEADER-STATUS,
       VENDOR_NAME TYPE ZFREIGHT_HEADER-VENDOR_NAME,
       FROM_LOC TYPE ZFREIGHT_HEADER-FROM_LOC,
       TO_LOC TYPE ZFREIGHT_HEADER-TO_LOC,
       TRUCK_TYPE TYPE ZFREIGHT_HEADER-TRUCK_TYPE,
       TRUCK_DES TYPE ZFREIGHT_HEADER-TRUCK_DES,
       FILLING_TYPE TYPE ZFREIGHT_HEADER-FILLING_TYPE,
       FILING_DES TYPE ZFREIGHT_HEADER-FILING_DES,
       FREIGHT_CHARGE TYPE ZFREIGHT_HEADER-FREIGHT_CHARGE,
       LODING_CHARGE TYPE ZFREIGHT_HEADER-LODING_CHARGE,
       UNLOAD_CHARGE TYPE ZFREIGHT_HEADER-UNLOAD_CHARGE,
       HALT_CHARGE TYPE ZFREIGHT_HEADER-HALT_CHARGE,
       LR_CHARGE TYPE ZFREIGHT_HEADER-LR_CHARGE,
       NO_OF_DAYS TYPE ZFREIGHT_HEADER-NO_OF_DAYS,
       VECHILE_NUMBER TYPE ZFREIGHT_HEADER-VECHILE_NUMBER,
       CRDATE TYPE ZFREIGHT_HEADER-CRDATE,
       TRN_VALUE TYPE ZFREIGHT_HEADER-TRN_VALUE,
       REMARKS TYPE ZFREIGHT_HEADER-REMARKS,
     END OF GS_HEADER .

TYPES : BEGIN OF GS_ITEM,
         TRIP_NO TYPE ZFREIGHT_ITEM-TRIP_NO ,
         INVOICE_NO TYPE ZFREIGHT_ITEM-INVOICE_NO,
         CUSTOMER_NAME TYPE ZFREIGHT_ITEM-CUSTOMER_NAME,
         INVOICE_AMOUNT TYPE ZFREIGHT_ITEM-INVOICE_AMOUNT,
         WEIGHT TYPE ZFREIGHT_ITEM-WEIGHT,
         COMPANY_CODE TYPE ZFREIGHT_ITEM-COMPANY_CODE,
         LOCATION TYPE ZFREIGHT_ITEM-LOCATION,
         LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
         "REMARKS TYPE ZFREIGHT_ITEM-REMARKS,
       END OF GS_ITEM .

TYPES : BEGIN OF GS_FINAL,
          TRIP_NO TYPE ZFREIGHT_HEADER-TRIP_NO,
          CRDATE TYPE ZFREIGHT_HEADER-CRDATE,
          VECHILE_NUMBER TYPE ZFREIGHT_HEADER-VECHILE_NUMBER,
          TRN_VALUE TYPE ZFREIGHT_HEADER-TRN_VALUE,
          LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
          VEN_IN_NO(20) ,
         END OF GS_FINAL .

TYPES : BEGIN OF GS_UPDATE ,
          MANDT  TYPE ZFREIGHT_MAP-MANDT,
          TRIP_NO TYPE ZFREIGHT_MAP-TRIP_NO,
          CRDATE TYPE ZFREIGHT_MAP-CRDATE,
          VECHILE_NUMBER TYPE ZFREIGHT_MAP-VECHILE_NUMBER,
          TRN_VALUE TYPE ZFREIGHT_MAP-TRN_VALUE,
          LR_NO TYPE ZFREIGHT_MAP-LR_NO,
          VEN_IN_NO TYPE ZFREIGHT_MAP-VECH_INV_NUMBER ,
        END OF GS_UPDATE ,

BEGIN OF GS_VEN,
            SNO TYPE I,
            VEN_IN_NO TYPE ZFREIGHT_MAP-VECH_INV_NUMBER ,
END OF GS_VEN.


DATA : GT_HEADER TYPE TABLE OF GS_HEADER ,
       WA_HEADER TYPE GS_HEADER .

DATA : GT_ITEM TYPE TABLE OF GS_ITEM ,
       WA_ITEM TYPE GS_ITEM .

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL,
       GT_FINAL1 TYPE TABLE OF ZFREIGHT_MAP,
       WA_FINAL1 TYPE ZFREIGHT_MAP.

DATA : GT_UPDATE TYPE TABLE OF GS_UPDATE,
       WA_UPDATE TYPE GS_UPDATE .

DATA : GT_FIN TYPE TABLE OF ZFREIGHT_MAP,
       WA_FIN TYPE ZFREIGHT_MAP,
       GT_CHK TYPE TABLE OF ZFREIGHT_MAP,
       WA_CHK TYPE ZFREIGHT_MAP,
       GT_VEND TYPE TABLE OF GS_VEN,
       GS_VEND TYPE GS_VEN,
       SNO TYPE I VALUE 0.


************* Tables related to Search help *************
DATA : IT_RETURN TYPE STANDARD TABLE OF DDSHRETVAL,
     WA_RETURN TYPE DDSHRETVAL,
     IT_LFA1 TYPE TABLE OF TY_LFA1,
     IT_DYFIELDS TYPE TABLE OF DYNPREAD,
     GS_DYFIELDS TYPE DYNPREAD .
************** Tables related to Search help ************

DATA : INV_VEN1 TYPE ZFREIGHT_HEADER-VENDOR_NAME .

DATA : OK_CODE TYPE SY-UCOMM .

CONTROLS: TC_INV TYPE TABLEVIEW USING SCREEN 1111.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1111 OUTPUT.
  CLEAR: SNO.
  REFRESH: GT_VEND.

  SET PF-STATUS '1111PF'.
  SET TITLEBAR '1111_TIT'.

ENDMODULE.                 " STATUS_1111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SH_1111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SH_1111 INPUT.

  PERFORM SEARCH_HELP_VENDOR_1111.

ENDMODULE.                 " SH_1111  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_VENDOR_1111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_HELP_VENDOR_1111 .

  SELECT LIFNR
     NAME1
     FROM LFA1
     INTO TABLE IT_LFA1
     WHERE KTOKK EQ 'YBTR'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
EXPORTING
*   DDIC_STRUCTURE         = ' '
RETFIELD               = 'NAME1_GP'
*   PVALKEY                = ' '
*   DYNPPROG               = ' '
*   DYNPNR                 = ' '
DYNPROFIELD            = 'INV_VEN1'
*   STEPL                  = 0
WINDOW_TITLE           = 'Transporter Name'
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
VALUE_TAB              = IT_LFA1
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

    GS_DYFIELDS-FIELDNAME  = 'INV_VEN1'.

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

ENDFORM.                    " SEARCH_HELP_VENDOR_1111
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1111 INPUT.

  CLEAR OK_CODE.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'INV'.
      PERFORM GET_INVOICE .
    WHEN 'IN_SUB'.
      PERFORM GET_UPDATE .
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1111  INPUT


*&---------------------------------------------------------------------*
*&      Form  GET_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_INVOICE .


  SELECT  MANDT
        TRIP_NO
        STATUS
        VENDOR_NAME
        FROM_LOC
        TO_LOC
        TRUCK_DES
        TRUCK_DES
        FILLING_TYPE
        FILING_DES
        FREIGHT_CHARGE
        LODING_CHARGE
        UNLOAD_CHARGE
        HALT_CHARGE
        LR_CHARGE
        NO_OF_DAYS
        VECHILE_NUMBER
        CRDATE
        TRN_VALUE
       REMARKS FROM ZFREIGHT_HEADER INTO TABLE GT_HEADER . " WHERE VENDOR_NAME IN INV_VEN1.  "WHERE VENDOR_NAME EQ INV_VEN1 .

  SELECT TRIP_NO
         INVOICE_NO
         CUSTOMER_NAME
         INVOICE_AMOUNT
         WEIGHT
         COMPANY_CODE
         LOCATION
         LR_NO
         FROM ZFREIGHT_ITEM INTO TABLE GT_ITEM FOR ALL ENTRIES IN GT_HEADER WHERE TRIP_NO = GT_HEADER-TRIP_NO .

  SELECT MANDT
         TRIP_NO
         CRDATE
         LR_NO
         TRN_VALUE
         VECHILE_NUMBER
         VECH_INV_NUMBER
         FROM ZFREIGHT_MAP CLIENT SPECIFIED
         INTO TABLE GT_FINAL1.

  LOOP AT GT_ITEM INTO WA_ITEM .
    WA_FINAL-LR_NO = WA_ITEM-LR_NO.
    WA_FINAL-TRIP_NO = WA_ITEM-TRIP_NO.
    LOOP AT GT_HEADER INTO WA_HEADER  WHERE TRIP_NO = WA_FINAL-TRIP_NO .
      WA_FINAL-CRDATE  = WA_HEADER-CRDATE .
      WA_FINAL-VECHILE_NUMBER = WA_HEADER-VECHILE_NUMBER.
      WA_FINAL-TRN_VALUE = WA_HEADER-TRN_VALUE.
      READ TABLE GT_FINAL1 INTO WA_FINAL1 WITH KEY LR_NO = WA_FINAL-LR_NO
                                                   TRIP_NO = WA_FINAL-TRIP_NO
                                                   CRDATE = WA_FINAL-CRDATE
                                                   VECHILE_NUMBER = WA_HEADER-VECHILE_NUMBER
                                                   TRN_VALUE = WA_FINAL-TRN_VALUE.
      IF SY-SUBRC EQ 0.

        WA_FINAL-VEN_IN_NO = WA_FINAL1-VECH_INV_NUMBER.

      ELSE.

        CLEAR :WA_FINAL-VEN_IN_NO.  "= wa_final1-VECH_INV_NUMBER.

      ENDIF.




    ENDLOOP.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.










ENDFORM.                    " GET_INVOICE
*&---------------------------------------------------------------------*
*&      Form  GET_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_UPDATE .

  "GS_FHEAD-MANDT = SY-MANDT.
  LOOP AT GT_FINAL INTO WA_FINAL.

    WA_FIN-MANDT = SY-MANDT.
    WA_FIN-LR_NO = WA_FINAL-LR_NO.
    WA_FIN-TRIP_NO = WA_FINAL-TRIP_NO.
    WA_FIN-CRDATE  = WA_FINAL-CRDATE .
    WA_FIN-VECHILE_NUMBER = WA_FINAL-VECHILE_NUMBER.
    WA_FIN-TRN_VALUE = WA_FINAL-TRN_VALUE.
    READ TABLE GT_VEND INTO GS_VEND INDEX SY-TABIX.
    IF SY-SUBRC EQ 0.
      WA_FIN-VECH_INV_NUMBER = GS_VEND-VEN_IN_NO.
    ENDIF.
*    WA_FIN-VECH_INV_NUMBER = WA_FINAL-VEN_IN_NO.

    APPEND WA_FIN TO GT_FIN .

  ENDLOOP.

  SELECT MANDT
         TRIP_NO
         CRDATE
         LR_NO
         TRN_VALUE
         VECHILE_NUMBER
         VECH_INV_NUMBER
         FROM ZFREIGHT_MAP
         INTO TABLE GT_CHK
         FOR ALL ENTRIES IN GT_FIN WHERE
         TRIP_NO EQ GT_FIN-TRIP_NO.


  LOOP AT GT_FIN INTO WA_FIN.
    READ TABLE GT_CHK INTO WA_CHK WITH KEY MANDT = WA_FIN-MANDT
                                                 TRIP_NO = WA_FIN-TRIP_NO
                                                 CRDATE = WA_FIN-CRDATE
                                                 LR_NO = WA_FIN-LR_NO
                                                 TRN_VALUE = WA_FIN-TRN_VALUE
                                                 VECHILE_NUMBER =  WA_FIN-VECHILE_NUMBER.


    IF SY-SUBRC EQ 0.
      UPDATE ZFREIGHT_MAP FROM  WA_FIN."TABLE GT_FIN.
      COMMIT WORK.
    ELSE.
      INSERT ZFREIGHT_MAP FROM WA_FIN."TABLE GT_FIN.
      COMMIT WORK.
    ENDIF.



  ENDLOOP.




ENDFORM.                    " GET_UPDATE

*&---------------------------------------------------------------------*
*&      Module  TC_VALUES_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_VALUES_GET INPUT.

  CLEAR OK_CODE.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'IN_SUB'.
      PERFORM GET_VEN_INV_UPDATE .
  ENDCASE.

ENDMODULE.                 " TC_VALUES_GET  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_VEN_INV_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VEN_INV_UPDATE .
*  refresh: GT_VEND.

  SNO = SNO + 1.
  GS_VEND-SNO = SNO.
  GS_VEND-VEN_IN_NO = WA_FINAL-VEN_IN_NO.

  APPEND GS_VEND TO GT_VEND.
  CLEAR: GS_VEND.


ENDFORM.                    " GET_VEN_INV_UPDATE
