class ZCL_IM_MM_VENDOR_DEBIT_PO definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_VENDOR_DEBIT_PO IMPLEMENTATION.


METHOD IF_EX_ME_PURCHDOC_POSTED~POSTED.

*  TYPES : BEGIN OF GS_BSIK,
*                LIFNR TYPE BSIK-LIFNR,
*                BLART TYPE BSIK-BLART,
*                SHKZG TYPE BSIK-SHKZG,
*                DMBTR TYPE BSIK-DMBTR,
*               END OF GS_BSIK.
*  DATA : WA_IM_KOMV LIKE LINE OF IM_KOMV.
*
*  "DATA : LV_BAL TYPE ZVEN_DEBIT_TAB-BALANCE .
*
*  DATA : LV_BAL TYPE ZVEN_DEBIT_TAB-PURCHASE_LIMIT .
*
*  DATA : LV_PURLIMIT TYPE ZVEN_DEBIT_TAB-PURCHASE_LIMIT .
*
*  DATA : LV_MAXDAYS1 TYPE ZVEN_DEBIT_TAB-LV_DAYS1 .
*
*  DATA : LV_MAXDAYS(10) TYPE N.
*
*  DATA : MS_LIMIT(10) .
*
*  DATA : MS_DAYS(6) .
*
*  DATA : LV_ALLDAYS1 TYPE ZVEN_DEBIT_TAB-ALLOWED_DAYS .
*
*  DATA : LV_ALLDAYS(10) TYPE N.
*
*  DATA : LV_BAL1 TYPE KONV-KWERT .
*
*  DATA : LV_AMOUNT TYPE ZPURCHASE_LMT.
*
*  DATA : GT_BSIK TYPE STANDARD TABLE OF GS_BSIK,
*         WA_BSIK TYPE GS_BSIK.
*
*  DATA : IT_EKPO TYPE TABLE OF UEKPO ,
*         WA_EKPO TYPE UEKPO.
*
*  DATA : LV_MTART TYPE EKPO-MTART.
*
*  DATA : MSG(600).
*
*  DATA : WA_IM_EKPO LIKE LINE OF IM_EKPO .
*
*
*  DATA : LV_BUKRS TYPE EKPO-BUKRS.
*
*  LOOP AT IM_EKPO INTO WA_IM_EKPO .
*
*    LV_MTART = WA_IM_EKPO-MTART .
*
*  ENDLOOP.
*
*  IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N' .
*
*
*    IF IM_EKKO-BSART EQ 'ZNB' OR IM_EKKO-BSART EQ 'NB'.
*
*      IF IM_EKKO-BUKRS EQ '1400'.
*
*      ELSE.
*
*        IF LV_MTART EQ 'VERP' OR LV_MTART EQ 'ROH' .
*
*          SELECT SINGLE PURCHASE_LIMIT FROM ZVEN_DEBIT_TAB INTO LV_PURLIMIT WHERE LIFNR = IM_EKKO-LIFNR .
*
*          SELECT  LIFNR
*                  BLART
*                  SHKZG
*                  DMBTR
*                      FROM BSIK INTO TABLE GT_BSIK WHERE LIFNR = IM_EKKO-LIFNR AND BLART NE 'KA' .
*
*          LOOP AT GT_BSIK INTO WA_BSIK.
*            IF WA_BSIK-SHKZG EQ 'S' .
*              WA_BSIK-DMBTR  = - ( WA_BSIK-DMBTR )  .
*            ENDIF.
*            LV_AMOUNT = LV_AMOUNT + WA_BSIK-DMBTR.
*          ENDLOOP.
*
*          SELECT SINGLE LV_DAYS1 FROM ZVEN_DEBIT_TAB INTO LV_MAXDAYS1 WHERE LIFNR = IM_EKKO-LIFNR .
*
*          LV_MAXDAYS = LV_MAXDAYS1.
*
*          SELECT SINGLE ALLOWED_DAYS FROM ZVEN_DEBIT_TAB INTO LV_ALLDAYS1 WHERE LIFNR = IM_EKKO-LIFNR .
*
*
*          LV_ALLDAYS = LV_ALLDAYS1.
*
*          LOOP AT IM_KOMV INTO WA_IM_KOMV WHERE KINAK <> 'W'.
*            LV_BAL1 = LV_BAL1 + WA_IM_KOMV-KWERT .
*          ENDLOOP.
*
*          LV_BAL = LV_PURLIMIT  - LV_AMOUNT .
*
*          IF  ( LV_BAL1 > LV_BAL ) OR ( LV_MAXDAYS > LV_ALLDAYS ) .
*            IF ( LV_BAL1 > LV_BAL ) AND ( LV_MAXDAYS < LV_ALLDAYS ) .
*              MS_LIMIT = LV_BAL1 - ( LV_BAL ).
*              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*                CHANGING
*                  VALUE = MS_LIMIT.
*              CONCATENATE 'Vendor Debit Limit has been Exceeded By' MS_LIMIT 'Rs' INTO MSG SEPARATED BY SPACE.
*              MESSAGE MSG TYPE 'E' DISPLAY LIKE 'I'.
*              LEAVE TO CURRENT TRANSACTION.
**            ELSE.
*            ENDIF.
*
*
*            IF ( LV_BAL1 < LV_BAL )  AND ( LV_MAXDAYS > LV_ALLDAYS ).
*              MS_DAYS = LV_ALLDAYS - LV_MAXDAYS .
*              IF MS_DAYS < 0.
*                CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*                  CHANGING
*                    VALUE = MS_DAYS.
*                MS_DAYS = - ( MS_DAYS ).
*                CONCATENATE 'Vendor Debit Limit has been Exceeded By' MS_DAYS 'Days' INTO MSG SEPARATED BY SPACE.
*                MESSAGE MSG TYPE 'E' DISPLAY LIKE 'I'.
*                LEAVE TO CURRENT TRANSACTION.
*              ENDIF.
**            ELSE.
*            ENDIF.
*            IF ( LV_BAL1 > LV_BAL ) AND ( LV_MAXDAYS > LV_ALLDAYS ).
*              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*                CHANGING
*                  VALUE = MS_LIMIT.
*              MS_LIMIT = LV_BAL1 - LV_BAL .
*              MS_DAYS =  LV_ALLDAYS - LV_MAXDAYS .
*              MS_DAYS = - ( MS_DAYS ) .
*              CONCATENATE 'Vendor Debit Limit has been Exceeded By' MS_LIMIT 'Rs And' MS_DAYS 'Days' INTO MSG SEPARATED BY SPACE.
*              MESSAGE MSG TYPE 'E' DISPLAY LIKE 'I'.
*              LEAVE TO CURRENT TRANSACTION.
**            ELSE.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.

ENDMETHOD.
ENDCLASS.
