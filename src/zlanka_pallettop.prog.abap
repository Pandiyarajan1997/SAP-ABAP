*&---------------------------------------------------------------------*
*& Include ZLANKA_PALLETTOP                                  Module Pool      ZSRILANKA_PALLET
*&
*&---------------------------------------------------------------------*

PROGRAM ZSRILANKA_PALLET.

DATA:OK_CODE TYPE SY-UCOMM.

TYPES : BEGIN OF GS_VBRP,
         VBELN TYPE VBELN_VF,
         POSNR TYPE VBRP-POSNR,
         MATNR TYPE VBRP-MATNR,
         FKIMG TYPE VBRP-FKIMG,
        END OF GS_VBRP.

TYPES : BEGIN OF GS_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF GS_MAKT.

TYPES : BEGIN OF GS_HEADER,
         MANDT TYPE ZPALLET_HEADERS-MANDT,
         VBELN TYPE ZPACK_PALLET-VBELN,
         PORT TYPE ZPALLET_HEADERS-PORT,
         PLACE TYPE ZPALLET_HEADERS-PLACE,
         FREIGHT TYPE ZPALLET_HEADERS-FREIGHT,
         INSURANCE TYPE ZPALLET_HEADERS-INSURANCE,
        END OF GS_HEADER.


*TYPES : BEGIN OF GS_FINAL,
*         VBELN TYPE VBELN_VF,
*         POSNR TYPE VBRP-POSNR,
*         MATNR TYPE VBRP-MATNR,
*         FKIMG TYPE VBRP-FKIMG,
*         MAKTX TYPE MAKT-MAKTX,
*         sel_type type char25,
*         SEL_NO TYPE ZPACK_PALLET-PAL_NUMBER ,
*         PACK_QTY TYPE I,
*        END OF GS_FINAL.


TYPES : BEGIN OF GS_FINAL,
         MANDT TYPE ZPACK_PALLET-MANDT,
         VBELN TYPE ZPACK_PALLET-VBELN,
         POSNR TYPE ZPACK_PALLET-POSNR ,
         SL_NO TYPE ZPACK_PALLET-SL_NO,
         MATNR TYPE ZPACK_PALLET-MATNR,
         MAKTX TYPE ZPACK_PALLET-MAKTX,
         FKIMG TYPE ZPACK_PALLET-FKIMG,
         SEL_TYPE TYPE ZPACK_PALLET-SEL_TYPE ,
         PAL_NUMBER TYPE ZPACK_PALLET-PAL_NUMBER,
         PACK_QTY TYPE ZPACK_PALLET-PACK_QTY,
         NET_WEI TYPE ZPACK_PALLET-NET_WEI,
         GROSS_WEI TYPE ZPACK_PALLET-GROSS_WEI,
         CONTAIN_NO TYPE ZPACK_PALLET-CONTAIN_NO,
         CREDATED_BY TYPE ZPACK_PALLET-CREDATED_BY,
         CREDATED_DATE TYPE ZPACK_PALLET-CREDATED_DATE,
         CREDATED_TIME TYPE ZPACK_PALLET-CREDATED_TIME,
        END OF GS_FINAL.

TYPES : BEGIN OF GS_FINAL1,
         MANDT TYPE ZPACK_PALLET-MANDT,
         VBELN TYPE ZPACK_PALLET-VBELN,
         POSNR TYPE ZPACK_PALLET-POSNR ,
         SL_NO TYPE ZPACK_PALLET-SL_NO,
         MATNR TYPE ZPACK_PALLET-MATNR,
         MAKTX TYPE ZPACK_PALLET-MAKTX,
         FKIMG TYPE ZPACK_PALLET-FKIMG,
         SEL_TYPE TYPE ZPACK_PALLET-SEL_TYPE ,
         PAL_NUMBER TYPE ZPACK_PALLET-PAL_NUMBER,
         PACK_QTY TYPE ZPACK_PALLET-PACK_QTY,
         NET_WEI TYPE ZPACK_PALLET-NET_WEI,
         GROSS_WEI TYPE ZPACK_PALLET-GROSS_WEI,
         CONTAIN_NO TYPE ZPACK_PALLET-CONTAIN_NO,
         CREDATED_BY TYPE ZPACK_PALLET-CREDATED_BY,
         CREDATED_DATE TYPE ZPACK_PALLET-CREDATED_DATE,
         CREDATED_TIME TYPE ZPACK_PALLET-CREDATED_TIME,
        END OF GS_FINAL1.

* DATA: BEGIN OF ZPACK_PALLET,
*         MANDT TYPE ZPACK_PALLET-MANDT,
*         VBELN TYPE ZPACK_PALLET-VBELN,
*         SL_NO TYPE ZPACK_PALLET-SL_NO,
*         MATNR TYPE ZPACK_PALLET-MATNR,
*         MAKTX TYPE ZPACK_PALLET-MAKTX,
*         FKIMG TYPE ZPACK_PALLET-FKIMG,
*         SEL_TYPE TYPE ZPACK_PALLET-SEL_TYPE,
*         PAL_NUMBER TYPE ZPACK_PALLET-PAL_NUMBER,
*         PACK_QTY TYPE ZPACK_PALLET-PACK_QTY,
*         CREDATED_BY TYPE ZPACK_PALLET-CREDATED_BY,
*         CREDATED_DATE TYPE ZPACK_PALLET-CREDATED_DATE,
*         CREDATED_TIME TYPE ZPACK_PALLET-CREDATED_TIME,
*       END OF ZPACK_PALLET.

DATA : IT_VBRP TYPE TABLE OF GS_VBRP,
       WA_VBRP TYPE GS_VBRP.
DATA : IT_MAKT TYPE TABLE OF GS_MAKT,
       WA_MAKT TYPE GS_MAKT.
DATA : IT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : IT_FINAL1 TYPE TABLE OF GS_FINAL1,
       WA_FINAL1 TYPE GS_FINAL1.

DATA : INV_NO TYPE VBRK-VBELN.

DATA : CUS_VBELN TYPE ZPACK_PALLET-VBELN.

DATA : LV_INV TYPE VBRP-VBELN.
DATA : LOCAL_INV TYPE VBRP-VBELN.

DATA : LV_COUNT TYPE I.

DATA : PORT_NO TYPE ZPALLET_HEADERS-PORT,
       PLACE TYPE ZPALLET_HEADERS-PLACE,
       FREIGHT TYPE ZPALLET_HEADERS-FREIGHT,
       INSU TYPE ZPALLET_HEADERS-INSURANCE.

"DATA : SER_NO TYPE ZPACK_PALLET-SL_NO.

DATA : LV_MAKTX TYPE ZPACK_PALLET-MAKTX.
DATA : LV_FKIMG TYPE ZPACK_PALLET-FKIMG.

DATA : USER TYPE ZPACK_PALLET-CREDATED_BY,
       USER_DATE TYPE ZPACK_PALLET-CREDATED_DATE,
       USER_TIME TYPE ZPACK_PALLET-CREDATED_TIME.

TYPE-POOLS : VRM.
DATA: LD_FIELD    TYPE VRM_ID ,
      IT_LISTBOX  TYPE VRM_VALUES,
      WA_LISTBOX  LIKE LINE OF IT_LISTBOX.

DATA: IT_ZPACK TYPE STANDARD TABLE OF ZPACKLIST,
      WA_ZPACK TYPE ZPACKLIST.

DATA :IT_HEADER TYPE TABLE OF GS_HEADER,
      WA_HEADER TYPE GS_HEADER.

*TYPES : BEGIN OF TAB_STR,
*         VBELN TYPE ZPACK_PALLET-VBELN,
*         POSNR TYPE ZPACK_PALLET-POSNR ,
*         SL_NO TYPE ZPACK_PALLET-SL_NO,
*         MATNR TYPE ZPACK_PALLET-MATNR,
*         MAKTX TYPE ZPACK_PALLET-MAKTX,
*         FKIMG TYPE ZPACK_PALLET-FKIMG,
*         SEL_TYPE TYPE ZPACK_PALLET-SEL_TYPE ,
*         PAL_NUMBER TYPE ZPACK_PALLET-PAL_NUMBER,
*         PACK_QTY TYPE ZPACK_PALLET-PACK_QTY,
*         NET_WEI TYPE ZPACK_PALLET-NET_WEI,
*         GROSS_WEI TYPE ZPACK_PALLET-GROSS_WEI,
*         CONTAIN_NO TYPE ZPACK_PALLET-CONTAIN_NO,
*      END OF TAB_STR.
*
*DATA : TAB TYPE TABLE OF TAB_STR.
CONTROLS: TAB TYPE TABLEVIEW USING SCREEN 1111.
"CONTROLS: TAB TYPE TABLEVIEW USING SCREEN 200.

DATA : GT_VALUES   TYPE TABLE OF DYNPREAD,
       WA_VALUES  TYPE DYNPREAD.

DATA : IT_RETURN TYPE STANDARD TABLE OF DDSHRETVAL,
       WA_RETURN TYPE DDSHRETVAL,
       GS_DYFIELDS TYPE DYNPREAD,
       IT_DYFIELDS TYPE TABLE OF DYNPREAD.
