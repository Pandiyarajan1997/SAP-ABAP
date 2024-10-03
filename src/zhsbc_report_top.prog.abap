*&---------------------------------------------------------------------*
*&  Include           ZHSBC_REPORT_TOP
*&---------------------------------------------------------------------*

tables : ZHSBC_ITEMS, t001, t012k.


 DATA :IT_RETURN     TYPE TABLE OF DDSHRETVAL,
      WA_RETURN     TYPE DDSHRETVAL,
      IT_DYNPREAD   TYPE TABLE OF DYNPREAD,
      WA_DYNPREAD   TYPE DYNPREAD,
      G_TAB_T001          TYPE TABLE OF T001.


 DATA : IT_FCAT TYPE  SLIS_T_FIELDCAT_ALV,
        WA_FCAT TYPE  SLIS_FIELDCAT_ALV,
        WA_LAYOUT TYPE SLIS_LAYOUT_ALV,

        WA_FINAL TYPE ZHSBC_ITEMS,
        IT_FINAL TYPE TABLE OF ZHSBC_ITEMS.


TYPES: BEGIN OF TY_T012K,
        BUKRS TYPE T012K-BUKRS,
        HBKID TYPE T012K-HBKID,
        HKTID TYPE T012K-HKTID,
        BANKN TYPE T012K-BANKN,
        BKONT TYPE T012K-BKONT,
        WAERS TYPE T012K-WAERS,
        HKONT TYPE T012K-HKONT,
      END OF TY_T012K.

DATA : WA_T012K TYPE TY_T012K,
       IT_T012K TYPE TABLE OF TY_T012K.
