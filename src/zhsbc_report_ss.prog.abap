*&---------------------------------------------------------------------*
*&  Include           ZHSBC_REPORT_SS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE  TEXT-S04 .
SELECTION-SCREEN BEGIN OF LINE.


PARAMETERS: P_VENDOR RADIOBUTTON GROUP R1  USER-COMMAND FLG DEFAULT 'X' .
SELECTION-SCREEN COMMENT 3(15) TEXT-S01.

PARAMETERS: P_GL   RADIOBUTTON GROUP R1 .
SELECTION-SCREEN COMMENT 21(17) TEXT-S03." MODIF ID REZ FOR FIELD P_GL.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S05.

PARAMETERS :  P_BUKRS TYPE T001-BUKRS,

              P_HBKID TYPE T012K-HBKID ,

              P_HKTID TYPE T012K-HKTID.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.

SELECT-OPTIONS : S_LIFNR   FOR zhsbc_items-LIFNR MODIF ID VEN,
                 S_HKONT   FOR zhsbc_items-HKONT MODIF ID GL.

SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S06.

SELECT-OPTIONS : S_BUDAT   FOR zhsbc_items-BUDAT,
                 S_EXDATE  FOR zhsbc_items-EXTR_DATE DEFAULT SY-DATUM ,
                 S_REDATE  FOR zhsbc_items-REX_DATE.

SELECTION-SCREEN END OF BLOCK B2.

AT SELECTION-SCREEN.

IF  sy-ucomm = 'ONLI' and p_bukrs is INITIAL.

  message 'Enter Company Code....' TYPE 'E'.

ENDIF.
