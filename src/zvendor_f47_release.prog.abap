*&---------------------------------------------------------------------*
*& Report  ZVENDOR_F47_RELEASE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZVENDOR_F47_RELEASE.
TABLES: LFA1.
TABLES : SSCRFIELDS.

DATA: IT_VEND_F47 TYPE TABLE OF ZVEND_F47,
      WA_VEND_F47 TYPE ZVEND_F47.


DATA: I TYPE N VALUE 0.
DATA: J TYPE I VALUE 1.
DATA: MSG TYPE CHAR50.

SELECTION-SCREEN:BEGIN OF BLOCK VR WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_LIFNR FOR LFA1-LIFNR NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK VR.

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZRELEASE'.

AT SELECTION-SCREEN.

  CASE SSCRFIELDS-UCOMM .
    WHEN 'RELEASE'.
      IF S_LIFNR IS NOT INITIAL.
        LOOP AT S_LIFNR.
          SELECT SINGLE * FROM ZVEND_F47 INTO WA_VEND_F47 WHERE LIFNR EQ S_LIFNR-LOW.
          IF WA_VEND_F47 IS NOT INITIAL.
            UPDATE ZVEND_F47 SET RE_FLAG = 'T' RE_DATE = SY-DATUM WHERE LIFNR = S_LIFNR-LOW.
            CLEAR: WA_VEND_F47.
          ELSE.

            MESSAGE: 'Please Enter Valid Vendor Code' TYPE 'I' DISPLAY LIKE 'E'.
            J = J + 1 .
*      WA_VEND_F47-LIFNR = S_LIFNR-LOW.
*      WA_VEND_F47-RE_FLAG = 'T'.
*      WA_VEND_F47-RE_DATE = SY-DATUM .
*      INSERT INTO ZVEND_F47 VALUES WA_VEND_F47 .
*      CLEAR: WA_VEND_F47.
*      COMMIT WORK.
          ENDIF.
          I = I + 1.
        ENDLOOP.

        IF I > 1 AND J <> 2.
          CONCATENATE 'Vendor ' I ' Released for advance payment' INTO MSG.
          MESSAGE MSG TYPE 'I' DISPLAY LIKE 'I'.
        ELSEIF I = 1 AND J <> 2 .
          SHIFT S_LIFNR-LOW LEFT DELETING LEADING '0'.
          CONCATENATE 'Vendor' S_LIFNR-LOW ' Released for advance payment' INTO MSG.
          MESSAGE MSG TYPE 'I' DISPLAY LIKE 'I'.
        ENDIF.
        CLEAR : J .
      ELSE.
        MESSAGE 'Please Enter Valid Data' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
      CLEAR: I,J.
    WHEN '%001'.

    WHEN 'BACK'.
      "EXIT.
      LEAVE TO SCREEN 1000 .
    WHEN OTHERS.
      MESSAGE 'Please Click Valid Button' TYPE 'I' DISPLAY LIKE 'I'.
  ENDCASE.
