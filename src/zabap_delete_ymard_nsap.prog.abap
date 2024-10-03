*&---------------------------------------------------------------------*
*& Report ZABAP_DELETE_YMARD_NSAP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_delete_ymard_nsap.

TABLES: ymard_nsap.

DATA: it_ymard_nsap TYPE TABLE OF ymard_nsap.

DATA: lv_cnt TYPE i.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_date FOR ymard_nsap-erdat OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk1.

START-OF-SELECTION.

  IF sy-uname = '704322' OR sy-uname = 'SPLIT'.
    SELECT * FROM ymard_nsap INTO TABLE it_ymard_nsap WHERE erdat IN s_date.
    IF sy-subrc = 0.
      DESCRIBE TABLE it_ymard_nsap LINES lv_cnt.

      WRITE:/ 'No of lines selected' , lv_cnt.

      DELETE ymard_nsap FROM TABLE it_ymard_nsap.
      WRITE:/ 'No of lines deleted' , sy-dbcnt.
    else.
      WRITE:/ 'No Records found for Deletion'.
    ENDIF.
  ELSE.
    WRITE:/ 'You are not allowed to run the deletion program'.
  ENDIF.
