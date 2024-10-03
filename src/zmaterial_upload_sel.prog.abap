*&---------------------------------------------------------------------*
*& Include zmaterial_upload_sel
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&    Selection Screen Declarations
*&---------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_fname  TYPE rlgrap-filename.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN:BEGIN of BLOCK B2 WITH FRAME TITLE text-003.
SELECTION-SCREEN:BEGIN OF Line.
       SELECTION-SCREEN COMMENT 1(75) Text-002.
SELECTION-SCREEN:END OF LINE.
SELECTION-SCREEN: END  OF BLOCK B2.
SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.
