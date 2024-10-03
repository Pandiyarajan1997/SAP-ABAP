*&---------------------------------------------------------------------*
*& Include zmm_sled_update_sel
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE text-001.
       PARAMETERS: p_fname type rlgrap-filename .
       SELECTION-SCREEN SKIP.

SELECTION-SCREEN:BEGIN of BLOCK B2 WITH FRAME TITLE text-003.
SELECTION-SCREEN:BEGIN OF Line.
       SELECTION-SCREEN COMMENT 1(75) Text-002.
SELECTION-SCREEN:END OF LINE.
SELECTION-SCREEN: END  OF BLOCK B2.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN FUNCTION KEY 1.

DATA:p_budat type budat.
