*&---------------------------------------------------------------------*
*& Report  ZSERACH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSERACH.

DATA : GT_GJAHR TYPE TABLE OF BKPF,
       WA_GJAHR TYPE TABLE OF BKPF .

SELECT GJAHR FROM BKPF INTO TABLE GT_GJAHR  .
