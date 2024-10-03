*&---------------------------------------------------------------------*
*& Report ZHR_ADMIN_PRG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_hoadmin_emp.
INCLUDE zhr_hoadmin_emp_top.
INCLUDE zhr_hoadmin_emp_cls.


START-OF-SELECTION.

  CREATE OBJECT go_main.

  CALL SCREEN 9001.


END-OF-SELECTION.
