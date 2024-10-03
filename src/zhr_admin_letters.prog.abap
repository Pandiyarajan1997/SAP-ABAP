*&---------------------------------------------------------------------*
*& Report ZHR_ADMIN_LETTERS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_admin_letters.
INCLUDE zhr_admin_letters_emp_top.
*INCLUDE zhr_admin_letters_emp_cls.

INCLUDE zhr_admin_letter_emp_cla.

INCLUDE zhr_admin_letter_emp_pbo.

INCLUDE zhr_admin_letter_emp_pai.

AT SELECTION-SCREEN OUTPUT.

INITIALIZATION.

  CREATE OBJECT go_adobe.

START-OF-SELECTION.


  CALL SCREEN 9001.


END-OF-SELECTION.
