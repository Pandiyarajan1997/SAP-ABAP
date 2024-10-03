*&---------------------------------------------------------------------*
*& Report ZRAMKEEPALIVE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRAMKEEPALIVE.

DATA: t_file TYPE STANDARD TABLE OF localfile.
APPEND 'test' TO t_file.
DATA: w_string_file TYPE string.
w_string_file =  'D:\123.txt'.

DO .
   CALL FUNCTION 'GUI_DOWNLOAD'
     EXPORTING
       filename = w_string_file
     TABLES
       data_tab = t_file
     EXCEPTIONS
       OTHERS   = 22.

   IF sy-subrc <> 0.
   ENDIF.

   CALL FUNCTION 'GUI_UPLOAD'
     EXPORTING
       filename = w_string_file
     TABLES
       data_tab = t_file
     EXCEPTIONS
       OTHERS   = 17.
   IF sy-subrc <> 0.
   ENDIF.
ENDDO.
