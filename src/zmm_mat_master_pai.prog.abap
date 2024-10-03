*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_PAI
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZMM_MAT'.
  SET TITLEBAR 'ZMM_MAT'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.

      CLEAR lobj_mat.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.

      LEAVE PROGRAM.

    WHEN 'EDIT'.          "make it editable the field

      lobj_mat->alv('X')."call method for display main alv.


    WHEN 'UPDATE'.        "Save the changes into database.

      lobj_mat->pop_up( ).

  ENDCASE.

ENDMODULE.
