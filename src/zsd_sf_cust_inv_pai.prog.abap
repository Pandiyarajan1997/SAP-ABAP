*&---------------------------------------------------------------------*
*& Include          ZSD_SF_CUST_INV_PAI
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  IF p_appr = abap_true .
    SET PF-STATUS 'ZSD_CUST'.
  ELSE.
    SET PF-STATUS 'ZSD_CUST' EXCLUDING fcode.
  ENDIF.
  SET TITLEBAR 'ZSD_CUST'.
  CALL METHOD lobj_cust->alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
*      IF p_status NE '99'.
*        MESSAGE 'Records status contains other than 99 . Rerun the program with status 99 to update'
*        TYPE 'I' DISPLAY LIKE 'E'.
*      ELSE.
*        lobj_cust->update_10( ).
*        MESSAGE 'Records has been updated successfully'
*        TYPE 'I' DISPLAY LIKE 'S'.
*        LEAVE TO SCREEN 0.
*      ENDIF.
    WHEN 'GRN'.
*      IF p_status NE '99'.
*        MESSAGE 'Records status contains other than 99 . Rerun the program with status 99 to update'
*        TYPE 'I' DISPLAY LIKE 'E'.
*      ELSE.
        lobj_cust->update_20( ).
        MESSAGE 'Records has been updated successfully'
        TYPE 'I' DISPLAY LIKE 'S'.
        LEAVE TO SCREEN 0.
*      ENDIF.
    WHEN 'BACK'.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.

      LEAVE PROGRAM.

  ENDCASE.

ENDMODULE.
