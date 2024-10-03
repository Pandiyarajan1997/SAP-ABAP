*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_E01
*&---------------------------------------------------------------------*

INITIALIZATION.

  DATA lobj_mat TYPE REF TO lcl_material.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.

      CREATE OBJECT lobj_mat. "create the object for local class

      IF lobj_mat IS NOT INITIAL.

        if r1 = 'X'.

        lobj_mat->validation( ).  "call method for input field validation
        lobj_mat->fetch( ).       "call method for fetching the data
        lobj_mat->alv( ).         "call method for display main alv
        CALL SCREEN 9001.         "call the alv screen

        ELSEIF r2 = 'X'.

          lobj_mat->validation( ).  "call method for input field validation
          lobj_mat->excel( ).       "call method for excel to sap

      ENDIF.

      ENDIF.

  ENDCASE.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.       "f4 functionality to file path

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF r1 = 'X' AND screen-name = 'P_FNAME'.          "disable excel file path

      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

    IF r2 = 'X' AND screen-name CS 'SO_MATNR'.        ""disable input fields

      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

    IF r2 = 'X' AND screen-name CS 'SO_WERKS'.

      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

ENDLOOP.
