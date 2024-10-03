*----------------------------------------------------------------------*
***INCLUDE LZFG_PO_MAPPINGF04.
*----------------------------------------------------------------------*
FORM before_save.
  DATA: l_msg TYPE string.
  FIELD-SYMBOLS: <fs_field> TYPE any .
  LOOP AT total.
    CHECK <action> EQ aendern.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      DATA(l_rec_plant) = CONV werks_d( <fs_field> ).
    ENDIF.

    ASSIGN COMPONENT 'IDENT' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      IF <fs_field> = 'I'.
        ASSIGN COMPONENT 'RESWK' OF STRUCTURE <vim_total_struc> TO <fs_field>.
        IF sy-subrc = 0.
          IF <fs_field> IS INITIAL.
            CLEAR l_msg.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = 'ZMM_POERROR'
                msgnr               = '006'
              IMPORTING
                message_text_output = l_msg.
            MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
          ELSE.
            DATA(l_supp_plant) = CONV werks_d( <fs_field> ).
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <vim_total_struc> TO <fs_field>.
        IF sy-subrc = 0.
          IF <fs_field> IS NOT INITIAL.
            CLEAR l_msg.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = 'ZMM_POERROR'
                msgnr               = '007'
                msgv1               = 'I'
              IMPORTING
                message_text_output = l_msg.
            MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ELSEIF <fs_field> = 'E'.
        ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <vim_total_struc> TO <fs_field>.
        IF sy-subrc = 0.
          IF <fs_field> IS INITIAL.
            CLEAR l_msg.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = 'ZMM_POERROR'
                msgnr               = '008'
              IMPORTING
                message_text_output = l_msg.
            MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
          ELSE.
            DATA(l_vendor) = CONV lifnr( <fs_field> ).
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT 'RESWK' OF STRUCTURE <vim_total_struc> TO <fs_field>.
        IF sy-subrc = 0.
          IF <fs_field> IS NOT INITIAL.
            CLEAR l_msg.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = 'ZMM_POERROR'
                msgnr               = '009'
                msgv1               = 'E'
              IMPORTING
                message_text_output = l_msg.
            MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE regio FROM t001w
      INTO @DATA(l_rec_reg)
      WHERE werks = @l_rec_plant.
*Document type while Changing
    ASSIGN COMPONENT 'BSART' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      IF l_supp_plant IS NOT INITIAL.
        SELECT SINGLE regio FROM t001w
          INTO @DATA(l_supp_reg)
          WHERE werks = @l_supp_plant.
        IF l_supp_reg = l_rec_reg.
          <fs_field> = 'UB'.
        ELSE.
          <fs_field> = 'ZUB'.
        ENDIF.
      ELSEIF l_vendor IS NOT INITIAL.
        SELECT SINGLE regio FROM lfa1
                 INTO @DATA(l_ven_reg)
                 WHERE lifnr = @l_vendor.
        IF l_ven_reg = l_rec_reg.
          <fs_field> = 'ZNB'.
        ENDIF.
      ENDIF.
    ENDIF.

** -- Updated By
    ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      <fs_field> = sy-datum.
    ENDIF.
** -- Updated On
    ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      <fs_field> = sy-uname.
    ENDIF.
    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc EQ 0.
      extract = total.
      MODIFY extract INDEX sy-tabix.
    ENDIF.
    IF total IS NOT INITIAL.
      MODIFY total.
    ENDIF.
  ENDLOOP.
ENDFORM.
