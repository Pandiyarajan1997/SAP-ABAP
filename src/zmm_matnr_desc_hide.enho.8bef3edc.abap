"Name: \PR:SAPLSLVC_FULLSCREEN\FO:PBO\SE:BEGIN\EI
ENHANCEMENT 0 ZMM_MATNR_DESC_HIDE.
  IF sy-tcode = 'CS11' OR
     sy-tcode = 'CS12' OR
     sy-tcode = 'CS13' OR
     sy-tcode = 'CS14' OR
     sy-tcode = 'CS15' OR
     sy-tcode = 'COOISPI'.
    SELECT SINGLE ernam FROM zmm_userid INTO @DATA(ls_userid) WHERE ernam = @sy-uname.
    IF sy-subrc <> 0.
      LOOP AT t_outtab ASSIGNING FIELD-SYMBOL(<fs_stb>).
        CASE sy-tcode.
          WHEN 'CS11'.
                       ASSIGN COMPONENT 'OJTXB' OF STRUCTURE <fs_stb> TO FIELD-SYMBOL(<fs_mat_desc>).
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
                       ASSIGN COMPONENT 'OJTXP' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN 'CS12'.
                       ASSIGN COMPONENT 'OJTXB' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
                       ASSIGN COMPONENT 'OJTXP' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN 'CS13'.
                       ASSIGN COMPONENT 'OJTXB' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
                       ASSIGN COMPONENT 'OJTXP' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN 'CS14'.
                       ASSIGN COMPONENT 'AKT_TEXT' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN 'CS15'.
                       ASSIGN COMPONENT 'OJTXB' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN 'COOISPI'.
                       ASSIGN COMPONENT 'MITXT' OF STRUCTURE <fs_stb> TO <fs_mat_desc>.
                       IF <fs_mat_desc> IS ASSIGNED.
                         CLEAR <fs_mat_desc>.
                       ENDIF.
          WHEN OTHERS.
        ENDCASE.
*        CLEAR <fs_stb>-ojtxb.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
