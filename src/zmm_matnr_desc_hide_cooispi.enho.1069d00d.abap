"Name: \PR:SAPLCOISOUTPUT\FO:PBO_0100\SE:BEGIN\EI
ENHANCEMENT 0 ZMM_MATNR_DESC_HIDE_COOISPI.
*
DATA: lv_userid TYPE sy-uname.
FIELD-SYMBOLS: <ls_detail_list_new>  TYPE cois_s_detail_list,
               <table_new>           TYPE table.
SELECT SINGLE ernam FROM zmm_userid INTO lv_userid WHERE ernam = sy-uname.
  IF sy-subrc <> 0.
      LOOP AT gt_detail_list ASSIGNING <ls_detail_list_new>
                             WHERE NOT position IS INITIAL.
        ASSIGN <ls_detail_list_new>-table->* TO <table_new>.
        LOOP AT <table_new> ASSIGNING FIELD-SYMBOL(<fs_tb_field>).
          ASSIGN COMPONENT 'KTEXT' OF STRUCTURE <fs_tb_field> TO FIELD-SYMBOL(<fs_value>).
          IF <fs_value> IS ASSIGNED.
            CLEAR <fs_value>.
          ENDIF.
          ASSIGN COMPONENT 'MATXT' OF STRUCTURE <fs_tb_field> TO <fs_value>.
          IF <fs_value> IS ASSIGNED.
            CLEAR <fs_value>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  ENDIF.
ENDENHANCEMENT.
