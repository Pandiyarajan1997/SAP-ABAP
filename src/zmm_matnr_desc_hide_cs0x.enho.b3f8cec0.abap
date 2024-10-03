"Name: \PR:SAPLCSDI\FO:RC29P_KTEXT_SET\SE:END\EI
ENHANCEMENT 0 ZMM_MATNR_DESC_HIDE_CS0X.
    DATA ls_userid TYPE sy-uname.
    IF sy-tcode EQ 'CS03'.
    SELECT SINGLE ernam FROM zmm_userid INTO ls_userid WHERE ernam = sy-uname.
    IF sy-subrc <> 0.
      CLEAR RC29P-KTEXT.
    ENDIF.
    ENDIF.
ENDENHANCEMENT.
