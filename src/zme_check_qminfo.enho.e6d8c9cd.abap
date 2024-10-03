"Name: \FU:ME_CHECK_SOURCE_OF_SUPPLY_QM\SE:END\EI
ENHANCEMENT 0 ZME_CHECK_QMINFO.
IF SY-TCODE EQ 'ME11'.

CASE sy-subrc.
    WHEN 03.
      MESSAGE E899 WITH supplier INTO gl_dummy.
      mmpur_message 'E' '06' '899' supplier '' '' ''.

  ENDCASE.
  ENDIF.
ENDENHANCEMENT.
