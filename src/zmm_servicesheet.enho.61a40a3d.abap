"Name: \PR:SAPLMLSR\FO:ACCEPT_FLAG\SE:END\EI
ENHANCEMENT 0 ZMM_SERVICESHEET.
** Pruefung/Warnung Storno
if not storno is initial.
  peRFORM CHECK_COMPRESS.
  if betz-remng ne 0.
    message E388(11).
  endif.
endif.
ENDENHANCEMENT.
