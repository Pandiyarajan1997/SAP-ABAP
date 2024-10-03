FUNCTION-POOL ZFG_CUSGRP_TAB
                   MESSAGE-ID m3.

*DATA gv_isbatch(1) TYPE c.


*TF 4.6C Materialfixierung=================================
INCLUDE <icon>.
*TF 4.6C Materialfixierung=================================

INCLUDE mmmgtrbb.
INCLUDE mmmgbbau.
*-----------------------------
INCLUDE wstr_definition. "Holds BADI global definition

INCLUDE lmgd1iv0.   "IS2ERP

"{ Begin ENHO /NFM/CA_LMGD1TOP IS-MP-NF /NFM/GENERAL }
* Global Dates nNE                                               "/NFM/
INCLUDE /nfm/global_data.                                        "/NFM/
* Constants NE                                                   "/NFM/
INCLUDE /nfm/constants.                                          "/NFM/
* Routines NE                                                    "/NFM/
INCLUDE /nfm/mgd1.                                               "/NFM/
"{ End ENHO /NFM/CA_LMGD1TOP IS-MP-NF /NFM/GENERAL }
*
INCLUDE /cwm/mgd1i01.
INCLUDE /cwm/mgd1o01.

ENHANCEMENT-POINT lmgd1top_01 SPOTS es_lmgd1top STATIC.

ENHANCEMENT-POINT ehp603_lmgd1top_01 SPOTS es_lmgd1top STATIC .


LOAD-OF-PROGRAM.
  IF 1 = 2. ENDIF.                                        "Note 2668968
