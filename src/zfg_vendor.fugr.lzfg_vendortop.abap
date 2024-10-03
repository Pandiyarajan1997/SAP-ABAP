FUNCTION-POOL ZFG_VENDOR                 MESSAGE-ID SV.

* INCLUDE LZFG_VENDORD...
" Local class definition
DATA: gw_bdcdata TYPE bdcdata,
      gt_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZFG_VENDORT00                          . "view rel. data dcl.
