FUNCTION-POOL ZFG_CUSTOMER_DAT           MESSAGE-ID SV.

* INCLUDE LZFG_CUSTOMER_DATD...              " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZFG_CUSTOMER_DATT00                    . "view rel. data dcl.

  DATA: cusname TYPE name1_gp.
