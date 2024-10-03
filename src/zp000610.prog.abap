*----------------------------------------------------------------------*
*   INCLUDE ZP000600                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP000600 MESSAGE-ID RP.
TABLES: P0006.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0006 DEFAULT P0006.

DATA: PSAVE LIKE P0006.
