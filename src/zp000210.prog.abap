*----------------------------------------------------------------------*
*   INCLUDE ZP000200                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP000200 MESSAGE-ID RP.
TABLES: P0002.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0002 DEFAULT P0002.

DATA: PSAVE LIKE P0002.
