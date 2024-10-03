*----------------------------------------------------------------------*
*   INCLUDE ZP004000                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP004000 MESSAGE-ID RP.
TABLES: P0040.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0040 DEFAULT P0040.

DATA: PSAVE LIKE P0040.
