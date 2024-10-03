*----------------------------------------------------------------------*
*   INCLUDE ZP002100                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP002100 MESSAGE-ID RP.
TABLES: P0021.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0021 DEFAULT P0021.

DATA: PSAVE LIKE P0021.
