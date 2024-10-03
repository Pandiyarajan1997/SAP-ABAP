*----------------------------------------------------------------------*
*   INCLUDE ZP018500                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP018500 MESSAGE-ID RP.
TABLES: P0185.

*tables: ZPLISmmmm

FIELD-SYMBOLS: <PNNNN> STRUCTURE P0185 DEFAULT P0185.

DATA: PSAVE LIKE P0185.
