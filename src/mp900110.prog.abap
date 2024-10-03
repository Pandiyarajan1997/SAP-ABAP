*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9001                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP900100 MESSAGE-ID RP.

TABLES: P9001.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9001
                       DEFAULT P9001.

DATA: PSAVE LIKE P9001.
