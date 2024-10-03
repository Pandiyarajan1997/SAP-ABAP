*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9021                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP902100 MESSAGE-ID RP.

TABLES: P9021.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9021
                       DEFAULT P9021.

DATA: PSAVE LIKE P9021.
