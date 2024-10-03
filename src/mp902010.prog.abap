*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9020                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP902000 MESSAGE-ID RP.

TABLES: P9020.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9020
                       DEFAULT P9020.

DATA: PSAVE LIKE P9020.
