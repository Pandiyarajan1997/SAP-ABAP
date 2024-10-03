*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9022                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP902200 MESSAGE-ID RP.

TABLES: P9022.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9022
                       DEFAULT P9022.

DATA: PSAVE LIKE P9022.
