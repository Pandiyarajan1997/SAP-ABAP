* modulpool infotype 9011
PROGRAM MP901100 MESSAGE-ID 5A.
INCLUDE MPH5ATOP.                      "header
TABLES: WPLOG,
        PPPAR, PPHDR, PPHDX, PPSEL, PPENQ,
        T777O, T777P, T777S, T777T,
        P1000, P1001, P9011.
INCLUDE MPHCOM00.                      "common areas
INCLUDE FHVTAB00.                      "update tables
INCLUDE FHVIEW00.                      "USER-VIEW
INCLUDE MPHFCOD0.                      "function codes
INCLUDE MPHDAT00.                      "general data
INCLUDE MPHPBO00.                      "PBO modules
INCLUDE MPHPAI00.                      "PAI modules
INCLUDE MP901120.                      "specific PAI/PBO modules
*include mpxxxxbi.                      "Batch-Input von der WPLOG
