*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZMGD1TOP.                         " Global Declarations
  INCLUDE LZMGD1UXX.                         " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZMGD1F...                         " Subroutines
* INCLUDE LZMGD1O...                         " PBO-Modules
* INCLUDE LZMGD1I...                         " PAI-Modules
* INCLUDE LZMGD1E...                         " Events
* INCLUDE LZMGD1P...                         " Local class implement.
* INCLUDE LZMGD1T99.                         " ABAP Unit tests

* DIESES INCLUDE NICHT MEHR AENDERN!                                 *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                    *

*------------------------------------------------------------------
*           PBO-Module
*------------------------------------------------------------------
INCLUDE LMGD1OXX.     "zentrale PBO-Module Bildbausteine
INCLUDE LMGD1O01.     "PBO-Module für Kurztexthandling
INCLUDE LMGD1O02.     "PBO-Module für Steuerhandling
INCLUDE LMGD1O03.     "PBO-Module für Verbrauchswerte
INCLUDE LMGD1O04.     "PBO-Mdoule Mengeneinheiten
INCLUDE LMGD1O05.     "PBO-Module für Prognosewerte
INCLUDE LMGD1O06.     "PBO-Module für EAN
INCLUDE LMGD1O07.     "PBO-Module für Langtexte
INCLUDE LMGD1O08.     "PBO-Module für Table-Control Steuerung
INCLUDE LMGD1O1K.     "PBO-Modul für Klassif.-Subscreen
*------------------------------------------------------------------
*           PAI-Module
*------------------------------------------------------------------
INCLUDE LMGD1IXX.     "zentrale PAI-Module Bildbausteine
INCLUDE LMGD1IYY.     "Gemeinsame PAI-Module Bildbaustein/Trägerprogramm
INCLUDE LMGD1I01.     "Prüfmodule Datenbilder  MARA, MAKT (Kopfbaustein)
INCLUDE LMGD1I02.     "Prüfmodule Datenbilder  MARC, MARD, MPGD
INCLUDE LMGD1I03.     "Prüfmodule Datenbilder  QM-Daten (MARA/MARC)
INCLUDE LMGD1I04.     "Prüfmodule Datenbilder  MBEW
INCLUDE LMGD1I05.     "Prüfmodule Datenbilder  MFHM
INCLUDE LMGD1I06.     "Prüfmodule Datenbilder  MLGN, MLGT
INCLUDE LMGD1I07.     "Prüfmodule Datenbilder  MPOP
INCLUDE LMGD1I08.     "Prüfmodule Datenbilder  MVKE
INCLUDE LMGD1I09.     "Prüfmodule für Kurztexthandling
INCLUDE LMGD1I10.     "PAI-Module für Steuerhandling
INCLUDE LMGD1I11.     "PAI-Module für Verbrauchswerte
INCLUDE LMGD1I12.     "PAI-Module Mengeneinheiten
INCLUDE LMGD1I13.     "PAI-Module für Prognosewerte
INCLUDE LMGD1I14.     "PAI-Module EAN
INCLUDE LMGD1I15.     "PAI-Module für Langtexte
INCLUDE LMGD1I17.     "PAI-Module für TC-Steuerung
INCLUDE LMGD1I7O.     "PAI-Module für Klassif.-Subscreen
INCLUDE LMGD1IHX.     "Eingabehilfen Bildbausteine
*------------------------------------------------------------------
*
*           FORM-Routinen
*
*------------------------------------------------------------------
INCLUDE LMGD1FXX.        "zentrale Formroutinen Bildbausteine
INCLUDE LMGD1FYY.        "Gemeinsame Form-Routinen Bildbaustein/Tägerpr.
INCLUDE LMGD1FSC.        "zentrale Blätterroutinen   Bildbausteine
INCLUDE LMGD1F01.        "Form-Routinen Kurztexthandling
INCLUDE LMGD1F02.        "Form-Routinen Steuerhandling
INCLUDE LMGD1F03.        "Form-Routinen I Verbrauchswerte/Prognosewerte
INCLUDE LMGD1F06.        "Form-Routinen II Verbrauchswerte/Prognosewerte
INCLUDE LMGD1F04.        "Form-Routinen Mengeneinheiten
INCLUDE LMGD1F05.        "Form-Routinen EAN

*
INCLUDE LMGD1FHX.       "spezielle Eingabehilfen Bildbausteine
INCLUDE LMGMMFHX.       "allg. Routinen Eingabehilfen
* generierte Form-Routinen für Bildbausteine
 INCLUDE MMMGXGUW.        "Holen der Daten auf den Bildbaustein
 INCLUDE MMMGXSUW.        "Übergeben der Daten vom Bildbaustein
 INCLUDE MMMGXRBD.        "Zus. Vorschlagshandling before  Dialog
 INCLUDE MMMGXRAD.        "Zus. Vorschlagshandling after   Dialog

INCLUDE LMGD1I7K.

*INCLUDE LMGD1F2F.

INCLUDE LMGD1O1J.

INCLUDE LMGD1I7Q.

INCLUDE LMGD1OV0.

INCLUDE LMGD1I7T.

* DIESES INCLUDE NICHT MEHR AENDERN!                                 *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                    *

INCLUDE lmgd1o20.

INCLUDE lmgd1o21.

INCLUDE lmgd1o22.

*INCLUDE lmgd1i63.

INCLUDE lzmgd1o01.
