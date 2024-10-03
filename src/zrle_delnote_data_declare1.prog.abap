*----------------------------------------------------------------------*
*   INCLUDE RLE_DELNOTE_DATA_DECLARE                                   *
*----------------------------------------------------------------------*

INCLUDE RVADTABL.

DATA:   XSCREEN(1)   TYPE C.                "Output on printer or screen

* current language for read buffered.
DATA: GF_LANGUAGE LIKE SY-LANGU.
