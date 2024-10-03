*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9021                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9021 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE P9021 OUTPUT.
  IF PSYST-NSELC EQ YES.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    IF PSYST-IINIT = YES AND PSYST-IOPER = INSERT.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.
  ENDIF.
ENDMODULE.
*----------------------------------------------------------------------*
*       MODULE  P9021L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE P9021L OUTPUT.
* PERFORM RExxxx.
ENDMODULE.
