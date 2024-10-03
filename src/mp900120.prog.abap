*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9001                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9001 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE P9001 OUTPUT.
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
*       MODULE  P9001L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE P9001L OUTPUT.
* PERFORM RExxxx.
ENDMODULE.
