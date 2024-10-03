*----------------------------------------------------------------------*
*       FORM  BILDTITEL_SETZEN                                         *
*----------------------------------------------------------------------*
*       Setzt Bildtitel fuer Listen                                    *
*----------------------------------------------------------------------*
*  -->  B01-TRVOG  Transaktionsvorgangsgruppe                          *
*  -->  B01-ANZGR  Anzeigegruppe                                       *
*  -->  B01-ZUART  Lesetiefe                                           *
*----------------------------------------------------------------------*
FORM BILDTITEL_SETZEN USING B01-TRVOG B01-ANZGR.
  CASE CARDMODE.
    WHEN 'X'.               "CCARD
      SET TITLEBAR '007' WITH B01-TRVOG B01-ANZGR.
    WHEN 'F'.               "Finanzdokumente
      SET TITLEBAR '008' .
    WHEN OTHERS.
      SET TITLEBAR '004' WITH B01-TRVOG B01-ANZGR.
  ENDCASE.
  CHECK SY-TCODE <> 'SE38'.
  SELECT SINGLE * FROM TSTCT WHERE SPRSL = SY-LANGU
                             AND   TCODE = SY-TCODE.
  IF SY-SUBRC = 0.
    SET TITLEBAR '005' WITH TSTCT-TTEXT.
  ENDIF.
ENDFORM.
