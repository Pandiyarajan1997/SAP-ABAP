"Name: \FU:MAKT_GET_SUB\SE:END\EI
ENHANCEMENT 0 ZMM_MAT_DESC_HIDE.
*Created by: Samsudeen M
*Created on: 13.02.2023
*Purpose Hiding: Material Description
*AUTHORITY-CHECK OBJECT 'ZMAT_DESC' ID 'DISPLAY' FIELD '01'.
*IF sy-subrc <> 0.
*CLEAR: dktext,ymakt,wmakt,xmakt,wktext,yktext,xktext,dmakt.
*ENDIF.
ENDENHANCEMENT.
