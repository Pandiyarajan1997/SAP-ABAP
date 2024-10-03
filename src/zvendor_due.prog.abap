*&---------------------------------------------------------------------*
*& Report  ZVENDOR_DUE
*&
*&---------------------------------------------------------------------*
*&-----------------------  CREATED    ---------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  : 13 May 2014                           *
*& Title                       : Vendor Wise- Age Wise- Bill Wise Report
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Report Name                 : ZVENDOR_DUE                           *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : ALV Hierarchical Display For          *
*&                               Vendor Wise - Age Wise -              *
*&                               BillWise Report                       *
*&---------------------------------------------------------------------*

REPORT ZVENDOR_DUE MESSAGE-ID FR.
INCLUDE ZVENDOR_DUE_TOP.
INCLUDE ZVENDOR_DUE_SUB.

AT SELECTION-SCREEN.
  PERFORM DATA_VALIDITY.

START-OF-SELECTION.
  PERFORM DATA_FETCH.
  PERFORM FIELDCAT.
***************Created By Prabhu on 01.04.2020 Starts
***  PERFORM SORT.
***************Created By Prabhu on 01.04.2020 Ends
  PERFORM DISPLAY.
END-OF-SELECTION.
