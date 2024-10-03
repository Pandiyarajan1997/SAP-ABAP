*----------------------------------------------------------------------*
* PDF Form CONVERSION FOR: HINCEPF0

* Author: G. Ashok kumar  User:C5061983
*
* Date   : 30-12-2004
*
* Program description: Modified print program for HR_IN_EPF010_99M,
* HR_IN_EPF005_99M AND HR_IN_EPF12A_99M
* form to get the display in PDF and SAP Script form.

*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Krishna & Mr.Govindarajan M        *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : HR:Employees' Provident Fund Reports  *
*& Report Name                 : ZHINCEPF0                             *
*& Development Id              : kpabap                                *
*& Related Information         : HR:Employees Provident Fund - Reports *
*&---------------------------------------------------------------------*
*  Report for the Provident Fund Calculations of India Payroll

REPORT ZHINCEPF0.

INCLUDE PCEPFIN1.             " Declaring tables and infotypes used
INCLUDE ZPCEPFIN2.             " Defines Internal Tables
INCLUDE PCREMIN6.             " Include for Indian Cluster
INCLUDE PCEPFIN6.             " Include for Selection Screen
INCLUDE PCEPFIN5.             " Include for Layouts
INCLUDE PCEPFIN3.             " Main processing
INCLUDE PCSAPIN0.             " SAP Script routines
INCLUDE ZPCEPFIN4.             " Standard routines to display results
INCLUDE PCEPFIN7.             " Subroutines to read DB tables
INCLUDE PCEPFIN8.             " Common routines required for EPF report
INCLUDE PCEPFIN9.             " Subroutines to read infotype data
