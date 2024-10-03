*=============================================================================*
*  OBJECT ID              :
*  Functional spec ID     :
*  Program ID             :
*  PROGRAM TITLE          : ZAXIS_PAYMENT_EXTRACTION
*  Program Description    : AXIS Payment Extraction (H2H Extraction program)
*  PROGRAM TYPE           : Report
*  MODULE                 : FI
*  Functional Consultant  :
*  Created By             : Satyanarayana Reddy
*  Start Date             :
*  End Date               :
*  TS Ver                 : 1.0
*  Base TP requests       : IDSK900250 -->AXIS: PAYMENT EXTRACTOR BASE TP ON 30.12.2015(Transport of copies)
*
*==============================================================================*


REPORT  ZAXIS_PAYMENT_EXTRACTION_TEMP.

INCLUDE ZAXIS_PAY_EXTR_TOP_T.
*INCLUDE ZAXIS_PAY_EXTR_TOP.             " Global Declarations

INCLUDE ZAXIS_PAY_EXTR_SEL_T.
*INCLUDE ZAXIS_PAY_EXTR_SEL.             " Selection Screen & its events

INCLUDE ZAXIS_PAY_EXTR_MAIN_T.
*INCLUDE ZAXIS_PAY_EXTR_MAIN.            " Main Program / Process

INCLUDE ZAXIS_PAY_EXTR_FORM1_T.
*INCLUDE ZAXIS_PAY_EXTR_FORM1.           " ALL SELECT QUERIES to get the data

INCLUDE ZAXIS_PAY_EXTR_FORM2_T.
*INCLUDE ZAXIS_PAY_EXTR_FORM2.           " NOT AT ALL USING WE CAN DELETE THIS INCLUDE

INCLUDE ZAXIS_PAY_EXTR_FORM3_T.
*INCLUDE ZAXIS_PAY_EXTR_FORM3.           " EXTRACTION SUB FORMS

INCLUDE ZAXIS_PAY_EXTR_FORM4_T.
*INCLUDE ZAXIS_PAY_EXTR_FORM4.           " FETCH GLOBAL DATA AND LOG DETAILS ETC.

INCLUDE ZAXIS_PAY_EXTR_FORM5_T.
*INCLUDE ZAXIS_PAY_EXTR_FORM5.           " APPEND RECORDS AND DISPLAY RELATED FORMS

INCLUDE ZAXIS_PAY_FILE_DOWNLOAD_T.
*INCLUDE ZAXIS_PAY_FILE_DOWNLOAD.        " DOWNLOADING THE DATA AND WRITING LOGFILE AND UPDATING LOG TABLE

INCLUDE ZAXIS_PAY_EXTRACT_DATA_T.
*INCLUDE ZAXIS_PAY_EXTRACT_DATA.         " DATA READING FOR EXTRACTION AND EXTRACTION OF FILE




*==============================================================================*
*                     Modify Log History.
* ------------------------------------------------------------------------------
* DATE       |User ID    |TS Ver  | Transport Request| Description
* ------------------------------------------------------------------------------
* 25.04.2016 |DEVELOPER  |2.0     |IDSK900258        |ZAXIS adapter enhancements on 25.04.2016
* 25.04.2016 |DEVELOPER  |2.0     |IDSK900261        |ZAXIS : Code Enhancement on 26.04.2016
* 05.05.2016 |DEVELOPER  |2.0     |IDSK900265        |ZAXIS : Base TP creation by satya on 05.06.2016
* 05.05.2016 |DEVELOPER  |2.0     |IDSK900266(WB)    |ZAXIS : IMPS Addition to H2H Program on 05.06.2016
* 05.05.2016 |DEVELOPER  |2.0     |IDSK900269(CZ)    |ZAXIS : table entries for IMPS to H2H Program on 05.06.2016
* 19.05.2016 |DEVELOPER  |2.0     |IDSK900271(BTP)   |AXIS : IMPS ADDITION BASE TP ON 16.05.2016
* 19.05.2016 |DEVELOPER  |2.0     |IDSK900272(WB)    |ZAXIS : Base Tp Error removed on 18.05.2016
* 19.05.2016 |DEVELOPER  |2.0     |IDSK900275(CZ)    |ZAXIS : Base TP additional Table data on 19.05.2016
* 26.05.2016 |DEVELOPER  |2.0     |IDSK900281(WB)    |ZAXIS : Addings to package on 26.05.2016 (AUTO PAY adding bvtyp, Multi gl credit validation)
* 13.06.2016 |DEVELOPER  |2.0     |IDSK900284(CZ)    |ZAXIS : table data after inox for bug fix on 13.06.2016
* 13.06.2016 |DEVELOPER  |2.0     |IDSK900286(BTP    |ZAXIS : Base Tp creation for Godrej on 13.06.2016
* 14.06.2016 |DEVELOPER  |2.0     |IDSK900287(WB)    |ZAXIS : Mandatory field change on 14.06.2016
* 15.06.2016 |DEVELOPER  |2.0     |IDSK900289(CZ)    |ZAXIS : Table data for Mandatory Fields chage on 15.06.2015
* 08.07.2016 |DEVELOPER  |2.0     |IDSK900293(BTP)   |ZAXIS : Base Tp Creation for JSPL on 08.07.2016
* 14.07.2016 |DEVELOPER  |2.0     |IDSK900294(WB)    |ZAXIS : Minur Bug fixing on 14.07.2016
* 23.08.2016 |DEVELOPER  |2.0     | IDSK900318(BTP)  |ZAXIS : Base Tp Creation for N R Agarwal on 23.08.2016
* 03.09.2016 |DEVELOPER  |2.0     |IDSK900324(WB)    |ZAXIS : Field Validations edition on 03.09.2016
* 27.09.2016 |DEVELOPER  |2.0     |IDSK900331(CZ)    |ZAXIS : Table data for GL Payment Changes on 27.09.2016
* 30.09.2016 |DEVELOPER  |2.0     |IDSK900339(ER)    |ZAXIS : Base Tp Creation for Tata Sky on 30.09.2016
* 01.10.2016 |DEVELOPER  |2.0     |IDSK900340(WB)    |ZAXIS : Error Remove on Tata Base Tp on 01.10.2016
* 01.10.2016 |DEVELOPER  |2.0     |IDSK900343(BTP)   |ZAXIS : Base Tp Creation for Tata Sky on 01.10.2016
* 07.10.2016 |DEVELOPER  |2.0     |IDSK900352(WB)    |ZAXIS : Payment Method Changes on 07.10.2016
* 19.10.2016 |DEVELOPER  |2.0     |IDSK900359(WB)    |ZAXIS : Convertion Table update Program on 27.10.2016
* 07.10.2016 |DEVELOPER  |2.0     |IDSK900361(BTP)   |ZAXIS : Base TP creation for Sterling on 05.11.2016
* 07.11.2016 |DEVELOPER  |2.0     |IDSK900362(WB)    |ZAXIS : Reverse Feed program bug fixing on 07.11.2016
* 08.11.2016 |DEVELOPER  |2.0     |IDSK900367(CZ)    |ZAXIS : HB Table Maintanence on 08.11.2016
* 10.11.2016 |DEVELOPER  |2.0     |IDSK900369(CZ)    |ZAXIS : Table Data for reverse feed bug fixing on 10.11.2016
* 23.12.2016 |DEVELOPER  |2.0     |IDSK900379(BTP)   |ZAXIS : Base TP Creation for Apollo Tyres 23.12.2016
* 23.12.2016 |DEVELOPER  |2.0     |IDSK900380(WB)    |ZAXIS : Server Path Download Error Added on 23.12.2016
* 04.01.2017 |DEVELOPER  |2.0     |IDSK900383(CZ)    |ZAXIS : Table data for RTGS time limit on 04.01.2017
*
*
*
*======================================================================*
