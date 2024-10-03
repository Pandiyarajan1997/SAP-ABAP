*&---------------------------------------------------------------------*
*& Report zmaterial_master_upload
*&---------------------------------------------------------------------*
*& Functional                  : Gopal Rajendran                      *
*& Developer                   : Zakir Hussain                         *
*& Created On                  : 12 Aug 2014                           *
*& Title                       : Material Master Upload                *
*& Report Name                 : ZMATERIAL_MASTER_UPLOAD               *
*& Development Id              : CNABAP                                *
*& Transport Request           :                                       *
*& Related Information         : Mass Material master creation by      *
*                                Excel upload                          *
*&---------------------------------------------------------------------*
REPORT zmaterial_master_upload.

INCLUDE zmaterial_upload_top.
INCLUDE zmaterial_upload_sel.
INCLUDE zmaterial_upload_class.
INCLUDE zexcel_format_multiple.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
****F4 Help for Browsing the Flat File To Upload
  lcl_material=>get_instance( )->file_name( ).

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZPF_STATUS'.

AT SELECTION-SCREEN.
  IF sy-ucomm <> 'FC01' AND p_fname IS INITIAL.
    MESSAGE 'Please Upload a Excel File' TYPE 'E'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM Excel_download USING lo_excel.
    lcl_output=>format_download( lo_excel ).
  ENDCASE.

*&---------------------------------------------------------------------*
*&    Initialization Declarations
*&---------------------------------------------------------------------*
INITIALIZATION.
*****Refreshing the Internal Tables
  REFRESH : it_mattab,it_altuom.

*&---------------------------------------------------------------------*
*&     Start-of-Selection Declarations
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_material=>get_instance( )->upload( ).
  IF it_mattab IS INITIAL.
    MESSAGE 'No Data in the upload file' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.
*****Material Master data conversion and Validate
  lcl_material=>get_instance( )->validate( ).
*****Material Master upload using BAPI
  lcl_material=>get_instance( )->update( ).

END-OF-SELECTION.
  IF lt_msg IS NOT INITIAL.
    lcl_material=>get_instance(  )->display( ).
  ENDIF.
