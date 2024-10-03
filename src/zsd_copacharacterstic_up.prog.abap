*&---------------------------------------------------------------------*
*& Report ZSD_COPACHARACTERSTIC_UP
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created on: 07.07.2023
"Purpose: Updating Area sales Manager, Regional Sales Manager
"         Sales Officer in KES1 Tcode in Background Job Automatically
*&---------------------------------------------------------------------*
REPORT zsd_copacharacterstic_up.

INCLUDE zsd_copacharacterstic_up_cls.

INITIALIZATION.
  DATA: lo_main TYPE REF TO lcl_copachar_updation.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  "Fetching Partner Function data to Upload
  lo_main->existing_pf_data( ).
  "Get all Existing Characterstic Values
  lo_main->get_existing_values( ).
  "Data Manipulation
  lo_main->data_manipulation( ).
  "Data Updation
  lo_main->data_updation( ).
  "Alv display
  lo_main->build_alv( ).

*
