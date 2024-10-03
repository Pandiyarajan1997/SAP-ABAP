*&---------------------------------------------------------------------*
*& Report ZPP_BATCH_COSTING_EXCEL_DOWN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpp_batch_costing_excel_down.

DATA : gv_aufnr         TYPE aufnr,
       bom_cost         TYPE TABLE OF zstr_procord_batch_bom_cost,
       consumption_cost TYPE TABLE OF zstr_procord_consumption_cost,
       activity_cost    TYPE TABLE OF zstr_procord_activity_cost,
       lv_sheetname     TYPE RLGRAP-FILENAME.

PARAMETERS : p_werks TYPE marc-werks OBLIGATORY,
             p_from  TYPE datum OBLIGATORY,
             p_to    TYPE datum OBLIGATORY.
SELECT-OPTIONS so_aufnr FOR gv_aufnr.
PARAMETERS: p_file LIKE rlgrap-filename OBLIGATORY.

DATA : BEGIN OF int_head OCCURS 0,
         filed1(20) TYPE c,                     " Header Data
       END OF int_head.

DATA : BEGIN OF int_data OCCURS 0,
         field1(20) TYPE c,                     " Data
         field2(20) TYPE c,
         field3(20) TYPE c,
         field4(20) TYPE c,
       END OF int_data.


int_head-filed1 = 'Sales Ord'.
APPEND int_head.
CLEAR  int_head.

int_head-filed1 = 'Sold-to-Party'.
APPEND int_head.
CLEAR  int_head.

int_head-filed1 = 'Purchase Ord'.
APPEND int_head.
CLEAR  int_head.

int_head-filed1 = 'Ship-to-Party'.
APPEND int_head.
CLEAR  int_head.

int_data-field1 = '1JOHN'.
int_data-field2 = '2TOM'.
int_data-field3 = '3BRAD'.
int_data-field4 = '4PETER'.
APPEND int_data.
CLEAR int_data.

START-OF-SELECTION.
********************call the batch costing function module**************
  CALL FUNCTION 'ZPP_PROCORD_BATCH_BOM_COST'
    EXPORTING
      plant            = p_werks
      from_date        = p_from
      to_date          = p_to
    TABLES
      bom_cost         = bom_cost
      consumption_cost = consumption_cost
      activity_cost    = activity_cost
      aufnr            = so_aufnr
* EXCEPTIONS
*     INCORRECT_UOM    = 1
*     OTHERS           = 2
    .

  IF bom_cost IS INITIAL AND consumption_cost IS INITIAL AND activity_cost IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF bom_cost IS NOT INITIAL.
    lv_sheetname = 'BOM Cost'.
    PERFORM download TABLES bom_cost.
  ENDIF.

  IF consumption_cost IS NOT INITIAL.
    lv_sheetname = 'Consumption Cost'.
    PERFORM download TABLES consumption_cost.
  ENDIF.

  IF activity_cost IS NOT INITIAL.
    lv_sheetname = 'Activity Cost'.
    PERFORM download TABLES activity_cost.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE '
    CHANGING
      file_name     = p_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

FORM download TABLES table.

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
      file_name                 = p_file " path offile where u need to download
*     CREATE_PIVOT              = 0
      data_sheet_name           = lv_sheetname
*     PIVOT_SHEET_NAME          = ' '
*     PASSWORD                  = ' '
*     PASSWORD_OPTION           = 0
    TABLES
*     PIVOT_FIELD_TAB           =
      data_tab                  = table "internal table with data
*     fieldnames                = int_head "internal table with header
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      OTHERS                    = 9.

ENDFORM.
