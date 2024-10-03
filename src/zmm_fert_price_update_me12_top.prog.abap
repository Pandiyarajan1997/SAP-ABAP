*&---------------------------------------------------------------------*
*& Include          ZMM_FERT_PRICE_UPDATE_ME12_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_TO_MR21_UPD_SEL
*&---------------------------------------------------------------------*
*TYPE-POOLS: slis.
TYPES:
  BEGIN OF gty_output,
    matnr   TYPE mbew-matnr, "Material Number
    maktx   TYPE makt-maktx, "Material Number
    werks   TYPE marc-werks, "Valuation Area
    lifnr   TYPE elifn,      "Supplier
    vprsv   TYPE mbew-vprsv, "Price Control Indicator
    verpr   TYPE mbew-verpr, "Moving Average Price
    stprs   TYPE mbew-stprs, "Standard price
    infnr   TYPE infnr,
    datbi	  TYPE kodatbi,
    datab	  TYPE kodatab,
    message TYPE text255,
  END OF gty_output,
  BEGIN OF gty_output_vk11,
    vkorg   TYPE a304-vkorg,
    vtweg   TYPE a304-vtweg, "Material Number
    matnr   TYPE a304-matnr, "Material Number
    datbi   TYPE a304-matnr, "Material Number
    datab   TYPE a304-matnr, "Material Number
    kbetr   TYPE konp-kbetr, "Material Number
    kpein   TYPE konp-kpein, "Material Number
    kmein   TYPE konp-kmein, "Material Number
    message TYPE text255,
  END OF gty_output_vk11.
DATA: gt_supplier_list TYPE STANDARD TABLE OF zmm_plant_supp.
DATA: gt_output TYPE TABLE OF gty_output.
DATA: gw_output TYPE gty_output.
DATA: gt_output_vk11 TYPE TABLE OF gty_output.
DATA: gw_output_vk11 TYPE gty_output.
DATA gv_return TYPE c.
** Internal Table for BDC **
DATA: gw_bdcdata TYPE bdcdata,
*      gt_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.

CLASS cl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.                    "cl_handler DEFINITION

CLASS cl_handler IMPLEMENTATION.
  METHOD on_double_click.
    IF column EQ 'MATNR'.
      READ TABLE gt_output INTO DATA(wa_st_data) INDEX row.

* Check that material exists
      SELECT COUNT( * ) FROM mara UP TO 1 ROWS WHERE matnr EQ wa_st_data-matnr.

      IF sy-subrc = 0. " Exists?
* Load parameters
        SET PARAMETER ID 'MXX' FIELD 'K'. " Default view
        SET PARAMETER ID 'MAT' FIELD wa_st_data-matnr. " Material number

        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ELSE. " No ?

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_st_data-matnr
          IMPORTING
            output = wa_st_data-matnr.

        DATA(lv_err) = `Material ` && wa_st_data-matnr && ` does not exist.`.
        MESSAGE lv_err TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE TEXT-002 TYPE 'I'. " Invalid cell
    ENDIF.
  ENDMETHOD.
ENDCLASS.
