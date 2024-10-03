class ZCL_API_DMS_ERROR_LOG_ENTRIES definition
  public
  final
  create public .

public section.

  methods LOG_ENTRY_STORE
    importing
      !TYPE type ZTYPE optional
      !STATUS type ZSTATUSS optional
      !DMS_ORDERID type ZORDER_ID optional
      !DISTRIBUTOR type KUNNR optional
      !PLANT type WERKS_D optional
      !DEALER type KUNNR optional
      !MSG type STRING optional
      !MATERIAL type MATNR optional
    exceptions
      DMS_ORDERID_MISSING
      DISTRIBUTOR_MISSING
      PLANT_MISSING
      DEALER_MISSING
      MSG_MISSING
      TYPE_MISSING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_ERROR_LOG_ENTRIES IMPLEMENTATION.


  METHOD log_entry_store.
*Created By: Pandiarajan
*Created On: 25.12.2023
*Reference: Ramakrishnan J
*Purpose: Storing error logs for dms application - zdms_com_err_log
*-----------------------------------------------------------*
    DATA: lv_refno TYPE zdms_refid.
    DATA: lv_string TYPE string.

    CLEAR lv_refno.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZDMS_REFID'
      IMPORTING
        number                  = lv_refno
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
      DATA(ls_log) = VALUE zdms_com_err_log( mandt       = sy-mandt
                                             refid       = lv_refno
                                             type        = type
                                             status      = '10'
                                             dms_orderid = dms_orderid
                                             distributor = distributor
                                             plant       = plant
                                             dealer      = dealer
                                             Material    = Material
                                             msg         = msg
                                             erdat       = sy-datum
                                             ernam       = sy-uname
                                             erzet       = sy-uzeit ).
      MODIFY zdms_com_err_log FROM ls_log.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
