class ZCL_API_LOG_ENTRIES_STORE definition
  public
  final
  create public .

public section.

  methods LOG_ENTRY_STORE
    importing
      !APINAME type ZAPINAME
      !IJSON type /AIF/DDIC_STRUC_XML
      !OJSON type /AIF/DDIC_STRUC_XML
      !DISTRIBUTOR type KUNNR optional
      !RETAILER type KUNNR optional
    exceptions
      APINAME_MISSING
      JSON_MISSING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_LOG_ENTRIES_STORE IMPLEMENTATION.


  METHOD log_entry_store.
*Created By: Samsudeen M
*Created On: 15.06.2023
*Reference: Ramakrishnan J
*Purpose: Storing Input & Output of API in table ZAPI_LOGS
*-----------------------------------------------------------*
    DATA: lv_refno TYPE numc10.
    DATA: lv_string TYPE string.
    DATA: lv_max     TYPE tvarv_numb,
          lv_apiname TYPE zapiname.

    IF apiname IS INITIAL.
      RAISE apiname_missing.
    ELSE.
      lv_apiname = apiname.
      TRANSLATE lv_apiname TO UPPER CASE.
*check if the api name is in the tvarvc variable used in the zapi_logs_disp report
      SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc)
        WHERE name = 'APINAME_SHLP'
        AND type = 'S' AND sign = 'I' AND opti = 'EQ' AND low = @lv_apiname.
      IF sy-subrc NE 0.
*if not there then select the max number from the available ones
        CLEAR lv_max.
        SELECT MAX( numb ) FROM tvarvc INTO lv_max WHERE name = 'APINAME_SHLP'
        AND type = 'S'.
        IF sy-subrc = 0.
          lv_max = lv_max + 1.

*insert into tvarvc based on the max
          DATA(ls_tvarvcins) = VALUE tvarvc( mandt = sy-mandt
                                             name  = 'APINAME_SHLP'
                                             type  = 'S'
                                             numb  = lv_max
                                             sign  = 'I'
                                             opti = 'EQ'
                                             low    = lv_apiname ).
          INSERT tvarvc FROM ls_tvarvcins.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ijson IS INITIAL.
      RAISE json_missing.
    ENDIF.
*check the type error in ojson response
    IF ojson IS INITIAL.
      RAISE json_missing.
    ELSE.
      lv_string = ojson.
      TRANSLATE lv_string TO UPPER CASE.
      SEARCH lv_string FOR '"S"'.
      IF sy-subrc = 0.
        DATA(lv_type) = 'S'.
      ELSE.
        SEARCH lv_string FOR '"E"'.
        IF sy-subrc = 0.
          lv_type = 'E'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF lv_type IS INITIAL.
      SEARCH lv_string FOR '"ERROR"'.
      IF sy-subrc = 0.
        lv_type = 'E'.
      ELSE.
        CLEAR lv_type.
      ENDIF.

      SEARCH lv_string FOR '"ERR"'.
      IF sy-subrc = 0.
        lv_type = 'E'.
      ELSE.
        CLEAR lv_type.
      ENDIF.
    ENDIF.

    CLEAR lv_refno.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZAPI_REFID'
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
      SELECT SINGLE
          apiservice,
          classname
        FROM zapi_service INTO @DATA(lw_api_service) WHERE apiname = @apiname.
      DATA(ls_log) = VALUE zapi_logs( mandt       = sy-mandt
                                      refid       = lv_refno
                                      apiname     = apiname
                                      apiservice  = lw_api_service-apiservice
                                      classname   = lw_api_service-classname
                                      distributor = distributor
                                      retailer    = retailer
                                      erdat       = sy-datum
                                      erzet       = sy-uzeit
                                      uname       = sy-uname
                                      ijson       = ijson
                                      ojson       = ojson
                                      type        = lv_type ).
      INSERT zapi_logs FROM ls_log.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
