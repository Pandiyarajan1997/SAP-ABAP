class ZCL_CLEARING_VENDOR_INV_RP definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CLEARING_VENDOR_INV_RP IMPLEMENTATION.


  METHOD if_rest_resource~post.
*CALL METHOD SUPER->IF_REST_RESOURCE~POST
*  EXPORTING
*    IO_ENTITY =
*    .
    TYPES: BEGIN OF ty_input,
             ccode             TYPE string,
             vendor            TYPE string,
             org_inv           TYPE string,
             amt               TYPE string,
             inv_refno         TYPE string,
             payment_reference TYPE string,
             assignment        TYPE string,
           END OF ty_input.
    DATA: gw_input TYPE ty_input.
    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA(lo_entity) = mo_request->get_entity( ).
    DATA(lo_response) = mo_response->create_entity( ).
*** Get String Data from header ***
    DATA(lv_data) = lo_entity->get_string_data( ).

    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.

** Serialize input to JSON text ***
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = lv_data
        pretty_name = /ui2/cl_json=>pretty_mode-user
      RECEIVING
        r_json      = DATA(lv_response).
    REPLACE ALL OCCURRENCES OF '\'  IN lv_response WITH ''.
    REPLACE ALL OCCURRENCES OF '/'  IN lv_response WITH ''.
    SHIFT lv_response LEFT DELETING LEADING '"'.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_response
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = gw_input ).

    CLEAR:  lv_message, lv_subrc.
    IF gw_input IS INITIAL.
      CLEAR lv_message.
      lv_message = 'Missing Input Parameters'.
      lv_subrc = '4'.
    ELSE.
      IF gw_input-ccode IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Company Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-vendor IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Vendor Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-org_inv IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Original invoice Document Number Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-inv_refno IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Invoice Reference No Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-amt IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Amount Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-payment_reference IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Payment Reference is Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-assignment IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Assignment is Mandatory'.
        lv_subrc = '4'.
      ENDIF.
    ENDIF.
** If error arise stop the request ***
    IF lv_message IS NOT INITIAL.
      CLEAR lv_body.
      lv_body = | { '{' } { '"Error":' } { '"' } { lv_message } { '"' } { '}' } |.
      CLEAR v_jsonload.
      v_jsonload = | { '[' } { lv_body } { ']' } |.

      lo_response->set_header_field(
       EXPORTING
         iv_name = 'Content-Type'
         iv_value = 'application/json; charset=UTF-8'
         ).
      lo_response->set_string_data( iv_data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
