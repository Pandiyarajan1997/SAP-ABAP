class ZCL_CLEARING_VENDOR_INV_RH definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CLEARING_VENDOR_INV_RH IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.
*CALL METHOD SUPER->IF_REST_APPLICATION~GET_ROOT_HANDLER
*  RECEIVING
*    RO_ROOT_HANDLER =
*    .
    DATA(io_router) = NEW cl_rest_router( ).

    io_router->attach(
      EXPORTING
       iv_template = '/clearing'
       iv_handler_class = 'ZCL_CLEARING_VENDOR_INV_RP'
      ).

    ro_root_handler = io_router.


  ENDMETHOD.
ENDCLASS.
