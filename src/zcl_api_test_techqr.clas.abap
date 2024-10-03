class ZCL_API_TEST_TECHQR definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_TEST_TECHQR IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Pandiarajan
*Createed On: 27.09.2023
*Purpose : test api for techservice einvoice qr
*---------------------------------------------------------------*

    DATA: gs_response TYPE zeinv_response_st.
    DATA  gt_response  TYPE TABLE OF zeinv_response_st.
    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.

    DATA : lv_res TYPE zqr_demo.

    lr_request = server->request.
    lr_response = server->response.

* Check the Calling Method
    CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data).
    IF lv_data IS NOT INITIAL.

*      lv_res-p_no = 2.
*      lv_res-response = lv_data.
*
*      MODIFY zqr_demo FROM lv_res.

** Deserialize the input our required input ***

      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data
       pretty_name  = /ui2/cl_json=>pretty_mode-user
       assoc_arrays = abap_true
      CHANGING
       data         = gt_response ).
    ENDIF.

    IF gt_response[] IS NOT INITIAL.


    ENDIF.

  ENDMETHOD.
ENDCLASS.
