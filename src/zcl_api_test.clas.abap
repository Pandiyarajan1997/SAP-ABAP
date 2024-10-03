class ZCL_API_TEST definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_TEST IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Samsudeen M
*Createed On: 27.07.2023
*Purpose : Posting the GL Account entries from MIS
*---------------------------------------------------------------*
    DATA: gs_response TYPE zeinv_error_st.
    DATA: gs_response1 TYPE zeinv_resp_success_st.
    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.
    lr_request = server->request.
    lr_response = server->response.
* Check the Calling Method
    IF lr_request->get_method( ) EQ 'POST'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data).
      IF lv_data IS NOT INITIAL.
        SPLIT lv_data AT ',' INTO DATA(lv_succ) DATA(lv_resp).
        REPLACE ALL OCCURRENCES OF '"' IN lv_succ WITH ''.
        SPLIT lv_succ AT ':' INTO DATA(lv_text) DATA(lv_type).
        CONDENSE lv_type NO-GAPS.
      ENDIF.
      CLEAR: gs_response,gs_response1.
      IF lv_type = 'N'.
** Deserialize the input our required input ***
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_data
         pretty_name  = /ui2/cl_json=>pretty_mode-user
         assoc_arrays = abap_true
        CHANGING
         data         = gs_response ).

      ELSEIF lv_type = 'Y'.
** Deserialize the input our required input ***

        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_data
         pretty_name  = /ui2/cl_json=>pretty_mode-user
         assoc_arrays = abap_true
        CHANGING
         data         = gs_response1 ).
      ENDIF.
    ENDIF.

    IF gs_response IS NOT INITIAL.
    ELSEIF gs_response1 IS NOT INITIAL.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
