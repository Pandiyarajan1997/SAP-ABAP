*&---------------------------------------------------------------------*
*&  Include           ZHSBC_PAYMENT_FTP
*&---------------------------------------------------------------------*
form ftp_connect.

* data : "temp TYPE xstring VALUE '73616d706c65',
*       uri TYPE string VALUE 'ecom-sftp.fguk-pprd.hsbc.com/'..
*
*
*data : lo_client TYPE REF TO IF_HTTP_CLIENT,
*       lo_req    TYPE REF TO IF_HTTP_REQUEST.
*
*data: it_formulario type TIHTTPNVP,
*          wa_formulario like line of it_formulario.
*
*CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_DESTINATION
*  EXPORTING
*    DESTINATION              = 'HSBC SIS'
*  IMPORTING
*    CLIENT                   =  lo_client
**  EXCEPTIONS
**    ARGUMENT_NOT_FOUND       = 1
**    DESTINATION_NOT_FOUND    = 2
**    DESTINATION_NO_AUTHORITY = 3
**    PLUGIN_NOT_ACTIVE        = 4
**    INTERNAL_ERROR           = 5
**    OTHERS                   = 6
*        .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*
*
*
*CALL METHOD CL_HTTP_UTILITY=>SET_REQUEST_URI
*  EXPORTING
*    REQUEST = lo_client->REQUEST
*    URI     = uri
*    .
*
*CALL METHOD lo_client->REQUEST->SET_HEADER_FIELD
*  EXPORTING
*    NAME   = '~request_method'
*    VALUE  = 'PUT'
*    .
*
*
*CALL METHOD lo_client->REQUEST->SET_HEADER_FIELD
*  EXPORTING
*    NAME   = '~server_protocal'
*    VALUE  = 'HTTP/1.0'
*    .
*
*CALL METHOD lo_client->REQUEST->SET_HEADER_FIELD
*  EXPORTING
*    NAME   = 'Content-Type'
*    value = 'text/xml; charset=utf-8'.
*
*
*wa_formulario-name  = 'file'.
*
*data: temp123 TYPE string.
*
*CONCATENATE FILE_NAME_XML '.xml' INTO temp123.
*condense temp123.
*
*    wa_formulario-value = temp123.
*    append wa_formulario to it_formulario.
*
*
*    lo_client->request->set_form_fields( FIELDS = it_formulario ).
*
*CALL METHOD lo_client->REQUEST->SET_DATA
*  EXPORTING
*    DATA               = Xfile
**    OFFSET             = 0
**    LENGTH             = -1
**    VSCAN_SCAN_ALWAYS  = SPACE
**    VIRUS_SCAN_PROFILE = '/SIHTTP/HTTP_DOWNLOAD'
*    .
*
*CALL METHOD LO_CLIENT->SEND
**  EXPORTING
**    TIMEOUT                    = CO_TIMEOUT_DEFAULT
**  EXCEPTIONS
**    HTTP_COMMUNICATION_FAILURE = 1
**    HTTP_INVALID_STATE         = 2
**    HTTP_PROCESSING_FAILED     = 3
**    HTTP_INVALID_TIMEOUT       = 4
**    OTHERS                     = 5
*        .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*
*CALL METHOD LO_CLIENT->CLOSE
**  EXCEPTIONS
**    HTTP_INVALID_STATE = 1
**    OTHERS             = 2
*        .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
























endform.
