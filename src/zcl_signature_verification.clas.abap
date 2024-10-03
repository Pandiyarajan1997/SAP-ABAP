CLASS zcl_signature_verification DEFINITION
  PUBLIC
  FINAL .

   PUBLIC SECTION.
    CLASS-METHODS:
      verify_signature
        IMPORTING
          iv_payload     TYPE string
          iv_sig_header  TYPE string
          iv_secret      TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool
          RAISING cx_sy_conversion_error ,
      compute_signature
        IMPORTING
          iv_message TYPE string
          iv_key     TYPE string
        RETURNING
          VALUE(rv_signature) TYPE string
        RAISING
          cx_abap_message_digest.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_signature_verification IMPLEMENTATION.
  METHOD verify_signature.
    DATA: lv_timestamp       TYPE i,
          lv_expected_sig    TYPE string,
          lv_signed_payload  TYPE string,
          lv_computed_sig    TYPE string,
          lt_split           TYPE TABLE OF string,
          lv_sig_part        TYPE string,
          lv_part            TYPE string,
          lv_part_len        TYPE i.

    TRY.
        SPLIT iv_sig_header AT ',' INTO TABLE lt_split.

        READ TABLE lt_split INTO lv_part INDEX 1.
        lv_part_len = STRLEN( lv_part ).
        data(lv_part_l) = lv_part_len - 2.
        lv_timestamp = lv_part+2(lv_part_l).

        READ TABLE lt_split INTO lv_part INDEX 2.
        lv_part_len = STRLEN( lv_part ).
        data(lv_part_2) = lv_part_len - 2.
        lv_expected_sig = lv_part+2(lv_part_2).
      CATCH cx_sy_itab_line_not_found
                cx_sy_conversion_error INTO DATA(gx_conversion_error).
        data(lo_text) = gx_conversion_error->get_text( ).
        MESSAGE lo_text TYPE 'E'.
    ENDTRY.

    lv_signed_payload = |{ lv_timestamp }.{ iv_payload }|.

    TRY.
        lv_computed_sig = compute_signature( iv_message = lv_signed_payload iv_key = iv_secret ).
      CATCH cx_root INTO DATA(lx_exception).
      data(go_text) = lx_exception->get_text( ).
     MESSAGE go_text TYPE 'E'.
**Verifying SHA256 data = Secret key
    rv_valid = lv_expected_sig = lv_computed_sig.
     ENDTRY.

 lv_signed_payload = |{ lv_timestamp }.{ iv_payload }|.

    TRY.
        lv_computed_sig = compute_signature( iv_message = lv_signed_payload iv_key = iv_secret ).
      CATCH cx_root INTO DATA(gx_exception).
*        RAISE EXCEPTION TYPE cx_root
*          EXPORTING textid = lx_exception->get_text( ).
        data(zo_text) = gx_exception->get_text( ).
     MESSAGE zo_text TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD compute_signature.
    DATA: lv_hmac       TYPE string,
          lv_hex        TYPE string,
          lv_str TYPE string,
          lv_xstr TYPE xstring,
          lt_hmac_bytes TYPE xstring,
          lv_len type i.

    " Assuming a custom function module for HMAC SHA-256 calculation
    DATA(lv_binary_secret) = cl_abap_hmac=>string_to_xstring( iv_key ).
    CL_ABAP_HMAC=>calculate_hmac_for_char(
      EXPORTING
        if_algorithm     = 'SHA256'
        if_key           = lv_binary_secret
        if_data          = iv_message
*        if_length        = 0
      IMPORTING
        ef_hmacstring    = lv_str
        ef_hmacxstring   = lv_xstr
        ef_hmacb64string = lv_hmac
    ).

    CL_ABAP_MESSAGE_DIGEST=>calculate_hash_for_char(
      EXPORTING
        if_algorithm     = 'SHA256'
        if_data          = iv_key
*        if_length        = 0
      IMPORTING
        ef_hashstring    = lv_hex
*        ef_hashxstring   =
*        ef_hashb64string =
*        ef_hashx         =
    ).
*    CATCH cx_abap_message_digest.
    CL_HTTP_UTILITY=>decode_base64( encoded = lv_hex ).

        call FUNCTION 'SCMS_BASE64_DECODE_STR'
          EXPORTING
            input    = lv_hmac
*            unescape = 'X'
          IMPORTING
            output   = lv_xstr .

CL_BCS_CONVERT=>xstring_to_string(
    EXPORTING
      iv_xstr   = lv_xstr
      iv_cp     =  1100                " SAP character set identification
    RECEIVING
      rv_string = DATA(lv_string)
  ).

*    CALL FUNCTION 'HR_RU_CONVERT_HEX_TO_STRING'
*      EXPORTING
*        xstring = lv_xstr
*      IMPORTING
*        cstring = lv_hex
*      .

    rv_signature = lv_string.
  ENDMETHOD.
ENDCLASS.
