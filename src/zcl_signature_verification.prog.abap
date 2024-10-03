*&---------------------------------------------------------------------*
*& Report zcl_signature_verification
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_signature_verification.
CLASS zcl_signature_verification DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      verify_signature
        IMPORTING
          iv_payload     TYPE string
          iv_sig_header  TYPE string
          iv_secret      TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool
          RAISING cx_sy_conversion_error,
      compute_signature
        IMPORTING
          iv_message TYPE string
          iv_key     TYPE string
        RETURNING
          VALUE(rv_signature) TYPE string.
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
          lt_hmac_bytes TYPE xstring.

    " Assuming a custom function module for HMAC SHA-256 calculation
    CL_ABAP_HMAC=>calculate_hmac_for_char(
      EXPORTING
        if_algorithm     = 'SHA256'
        if_key           = lt_hmac_bytes
        if_data          = iv_message
*        if_length        = 0
*      IMPORTING
*        ef_hmacstring    =
*        ef_hmacxstring   =
*        ef_hmacb64string =
    ).
*    CATCH cx_abap_message_digest.
*    CALL FUNCTION 'Z_HMAC_SHA256'
*      EXPORTING
*        iv_key    = iv_key
*        iv_message = iv_message
*      IMPORTING
*        ev_hmac   = lt_hmac_bytes.

    CALL FUNCTION 'HR_RU_CONVERT_HEX_TO_STRING'
      EXPORTING
        xstring = lt_hmac_bytes
      IMPORTING
        cstring = lv_hex
      .

    rv_signature = lv_hex.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lv_payload     TYPE string VALUE 'PAYLOAD',
        lv_sig_header  TYPE string VALUE 't=12345,SIGNATURE',
        lv_secret      TYPE string VALUE 'SECRET',
        lv_valid       TYPE abap_bool.

  lv_valid = zcl_signature_verification=>verify_signature(
    iv_payload = lv_payload
    iv_sig_header = lv_sig_header
    iv_secret = lv_secret ).

  WRITE: / 'Signature valid:', lv_valid.
