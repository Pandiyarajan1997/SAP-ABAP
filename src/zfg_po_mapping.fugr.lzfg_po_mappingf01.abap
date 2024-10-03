*----------------------------------------------------------------------*
***INCLUDE LZFG_PO_MAPPINGF01.
*----------------------------------------------------------------------*
*Added by: Samsudeen M
*Added on: 22.05.2023
*Purpose: While Storing Entries some Checks
*************************************************************************
FORM new_entry.
  DATA: l_msg TYPE string.
  CONSTANTS: c_znb TYPE bsart VALUE 'ZNB'.
*Recieving Plant Existence Check
  IF zmm_po_mapping-werks IS NOT INITIAL.
    SELECT SINGLE werks FROM t001w
      INTO @DATA(l_werks)
      WHERE werks = @zmm_po_mapping-werks.
    IF sy-subrc NE 0.
      CLEAR l_msg.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZMM_POERROR'
          msgnr               = '000'
          msgv1               = zmm_po_mapping-werks
        IMPORTING
          message_text_output = l_msg.
      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
*Material Code Validation
  IF zmm_po_mapping-matnr IS NOT INITIAL.
    SELECT SINGLE matnr FROM mara
      INTO @DATA(l_matnr)
      WHERE matnr = @zmm_po_mapping-matnr.
    IF sy-subrc NE 0.
      CLEAR l_msg.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZMM_POERROR'
          msgnr               = '001'
          msgv1               = zmm_po_mapping-matnr
        IMPORTING
          message_text_output = l_msg.
      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
*Material Extension Checks
  IF zmm_po_mapping-werks IS NOT INITIAL AND zmm_po_mapping-matnr IS NOT INITIAL.
    SELECT SINGLE matnr FROM marc
      INTO @DATA(l_matnr_ext)
      WHERE matnr = @zmm_po_mapping-matnr
      AND werks = @zmm_po_mapping-werks.
    IF sy-subrc NE 0.
      CLEAR l_msg.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZMM_POERROR'
          msgnr               = '002'
          msgv1               = zmm_po_mapping-matnr
          msgv2               = zmm_po_mapping-werks
        IMPORTING
          message_text_output = l_msg.
      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
*Supplying Plant Code Existence
  IF zmm_po_mapping-reswk IS NOT INITIAL.
    SELECT SINGLE werks FROM t001w
      INTO @DATA(l_supp_plant)
      WHERE werks = @zmm_po_mapping-reswk.
    IF sy-subrc NE 0.
      CLEAR l_msg.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZMM_POERROR'
          msgnr               = '003'
          msgv1               = zmm_po_mapping-reswk
        IMPORTING
          message_text_output = l_msg.
      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
*Supplier Code Validation
  IF zmm_po_mapping-lifnr IS NOT INITIAL.
    SELECT SINGLE lifnr FROM lfa1
      INTO @DATA(l_vendor)
      WHERE lifnr = @zmm_po_mapping-lifnr.
    IF sy-subrc NE 0.
      CLEAR l_msg.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZMM_POERROR'
          msgnr               = '004'
          msgv1               = zmm_po_mapping-lifnr
        IMPORTING
          message_text_output = l_msg.
      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.

*If Internal Procurement Means
  IF zmm_po_mapping-ident EQ 'I' AND zmm_po_mapping-reswk IS INITIAL.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZMM_POERROR'
        msgnr               = '006'
      IMPORTING
        message_text_output = l_msg.
    MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

*Identification 'I' Means Supplier Code is not allowed.
  IF zmm_po_mapping-ident EQ 'I' AND zmm_po_mapping-lifnr IS NOT INITIAL.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZMM_POERROR'
        msgnr               = '007'
        msgv1               = zmm_po_mapping-ident
      IMPORTING
        message_text_output = l_msg.
    MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
*If External Procurement Means
  IF zmm_po_mapping-ident EQ 'E' AND zmm_po_mapping-lifnr IS INITIAL.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZMM_POERROR'
        msgnr               = '008'
      IMPORTING
        message_text_output = l_msg.
    MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
*Identification 'E' Means Supplying Plant is not allowed.
  IF zmm_po_mapping-ident EQ 'E' AND zmm_po_mapping-reswk IS NOT INITIAL.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZMM_POERROR'
        msgnr               = '009'
        msgv1               = zmm_po_mapping-ident
      IMPORTING
        message_text_output = l_msg.
    MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

  "Recieving Plant Region
  SELECT SINGLE regio FROM t001w
  INTO @DATA(l_rec_reg)
  WHERE werks = @zmm_po_mapping-werks.

*Based On region of recieving Plant and Supplying Plant or supplier PO document type fixing
  IF zmm_po_mapping-ident EQ 'I'.
*Document Type Fixing
    SELECT SINGLE regio FROM t001w
      INTO @DATA(l_supp_reg)
      WHERE werks = @zmm_po_mapping-reswk.
    IF l_rec_reg = l_supp_reg.
      zmm_po_mapping-bsart = 'UB'.
    ELSE.
      zmm_po_mapping-bsart = 'ZUB'.
    ENDIF.
  ENDIF.

  IF zmm_po_mapping-ident EQ 'E'.
*Document Type Fixing
    zmm_po_mapping-bsart = c_znb.
  ENDIF.

  zmm_po_mapping-erdat = sy-datum.
  zmm_po_mapping-ernam = sy-uname.
  zmm_po_mapping-erzet = sy-uzeit.

ENDFORM.
