FUNCTION zattach_pdf_appl_server.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FNAME) TYPE  TEXT100
*"     REFERENCE(BELNR) TYPE  BELNR_D OPTIONAL
*"     REFERENCE(MBLNR) TYPE  BELNR_D OPTIONAL
*"     REFERENCE(MJAHR) TYPE  MJAHR
*"     REFERENCE(BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(DOCTYP) TYPE  TOAOM-AR_OBJECT
*"     REFERENCE(OBJECTTYPE) TYPE  TOAOM-SAP_OBJECT
*"  EXPORTING
*"     REFERENCE(STATUS) TYPE  FLAG
*"     REFERENCE(MSG) TYPE  STRING
*"  EXCEPTIONS
*"      FILENAME_MISSING
*"      OPENDATA_SETERROR
*"      DOCNO_MISSING
*"----------------------------------------------------------------------
*& Created By: Samsudeen M
*& Created On: 16.05.2023
*& Purpose : File Attachement in Financial Documents
*----------------------------------------------------------------------
  DATA: gx_string     TYPE xstring,
        lt_attachment TYPE STANDARD TABLE OF tbl1024.
  DATA: lv_out      TYPE i.
  DATA: lv_objectid TYPE sapb-sapobjid.
  DATA: lv_objid  TYPE sapb-sapobjid.
  DATA: lv_archiv_id  TYPE saearchivi. "Archive connection ID
  DATA: ls_toadt TYPE toadt.
  DATA: lv_archid TYPE toav0-archiv_id.
  DATA: lv_filename TYPE toaat-filename.

  IF belnr IS INITIAL AND mblnr IS INITIAL.
    RAISE docno_missing.
  ENDIF.

  IF fname IS NOT INITIAL.
    OPEN DATASET fname FOR INPUT IN BINARY MODE.
    IF sy-subrc = 0.
      CLEAR gx_string.
      READ DATASET fname INTO gx_string.
      CLOSE DATASET fname.
      CLEAR lv_out. REFRESH: lt_attachment.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = gx_string
        IMPORTING
          output_length = lv_out
        TABLES
          binary_tab    = lt_attachment.
      IF sy-subrc = 0.
        DESCRIBE TABLE lt_attachment LINES DATA(lv_lines).
        DATA(length) = CONV sapb-length( lv_lines * 1024 ).
        "Archiv Connection get
        CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
          EXPORTING
            objecttype    = objecttype
            documenttype  = doctyp
            client        = sy-mandt
          IMPORTING
            archivid      = lv_archiv_id
          EXCEPTIONS
            nothing_found = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          "Archiv Create Table function Module
          CLEAR: lv_objid,ls_toadt,lv_objectid.
          IF belnr IS NOT INITIAL.
            lv_objectid = |{ belnr }{ bukrs }{ mjahr }|.
            lv_objid = |{ bukrs }{ belnr }{ mjahr }|.
          ELSE.
            lv_objectid = |{ mblnr }{ mjahr }|.
            lv_objid = |{ mjahr }{ mblnr }|.
          ENDIF.

          CONDENSE lv_objid NO-GAPS.
          lv_filename = fname.

          CALL FUNCTION 'ARCHIV_CREATE_TABLE'
            EXPORTING
              ar_object                = doctyp
              object_id                = lv_objectid
              sap_object               = objecttype
              flength                  = length "File size
              filename                 = lv_filename
            IMPORTING
              outdoc                   = ls_toadt "Return table, decisive parameter ARC_DOC_ID
            TABLES
              "archiveobject = lt_archiveobj
              binarchivobject          = lt_attachment
            EXCEPTIONS
              error_archive            = 01
              error_communicationtable = 02
              error_connectiontable    = 03
              error_kernel             = 04
              error_parameter          = 05
              OTHERS                   = 06.
          IF sy-subrc = 0.
            CLEAR lv_archid.
            lv_archid = lv_archiv_id.
            CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
              EXPORTING
                arc_doc_id            = ls_toadt-arc_doc_id
                ar_object             = doctyp
                object_id             = lv_objid
                sap_object            = objecttype
              EXCEPTIONS
                error_connectiontable = 1
                OTHERS                = 2.
            IF sy-subrc = 0.
              status = abap_true.
              DELETE DATASET lv_filename.
              IF sy-subrc EQ 0.
                msg = |File Updated and Deleted|.
              ELSE.
                msg = |File Updated but not deleted|.
              ENDIF.
            ELSE.
              CASE sy-subrc.
                WHEN '1'.
                  msg = |Archive Insert::error_connectiontable|.
                WHEN OTHERS.
                  msg = |Archive Insert::Other Error|.
              ENDCASE.
            ENDIF.
          ELSE.
            CASE sy-subrc.
              WHEN '1'.
                msg = |Archive Create::error_archive|.
              WHEN '2'.
                msg = |Archive Create::error_communicationtable|.
              WHEN '3'.
                msg = |Archive Create::error_connectiontable|.
              WHEN '4'.
                msg = |Archive Create::error_kernel|.
              WHEN '5'.
                msg = |Archive Create::error_parameter|.
              WHEN '6'.
                msg = |Archive Create::Other Error|.
            ENDCASE.
          ENDIF.
        ELSE.
          CASE sy-subrc.
            WHEN '1'.
              msg = |Archive Connection::Nothing_found|.
            WHEN '2'.
              msg = |Archive Connection::Other Error|.
          ENDCASE.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE opendata_seterror.
    ENDIF.
  ELSE.
    RAISE filename_missing.
  ENDIF.


ENDFUNCTION.
