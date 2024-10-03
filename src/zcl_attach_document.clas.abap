class ZCL_ATTACH_DOCUMENT definition
  public
  final
  create public .

public section.

  methods UPLOAD_FILE
    importing
      value(BUKRS) type BUKRS optional
      value(GJAHR) type GJAHR
      value(BELNR) type BELNR_D optional
      value(MBLNR) type MBLNR optional
      value(OBJECTTYPE) type TOAOM-SAP_OBJECT default 'BKPF'
      value(DOCUMENTTYPE) type TOAOM-AR_OBJECT default 'ZVENINV'
      value(FNAME) type STRING
    exporting
      !UPLOAD_STATUS type FLAG
    exceptions
      ERROR_ARCHIVE
      ERROR_COMMUNICATIONTABLE
      ERROR_CONNECTIONTABLE
      ERROR_KERNEL
      ERROR_PARAMETER
      DOCUMENT_NUMBER_NOT_FOUND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ATTACH_DOCUMENT IMPLEMENTATION.


  METHOD upload_file.
    DATA: lv_len        TYPE i,
          lt_content    TYPE soli_tab,
          lt_attachment TYPE STANDARD TABLE OF tbl1024.
*          length TYPE num12.
* value( OBJECTTYPE ) Values for INVOICE - 'BKPF' MIGO - 'BUS2017' MIRO - 'BUS2081'
* value( DOCUMENTTYPE ) for   INVOICE - 'ZVENINV' MIGO - 'ZGR' MIRO - 'ZVENINV'
    DATA: lv_objectid TYPE sapb-sapobjid.
    DATA: lv_objid  TYPE sapb-sapobjid.
    DATA: lv_archiv_id  TYPE saearchivi. "Archive connection ID
    DATA: ls_toadt TYPE toadt.
    DATA: lv_archid TYPE toav0-archiv_id.
    DATA: lv_filename TYPE toaat-filename,
          l_descr     TYPE toaat-descr.

*    DATA: lt_binar TYPE STANDARD TABLE OF tbl1024.

    DATA: lv_xstring TYPE xstring.
    IF bukrs IS INITIAL AND mblnr IS INITIAL.
      RAISE document_number_not_found.
    ENDIF.
    DATA: lv_out TYPE i.
    CLEAR lt_content.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename   = fname
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      TABLES
        data_tab   = lt_content.
    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_len
*         FIRST_LINE   = 0
*         LAST_LINE    = 0
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = lt_content.
      IF sy-subrc = 0.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_out
          TABLES
            binary_tab    = lt_attachment.
        IF sy-subrc = 0.
          DESCRIBE TABLE lt_attachment LINES DATA(lv_lines).
          DATA(length) = CONV sapb-length( lv_lines * 1024 ).
        ENDIF.
      ENDIF.
    ENDIF.

    "Archiv Connection get
    CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
      EXPORTING
        objecttype    = objecttype
        documenttype  = documenttype
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
        lv_objectid = |{ belnr }{ bukrs }{ gjahr }|.
        lv_objid = |{ bukrs }{ belnr }{ gjahr }|.
        DATA(invocie_no) =  belnr.
      ELSE.
        lv_objectid = |{ mblnr }{ gjahr }|.
        lv_objid = |{ gjahr }{ mblnr }|.
        invocie_no =  mblnr.
      ENDIF.

      CONDENSE lv_objid NO-GAPS.
*      lv_filename = fname.
      l_descr = invocie_no.
      lv_filename = |{ l_descr }.pdf|.

      CALL FUNCTION 'ARCHIV_CREATE_TABLE'
        EXPORTING
          ar_object                = documenttype
          object_id                = lv_objectid
          sap_object               = objecttype
          flength                  = length "File size
          filename                 = lv_filename
          descr                    = l_descr
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
            ar_object             = documenttype
            object_id             = lv_objid
            sap_object            = objecttype
          EXCEPTIONS
            error_connectiontable = 1
            OTHERS                = 2.
        IF sy-subrc = 0.
          upload_status = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
