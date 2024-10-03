*&---------------------------------------------------------------------*
*& Include          ZHR_POSITION_CREATION_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv,
         sel          TYPE char1,
         bp           TYPE char1,
         orgunit_des  TYPE hrp1000-stext, "Orgunit Des
         repmng_pdes  TYPE hrp1000-stext, "Reporting Manager Position Desc
         repmng_name  TYPE p0001-ename,  "Reporting Manager Name
         job_des      TYPE hrp1000-stext, "Job Desc
         bpcreate(01) TYPE c,
         bpextend(01) TYPE c,
         bprel(01)    TYPE c,
         ccrel(01)    TYPE c,
*         latitude    TYPE zhrlatit,
*         longitude   TYPE zhrlong,
*         tr_allow    TYPE zhr_travel,
*         tr_cat      TYPE zhr_cat,
         frompos      TYPE pa0001-plans,
         status_desc  TYPE char25,
         message      TYPE char120.
         INCLUDE STRUCTURE zhr_position_tab.
TYPES:   END OF ty_alv.

TYPES: BEGIN OF ty_name,         "Structure For Reporting Manager Name
         pernr TYPE p0001-pernr,
         plans TYPE p0001-plans,
         kostl TYPE p0001-kostl,
         ename TYPE p0001-ename,
       END OF ty_name.
** Orgunit ID checks **
TYPES: BEGIN OF ty_hrp1001,
         objid TYPE objid,
       END OF ty_hrp1001.
** Position Creation structure **
TYPES: BEGIN OF ty_create_object,
         objid TYPE hrp1000-objid,
         begda TYPE hrp1000-begda,
         endda TYPE hrp1000-endda,
         short TYPE hrp1000-short,
         stext TYPE hrp1000-stext,
         sobid TYPE hrp1001-sobid,  "Orgunit
         jobid TYPE hrp1001-sobid,  "JObcode
         mgrid TYPE hrp1001-sobid,  "Manager
       END OF ty_create_object.
** Approve Screen Output Display  **
TYPES: BEGIN OF ty_object_out,
         objid TYPE hrp1001-objid,
         begda TYPE hrp1000-begda,
         endda TYPE hrp1000-endda,
         short TYPE hrp1000-short,
         stext TYPE hrp1000-stext,
         sobid TYPE hrp1001-sobid,
         indic TYPE char1,
         p1000 TYPE char255,
         p1001 TYPE char255,
       END OF ty_object_out.

DATA: BEGIN OF dum.
        INCLUDE STRUCTURE objec.
DATA: END   OF dum.

TYPES: BEGIN OF gty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF gty_fields.

TYPES: BEGIN OF ty_chg_rep,
         sel          TYPE char1,
         pernr        TYPE p0001-pernr,
         name         TYPE p0001-ename,
         position     TYPE hrp1001-sobid,
         posdess      TYPE hrp1000-short,
         posdesl      TYPE hrp1000-stext,
         repmng       TYPE p0001-pernr,
         rmname       TYPE p0001-ename,
         rmpos        TYPE hrp1001-sobid,
         rmposdes     TYPE hrp1000-stext,
         date         TYPE hrp1000-begda,
         newrep       TYPE p0001-pernr,
         newrepname   TYPE p0001-ename,
         newrepos     TYPE hrp1001-sobid,
         message(150) TYPE c,
       END OF ty_chg_rep.
DATA: gt_chg_rep TYPE TABLE OF ty_chg_rep,
      gs_chg_rep TYPE ty_chg_rep.

TYPES: BEGIN OF ty_pernr,
         pernr TYPE pernr_d,
       END OF ty_pernr.
DATA: gt_pernr TYPE TABLE OF ty_pernr.
CONSTANTS: gc_otype_o TYPE hrp1000-otype VALUE 'O',
           gc_otype_s TYPE hrp1000-otype VALUE 'S',
           gc_otype_c TYPE hrp1000-otype VALUE 'C',
           gc_otype_k TYPE hrp1000-otype VALUE 'K'.

CONSTANTS: gc_plan_ver TYPE hrp1000-plvar VALUE '01'.

CONSTANTS: gc_rsign_a TYPE hrp1001-rsign VALUE 'A',
           gc_rsign_b TYPE hrp1001-rsign VALUE 'B'.

CONSTANTS: gc_relat_002 TYPE hrp1001-relat VALUE '002',
           gc_relat_003 TYPE hrp1001-relat VALUE '003',
           gc_relat_007 TYPE hrp1001-relat VALUE '007',
           gc_relat_011 TYPE hrp1001-relat VALUE '011'.

CONSTANTS: gc_high_date TYPE hrp1000-endda VALUE '99991231'.


** Internal Table Declaration **
DATA: ls_object_op TYPE ty_create_object. "Structure for Position Create

DATA: lv_obj_fetch TYPE c.

DATA: gt_output_op TYPE TABLE OF ty_object_out, "Alv Display for Approve output
      gs_output_op TYPE ty_object_out.

DATA: gt_display TYPE TABLE OF ty_alv,
      gs_display TYPE ty_alv.

DATA: gt_name TYPE TABLE OF ty_name,
      gs_name TYPE ty_name.

DATA: gt_orgunit TYPE TABLE OF ty_hrp1001,
      gs_orgunit TYPE ty_hrp1001.

DATA: gt_jobid TYPE TABLE OF ty_hrp1001,
      gs_jobid TYPE ty_hrp1001.

DATA: gt_position TYPE STANDARD TABLE OF hrp1000,
      gs_position TYPE hrp1000.

* Internal Table and Work Area Declaration for FieldCatlog
DATA :gt_fcat    TYPE lvc_t_fcat,
      gs_fcat    TYPE lvc_s_fcat,
      gt_exclude TYPE ui_functions,
      gs_excl    TYPE ui_func,
      gs_layout  TYPE lvc_s_layo.

CLASS lcl_handle_events DEFINITION DEFERRED.

** ALV Containers and Events **
DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_container.

DATA: gt_pos_tab TYPE STANDARD TABLE OF zhr_position_tab,
      gs_pos_tab TYPE zhr_position_tab.

DATA: lv_refno TYPE num06.
DATA: gt_fields TYPE STANDARD TABLE OF gty_fields,
      gs_field  TYPE gty_fields.
********** Internal Table for ALV **********************
DATA: gt_fcat1   TYPE lvc_t_fcat,
      gs_fcat1   TYPE lvc_s_fcat,
      gs_layout1 TYPE lvc_s_layo.
DATA: lo_grid1 TYPE REF TO cl_salv_table.

DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
** Status Description **
DATA: lt_domain TYPE TABLE OF dd07v,
      ls_domain TYPE dd07v.
** Excluding Unwanted UI function **
DATA ls_exclude TYPE ui_func.
DATA: lt_exclude TYPE ui_functions.
*** Mail Data Declarations
DATA: lt_attachment TYPE TABLE OF solisti1,
      ls_attachment LIKE LINE OF lt_attachment.

DATA: lv_data TYPE string.
DATA: lv_xstring TYPE xstring.
DATA: lt_bin TYPE solix_tab.
DATA : lv_sub TYPE sood-objdes.
DATA: it_body_msg   TYPE STANDARD TABLE OF solisti1.
DATA: salutation TYPE string.
DATA: body TYPE string.
DATA: footer TYPE string.
DATA: lv_date(10) TYPE c.
DATA: lv_date1(10) TYPE c.
DATA: lo_send_request TYPE REF TO cl_bcs,
      lo_document     TYPE REF TO cl_document_bcs,
      lo_sender       TYPE REF TO if_sender_bcs,
      sender_mail     TYPE  adr6-smtp_addr,
      lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
      lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
      lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
      lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
      lx_document_bcs TYPE REF TO cx_document_bcs,
      lv_sent_to_all  TYPE os_boolean.

DATA: lv_body(300) TYPE c.
DATA: lv_tot   TYPE char50.
DATA: lt_mail TYPE TABLE OF zhr_hiring_email,
      ls_mail TYPE zhr_hiring_email.

DATA: ls_tvarvc TYPE tvarvc.
DATA: it_mailid  TYPE TABLE OF ad_smtpadr,
      it_mailid1 TYPE TABLE OF ad_smtpadr,
      in_mailid  TYPE ad_smtpadr,
      in_mailid1 TYPE ad_smtpadr,
      in_mailid2 TYPE ad_smtpadr,
      in_mailid3 TYPE ad_smtpadr.

DATA: lv_posname TYPE hr_mcstext.
DATA: lv_orgname TYPE hr_mcstext,
      lv_jobname TYPE hrp1000-stext.
DATA: lv_mgrname TYPE p0001-ename.
DATA: wa_bdcdata TYPE bdcdata,
      it_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gs_bdcmsg  TYPE bdcmsgcoll.
DATA ls_stylerow TYPE lvc_s_styl .
DATA lt_styletab TYPE lvc_t_styl .

** Editable Fields for Propose Screen  **
CLASS      lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

    METHODS:
      handle_on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no
                  er_event_data.

ENDCLASS.
