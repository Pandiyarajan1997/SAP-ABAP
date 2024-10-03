*&---------------------------------------------------------------------*
*& Include          ZMM_FERT_PRICE_UPDATE_ME12_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_TO_MR21_UPD_SEL
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_list,
         fcode TYPE char6,
       END OF ty_list.

TYPES: BEGIN OF ty_tab,
         mark(1) TYPE c,
         kunnr   TYPE bsad-kunnr,        "Customer
         wrbtr   TYPE bsad-wrbtr,        "Amount
         belnr   TYPE belnr_d,         "Bill/DR memo
       END OF ty_tab.

TYPES: BEGIN OF ty_header,
         bldat     TYPE bkpf-bldat,       "Document Date
         budat     TYPE bkpf-budat,       "Posting Date
         blart     TYPE bkpf-blart,       "Document type
         xblnr     TYPE bkpf-xblnr,       "Reference
         xtext     TYPE bseg-sgtxt,       "added on 12/3
         newko     TYPE saknr , "rf05a-newko,      "Bank G/L
         bukrs     TYPE bkpf-bukrs,       "Company code
         waers     TYPE bkpf-waers,       "Currency
         gjahr     TYPE bkpf-gjahr,       "Fiscal year
         monat     TYPE bkpf-monat,       "period
         bktxt     LIKE bkpf-bktxt,       "doc text
         augtx     TYPE rf05a-augtx,      "doc clearing text
         valut(10) TYPE c,                "value date
         gldesc    TYPE txt50_skat,              " GL Desc
         distb     TYPE kunnr,
         gsber     TYPE gsber,
       END OF ty_header.

TYPES: BEGIN OF ty_message,
         kunnr        TYPE bsad-kunnr,        "Customer
         name1        TYPE name1_gp,          " Customer description
         wrbtr        TYPE bsad-wrbtr,        "Amount
         belnr        TYPE belnr_d,         "Bill/DR memo
         mess_type(1) TYPE c,
         message      TYPE bapi_msg,
         status(4)    TYPE c,
       END OF ty_message.

TYPES: BEGIN OF ty_openitems.
         INCLUDE  STRUCTURE bapi3007_2.
TYPES:   due_date        TYPE netdt,
         due_cashd1      TYPE sk1dt,       "due date for cash discount1
         due_cashd2      TYPE sk2dt,       " due date for cash discount2
         previous(1)     TYPE c,
         root_doc        TYPE belnr_d,
         root_year       TYPE gjahr,
         root_item       TYPE buzei,
         rootdocyear(17) TYPE c,
         docyear(17)     TYPE c,
         cpudt           TYPE cpudt,                "Entry Date
         cputm           TYPE cputm,                "Entry time
         cnote(1)        TYPE c,                 " credit note process required
       END OF ty_openitems.

TYPES: BEGIN OF ty_amnt,
         root_doc        TYPE belnr_d,
         root_year       TYPE gjahr,
         root_item       TYPE buzei,
         net_amnt        TYPE wrbtr,
         root_amnt       TYPE wrbtr,
         due_date        TYPE netdt,
         disc            TYPE dzproz,
         disc_amnt       TYPE wrbtr,
         count_s         TYPE i,
         count_h         TYPE i,
         count           TYPE i,
         prev(1)         TYPE c,
         rootdocyear(17) TYPE c,
         docyear(17)     TYPE c,
         cpudt           TYPE cpudt,                "Entry Date
         cputm           TYPE cputm,                "Entry time
         cnote(1)        TYPE c,  " credit note process required
         proc(1)         TYPE c,    " process in FB05.
       END OF ty_amnt.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
       END OF ty_kna1.

TYPES: BEGIN OF ty_bdc,
         fnam1 TYPE fnam_____4,
         fval1 TYPE bdc_fval,
         fnam2 TYPE fnam_____4,
         fval2 TYPE bdc_fval,
       END OF ty_bdc.

TYPES: BEGIN OF ty_cusven,
         kunnr TYPE kunnr,
         lifnr TYPE lifnr,
       END OF ty_cusven.

DATA: gs_header TYPE ty_header,
      gt_tab    TYPE STANDARD TABLE OF ty_tab,
      gs_tab    TYPE ty_tab,
      gt_kna1   TYPE STANDARD TABLE OF ty_kna1,
      gs_kna1   TYPE ty_kna1.

DATA: gs_bapireturn TYPE bapireturn,
      gt_lineitems  TYPE STANDARD TABLE OF bapi3007_2,       " line items for customer
      gs_lineitems  TYPE bapi3007_2,                         " line items for customer
      gt_lineitemsv TYPE STANDARD TABLE OF bapi3008_2,     " line items for vendor
      gs_lineitemsv TYPE bapi3008_2,                        " line items for vendor
      gt_openitems  TYPE STANDARD TABLE OF ty_openitems,
      gs_openitems  TYPE ty_openitems,
      gs_openitems1 TYPE ty_openitems,
      gt_openitemsc TYPE STANDARD TABLE OF ty_openitems,  " table for credit notes
      gt_amnt       TYPE STANDARD TABLE OF ty_amnt,
      gs_amnt       TYPE ty_amnt,
      gt_message    TYPE STANDARD TABLE OF ty_message,
      gs_message    TYPE ty_message,
      gt_list       TYPE STANDARD TABLE OF ty_list,
      gs_list       TYPE ty_list,
      gt_cusven     TYPE STANDARD TABLE OF ty_cusven,               " customer who is also a vendor
      gs_cusven     TYPE ty_cusven,
      gs_faede      TYPE faede,
      gt_bkpf       TYPE STANDARD TABLE OF bkpf,
      gs_bkpf       TYPE bkpf,
      gt_bdc        TYPE STANDARD TABLE OF ty_bdc,
      gt_bdcc       TYPE STANDARD TABLE OF ty_bdc,
      gs_bdc        TYPE ty_bdc,
      gs_bdcc       TYPE ty_bdc.

DATA: check(1)      TYPE c,
      gv_partial(1) TYPE c,       " partial case scenario in fb05
      gv_excess(1)  TYPE c,       " excess case scenario in fb05
      gv_exact(1)   TYPE c,       " exact case scenario in fb05
      gv_sclear(1)  TYPE c,        " Self clear.
      gv_vclear(1)  TYPE c,        " vendor as a customer clear.
      gv_nclear(1)  TYPE c,        " note/excess/un-addressed partial docs  Clear.
*      gv_resid(1) TYPE c,       " residual and amnt to be posted less than net amnt.
      gv_wrbtr      TYPE bsad-wrbtr,
      gv_wrbtr1     TYPE bsad-wrbtr,
      gv_wrbtrc     TYPE bsad-wrbtr, "credit note amount.
      gv_wrbtro     TYPE bsad-wrbtr, "open items net amnt
      gv_pwrbtr     TYPE bsad-wrbtr, "print amnt for credit cnote payment
      gv_value      LIKE ftpost-fval,
      gv_mode       TYPE apqi-putactive VALUE 'N',
      gv_class      TYPE xuclass,          " user group
      gd_percent    TYPE i,
      g_doc_no      TYPE belnr_d,
      mcobjekt      LIKE dd23l-mconame,      " Matchcodeobjekt
      f4rc          LIKE sy-subrc,          " Return-Code
      shlp_mf05a    TYPE shlp_descr_t,
      shlpname      LIKE ddshdescr-shlpname,
      interface     LIKE ddshiface,
      selstr        LIKE rf05a-newko,        " Suchstring fr MC
      f4dyn         LIKE sy-dynnr.          " Dynpronummer

CONSTANTS: gc_p TYPE char1 VALUE 'P',     " Previous Partial payment done
           gc_r TYPE char1 VALUE 'R',     " previous residual payment done
           gc_n TYPE char1 VALUE 'N',     " No previous payment done
           gc_c TYPE char1 VALUE 'C',     " Credit note
           gc_k TYPE char1 VALUE 'K'.     " KR doc for customer also a vendor(similar to Credit note)

*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA: messtab       LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      gs_messtab    TYPE bdcmsgcoll,
      return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

DATA:    BEGIN OF f4hlp OCCURS 1.
           INCLUDE STRUCTURE dynpread.
DATA:    END OF f4hlp.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_9000' ITSELF
CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_9000'
DATA: g_tc_9000_lines LIKE sy-loopc,
      waa             LIKE LINE OF tc_9000-cols.

DATA: gv_container TYPE REF TO cl_gui_custom_container,
      gv_alv       TYPE REF TO cl_gui_alv_grid,
      gt_fcat      TYPE lvc_t_fcat,
      gs_fcat      TYPE  lvc_s_fcat.

**TYPE-POOLS: slis.
*TYPES:
*  BEGIN OF gty_output,
*    matnr   TYPE mbew-matnr, "Material Number
*    maktx   TYPE makt-maktx, "Material Number
*    werks   TYPE marc-werks, "Valuation Area
*    lifnr   TYPE elifn,      "Supplier
*    vprsv   TYPE mbew-vprsv, "Price Control Indicator
*    verpr   TYPE mbew-verpr, "Moving Average Price
*    stprs   TYPE mbew-stprs, "Standard price
*    infnr   TYPE infnr,
*    datbi    TYPE kodatbi,
*    datab    TYPE kodatab,
*    message TYPE text255,
*  END OF gty_output.
*DATA: gt_supplier_list TYPE STANDARD TABLE OF zmm_plant_supp.
*DATA: gt_output TYPE TABLE OF gty_output.
*DATA: gw_output TYPE gty_output.
*DATA gv_return TYPE c.
*** Internal Table for BDC **
*DATA: gw_bdcdata TYPE bdcdata,
**      gt_bdcdata TYPE STANDARD TABLE OF bdcdata,
*      gt_bdcdata  TYPE TABLE OF bdcdata WITH EMPTY KEY,
*      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
*      gw_bdcmsg  TYPE bdcmsgcoll.
*
*CLASS cl_handler DEFINITION.
*  PUBLIC SECTION.
*    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
*      IMPORTING row column.
*ENDCLASS.                    "cl_handler DEFINITION
*
*CLASS cl_handler IMPLEMENTATION.
*  METHOD on_double_click.
*    IF column EQ 'MATNR'.
*      READ TABLE gt_output INTO DATA(wa_st_data) INDEX row.
*
** Check that material exists
*      SELECT COUNT( * ) FROM mara UP TO 1 ROWS WHERE matnr EQ wa_st_data-matnr.
*
*      IF sy-subrc = 0. " Exists?
** Load parameters
*        SET PARAMETER ID 'MXX' FIELD 'K'. " Default view
*        SET PARAMETER ID 'MAT' FIELD wa_st_data-matnr. " Material number
*
*        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*      ELSE. " No ?
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_st_data-matnr
*          IMPORTING
*            output = wa_st_data-matnr.
*
*        DATA(lv_err) = `Material ` && wa_st_data-matnr && ` does not exist.`.
*        MESSAGE lv_err TYPE 'I' DISPLAY LIKE 'E'.
*      ENDIF.
*    ELSE.
*      MESSAGE TEXT-002 TYPE 'I'. " Invalid cell
*    ENDIF.
*  ENDMETHOD.
*ENDCLASS.
