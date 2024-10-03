*&---------------------------------------------------------------------*
*& Report ZFI_GL_BANK_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_GL_BANK_REPORT.


TYPE-POOLS: slis.

TABLES: bsis,bkpf.

TYPES: BEGIN OF ty_final,
       belnr TYPE bsis-belnr,
       budat TYPE bsis-budat,
       blart TYPE bsis-blart,
       hkont TYPE bsis-hkont,
       gldesc TYPE char255,   "GL DESCRIPTION
       gsber TYPE tgsbt-gsber, "Business Area
       gtext TYPE tgsbt-gtext, "Business Area Description
       zuonr TYPE bseg-zuonr,  "ASSIGNMENT
       dmbtr TYPE bsis-dmbtr,
       sgtxt   TYPE bsis-sgtxt,  "NARRATION
***********TO GIVE LINE COLOR ****************
       line_color(4) TYPE c,
*********************************************
*       DMBTR TYPE BSIS-DMBTR,

**********************  Added Doc Header Text Riyas
          BKTXT type BKPF-BKTXT,
*******************************

       END OF ty_final.

TYPES: BEGIN OF ty_bsis,
       bukrs TYPE bsis-bukrs,
       hkont TYPE bsis-hkont,
       gjahr TYPE bsis-gjahr,
       belnr TYPE bsis-belnr,
       budat TYPE bsis-budat,
       blart TYPE bsis-blart,
       bschl TYPE bsis-bschl,
       gsber TYPE bsis-gsber,
       dmbtr TYPE bsis-dmbtr,
       shkzg TYPE bsis-shkzg,
       sgtxt TYPE bsis-sgtxt,
       END OF ty_bsis.

TYPES: BEGIN OF ty_bsid,
       bukrs TYPE bsid-bukrs,
       kunnr TYPE bsid-kunnr,
       gjahr TYPE bsid-gjahr,
       belnr TYPE bsid-belnr,
       budat TYPE bsid-budat,
       blart TYPE bsid-blart,
       shkzg TYPE bsid-shkzg,
       gsber TYPE bsid-gsber,
       dmbtr TYPE bsid-dmbtr,
       hkont TYPE bsid-hkont,
       END OF ty_bsid.

TYPES: BEGIN OF ty_bsik,
       bukrs TYPE bsik-bukrs,
       lifnr TYPE bsik-lifnr,
       gjahr TYPE bsik-gjahr,
       belnr TYPE bsik-belnr,
       budat TYPE bsis-budat,
       blart TYPE bsid-blart,
       shkzg TYPE bsik-shkzg,
       gsber TYPE bsik-gsber,
       dmbtr TYPE bsik-dmbtr,
       hkont TYPE bsik-hkont,
       END OF ty_bsik.

TYPES: BEGIN OF ty_bsis1,
       bukrs TYPE bsis-bukrs,
       hkont TYPE bsis-hkont,
       gjahr TYPE bsis-gjahr,
       belnr TYPE bsis-belnr,
       budat TYPE bsis-budat,
       blart TYPE bsid-blart,
       bschl TYPE bsis-bschl,
       gsber TYPE bsis-gsber,
       dmbtr TYPE bsis-dmbtr,
       shkzg TYPE bsis-shkzg,
       sgtxt TYPE bsis-sgtxt,
       END OF ty_bsis1.

TYPES: BEGIN OF ty_bseg,
       bukrs TYPE bseg-bukrs,
       hkont TYPE bseg-hkont,
       gjahr TYPE bseg-gjahr,
       belnr TYPE bseg-belnr,
       augbl TYPE bseg-augbl,
       bschl TYPE bseg-bschl,
       gsber TYPE bseg-gsber,
       dmbtr TYPE bseg-dmbtr,
       shkzg TYPE bseg-shkzg,
       zuonr TYPE bseg-zuonr,
       sgtxt TYPE bseg-sgtxt,
       lifnr TYPE bseg-lifnr,
       kunnr TYPE bseg-kunnr ,
       END OF ty_bseg.

TYPES: BEGIN OF ty_lfa1,
       lifnr TYPE lfa1-lifnr,
       name1 TYPE lfa1-name1,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_kna1,
       kunnr TYPE kna1-lifnr,
       name1 TYPE kna1-name1,
       END OF ty_kna1.

TYPES: BEGIN OF ty_skat,
       saknr TYPE skat-saknr,
       txt20 TYPE skat-txt50,
       END OF ty_skat.

TYPES: BEGIN OF ty_head,
       bukrs TYPE bsik-bukrs,
       gsber TYPE bsik-gsber,
       END OF ty_head.

TYPES: BEGIN OF ty_dat,
       f_dat TYPE bsis-bldat,
       t_dat TYPE bsis-bldat,
       END OF ty_dat.

TYPES: BEGIN OF ty_tgsbt,
       gsber TYPE tgsbt-gsber,
       gtext TYPE tgsbt-gtext,
       END OF ty_tgsbt.

TYPES: BEGIN OF ty_data,
       hkont TYPE bsis-hkont,
       gjahr TYPE bsis-gjahr,
       belnr TYPE bsis-belnr,
       shkzg TYPE bsis-shkzg,
       dmbtr TYPE bsis-dmbtr,
       monat TYPE bsis-monat,
       END OF ty_data.

TYPES: BEGIN OF ty_opbal,
       opbal TYPE bsis-dmbtr,
       END OF ty_opbal.

*************************   Referen text added from Bkpf Riyas
           TYPES: BEGIN OF TY_BKPF,
bukrs TYPE bkpf-bukrs,
belnr TYPE bkpf-belnr,
GJAHR TYPE BKPF-GJAHR,
BLART TYPE BKPF-BLART,           " Document Type
BLDAT TYPE BKPF-BLDAT,           " Document Date in Document
budat TYPE bkpf-budat,           " Posting Date in the Document
XBLNR TYPE BKPF-XBLNR,           " Reference Document Number
BKTXT TYPE BKPF-BKTXT,
END OF TY_BKPF.

DATA: it_bkpf TYPE TABLE OF ty_bkpf,
      wa_bkpf TYPE ty_bkpf.

**********************


DATA: it_debit TYPE TABLE OF ty_final,
      wa_debit TYPE ty_final,
      it_credit TYPE TABLE OF ty_final,
      wa_credit TYPE ty_final,
      it_bsis TYPE TABLE OF ty_bsis,
      wa_bsis TYPE ty_bsis,
      it_bsis1 TYPE TABLE OF ty_bsis1,
      wa_bsis1 TYPE ty_bsis1,
      it_bsis2 TYPE TABLE OF ty_bsis,
      wa_bsis2 TYPE ty_bsis,
      it_bsis3 TYPE TABLE OF ty_bsis,
      wa_bsis3 TYPE ty_bsis,
      it_bseg TYPE TABLE OF ty_bseg,
      wa_bseg TYPE ty_bseg,
      it_bseg1 TYPE TABLE OF ty_bseg,
      wa_bseg1 TYPE ty_bseg.

DATA: it_opbal TYPE TABLE OF ty_opbal,
      wa_opbal TYPE ty_opbal.

DATA: it_bsid TYPE TABLE OF ty_bsid,
      wa_bsid TYPE ty_bsid,
      it_bsik TYPE TABLE OF ty_bsik,
      wa_bsik TYPE ty_bsik.

DATA: it_dat TYPE TABLE OF ty_dat,
      wa_dat TYPE ty_dat,
      it_dat1 TYPE TABLE OF ty_dat,
      wa_dat1 TYPE ty_dat.


DATA: it_data TYPE TABLE OF ty_data,
      wa_data TYPE ty_data.



DATA: it_skat TYPE TABLE OF ty_skat,
      wa_skat TYPE ty_skat.

DATA: it_skat1 TYPE TABLE OF ty_skat,
      wa_skat1 TYPE ty_skat.

DATA: it_lfa1 TYPE TABLE OF ty_lfa1,
      wa_lfa1 TYPE ty_lfa1,
      it_kna1 TYPE TABLE OF ty_kna1,
      wa_kna1 TYPE ty_kna1.

DATA: it_tgsbt TYPE TABLE OF ty_tgsbt,
      wa_tgsbt TYPE ty_tgsbt.


DATA: it_alv TYPE lvc_t_fcat,
      wa_alv TYPE lvc_s_fcat,
      it_lay TYPE TABLE OF lvc_s_layo,
      wa_lay TYPE lvc_s_layo,
      wa_lay1 TYPE lvc_s_layo.

DATA: wa_title TYPE lvc_title,
      wa_title1 TYPE lvc_title,
      wa_title2 TYPE lvc_title.
*
DATA: it_alv1 TYPE lvc_t_fcat,
      wa_alv1 TYPE lvc_s_fcat.

DATA: gs_variant TYPE disvariant.
*



CLASS: lcl_event_reciever DEFINITION DEFERRED.

DATA: r_grid1 TYPE REF TO cl_gui_alv_grid,
      r_grid2 TYPE REF TO cl_gui_alv_grid,
      g_container1 TYPE REF TO cl_gui_custom_container,
      g_container2 TYPE REF TO cl_gui_custom_container,
      o_dyndoc_id  TYPE REF TO cl_dd_document,
      o_dyndoc_id1  TYPE REF TO cl_dd_document,
      o_dyndoc_id2  TYPE REF TO cl_dd_document,
      o_event_reciever TYPE REF TO lcl_event_reciever,
      o_splitter   TYPE REF TO cl_gui_splitter_container,
      o_splitter1   TYPE REF TO cl_gui_splitter_container,
      o_parent_grid TYPE REF TO cl_gui_container,
      o_parent_grid1 TYPE REF TO cl_gui_container,
      o_parent_grid2 TYPE REF TO cl_gui_container,
      o_parent_top TYPE REF TO cl_gui_container,
      o_parent_top1 TYPE REF TO cl_gui_container,
      o_parent_bottom TYPE REF TO cl_gui_container,
      o_html_cntrl TYPE REF TO cl_gui_html_viewer,
      o_html_cntrl1 TYPE REF TO cl_gui_html_viewer,
      o_html_cntrl2 TYPE REF TO cl_gui_html_viewer.

DATA: wa_print TYPE lvc_s_prnt.

DATA: amtchqi TYPE bsis-dmbtr,
      amtchqd TYPE bsis-dmbtr,
      clbal TYPE bsis-dmbtr.

DATA: l_from TYPE sy-tabix.

DATA: gl_text TYPE string,
      gl_text1 TYPE string,
      gl_bukrs TYPE string,
      gl_gsber TYPE string.

DATA: it_head1 TYPE TABLE OF ty_head,
       wa_head1 TYPE ty_head,
      it_head2 TYPE TABLE OF  ty_head,
      wa_head2 TYPE ty_head.

DATA: f_per TYPE char25,  "BSIS-BUDAT,
      to_per TYPE char25,  "BSIS-BUDAT,
      f_per1 TYPE bsis-budat,
      to_per1 TYPE bsis-budat.


*CNT = 1.

SELECTION-SCREEN: BEGIN OF BLOCK b1.
PARAMETERS: p_bukrs TYPE bsis-bukrs,
   p_chqd type bsis-hkont, " Riyas
            p_chqi type bsis-hkont, " Riyas
            p_main type bsis-hkont. " Riyas
"SELECT-OPTIONS:"S_HKONT FOR BSIS-HKONT,
  SELECT-OPTIONS: s_budat FOR bsis-budat.
  PARAMETERS :f_year  TYPE bseg-gjahr.
SELECTION-SCREEN: END OF BLOCK b1.


SELECT saknr txt20 FROM skat INTO TABLE it_skat1 WHERE saknr = p_chqd.


SELECT saknr txt20 FROM skat APPENDING CORRESPONDING FIELDS OF TABLE it_skat1
WHERE saknr = p_chqi.



SELECT SINGLE bukrs gsber FROM bsis INTO wa_head1 WHERE hkont EQ p_chqd. "old
SELECT SINGLE bukrs gsber FROM bsis INTO wa_head2 WHERE hkont EQ p_chqi. "old

*  SELECT  bukrs gsber FROM bsis INTO TABLE it_head1 WHERE hkont = p_chqd. " Riyas
*
*  loop at it_head1 into wa_head1.
*  endloop.
*
*SELECT   bukrs gsber FROM bsis INTO TABLE  it_head2 WHERE hkont = p_chqi. " Riyas
*
*loop at it_head2 into wa_head2.
*
*endloop.

********************TO TAKE OPENING BALANCE OF ALL THREE G/L***********************

SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsis INTO TABLE it_data
WHERE  budat LT s_budat-low AND  "For calculating opening balance
bukrs EQ p_bukrs AND hkont = p_chqi.

SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsas  APPENDING CORRESPONDING FIELDS OF TABLE it_data
WHERE bukrs EQ p_bukrs AND hkont = p_chqi AND
budat LT s_budat-low AND augdt GT s_budat-low.


SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsis  APPENDING CORRESPONDING FIELDS OF TABLE it_data
WHERE  budat LT s_budat-low AND  "For calculating opening balance
bukrs EQ p_bukrs AND hkont = p_chqd.


SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsas  APPENDING CORRESPONDING FIELDS OF TABLE it_data
WHERE bukrs EQ p_bukrs AND hkont = p_chqd AND
budat LT s_budat-low AND augdt GT s_budat-low.


SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsis  APPENDING CORRESPONDING FIELDS OF TABLE it_data
WHERE  budat LT s_budat-low AND  "For calculating opening balance
bukrs EQ p_bukrs AND hkont = p_main.


SELECT hkont gjahr belnr  shkzg dmbtr monat FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE it_data
WHERE bukrs EQ p_bukrs AND hkont = p_main AND
budat LT s_budat-low AND augdt GT s_budat-low.



********************************************************

CLASS lcl_event_reciever DEFINITION.

PUBLIC SECTION.

METHODS: handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid IMPORTING e_dyndoc_id,
         handle_top_of_page1 FOR EVENT top_of_page OF cl_gui_alv_grid IMPORTING e_dyndoc_id,
         handle_print_top_of_page FOR EVENT print_top_of_page OF cl_gui_alv_grid,
         handle_print_top_of_page1 FOR EVENT print_top_of_page OF cl_gui_alv_grid,
         handle_end_of_list FOR EVENT end_of_list OF cl_gui_alv_grid IMPORTING e_dyndoc_id,
         handle_print_end_of_list FOR EVENT print_end_of_list OF cl_gui_alv_grid.

ENDCLASS.


CLASS lcl_event_reciever IMPLEMENTATION.

METHOD handle_top_of_page.

PERFORM event_top_of_page USING o_dyndoc_id.

ENDMETHOD.

METHOD handle_print_top_of_page.

DATA: ls_head TYPE slis_t_listheader,
      wa_head LIKE LINE OF ls_head.

wa_head-typ = 'H'.
wa_head-info = 'G/L Bank Report Cheque Deposit'.
APPEND wa_head TO ls_head.
CLEAR wa_head.

*
*WA_HEAD-TYP = 'S'.
*WA_HEAD-KEY = 'Company Code:'.
*WA_HEAD-INFO = WA_HEAD1-BUKRS.
*APPEND WA_HEAD TO LS_HEAD.
*CLEAR WA_HEAD.
*
*
*WA_HEAD-TYP = 'S'.
*WA_HEAD-KEY = 'Business Area:'.
*WA_HEAD-INFO = WA_HEAD1-GSBER.
*APPEND WA_HEAD TO LS_HEAD.
*CLEAR WA_HEAD.



READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqd.

wa_head-typ = 'S'.
wa_head-key = 'G/L Code:'.
wa_head-info = p_chqd.
APPEND wa_head TO ls_head.
CLEAR wa_head.


wa_head-typ = 'S'.
wa_head-key = 'G/L Text:'.
wa_head-info = wa_skat1-txt20.
APPEND wa_head TO ls_head.
CLEAR wa_head.

CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.' s_budat-low(4) INTO f_per.

CONCATENATE s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4) INTO to_per.

wa_head-typ = 'S'.
wa_head-key = 'From Date:'.
wa_head-info = f_per.
APPEND wa_head TO ls_head.
CLEAR wa_head.

wa_head-typ = 'S'.
wa_head-key = 'To Date:'.
wa_head-info = to_per.
APPEND wa_head TO ls_head.
CLEAR wa_head.

READ TABLE it_opbal INTO wa_opbal INDEX 1.

wa_head-typ = 'S'.
wa_head-key = 'Opening Balance 3GLs'.
wa_head-info = wa_opbal-opbal.
*WA_HEAD-DDICTXT = 'L'.
APPEND wa_head TO ls_head.
CLEAR wa_head.

CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
    it_list_commentary       = ls_head[]
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
          .

ENDMETHOD.


METHOD handle_top_of_page1.

PERFORM event_top_of_page1 USING o_dyndoc_id2.

ENDMETHOD.

METHOD handle_print_top_of_page1.

DATA: ls_head2 TYPE slis_t_listheader,
      wa_head2 LIKE LINE OF ls_head2.

wa_head2-typ = 'H'.
wa_head2-info = 'G/L Bank Report Cheque Issued'.
APPEND wa_head2 TO ls_head2.
CLEAR wa_head2.

*
*WA_HEAD2-TYP = 'S'.
*WA_HEAD2-KEY = 'Company Code:'.
*WA_HEAD2-INFO = WA_HEAD1-BUKRS.
*APPEND WA_HEAD2 TO LS_HEAD2.
*CLEAR WA_HEAD2.
*
*
*WA_HEAD2-TYP = 'S'.
*WA_HEAD2-KEY = 'Business Area:'.
*WA_HEAD2-INFO = WA_HEAD1-GSBER.
*APPEND WA_HEAD2 TO LS_HEAD2.
*CLEAR WA_HEAD2.



READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqi.

wa_head2-typ = 'S'.
wa_head2-key = 'G/L Code:'.
wa_head2-info = p_chqi.
APPEND wa_head2 TO ls_head2.
CLEAR wa_head2.


wa_head2-typ = 'S'.
wa_head2-key = 'G/L Text:'.
wa_head2-info = wa_skat1-txt20.
APPEND wa_head2 TO ls_head2.
CLEAR wa_head2.

CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.' s_budat-low(4) INTO f_per.

CONCATENATE s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4) INTO to_per.

wa_head2-typ = 'S'.
wa_head2-key = 'From Date:'.
wa_head2-info = f_per.
APPEND wa_head2 TO ls_head2.
CLEAR wa_head2.

wa_head2-typ = 'S'.
wa_head2-key = 'To Date:'.
wa_head2-info = to_per.
APPEND wa_head2 TO ls_head2.
CLEAR wa_head2.

*read table it_opbal into wa_opbal index 1.
*
*WA_HEAD2-TYP = 'S'.
*WA_HEAD2-KEY = 'Opening Balance:'.
*WA_HEAD2-INFO = wa_opbal-opbal.
*APPEND WA_HEAD2 TO LS_HEAD2.
*CLEAR WA_HEAD2.

CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
    it_list_commentary       = ls_head2[]
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
          .

ENDMETHOD.



METHOD handle_end_of_list.

PERFORM event_end_of_list USING o_dyndoc_id1.

ENDMETHOD.

METHOD handle_print_end_of_list.

DATA: ls_head3 TYPE slis_t_listheader,
      wa_head3 LIKE LINE OF ls_head3.

wa_head3-typ = 'S'.
wa_head3-key = 'Closing Balance 3GLs'.
wa_head3-info = clbal.
APPEND wa_head3 TO ls_head3.
CLEAR wa_head3.

CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
    it_list_commentary       = ls_head3[]
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
          .

ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

*SELECT HKONT BELNR BUDAT GSBER DMBTR SHKZG SGTXT FROM BSIS INTO TABLE IT_BSIS1 WHERE HKONT IN S_HKONT
*AND BUDAT IN S_BUDAT AND BUKRS EQ P_BUKRS.

SELECT bukrs hkont gjahr belnr budat blart bschl gsber dmbtr shkzg sgtxt FROM bsis INTO TABLE it_bsis1 WHERE hkont EQ p_chqd
AND budat IN s_budat AND bukrs EQ p_bukrs.

SELECT bukrs hkont gjahr belnr budat blart bschl gsber dmbtr shkzg sgtxt FROM bsis APPENDING CORRESPONDING
FIELDS OF TABLE it_bsis1 WHERE hkont EQ p_chqi AND budat IN s_budat AND bukrs EQ p_bukrs.

******************To bring cleared item also**********************

SELECT bukrs hkont gjahr belnr budat blart  bschl gsber dmbtr shkzg sgtxt FROM bsas APPENDING CORRESPONDING FIELDS OF
TABLE it_bsis1 WHERE hkont EQ p_chqd AND budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART EQ 'RD'.
*
SELECT bukrs hkont gjahr belnr budat blart bschl gsber dmbtr shkzg sgtxt FROM bsas APPENDING CORRESPONDING
FIELDS OF TABLE it_bsis1 WHERE hkont EQ p_chqi AND budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART EQ 'RD'.

***************************************************************************************

IF it_bsis1 IS NOT INITIAL.

SELECT bukrs hkont gjahr belnr augbl bschl gsber dmbtr shkzg zuonr sgtxt lifnr kunnr FROM bseg INTO TABLE it_bseg
FOR ALL ENTRIES IN it_bsis1 WHERE
bukrs EQ p_bukrs AND belnr EQ it_bsis1-belnr AND hkont NE it_bsis1-hkont.  " SHKZG EQ 'S'.


****************to take zuonr field from bseg table******************************
SELECT bukrs hkont gjahr belnr augbl bschl gsber dmbtr shkzg zuonr sgtxt lifnr kunnr FROM bseg INTO TABLE it_bseg1
FOR ALL ENTRIES IN it_bsis1 WHERE
bukrs EQ p_bukrs AND belnr EQ it_bsis1-belnr AND hkont EQ it_bsis1-hkont.  " SHKZG EQ 'S'.
*******************************************************************************************************



*SELECT HKONT BELNR BUDAT GSBER DMBTR SHKZG SGTXT FROM BSIS INTO TABLE IT_BSIS
*FOR ALL ENTRIES IN IT_BSIS1 WHERE BUDAT IN S_BUDAT AND BUKRS EQ P_BUKRS AND
*BELNR EQ IT_BSIS1-BELNR AND HKONT NOT IN S_HKONT.

SELECT bukrs hkont gjahr belnr budat blart bschl gsber dmbtr shkzg sgtxt FROM bsis INTO TABLE it_bsis
FOR ALL ENTRIES IN it_bsis1 WHERE budat IN s_budat AND bukrs EQ p_bukrs AND
belnr EQ it_bsis1-belnr AND hkont NE p_chqd AND hkont NE p_chqi. " AND BLART NE 'ZR'.

******************To bring cleared item also**********************
SELECT bukrs hkont gjahr belnr budat blart bschl gsber dmbtr shkzg sgtxt FROM bsas APPENDING CORRESPONDING FIELDS OF
TABLE it_bsis FOR ALL ENTRIES IN it_bsis1 WHERE budat IN s_budat AND bukrs EQ p_bukrs AND
belnr EQ it_bsis1-belnr AND hkont NE p_chqd AND hkont NE p_chqi. "  AND BLART EQ 'RD'.

ENDIF.

*SELECT HKONT GJAHR BELNR BUDAT BLART BSCHL GSBER DMBTR SHKZG SGTXT FROM BSAS APPENDING CORRESPONDING FIELDS OF
*TABLE IT_BSIS FOR ALL ENTRIES IN IT_BSIS3 WHERE BUDAT IN S_BUDAT AND BUKRS EQ P_BUKRS AND
*AUGBL EQ IT_BSIS3-BELNR AND HKONT NE P_CHQD AND HKONT NE P_CHQI.


**********************************************************************************


IF it_bsis IS NOT INITIAL.

SELECT * FROM "KUNNR BELNR SHKZG GSBER DMBTR
bsid INTO  CORRESPONDING FIELDS OF TABLE it_bsid
FOR ALL ENTRIES IN it_bsis WHERE belnr EQ it_bsis-belnr AND
budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART NE 'ZR'.

******************To bring cleared item also**********************
SELECT * FROM "KUNNR BELNR SHKZG GSBER DMBTR
bsad APPENDING CORRESPONDING FIELDS OF TABLE it_bsid
FOR ALL ENTRIES IN it_bsis WHERE belnr EQ it_bsis-belnr AND
budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART NE 'ZR'.
*******************************************************

*IF IT_BSID IS INITIAL.



SELECT * FROM "LIFNR BELNR SHKZG GSBER DMBTR FROM
bsik INTO CORRESPONDING FIELDS OF TABLE it_bsik
FOR ALL ENTRIES IN it_bsis WHERE belnr EQ it_bsis-belnr
AND budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART NE 'ZR'.

******************To bring cleared item also**********************
SELECT * FROM "LIFNR BELNR SHKZG GSBER DMBTR FROM
bsak APPENDING CORRESPONDING FIELDS OF TABLE it_bsik
FOR ALL ENTRIES IN it_bsis WHERE belnr EQ it_bsis-belnr
AND budat IN s_budat AND bukrs EQ p_bukrs. " AND BLART NE 'ZR'.

*******************************************************

ENDIF.


CLEAR wa_bsis1.


LOOP AT it_bsis1 INTO wa_bsis1.


************TO NEGLET BRS LINE ITEM******************
IF wa_bsis1-blart NE 'ZR'.
***************************************************

READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsis1-belnr.

IF sy-subrc <> 0.

READ TABLE it_bseg INTO wa_bseg WITH KEY
belnr = wa_bsis1-belnr gsber = wa_bsis1-gsber gjahr = wa_bsis1-gjahr.

IF sy-subrc EQ 0.

IF wa_bseg-lifnr IS NOT INITIAL.
wa_bsis-hkont = wa_bseg-lifnr.
ELSEIF wa_bseg-kunnr IS NOT INITIAL.
wa_bsis-hkont = wa_bseg-kunnr.
ELSE.
wa_bsis-hkont = wa_bsis1-hkont.
ENDIF.
wa_bsis-gjahr = wa_bsis1-gjahr.
wa_bsis-belnr = wa_bsis1-belnr.
wa_bsis-budat = wa_bsis1-budat.
wa_bsis-blart = wa_bsis1-blart.
wa_bsis-bschl = wa_bsis1-bschl.
wa_bsis-gsber = wa_bseg-gsber.
wa_bsis-dmbtr =  wa_bsis1-dmbtr.
wa_bsis-shkzg = wa_bseg-shkzg.
wa_bsis-sgtxt = wa_bsis1-sgtxt.

APPEND wa_bsis TO it_bsis.
CLEAR: wa_bsis1,wa_bseg.

ENDIF.
ENDIF.
ENDIF.

ENDLOOP.

CLEAR: wa_bsis,wa_bseg.

********************************************************

LOOP AT it_bsik INTO wa_bsik.
CLEAR wa_bsis2.

*CNT = CNT + 1.

************TO NEGLET BRS LINE ITEM******************
IF wa_bsik-blart NE 'ZR'.
***************************************************

READ TABLE it_bsis INTO wa_bsis "TRANSPORTING NO FIELDS
WITH KEY belnr = wa_bsik-belnr.


IF sy-subrc EQ 0.

*L_FROM = SY-TABIX.


*AT NEW BELNR.


*LOOP AT IT_BSIS INTO WA_BSIS FROM L_FROM
*WHERE BELNR = WA_BSIK-BELNR.
*
*AT NEW BELNR.

wa_bsis2-bukrs = wa_bsik-bukrs.
wa_bsis2-hkont = wa_bsik-lifnr.
wa_bsis2-gjahr = wa_bsik-gjahr.
wa_bsis2-belnr = wa_bsik-belnr.
wa_bsis2-budat = wa_bsik-budat.
wa_bsis2-blart = wa_bsik-blart.
wa_bsis2-bschl = wa_bsis-bschl.
wa_bsis2-shkzg = wa_bsik-shkzg.
wa_bsis2-gsber = wa_bsik-gsber.
IF wa_bsik-shkzg EQ 'H'.
wa_bsis2-dmbtr = wa_bsik-dmbtr * -1.
ELSEIF  wa_bsik-shkzg EQ 'S'.
wa_bsis2-dmbtr = wa_bsik-dmbtr.
ENDIF.


APPEND wa_bsis2 TO it_bsis2.
*ENDAT.


CLEAR: wa_bsik, wa_bsis.


ENDIF.
ENDIF.

ENDLOOP.

*ENDAT.

CLEAR: wa_bsik, wa_bsis.

LOOP AT it_bsis INTO wa_bsis.
CLEAR: wa_bsis2, wa_bseg.

************TO NEGLET BRS LINE ITEM******************
IF wa_bsis-blart NE 'ZR'.
***************************************************

READ TABLE it_bsik INTO wa_bsik WITH KEY hkont = wa_bsis-hkont.

IF sy-subrc <> 0.

READ TABLE it_bsid INTO wa_bsid WITH KEY hkont = wa_bsis-hkont.

IF sy-subrc <> 0.
wa_bsis2-bukrs = wa_bsis-bukrs.
wa_bsis2-hkont = wa_bsis-hkont.
wa_bsis2-gjahr = wa_bsis-gjahr.
wa_bsis2-belnr = wa_bsis-belnr.
wa_bsis2-budat = wa_bsis-budat.
wa_bsis2-blart = wa_bsis-blart.
wa_bsis2-bschl = wa_bsis-bschl.
wa_bsis2-shkzg = wa_bsis-shkzg.
wa_bsis2-gsber = wa_bsis-gsber.
*READ TABLE IT_BSEG INTO WA_BSEG WITH KEY AUGBL = WA_BSIS-BELNR HKONT = WA_BSIS-HKONT.
*IF SY-SUBRC EQ 0.
IF wa_bsis-shkzg EQ 'H'.
wa_bsis2-dmbtr =  wa_bsis-dmbtr * -1.
ELSE.
wa_bsis2-dmbtr = wa_bsis-dmbtr.
ENDIF.


APPEND wa_bsis2 TO it_bsis2.

ENDIF.

ENDIF.
ENDIF.

ENDLOOP.

CLEAR: wa_bsik, wa_bsis.

LOOP AT it_bsid INTO wa_bsid.
CLEAR wa_bsis2.

************TO NEGLET BRS LINE ITEM******************
IF wa_bsis-blart NE 'ZR'.
***************************************************


READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsid-belnr.

IF sy-subrc EQ 0.
wa_bsis2-bukrs = wa_bsid-bukrs.
wa_bsis2-hkont = wa_bsid-kunnr.
wa_bsis2-gjahr = wa_bsid-gjahr.
wa_bsis2-belnr = wa_bsid-belnr.
wa_bsis2-budat = wa_bsid-budat.
wa_bsis2-blart = wa_bsid-blart.
wa_bsis2-bschl = wa_bsis-bschl.
wa_bsis2-shkzg = wa_bsid-shkzg.
wa_bsis2-gsber = wa_bsid-gsber.
IF wa_bsid-shkzg EQ 'H'.
wa_bsis2-dmbtr = wa_bsid-dmbtr * -1.
ELSEIF wa_bsid-shkzg EQ 'S'.
wa_bsis2-dmbtr = wa_bsid-dmbtr.
ENDIF.

APPEND wa_bsis2 TO it_bsis2.

ENDIF.
ENDIF.

ENDLOOP.

CLEAR: wa_bsik, wa_bsis, wa_bsid.

IF it_bsid IS NOT INITIAL.


SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1 FOR ALL ENTRIES IN it_bsis2
WHERE kunnr EQ it_bsis2-hkont.

ENDIF.

IF it_bsik IS NOT INITIAL.

SELECT lifnr name1 FROM lfa1 INTO TABLE it_lfa1 FOR ALL ENTRIES IN it_bsis2
WHERE lifnr EQ it_bsis2-hkont.

ENDIF.

SELECT saknr txt50 FROM skat INTO TABLE it_skat FOR ALL ENTRIES IN it_bsis2
WHERE saknr EQ it_bsis2-hkont AND ktopl EQ 'KPPL'.



*************TO GET BUSINESS AREA DESCRIPTION*****************
SELECT gsber gtext FROM tgsbt INTO TABLE it_tgsbt FOR ALL ENTRIES IN it_bsis2
WHERE gsber EQ it_bsis2-gsber AND spras EQ 'EN'.
********************************************************************



LOOP AT it_bsis2 INTO wa_bsis2.

CLEAR: wa_credit, wa_debit,wa_lfa1,wa_kna1,wa_skat, wa_tgsbt ,wa_bseg1.

READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY belnr = wa_bsis2-belnr.

IF sy-subrc EQ 0.

*IF WA_BSIS2-SHKZG EQ 'H'.

IF wa_bsis1-hkont EQ p_chqi.

*IF WA_BSIS2-HKONT EQ '0045000000'.

*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR * -1.

*ENDIF.
*READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_BSIS2-BELNR HKONT = WA_BSIS1-HKONT.
*IF SY-SUBRC EQ 0.
*IF WA_BSEG-BSCHL EQ '40'.
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR * -1.
*ENDIF.
*ENDIF.
*BREAK abap.
*IF WA_BSIS2-SHKZG EQ 'H'.
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR * -1.
*ELSEIF WA_BSIS2-SHKZG EQ 'S'.
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR.
*ENDIF.



READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsis2-hkont.
READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsis2-hkont.
READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_bsis2-hkont.

wa_credit-belnr = wa_bsis2-belnr.
wa_credit-budat = wa_bsis2-budat.
wa_credit-hkont = wa_bsis2-hkont.

IF wa_lfa1 IS NOT INITIAL.
wa_credit-gldesc = wa_lfa1-name1.
ELSEIF wa_skat IS NOT INITIAL.
wa_credit-gldesc = wa_skat-txt20.
ELSEIF wa_kna1 IS NOT INITIAL.
wa_credit-gldesc = wa_kna1-name1.
ENDIF.

wa_credit-gsber = wa_bsis2-gsber.

READ TABLE it_tgsbt INTO wa_tgsbt WITH KEY gsber = wa_bsis2-gsber.

IF sy-subrc EQ 0.

wa_credit-gtext = wa_tgsbt-gtext.

ENDIF.

READ TABLE it_bseg1 INTO wa_bseg1 WITH KEY belnr = wa_bsis2-belnr gjahr = wa_bsis2-gjahr bukrs = wa_bsis2-bukrs.

IF sy-subrc EQ 0.

wa_credit-zuonr = wa_bseg1-zuonr.

ENDIF.


wa_credit-dmbtr = wa_bsis2-dmbtr.

amtchqi = amtchqi + wa_credit-dmbtr.


*READ TABLE IT_BSIS1 INTO WA_BSIS1 WITH KEY HKONT = S_HKONT-LOW.
READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY hkont = p_chqi belnr = wa_bsis2-belnr.

wa_credit-sgtxt = wa_bsis1-sgtxt.

************************** Reference Doc No Appen

*BREAK AbAP.

if F_YEAR is not INITIAL.

*BREAK AbAP.

 SELECT SINGLE  bukrs belnr gjahr blart bldat budat xblnr bktxt FROM bkpf INTO  wa_bkpf
WHERE bukrs = P_BUKRS
AND belnr = wa_bsis2-belnr
AND gjahr = F_YEAR.

  wa_credit-BKTXT = wa_bkpf-bktxt.

Endif.
*************************************************

APPEND wa_credit TO it_credit.



*************************************************



*ELSEIF WA_BSIS2-SHKZG EQ 'S'.

ELSEIF wa_bsis1-hkont EQ p_chqd.

**
*IF WA_BSIS2-HKONT = '0045000000'.
*READ TABLE IT_BSEG INTO WA_BSEG WITH KEY HKONT = '0045000000' BELNR = WA_BSIS2-BELNR.
*
*IF SY-SUBRC EQ 0.
*
*IF WA_BSIS2-HKONT NE '0045000000' AND WA_BSIS2-BELNR EQ WA_BSEG-BELNR.
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR - WA_BSEG-DMBTR.
*ENDIF.
*
*ENDIF.

*IF WA_BSIS2-BLART EQ 'RD'.
*
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR * -1.
*
*ENDIF.

*BREAK abap.

*READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_BSIS2-BELNR HKONT = WA_BSIS1-HKONT.
*IF SY-SUBRC EQ 0.
*IF WA_BSEG-BSCHL EQ '50'.
*WA_BSIS2-DMBTR = WA_BSIS2-DMBTR * -1.
*ENDIF.
*ENDIF.

*READ TABLE IT_BSEG INTO WA_BSEG WITH KEY
*BELNR = WA_BSIS2-BELNR GSBER = WA_BSIS2-GSBER GJAHR = WA_BSIS2-GJAHR.

IF wa_bsis2-dmbtr LT 0.
wa_bsis2-dmbtr = wa_bsis2-dmbtr * -1.
ELSE.
wa_bsis2-dmbtr = wa_bsis2-dmbtr * -1.
ENDIF.




wa_debit-belnr = wa_bsis2-belnr.
wa_debit-budat = wa_bsis2-budat.
wa_debit-hkont = wa_bsis2-hkont.




READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsis2-hkont.
READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsis2-hkont.
READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_bsis2-hkont.

IF wa_lfa1 IS NOT INITIAL.
wa_debit-gldesc = wa_lfa1-name1.
ELSEIF wa_skat IS NOT INITIAL.
wa_debit-gldesc = wa_skat-txt20.
ELSEIF wa_kna1 IS NOT INITIAL.
wa_debit-gldesc = wa_kna1-name1.
ENDIF.

wa_debit-gsber = wa_bsis2-gsber.





READ TABLE it_tgsbt INTO wa_tgsbt WITH KEY gsber = wa_bsis2-gsber.

IF sy-subrc EQ 0.

wa_debit-gtext = wa_tgsbt-gtext.

ENDIF.

READ TABLE it_bseg1 INTO wa_bseg1 WITH KEY belnr = wa_bsis2-belnr gjahr = wa_bsis2-gjahr bukrs = wa_bsis2-bukrs.

IF sy-subrc EQ 0.

wa_debit-zuonr = wa_bseg1-zuonr.

ENDIF.

wa_debit-dmbtr = wa_bsis2-dmbtr.

amtchqd = amtchqd + wa_debit-dmbtr.


*READ TABLE IT_BSIS1 INTO WA_BSIS1 WITH KEY HKONT = S_HKONT-HIGH.
READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY hkont = p_chqd belnr = wa_bsis2-belnr.

wa_debit-sgtxt = wa_bsis1-sgtxt.


*Break abap.
APPEND wa_debit TO it_debit.


ENDIF.

ENDIF.

ENDLOOP.

SORT it_debit BY belnr ASCENDING.
SORT it_credit BY belnr ASCENDING.



LOOP AT it_data INTO wa_data.

IF wa_data-shkzg EQ 'H'.

wa_opbal-opbal = wa_data-dmbtr * -1.

ELSEIF  wa_data-shkzg EQ 'S'.

wa_opbal-opbal = wa_data-dmbtr.

ENDIF.

COLLECT wa_opbal INTO it_opbal.

ENDLOOP.


READ TABLE it_opbal INTO wa_opbal INDEX 1.

clbal = ( wa_opbal-opbal + amtchqd ) - amtchqi.


CLEAR  wa_opbal.







CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.' s_budat-low(4) INTO f_per.

CONCATENATE s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4) INTO to_per.


READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqd.
CONCATENATE p_chqd '-' wa_skat1-txt20 '[' wa_head1-bukrs '-' wa_head1-gsber ']' '[' f_per '-' to_per ']'
INTO gl_text." SEPARATED BY SPACE.
wa_lay-grid_title = gl_text.
*WA_TITLE = GL_TEXT.
wa_lay-info_fname = 'LINE_COLOR'.
READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqi.
CONCATENATE p_chqi '-' wa_skat1-txt20 '[' wa_head1-bukrs '-' wa_head1-gsber ']' '[' f_per '-' to_per ']'
INTO gl_text1. " SEPARATED BY SPACE.
wa_lay1-grid_title = gl_text1.
wa_lay1-info_fname = 'LINE_COLOR'.
*break abap.

*gs_layo-small_title = 'X' .
IF gs_variant-report IS INITIAL.
gs_variant-report = sy-repid.
ENDIF.



CALL SCREEN 101.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'ALVMENU'.
  SET TITLEBAR 'ALV REPORT'.

PERFORM create_and_init_alv.

ENDMODULE.                 " STATUS_0101  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

*READ TABLE IT_DAT INTO WA_DAT INDEX 1.
*READ TABLE IT_DAT1 INTO WA_DAT1 INDEX 1.

*READ TABLE IT_HEAD1 INTO WA_HEAD1 INDEX 1.
*READ TABLE IT_HEAD2 INTO WA_HEAD2 INDEX 1.

CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO  SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'PRIN'.
      NEW-PAGE PRINT ON DESTINATION 'LP01' IMMEDIATELY ' ' KEEP IN SPOOL 'X'.
*    no dialog .

  ENDCASE.                             " CASE SY-UCOMM

ENDMODULE.                 " USER_COMMAND_0101  INPUT


FORM create_and_init_alv.

wa_alv-fieldname = 'BELNR'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Document no.'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'BUDAT'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Posting Date.'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'HKONT'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Party'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'GLDESC'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Description'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'GSBER'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Business Area'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'GTEXT'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'BusA Description'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'ZUONR'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Assignment'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'DMBTR'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Amount'.
wa_alv-do_sum = 'X'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

wa_alv-fieldname = 'SGTXT'.
wa_alv-tabname  = 'IT_CREDIT'.
wa_alv-reptext = 'Narration'.
APPEND wa_alv TO it_alv.
CLEAR wa_alv.

*wa_alv-fieldname = 'BKTXT'.
*wa_alv-tabname  = 'IT_CREDIT'.
*wa_alv-reptext = 'Ref No'.
*APPEND wa_alv TO it_alv.
*CLEAR wa_alv.


**************ALV FIELDCAT 2**********************


wa_alv1-fieldname = 'BELNR'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Document no.'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'BUDAT'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Posting Date.'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'HKONT'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Party'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'GLDESC'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Description'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'GSBER'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Business Area'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'GTEXT'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'BusA Description'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'ZUONR'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Assignment'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'DMBTR'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Amount'.
wa_alv1-do_sum = 'X'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.


wa_alv1-fieldname = 'SGTXT'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Narration'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_alv1-fieldname = 'BKTXT'.
wa_alv1-tabname  = 'IT_DEBIT'.
wa_alv1-reptext = 'Ref No'.
APPEND wa_alv1 TO it_alv1.
CLEAR wa_alv1.

wa_lay-cwidth_opt = 'X'.
wa_lay1-cwidth_opt = 'X'.


CREATE OBJECT g_container1
    EXPORTING
      container_name              = 'CONTAINER1'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

CREATE OBJECT o_dyndoc_id
  EXPORTING
    style  = 'ALV_GRID'
*    background_color =
*    bds_stylesheet =
*    no_margins =
    .

CREATE OBJECT o_splitter
  EXPORTING
*    link_dynnr        =
*    link_repid        =
*    shellstyle        =
*    left              =
*    top               =
*    width             =
*    height            =
*    metric            = cntl_metric_dynpro
*    align             = 15
    parent            = g_container1
    rows              = 2
    columns           = 1
*    no_autodef_progid_dynnr =
*    name              =
*  EXCEPTIONS
*    cntl_error        = 1
*    cntl_system_error = 2
*    others            = 3
    .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


CALL METHOD o_splitter->get_container
  EXPORTING
    row       = 1
    column    = 1
  RECEIVING
    container = o_parent_top
    .

CALL METHOD o_splitter->get_container
  EXPORTING
    row       = 2
    column    = 1
  RECEIVING
    container = o_parent_grid
    .


    CALL METHOD o_splitter->set_row_height
      EXPORTING
        id                = 1
        height            = 20
*      IMPORTING
*        result            =
*      EXCEPTIONS
*        cntl_error        = 1
*        cntl_system_error = 2
*        others            = 3
            .
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.



* Object for Alv grid
  CREATE OBJECT r_grid1
    EXPORTING
      i_parent          = o_parent_grid
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT o_event_reciever.

  SET HANDLER o_event_reciever->handle_top_of_page FOR r_grid1.

  SET HANDLER o_event_reciever->handle_print_top_of_page FOR r_grid1.

  CALL METHOD r_grid1->set_table_for_first_display
    EXPORTING
      i_structure_name              = 'TY_FINAL'
      is_variant                    = gs_variant
*      i_save                        =
*      i_default                     = 'X'
      is_layout                     = wa_lay
*      is_print                      = WA_PRINT
*      it_special_groups             =
*      it_toolbar_excluding          =
*      it_hyperlink                  =
*      it_alv_graphics               =
*      it_except_qinfo               =
*      ir_salv_adapter               =
    CHANGING
      it_outtab                     = it_debit[]
      it_fieldcatalog               = it_alv
*      it_sort                       =
*      it_filter                     =
*    exceptions
*      invalid_parameter_combination = 1
*      program_error                 = 2
*      too_many_lines                = 3
*      others                        = 4
          .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  call method r_grid1->set_gridtitle
*    exporting
*      i_gridtitle = WA_TITLE
*      .

CALL METHOD o_dyndoc_id->initialize_document
  EXPORTING
*    first_time       =
*    style            =
    background_color = cl_dd_area=>col_textarea
*    bds_stylesheet   =
*    no_margins       =
    .
CALL METHOD r_grid1->list_processing_events
  EXPORTING
    i_event_name      =  'TOP_OF_PAGE'
    i_dyndoc_id       = o_dyndoc_id
*    is_subtottxt_info =
*    ip_subtot_line    =
*    i_table_index     =
*  CHANGING
*    c_subtottxt       =
    .


  CREATE OBJECT g_container2
    EXPORTING
      container_name              = 'CONTAINER2'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

CREATE OBJECT o_dyndoc_id1
  EXPORTING
    style  = 'ALV_GRID'
*    background_color =
*    bds_stylesheet =
*    no_margins =
    .

CREATE OBJECT o_dyndoc_id2
  EXPORTING
    style  = 'ALV_GRID'
*    background_color =
*    bds_stylesheet =
*    no_margins =
    .

    CREATE OBJECT o_splitter1
      EXPORTING
*        link_dynnr        =
*        link_repid        =
*        shellstyle        =
*        left              =
*        top               =
*        width             =
*        height            =
*        metric            = cntl_metric_dynpro
*        align             = 15
        parent            = g_container2
        rows              = 3
        columns           = 1
*        no_autodef_progid_dynnr =
*        name              =
*      EXCEPTIONS
*        cntl_error        = 1
*        cntl_system_error = 2
*        others            = 3
        .
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

CALL METHOD o_splitter1->get_container
  EXPORTING
    row       = 1
    column    = 1
  RECEIVING
    container = o_parent_top1
    .

CALL METHOD o_splitter1->get_container
  EXPORTING
    row       = 2
    column    = 1
  RECEIVING
    container = o_parent_grid1
    .

CALL METHOD o_splitter1->get_container
  EXPORTING
    row       = 3
    column    = 1
  RECEIVING
    container = o_parent_bottom
    .
CALL METHOD o_splitter1->set_row_height
  EXPORTING
    id                = 1
    height            = 20
*  IMPORTING
*    result            =
*  EXCEPTIONS
*    cntl_error        = 1
*    cntl_system_error = 2
*    others            = 3
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

CALL METHOD o_splitter1->set_row_height
  EXPORTING
    id                = 3
    height            = 20
*  IMPORTING
*    result            =
*  EXCEPTIONS
*    cntl_error        = 1
*    cntl_system_error = 2
*    others            = 3
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


* Object for Alv grid
  CREATE OBJECT r_grid2
    EXPORTING
      i_parent          = o_parent_grid1
      EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

SET HANDLER o_event_reciever->handle_top_of_page1 FOR r_grid2.

SET HANDLER o_event_reciever->handle_print_top_of_page1 FOR r_grid2.

SET HANDLER o_event_reciever->handle_end_of_list FOR r_grid2.

SET HANDLER o_event_reciever->handle_print_end_of_list FOR r_grid2.


  CALL METHOD r_grid2->set_table_for_first_display
    EXPORTING
      i_structure_name              = 'TY_FINAL'
*      is_variant                    =
*      i_save                        =
*      i_default                     = 'X'
      is_layout                     = wa_lay1
*      is_print                      =
*      it_special_groups             =
*      it_toolbar_excluding          =
*      it_hyperlink                  =
*      it_alv_graphics               =
*      it_except_qinfo               =
*      ir_salv_adapter               =
    CHANGING
      it_outtab                     = it_credit[]
      it_fieldcatalog               = it_alv1
*      it_sort                       =
*      it_filter                     =
*    exceptions
*      invalid_parameter_combination = 1
*      program_error                 = 2
*      too_many_lines                = 3
*      others                        = 4
          .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

CALL METHOD o_dyndoc_id1->initialize_document
  EXPORTING
*    first_time       =
*    style            =
    background_color = cl_dd_area=>col_textarea
*    bds_stylesheet   =
*    no_margins       =
    .

CALL METHOD r_grid2->list_processing_events
  EXPORTING
    i_event_name      = 'TOP_OF_PAGE'
    i_dyndoc_id       = o_dyndoc_id2
*    is_subtottxt_info =
*    ip_subtot_line    =
*    i_table_index     =
*  CHANGING
*    c_subtottxt       =
    .

CALL METHOD r_grid2->list_processing_events
  EXPORTING
    i_event_name      = 'END_OF_LIST'
    i_dyndoc_id       = o_dyndoc_id1
*    is_subtottxt_info =
*    ip_subtot_line    =
*    i_table_index     =
*  CHANGING
*    c_subtottxt       =
    .



ENDFORM.











*FORM EVENT_TOP_OF_PAGE USING O_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.
**WRITE: 'TEST'.
*
*DATA : DL_TEXT(255) TYPE C.  "Text
*  CALL METHOD O_DYNDOC_ID->ADD_TEXT
*    EXPORTING
*    TEXT = 'Flight Details'
*    SAP_STYLE = CL_DD_AREA=>HEADING
*    SAP_FONTSIZE = CL_DD_AREA=>LARGE
*    SAP_COLOR = CL_DD_AREA=>LIST_HEADING_INT.
*
*  CALL METHOD O_DYNDOC_ID->ADD_GAP
*    EXPORTING
*    WIDTH = 200.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_O_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM event_top_of_page  USING    p_o_dyndoc_id TYPE REF TO cl_dd_document.

DATA : dl_text(255) TYPE c.  "Text


CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = 'G/L Bank Report Cheque Deposit'
*    text_table    =
*    fix_lines     =
    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_heading_int
    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .
*CALL METHOD p_o_dyndoc_id->add_gap
*  EXPORTING
*    width      = 50.
*
*CALL METHOD p_o_dyndoc_id->new_line.
*
*clear dl_text.

SELECT SINGLE bukrs gsber FROM bsis INTO wa_head1 WHERE hkont EQ p_chqd.
SELECT SINGLE bukrs gsber FROM bsis INTO wa_head2 WHERE hkont EQ p_chqi.

*dl_text = 'Company Code:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text = wa_head1-bukrs.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

*CALL METHOD p_o_dyndoc_id->add_gap
*  EXPORTING
*    width      = 50.
*
*CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.


*dl_text = 'Business Area:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text = wa_head1-gsber.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .


*CALL METHOD p_o_dyndoc_id->add_gap
*  EXPORTING
*    width      = 50.
*
CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.


dl_text = 'G/L Code:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text = p_chqd.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .



*CALL METHOD p_o_dyndoc_id->add_gap
*  EXPORTING
*    width      = 50.
*
CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.


dl_text = 'G/L Text:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqd.

dl_text = wa_skat1-txt20.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .



CALL METHOD p_o_dyndoc_id->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.


dl_text = 'From Date:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.' s_budat-low(4) INTO f_per.


dl_text = f_per.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CALL METHOD p_o_dyndoc_id->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.

dl_text = 'To Date:'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CONCATENATE s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4) INTO to_per.

dl_text = to_per.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .


CALL METHOD p_o_dyndoc_id->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id->new_line.

CLEAR dl_text.


dl_text = 'Opening Balance (of all 3 GL codes):'.

CALL METHOD p_o_dyndoc_id->add_gap.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CLEAR dl_text.

READ TABLE it_opbal INTO wa_opbal INDEX 1.

dl_text = wa_opbal-opbal.

CALL METHOD p_o_dyndoc_id->add_text
  EXPORTING
    text          = dl_text
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CALL METHOD p_o_dyndoc_id->new_line.

PERFORM display.

ENDFORM.                    " EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .

IF o_html_cntrl IS INITIAL.

CREATE OBJECT o_html_cntrl
  EXPORTING
*    shellstyle         =
    parent             = o_parent_top
*    lifetime           = lifetime_default
*    saphtmlp           =
*    uiflag             =
*    name               =
*    saphttp            =
*    query_table_disabled = ''
*  EXCEPTIONS
*    cntl_error         = 1
*    cntl_install_error = 2
*    dp_install_error   = 3
*    dp_error           = 4
*    others             = 5
    .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDIF.


CALL METHOD o_dyndoc_id->merge_document.

o_dyndoc_id->html_control = o_html_cntrl.

CALL METHOD o_dyndoc_id->display_document
  EXPORTING
    reuse_control      = 'X'
*    reuse_registration =
*    container          =
    parent             = o_parent_top
*  EXCEPTIONS
*    html_display_error = 1
*    others             = 2
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_O_DYNDOC_ID1  text
*----------------------------------------------------------------------*
FORM event_end_of_list  USING    p_o_dyndoc_id1 TYPE REF TO cl_dd_document.

DATA: dl_text1(255) TYPE c.

DATA: dl_text4(255) TYPE c.


dl_text1 = 'Closing Balance(of all 3 GL codes):'.

*CONDENSE DL_TEXT1.

CALL METHOD p_o_dyndoc_id1->add_gap.

CALL METHOD p_o_dyndoc_id1->add_text
  EXPORTING
    text          = dl_text1
*    text_table    =
*    fix_lines     =
*    sap_style     = CL_DD_AREA=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  =
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CLEAR dl_text1.


dl_text1 = clbal.

CALL METHOD p_o_dyndoc_id1->add_text
  EXPORTING
    text          = dl_text1
*    text_table    =
*    fix_lines     =
*    sap_style     =
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  =
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CALL METHOD p_o_dyndoc_id1->new_line.

PERFORM display1.

ENDFORM.                    " EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  DISPLAY1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display1 .

CREATE OBJECT o_html_cntrl1
  EXPORTING
*    shellstyle         =
    parent             = o_parent_bottom
*    lifetime           = lifetime_default
*    saphtmlp           =
*    uiflag             =
*    name               =
*    saphttp            =
*    query_table_disabled = ''
*  EXCEPTIONS
*    cntl_error         = 1
*    cntl_install_error = 2
*    dp_install_error   = 3
*    dp_error           = 4
*    others             = 5
    .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

CALL METHOD o_dyndoc_id1->merge_document.

o_dyndoc_id1->html_control = o_html_cntrl1.

CALL METHOD o_dyndoc_id1->display_document
  EXPORTING
    reuse_control      = 'X'
*    reuse_registration =
*    container          =
    parent             = o_parent_bottom
*  EXCEPTIONS
*    html_display_error = 1
*    others             = 2
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.



ENDFORM.                    " DISPLAY1
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_O_DYNDOC_ID2  text
*----------------------------------------------------------------------*
FORM event_top_of_page1  USING    p_o_dyndoc_id2 TYPE REF TO cl_dd_document.

DATA : dl_text2(255) TYPE c.  "Text


CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = 'G/L Bank Report  Cheque issued'
*    text_table    =
*    fix_lines     =
    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_heading_int
    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .
CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.

SELECT SINGLE bukrs gsber FROM bsis INTO wa_head2 WHERE hkont EQ p_chqi.

*DL_TEXT2 = 'Company Code:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text2 = wa_head2-bukrs.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.


*DL_TEXT2 = 'Business Area:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text2 = wa_head2-gsber.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .


CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.


dl_text2 = 'G/L Code:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

dl_text2 = p_chqi.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .



CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.


dl_text2 = 'G/L Text:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

READ TABLE it_skat1 INTO wa_skat1 WITH KEY saknr = p_chqi.

dl_text2 = wa_skat1-txt20.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .



CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.


dl_text2 = 'From Date:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.' s_budat-low(4) INTO f_per.


dl_text2 = f_per.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.

dl_text2 = 'To Date:'.

CALL METHOD p_o_dyndoc_id2->add_gap.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>HEADING
    sap_color     = cl_dd_area=>list_heading_int
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .

CONCATENATE s_budat-high+6(2) '.' s_budat-high+4(2) '.' s_budat-high(4) INTO to_per.

dl_text2 = to_per.

CALL METHOD p_o_dyndoc_id2->add_text
  EXPORTING
    text          = dl_text2
*    text_table    =
*    fix_lines     =
*    sap_style     = cl_dd_area=>heading
    sap_color     = cl_dd_area=>list_negative_inv
*    sap_fontsize  = cl_dd_area=>large
*    sap_fontstyle =
*    sap_emphasis  =
*    style_class   =
*  CHANGING
*    document      =
    .


CALL METHOD p_o_dyndoc_id2->add_gap
  EXPORTING
    width      = 50.

CALL METHOD p_o_dyndoc_id2->new_line.

CLEAR dl_text2.


*DL_TEXT2 = 'Opening Balance:'.
*
*CALL METHOD p_o_dyndoc_id2->add_gap.
*
*CALL METHOD p_o_dyndoc_id2->add_text
*  EXPORTING
*    text          = DL_TEXT2
**    text_table    =
**    fix_lines     =
**    sap_style     = cl_dd_area=>HEADING
*    sap_color     = cl_dd_area=>list_heading_int
**    sap_fontsize  = cl_dd_area=>large
**    sap_fontstyle =
**    sap_emphasis  =
**    style_class   =
**  CHANGING
**    document      =
*    .
*
*clear DL_TEXT2.
*
*read table it_opbal into wa_opbal index 1.
*
*DL_TEXT2 = wa_opbal-opbal.
*
*CALL METHOD p_o_dyndoc_id2->add_text
*  EXPORTING
*    text          = DL_TEXT2
**    text_table    =
**    fix_lines     =
**    sap_style     = cl_dd_area=>heading
*    sap_color     = cl_dd_area=>list_negative_inv
**    sap_fontsize  = cl_dd_area=>large
**    sap_fontstyle =
**    sap_emphasis  =
**    style_class   =
**  CHANGING
**    document      =
*    .

CALL METHOD p_o_dyndoc_id2->new_line.

PERFORM display2.

ENDFORM.                    " EVENT_TOP_OF_PAGE1
*&---------------------------------------------------------------------*
*&      Form  DISPLAY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display2 .

CREATE OBJECT o_html_cntrl2
  EXPORTING
*    shellstyle         =
    parent             = o_parent_top1
*    lifetime           = lifetime_default
*    saphtmlp           =
*    uiflag             =
*    name               =
*    saphttp            =
*    query_table_disabled = ''
*  EXCEPTIONS
*    cntl_error         = 1
*    cntl_install_error = 2
*    dp_install_error   = 3
*    dp_error           = 4
*    others             = 5
    .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

CALL METHOD o_dyndoc_id2->merge_document.

o_dyndoc_id2->html_control = o_html_cntrl2.

CALL METHOD o_dyndoc_id2->display_document
  EXPORTING
    reuse_control      = 'X'
*    reuse_registration =
*    container          =
    parent             = o_parent_top1
*  EXCEPTIONS
*    html_display_error = 1
*    others             = 2
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.




ENDFORM.                    " DISPLAY2
