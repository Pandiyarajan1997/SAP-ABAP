*&---------------------------------------------------------------------*
*& Include          ZDMS_COPY_ACC_DOC_TO_DISTRIB01
*&---------------------------------------------------------------------*
*    TYPES: BEGIN OF ty_output,
*             check   TYPE check,
*             kunnr   TYPE kunnr,
*             name1   TYPE name1,
*             werks   TYPE werks_d,
*             gsber   TYPE gsber,
*             gtext   TYPE gtext,
*             belnr   TYPE belnr_d,
*             sgtxt   TYPE sgtxt,
*             gjahr   TYPE gjahr,
*             blart   TYPE blart,
*             stblg   TYPE stblg,
*             stjah   TYPE stjah,
*             blart1  TYPE blart,
*             bktxt   TYPE bktxt,
*             cpudt   TYPE cpudt,
*             budat   TYPE budat,
*             bldat   TYPE bldat,
*             vbeln   TYPE vbeln,
*             shkzg   TYPE shkzg,
*             dmbtr   TYPE dmbtr,
*             netwr   TYPE netwr,
*             mwsbk   TYPE mwsbp,
*             kschl   TYPE kschl,
*             kwert   TYPE kwert,
*             josg    TYPE kwert,
*             jocg    TYPE kwert,
*             joig    TYPE kwert,
*             jtc1    TYPE kwert,
*             blart2  TYPE blart,
*             belnr2  TYPE belnr_d,
*             belnr3  TYPE belnr_d,
*             type(1) TYPE c,
*             remarks TYPE remarks,
*           END OF ty_output.
*    DATA: gt_final_tab TYPE STANDARD TABLE OF ty_output.

DATA: g_budat TYPE bkpf-cpudt.
DATA: g_blart TYPE bkpf-blart.
DATA: g_kunnr TYPE bseg-kunnr.
DATA: g_mtype TYPE zbapi_mtype.
SELECT-OPTIONS: so_docdt  FOR g_budat OBLIGATORY.
SELECT-OPTIONS: so_docty  FOR g_blart NO INTERVALS.
SELECT-OPTIONS: so_kunnr  FOR g_kunnr NO INTERVALS.
SELECT-OPTIONS: so_mtype  FOR g_mtype NO INTERVALS.
PARAMETERS: p_lifnr TYPE lifnr DEFAULT '0010002452'.

TYPES: t_kunnr TYPE RANGE OF kunnr.
DATA : gr_kunnr TYPE RANGE OF kunnr.
