*&---------------------------------------------------------------------*
*& Include          ZDMS_COPY_ACC_DOC_TO_DISTRIB01
*&---------------------------------------------------------------------*

DATA: g_budat TYPE bkpf-cpudt.
DATA: g_blart TYPE bkpf-blart.
DATA: g_kunnr TYPE bseg-kunnr.
SELECT-OPTIONS: so_docdt  FOR g_budat OBLIGATORY.
SELECT-OPTIONS: so_docty  FOR g_blart NO INTERVALS.
SELECT-OPTIONS: so_kunnr  FOR g_kunnr NO INTERVALS.
PARAMETERS: p_lifnr TYPE lifnr.
