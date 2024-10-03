FUNCTION zdistributor_stock_inward.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DISTRIBUTOR_CODE) TYPE  KUNNR
*"     REFERENCE(VBELN) TYPE  VBELN_VF
*"----------------------------------------------------------------------
*----- Invoice Structure -------*
*  TYPES: BEGIN OF invoice,
*           vbeln TYPE vbeln_vf,
*           fkdat TYPE fkdat,
*           knumv TYPE knumv,
*           kunag TYPE kunag,
*           posnr TYPE posnr_vf,
*           fkimg TYPE fkimg,
*           vrkme TYPE vrkme,
*           netwr TYPE netwr_fp,
*           matnr TYPE matnr,
*           werks TYPE werks_d,
*           matkl TYPE matkl,
*           vgbel TYPE vgbel,
*           aubel TYPE vbeln_va,
*           aupos TYPE posnr_va,
*           regio TYPE regio,
*         END OF invoice.
*  DATA:gt_invoice TYPE TABLE OF invoice.
**------ Info record Structure -------*
*  TYPES: BEGIN OF info_record,
*           lifnr TYPE lifnr,
*           matnr TYPE matnr,
*           ekorg TYPE ekorg,
*           werks TYPE werks_d,
*         END OF info_record.
*  DATA: gt_inforecord TYPE TABLE OF info_record.
*  REFRESH: gt_invoice,gt_inforecord.
**--- Fetching Invoice Data from Billing Documents ----------------*
*  SELECT a~vbeln
*         a~fkdat
*         a~knumv
*         a~kunag
*         b~posnr
*         b~fkimg
*         b~vrkme
*         b~netwr
*         b~matnr
*         b~werks
*         b~matkl
*         b~vgbel
*         b~aubel
*         b~aupos
*         c~regio INTO CORRESPONDING FIELDS OF TABLE me->gt_invoice
*                 FROM vbrk AS a INNER JOIN vbrp AS b
*                 ON a~vbeln = b~vbeln
*                 INNER JOIN t001w AS c
*                 ON c~werks = b~werks
*                 FOR ALL ENTRIES IN me->gt_alv_disp1
*                 WHERE a~vbeln = me->gt_alv_disp1-vbeln
*                 AND a~fkart = 'YBBR'
*                 AND a~fkdat = me->gt_alv_disp1-fkdat
*                 AND a~fksto NE 'X'.
*  IF sy-subrc = 0.
*    SORT me->gt_invoice[] BY vbeln matnr posnr.
**----- Inforecord Details based on Invoice data --*
*    SELECT lifnr
*           matnr
*           ekorg
*           werks FROM a017
*                 INTO TABLE me->gt_inforecord
*                 FOR ALL ENTRIES IN me->gt_invoice
*                 WHERE lifnr = p_lifnr
*                 AND matnr = me->gt_invoice-matnr
*                 AND ekorg = 'DMS1'
*                 AND datbi GE sy-datum
*                 AND datab LE sy-datum.
*    IF sy-subrc = 0.
*      SORT me->gt_inforecord[] BY lifnr matnr.
*    ENDIF.
*  ENDIF.




ENDFUNCTION.
