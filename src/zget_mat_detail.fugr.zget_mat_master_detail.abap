FUNCTION zget_mat_master_detail.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LV_VKORG) TYPE  VKORG OPTIONAL
*"     VALUE(LV_VTWEG) TYPE  VTWEG OPTIONAL
*"  TABLES
*"      IT_MAT_DETAIL STRUCTURE  ZGET_MAT_DETAIL
*"----------------------------------------------------------------------

  TYPES:BEGIN OF ty_mvke,
          vkorg TYPE mvke-vkorg,
          vtweg TYPE mvke-vtweg,
          matnr TYPE mvke-matnr,
        END OF ty_mvke.

  DATA:it_mvke TYPE TABLE OF ty_mvke,
       wa_mvke TYPE ty_mvke.





  TYPES:BEGIN OF ty_mara,
          matnr  TYPE mara-matnr,
          matkl  TYPE mara-matkl,
          mtart  TYPE mara-mtart,
          meins  TYPE mara-meins,
          spart  TYPE mara-spart,
          zeivr  TYPE mara-zeivr,
          mstae  TYPE mara-mstae,
          ersda  TYPE mara-ersda,
          ernam  TYPE mara-ernam,
          laeda  TYPE mara-laeda,
          aenam  TYPE mara-aenam,
          lvorm  TYPE mara-lvorm,
          status TYPE mara-status,
        END OF ty_mara.

  DATA:it_mara TYPE TABLE OF ty_mara,
       wa_mara TYPE ty_mara.

  TYPES:BEGIN OF ty_makt,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF ty_makt.
  DATA:it_makt TYPE TABLE OF ty_makt,
       wa_makt TYPE ty_makt.


  TYPES: BEGIN OF ty_t023t,
           matkl TYPE t023t-matkl,
           wgbez TYPE t023t-wgbez,
         END OF ty_t023t.
  DATA:it_t023t TYPE TABLE OF ty_t023t,
       wa_t023t TYPE ty_t023t.

  TYPES: BEGIN OF ty_t134t,
           mtart TYPE t134t-mtart,
           mtbez TYPE t134t-mtbez,
         END OF ty_t134t.
  DATA:it_t134t TYPE TABLE OF ty_t134t,
       wa_t134t TYPE ty_t134t.


  DATA: it_t006a TYPE STANDARD TABLE OF t006a,
        wa_t006a TYPE t006a.

  DATA: it_tspat TYPE STANDARD TABLE OF tspat,
        wa_tspat TYPE tspat.



  TYPES: BEGIN OF ty_marc,
           matnr TYPE marc-matnr,
           werks TYPE marc-werks,
           steuc TYPE marc-steuc,
         END OF ty_marc.
  DATA:it_marc TYPE TABLE OF ty_marc,
       wa_marc TYPE ty_marc.






  SELECT vkorg
         vtweg
         matnr
       FROM mvke  INTO TABLE it_mvke
        WHERE vkorg EQ lv_vkorg AND
              vtweg EQ lv_vtweg.


  SELECT * FROM tspat
    INTO TABLE it_tspat
      WHERE spras EQ sy-langu ORDER BY PRIMARY KEY.


  IF it_mvke[] IS NOT INITIAL.
    SELECT matnr
           matkl
           mtart
           meins
           spart
           zeivr
           mstae
           ersda
           ernam
           laeda
           aenam
           lvorm
           status
            FROM mara  INTO CORRESPONDING FIELDS OF TABLE it_mara
         FOR ALL ENTRIES IN it_mvke
         WHERE matnr EQ it_mvke-matnr.  "AND STATUS = SPACE. (to pull all the datas irrespective of active material and inactive material.)

  ENDIF.

  IF it_mara[] IS NOT INITIAL.


    SELECT * FROM t006a INTO TABLE it_t006a.


    IF it_mara IS NOT INITIAL.
      SELECT matkl
             wgbez FROM t023t INTO TABLE it_t023t
                 FOR ALL ENTRIES IN it_mara
                  WHERE matkl = it_mara-matkl AND
                        spras = sy-langu.
      SELECT matnr
             maktx FROM makt INTO TABLE it_makt
                FOR ALL ENTRIES IN it_mara
                    WHERE matnr = it_mara-matnr  AND
                          spras = sy-langu.



      SELECT mtart
             mtbez FROM t134t INTO TABLE it_t134t
                 FOR ALL ENTRIES IN it_mara
                     WHERE mtart = it_mara-mtart AND
                           spras = sy-langu.
    ENDIF.
  ENDIF.



  SORT it_mara ASCENDING BY  matnr.
  SORT it_makt ASCENDING BY  matnr.
  SORT it_t023t ASCENDING BY matkl.
  SORT it_t134t ASCENDING BY mtart.
  SORT it_t006a ASCENDING BY msehi.

  LOOP AT it_mvke INTO wa_mvke.

    it_mat_detail-matnr = wa_mvke-matnr.
    it_mat_detail-vkorg = wa_mvke-vkorg.
    it_mat_detail-vtweg = wa_mvke-vtweg.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mvke-matnr BINARY SEARCH.
    IF sy-subrc  = 0.

      it_mat_detail-matnr = wa_mara-matnr.
      it_mat_detail-mtart = wa_mara-mtart.
      it_mat_detail-matkl = wa_mara-matkl.
      it_mat_detail-meins = wa_mara-meins.
      it_mat_detail-spart = wa_mara-spart.

      it_mat_detail-mstae = wa_mara-mstae.
      it_mat_detail-ersda = wa_mara-ersda.
      it_mat_detail-ernam = wa_mara-ernam.
      it_mat_detail-laeda = wa_mara-laeda.
      it_mat_detail-aenam = wa_mara-aenam.
      IF wa_mara-status = 'X' OR wa_mara-lvorm = 'X'.
        it_mat_detail-status = 'X'.
      ENDIF.

    ELSE.
      CLEAR:it_mat_detail,wa_mara,wa_makt,wa_t023t,wa_t134t.
      CONTINUE.
    ENDIF.



    READ TABLE it_makt INTO wa_makt
         WITH KEY matnr = it_mat_detail-matnr BINARY SEARCH.

    IF sy-subrc = 0.
      it_mat_detail-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_t023t INTO wa_t023t
            WITH KEY matkl = it_mat_detail-matkl BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat_detail-wgbez = wa_t023t-wgbez.
    ENDIF.

    READ TABLE it_t134t INTO wa_t134t
              WITH KEY mtart = it_mat_detail-mtart BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat_detail-mtbez = wa_t134t-mtbez.
    ENDIF.


    READ TABLE it_t006a INTO wa_t006a
                WITH KEY msehi = it_mat_detail-meins BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat_detail-mseht = wa_t006a-mseht.
    ENDIF.


    READ TABLE it_tspat INTO wa_tspat "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
              WITH KEY spart = it_mat_detail-spart BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat_detail-spartx = wa_tspat-vtext.
    ENDIF.


    APPEND it_mat_detail.

    CLEAR:it_mat_detail,wa_mara,wa_makt,wa_t023t,wa_t134t.
  ENDLOOP.



ENDFUNCTION.
