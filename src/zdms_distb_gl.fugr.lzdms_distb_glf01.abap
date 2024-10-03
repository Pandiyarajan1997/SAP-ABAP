*----------------------------------------------------------------------*
***INCLUDE LZDMS_DISTB_GLF01.
*----------------------------------------------------------------------*
FORM new_enrty.

  SELECT SINGLE werks
    FROM kna1 INTO @DATA(werks)
    WHERE kunnr = @zdms_distb_gl-kunnr
    AND   werks = @zdms_distb_gl-werks.
  IF sy-subrc <> 0.
    MESSAGE 'Mismatch Dustributor code with Plant' TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.

  SELECT SINGLE kunnr
    FROM zdms_distb_gl INTO @DATA(l_kunnr)
    WHERE   mingl = @zdms_distb_gl-mingl
         OR incgl = @zdms_distb_gl-incgl
         OR outgl = @zdms_distb_gl-outgl.
  IF sy-subrc = 0.
    DATA(l_txt) = |G/L is already used for other customer code { l_kunnr }|.
    MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.
  SELECT SINGLE saknr
    FROM skb1 INTO @DATA(l_saknr)
    WHERE bukrs = 'DMS1'
    AND   saknr = @zdms_distb_gl-mingl.
  IF sy-subrc <> 0.
    l_txt = |G/L is not mapped with company DMS1 code { zdms_distb_gl-mingl }|.
    MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.
  SELECT SINGLE saknr
    FROM skb1 INTO @l_saknr
    WHERE bukrs = 'DMS1'
    AND   saknr = @zdms_distb_gl-incgl.
  IF sy-subrc <> 0.
    l_txt = |G/L is not mapped with company DMS1 code { zdms_distb_gl-incgl }|.
    MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.
  SELECT SINGLE saknr
    FROM skb1 INTO @l_saknr
    WHERE bukrs = 'DMS1'
    AND   saknr = @zdms_distb_gl-outgl.
  IF sy-subrc <> 0.
    l_txt = |G/L is not mapped with company DMS1 code { zdms_distb_gl-outgl }|.
    MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
