*----------------------------------------------------------------------*
***INCLUDE LZHR_JOBCODE_VF01.
*----------------------------------------------------------------------*
FORM create.
  zhr_jobcode_v-jcode = zhr_jobcode_v-parvw.
  SELECT SINGLE stext INTO zhr_jobcode_v-stext
    FROM hrp1000
    WHERE plvar = '01' AND
          otype = 'C' AND
          objid = zhr_jobcode_v-stell AND
          istat = '1' AND
          begda LE sy-datum AND
          endda GE sy-datum.
ENDFORM.
FORM update.
  zhr_jobcode_v-jcode = zhr_jobcode_v-parvw.
  SELECT SINGLE stext INTO zhr_jobcode_v-stext
    FROM hrp1000
    WHERE plvar = '01' AND
          otype = 'C' AND
          objid = zhr_jobcode_v-stell AND
          istat = '1' AND
          begda LE sy-datum AND
          endda GE sy-datum.
ENDFORM.
