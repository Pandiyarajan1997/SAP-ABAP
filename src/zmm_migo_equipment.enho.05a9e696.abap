"Name: \PR:SAPMM07M\FO:SLL_DOCUMENT_TRANSFER\SE:BEGIN\EI
ENHANCEMENT 0 ZMM_MIGO_EQUIPMENT.
*Created by: Samsudeen M
*Created on: 04.05.2023
*Purpose: Storing the Equipment field in MSEG
DATA: lt_migo_item TYPE STANDARD TABLE OF goitem.
REFRESH: lt_migo_item.
IMPORT PT_GOITEM TO lt_migo_item FROM MEMORY ID 'MTAB'.
IF lt_migo_item IS NOT INITIAL.
LOOP AT lt_migo_item INTO DATA(lw_item).
LOOP AT xmseg ASSIGNING FIELD-SYMBOL(<fs_mseg>) WHERE zeile = lw_item-zeile.
<fs_mseg>-equnr = lw_item-equnr.
ENDLOOP.
ENDLOOP.
ENDIF.
ENDENHANCEMENT.
