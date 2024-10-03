*&---------------------------------------------------------------------*
*& Include zmaterial_upload_top
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&    Structure Declarations
*&---------------------------------------------------------------------*
*****Structure Declaration for Uploading the data
TYPES : BEGIN OF ty_upload,
          matnr         TYPE String, "RMMG1-MATNR,  " Material Num
          mbrsh         TYPE string, "RMMG1-MBRSH,  " Ind Sector
          mtart         TYPE string, "RMMG1-MTART,  " Material Type
*&---------------------------------------------------------------------*
*   Org Level Selection
*&---------------------------------------------------------------------*
          plant         TYPE string, "WERKS_D,      " Plant
          sales_org     TYPE string, "VKORG,        " Sales Organization
          distr_chan    TYPE string, "VTWEG,        " Distribution Channel
          lgort         TYPE string, "LGORT_D,      " Stg Loc

*&---------------------------------------------------------------------*
*   Basic Data1
*&---------------------------------------------------------------------*
          maktx         TYPE string, "MAKT-MAKTX,   " Material Desc
          meins         TYPE string, "MARA-MEINS,   " UOM
          matl_group    TYPE string, "MATKL,        " Material Group
          bismt         TYPE string, " Old material Number
          Divison       TYPE string,
*          mtpos_mara    TYPEstring,mara, " General item category group
          Payterm       TYPE string, "Payment Term
          gross_wt      TYPE string,       " Gross Weight
          wt_unit       TYPE string, "Weight Unit
          net_weight    TYPE string,       " Net Weight
          Volume        TYPE string, "Volume
          Vol_unit      TYPE string, "Volume Unit
          size_dim      TYPE string, "Size/Dimensions
          ecom_name     TYPE string,       " ECOMM
          ecom_category TYPE string,       " ECOMM Caterory
          ecom_matnr    TYPE string,    "ECOMM Material

*&---------------------------------------------------------------------*
*  Classification View
*&---------------------------------------------------------------------*
          klart         TYPE string, "KLASSENART,   " Class Type
          class         TYPE string, "KLASSE_D  ,   " Class
          Date          TYPE string,
*&---------------------------------------------------------------------*
*  Sales View 1
*&---------------------------------------------------------------------*
*          base_unit     TYPE string, " Base unit of measure
*          division           TYPE string, "SPART,        " Division
*          Tax_clsf      TYPE string,
*&---------------------------------------------------------------------*
*  Sales: General/Plant
*&---------------------------------------------------------------------*
*          tragr              TYPE string,       " Transportation Group
*          ladgr              TYPE string,       " Loading Group
          prctr         TYPE string,       " profit center
*          mtvfp              TYPE string,       " Availability Check
*          xchpf              TYPE string,   "mara-XCHPF
*          xchpf_werks        TYPE string,
*&---------------------------------------------------------------------*
*  Foreign Trade Import
*&---------------------------------------------------------------------*
          cntrl_code    TYPE string,    "Control code
*&---------------------------------------------------------------------*
*   Plant Storage data1
*&---------------------------------------------------------------------*
          min_shelflife TYPE string,
          tot_shelflife TYPE string,
*          xchpf_MARC           TYPE string, "MARC-XCHPF,   " BATCH MANG
*          iprkz           TYPE string, "Period Ind. for SLED
*&---------------------------------------------------------------------*
*   Plant data/ Storage 2
*&---------------------------------------------------------------------*
*          prctr           TYPE char10,       " Profit Center(31)
*&---------------------------------------------------------------------*
*   Accounting 1
*&---------------------------------------------------------------------*
          std_price     TYPE string, "Standard price

        END OF ty_upload.
TYPES:BEGIN OF ty_altuom,
        Matnr         TYPE string,
        denominator   TYPE string,
        alt_uom       TYPE string,
        numerator     TYPE string,
        dummy1 TYPE string, "VKORG,        " Sales Organization
        dummy2 TYPE string, "VTWEG,        " Distribution Channel
        dummy3 TYPE string, "LGORT_D,      " Stg Loc
        dummy4 TYPE string, "MAKT-MAKTX,   " Material Desc
        dummy5 TYPE string, "MARA-MEINS,   " UOMx`
        dummy6 TYPE string, "MATKL,        " Material Group
        dummy7 TYPE string, " Old material Number
        dummy8 TYPE string,
*       dummy    TYPEstring,mara, " General item category group
        dummy9 TYPE string, "Payment Term
        dummy10 TYPE string,       " Gross Weight
        dummy11 TYPE string, "Weight Unit
        dummy12 TYPE string,       " Net Weight
        dummy13 TYPE string, "Volume
        dummy14 TYPE string, "Volume Unit
        dummy15 TYPE string, "Size/Dimensions
        dummy16 TYPE string,       " ECOMM
        dummy17 TYPE string,       " ECOMM Caterory
        dummy18 TYPE string,    "ECOMM Material
        dummy19 TYPE string, "KLASSENART,   " Class Type
        dummy20 TYPE string, "KLASSE_D  ,   " Class
        dummy21 TYPE string,
        dummy22 TYPE string,       " profit center
        dummy23 TYPE string,    "Control code
        dummy24 TYPE string,
        dummy25 TYPE string,
        dummy26 TYPE string, "Standard price

      END OF ty_altuom.

TYPES: BEGIN OF ty_msg,
         matnr   TYPE matnr,
         msgty   TYPE msgty,
         message TYPE umk_y_message,
       END OF ty_msg.
*&---------------------------------------------------------------------*
*&    Internal table and Work Area Declaration
*&---------------------------------------------------------------------*

*****Internal Table and Work Area Declaration for Uploading the Data
DATA :it_mattab TYPE STANDARD TABLE OF ty_upload,
      it_altuom TYPE STANDARD TABLE OF ty_altuom.

DATA : l_mode TYPE char1 VALUE 'N'.
*DATA : it_bdcdata TYPE TABLE OF bdcdata,
*       wa_bdcdata TYPE bdcdata,
*       wa_msgcoll TYPE bdcmsgcoll,
*       it_msgcoll TYPE TABLE OF bdcmsgcoll,
*       it_err_msg TYPE TABLE OF bdcmsgcoll,
*       wa_err_msg TYPE bdcmsgcoll,

DATA:
  bdcdata     LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
  messtab     LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
  gwa_messtab LIKE LINE OF messtab,


  lt_msg      TYPE TABLE OF ty_msg,
  ls_msg      TYPE ty_msg,
*&---------------------------------------------------------------------*
*&    ALV Fucntion Declaration
*&---------------------------------------------------------------------*
  it_fcat     TYPE slis_t_fieldcat_alv,
  wa_fcat     TYPE slis_fieldcat_alv,
  wa_layo     TYPE slis_layout_alv.
*&---------------------------------------------------------------------*
*&    Global Variable Declarations
*&---------------------------------------------------------------------*

CONSTANTS: c_x      TYPE c VALUE 'X',
           c_itcat  TYPE mtpos_mara VALUE 'NORM',
           c_pind   TYPE perkz VALUE 'P',
           c_achk   TYPE mtvfp VALUE '01',
           c_expd   TYPE sled_bbd VALUE 'B',
           c_tgrp   TYPE tragr VALUE '0001',
           c_lgrp   TYPE ladgr VALUE '0004',
           c_pcntrl TYPE vprsv VALUE 'S',
           c_prunit TYPE peinh VALUE 1,
           c_taxcl  TYPE taxkm VALUE '0'.

DATA: wa_bapi_te_mara  TYPE bapi_te_mara,
      wa_bapi_te_marax TYPE bapi_te_marax,
      extensionin      TYPE TABLE OF bapiparex  WITH HEADER LINE,
      extensioninx     TYPE TABLE OF bapiparexx WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&  BAPI Declarations
*&---------------------------------------------------------------------*
DATA : wa_headdata             TYPE                   bapimathead,
       wa_clientdata           TYPE                   bapi_mara,
       wa_clientdatax          TYPE                   bapi_marax,
       wa_return               TYPE                   bapiret2,
       wa_return1              LIKE                   bapiret2,
       wa_plantdata            TYPE                   bapi_marc,
       wa_plantdatax           TYPE                   bapi_marcx,
       wa_materialdescription  TYPE                   bapi_makt,
       wa_salesdata            TYPE                   bapi_mvke,
       wa_salesdatax           TYPE                   bapi_mvkex,
       wa_unitsofmeasure       TYPE                   bapi_marm,
       wa_unitsofmeasurex      TYPE                   bapi_marmx,
       wa_taxclassifications   TYPE                   bapi_mlan,
       wa_storagelocationdata  TYPE                   bapi_mard,
       wa_storagelocationdatax TYPE                   bapi_mardx,
       wa_valuationdata        TYPE                   bapi_mbew,
       wa_valuationdatax       TYPE                   bapi_mbewx,
       wa_warehousenumberdata  TYPE                   bapi_mlgn,
       wa_warehousenumberdatax TYPE                   bapi_mlgnx,
       wa_storagetypedata      TYPE                   bapi_mlgt,
       wa_storagetypedatax     TYPE                   bapi_mlgtx,

       ist_materialdescription TYPE STANDARD TABLE OF bapi_makt INITIAL SIZE 0,
       ist_unitsofmeasure      TYPE STANDARD TABLE OF bapi_marm INITIAL SIZE 0,
       ist_unitsofmeasurex     TYPE STANDARD TABLE OF bapi_marmx INITIAL SIZE 0,
       ist_prtdata             TYPE STANDARD TABLE OF bapi_mfhm INITIAL SIZE 1,
       ist_materiallongtext    TYPE STANDARD TABLE OF bapi_mltx INITIAL SIZE 1,
       wa_materiallongtext     TYPE                   bapi_mltx,
       wa_prtdata              TYPE                   bapi_mfhm,
       ist_prtdatax            TYPE STANDARD TABLE OF bapi_mfhmx INITIAL SIZE 1,
       wa_prtdatax             TYPE                   bapi_mfhmx,
       wa_forecastdata         TYPE                   bapi_mpop,
       it_forecastdata         TYPE STANDARD TABLE OF bapi_mpop,
       wa_forecastdatax        TYPE bapi_mpopx,
       it_forecastdatax        TYPE STANDARD TABLE OF bapi_mpopx,
       ist_taxclassifications  TYPE STANDARD TABLE OF bapi_mlan INITIAL SIZE 0,
       ist_ins                 TYPE STANDARD TABLE OF bapi1001004_qmat  INITIAL SIZE 0,
       wa_ins                  TYPE                   bapi1001004_qmat,
       l_matnr                 TYPE                   matnr,
       it_return               TYPE TABLE OF bapiret2 INITIAL SIZE 0,
       it_return_tmp           TYPE TABLE OF bapiret2 INITIAL SIZE 0,
       it_insp                 TYPE TABLE OF bapi1001004_qmat INITIAL SIZE 0.
*&---------------------------------------------------------------------*
*   Classification BAPI Declarations
*&---------------------------------------------------------------------*

DATA: ls_objectkeynew      TYPE bapi1003_key-object,
      ls_objecttablenew    TYPE bapi1003_key-objecttable,
      ls_classnumnew       TYPE bapi1003_key-classnum,
      ls_classtypenew      TYPE bapi1003_key-classtype,
      ls_status            TYPE bapi1003_key-status,
      ls_standardclass     TYPE bapi1003_key-stdclass,
      ls_changenumber      TYPE bapi1003_key-changenumber,
      ls_keydate           TYPE bapi1003_key-keydate,
      ls_no_default_values TYPE bapi1003_key-flag,

      ls_classif_status    TYPE bapi1003_key-status,

      lt_allocvaluesnum    TYPE TABLE OF bapi1003_alloc_values_num,
      lt_allocvalueschar   TYPE TABLE OF bapi1003_alloc_values_char,
      lt_allocvaluescurr   TYPE TABLE OF bapi1003_alloc_values_curr,
      lt_return            TYPE TABLE OF bapiret2,
      ls_return            TYPE bapiret2,


      ls_allocvaluesnum    TYPE bapi1003_alloc_values_num,
      ls_allocvalueschar   TYPE bapi1003_alloc_values_char,
      ls_allocvaluescurr   TYPE bapi1003_alloc_values_curr,

*&---------------------------------------------------------------------*
*    BAPI BAPI_MATERIAL_GETINTNUMBER Declarations for Internal Number Range
*&---------------------------------------------------------------------*
      wa_ret               TYPE bapireturn1,
      it_matnr             TYPE STANDARD TABLE OF bapimatinr INITIAL SIZE 0,
      wa_matnr             TYPE bapimatinr,
      gv_mtart             TYPE bapimatdoa-matl_type,
      gv_mbrsh             TYPE bapimatdoa-ind_sector.

*******Local class definations****
CLASS lcl_material DEFINITION DEFERRED.
**********************************************************************Multiple Sheets excel upload data declaration
DATA : lt_records       TYPE solix_tab,
       lv_headerxstring TYPE xstring,
       lv_filelength    TYPE i.
*********************************************************************
DATA: l_t_data  TYPE w3mimetabtype,
      l_t_files TYPE filetable,
      l_rc      TYPE i.
FIELD-SYMBOLS : <gt_data> TYPE STANDARD TABLE.
DATA: lo_excel                TYPE REF TO zcl_excel.
