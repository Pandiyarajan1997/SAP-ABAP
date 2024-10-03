*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTRT_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS : SLIS,VRM.


*---------------------------------------------------------------------*
*       TABLES                                                        *
*---------------------------------------------------------------------*

TABLES  :   BSAK,             "Secondary Index Vendors (Cleared Items)
            BKPF,
            BSAD,             "Customer Accounting Docs
            PAYR,             "PAYMENT MEDIUM FILE.
            LFA1,             "VENDOR MASTER (GENERAL SECTION).
            LFB1,             "VENDOR MASTER (COMPANY CODE).
            KNB1,             "CUSTOMER MASTER(COMPANY CODE)
            ADRC,             "ADDRESSES (CENTRAL ADDRESS ADMIN.)
            T001,             "COMPANY CODES
            BNKA,             "BANK MASTER
            LFBK,             "Vendor Bank Data
            KNBK,             "Customer Bank Data
            KNA1,
            T012,             "House Banks
            T012K,            "House Bank Acount Ids
            BSEC,             "Document Posting One-Time data
            REGUP,            "Payment Document Items
            BSEG,             "Acc Doc Line Items
            BSIK,             "Secondary Index for Vendors - Advances
            BSID,             "Secondary Index for Customers - Advances
            BSIS,             "Secondary Index for G/L Accounts
            BSAS,             "Index for G/L Accounts (Cleared Items)
            WITH_ITEM,        "With Holding Tax Items
            BVOR,             "Intercompany posting procedures
            CEPC,             " PROFIT CENTER
            TGSB,             " BUSSENESS AREA
            USR21,
            ADR6,
            SSCRFIELDS.

TABLES  :   *BSAK,             "Vendor Accounting Docs
            *BSAD,             "Customer Accounting Docs
            *BSID,
            *BSIK.

TABLES : ZAXIS_TAB_CONVER,     "Field Conversions
         ZAXIS_TAB_CTLTA1,     "Control table
         ZAXIS_TAB_MAP,        "Mapping table for output structures
         ZAXIS_TAB_FLDLEN,     "Maximum Field Lengths
         ZAXIS_STR_DETAIL,     "Details Structure
         ZAXIS_STR_HEADER,     "Header Structure
         ZAXIS_TAB_PAYLOC,     "Print Location
         ZAXIS_TSB_MICR,
         ZAXIS_TAB_HB,
         ZAXIS_TAB_SEP.         "Axis Bank : Field Sepration


*---------------------------------------------------------------------*
* GLOBAL DATA OBJECTS (VARIABLES)
*---------------------------------------------------------------------*

DATA : GV_LIFNR           TYPE LIFNR,
       GV_ALT_LIFNR       TYPE LIFNR,
       GV_VENDOR_NAME     TYPE STRING,
       GV_ACC_NO          TYPE BANKN,
       GV_IFSC_CODE       TYPE CHAR15,
       GV_BUDAT           TYPE BUDAT,
       GV_EXTR_TIME       TYPE UZEIT,
       GV_EXTR_DATE       TYPE DATUM,
       GV_PAYMENT         TYPE CHAR03,
       GV_CORP_CODE       TYPE CHAR25,
       GV_PRODUCT_CODE    TYPE CHAR04,
       GV_FILENAME        TYPE STRING,
       GV_LOG_FILENAME    TYPE STRING,
       GV_AMOUNT          TYPE CHAR17,
       GV_NO_INV          TYPE I,
       GV_STATUS          TYPE CHAR30,
       GV_REMARKS         TYPE STRING,
       GV_TEXT            TYPE STRING,
       GV_TEXT1           TYPE STRING,
       GV_TEXT2           TYPE STRING,
       L_PATH1            TYPE STRING.  " export the file name for auto matic process



**&---------------------------------------------------------------------*
**&            data definations for file download.
**&---------------------------------------------------------------------*

DATA: L_FILE_COUNT            TYPE I,
      L_DIR_COUNT             TYPE I,
      L_WA_TAB_SDOKPATH       TYPE SDOKPATH,
      L_TAB_SDOKPATH          TYPE TABLE OF SDOKPATH,
      L_WA_TAB1_SDOKPATH      TYPE SDOKPATH,                "#EC NEEDED
      L_TAB1_SDOKPATH         TYPE TABLE OF SDOKPATH,
      L_SOURCE                TYPE STRING, "RLGRAP-FILENAME,
      L_DESTINATION           TYPE STRING, "RLGRAP-FILENAME,
      L_RETURN                TYPE I,
      L_FILEPATH_IMPORT       TYPE RLGRAP-FILENAME,
      L_FILEPATH_PROCESSED    TYPE RLGRAP-FILENAME,
      DIR                     TYPE STRING.

DATA : LP_FILEPATH      TYPE STRING,
       SP_FILEPATH      TYPE STRING,
       FTP_FILEPATH     TYPE STRING,
       FTP_FILE         TYPE STRING,
       LP_FILE          TYPE STRING,
       SHARED_FILE      TYPE STRING,
       LP_LOG_FILE      TYPE STRING,
       SP_FILE          TYPE STRING.


*---------------------------------------------------------------------*
* GLOBAL flags
*---------------------------------------------------------------------*

DATA : F_FIELD_EMPTY              TYPE C,
       F_PAY_MTD_ERROR            TYPE C,
       DOWNLOAD_ERROR(1)          TYPE C,
       FTP_DOWNLOAD_ERROR(1)      TYPE C,
       LP_DOWNLOAD_ERROR(1)       TYPE C,
       SP_DOWNLOAD_ERROR(1)       TYPE C,
       SHARED_DOWNLOAD_ERROR(1)   TYPE C,
       RTGS_TIME_VAL(1)           TYPE C,
       F_EDIT_BANK                TYPE C,
       F_EDIT_PAY_MTD             TYPE C.


DATA:   G_ERROR_PAYEE_DETAILS,      "(vendor number is not specified)
        G_ERROR_DEBIT_ACC_NO,       "(Debit A/C number is not specified)
        G_FILENAME                  TYPE LOCALFILE,           "File name to download the data's
        G_SER_FILENAME              TYPE LOCALFILE,           "Field name to download the data's in server
        G_FILEPATH_IMPORT           TYPE RLGRAP-FILENAME,     "Field name to download the data's in local
        G_FILEPATH_PROCESSED        TYPE RLGRAP-FILENAME,     "Field name to download the data's in local
        G_FILEPATH_PROCESSED_TMP    TYPE RLGRAP-FILENAME,
        G_LOCAL_PATH                TYPE SAEPFAD ,
        G_TABLENAME                 TYPE DDOBJNAME,           "Dynamic Table Names
        G_TABLE_FIELDNAME           TYPE FIELDNAME,           "Dynamic Table Field Names
        G_FIELD_VALUE(255)          TYPE C,                   "Field Value
        G_FIELD_CONCATENATE(255)    TYPE C,                   "Same Field with different seq. no.
        G_CROSS_COMPANY_CHECK(1)    TYPE C,                   "Cross Company Check.
        G_CONSTANT_VALUE(20)        TYPE C,                   "Concatenate constant field value
        G_FIELD_CHECK(30)           TYPE C,                   "Mandatory Field Value Checking.
        G_HKTID                     TYPE HBKID,               "Field For 4.7 version
        G_FIELD3(2)                 TYPE C,                   "Book Transfer
        GV_FLAG                     TYPE C,                   "House bank check
        GV_CHECK                    TYPE C,                   "Setting flag for error message
        GV_GL                       TYPE C,                   "Setting flag for G/L error message
        GV_MAN                      TYPE C,                   "Setting flag for both manual and automatic for required fields
        GV_POSTING                  TYPE C,                   "Flag for posting date and document number mismatch
        GV_CC_PAY                   TYPE C,
        RTGS_AMT_LIMIT(20)          TYPE N,
        IMPS_AMT_LIMIT(20)          TYPE N,
        RTGS_CUT_TIME               TYPE SY-UZEIT.

*---------------------------------------------------------------------*
*       TYPES
*---------------------------------------------------------------------*

TYPES : T_OUTPUTREC(2050).

TYPES: BEGIN OF T_REGUH.
INCLUDE TYPE REGUH.
TYPES YEAR(4) TYPE N.
TYPES END OF T_REGUH.

*---------------------------------------------------------------------*
*       TO CREATE LOG FILE ON USER LOCAL MACHINE
*---------------------------------------------------------------------*

TYPES : BEGIN OF TY_FNAME,
        TEXT(300) TYPE C,
        END OF TY_FNAME.

DATA : GIT_LOGFILE TYPE TABLE OF  TY_FNAME,
       WA_LOGFILE TYPE TY_FNAME.


*---------------------------------------------------------------------*
*       header global structure for store erquired values.
*---------------------------------------------------------------------*

TYPES : BEGIN OF TY_HEADER,
        CORP_CODE       TYPE CHAR20,
        LIFNR           TYPE LIFNR,
        VENDOR_NAME     TYPE CHAR40,
        ACC_NO          TYPE BANKN,
        IFSC_CODE       TYPE CHAR20,
        PRODUCT_CODE    TYPE CHAR04,
        NO_INV          TYPE I,
        PAYMENT         TYPE CHAR03,
        END OF TY_HEADER.

TYPES :  BEGIN OF TY_DISPLAY,
         CHK_BOX(1)       TYPE C,
         AUGBL          TYPE BSAK-AUGBL,
*         ZLSCH          TYPE BSAK-ZLSCH,
         BENE_CODE      TYPE CHAR10,
         BENE_NAME      TYPE CHAR50,
         DMBTR          TYPE DMBTR,
         BLART          TYPE BSAK-BLART,
         USNAM          TYPE BKPF-USNAM,
         TCODE          TYPE BKPF-TCODE,
         BUKRS          TYPE BSAK-BUKRS,
         GJAHR          TYPE BSAK-GJAHR,
         BUDAT          TYPE BSAK-BUDAT,
*         GSBER          TYPE GSBER,
         ZUONR          TYPE BSAK-ZUONR,
         ACC_NO         TYPE BANKN,
         IFSC           TYPE CHAR15,
         PAYMENT_TYPE   TYPE CHAR03,
         FIELD_STYLE    TYPE LVC_T_STYL,
         COLOUR         TYPE CHAR04,
         EMAIL          TYPE AD_SMTPADR,
 END OF TY_DISPLAY.


DATA :  HEADER TYPE TY_HEADER.
*---------------------------------------------------------------------*
*       DATA                     Variables
*---------------------------------------------------------------------*

DATA:   L_WA_OUTPUTREC     TYPE T_OUTPUTREC,
        LV_PAY             TYPE C,
        LV_MICR(6)         TYPE C,
        KINDEX1            TYPE SY-TABIX,
        G_POPUP_ANSWER     TYPE C,
        LV_TIME            TYPE SY-UZEIT.
*---------------------------------------------------------------------*
* CONSTANT DATA OBJECTS WITH VALUES
*---------------------------------------------------------------------*

CONSTANTS : C_BANK_CODE           TYPE ZAXIS_DTE_BANKS VALUE '001',     "axis
            C_NO_HDR_FIELDS       TYPE I VALUE 41,   "No Of Header Fields
            C_NO_DTL_FIELDS       TYPE I VALUE 7,    "No Of Detail Fields
            C_HDR_DELIMITER       VALUE '^',        "Header Delimiter
            C_DTL_DELIMITER       VALUE '^',        "Detail Delimiter
            C_CLOSE               VALUE '1',
            LC_DATE(2)            TYPE C VALUE '60',
            C_RED_COLOUR(5)       VALUE 'C600',
            C_GRN_COLOUR(5)       VALUE 'C110',
            C_GRN1_COLOUR(5)      VALUE 'C601',
            C_NRM_COLOUR(5)       VALUE 'C500',
            C_TRF(3)              VALUE 'TRF',
            C_NEFT(2)             VALUE 'NE',
            C_RTGS(2)             VALUE 'RT',
            C_CC(2)               VALUE 'CC',
            C_DD(2)               VALUE 'DD',
            C_FT(2)               VALUE 'FT',
            C_IMPS(2)             VALUE 'PA',
            C_NEFT_PRD_CODE(4)    VALUE 'NEFT',
            C_RTGS_PRD_CODE(4)    VALUE 'RTGS',
            C_CC_PRD_CODE(4)      VALUE 'CC',
            C_DD_PRD_CODE(4)      VALUE 'DD',
            C_FT_PRD_CODE(4)      VALUE 'FT',
            C_IMPS_PRD_CODE(4)    VALUE 'IMPS',
            C_CHEQUE(8)           VALUE 'CHEQUE',
            C_BANK_TRF(15)        VALUE 'BANK TRANSFER',
            C_TRANSFER(15)        VALUE 'NEFT/RTGS/TRF',
            C_VEND_DOC_TYPE(2)    VALUE 'KZ',
            C_VEND_ADV_DOC_TYPE(2) VALUE 'KA',
            ABAP_TRUE              TYPE C VALUE 'X',
            ABAP_FALSE             TYPE C VALUE ' '.




CONSTANTS : C_TCODE_EXTR      TYPE SYTCODE VALUE 'ZHDFC_EXTRAC', "Extraction
            C_TCODE_REXTR     TYPE SYTCODE VALUE 'ZHDFC_REXTRA',"Re-extraction
            C_XVORE           TYPE C VALUE 'X'.

*---------------------------------------------------------------------*
* BODY DECLARATIONS FOR PRE-DEFINED TABLES
*---------------------------------------------------------------------*

DATA: G_TAB_T001          TYPE TABLE OF T001,
      G_TAB_BSAK          TYPE TABLE OF BSAK     INITIAL SIZE 0,       "*Secondary Index for Vendors (Cleared Items)
      G_TAB_BSAD          TYPE TABLE OF BSAD     INITIAL SIZE 0,       "*Accounting: Secondary Index for Customers (Cleared Items)
      G_TAB_BSAK_PYMNT    TYPE STANDARD TABLE OF BSAK     INITIAL SIZE 0,       "*Accounting: Secondary Index for Vendors (Cleared Items)
      G_TAB_BSAD_PYMNT    TYPE STANDARD TABLE OF BSAD     INITIAL SIZE 0,       "*Accounting: Secondary Index for Customers (Cleared Items)
      G_TAB_REGUH         TYPE STANDARD TABLE OF T_REGUH  INITIAL SIZE 0,       "*Settlement data from payment program
      G_TAB_REGUV         TYPE STANDARD TABLE OF REGUV    INITIAL SIZE 0,       "*Settlement data from payment program
      G_TAB_PAYR          TYPE STANDARD TABLE OF PAYR     INITIAL SIZE 0,       "*Payment Medium File
      G_TAB_LFA1          TYPE STANDARD TABLE OF LFA1     INITIAL SIZE 0,       "*Vendor Master (General Section)
      G_TAB_LFB1          TYPE STANDARD TABLE OF LFB1     INITIAL SIZE 0,       "*Vendor Master (Company Code)
      G_TAB_LFB1_TEMP     TYPE STANDARD TABLE OF LFB1     INITIAL SIZE 0,       "*Vendor Master (Company Code)
      G_TAB_KNA1          TYPE STANDARD TABLE OF KNA1     INITIAL SIZE 0,       "*General Data in Customer Master
      G_TAB_ADRC          TYPE STANDARD TABLE OF ADRC     INITIAL SIZE 0,       "*Addresses (Business Address Services)
      G_TAB_ADR6          TYPE STANDARD TABLE OF ADR6     INITIAL SIZE 0,       "*EMAIL (Business Address Services)
      G_TAB_BNKA          TYPE STANDARD TABLE OF BNKA     INITIAL SIZE 0,       ""Bank master record
      G_TAB_LFBK          TYPE STANDARD TABLE OF LFBK     INITIAL SIZE 0,       "*Vendor Master (Bank Details)
      G_TAB_KNBK          TYPE STANDARD TABLE OF KNBK     INITIAL SIZE 0,       "*Customer Master (Bank Details)
      G_TAB_T012          TYPE STANDARD TABLE OF T012     INITIAL SIZE 0,       "*House Banks
      G_TAB_T012K         TYPE STANDARD TABLE OF T012K    INITIAL SIZE 0,       "*House Bank Accounts
      G_TAB_BSEC          TYPE STANDARD TABLE OF BSEC     INITIAL SIZE 0,       "*One-Time Account Data Document Segment
      G_TAB_REGUP         TYPE STANDARD TABLE OF REGUP    INITIAL SIZE 0,       "*Processed items from payment program
      G_TAB_BSEG          TYPE STANDARD TABLE OF BSEG     INITIAL SIZE 0,       "*Accounting Document Segment
      G_TAB_BSEG_RES      TYPE STANDARD TABLE OF BSEG     INITIAL SIZE 0,
      G_TAB_BSEG_RES1     TYPE STANDARD TABLE OF BSEG     INITIAL SIZE 0,
      G_TAB_BSIK          TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,       "*Accounting: Secondary Index for Vendors
      G_TAB_BSIK1         TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,
      G_TAB_BSIK2         TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,
      G_TAB_BSID          TYPE STANDARD TABLE OF BSID     INITIAL SIZE 0,       "*Accounting: Secondary Index for Customers
      G_TAB_BSIS          TYPE STANDARD TABLE OF BSIS     INITIAL SIZE 0,       "*Accounting: Secondary Index for G/L Accounts
      G_TAB_BSAS          TYPE STANDARD TABLE OF BSAS     INITIAL SIZE 0,       "*Accounting: Secondary Index for G/L Accounts (Cleared Items)
      G_TAB_DFIES         TYPE STANDARD TABLE OF DFIES    INITIAL SIZE 0,       "Structure: Table Fields for DDIF_FIELDINFO_GET
      G_TAB_WITH_ITEM     TYPE STANDARD TABLE OF WITH_ITEM INITIAL SIZE 0,      "*With Holding Tax Details
      G_TAB_BVOR          TYPE STANDARD TABLE OF BVOR     INITIAL SIZE 0,       "*Intercompany posting procedures
      G_TAB_BKPF          TYPE STANDARD TABLE OF BKPF     INITIAL SIZE 0,
      G_TAB_BKPF1         TYPE STANDARD TABLE OF BKPF     INITIAL SIZE 0,
      G_TAB_BSIK3         TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,
      G_TAB_BKPF_RES      TYPE STANDARD TABLE OF BKPF     INITIAL SIZE 0,
      LT_BSIK_TEMP        TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,
      G_TAB_BSIK_RES      TYPE STANDARD TABLE OF BSIK     INITIAL SIZE 0,
      G_TAB_USR21         TYPE STANDARD TABLE OF USR21    INITIAL SIZE 0.



*---------------------------------------------------------------------*
* BODY DECLARATIONS FOR USER-DEFINED TABLES
*---------------------------------------------------------------------*

DATA: G_TAB_ZAXIS_TAB_CONVERS TYPE STANDARD TABLE OF ZAXIS_TAB_CONVER INITIAL SIZE 0,
      GIT_CONVERS             TYPE STANDARD TABLE OF ZAXIS_TAB_CONVER INITIAL SIZE 0,
      G_TAB_ZAXIS_TAB_CTRLTAB TYPE STANDARD TABLE OF ZAXIS_TAB_CTLTA1 INITIAL SIZE 0,     "*Control Table Extracted Payment Documents
      GIT_CTRLTAB_TEMP        TYPE STANDARD TABLE OF ZAXIS_TAB_CTLTA1 INITIAL SIZE 0,
      G_TAB_ZAXIS_USER        TYPE STANDARD TABLE OF ZAXIS_USERS      INITIAL SIZE 0,             "* users Table
      G_TAB_ZAXIS_TAB_MAP     TYPE STANDARD TABLE OF ZAXIS_TAB_MAP    INITIAL SIZE 0,    "*Mapping Table For Output Structures
      G_TAB_ZAXIS_TAB_FLDLENG TYPE STANDARD TABLE OF ZAXIS_TAB_FLDLEN INITIAL SIZE 0,    "*Max Fieldlength Permissible
      G_TAB_ZAXIS_TAB_PAYLOC  TYPE STANDARD TABLE OF ZAXIS_TAB_PAYLOC INITIAL SIZE 0,    "*Pay and Print Locations
      GIT_PRINT_LOC           TYPE STANDARD TABLE OF ZAXIS_TAB_PAYLOC INITIAL SIZE 0,    "*Pay and Print Locations
      G_TAB_ZAXIS_TAB_SEP     TYPE STANDARD TABLE OF ZAXIS_TAB_SEP    INITIAL SIZE 0.


DATA : G_IT_OUTPUT            TYPE TABLE OF T_OUTPUTREC,
       G_IT_OUTPUT_TMP        TYPE TABLE OF T_OUTPUTREC,
       G_IT_CTLTAB            TYPE TABLE OF ZAXIS_TAB_CTLTAB,
       GIT_TAB_HB             TYPE TABLE OF ZAXIS_TAB_HB.

DATA : GIT_DISPLAY            TYPE TABLE OF ZAXIS_DISPLAY,
       GIT_SUCCESS_DISPLAY    TYPE TABLE OF ZAXIS_DISPLAY,
       GIT_ERROR_DISPLAY      TYPE TABLE OF ZAXIS_DISPLAY,
       GIT_AVOID_DISPLAY      TYPE TABLE OF ZAXIS_DISPLAY,
       GIT_EXCEPTION          TYPE TABLE OF ZAXIS_DISPLAY,
       GIT_LOG_TABLE          TYPE TABLE OF ZAXIS_TAB_CTLTA1,
       GIT_USERS              TYPE TABLE OF ZAXIS_USERS,
       GIT_DOC_TYPES          TYPE TABLE OF ZAXIS_DOC_TYPES,
       GIT_POST_USERS         TYPE TABLE OF ZAXIS_POST_USERS,
       GIT_PAY_MTD            TYPE TABLE OF ZAXIS_PAY_MTD,
       GIT_SEL_DISPLAY        TYPE TABLE OF TY_DISPLAY.


*---------------------------------------------------------------------*
* WORKAREA FOR USER-DEFINED TABLES
*---------------------------------------------------------------------*

DATA  :   WA_ZAXIS_TAB_CONVERS      TYPE ZAXIS_TAB_CONVER,      "*Field Conversion Table
          WA_ZAXIS_TAB_CONVERS1     TYPE ZAXIS_TAB_CONVER,
          WA_ZAXIS_TAB_CTRLTAB      TYPE ZAXIS_TAB_CTLTA1,       " *Control Table Extracted Payment Documents
          WA_ZAXIS_TAB_SEP          TYPE ZAXIS_TAB_SEP,
          WA_ZAXIS_TAB_MAP          TYPE ZAXIS_TAB_MAP,          "*Mapping Table For Output Structures
          WA_ZAXIS_TAB_MAP1         TYPE ZAXIS_TAB_MAP,
          WA_ZAXIS_TAB_FLDLENG      TYPE ZAXIS_TAB_FLDLEN,        "*Max Fieldlength Permissible
          WA_DOC_TYPES              TYPE ZAXIS_DOC_TYPES,
          WA_POST_USERS             TYPE ZAXIS_POST_USERS,
          WA_G_TAB_REGUH            TYPE REGUH,                   "*Structure: For BSIS
          WA_TAB_BKPF               TYPE BKPF,                    "*Structure: For BKPF
          WA_TAB_WITH_ITEM          TYPE WITH_ITEM,
          WA_PRINT_LOC              TYPE ZAXIS_TAB_PAYLOC. "*Structure: For WITH_ITEM (User Defined)


DATA :   WA_DISPLAY      TYPE  ZAXIS_DISPLAY,
         WA_EXCEPTION    TYPE  ZAXIS_DISPLAY,
         WA_LOG          TYPE  ZAXIS_TAB_CTLTA1,
         WA_LOG_TMP      TYPE  ZAXIS_TAB_CTLTA1,
         WA_CONVERS      TYPE  ZAXIS_TAB_CONVER,
         WA_PAY_MTD      TYPE  ZAXIS_PAY_MTD,
         WA_PAY_MTD1     TYPE  ZAXIS_PAY_MTD,
         WA_USERS        TYPE  ZAXIS_USERS,
         WA_HB           TYPE  ZAXIS_TAB_HB,
         WA_CTLTAB       TYPE  ZAXIS_TAB_CTLTAB,
         WA_OUTPUT       TYPE  T_OUTPUTREC,
         WA_SEL_CHANGE   TYPE  TY_DISPLAY,
         WA_SEL_DISPLAY  TYPE  TY_DISPLAY,
         WA_SEL_DISPLAY1 TYPE  TY_DISPLAY.


DATA :   WA_BSAK       TYPE BSAK,
         WA_BSAK1      TYPE BSAK,
         WA_BKPF       TYPE BKPF,
         WA_BSIS       TYPE BSIS,
         WA_BSAD       TYPE BSAD,
         WA_BSIK       TYPE BSIK,
         WA_BSIK1      TYPE BSIK,
         WA_BVOR       TYPE BVOR,
         WA_BSIK3      TYPE BSIK,
         BSEG1         TYPE BSEG,
         WA_T001       TYPE T001,
         BSAK1         TYPE BSAK,
         REGUH         TYPE T_REGUH,    "Structure: For REGUH (User Defined)
         WA_DFIES      TYPE DFIES.


*---------------------------------------------------------------------*
*       RANGES
*---------------------------------------------------------------------*

DATA : R_HKONT  TYPE RANGE OF HKONT,
       R_HKONT_LINE LIKE LINE OF R_HKONT,
       R_BLART  TYPE TABLE OF SELOPT,
       WA_BLART TYPE SELOPT,
       R_USNAM  TYPE TABLE OF SELOPT,
       WA_USNAM TYPE SELOPT,
       R_LIFNR  TYPE TABLE OF SELOPT,
       WA_LIFNR TYPE SELOPT,
       R_KUNNR  TYPE TABLE OF SELOPT,
       WA_KUNNR TYPE SELOPT.

*---------------------------------------------------------------------*
*       LIST BOX RELATED VARIABLES.
*---------------------------------------------------------------------*

DATA: NAME TYPE VRM_ID,
      LIST TYPE VRM_VALUES,
      VALUE TYPE VRM_VALUE.


*---------------------------------------------------------------------*
*       F4 HELP RELATED DATA DICLARATIONS
*---------------------------------------------------------------------*

DATA :IT_RETURN     TYPE TABLE OF DDSHRETVAL,
      WA_RETURN     TYPE DDSHRETVAL,
      IT_DYNPREAD   TYPE TABLE OF DYNPREAD,
      WA_DYNPREAD   TYPE DYNPREAD.


*---------------------------------------------------------------------*
*       alv related data declarations
*---------------------------------------------------------------------*

DATA: WA_FCAT_LVC        TYPE LVC_S_FCAT,
      GIT_FCAT_LVC       TYPE LVC_T_FCAT,
      WA_LAYOUT_LVC      TYPE LVC_S_LAYO,
      GIT_EVENTS         TYPE SLIS_T_EVENT,
      WA_EVENTS          TYPE SLIS_ALV_EVENT,
      WA_FCAT            TYPE SLIS_FIELDCAT_ALV,
      GIT_FCAT           TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT          TYPE SLIS_LAYOUT_ALV.

DATA : LS_VARIANT TYPE DISVARIANT.

DATA: GIT_HEADING     TYPE SLIS_T_LISTHEADER,
      WA_HEADING      TYPE SLIS_LISTHEADER.
DATA: GIT_EXCLUDE     TYPE SLIS_T_EXTAB,
      WA_EXCLUDE      TYPE SLIS_EXTAB,
      LS_STYLEROW     TYPE LVC_S_STYL,
      LT_STYLETAB     TYPE LVC_T_STYL,
      REF_GRID        TYPE REF TO CL_GUI_ALV_GRID,
       W_ANSWER       TYPE STRING,
       W_TOTAL        TYPE REGUP-WRBTR,
       W_TOTAL_C(25)  TYPE C,
       W_TITLE        TYPE LVC_TITLE..



*---------------------------------------------------------------------*
* FIELD SYMBOLS
*---------------------------------------------------------------------*

FIELD-SYMBOLS : <FS_OUTPUTSTRUC>      TYPE ANY,
                <FS_TABLE_FIELD>      TYPE ANY,       "Dynamic Table Field
                <FS_TABLE_NAME>       TYPE ANY ,       "Dynamic Table
                <FS_FIELD_VALUE>      TYPE ANY,       "Field Value
                <FS_ZAXIS_STR_DETAIL> TYPE ANY ,      "Zaxis_STR_DETAIL Str
                <FS_OUT>              TYPE ANY.       "Header/Detail Str

FIELD-SYMBOLS : <FS_ACCNO>            TYPE ANY,
                <FS_IFSCCODE>         TYPE ANY,
                <FS_VEN_NAME>         TYPE ANY,
                <FS_ALT_PAYE>         TYPE ANY,
                <FS_LOG>              TYPE ZAXIS_TAB_CTLTA1,
                <FS_DISPLAY>          TYPE ZAXIS_DISPLAY,
                <FS_BSAK>             TYPE BSAK.
