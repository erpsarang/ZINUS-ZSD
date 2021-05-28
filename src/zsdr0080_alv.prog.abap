*&---------------------------------------------------------------------*
*& Include          ZSDR0080_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER
    EXPORTING
      PARENT  = GC_DOCKING
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GC_CONTAINER
*&      <-- GC_GRID
*&---------------------------------------------------------------------*
FORM SET_GRID CHANGING PC_CONTAINER TYPE REF TO CL_GUI_CONTAINER
                       PC_GRID      TYPE REF TO CL_GUI_ALV_GRID.

  CREATE OBJECT PC_GRID
    EXPORTING
      I_PARENT = PC_CONTAINER.
*      i_appl_events = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_GRID
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV USING P_GRID.
  " Variant
  CLEAR GS_VARIANT.
  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = 'A'.
  GS_VARIANT-LOG_GROUP   = 'AAA'.

  " Layout
  PERFORM SET_LAYOUT.
  " Toolbar
  PERFORM SET_TOOLBAR.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG.
  " Event
  PERFORM SET_EVENT.
  " Sort
  PERFORM SET_SORT.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .
  CLEAR: GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT-ZEBRA      = ABAP_ON.
  GS_LAYOUT-SEL_MODE   = 'A'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR .
  _CLEAR: GT_TOOLBAR_EXCLUDE.

  SET_TOOLBAR_0100 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME

  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT USING:
*#1          #2  #3  #4  #5  #6  #7  #8  #9  #10         #11  #12    #13        #14
'AUART'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F44 ' ' '     ' 'AUART'     'VBAK',  " Order Type
'AEZEI'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F45 ' ' '     ' 'BEZEI'     'TVAKT', " Order Type Doc.
'VBELN'	     ' ' 'L' ' ' 'X' ' ' ' ' ' ' '    ' TEXT-F07 ' ' '     ' 'VBELN'     'VBAP',  " SO
'VTWEG'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' ''       ' ' '     ' 'VTWEG'     'VBAK',  " Distri chan.
'BSTKD'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F04 ' ' '     ' 'BSTKD'     'VBKD',  " Customer PO
'KUNNR'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F01 ' ' '     ' 'KUNNR'     'VBAK',  " Sold to-party
'NAME1'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F02 ' ' '     ' 'NAME1'     'KNA1',  " Sold to-party desc
'KUNWE'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G11 ' ' '     ' 'KUNNR'     'VBPA',  " Ship to-party
'SAME1'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F47 ' ' '     ' 'NAME1'     'ADRC',  " Ship to-party Name

'PRSDT'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G15 ' ' '     ' 'PRSDT'     'VBKD',  " Pricing Date
'KONDA'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G13 ' ' '     ' 'KONDA'     'VBKD',  " PRICE GRP
'KONDA_TXT'	 ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G14 ' ' '     ' 'VTEXT'     'T188T', " PRICE GRP TXT


'POSEX'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F05 ' ' '     ' 'POSEX'     'VBAP',  " Customer PO Item
'POSNR'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F08 ' ' '     ' 'POSNR'     'VBAP',  " SO Item
'MATNR'	     ' ' 'L' ' ' 'X' ' ' ' ' ' ' '    ' TEXT-F09 ' ' '     ' 'MATNR'     'VBAP',  " ZINUS SKU
'KDMAT'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F06 ' ' '     ' 'KDMAT'     'VBAP',  " Customer SKU
'MAKTX'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F71 ' ' '     ' 'MAKTX'     'MAKT',  " ZINUS SKU Description

'EDATU'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F59 ' ' '     ' 'EDATU'     'VBEP',  " Delivery Date
'ERDAT'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F03 ' ' '     ' 'ERDAT'     'VBAK',  " Order Creation date
'ABGRU'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F34 ' ' '     ' 'ABGRU'     'VBAP',  " Reject
'BEZEI'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F35 ' ' '     ' 'BEZEI'     'TVAGT', " Reject Dec

'PRODH_TXT'	 ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G04 ' ' '     ' 'VTEXT'     'T179T',  "Product hierarchy text
'KWMENG'     ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F10 ' ' 'VRKME' 'KWMENG'    'VBAP',  " SO item QTY
'REMAIN_QTY' ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G16 ' ' 'VRKME' 'KWMENG'    'VBAP',  " Remaining qty
'PR00'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F87 'WAERK' '   ' 'KBETR'   'KONP',   " PR00
'PR01'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G10 'WAERK' '   ' 'KBETR'   'KONP',   " PR01
'EDI1'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F88 'WAERK' '   ' 'KBETR'   'KONP',   " EDI1
'MWSI'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G07 'WAERK' '   ' 'NETWR'   'VBAP',   " VAT
'ZDRV'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G08 'WAERK' '   ' 'NETWR'   'VBAP',   " Delivery Fee
'ZDRX'       ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G09 'WAERK' '   ' 'NETWR'   'VBAP',   " Install Fee
'NETWR'      ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F57 'WAERK' '   ' 'NETWR'   'VBAP',  " SO ITEM Net VAlue

'LDDAT'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C700' TEXT-F86 ' ' '     ' 'LDDAT'     'VBEP',  " Loading Planned
'AUDAT'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C700' TEXT-F56 ' ' '     ' 'AUDAT'     'VBAK',  " Customer Purchase Date
'ERZET'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C700' TEXT-F74 ' ' '     ' 'ERZET'     'VBAK',  " Order Creation time
'VDATU'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C601' TEXT-F14 ' ' '     ' 'VDATU'     'VBAK',  " Customer Request date


'ICON_I'    ' ' 'C' ' ' 'X' ' ' ' ' ' ' '    ' TEXT-F15 ' ' '     ' 'ID'        'ICON',  " Incomp.
'BWTAR'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G12 ' ' '     ' 'BWTAR'     'VBAP',  " Val Type
'VBELV'	    ' ' 'L' ' ' 'X' ' ' ' ' ' ' 'C500' TEXT-F19 ' ' '     ' 'VBELN'     'LIPS',  " DO
'POSNV'	    ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C500' TEXT-F20 ' ' '     ' 'POSNR'     'LIPS',  " DO Item
'LFIMG'	    ' ' 'L' ' ' ' ' ' ' ' ' 'X' 'C500' TEXT-G06 ' ' 'VRKME' 'LFIMG'     'LIPS',  " DO qty
'WADAT_IST' ' ' 'L' ' ' ' ' ' ' ' ' ' ' 'C500' TEXT-F26 ' ' '     ' 'WADAT_IST' 'LIKP',  " GI DATE
'VBELB'	    ' ' 'L' ' ' 'X' ' ' ' ' ' ' '    ' TEXT-F29 ' ' '     ' 'VBELN'     'VBRP',  " Billing
'POSNB'	    ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F30 ' ' '     ' 'POSNR'     'VBRP',  " Billing Item
'BETWR'     ' ' 'R' ' ' ' ' ' ' ' ' 'X' '    ' TEXT-F69 'WAERK' '   ' 'NETWR'   'VBRP',  " Billing Net VAlue
'MWSBP'     ' ' 'R' ' ' ' ' ' ' ' ' 'X' '    ' TEXT-G17 'WAERK' '   ' 'MWSBP'   'VBRP',  " Billing VAT

'ZZKOINV'	  ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-G05 ' ' '     ' 'ZZKOINV'   'VBRK',  " KR Invoice
'WAERK'      ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F58 '     ' '   ' 'WAERK'   'VBAP',  " Currency
'VRKME'	     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F11 ' ' '     ' 'VRKME'     'VBAP',  " Unit

'WERKS'	    ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F18 ' ' '     ' 'WERKS'     'VBAP',  " Warehouse


'DOCNUM'     ' ' 'L' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F72 ' ' '     ' 'DOCNUM'    'ZSD1T0021',  " EDI Number
'ZIFTYPE'    ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F73 ' ' '     ' 'ZIFTYPE'   'ZSD1T0021',  " EDI Type

'VSBED'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F46 ' ' '     ' 'VSBED'     'VBAK',  " Shipping Conditions
'STREET'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F48 ' ' '     ' 'STREET'    'ADRC',  " Shipping Address

'POST_CODE1' ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F70 ' ' '     ' 'POST_CODE1' 'ADRC',  " Shipping ZIP Code
'CITY1'      ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F90 ' ' '     ' 'CITY1'      'ADRC',  " Shipping CITY
'REGION'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F91 ' ' '     ' 'REGION'     'ADRC',  " Shipping REGION

'LIFNR'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F49 ' ' '     ' 'LIFNR'     'VBPA',  " Carrier
'LAME1'	     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F50 ' ' '     ' 'NAME1'     'LFA1',  " Carrier Name

*'WGBEZ60'     ' ' 'L' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-F60 ' ' '     ' 'WGBEZ60'   'T023T', " Material GROUP


'RC00'       ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F98 'WAERK' '   ' 'KBETR'   'KONP',   " RC00
'EDID'       ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F99 'WAERK' '   ' 'KBETR'   'KONP',   " EDID
'CTAX'       ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-G01 'WAERK' '   ' 'KBETR'   'KONP',   " CTAX
'SALES_AMT'  ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C300' TEXT-F93 'WAERK' '   ' 'KBETR'   'KONP',   " DO Qty * EDI1
'SALES_TOT'  ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C300' TEXT-G02 'WAERK' '   ' 'KBETR'   'KONP',   " Sales Total
'SALES_DIFF' ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C700' TEXT-G03 'WAERK' '   ' 'KBETR'   'KONP',   " Sales Diff
'DIFF'       ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F89 'WAERK' '   ' 'KBETR'   'KONP',   " DIFF = PR00 - EDI1.


*'KWMENG'    ' ' 'R' ' ' ' ' ' ' ' ' ' ' '    ' TEXT-f10 ' ' 'VRKME' 'KWMENG'    'VBAP',  " QTY
*'VRKME'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-f11 ' ' '     ' 'VRKME'     'VBAP',  " Unit

*#1          #2  #3  #4  #5  #6  #7  #8  #9  #10         #11  #12    #13        #14
'BRGEW'     ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F51 ' ' 'GEWEI' 'BRGEW'     'VBAP',  " Gross Weight of the Item
'NTGEW'     ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F52 ' ' 'GEWEI' 'NTGEW'     'VBAP',  " Net Weight of the Item
'GEWEI'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F53 ' ' '     ' 'GEWEI'     'VBAP',  " Weight Unit
'VOLUM'     ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F54 ' ' 'VOLEH' 'VOLUM'     'VBAP',  " Volume of the item
'VOLEH'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F55 ' ' '     ' 'VOLEH'     'VBAP',  " Volume unit

'PSTYV'	    ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F12 ' ' '     ' 'PSTYV'     'VBAP',  " Item Type
'VTEXT'	    ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F13 ' ' '     ' 'VTEXT'     'TVAPT', " Item Type Dec.

'ICON_S'    ' ' 'C' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F16 ' ' '     ' 'ID'        'ICON',  " SO Ack
'STATUS'    ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F17 ' ' '     ' ' '         ' ',     " SO Status
'DDTEXT'    ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C601' TEXT-F32 ' ' '     ' 'DDTEXT'    'DD07V', " SO Status
'DO_VALUE'  ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F92 ' ' 'WAERK' 'KBETR'     'KONP',  " DO VALUE
'DO_VOLUME' ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F94 ' ' 'VOLEH' 'VOLUM'     'VBAP',  " DO Volume
'DO_WEIGHT' ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F95 ' ' 'GEWEI' 'BRGEW'     'VBAP',  " DO Weight

'TKNUM'	    ' ' 'L' 'X' 'X' ' ' ' ' ' ' 'C500' TEXT-F21 ' ' '     ' 'TKNUM'     'VTTK',  " Shipment
'EXTI2'	    ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F97 ' ' '     ' 'EXTI2'     'VTTK',  " ARN No
'MASTER_TR' ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-C01 ' ' '     ' 'EXIDV2'    'VEKP',  " Master Tr No.
'ICON_T'    ' ' 'C' 'X' 'X' ' ' ' ' ' ' 'C500' TEXT-F22 ' ' '     ' 'ID'        'ICON',  " Tracking
'QDATU'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F23 ' ' '     ' 'QDATU'     'LTAK',  " Picked
'RFMNG'     ' ' 'R' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F24 ' ' 'VRKME' 'RFMNG'     'VBFA',  " Pick QTY
'DALEN'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F25 ' ' '     ' 'DALEN'     'VTTK',  " Loaded
'ICON_P'    ' ' 'C' 'X' 'X' ' ' ' ' ' ' 'C500' TEXT-F27 ' ' '     ' 'ID'        'ICON',  " Transp
'PODAT'     ' ' 'L' 'X' ' ' ' ' ' ' ' ' 'C500' TEXT-F28 ' ' '     ' 'PODAT'     'LIKP',  " POD
'VBELF'	    ' ' 'C' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F29 ' ' '     ' 'ID'        'ICON',  " Billing


'ICON_D'    ' ' 'C' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F31 ' ' '     ' 'ID'        'ICON',  " IV Send

'N_BLDAT'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F61 ' ' '     ' 'BLDAT'     'LIKP',  " Released.
'N_DTDIS'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F62 ' ' '     ' 'DTDIS'     'VTTK',  " Carrier Planned
'N_QDATU'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F63 ' ' '     ' 'QDATU'     'LTAK',  " Picked
'N_DALEN'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F64 ' ' '     ' 'DALEN'     'VTTK',  " Loaded
'N_WADAT'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F65 ' ' '     ' 'WADAT_IST' 'LIKP',  " Goods Issued
'N_PTIME'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F66 ' ' '     ' 'WADAT_IST' 'LIKP',  " Carrier First Scanned
'N_PODAT'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F67 ' ' '     ' 'PODAT'     'LIKP',  " POD
'N_FKDAT'   ' ' 'L' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F68 ' ' '     ' 'FKDAT'     'VBRK',  " Invoiced


'855'       ' ' 'L' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F75 ' ' '     ' 'DOCNUM'    'ZSD1T0021', " 855
'855_S'     ' ' 'C' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F76 ' ' '     ' 'ID'        'ICON',      " 855 Status
'ASN'       ' ' 'L' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F77 ' ' '     ' 'DOCNUM'    'ZSD1T0021', " ASN
'ASN_S'     ' ' 'C' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F78 ' ' '     ' 'ID'        'ICON',      " ASN Status
'810'       ' ' 'L' 'X' 'X' ' ' ' ' ' ' '    ' TEXT-F79 ' ' '     ' 'DOCNUM'    'ZSD1T0021', " 810
'810_S'     ' ' 'C' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F80 ' ' '     ' 'ID'        'ICON',      " 810 Status

'GD'        ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F81 ' ' '     ' ' '         ' ',      " GD
'GS'        ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F82 ' ' '     ' ' '         ' ',      " GS
'GA'        ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F83 ' ' '     ' ' '         ' ',      " GA
'LIFEX'     ' ' 'R' 'X' ' ' ' ' ' ' ' ' '    ' TEXT-F96 ' ' '     ' ' '         ' '.      " BOL#

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT .
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'SALES_TOT'.
        GS_FIELDCAT-TOOLTIP = ' = PROO * DO Qty [KE24]'.
        MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
      WHEN 'SALES_AMT'.
        GS_FIELDCAT-TOOLTIP = ' = EDI1 * DO Qty'.
        MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REBUILD_LVC_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_FIELDCAT
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> TEXT_H01
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM REBUILD_LVC_CATALOG TABLES PT_FIELDCAT
                          USING P_FIELDNAME  "필드명칭
                                P_KEY        "키지정
                                P_JUST       "정렬
                                P_NO_OUT     "안보이기
                                P_HOTSPOT    "핫스팟
                                P_CHECKBOX   "체크박스
                                P_EDIT       "수정모드
                                P_DO_SUM     "합계
                                P_EMPHASIZE  "색강조
                                P_TITLE      "필드타이틀
                                P_CFIELDNAME "금액참조필드
                                P_QFIELDNAME "수량참조필드
                                P_FNAME      "Reference Field
                                P_TABNAME.   "Reference Table
  "필드 순번 증가
  GV_POS = GV_POS + 1.
  "입력된 값들 적용
  MOVE: P_FIELDNAME  TO GS_FIELDCAT-FIELDNAME,  "필드명
        P_KEY        TO GS_FIELDCAT-KEY,        "키지정
        P_JUST       TO GS_FIELDCAT-JUST,       "정렬
        P_NO_OUT     TO GS_FIELDCAT-NO_OUT,     "안보이기
        P_HOTSPOT    TO GS_FIELDCAT-HOTSPOT,    "핫스팟
        P_CHECKBOX   TO GS_FIELDCAT-CHECKBOX,   "체크박스
        P_EDIT       TO GS_FIELDCAT-EDIT,       "수정모드
        P_DO_SUM     TO GS_FIELDCAT-DO_SUM,     "합계
        P_EMPHASIZE  TO GS_FIELDCAT-EMPHASIZE,  "색강조
        P_CFIELDNAME TO GS_FIELDCAT-CFIELDNAME, "금액참조필드
        P_QFIELDNAME TO GS_FIELDCAT-QFIELDNAME, "수량참조필드
        P_FNAME      TO GS_FIELDCAT-REF_FIELD,  "Reference Field
        P_TABNAME    TO GS_FIELDCAT-REF_TABLE,  "Reference Table

        GV_POS       TO GS_FIELDCAT-COL_POS.    "순번
  "타이틀은 따로 입력된 경우만 적용
  MOVE: P_TITLE TO GS_FIELDCAT-COLTEXT,    "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_L,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_M,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_S.  "필드타이틀

  IF P_FIELDNAME = 'EXIDV2' OR
     P_FIELDNAME = 'INHALT'.
    GS_FIELDCAT-NO_CONVEXT = ABAP_ON.
  ENDIF.

  APPEND GS_FIELDCAT TO PT_FIELDCAT.
  CLEAR GS_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT .
  CALL METHOD GC_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER: G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR GC_GRID,
               G_EVENT_HANDLER->HANDLE_TOOLBAR       FOR GC_GRID,
               G_EVENT_HANDLER->HANDLE_USER_COMMAND  FOR GC_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT .
  _CLEAR: GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'AUART'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='2'.
  GS_SORT-FIELDNAME = 'AEZEI'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='3'.
  GS_SORT-FIELDNAME = 'VBELN'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='4'.
  GS_SORT-FIELDNAME = 'VTWEG'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='5'.
  GS_SORT-FIELDNAME = 'BSTKD'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='6'.
  GS_SORT-FIELDNAME = 'KUNNR'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='7'.
  GS_SORT-FIELDNAME = 'NAME1'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='8'.
  GS_SORT-FIELDNAME = 'KUNWE'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

  CLEAR GS_SORT.
  GS_SORT-SPOS ='9'.
  GS_SORT-FIELDNAME = 'SAME1'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY .
  CALL METHOD GC_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE
    CHANGING
      IT_OUTTAB                     = GT_DISP[]
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FREE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_GRID_1
*&---------------------------------------------------------------------*
FORM ALV_GRID_FREE USING PC_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF PC_GRID IS NOT INITIAL.
    CALL METHOD PC_GRID->FREE.
    CLEAR PC_GRID.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DOCKING_FREE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_DOCKING
*&---------------------------------------------------------------------*
FORM ALV_DOCKING_FREE USING PC_DOCKING TYPE REF TO CL_GUI_DOCKING_CONTAINER.
  IF PC_DOCKING IS NOT INITIAL.
    CALL METHOD PC_DOCKING->FREE.
    CLEAR PC_DOCKING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK USING P_ROW_ID TYPE LVC_S_ROW
                             P_COLUMN_ID TYPE LVC_S_COL
                             P_ROW_NO TYPE LVC_S_ROID.
  DATA: LT_SELTAB TYPE TABLE OF RSPARAMS,
        LS_SELTAB LIKE LINE OF LT_SELTAB,
        LV_ERDAT  LIKE VBAK-ERDAT,
        LV_TIME   LIKE ZSD1T0071-ZTIME.

  DATA: BEGIN OF LT_AR OCCURS 0,
          VBELN  LIKE ZSD1T0070-VBELN,
          INHALT LIKE ZSD1T0070-INHALT,
        END OF LT_AR.

  DATA: LS_DISP LIKE GT_DISP.
  CLEAR LS_DISP.

  READ TABLE GT_DISP INTO LS_DISP INDEX P_ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  CASE P_COLUMN_ID.
    WHEN 'MATNR'.
      IF LS_DISP-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
        SET PARAMETER ID 'MXX' FIELD 'K'.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELN'.
      IF LS_DISP-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD LS_DISP-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELV'.
      IF LS_DISP-VBELV IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD LS_DISP-VBELV.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'TKNUM'.
      IF LS_DISP-TKNUM IS NOT INITIAL.
        SET PARAMETER ID 'TNR' FIELD LS_DISP-TKNUM.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELF'.
      CLEAR: GT_BILLI. SORT GT_BILLI BY VGBEL VGPOS.
      _CLEAR: GT_VBELF.
      READ TABLE GT_BILLI WITH KEY VGBEL = LS_DISP-VBELV
                                   VGPOS = LS_DISP-POSNV.
      IF SY-SUBRC = 0.
        LOOP AT GT_BILLI FROM SY-TABIX.
          IF GT_BILLI-VGBEL = LS_DISP-VBELV AND
             GT_BILLI-VGPOS = LS_DISP-POSNV.
            MOVE-CORRESPONDING GT_BILLI TO GT_VBELF.
            _APPEND GT_VBELF.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE GT_BILLI WITH KEY VGBEL = LS_DISP-VBELN
                                     VGPOS = LS_DISP-POSNR.
        IF SY-SUBRC = 0.
          LOOP AT GT_BILLI FROM SY-TABIX.
            IF GT_BILLI-VGBEL = LS_DISP-VBELN AND
               GT_BILLI-VGPOS = LS_DISP-POSNR.
              MOVE-CORRESPONDING GT_BILLI TO GT_VBELF.
              _APPEND GT_VBELF.
            ELSE.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF GT_VBELF[] IS NOT INITIAL.
        CLEAR: GT_VBELF.
        SORT GT_VBELF BY AUBEL AUPOS VGBEL VGPOS VBELN POSNR.

        CALL SCREEN '0400' STARTING AT 20 20
                             ENDING AT 140 32.
      ENDIF.

    WHEN 'VBELB'.
      IF LS_DISP-VBELB IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD LS_DISP-VBELB.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'ICON_I'.
      IF LS_DISP-BSTKD IS NOT INITIAL.
        CLEAR: LT_SELTAB, LS_SELTAB, LV_ERDAT.
        SELECT SINGLE ERDAT INTO LV_ERDAT
          FROM VBAK
         WHERE VBELN EQ LS_DISP-VBELN.

        LS_SELTAB-SELNAME = 'S_BSTKD'.
        LS_SELTAB-KIND    = 'S'.
        LS_SELTAB-SIGN    = 'I'.
        LS_SELTAB-OPTION  = 'EQ'.
        LS_SELTAB-LOW     = LS_DISP-BSTKD.
        APPEND LS_SELTAB TO LT_SELTAB.

        LS_SELTAB-SELNAME = 'S_ERDAT'.
        LS_SELTAB-KIND    = 'S'.
        LS_SELTAB-SIGN    = 'I'.
        LS_SELTAB-OPTION  = 'EQ'.
        LS_SELTAB-LOW     = LV_ERDAT.
        APPEND LS_SELTAB TO LT_SELTAB.

        SUBMIT ZSD1R0070
          WITH SELECTION-TABLE LT_SELTAB
           AND RETURN.

      ENDIF.
    WHEN 'ICON_S'.
      IF LS_DISP-VBELN IS NOT INITIAL.
        CLEAR: GT_0023, GT_0025.
        SORT: GT_0025 BY VBELN POSNR.
        _CLEAR: GT_ACK.
        CLEAR : GS_0023.
        READ TABLE GT_0023 INTO GS_0023 WITH KEY VBELN = LS_DISP-VBELN.
        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING GS_0023 TO GT_ACK.
          GT_ACK-TYPE = '855'.
          _APPEND GT_ACK.
        ENDIF.

        CLEAR : GT_0025.
        LOOP AT GT_0025 INTO GS_0025 WHERE VBELN EQ LS_DISP-VBELN.
          CLEAR : GT_ACK.
          MOVE-CORRESPONDING GS_0025 TO GT_ACK.
          GT_ACK-TYPE = '865'.
          _APPEND GT_ACK.

          CLEAR : GS_0025.
        ENDLOOP.

        IF GT_ACK[] IS NOT INITIAL.
          CLEAR: GT_ACK. SORT GT_ACK BY DOCNUM.
          DELETE ADJACENT DUPLICATES FROM GT_ACK COMPARING DOCNUM.
          CLEAR: GT_ACK. SORT GT_ACK BY DOCNUM.

          CALL SCREEN '0200' STARTING AT 20 20
                               ENDING AT 140 32.
        ENDIF.

      ENDIF.
    WHEN 'ICON_T'.
      IF LS_DISP-VBELN IS NOT INITIAL.

        IF LS_DISP-VSBED = 'LT'.
          IF LS_DISP-LIFNR = '0000080038'.
            CLEAR: GS_VEKP.
            _CLEAR: GT_TRACK.
            LOOP AT GT_VEKP INTO GS_VEKP WHERE VBELN = LS_DISP-VBELV.
              MOVE-CORRESPONDING GS_VEKP TO GT_TRACK.
              _APPEND GT_TRACK.
              CLEAR : GS_VEKP.
            ENDLOOP.
          ELSE.
            READ TABLE GT_DELIV WITH KEY VGBEL = LS_DISP-VBELN
                                         VGPOS = LS_DISP-POSNR.
            IF SY-SUBRC = 0.
              _CLEAR: GT_TRACK.
              GT_TRACK-VBELN   = GT_DELIV-VBELN.
              GT_TRACK-EXIDV2  = GT_DELIV-LIFEX.
              GT_TRACK-INHALT  = GT_DELIV-LIFEX.
              _APPEND GT_TRACK.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR: GS_VEKP.
          _CLEAR: GT_TRACK.
          LOOP AT GT_VEKP INTO GS_VEKP WHERE VBELN = LS_DISP-VBELV.
            MOVE-CORRESPONDING GS_VEKP TO GT_TRACK.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GT_TRACK-EXIDV2
              IMPORTING
                OUTPUT = GT_TRACK-EXIDV2.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GT_TRACK-INHALT
              IMPORTING
                OUTPUT = GT_TRACK-INHALT.

            _APPEND GT_TRACK.

            CLEAR : GS_VEKP.
          ENDLOOP.
        ENDIF.

        IF GT_TRACK[] IS NOT INITIAL.
          CLEAR: GT_TRACK. SORT GT_TRACK BY VBELN EXIDV2 INHALT.

          CALL SCREEN '0600' STARTING AT 20 20
                               ENDING AT 140 32.
        ENDIF.

      ENDIF.
    WHEN 'ICON_P'.
      IF LS_DISP-VBELN IS NOT INITIAL.
        CLEAR: GT_0072. SORT GT_0072 BY VBELN INHALT.
        _CLEAR: GT_STATUS.

        LOOP AT GT_0072 WHERE VBELN = LS_DISP-VBELV.
          MOVE-CORRESPONDING GT_0072 TO GT_STATUS.
          _APPEND GT_STATUS.
        ENDLOOP.

        CLEAR: GT_STATUS.
        SORT GT_STATUS BY VBELN INHALT ZNUM.

        IF GT_STATUS[] IS NOT INITIAL.
          CALL SCREEN '0500' STARTING AT 20 20
                               ENDING AT 140 32.
        ENDIF.

      ENDIF.

    WHEN 'ICON_D'.
      IF LS_DISP-VBELN IS NOT INITIAL.
        _CLEAR: GT_IDOC.
        CLEAR : GS_0024.
        READ TABLE GT_0024 INTO GS_0024 WITH KEY VBELN = LS_DISP-VBELN.
        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING GS_0024 TO GT_IDOC.
          _APPEND GT_IDOC.
        ENDIF.

        IF GT_IDOC[] IS NOT INITIAL.
          CLEAR: GT_IDOC. SORT GT_IDOC BY DOCNUM VBELN.
          CALL SCREEN '0300' STARTING AT 20 20
                               ENDING AT 140 32.
        ENDIF.

      ENDIF.

    WHEN 'DOCNUM'.
      IF LS_DISP-DOCNUM IS NOT INITIAL AND
         LS_DISP-ZIFTYPE = 'I'.
        CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
          EXPORTING
            DOCNUM = LS_DISP-DOCNUM.
      ENDIF.
    WHEN '855'.
      IF LS_DISP-855 IS NOT INITIAL.
        CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
          EXPORTING
            DOCNUM = LS_DISP-855.
      ENDIF.
    WHEN 'ASN'.
      IF LS_DISP-ASN IS NOT INITIAL.
        CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
          EXPORTING
            DOCNUM = LS_DISP-ASN.
      ENDIF.
    WHEN '810'.
      IF LS_DISP-810 IS NOT INITIAL.
        CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
          EXPORTING
            DOCNUM = LS_DISP-810.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&---------------------------------------------------------------------*
FORM ALV_TOOLBAR USING P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                       P_INTERACTIVE TYPE C.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  LS_TOOLBAR-FUNCTION  = TEXT-D01.
  LS_TOOLBAR-BUTN_TYPE = '3'.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = 'REF'.
  LS_TOOLBAR-ICON      = ICON_REFRESH.
  LS_TOOLBAR-QUICKINFO = 'Refresh'.
  LS_TOOLBAR-TEXT      = 'Refresh'.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_UCOMM
*&---------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING PF_UCOMM TYPE SYUCOMM.
  CLEAR: GV_ERROR.

  CASE PF_UCOMM.
    WHEN 'REF'.
      PERFORM GET_BASE_DATA.
      PERFORM GET_STATUS_VBELN.
      CLEAR GT_DISP.
      SORT GT_DISP BY KUNNR VBELN POSNR.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_200 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_200
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_200
    EXPORTING
      PARENT  = GC_DOCKING_200
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_200->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_200 .
  " Variant
  CLEAR GS_VARIANT_200.
  GS_VARIANT_200-REPORT      = SY-REPID.
  GS_VARIANT_200-HANDLE      = 'B'.
  GS_VARIANT_200-LOG_GROUP   = 'BBB'.
  " Layout
  PERFORM SET_LAYOUT_200.
  " Toolbar
  PERFORM SET_TOOLBAR_200.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_200.
  " Event
  PERFORM SET_EVENT_200.
  " Sort
  PERFORM SET_SORT_200.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_200 .
  CLEAR: GS_LAYOUT_200.
  GS_LAYOUT_200-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_200-ZEBRA      = ABAP_ON.
  GS_LAYOUT_200-SEL_MODE   = 'A'.
  GS_LAYOUT_200-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_200 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_200.

  SET_TOOLBAR_0200 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_200 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_200.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_200 USING:
*#1       #2  #3  #4  #5  #6  #7  #8  #9  #10      #11 #12 #13      #14
'DOCNUM'  ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F36 ' ' ' ' 'DOCNUM' 'EDID4',
'VBELN'   ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F07 ' ' ' ' 'VBELN'  'VBAK',
'TYPE'    ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F37 ' ' ' ' ' '      ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_200 .
  CALL METHOD GC_GRID_200->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_200->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_200 FOR GC_GRID_200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_200 .
  CLEAR GS_SORT. _CLEAR GT_SORT_200.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'DOCNUM'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_200 .
  CALL METHOD GC_GRID_200->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_200
      IS_VARIANT                    = GS_VARIANT_200
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_200
    CHANGING
      IT_OUTTAB                     = GT_ACK[]
      IT_FIELDCATALOG               = GT_FIELDCAT_200
      IT_SORT                       = GT_SORT_200
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_200 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LS_ACK LIKE GT_ACK.
  CLEAR LS_ACK.

  IF P_ROW_ID-ROWTYPE+0(1) <> 'S'.
    READ TABLE GT_ACK INTO LS_ACK INDEX P_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.
    CASE P_COLUMN_ID.
      WHEN 'DOCNUM'.
        IF LS_ACK-DOCNUM IS NOT INITIAL.
          CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
            EXPORTING
              DOCNUM = LS_ACK-DOCNUM.
        ENDIF.
      WHEN 'VBELN'.
        IF LS_ACK-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD LS_ACK-VBELN.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_300 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_300
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_300
    EXPORTING
      PARENT  = GC_DOCKING_300
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_300->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_300.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_300 .
  " Variant
  CLEAR GS_VARIANT_300.
  GS_VARIANT_300-REPORT      = SY-REPID.
  GS_VARIANT_300-HANDLE      = 'C'.
  GS_VARIANT_300-LOG_GROUP   = 'CCC'.
  " Layout
  PERFORM SET_LAYOUT_300.
  " Toolbar
  PERFORM SET_TOOLBAR_300.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_300.
  " Event
  PERFORM SET_EVENT_300.
  " Sort
  PERFORM SET_SORT_300.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_300.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_300 .
  CLEAR: GS_LAYOUT_300.
  GS_LAYOUT_300-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_300-ZEBRA      = ABAP_ON.
  GS_LAYOUT_300-SEL_MODE   = 'A'.
  GS_LAYOUT_300-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_300 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_300.

  SET_TOOLBAR_0300 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_300 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_300.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_300 USING:
*#1       #2  #3  #4  #5  #6  #7  #8  #9  #10      #11 #12 #13      #14
'DOCNUM'  'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F36 ' ' ' ' 'DOCNUM' 'EDID4',
'VBELN'   ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F07 ' ' ' ' 'VBELN'  'VBAK',
'VBEVF'   ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F29 ' ' ' ' 'VBELN'  'VBRK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_300 .
  CALL METHOD GC_GRID_300->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_300->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_300 FOR GC_GRID_300.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_300 .
  CLEAR GS_SORT. _CLEAR GT_SORT_300.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'DOCNUM'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_300.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_300 .
  CALL METHOD GC_GRID_300->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_300
      IS_VARIANT                    = GS_VARIANT_300
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_300
    CHANGING
      IT_OUTTAB                     = GT_IDOC[]
      IT_FIELDCATALOG               = GT_FIELDCAT_300
      IT_SORT                       = GT_SORT_300
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_300 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LS_IDOC LIKE GT_IDOC.
  CLEAR LS_IDOC.

  IF P_ROW_ID-ROWTYPE+0(1) <> 'S'.
    READ TABLE GT_IDOC INTO LS_IDOC INDEX P_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.
    CASE P_COLUMN_ID.
      WHEN 'DOCNUM'.
        IF LS_IDOC-DOCNUM IS NOT INITIAL.
          CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
            EXPORTING
              DOCNUM = LS_IDOC-DOCNUM.
        ENDIF.
      WHEN 'VBELN'.
        IF LS_IDOC-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD LS_IDOC-VBELN.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VBEVF'.
        IF LS_IDOC-VBEVF IS NOT INITIAL.
          SET PARAMETER ID 'VF'   FIELD LS_IDOC-VBEVF.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        ENDIF.

    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_400 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_400
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_400
    EXPORTING
      PARENT  = GC_DOCKING_400
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_400->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_400.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_400 .
  " Variant
  CLEAR GS_VARIANT_400.
  GS_VARIANT_400-REPORT      = SY-REPID.
  GS_VARIANT_400-HANDLE      = 'D'.
  GS_VARIANT_400-LOG_GROUP   = 'DDD'.
  " Layout
  PERFORM SET_LAYOUT_400.
  " Toolbar
  PERFORM SET_TOOLBAR_400.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_400.
  " Event
  PERFORM SET_EVENT_400.
  " Sort
  PERFORM SET_SORT_400.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_400.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_400 .
  CLEAR: GS_LAYOUT_400.
  GS_LAYOUT_400-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_400-ZEBRA      = ABAP_ON.
  GS_LAYOUT_400-SEL_MODE   = 'A'.
  GS_LAYOUT_400-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_400 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_400.

  SET_TOOLBAR_0400 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_400 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_400.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_400 USING:
*#1     #2  #3  #4  #5  #6  #7  #8  #9  #10      #11 #12 #13      #14
'AUBEL' 'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F07 ' ' ' ' 'AUBEL' 'VBRP',
'AUPOS' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F08 ' ' ' ' 'AUPOS' 'VBRP',
'VGBEL' ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F19 ' ' ' ' 'VGBEL' 'VBRP',
'VGPOS' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F20 ' ' ' ' 'VGPOS' 'VBRP',
'VBELN' ' ' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F29 ' ' ' ' 'VBELN' 'VBRP',
'POSNR' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F30 ' ' ' ' 'POSNR' 'VBRP',
'SFAKN' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F38 ' ' ' ' 'SFAKN' 'VBRK',
'FKSTO' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F39 ' ' ' ' 'FKSTO' 'VBRK',
'FKDAT' 'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-G01 ' ' ' ' 'FKDAT' 'VBRK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_400 .
  CALL METHOD GC_GRID_400->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_400->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_400 FOR GC_GRID_400.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_400 .
  CLEAR GS_SORT. _CLEAR GT_SORT_400.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'AUBEL'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.

  GS_SORT-SPOS ='2'.
  GS_SORT-FIELDNAME = 'AUPOS'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.

  GS_SORT-SPOS ='3'.
  GS_SORT-FIELDNAME = 'VGBEL'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.

  GS_SORT-SPOS ='4'.
  GS_SORT-FIELDNAME = 'VGPOS'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.

  GS_SORT-SPOS ='5'.
  GS_SORT-FIELDNAME = 'VBELN'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.

  GS_SORT-SPOS ='6'.
  GS_SORT-FIELDNAME = 'POSNR'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_400.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_400 .
  CALL METHOD GC_GRID_400->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_400
      IS_VARIANT                    = GS_VARIANT_400
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_400
    CHANGING
      IT_OUTTAB                     = GT_VBELF[]
      IT_FIELDCATALOG               = GT_FIELDCAT_400
      IT_SORT                       = GT_SORT_400
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_400
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_400 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LS_VBELF LIKE GT_VBELF.
  CLEAR LS_VBELF.

  IF P_ROW_ID-ROWTYPE+0(1) <> 'S'.
    READ TABLE GT_VBELF INTO LS_VBELF INDEX P_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.
    CASE P_COLUMN_ID.
      WHEN 'AUBEL'.
        IF LS_VBELF-AUBEL IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD LS_VBELF-AUBEL.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VGBEL'.
        IF LS_VBELF-VGBEL IS NOT INITIAL.
          SET PARAMETER ID 'VL' FIELD LS_VBELF-VGBEL.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VBELN'.
        IF LS_VBELF-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'VF'   FIELD LS_VBELF-VBELN.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_500 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_500
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_500
    EXPORTING
      PARENT  = GC_DOCKING_500
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_500->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_500.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_500 .
  " Variant
  CLEAR GS_VARIANT_500.
  GS_VARIANT_500-REPORT      = SY-REPID.
  GS_VARIANT_500-HANDLE      = 'E'.
  GS_VARIANT_500-LOG_GROUP   = 'EEE'.
  " Layout
  PERFORM SET_LAYOUT_500.
  " Toolbar
  PERFORM SET_TOOLBAR_500.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_500.
  " Event
  PERFORM SET_EVENT_500.
  " Sort
  PERFORM SET_SORT_500.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_500.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_500 .
  CLEAR: GS_LAYOUT_500.
  GS_LAYOUT_500-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_500-ZEBRA      = ABAP_ON.
  GS_LAYOUT_500-SEL_MODE   = 'A'.
  GS_LAYOUT_500-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_500 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_500.

  SET_TOOLBAR_0500 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_500 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_500.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_500 USING:
*#1     #2  #3  #4  #5  #6  #7  #8  #9  #10       #11 #12 #13      #14
'VBELN'  'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F19 ' ' ' ' 'VBELN'  'ZSD1T0070',
'INHALT' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F22 ' ' ' ' 'INHALT' 'ZSD1T0070',
'ZCODE'  ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F40 ' ' ' ' 'ZCODE'  'ZSD1T0070',
'ZDESC'  ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F41 ' ' ' ' 'ZDESC'  'ZSD1T0070',
'ZTIME'  ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F42 ' ' ' ' 'ZTIME'  'ZSD1T0070'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_500 .
  CALL METHOD GC_GRID_500->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_500->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_500 FOR GC_GRID_500.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_500 .
  CLEAR GS_SORT. _CLEAR GT_SORT_500.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'VBELN'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_500.

  GS_SORT-SPOS ='2'.
  GS_SORT-FIELDNAME = 'INHALT'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_500.

  GS_SORT-SPOS ='3'.
  GS_SORT-FIELDNAME = 'ZTIME'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_500.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_500 .
  CALL METHOD GC_GRID_500->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_500
      IS_VARIANT                    = GS_VARIANT_500
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_500
    CHANGING
      IT_OUTTAB                     = GT_STATUS[]
      IT_FIELDCATALOG               = GT_FIELDCAT_500
      IT_SORT                       = GT_SORT_500
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_500
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_500 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LS_STATUS LIKE GT_STATUS.
  CLEAR LS_STATUS.

  IF P_ROW_ID-ROWTYPE+0(1) <> 'S'.
    READ TABLE GT_STATUS INTO LS_STATUS INDEX P_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.
    CASE P_COLUMN_ID.
      WHEN 'VBELN'.
        IF LS_STATUS-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'VL' FIELD LS_STATUS-VBELN.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_600 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_600
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_600
    EXPORTING
      PARENT  = GC_DOCKING_600
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_600->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_600.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_600 .
  " Variant
  CLEAR GS_VARIANT_600.
  GS_VARIANT_600-REPORT      = SY-REPID.
  GS_VARIANT_600-HANDLE      = 'F'.
  GS_VARIANT_600-LOG_GROUP   = 'FFF'.
  " Layout
  PERFORM SET_LAYOUT_600.
  " Toolbar
  PERFORM SET_TOOLBAR_600.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_600.
  " Event
  PERFORM SET_EVENT_600.
  " Sort
  PERFORM SET_SORT_600.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_600.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_600 .
  CLEAR: GS_LAYOUT_600.
  GS_LAYOUT_600-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_600-ZEBRA      = ABAP_ON.
  GS_LAYOUT_600-SEL_MODE   = 'A'.
  GS_LAYOUT_600-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_600 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_600.

  SET_TOOLBAR_0600 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_600 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_600.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_600 USING:
*#1     #2  #3  #4  #5  #6  #7  #8  #9  #10       #11 #12 #13      #14
'VBELN'  'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F19 ' ' ' ' 'VBELN'  'LIPS',
'EXIDV2' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F43 ' ' ' ' 'EXIDV2' 'VEKP',
'INHALT' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F22 ' ' ' ' 'INHALT' 'VEKP'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_600 .
  CALL METHOD GC_GRID_600->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_600->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_600 FOR GC_GRID_600.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_600 .
  CLEAR GS_SORT. _CLEAR GT_SORT_600.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'VBELN'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_600.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_600 .
  CALL METHOD GC_GRID_600->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_600
      IS_VARIANT                    = GS_VARIANT_600
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_600
    CHANGING
      IT_OUTTAB                     = GT_TRACK[]
      IT_FIELDCATALOG               = GT_FIELDCAT_600
      IT_SORT                       = GT_SORT_600
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_600
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_600 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LS_TRACK LIKE GT_TRACK.
  CLEAR LS_TRACK.

  IF P_ROW_ID-ROWTYPE+0(1) <> 'S'.
    READ TABLE GT_TRACK INTO LS_TRACK INDEX P_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.
    CASE P_COLUMN_ID.
      WHEN 'VBELN'.
        IF LS_TRACK-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'VL' FIELD LS_TRACK-VBELN.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER_700 .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING_700
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER_700
    EXPORTING
      PARENT  = GC_DOCKING_700
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER_700->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER_700.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_700 .
  " Variant
  CLEAR GS_VARIANT_700.
  GS_VARIANT_700-REPORT      = SY-REPID.
  GS_VARIANT_700-HANDLE      = 'G'.
  GS_VARIANT_700-LOG_GROUP   = 'GGG'.
  " Layout
  PERFORM SET_LAYOUT_700.
  " Toolbar
  PERFORM SET_TOOLBAR_700.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG_700.
  " Event
  PERFORM SET_EVENT_700.
  " Sort
  PERFORM SET_SORT_700.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY_700.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_700 .
  CLEAR: GS_LAYOUT_700.
  GS_LAYOUT_700-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT_700-ZEBRA      = ABAP_ON.
  GS_LAYOUT_700-SEL_MODE   = 'A'.
  GS_LAYOUT_700-NO_TOOLBAR = ABAP_ON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR_700 .
  _CLEAR: GT_TOOLBAR_EXCLUDE_700.

  SET_TOOLBAR_0700 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG_700 .
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT_700.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT_700 USING:
*#1      #2  #3  #4  #5  #6  #7  #8  #9  #10       #11 #12 #13      #14
'BSTKD'	 'X' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F04 ' ' ' ' 'BSTKD' 'VBKD',  " Customer PO
'VBELN'	 'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F07 ' ' ' ' 'VBELN' 'VBAP',  " SO
'VBELV'	 'X' 'L' ' ' 'X' ' ' ' ' ' ' ' ' TEXT-F19 ' ' ' ' 'VBELN' 'LIPS',  " DO
'LIFNR'	 ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F49 ' ' ' ' 'LIFNR' 'VBPA',  " Carrier
'LAME1'	 ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F50 ' ' ' ' 'NAME1' 'LFA1',  " Carrier Name
'INHALT' ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F22 ' ' ' ' 'INHALT' 'ZSD1T0070',
'FTIME'  ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F84 ' ' ' ' 'ZTIME'  'ZSD1T0070',
'TTIME'  ' ' 'L' ' ' ' ' ' ' ' ' ' ' ' ' TEXT-F85 ' ' ' ' 'ZTIME'  'ZSD1T0070'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_EVENT_700 .
  CALL METHOD GC_GRID_700->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID_700->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK_700 FOR GC_GRID_700.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SORT_700 .
  CLEAR GS_SORT. _CLEAR GT_SORT_700.

  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'BSTKD'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='2'.
  GS_SORT-FIELDNAME = 'VBELN'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='3'.
  GS_SORT-FIELDNAME = 'VBELV'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='4'.
  GS_SORT-FIELDNAME = 'INHALT'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='5'.
  GS_SORT-FIELDNAME = 'LIFNR'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='6'.
  GS_SORT-FIELDNAME = 'LAME1'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

  GS_SORT-SPOS ='6'.
  GS_SORT-FIELDNAME = 'INHALT'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT_700.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY_700 .
  CALL METHOD GC_GRID_700->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT_700
      IS_VARIANT                    = GS_VARIANT_700
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE_700
    CHANGING
      IT_OUTTAB                     = GT_INFO[]
      IT_FIELDCATALOG               = GT_FIELDCAT_700
      IT_SORT                       = GT_SORT_700
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK_700
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK_700 USING P_ROW_ID TYPE LVC_S_ROW
                                 P_COLUMN_ID TYPE LVC_S_COL
                                 P_ROW_NO TYPE LVC_S_ROID.
  DATA: LT_SELTAB TYPE TABLE OF RSPARAMS,
        LS_SELTAB LIKE LINE OF LT_SELTAB.

  DATA: LS_DISP LIKE GT_INFO.
  CLEAR LS_DISP.

  READ TABLE GT_INFO INTO LS_DISP INDEX P_ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  CASE P_COLUMN_ID.
    WHEN 'VBELN'.
      IF LS_DISP-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD LS_DISP-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELV'.
      IF LS_DISP-VBELV IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD LS_DISP-VBELV.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.
