*&---------------------------------------------------------------------*
*& Include          ZSDR0130F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .
  _CLEAR S_CREDAT.
  S_CREDAT-LOW = SY-DATUM(6) && '01'.
  S_CREDAT-HIGH = SY-DATUM.
  S_CREDAT-SIGN = 'I'.
  S_CREDAT-OPTION = 'BT'.
  APPEND S_CREDAT.

ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS TYPE LVC_S_MODI.

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'TRKORR'.

    ENDCASE.
  ENDLOOP.


ENDFORM. " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM.

  CASE P_UCOMM.

  ENDCASE.

ENDFORM. " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form SET_SELECTION
*&---------------------------------------------------------------------*
FORM SET_SELECTION .



ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_MSG
*&---------------------------------------------------------------------*
FORM POPUP_MSG USING P_MSG1 P_MSG2 PV_CHECK.

  CLEAR PV_CHECK.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = P_MSG1
      TEXT_QUESTION  = P_MSG2
      TEXT_BUTTON_1  = 'YES'
      TEXT_BUTTON_2  = 'NO'
    IMPORTING
      ANSWER         = PV_CHECK
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING P_ROW_ID
                                 P_COLUMN_ID.

  CLEAR GT_LIST.
  READ TABLE GT_LIST INDEX P_ROW_ID.
  CASE P_COLUMN_ID.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM ALPHA_INPUT  USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR   USING PE_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                            PE_INTERACTIVE.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.
  DATA: LS_UPLOAD  TYPE STB_BUTTON.

  CLEAR LS_UPLOAD.
  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.

*    LS_TOOLBAR-FUNCTION  = SPACE.
*    LS_TOOLBAR-ICON      = SPACE.
*    LS_TOOLBAR-BUTN_TYPE = '3'.
*    LS_TOOLBAR-DISABLED  = SPACE.
*    LS_TOOLBAR-TEXT      = SPACE.
*    LS_TOOLBAR-QUICKINFO = SPACE.
*    LS_TOOLBAR-CHECKED   = SPACE.
*    APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.
*
*    LS_TOOLBAR-FUNCTION  = 'DELETE'.
*    LS_TOOLBAR-ICON      = ICON_DELETE.
*    LS_TOOLBAR-BUTN_TYPE = SPACE.
*    LS_TOOLBAR-DISABLED  = SPACE.
*    LS_TOOLBAR-TEXT      = 'DELETE'.
*    LS_TOOLBAR-QUICKINFO = 'DELETE'.
*    LS_TOOLBAR-CHECKED   = SPACE.
*    APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    PE_ROW
                                   PE_COLUMN.

  CASE PE_COLUMN.
    WHEN 'VBELN'.
      CLEAR : GT_LIST.
      READ TABLE GT_LIST INDEX PE_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'AUN' FIELD GT_LIST-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'EBELN'.
      CLEAR : GT_LIST.
      READ TABLE GT_LIST INDEX PE_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'BES' FIELD GT_LIST-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

  DATA : LV_TEXT(255) TYPE C.

  CLEAR : LV_TEXT.
  DESCRIBE TABLE GT_LIST LINES DATA(LV_ROWS).
  LV_TEXT = LV_ROWS.
  CONDENSE LV_TEXT.
  CONCATENATE TEXT-001 ' :' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MEDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR : LV_TEXT.
  LV_TEXT = TEXT-004 && ' :' && GV_SUCCESS.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_EMPHASIS = CL_DD_AREA=>HEADING
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CLEAR : LV_TEXT.
  LV_TEXT = TEXT-005 && ' :' && GV_FAILURE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_EMPHASIS = CL_DD_AREA=>HEADING
      SAP_COLOR    = CL_DD_AREA=>LIST_NEGATIVE_INT.


  IF GO_HEADER IS INITIAL.

    CREATE OBJECT GO_HEADER
      EXPORTING
        PARENT = G_PARENT_HTML.

  ENDIF.

  CALL METHOD PE_DYNDOC_ID->MERGE_DOCUMENT.
  PE_DYNDOC_ID->HTML_CONTROL = GO_HEADER.

  CALL METHOD PE_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = G_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA : LT_EDID4    LIKE EDID4 OCCURS 0 WITH HEADER LINE,
         LT_MSG      TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
         LV_MSG(200).

  DATA : BEGIN OF LT_DATA OCCURS 0,
           DOCNUM LIKE EDIDC-DOCNUM,
           STATUS LIKE EDIDC-STATUS,
           CREDAT LIKE EDIDC-CREDAT,
           MESTYP LIKE EDIDC-MESTYP,
           COUNTR LIKE EDIDS-COUNTR,
           STAPA1 LIKE EDIDS-STAPA1,
           STAPA2 LIKE EDIDS-STAPA2,
           STAPA3 LIKE EDIDS-STAPA3,
           STAPA4 LIKE EDIDS-STAPA4,
           STAMID LIKE EDIDS-STAMID,
           STAMNO LIKE EDIDS-STAMNO,
         END OF LT_DATA.

  DATA : BEGIN OF LT_KEY OCCURS 0,
           DOCNUM LIKE EDIDC-DOCNUM,
           EBELN  LIKE EKKO-EBELN,
         END OF LT_KEY.

  SELECT A~DOCNUM
         A~STATUS
         A~CREDAT
         A~MESTYP
         B~COUNTR
         B~STAPA1
         B~STAPA2
         B~STAPA3
         B~STAPA4
         B~STAMID
         B~STAMNO
   INTO CORRESPONDING FIELDS OF TABLE LT_DATA
   FROM EDIDC AS A  INNER JOIN EDIDS AS B ON A~DOCNUM = B~DOCNUM
                                         AND A~STATUS = B~STATUS
    WHERE A~DIRECT = '2'
      AND A~RCVPRT = 'LS'
      AND A~DOCNUM IN S_DOCNUM
      AND A~MESTYP IN S_MESTYP
      AND A~IDOCTP = 'ORDERS05'
      AND A~CREDAT IN S_CREDAT
      AND A~STATUS IN ('51', '53').

  CHECK LT_DATA[] IS NOT INITIAL.

  SORT LT_DATA BY DOCNUM ASCENDING COUNTR DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING DOCNUM.

  _CLEAR LT_EDID4.
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE LT_EDID4
  FROM EDID4
  FOR ALL ENTRIES IN LT_DATA
  WHERE DOCNUM = LT_DATA-DOCNUM
    AND SEGNAM = 'E1EDK01'.

  LOOP AT LT_EDID4.
    LT_KEY-DOCNUM = LT_EDID4-DOCNUM.
    LT_KEY-EBELN  = LT_EDID4-SDATA+83(10).
    PERFORM ALPHA_INPUT USING LT_KEY-EBELN.
    COLLECT LT_KEY. CLEAR LT_KEY.
  ENDLOOP.

  SORT LT_KEY BY DOCNUM.

  SELECT A~EBELN, A~BEDAT,
         B~VBELN,
         C~IHREZ,
         D~LIFNR,
         E~NAME_ORG1 AS LIFNR_TXT
   INTO TABLE @DATA(LT_ORDER)
   FROM EKKO AS A LEFT OUTER JOIN VBKD   AS B  ON A~EBELN = B~BSTKD
                  LEFT OUTER JOIN VBAK   AS C  ON B~VBELN = C~VBELN
                  LEFT OUTER JOIN VBPA   AS D  ON C~VBELN = D~VBELN
                                              AND D~POSNR = '000000'
                                              AND D~PARVW = 'WL'
                  LEFT OUTER JOIN BUT000 AS E  ON D~LIFNR = E~PARTNER
    FOR ALL ENTRIES IN @LT_KEY
    WHERE A~EBELN = @LT_KEY-EBELN
      AND C~AUART = 'ZOR'.

  SORT LT_ORDER BY EBELN.

  _CLEAR GT_LIST.

  CLEAR : GV_SUCCESS, GV_FAILURE.

  LOOP AT LT_DATA.
    MOVE-CORRESPONDING LT_DATA TO GT_LIST.

    IF GT_LIST-STATUS EQ '51'.
      GT_LIST-ICON = ICON_RED_LIGHT.
      ADD 1 TO GV_FAILURE.
    ELSE.
      GT_LIST-ICON = ICON_GREEN_LIGHT.
      ADD 1 TO GV_SUCCESS.
    ENDIF.

    PERFORM GET_MESSAGE USING : LV_MSG
                                LT_DATA-STAPA1
                                LT_DATA-STAPA2
                                LT_DATA-STAPA3
                                LT_DATA-STAPA4
                                LT_DATA-STAMID
                                LT_DATA-STAMNO.
    GT_LIST-MESSAGE = LV_MSG.

    READ TABLE LT_KEY WITH KEY DOCNUM = GT_LIST-DOCNUM BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-EBELN = LT_KEY-EBELN.
      READ TABLE LT_ORDER INTO DATA(LS_ORDER) WITH KEY EBELN = GT_LIST-EBELN BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-BEDAT     = LS_ORDER-BEDAT.
        GT_LIST-VBELN     = LS_ORDER-VBELN.
        GT_LIST-LIFNR     = LS_ORDER-LIFNR.
        GT_LIST-LIFNR_TXT = LS_ORDER-LIFNR_TXT.
        IF LS_ORDER-IHREZ = 'OPEN_PO'.
          GT_LIST-OPEN_PO = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY DOCNUM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MESSAGE
*&---------------------------------------------------------------------*
FORM GET_MESSAGE  USING    PV_MSG
                           PV_STAPA1
                           PV_STAPA2
                           PV_STAPA3
                           PV_STAPA4
                           PV_STAMID
                           PV_STAMNO.

  CLEAR PV_MSG.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PV_STAMID
      MSGNR               = PV_STAMNO
      MSGV1               = PV_STAPA1
      MSGV2               = PV_STAPA2
      MSGV3               = PV_STAPA3
      MSGV4               = PV_STAPA4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_MSG.

ENDFORM.
