*&---------------------------------------------------------------------*
*& Include          ZSDR0011I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE. " EXIT_9000 INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE OK_CODE.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
    WHEN 'COPY'.
      PERFORM COPY_DATA.
    WHEN 'DATE'.
      PERFORM SET_DATE.
    WHEN 'EXCEL'.
      PERFORM EXCEL_FILE_PATH.
  ENDCASE.
  CLEAR OK_CODE.

ENDMODULE. " USER_COMMAND_9000 INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_9100 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_DATE  INPUT
*&---------------------------------------------------------------------*
MODULE F4_DATE INPUT.

  DATA : BEGIN OF LT_LINE OCCURS 0,
           ZSTART LIKE ZSDT0020-ZSTART,
         END OF LT_LINE.

  _CLEAR LT_LINE.
  SELECT ZSTART
  INTO CORRESPONDING FIELDS OF TABLE LT_LINE
  FROM ZSDT0021
  WHERE ZCONFIRM = 'X'
    AND ZTYPE    = GV_ZTYPE.

  SORT LT_LINE BY ZSTART.
  DELETE ADJACENT DUPLICATES FROM LT_LINE COMPARING ZSTART.


  DATA : LV_SELECTFIELD  LIKE  HELP_INFO-FIELDNAME,
         LT_FIELDS       LIKE  HELP_VALUE OCCURS 0 WITH HEADER LINE,
         LV_SELECT_VALUE LIKE  HELP_INFO-FLDVALUE,
         LD_TABIX        LIKE  SY-TABIX.

  CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
  REFRESH: LT_FIELDS.

  LT_FIELDS-TABNAME = 'ZSDT0021'.
  LT_FIELDS-FIELDNAME = 'ZSTART'.
  LT_FIELDS-SELECTFLAG = 'X'.
  APPEND LT_FIELDS.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      SELECTFIELD                  = LV_SELECTFIELD
    IMPORTING
      IND                          = LD_TABIX
      SELECT_VALUE                 = LV_SELECT_VALUE
    TABLES
      FIELDS                       = LT_FIELDS
      FULL_TABLE                   = LT_LINE
    EXCEPTIONS
      FULL_TABLE_EMPTY             = 1
      NO_TABLESTRUCTURE_GIVEN      = 2
      NO_TABLEFIELDS_IN_DICTIONARY = 3
      MORE_THEN_ONE_SELECTFIELD    = 4
      NO_SELECTFIELD               = 5
      OTHERS                       = 6.
  CHECK NOT LD_TABIX IS INITIAL.
  READ TABLE LT_LINE INDEX LD_TABIX.

  GV_SDATE = LT_LINE-ZSTART.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9200 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.

  CASE OK_CODE.
    WHEN 'UPLOAD'.
      PERFORM UPLOAD_DATA.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR OK_CODE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_GV_FILE  INPUT
*&---------------------------------------------------------------------*
MODULE F4_GV_FILE INPUT.

  DATA : L_TITLE TYPE STRING,
         L_RC    TYPE SY-SUBRC,
         L_LEN   TYPE I,
         LT_FILE TYPE FILETABLE WITH HEADER LINE.

  CLEAR   : L_RC, LT_FILE, GV_PATH, L_LEN.
  REFRESH : LT_FILE.

  L_TITLE = TEXT-001.

*-- File open dialog
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = L_TITLE
      FILE_FILTER             = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
    CHANGING
      FILE_TABLE              = LT_FILE[]
      RC                      = L_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF NOT LT_FILE[] IS INITIAL.
*-- The length of file path should be in 128
      READ TABLE LT_FILE INDEX 1.
      IF SY-SUBRC EQ 0.
        L_LEN = STRLEN( LT_FILE ).
        IF L_LEN GE 128.
          MESSAGE S002 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ELSE.
          GV_PATH = LT_FILE.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE S003.
    ENDIF.
  ENDIF.

ENDMODULE.
