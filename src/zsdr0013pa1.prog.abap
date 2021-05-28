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
    WHEN 'APPLY'.
      PERFORM CREATE_PRICE.

    WHEN 'REFRESH'.
      PERFORM GET_DATA.
      FREE GO_DOCUMENT.
      CREATE OBJECT GO_DOCUMENT
        EXPORTING
          STYLE = 'TOP_OF_PAGE'.
      PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

    WHEN 'ZSDC0010'.
      CALL TRANSACTION 'ZSDC0010'.
    WHEN 'ZSDR0011'.
      CALL TRANSACTION 'ZSDR0011'.
    WHEN 'ZSDR0012'.
      CALL TRANSACTION 'ZSDR0012'.
  ENDCASE.

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
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  CASE OK_CODE.
    WHEN 'APPLY'.
      PERFORM CREATE_PRICE_2.

    WHEN 'REFRESH'.
      PERFORM GET_DATA.
      FREE GO_DOCUMENT.
      CREATE OBJECT GO_DOCUMENT
        EXPORTING
          STYLE = 'TOP_OF_PAGE'.
      PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

    WHEN 'ZSDC0010'.
      CALL TRANSACTION 'ZSDC0010'.
    WHEN 'ZSDR0011'.
      CALL TRANSACTION 'ZSDR0011'.
    WHEN 'ZSDR0012'.
      CALL TRANSACTION 'ZSDR0012'.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9200  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_9200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.

  CASE OK_CODE.
    WHEN 'APPLY'.
      PERFORM CREATE_PRICE_2.

    WHEN 'REFRESH'.
      PERFORM GET_DATA.
      FREE GO_DOCUMENT.
      CREATE OBJECT GO_DOCUMENT
        EXPORTING
          STYLE = 'TOP_OF_PAGE'.
      PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

    WHEN 'ZSDC0010'.
      CALL TRANSACTION 'ZSDC0010'.
    WHEN 'ZSDR0011'.
      CALL TRANSACTION 'ZSDR0011'.
    WHEN 'ZSDR0012'.
      CALL TRANSACTION 'ZSDR0012'.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9200 INPUT
