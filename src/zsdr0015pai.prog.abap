*&---------------------------------------------------------------------*
*& Include          ZSDR0015PAI
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
    WHEN 'REFRESH'.
      PERFORM GET_DATA.
      FREE GO_DOCUMENT.
      CREATE OBJECT GO_DOCUMENT
        EXPORTING
          STYLE = 'TOP_OF_PAGE'.
      PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
