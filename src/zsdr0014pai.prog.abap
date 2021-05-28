*&---------------------------------------------------------------------*
*& Include          ZSDR0014PAI
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
*      PERFORM CREATE_PRICE.
      IF P_INFO IS NOT INITIAL.
        PERFORM CREATE_PRICE_MEK1_BDC.
      ELSE.
        PERFORM CREATE_PRICE_VK11_BDC.
      ENDIF.

    WHEN 'REFRESH'.
      PERFORM GET_DATA.
      FREE GO_DOCUMENT.
      CREATE OBJECT GO_DOCUMENT
        EXPORTING
          STYLE = 'TOP_OF_PAGE'.
      PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

    WHEN 'CONFIRM'.
      PERFORM CONFIRM.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
