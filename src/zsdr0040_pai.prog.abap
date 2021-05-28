*----------------------------------------------------------------------*
***INCLUDE ZSDR0040_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       BACK, EXIT, CANC
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN OTHERS.
      CHECK 1 = 1.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'ENTE'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      CHECK 1 = 1.
  ENDCASE.
ENDMODULE.
