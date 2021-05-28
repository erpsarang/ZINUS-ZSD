*&---------------------------------------------------------------------*
*& Include          ZSDR0100_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK = OK_CODE.
  CASE SAVE_OK.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR SAVE_OK.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_OK = OK_CODE.
  CASE SAVE_OK.
*    WHEN 'EXEC'.
*      gv_chk = gv_chk + 1.
*      PERFORM reserv_create.
*    WHEN 'DELT'.
*      PERFORM reserv_delete.
*    WHEN 'COMP'.
*      PERFORM reserv_complete.
*    WHEN 'CHAN'.
*      PERFORM reserv_change.
    WHEN 'FRESH'.
      PERFORM REFRSH_ALV.

*S_2021/3/5 add function BY E00064
    WHEN 'BILLING'.
      PERFORM CREATE_BILLING.
    WHEN 'BILLCAN'.
      PERFORM CANCEL_BILLING.
*E_2021/3/5 add function BY E00064

    WHEN 'PRICE'.
      PERFORM BDC_NEW_PRICING.
    WHEN 'PRINT'.
      PERFORM PRINT_INVOICE.
      IF GV_ERROR IS INITIAL.
        PERFORM CALL_SMARTFORMS.
      ENDIF.
    WHEN 'PDF'.
      PERFORM CONFIRM_POPUP USING TEXT-T09
                                  TEXT-T11
                                  CHANGING GV_ANSWER.
      IF GV_ANSWER EQ 1.
        PERFORM PRINT_INVOICE.
        IF GV_ERROR IS INITIAL.
          PERFORM DOWNLOAD_PDF.
        ENDIF.
      ELSE.
        MESSAGE S000 WITH 'Process Canceled' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'EMAIL'.
      PERFORM CONFIRM_POPUP USING TEXT-T09
                                  TEXT-T12
                                  CHANGING GV_ANSWER.
      IF GV_ANSWER EQ 1.
        PERFORM PRINT_INVOICE.
        IF GV_ERROR IS INITIAL.
          PERFORM SEND_EMAIL.
        ENDIF.
      ELSE.
        MESSAGE S000 WITH 'Process Canceled' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
  CLEAR SAVE_OK.
ENDMODULE.                 " USER_COM
