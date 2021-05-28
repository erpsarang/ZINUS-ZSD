*&---------------------------------------------------------------------*
*& Report ZSDR0051
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zsdr0051 message-id sy.

include zsdr0051_top.
include zsdr0051_f01.

start-of-selection.
  perform enqueue_document.
  case p_can.
    when space.
      perform create_invoice.
    when 'X'.
      perform cancel_invoice.
  endcase.
  perform dequeue_document.
