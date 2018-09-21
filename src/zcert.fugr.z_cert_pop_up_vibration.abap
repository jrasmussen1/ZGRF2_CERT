FUNCTION Z_CERT_POP_UP_VIBRATION .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_CONTINUE) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(CH_VIBRATION) TYPE  ZSCERT_VIBRATION
*"----------------------------------------------------------------------

  gv_vibration = ch_vibration.

  CALL SCREEN '0120' STARTING AT 30 10.

  ex_continue = gv_continue.   "Continue or cancel
  ch_vibration = gv_vibration.

ENDFUNCTION.
