FUNCTION z_cert_pop_up_confirmation .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_CONTINUE) TYPE  XFELD
*"----------------------------------------------------------------------

* Clear status code for all attachments
  LOOP AT gt_docs INTO gwa_docs.
    IF gwa_docs-status IS NOT INITIAL.
      CLEAR gwa_docs-status.
      MODIFY gt_docs FROM gwa_docs.
    ENDIF.
  ENDLOOP.

  CALL SCREEN '0300' STARTING AT 30 10.

  ex_continue  = gv_continue.   "Continue or cancel

ENDFUNCTION.
