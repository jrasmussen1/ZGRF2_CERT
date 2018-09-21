FUNCTION Z_CERT_POP_UP_ATEX .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_BOM_MAT) TYPE  ZTT_MATNR
*"  EXPORTING
*"     REFERENCE(EX_CONTINUE) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(CH_ATEX) TYPE  ZSCERT_ATEX
*"----------------------------------------------------------------------

  gv_atex = ch_atex.
  gt_bom_mat[] = im_bom_mat[].

  CALL SCREEN '0110' STARTING AT 30 10.

  ex_continue = gv_continue.   "Continue or cancel
  ch_atex = gv_atex.

ENDFUNCTION.
