FUNCTION Z_CERT_POP_UP_ROUGHNESS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_BOM_MAT) TYPE  ZTT_MATNR
*"  EXPORTING
*"     REFERENCE(EX_CONTINUE) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(CH_ROUGHNESS) TYPE  ZSCERT_ROUGHNESS
*"----------------------------------------------------------------------


  gv_data = ch_roughness.
  gt_bom_mat[] = im_bom_mat[].

  CALL SCREEN '0100' STARTING AT 30 10.

  ex_continue = gv_continue.   "Continue or cancel
  ch_roughness = gv_data.

ENDFUNCTION.
