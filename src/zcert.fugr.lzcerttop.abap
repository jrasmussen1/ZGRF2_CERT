FUNCTION-POOL zcert.                        "MESSAGE-ID ..

TYPE-POOLS: icon.

DATA: gv_okcode    TYPE ok,
      gv_data      TYPE zscert_roughness,
      gv_atex      TYPE zscert_atex,
      gv_vibration TYPE zscert_vibration,
      gv_header    TYPE zscert_header,
      gv_continue  TYPE xfeld,
      gv_rb_treat1 TYPE xfeld VALUE ' ',
      gv_rb_treat2 TYPE xfeld VALUE 'X',
      gv_langu     TYPE lang_key,
      gv_cb_name2  TYPE xfeld VALUE ' '.

DATA: gt_bom_mat TYPE ztt_matnr.

DATA: gv_text_01 TYPE char50,
      gv_text_02 TYPE char50,
      gv_text_03 TYPE char50,
      gv_text_04 TYPE char50,
      gv_text_05 TYPE char50,
      gv_text_06 TYPE char50,
      gv_text_07 TYPE char50,
      gv_text_08 TYPE char50,
      gv_text_09 TYPE char50,
      gv_text_10 TYPE char50,
      gv_text_11 TYPE char50,
      gv_text_12 TYPE char50,
      gv_text_13 TYPE char50,
      gv_text_14 TYPE char50,
      gv_text_15 TYPE char50.

* Definitions for screen 250
CONTROLS gv_tc_0250 TYPE TABLEVIEW USING SCREEN 0250.

TYPES: BEGIN OF ty_docs,
         status          TYPE char9,
         desc            TYPE char40,
         fname           TYPE char100,
         documenttype    TYPE dokar,
         documentnumber  TYPE doknr,
         documentpart    TYPE doktl_d,
         documentversion TYPE dokvr,
       END OF ty_docs.

DATA: gt_docs  TYPE TABLE OF ty_docs WITH HEADER LINE,   "Need to be defined with header line, when used in table control.
      gwa_docs TYPE ty_docs.

DATA: gv_doc      TYPE bapi_doc_files2,
      gv_field    TYPE char30,
      gv_value    type ty_docs-fname,
      gv_line     type i.

DATA: gv_size TYPE int4,
      gt_doc_bin TYPE TABLE OF sdokcntbin.

DATA: go_html_container TYPE REF TO cl_gui_custom_container,
      go_html_viewer    TYPE REF TO cl_gui_html_viewer,
      gv_url(80)        TYPE c.
