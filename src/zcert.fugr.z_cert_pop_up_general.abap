FUNCTION Z_CERT_POP_UP_GENERAL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_TEXT_TAB) TYPE  LOP_TDLINE_TAB
*"     REFERENCE(IM_DOC_TAB) TYPE  TT_BAPI_DOC_FILES2
*"  EXPORTING
*"     REFERENCE(EX_CONTINUE) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(CH_CERT_HEADER) TYPE  ZSCERT_HEADER
*"----------------------------------------------------------------------

  gv_header = ch_cert_header.

  DATA: lv_num2(2) TYPE n,
        lv_fname   TYPE char10.

  LOOP AT im_text_tab ASSIGNING FIELD-SYMBOL(<l_txt_lin>).
    lv_num2 = sy-tabix.
    lv_fname =  'GV_TEXT_' && lv_num2.

    ASSIGN (lv_fname) TO FIELD-SYMBOL(<l_fname>).
    <l_fname> = <l_txt_lin>.
  ENDLOOP.

  IF gv_header-langu IS INITIAL.
    gv_langu = 'EN'.
  ELSE.
    CALL FUNCTION 'CONVERT_SAP_LANG_TO_ISO_LANG'
      EXPORTING
        input            = gv_header-langu
      IMPORTING
        output           = gv_langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
* Should never happen
      CHECK 1 = 1.
    ENDIF.
  ENDIF.

* Get documents - used in subscreen 250
  CLEAR gt_docs[].

  LOOP AT im_doc_tab INTO gv_doc.
    CLEAR gwa_docs.
    MOVE-CORRESPONDING gv_doc TO gwa_docs.
    gwa_docs-desc = gv_doc-description.
    gwa_docs-fname = gv_doc-docfile.
    APPEND gwa_docs TO gt_docs.
  ENDLOOP.


  CALL SCREEN '0200' STARTING AT 30 10.

  ex_continue = gv_continue.   "Continue or cancel
  ch_cert_header = gv_header.

ENDFUNCTION.



FORM get_doc_file USING u_doc TYPE ty_docs.

  DATA: lv_docdata TYPE bapi_doc_draw2.

  DATA: lt_docfiles  TYPE TABLE OF bapi_doc_files2,
        lwa_docfiles TYPE bapi_doc_files2.

  DATA: lv_bapireturn TYPE bapiret2.

  DATA: lt_access  TYPE TABLE OF scms_acinf,
        lwa_access TYPE scms_acinf.

  DATA: lv_tstamp TYPE tzonref-tstamps.


  CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
    EXPORTING
      documenttype    = u_doc-documenttype
      documentnumber  = u_doc-documentnumber
      documentpart    = u_doc-documentpart
      documentversion = u_doc-documentversion
      getactivefiles  = abap_true
    IMPORTING
      documentdata    = lv_docdata
      return          = lv_bapireturn
    TABLES
      documentfiles   = lt_docfiles.


  READ TABLE lt_docfiles INTO lwa_docfiles INDEX 1.
  CHECK sy-subrc = 0.


  CALL FUNCTION 'SCMS_DOC_READ'
    EXPORTING
      stor_cat              = lwa_docfiles-storagecategory
      doc_id                = lwa_docfiles-file_id
    TABLES
      access_info           = lt_access
      content_bin           = gt_doc_bin
    EXCEPTIONS
      bad_storage_type      = 1
      bad_request           = 2
      unauthorized          = 3
      comp_not_found        = 4
      not_found             = 5
      forbidden             = 6
      conflict              = 7
      internal_server_error = 8
      error_http            = 9
      error_signature       = 10
      error_config          = 11
      error_format          = 12
      error_parameter       = 13
      error                 = 14
      OTHERS                = 15.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_access INTO lwa_access INDEX 1.
  CHECK sy-subrc = 0.
  gv_size = lwa_access-comp_size.



  IF go_html_container IS NOT BOUND.
    CREATE OBJECT go_html_container
      EXPORTING
        container_name = 'PDF_VIEWER'.

    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = go_html_container.

  ENDIF.

  GET TIME STAMP FIELD lv_tstamp.
  gv_url = 'PDF' && lv_tstamp && '.pdf'.

  CALL METHOD go_html_viewer->load_data
    EXPORTING
      url          = gv_url
      size         = gv_size
      type         = 'text'
      subtype      = 'pdf'
    IMPORTING
      assigned_url = gv_url
    CHANGING
      data_table   = gt_doc_bin.

*  CALL METHOD go_html_viewer->enable_simplebrowsing( EXPORTING benable = 1 ).

*  CALL METHOD go_html_viewer->show_data
*    EXPORTING
*      url      = gv_url
*      in_place = 'X'.

  CALL METHOD go_html_viewer->show_url_in_browser
    EXPORTING
      url        = gv_url
    EXCEPTIONS
      cntl_error = 1
      others     = 2.

  IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    message i999(ZM) with 'Document displayed in browser'.
  ENDIF.

ENDFORM.
