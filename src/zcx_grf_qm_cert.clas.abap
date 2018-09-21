CLASS zcx_grf_qm_cert DEFINITION
  PUBLIC
  INHERITING FROM zcx_grf_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_form_not_found,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '050',
        attr1 TYPE scx_attrname VALUE 'GV_FORM_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_form_not_found .
    CONSTANTS:
      BEGIN OF gc_form_open_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE 'GV_FORM_NAME',
        attr2 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_form_open_error .
    CONSTANTS:
      BEGIN OF gc_form_excution_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE 'GV_FORM_NAME',
        attr2 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_form_excution_error .
    CONSTANTS:
      BEGIN OF gc_form_close_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'GV_FORM_NAME',
        attr2 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_form_close_error .
    CONSTANTS:
      BEGIN OF gc_classification_atinn,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE 'GV_CHARACTERIC_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_classification_atinn .
    CONSTANTS:
      BEGIN OF gc_classification_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_classification_error .
    CONSTANTS:
      BEGIN OF gc_classification_conv,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE 'GV_CHARACTERIC_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_classification_conv .
    CONSTANTS:
      BEGIN OF gc_classification_conv_no_num,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '043',
        attr1 TYPE scx_attrname VALUE 'GV_CHARACTERIC_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_classification_conv_no_num .
    CONSTANTS:
      BEGIN OF gc_open_dataset_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_open_dataset_error .
    CONSTANTS:
      BEGIN OF gc_change_doc_status,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_change_doc_status .
    CONSTANTS:
      BEGIN OF gc_doc_already_exists,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '062',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_doc_already_exists .
    CONSTANTS:
      BEGIN OF gc_error_when_create_dir,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '063',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_error_when_create_dir .
    CONSTANTS:
      BEGIN OF gc_error_when_release_set,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '064',
        attr1 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_error_when_release_set .
    CONSTANTS:
      BEGIN OF gc_read_doc_failed,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '065',
        attr1 TYPE scx_attrname VALUE 'GV_DOCUMENTTYPE',
        attr2 TYPE scx_attrname VALUE 'GV_DOCUMENTNUMBER',
        attr3 TYPE scx_attrname VALUE 'GV_DOCUMENTPART',
        attr4 TYPE scx_attrname VALUE 'GV_DOCUMENTVERSION',
      END OF gc_read_doc_failed .
    CONSTANTS:
      BEGIN OF gc_attachment_not_found,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '066',
        attr1 TYPE scx_attrname VALUE 'GV_DOCUMENTTYPE',
        attr2 TYPE scx_attrname VALUE 'GV_DOCUMENTNUMBER',
        attr3 TYPE scx_attrname VALUE 'GV_DOCUMENTPART',
        attr4 TYPE scx_attrname VALUE 'GV_DOCUMENTVERSION',
      END OF gc_attachment_not_found .
    CONSTANTS:
      BEGIN OF gc_error_func_module,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '067',
        attr1 TYPE scx_attrname VALUE 'GV_FM_NAME',
        attr2 TYPE scx_attrname VALUE 'GV_RETURN_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_error_func_module .
    CONSTANTS:
      BEGIN OF gc_file_size_not_found,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '080',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_file_size_not_found .
    CONSTANTS:
      BEGIN OF gc_email_error,
        msgid TYPE symsgid VALUE 'ZGRF_QM_CERT',
        msgno TYPE symsgno VALUE '054',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_email_error .
    CLASS-DATA gv_form_name TYPE fpname .
    CLASS-DATA gv_return_code TYPE num4 .
    CLASS-DATA gv_characteric_name TYPE atnam .
    CLASS-DATA gv_documenttype TYPE dokar .
    CLASS-DATA gv_documentnumber TYPE doknr .
    CLASS-DATA gv_documentpart TYPE doktl_d .
    CLASS-DATA gv_documentversion TYPE dokvr .
    CLASS-DATA gv_fm_name TYPE funcname .
    CLASS-DATA gv_errtext TYPE string .

    METHODS constructor
      IMPORTING
        !textid                LIKE if_t100_message=>t100key OPTIONAL
        !previous              LIKE previous OPTIONAL
        !gc_exc_type_technical TYPE z_exc_type DEFAULT 'TECHNICAL'
        !gc_exc_type_business  TYPE z_exc_type DEFAULT 'BUSINESS'
        !gv_form_name          TYPE fpname OPTIONAL
        !gv_return_code        TYPE num4 OPTIONAL
        !gv_characteric_name   TYPE atnam OPTIONAL
        !gv_documenttype       TYPE dokar OPTIONAL
        !gv_documentnumber     TYPE doknr OPTIONAL
        !gv_documentpart       TYPE doktl_d OPTIONAL
        !gv_documentversion    TYPE dokvr OPTIONAL
        !gv_fm_name            TYPE funcname OPTIONAL
        !gv_errtext            TYPE string OPTIONAL .

    METHODS log_exception
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_GRF_QM_CERT IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous              = previous
        gc_exc_type_technical = gc_exc_type_technical
        gc_exc_type_business  = gc_exc_type_business.
    me->gv_form_name = gv_form_name .
    me->gv_return_code = gv_return_code .
    me->gv_characteric_name = gv_characteric_name .
    me->gv_documenttype = gv_documenttype .
    me->gv_documentnumber = gv_documentnumber .
    me->gv_documentpart = gv_documentpart .
    me->gv_documentversion = gv_documentversion .
    me->gv_fm_name = gv_fm_name .
    me->gv_errtext = gv_errtext.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD log_exception.
    IF gv_errtext IS NOT INITIAL.
      me->add_text( gv_errtext ).
    ENDIF.

    CALL METHOD super->log_exception
      EXPORTING
        im_exc_type = im_exc_type
        im_exc_info = im_exc_info
        im_options  = im_options.

  ENDMETHOD.
ENDCLASS.
