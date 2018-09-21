*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 05.01.2018 at 13:39:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZQCERT_COMP.....................................*
DATA:  BEGIN OF STATUS_ZQCERT_COMP                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_COMP                   .
CONTROLS: TCTRL_ZQCERT_COMP
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZQCERT_COMP_DATA................................*
DATA:  BEGIN OF STATUS_ZQCERT_COMP_DATA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_COMP_DATA              .
CONTROLS: TCTRL_ZQCERT_COMP_DATA
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZQCERT_EMAIL....................................*
DATA:  BEGIN OF STATUS_ZQCERT_EMAIL                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_EMAIL                  .
CONTROLS: TCTRL_ZQCERT_EMAIL
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZQCERT_FORMAT...................................*
DATA:  BEGIN OF STATUS_ZQCERT_FORMAT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_FORMAT                 .
CONTROLS: TCTRL_ZQCERT_FORMAT
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZQCERT_FORMS....................................*
DATA:  BEGIN OF STATUS_ZQCERT_FORMS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_FORMS                  .
CONTROLS: TCTRL_ZQCERT_FORMS
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZQCERT_GRADE....................................*
DATA:  BEGIN OF STATUS_ZQCERT_GRADE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_GRADE                  .
CONTROLS: TCTRL_ZQCERT_GRADE
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZQCERT_GRADE_TXT................................*
DATA:  BEGIN OF STATUS_ZQCERT_GRADE_TXT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_GRADE_TXT              .
CONTROLS: TCTRL_ZQCERT_GRADE_TXT
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZQCERT_ROUGHNESS................................*
DATA:  BEGIN OF STATUS_ZQCERT_ROUGHNESS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_ROUGHNESS              .
CONTROLS: TCTRL_ZQCERT_ROUGHNESS
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZQCERT_VENDOR...................................*
DATA:  BEGIN OF STATUS_ZQCERT_VENDOR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQCERT_VENDOR                 .
CONTROLS: TCTRL_ZQCERT_VENDOR
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZQCERT_COMP                   .
TABLES: *ZQCERT_COMP_DATA              .
TABLES: *ZQCERT_EMAIL                  .
TABLES: *ZQCERT_FORMAT                 .
TABLES: *ZQCERT_FORMS                  .
TABLES: *ZQCERT_GRADE                  .
TABLES: *ZQCERT_GRADE_TXT              .
TABLES: *ZQCERT_ROUGHNESS              .
TABLES: *ZQCERT_VENDOR                 .
TABLES: ZQCERT_COMP                    .
TABLES: ZQCERT_COMP_DATA               .
TABLES: ZQCERT_EMAIL                   .
TABLES: ZQCERT_FORMAT                  .
TABLES: ZQCERT_FORMS                   .
TABLES: ZQCERT_GRADE                   .
TABLES: ZQCERT_GRADE_TXT               .
TABLES: ZQCERT_ROUGHNESS               .
TABLES: ZQCERT_VENDOR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
