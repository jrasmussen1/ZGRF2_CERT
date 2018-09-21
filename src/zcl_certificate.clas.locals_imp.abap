*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*CLASS lcl_handle_events IMPLEMENTATION.
*
*  METHOD on_double_click.
*
**  data: message type string.
**  data: row_c(4) type c.
**
**  row_c = row.
**  concatenate 'Row' row_c 'Column' column into message separated by space.
**  message i001(00) with 'You double-clicked on ' message.
*
**  zcl_certificate=>set_selected_row( row ).
**
**  me->gv_sel_row = row.
*
*  ENDMETHOD.
*
*ENDCLASS.
