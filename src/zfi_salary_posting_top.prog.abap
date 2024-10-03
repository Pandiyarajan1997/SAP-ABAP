*&---------------------------------------------------------------------*
*& Include          ZFI_SALARY_POSTING_TOP
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
class lcl_handle_events definition.
  public section.
    methods:
      on_user_command for event added_function of cl_salv_events
        importing e_salv_function.

endclass.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
class lcl_handle_events implementation.
  method on_user_command.
    perform handle_user_command using e_salv_function.
  endmethod.                    "on_user_command

endclass.                    "lcl_handle_events IMPLEMENTATION
