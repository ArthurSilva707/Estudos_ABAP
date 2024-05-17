*&---------------------------------------------------------------------*
*& Report ZART_INSTANCIASOO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zart_instanciasoo.

CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome            TYPE string,
      idade           TYPE i,
      ultima_mensagem TYPE string.

    METHODS:
      falar IMPORTING iv_mensagem TYPE string.
ENDCLASS.

START-OF-SELECTION.
  DATA: o_locutor TYPE REF TO lcl_locutor.

CREATE OBJECT o_Locutor.

WRITE:/ o_locutor->nome.


CLASS lcl_locutor IMPLEMENTATION.
  METHOD falar.
  ENDMETHOD.
ENDCLASS.