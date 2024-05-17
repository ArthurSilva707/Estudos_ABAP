*&---------------------------------------------------------------------*
*& Report ZART_ATRIBUTOSOO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zart_atributosoo.

CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      _instancia TYPE REF TO lcl_locutor,
      _nome      TYPE string.


    DATA:
      nome            TYPE string,
      idade           TYPE i,
      ultima_mensagem TYPE string.
    METHODS:
      falar IMPORTING iv_mensagem TYPE string.

    CLASS-METHODS:
      _gerar_instancia RETURNING VALUE(instancia) TYPE REF TO lcl_locutor.

ENDCLASS.


CLASS lcl_locutor IMPLEMENTATION.
  METHOD falar.
  ENDMETHOD.

  METHOD _gerar_instancia.
  ENDMETHOD.
ENDCLASS.