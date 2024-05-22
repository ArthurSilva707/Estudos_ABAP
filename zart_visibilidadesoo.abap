*&---------------------------------------------------------------------*
*& Report ZART_VISIBILIDADEOO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_VISIBILIDADEOO.

CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string,

      perguntar_nome IMPORTING io_pessoa TYPE REF TO lcl_pessoa,
      responder_nome.

  PRIVATE SECTION.
    DATA:
      nome TYPE string.

    METHODS:
      falar IMPORTING iv_frase TYPE string.
ENDCLASS.


START-OF-SELECTION.
  data: lo_pessoa_arthur TYPE REF TO lcl_pessoa,
        lo_pessoa_mariana   TYPE REF TO lcl_pessoa.

  CREATE OBJECT lo_pessoa_arthur
    EXPORTING
      iv_nome = 'Arthur'.

  CREATE OBJECT lo_pessoa_mariana
    EXPORTING
      iv_nome = 'Mariana'.

  lo_pessoa_mariana->perguntar_nome( lo_pessoa_arthur ).
  SKIP.
  lo_pessoa_arthur->perguntar_nome( lo_pessoa_mariana ).

CLASS lcl_pessoa IMPLEMENTATION.
  METHOD constructor.
    me->nome = iv_nome.
  ENDMETHOD.

  METHOD falar.
    WRITE:/ iv_frase.
  ENDMETHOD.

  METHOD perguntar_nome.
    me->falar( 'Qual o seu nome?' ).
    io_pessoa->responder_nome( ).
  ENDMETHOD.

  METHOD responder_nome.
    me->falar( | Meu nome é { me->nome }| ).
  ENDMETHOD.
ENDCLASS.