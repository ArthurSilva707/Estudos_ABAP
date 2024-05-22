**&---------------------------------------------------------------------*
**& Report ZART_METODOS_OO
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zart_metodos_oo.

CLASS lcl_locutor DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome            TYPE string,
      idade           TYPE i,
      ultima_mensagem TYPE string.

*    CLASS-METHODS:
*      _gerar_locutor IMPORTING iv_nome    TYPE string
*                               iv_idade   TYPE i OPTIONAL
*                     EXPORTING so_locutor TYPE REF TO lcl_locutor
*                     RETURNING VALUE(instancia) TYPE REF TO lcl_locutor.


    METHODS:
      constructor IMPORTING iv_nome  TYPE string
                            iv_idade TYPE i OPTIONAL,

      falar IMPORTING iv_mensagem  TYPE string OPTIONAL
                      it_mensagens TYPE string_table OPTIONAL
                      PREFERRED PARAMETER iv_mensagem,

      possui_idade RETURNING VALUE(return) TYPE boolean.
ENDCLASS.
************************************************************

START-OF-SELECTION.
  DATA: lo_locutor1 TYPE REF TO lcl_locutor,
        lo_locutor2 TYPE REF TO lcl_locutor.

*  CREATE OBJECT lo_locutor1
*    EXPORTING
*      iv_nome  = 'Max'
*      iv_idade = 23.
*
*  CREATE OBJECT lo_locutor2
*    EXPORTING
*      iv_nome = 'Arthur'.
**      iv_idade = 23.
*
*  lo_locutor1->falar( iv_mensagem = 'Quero café!' ).
*  lo_locutor2->falar( iv_mensagem = 'NÓS É O TRAMPO E NÓS NÃO TREME!' ).
*
*
*  IF lo_locutor1->possui_idade( ) = abap_true.
*    WRITE:/ 'Sim, possui idade'.
*  ELSE.
*    WRITE:/ 'Não, não possui idade'.
*  ENDIF.
*
*
*  IF lo_locutor2->possui_idade( ) = abap_true.
*    WRITE:/ 'Sim, possui idade'.
*  ELSE.
*    WRITE:/ 'Não, não possui idade'.
*  ENDIF.

  CREATE OBJECT lo_locutor1
    EXPORTING
      iv_nome  = 'Arthur'
      iv_idade = 23.

  lo_locutor1->falar('Quero sabedoria' ).



***********************************************************

  CLASS lcl_locutor IMPLEMENTATION.
  METHOD constructor.
    nome = iv_nome.
    idade = iv_idade.
  ENDMETHOD.

  METHOD falar.
    WRITE:/'O locutor', nome COLOR COL_TOTAL, 'diz:', iv_mensagem COLOR COL_POSITIVE.
    ultima_mensagem = iv_mensagem.
  ENDMETHOD.

  METHOD possui_idade.
    IF idade IS INITIAL.
      return = abap_false.
    ELSE.
      return = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*  METHOD falar.
*    WRITE:/'O brabo:', nome COLOR COL_TOTAL, 'diz:', iv_mensagem COLOR COL_POSITIVE.
*    ultima_mensagem = iv_mensagem.
*  ENDMETHOD.


*CLASS lcl_loucutor DEFINITION.
*  PUBLIC SECTION.
*    DATA:
*      nome            TYPE string,
*      idade           TYPE i,
*      ultima_mensagem TYPE string.
*
**    CLASS-METHODS:
**      _gerar_loucutor IMPORTING iv_nome  TYPE string
**                                iv_idade TYPE i OPTIONAL
**                      EXPORTING eo_loucutor      TYPE REF TO lcl_loucutor
**                      RETURNING VALUE(instancia) TYPE REF TO lcl_loucutor.
*
*    METHODS:
*      constructor IMPORTING iv_nome  TYPE string
*                            iv_idade TYPE i OPTIONAL,
*
*      falar IMPORTING iv_mensagem  TYPE string       OPTIONAL
*                      it_mensagens TYPE string_table OPTIONAL
*                      PREFERRED PARAMETER iv_mensagem,
*
*      possui_idade RETURNING VALUE(return) TYPE boolean.
*ENDCLASS.
***********************************************************************
*
*START-OF-SELECTION.
*  DATA: lo_loucutor1 TYPE REF TO lcl_loucutor,
*        lo_loucutor2 TYPE REF TO lcl_loucutor.
*
*  CREATE OBJECT lo_loucutor1
*    EXPORTING
*      iv_nome  = 'Max'
*      iv_idade = 23.
*
*  CREATE OBJECT lo_loucutor2
*    EXPORTING
*      iv_nome  = 'José'.
*
*  lo_loucutor1->falar( iv_mensagem = 'Quero café!' ).
*  lo_loucutor2->falar( iv_mensagem = 'Quero ir para casa!' ).
*
*  IF lo_loucutor1->possui_idade( ) = abap_true.
*    WRITE:/ 'Sim, possui idade'.
*  ELSE.
*    WRITE:/ 'Não, não possui idade'.
*  ENDIF.
*
*  IF lo_loucutor2->possui_idade( ) = abap_true.
*    WRITE:/ 'Sim, possui idade'.
*  ELSE.
*    WRITE:/ 'Não, não possui idade'.
*  ENDIF.
*
*  CREATE OBJECT lo_loucutor1
*    EXPORTING
*      iv_nome  = 'Max'
*      iv_idade = 23.
*
*  lo_loucutor1->falar( 'Quero café' ).
*
*
***********************************************************************
*CLASS lcl_loucutor IMPLEMENTATION.
*  METHOD constructor.
*    nome  = iv_nome.
*    idade = iv_idade.
*  ENDMETHOD.
*
*  METHOD falar.
*    WRITE:/ 'O loucutor', nome COLOR COL_TOTAL, 'diz:' , iv_mensagem COLOR COL_POSITIVE.
*    ultima_mensagem = iv_mensagem.
*  ENDMETHOD.
*
*  METHOD possui_idade.
*    IF idade IS INITIAL.
*      return = abap_false.
*    ELSE.
*      return = abap_true.
*    ENDIF.
*  ENDMETHOD.
*ENDCLASS.