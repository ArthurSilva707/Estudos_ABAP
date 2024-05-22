*&---------------------------------------------------------------------*
*& Report ZART_DESTRUTORES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zart_destrutores.

CLASS lcl_loucutor DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome            TYPE string,
      idade           TYPE i,
      ultima_mensagem TYPE string.

    METHODS:
      constructor IMPORTING iv_nome  TYPE string
                            iv_idade TYPE i OPTIONAL,

      falar IMPORTING iv_mensagem        TYPE string       OPTIONAL
                      it_mensagens       TYPE string_table OPTIONAL
                      iv_ultima_mensagem TYPE boolean      OPTIONAL
                        PREFERRED PARAMETER iv_mensagem,

      dizer_ultima_mensagem.
ENDCLASS.
**********************************************************************

START-OF-SELECTION.
  DATA: lo_loucutor1 TYPE REF TO lcl_loucutor.

  CREATE OBJECT lo_loucutor1
    EXPORTING
      iv_nome  = 'Max'
      iv_idade = 23.

  lo_loucutor1->falar( 'Olá, sejam bem vindos' ).
  lo_loucutor1->falar( 'Quero café' ).

  lo_loucutor1->dizer_ultima_mensagem( ).

  lo_loucutor1->falar( 'Quero ir para casa!' ).
  lo_loucutor1->falar( 'Quero dormir' ).


  BREAK arthsantos .
*  CLEAR lo_locutor1
  FREE lo_loucutor1.
  BREAK arthsantos.





**********************************************************************
CLASS lcl_loucutor IMPLEMENTATION.
  METHOD constructor.
    nome  = iv_nome.
    idade = iv_idade.
  ENDMETHOD.

  METHOD falar.
    IF iv_ultima_mensagem IS NOT SUPPLIED.

      WRITE:/ 'O loucutor', nome COLOR COL_TOTAL, 'diz:' , iv_mensagem COLOR COL_POSITIVE.

    ELSE.

      WRITE:/ 'O loucutor', nome COLOR COL_TOTAL, 'diz novamente:' , iv_mensagem COLOR COL_GROUP.

    ENDIF.

    ultima_mensagem = iv_mensagem.
  ENDMETHOD.

  METHOD dizer_ultima_mensagem.
    DATA: ultima_mensagem TYPE string.

    me->falar( iv_mensagem        = me->ultima_mensagem
               iv_ultima_mensagem = abap_true ).

*    WRITE:/ 'Ultima mensagem dita:', me->ultima_mensagem COLOR COL_GROUP.
  ENDMETHOD.
ENDCLASS.