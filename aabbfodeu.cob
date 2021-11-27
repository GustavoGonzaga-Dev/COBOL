
      ******************************************************************
      * Author: Samuel Marcilio Mena
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP003.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPROD ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ERRO
                    ALTERNATE RECORD KEY IS DESCRICAO WITH DUPLICATES.

           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ERRO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES.

           SELECT CADFORN ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CNPJ
                    FILE STATUS  IS ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       FD CADPROD
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPROD.DAT".
       01 REGPROD.
           03 CODIGO          PIC 9(06).
           03 DESCRICAO       PIC X(30).
           03 UNIDADE         PIC X(02).
           03 APLICACAO       PIC 9(01).
           03 PRECO           PIC 9(07)V99.
           03 QUANTIDADE      PIC 9(05)V9.
           03 CNPJ-PROD       PIC 9(15).

       FD CADFORN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFORN.DAT".
       01 REGFORN.
           03 CNPJ            PIC 9(15).
           03 RAZAO           PIC X(40).
           03 NOME            PIC X(12).
           03 NUMERO          PIC 9(04).
           03 COMPLEMENTO     PIC X(10).
           03 CEP-FORN        PIC 9(08).

       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CEP             PIC 9(08).
           03 ENDERECO        PIC X(40).
           03 BAIRRO          PIC X(20).
           03 CIDADE          PIC X(20).
           03 UF              PIC X(02).

       WORKING-STORAGE SECTION.
       77 ERRO           PIC X(02) VALUE "00".
       77 MENSAGEM       PIC X(50) VALUE SPACES.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       01 UF-EXT         PIC X(19) VALUE SPACES.
       01 ITERATOR       PIC 9(02) VALUE ZEROS.

       01 BUFFER-UNIDADES.
           03 FILLER PIC X(12) VALUE "LTLITRO     ".
           03 FILLER PIC X(12) VALUE "PCPECA      ".
           03 FILLER PIC X(12) VALUE "KGTONELADA  ".
           03 FILLER PIC X(12) VALUE "ARARROBA    ".
           03 FILLER PIC X(12) VALUE "GRGRAMA     ".
           03 FILLER PIC X(12) VALUE "MTMETRO     ".
           03 FILLER PIC X(12) VALUE "CMCENTIMETRO".
           03 FILLER PIC X(12) VALUE "CJCONJUNTO  ".

       01 VECTOR-UNIDADES REDEFINES BUFFER-UNIDADES.
           03 VECTOR-UNIDADE PIC X(12) OCCURS 8 TIMES.

       01 VIEW-UNIDADES.
           03 VIEW-UNIDADE-SIGRA PIC X(02) VALUE SPACES.
           03 VIEW-UNIDADE-EXT   PIC X(10) VALUE SPACES.

       01 BUFFER-APLICACOES.
           03 FILLER PIC 9(01) VALUE 1.
           03 FILLER PIC X(15) VALUE "SAUDE          ".
           03 FILLER PIC 9(01) VALUE 2.
           03 FILLER PIC X(15) VALUE "ELETRONICO     ".
           03 FILLER PIC 9(01) VALUE 3.
           03 FILLER PIC X(15) VALUE "HIGIENE PESSOAL".
           03 FILLER PIC 9(01) VALUE 4.
           03 FILLER PIC X(15) VALUE "ALIMENTACAO    ".
           03 FILLER PIC 9(01) VALUE 5.
           03 FILLER PIC X(15) VALUE "LIMPESA        ".
           03 FILLER PIC 9(01) VALUE 6.
           03 FILLER PIC X(15) VALUE "SEGURANCA      ".
           03 FILLER PIC 9(01) VALUE 7.
           03 FILLER PIC X(15) VALUE "GESTAO         ".
           03 FILLER PIC 9(01) VALUE 8.
           03 FILLER PIC X(15) VALUE "VESTUARIO      ".
           03 FILLER PIC 9(01) VALUE 9.
           03 FILLER PIC X(15) VALUE "OUTRAS         ".

       01 VECTOR-APLICACOES REDEFINES BUFFER-APLICACOES.
           03 VECTOR-APLICACAO OCCURS 9 TIMES.
               05 FILLER PIC 9(01).
               05 FILLER PIC X(15).

       01 VIEW-APLICACOES.
           03 VIEW-APLICACAO-NUMERO PIC 9(01) VALUE ZEROS.
           03 VIEW-APLICACAO-EXT    PIC X(15) VALUE SPACES.

       01 BUFFER-UFS.
           03 FILLER    PIC X(21) VALUE "ACACRE               ".
           03 FILLER    PIC X(21) VALUE "ALALAGOAS            ".
           03 FILLER    PIC X(21) VALUE "APAMAPA              ".
           03 FILLER    PIC X(21) VALUE "AMAMAZONAS           ".
           03 FILLER    PIC X(21) VALUE "BABAHIA              ".
           03 FILLER    PIC X(21) VALUE "CECEARA              ".
           03 FILLER    PIC X(21) VALUE "ESESPIRITO SANTO     ".
           03 FILLER    PIC X(21) VALUE "GOGOIAS              ".
           03 FILLER    PIC X(21) VALUE "MAMARANHAO           ".
           03 FILLER    PIC X(21) VALUE "MTMATO GROSSO        ".
           03 FILLER    PIC X(21) VALUE "MSMATO GROSSO DO SUL ".
           03 FILLER    PIC X(21) VALUE "MGMINAS GERAIS       ".
           03 FILLER    PIC X(21) VALUE "PAPARA               ".
           03 FILLER    PIC X(21) VALUE "PBPARAIBA            ".
           03 FILLER    PIC X(21) VALUE "PRPARANA             ".
           03 FILLER    PIC X(21) VALUE "PEPERNAMBUCO         ".
           03 FILLER    PIC X(21) VALUE "PIPIAUI              ".
           03 FILLER    PIC X(21) VALUE "RJRIO DE JANEIRO     ".
           03 FILLER    PIC X(21) VALUE "RNRIO GRANDE DO NORTE".
           03 FILLER    PIC X(21) VALUE "RSRIO GRANDE DO SUL  ".
           03 FILLER    PIC X(21) VALUE "RORONDONIA           ".
           03 FILLER    PIC X(21) VALUE "RRRORAIMA            ".
           03 FILLER    PIC X(21) VALUE "SCSANTA CATARINA     ".
           03 FILLER    PIC X(21) VALUE "SPSAO PAULO          ".
           03 FILLER    PIC X(21) VALUE "SESERGIPE            ".
           03 FILLER    PIC X(21) VALUE "TOTOCANTINS          ".

       01 VECTOR-UFS REDEFINES BUFFER-UFS.
           03 VECTOR-UF    PIC X(21) OCCURS 26 TIMES.

       01 VIEW-UF.
           03 VIEW-UF-SIGRA     PIC X(02) VALUE SPACES.
           03 VIEW-UF-EXT       PIC X(19) VALUE SPACES.

       SCREEN SECTION.
       01  TELA.
           05  LINE 02  COLUMN 01
               VALUE  "---------------------------[ CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  " PRODUTOS ]-----------------------------".
           05  LINE 03  COLUMN 01
               VALUE  "  CODIGO:                 DESCRICAO:".
           05  LINE 05  COLUMN 01
               VALUE  "  UNIDADE:                APLICACAO:".
           05  LINE 07  COLUMN 01
               VALUE  "  QUANTIDADE:             PRECO:".
           05  LINE 08  COLUMN 01
               VALUE  "---------------------------[     FORNECE".
           05  LINE 08  COLUMN 41
               VALUE  "DORES     ]-----------------------------".
           05  LINE 09  COLUMN 01
               VALUE  "  CNPJ:".
           05  LINE 11  COLUMN 01
               VALUE  "  RAZAO SOCIAL:".
           05  LINE 13  COLUMN 01
               VALUE  "  NOME FANTASIA:".
           05  LINE 15  COLUMN 01
               VALUE  "  CEP:            ENDERECO:".
           05  LINE 15  COLUMN 41
               VALUE  "                             N:".
           05  LINE 17  COLUMN 01
               VALUE  "  COMPLEMENTO:            BAIRRO:".
           05  LINE 19  COLUMN 01
               VALUE  "  CIDADE:                      UF:".
           05  LINE 20  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 20  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 22  COLUMN 01
               VALUE  "  MENSAGEM:".
           05  SCREEN-CODIGO
               LINE 03  COLUMN 11  PIC 999.999
               USING  CODIGO.
           05  SCREEN-DESCRICAO
               LINE 03  COLUMN 38  PIC X(30)
               USING  DESCRICAO.
           05  SCREEN-UNIDADE
               LINE 05  COLUMN 12  PIC X(02)
               USING  UNIDADE.
           05  SCREEN-UNIDADE-EXT
               LINE 05  COLUMN 15  PIC X(10)
               USING  VIEW-UNIDADE-EXT.
           05  SCREEN-APLICACAO
               LINE 05  COLUMN 38  PIC 9(01)
               USING  APLICACAO.
           05  SCREEN-APLICACAO-EXT
               LINE 05  COLUMN 40  PIC X(15)
               USING  VIEW-APLICACAO-EXT.
           05  SCREEN-QUANTIDADE
               LINE 07  COLUMN 15  PIC ZZ.ZZ9,9
               USING  QUANTIDADE.
           05  SCREEN-PRECO
               LINE 07  COLUMN 34  PIC ZZZZ.ZZ9,99
               USING  PRECO.
           05  SCREEN-CNPJ
               LINE 09  COLUMN 09  PIC 999.999.999.9999.99
               USING  CNPJ.
           05  SCREEN-RAZAO
               LINE 11  COLUMN 17  PIC X(40)
               USING  RAZAO.
           05  SCREEN-NOME
               LINE 13  COLUMN 18  PIC X(15)
               USING  NOME.
           05  SCREEN-CEP
               LINE 15  COLUMN 08  PIC 99999.999
               USING  CEP.
           05  SCREEN-ENDERECO
               LINE 15  COLUMN 29  PIC X(40)
               USING  ENDERECO.
           05  SCREEN-NUMERO
               LINE 15  COLUMN 73  PIC 9(04)
               USING  NUMERO.
           05  SCREEN-COMPLEMENTO
               LINE 17  COLUMN 16  PIC X(10)
               USING  COMPLEMENTO.
           05  SCREEN-BAIRRO
               LINE 17  COLUMN 35  PIC X(20)
               USING  BAIRRO.
           05  SCREEN-CIDADE
               LINE 19  COLUMN 11  PIC X(20)
               USING  CIDADE.
           05  SCREEN-UF
               LINE 19  COLUMN 36  PIC X(02)
               USING  UF.
           05  SCREEN-UF-EXT
               LINE 19  COLUMN 39  PIC X(19)
               USING  VIEW-UF-EXT.
           05  SCREEN-MENSAGEM
               LINE 22  COLUMN 13  PIC X(50)
               USING  MENSAGEM.

       01 TELA-UNIDADE.
           05 LINE 5 COLUMN 55 VALUE "|LT-LITRO     |".
           05 LINE 6 COLUMN 55 VALUE "|PC-PECA      |".
           05 LINE 7 COLUMN 55 VALUE "|KG-TONELADA  |".
           05 LINE 8 COLUMN 55 VALUE "|AR-ARROBA    |".
           05 LINE 9 COLUMN 55 VALUE "|GR-GRAMA     |".
           05 LINE 10 COLUMN 55 VALUE "|MT-METRO     |".
           05 LINE 11 COLUMN 55 VALUE "|CM-CENTIMETRO|".
           05 LINE 12 COLUMN 55 VALUE "|CJ-CONJUNTO  |".

       01 TELA-UNIDADE-LIMPA.
           05 LINE 5 COLUMN 55 VALUE "               ".
           05 LINE 6 COLUMN 55 VALUE "               ".
           05 LINE 7 COLUMN 55 VALUE "               ".
           05 LINE 8 COLUMN 55 VALUE "               ".
           05 LINE 9 COLUMN 55 VALUE "               ".
           05 LINE 10 COLUMN 55 VALUE "               ".
           05 LINE 11 COLUMN 55 VALUE "               ".
           05 LINE 12 COLUMN 55 VALUE "               ".

       01 TELA-APLICACAO.
           05 LINE 5 COLUMN 55 VALUE "|1-SAUDE          |".
           05 LINE 6 COLUMN 55 VALUE "|2-ELETRONICO     |".
           05 LINE 7 COLUMN 55 VALUE "|3-HIGIENE PESSOAL|".
           05 LINE 8 COLUMN 55 VALUE "|4-ALIMENTACAO    |".
           05 LINE 9 COLUMN 55 VALUE "|5-LIMPESA        |".
           05 LINE 10 COLUMN 55 VALUE "|6-SEGURANCA      |".
           05 LINE 11 COLUMN 55 VALUE "|7-GESTAO         |".
           05 LINE 12 COLUMN 55 VALUE "|8-VESTUARIO      |".
           05 LINE 12 COLUMN 55 VALUE "|9-OUTRAS         |".

       01 TELA-APLICACAO-LIMPA.
           05 LINE 5 COLUMN 55 VALUE "                   ".
           05 LINE 6 COLUMN 55 VALUE "                   ".
           05 LINE 7 COLUMN 55 VALUE "                   ".
           05 LINE 8 COLUMN 55 VALUE "                   ".
           05 LINE 9 COLUMN 55 VALUE "                   ".
           05 LINE 10 COLUMN 55 VALUE "                   ".
           05 LINE 11 COLUMN 55 VALUE "                   ".
           05 LINE 12 COLUMN 55 VALUE "                   ".
           05 LINE 12 COLUMN 55 VALUE "                   ".

       01 TELA-UF.
           05 LINE 1 COLUMN 55 VALUE "|AC-ACRE               |".
           05 LINE 2 COLUMN 55 VALUE "|AL-ALAGOAS            |".
           05 LINE 3 COLUMN 55 VALUE "|AP-AMAPA              |".
           05 LINE 4 COLUMN 55 VALUE "|AM-AMAZONAS           |".
           05 LINE 5 COLUMN 55 VALUE "|BA-BAHIA              |".
           05 LINE 6 COLUMN 55 VALUE "|CE-CEARA              |".
           05 LINE 7 COLUMN 55 VALUE "|ES-ESPIRITO SANTO     |".
           05 LINE 8 COLUMN 55 VALUE "|GO-GOIAS              |".
           05 LINE 9 COLUMN 55 VALUE "|MA-MARANHAO           |".
           05 LINE 10 COLUMN 55 VALUE "|MT-MATO GROSSO        |".
           05 LINE 11 COLUMN 55 VALUE "|MS-MATO GROSSO DO SUL |".
           05 LINE 12 COLUMN 55 VALUE "|MG-MINAS GERAIS       |".
           05 LINE 13 COLUMN 55 VALUE "|PA-PARA               |".
           05 LINE 14 COLUMN 55 VALUE "|PB-PARAIBA            |".
           05 LINE 15 COLUMN 55 VALUE "|PR-PARANA             |".
           05 LINE 16 COLUMN 55 VALUE "|PE-PERNAMBUCO         |".
           05 LINE 17 COLUMN 55 VALUE "|PI-PIAUI              |".
           05 LINE 18 COLUMN 55 VALUE "|RJ-RIO DE JANEIRO     |".
           05 LINE 19 COLUMN 55 VALUE "|RN-RIO GRANDE DO NORTE|".
           05 LINE 20 COLUMN 55 VALUE "|RS-RIO GRANDE DO SUL  |".
           05 LINE 21 COLUMN 55 VALUE "|RO-RONDONIA           |".
           05 LINE 22 COLUMN 55 VALUE "|RR-RORAIMA            |".
           05 LINE 23 COLUMN 55 VALUE "|SC-SANTA CATARINA     |".
           05 LINE 24 COLUMN 55 VALUE "|SP-SAO PAULO          |".
           05 LINE 20 COLUMN 31 VALUE "|SE-SERGIPE            |".
           05 LINE 21 COLUMN 31 VALUE "|TO-TOCANTINS          |".

       01 TELA-UF-LIMPA.
           05 LINE 1 COLUMN 55 VALUE "                        ".
           05 LINE 2 COLUMN 55 VALUE "                        ".
           05 LINE 3 COLUMN 55 VALUE "                        ".
           05 LINE 4 COLUMN 55 VALUE "                        ".
           05 LINE 5 COLUMN 55 VALUE "                        ".
           05 LINE 6 COLUMN 55 VALUE "                        ".
           05 LINE 7 COLUMN 55 VALUE "                        ".
           05 LINE 8 COLUMN 55 VALUE "                        ".
           05 LINE 9 COLUMN 55 VALUE "                        ".
           05 LINE 10 COLUMN 55 VALUE "                        ".
           05 LINE 11 COLUMN 55 VALUE "                        ".
           05 LINE 12 COLUMN 55 VALUE "                        ".
           05 LINE 13 COLUMN 55 VALUE "                        ".
           05 LINE 14 COLUMN 55 VALUE "                        ".
           05 LINE 15 COLUMN 55 VALUE "                        ".
           05 LINE 16 COLUMN 55 VALUE "                        ".
           05 LINE 17 COLUMN 55 VALUE "                        ".
           05 LINE 18 COLUMN 55 VALUE "                        ".
           05 LINE 19 COLUMN 55 VALUE "                        ".
           05 LINE 20 COLUMN 55 VALUE "                        ".
           05 LINE 21 COLUMN 55 VALUE "                        ".
           05 LINE 22 COLUMN 55 VALUE "                        ".
           05 LINE 23 COLUMN 55 VALUE "                        ".
           05 LINE 24 COLUMN 55 VALUE "                        ".
           05 LINE 20 COLUMN 31 VALUE "                        ".
           05 LINE 21 COLUMN 31 VALUE "                        ".

       PROCEDURE DIVISION.
       CREATE-FILES.
           PERFORM I-O-CADPROD.
           PERFORM I-O-CADCEP.
           PERFORM I-O-CADFORN.

       CLEAR-REGPROD.
           MOVE SPACES TO DESCRICAO UNIDADE.
           MOVE ZEROS TO CODIGO APLICACAO PRECO QUANTIDADE CNPJ-PROD.
           MOVE SPACES TO VIEW-UNIDADES VIEW-APLICACOES.

       CLEAR-REGFORN.
           MOVE SPACES TO RAZAO NOME COMPLEMENTO.
           MOVE ZEROS TO CNPJ NUMERO CEP-FORN.

       CLEAR-REGCEP.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF.
           MOVE ZEROS TO CEP.
           MOVE SPACES TO VIEW-UF.

       INICIO.
           DISPLAY TELA.

       INPUT-CODIGO.
           ACCEPT SCREEN-CODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO FIM.

           IF CODIGO = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-CODIGO.

           PERFORM READ-CADPROD.
           IF ERRO = "00" PERFORM DELETAR.

       INPUT-DESCRICAO.
           ACCEPT SCREEN-DESCRICAO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CODIGO.

           IF DESCRICAO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-DESCRICAO.

       INPUT-UNIDADE.
           DISPLAY TELA-UNIDADE.
           ACCEPT SCREEN-UNIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01
               DISPLAY TELA-UNIDADE-LIMPA
               DISPLAY TELA
               GO TO INPUT-DESCRICAO.

           IF UNIDADE = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-UNIDADE.

           PERFORM CHECK-UNIDADE.
           IF UNIDADE NOT = VIEW-UNIDADE-SIGRA
               PERFORM ERROR-VALID
               GO TO INPUT-UNIDADE.

           DISPLAY TELA-UNIDADE-LIMPA.
           DISPLAY TELA.

       INPUT-APLICACAO.
           DISPLAY TELA-APLICACAO.
           ACCEPT SCREEN-APLICACAO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01
               DISPLAY TELA-APLICACAO-LIMPA
               DISPLAY TELA
               GO TO INPUT-UNIDADE.

           IF APLICACAO = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-APLICACAO.

           PERFORM CHECK-APLICACAO.
           IF APLICACAO NOT = VIEW-APLICACAO-NUMERO
               PERFORM ERROR-VALID
               GO TO INPUT-APLICACAO.

           DISPLAY TELA-APLICACAO-LIMPA.
           DISPLAY TELA.

       INPUT-QUANTIDADE.
           ACCEPT SCREEN-QUANTIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-APLICACAO.

           IF QUANTIDADE = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-QUANTIDADE.

       INPUT-PRECO.
           ACCEPT SCREEN-PRECO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-QUANTIDADE.

           IF PRECO = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-PRECO.

       INPUT-CNPJ.
           ACCEPT SCREEN-CNPJ.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-PRECO.

           IF CNPJ = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-CNPJ.

           PERFORM READ-CADFORN.

       INPUT-RAZAO.
           ACCEPT SCREEN-RAZAO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CNPJ.

           IF RAZAO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-RAZAO.

       INPUT-NOME.
           ACCEPT SCREEN-NOME.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-RAZAO.

           IF NOME = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-NOME.

       INPUT-CEP.
           ACCEPT SCREEN-CEP.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-NOME.

           IF CEP = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-CEP.

           PERFORM READ-CADCEP.

       INPUT-ENDERECO.
           ACCEPT SCREEN-ENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CEP.

           IF ENDERECO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-ENDERECO.

       INPUT-NUMERO.
           ACCEPT SCREEN-NUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-ENDERECO.

           IF NUMERO = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-NUMERO.

       INPUT-COMPLEMENTO.
           ACCEPT SCREEN-COMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-NUMERO.

       INPUT-BAIRRO.
           ACCEPT SCREEN-BAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-COMPLEMENTO.

           IF BAIRRO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-BAIRRO.

       INPUT-CIDADE.
           ACCEPT SCREEN-CIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-BAIRRO.

           IF CIDADE = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-CIDADE.

       INPUT-UF.
           DISPLAY TELA-UF.
           ACCEPT SCREEN-UF.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01
               DISPLAY TELA-UF-LIMPA
               DISPLAY TELA
               GO TO INPUT-CIDADE.

           IF UF = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-UF.

           PERFORM CHECK-UF.
           IF UF NOT = VIEW-UF-SIGRA
               PERFORM ERROR-VALID
               GO TO INPUT-UF.

           DISPLAY TELA-UF-LIMPA.
           DISPLAY TELA.

       SALVAR.
           MOVE "VOCE QUER SALVAR? (S/N)" TO MENSAGEM
           DISPLAY SCREEN-MENSAGEM.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENSAGEM.
           DISPLAY SCREEN-MENSAGEM.

           IF W-ACT = 01 GO TO INPUT-UF.

           IF W-OPCAO = "S" or "s"
               MOVE CNPJ TO CNPJ-PROD
               MOVE CEP TO CEP-FORN
               PERFORM WRITE-CADPROD
               PERFORM WRITE-CADFORN
               PERFORM WRITE-CADCEP
           ELSE IF W-OPCAO = "N" or "n" GO TO CLEAR-REGPROD
           ELSE PERFORM ERROR-VALID GO TO SALVAR.
           GO TO CLEAR-REGPROD.

       DELETAR.
           MOVE "VOCE QUER DELETAR? (S/N)" TO MENSAGEM
           DISPLAY SCREEN-MENSAGEM.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENSAGEM.
           DISPLAY SCREEN-MENSAGEM.

           IF W-ACT = 01 GO TO INPUT-UF.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-CADPROD
               GO TO CLEAR-REGPROD
           ELSE IF W-OPCAO = "N" or "n" NEXT SENTENCE
           ELSE PERFORM ERROR-VALID GO TO DELETAR.

       FIM.
           CLOSE CADPROD
           CLOSE CADCEP
           CLOSE CADFORN
           STOP RUN.

       CHECK-UNIDADE.
           MOVE 1 TO ITERATOR
           PERFORM CHECK-UNIDADE-1.

       CHECK-UNIDADE-1.
           MOVE VECTOR-UNIDADE(ITERATOR) TO VIEW-UNIDADES
           IF VIEW-UNIDADE-SIGRA = UNIDADE
               MOVE 8 TO ITERATOR
           ELSE MOVE SPACES TO VIEW-UNIDADES.
           ADD 1 TO ITERATOR.
           IF ITERATOR < 9 GO TO CHECK-UNIDADE-1.

       CHECK-APLICACAO.
           MOVE 1 TO ITERATOR
           PERFORM CHECK-APLICACAO-1.

       CHECK-APLICACAO-1.
           MOVE VECTOR-APLICACAO(ITERATOR) TO VIEW-APLICACOES
           IF VIEW-APLICACAO-NUMERO = APLICACAO
               MOVE 9 TO ITERATOR
           ELSE MOVE SPACES TO VIEW-APLICACOES.
           ADD 1 TO ITERATOR.
           IF ITERATOR < 10 GO TO CHECK-APLICACAO-1.

       CHECK-UF.
           MOVE 1 TO ITERATOR
           PERFORM CHECK-UF-1.

       CHECK-UF-1.
           MOVE VECTOR-UF(ITERATOR) TO VIEW-UF
           IF VIEW-UF-SIGRA = UF
               MOVE 26 TO ITERATOR
           ELSE MOVE SPACES TO VIEW-UF.
           ADD 1 TO ITERATOR.
           IF ITERATOR < 27 GO TO CHECK-UF-1.

       ERROR-EMPTY.
           MOVE "O CAMPO PRECISA SER INFORMADO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-VALID.
           MOVE "OPCAO NAO VALIDA" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-FILE-NOT-OPENED.
           MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-DATA-NOT-SAVED.
           MOVE "DADOS NAO GRAVADOS" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-DATA-NOT-FOUND.
           MOVE "DADOS NAO ENCONTRADOS" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-FILE-NOT-READED.
           MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-DATA-NOT-DELETED.
           MOVE "DADOS NAO DELETADOS" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       LOG-FILE-CREATED.
           MOVE "ARQUIVO CRIADO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       LOG-DATA-SAVED.
           MOVE "DADOS GRAVADOS" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       LOG-DATA-DELETED.
           MOVE "DADOS DELETADOS" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       I-O-CADPROD.
           OPEN I-O CADPROD
           IF ERRO NOT = "00"
              IF ERRO = "30"
                 OPEN OUTPUT CADPROD
                 CLOSE CADPROD
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADPROD
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO FIM.

       I-O-CADCEP.
           OPEN I-O CADCEP
           IF ERRO NOT = "00"
              IF ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADCEP
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO FIM.

       I-O-CADFORN.
           OPEN I-O CADFORN
           IF ERRO NOT = "00"
              IF ERRO = "30"
                 OPEN OUTPUT CADFORN
                 CLOSE CADFORN
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADFORN
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO FIM.

       CHECK-WRITE.
           IF ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO FIM.

       CHECK-READ.
           IF ERRO NOT = "23"
               IF ERRO = "00" DISPLAY TELA
               ELSE PERFORM ERROR-FILE-NOT-READED GO TO FIM.

       CHECK-DELETE.
           IF ERRO = "00" PERFORM LOG-DATA-DELETED
           ELSE PERFORM ERROR-DATA-NOT-DELETED GO TO FIM.

       WRITE-CADPROD.
           WRITE REGPROD.
           IF ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
           ELSE IF ERRO = "22" PERFORM REWRITE-CADPROD
           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO FIM.

       WRITE-CADFORN.
           WRITE REGFORN.
           IF ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
           ELSE IF ERRO = "22" PERFORM REWRITE-CADFORN
           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO FIM.

       WRITE-CADCEP.
           WRITE REGCEP.
           IF ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
           ELSE IF ERRO = "22" PERFORM REWRITE-CADCEP
           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO FIM.

       REWRITE-CADPROD. REWRITE REGPROD. PERFORM CHECK-WRITE.
       REWRITE-CADFORN. REWRITE REGFORN. PERFORM CHECK-WRITE.
       REWRITE-CADCEP. REWRITE REGCEP. PERFORM CHECK-WRITE.

       READ-CADPROD. READ CADPROD. PERFORM CHECK-READ.
           IF ERRO = "00" MOVE CNPJ-PROD TO CNPJ PERFORM READ-CADFORN.
           PERFORM CHECK-UNIDADE.
           PERFORM CHECK-APLICACAO.
           DISPLAY TELA.

       READ-CADFORN. READ CADFORN. PERFORM CHECK-READ.
           IF ERRO = "00" MOVE CEP-FORN TO CEP PERFORM READ-CADCEP.
           DISPLAY TELA.

       READ-CADCEP. READ CADCEP. PERFORM CHECK-READ.
           PERFORM CHECK-UF.
           DISPLAY TELA.

       DELETE-CADPROD. DELETE CADPROD RECORD. PERFORM CHECK-DELETE.
       DELETE-CADFORN. DELETE CADFORN RECORD. PERFORM CHECK-DELETE.
       DELETE-CADCEP. DELETE CADCEP RECORD. PERFORM CHECK-DELETE.

       SHOW-MESSAGE.
           MOVE ZEROS TO W-CONT.
           DISPLAY SCREEN-MENSAGEM.
           PERFORM SHOW-MESSAGE-1.

       SHOW-MESSAGE-1.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
               GO TO SHOW-MESSAGE-1
           ELSE
               MOVE SPACES TO MENSAGEM
               DISPLAY SCREEN-MENSAGEM.
