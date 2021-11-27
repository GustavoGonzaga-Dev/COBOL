       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP009.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CEP-NUMCEP
                      ALTERNATE RECORD KEY IS CEP-LOGRA
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 CEP-NUMCEP               PIC 9(08).
          03 CEP-LOGRA                PIC X(20).
          03 CEP-BAIRRO               PIC X(20).
          03 CEP-CIDADE               PIC X(20).
          03 CEP-UF                   PIC X(02).

       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE 1.

       01 TABFEDERACAO.
          03 FILLER PIC X(35) VALUE "ACACRE".
          03 FILLER PIC X(35) VALUE "ALALAGOAS".
          03 FILLER PIC X(35) VALUE "APAMAPA".
          03 FILLER PIC X(35) VALUE "AMAMAZONAS".
          03 FILLER PIC X(35) VALUE "BABAHIA".
          03 FILLER PIC X(35) VALUE "CECEARA".
          03 FILLER PIC X(35) VALUE "DFDISTRITO FEDERAL".
          03 FILLER PIC X(35) VALUE "ESESPIRITO SANTO".
          03 FILLER PIC X(35) VALUE "GOGOIAS".
          03 FILLER PIC X(35) VALUE "MAMARANHAO".
          03 FILLER PIC X(35) VALUE "MTMATO GROSSO".
          03 FILLER PIC X(35) VALUE "MSMATO GROSSO DO SUL".
          03 FILLER PIC X(35) VALUE "MGMINAS GERAIS".
          03 FILLER PIC X(35) VALUE "PAPARA".
          03 FILLER PIC X(35) VALUE "PBPARAIBA".
          03 FILLER PIC X(35) VALUE "PRPARANA".
          03 FILLER PIC X(35) VALUE "PEPERNAMBUCO".
          03 FILLER PIC X(35) VALUE "PIPIAUI".
          03 FILLER PIC X(35) VALUE "RJRIO DE JANEIRO".
          03 FILLER PIC X(35) VALUE "RNRIO GRANDE DO NORTE".
          03 FILLER PIC X(35) VALUE "RSRIO GRANDE DO SUL".
          03 FILLER PIC X(35) VALUE "RORONDONIA".
          03 FILLER PIC X(35) VALUE "RRRORAIMA".
          03 FILLER PIC X(35) VALUE "SCSANTA CATARINA".
          03 FILLER PIC X(35) VALUE "SPSAO PAULO".
          03 FILLER PIC X(35) VALUE "SESERGIPE".
          03 FILLER PIC X(35) VALUE "TOTOCANTINS".

       01 TABFEDERACAOX   REDEFINES TABFEDERACAO.
          03 TBFEDERACAO  PIC X(35) OCCURS 27 TIMES.
		  
       01 TXTFEDERACAO.
          03 TXTFEDERACAO1 PIC x(02) VALUE SPACES.
          03 TXTFEDERACAO2 PIC X(33) VALUE SPACES.

       SCREEN SECTION.
       01  TELAPRINCIPAL.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "-----CADASTRO DE CEP-----".
           05  LINE 05  COLUMN 01
               VALUE  "      CEP: ".
           05  LINE 07  COLUMN 01
               VALUE  "      LOGRADOURO: ".
           05  LINE 09  COLUMN 01
               VALUE  "      BAIRRO: ".
           05  LINE 11  COLUMN 01
               VALUE  "      CIDADE: ".
           05  LINE 13  COLUMN 01
               VALUE  "      UF: ".
           05  LINE 23  COLUMN 01
               VALUE  " MENSAGEM :".

           05  TCEP
               LINE 05  COLUMN 23  PIC 99999.999
               USING  CEP-NUMCEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 07  COLUMN 23  PIC X(20)
               USING  CEP-LOGRA
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09  COLUMN 23  PIC X(20)
               USING  CEP-BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 11  COLUMN 23  PIC X(20)
               USING  CEP-CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 13  COLUMN 23  PIC X(02)
               USING  CEP-UF
               HIGHLIGHT.

           01  TELAFEDERACAO.
           05  LINE 02 COLUMN 55 
               VALUE   "AC - ACRE".
           05  LINE 03 COLUMN 55 
               VALUE   "AL - ALAGOAS".
           05  LINE 04 COLUMN 55
               VALUE   "AP - AMAPA".
           05  LINE 04 COLUMN 55
               VALUE   "AM - AMAZONAS".
           05  LINE 05 COLUMN 55
               VALUE   "BA - BAHIA".
           05  LINE 06 COLUMN 55
               VALUE   "CE - CEARA".
           05  LINE 07 COLUMN 55
               VALUE   "DF - DISTRITO FEDERAL".
           05  LINE 08 COLUMN 55
               VALUE   "ES - ESPIRITO SANTO".
           05  LINE 09 COLUMN 55
               VALUE   "MA - MARANHAO".
           05  LINE 10 COLUMN 55
               VALUE   "MT - MATO GROSSO".
           05  LINE 11 COLUMN 55
               VALUE   "MS - MATO GROSSO DO SUL".
           05  LINE 12 COLUMN 55
               VALUE   "MG - MINAS GERAIS".
           05  LINE 13 COLUMN 55
               VALUE   "PB - PARAIBA".
           05  LINE 14 COLUMN 55
               VALUE   "PR - PARANA".
           05  LINE 15 COLUMN 55
               VALUE   "PE - PERNAMBUCO".
           05  LINE 16 COLUMN 55
               VALUE   "PI - PIAUI".
           05  LINE 17 COLUMN 55
               VALUE   "RJ - RIO DE JANEIRO".
           05  LINE 18 COLUMN 55
               VALUE   "RN - RIO GRANDE DO NORTE".
           05  LINE 19 COLUMN 55
               VALUE   "RS - RIO GRANDE DO SUL".
           05  LINE 20 COLUMN 55
               VALUE   "RO - RONDONIA".
           05  LINE 21 COLUMN 55
               VALUE   "RR - RORAIMA".
           05  LINE 22 COLUMN 55
               VALUE   "SC - SANTA CATARINA".
           05  LINE 23 COLUMN 55
               VALUE   "SP - SAO PAULO".
           05  LINE 24 COLUMN 55
               VALUE   "SE - SERGIPE".
           05  LINE 25 COLUMN 55
               VALUE   "TO - TOCANTINS".

       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** ARQUIVO CADCEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO NO INICIO! **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE SPACES TO CEP-LOGRA CEP-BAIRRO CEP-CIDADE CEP-UF
           MOVE ZEROS TO CEP-NUMCEP W-SEL

           DISPLAY TELAPRINCIPAL.

       R2.
           ACCEPT TCEP
           IF CEP-NUMCEP < 1000000
              MOVE "---CEP INVALIDO!---" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R5A
                DISPLAY TELAPRINCIPAL
                MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CEP NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R2A.
           ACCEPT TLOGRADOURO
           IF CEP-LOGRA = SPACES
              MOVE "CAMPO LOGRADOURO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2A.
       R3.
           ACCEPT TBAIRRO
           IF CEP-BAIRRO = SPACES
              MOVE "CAMPO BAIRRO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R4.
           ACCEPT TCIDADE
           IF CEP-CIDADE = SPACES
              MOVE "CAMPO CIDADE VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           DISPLAY TELAFEDERACAO
           ACCEPT TUF.
       R5A.
           MOVE TBFEDERACAO(IND) TO TXTFEDERACAO
           IF TXTFEDERACAO1 NOT = CEP-UF
              ADD 1 TO IND
              IF IND < 28
                 GO TO R5A
              ELSE
                 DISPLAY (23, 14) "FEDERACAO INVALIDA"
                 MOVE 1 TO IND
                 GO TO R5
           ELSE
               DISPLAY TELAPRINCIPAL.
               MOVE 1 TO IND
               DISPLAY (13, 26) TXTFEDERACAO2.

           IF W-SEL = 1
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
           WRITE REGCEP
               IF ST-ERRO = "00" OR "02"
                   MOVE "*** DADOS GRAVADOS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
               IF ST-ERRO = "22"
                   MOVE "* CEP JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1
               ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEP" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.

       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R2A.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DE CEP EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DE CEP"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADCEP.
           STOP RUN.

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
