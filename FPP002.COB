       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS COD
                      ALTERNATE RECORD KEY IS DENOM WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
          03 COD                PIC 9(03).
          03 DENOM              PIC X(45).
          03 TPSALARIO          PIC X(01).
          03 SALARIOBASE        PIC 9(06)V99.
          03 CARGSTATUS         PIC X(01).

       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE ZEROS.
       01 INDSALARIO      PIC 9(01) VALUE ZEROS.
       01 TXTTSALARIO     PIC X(20) VALUE SPACES.

       01 TABSALARIO.
           03 TBSALARIO      PIC X(20) OCCURS 3 TIMES.

       01 TABSTATUS .
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(20) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS1 PIC X(01) VALUE SPACES.
           03 TXTSTATUS2 PIC X(19) VALUE SPACES.

       SCREEN SECTION.
       01  TELACARGOS.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "-----CADASTRO DE CARGOS-----".
           05  LINE 05  COLUMN 01
               VALUE  "   CODIGO DO CARGO:".
           05  LINE 07  COLUMN 01
               VALUE  "   DENOMINACAO DO CARGO:".
           05  LINE 09  COLUMN 01
               VALUE  "   TIPO DE SALARIO:".
           05  LINE 11  COLUMN 01
               VALUE  "   SALARIO BASE:".
           05  LINE 13  COLUMN 01
               VALUE  "   STATUS:".
           05  LINE 22  COLUMN 01
               VALUE  "   MENSAGEM:".


           05  TCOD
               LINE 05  COLUMN 22  PIC 9(03)
               USING  COD
               HIGHLIGHT.
           05  TDENOM
               LINE 07  COLUMN 25  PIC X(45)
               USING  DENOM
               HIGHLIGHT.
           05  TTPSALARIO
               LINE 09  COLUMN 21  PIC X
               USING  TPSALARIO
               HIGHLIGHT.
           05  TXTSALARIO
               LINE 09  COLUMN 23  PIC X(20)
               USING  TXTTSALARIO
               HIGHLIGHT.
           05  TSALARIOBASE
               LINE 11  COLUMN 18  PIC 999999.99
               USING  SALARIOBASE
               HIGHLIGHT.
           05  TSTATUSS
               LINE 13  COLUMN 12  PIC X(25)
               USING  CARGSTATUS
               HIGHLIGHT.

           01 TELATIPOSALARIO.
           05  LINE 15  COLUMN 50
               VALUE  "H-HORISTA".
           05  LINE 16  COLUMN 50
               VALUE  "D-DIARISTA".
           05  LINE 17  COLUMN 50
               VALUE  "M-MENSALISTA".

           01 TELASTATUS.
           05  LINE 15  COLUMN 50
               VALUE  "ATIVA".
           05  LINE 16  COLUMN 50
               VALUE  "SUSPENSA".
           05  LINE 17  COLUMN 50
               VALUE  "DESATIVADA".

       PROCEDURE DIVISION.
       INICIO.
           MOVE "HORISTA"      TO TBSALARIO(1)
           MOVE "DIARISTA"     TO TBSALARIO(2)
           MOVE "MENSALISTA"   TO TBSALARIO(3).

       R0.
           OPEN I-O ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQCARG
                 CLOSE ARQCARG
                 MOVE "*** ARQUIVO ARQCARG FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO ARQCARG" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.
           MOVE SPACES TO DENOM CARGSTATUS TXTTSALARIO
           MOVE ZEROS TO COD W-SEL SALARIOBASE

           DISPLAY TELACARGOS.

       R2.
           ACCEPT TCOD.
           IF COD < 1
              MOVE "CODIGO INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-ARQCARG.
           READ ARQCARG
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R6A
                DISPLAY TELACARGOS
                MOVE "*** CARGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQCARG" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CARGO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
           ACCEPT TDENOM.
           IF DENOM = SPACES
              MOVE "CAMPO DENOMINACAO DO CARGO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           DISPLAY TELATIPOSALARIO
           ACCEPT TTPSALARIO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                DISPLAY TELACARGOS
                GO TO R4.
           IF TPSALARIO = "H"
               MOVE 1 TO INDSALARIO
           ELSE IF TPSALARIO = "D"
               MOVE 2 TO INDSALARIO
           ELSE IF TPSALARIO = "M"
               MOVE 3 TO INDSALARIO
           ELSE
                MOVE "*** OPCAO INVALIDA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.
       R4A.
           DISPLAY TELACARGOS
           MOVE TBSALARIO(INDSALARIO) TO TXTTSALARIO
           DISPLAY TXTSALARIO.


       R5.
           ACCEPT TSALARIOBASE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.

       R6.
           DISPLAY TELASTATUS
           ACCEPT TSTATUSS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.

       R6A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = CARGSTATUS
              ADD 1 TO IND
              IF IND < 4
                 GO TO R6A
              ELSE
                 MOVE "STATUS INVALIDO" TO MENS
                 MOVE 1 TO IND
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
              DISPLAY TELACARGOS.
              MOVE 1 TO IND
              DISPLAY (13 , 12) TXTSTATUS2.

           IF W-SEL = 1
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (22, 40) "DADOS OK (S/N) : ".
                ACCEPT (22, 57) W-OPCAO WITH UPDATE
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCARG
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CARGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQCARG" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       ACE-001.
                DISPLAY (22, 12)
                     "N=NOVO CARGO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (22, 55) W-OPCAO
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
                      GO TO R2.
      *
       EXC-OPC.
                DISPLAY (22, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCARG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DE CARGO EXCLUIDO ***"
                   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (22, 40) "ALTERAR  (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCARG
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DE CARGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE ARQCARG.
           STOP RUN.

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (22, 20) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (22, 20) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
