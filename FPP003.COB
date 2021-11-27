       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQDEPE ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS DEPEN-KEY
                      ALTERNATE RECORD KEY IS DEPEN-NOME
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

           SELECT ARQFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CHAPA
                      ALTERNATE RECORD KEY IS NOME
                      ALTERNATE RECORD KEY IS DTNASC
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQDEPE
                 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "ARQDEPE.DAT".

       01 REGDEPE.
           03 TPGRAU             PIC 9(01).
           03 DEPEN-KEY.
            05 DEPEN-SEQ          PIC 9(01).
            05 DEPEN-CHAPA        PIC 9(06).
           03 DEPEN-NOME         PIC X(30).
           03 DEPEN-DATANASC           PIC 9(08).
           03 DTNASCIMENTO REDEFINES DEPEN-DATANASC.
            05 DEPEN-DIA                 PIC 9(02).
            05 DEPEN-MES                 PIC 9(02).
            05 DEPEN-ANO                 PIC 9(04).
           03 DEPEN-SEXO        PIC X(01).
           03 DEPEN-INSS        PIC X(01).
           03 DEPEN-IMPRENDA    PIC X(01).
           03 DEPEN-STATUS      PIC X(20).
           

       FD ARQFUNC
                 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "ARQFUNC.DAT".
       01 REGFUNC.
           03 CHAPA             PIC 9(06).
           03 NOME              PIC X(30).
           03 DATANASC          PIC 9(08).
           03 DTNASC REDEFINES DATANASC.
            05 DIA                 PIC 9(02).
            05 MES                 PIC 9(02).
            05 ANO                 PIC 9(04).
           03 SEXO              PIC X(01).
           03 GENERO            PIC X(01).
           03 NIVELSALARIAL     PIC 9(02).
           03 QTDDEPENDENTES    PIC 9(01).
           03 IMPRENDA          PIC X(01).
           03 FUNCSTATUS        PIC X(01).
           03 CEPFUNC           PIC 9(08).
           03 CODDEP            PIC 9(03).
           03 CODIGOCARG        PIC 9(03).
           03 CEP-COMPLE        PIC X(12).
           03 CEP-NUMRES        PIC 9(04).

       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(01) VALUE ZEROS.
       01 TXTTGRAU        PIC X(20) VALUE SPACES.
       01 TEXSEXO         PIC X(15) VALUE SPACES.
       01 TDEPENINSS      PIC X(10) VALUE SPACES.
       01 TDEPENSEXO      PIC X(10) VALUE SPACES.

       01 TABGRAU.
           03 TBGRAU     PIC X(20) OCCURS 09 TIMES.

       01 TABSTATUS.
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".
           03 FILLER PIC X(20) VALUE "XEX DEPENDENTE".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(20) OCCURS 4 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS1 PIC X(01) VALUE SPACES.
           03 TXTSTATUS2 PIC X(19) VALUE SPACES.

       SCREEN SECTION.
       01  TELADEPEN.
           05  LINE 02  COLUMN 01
               VALUE  "-----CADASTRO DE DEPENDENTES-----".
           05  LINE 04  COLUMN 01
               VALUE  "  CHAPA DO FUNCIONARIO:                N".
           05  LINE 04  COLUMN 41
               VALUE  "OME:".
           05  LINE 05  COLUMN 01
               VALUE  "  DATA NASC:                 SEXO:".
           05  LINE 05  COLUMN 41
               VALUE  "            GENERO:".
           05  LINE 06  COLUMN 01
               VALUE  "  NIVEL SALARIAL:            IMP. RENDA:".
           05  LINE 06  COLUMN 41
               VALUE  "            QTD. DEPEN.:".
           05  LINE 07  COLUMN 01
               VALUE  "  STATUS DO FUNCIONARIO:".
           05  LINE 08  COLUMN 01
               VALUE  "-------------------------------------".
           05  LINE 08  COLUMN 41
               VALUE  "-------------------------------------".
           05  LINE 09  COLUMN 01
               VALUE  "  SEQ. DEPENDENTE:                    GR".
           05  LINE 09  COLUMN 41
               VALUE  "AU PARENTESCO:".
           05  LINE 10  COLUMN 01
               VALUE  "  NOME DO DEPENDENTE:".
           05  LINE 10  COLUMN 41
               VALUE  "                 DATA NASC:".
           05  LINE 11  COLUMN 01
               VALUE  "  SEXO:               DEPEND. INSS:".
           05  LINE 11  COLUMN 41
               VALUE  "         DEPEND. IMP. RENDA:".
           05  LINE 14  COLUMN 01
               VALUE  "  STATUS DO DEPENDENTE:".
           05  LINE 15  COLUMN 01
               VALUE  "-------------------------------------".
           05  LINE 15  COLUMN 41
               VALUE  "-------------------------------------".
           05  LINE 20  COLUMN 01
               VALUE  "  MENSAGEM:".
           05  TNOME
               LINE 04  COLUMN 45  PIC x(30)
               USING  NOME
               HIGHLIGHT.
           05  TDTNASC
               LINE 05  COLUMN 13  PIC 99/99/9999
               USING  DTNASC
               HIGHLIGHT.
           05  TSEXO
               LINE 05  COLUMN 36  PIC X(02)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 05  COLUMN 38  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 05  COLUMN 60  PIC x(01)
               USING  GENERO
               HIGHLIGHT.
           05  TNIVELSALARIAL
               LINE 06  COLUMN 18 PIC 9(02)
               USING  NIVELSALARIAL
               HIGHLIGHT.
           05  TIMPRENDA
               LINE 06  COLUMN 43 PIC X(01)
               USING  IMPRENDA
               HIGHLIGHT.
           05  TQTDDEPENDENTES
               LINE 06  COLUMN 66 PIC 9(01)
               USING  QTDDEPENDENTES
               HIGHLIGHT.
           05  TFUNCSTATUS
               LINE 07  COLUMN 25  PIC X(25)
               USING  FUNCSTATUS
               HIGHLIGHT.
           05  TDEPEN-CHAPA
               LINE 04  COLUMN 24  PIC 9(06)
               USING  DEPEN-CHAPA
               HIGHLIGHT.
           05  TDEPEN-NOME
               LINE 10  COLUMN 23  PIC X(30)
               USING  DEPEN-NOME
               HIGHLIGHT.
           05  TDPEN-SEQ
               LINE 09  COLUMN 19  PIC 9(01)
               USING  DEPEN-SEQ
               HIGHLIGHT.
           05  TTPGRAU
               LINE 09  COLUMN 56  PIC 9(01)
               USING  TPGRAU
               HIGHLIGHT.
           05  TXTGRAU
               LINE 09  COLUMN 58  PIC X(20)
               USING  TXTTGRAU
               HIGHLIGHT.
           05  TDTNASCIMENTO
               LINE 10  COLUMN 68  PIC 99/99/9999
               USING  DTNASCIMENTO
               HIGHLIGHT.
           05  TDEPEN-SEXO
               LINE 11  COLUMN 08 PIC X(01)
               USING  DEPEN-SEXO
               HIGHLIGHT.
           05  TTDEPEN-SEXO
               LINE 11  COLUMN 10 PIC X(10)
               USING  TDEPENSEXO
               HIGHLIGHT.
           05  TDEPEN-INSS
               LINE 11  COLUMN 38 PIC X(01)
               USING  DEPEN-INSS
               HIGHLIGHT.
           05  TTDEPEN-INSS
               LINE 11  COLUMN 39 PIC X(10)
               USING TDEPENINSS
               HIGHLIGHT.
           05  TDEPEN-IMPRENDA
               LINE 11  COLUMN 69 PIC X(01)
               USING  DEPEN-IMPRENDA
               HIGHLIGHT.
           05  TDEPEN-STATUS
               LINE 14 COLUMN 25  PIC X(20)
               USING  DEPEN-STATUS
               HIGHLIGHT.

       PROCEDURE DIVISION.
       INICIO.
           MOVE "1-ESPOSA "           TO TBGRAU(1)
           MOVE "2-FILHO(A)"          TO TBGRAU(2)
           MOVE "3-ENTEADO(A)"        TO TBGRAU(3)
           MOVE "4-PAI"               TO TBGRAU(4)
           MOVE "5-MAE"               TO TBGRAU(5)
           MOVE "6-AVOS"              TO TBGRAU(6)
           MOVE "7-NETO(A)"           TO TBGRAU(7)
           MOVE "8-RESPONSAVEL LEGAL" TO TBGRAU(8)
           MOVE "9-OUTROS"            TO TBGRAU(9).

       R0.
           OPEN I-O ARQDEPE
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQDEPE
                 CLOSE ARQDEPE
                 MOVE "*** ARQUIVO ARQDEPE FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEPE" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0A.
           OPEN INPUT ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO ARQFUNC NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQFUNC" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.
           MOVE SPACES TO NOME SEXO GENERO IMPRENDA FUNCSTATUS
           MOVE SPACES TO DEPEN-SEXO DEPEN-INSS  DEPEN-IMPRENDA
           MOVE SPACES TO DEPEN-STATUS TXTTGRAU 

           MOVE ZEROS TO DEPEN-SEQ TPGRAU DTNASCIMENTO DEPEN-CHAPA
           MOVE ZEROS TO DTNASC NIVELSALARIAL QTDDEPENDENTES

           DISPLAY TELADEPEN.

       R2.
           ACCEPT TDEPEN-CHAPA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELADEPEN
                   GO TO R2.
           IF DEPEN-CHAPA < 0
                 MOVE "*** CODIGO CARGO. NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R2.

       R3.
           MOVE DEPEN-CHAPA TO CHAPA.
           READ ARQFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** FUNCIONARIO DIGITADO NAO ENCONTRADO ***"
                   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO FUNCIONARIO"
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELADEPEN.
       ROT-NOME.

       R4.

       LER-ARQDEPE.
           READ ARQDEPE
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELADEPEN
                MOVE "*** DEPENDENTE JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
      *          GO TO R5
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQDEPE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** DEPENDENTE NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R5.
           ACCEPT TDPEN-SEQ.
           IF DEPEN-SEQ < 0
              MOVE "SEQ. DEPENDENTE INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       R6.
           ACCEPT TTPGRAU.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                DISPLAY TELADEPEN
                GO TO R6.
           IF TPGRAU < 0 OR > 10
                MOVE "*** OPCAO INVALIDA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R6.
       R6A.
           ADD TPGRAU TO IND
           DISPLAY TELADEPEN
           MOVE TBGRAU(IND) TO TXTTGRAU
           DISPLAY TXTGRAU.

       R7.
           ACCEPT TDEPEN-NOME.
           IF DEPEN-NOME = SPACES
              MOVE "CAMPO NOME VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.

       R8.
           ACCEPT TDTNASCIMENTO.
           IF DTNASCIMENTO = SPACES
              MOVE "CAMPO DATA NASCIMENTO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.

       R9.
           ACCEPT TDEPEN-SEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
       R9A.
           IF DEPEN-SEXO = "M" OR DEPEN-SEXO = "m"
              MOVE "MASCULINO" TO TDEPENSEXO
           ELSE
              IF DEPEN-SEXO = "F" OR DEPEN-SEXO = "f"
                 MOVE "FEMININO" TO TDEPENSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.
           DISPLAY TTDEPEN-SEXO.


       R10.
           ACCEPT TDEPEN-INSS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
       R10A.
           IF DEPEN-INSS = "S" OR DEPEN-INSS = "s"
              MOVE "SIM" TO TDEPENINSS
           ELSE
              IF DEPEN-INSS = "N" OR DEPEN-INSS = "n"
                 MOVE "NAO" TO TDEPENINSS
              ELSE
                 IF DEPEN-INSS = "E" OR DEPEN-INSS = 'e'
                     MOVE "ESPECIAL" TO TDEPENINSS
                 ELSE
                     MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R10.
           DISPLAY TDEPENINSS.

       R11.
           ACCEPT TDEPEN-IMPRENDA.
           IF DEPEN-IMPRENDA = SPACES
              MOVE "CAMPO IMP. RENDA VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.

       R12.
           MOVE 1 TO IND
           ACCEPT TDEPEN-STATUS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R11.

       R12A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = DEPEN-STATUS
              ADD 1 TO IND
              IF IND < 4
                 GO TO R12A
              ELSE
                 MOVE "STATUS INVALIDO" TO MENS
                 MOVE 1 TO IND
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R12
           ELSE
              DISPLAY TELADEPEN.
              ADD 1 TO IND
              DISPLAY (14 , 25) TXTSTATUS2.

           IF W-SEL = 1
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (20, 40) "DADOS OK (S/N) : ".
                ACCEPT (20, 57) W-OPCAO WITH UPDATE
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
           WRITE REGDEPE
               IF ST-ERRO = "00" OR "02"
                   MOVE "*** DADOS GRAVADOS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
               IF ST-ERRO = "22"
                   MOVE "* DEP. JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1
               ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQDEPE" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.

       ACE-001.
                DISPLAY (20, 15)
                    "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (20, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (20, 15) MENS
                IF W-OPCAO = "N"
                   GO TO R5
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R5.
      *
       EXC-OPC.
                DISPLAY (23, 15) "EXCLUIR   (S/N) : ".
                ACCEPT W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQDEPE RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DE DEPENDENTE EXCLUIDO ***"
                   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (20, 12) "ALTERAR  (S/N) : ".
                ACCEPT W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGDEPE
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DE DEPENDENTE"
                TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE ARQDEPE ARQFUNC.
           STOP RUN.

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 15) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (20, 15) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
