       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP004.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CHAPA
                      ALTERNATE RECORD KEY IS NOME
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

           SELECT ARQDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CODIGO
                      ALTERNATE RECORD KEY IS DENOMINACAO
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS COD
                      ALTERNATE RECORD KEY IS DENOM
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

           SELECT CADCEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CEP-NUMCEP
                      ALTERNATE RECORD KEY IS CEP-LOGRA
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
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

       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
          03 CODIGO                   PIC 9(03).
          03 DENOMINACAO              PIC X(25).
          03 DEPSTATUS                PIC X(01).
          03 TPHIERARC                PIC 9(02).
       

       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
          03 COD                PIC 9(03).
          03 DENOM              PIC X(45).
          03 TPSALARIO          PIC X(01).
          03 SALARIOBASE        PIC 9(06)V99.
          03 CARGSTATUS         PIC X(01).
       

       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 CEP-NUMCEP               PIC 9(08).
          03 CEP-LOGRA                PIC X(30).
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
       01 IND             PIC 9(02) VALUE ZEROS.
       01 TEXSEXO         PIC X(12) VALUE SPACES.

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "THETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "HHOMESEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "AA           ".
          03 FILLER     PIC X(15) VALUE "BB           ".
          03 FILLER     PIC X(15) VALUE "CC           ".
          03 FILLER     PIC X(15) VALUE "DD            ".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".

       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 9 TIMES.

       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES.

       01 TABSTATUS.
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(20) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS1 PIC X(01) VALUE SPACES.
           03 TXTSTATUS2 PIC X(19) VALUE SPACES.

       SCREEN SECTION.
       01  TELAFUNC.
           05  LINE 01  COLUMN 01
               VALUE  "-----CADASTRO DE FUNCIONARIO-----".
           05  LINE 03  COLUMN 01
               VALUE  " CHAPA:           NOME:".
           05  LINE 03  COLUMN 41
               VALUE  "               DATA NASC:".
           05  LINE 04  COLUMN 01
               VALUE  " SEXO:                    GENERO:".
           05  LINE 04  COLUMN 41
               VALUE  "                  COD DEP:".
           05  LINE 05  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 05  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 06  COLUMN 01
               VALUE  " DENOMINACAO DEP:".
           05  LINE 06  COLUMN 41
               VALUE  "             STATUS DEP:".
           05  LINE 07  COLUMN 01
               VALUE  " NIVEL HIERARQUICO:".
           05  LINE 08  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 08  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 09  COLUMN 01
               VALUE  " COD. CARGO:              DENOMINACAO CARGO:".
           05  LINE 10  COLUMN 01
               VALUE  " TIPO DE SALARIO:                  SALAR".
           05  LINE 10  COLUMN 41
               VALUE  "IO BASE:".
           05  LINE 11  COLUMN 01
               VALUE  " STATUS DO CARGO:".
           05  LINE 12  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 12  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 13  COLUMN 01
               VALUE  " NIVEL SALARIAL:          IMP. RENDA:".
           05  LINE 13  COLUMN 41
               VALUE  "         QTD. DEPENDENTES:".
           05  LINE 14  COLUMN 01
               VALUE  " STATUS DO FUNCIONARIO:".
           05  LINE 15  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 16  COLUMN 01
               VALUE  " CEP DO FUNCIONARIO:                 LOG".
           05  LINE 16  COLUMN 41
               VALUE  "RADOURO:".
           05  LINE 17  COLUMN 01
               VALUE  " NUM:             COMPLEMENTO:".
           05  LINE 17  COLUMN 41
               VALUE  "        BAIRRO:".
           05  LINE 18  COLUMN 01
               VALUE  " CIDADE:                      UF:".
           05  LINE 19  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 19  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 01
               VALUE  " MENSAGEM:".
			   
           05  TCHAPA
               LINE 03  COLUMN 08  PIC 9(06)
               USING  CHAPA
               HIGHLIGHT.
           05  TNOME
               LINE 03  COLUMN 25  PIC x(30)
               USING  NOME
               HIGHLIGHT.
           05  TDTNASC
               LINE 03  COLUMN 67  PIC 99/99/9999
               USING  DTNASC
               HIGHLIGHT.
           05  TSEXO
               LINE 04  COLUMN 07  PIC X(02)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 04  COLUMN 09  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 04  COLUMN 34  PIC x(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO2
               LINE 04  COLUMN 36 PIC X(14)
               USING  TXTGENERO2
               HIGHLIGHT.
           05  TNIVELSALARIAL
               LINE 13  COLUMN 17 PIC 9(02)
               USING  NIVELSALARIAL
               HIGHLIGHT.
           05  TQTDDEPENDENTES
               LINE 13  COLUMN 67 PIC 9(01)
               USING  QTDDEPENDENTES
               HIGHLIGHT.
           05  TIMPRENDA
               LINE 13  COLUMN 39 PIC X(01)
               USING  IMPRENDA
               HIGHLIGHT.
           05  TFUNCSTATUS
               LINE 14  COLUMN 24  PIC X(25)
               USING  FUNCSTATUS
               HIGHLIGHT.
           05  TCEP-COMPLE
               LINE 17  COLUMN 31 PIC X(12)
               USING  CEP-COMPLE
               HIGHLIGHT.
           05  TCEP-NUMRES
               LINE 17  COLUMN 12 PIC 9(04)
               USING  CEP-NUMRES
               HIGHLIGHT.

           05  TCODDEP
               LINE 04  COLUMN 67  PIC 9(03)
               USING  CODDEP
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 06  COLUMN 20  PIC X(25)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TDEPSTATUS
               LINE 06  COLUMN 67  PIC X(10)
               USING  DEPSTATUS
               HIGHLIGHT.
           05  TTPHIERARC
               LINE 07 COLUMN 20 PIC 9(02)
               USING TPHIERARC
               HIGHLIGHT.

           05  TCODIGOCARG
               LINE 09 COLUMN 13  PIC 9(03)
               USING CODIGOCARG
               HIGHLIGHT.
           05  TDENOM
               LINE 09 COLUMN 45 PIC X(25)
               USING DENOM
               HIGHLIGHT.
           05  TTPSALARIO
               LINE 10  COLUMN 18  PIC X
               USING  TPSALARIO
               HIGHLIGHT.
           05  TSALARIOBASE
               LINE 10  COLUMN 50  PIC 999999.99
               USING  SALARIOBASE
               HIGHLIGHT.
           05  TSTATUSS
               LINE 11 COLUMN 18 PIC X(01)
               USING CARGSTATUS
               HIGHLIGHT.
           
           05  TCEPFUNC
               LINE 16 COLUMN 22 PIC 99999.999
               USING CEPFUNC
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 16 COLUMN 50 PIC X(20)
               USING CEP-LOGRA
               HIGHLIGHT.
           05  TBAIRRO
              LINE 17 COLUMN 57 PIC X(20)
               USING CEP-BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 18 COLUMN 09 PIC X(20)
               USING CEP-CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 18 COLUMN 34  PIC X(02)
               USING CEP-UF
               HIGHLIGHT.

       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQFUNC
                 CLOSE ARQFUNC
                 MOVE "*** ARQUIVO ARQFUNC FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO ARQFUNC" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN I-O ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO ARQDEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0C.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO ARQCARG NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQCARG" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0D.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADCEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE SPACES TO NOME SEXO IMPRENDA FUNCSTATUS DEPSTATUS
           MOVE SPACES TO CARGSTATUS CEP-COMPLE GENERO DENOM
           MOVE SPACES TO DENOMINACAO TPSALARIO 

           MOVE ZEROS TO CEP-NUMRES NIVELSALARIAL QTDDEPENDENTES
           MOVE ZEROS TO CHAPA W-SEL DTNASC TPHIERARC
           MOVE ZEROS TO CEPFUNC CODDEP CODIGOCARG SALARIOBASE

           DISPLAY  TELAFUNC.

       R2.
           ACCEPT TCHAPA.
           IF CHAPA < 1
              MOVE "CHAPA INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-ARQFUNC.
           READ ARQFUNC
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R8
                PERFORM R10
                PERFORM R16
                DISPLAY TELAFUNC
                MOVE "*** FUNCIONARIO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQFUNC" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FUNCIONARIO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.


       R3.
           ACCEPT TNOME.
           IF NOME = SPACES
              MOVE "CAMPO NOME VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R4.
           ACCEPT TDTNASC.
           IF W-ACT = 01
                   GO TO R3.



       R5.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R5A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TEXSEXO
           ELSE
              IF SEXO = "F"
                 MOVE "FEMENINO" TO TEXSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           DISPLAY TTEXSEXO.
       R6.
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               DISPLAY TTXTGENERO2.

       R7.
           ACCEPT TCODDEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFUNC
                   GO TO R5.
           IF CODDEP = 0
                 MOVE "*** CODIGO DEP. NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.

       R8.
           MOVE CODDEP TO CODIGO.
           READ ARQDEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** DEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R7
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           DISPLAY TELAFUNC.


       R9.
           ACCEPT TCODIGOCARG.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFUNC
                   GO TO R8.
           IF CODIGOCARG = 0
                 MOVE "*** CODIGO CARGO. NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.

       R10.
           MOVE CODIGOCARG TO COD.
           READ ARQCARG
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CARGO DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CARGO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAFUNC.

       R11.
           ACCEPT TNIVELSALARIAL.
           IF NIVELSALARIAL < 0 OR > 20
              MOVE "NIVEL SALARIAL INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.

       R12.
           ACCEPT TQTDDEPENDENTES.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R12.

       R13.
           ACCEPT TIMPRENDA.
           IF IMPRENDA = SPACES
              MOVE "CAMPO IMP. RENDA VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.

       R14.
           ACCEPT TFUNCSTATUS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.

       R14A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = FUNCSTATUS
              ADD 1 TO IND
              IF IND < 4
                 GO TO R14A
              ELSE
                 MOVE "STATUS INVALIDO" TO MENS
                 MOVE 1 TO IND
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R14
           ELSE
              DISPLAY TELAFUNC.
              MOVE 1 TO IND
              DISPLAY (14 , 24) TXTSTATUS2.

       R15.
           ACCEPT TCEPFUNC.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFUNC
                   GO TO R5.
           IF CEPFUNC = 0
                 MOVE "*** CEP FUNCIONARIO NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R14.


       R16.
           MOVE CEPFUNC TO CEP-NUMCEP.
           READ CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAFUNC.

       R17.
           ACCEPT TCEP-NUMRES.
           IF CEP-NUMRES < 0
              MOVE "NUMERO INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R16.
       R18.
           ACCEPT TCEP-COMPLE.
           IF W-ACT = 01
                   DISPLAY TELAFUNC
                   GO TO R16.

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
                WRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                     MOVE "*FUNCIONARIO JA EXISTE,DADOS NAO GRAVADOS*"
                     TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R1
                ELSE
                     MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQFUNC" TO MENS
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
                      GO TO R2.
      *
       EXC-OPC.
                DISPLAY "EXCLUIR   (S/N) : ".
                ACCEPT W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQFUNC RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DE FUNC. EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 12) "ALTERAR  (S/N) : ".
                ACCEPT W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DE CEP"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE ARQFUNC ARQCARG ARQDEP CADCEP.
           STOP RUN.

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 15) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 15) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
