       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP013.
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

           SELECT ARQDEPE ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS DEPEN-KEY
                      ALTERNATE RECORD KEY IS DEPEN-NOME
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS COD
                      ALTERNATE RECORD KEY IS DENOM WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.
                      
           SELECT ARQDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODIGO
                      ALTERNATE RECORD KEY IS DENOMINACAO
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
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

       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
          03 COD                PIC 9(03).
          03 DENOM              PIC X(45).
          03 TPSALARIO          PIC X(01).
          03 SALARIOBASE        PIC 9(06)V99.
          03 CARGSTATUS         PIC X(01).

       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
          03 CODIGO                   PIC 9(03).
          03 DENOMINACAO              PIC X(25).
          03 DEPSTATUS                PIC X(01).
          03 TPHIERARC                PIC 9(02).
		  
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CHAPAENTR    PIC 9(03) VALUE ZEROS.
       01 NOMENTR      PIC X(45) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.

       01 TABSTATUS.
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(15) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS2 PIC X(14) VALUE SPACES.

       01 DET2.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETCHAPA        PIC 999999.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETNOME         PIC X(29) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETDEP          PIC X(30).
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETDENOM        PIC X(30).
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETSEQ          PIC 9.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETNOMEDEP      PIC X(30).
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETGRAU         PIC X(20).
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETDATANASC     PIC 99/99/9999.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETSTATUS       PIC X(20) VALUE SPACES.

       SCREEN SECTION.
       01  TELACONSDEPEN.
           05 BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "-----CONSULTA DE DEPENDENTES-----".
           05  LINE 04  COLUMN 01
               VALUE  "CODIGO DO FUNCIONARIO:".
           05  LINE 04  COLUMN 41
               VALUE  "NOME:".
           05  LINE 05  COLUMN 01
               VALUE  "DEPARTAMENTO:".
           05  LINE 05  COLUMN 41
               VALUE  "CARGO:". 
           05  LINE 07 COLUMN 01
               VALUE "    ****DEPENDENTE***                  GRAU".
           05  LINE 07 COLUMN 48
               VALUE "    DATA".
           05  LINE 08 COLUMN 01
               VALUE "SEQ        NOME                     PARENTESCO".
           05  LINE 08 COLUMN 50
               VALUE "NASCM.                STATUS". 
           05  LINE 09 COLUMN 01
               VALUE "------------------------------------- --------".
           05  LINE 09 COLUMN 41
               VALUE "----------------------------------".  
           05  LINE 22  COLUMN 01
               VALUE  "  MENSAGEM:".
			   
           05  TCHAPAENTR
               LINE 04  COLUMN 23  PIC 9(06)
               USING CHAPAENTR.
           05  TNOMENTR
               LINE 04  COLUMN 47  PIC X(30)
               USING NOMENTR.

       PROCEDURE DIVISION.
       INICIO.

       INC-OP1.
           OPEN INPUT ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQFUNC NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
       INC-OP2.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQCARG NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

       INC-OP3.
           OPEN INPUT ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

       INC-OP4.
           OPEN INPUT ARQDEPE
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO ARQDEPE NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEPE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       INC-001.
           MOVE SPACES TO NOMENTR 
           MOVE ZEROS TO CHAPAENTR 

           DISPLAY  TELACONSDEPEN.

       INC-001A.
           DISPLAY (22, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/CHAPA"
           ACCEPT TCHAPAENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CHAPAENTR TO CHAPA
           START ARQFUNC KEY IS NOT LESS CHAPA INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (22, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/NOME "
           ACCEPT TNOMENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001B.
           MOVE NOMENTR TO NOME
           START ARQFUNC KEY IS NOT LESS NOME INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.


       INC-RD2.
           DISPLAY (22, 13) LIMPA
           READ ARQFUNC NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 PERFORM ROT-MONTAR
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       
       INC-RD3.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQDEPE
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001B.
           START ARQDEPE KEY IS NOT LESS DEPEN-SEQ INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.

       LER-ARQDEPE.
           READ ARQDEPE NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 IF CHAPA = DEPEN-CHAPA
                 PERFORM ROT-MONTAR
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQDEPE"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       LER-ARQDEP.
           READ ARQDEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 PERFORM ROT-MONTAR
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       
       LER-ARQCARG.
           READ ARQCARG NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 PERFORM ROT-MONTAR
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

           IF FUNCSTATUS = "A"
               MOVE "ATIVA" TO TXTSTATUS
           ELSE
               IF FUNCSTATUS = "S"
                  MOVE "SUSPENSA" TO TXTSTATUS
               ELSE
                   IF FUNCSTATUS = "D"
                      MOVE "DESATIVADA" TO TXTSTATUS.
           

       ROT-MONTAR.

           MOVE DENOMINACAO TO DETDEP 
           DISPLAY (04, 47) NOME
           DISPLAY (05, 47) DENOM
           DISPLAY (05, 15) DENOMINACAO
           MOVE DEPEN-SEQ TO DETSEQ
           MOVE DEPEN-NOME TO DETNOMEDEP
           MOVE "ESPOSA" TO DETGRAU
           MOVE DATANASC TO DETDATANASC
           MOVE TXTSTATUS2  TO DETSTATUS

           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 12
                  GO TO INC-RD2.

       ROT-SOL.
           DISPLAY (22, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (22, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACONSDEPEN
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           MOVE ZEROS TO CHAPAENTR
           GO TO INC-001.

       NOV-SOL.
           DISPLAY (22, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (22, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.

       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQCARG ARQDEP ARQDEPE ARQFUNC.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.

       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (22, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (22, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.