       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP001.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.       
           SELECT ARQDEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.							
       DATA DIVISION.
       FILE SECTION.
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
           03 CODIGO          PIC 9(03).
           03 DENOMINACAO     PIC X(25).
           03 HIERARQUIA      PIC 9(02).
           03 STATUS1         PIC X(01).
       WORKING-STORAGE SECTION.
       01 W-CONT             PIC 9(04) VALUE ZEROS.
       77 W-ACT              PIC 9(02) VALUE ZEROS.	   
       01 W-OPCAO            PIC X(01) VALUE SPACES.
	   01 OPCAO              PIC 9(02) VALUE ZEROS.
       01 W-SEL              PIC 9(01) VALUE ZEROS.	   
       01 ST-ERRO            PIC X(02) VALUE "00".
       01 MENS               PIC X(50) VALUE SPACES.
       01 LIMPA              PIC X(50) VALUE SPACES.
       01 W-PROGR            PIC X(07) VALUE SPACES.
       01 AUX                PIC 9(02) VALUE ZEROS.
       01 TXTAPLICACAO       PIC X(25) VALUE SPACES.	   	   
	   01 DISAUX             PIC X(25) VALUE SPACES.	   
	   01 TABAPLICACAO.
	      03 TBAPLICACAO  PIC X(25) OCCURS 10 TIMES.	  
	  
       01 TABELA-STATUS.
          03 FILLER     PIC X(13) VALUE "AATIVA".
		  03 FILLER     PIC X(13) VALUE "SSUSPENSA".
		  03 FILLER     PIC X(13) VALUE "DDESATIVADA".
      
       01 TABELA REDEFINES TABELA-STATUS.
          03 TBSTATUS  PIC X(13) OCCURS 3 TIMES.
      
       01 TXT.
          03 TXT1 PIC X(01) VALUE SPACES.
          03 TXT2 PIC X(12) VALUE SPACES. 	  
	     
       SCREEN SECTION.	   
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "-----CADASTRO DE DEPARTAMENTO-----".
           05  LINE 05  COLUMN 01 
               VALUE  "            CODIGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "            DENOMINACAO:".
		   05  LINE 09  COLUMN 01
		       VALUE  "            NIVEL HIERARQUICO:".
           05  LINE 11  COLUMN 01 
               VALUE  "            STATUS :".           
		   05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".			   
           05  TCODIGO
               LINE 05  COLUMN 26  PIC 9(03)
               USING  CODIGO.
           05  TDENOMINACAO
               LINE 07  COLUMN 26  PIC X(25)
               USING  DENOMINACAO.
		   05  THIERARQUIA
		       LINE 09  COLUMN 35  PIC 9(02)
			   USING HIERARQUIA.
           05  TDISAUX
               LINE 09  COLUMN 38 PIC X(25)
               USING TXTAPLICACAO.
           05  TSTATUS1
		       LINE 11  COLUMN 26 PIC X(01)
			   USING STATUS1.
		   05  TSTATUS2
		       LINE 11  COLUMN 29  PIC X(11)
			   USING TXT2.			

       01  TELA-HIERARQUIA.
           05  LINE 13  COLUMN 50
		       VALUE  "1-CONSELHO CONSULTIVO".
           05  LINE 14  COLUMN 50
		       VALUE  "2-CONSELHO FISCA".
           05  LINE 15  COLUMN 50
		       VALUE  "3-PRESIDENCIA".
           05  LINE 16  COLUMN 50
		       VALUE  "4-DIRETORIA ADMINISTRATIVA". 
           05  LINE 17  COLUMN 50
		       VALUE  "5-DIRETORIA INDUSTRIAL".
           05  LINE 18  COLUMN 50
		       VALUE  "6-DIRETORIA COMERCIAL".
           05  LINE 19  COLUMN 50
		       VALUE  "7-DIRETORIA MARKETING".
           05  LINE 20  COLUMN 50
		       VALUE  "8-DIRETORIA JURIDICA". 
           05  LINE 21  COLUMN 50
		       VALUE  "9-CONSULTORIA". 
           05  LINE 22  COLUMN 50
		       VALUE  "      ". 	

       01  TELA-STATUS.
           05  LINE 13  COLUMN 50
		       VALUE  "A-ATIVA".
           05  LINE 14  COLUMN 50
		       VALUE  "S-SUSPENSA".
           05  LINE 15  COLUMN 50
		       VALUE  "D-DESATIVADA".
			   
       PROCEDURE DIVISION.
       INICIO.
           MOVE "CONSELHO CONSULTIVO"       TO TBAPLICACAO(01)
           MOVE "CONSELHO FISCAL"           TO TBAPLICACAO(02)
           MOVE "PRESIDENCIA"               TO TBAPLICACAO(03)
           MOVE "DIRETORIA ADMINISTRATIVA"  TO TBAPLICACAO(04)
           MOVE "DIRETORIA INDUSTRIAL"      TO TBAPLICACAO(05)
           MOVE "DIRETORIA COMERCIAL"       TO TBAPLICACAO(06)
           MOVE "DIRETORIA MARKETING"       TO TBAPLICACAO(07)
           MOVE "DIRETORIA JURIDICA"        TO TBAPLICACAO(08)
           MOVE "CONSULTORIA"               TO TBAPLICACAO(09).	   
	   

       CREATE-FILES.
	       PERFORM I-O-ARQDEP.	      
       R0.
                MOVE ZEROS  TO CODIGO HIERARQUIA TXTAPLICACAO.
				MOVE ZEROS  TO W-SEL.
                MOVE SPACES TO DENOMINACAO STATUS1 TXT.
               DISPLAY TELA.
	  
       R1.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		      GO TO ROT-FIM.
           IF CODIGO = ZEROS
              MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
			  GO TO R1.
                           
              PERFORM READ-ARQDEP.
           IF ST-ERRO = "00" 
		        MOVE "*** CODIGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001.

       R2. 
                ACCEPT TDENOMINACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 
                   GO TO R1.
                IF DENOMINACAO  = SPACES
                   MOVE " ESPACO NAO PODE FICAR EM BRANCO " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2.  	

       R3.
	       DISPLAY TELA-HIERARQUIA
           ACCEPT THIERARQUIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R2.
           IF HIERARQUIA = 0 OR HIERARQUIA < 1 OR HIERARQUIA > 9
                 MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3.	
       R3A.
           MOVE TBAPLICACAO(HIERARQUIA) TO TXTAPLICACAO
           DISPLAY TDISAUX.
           DISPLAY TELA. 
	   
       R4.
	       DISPLAY TELA-STATUS
           MOVE 1 TO AUX
           ACCEPT TSTATUS1.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
		   IF STATUS1 = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.	
       R4A.
           MOVE TBSTATUS(AUX) TO TXT
           IF TXT1 NOT = STATUS1
              ADD 1 TO AUX
              IF AUX < 4
                 GO TO R4A
              ELSE
                 MOVE "*** TIPO STATUS INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
           ELSE
               DISPLAY TSTATUS2.
			   DISPLAY TELA.	  
 			
       SALVAR.
           MOVE "VOCE QUER GRAVAR OS DADOS? (S/N)" TO MENS
           DISPLAY (23, 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (23, 12) MENS.

           IF W-ACT = 01 GO TO R4.

           IF W-OPCAO = "S" or "s"
			   PERFORM WRITE-ARQDEP
           ELSE IF W-OPCAO = "N" or "n" GO TO R0
           ELSE PERFORM ERROR-VALID GO TO SALVAR.
           GO TO R0.

       DELETAR.
           MOVE "VOCE QUER EXCLUIR OS DADOS? (S/N)" TO MENS
           DISPLAY (23 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (23, 12) MENS.

           IF W-ACT = 01 GO TO R4.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-ARQDEP
               GO TO R0
           ELSE IF W-OPCAO = "N" or "n" NEXT SENTENCE
           ELSE PERFORM ERROR-VALID GO TO DELETAR.
 
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R0  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
			    ELSE
				   IF W-OPCAO = "E"
				      GO TO DELETAR.

       ERROR-VALID.
           MOVE "OPCAO NAO VALIDA" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-EMPTY.
           MOVE " ESPACO NAO PODE FICAR EM BRANCO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-FILE-NOT-OPENED.
           MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-SAVED.
           MOVE "DADOS NAO GRAVADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-FOUND.
           MOVE "DADOS NAO ENCONTRADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-FILE-NOT-READED.
           MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-DELETED.
           MOVE "DADOS NAO DELETADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-FILE-CREATED.
           MOVE "ARQUIVO CRIADO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-DATA-SAVED.
           MOVE "DADOS GRAVADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-DATA-DELETED.
           MOVE "DADOS DELETADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       I-O-ARQDEP.
	       OPEN I-O ARQDEP
		   IF ST-ERRO NOT = "00"
		      IF ST-ERRO = "30"
			   OPEN OUTPUT ARQDEP
			   CLOSE ARQDEP
			   PERFORM LOG-FILE-CREATED
			   GO TO I-O-ARQDEP
		      ELSE
			     IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXECUTADO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO ROT-FIM
			    ELSE 
				    PERFORM ERROR-FILE-NOT-OPENED
					GO TO ROT-FIM.

       CHECK-WRITE.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE 
			  PERFORM ERROR-DATA-NOT-SAVED 
			  GO TO ROT-FIM.

       CHECK-READ.
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00" 
			   PERFORM R3A
			   PERFORM R4A
 			   DISPLAY TELA		
			
           ELSE 
              PERFORM ERROR-FILE-NOT-READED 
			  GO TO ROT-FIM.

       CHECK-DELETE.
           IF ST-ERRO = "00" 
		      PERFORM LOG-DATA-DELETED
           ELSE 
		      PERFORM ERROR-DATA-NOT-DELETED 
			  GO TO ROT-FIM.

       WRITE-ARQDEP.
           WRITE REGDEP.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
              PERFORM REWRITE-ARQDEP
           ELSE PERFORM ERROR-DATA-NOT-SAVED 
		      GO TO ROT-FIM.

       REWRITE-ARQDEP.
     	   REWRITE REGDEP. 
		   PERFORM CHECK-WRITE.	

       READ-ARQDEP.
     	   READ ARQDEP. 
		   PERFORM CHECK-READ.
           IF ST-ERRO = "00" 
           DISPLAY TELA.
		
       DELETE-ARQDEP. 
	       DELETE ARQDEP RECORD. 
		   PERFORM CHECK-DELETE.

       ROT-FIM.
           CLOSE ARQDEP
           STOP RUN.
           EXIT PROGRAM.

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
