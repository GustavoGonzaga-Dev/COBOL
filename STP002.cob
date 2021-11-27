       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP002. 
      *AUTOR: GUSTAVO GONZAGA DE FARIAS             *
      ***********************************************
      *----------------------------------------------------------------	   
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES
					FILE STATUS  IS ST-ERRO.
                    
           SELECT CADFORN ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CNPJ
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
					FILE STATUS  IS ST-ERRO.                   
      *
      *----------------------------------------------------------------- 
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CEP             PIC 9(08).
           03 ENDERECO        PIC X(40).
           03 BAIRRO          PIC X(20).
           03 CIDADE          PIC X(20).
           03 UF              PIC X(02).

       FD CADFORN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFORN.DAT".
       01 REGFORN.
           03 CNPJ            PIC 9(15).
           03 RAZAO           PIC X(40).
           03 NOME            PIC X(12).
           03 NUM             PIC 9(04).
           03 COMPREMENTO     PIC X(12).
           03 CEP-FORN        PIC 9(08).

       WORKING-STORAGE SECTION.
       77 ST-ERRO       PIC X(02) VALUE "00".
       77 MENS          PIC X(50) VALUE SPACES.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       01 AUX             PIC 9(02) VALUE ZEROS.

       01 BUFFER-ESTADOS.
          03 FILLER     PIC X(25) VALUE "RORONDONIA".
		  03 FILLER     PIC X(25) VALUE "ACACRE".
		  03 FILLER     PIC X(25) VALUE "AMAMAZONAS".
		  03 FILLER     PIC X(25) VALUE "RRRORAIMA".
		  03 FILLER     PIC X(25) VALUE "PAPARA".
		  03 FILLER     PIC X(25) VALUE "APAMAPA".
		  03 FILLER     PIC X(25) VALUE "TOTOCANTINS".
		  03 FILLER     PIC X(25) VALUE "MAMARANHAO".
		  03 FILLER     PIC X(25) VALUE "PIPIAUI".
		  03 FILLER     PIC X(25) VALUE "CECEARA".
		  03 FILLER     PIC X(25) VALUE "RNRIO GRANDE DO NORTE".
		  03 FILLER     PIC X(25) VALUE "PBPARAIBA".
		  03 FILLER     PIC X(25) VALUE "PEPERNAMBUCO".
		  03 FILLER     PIC X(25) VALUE "ALALAGOAS".
		  03 FILLER     PIC X(25) VALUE "SESERGIPE".
		  03 FILLER     PIC X(25) VALUE "BABAHIA".
		  03 FILLER     PIC X(25) VALUE "MGMINAS GERAIS".
		  03 FILLER     PIC X(25) VALUE "ESESPIRITO SANTO".
		  03 FILLER     PIC X(25) VALUE "RJRIO DE JANEIRO".
		  03 FILLER     PIC X(25) VALUE "SPSAO PAULO".
		  03 FILLER     PIC X(25) VALUE "PRPARANA".
		  03 FILLER     PIC X(25) VALUE "SCSANTA CATARINA".
		  03 FILLER     PIC X(25) VALUE "RSRIO GRANDE DO SUL".
		  03 FILLER     PIC X(25) VALUE "MSMATO GROSSO DO SUL".
		  03 FILLER     PIC X(25) VALUE "MTMATO GROSSO".
		  03 FILLER     PIC X(25) VALUE "GOGOIAS".
		  03 FILLER     PIC X(25) VALUE "DFDISTRITO FEDERAL".

       01 VETOR-ESTADOS REDEFINES BUFFER-ESTADOS.
           03 ESTADOS    PIC X(25) OCCURS 27 TIMES.

       01 VIEW-ESTADOS.
           03 VIEW-UF   PIC X(02) VALUE SPACES.
           03 ESTADO    PIC X(19) VALUE SPACES.
          
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05  LINE 02  COLUMN 01 
               VALUE  "    CADASTRO DE FORNECEDORES".
           05  LINE 04  COLUMN 01 
               VALUE  "    CNPJ:".
           05  LINE 06  COLUMN 01 
               VALUE  "    RAZAO SOCIAL:".
           05  LINE 08  COLUMN 01 
               VALUE  "    NOME FANTASIA:".
           05  LINE 10  COLUMN 01 
               VALUE  "    CEP:".
           05  LINE 12  COLUMN 01 
               VALUE  "    ENDERECO:".
           05  LINE 14  COLUMN 01 
               VALUE  "    NUMERO:".
           05  LINE 14  COLUMN 41 
               VALUE  "  COMPLEMENTO:".
           05  LINE 16  COLUMN 01 
               VALUE  "    BAIRRO:".
           05  LINE 18  COLUMN 01 
               VALUE  "    CIDADE:".
		   05  LINE 20  COLUMN 01
               VALUE  "    UF:".		   
           05  LINE 22  COLUMN 01 
               VALUE  "    MENSAGEM:".
           05  TCNPJ
               LINE 04  COLUMN 11  PIC 999.999.999.9999.99
               USING  CNPJ.
           05  TRAZAO
               LINE 06  COLUMN 19  PIC X(40)
               USING  RAZAO.
           05  TNOME
               LINE 08  COLUMN 20  PIC X(12)
               USING  NOME.
           05  TCEP
               LINE 10  COLUMN 10  PIC 99999.999
               USING  CEP.
           05  TENDERECO
               LINE 12  COLUMN 15  PIC X(40)
               USING  ENDERECO.
           05  TNUM
               LINE 14  COLUMN 12  PIC 9(04)
               USING  NUM.
           05  TCOMPREMENTO
               LINE 14  COLUMN 56  PIC X(12)
               USING  COMPREMENTO.
           05  TBAIRRO
               LINE 16  COLUMN 13  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 18  COLUMN 13  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 20  COLUMN 10  PIC X(02)
               USING  UF.
           05  TMENS
               LINE 22  COLUMN 15  PIC X(50)
               USING  MENS.
          
      *-----------------------------------------------------------------			   
       PROCEDURE DIVISION.

       CREATE-FILES.
           PERFORM I-O-CADCEP.
           PERFORM I-O-CADFORN.
      *
      *------------[ INICIALIZACAO DAS VARIAVEIS ]--------------------- 
       CLEAR-DATA.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF VIEW-ESTADOS.
           MOVE ZEROS TO CEP.
           MOVE SPACES TO RAZAO NOME COMPREMENTO.
           MOVE ZEROS TO CNPJ NUM CEP-FORN.
       
      *---------[VISUALIZACAO DA TELA]--------------------           
		   DISPLAY TELA.

       INPUT-CNPJ.
           ACCEPT TCNPJ.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO ROT-FIM.

           IF CNPJ = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-CNPJ.

           PERFORM READ-CADFORN
           IF ST-ERRO = "00"
               MOVE CEP-FORN TO CEP
               PERFORM READ-CADCEP.

       INPUT-RAZAO.
           ACCEPT TRAZAO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CNPJ.

           IF RAZAO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-RAZAO.

       INPUT-NOME.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-RAZAO.

           IF NOME = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-NOME.

       INPUT-CEP.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-NOME.

           IF CEP = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-CEP.

           PERFORM READ-CADCEP.

       INPUT-ENDERECO.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CEP.

           IF ENDERECO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-ENDERECO.

       INPUT-NUM.
           ACCEPT TNUM.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-ENDERECO.

           IF NUM = ZEROS
               PERFORM ERROR-EMPTY
               GO TO INPUT-NUM.

       INPUT-COMPREMENTO.
           ACCEPT TCOMPREMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-NUM.

       INPUT-BAIRRO.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-COMPREMENTO.

           IF BAIRRO = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-BAIRRO.

       INPUT-CIDADE.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-BAIRRO.

           IF CIDADE = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-CIDADE.

       INPUT-TUF.
           ACCEPT TUF.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO INPUT-CIDADE.

           IF UF = SPACES
               PERFORM ERROR-EMPTY
               GO TO INPUT-TUF.

           PERFORM CHECK-UF THRU CHECK-UF-END

           IF UF NOT = VIEW-UF
               PERFORM ERROR-VALID
               GO TO INPUT-TUF.

           

       GRAVAR-FORNECEDOR.
           MOVE "VOCE QUER GRAVAR OS DADOS DO FORNECEDOR? (S/N)" TO MENS
           DISPLAY TMENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY TMENS.

           IF W-ACT = 01 GO TO INPUT-TUF.

           IF W-OPCAO = "S" or "s"
               MOVE CEP TO CEP-FORN
               PERFORM WRITE-CADFORN
           ELSE IF W-OPCAO = "N" or "n" GO TO GRAVAR-CEP
           ELSE PERFORM ERROR-VALID GO TO GRAVAR-FORNECEDOR.

       GRAVAR-CEP.
           MOVE "VOCE QUER GRAVAR OS DADOS DO CEP? (S/N)" TO MENS
           DISPLAY TMENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY TMENS.

           IF W-ACT = 01 GO TO INPUT-TUF.

           IF W-OPCAO = "S" or "s" PERFORM WRITE-CADCEP
           ELSE IF W-OPCAO = "N" or "n" GO TO CLEAR-DATA
           ELSE PERFORM ERROR-VALID GO TO GRAVAR-CEP.

           GO TO CLEAR-DATA.

       ROT-FIM.
           CLOSE CADCEP
           CLOSE CADFORN
           STOP RUN.

       CHECK-UF.
           MOVE 1 TO AUX.

       CHECK-UF-END.
           MOVE ESTADOS(AUX) TO VIEW-ESTADOS
           IF UF = VIEW-UF MOVE 27 TO AUX.
           ADD 1 TO AUX
           IF AUX < 28 GO TO CHECK-UF-END.

       ERROR-VALID.
           MOVE "OPCAO NAO VALIDA" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-EMPTY.
           MOVE "O CAMPO PRECISA SER INFORMADO" TO MENS
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

       I-O-CADCEP.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADCEP
			  ELSE 
			    IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM				  
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO ROT-FIM.

       I-O-CADFORN.
           OPEN I-O CADFORN
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFORN
                 CLOSE CADFORN
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADFORN
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO ROT-FIM.

       WRITE-CADCEP.
           WRITE REGCEP
           IF ST-ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED

           ELSE IF ST-ERRO = "22" REWRITE REGCEP
               IF ST-ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
               ELSE PERFORM ERROR-DATA-NOT-SAVED

           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO ROT-FIM.

       WRITE-CADFORN.
           WRITE REGFORN
           IF ST-ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED

           ELSE IF ST-ERRO = "22" REWRITE REGFORN
               IF ST-ERRO = "00" OR "02" PERFORM LOG-DATA-SAVED
               ELSE PERFORM ERROR-DATA-NOT-SAVED

           ELSE PERFORM ERROR-DATA-NOT-SAVED GO TO ROT-FIM.

       READ-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                  PERFORM CHECK-UF THRU CHECK-UF-END
                  DISPLAY TELA
              ELSE PERFORM ERROR-FILE-NOT-READED GO TO ROT-FIM.

       READ-CADFORN.
           READ CADFORN
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00" DISPLAY TELA
              ELSE PERFORM ERROR-FILE-NOT-READED GO TO ROT-FIM.

       DELETE-CADCEP.
           DELETE CADCEP RECORD
           IF ST-ERRO = "00" PERFORM LOG-DATA-DELETED
           ELSE PERFORM ERROR-DATA-NOT-DELETED GO TO ROT-FIM.

       DELETE-CADFORN.
           DELETE CADFORN RECORD
           IF ST-ERRO = "00" PERFORM LOG-DATA-DELETED
           ELSE PERFORM ERROR-DATA-NOT-DELETED GO TO ROT-FIM.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY TMENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
               GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY TMENS.
       ROT-MENS-FIM.
