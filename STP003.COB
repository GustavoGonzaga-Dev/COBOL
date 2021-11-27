       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP003.
      *AUTOR: GUSTAVO GONZAGA DE FARIAS              *
      ***********************************************
      *      MANUTENCAO DO CADASTRO DE PRODUTO      *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
		   SELECT CADPROD ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODGPRODUTO
                      ALTERNATE RECORD KEY IS DESCRICAO WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
					  
           SELECT CADFORN ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CNPJ
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.	

           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.						
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPROD
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADPROD.DAT".
       01 REGPROD.
           03 CODGPRODUTO PIC 9(06).
           03 DESCRICAO PIC X(30).
		   03 UNIDADE PIC X(02).
		   03 APLICACAO PIC 9(01).
           03 QUANTIDADE PIC 9(05)V9.
           03 PRECO PIC 9(07)V99.
           03 CNPJ-PROD  PIC 9(15).
		   
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
           03 CEP-FORN        PIC 9(08).
		   03 COMPLEMENTO     PIC X(11).	
		   03 NUMERO          PIC 9(04).		
		   

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 AUX             PIC 9(02) VALUE ZEROS.
       01 TXTAPLICACAO    PIC X(15) VALUE SPACES.	
	   01 AUX2          PIC 9(02) VALUE ZEROS.	   
       
	   01 DISPESTADO    PIC X(23) VALUE SPACES.	   
	   01 DISAUX    PIC X(23) VALUE SPACES.
	   
		   
	   01 TABAPLICACAO.
	      03 TBAPLICACAO  PIC X(15) OCCURS 8 TIMES.
	   
      *
       01 TABSITUACAO1.
          03 FILLER     PIC X(13) VALUE "LTLITRO".
		  03 FILLER     PIC X(13) VALUE "PCPECA".
		  03 FILLER     PIC X(13) VALUE "KGTONELADA".
		  03 FILLER     PIC X(13) VALUE "ARARROBA".
		  03 FILLER     PIC X(13) VALUE "GRGRAMA".
		  03 FILLER     PIC X(13) VALUE "MTMETRO".
		  03 FILLER     PIC X(13) VALUE "CMCENTIMETRO".
		  03 FILLER     PIC X(13) VALUE "CJCONJUNTO".
      *
       01 TABSITUACAO REDEFINES TABSITUACAO1.
          03 TBSITUACAO   PIC X(13) OCCURS 8 TIMES.
      *
       01 TXTSITUACAO.
          03 TXTSITUA1 PIC X(02) VALUE SPACES.
          03 TXTSITUA2 PIC X(11) VALUE SPACES. 
		  
       01 TABUF1.
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
      *
	   01 TABUF REDEFINES TABUF1.
	      03 TBUF PIC X(25) OCCURS 27 TIMES.
      *
	   01 TXTUF.
	      03 TXTUF1 PIC X(02) VALUE SPACES.
		  03 TXTUF2 PIC X(23) VALUE SPACES.		  
          
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "AREA DO PRODUTO           S2".
           05  LINE 03  COLUMN 01 
               VALUE  "CODIGO:".
           05  LINE 03  COLUMN 35 
               VALUE  "DESCRICAO:".
		   05  LINE 05 COLUMN 01
		       VALUE  "UNIDADE:".
           05  LINE 05  COLUMN 35 
               VALUE  "APLICACAO:".
           05  LINE 07  COLUMN 01 
               VALUE  "QUANTIDADE:".
           05  LINE 07  COLUMN 35 
               VALUE  "PRECO:".	 
           05  LINE 09  COLUMN 01
               VALUE  "------------------------------------------".		
           05  LINE 09  COLUMN 40
               VALUE  "------------------------------------------".				   
           05  LINE 10  COLUMN 01
               VALUE  "AREA DO FORNECEDOR        S2".
           05  LINE 11  COLUMN 01
		       VALUE  "CNPJ: ".
		   05  LINE 11  COLUMN 26
		       VALUE  "RAZAO SOCIAL:".
		   05  LINE 13  COLUMN 01
		       VALUE  "NOME FANTASIA:".
		   05  LINE 15  COLUMN 01
		       VALUE  "CEP:".
		   05  LINE 15  COLUMN 26
		       VALUE  "ENDERECO:".
           05  LINE 17  COLUMN 01 
               VALUE  "NUMERO:".
           05  LINE 17  COLUMN 26 
               VALUE  "COMPLEMENTO:".
           05  LINE 19  COLUMN 01 
               VALUE  "BAIRRO:".
           05  LINE 21  COLUMN 01 
               VALUE  "CIDADE:".
		   05  LINE 23  COLUMN 01
               VALUE  "UF:".		      
           05  LINE 25  COLUMN 01
               VALUE  "MENSAGEM:".
	   
           05  TCODIGO
               LINE 03 COLUMN 15 PIC 999.999
               USING CODGPRODUTO.
               
           05  TDESCRICAO
               LINE 03 COLUMN 45 PIC X(30)
               USING DESCRICAO.
 
           05  TUNIDADE
               LINE 05 COLUMN 15 PIC X(02)
               USING UNIDADE.

		   05  TTXTSITUACAO
		       LINE 05 COLUMN 18 PIC X(11)
			   USING TXTSITUA2.
			   
		   05  TAPLICACAO 
               LINE 05 COLUMN 45 PIC 9
               USING APLICACAO.

           05  TDISAUX
               LINE 05 COLUMN 47 PIC X(15)
               USING TXTAPLICACAO.

			   
		   05  TQUANTIDADE
		       LINE 07 COLUMN 15 PIC ZZ.ZZ9,9
			   USING QUANTIDADE.

           05  TPRECO
               LINE 07 COLUMN 45 PIC ZZZ.ZZ9,99
               USING PRECO.

			   
		   05  TCNPJ
		       LINE 11 COLUMN 6 PIC 999.999.999.9999.99
			   USING CNPJ.

		   05  TRAZAO
		       LINE 11 COLUMN 39 PIC X(40)
               USING RAZAO.
			   
           05  TNOME 
		       LINE 13 COLUMN 15 PIC X(12)
			   USING NOME.

		   05  TCEP 
		       LINE 15 COLUMN 6  PIC 99999.999
			   USING CEP.
		   05  TENDERECO
		       LINE 15 COLUMN 36 PIC X(40)
			   USING ENDERECO.

           05  TNUMERO
               LINE 17  COLUMN 10  PIC 9(04)
               USING  NUMERO.
           05  TCOMPLEMENTO
               LINE 17  COLUMN 40  PIC X(12)
               USING  COMPLEMENTO.
           05  TBAIRRO
               LINE 19  COLUMN 10  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 21  COLUMN 10  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 23  COLUMN 6  PIC X(02)
               USING  UF.
           05  TDISPESTADO
               LINE 23 COLUMN 10 PIC X(23)
               USING DISPESTADO.			   
      *
       01  TELAINFO.
           05  LINE 13  COLUMN 60
		       VALUE  "LT-LITRO".
           05  LINE 14  COLUMN 60
		       VALUE  "PC-PECA".
           05  LINE 15  COLUMN 60
		       VALUE  "KG-TONELADA".
           05  LINE 16  COLUMN 60
		       VALUE  "AR-ARROBA".          
           05  LINE 17  COLUMN 60
		       VALUE  "GR-GRAMA".
           05  LINE 18  COLUMN 60
		       VALUE  "MT-METRO".      
           05  LINE 19  COLUMN 60
		       VALUE  "CM-CENTIMETRO".
           05  LINE 20  COLUMN 60
		       VALUE  "CJ-CONJUNTO".             
                  			   
			   
       01  TELASIT.
           05  LINE 13  COLUMN 60
		       VALUE  "1-SAUDE".
           05  LINE 14  COLUMN 60
		       VALUE  "2-HIGIENE PESSOAL".
           05  LINE 15  COLUMN 60
		       VALUE  "3-ALIMENTO".
           05  LINE 16  COLUMN 60
		       VALUE  "4-LIMPEZA". 
           05  LINE 17  COLUMN 60
		       VALUE  "5-SEGURANCA".
           05  LINE 18  COLUMN 60
		       VALUE  "6-GESTAO".
           05  LINE 19  COLUMN 60
		       VALUE  "7-VESTUARIO".
           05  LINE 20  COLUMN 60
		       VALUE  "8-OUTRAS". 
			   
       01  TELAUF.
           05  LINE 10  COLUMN 35
		       VALUE  "|RO-RONDONIA".
           05  LINE 11  COLUMN 35
		       VALUE  "|AC-ACRE".
           05  LINE 12  COLUMN 35
		       VALUE  "|AM-AMAZONAS".
           05  LINE 13  COLUMN 35
		       VALUE  "|RR-RORAIMA".          
           05  LINE 14  COLUMN 35
		       VALUE  "|PA-PARA".
           05  LINE 15  COLUMN 35
		       VALUE  "|AP-AMAPA".      
           05  LINE 16  COLUMN 35
		       VALUE  "|TO-TOCANTINS".
           05  LINE 17  COLUMN 35
		       VALUE  "|MA-MARANHAO".             
           05  LINE 18  COLUMN 35
		       VALUE  "|PI-PIAUI". 
           05  LINE 19  COLUMN 35
               VALUE  "|CE-CEARA".		   
           05  LINE 20  COLUMN 35
		       VALUE  "|RN-RIO GRANDE DO NORTE".
           05  LINE 21  COLUMN 35
		       VALUE  "|PB-PARAIBA".
           05  LINE 22  COLUMN 35
		       VALUE  "|PE-PERNAMBUCO".          
           05  LINE 23  COLUMN 35
		       VALUE  "|AL-ALAGOAS".
           05  LINE 10  COLUMN 59
		       VALUE  "|SE-SERGIPE".      
           05  LINE 11  COLUMN 59
		       VALUE  "|BA-BAHIA".
           05  LINE 12  COLUMN 59
		       VALUE  "|MG-MINAS GERAIS".             
           05  LINE 13  COLUMN 59
		       VALUE  "|ES-ESPIRITO SANTO". 
           05  LINE 14  COLUMN 59
               VALUE  "|RJ-RIO DE JANEIRO".	
           05  LINE 15  COLUMN 59
		       VALUE  "|SP-SAO PAULO".
           05  LINE 16  COLUMN 59
		       VALUE  "|PR-PARANA".          
           05  LINE 17  COLUMN 59
		       VALUE  "|SC-SANTA CATARINA".
           05  LINE 18  COLUMN 59
		       VALUE  "|RS-RIO GRANDE DO SUL".      
           05  LINE 19  COLUMN 59
		       VALUE  "|MS-MATO GROSSO DO SUL".
           05  LINE 20  COLUMN 59
		       VALUE  "|MT-MATO GROSSO".             
           05  LINE 21  COLUMN 59
		       VALUE  "|GO-GOIAS". 
           05  LINE 22  COLUMN 59
               VALUE  "|DF-DISTRITO FEDERAL".   			   
			   
		
      *-----------------------------------------------------------------    
       PROCEDURE DIVISION. 

		   
       INICIO.
           MOVE "SAUDE"           TO TBAPLICACAO(1)
           MOVE "HIGIENE PESSOAL" TO TBAPLICACAO(2)
           MOVE "ALIMENTO"        TO TBAPLICACAO(3)
           MOVE "LIMPEZA"         TO TBAPLICACAO(4)
           MOVE "SEGURANCA"       TO TBAPLICACAO(5)
           MOVE "GESTAO"          TO TBAPLICACAO(6)
           MOVE "VESTUARIO"       TO TBAPLICACAO(7)
           MOVE "OUTRAS"          TO TBAPLICACAO(8).



       CREATE-FILES.
	       PERFORM I-O-CADPROD.
           PERFORM I-O-CADCEP.
           PERFORM I-O-CADFORN.		
      *
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------  
       R1.
     	   MOVE SPACES TO DESCRICAO TXTAPLICACAO.
		   MOVE ZEROS TO  CODGPRODUTO QUANTIDADE CNPJ-PROD.		   
           MOVE SPACES TO UNIDADE TXTSITUACAO.
           MOVE ZEROS  TO APLICACAO PRECO W-SEL.
       R1A.
		   MOVE SPACES TO RAZAO  NOME COMPLEMENTO.	   
           MOVE ZEROS  TO CNPJ CEP-FORN NUMERO.	   
       R1B.
		   MOVE SPACES TO ENDERECO DISPESTADO BAIRRO CIDADE UF.	   
		   MOVE ZEROS TO CEP.
      *---------[VISUALIZACAO DA TELA]--------------------
           DISPLAY TELA.
           
       R2.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		      GO TO ROT-FIM.
           IF CODGPRODUTO = ZEROS
              MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
                           
              PERFORM READ-CADPROD.
           IF ST-ERRO = "00" 
		        MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001.
		   
       R3.       
           ACCEPT TDESCRICAO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO R2.
		   IF DESCRICAO = SPACES
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.		  
       R4.
	       DISPLAY TELAINFO
           MOVE 1 TO AUX
           ACCEPT TUNIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
		   IF UNIDADE = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.	
       R4A.
           MOVE TBSITUACAO(AUX) TO TXTSITUACAO
           IF TXTSITUA1 NOT = UNIDADE
              ADD 1 TO AUX
              IF AUX < 9
                 GO TO R4A
              ELSE
                 MOVE "*** TIPO UNIDADE INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
           ELSE
               DISPLAY TTXTSITUACAO.
			   DISPLAY TELA.
			  
       R5.
	       DISPLAY TELASIT
           ACCEPT TAPLICACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R4.
           IF APLICACAO = 0 
                 MOVE "*** DIGITE APENAS DE 1 ATE 8 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
       R5A.
           MOVE TBAPLICACAO(APLICACAO) TO TXTAPLICACAO
           DISPLAY TDISAUX.
           DISPLAY TELA.
		   
       R6.
	       ACCEPT TQUANTIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R5.
	          IF QUANTIDADE = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.	
       R7.
	       ACCEPT TPRECO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R6.
		   IF PRECO = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.		 
       R8.
           ACCEPT TCNPJ.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R7.

           IF CNPJ = ZEROS
               PERFORM ERROR-EMPTY
               GO TO R8.

           PERFORM READ-CADFORN.

       R9.
           ACCEPT TRAZAO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R8.

           IF RAZAO = SPACES
               PERFORM ERROR-EMPTY
               GO TO R9.				       

       R10.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R9.

           IF NOME = SPACES
               PERFORM ERROR-EMPTY
               GO TO R10.
	   
       R11.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R10.

           IF CEP = ZEROS
               PERFORM ERROR-EMPTY
               GO TO R11.

           PERFORM READ-CADCEP.
			  
       R12.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R11.

           IF ENDERECO = SPACES
               PERFORM ERROR-EMPTY
               GO TO R12. 
       
       R13.
           ACCEPT TNUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R12.

           IF NUMERO = ZEROS
               PERFORM ERROR-EMPTY
               GO TO R13.	
			   
       R14.
           ACCEPT TCOMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R13.      

       R15.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R14.

           IF BAIRRO = SPACES
               PERFORM ERROR-EMPTY
               GO TO R15.    
			   
       R16.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY.

           IF W-ACT = 01 GO TO R15.

           IF CIDADE = SPACES
               PERFORM ERROR-EMPTY
               GO TO R16.	
			   
       R17.
	       DISPLAY TELAUF. 
		   MOVE 1 TO AUX2
		   ACCEPT TUF.
		   ACCEPT W-ACT FROM ESCAPE KEY
		   IF W-ACT = 01
			   GO TO R16.
       R17A.
	       MOVE TBUF (AUX2) TO TXTUF
		   IF TXTUF1 NOT = UF
		      ADD 1 TO AUX2
			  IF AUX2 < 28
			     GO TO R17A
			  ELSE
			     MOVE "*** UF INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R17
           ELSE 
             MOVE TXTUF2 TO DISPESTADO. 
			 DISPLAY TELA.				   
			   

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
			
       SALVAR.
           MOVE "VOCE QUER GRAVAR OS DADOS? (S/N)" TO MENS
           DISPLAY (25, 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (25, 12) MENS.

           IF W-ACT = 01 GO TO R17.

           IF W-OPCAO = "S" or "s"
               MOVE CNPJ TO CNPJ-PROD
               MOVE CEP TO CEP-FORN
			   PERFORM WRITE-CADPROD
               PERFORM WRITE-CADFORN
               PERFORM WRITE-CADCEP
           ELSE IF W-OPCAO = "N" or "n" GO TO R1
           ELSE PERFORM ERROR-VALID GO TO SALVAR.
           GO TO R1.

      *
       DELETAR.
           MOVE "VOCE QUER EXCLUIR OS DADOS? (S/N)" TO MENS
           DISPLAY (25, 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (25, 12) MENS.

           IF W-ACT = 01 GO TO R17.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-CADPROD
               GO TO R1
           ELSE IF W-OPCAO = "N" or "n" NEXT SENTENCE
           ELSE PERFORM ERROR-VALID GO TO DELETAR.
      * 
       ACE-001.
                DISPLAY (25, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (25, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (25, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
			    ELSE
				   IF W-OPCAO = "E"
				      GO TO DELETAR.
      * 				
       CHECK-UF.
           MOVE 1 TO AUX
           PERFORM CHECK-UF-1.

       CHECK-UF-1.
           MOVE TBUF(AUX) TO TXTUF
           IF TXTUF1 = UF
               MOVE 27 TO AUX
           ELSE MOVE SPACES TO TXTUF.
           ADD 1 TO AUX.
           IF AUX < 28 GO TO CHECK-UF-1.
		   
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

       I-O-CADPROD.
           OPEN I-O CADPROD
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPROD
                 CLOSE CADPROD
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADPROD
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM				 
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO ROT-FIM.
		   
       I-O-CADCEP.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADCEP
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

       CHECK-WRITE.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE 
			  PERFORM ERROR-DATA-NOT-SAVED 
			  GO TO ROT-FIM.

       CHECK-READ.
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00" 
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

       WRITE-CADPROD.
           WRITE REGPROD.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
			  PERFORM REWRITE-CADPROD
           ELSE PERFORM ERROR-DATA-NOT-SAVED 
		      GO TO ROT-FIM.
		   
       WRITE-CADFORN.
           WRITE REGFORN.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22"
		      PERFORM REWRITE-CADFORN
           ELSE PERFORM ERROR-DATA-NOT-SAVED  
		      GO TO ROT-FIM.

       WRITE-CADCEP.
           WRITE REGCEP.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
			  PERFORM REWRITE-CADCEP
           ELSE PERFORM ERROR-DATA-NOT-SAVED 
		      GO TO ROT-FIM.

       REWRITE-CADPROD.
     	   REWRITE REGPROD. 
		   PERFORM CHECK-WRITE.
       REWRITE-CADFORN. 
	       REWRITE REGFORN. 
		   PERFORM CHECK-WRITE.
       REWRITE-CADCEP. 
	       REWRITE REGCEP. 
		   PERFORM CHECK-WRITE.

       READ-CADPROD.
     	   READ CADPROD. 
		   PERFORM CHECK-READ.
           IF ST-ERRO = "00" 
		      MOVE CNPJ-PROD TO CNPJ PERFORM READ-CADFORN.
           DISPLAY TELA.
		   
       READ-CADFORN. 
	       READ CADFORN. 
		   PERFORM CHECK-READ.
           IF ST-ERRO = "00" 
		      MOVE CEP-FORN TO CEP PERFORM READ-CADCEP.
           DISPLAY TELA.

       READ-CADCEP. 
	       READ CADCEP. 
		   PERFORM CHECK-READ.
           PERFORM CHECK-UF.
           DISPLAY TELA.

       DELETE-CADPROD. 
	       DELETE CADPROD RECORD. 
		   PERFORM CHECK-DELETE.
       DELETE-CADFORN. 
	       DELETE CADFORN RECORD. 
		   PERFORM CHECK-DELETE.
       DELETE-CADCEP.
     	   DELETE CADCEP RECORD.
		   PERFORM CHECK-DELETE.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPROD
		   CLOSE CADFORN
		   CLOSE CADCEP
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (25, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (25, 12) MENS.
       ROT-MENS-FIM.