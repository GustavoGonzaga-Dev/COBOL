       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP001.
      ***********************************************
      *        MANUTENCAO DO CADASTRO DE CEP        *
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
                ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
            LABEL RECORD IS STANDARD
            VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CEP PIC 9(08).
		   03 CODCEP PIC 99999.999.
           03 LOGRADOURO PIC X(35).
		   03 BAIRRO PIC X(20).
		   03 CIDADE PIC X(20).
           03 UF PIC X(02).
           03 PONTOREFERENCIA PIC X(35).
           03 SITUACAO PIC X(01).
		   03 OBSERVACAO PIC X(40).
		   03 DATA-IM.
		      05 DIA PIC 99.
			  05 MES PIC 9(02).
			  05 ANO.
			     07 MIL PIC 9.
				 07 SEC PIC 9.
				 07 DEC PIC 99.
		   03 DATA2 PIC 9(08).
           03 MASCARA PIC 99/99/9999.     
           03 TPUF PIC 9(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT PIC 9(06) VALUE ZEROS.
       77 W-OPCAO PIC X(01) VALUE SPACES.
       77 W-ACT             PIC 9(02) VALUE ZEROS.
       77 MENS              PIC X(50) VALUE SPACES.
       77 LIMPA              PIC X(50) VALUE SPACES. 
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 AUX           PIC 9(02) VALUE ZEROS.
	   01 AUX2          PIC 9(02) VALUE ZEROS.
       
       01 TABUF1.
          03 FILLER     PIC X(20) VALUE "RORONDONIA".
		  03 FILLER     PIC X(20) VALUE "ACACRE".
		  03 FILLER     PIC X(20) VALUE "AMAMAZONAS".
		  03 FILLER     PIC X(20) VALUE "RRRORAIMA".
		  03 FILLER     PIC X(20) VALUE "PAPARA".
		  03 FILLER     PIC X(20) VALUE "AMAMAPA".
		  03 FILLER     PIC X(20) VALUE "TOTOCANTINS".
		  03 FILLER     PIC X(20) VALUE "MAMARANHAO".
		  03 FILLER     PIC X(20) VALUE "PIPIAUI".
		  03 FILLER     PIC X(20) VALUE "CECEARA".
		  03 FILLER     PIC X(20) VALUE "RNRIOGRANDEDONORTE".
		  03 FILLER     PIC X(20) VALUE "PBPARAIBA".
		  03 FILLER     PIC X(20) VALUE "PEPERNAMBUCO".
		  03 FILLER     PIC X(20) VALUE "ALALAGOAS".
		  03 FILLER     PIC X(20) VALUE "SESERGIPE".
		  03 FILLER     PIC X(20) VALUE "BABAHIA".
		  03 FILLER     PIC X(20) VALUE "MGMINASGERAIS".
		  03 FILLER     PIC X(20) VALUE "ESESPIRITOSANTO".
		  03 FILLER     PIC X(20) VALUE "RJRIODEJANEIRO".
		  03 FILLER     PIC X(20) VALUE "SPSAOPAULO".
		  03 FILLER     PIC X(20) VALUE "PRPARANA".
		  03 FILLER     PIC X(20) VALUE "SCSANTACATARINA".
		  03 FILLER     PIC X(20) VALUE "RSRIOGRANDEDOSUL".
		  03 FILLER     PIC X(20) VALUE "MSMATOGROSSODOSUL".
		  03 FILLER     PIC X(20) VALUE "MTMATOGROSSO".
		  03 FILLER     PIC X(20) VALUE "GOGOIAS".
		  03 FILLER     PIC X(20) VALUE "DFDISTRITOFEDERAL".
      *
	   01 TABUF REDEFINES TABUF1.
	      03 TBUF PIC X(20) OCCURS 5 TIMES.
      *
	   01 TXTUF.
	      03 TXTUF1 PIC X(02) VALUE SPACES.
		  03 TXTUF2 PIC X(14) VALUE SPACES.
      *
       01 TABSITUACAO1.
          03 FILLER     PIC X(15) VALUE "AATIVO".
          03 FILLER     PIC X(15) VALUE "IINATIVO".
          03 FILLER     PIC X(15) VALUE "CCANCELADO".
          03 FILLER     PIC X(15) VALUE "EESPECIFICO".
      *
       01 TABSITUACAO REDEFINES TABSITUACAO1.
          03 TBSITUACAO   PIC X(15) OCCURS 5 TIMES.
      *
       01 TXTSITUACAO.
          03 TXTSITUA1 PIC X(01) VALUE SPACES.
          03 TXTSITUA2 PIC X(14) VALUE SPACES. 
          
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  " MANUTENCAO DO CADASTRO DE CEP".
           05  LINE 05  COLUMN 01 
               VALUE  "CODIGO CEP:".
           05  LINE 07  COLUMN 01 
               VALUE  "LOGRADOURO:".
           05  LINE 09  COLUMN 01 
               VALUE  "BAIRRO:".
           05  LINE 11  COLUMN 01 
               VALUE  "CIDADE:".
           05  LINE 13  COLUMN 01 
               VALUE  "UF:".
		   05  LINE 15  COLUMN 01
		       VALUE  "PONTO REFERENCIA:".
		   05  LINE 17  COLUMN 01
		       VALUE "STATUS:".
		   05  LINE 19  COLUMN 01
		       VALUE "OBSERVACAO:".
		   05  LINE 21  COLUMN 01 
		       VALUE "DATA INCL/MANUT:".
           05  LINE 23 COLUMN  01
               VALUE  "MENSAGEM:".
           05  TCEP
               LINE 05 COLUMN 20 PIC 9(08)
               USING CEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 07 COLUMN 20 PIC X(35)
               USING LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09 COLUMN 20 PIC X(20)
               USING BAIRRO
               HIGHLIGHT.
		   05  TCIDADE
		       LINE 11 COLUMN 20 PIC X(20)
			   USING CIDADE
			   HIGHLIGHT.
		   05  TUF 
               LINE 13 COLUMN 20 PIC X(02)
               USING UF
               HIGHLIGHT.
           05  TTXTPUF
               LINE 13 COLUMN 22 PIC X(10)
               USING TXTUF2
			   HIGHLIGHT.
		   05  TPONTOREFERENCIA
		       LINE 15 COLUMN 20 PIC X(35)
			   USING PONTOREFERENCIA
			   HIGHLIGHT.
           05  TSITUACAO
               LINE 17 COLUMN 20 PIC X(01)
               USING SITUACAO
               HIGHLIGHT.
           05  TTXTSITUACAO
               LINE 17 COLUMN 20 PIC X(14)
               USING TXTSITUA2
               HIGHLIGHT.
           05  TOBSERVACAO
		       LINE 19 COLUMN 20 PIC X(40)
			   USING OBSERVACAO
			   HIGHLIGHT.
      *
       01  TELAINFO.
           05  LINE 03  COLUMN 32
		       VALUE  "RO - RONDONIA".
           05  LINE 04  COLUMN 32
		       VALUE  "AC - ACRE".
           05  LINE 05  COLUMN 32
		       VALUE  "AM - AMAZONAS".
           05  LINE 06  COLUMN 32
		       VALUE  "RR - RORAIMA".          
           05  LINE 07  COLUMN 32
		       VALUE  "PA - PARA".
           05  LINE 08  COLUMN 32
		       VALUE  "AP - AMAPA".      
           05  LINE 09  COLUMN 32
		       VALUE  "TO - TOCANTINS".
           05  LINE 10  COLUMN 32
		       VALUE  "MA - MARANHAO".             
           05  LINE 11  COLUMN 32
		       VALUE  "PI - PIAUI". 
           05  LINE 12  COLUMN 32
               VALUE  "CE - CEARA".		   
           05  LINE 13  COLUMN 32
		       VALUE  "RN - RIO GRANDE DO NORTE".
           05  LINE 14  COLUMN 32
		       VALUE  "PB - PARAIBA".
           05  LINE 15  COLUMN 32
		       VALUE  "PE - PERNAMBUCO".          
           05  LINE 16  COLUMN 32
		       VALUE  "AL - ALAGOAS".
           05  LINE 03  COLUMN 57
		       VALUE  "SE - SERGIPE".      
           05  LINE 04  COLUMN 57
		       VALUE  "BA - BAHIA".
           05  LINE 05  COLUMN 57
		       VALUE  "MG - MINAS GERAIS".             
           05  LINE 06  COLUMN 57
		       VALUE  "ES - ESPIRITO SANTO". 
           05  LINE 07  COLUMN 57
               VALUE  "RJ - RIO DE JANEIRO".	
           05  LINE 08  COLUMN 57
		       VALUE  "SP - SAO PAULO".
           05  LINE 09  COLUMN 57
		       VALUE  "PR - PARANA".          
           05  LINE 10  COLUMN 57
		       VALUE  "SC - SANTA CATARINA".
           05  LINE 11  COLUMN 57
		       VALUE  "RS - RIO GRANDE DO SUL".      
           05  LINE 12  COLUMN 57
		       VALUE  "MS - MATO GROSSO DO SUL".
           05  LINE 13  COLUMN 57
		       VALUE  "MT - MATO GROSSO".             
           05  LINE 14  COLUMN 57
		       VALUE  "GO - GOIAS". 
           05  LINE 15  COLUMN 57
               VALUE  "DF - DISTRITO FEDERAL".          			   
			   
       01  TELASIT.
           05  LINE 18  COLUMN 41
		       VALUE  "A - ATIVO".
           05  LINE 19  COLUMN 41
		       VALUE  "I - INATIVO".
           05  LINE 20  COLUMN 41
		       VALUE  "C - CANCELADO".
           05  LINE 21  COLUMN 41
		       VALUE  "E - ESPECIFICO".          	
		
      *-----------------------------------------------------------------    
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
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" 
					TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------  
       R1.
           MOVE SPACES TO LOGRADOURO BAIRRO CIDADE SITUACAO
		   MOVE SPACES TO PONTOREFERENCIA OBSERVACAO
           MOVE SPACES TO TXTUF TXTSITUACAO
           MOVE ZEROS  TO CEP DATA-IM W-SEL.
       
      *---------[VISUALIZACAO DA TELA]--------------------
           DISPLAY TELA2.
           
       R2.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADCEP
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF CEP = SPACES
                     MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
                           
       LER-CADMCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R6A
                PERFORM R8A
                DISPLAY TELA2
                MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CEP NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TLOGRADOURO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
 
       R4.
           ACCEPT TBAIRRO.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
	       ACCEPT TCIDADE.
		   ACCEPT W-ACT FROM ESCAPE KEY
		   IF W-ACT = 01
		           GO TO R4.
       R6.
	      DISPLAY TELAINFO
		  MOVE 1 TO AUX2
		  ACCEPT TUF.
		  ACCEPT W-ACT FROM ESCAPE KEY
		  IF W-ACT = 1
				   GO TO R5.
       R6A.
	       MOVE TBUF (AUX2) TO TXTUF
		   IF TXTUF1 NOT = UF
		      ADD 1 TO AUX2
			  IF AUX2 < 10 
			     GO TO R6A
			  ELSE
			     MOVE "*** UF INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE 
             DISPLAY TTXTPUF.
			 DISPLAY TELA2.
       R7.
	       ACCEPT TPONTOREFERENCIA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R6.		   
       R8.
	       DISPLAY TELASIT
           MOVE 1 TO AUX
           ACCEPT TSITUACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
       R8A.
           MOVE TBSITUACAO(AUX) TO TXTSITUACAO
           IF TXTSITUA1 NOT = SITUACAO
              ADD 1 TO AUX
              IF AUX < 10
                 GO TO R8A
              ELSE
                 MOVE "*** TIPO STATUS INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8
           ELSE
               DISPLAY TTXTSITUACAO.
			   DISPLAY TELA2.
       R9.
	       ACCEPT TOBSERVACAO
		   ACCEPT W-ACT FROM ESCAPE KEY
		   IF W-ACT = 1
		         GO TO R8.
       R10.
	       ACCEPT (21, 20) DATA-IM
		   MOVE DATA-IM TO DATA2
		   MOVE DATA2 TO MASCARA
		   DISPLAY (21, 20) MASCARA.
		   ACCEPT W-ACT FROM ESCAPE KEY
		   IF W-ACT = 1
		         GO TO R9.

           
      * ------------- VERICAR SE E ALTERACAO -----------------         
                IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R10.
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
                  MOVE "* CEP JA EXISTE,DADOS NAO GRAVADOS *"
				  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
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
                   MOVE "*** REGISTRO CEP EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R10.
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
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCEP.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
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