      * VINCULO 1 SI QUIEREN UNA IMAGEN                                 MD291111
      *         2 NUMERO DE ATRIBUTO                                    MD291111
      *         3 VALOR A COMPARAR                                      MD291111
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. EMIFACNM.                                            MD160113
       AUTHOR.    MAXI.                                                 MD130711
       DATE-WRITTEN. 18/05/12.                                          MD180512

       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SOURCE-COMPUTER. RM-COBOL.                                       
       OBJECT-COMPUTER. RM-COBOL.                                       
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       
           COPY "ARTICULO.SD".                                          MD291111
           COPY "CODIGOS.SD".                                           
           COPY "IMPREACT.SD".                                          MD200911
           COPY "TDOCRUTI.SD".                                          MD200911
           
           SELECT IMPRESOR   ASSIGN TO PRINT  SYN-LO
                             FILE STATUS  IS ST-XX.
                                                                         
       DATA DIVISION.                                                   
       FILE SECTION.                                                    

       COPY "ARTICULO.FDS".                                             MD291111
       COPY "CODIGOS.FDS".                                              
       COPY "TDOCRUTI.FDS".                                             MD200911
       COPY "IMPREACT.FDS".                                             MD200911

       FD  IMPRESOR                                                    
               LABEL RECORDS ARE OMITTED.                               
       01  LINEA                        PIC X(160).                     LG191108

                                                                        
       WORKING-STORAGE SECTION.                                         
       
       COPY "COLOR.VAR".
       COPY "CBD-FIJO.VAR".                                             MD240608
                                                                        MD210711
       COPY "ARTICULO.BD".                                              MD291111
       COPY "CODIGOS.BD".                                               MD190911
       COPY "TDOCRUTI.BD".                                              MD200911
                                                                        
       COPY "FECHA.VAR".
       COPY "PARVENT.VAR".
       COPY "ST-ACE.VAR".
       COPY "EMIFAC.VAR".                                               MD130711
       COPY "EMIFACN.VAR".                                              MD130711
       COPY "SETEOIMP.WRK".
                                                                        MD221211
       COPY "PARAMPDF.REG".                                             MD130711
                                                                        MD291111
       COPY "CHARAVAL.VAR".                                             MD291111
                                                                        MD230512
       01  FEC-6               PIC 9(006).                              MD230512
       01  FEC-6-RED REDEFINES FEC-6.                                   MD230512
           02  DD-6            PIC 9(002).                              MD230512
           02  MM-6            PIC 9(002).                              MD230512
           02  AA-6            PIC 9(002).                              MD230512
                                                                        MD230512
       01  FECHA-EDM           PIC X(008).                              MD230512
                                                                        MD291111
       01  C-MN                PIC 9(002)V99 VALUE 14,2.                MD041013
       01  C-ME                PIC 9(002)V99 VALUE 14,6.                MD041013
       01  C-B                 PIC 9(002)V99 VALUE 18,7.                MD011013
                                                                        MD040212
       01  HORA-ING            PIC 9(006).                              MD040212
       01  HORA-RED REDEFINES HORA-ING.                                 MD040212
           02  H-HH            PIC 99.                                  MD040212
           02  H-MM            PIC 99.                                  MD040212
           02  H-SS            PIC 99.                                  MD040212
       01  HORA-ED.                                                     MD040212
           02  M-HORA          PIC 99.                                  MD040212
           02  FILLER          PIC X VALUE ":".                         MD040212
           02  M-MINU          PIC 99.                                  MD040212
           02  FILLER          PIC X VALUE ":".                         MD040212
           02  M-SEGU          PIC 99.                                  MD040212
                                                                        MD040212
       01  I-FA                PIC 99 COMP-6.                           
                                                                        MD291111
       01  DATO-FILLER.                                                 MD221211
           03 DATO-FECHA       PIC 9(008) COMP-6.                       MD221211
           03 FILLER           PIC X(008).                              MD221211
       01  FECHA-MAX   REDEFINES DATO-FILLER PIC X(012).                MD221211
                                                                        MD221211
                                                                        MD221211
       01  FILLER              PIC X.                                   MD291111
           88  IMPRIME-BITMAP  VALUE "S" WHEN FALSE "N".                MD291111
                                                                        MD291111
       01  ARCHIVO-BITMAP      PIC X(030).                              MD291111
       01  ALTO-BITMAP         PIC 99V99.                               MD291111
       01  ANCHO-BITMAP        PIC 99V99.                               MD291111
                                                                        
       01  I-M                 PIC 9(004) COMP-6.                       MD190911
                                                                        MD040811
       01  M-RUC-FA        PIC XXBXXXXXXBXXXX.                          MD260911
       01  M-CF            PIC X.                                       MD260911
                                                                        MD260911
                                                                        
      *    linea item                                                   MD130711
       01  LINEA-CANT-FA.                                               MD130711
           02 M-CANT-FA        PIC ZZZ.ZZ9 BLANK WHEN ZERO.             MD170113
           02 M-CANT-FA1   REDEFINES M-CANT-FA                          MD130711
                               PIC ZZZZ9,9 BLANK WHEN ZERO.             MD170113
           02 M-CANT-FA2   REDEFINES M-CANT-FA                          MD130711
                               PIC ZZZ9,99 BLANK WHEN ZERO.             MD170113
           02 M-CANT-FA3   REDEFINES M-CANT-FA                          MD130711
                               PIC ZZ9,999 BLANK WHEN ZERO.             MD170113
           02 M-CANT-FA4   REDEFINES M-CANT-FA                          MD130711
                               PIC Z9,9999 BLANK WHEN ZERO.             MD170113
           02 M-CANT-FA5   REDEFINES M-CANT-FA                          MD170113
                               PIC 9,99999 BLANK WHEN ZERO.             MD170113
                                                                        MD130711
       01  AREA-PRECIO.                                                 MD130711
           02  M-PREC-FA                PIC ZZ.ZZZ.ZZ9                  MD130711
                                        BLANK WHEN ZERO.                MD130711
           02  M-PREC-FA1     REDEFINES M-PREC-FA PIC ZZZZ.ZZ9,9        MD130711
                                        BLANK WHEN ZERO.                MD130711
           02  M-PREC-FA2     REDEFINES M-PREC-FA PIC ZZZ.ZZ9,99        MD130711
                                        BLANK WHEN ZERO.                MD130711
           02  M-PREC-FA3     REDEFINES M-PREC-FA PIC ZZ.ZZ9,999        MD130711
                                        BLANK WHEN ZERO.                MD130711
           02  M-PREC-FA4     REDEFINES M-PREC-FA PIC ZZZZ9,9999        MD130711
                                        BLANK WHEN ZERO.                MD130711
           02  M-PREC-FA5     REDEFINES M-PREC-FA PIC ZZZ9,99999        MD130711
                                        BLANK WHEN ZERO.                MD130711
                                                                        MD130711
       01  M-BRUTO-FA          PIC ZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.       MD210512
       01  M-PIVA              PIC Z9.                                  MD260911
                                                                        MD130711
       01  AUX-IMPORTE         PIC S9(011)V99     COMP-3.               
       
       01  PRIM-VEZ            PIC  9 VALUE 0.
                                                                        LG191108
       01  I                   PIC  99 COMP-1.                          LG191108
       01  YY                  PIC  99 COMP-1.                          MD240608
       01  J                   PIC  99 COMP-1.


       01  LINEA-IMPRESA       PIC X(160).                              LG191108
      
       01  LINE-TIPDOC        REDEFINES LINEA-IMPRESA.
           02  FILLER                   PIC X(095).                     LG191108
           02  M-NOM-DOC                PIC X(030).                     LG191108
       01  LINE-NRO-FA        REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(139).                     LG251108
           02  M-NRO-FA                 PIC 9(12).                      SF150914

       01  LINE-CLIE-FA       REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(106).                     LG191108
           02  M-CLIEN-FA               PIC X(006).                     
       01  LINE-MONE-FA      REDEFINES LINEA-IMPRESA.                   LG191108
           02  FILLER                   PIC X(123).                     
           02  M-MONE-FA                PIC X(003).                     LG191108
       01  LINE-FECHA-FA      REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(138).                     LG191108
           02  M-FECHA-FA               PIC 99/99/9999.                 LG191108

       01  LINE-NOMB-CLIE-FA  REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(017).                     LG191108
           02  M-NOMB-CLIE              PIC X(040).                     
       01  LINE-DIR           REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(017).                     LG191108
           02  M-DIREC-CLIE             PIC X(030).                     
       01  LINE-LOCALIDAD     REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(017).                     LG191108
           02  M-LOCA-CLIE              PIC X(020).                     

       01  LINE-TENLA-FA      REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(111).                     LG191108
           02  M-TENLA-FA               PIC X(030).                     LG191108
       01  LINE-NENLA-FA      REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(111).                     LG191108
           02  M-NENLA-FA               PIC 9(006).                     LG191108


      * linea de descripcion 
       01  LINE-ARTIC-FA      REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(003).                     LG191108
           02  M-ARTIC-FA               PIC X(012).                     LG191108
                                                                        LG191108
       01  LINE-UNID-FA       REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(018).                     LG191108
           02  M-UNID-FA                PIC X(004).                     LG191108
                                                                        LG191108
       01  LINE-DESC-FA       REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(24).                      LG191108
           02  M-DESC-FA                PIC X(040).                     LG191108
           02  FILLER                   PIC X.
           02  M-REFE-FA                PIC X(20).
                                                                        
       01  LINE-DTOLIN-FA     REDEFINES LINEA-IMPRESA.                  
           02  FILLER                   PIC X(130).                     LG251108
           02  M-DTOLIN-FA              PIC ZZ9,99   BLANK WHEN ZERO.   
           02  FILLER                   PIC X(1).                       
           02  M-PORCL                 PIC X(1).                        
                                                                        

       01  LINE-DESC-FP        REDEFINES LINEA-IMPRESA.                 LG191108
           02  FILLER                   PIC X(027).                     LG191108
           02  M-DESC-FP                PIC X(025).                     LG191108
                                                                        LG191108
       01  LINE-IVA            REDEFINES LINEA-IMPRESA.                 LG191108
           02  FILLER                   PIC X(114).                     LG191108
           02  M-IVA                    PIC Z9 BLANK WHEN ZERO.         MD200711
                                                                        LG191108
       01  LINE-OBS-1         REDEFINES LINEA-IMPRESA.                  LG191108
           02  FILLER                   PIC X(027).                     LG191108
           02  M-OBS-FA                 PIC X(040).                     LG191108
                                                                        LG191108
       COPY "ARTICULO.AUW".                                             MD291111
       COPY "CODIGOS.AUW".                                              
       COPY "TDOCRUTI.AUW".                                             MD200911
                                                                        LG191108
       01  TABLA-SYN.                                                   LG191108
           02  CANT-SYN        PIC 99 VALUE 03.                         MD291111
           02  SYN-ARTICULO    PIC X(030) VALUE "ARTICULO".             MD291111
           02  SYN-CODIGOS     PIC X(030) VALUE "CODIGOS ".             MD291111
           02  SYN-TDOCRUTI    PIC X(030) VALUE "TDOCRUTI".             MD200911
                                                                        MD240608
       01  SYN-ERROR           PIC  9(001) VALUE 0.                     MD240608
                                                                        MD240608
       LINKAGE SECTION.                                                 MD240608
       																 MD011013
       COPY "EMIFAC.LNK". 												 MD011013
       																 MD011013
       DECLARATIVES.                                                    MD011013
       ERROR-ARTICULO  SECTION.                                         MD011013
           USE AFTER STANDARD ERROR PROCEDURE ON ARTICULO.              
       ERROR-CODIGOS   SECTION.                                         
           USE AFTER STANDARD ERROR PROCEDURE ON CODIGOS.               
       ERROR-TDOCRUTI  SECTION.                                         
           USE AFTER STANDARD ERROR PROCEDURE ON TDOCRUTI.              
       ERROR-IMPRESOR  SECTION.                                         
           USE AFTER STANDARD ERROR PROCEDURE ON IMPRESOR.              
       END DECLARATIVES.                                                

       APERTURA SECTION.                                                
       ABRE-ARCHIVOS.                                                   
           MOVE "00"      TO ST-XX-CA.
           MOVE DIRE-IMPR TO SYN-LO
           MOVE CONF-IMPR TO CONFIG-AUX                                 
                                                                        
           IF PRIM-VEZ     = 0 AND                                      
              FUNCION  NOT = 2 AND                                      
              CIMP-PDC NOT = 0   
               PERFORM PRIMER-LLAMADO THRU EXIT-P-LLAMA
           END-IF
           
           IF FUNCION = 1 AND CIMP-PDC NOT = 0
               PERFORM TESTEO-IMP
               
               SET PRU TO FALSE
               PERFORM PONGO-VENT
                                                                        
               PERFORM CARGO-DATOS THRU EXIT-CD                         
                                                                        
               PERFORM IMPRE-LINEA-1 THRU EXIT-IMPRE                    
                           VARYING YY FROM 1 BY 1                       
                           UNTIL YY > CIMP-PDC                          MD130711
                                                                        MD221211
               PERFORM SACO-VENT
               CLOSE IMPRESOR 
                                                                        MD011013
               PERFORM CANCEL-CHARAVAL                                  MD011013
           END-IF                                                       LG191108
                                                                        MD130711
           IF FUNCION = 2                                               LG191108
               PERFORM CLOSE-ARTICULO                                   MD291111
               PERFORM CLOSE-CODIGOS                                    MD190911
               PERFORM CLOSE-TDOCRUTI                                   MD200911
           END-IF.                                                      LG191108
       EXIT-PGM.                                                        
           EXIT PROGRAM.                                                

       COPY "cbd-fijo.rut".                                             MD240608
       COPY "ERRORSTD.RUT".                                             MD160113
                                                                        
       COPY "CHARAVAL.RUT".                                             MD011013
       COPY "CHARAVAL.CAN".                                             MD011013
       COPY "FECHA.RUT".
       COPY "EMIFAC.RUT".                                               MD130711
       COPY "EMIFACN.RUT".                                              MD130711
       COPY "SETEOIMP.RUT".
       
       SETEO-IMPRE.
           INITIALIZE PrintDialog                                       MD130711
           CALL "P$ClearDialog"                                         MD130711
                                                                        MD130711
      *    Orientacion del papel                                        MD130711
           SET DM-OrientationField      TO TRUE                         MD130711
           SET DM-OrientationIsPortrait TO TRUE                         MD130711
                                                                        MD130711
      *    Tamaño del papel                                             MD130711
           SET DM-PaperSizeField        TO TRUE                         MD130711
           SET DM-PaperSizeIsA4         TO TRUE                         MD130711
                                                                        MD130711
           SET DM-PaperLengthField      TO TRUE                         MD130711
           SET DM-PaperWidthField       TO TRUE                         MD130711
                                                                        MD130711
           CALL "P$SetDialog" USING PrintDialog                         MD130711
           CALL "P$EnableDialog"                                        MD130711
                                                                        MD130711
           CALL "P$SetPen" USING PenStyleSolid,                         MD130711
                                 4,                                     MD130711
                                 ColorBlack                             MD130711
                                                                        MD130711
           CALL "P$GetDeviceCapabilities" USING DeviceCapabilities      MD130711
                                                                        MD130711
           COMPUTE X-MAX ROUNDED = DC-HorzSize / 10                     MD130711
           COMPUTE Y-MAX ROUNDED = DC-VertSize / 10.                    MD130711
                                                                        MD130711
           CALL "C$NARG" USING ARGUMENTOS                               MD130711
           MOVE SPACES       TO REG-PARAMPDF.                           MD271011
      *    IF ARGUMENTOS < 15                                           MD271011
      *        MOVE SPACES       TO REG-PARAMPDF                        MD271011
      *    ELSE                                                         MD271011
      *        MOVE REG-PARAMPDC TO REG-PARAMPDF                        MD271011
      *    END-IF.                                                      MD271011
                                                                        MD130711
      *    IF TIPO-IMPR = 1                                             MD130711
      *        SET 6-LPI            TO TRUE                             MD130711
      *        MOVE RM-LINE-SPACING TO LINEA                            MD130711
      *    ELSE                                                         MD130711
      *        MOVE H"1B4327" TO LINEA                                  MD130711
      *    END-IF                                                       MD130711
      *    WRITE LINEA AFTER 0 LINES                                    MD130711
      *    MOVE SPACES TO LINEA                                         MD130711
      *    PERFORM SETEO-20.                                            MD130711
      *    PERFORM SETEO-NEGRITA.                                       MD130711
                                                                        LG240909
      ******************************************************************
      *                                                                 
      *        RUTINAS DE ENTRADA/SALIDA Y AUDITORIA DE ERRORES         
      *                                                                 
                                                                        
       PRIMER-LLAMADO.
           MOVE 1 TO PRIM-VEZ.
           CALL "ASIGNA" USING TABLA-SYN SYN-ERROR.
           IF SYN-ERROR NOT = 0 
               MOVE "ASI"       TO MOTIVO-CA
               MOVE "EMIFAC"    TO NOMBRE-AR-CA
               MOVE ST-XX       TO ST-XX-CA
               GO TO EXIT-PGM
           END-IF
                                                                        MD200911
           PERFORM OPEN-INPUT-ARTICULO.                                 MD291111
           PERFORM OPEN-INPUT-CODIGOS.                                  MD190911
           PERFORM OPEN-INPUT-TDOCRUTI                                  MD200911
                                                                        MD240608
           PERFORM TESTEO-IMP THRU EXIT-TIMP.
       EXIT-P-LLAMA.
           EXIT.  

       IMPRI-PRUEBA.
       EXIT-PRU.
           EXIT.

       IMPRE-LINEA-1.                                                   
           PERFORM SETEO-IMPRE 
                                                                        
           IF FONDOBMP-PPDF NOT = SPACES                                
               MOVE FONDOBMP-PPDF TO IMAGEN                             
               MOVE 0,0           TO X-POS                              
               MOVE 0,0           TO Y-POS                              
               MOVE ANCHOBMP-PPDF TO X-DIM                              
               MOVE ALTOBMP-PPDF  TO Y-DIM                              
               PERFORM IMPRIME-IMAGEN                                   
           ELSE                                                         
               IF IMPRIME-BITMAP                                        
                   MOVE ARCHIVO-BITMAP TO IMAGEN                        
                                                                        
                   MOVE  0,0           TO X-POS                         
                   MOVE  0,0           TO Y-POS                         
                   MOVE ANCHO-BITMAP   TO X-DIM                         
                   MOVE ALTO-BITMAP    TO Y-DIM                         
                                                                        
                   PERFORM IMPRIME-IMAGEN                               
               END-IF                                                   
           END-IF                                                       
                                                                        
           IF HABILIT-IMPR = "N"                                        
               MOVE DESH-AUX TO X-INI                                   
               MOVE DESV-AUX TO Y-INI                                   
           ELSE                                                         
               MOVE 0        TO X-INI                                   
               MOVE 0        TO Y-INI                                   
           END-IF                                                       
                                                                        
           MOVE "Courier New" TO LF-FaceName.                           
           MOVE 11            TO LF-Height.                             
           MOVE 600           TO LF-WeightValue.                        
      * FECHA DE DOCUMENTO                                              
           COMPUTE Y-POS  = Y-INI + 3,6                                 MD011013
           COMPUTE X-POS  = X-INI + 16,3                                
           MOVE FECHA-FC TO FECHA-AMD                                   MD011013
           PERFORM AMD-TO-ED                                            
           MOVE FECHA-ED TO TEXTO                                       
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
      * NOMBRE DEL DOCUMENTO                                            
           COMPUTE Y-POS  = Y-INI + 5,1                                 
           COMPUTE X-POS  = X-INI + 11,8                                
           IF DESCI-PDC NOT = SPACES                                    
               MOVE DESCI-PDC TO TEXTO(1:22)                            
           ELSE                                                         
               MOVE DESC-PDC  TO TEXTO(1:22)                            
           END-IF                                                       
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
      * NUMERO DE DOCUMENTO                                             
           COMPUTE X-POS  = X-INI + 16,7                                
           MOVE NRO-FC   TO M-NRO-FA                                    MD011013
           MOVE M-NRO-FA TO TEXTO                                       
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
           MOVE 400           TO LF-WeightValue.                        
      * NOMBRE                                                          
           COMPUTE Y-POS  = Y-INI + 5,25                                MD011013
           COMPUTE X-POS  = X-INI + 3,4                                 
           IF RSOC-FC NOT = SPACE                                       MD011013
               MOVE RSOC-FC   TO TEXTO                                  MD011013
           ELSE                                                         
               MOVE NOMCLI-FC TO TEXTO                                  MD011013
           END-IF                                                       
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
      * DIRECCION                                                       
           COMPUTE Y-POS  = Y-INI + 5,85                                MD011013
           MOVE DIREC-FC TO TEXTO                                       MD011013
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
      * LOCALIDAD                                                       
           COMPUTE Y-POS  = Y-INI + 6,55                                MD011013
           MOVE LOCALID-FC TO TEXTO                                     MD011013
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
           IF RUC-FC NOT = SPACES                                       MD011013
               MOVE RUC-FC  TO M-RUC-FA                                 MD011013
               MOVE SPACES  TO M-CF                                     
           ELSE                                                         
               MOVE ALL "-" TO M-RUC-FA                                 
               MOVE "X"     TO M-CF                                     
           END-IF                                                       
                                                                        
      * RUC                                                             
           COMPUTE Y-POS  = Y-INI + 6,6                                 MD011013
           COMPUTE X-POS  = X-INI + 13,0                                
           MOVE M-RUC-FA    TO TEXTO                                    
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        
      * CF                                                              
           COMPUTE X-POS  = X-INI + 18,0                                
           MOVE M-CF   TO TEXTO                                         
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
           															 MD1
           COMPUTE Y-POS  = Y-INI + 9,1                                 
                                                                        
           MOVE 1 TO I-FA                                               
                                                                        
           PERFORM VARYING I-FA FROM 1 BY 1 UNTIL I-FA > CLIN-FC OR     MD011013
                                                  I-FA > 37             
               ADD 0,4 TO Y-POS                                         
                                                                        
               IF EXART-FC(I-FA) NOT = SPACES                           MD011013
                   MOVE EXART-FC(I-FA) TO CHARAVAL-VAR                  SF150914
                   PERFORM CALL-CHARAVAL                                
                   IF CHARAVAL-RET = 0                                  SF150914
                       MOVE CHARAVAL-NUM   TO FEC-6                     SF150914
                       MOVE DD-6           TO FECHA-EDM                 MD011013
                       MOVE "/"            TO FECHA-EDM(3:)             MD011013
                       MOVE MM-6           TO FECHA-EDM(4:)             MD011013
                       MOVE "/"            TO FECHA-EDM(6:)             MD011013
                       MOVE AA-6           TO FECHA-EDM(7:)             MD011013
                       MOVE FECHA-EDM      TO TEXTO                     MD011013
                                                                        
                       COMPUTE X-POS  = X-INI + 3,4                     
                       PERFORM IMPRIME-TEXTO-DERECHA                    
                   END-IF                                               
               END-IF                                                   
                                                                        
               COMPUTE X-POS  = X-INI + 2,8                             MD041013
               EVALUATE DECIC-FC(I-FA)                                  MD011013
                   WHEN 0 MOVE ICANT-FC(I-FA) TO M-CANT-FA              MD011013
                   WHEN 1 MOVE ICANT-FC(I-FA) TO M-CANT-FA1             MD011013
                   WHEN 2 MOVE ICANT-FC(I-FA) TO M-CANT-FA2             MD011013
                   WHEN 3 MOVE ICANT-FC(I-FA) TO M-CANT-FA3             MD011013
                   WHEN 4 MOVE ICANT-FC(I-FA) TO M-CANT-FA4             MD011013
                   WHEN 5 MOVE ICANT-FC(I-FA) TO M-CANT-FA5             MD011013
               END-EVALUATE                                             
               MOVE LINEA-CANT-FA TO TEXTO                              
               PERFORM IMPRIME-TEXTO-DERECHA                            
                                                                        
               COMPUTE X-POS  = X-INI + 3,5                             
               MOVE DESC-FC(I-FA) TO TEXTO                              MD011013
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          
                                                                        
               IF EMVAL-PDC(YY) NOT = "N"                               
                   COMPUTE X-POS  = X-INI + C-B                         
                   MOVE BRUTO-CIV-CPY(I-FA) TO M-BRUTO-FA               SF150914
                   MOVE M-BRUTO-FA      TO TEXTO                        
                   PERFORM IMPRIME-TEXTO-DERECHA                        
               END-IF                                                   
           END-PERFORM                                                  
                                                                        
           IF EMVAL-PDC(YY) NOT = "N"                                   
               MOVE 600           TO LF-WeightValue                     
                                                                        
               COMPUTE Y-POS  = Y-INI + 25,2                            MD011013
                                                                        
               IF MONE-FC = MONE-PSYS                                   MD011013
                   COMPUTE X-POS  = X-INI + C-ME                        
                   MOVE "XXXX" TO TEXTO                                 
               ELSE                                                     
                   COMPUTE X-POS  = X-INI + C-MN                        
                   MOVE "XX"  TO TEXTO                                  
               END-IF                                                   
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          
                                                                        MD180512
               COMPUTE X-POS  = X-INI + C-B                             MD180512
               MOVE SUMA-BRUTO-CIV-CPY TO M-BRUTO-FA                    SF150914
               MOVE M-BRUTO-FA     TO TEXTO                             MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
                                                                        MD180512
               IF TOTIVA-CPY NOT = 0                                    SF150914
                   COMPUTE Y-POS  = Y-INI + 26,2                        MD011013
                                                                        MD180512
                   COMPUTE X-POS  = X-INI + C-B                         MD180512
                   MOVE TOTIVA-CPY TO M-BRUTO-FA                        SF150914
                   MOVE M-BRUTO-FA TO TEXTO                             MD180512
                   PERFORM IMPRIME-TEXTO-DERECHA                        MD180512
                                                                        MD180512
                   IF MONE-FC = MONE-PSYS                               MD011013
                       COMPUTE X-POS  = X-INI + C-ME                    MD240512
                       MOVE "XXXX" TO TEXTO                             MD180512
                   ELSE                                                 MD180512
                       COMPUTE X-POS  = X-INI + C-MN                    MD240512
                       MOVE "XX"  TO TEXTO                              MD180512
                   END-IF                                               MD180512
                   PERFORM IMPRIME-TEXTO-IZQUIERDA                      MD180512
                                                                        MD180512
                   COMPUTE X-POS  = X-INI + 13,4                        MD240512
                   IF TOTIVAN-CPY(1) NOT = 0                            SF150914
                       MOVE PIVA-FC(1) TO M-PIVA                        MD011013
                   ELSE                                                 MD180512
                       MOVE PIVA-FC(2) TO M-PIVA                        MD011013
                   END-IF                                               MD180512
                   MOVE M-PIVA     TO TEXTO                             MD180512
                   PERFORM IMPRIME-TEXTO-IZQUIERDA                      MD180512
               END-IF                                                   MD180512
                                                                        MD180512
               COMPUTE Y-POS  = Y-INI + 27,1                            MD011013
                                                                        MD180512
               IF MONE-FC = MONE-PSYS                                   MD011013
                   COMPUTE X-POS  = X-INI + C-ME                        MD240512
                   MOVE "XXXX" TO TEXTO                                 MD180512
               ELSE                                                     MD180512
                   COMPUTE X-POS  = X-INI + C-MN                        MD240512
                   MOVE "XX"  TO TEXTO                                  MD180512
               END-IF                                                   MD180512
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + C-B                             MD180512
               MOVE TOTAL-A-PAGAR-CPY TO M-BRUTO-FA                     SF150914
               MOVE M-BRUTO-FA    TO TEXTO                              MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
           END-IF                                                       MD180512
                                                                        MD210512
           MOVE 400           TO LF-WeightValue.                        MD210512
                                                                        
      * OBS 1                                                           
           COMPUTE Y-POS  = Y-INI + 24,9                                MD180512
           COMPUTE X-POS  = X-INI + 1,5                                 MD180512
           MOVE MSG-PDC(1)  TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        
      * OBS 2                                                           
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           MOVE MSG-PDC(2)  TO TEXTO(1:40)                              MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        
      * OBS 1                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           COMPUTE X-POS  = X-INI + 1,5                                 MD180512
           MOVE OBS-FC(1)  TO TEXTO                                     MD011013
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * OBS 2                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           MOVE OBS-FC(2)  TO TEXTO(1:40)                               MD011013
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD240512
                                                                        MD240512
           IF TVIA-PDC(YY) NOT = SPACES                                 MD240512
               COMPUTE Y-POS  = Y-INI + 27,3                            MD240512
               COMPUTE X-POS  = X-INI + C-B                             MD240512
               MOVE TVIA-PDC(YY) TO TEXTO                               MD240512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD240512
           END-IF                                                       MD240512
                                                                        MD180512
           IF TIPO-IMPR NOT = 1 OR YY NOT = CIMP-PDC                    
               WRITE LINEA FROM SPACES BEFORE PAGE                      
           END-IF.                                                      
       EXIT-IMPRE.                                                      
           EXIT.                                                        
                                                                        
       COPY "ARTICULO_SL.ES".                                           MD160113
       COPY "CODIGOS.ES".                                               
       COPY "TDOCRUTI.ES".                                              
       COPY "ARTICULO.AUP".                                             
       COPY "CODIGOS.AUP".                                              
       COPY "TDOCRUTI.AUP".                                             
                                                                        
       CARGO-DATOS.                                                     
           SET IMPRIME-BITMAP TO FALSE                                  
                                                                        
           MOVE TIDOC-FC   TO TIDOC-TDR.                                MD011013
           MOVE "EMIFACNM" TO RUTINA-TDR.                               MD160113
           PERFORM LEC01-TDOCRUTI.                                      
           IF ST-XX = "23"                                              
               MOVE SPACES TO TIDOC-TDR                                 MD160113
               PERFORM LEC01-TDOCRUTI                                   MD160113
               IF ST-XX = "23"                                          MD160113
                   GO TO EXIT-CD                                        MD160113
               END-IF                                                   MD160113
           END-IF                                                       MD160113
                                                                        
           IF LINERUT-TDR(1) NOT = SPACES                               
               MOVE LINERUT-TDR (1) TO CODI-COD                         
               PERFORM LEC01-CODIGOS                                    
               IF ST-XX = "00"                                          
                   SET IMPRIME-BITMAP TO TRUE                           
                   MOVE CON1-COD      TO ARCHIVO-BITMAP                 
                   MOVE DTO-COD (1)   TO ANCHO-BITMAP                   
                   MOVE DTO-COD (2)   TO ALTO-BITMAP                    
               END-IF                                                   
           END-IF.                                                      
       EXIT-CD.                                                         
           EXIT.                                                        
                                                                        
