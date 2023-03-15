      * VINCULO 1 SI QUIEREN UNA IMAGEN                                 MD291111
      *         2 NUMERO DE ATRIBUTO                                    MD291111
      *         3 VALOR A COMPARAR                                      MD291111
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. EMIFACNN.                                            MD210512
       AUTHOR.    MAXI.                                                 MD130711
       DATE-WRITTEN. 18/05/12.                                          MD180512

       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SOURCE-COMPUTER. RM-COBOL.                                       
       OBJECT-COMPUTER. RM-COBOL.                                       
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       ahora modifico emifacnn
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
      * Variables auxiliares para charaval                              
      *01  CHARAVAL-VAR        PIC X(30).                               SF150914
      *01  CHARAVAL-TXT        PIC X(30).                               SF150914
      *01  CHARAVAL-NUM        PIC S9(020)V9(010).                      SF150914
      *01  CHARAVAL-RET        PIC S9(005) COMP-1.                      SF150914
                                                                        MD180512
       01  C-MN                PIC 9(002)V99 VALUE 14,1.                MD240512
       01  C-ME                PIC 9(002)V99 VALUE 14,4.                MD240512
       01  C-B                 PIC 9(002)V99 VALUE 18,6.                MD180512
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
       01  NUMLET-PAR.                                                  MD040811
           02 IMPORTE-PAR      PIC S9(11)V99.                           MD040811
           02 RCG-PAR          PIC 9 VALUE 0.                           MD040811
           02 CON100-PAR       PIC 9 VALUE 1.                           MD040811
           02 LETRAS-PAR.                                               MD040811
               03  LINMAT OCCURS 3 PIC X(99).                           MD040811
           02 CAR-L1-PAR       PIC 9(3) VALUE 71.                       MD040811
           02 CAR-L2-PAR       PIC 9(3) VALUE 87.                       MD040811
           02 CAR-L3-PAR       PIC 9(3) VALUE 99.                       MD040811
                                                                        MD260911
       01  M-RUC-FA        PIC XXBXXXXXXBXXXX.                          MD260911
       01  M-CF            PIC X.                                       MD260911
                                                                        MD260911
                                                                        
      *    linea item                                                   MD130711
       01  LINEA-CANT-FA.                                               MD130711
           02 M-CANT-FA        PIC ZZ.ZZ9 BLANK WHEN ZERO.              MD260911
           02 M-CANT-FA1   REDEFINES M-CANT-FA                          MD130711
                               PIC ZZZ9,9 BLANK WHEN ZERO.              MD260911
           02 M-CANT-FA2   REDEFINES M-CANT-FA                          MD130711
                               PIC ZZ9,99 BLANK WHEN ZERO.              MD260911
           02 M-CANT-FA3   REDEFINES M-CANT-FA                          MD130711
                               PIC Z9,999 BLANK WHEN ZERO.              MD260911
           02 M-CANT-FA4   REDEFINES M-CANT-FA                          MD130711
                               PIC 9,9999 BLANK WHEN ZERO.              MD260911
      *    02 M-CANT-FA5   REDEFINES M-CANT-FA                          MD180512
      *                        PIC Z9,99999 BLANK WHEN ZERO.            MD180512
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
                                                                        MD240608
       01  FUNCION             PIC  9.                                  MD240608

       COPY "PARAMFAC.REG".
       COPY "MONEDAS.REG".
       COPY "PARAMDOC.CPY".
       COPY "FACTURAS.VAR".
       COPY "CLIENTES.REG".
       COPY "FORMAPAG.REG".
       COPY "PARSYS.REG".
       COPY "VIRTUAL.REG".
       COPY "SELE-CA.VAR".
       COPY "VENDEDOR.REG".                                             MD240608
       COPY "LOCAL.REG".                                                MD240608
       COPY "IMPRES.REG".    
       COPY "NUMERADO.REG".
       COPY "PARAMPDF.CPY".                                             MD130711
       
       PROCEDURE DIVISION USING FUNCION
                                REG-PARAMFAC
                                REG-MONEDAS
                                REG-PDOCCPY
                                REG-FACTURAS
                                REG-CLIENTES
                                REG-FORMAPAG
                                REG-VENDEDOR
                                REG-LOCALES
                                REG-PARSYS
                                REG-VIRTUAL
                                ERROR-CA
                                REG-IMPRES
                                REG-NUMERADO                            MD130711
                                REG-PARAMPDC.                           MD130711
                                                                        MD130711
       DECLARATIVES.                                                    
       ERROR-ARTICULO  SECTION.                                         MD291111
           USE AFTER STANDARD ERROR PROCEDURE ON ARTICULO.              MD291111
       ERROR-CODIGOS   SECTION.                                         MD200911
           USE AFTER STANDARD ERROR PROCEDURE ON CODIGOS.               MD200911
       ERROR-TDOCRUTI  SECTION.                                         MD200911
           USE AFTER STANDARD ERROR PROCEDURE ON TDOCRUTI.              MD200911
       ERROR-IMPRESOR  SECTION.                                         MD200911
           USE AFTER STANDARD ERROR PROCEDURE ON IMPRESOR.              MD200911
       END DECLARATIVES.                                                

       APERTURA SECTION.                                                
       ABRE-ARCHIVOS.                                                   
           MOVE "00"      TO ST-XX-CA.
           MOVE DIRE-IMPR TO SYN-LO
           MOVE CONF-IMPR TO CONFIG-AUX                                 MD180512
                                                                        MD180512
           IF PRIM-VEZ     = 0 AND                                      
              FUNCION  NOT = 2 AND                                      MD130711
              CIMP-PDC NOT = 0   
               PERFORM PRIMER-LLAMADO THRU EXIT-P-LLAMA
           END-IF
           
           IF FUNCION = 1 AND CIMP-PDC NOT = 0
               PERFORM TESTEO-IMP
               
               SET PRU TO FALSE
               PERFORM PONGO-VENT
                                                                        MD221211
               PERFORM CARGO-DATOS THRU EXIT-CD                         
                                                                        LG211108
               PERFORM IMPRE-LINEA-1 THRU EXIT-IMPRE                    MD130711
                           VARYING YY FROM 1 BY 1                       MD130711
                           UNTIL YY > CIMP-PDC                          MD130711
                                                                        MD221211
               PERFORM SACO-VENT
               CLOSE IMPRESOR 
                                                                        SF150914
               PERFORM CANCEL-CHARAVAL                                  SF150914
           END-IF                                                       LG191108
                                                                        MD130711
           IF FUNCION = 2                                               LG191108
               PERFORM CLOSE-ARTICULO                                   MD291111
               PERFORM CLOSE-CODIGOS                                    MD190911
               PERFORM CLOSE-TDOCRUTI                                   MD200911
           END-IF.                                                      LG191108
       EXIT-PGM.                                                        
           EXIT PROGRAM.                                                

       ERROR-STANDARD.                                                  MD240608
           MOVE ERROR-ARCH TO ERROR-CA                                  MD240608
           CALL "AUDIT" USING ERROR-CA.                                 MD240608
           GO TO EXIT-PGM.                                              MD240608
                                                                        MD240608
       COPY "cbd-fijo.rut".                                             MD240608
                                                                        MD240608
                                                                        
       COPY "CHARAVAL.RUT".                                             SF150914
       COPY "CHARAVAL.CAN".                                             SF150914
       COPY "FECHA.RUT".
       COPY "EMIFAC.RUT".                                               MD130711
       COPY "EMIFACN.RUT".                                              MD130711
       COPY "SETEOIMP.RUT".
       COPY "NUMLET.CAN".                                               MD040811
       
       SETEO-IMPRE.
           INITIALIZE PrintDialog                                       MD130711
           CALL "P$ClearDialog"                                         MD130711
                                                                        MD130711
      *    Orientacion del papel                                        MD130711
           SET DM-OrientationField      TO TRUE                         MD130711
           SET DM-OrientationIsPortrait TO TRUE                         MD130711
                                                                        MD130711
      *    Tama�o del papel                                             MD130711
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
           PERFORM SETEO-IMPRE                                          MD180512
                                                                        MD180512
           IF FONDOBMP-PPDF NOT = SPACES                                MD180512
               MOVE FONDOBMP-PPDF TO IMAGEN                             MD180512
               MOVE 0,0           TO X-POS                              MD180512
               MOVE 0,0           TO Y-POS                              MD180512
               MOVE ANCHOBMP-PPDF TO X-DIM                              MD180512
               MOVE ALTOBMP-PPDF  TO Y-DIM                              MD180512
               PERFORM IMPRIME-IMAGEN                                   MD180512
           ELSE                                                         MD180512
               IF IMPRIME-BITMAP                                        MD180512
                   MOVE ARCHIVO-BITMAP TO IMAGEN                        MD180512
                                                                        MD180512
                   MOVE  0,0           TO X-POS                         MD180512
                   MOVE  0,0           TO Y-POS                         MD180512
                   MOVE ANCHO-BITMAP   TO X-DIM                         MD180512
                   MOVE ALTO-BITMAP    TO Y-DIM                         MD180512
                                                                        MD180512
                   PERFORM IMPRIME-IMAGEN                               MD180512
               END-IF                                                   MD180512
           END-IF                                                       MD180512
                                                                        MD180512
           IF HABILIT-IMPR = "N"                                        MD180512
               MOVE DESH-AUX TO X-INI                                   MD180512
               MOVE DESV-AUX TO Y-INI                                   MD180512
           ELSE                                                         MD180512
               MOVE 0        TO X-INI                                   MD180512
               MOVE 0        TO Y-INI                                   MD180512
           END-IF                                                       MD180512
                                                                        MD180512
           MOVE "Courier New" TO LF-FaceName.                           MD180512
           MOVE 09            TO LF-Height.                             MD180512
           MOVE 400           TO LF-WeightValue.                        MD180512
                                                                        MD180512
      * FECHA DE DOCUMENTO                                              MD180512
           COMPUTE Y-POS  = Y-INI + 3,3                                 MD180512
           COMPUTE X-POS  = X-INI + 16,5                                MD180512
           MOVE NUEVES   TO FECHA-AMD                                   MD180512
           PERFORM AMD-TO-ED                                            MD180512
           MOVE FECHA-ED TO TEXTO                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * NOMBRE DEL DOCUMENTO                                            MD180512
           COMPUTE Y-POS  = Y-INI + 4,3                                 MD180512
           COMPUTE X-POS  = X-INI + 11,6                                MD180512
           MOVE XX(1:22)  TO TEXTO                                      MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * NUMERO DE DOCUMENTO                                             MD180512
           COMPUTE X-POS  = X-INI + 16,7                                MD180512
           MOVE NUEVES   TO M-NRO-FA                                    MD180512
           MOVE M-NRO-FA TO TEXTO                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * NOMBRE                                                          MD180512
           COMPUTE Y-POS  = Y-INI + 5,7                                 MD180512
           COMPUTE X-POS  = X-INI + 3,4                                 MD180512
           MOVE XX(1:40)  TO TEXTO                                      MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * DIRECCION                                                       MD180512
           COMPUTE Y-POS  = Y-INI + 6,3                                 MD180512
           MOVE XX(1:30)  TO TEXTO                                      MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * LOCALIDAD                                                       MD180512
           COMPUTE Y-POS  = Y-INI + 6,9                                 MD180512
           MOVE XX(1:30)  TO TEXTO                                      MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           MOVE ALL "-" TO M-RUC-FA                                     MD180512
           MOVE "X"     TO M-CF                                         MD180512
                                                                        MD180512
      * RUC                                                             MD180512
           COMPUTE Y-POS  = Y-INI + 6,0                                 MD180512
           COMPUTE X-POS  = X-INI + 13,0                                MD180512
           MOVE M-RUC-FA    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * CF                                                              MD180512
           COMPUTE X-POS  = X-INI + 18,0                                MD180512
           MOVE M-CF   TO TEXTO                                         MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
           															 MD180512
           COMPUTE Y-POS  = Y-INI + 9,1                                 MD180512
                                                                        MD180512
           MOVE 1 TO I-FA                                               MD180512
                                                                        MD180512
           PERFORM VARYING I-FA FROM 1 BY 1 UNTIL I-FA > 37             MD180512
               ADD 0,4 TO Y-POS                                         MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + 3,0                             MD180512
               MOVE NUEVES         TO M-CANT-FA2                        MD180512
               MOVE LINEA-CANT-FA TO TEXTO                              MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + 3,5                             MD180512
               MOVE XX(1:40)      TO TEXTO                              MD180512
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + 15,2                            MD180512
               MOVE NUEVES         TO M-PREC-FA2                        MD180512
               MOVE AREA-PRECIO TO TEXTO                                MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + C-B                             MD180512
               MOVE NUEVES          TO M-BRUTO-FA                       MD180512
               MOVE M-BRUTO-FA      TO TEXTO                            MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
           END-PERFORM                                                  MD180512
                                                                        MD180512
           COMPUTE Y-POS  = Y-INI + 24,8                                MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-ME                                MD240512
           MOVE "XXXX" TO TEXTO                                         MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-MN                                MD240512
           MOVE "XX"  TO TEXTO                                          MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-B                                 MD180512
           MOVE NUEVES         TO M-BRUTO-FA                            MD180512
           MOVE M-BRUTO-FA     TO TEXTO                                 MD180512
           PERFORM IMPRIME-TEXTO-DERECHA                                MD180512
                                                                        MD180512
           COMPUTE Y-POS  = Y-INI + 25,8                                MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-B                                 MD180512
           MOVE NUEVES     TO M-BRUTO-FA                                MD180512
           MOVE M-BRUTO-FA TO TEXTO                                     MD180512
           PERFORM IMPRIME-TEXTO-DERECHA                                MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-ME                                MD240512
           MOVE "XXXX" TO TEXTO                                         MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + C-MN                                MD240512
           MOVE "XX"  TO TEXTO                                          MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           COMPUTE X-POS  = X-INI + 13,5                                MD180512
           MOVE NUEVES     TO M-PIVA                                    MD180512
           MOVE M-PIVA     TO TEXTO                                     MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           COMPUTE Y-POS  = Y-INI + 26,7                                MD180512
           COMPUTE X-POS  = X-INI + C-B                                 MD180512
           MOVE NUEVES        TO M-BRUTO-FA                             MD180512
           MOVE M-BRUTO-FA    TO TEXTO                                  MD180512
           PERFORM IMPRIME-TEXTO-DERECHA                                MD180512
                                                                        MD180512
      * OBS 1                                                           MD180512
           COMPUTE Y-POS  = Y-INI + 24,9                                MD180512
           COMPUTE X-POS  = X-INI + 1,5                                 MD180512
           MOVE XX(1:60)    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * OBS 2                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           MOVE XX(1:60)    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * OBS 1                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           COMPUTE X-POS  = X-INI + 1,5                                 MD180512
           MOVE XX(1:40)    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * OBS 2                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           MOVE XX(1:40)    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           WRITE LINEA FROM SPACES BEFORE PAGE.                         MD180512
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
                                                                        MD020112
           IF HABILIT-IMPR = "N"                                        MD020112
               MOVE DESH-AUX TO X-INI                                   MD221211
               MOVE DESV-AUX TO Y-INI                                   MD221211
           ELSE                                                         MD221211
               MOVE 0        TO X-INI                                   MD221211
               MOVE 0        TO Y-INI                                   MD221211
           END-IF                                                       MD221211
                                                                        MD020112
           MOVE "Courier New" TO LF-FaceName.                           
           MOVE 11            TO LF-Height.                             MD240512
           MOVE 600           TO LF-WeightValue.                        MD210512
      * FECHA DE DOCUMENTO                                              MD180512
           COMPUTE Y-POS  = Y-INI + 3,8                                 MD240512
           COMPUTE X-POS  = X-INI + 16,3                                MD240512
           MOVE FECHA-FA TO FECHA-AMD                                   MD180512
           PERFORM AMD-TO-ED                                            MD180512
           MOVE FECHA-ED TO TEXTO                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * NOMBRE DEL DOCUMENTO                                            MD180512
           COMPUTE Y-POS  = Y-INI + 5,1                                 MD240512
           COMPUTE X-POS  = X-INI + 11,8                                MD240512
           IF DESCI-PDC NOT = SPACES                                    
               MOVE DESCI-PDC TO TEXTO(1:22)                            MD180512
           ELSE                                                         MD180512
               MOVE DESC-PDC  TO TEXTO(1:22)                            MD180512
           END-IF                                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * NUMERO DE DOCUMENTO                                             MD180512
           COMPUTE X-POS  = X-INI + 16,7                                MD180512
           MOVE NRO-FA   TO M-NRO-FA                                    MD180512
           MOVE M-NRO-FA TO TEXTO                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           MOVE 400           TO LF-WeightValue.                        MD210512
      * NOMBRE                                                          MD180512
           COMPUTE Y-POS  = Y-INI + 5,55                                MD240512
           COMPUTE X-POS  = X-INI + 3,4                                 MD180512
           IF RSOC-FA NOT = SPACE                                       MD180512
               MOVE RSOC-FA   TO TEXTO                                  MD180512
           ELSE                                                         MD180512
               MOVE NOMCLI-FA TO TEXTO                                  MD180512
           END-IF                                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        
      * DIRECCION                                                       MD221211
           COMPUTE Y-POS  = Y-INI + 6,15                                MD240512
           MOVE DIREC-FA TO TEXTO                                       MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              
                                                                        MD180512
      * LOCALIDAD                                                       MD180512
           COMPUTE Y-POS  = Y-INI + 6,85                                MD210512
           MOVE LOCALID-FA TO TEXTO                                     MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
           IF RUC-FA NOT = SPACES                                       MD180512
               MOVE RUC-FA  TO M-RUC-FA                                 MD180512
               MOVE SPACES  TO M-CF                                     MD180512
           ELSE                                                         MD180512
               MOVE ALL "-" TO M-RUC-FA                                 MD180512
               MOVE "X"     TO M-CF                                     MD180512
           END-IF                                                       MD180512
                                                                        MD180512
      * RUC                                                             MD180512
           COMPUTE Y-POS  = Y-INI + 6,7                                 MD240512
           COMPUTE X-POS  = X-INI + 13,0                                MD180512
           MOVE M-RUC-FA    TO TEXTO                                    MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * CF                                                              MD180512
           COMPUTE X-POS  = X-INI + 18,0                                MD180512
           MOVE M-CF   TO TEXTO                                         MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
           															 MD180512
           COMPUTE Y-POS  = Y-INI + 9,1                                 MD180512
                                                                        
           MOVE 1 TO I-FA                                               
                                                                        
           PERFORM VARYING I-FA FROM 1 BY 1 UNTIL I-FA > CLIN-FA OR     MD180512
                                                  I-FA > 37             MD180512
               ADD 0,4 TO Y-POS                                         MD180512
                                                                        MD230512
               IF EXART-FA(I-FA) NOT = SPACES                           MD230512
                   MOVE EXART-FA(I-FA) TO CHARAVAL-VAR                  MD230512
                   PERFORM CALL-CHARAVAL                                MD230512
                   IF CHARAVAL-RET = 0                                  MD240512
                       MOVE CHARAVAL-NUM TO FEC-6                       MD230512
                       MOVE DD-6         TO FECHA-EDM                   MD230512
                       MOVE "/"          TO FECHA-EDM(3:)               MD230512
                       MOVE MM-6         TO FECHA-EDM(4:)               MD230512
                       MOVE "/"          TO FECHA-EDM(6:)               MD230512
                       MOVE AA-6         TO FECHA-EDM(7:)               MD230512
                       MOVE FECHA-EDM    TO TEXTO                       MD230512
                                                                        MD230512
                       COMPUTE X-POS  = X-INI + 3,4                     MD240512
                       PERFORM IMPRIME-TEXTO-DERECHA                    MD230512
                   END-IF                                               MD230512
               END-IF                                                   MD230512
                                                                        MD180512
      *        COMPUTE X-POS  = X-INI + 3,0                             MD230512
      *        EVALUATE DECIC-FA(I-FA)                                  MD230512
      *            WHEN 0 MOVE ICANT-FA(I-FA) TO M-CANT-FA              MD230512
      *            WHEN 1 MOVE ICANT-FA(I-FA) TO M-CANT-FA1             MD230512
      *            WHEN 2 MOVE ICANT-FA(I-FA) TO M-CANT-FA2             MD230512
      *            WHEN 3 MOVE ICANT-FA(I-FA) TO M-CANT-FA3             MD230512
      *            WHEN 4 MOVE ICANT-FA(I-FA) TO M-CANT-FA4             MD230512
      *            WHEN 5 MOVE ICANT-FA(I-FA) TO M-CANT-FA5             MD180512
      *        END-EVALUATE                                             MD230512
      *        MOVE LINEA-CANT-FA TO TEXTO                              MD230512
      *        PERFORM IMPRIME-TEXTO-DERECHA                            MD230512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + 3,5                             MD180512
               MOVE DESC-FA(I-FA) TO TEXTO                              
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          
                                                                        MD180512
               IF EMVAL-PDC(YY) NOT = "N"                               MD180512
      *            COMPUTE X-POS  = X-INI + 15,2                        MD230512
      *            EVALUATE DECPRE-FA                                   MD230512
      *                WHEN 0 MOVE PREC-FA(I-FA) TO M-PREC-FA           MD230512
      *                WHEN 1 MOVE PREC-FA(I-FA) TO M-PREC-FA1          MD230512
      *                WHEN 2 MOVE PREC-FA(I-FA) TO M-PREC-FA2          MD230512
      *                WHEN 3 MOVE PREC-FA(I-FA) TO M-PREC-FA3          MD230512
      *                WHEN 4 MOVE PREC-FA(I-FA) TO M-PREC-FA4          MD230512
      *                WHEN 5 MOVE PREC-FA(I-FA) TO M-PREC-FA5          MD230512
      *            END-EVALUATE                                         MD230512
      *            MOVE AREA-PRECIO TO TEXTO                            MD230512
      *            PERFORM IMPRIME-TEXTO-DERECHA                        MD230512
                                                                        MD180512
                   COMPUTE X-POS  = X-INI + C-B                         MD180512
                   MOVE BRUTO-CIV(I-FA) TO M-BRUTO-FA                   MD180512
                   MOVE M-BRUTO-FA      TO TEXTO                        MD180512
                   PERFORM IMPRIME-TEXTO-DERECHA                        MD180512
               END-IF                                                   MD180512
           END-PERFORM                                                  
                                                                        MD180512
           IF EMVAL-PDC(YY) NOT = "N"                                   MD180512
               MOVE 600           TO LF-WeightValue                     MD210512
                                                                        MD210512
               COMPUTE Y-POS  = Y-INI + 24,4                            MD240512
                                                                        MD180512
               IF MONE-FA = MONE-PSYS                                   MD180512
                   COMPUTE X-POS  = X-INI + C-ME                        MD240512
                   MOVE "XXXX" TO TEXTO                                 MD180512
               ELSE                                                     MD180512
                   COMPUTE X-POS  = X-INI + C-MN                        MD240512
                   MOVE "XX"  TO TEXTO                                  MD180512
               END-IF                                                   MD180512
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + C-B                             MD180512
               MOVE SUMA-BRUTO-CIV TO M-BRUTO-FA                        MD180512
               MOVE M-BRUTO-FA     TO TEXTO                             MD180512
               PERFORM IMPRIME-TEXTO-DERECHA                            MD180512
                                                                        MD180512
               IF TOTIVA NOT = 0                                        MD180512
                   COMPUTE Y-POS  = Y-INI + 25,3                        MD240512
                                                                        MD180512
                   COMPUTE X-POS  = X-INI + C-B                         MD180512
                   MOVE TOTIVA     TO M-BRUTO-FA                        MD180512
                   MOVE M-BRUTO-FA TO TEXTO                             MD180512
                   PERFORM IMPRIME-TEXTO-DERECHA                        MD180512
                                                                        MD180512
                   IF MONE-FA = MONE-PSYS                               MD180512
                       COMPUTE X-POS  = X-INI + C-ME                    MD240512
                       MOVE "XXXX" TO TEXTO                             MD180512
                   ELSE                                                 MD180512
                       COMPUTE X-POS  = X-INI + C-MN                    MD240512
                       MOVE "XX"  TO TEXTO                              MD180512
                   END-IF                                               MD180512
                   PERFORM IMPRIME-TEXTO-IZQUIERDA                      MD180512
                                                                        MD180512
                   COMPUTE X-POS  = X-INI + 13,4                        MD240512
                   IF TOTIVAN(1) NOT = 0                                SF150914
                       MOVE PIVA-FA(1) TO M-PIVA                        MD180512
                   ELSE                                                 MD180512
                       MOVE PIVA-FA(2) TO M-PIVA                        MD180512
                   END-IF                                               MD180512
                   MOVE M-PIVA     TO TEXTO                             MD180512
                   PERFORM IMPRIME-TEXTO-IZQUIERDA                      MD180512
               END-IF                                                   MD180512
                                                                        MD180512
               COMPUTE Y-POS  = Y-INI + 26,3                            MD240512
                                                                        MD180512
               IF MONE-FA = MONE-PSYS                                   MD180512
                   COMPUTE X-POS  = X-INI + C-ME                        MD240512
                   MOVE "XXXX" TO TEXTO                                 MD180512
               ELSE                                                     MD180512
                   COMPUTE X-POS  = X-INI + C-MN                        MD240512
                   MOVE "XX"  TO TEXTO                                  MD180512
               END-IF                                                   MD180512
               PERFORM IMPRIME-TEXTO-IZQUIERDA                          MD180512
                                                                        MD180512
               COMPUTE X-POS  = X-INI + C-B                             MD180512
               MOVE TOTAL-A-PAGAR TO M-BRUTO-FA                         MD180512
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
           MOVE OBS-FA(1)  TO TEXTO                                     MD180512
           PERFORM IMPRIME-TEXTO-IZQUIERDA                              MD180512
                                                                        MD180512
      * OBS 2                                                           MD180512
           COMPUTE Y-POS  = Y-POS + 0,4                                 MD180512
           MOVE OBS-FA(2)  TO TEXTO(1:40)                               MD180512
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
                                                                        
       COPY "ARTICULO_SL.ES".                                           SF150914
       COPY "CODIGOS.ES".                                               
       COPY "TDOCRUTI.ES".                                              
       COPY "ARTICULO.AUP".                                             
       COPY "CODIGOS.AUP".                                              
       COPY "TDOCRUTI.AUP".                                             
                                                                        
       CARGO-DATOS.                                                     
           MOVE TOTAL-A-PAGAR TO IMPORTE-PAR                            
           MOVE 0 TO CON100-PAR                                         
           CALL "NUMLET"  USING NUMLET-PAR                              
           PERFORM CANCEL-NUMLET.                                       
                                                                        
           SET IMPRIME-BITMAP TO FALSE                                  
                                                                        
           MOVE TIDOC-FA  TO TIDOC-TDR.                                 
           MOVE "EMIFACN" TO RUTINA-TDR.                                MD180512
           PERFORM LEC01-TDOCRUTI.                                      
           IF ST-XX = "23"                                              
               GO TO EXIT-CD                                            
           END-IF                                                       
                                                                        
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
                                                                        
      *CALL-CHARAVAL.                                                   SF150914
      *    MOVE CHARAVAL-VAR TO CHARAVAL-TXT                            SF150914
      *                                                                 SF150914
      *    MOVE 0      TO CHARAVAL-NUM                                  SF150914
      *    MOVE 0      TO CHARAVAL-RET                                  SF150914
      *                                                                 SF150914
      *    CALL "CHARAVAL" USING CHARAVAL-TXT                           SF150914
      *                          CHARAVAL-NUM                           SF150914
      *                          GIVING CHARAVAL-RET                    SF150914
      *    END-CALL.                                                    SF150914
      *    CANCEL "CHARAVAL".                                           SF150914
                                                                        
       