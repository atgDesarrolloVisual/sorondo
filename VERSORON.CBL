        IDENTIFICATION DIVISION.                                                
        PROGRAM-ID. VERSORON.                                                   
        ENVIRONMENT DIVISION.                                                   
        DATA DIVISION.                                                          
        WORKING-STORAGE SECTION.                                                
        01  DATO-VERSION                                                        
              PIC X(14) VALUE "00000000000000".                                 
        01  DATO-SECCION                                                        
              PIC X(40) VALUE SPACES.                                           
        LINKAGE SECTION.                                                        
        01  LINK-VERSION         PIC X(14).                                     
        01  LINK-SECCION         PIC X(40).                                     
        PROCEDURE DIVISION USING LINK-VERSION                                   
                                 LINK-SECCION.                                  
        COMIENZO SECTION.                                                       
        PROCESO.                                                                
           MOVE DATO-VERSION TO LINK-VERSION.                                   
           MOVE DATO-SECCION TO LINK-SECCION.                                   
        FIN-NORMAL.                                                             
            EXIT PROGRAM.                                                       
