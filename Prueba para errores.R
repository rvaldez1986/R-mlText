Sys.setlocale('LC_ALL','C') 
#================== Libraries ===============================================
library(data.table)
library(stringdist) #es la unica libreria impresidible
library(plyr)

#================== Funciones ==============================================



#######====================Funcion de comparacion(usa lo anterior)=======================================
#######==================================================================================================


#source functions (validate and compare) from files
source("C:/Users/rober/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_val_act.R")
source("C:/Users/rober/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_val_ant.R")
source("C:/Users/rober/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_comp.R")

VAL1 = fun_validate_ant("BASE ANTERIOR TEVCOL.csv", "C:/Users/rober/Documentos/ACTUARIA/act-remote/r_comparador/BD_pruebas/V2")
VAL2 = fun_validate_act("BASE ACTUAL TEVCOL.csv", "C:/Users/rober/Documentos/ACTUARIA/act-remote/r_comparador/BD_pruebas/V2")



data1 = VAL1$data1
data2 = VAL2$data2

dim(data1)
dim(data2)

head(data2)

  
f_calc = "2016.12.31"
F_Calc = as.Date(f_calc, format="%Y.%m.%d")
  

  
data1.1 = subset(data1, !is.na(data1$CEDULA))
data2.1 = subset(data2, !is.na(data2$CEDULA))
  
data1.2 = subset(data1, is.na(data1$CEDULA)) 
data2.2 = subset(data2, is.na(data2$CEDULA))
  
Nueva = data.table(data2.1) #Nuevas con cedula
Nueva2 = data.table(data2.2) #Nuevas sin cedula
Nueva3 = rbind(Nueva, Nueva2)
Vieja = data.table(data1.1)  #Viejas con cedula
Vieja2 = data.table(data1.2) #Viejas sin cedula
Vieja3 = rbind(Vieja, Vieja2)
  
Vieja = Vieja[,ESTADO := "NA"]  #anadimos nueva columna, llamada esta, valor NA
Vieja2 = Vieja2[,ESTADO := "NA"] #" "
  
n2.1 = nrow(Nueva)
n2.2 = nrow(Nueva2)
n1.1 = nrow(Vieja)
n1.2 = nrow(Vieja2)
n1 = n1.1 + n1.2
n2 = n2.1 + n2.2

#Incluimos path determinado

RES = cedulaCompare(Nueva, Vieja)

#Returns 

#Antiguos y Actuales

head(RES$ACT_ANT)
dim(RES$ACT_ANT)  #25 cols, 13 + 11 + ESTADO

#Si hay algunos que no se encuntran (RES$INGRESOS), comparamos con los que no tienen cedula, simple

RES2 = noCedulaCompare(RES$INGRESOS, Vieja2, 1)  #Type = 1, SimpleCompare

#Si hay algunos que no se encuntran (RES2$INGRESOS), comparamos con los que no tienen cedula, smart

RES3 = noCedulaCompare(RES2$INGRESOS, Vieja2, 2)

RES4 = noCedulaCompare(Nueva2, Vieja3, 1)

RES5 = noCedulaCompare(RES4$INGRESOS, Vieja3, 2)


ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES3$ACT_ANT, 
                RES4$ACT_ANT, RES5$ACT_ANT)

INGRESOS = rbind(RES3$INGRESOS, RES5$INGRESOS)

SALIDAS = Vieja3[ !(N %in% ACT_ANT$N2) ]

COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
COUNT2 = COUNT[N > 1]
NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
NS$ESTADO = "Unsure"

ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)

UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
ACTIVOS_ANTIG = ACT_ANT3

#No hay post_compare, unsure


















  
if(n1.2 > 0 & n1.2 < n1 & n2.2 > 0 & n2.2 < n2){
    
  RES = cedulaCompare(Nueva, Vieja)
    
  if(nrow(RES$INGRESOS) > 0){
      
    RES2 = noCedulaCompare(RES$INGRESOS, Vieja2, 1)
      
    if(nrow(RES2$INGRESOS) > 0){
        
      RES3 = noCedulaCompare(RES2$INGRESOS, Vieja2, 2)
        
    }
      
  }
    
  RES4 = noCedulaCompare(Nueva2, Vieja3, 1)
    
  if(nrow(RES4$INGRESOS) > 0){
      
    RES5 = noCedulaCompare(RES4$INGRESOS, Vieja3, 2)
      
  }
    
  if(nrow(RES$INGRESOS) > 0){
      
    if(nrow(RES2$INGRESOS) > 0){
        
      if(nrow(RES4$INGRESOS) > 0){
          
        ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES3$ACT_ANT, 
                          RES4$ACT_ANT, RES5$ACT_ANT)
          
        INGRESOS = rbind(RES3$INGRESOS, RES5$INGRESOS)
          
      } else{
          
        ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES3$ACT_ANT, 
                          RES4$ACT_ANT)
          
        INGRESOS = rbind(RES3$INGRESOS)
      }
        
        
    } else{
        
      if(nrow(RES4$INGRESOS) > 0){
          
        ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT,  
                          RES4$ACT_ANT, RES5$ACT_ANT)
          
        INGRESOS = rbind(RES2$INGRESOS, RES5$INGRESOS)
          
      } else{
          
        ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES4$ACT_ANT)
          
        INGRESOS = rbind(RES2$INGRESOS, RES4$INGRESOS)
          
      }
        
    }
      
  } else{
      
    if(nrow(RES4$INGRESOS) > 0){
        
      ACT_ANT = rbind(RES$ACT_ANT, RES4$ACT_ANT, RES5$ACT_ANT)
        
      INGRESOS = rbind(RES$INGRESOS, RES5$INGRESOS)
        
    } else{
        
      ACT_ANT = rbind(RES$ACT_ANT, RES4$ACT_ANT)
        
      INGRESOS = rbind(RES$INGRESOS, RES4$INGRESOS)
        
    }
      
  }
    
  SALIDAS = Vieja3[ !(N %in% ACT_ANT$N2) ]
    
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
    
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
    
  UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
  ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
  ACTIVOS_ANTIG = ACT_ANT3
    
} else if(n1.2 == 0 & n2.2 > 0 & n2.2 < n2){
    
  #Vieja todos con cedula
  RES = cedulaCompare(Nueva, Vieja)
  RES2 = noCedulaCompare(Nueva2, Vieja, 1)
    
  if(nrow(RES2$INGRESOS) > 0){
      
    RES3 = noCedulaCompare(RES2$INGRESOS, Vieja, 2)
      
  }
    
  if(nrow(RES2$INGRESOS) > 0){
      
    ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES3$ACT_ANT)
      
    INGRESOS = rbind(RES$INGRESOS, RES3$INGRESOS)
      
  } else{
      
    ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT)
      
    INGRESOS = rbind(RES$INGRESOS, RES2$INGRESOS)
      
  }
    
  SALIDAS = Vieja3[ !(N %in% ACT_ANT$N2) ]
    
    
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
    
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
    
  UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
  ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
  ACTIVOS_ANTIG = ACT_ANT3
    
} else if(n1.2 > 0 & n1.2 < n1 & n2.2 == 0){
    
  #Nueva todos con cedula
  RES = cedulaCompare(Nueva, Vieja)
    
  if(nrow(RES$INGRESOS) > 0){
      
    RES2 = noCedulaCompare(RES$INGRESOS, Vieja2 , 1)
      
    if(nrow(RES2$INGRESOS) > 0){
        
      RES3 = noCedulaCompare(RES2$INGRESOS, Vieja2 , 2)
        
    }
      
  }
    
  if(nrow(RES$INGRESOS) > 0){
      
    if(nrow(RES2$INGRESOS) > 0){
        
      ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT, RES3$ACT_ANT)
        
      INGRESOS = rbind(RES3$INGRESOS)
        
        
    } else{
        
      ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT)
        
      INGRESOS = rbind(RES2$INGRESOS)
        
    }
      
      
  } else{
      
    ACT_ANT = rbind(RES$ACT_ANT)
      
    INGRESOS = rbind(RES$INGRESOS)
      
  }
    
  SALIDAS = Vieja3[ !(N %in% ACT_ANT$N2) ]
    
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
    
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
    
  UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
  ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
  ACTIVOS_ANTIG = ACT_ANT3
    
} else if(n1.2 == 0 & n2.2 == 0){  
    
  #Modelo con cedula todos
  RES = cedulaCompare(Nueva, Vieja)
    
  ACT_ANT = RES$ACT_ANT
  SALIDAS = Vieja[ !(N %in% ACT_ANT$N2) ]
  INGRESOS = RES$INGRESOS
    
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
    
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
    
  UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
  ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
  ACTIVOS_ANTIG = ACT_ANT3
    
} else {
    
  #Modelo sin cedula todos
  RES = noCedulaCompare(Nueva3, Vieja3, 1)
    
  if(nrow(RES$INGRESOS) > 0){
      
    RES2 = noCedulaCompare(RES$INGRESOS, Vieja3, 2)
      
  }
    
  if(nrow(RES$INGRESOS) > 0){
      
    ACT_ANT = rbind(RES$ACT_ANT, RES2$ACT_ANT)
      
    INGRESOS = rbind(RES2$INGRESOS)
      
  } else{
      
    ACT_ANT = rbind(RES$ACT_ANT)
    INGRESOS = rbind(RES$INGRESOS)
      
  }
    
  SALIDAS = Vieja3[ !(N %in% ACT_ANT$N2) ]
    
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
    
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
    
  UNSURE = ACT_ANT2[ ESTADO == "Unsure"]
  ACT_ANT3 = ACT_ANT2[ ESTADO != "Unsure"]
  ACTIVOS_ANTIG = ACT_ANT3
    
}
  
#Post Compare
  
if(nrow(UNSURE) > 0){
    
  UNS2 = post_compare(UNSURE)
  ACTIVOS_ANTIG = rbind(ACTIVOS_ANTIG, UNS2$ACT_ANT,fill=TRUE)
  INGRESOS = rbind(INGRESOS, UNS2$INGRESOS, fill=TRUE)
  SALIDAS = rbind(SALIDAS, UNS2$SALIDAS, fill=TRUE)
  UNSURE = UNS2$UNSURE
    
}
  
ACTIVOS_ANTIG = ACTIVOS_ANTIG[order(-NOMBRE, decreasing=TRUE),]
INGRESOS = INGRESOS[order(-NOMBRE, decreasing=TRUE),] 
SALIDAS = SALIDAS[order(-NOMBRE, decreasing=TRUE),] 
UNSURE = UNSURE[order(-NOMBRE, decreasing=TRUE),] 
  
#Crear Validation column
  
ACTIVOS_ANTIG$SUELDO_JUB = as.character(ACTIVOS_ANTIG$SUELDO_JUB)
ACTIVOS_ANTIG$SUELDO_JUB2 = as.character(ACTIVOS_ANTIG$SUELDO_JUB2)
  
ACTIVOS_ANTIG$SUELDO_DES = as.character(ACTIVOS_ANTIG$SUELDO_DES)
ACTIVOS_ANTIG$SUELDO_DES2 = as.character(ACTIVOS_ANTIG$SUELDO_DES2)
  
ACTIVOS_ANTIG$SUELDO_JUB = gsub("\\,", "", ACTIVOS_ANTIG$SUELDO_JUB)
ACTIVOS_ANTIG$SUELDO_JUB2 = gsub("\\,", "", ACTIVOS_ANTIG$SUELDO_JUB2)
  
ACTIVOS_ANTIG$SUELDO_DES = gsub("\\,", "", ACTIVOS_ANTIG$SUELDO_DES)
ACTIVOS_ANTIG$SUELDO_DES2 = gsub("\\,", "", ACTIVOS_ANTIG$SUELDO_DES2)
  
ACTIVOS_ANTIG$SUELDO_JUB = as.numeric(ACTIVOS_ANTIG$SUELDO_JUB)
ACTIVOS_ANTIG$SUELDO_JUB2 = as.numeric(ACTIVOS_ANTIG$SUELDO_JUB2)
  
ACTIVOS_ANTIG$SUELDO_DES = as.numeric(ACTIVOS_ANTIG$SUELDO_DES)
ACTIVOS_ANTIG$SUELDO_DES2 = as.numeric(ACTIVOS_ANTIG$SUELDO_DES2)
  
DIF_JUB =  ACTIVOS_ANTIG$SUELDO_JUB -  ACTIVOS_ANTIG$SUELDO_JUB2
PERC_JUB = DIF_JUB/ACTIVOS_ANTIG$SUELDO_JUB2
  
DIF_DES =  ACTIVOS_ANTIG$SUELDO_DES -  ACTIVOS_ANTIG$SUELDO_DES2
PERC_DES = DIF_DES/ACTIVOS_ANTIG$SUELDO_DES2
  
c1 = ifelse(ACTIVOS_ANTIG$NOMBRE != ACTIVOS_ANTIG$NOMBRE2, "Corregir Nombre", "") 
c2 = ifelse(ACTIVOS_ANTIG$SEXO != ACTIVOS_ANTIG$SEXO2, "Corregir Sexo", "") 
c3 = ifelse(ACTIVOS_ANTIG$F_NAC != ACTIVOS_ANTIG$F_NAC2, "Corregir F_NAC", "")
c4 = ifelse(ACTIVOS_ANTIG$F_ING != ACTIVOS_ANTIG$F_ING2, "Corregir F_ING", "")
c5 = ifelse(ACTIVOS_ANTIG$F_DES != ACTIVOS_ANTIG$F_DES2, "Corregir F_DES", "")
c6 = ifelse(is.na(ACTIVOS_ANTIG$CEDULA),
              ifelse(is.na(ACTIVOS_ANTIG$CEDULA2),"","Corregir Cedula"),
              ifelse(is.na(ACTIVOS_ANTIG$CEDULA2),"Corregir Cedula",
                     ifelse(ACTIVOS_ANTIG$CEDULA != ACTIVOS_ANTIG$CEDULA2, "Corregir Cedula", "")))
  
c7 = ifelse(is.na(DIF_JUB)|is.na(PERC_JUB), "Corregir Sueldo Jubilacion", ifelse(DIF_JUB > 400 & PERC_JUB > 0.4, "Corregir Sueldo Jubilacion",
                                                                ifelse(DIF_JUB < -200 & PERC_JUB < -0.2, "Corregir Sueldo Jubilacion", "")))
  
c8 = ifelse(is.na(DIF_DES)|is.na(PERC_DES), "Corregir Sueldo Desahucio", ifelse(DIF_DES > 400 & PERC_DES > 0.4, "Corregir Sueldo Desahucio",
                                                                        ifelse(DIF_DES < -200 & PERC_DES < -0.2, "Corregir Sueldo Desahucio", "")))
  
ACTIVOS_ANTIG$ESTADO = trim(paste(c1,c2,c3,c4,c5,c6,c7,c8))
  
  #Validador con F_Calc
  
sal = (F_Calc - SALIDAS$F_ING)/365.25
ing = (F_Calc - INGRESOS$F_ING)/365.25
  
c_sal = ifelse(sal >= 20, "Verificar Jub Patronal", "")
c_ing = ifelse(ing >= 1, "Verificar Ingresos", "")
  
SALIDAS$ESTADO = c_sal
INGRESOS$ESTADO = c_ing
  
#Formatear entrega de archivo (fechas, numeros y columna en blanco)
#Aca hay que chequear si se cambian las columnas!!!!
  
#ACTIVOS_ANTIG = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES ,
#TIPO , CEDULA , N2 , CC2 , NOMBRE2 , SEXO2, F_NAC2 , F_ING2 , F_DES2 , SUELDO_JUB2 , SUELDO_DES2 , 
#TIPO2 , CEDULA2 , RESERVA_JUB , RESERVA_DES , ESTADO 
  
setcolorder(ACTIVOS_ANTIG, c(12:24, 1:11, 25)) #Movemos viejos primero, nuevos despues
b1 = rep(" ", nrow(ACTIVOS_ANTIG)) #Ingresar columna de espacios en blanco
ACTIVOS_ANTIG$SEP = b1
setcolorder(ACTIVOS_ANTIG, c(1:13,26,14:25))
  
setcolorder(UNSURE, c(12:24, 1:11, 25))
b2 = rep(" ", nrow(UNSURE)) #Ingresar columna de espacios en blanco
UNSURE$SEP = b2
setcolorder(UNSURE, c(1:13,26,14:25))
  
ACTIVOS_ANTIG$F_NAC = format(ACTIVOS_ANTIG$F_NAC, "%Y.%m.%d") 
ACTIVOS_ANTIG$F_ING = format(ACTIVOS_ANTIG$F_ING, "%Y.%m.%d") 
ACTIVOS_ANTIG$F_DES = format(ACTIVOS_ANTIG$F_DES, "%Y.%m.%d")
ACTIVOS_ANTIG$F_NAC2 = format(ACTIVOS_ANTIG$F_NAC2, "%Y.%m.%d") 
ACTIVOS_ANTIG$F_ING2 = format(ACTIVOS_ANTIG$F_ING2, "%Y.%m.%d") 
ACTIVOS_ANTIG$F_DES2 = format(ACTIVOS_ANTIG$F_DES2, "%Y.%m.%d")
  
INGRESOS$F_NAC = format(INGRESOS$F_NAC, "%Y.%m.%d") 
INGRESOS$F_ING = format(INGRESOS$F_ING, "%Y.%m.%d") 
INGRESOS$F_DES = format(INGRESOS$F_DES, "%Y.%m.%d")
  
SALIDAS$F_NAC = format(SALIDAS$F_NAC, "%Y.%m.%d") 
SALIDAS$F_ING = format(SALIDAS$F_ING, "%Y.%m.%d") 
SALIDAS$F_DES = format(SALIDAS$F_DES, "%Y.%m.%d")
  
UNSURE$F_NAC = format(UNSURE$F_NAC, "%Y.%m.%d") 
UNSURE$F_ING = format(UNSURE$F_ING, "%Y.%m.%d") 
UNSURE$F_DES = format(UNSURE$F_DES, "%Y.%m.%d")
UNSURE$F_NAC2 = format(UNSURE$F_NAC2, "%Y.%m.%d") 
UNSURE$F_ING2 = format(UNSURE$F_ING2, "%Y.%m.%d") 
UNSURE$F_DES2 = format(UNSURE$F_DES2, "%Y.%m.%d")
  
#Escribir los archivos
  
write.csv(ACTIVOS_ANTIG, paste("", "ACT_ANT.csv", sep="_"), row.names = F)
write.csv(INGRESOS, paste("", "INGRESOS.csv", sep="_"), row.names = F)
write.csv(SALIDAS, paste("", "SALIDAS.csv", sep="_"), row.names = F)
write.csv(UNSURE, paste("", "UNSURE.csv", sep="_"), row.names = F)
  


#============================== fin funciones =============================================



 

  
  