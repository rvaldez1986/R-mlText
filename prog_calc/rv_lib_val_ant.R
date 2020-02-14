#================== Libraries ===============================================
library(plyr)
library(Hmisc)

#================== Funciones ==============================================

funval<-function(x){
  if(nchar(x)!=10){return("numero incorrecto de caracteres")}
  else{
    pares <- substring(x,c(2,4,6,8),c(2,4,6,8))
    impares <- substring(x,c(1,3,5,7,9),c(1,3,5,7,9))
    ultdig <- substr(x,10,10)
    if(!all.is.numeric(x)) {return("error en caracteres")}  #hmisc
    else{
      pares <- as.numeric(pares)
      impares <- as.numeric(impares)
      ultdig <- as.numeric(ultdig)
      provincia <- impares[1] * 10 + pares[1]
      if(provincia < 1 | provincia > 24){return("error en provincia")}
      else{
        impares2<-ifelse(2 * impares > 9, 2 * impares - 9, 2 * impares)
        spares<-sum(pares)
        simpares<-sum(impares2)
        stot<-spares+simpares
        DS<-round_any(stot, 10, f = ceiling)
        verif<-(DS - stot) - ultdig
        if(verif==0){return("correcta")} else{return("error en cedula")}
      }
    }
  }
}

extractNonNum = function(x){
  
  v = unlist(strsplit(x,""))
  v2 = paste(v[!is.na(suppressWarnings(as.numeric(v)))],collapse = "")  
  return(as.character(v2))  
  
}

depur1<-function(datos,ftc=TRUE){
  if(ftc){datos<-fact_to_char(datos)}#convierte todo Factor en char (opcional)
  for(i in 1:ncol(datos)){
    if(class(datos[,i])=="character") {datos[,i]<-gsub("^$",NA,datos[,i])}
  } #reemplaza vacios por NA
  if(ftc){datos<-fact_to_char(datos)}#convierte todo Factor en char (opcional)
  datos<-col.rowNA(datos) #saca filas y columnas totalmente vacias
  datos<-trimBase(datos)
  datos
}

fact_to_char<-function(base){ 
  for(i in 1:ncol(base)){
    if(class(base[,i])[1]=="factor"){base[,i]<-as.character(base[,i])} 
  }
  base
}

col.rowNA<-function(datos){
  datos<-datos[(rowSums(is.na(datos))<ncol(datos)),colSums(is.na(datos))<nrow(datos)]
  datos
}

trimBase<-function(base){
  a<-grep("character",base.clases(base)$clase)
  for(i in a){base[,i]<-trim(base[,i])}
  base
}

base.clases<-function(base) {
  clases<-data.frame("clase"=rep(NA,ncol(base)))
  for(i in 1:ncol(base)){
    clases$clase[i]<-class(base[,i])[1]
  }
  clases$nombres<-names(base)
  clases 
}

trim<-function(x){gsub("^\\s+|\\s+$","",x)}


fun_validate_ant = function(B_ANTERIOR, read_dir){
  
  setwd(read_dir)
  
  data1 = read.csv(B_ANTERIOR, sep=",")
  
  if(ncol(data1) != 13)
    {stop(sprintf("Error en numero de columnas"))}
  
  if(length(which((colnames(data1) == c("N","CC","NOMBRE","SEXO","F_NAC","F_ING","F_DES","SUELDO_JUB","SUELDO_DES","TIPO","CEDULA","RESERVA_JUB","RESERVA_DES")) == FALSE)) > 0)
    {stop(sprintf("Error en nombre de columnas"))}
  
  data1 = depur1(data1)
  
  if(ncol(data1) != 13)
  {stop(sprintf("Error existe por lo menos una columna sin datos en ninguna fila, llene por lo menos 1 fila con valores"))}
  
  if(length(which(is.na(data1$NOMBRE) == TRUE)) > 0)
    {stop(paste("Error en casilla NOMBRE, lineas: ", toString(which(is.na(data1$NOMBRE) == TRUE)+1)))}
  
  if(length(which(is.null(data1$CC) == TRUE)) > 0)
    {stop(paste("Error en casilla CC, lineas: ", toString(which(is.null(data1$CC) == TRUE)+1)))}
  
  data1$NOMBRE = gsub("  "," ", gsub("  ", " ", data1$NOMBRE))
  data1$NOMBRE <- gsub("\240", " ", data1$NOMBRE, fixed = TRUE)
  data1$NOMBRE <- gsub("\u00A0", " ", data1$NOMBRE, fixed = TRUE)
    
  data1$F_NAC = as.Date(data1$F_NAC, format="%Y.%m.%d")
  
  if(length(which(is.na(data1$F_NAC) == TRUE)) > 0)
    {stop(paste("Error en F_NAC, lineas: ", toString(which(is.na(data1$F_NAC) == TRUE)+1)))}
  
  data1$F_ING = as.Date(data1$F_ING, format="%Y.%m.%d")
  data1$F_DES = as.Date(data1$F_DES, format="%Y.%m.%d")
  
  data1$N = as.numeric(data1$N)
  
  if(length(which(is.na(data1$N) == TRUE)) > 0)
    {stop(paste("No numerico en columna secuencial (N), lineas: ", toString(which(is.na(data1$N) == TRUE)+1)))}
  
  if(length(which(duplicated(data1[,c("N")]) == TRUE)) > 0)
    {stop(paste("Duplicado en columna secuencial (N), lineas: ", toString(which(duplicated(data1[,c("N")]) == TRUE)+1)))}
  
  #Transformamos a las cedulas en caracteres
  #data1 = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES , TIPO , CEDULA ,
  #RESERVA_JUB , RESERVA_DES
  
  data1$CEDULA = as.character(data1$CEDULA) 
  
  #Les quitamos los caracteres especiales
  
  data1$CEDULA = unlist(lapply(data1$CEDULA, extractNonNum))
  
  #Le pegamos un 0 a las que tienen 9 caracteres
  
  data1$CEDULA = ifelse(nchar(data1$CEDULA) == 9, 
                        paste("0", data1$CEDULA, sep=""), data1$CEDULA)
  
  nced = 9 
  data1$CEDULA2 = paste("0", substr(data1$CEDULA,(nchar(data1$CEDULA)+1)-nced,
                                    nchar(data1$CEDULA)), sep = "")
  
  #data1 = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES , TIPO , CEDULA ,
  #RESERVA_JUB , RESERVA_DES , CEDULA2
 
  Validacion11 = unlist(lapply(data1$CEDULA, funval))
  Validacion12 = unlist(lapply(data1$CEDULA2, funval))
  data1$CEDULA.C = ifelse(Validacion11 == "correcta", data1$CEDULA,
                          ifelse(Validacion12 == "correcta",
                                 data1$CEDULA2,NA))
  
  data1$CEDULA = NULL ; data1$CEDULA2 = NULL 
  data1 = rename(data1, c("CEDULA.C"="CEDULA"))
  
  #data1 = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES , TIPO , 
  #RESERVA_JUB , RESERVA_DES , CEDULA
  
  #Chequear columnas, modificar si cambia el n??mero de columnas
  data1 = data1[,c(1:10,13,11:12)]
  
  #Chequear duplicados
  if(length(which(duplicated(data1[,c("NOMBRE","F_NAC")]) == TRUE)) > 0)
    {stop(paste("Duplicado, lineas: ", toString(which(duplicated(data1[,c("NOMBRE","F_NAC")]) == TRUE)+1)))}
  
  newlist = list("data1" = data1)
  
  return(newlist)
  
}

#============================== Fin Funcions =============================================


  
  
