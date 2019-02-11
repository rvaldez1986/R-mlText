#================== Libraries ===============================================
library(data.table)
library(RecordLinkage)
library(plyr)
library(Hmisc)
library(kernlab)
library(stringdist)

#================== Funciones =============================================

funval<-function(x){
  if(nchar(x)!=10){return("numero incorrecto de caracteres")}
  else{
    pares <- substring(x,c(2,4,6,8),c(2,4,6,8))
    impares <- substring(x,c(1,3,5,7,9),c(1,3,5,7,9))
    ultdig <- substr(x,10,10)
    if(!all.is.numeric(x)) {return("error en caracteres")}
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

#===========================================================================

#source("//server/Publica Consultoria/SCRIPTS/funciones esteban.R")
setwd("C:/Users/rober/Desktop/Modelos ml texto")
dir()


data1 = read.csv("Base Modelos con Cedula (5).csv", sep=",")
#base.clases(data1)
data1 = depur1(data1)
data1$F_NAC = as.Date(data1$F_NAC, format="%d/%m/%Y")
data1$F_NAC2 = as.Date(data1$F_NAC2, format="%d/%m/%Y")

#Transformamos a las cedulas en caracteres

data1$CEDULA = as.character(data1$CEDULA) 
data1$CEDULA2 = as.character(data1$CEDULA2)

data1$CEDULA = ifelse(nchar(data1$CEDULA) == 9, 
                      paste("0", data1$CEDULA, sep=""), data1$CEDULA)
data1$CEDULA2 = ifelse(nchar(data1$CEDULA2) == 9, 
                      paste("0", data1$CEDULA2, sep=""), data1$CEDULA2)

summary(nchar(data1$CEDULA)); summary(nchar(data1$CEDULA2)) #Todos tienen 10
Validacion11 = unlist(lapply(data1$CEDULA, funval))
Validacion12 = unlist(lapply(data1$CEDULA2, funval))
table(Validacion11); table(Validacion12)

#================Modelo=======================================================

data1$PNAMES = jarowinkler(data1$NOMBRE,data1$NOMBRE2)
data1$PCED =  jarowinkler(data1$CEDULA, data1$CEDULA2)
data1$DFNAC = ifelse(data1$F_NAC == data1$F_NAC2, 1,0)
data1$DSUELDO = data1$SUELDO - data1$SUELDO2
data1$DCED = ifelse(data1$CEDULA == data1$CEDULA2, 1,0)
data1$PNAMES2 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'jaccard', q = 4)
data1$PNAMES22 = data1$PNAMES2^2
data1$PNAMES23 = data1$PNAMES2^3

#======================Vamos a utilizar l =========================
x = as.matrix(data1[,c(19,18,20,21)])  #we only use jaccard: linear, cuadratic and cubic and binary cedula
y = as.matrix(data1$LABEL)

svp <- ksvm(x,y,type="C-svc",kernel="vanilladot",C=500,scaled = FALSE,
            prob.model = TRUE)

w <- colSums(coef(svp)[[1]] * x[unlist(alphaindex(svp)),])
b <- b(svp)

x1 = as.matrix(data1[,c(19)])
y1 = as.matrix(data1[,c(18)])

opar <- par() 
par(mar=c(5.1, 4.1, 4.1, 7.5), xpd=FALSE)
plot(x1,y1, col=y+2, pch=y+2, xlab="f1: Name", ylab="f2: Cedula", main="SVM for data entries")
x2 = seq(from = 0, to = 1, by = 0.01)
y2 = -w[1]/w[2] * x2 - w[3]/w[2] * x2^2 - w[4]/w[2] * x2^3 + b/w[2] 
y3 = -w[1]/w[2] * x2 - w[3]/w[2] * x2^2 - w[4]/w[2] * x2^3 + (b+1)/w[2] 
y4 = -w[1]/w[2] * x2 - w[3]/w[2] * x2^2 - w[4]/w[2] * x2^3 + (b-1)/w[2] 
lines(x2,y2, col="blue")
lines(x2,y3,lty=2, col="blue")
lines(x2,y4,lty=2, col="blue")
legend("topright", inset=c(-0.2,0), legend=c("correct", "incorrect","svm divisor","svm margin"),
       col=c("green","red","blue","blue"), lty=c(NA,NA,1,2), pch=c(3,2,NA,NA), xpd=TRUE, cex=0.8)
par(opar)




print(w[1], digits = 20)
print(w[2], digits = 20)
print(w[3], digits = 20)
print(w[4], digits = 20)
print(b, digits = 20)

4.0064180710247541  #PNAMES2 
3.2831713463476326   #DCED
1.2412840802393599  #PNAMES22
0.20433702509432317  #PNAMES23
4.2828925951835375  #b






















