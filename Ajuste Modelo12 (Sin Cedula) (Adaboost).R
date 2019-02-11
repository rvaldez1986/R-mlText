#================== Libraries ===============================================
library(stringdist)
library("rpart")
library("ada")
library(corrplot)

#================== Funciones ==============================================
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

source("//server/Publica Consultoria/SCRIPTS/funciones esteban.R")
setwd("C:/Users/rvaldez/Desktop")
dir()

data1 = read.csv("Base Modelos (Sin Ced) (N6).csv", sep=",", stringsAsFactors=FALSE)

base.clases(data1)
data1 = depur1(data1)
data1$F_NAC = as.Date(data1$F_NAC, format="%d/%m/%Y")
data1$F_NAC2 = as.Date(data1$F_NAC2, format="%d/%m/%Y")
summary(data1$F_NAC); summary(data1$F_NAC2)

#================Variables=======================================================
data1$DFNAC = ifelse(data1$F_NAC == data1$F_NAC2, 1,0)   #2
data1$NCHAR1 =  nchar(data1$NOMBRE)
data1$NCHAR2 =  nchar(data1$NOMBRE2)
data1$CHARDIF = data1$NCHAR1 - data1$NCHAR2
data1$PNAMESjac1 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'jaccard', q = 1)
data1$PNAMESjac2 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'jaccard', q = 2)
data1$PNAMESjac3 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'jaccard', q = 3) #8
data1$PNAMESjac6 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'jaccard', q = 6)
data1$PNAMESq6 = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'qgram', q = 6)
data1$PNAMESlcs = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'lcs')
data1$PNAMESlv = stringsim(data1$NOMBRE, data1$NOMBRE2, method='lv')
data1$PNAMESdl = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'dl')
data1$PNAMEScos = stringsim(data1$NOMBRE, data1$NOMBRE2, method = 'cosine') #14

data2 = data1[,c(11:24)]
data2$LABEL = ifelse(data2$LABEL == 0, -1, data2$LABEL)


#================We use cross validation to choose depth of tree and model============================

#5 fold cross validation
res = matrix(,20,9)
set.seed(600)

mydepth1 = c(  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3)   #1 is stump, 2 is four
nmodels  = c( 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)  #number of models used
vars = list(c(1,2,4,6,12,3),c(1,2,4,6,12,5),c(1,2,4,6,12,7),c(1,2,4,6,12,8),c(1,2,4,6,12,9),
            c(1,2,4,6,12,10),c(1,2,4,6,12,11),c(1,2,4,6,12,13),c(1,2,4,6,12,14))            

for(i in 1:20){  
  
  set.seed(i+200*i)  
  
  error = vector()
  
  for(j in 1:9){
        
    data3 = data2[,vars[[j]]]
    nvar = ncol(data3)    
    
    n = 1:nrow(data3)
    g1 = sort(sample(n,floor(length(n)/5),replace=FALSE))
    g2 = sort(sample(n[-g1],floor(length(n)/5),replace=FALSE))
    g3 = sort(sample(n[-c(g1,g2)],floor(length(n)/5),replace=FALSE))
    g4 = sort(sample(n[-c(g1,g2,g3)],floor(length(n)/5),replace=FALSE))
    g5 = sort(sample(n[-c(g1,g2,g3,g4)],length(n) - 4*floor(length(n)/5) ,replace=FALSE))    
    
    train1 = data3[g1,]
    train2 = data3[g2,]
    train3 = data3[g3,]
    train4 = data3[g4,]
    train5 = data3[g5,]    
    
    control <- rpart.control(cp = -1 , maxdepth = mydepth1[j] , minsplit = 0)
    
    
    gen1 <- ada(LABEL~., data = rbind(train1, train2, train3, train4), 
                type = "gentle", control = control, iter = nmodels[j])
    predicted1 = predict(gen1, train5[,2:nvar])
    error1 = sum(ifelse(predicted1 == train5[,1],0,1))
    
    gen2 <- ada(LABEL~., data = rbind(train1, train2, train3, train5), 
                type = "gentle", control = control, iter = nmodels[j])
    predicted2 = predict(gen2, train4[,2:nvar])
    error2 = sum(ifelse(predicted2 == train4[,1],0,1))
    
    gen3 <- ada(LABEL~., data = rbind(train1, train2, train4, train5), 
                type = "gentle", control = control, iter = nmodels[j])
    predicted3 = predict(gen3, train3[,2:nvar])
    error3 = sum(ifelse(predicted3 == train3[,1],0,1))
    
    gen4 <- ada(LABEL~., data = rbind(train1, train3, train4, train5), 
                type = "gentle", control = control, iter = nmodels[j])
    predicted4 = predict(gen4, train2[,2:nvar])
    error4 = sum(ifelse(predicted4 == train2[,1],0,1))
    
    gen5 <- ada(LABEL~., data = rbind(train2, train3, train4, train5), 
                type = "gentle", control = control, iter = nmodels[j])
    predicted5 = predict(gen5, train1[,2:nvar])
    error5 = sum(ifelse(predicted5 == train1[,1],0,1))
    
    error[j] = error1 + error2 + error3 + error4 + error5
    
  }
  
  
  res[i,] = error
  print(sprintf("Interation: %d of %d", i, 20)) 
  
}

res
colMeans(res)

#25 da c(1,2,4,5,6,7,8,11,12,13,14) 10 variables, error pro = 2




#=====================Corr plot============================================================
data3 = data2[,c(2:13)]
M = cor(data3)
corrplot(M, method = "number")



#============================== Fit the tree  =============================================
data3 = data2[,c(1,2,4,5,6,7,8,11,12,13,14)]
control <- rpart.control(cp = -1 , maxdepth = 3 , minsplit = 0)
set.seed(600)
gen1 <- ada(LABEL~., data = data3, type = "gentle", control = control, iter = 31)
summary(gen1)
varplot(gen1)
plot(gen1)

gen1$model$trees
gen1$model$alpha

save(gen1, file = "my_model1.rda")

#====================== Construct the Model ==================================================

f1 = ifelse(data1$DFNAC < 0.5, 
            ifelse(data1$PNAMESlcs < 0.9481481, 
                   ifelse(data1$CHARDIF < 5, -1, 0),
                   1),
            ifelse(data1$PNAMESjac2 < 0.48125,-1,1))

f2 = ifelse(data1$PNAMESjac3 < 0.5166667, -1, 
            ifelse(data1$DFNAC < 0.5,
                   ifelse(data1$CHARDIF >= 0.5, -0.7005385, 0.3333333),
                   1)) 
            
f3 = ifelse(data1$DFNAC < 0.5, 
            ifelse(data1$PNAMESlv < 0.944444, -1, 1),
            ifelse(data1$PNAMESjac2 < 0.4654605, -1, 1))

f4 = ifelse(data1$DFNAC < 0.5, 
            ifelse(data1$PNAMEScos < 0.9725061, -1,
                   ifelse(data1$PNAMESjac2 < 0.7222222, -1, 1)),
            ifelse(data1$PNAMESjac2 < 0.48125, -1, 1))
              
f5 = ifelse(data1$DFNAC < 0.5, 
            ifelse(data1$PNAMESlv < 0.9444444, -1, 1),
            ifelse(data1$PNAMESjac2 < 0.4857143, -1, 1))

f6 = ifelse(data1$DFNAC < 0.5, 
            ifelse(data1$PNAMEScos < 0.9821023, -1, 1),
            ifelse(data1$PNAMESjac2 < 0.48125, -1, 1))

f7 =  ifelse(data1$PNAMESjac3 < 0.5880399, 
             ifelse(data1$NCHAR2 < 32, -1, 
                    ifelse(data1$DFNAC < 0.5, -1, 1)),
             ifelse(data1$DFNAC < 0.5,
                    ifelse(data1$PNAMEScos < 0.9666542, -1, 0.4659495),
                    1))

f8 = ifelse(data1$DFNAC < 0.5,
            ifelse(data1$PNAMESjac3 < 0.6835373, -1, 0.2737800),
            0.9585556)  

f9 = ifelse(data1$DFNAC < 0.5,
            ifelse(data1$PNAMESlv < 0.9430199, -1, 1),
            ifelse(data1$PNAMESjac2 < 0.4857143, -1, 1))

f10 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlv < 0.9454094, -1, 1),
             ifelse(data1$PNAMESlv < 0.5753676, -1, 1))

f11 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESjac2 < 0.8819444, -1, 1),
             ifelse(data1$PNAMESjac2 < 0.5035714, -1, 1))

f12 = ifelse(data1$PNAMESjac3 < 0.5857843, -0.8832027, 0.7990412)

f13 = ifelse(data1$PNAMESjac3 < 0.5178571, -1,
             ifelse(data1$PNAMEScos < 0.9725061,
                    ifelse(data1$DFNAC < 0.5, -1, 1),
                    1))

f14 = ifelse(data1$PNAMESjac3 < 0.8082437,
             ifelse(data1$CHARDIF < 5.5,
                    ifelse(data1$CHARDIF >= -5.5, -0.9654504, 1),
                    ifelse(data1$PNAMESjac1 < 0.7457983, -1, 1)),
             1)

f15 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlcs < 0.9614815, -1, 1),
             ifelse(data1$PNAMESjac1 < 0.6950464, -1, 1))

f16 = ifelse(data1$PNAMEScos < 0.9712969,
             ifelse(data1$DFNAC < 0.5, -1, 
                    ifelse(data1$NCHAR2 >= 28, -0.05654206, 1)),
             0.98167760)

f17 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESjac2 < 0.9375, -1, 1),
             ifelse(data1$PNAMESjac2 < 0.4699248, -1, 1))
             
f18 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlcs < 0.9597424, -1, 1),
             ifelse(data1$PNAMESjac2 < 0.48125, -1, 1))

f19 = ifelse(data1$PNAMEScos< 0.9725061,
             ifelse(data1$DFNAC<  0.5, -1,
                    ifelse(data1$PNAMESjac1 < 0.6735294, -1, 1)),
             1)

f20 = ifelse(data1$PNAMEScos< 0.9725061,
             ifelse(data1$CHARDIF < 5.5, 
                    ifelse(data1$CHARDIF >= -5.5, -1, 0.2327424),
                    ifelse(data1$DFNAC < 0.5, -1, 1)),		
             ifelse(data1$PNAMESjac1 < 0.8619048, -1,
                    ifelse(data1$PNAMESjac2 < 0.6969697, -1, 1)))

f21 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESjac2 < 0.8730159, -1, 1),
             ifelse(data1$PNAMESjac2 < 0.48125, -1, 1))

f22 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlv < 0.9454094, -1, 1),
             ifelse(data1$PNAMESjac1 < 0.6764706, -1, 1))

f23 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlv < 0.9430199, -1, 1),
             ifelse(data1$PNAMESjac1 < 0.8229167, -1, 1))

f24 = ifelse(data1$PNAMESjac3< 0.5939922, -1,
             ifelse(data1$PNAMESjac1 < 0.8619048,
                    ifelse(data1$DFNAC < 0.5, -1, 1),
                    ifelse(data1$PNAMEScos < 0.9471683, 0.07881826, 1)))

f25 = ifelse(data1$NCHAR2 < 13.5,
             ifelse(data1$DFNAC < 0.5, -1, 1),
             ifelse(data1$PNAMESlv < 0.7171429,
                    ifelse(data1$DFNAC < 0.5, -1, 0.5436138),
                    ifelse(data1$PNAMESjac3 < 0.6364943, 0.6951905, 1)))

f26 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlcs < 0.9614815, -1, 1),
             ifelse(data1$PNAMESjac2 < 0.48125, -1, 1))

f27 = ifelse(data1$DFNAC < 0.5,
             ifelse(data1$PNAMESlcs < 0.9614815, -1, 1),
             ifelse(data1$PNAMESjac1 < 0.6950464, -1, 1))

f28 = ifelse(data1$NCHAR2 < 13.5,
             ifelse(data1$DFNAC < 0.5,
                    ifelse(data1$PNAMESjac2 < 0.9375, -1, 1),
                    1),
             ifelse(data1$PNAMESjac3 < 0.5939922, -1,
                    ifelse(data1$CHARDIF < -0.5, 0.146123, 0.9861035))) 

f29 = ifelse(data1$NCHAR2 < 13.5,
             ifelse(data1$DFNAC < 0.5,
                    ifelse(data1$PNAMESlcs < 0.98, -1, 1),
                    1),
             ifelse(data1$PNAMESlcs < 0.7163121, -1,
                    ifelse(data1$CHARDIF < -0.5, 0.03526626, 0.98198990)))

f30 = ifelse(data1$PNAMESjac1 < 0.8619048,
             ifelse(data1$DFNAC < 0.5, -1,
                    ifelse(data1$PNAMESjac2 < 0.4351433, -1, 1)),
             ifelse(data1$CHARDIF < -1.5,
                    ifelse(data1$DFNAC < 0.5, -1, 1),
                    ifelse(data1$PNAMESjac2< 0.6742424, -1, 0.9693185)))


f31 = ifelse(data1$NCHAR2 < 12.5,
             ifelse(data1$DFNAC < 0.5,
                    ifelse(data1$PNAMESlcs < 0.98, -1, 1),
                    1),
             ifelse(data1$PNAMESjac1 < 0.8619048,
                    ifelse(data1$DFNAC < 0.5, -1, 0.5102496),
                    ifelse(data1$CHARDIF < -0.5, -0.441472, 1))) 

alpha1 = c(0.25880749,0.24434127,0.26185314,0.23757642,0.23592994,0.19608824,
           0.22586810,0.19962323,0.18242071,0.15922029,0.13398742,0.13348871,
           0.17193126,0.15918270,0.13217831,0.14562266,0.11336393,0.10111113,
           0.12893863,0.14462536,0.09114153,0.09160992,0.07845571,0.10728246,
           0.14302943,0.08535800,0.07175277,0.14736267,0.14712959,0.11709520,
           0.24984695)

data2$fitted = f1 * alpha1[1] + f2 * alpha1[2] + f3 * alpha1[3] + f4 * alpha1[4] + f5 * alpha1[5] +
  f6 * alpha1[6] + f7 * alpha1[7] + f8 * alpha1[8] + f9 * alpha1[9] + f10 * alpha1[10] + f11 * alpha1[11] +
  f12 * alpha1[12] + f13 * alpha1[13] + f14 * alpha1[14] + f15 * alpha1[15] + f16 * alpha1[16] + f17 * alpha1[17] +
  f18 * alpha1[18] + f19 * alpha1[19] + f20 * alpha1[20] + f21 * alpha1[21] + f22 * alpha1[22] + f23 * alpha1[23] +
  f24 * alpha1[24] + f25 * alpha1[25] + f26 * alpha1[26] + f27 * alpha1[27] + f28 * alpha1[28] + f29 * alpha1[29] + 
  f30 * alpha1[30] + f31 * alpha1[31] 

data2$predicted = sign(data2$fitted)
data2$error = data2$LABEL - data2$predicted
sum(data2$error) #0!!!!

data1$fitted = data2$fitted
data1$predicted = data2$predicted
data1$error = data2$error
write.csv(data1,"mmm.csv",row.names = F)






