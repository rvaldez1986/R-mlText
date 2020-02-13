Sys.setlocale('LC_ALL','C') 
#================== Libraries ===============================================
library(data.table)
library(stringdist) #es la unica libreria impresidible
library(plyr)

#================== Funciones ==============================================
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

smartCompare = function(v1,v2,v3,v4){
  
  #v1 y v2 cedulas!!, v3 y v4 nombres!!
  
  k1 = stringsim(v3, v4, method = 'jaccard', q = 4)
  k2 = ifelse(v1 == v2, 1, 0)
  k3 = k1^2
  k4 = k1^3
  v1 = 4.0064180710247541  
  v2 = 3.2831713463476326 
  v3 = 1.2412840802393599 
  v4 = 0.20433702509432317
  b = 4.2828925951835375
  
  r1 = v1 * k1 + v2 * k2 + v3 * k3 + v4 * k4 - b
  return(r1)
  
}

smartCompare2 = function(v1,v2,v3,v4){
  
  #v1 y v2 nombres, v3 y v4 son fechas de nacimiento!!
  
  dfnac = ifelse(v3 == v4, 1,0) 
  ncv1 =  nchar(v1)
  ncv2 =  nchar(v2)
  cdif = ncv1 - ncv2
  j1 = stringsim(v1, v2, method = 'jaccard', q = 1)
  j2 = stringsim(v1, v2, method = 'jaccard', q = 2)
  j3 = stringsim(v1, v2, method = 'jaccard', q = 3)
  lcs = stringsim(v1, v2, method='lcs')
  lv = stringsim(v1, v2, method='lv')
  dl = stringsim(v1, v2, method='dl')
  cos = stringsim(v1, v2, method='cosine')
  
  f1 = ifelse(dfnac < 0.5, 
              ifelse(lcs < 0.9481481, 
                     ifelse(cdif < 5, -1, 0),
                     1),
              ifelse(j2 < 0.48125,-1,1))
  
  f2 = ifelse(j3 < 0.5166667, -1, 
              ifelse(dfnac < 0.5,
                     ifelse(cdif >= 0.5, -0.7005385, 0.3333333),
                     1)) 
  
  f3 = ifelse(dfnac < 0.5, 
              ifelse(lv < 0.944444, -1, 1),
              ifelse(j2 < 0.4654605, -1, 1))
  
  f4 = ifelse(dfnac < 0.5, 
              ifelse(cos < 0.9725061, -1,
                     ifelse(j2 < 0.7222222, -1, 1)),
              ifelse(j2 < 0.48125, -1, 1))
  
  f5 = ifelse(dfnac < 0.5, 
              ifelse(lv < 0.9444444, -1, 1),
              ifelse(j2 < 0.4857143, -1, 1))
  
  f6 = ifelse(dfnac < 0.5, 
              ifelse(cos < 0.9821023, -1, 1),
              ifelse(j2 < 0.48125, -1, 1))
  
  f7 =  ifelse(j3 < 0.5880399, 
               ifelse(ncv2 < 32, -1, 
                      ifelse(dfnac < 0.5, -1, 1)),
               ifelse(dfnac < 0.5,
                      ifelse(cos < 0.9666542, -1, 0.4659495),
                      1))
  
  f8 = ifelse(dfnac < 0.5,
              ifelse(j3 < 0.6835373, -1, 0.2737800),
              0.9585556)  
  
  f9 = ifelse(dfnac < 0.5,
              ifelse(lv < 0.9430199, -1, 1),
              ifelse(j2 < 0.4857143, -1, 1))
  
  f10 = ifelse(dfnac < 0.5,
               ifelse(lv < 0.9454094, -1, 1),
               ifelse(lv < 0.5753676, -1, 1))
  
  f11 = ifelse(dfnac < 0.5,
               ifelse(j2 < 0.8819444, -1, 1),
               ifelse(j2 < 0.5035714, -1, 1))
  
  f12 = ifelse(j3 < 0.5857843, -0.8832027, 0.7990412)
  
  f13 = ifelse(j3 < 0.5178571, -1,
               ifelse(cos < 0.9725061,
                      ifelse(dfnac < 0.5, -1, 1),
                      1))
  
  f14 = ifelse(j3 < 0.8082437,
               ifelse(cdif < 5.5,
                      ifelse(cdif >= -5.5, -0.9654504, 1),
                      ifelse(j1 < 0.7457983, -1, 1)),
               1)
  
  f15 = ifelse(dfnac < 0.5,
               ifelse(lcs < 0.9614815, -1, 1),
               ifelse(j1 < 0.6950464, -1, 1))
  
  f16 = ifelse(cos < 0.9712969,
               ifelse(dfnac < 0.5, -1, 
                      ifelse(ncv2 >= 28, -0.05654206, 1)),
               0.98167760)
  
  f17 = ifelse(dfnac < 0.5,
               ifelse(j2 < 0.9375, -1, 1),
               ifelse(j2 < 0.4699248, -1, 1))
  
  f18 = ifelse(dfnac < 0.5,
               ifelse(lcs < 0.9597424, -1, 1),
               ifelse(j2 < 0.48125, -1, 1))
  
  f19 = ifelse(cos< 0.9725061,
               ifelse(dfnac<  0.5, -1,
                      ifelse(j1 < 0.6735294, -1, 1)),
               1)
  
  f20 = ifelse(cos< 0.9725061,
               ifelse(cdif < 5.5, 
                      ifelse(cdif >= -5.5, -1, 0.2327424),
                      ifelse(dfnac < 0.5, -1, 1)),		
               ifelse(j1 < 0.8619048, -1,
                      ifelse(j2 < 0.6969697, -1, 1)))
  
  f21 = ifelse(dfnac < 0.5,
               ifelse(j2 < 0.8730159, -1, 1),
               ifelse(j2 < 0.48125, -1, 1))
  
  f22 = ifelse(dfnac < 0.5,
               ifelse(lv < 0.9454094, -1, 1),
               ifelse(j1 < 0.6764706, -1, 1))
  
  f23 = ifelse(dfnac < 0.5,
               ifelse(lv < 0.9430199, -1, 1),
               ifelse(j1 < 0.8229167, -1, 1))
  
  f24 = ifelse(j3< 0.5939922, -1,
               ifelse(j1 < 0.8619048,
                      ifelse(dfnac < 0.5, -1, 1),
                      ifelse(cos < 0.9471683, 0.07881826, 1)))
  
  f25 = ifelse(ncv2 < 13.5,
               ifelse(dfnac < 0.5, -1, 1),
               ifelse(lv < 0.7171429,
                      ifelse(dfnac < 0.5, -1, 0.5436138),
                      ifelse(j3 < 0.6364943, 0.6951905, 1)))
  
  f26 = ifelse(dfnac < 0.5,
               ifelse(lcs < 0.9614815, -1, 1),
               ifelse(j2 < 0.48125, -1, 1))
  
  f27 = ifelse(dfnac < 0.5,
               ifelse(lcs < 0.9614815, -1, 1),
               ifelse(j1 < 0.6950464, -1, 1))
  
  f28 = ifelse(ncv2 < 13.5,
               ifelse(dfnac < 0.5,
                      ifelse(j2 < 0.9375, -1, 1),
                      1),
               ifelse(j3 < 0.5939922, -1,
                      ifelse(cdif < -0.5, 0.146123, 0.9861035))) 
  
  f29 = ifelse(ncv2 < 13.5,
               ifelse(dfnac < 0.5,
                      ifelse(lcs < 0.98, -1, 1),
                      1),
               ifelse(lcs < 0.7163121, -1,
                      ifelse(cdif < -0.5, 0.03526626, 0.98198990)))
  
  f30 = ifelse(j1 < 0.8619048,
               ifelse(dfnac < 0.5, -1,
                      ifelse(j2 < 0.4351433, -1, 1)),
               ifelse(cdif < -1.5,
                      ifelse(dfnac < 0.5, -1, 1),
                      ifelse(j2< 0.6742424, -1, 0.9693185)))
  
  f31 = ifelse(ncv2 < 12.5,
               ifelse(dfnac < 0.5,
                      ifelse(lcs < 0.98, -1, 1),
                      1),
               ifelse(j1 < 0.8619048,
                      ifelse(dfnac < 0.5, -1, 0.5102496),
                      ifelse(cdif < -0.5, -0.441472, 1))) 
  
  alpha1 = c(0.25880749,0.24434127,0.26185314,0.23757642,0.23592994,0.19608824,
             0.22586810,0.19962323,0.18242071,0.15922029,0.13398742,0.13348871,
             0.17193126,0.15918270,0.13217831,0.14562266,0.11336393,0.10111113,
             0.12893863,0.14462536,0.09114153,0.09160992,0.07845571,0.10728246,
             0.14302943,0.08535800,0.07175277,0.14736267,0.14712959,0.11709520,
             0.24984695)
  
  r1 = f1 * alpha1[1] + f2 * alpha1[2] + f3 * alpha1[3] + f4 * alpha1[4] + f5 * alpha1[5] +
    f6 * alpha1[6] + f7 * alpha1[7] + f8 * alpha1[8] + f9 * alpha1[9] + f10 * alpha1[10] + f11 * alpha1[11] +
    f12 * alpha1[12] + f13 * alpha1[13] + f14 * alpha1[14] + f15 * alpha1[15] + f16 * alpha1[16] + f17 * alpha1[17] +
    f18 * alpha1[18] + f19 * alpha1[19] + f20 * alpha1[20] + f21 * alpha1[21] + f22 * alpha1[22] + f23 * alpha1[23] +
    f24 * alpha1[24] + f25 * alpha1[25] + f26 * alpha1[26] + f27 * alpha1[27] + f28 * alpha1[28] + f29 * alpha1[29] + 
    f30 * alpha1[30] + f31 * alpha1[31] 
  
  
  return(r1)
  
}

simpleCompare = function(v1,v2,v3,v4){
  
  #v1 y v2 nombres, v3 y v4 son fechas de nacimiento!!
  
  dname = ifelse(v1 == v2, 1, 0)
  dfnac = ifelse(v3 == v4, 1, 0)
  
  r1 = ifelse(dname + dfnac == 2, 1, -2)
  
  return(r1)
  
}

cedulaCompare = function(Nueva, Vieja){
  
  
  Vieja = Vieja[,ESTADO := "NA"]                         #CHEQ
  M = Vieja[-c(1:nrow(Vieja)), ]
  
  n1 = nrow(Nueva); c1 = ncol(Nueva)  #c1 = 11
  n2 = nrow(Vieja); c2 = ncol(Vieja)  #c2 = 14
  
  #Esto se debe actualizar si se eliminan o se incluyen columnas
  #colnames de nueva = N,CC,NOMBRE,SEXO,F_NAC,F_ING,F_DES,SUELDO_JUB,SUELDO_DES,TIPO,CEDULA (11)
  #colnames de vieja = N,CC,NOMBRE,SEXO,F_NAC,F_ING,F_DES,SUELDO_JUB,SUELDO_DES,TIPO,CEDULA,RESERVA_JUB,RESERVA_DES,ESTADO(14)
  #M, mismas columnas que vieja, esta data va a almacenar los comparados
  
  M2 = rbind(M, list(0, 0, "Entrada", "Entrada", as.Date(0, origin = "1900-01-01"),
                     as.Date(0, origin = "1900-01-01"), as.Date(0, origin = "1900-01-01"),
                     0, 0,0,"Entrada", 0, 0, "Entrada")) #Mismas columnas que vieja,esta se incluye si es entrada,sino se coloca el correspondiente de vieja          
  
  for(i in 1:n1){
    
    Nueva.Nombre = Nueva$NOMBRE[i]
    Nueva.Cedula = Nueva$CEDULA[i]
    
    v = vector()
    
    v = smartCompare(rep(Nueva.Cedula, n2), Vieja$CEDULA
                     ,rep(Nueva.Nombre, n2), Vieja$NOMBRE)
    
    m = max(v)
    p = ifelse(m > 0, which(v == m)[1], "Entrada")
    
    if(p != "Entrada"){
      
      M = rbind(M, Vieja[p,])
      
    } else{
      
      M = rbind(M,M2)
      
    }
    
    print(sprintf("Interation: %d of %d", i, n1)) 
    
  }
  
  #Esto se debe actualizar si se eliminan o se incluyen columnas
  #esto solo cambia para cuando se hace el cbind no tengan el mismo nombre las columnas
  
  M = rename(M, c("N"="N2", "CC" = "CC2", "NOMBRE"="NOMBRE2", "SEXO"="SEXO2",
                  "F_NAC"="F_NAC2", "F_ING" = "F_ING2", "F_DES" = "F_DES2", 
                  "SUELDO_JUB"="SUELDO_JUB2","SUELDO_DES"="SUELDO_DES2", "TIPO" = "TIPO2", "CEDULA"="CEDULA2"))
  
  COMP = cbind(Nueva, M) #tiene 25 columnas, 1:11 nueva, 12:25 viejas
  
  ACT_ANT = COMP[ NOMBRE2 != "Entrada" ]
  INGRESOS = COMP[ NOMBRE2 == "Entrada" ]
  #Esto se debe actualizar si se cambian columnas, COMP que tiene todas las columnas nos quedamos solo
  #con las correspondientes a NUEVA
  INGRESOS = INGRESOS[, !c(12:25), with=FALSE]         
  SALIDAS = Vieja[ !(N %in% ACT_ANT$N2) ]  #esta parte de la vieja, asi que no hay que quitar columnas
  
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
  
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
  
  newList <- list("ACT_ANT" = ACT_ANT2, "INGRESOS" = INGRESOS,
                  "SALIDAS" = SALIDAS)
  
}

noCedulaCompare = function(Nueva, Vieja, Type){
  
  Vieja = Vieja[,ESTADO := "NA"]
  M = Vieja[-c(1:nrow(Vieja)), ]
  
  n1 = nrow(Nueva); c1 = ncol(Nueva)
  n2 = nrow(Vieja); c2 = ncol(Vieja)
  
  #Esto se debe actualizar si se eliminan o se incluyen columnas
  #colnames de nueva = N,CC,NOMBRE,SEXO,F_NAC,F_ING,F_DES,SUELDO_JUB,SUELDO_DES,TIPO,CEDULA
  #colnames de vieja = N,CC,NOMBRE,SEXO,F_NAC,F_ING,F_DES,SUELDO_JUB,SUELDO_DES,TIPO,CEDULA,RESERVA_JUB,RESERVA_DES,ESTADO
  #M, mismas columnas que vieja, esta data va a almacenar los comparados
  
  M2 = rbind(M, list(0, 0, "Entrada", "Entrada", as.Date(0, origin = "1900-01-01"),
                     as.Date(0, origin = "1900-01-01"), as.Date(0, origin = "1900-01-01"),
                     0,0, 0,"Entrada", 0, 0, "Entrada"))
  
  for(i in 1:n1){
    
    Nueva.Nombre = Nueva$NOMBRE[i]
    Nueva.F_NAC = Nueva$F_NAC[i]
    
    v = vector()
    
    if(Type == 1){
      
      v = simpleCompare(rep(Nueva.Nombre,n2), Vieja$NOMBRE, rep(Nueva.F_NAC),
                        Vieja$F_NAC)
    } else {
      
      v = smartCompare2(rep(Nueva.Nombre,n2), Vieja$NOMBRE, rep(Nueva.F_NAC),
                        Vieja$F_NAC)
      
    }
    
    m1 = max(v)
    p1 = which(v == m1)[1]
    
    myCond = ifelse(m1 > 0, "True", ifelse(m1 >= -1.35, "Unsure", "Entrada"))
    
    if(myCond == "Entrada"){
      
      M = rbind(M,M2)
      
    } else if(myCond == "Unsure"){
      
      Vieja[p1,]$ESTADO = "Unsure"
      M = rbind(M, Vieja[p1,])
      
    } else{
      
      M = rbind(M, Vieja[p1,])
      
    }
    
    print(sprintf("Interation: %d of %d", i, n1)) 
  }
  
  #Esto se debe actualizar si se eliminan o se incluyen columnas
  #esto solo cambia para cuando se hace el cbind no tengan el mismo nombre las columnas
  
  M = rename(M, c("N"="N2", "CC" = "CC2", "NOMBRE"="NOMBRE2", "SEXO"="SEXO2",
                  "F_NAC"="F_NAC2", "F_ING" = "F_ING2", "F_DES" = "F_DES2", 
                  "SUELDO_JUB"="SUELDO_JUB2", "SUELDO_DES"="SUELDO_DES2", "TIPO" = "TIPO2", "CEDULA"="CEDULA2"))
  
  COMP = cbind(Nueva, M)   #tiene 25 columnas, 1:11 nueva, 12:24 viejas
  
  ACT_ANT = COMP[ NOMBRE2 != "Entrada" ]
  INGRESOS = COMP[ NOMBRE2 == "Entrada" ]
  #Esto se debe actualizar si se cambian columnas, COMP que tiene todas las columnas nos quedamos solo
  #con las correspondientes a NUEVA
  INGRESOS = INGRESOS[, !c(12:25), with=FALSE]
  SALIDAS = Vieja[ !(N %in% ACT_ANT$N2) ]
  
  COUNT = ACT_ANT[,.N,by = "NOMBRE2"]
  COUNT2 = COUNT[N > 1]
  NS = ACT_ANT[ NOMBRE2 %in% COUNT2$NOMBRE2 ]
  NS$ESTADO = "Unsure"
  
  ACT_ANT2 = rbind(ACT_ANT[ !(NOMBRE2 %in% COUNT2$NOMBRE2) ], NS)
  
  newList <- list("ACT_ANT" = ACT_ANT2, "INGRESOS" = INGRESOS,
                  "SALIDAS" = SALIDAS)
  
} 

post_compare = function(data2){
  
  #Corre si UNSURE tiene al menos 1 registro
  #UNSURE, tiene 25 columnas
  
  data2 = data2[order(-NOMBRE2, decreasing=TRUE),]
  group = data2[,.(COUNT = length(N)), by = "NOMBRE2"]
  data3 = merge(data2, group, by = "NOMBRE2")
  
  #data3 tiene 26 columnas, 25 anteriores + count, NOMBRE2 primera
  #la columna 1 se vuelve NOMBRE2, la base esta desarreglada
  #Aca hay que verificar si se cambian numero de columnas, con el sig. comando se pone a NOMBRE2 en su puesto
  
  #data3 = NOMBRE2,N,CC,NOMBRE,SEXO,F_NAC,F_ING,F_DES,SUELDO_JUB,SUELDO_DES,TIPO,CEDULA,N2,CC2,SEXO2,
  #F_NAC2,F_ING2,F_DES2,SUELDO_JUB2,SUELDO_DES2,TIPO2,CEDULA2,RESERVA_JUB,RESERVA_DES,ESTADO,COUNT
  
  setcolorder(data3, c( 2:14, 1, 15:26)) #14 porque 2 
  
  data3$PNAMESjac3r = stringsim(data2$NOMBRE, data2$NOMBRE2, method = 'jaccard', q = 3) 
  data3$PNAMESlcsr = stringsim(data2$NOMBRE, data2$NOMBRE2, method = 'lcs')
  
  data3.1 = subset(data3, data3$COUNT == 1)
  data3.2 = subset(data3, data3$COUNT == 2)
  data3.3 = subset(data3, data3$COUNT > 2)
  n3.1 = nrow(data3.1)
  n3.2 = nrow(data3.2)
  n3.3 = nrow(data3.3)
  
  #Modelo para count = 1
  if(n3.1 > 0){
    
    #3.1 tiene las mismas 25 columnas en su correcto orden + 1 que se llama count y 2 mas, jac3 y lcs
    #en total 28
    
    #se modifica la variable estado (no se incluye una nueva!!)
    data3.1$ESTADO = ifelse(data3.1$PNAMESlcsr > 0.93, "ACT_ANT", "SALIDA")
    
    #Aca hay que verificar el numero de columnas, se selecciona en este caso las 25 (no se incluye count ni las dos extras)
    data4 = data3.1[, 1:25, with = FALSE]
    
    ACT_ANT1 = subset(data4, data4$ESTADO == "ACT_ANT")
    INGRESOS1 = subset(data4, data4$ESTADO == "INGRESO")                       
    SALIDAS1 = subset(data4, data4$ESTADO == "SALIDA")
    UNSURE1 = subset(data4, data4$ESTADO == "Unsure")
    
  }
  
  #Modelo para count = 2
  if(n3.2 > 0){
    
    #3.2 tiene las mismas 25 columnas en su correcto orden + 1 que se llama count y 2 mas, jac3 y lcs
    #en total 28
    
    data3.2.2 = data3.2[,.(COUNT = length(N), j3rmax = max(PNAMESjac3r)), by = "NOMBRE2"]
    a1 = data3.2$NOMBRE[seq(1, n3.2, 2)]
    a2 = data3.2$NOMBRE[seq(2, n3.2, 2)]
    a3 = data3.2$F_NAC[seq(1, n3.2, 2)]
    a4 = data3.2$F_NAC[seq(2, n3.2, 2)]
    
    data3.2.2$jac3d = stringsim(a1, a2, method = 'jaccard', q = 3) 
    data3.2.2$DFNACd = ifelse(a3 == a4, 1, 0)
    data3.2.2$fitted = ifelse(data3.2.2$DFNACd == 0 & data3.2.2$jac3d < 1 & data3.2.2$j3rmax > 0.99
                              , "ACT_ANT", "Unsure")
    
    data4 = merge(data3, data3.2.2[, .(NOMBRE2, j3rmax, fitted)], by = "NOMBRE2")
    #la variable data4, NOMBRE2 esta como primera variable y tiene las 28 anteriores
    #mas j3rmax y fitted, en total 30
    
    #data4 = NOMBRE2 , N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES ,
    #TIPO , CEDULA , N2 , CC2 , SEXO2, F_NAC2 , F_ING2 , F_DES2 , SUELDO_JUB2 , SUELDO_DES2 , 
    #TIPO2 , CEDULA2 , RESERVA_JUB , RESERVA_DES , ESTADO , COUNT , PNAMESjac3r , PNAMESlcsr, j3rmax, fitted
    
    #Aca se debe modificar si se altera el numero de columnas
    #Les ponemos en orden, poniendo a NOMBRE2 en su puesto, y luego las adicionales 
    setcolorder(data4, c( 2:14, 1, 15:30))
    
    data4$ESTADO = ifelse(data4$fitted == "ACT_ANT",
                          ifelse(data4$j3rmax == data4$PNAMESjac3r, "ACT_ANT", "INGRESO"),
                          data4$fitted)
    
    #Aca se debe modificar si se altera el numero de columnas
    #se selecciona las primeras 25 columnas
    data5 = data4[, 1:25, with = FALSE]
    
    ACT_ANT2 = subset(data5, data5$ESTADO == "ACT_ANT")
    INGRESOS2 = subset(data5, data5$ESTADO == "INGRESO")  
    SALIDAS2 = subset(data5, data5$ESTADO == "SALIDA")
    UNSURE2 = subset(data5, data5$ESTADO == "Unsure")
    
  }
  
  #Estado para count > 2
  if(n3.3 > 0){
    
    #3.3 tiene las mismas 25 columnas en su correcto orden +1 que se llama count y 2 mas, jac3 y lcs
    #en total 28
    
    data3.3$ESTADO = "UNSURE"
    
    #Aca se debe modificar si se altera el numero de columnas
    #se selecciona las primeras 25 columnas
    data4 = data3.3[, 1:25, with = FALSE]
    
    ACT_ANT3 = subset(data4, data4$ESTADO == "ACT_ANT")
    INGRESOS3 = subset(data4, data4$ESTADO == "INGRESO")  
    SALIDAS3 = subset(data4, data4$ESTADO == "SALIDA")
    UNSURE3 = subset(data4, data4$ESTADO == "Unsure")
    
  }
  
  #Juntar 
  if(n3.1 > 0 & n3.2 > 0 & n3.3 > 0){
    
    ACT_ANTp = rbind(ACT_ANT1, ACT_ANT2, ACT_ANT3)
    INGRESOSp = rbind(INGRESOS1, INGRESOS2, INGRESOS3)
    SALIDASp = rbind(SALIDAS1, SALIDAS2, SALIDAS3)
    UNSUREp = rbind(UNSURE1, UNSURE2, UNSURE3)
    
  }else if(n3.1 > 0 & n3.2 > 0 & n3.3 == 0){
    
    ACT_ANTp = rbind(ACT_ANT1, ACT_ANT2)
    INGRESOSp = rbind(INGRESOS1, INGRESOS2)
    SALIDASp = rbind(SALIDAS1, SALIDAS2)
    UNSUREp = rbind(UNSURE1, UNSURE2)
    
  }else if(n3.1 > 0 & n3.2 == 0 & n3.3 == 0){
    
    ACT_ANTp = rbind(ACT_ANT1)
    INGRESOSp = rbind(INGRESOS1)
    SALIDASp = rbind(SALIDAS1)
    UNSUREp = rbind(UNSURE1)
    
  }else if(n3.1 == 0 & n3.2 > 0 & n3.3 == 0){
    
    ACT_ANTp = rbind(ACT_ANT2)
    INGRESOSp = rbind(INGRESOS2)
    SALIDASp = rbind(SALIDAS2)
    UNSUREp = rbind(UNSURE2)
    
  }else if(n3.1 == 0 & n3.2 > 0 & n3.3 > 0){
    
    ACT_ANTp = rbind(ACT_ANT2, ACT_ANT3)
    INGRESOSp = rbind(INGRESOS2, INGRESOS3)
    SALIDASp = rbind(SALIDAS2, SALIDAS3)
    UNSUREp = rbind(UNSURE2, UNSURE3)
    
  }else if(n3.1 == 0 & n3.2 == 0 & n3.3 > 0){
    
    ACT_ANTp = rbind(ACT_ANT3)
    INGRESOSp = rbind(INGRESOS3)
    SALIDASp = rbind(SALIDAS3)
    UNSUREp = rbind(UNSURE3)
    
  }
  
  #Arreglar
  ACT_ANTp$ESTADO = NA
  SALIDASp$ESTADO = NA
  
  #SALIDASp tiene 25 columnas y asi no esta en las otras instancias, hay que arreglar
  
  #SALIDASp = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES , SUELDO_JUB , SUELDO_DES ,
  #TIPO , CEDULA , N2 , CC2 , NOMBRE2 , SEXO2, F_NAC2 , F_ING2 , F_DES2 , SUELDO_JUB2 , SUELDO_DES2 , 
  #TIPO2 , CEDULA2 , RESERVA_JUB , RESERVA_DES , ESTADO 
  
  #colnames como deberia ser SALIDAS = N , CC , NOMBRE , SEXO , F_NAC , F_ING , F_DES,
  #SUELDO_JUB , SUELDO_DES , TIPO , CEDULA , RESERVA_JUB , RESERVA_DES , ESTADO
  
  #Aca hay que modificar si se alteran las columnas, elegimos las siguientes 
  SALIDAS = SALIDASp[, 12:25, with = FALSE]
  #cambiamos los nombres (para que no diga 2)
  setnames(SALIDAS, colnames(ACT_ANTp)[c(1:11, 23:25)] )
  
  #Aca hay que modificar si se alteran las columnas, elegimos las siguientes
  INGRESOSp1 = INGRESOSp[, 1:11, with = FALSE]
  INGRESOSp2 = SALIDASp[, 1:11, with = FALSE]
  
  newList = list("ACT_ANT" = ACT_ANTp, "INGRESOS" = rbind(INGRESOSp1,INGRESOSp2),
                 "SALIDAS" = SALIDAS, "UNSURE" = UNSUREp)
  
}





#######====================Funcion de comparacion(usa lo anterior)=======================================
#######==================================================================================================

fun_compare = function(data1, data2, write_dir, empresa, f_calc){
  
  F_Calc = as.Date(f_calc, format="%Y.%m.%d")
  if(is.na(F_Calc))
  {stop(sprintf("Error en fecha de calculo"))}
  
  setwd(write_dir)
  
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
  
  Vieja = Vieja[,ESTADO := "NA"]
  Vieja2 = Vieja2[,ESTADO := "NA"]
  
  n2.1 = nrow(Nueva)
  n2.2 = nrow(Nueva2)
  n1.1 = nrow(Vieja)
  n1.2 = nrow(Vieja2)
  n1 = n1.1 + n1.2
  n2 = n2.1 + n2.2
  
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
    INGRESOS = rbind(INGRESOS, UNS2$INGRESOS,fill=TRUE)
    SALIDAS = rbind(SALIDAS, UNS2$SALIDAS,fill=TRUE)
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
  
  write.csv(ACTIVOS_ANTIG, paste(empresa,"ACT_ANT.csv",sep="_"), row.names = F)
  write.csv(INGRESOS, paste(empresa,"INGRESOS.csv",sep="_"), row.names = F)
  write.csv(SALIDAS, paste(empresa,"SALIDAS.csv",sep="_"), row.names = F)
  write.csv(UNSURE, paste(empresa,"UNSURE.csv",sep="_"), row.names = F)
  
}

#============================== fin funciones =============================================



 

  
  