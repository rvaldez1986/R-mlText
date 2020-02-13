#function wrapper validate & compare

comparar=function(empresa, directorioEntrada, BaseAnterior, BaseActual, directorioSalida, f_calc){
  #empresa: Nombre de la empresa (String)
  #directorioEntrada: ubicacion bases
  #BaseAnterior, BaseActual: nombre de las bases de datos
  #directorioSalida: ubicacion donde se guardara resultados
  #f_calc: fecha de calculo AA.MM.DD (String)
  
	VAL1 = fun_validate_ant(BaseAnterior, directorioEntrada)
	VAL2 = fun_validate_act(BaseActual, directorioEntrada)
	fun_compare(VAL1$data1, VAL2$data2, directorioSalida, empresa, f_calc) #LLAMA VAL1 DE LA 1, VAL2 DE LA OTRA
}



