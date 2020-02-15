#No enviar, para pruebas

#source functions (validate and compare) from files
source("C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_comp.R")
source("C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_val_act.R")
source("C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/rv_lib_val_ant.R")
source("C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/prog_calc/Compare.R")

#empresa: Nombre de la empresa (String)
#directorioEntrada: ubicacion bases
#BaseAnterior, BaseActual: nombre de las bases de datos
#directorioSalida: ubicacion donde se guardara resultados
#f_calc: fecha de calculo AA.MM.DD (String)

#comparar(empresa, directorioEntrada, BaseAnterior, BaseActual, directorioSalida, f_calc)



ptm <- proc.time()
comparar("TEVCOL", "C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/BD_pruebas/V2", 
         "BASE ANTERIOR TEVCOL.csv", "BASE ACTUAL TEVCOL.csv", 
         "C:/Users/rober/OneDrive/Documentos/ACTUARIA/act-remote/r_comparador/BD_pruebas/V4/res", "2015.12.31") 
proc.time() - ptm

