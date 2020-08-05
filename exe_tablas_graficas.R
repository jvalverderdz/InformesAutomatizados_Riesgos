#Este script ejecuta las funciones definidas en 'Generador_tablas_y_graficas.R'
#Para crear los archivos .png que se utilizarán en la presentación
#@Author: JavierValverde


#ESTAMOS REORIENTANDO NUESTRO ESFUERZO. AHORA, ESTE SCRIPT SE EJECUTARÁ AUTOMÁTICAMENTE AL INICIO DE LA PRESENTACIÓN,
#GENERANDO EN PNG TODAS LAS TABLAS Y LAS GRÁFICAS. LUEGO, DICHAS IMÁGENES SERÁN REFERENCIADAS EN EL SCRIPT 
#DE LA PRESENTACIÓN

#NOTA 2: HAY QUE REHACER LA NOTA TÉCNICA, PORQUE CAMBIAMOS LA OPERACIONALIDAD COMPLETA DE LOS ARCHIVOS DE LA PRESENTACIÓN.
setwd("/home/ecornejo/R")
source("Generador_tablas_y_graficas.R")

##AQUI DEFINIMOS EL PERIODO
per <- "201912"

#Cálculo del periodo t-1
per_1 <- as.character(as.numeric(as.character(per))-1)
if (substr(per,5,6) == "01") per_1 <- as.character(as.numeric(per_1) - 89)

#Crea las carpetas donde se almacenarán tablas y gráficas, si no existen
if (file.exists("tables")==FALSE) dir.create("tables")
if (file.exists("graphs")==FALSE) dir.create("graphs")

gen_table_acreditados_interesantes(per)
gen_table_cartera_global(per)
gen_table_exposicion(per)
gen_table_reserva(per)
gen_table_pi(per, grandes=TRUE)
gen_table_pi(per, grandes=FALSE)
gen_table_saldos_calificacion(per)
gen_table_saldos_tipo(per)
gen_table_saldos_tipo(per, vencida = FALSE)
gen_table_saldos_anexo(per)
gen_table_cartera_gerencia(per)
gen_table_cambio_riesgo(per, "Peor")
gen_table_cambio_riesgo(per, "Mejor")
gen_table_estadisticos_clientes(per)
gen_table_diferencias_exposicion(per)
gen_table_canal_distribucion(per)
gen_table_grupo(per)
gen_table_cartera_plazos(per)

#===================================================================>

gen_graph_resumen(per)
gen_graph_brechas_liquidez(per)
#gen_graph_porcentaje_reserva(per)
gen_graph_saldos_calificacion(per)
gen_graph_exposicion_por_riesgo(per)
gen_graph_exposicion_por_acreditado(per)
gen_graph_saldos_tipo(per)
gen_graph_saldos_tipo(per, vencidas = FALSE)
gen_graph_saldos_anexo(per)
gen_graph_ei_riesgo_categoria(per)
gen_graph_grupo(per, vencida=TRUE)
gen_graph_grupo(per, vencida=TRUE, per_1 = per_1)
gen_graph_grupo(per, vencida=FALSE)
gen_graph_cartera_plazos(per)
gen_graph_pi_calificacion_reservas(per)
gen_graph_pi_calificacion_reservas2(per)
