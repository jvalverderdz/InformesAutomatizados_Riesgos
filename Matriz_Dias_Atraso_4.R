#ACTUALIZACIÓN DE MATRIZ DE DÍAS DE ATRASO ANUAL
#VERSIÓN 4: PRUEBA CON OPERACIONES VECTORIZADAS EN LUGAR DE Y CICLOS PARA CADA CELDA DE LA MATRIZ
#@Author: JavierValverde

library(BBmisc)
library(beepr)

time1 <- Sys.time()
#Conexión con la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver   = "FreeTDS",
                 Server   = "172.17.5.80\\FFMMSSQL, 1433",
                 Database = "FIFOMI_RIESGOS",
                 UID      = "riesgos",
                 PWD      = "53Cre7@",
                 Port     = 1433)

#Obtenemos la información de pagos
BD_Conciliacion <- dbGetQuery(con, "SELECT * FROM RSG_CONCILIACION")
#Generamos un dataframe con la información que nos importa
DF_Conciliacion <- data.frame("Contrato" = BD_Conciliacion$No_Contrato,
                              "Periodo" = BD_Conciliacion$Periodo,
                              "Dias_Atraso" = BD_Conciliacion$Días_Atraso)
rm(BD_Conciliacion)

#Cálculo de la calificación según pagos (con un case)
calificacion <- case_when(
  DF_Conciliacion$Dias_Atraso == 0 ~ 2,
  DF_Conciliacion$Dias_Atraso > 0 & DF_Conciliacion$Dias_Atraso <= 30 ~ 3,
  DF_Conciliacion$Dias_Atraso > 30 & DF_Conciliacion$Dias_Atraso <= 60 ~ 4,
  DF_Conciliacion$Dias_Atraso > 60 & DF_Conciliacion$Dias_Atraso <= 90 ~ 5,
  DF_Conciliacion$Dias_Atraso > 90 & DF_Conciliacion$Dias_Atraso <= 180 ~ 6,
  DF_Conciliacion$Dias_Atraso > 180 & DF_Conciliacion$Dias_Atraso <= 360 ~ 7,
  DF_Conciliacion$Dias_Atraso > 360 ~ 8
)

#Agregamos la calificación al Data Frame
DF_Conciliacion <- cbind(DF_Conciliacion, "Calificacion" = calificacion)

#Cálculo de las matrices de frecuencias

#Variable resultado: Una lista de matrices
matrices <- c()

#Variables auxiliares
periodos <- unique(DF_Conciliacion$Periodo)
contratos <- unique(DF_Conciliacion$Contrato)
n_por_periodo <- c()

year <- 2015

bar_t <- makeProgressBar(max=12, label="Estimando matriz de Transición...")
#Ciclo principal: Una matriz por periodo
for (t in 1:12){
  bar_t$inc(1)
  #Extracto de la información del periodo t y t-1 (empezando t en 2)
  periodo_t <- as.numeric(paste(year, "0", as.character(t), sep=''))
  if (t >=10) periodo_t <- as.numeric(paste(year, as.character(t), sep=''))
  periodo_anterior <- periodo_t - 1
  if (t == 1) periodo_anterior = as.numeric(paste((year-1),"12", sep=''))
  
  DF_Conciliacion_t <- DF_Conciliacion %>% filter(Periodo == periodo_t)
  DF_Conciliacion_anterior <- DF_Conciliacion %>% filter(Periodo == periodo_anterior)
  
  #Funcion para determinar la calificacion anterior para cada contrato del periodo t
  bar_cal_anterior <- makeProgressBar(max=nrow(DF_Conciliacion_t), label=paste("Estimando matriz del periodo",t))
  funcion_calificacion_anterior <- function(contrato){
    bar_cal_anterior$inc(1)
    cal_anterior <- (DF_Conciliacion_anterior %>% filter(Contrato == contrato))$Calificacion
    if (is.null(nrow(cal_anterior))) 1
    else cal_anterior
  }
  
  #Aplicación de la función de calificación anterior para cada contrato y se agrega al
  #Data Frame del periodo t, para tener todo en un mismo Data Frame
  calificacion_anterior <- sapply(DF_Conciliacion_t$Contrato, funcion_calificacion_anterior)
  DF_Conciliacion_t <- cbind(DF_Conciliacion_t, "Calificacion_anterior" = calificacion_anterior)
  
  #Se genera la matriz de frecuencias a rellenar
  matriz_frecuencias <- matrix(0, nrow = 8, ncol = 8)
  
  #Y por cada renglón y columna de la matriz
  for (r in 1:nrow(matriz_frecuencias)){
    for (c in 1:ncol(matriz_frecuencias)){
      #El valor de la celda r,c es la suma de observaciones de la calificación r en t y calificción c en t-1
      frecuencia_r_c <- sum(DF_Conciliacion_t$Calificacion_anterior == r & DF_Conciliacion_t$Calificacion == c)
      matriz_frecuencias[r,c] <- frecuencia_r_c
    }
  }
  
  for (r in 1:nrow(matriz_frecuencias)){
    frecuencia_missing <- sum((DF_Conciliacion_anterior %>% filter(Calificacion == r))$Contrato %in% DF_Conciliacion_t$Contrato == FALSE)
    matriz_frecuencias[r,1] <- frecuencia_missing
  }
  
  #En cada fila de la matriz de frecuencias se adiciona la observación de la columna 1 (sin información) a la de la diagonal
  for(row in 1:nrow(matriz_frecuencias)){
    matriz_frecuencias[row,row] <- matriz_frecuencias[[row,row]] + matriz_frecuencias[[row,1]]
  }
  
  #Se eliminan la fila y la columna 1 (sin información)
  matriz_frecuencias <- matriz_frecuencias[-1, -1]
  
  #Se calcula la columna del total de las filas
  col_sumas <- rowSums(matriz_frecuencias)
  
  #El n del periodo t es la suma de la columna de suma de filas
  n_t <- sum(col_sumas)
  
  #Se crea la matriz resultado para el periodo t
  matriz_transicion <- matrix(0, ncol = 7, nrow = 7)
  
  #Para cada fila, los valores se establecen como cada observación (frecuencia) dividida por la suma de la fila (el n-fila)
  col_suma_prob <- c()
  for(row in 1:nrow(matriz_frecuencias)){
    matriz_transicion[row,] <- matriz_frecuencias[row,]/col_sumas[[row]]
  }
  col_suma_prob <- rowSums(matriz_transicion)
  #Se adiciona la matriz resultado para el periodo t a la lista de matrices
  matrices[[t]] <- matriz_transicion

}

#=======================================================================================
#HASTA AQUI JALA TODO BIEN. EL LOOP PARECE QUE FUNCIONA, PERO NO SE HA COMPROBADO CON TODOS LOS DATOS
#EL VALOR ESTIMADO DE CÁLCULO PARA UN AÑO ES DE 70 MINUTOS.
#HAY QUE CORRER EL CÓDIGO COMPLETO HASTA AQUI PARA VER SI LAS MATRICES QUE SALEN TIENEN SENTIDO


#Se establece la matriz final como la multiplicación de las primeras 2 matrices
matriz_final <- matrices[[1]] %*% matrices[[2]]

#Y se realiza iterativamente el resto de las multiplicaciones
for(n in 3:length(matrices)){
  matriz_final <- matriz_final %*% matrices[[n]]
}

#Ya tenemos nuestra matriz final
matriz_final
beep()
time2 <- Sys.time
#Cálculo de lista de mejoramiento, mantenimiento o empeoramiento
time2-time1