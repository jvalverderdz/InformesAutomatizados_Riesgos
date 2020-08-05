#------------------------------------------------------------------------------
#PRUEBA DE HOLT-WINTERS CON DATOS DE INSUMOS TESORERIA 201908

#Conexión con la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver   = "FreeTDS",
                 Server   = "172.17.5.80\\FFMMSSQL, 1433",
                 Database = "FIFOMI_RIESGOS",
                 UID      = "riesgos",
                 PWD      = "53Cre7@",
                 Port     = 1433)

#Obtenemos la informacion de la base de datos
BD_Tesoreria <- dbGetQuery(con, "SELECT * FROM RSG_INSUMOS_TESORERIA")

#Generacion de las series de tiempo
Disponible_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$Disponible)),
                    start = c(2015, 12), freq = 12)
Cobranza_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$COBRANZA)),
                  start = c(2015, 12), freq = 12)
Otros_Ingresos_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$Otros.Ingresos.....FINANCIAMIE)),
                        start = c(2015, 12), freq = 12)
Credito_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$CREDITO)),
                 start = c(2015, 12), freq = 12)
Capital_Mas_Interes_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$Capital...Intereses.Deuda)),
                             start = c(2015, 12), freq = 12)
Gastos_Operacion_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$Gastos.de.Oper..Y.Admon)),
                          start = c(2015, 12), freq = 12)
Fondeo_ts <- ts(as.numeric(gsub(",","",BD_Tesoreria$TOTAL)),
                start = c(2015, 12), freq = 12)

#Creación de una lista con todas las series de tiempo
time_series <- list("Disponible" = Disponible_ts,
                    "Cobranza" = Cobranza_ts,
                    "Otros_Ingresos" = Otros_Ingresos_ts,
                    "Credito" = Credito_ts,
                    "Capital_Mas_Interes" = Capital_Mas_Interes_ts,
                    "Gastos_Operacion" = Gastos_Operacion_ts,
                    "Fondeo" = Fondeo_ts)

#Cálculo de las estimaciones Holt-Winters de cada serie de la lista
hw_series <- list()
for(n in c(1:length(time_series))) {
  hw_series[n] <- HoltWinters(time_series[n])
}
names(hw_series) <- names(time_series)

#Cálculo de las predicciones con Holt-Winters de cada serie de la lista
hw_forecast <- list()
for (n in c(1:length(hw_series))){
  hw_forecast[n] <- predict(hw_series[n], n.ahead = 12, prediction.interval = T, level = 0.95)
}
names(hw_forecast) <- names(hw_series)



#===============================================================================

#Cálculo de las predicciones adaptativas para los primeros 7 meses de 2019

#Definicion de las variables auxiliares  
months <- c("Sep-18", "Oct-18", "Nov-18", "Dic-18", "Ene-19", "Feb-19",
           "Mar-19", "Abr-19", "May-19", "Jun-19", "Jul-19", "Ago-19")
periodos_prueba <- c(201812, 201901, 201902, 201903, 201904,
                     201905, 201906, 201907)

#Definicion de las variables resultadrarcao  
hw_estimacion_series <- list()
hw_forecast_series <- list()
hw_observadas_series <- list()

#Por cada serie en la lista de series de tiempo
for (j in 1:length(time_series)){
  hw_estimacion_t <- list()
  hw_forecast_t <- list()
  hw_observadas_t <- list()
  #Se va a calcular por cada periodo de prueba
  for (t in 1:8){
    
    N <- length(time_series[[j]])
    
    #Una serie incompleta, con los valores de la serie j hasta el periodo de prueba
    serie_incompleta <- ts(time_series[[j]][1:(N - 9 + t)],
                           start = c(2015, 12), freq = 12)
    
    #Cálculo de la estimación Holt-Winters
    hw_estimacion_t[[t]] <- HoltWinters(serie_incompleta)
    
    #Se rellena el pronóstico con el valor observado hasta un periodo antes del periodo prueba
    if (t == 1) hw_observadas_t[[t]] <- NULL
    else hw_observadas_t[[t]] <- window(time_series[[j]], start = c(2019, 1), end = c(2019, t-1))
    #hw_observadas_t[[t]] <- ts(time_series[[j]][(N - 10):(N - 7 + t)], start = c(2018, 08), freq = 12)
    observadas_len <- length(hw_forecast_t[t])
    
    #Se rellena el pronóstico con los valores pronosticados desde el periodo de prueba
    hw_forecast_t[[t]] <- predict(hw_estimacion_t[[t]], n.ahead = (13-t), prediction.interval = T, level = 0.95)[,1]
    hw_estimacion_t[[t]] <- hw_estimacion_t[[t]]$fitted[,1]
    
    #hw_forecast_t[t][["Periodo_Prueba"]] <- periodos_prueba[t]
    
    #if (names(time_series[j]) %in% c("Disponible", "Cobranza", "Otros_Ingresos", "Fondeo")){
    #  hw_forecast_t[t][["Tipo"]] <- "Ingreso" 
    #}
    #else{
    #  hw_forecast_t[t][["Tipo"]] <- "Egreso"
    #}
    
  }
  names(hw_estimacion_t) <- periodos_prueba
  names(hw_forecast_t) <- periodos_prueba
  names(hw_observadas_t) <- periodos_prueba
  
  #se rellena la serie j-ésima con la lista de sus estimaciones y pronósticos
  hw_estimacion_series[[j]] <- hw_estimacion_t
  hw_forecast_series[[j]] <- hw_forecast_t
  hw_observadas_series[[j]] <- hw_observadas_t
}
names(hw_observadas_series) <- names(time_series)
names(hw_estimacion_series) <- names(time_series)
names(hw_forecast_series) <- names(time_series)




#=====================================================================
#CREACIÓN DE LA TABLA AGRUPADORA DE TODOS LOS PRONÓSTICOS

col_names <- c("ID","Periodo_Prueba", "Cuenta", "Tipo",
               "Mes_1", "Mes_2", "Mes_3", "Mes_4", "Mes_5", "Mes_6",
               "Mes_7", "Mes_8", "Mes_9", "Mes_10", "Mes_11", "Mes_12")

data_matrix <- matrix(ncol = length(col_names), nrow = 0)
colnames(data_matrix) <- col_names


for (t in c(1:length(periodos_prueba))){
  for (j in c(1:length(hw_forecast_series))){
    
    periodo_prueba <- as.numeric(names(hw_forecast_series[[j]][t]))
    nombre_cuenta <- names(hw_forecast_series[j])
    
    if (nombre_cuenta == "Disponible"){
      codigo_cuenta <- 11
    } else if (nombre_cuenta == "Cobranza"){
      codigo_cuenta <- 12
    } else if (nombre_cuenta == "Fondeo"){
      codigo_cuenta <- 13
    } else if (nombre_cuenta == "Credito"){
      codigo_cuenta <- 21
    } else if (nombre_cuenta == "Capital_Mas_Interes"){
      codigo_cuenta <- 22
    } else if (nombre_cuenta == "Gastos_Operacion"){
      codigo_cuenta <- 23
    }
    
    id <- as.numeric(paste(as.character(periodo_prueba),
                           as.character(codigo_cuenta), sep = ''))
    #Asignar tipo de cuenta
    if (nombre_cuenta %in% c("Disponible", "Cobranza", "Otros_Ingresos", "Fondeo")){
        tipo_cuenta <- "Ingreso"
      } else{
        tipo_cuenta <- "Egreso"
      }
    #Asignar los valores de los 12 meses
    months_value <- list()
    if (t == 1){
      for (n in c(1:length(hw_forecast_series[[j]][[t]]))){
        months_value[[n]] <- hw_forecast_series[[j]][[t]][[n]]
      }
    }
    else {
      observadas_forecast <- ts(c(hw_observadas_series[[j]][[t]], hw_forecast_series[[j]][[t]]),
                                start = start(hw_observadas_series[[j]][[t]]),
                                freq = frequency(hw_observadas_series[[j]][[t]]))
      for (n in c(1:length(observadas_forecast))) months_value[[n]] <- observadas_forecast[[n]]
    }
    
    #new_row <- c(c(id, periodo_prueba, codigo_cuenta, tipo_cuenta), months_value)
    new_row <- c(id, periodo_prueba, codigo_cuenta, tipo_cuenta,
                 months_value[[1]], months_value[[2]], months_value[[3]],
                 months_value[[4]], months_value[[5]], months_value[[6]],
                 months_value[[7]], months_value[[8]], months_value[[9]],
                 months_value[[10]], months_value[[11]],months_value[[12]])
    #Asignar las variables previas a un nuevo renglón de la matriz
    data_matrix <- rbind(data_matrix, new_row)
  }
}
rownames(data_matrix) <- NULL
colnames(data_matrix) <- col_names

#Asignar la matriz a un dataframe
hw_df <- as.data.frame(data_matrix)

#Exportar el dataframe a un csv para comprobar que todo esté bien
write.csv(hw_df, "HW_Adaptativo_Tesoreria.csv", row.names = FALSE)

#Una vez que todo esté en orden, exportar el dataframe a una tabla de SQL
dbGetQuery(con, "SET QUOTED_IDENTIFIER ON")
query <- dbWriteTable(con, "RSG_HW_ADAPTATIVO_TESORERIA", hw_df, row.names = NA, append = TRUE)

#Checamos que todo este ok
dbExistsTable(con, "RSG_HW_ADAPTATIVO_TESORERIA")


