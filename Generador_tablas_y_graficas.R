#Script con las funciones generadoras de tablas y gráficas para la presentación del Comité de Riesgos
#@Author: JavierValverde
library(knitr)
library(odbc)
library(DBI)
library(expss)
library(dplyr)
library(formattable)
library(ggplot2)
library(ggrepel)
library(reshape)
library(htmltools)
library(webshot)

#Opciones del sistema
options(digits=2)
options(scipen=99)
setwd("/home/ecornejo/R")

#Conexión con la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver   = "FreeTDS",
                 Server   = "172.17.5.80\\FFMMSSQL, 1433",
                 Database = "FIFOMI_RIESGOS",
                 UID      = "riesgos",
                 PWD      = "53Cre7@",
                 Port     = 1433)


#Definición de las listas de variables auxiliares
months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
calificaciones_list <- c("A-1", "A-2", "B-1", "B-2", "B-3", "C-1", "C-2", "D", "E")
nombres_plazos <- c("1-90 días", "91-180 días", "181-360 días", "1-3 años", "3-5 años", "5-7 años", "+ 7 años")
good_cals <- c("A-1", "A-2", "B-1", "B-2")
medium_cals <- c("B-2", "B-3")
bad_cals <- c("C-1", "C-2", "D", "E")



#Obtenemos la información de las tablas necesarias con todas las observaciones
#De estos dataframes vamos a obtener toda la información necesaria
Query_Conciliacion <- dbGetQuery(con, "SELECT * FROM RSG_CONCILIACION_PRUEBAV2")
Query_Reservas <- dbGetQuery(con, "SELECT * FROM RSG_RESERVA_PRUEBA")
Query_Reservas_Old <- dbGetQuery(con, "SELECT * FROM RSG_RESERVA")
names(Query_Reservas) <- names(Query_Reservas_Old)
Query_Tesoreria <- dbGetQuery(con, "SELECT * FROM RSG_INSUMOS_TESORERIA")

Query_Nombres <- dbGetQuery(con, "SELECT * FROM RSG_NOM_CORTOS_LARGOS")
Query_Nombres$NOMBRE_LARGO <- gsub(".", "", Query_Nombres$NOMBRE_LARGO, fixed = TRUE)
Query_Nombres$NOMBRE_LARGO <- gsub(",", "", Query_Nombres$NOMBRE_LARGO, fixed = TRUE)
Query_Nombres$NOMBRE_LARGO <- gsub(" SA\\w* DE \\w+", "", Query_Nombres$NOMBRE_LARGO)
Query_Nombres$NOMBRE_LARGO <- gsub(" SOFOM ENR| SOFOM ER", "", Query_Nombres$NOMBRE_LARGO)

#Funcion para exportar tablas de formattabla en formato .png
export_formattable <- function(f, name, width = "100%", height = NULL, background = "white", delay = 0.2){
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = name,
          selector = ".formattable_widget",
          delay = delay)
}

#Función para mostrar los nombres cortos de los acreditados
#función complementaria que acorta un solo nombre. Toma como input un nombre largo y devuelve un nombre corto
acortador <- function(nombre){
  nombre <- gsub(".", "", nombre, fixed = TRUE)
  nombre <- gsub(",", "", nombre, fixed = TRUE)
  nombre <- gsub(" SOFOM ENR| SOFOM ER", "", nombre)
  nombre <- gsub(" SA\\w* DE \\w+", "", nombre)
  nom_corto <- Query_Nombres$NOMBRE_CORTO[Query_Nombres$NOMBRE_LARGO == nombre]
  #Si no encontró ningún valor para el nombre largo en la base, devuelve el nombre largo en minúsculas
  if(length(nom_corto)==0) nom_corto <- tools::toTitleCase(tolower(nombre))
  nom_corto
}
#Toma como input una lista de nombres largos y devuelve una lista de nombres cortos
nombre_corto <- function(nombres_list){
  #Aplica la función complementaria a todos los elementos de la lista de nombres con un sapply
  nombres_output <- sapply(nombres_list, acortador)
  #El sapply devuelve una lista con nombres. Los eliminamos:
  names(nombres_output) <- NULL
  
  nombres_output
}
#Genera el estilo para colorear las columnas de calificación de las formattable
grado_colorear <- function(calif)  formatter("span", style = ~ style(color = ifelse(calif %in% good_cals, "green", ifelse(calif %in% medium_cals, "gold3", "red"))))

#FUNCIONES PARA GENERAR LAS TABLAS

gen_table_acreditados_interesantes <- function(per){
  #Obtener el subconjunto que nos interesa del dataframe principal (fecha, anexo y estatus deseados)
  interesantes <- Query_Reservas %>% filter(Anexo == "ANEXO 31" & Estatus =="Vigente" & Periodo == as.character(per))
  #Quedarnos solo con las variables que nos interesan
  interesantes <- interesantes[,c("IC_Principal","Nombre_RazonSocial_ICPrincipal", "PI", "EI",
                                  "Reserva", "Porcentaje_Reservas","Calificacion")]
  
  #Agregar las variables por ID del acreditado
  agg_interesantes <- interesantes %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              PI = mean(as.numeric(PI)),
              EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              Porcentaje_Reservas = sum(as.numeric(Reserva))/sum(as.numeric(EI)),
              Calificacion = first(Calificacion))
    
  #Crear un dataframe con los datos agregados, y en unidades de millones o por cientos
  df_interesantes <- data.frame("Acreditado" = nombre_corto(agg_interesantes$Nombre),
                                "Probabilidad Incumplimiento" = paste(round(agg_interesantes$PI*100, digits=2), "%", sep=""),
                                "Exposición" = agg_interesantes$EI/1000000,
                                "Reserva" = round(agg_interesantes$Reserva/1000000, digits=2),
                                "Porcentaje Reservas" = paste(round(agg_interesantes$Porcentaje_Reservas*100, digits=2), "%", sep=""),
                                "Calificación" = agg_interesantes$Calificacion,
                                stringsAsFactors = FALSE, check.names = FALSE) #StringsAsFactors = FALSE, de lo contrario, puede tomar las variables character y convertirlas en factor
  
  ft_interesantes <- formattable(df_interesantes,
                                 align = c("r", "c", "c", "c", "c", "c"),
                                 list(`Calificación`= formatter(
                                   "span", style = ~ style(color = ifelse(
                                     Calificación %in% good_cals, "green", ifelse(Calificación %in% medium_cals,"gold3", "red")),
                                    `border-radius` = "8px"))))
  
  
  export_formattable(ft_interesantes, "tables/acreditados_interesantes.png", width="1000px")
  
}

gen_table_cartera_global <- function(per){
  #Definimos el periodo t (per_0), t-1 (per_1) y t-12 (per_12)
  per_0 <- as.character(per)
  per_1 <- as.character(as.numeric(as.character(per))-1)
  if (substr(per_0,5,6) == "01") per_1 <- as.character(as.numeric(per_1) - 89)
  per_12 <- as.character(as.numeric(as.character(per))-100)
  
  #Definimos los meses y años para t, t-1 y t-12 en número y texto
  month_0 <- as.numeric(substr(per_0,5,6))
  month_1 <- as.numeric(substr(per_1,5,6))
  month_12 <- as.numeric(substr(per_12,5,6))
  year_0 <- (substr(per_0,1,4))
  year_1 <- (substr(per_1,1,4))
  year_12 <- (substr(per_12,1,4))
  
  per_12_text <- paste(months[month_12],year_12, sep="-")
  per_1_text <- paste(months[month_1],year_1, sep="-")
  per_0_text <- paste(months[month_0],year_0, sep="-")
  
  #Repetimos el proceso de la función anterior para los periodos, condiciones y variables deseados
  #para t, t-1 y t-12
  agregados_0 <- Query_Reservas %>% filter(Estatus =="Vigente" & Periodo == as.character(per_0))
  agregados_1 <- Query_Reservas %>% filter(Estatus =="Vigente" & Periodo == as.character(per_1))
  agregados_12 <- Query_Reservas %>% filter(Estatus =="Vigente" & Periodo == as.character(per_12))
  

  exposicion <- c(sum(agregados_12$EI), sum(agregados_1$EI), sum(agregados_0$EI))
  reservas <- c(sum(agregados_12$Reserva), sum(agregados_1$Reserva), sum(agregados_0$Reserva))
  porcentaje_reservas <- (reservas/exposicion)*100
  
  cartera_df <- data.frame("Exposicion al incumplimiento" = round(exposicion/1000000, digits=2),
                           "Reservas" = round(reservas/1000000, digits=2),
                           "Porcentaje de reserva" = round(porcentaje_reservas, digits=2))
  rownames(cartera_df) <- c(per_12_text, per_1_text, per_0_text)
  
  
  cartera_df_t <- as.data.frame(t(cartera_df))
  
  variacion <- round(((cartera_df_t[,3]/cartera_df_t[,2])-1)*100, digits=2)
  variacion[[3]] <- paste(as.character(round(variacion[[3]]*10, digits=2)),"PB", sep=" ")
  
  cartera_df_t["Variación anual"] <- variacion
  
  #cartera_df_t
  row.names(cartera_df_t) <- c("Exposición al incumplimiento (EI)",
                               "Reservas",
                               "Porcentaje de Reservas")
  row.names(cartera_df_t) <- gsub(".", " ", row.names(cartera_df_t), fixed = TRUE)
  ft_cartera <- formattable(cartera_df_t, align = c("c", "c", "c", "c", "c"))
  
  export_formattable(ft_cartera, "tables/cartera_global.png", width="800px")
  
}

gen_table_exposicion <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  
  bd_exposicion <- BD_Reservas[c("IC_Principal", "Estatus", "Nombre_RazonSocial_ICPrincipal", "EI",
                                  "Reserva", "PI", "Calificacion")]
  
  agg_exposicion <- bd_exposicion %>% group_by(IC_Principal) %>%
    summarize(Nombre_RazonSocial_ICPrincipal = first(Nombre_RazonSocial_ICPrincipal),
              Estatus = first(Estatus),
              EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              PI = mean(as.numeric(PI)),
              Calificacion = first(Calificacion))
  
  agg_exposicion_todos <- agg_exposicion
  agg_exposicion <- as.data.frame(agg_exposicion) %>% filter(Estatus == "Vigente")
  
  
  agg_top_exposicion <- as.data.frame(top_n(agg_exposicion,10,EI))
  agg_top_exposicion <- sort_desc(agg_top_exposicion, EI)
  row.names(agg_top_exposicion) <- c(1:nrow(agg_top_exposicion))
  
  total_exposicion <- sum(BD_Reservas$EI)
  exposicion_acum <- c(agg_top_exposicion$EI[[1]]/total_exposicion)
  #Ciclo for para calcular la exposición acumulada para cada renglón
  for (n in 2:nrow(agg_top_exposicion)) exposicion_acum[n] <- exposicion_acum[[n-1]] + agg_top_exposicion$EI[[n]]/total_exposicion
  
  agg_top_exposicion <- cbind(agg_top_exposicion, "Exposicion_acum" = exposicion_acum)
  
  top_exposicion_df <- data.frame("No." = c(1:nrow(agg_top_exposicion)),
                                  "Intermediario" = nombre_corto(agg_top_exposicion$Nombre_RazonSocial_ICPrincipal),
                                  "Exposicion" = round(agg_top_exposicion$EI/1000000, digits=2),
                                  "Reserva" = round(agg_top_exposicion$Reserva/1000000, digits=2),
                                  "Exposicion Acum." = as.character(paste(round(agg_top_exposicion$Exposicion_acum*100, digits=2),"%",sep="")),
                                  "Probabilidad Incumplimiento" = as.character(paste(round(agg_top_exposicion$PI*100, digits=2),"%",sep="")),
                                  "Grado de Riesgo" = as.character(agg_top_exposicion$Calificacion), stringsAsFactors = FALSE)
  
  top_exposicion_df <- rbind(top_exposicion_df,
                             c("No." = "",
                               "Intermediario" = "TOTAL 10 PARTICIPANTES",
                             "Exposicion" = sum(top_exposicion_df$Exposicion),
                             "Reserva" = sum(top_exposicion_df$Reserva),
                             "Exposicion Acum." = top_exposicion_df$Exposicion.Acum.[[nrow(agg_top_exposicion)]],
                             "Probabilidad Incumplimiento" = paste(round(weighted.mean(agg_top_exposicion$PI, agg_top_exposicion$EI)*100, digits=2),"%",sep=""),
                             "Grado de Riesgo" = ""))
  
  top_exposicion_df <- rbind(top_exposicion_df,
                             c("No." = "",
                               "Intermediario" = paste("TOTAL (",nrow(agg_exposicion_todos),")",sep=""),
                             "Exposicion" = round(sum(BD_Reservas$EI)/1000000, digits=2),
                             "Reserva" = round(sum(BD_Reservas$Reserva)/1000000, digits=2),
                             "Exposicion Acum." = "100%",
                             "Probabilidad Incumplimiento" = paste(round(weighted.mean(as.numeric(BD_Reservas$PI),as.numeric(BD_Reservas$EI))*100, digits=2),"%",sep=""),
                             "Grado de Riesgo" = ""))
  
  names(top_exposicion_df) <- c("No", "Intermediario", "EI", "Reserva", "EI Acum", "PI", "Grado de Riesgo")
  names(top_exposicion_df) <- gsub(".", " ", names(top_exposicion_df), fixed = TRUE)
  #formattable(top_exposicion_df)
  
  ft_exposicion <- formattable(top_exposicion_df,
                                 align = c("c", "r", "c", "c", "c", "c", "c"),
                                 list(`Grado de Riesgo`= formatter(
                                   "span", style = ~ style(color = ifelse(
                                     `Grado de Riesgo` %in% good_cals, "green", ifelse(`Grado de Riesgo` %in% medium_cals,"gold3", "red")),
                                     `border-radius` = "8px"))))
  
  export_formattable(ft_exposicion, "tables/exposicion.png")
  
}

gen_table_reserva <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  bd_reserva <- BD_Reservas[c("IC_Principal","Estatus", "Nombre_RazonSocial_ICPrincipal", "EI",
                                 "Reserva", "PI", "Calificacion")]
  
  agg_reserva <- bd_reserva %>% group_by(IC_Principal) %>%
    summarize(Nombre_RazonSocial_ICPrincipal = first(Nombre_RazonSocial_ICPrincipal),
              Estatus = first(Estatus),
              EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              PI = mean(as.numeric(PI)),
              Calificacion = first(Calificacion))
  
  agg_reserva_todos <- agg_reserva
  agg_reserva <- as.data.frame(agg_reserva) %>% filter(Estatus == "Vigente")
  
  
  agg_top_reserva <- as.data.frame(top_n(agg_reserva,10,Reserva))
  agg_top_reserva <- sort_desc(agg_top_reserva, Reserva)
  row.names(agg_top_reserva) <- c(1:nrow(agg_top_reserva))
  
  total_reserva <- sum(BD_Reservas$Reserva)
  reserva_acum <- c(agg_top_reserva$Reserva[[1]]/total_reserva)
  for (n in 2:nrow(agg_top_reserva)) reserva_acum[n] <- reserva_acum[[n-1]] + agg_top_reserva$Reserva[[n]]/total_reserva
  
  agg_top_reserva <- cbind(agg_top_reserva, "Reserva_acum" = reserva_acum)
  
  top_reserva_df <- data.frame("No." = c(1:nrow(agg_top_reserva)),
                               "Intermediario" = nombre_corto(as.character(agg_top_reserva$Nombre_RazonSocial_ICPrincipal)),
                               "Reserva" = round(as.numeric(agg_top_reserva$Reserva/1000000), digits=2),   
                               "EI" = round(as.numeric(agg_top_reserva$EI/1000000), digits=2),
                               "Reserva Acum." = as.character(paste(as.character(round(agg_top_reserva$Reserva_acum*100, digits=2)),"%",sep="")),
                               "Probabilidad Incumplimiento" = as.character(paste(as.character(round(agg_top_reserva$PI*100, digits=2)),"%",sep="")),
                               "Grado de Riesgo" = as.character(agg_top_reserva$Calificacion), stringsAsFactors = FALSE)
  
  top_reserva_df <- rbind(top_reserva_df,
                             c("No." = "",
                               "Intermediario" = "TOTAL 10 PARTICIPANTES",
                               "Reserva" = sum(top_reserva_df$Reserva),
                               "EI" = sum(top_reserva_df$EI),
                               "Reserva Acum." = top_reserva_df$Reserva.Acum.[[nrow(agg_top_reserva)]],
                               "Probabilidad Incumplimiento" = paste(as.character(round(mean(agg_top_reserva$PI)*100),digits=2),"%",sep=""),
                               "Grado de Riesgo" = ""))
  
  bd_vencido <- Query_Reservas %>% filter(Estatus =="Vencida" & Periodo == as.character(per))
  top_reserva_df <- rbind(top_reserva_df,
                          c("No." = "",
                            "Intermediario" = "Saldo Vencido",
                            "Reserva" = round(sum(bd_vencido$Reserva)/1000000,digits=2),
                            "EI" = round(sum(bd_vencido$EI)/1000000,digits=2),
                            "Reserva Acum." = paste(round(sum(bd_vencido$Reserva/sum(bd_vencido$EI))*100,digits=2), "%", sep=""),
                            "Probabilidad Incumplimiento" = "100%",
                            "Grado de Riesgo" = ""))
  
  top_reserva_df <- rbind(top_reserva_df,
                             c("No." = "",
                               "Intermediario" = paste("TOTAL (",nrow(agg_reserva_todos),")",sep=""),
                               "Reserva" = round(sum(BD_Reservas$Reserva)/1000000,digits=2),
                               "EI" = round(sum(BD_Reservas$EI)/1000000,digits=2),
                               "Reserva Acum." = "100%",
                               "Probabilidad Incumplimiento" = paste(as.character(round(weighted.mean(as.numeric(BD_Reservas$PI),as.numeric(BD_Reservas$EI))*100,digits=2)),"%",sep=""),
                               "Grado de Riesgo" = ""))
  
  names(top_reserva_df) <- gsub(".", " ", names(top_reserva_df), fixed = TRUE)
  
  ft_reserva <- formattable(top_reserva_df,
                               align = c("c", "r", "c", "c", "c", "c", "c"),
                               list(`Grado de Riesgo`= formatter(
                                 "span", style = ~ style(color = ifelse(
                                   `Grado de Riesgo` %in% good_cals, "green", ifelse(`Grado de Riesgo` %in% medium_cals,"gold3", "red")),
                                   `border-radius` = "8px"))))
  
  export_formattable(ft_reserva, "tables/reserva.png", width="1000px")
}

gen_table_pi <- function(per, grandes){
  BD_Reservas <- Query_Reservas %>% filter(Estatus == "Vigente" & Periodo == as.character(per))
  #Esta tabla se genera dos veces: una con acreditados grandes y otra con todos.
  
  
  bd_pi <- BD_Reservas[c("IC_Principal", "Nombre_RazonSocial_ICPrincipal", "EI",
                              "Reserva", "PI", "Calificacion")]
  
  agg_pi <- bd_pi %>% group_by(IC_Principal) %>%
    summarize(Nombre_RazonSocial_ICPrincipal = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              PI = mean(as.numeric(PI)),
              Calificacion = first(Calificacion))
  
  #Si grandes==TRUE, solo toma los acreditados grandes, si no, toma todos
  if (grandes == TRUE) agg_pi <- agg_pi %>% filter(EI>70000000)
  
  agg_top_pi <- as.data.frame(top_n(agg_pi,10,PI))
  agg_top_pi <- sort_desc(agg_top_pi, PI)
  row.names(agg_top_pi) <- c(1:nrow(agg_top_pi))
  
  total_reserva <- sum(Query_Reservas$Reserva)
  reserva_acum <- c(agg_top_pi$Reserva[[1]]/total_reserva)
  for (n in 2:nrow(agg_top_pi)) reserva_acum[n] <- reserva_acum[[n-1]] + agg_top_pi$Reserva[[n]]/total_reserva
  
  total_exposicion <- sum(Query_Reservas$EI)
  exposicion_acum <- c(agg_top_pi$EI[[1]]/total_exposicion)
  for (n in 2:nrow(agg_top_pi)) exposicion_acum[n] <- exposicion_acum[[n-1]] + agg_top_pi$EI[[n]]/total_exposicion
  
  agg_top_pi <- cbind(agg_top_pi, "Reserva_acum" = reserva_acum)
  agg_top_pi <- cbind(agg_top_pi, "Exposicion_acum" = exposicion_acum)
  
  top_pi_df <- data.frame("No." = c(1:nrow(agg_top_pi)),
                          "Intermediario" = nombre_corto(as.character(agg_top_pi$Nombre_RazonSocial_ICPrincipal)),
                          "Probabilidad Incumplimiento" = as.character(paste(as.character(round(agg_top_pi$PI*100, 2)),"%",sep="")),
                          "Exposicion" = as.numeric(round(agg_top_pi$EI/1000000,2)),
                          "Exposicion Acum." = paste(as.character(round(exposicion_acum*100,2)),"%",sep=""),
                          "Reserva" = as.numeric(round(agg_top_pi$Reserva/1000000,2)),   
                          "Reserva Acum." = paste(as.character(round(reserva_acum*100,2)),"%",sep=""),
                          "Grado de Riesgo" = as.character(agg_top_pi$Calificacion), stringsAsFactors = FALSE)
  
  top_pi_df <- rbind(top_pi_df,
                          c("No." = "",
                            "Intermediario" = "TOTAL",
                            "Probabilidad Incumplimiento" = paste(as.character(round(mean(as.numeric(agg_top_pi$PI))*100,2)),"%",sep=""),
                            "Exposicion" = round(sum(top_pi_df$Exposicion),2),
                            "Exposicion Acum." = top_pi_df$Exposicion.Acum.[[nrow(agg_top_pi)]],
                            "Reserva" = sum(top_pi_df$Reserva),
                            "Reserva Acum." = top_pi_df$Reserva.Acum.[[nrow(agg_top_pi)]],
                            "Grado de Riesgo" = ""))
  
  
  names(top_pi_df) <- gsub(".", " ", names(top_pi_df), fixed = TRUE)
  
  ft_pi <- formattable(top_pi_df,
                       align = c("c", "r", rep("c", 6)),
                       list(`Grado de Riesgo` = formatter(
                         "span", style = ~ style(color = ifelse(
                           `Grado de Riesgo` %in% good_cals, "green", ifelse(`Grado de Riesgo` %in% medium_cals,"gold3", "red"))))))
  
  if (grandes==TRUE) export_formattable(ft_pi, "tables/pi_grandes.png", width="1000px")
  else export_formattable(ft_pi, "tables/pi_todos.png", width="1000px")
}

gen_table_saldos_calificacion <- function(per){
  
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  bd_calificacion <- BD_Reservas[c("Calificacion", "EI")]
  
  agg_calificacion <- bd_calificacion %>% group_by(Calificacion) %>% summarize(EI = sum(as.numeric(EI)))
  
  total_exposicion <- sum(agg_calificacion$EI)
  
  exposiciones <- c(numeric())
  porcentajes_exposicion <- c(numeric())
  
  for (n in 1:length(calificaciones_list)){
    agg_calificacion_subset <- filter(agg_calificacion, Calificacion==calificaciones_list[[n]])
    if (nrow(agg_calificacion_subset) == 0){
      exposicion <- 0
      porcentaje_exposicion <- 0
    }
    else{
      exposicion <- agg_calificacion_subset$EI
      porcentaje_exposicion <- (agg_calificacion_subset$EI/total_exposicion)*100  
    }
    exposiciones[n] <- exposicion
    porcentajes_exposicion[n] <- porcentaje_exposicion
  }
  
  porcentajes_acumulados <- c(porcentajes_exposicion[[1]])
  for (n in 2:length(calificaciones_list)) porcentajes_acumulados[n] <- porcentajes_acumulados[[n-1]] + porcentajes_exposicion[[n]]
  
  calificacion_df <- data.frame("Grado de riesgo" = calificaciones_list,
                                "Exposicion" = round(exposiciones/1000000,2),
                                "Porcentaje exposicion" = paste(round(porcentajes_exposicion,2), "%", sep=""),
                                "Porcentaje acumulado" = paste(round(porcentajes_acumulados,2), "%", sep=""),
                                stringsAsFactors = FALSE)
  
  calificacion_df <- rbind(calificacion_df,
                           c("Grado de riesgo" = "TOTAL",
                           "Exposicion" = sum(calificacion_df$Exposicion),
                           "Porcentaje exposicion" = "",
                           "Porcentaje acumulado" = ""))
  
  names(calificacion_df) <- gsub(".", " ", names(calificacion_df), fixed = TRUE)
  
  ft_calificacion <- formattable(calificacion_df)
  export_formattable(ft_calificacion, "tables/saldos_calificacion.png", width="800px")
  
}

gen_table_saldos_tipo <- function(per, vencida = TRUE, return_df = FALSE){
  #AGREGAR OPCIÓN PARA DESPLEGAR SOLO LOS QUE SON vencida
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  if(vencida == FALSE){
    BD_Reservas_2 <- BD_Reservas
    BD_Reservas <- filter(BD_Reservas, Estatus == "Vigente")
  }
  
  tipo <- BD_Reservas[c("IC_Principal", "EI", "Categoria_Prestamo")]
  
  agg_tipo <- BD_Reservas %>% group_by(Categoria_Prestamo) %>%
    summarize(Tipo = first(Categoria_Prestamo),
      Acreditados = length(unique(IC_Principal)),
      EI = sum(EI)/1000000)
  
  agg_tipo <- sort_desc(agg_tipo, EI)
  
  Porcentaje_EI <- paste(round((agg_tipo$EI/sum(agg_tipo$EI))*100,2),"%",sep="")
  Exposicion_acum <- paste(round((cumsum(agg_tipo$EI)/sum(agg_tipo$EI))*100,2),"%",sep="")
  
  if(vencida == FALSE){
    Porcentaje_EI <- paste(round((agg_tipo$EI/(sum(BD_Reservas_2$EI)/1000000))*100,2),"%",sep="")
    Exposicion_acum <- paste(round((cumsum(agg_tipo$EI)/(sum(BD_Reservas_2$EI)/1000000))*100,2),"%",sep="")
  }
  
  df_tipo <- data.frame("Tipo" = agg_tipo$Tipo,
                       "No. de acreditados" = agg_tipo$Acreditados,
                       "EI" = round(agg_tipo$EI,2),
                       "Porcentaje EI" = Porcentaje_EI,
                       "Porcentaje EI Acum." = Exposicion_acum,
                       stringsAsFactors=FALSE)
  
  if(vencida==TRUE){
    df_tipo <- rbind(df_tipo, c("Tipo" = "TOTAL",
                                "No. de acreditados" = sum(df_tipo$No..de.acreditados),
                                "EI" = sum(df_tipo$EI),
                                "Porcentaje EI" = "100%",
                                "Porcentaje EI Acum." = "100%"))
  } else if (vencida == FALSE){
    df_tipo <- rbind(df_tipo, c("Tipo" = "TOTAL",
                                "No. de acreditados" = sum(df_tipo$No..de.acreditados),
                                "EI" = sum(df_tipo$EI),
                                "Porcentaje EI" = paste0(round((sum(as.numeric(df_tipo$EI))/(sum(BD_Reservas_2$EI)/1000000))*100,2),"%"),
                                "Porcentaje EI Acum." = df_tipo$`Porcentaje.EI.Acum.`[[nrow(df_tipo)]] ))
  }
  
  names(df_tipo) <- gsub(".", " ", names(df_tipo), fixed = TRUE)
  
  ft_tipo <- formattable(df_tipo)
  if(vencida == TRUE){
    export_formattable(ft_tipo, "tables/saldos_tipo.png", width="800px")  
  } else{
    export_formattable(ft_tipo, "tables/saldos_tipo_vigentes.png", width="800px")  
  }
  
  if(return_df == TRUE) df_tipo
}

gen_table_saldos_anexo <- function(per, return_df = FALSE){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  anexos <- BD_Reservas[c("IC_Principal", "EI", "Anexo")]
  agg_anexos <- BD_Reservas %>% group_by(Anexo) %>%
    summarize(Acreditados = length(unique(IC_Principal)),
              EI = sum(EI)/1000000)
  
  agg_anexos <- sort_desc(agg_anexos, EI)
  
  Porcentaje_EI <- paste(round((agg_anexos$EI/sum(agg_anexos$EI))*100,2),"%",sep="")
  Exposicion_acum <- paste(round((cumsum(agg_anexos$EI)/sum(agg_anexos$EI))*100,2),"%",sep="")
  
  df_anexos <- data.frame("Anexo" = agg_anexos$Anexo,
                        "No. de acreditados" = agg_anexos$Acreditados,
                        "EI" = round(agg_anexos$EI,2),
                        "Porcentaje EI" = Porcentaje_EI,
                        "Porcentaje EI Acum." = Exposicion_acum,
                        stringsAsFactors=FALSE)
  
  df_anexos <- rbind(df_anexos, c("Anexo" = "TOTAL",
                              "No. de acreditados" = sum(df_anexos$No..de.acreditados),
                              "EI" = sum(df_anexos$EI),
                              "Porcentaje EI" = "100%",
                              "Porcentaje EI Acum." = "100%"))
  names(df_anexos) <- gsub(".", " ", names(df_anexos), fixed = TRUE)
  
  ft_anexos <- formattable(df_anexos)
  export_formattable(ft_anexos, "tables/saldos_anexo.png", width="800px")
  if(return_df == TRUE) df_anexos
}

gen_table_cartera_gerencia <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  agg_gerencias <- BD_Reservas %>% group_by(Gerencia_Regional) %>%
    summarize(EI = round(sum(EI),2),
              PI = round(mean(as.numeric(PI)),2),
              Reserva = round(sum(Reserva),2))
  
  agg_gerencias_vencida <- BD_Reservas %>% filter(Estatus == "Vencida") %>%
    group_by(Gerencia_Regional, .drop=FALSE) %>%
    summarize(Exposición_Vencida = round(sum(EI)),2)
  
  cartera_vencida <- c(numeric())
  for(n in 1:nrow(agg_gerencias)){
    if (agg_gerencias$Gerencia_Regional[[n]] %in% agg_gerencias_vencida$Gerencia_Regional){
      cartera_vencida[n] <- agg_gerencias_vencida$Exposición_Vencida[[which(agg_gerencias_vencida$Gerencia_Regional==agg_gerencias$Gerencia_Regional[[n]])]]
    }
    else cartera_vencida[n] <- 0
  }
  
  
  imor <- (cartera_vencida/agg_gerencias$EI)*100
  porcentaje_reservas <- (agg_gerencias$Reserva/agg_gerencias$EI)*100
  
  df_gerencias <- data.frame("Gerencia Regional" = agg_gerencias$Gerencia_Regional,
                             "Exposición" = round(agg_gerencias$EI/1000000,2),
                             "Exposición Vencida" = round(cartera_vencida/1000000,2),
                             "IMOR" = paste(round(imor,2), "%", sep=""),
                             "Probabilidad de incumplimiento" = paste(round(agg_gerencias$PI*100,2), "%",sep=""),
                             "Reserva" = round(agg_gerencias$Reserva/1000000,2),
                             "Porcentaje Reservas" = paste(round(porcentaje_reservas,2),"%",sep=""),
                             stringsAsFactors = FALSE)
  
  df_gerencias <- sort_desc(df_gerencias, Exposición)
  
  df_gerencias <- rbind(df_gerencias, c("Gerencia Regional" = "TOTAL",
                        "Exposición" = sum(df_gerencias$Exposición),
                        "Exposición Vencida" = sum(df_gerencias$Exposición.Vencida),
                        "IMOR" = paste(round(mean(imor),2), "%", sep=""),
                        "Probabilidad de incumplimiento" = paste(round(mean(agg_gerencias$PI),2), "%",sep=""),
                        "Reserva" = sum(df_gerencias$Reserva),
                        "Porcentaje Reservas" = round(mean(porcentaje_reservas),2)))
  
  rownames(df_gerencias) <- NULL
  names(df_gerencias) <- gsub(".", " ", names(df_gerencias), fixed = TRUE)
  
  ft_gerencias <- formattable(df_gerencias)
  export_formattable(ft_gerencias, "tables/cartera_gerencia.png", width="1000px")
  
}

gen_table_cambio_riesgo <- function(per, condicion){
  per_0 <- as.character(per)
  per_1 <- as.character(as.numeric(per_0)-1)
  if (substr(per_0,5,6) == "01") per_1 <- as.character(as.numeric(per_1) - 89)
  
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per_0))
  BD_Reservas_1 <- Query_Reservas %>% filter(Periodo == as.character(per_1))
  
  agg_acreditados <- BD_Reservas %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              Exposición = sum(EI),
              Reserva = sum(Reserva),
              Calificacion = first(Calificacion))
  
  agg_acreditados_1 <- BD_Reservas_1 %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              Exposición = sum(EI),
              Reserva = sum(Reserva),
              Calificacion = first(Calificacion))
  
  cambio_calificacion <- c()
  for(n in 1:nrow(agg_acreditados)){
    if (agg_acreditados$IC_Principal[[n]] %in% agg_acreditados_1$IC_Principal){
      calificacion_0 <- which(agg_acreditados$Calificacion[[n]] == calificaciones_list)
      calificacion_1 <- which(subset(agg_acreditados_1, IC_Principal == agg_acreditados$IC_Principal[[n]])$Calificacion == calificaciones_list)
      if (condicion == "Mejor") cambio_calificacion[n] <- calificacion_0 < calificacion_1
      else if (condicion == "Peor") cambio_calificacion[n] <- calificacion_0 > calificacion_1
    }
    else cambio_calificacion[n] <- FALSE
  }
  
  agg_cambio <- agg_acreditados[which(cambio_calificacion),]
  
  agg_cambio_1 <- subset(agg_acreditados_1, IC_Principal %in% agg_cambio$IC_Principal)
  
  variacion_reserva <- (agg_cambio$Reserva - agg_cambio_1$Reserva)/1000000
  
  df_cambio <- data.frame("Cartera con cambio de riesgo" = agg_cambio$Nombre,
                          "Grado de riesgo anterior" = agg_cambio_1$Calificacion,
                          "Grado de riesgo actual" = agg_cambio$Calificacion,
                          "Reserva Anterior" = round(agg_cambio_1$Reserva/1000000,2),
                          "Reserva Actual" = round(agg_cambio$Reserva/1000000,2),
                          "Variación Reserva" = round(variacion_reserva,2),
                          "Exposición Anterior" = round(agg_cambio_1$Exposición/1000000,2),
                          "Exposición Actual" = round(agg_cambio$Exposición/1000000,2),
                          stringsAsFactors = FALSE)
  
  df_cambio <- rbind(df_cambio, c("SUBTOTAL",
                                  "",
                                  "",
                                  sum(df_cambio$Reserva.Anterior),
                                  sum(df_cambio$Reserva.Actual),
                                  sum(df_cambio$Variación.Reserva),
                                  sum(df_cambio$Exposición.Anterior),
                                  sum(df_cambio$Exposición.Actual)))
  
  names(df_cambio) <- gsub(".", " ", names(df_cambio), fixed = TRUE)
  ft_cambio <- formattable(df_cambio)
  if (condicion == "Peor") export_formattable(ft_cambio, "tables/cambio_riesgo_peor.png", width="1000px")
  else if (condicion =="Mejor") export_formattable(ft_cambio, "tables/cambio_riesgo_mejor.png", width="1000px")
}

gen_table_estadisticos_clientes <- function(per){
  per_0 <- as.character(per)
  per_1 <- as.character(as.numeric(per_0)-1)
  if (substr(per_0,5,6) == "01") per_1 <- as.character(as.numeric(per_1) - 89)
  per_12 <- as.character(as.numeric(as.character(per))-100)
  periodos <- c(per_12, per_1, per_0)
  
  per_0_text <- paste(months[[as.numeric(substr(per_0,5,6))]], substr(per_0,3,4))
  per_1_text <- paste(months[[as.numeric(substr(per_1,5,6))]], substr(per_1,3,4))
  per_12_text <- paste(months[[as.numeric(substr(per_12,5,6))]], substr(per_12,3,4))
  periodos_text <- c(per_12_text, per_1_text, per_0_text)
  
  categorias <- unique(Query_Reservas$Categoria_Prestamo)
  
  output_list <- list()
  for (n in 1:length(categorias)){
    BD_Reservas <- Query_Reservas %>% filter(Periodo %in% periodos & Categoria_Prestamo == categorias[[n]])
    agg_estadisticos <- BD_Reservas %>% group_by(Periodo) %>%
      summarize(Exposición = sum(EI),
                Reserva = sum(Reserva),
                Porcentaje_Reservas = sum(Reserva)/sum(EI))
    agg_estadisticos <- agg_estadisticos[order(agg_estadisticos$Periodo),]
    
    df_estadisticos <- data.frame("Exposición" = round(agg_estadisticos$Exposición/1000000,2),
                                  "Reserva" = round(agg_estadisticos$Reserva/1000000,2),
                                  "Porcentaje de Reservas" = round(agg_estadisticos$Porcentaje_Reservas*100,2),
                                  stringsAsFactors = FALSE)
    
    df_estadisticos <- as.data.frame(t(df_estadisticos), stringsAsFactors = FALSE)
    names(df_estadisticos) <- periodos_text
    
    df_estadisticos <- df_estadisticos %>% cbind("Variación Porcentual" = round(((df_estadisticos[,3]/df_estadisticos[,2])-1)*100,2)) %>%
      cbind("Variación Absoluta" = df_estadisticos[,3]-df_estadisticos[,2])
    
    df_estadisticos[3,4] <- paste(round((df_estadisticos[3,3]-df_estadisticos[3,2])/0.01,2),"PB")
    df_estadisticos[3,-c(4,5)] <- paste(df_estadisticos[3,-c(4,5)],"%",sep="")
    df_estadisticos[-3,4] <- paste(df_estadisticos[-3,4], "%", sep="")
    
    names(df_estadisticos) <- gsub(".", " ", names(df_estadisticos), fixed = TRUE)
    
    output_list[[n]] <- df_estadisticos
  }
  
  names(output_list) <- categorias
  
  for(n in 1:length(output_list)){
    ft_estadistico <- formattable(output_list[[n]])
    filename <- paste0("tables/estadisticos_clientes_",names(output_list[n]),".png")
    export_formattable(ft_estadistico, filename, width="600px")
  }
  
}

gen_table_diferencias_exposicion <- function(per){
  per_0 <- as.character(per)
  per_1 <- as.character(as.numeric(per_0)-1)
  if (substr(per_0,5,6) == "01") per_1 <- as.character(as.numeric(per_1) - 89)
  
  per_0_text <- paste(months[[as.numeric(substr(per_0,5,6))]], substr(per_0,3,4))
  per_1_text <- paste(months[[as.numeric(substr(per_1,5,6))]], substr(per_1,3,4))
  
  BD_Reservas_0 <- Query_Reservas %>% filter(Periodo == per_0)
  BD_Reservas_1 <- Query_Reservas %>% filter(Periodo == per_1)
  
  diferencias_exposicion_0 <- BD_Reservas_0 %>% group_by(IC_Principal) %>%
    summarize(Acreditado = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              EI_0 = sum(EI))
  
  diferencias_exposicion_1 <- BD_Reservas_1 %>% group_by(IC_Principal) %>%
    summarize(Acreditado = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              EI_1 = sum(EI))
  
  diferencias_exposicion <- merge(diferencias_exposicion_0, diferencias_exposicion_1, by="Acreditado")
  diferencias_exposicion <- cbind(diferencias_exposicion, dEI = diferencias_exposicion$EI_0 - diferencias_exposicion$EI_1)
  
  total_incremento_exposicion <- round(sum(diferencias_exposicion$dEI[diferencias_exposicion$dEI>=0])/1000000,2)
  total_decremento_exposicion <- round(sum(diferencias_exposicion$dEI[diferencias_exposicion$dEI<0])/1000000,2)
  
  top_incrementos_exposicion <- diferencias_exposicion %>% top_n(5, dEI) %>% sort_desc(dEI)
  top_decrementos_exposicion <- diferencias_exposicion %>% top_n(-5, dEI) %>% sort_asc(dEI)
  
  df_top_incrementos <- data.frame(top_incrementos_exposicion$Acreditado,
                                   round(top_incrementos_exposicion$EI_1/1000000,2),
                                   round(top_incrementos_exposicion$EI_0/1000000,2),
                                   round(top_incrementos_exposicion$dEI/1000000,2),
                                   stringsAsFactors=FALSE)
  
  df_top_decrementos <- data.frame(top_decrementos_exposicion$Acreditado,
                                   round(top_decrementos_exposicion$EI_1/1000000,2),
                                   round(top_decrementos_exposicion$EI_0/1000000,2),
                                   round(top_decrementos_exposicion$dEI/1000000,2),
                                   stringsAsFactors = FALSE)
  
  top_colnames <- c("Acreditado", per_1_text, per_0_text, "Diferencia")
  
  names(df_top_incrementos) = top_colnames
  names(df_top_decrementos) = top_colnames
  
  df_top_incrementos = rbind(df_top_incrementos,
                             c("Total de los 5 principales", sum(df_top_incrementos[,2]), sum(df_top_incrementos[,3]), sum(df_top_incrementos$Diferencia)),
                             c("TOTAL DEL INCREMENTO", "", "", total_incremento_exposicion))
  df_top_decrementos = rbind(df_top_decrementos,
                             c("Total de los 5 principales", sum(df_top_decrementos[,2]), sum(df_top_decrementos[,3]), sum(df_top_decrementos$Diferencia)),
                             c("TOTAL DEL DECREMENTO", "", "", total_decremento_exposicion),
                             c("INCREMENTO - DECREMENTO", "", "", sum(as.numeric(df_top_incrementos[7,4]), na.rm=TRUE) - sum(as.numeric(df_top_decrementos[7,4]), na.rm=TRUE)))
                               
  
  output_list <- list(df_top_incrementos, df_top_decrementos)
  
  for(n in 1:length(output_list)){
    ft_diferencias_exposicion <- formattable(output_list[[n]])
    filename <- paste0("tables/diferencias_exposicion_", n , ".png")
    export_formattable(ft_diferencias_exposicion, filename, width="600px")
  }
  
}

gen_table_canal_distribucion <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  BD_Reservas$Tipo_Intermediario[BD_Reservas$Estatus=="Vencida"] <- "Vencida"
  
  agg_canal_distribucion <- BD_Reservas %>% group_by(Tipo_Intermediario) %>%
    summarize(EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              PI = mean(as.numeric(PI)),
              SP = mean(as.numeric(SP)))
  
  porcentaje_reserva <- (agg_canal_distribucion$Reserva/agg_canal_distribucion$EI)*100
  participacion_cartera <- (agg_canal_distribucion$EI/sum(agg_canal_distribucion$EI))*100
  
  
  df_canal_distribucion <- data.frame("Canal de Distribución" = agg_canal_distribucion$Tipo_Intermediario,
                                      "Exposición" = round(agg_canal_distribucion$EI/1000000,2),
                                      "Reserva" = round(agg_canal_distribucion$Reserva/1000000,2),
                                      "Porcentaje de Reserva" = paste(round(porcentaje_reserva,2), "%", sep=""),
                                      "Participación de Cartera" = paste(round(participacion_cartera,2), "%", sep=""),
                                      "Probabilidad de Incumplimiento" = paste(round(agg_canal_distribucion$PI*100,2), "%", sep=""),
                                      "Severidad de la Pérdida" = paste(round(agg_canal_distribucion$SP*100,2), "%", sep=""),
                                      stringsAsFactors = FALSE)
  
  df_canal_distribucion <- rbind(df_canal_distribucion, c("Canal de Distribución" = "",
                                                          "Exposición" = sum(df_canal_distribucion$Exposición),
                                                          "Reserva" = sum(df_canal_distribucion$Reserva),
                                                          "Porcentaje de Reserva" = paste(round((sum(df_canal_distribucion$Reserva)/sum(df_canal_distribucion$Exposición))*100,2),"%", sep=""),
                                                          "Participación de Cartera" = "100%",
                                                          "Probabilidad de Incumplimiento" = paste(round(mean(agg_canal_distribucion$PI)*100,2),"%", sep=""),
                                                          "Severidad de la Pérdida" = paste(round(mean(agg_canal_distribucion$SP)*100,2), "%", sep="")))
  
  names(df_canal_distribucion) <- gsub(".", " ", names(df_canal_distribucion), fixed = TRUE)
  
  ft_canal_distribucion <- formattable(df_canal_distribucion)
  
  export_formattable(ft_canal_distribucion, "tables/canal_distribucion.png", width="1000px")
}

gen_table_grupo <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  BD_Reservas$`Grupo de act`[BD_Reservas$Estatus=="Vencida"] <- "Vencida"
  
  nombres_grupos <- c("Productores de mineral",
                      "Servicios a la industria de la minería",
                      "Procesadores primarios de mineral",
                      "Procesadores de mineral procesado",
                      "Distribuidores y comercializadores",
                      "")
  
  agg_grupo <- BD_Reservas %>% group_by(`Grupo de act`) %>%
    summarize(EI = sum(EI),
              Reserva = sum(Reserva),
              PI = mean(as.numeric(PI)))
  
  df_grupo <- data.frame("Grupos" = agg_grupo$`Grupo de act`,
                         "Nombre de Actividad" = nombres_grupos,
                         "Exposición" = round(agg_grupo$EI/1000000,2),
                         "Participación Cartera" = paste(round((agg_grupo$EI/sum(agg_grupo$EI))*100,2), "%", sep=""),
                         "Reserva" = round(agg_grupo$Reserva/1000000,2),
                         "Porcentaje de Reserva" = paste(round((agg_grupo$Reserva/agg_grupo$EI)*100,2), "%", sep=""),
                         "Probabilidad de Incumplimiento" = paste(round(agg_grupo$PI*100,2), "%", sep=""),
                         stringsAsFactors = FALSE)
  
  df_grupo <- rbind(df_grupo, c("Grupos" = "TOTAL",
                                "Nombre de Actividad" = "",
                                "Exposición" = sum(df_grupo$Exposición),
                                "Participación Cartera" = "100%",
                                "Reserva" = sum(df_grupo$Reserva),
                                "Porcentaje de Reserva" = paste(round(mean(agg_grupo$Reserva/agg_grupo$EI)*100,2),"%", sep=""),
                                "Probabilidad de Incumplimiento" = paste(round(mean(agg_grupo$PI)*100,2), "%", sep="")
                                ))
  
  names(df_grupo) <- gsub(".", " ", names(df_grupo), fixed = TRUE)
  
  ft_grupo <- formattable(df_grupo)
  export_formattable(ft_grupo, "tables/grupo.png", width="1000px")
  
}

gen_table_cartera_plazos <- function(per, return_df = FALSE){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  plazos_cast <- as.data.frame(cast(BD_Reservas, Calificacion ~ Plazo, sum, value = "EI"))
  missing_cals <- calificaciones_list[!(calificaciones_list %in% plazos_cast[[1]])]
  for (missed_cal in missing_cals){
    plazos_cast <- rbind(plazos_cast, rep(0, length(plazos_cast)))
    plazos_cast[nrow(plazos_cast),1] <- missed_cal
  }
  missing_plazos <- c(1:7)[!(c(1:7) %in% names(plazos_cast)[-1])]
  for (missed_plazo in missing_plazos) plazos_cast[,as.character(missed_plazo)] <- rep(0, nrow(plazos_cast))
  
  df_plazos <- plazos_cast
  df_plazos[,-1] <- round(df_plazos[,-1]/1000000, digits=1)
  df_plazos <- df_plazos[,order(names(df_plazos))]
  df_plazos <- df_plazos[c(8,1:7)]
  df_plazos <- df_plazos[order(df_plazos[1]),]
  names(df_plazos) <- c("Calificación", nombres_plazos)
  
  ft_plazos <- formattable(df_plazos)
  
  export_formattable(ft_plazos, "tables/cartera_plazos.png")
  
  if(return_df == TRUE) df_plazos
  
}

gen_table_concentracion <- function(per){
  ##GENERAR FUNCION PARA LA TABLA
}

gen_table_indices_concentracion <- function(per){
  ##GENERAR TABLA CON INFORMACION DE LA TABLA ANTERIOR
}

#===================================================================


#===================================================================
#===================================================================
gen_graph_resumen <- function(per){
  resumen <- Query_Reservas %>% filter(Estatus == "Vigente" & Periodo == as.character(per))
  resumen <- resumen[,c("IC_Principal","Nombre_RazonSocial_ICPrincipal", "PI", "EI",
                        "Reserva","Calificacion")]
  agg_resumen <- resumen %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              PI = mean(as.numeric(PI)),
              EI = sum(as.numeric(EI)),
              Reserva = sum(as.numeric(Reserva)),
              Calificacion = first(Calificacion))
  
  df_resumen <- data.frame("Acreditado" = agg_resumen$Nombre,
                           "Probabilidad Incumplimiento" = round(agg_resumen$PI*100, digits=2),
                           "Exposición" = agg_resumen$EI/1000000,
                           "Reserva" = agg_resumen$Reserva/1000000,
                           "Calificación" = agg_resumen$Calificacion,
                           stringsAsFactors = FALSE, check.names = FALSE)
 
  plot_resumen <- ggplot(data = df_resumen, aes(Exposición, `Probabilidad Incumplimiento`)) + theme_bw() +
    geom_point(aes(color = Calificación, size=Reserva), alpha = .5) + scale_size(range = c(1, 40)) +
    geom_text_repel(aes(label = Acreditado), data = df_resumen[df_resumen$Reserva %in% top_n(df_resumen, 10, Reserva)$Reserva|df_resumen$`Probabilidad Incumplimiento`>20,], point.padding = NA) +
    guides(colour = guide_legend(override.aes = list(size=10)), size = FALSE) +
    scale_color_manual(values = c("#257A24", "#3C8321", "#5A8C1E", "#7E951A", "#9E9315", "#A87310", "#B14A0A", "#BA1903", "#BF0003")) +
    theme(text = element_text(size = 10), axis.text = element_text(size = 10))
  
  ggsave(plot = plot_resumen, filename="graphs/resumen.png", width = 10, height = 6, dpi = 300)
    
}

gen_graph_brechas_liquidez <- function(per){
  if (as.numeric(per) > max(as.numeric(Query_Tesoreria$periodo))) per <- max(as.numeric(Query_Tesoreria$periodo))
  per_12 <- as.character(as.numeric(as.character(per))-99)
  year <- substr(as.character(per), 1, 4)
  brechas_liquidez <- Query_Tesoreria %>% filter(periodo %in% c(per_12:per))
  brechas_liquidez <- lapply(brechas_liquidez, gsub, pattern=",", replacement="")
  
  ingresos <- as.numeric(brechas_liquidez$Disponible) + as.numeric(brechas_liquidez$COBRANZA) + as.numeric(brechas_liquidez$Otros.Ingresos.....FINANCIAMIE)
  egresos <- as.numeric(brechas_liquidez$CREDITO) + as.numeric(brechas_liquidez$Capital...Intereses.Deuda) + as.numeric(brechas_liquidez$Gastos.de.Oper..Y.Admon)
  disponibilidad <- ingresos - abs(egresos)
    
  tesoreria <- data.frame("Ingresos" = round(ingresos/1000, digits=2),
                          "Egresos" = round(egresos/1000, digits=2),
                          "Disponibilidad" = round(disponibilidad/1000, digits=2))
  
  tesoreria_months <- sapply(sapply(c(per_12:per), substr, 5, 6), as.numeric)
  tesoreria_years <- sapply(c(per_12:per), substr,3,4)
  tesoreria_years <- tesoreria_years[as.numeric(tesoreria_months) <=12 & as.numeric(tesoreria_months)>0]
  tesoreria_months <- tesoreria_months[as.numeric(tesoreria_months) <=12 & as.numeric(tesoreria_months)>0]
  tesoreria_months_names <- months[tesoreria_months]
  tesoreria_pers <- paste(tesoreria_months_names, tesoreria_years)
  
  tesoreria["Periodo"] <- factor(tesoreria_pers, levels = unique(tesoreria_pers))
  
  plot_disponibilidad <- ggplot(data = tesoreria, aes(x=Periodo)) + theme_bw() +
    geom_bar(aes(y=Ingresos, fill = "Ingresos"), stat="identity") + 
    geom_bar(aes(y=Egresos, fill = "Egresos"), stat="identity") +
    geom_line(aes(y=Disponibilidad, group = 1, linetype = "Disponibilidad"), color = "black", size=1.5) +
    geom_text(aes(y = Ingresos, label = Ingresos), vjust=-1, color="gray15", size=3.5) +
    geom_text(aes(y = Egresos, label = Egresos), vjust = 1.3, color = "gray15", size=3.5) +
    geom_text(aes(y = Disponibilidad, label = Disponibilidad), vjust = -0.3, color = "white", size=3.5) +
    theme(legend.title = element_blank()) + scale_color_manual(c("green", "red", "yellow"))
  
  
  ggsave("graphs/brechas_liquidez.png", plot = plot_disponibilidad, width=10, height=6, dpi=300)
}

gen_graph_porcentaje_reserva <- function(per){}

gen_graph_saldos_calificacion <- function(per){
  
  calificaciones_list <- c("A-1", "A-2", "B-1", "B-2", "B-3", "C-1", "C-2", "D", "E")
  
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  
  bd_calificacion <- BD_Reservas[c("Calificacion", "EI")]
  
  agg_calificacion <- bd_calificacion %>% group_by(Calificacion) %>% summarize(EI = sum(as.numeric(EI)))
  
  exposiciones <- c(numeric())
  for (n in 1:length(calificaciones_list)){
    agg_calificacion_subset <- filter(agg_calificacion, Calificacion==calificaciones_list[[n]])
    if (nrow(agg_calificacion_subset) == 0) exposicion <- 0
    else exposicion <- agg_calificacion_subset$EI
    exposiciones[n] <- round(exposicion/1000000,2)
  }
  calificacion_df <- data.frame("Calificacion" = calificaciones_list,
                                "Exposición" = exposiciones,
                                stringsAsFactors = FALSE)
  plot_cal <- ggplot(data = calificacion_df, aes(x=Calificacion, y=Exposición)) + theme_bw() +
    geom_col(colour="navyblue", fill="navyblue") + geom_text(label = calificacion_df$Exposición, hjust = -0.1) +
    ggtitle("Monto de Exposición") + coord_flip()
  
  ggsave("graphs/saldos_calificacion.png", plot = plot_cal, width=10, height=6, dpi=300)
}

gen_graph_exposicion_por_riesgo <- function(per){
  calificaciones_list <- c("A-1", "A-2", "B-1", "B-2", "B-3", "C-1", "C-2", "D", "E")
  
  bd_calificacion <- Query_Reservas[c("Calificacion", "EI")]
  
  agg_exposicion <- bd_calificacion %>% group_by(Calificacion) %>% summarize(EI = sum(as.numeric(EI)))
  
  exposiciones <- c(numeric())
  for (n in 1:length(calificaciones_list)){
    agg_exposicion_subset <- filter(agg_exposicion, Calificacion==calificaciones_list[[n]])
    if (nrow(agg_exposicion_subset) == 0) exposicion <- 0
    else exposicion <- agg_exposicion_subset$EI
    exposiciones[n] <- exposicion/1000000
  }
  exposiciones_df <- data.frame("Calificacion" = calificaciones_list,
                                "Exposición" = exposiciones,
                                stringsAsFactors = FALSE)
  exposiciones_df <- sort_desc(exposiciones_df, Exposición)
  exposiciones_df <- cbind(exposiciones_df, "Exposición_acumulada" = round((cumsum(exposiciones_df$Exposición)/sum(exposiciones_df$Exposición))*100),3)
  
  plot_exp_cal <- ggplot(exposiciones_df, aes(x=reorder(Calificacion,Exposición_acumulada), y=Exposición_acumulada)) + theme_bw() +
    geom_point(size = 2) + ylim(0, NA) + 
    geom_text(label = paste(exposiciones_df$Exposición_acumulada, "%",sep=""),
              hjust = 0.5, vjust=-1, show.legend = FALSE) +
    theme(legend.position = "none", axis.title = element_blank())
    
  
  ggsave(plot = plot_exp_cal, filename="graphs/exposicion_por_riesgo.png", width = 10, height = 6, dpi = 300)
  
  #NOTA: NO SE SI AQUI EL ORDEN DE LAS CALIFICACIONES ES CORRECTO (MAYOR EXPOSICION A MENOR) O DEBERIAN IR ORDENADAS
  #EN EL ORDEN APROPIADO DE MAYOR A MENOR CALIFICACION.
  #Comentario a la nota: A mi me parece que este orden es correcto)
}

gen_graph_exposicion_por_acreditado <- function(per){
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per) & EI>0)
  
  bd_exposicion <- BD_Reservas[c("IC_Principal","Nombre_RazonSocial_ICPrincipal", "EI")]
  agg_exposicion <- bd_exposicion %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),EI = sum(as.numeric(EI)))
  
  agg_exposicion <- sort_desc(agg_exposicion, EI)
  
  exposicion_acumulada <- (cumsum(agg_exposicion$EI)/sum(agg_exposicion$EI))*100
  
  exposicion_df <- data.frame("Nombre" = agg_exposicion$Nombre,
                              "Exposición" = agg_exposicion$EI,
                              "Exposición_Acumulada" = exposicion_acumulada,
                              stringsAsFactors = FALSE)
  exposicion_df$Nombre <- factor(exposicion_df$Nombre, levels = unique(exposicion_df$Nombre))
  
  plot_exp_acr <- ggplot(exposicion_df, aes(x=reorder(Nombre,Exposición_Acumulada), y=Exposición_Acumulada)) + theme_bw() +
    geom_hline(aes(yintercept = 25), alpha = 0.4) + geom_hline(aes(yintercept = 75), alpha = 0.4) + geom_hline(aes(yintercept = 50), alpha = 0.4) +
    geom_point(size = 1) + ylim(0, NA) +
    theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_text(angle=90, hjust=1))
    
  ggsave(plot = plot_exp_acr, filename="graphs/exposicion_por_acreditado.png", width = 10, height = 6, dpi = 300)
  
  
}

gen_graph_saldos_tipo <- function(per, vencidas = TRUE){
  df_tipo <- head(gen_table_saldos_tipo(per, vencida = vencidas, return_df=TRUE),-1)
  df_tipo$`Porcentaje EI Acum ` <- as.numeric(gsub("%","",df_tipo$`Porcentaje EI Acum `))
  
  df_tipo$Tipo <- factor(df_tipo$Tipo, levels = unique(df_tipo$Tipo))
  
  plot_tipo <- ggplot(df_tipo, aes(x=Tipo, y=`Porcentaje EI Acum `)) + theme_bw() +
    geom_point(size = 1) +
    geom_text(label = paste(df_tipo$`Porcentaje EI Acum `,"%",sep=""),
              hjust=0.5, vjust = -1, show.legend = FALSE) +
    theme(legend.position = "none", axis.title = element_blank()) + ylim(0, NA) +
    ggtitle("Exposición al incumplimiento acumulada por tipo de acreditado")
  
  if(vencidas == TRUE){
    ggsave(plot = plot_tipo, filename="graphs/saldos_tipo.png", width = 10, height = 6, dpi = 300)
  } else{
    ggsave(plot = plot_tipo, filename="graphs/saldos_tipo_vigentes.png", width = 10, height = 6, dpi = 300)
  }
}

gen_graph_saldos_anexo <- function(per){
  df_anexos <- head(gen_table_saldos_anexo(per, return_df = TRUE),-1)
  df_anexos$`Porcentaje EI Acum ` <- as.numeric(gsub("%","",df_anexos$`Porcentaje EI Acum `))
  
  df_anexos$Anexo <- factor(df_anexos$Anexo, levels = unique(df_anexos$Anexo))
  
  plot_anexos <- ggplot(df_anexos, aes(x=Anexo, y=`Porcentaje EI Acum `)) + theme_bw() +
    geom_point(size = 1) +
    geom_text(label = paste(df_anexos$`Porcentaje EI Acum `,"%",sep=""),
              hjust=0.5, vjust = -1, show.legend = FALSE) +
    theme(legend.position = "none", axis.title = element_blank()) + ylim(0, NA) +
    ggtitle("Exposición al incumplimiento acumulada por tipo de anexo")
  
  ggsave(plot = plot_anexos, filename="graphs/saldos_anexo.png", width = 10, height = 6, dpi = 300)
  
}

gen_graph_ei_riesgo_categoria <- function(per){
   BD_Reservas <- Query_Reservas %>% filter(Estatus =="Vigente" & Periodo == as.character(per))
   
   riesgo_categoria <- BD_Reservas[c("IC_Principal", "Nombre_RazonSocial_ICPrincipal",
                                     "Categoria_Prestamo","EI","Calificacion")]
   agg_riesgo_categoria <- BD_Reservas %>% group_by(IC_Principal) %>%
     summarize(EI = sum(EI)/1000000,
               Acreditado = first(Nombre_RazonSocial_ICPrincipal),
               Calificacion = first(Calificacion),
               Categoria = first(Categoria_Prestamo))
   
   df_riesgo_categoria <- data.frame(matrix(ncol=3,nrow=9, dimnames=list(NULL, c("Directos", "IFB", "IFE"))))
   for (n in 1:length(calificaciones_list)){
     agg_riesgo_subset <- filter(agg_riesgo_categoria, Calificacion == calificaciones_list[[n]])
     
     df_riesgo_categoria$Directos[n] <- sum(agg_riesgo_subset$EI[agg_riesgo_subset$Categoria == "Directos"])
     df_riesgo_categoria$IFB[n] <- sum(agg_riesgo_subset$EI[agg_riesgo_subset$Categoria == "IFB"])
     df_riesgo_categoria$IFE[n] <- sum(agg_riesgo_subset$EI[agg_riesgo_subset$Categoria == "IFE"])
   }
   df_riesgo_categoria$Calificacion <- calificaciones_list
   
   plot_riesgo_categoria <- ggplot(df_riesgo_categoria, aes(x=Calificacion)) + theme_bw() +
     geom_bar(aes(y=IFE+IFB+Directos, fill="Directos"), stat="identity") +
     geom_bar(aes(y=IFE+IFB, fill="IFB"), stat="identity") +
     geom_bar(aes(y=IFE, fill="IFE"), stat="identity") + ylab("Exposición") +
     theme(legend.title = element_blank())
   plot_riesgo_categoria
   
   ggsave(plot = plot_riesgo_categoria, filename="graphs/ei_riesgo_categoria.png", width = 10, height = 6, dpi = 300)
   
}

gen_graph_grupo <- function(per, vencida, per_1 = NULL){
  if (is.null(per_1)==FALSE) per <- per_1
  per_text <- paste(months[[as.numeric(substr(per,5,6))]], substr(per,3,4))
  BD_Reservas <- Query_Reservas %>% filter(Periodo == as.character(per))
  if(vencida == TRUE) BD_Reservas <- BD_Reservas %>% filter(Estatus == "Vigente")
  
  nombres_grupos <- c("Productores de mineral",
                      "Servicios a la industria de la minería",
                      "Procesadores primarios de mineral",
                      "Procesadores de mineral procesado",
                      "Distribuidores y comercializadores")
  
  agg_grupo <- BD_Reservas %>% group_by(`Grupo de act`) %>%
    summarize(EI = sum(EI))
  
  df_grupo <- as.data.frame(agg_grupo, stringsAsFactors=FALSE)
  df_grupo$`Grupo de act` <- nombres_grupos
  df_grupo$relative_EI <- round((df_grupo$EI/sum(df_grupo$EI))*100, digits=0)
  df_grupo$ymax = cumsum(df_grupo$relative_EI)
  df_grupo$ymin = c(0, head(df_grupo$ymax, n=-1))
  
  label_position <- (df_grupo$ymin + df_grupo$ymax)/2
  
  grupo_plot <- ggplot(df_grupo, aes(ymax = ymax, ymin=ymin, xmax=4, xmin=2, x = "", y=relative_EI, fill=`Grupo de act`)) +
    geom_rect() +
    coord_polar("y", start=0) +
    geom_label(aes(label=paste(df_grupo$relative_EI,"%"), x=3, y=label_position, fill=df_grupo$`Grupo de act`[[5]]), size = 7, inherit.aes=TRUE, show.legend=FALSE) +
    scale_fill_brewer(palette=1) +
    theme_void() +
    ggtitle(per_text) + 
    theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5), legend.text = element_text(size=20))
      
  if(vencida==FALSE & is.null(per_1)) ggsave(plot = grupo_plot, filename="graphs/grupo_sin_vencida.png", width = 10, height = 6, dpi = 300)
  else if (vencida == TRUE & is.null(per_1) == FALSE) ggsave(plot = grupo_plot  + theme(legend.position = "none"), filename="graphs/grupo_t-1.png", width = 10, height = 6, dpi = 300)
  else if (vencida == TRUE & is.null(per_1)) ggsave(plot = grupo_plot + theme(legend.position = "none"), filename="graphs/grupo.png", width = 10, height = 6, dpi = 300)
  
}

gen_graph_cartera_plazos <- function(per){
  df_plazos <- gen_table_cartera_plazos(per, return_df=TRUE)
  agg_plazos <- colSums(df_plazos[,-1])
  df_agg_plazos <- as.data.frame(agg_plazos, stringsAsfactors=FALSE)
  df_agg_plazos$Plazos <- row.names(df_agg_plazos)
  row.names(df_agg_plazos) <- NULL
  
  df_agg_plazos$Plazos <- factor(df_agg_plazos$Plazos, levels = df_agg_plazos$Plazos)
  
  plot_cartera_plazos <- ggplot(df_agg_plazos, aes(x = Plazos, y=agg_plazos)) + geom_col(fill="dodgerblue4") + ylab("Exposición") + theme_bw() +
    geom_text(aes(y = agg_plazos, label = agg_plazos), vjust=-1, color="gray15", size=3.5)
  
  ggsave(plot = plot_cartera_plazos, filename="graphs/cartera_plazos.png", width = 10, height = 6, dpi = 300)
  
  #Generar un ggplot en lugar de barplot para poder guardar como .png y mostrar en la presentación
}

gen_graph_gerencias_tipo <- function(per){
  #Gráfica muy coqueta con las exposiciones por gerencias agrupadas por tipo de crédito
}

gen_graph_pi_calificacion_reservas <- function(per){
  resumen <- Query_Reservas %>% filter(Estatus == "Vigente" & Periodo == as.character(per))
  resumen <- resumen[,c("IC_Principal","Nombre_RazonSocial_ICPrincipal", "PI",
                        "Reserva","Calificacion")]
  agg_resumen <- resumen %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              PI = mean(as.numeric(PI)),
              Reserva = sum(as.numeric(Reserva)),
              Calificacion = first(Calificacion))
  
  df_resumen <- data.frame("Acreditado" = agg_resumen$Nombre,
                           "Probabilidad Incumplimiento" = round(agg_resumen$PI*100, digits=2),
                           "Reserva" = round(agg_resumen$Reserva/1000000,2),
                           "Calificación" = agg_resumen$Calificacion,
                           stringsAsFactors = FALSE, check.names = FALSE)
  
  plot_resumen2 <- ggplot(df_resumen, aes(x = `Calificación`, y = `Probabilidad Incumplimiento`)) + theme_bw()+
    geom_point(aes(size = `Reserva`, color = `Calificación`), alpha = 0.5) + scale_size(range = c(1,50)) +
    geom_text_repel(aes(label = `Acreditado`), data = filter(df_resumen, Reserva>3|`Probabilidad Incumplimiento`>19), point.padding = NA) +
    guides(colour = guide_legend(override.aes = list(size=10)), size = FALSE) + 
    scale_color_manual(values = c("#257A24", "#3C8321", "#5A8C1E", "#7E951A", "#9E9315", "#A87310", "#B14A0A", "#BA1903", "#BF0003")) +
    theme(text = element_text(size = 10), axis.text = element_text(size = 10), legend.position = "none")
  plot_resumen2
  
  ggsave(plot = plot_resumen2, filename="graphs/pi_calificacion_reservas.png", width = 10, height = 6, dpi = 300)
  
}

gen_graph_pi_calificacion_reservas2 <- function(per){
  resumen <- Query_Reservas %>% filter(Estatus == "Vigente" & Periodo == as.character(per))
  resumen <- resumen[,c("IC_Principal","Nombre_RazonSocial_ICPrincipal", "PI",
                        "Reserva","Calificacion")]
  agg_resumen <- resumen %>% group_by(IC_Principal) %>%
    summarize(Nombre = first(nombre_corto(Nombre_RazonSocial_ICPrincipal)),
              PI = mean(as.numeric(PI)),
              Reserva = sum(as.numeric(Reserva)),
              Calificacion = first(Calificacion))
  
  df_resumen <- data.frame("Acreditado" = agg_resumen$Nombre,
                           "Probabilidad Incumplimiento" = round(agg_resumen$PI*100, digits=2),
                           "Reserva" = round(agg_resumen$Reserva/1000000,2),
                           "Calificación" = agg_resumen$Calificacion,
                           stringsAsFactors = FALSE, check.names = FALSE)
  
  plot_resumen3 <- ggplot(df_resumen, aes(x = `Probabilidad Incumplimiento`, y = `Reserva`)) + theme_bw() +
    geom_rect(data = df_resumen[1,], xmin = 0, xmax = 20, ymin = 0, ymax = 30, fill = "green4", alpha = 0.6) +
    geom_rect(data = df_resumen[1,], xmin = 20, xmax = 100, ymin = 0, ymax = 30, fill = "orangered3", alpha = 0.6) +
    geom_rect(data = df_resumen[1,], xmin = 0, xmax = 20, ymin = 30, ymax = 120, fill = "goldenrod3", alpha = 0.6) +
    geom_rect(data = df_resumen[1,], xmin = 20, xmax = 100, ymin = 30, ymax = 120, fill = "red3", alpha = 0.6) +
    geom_point(color = "gray1", size = 3) + xlim(0, 100) + ylim(0,120) +
    geom_text_repel(aes(label = `Acreditado`), data = filter(df_resumen, Reserva>30|`Probabilidad Incumplimiento`>20)) + 
    theme(text = element_text(size = 10), axis.text = element_text(size = 10))
  plot_resumen3

  
  ggsave(plot = plot_resumen3, filename="graphs/pi_calificacion_reservas2.png", width = 10, height = 6, dpi = 300)
  
}
