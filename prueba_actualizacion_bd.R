#Actualizacion a la base de datos de información de Tesorería

#Importacion de la informacion en csv
dat <- read.csv("~/201908 Insumos Tesoreria CSV.csv", header = TRUE)

dat

#Conexion a la base de datos
con <- dbConnect(odbc::odbc(),
                 Driver   = "FreeTDS",
                 Server   = "172.17.5.80\\FFMMSSQL, 1433",
                 Database = "FIFOMI_RIESGOS",
                 UID      = "riesgos",
                 PWD      = "53Cre7@",
                 Port     = 1433)


dbGetQuery(con, "SET QUOTED_IDENTIFIER ON")

#CreaLaTabla
query <- dbWriteTable(con, "RSG_INSUMOS_TESORERIA", dat, row.names = NA, append = TRUE)

#Checamos que todo este ok
dbExistsTable(con, "RSG_INSUMOS_TESORERIA")

check <- dbGetQuery(con, "SELECT * FROM RSG_INSUMOS_TESORERIA")

check
