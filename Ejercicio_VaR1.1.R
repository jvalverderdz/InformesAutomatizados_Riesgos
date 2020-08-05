#Ejercicio cálculo VaR para 5 portafolios

#Importación de librerías
library(dplyr)

#Establecer el directorio
setwd("/home/ecornejo/R/Ejercicio_VaR")
options(digits=9)

##================VARIABLES EXÓGENAS=====================
#Monto total
monto <- 5000000
#Participacion de inversion
w <- c(0.2,0.2,0.2,0.2,0.2)
#Montos de inversion
s <- c(monto*w[[1]], monto*w[[2]], monto*w[[3]], monto*w[[4]], monto*w[[5]])
#Nivel de confianza
alfa <- 0.99
#Estadístico z de la normal
f = qnorm(0.99,mean=0,sd=1)
#Variable t
t <- 3

#==================CÁLCULOS INICIALES=====================================
portafolio <- data.frame("nombre" = c("Apple", "Bank of America", "Coca-Cola", "IBM", "Walmart"),
                         "weight" = w,
                         "montos" = s)

#Importar los archivos csv de las series históricas
AAPL_df <- read.csv("AAPL_Hist.csv", stringsAsFactors = FALSE)
AAPL_df$Date <- as.Date(AAPL_df$Date, format = "%d/%m/%Y")
BAC_df <- read.csv("BAC_Hist.csv", stringsAsFactors = FALSE)
BAC_df$Date <- as.Date(BAC_df$Date, format = "%d/%m/%Y")
KO_df <- read.csv("KO_Hist.csv", stringsAsFactors = FALSE)
KO_df$Date <- as.Date(KO_df$Date, format = "%d/%m/%Y")
WMT_df <- read.csv("WMT_Hist.csv", stringsAsFactors = FALSE)
WMT_df$Date <- as.Date(WMT_df$Date, format = "%d/%m/%Y")
IBM_df <- read.csv("IBM_Hist.csv", stringsAsFactors = FALSE)
IBM_df$Date <- as.Date(IBM_df$Date, format = "%d/%m/%Y")

date_0 <- as.Date("14/02/2019", format = "%d/%m/%Y")

#Ajustar las fechas para todos los instrumentos para el periodo deseado
AAPL_df <- filter(AAPL_df, Date > date_0)
BAC_df <- filter(BAC_df, Date > date_0)
KO_df <- filter(KO_df, Date > date_0)
WMT_df <- filter(WMT_df, Date > date_0)
IBM_df <- filter(IBM_df, Date > date_0)

#Definir los vectores con las series de precios
aapl <- as.numeric(AAPL_df$Close)
bac <- as.numeric(BAC_df$Close)
ko <- as.numeric(KO_df$Close)
wmt <- as.numeric(WMT_df$Close)
ibm <- as.numeric(IBM_df$Close)

#Definir un df con los precios
prices <- data.frame("aapl" = aapl, "bac" = bac, "ko" = ko, "ibm" = ibm, "wmt" = wmt)

#Calcular un df con los rendimientos
yields = data.frame("aapl_r" = ( (aapl / lag(aapl) ) - 1),
                    "bac_r" = ( (bac / lag(bac) ) - 1),
                    "ko_r" = ( (ko / lag(ko) ) - 1),
                    "ibm_r" = ( (ibm / lag(ibm) ) - 1),
                    "wmt_r" = ( (wmt / lag(wmt) ) - 1))


#Eliminar el renglón nulo
yields <- na.omit(yields)

#Volatilidad de los rendimientos diarios
volat <- sapply(yields, sd)
portafolio$volat <- volat

#Calcular la matriz de varianzas-covarianzas anualizado
sigma <- cov(yields)*252

#Rendimiento medio diario de cada instrumento
portafolio$yields <- colMeans(yields)

#Rendimiento, varianza y sd anualizados
portafolio$yields_y <- portafolio$yields * 252
portafolio$variance_y <- sapply(yields, var) * 252
portafolio$volat_y <- portafolio$volat * sqrt(252)


#================CÁLCULO DEL VAR==========================
#Calculo del VaR individual diario
portafolio$VaR <- f * portafolio$montos * portafolio$volat * sqrt(t)

#Cálculo del VaR de portafolio anual para 3 meses
volat_portafolio <- sqrt(t(w) %*% sigma %*% w)

VaR_portafolio <- f * monto * volat_portafolio*sqrt(0.25)


#Rendimiento esperado anual
yield_exp <- sum(portafolio$yields_y*portafolio$weight)


#===============Ejercicio de Generación de la frontera eficiente para Apple y BoA======================
W_1 <- rep(0, nrow(portafolio))
frontera_eficiente <- data.frame("yield" = numeric(), "volat" = numeric())
R_1 <- portafolio$yields_y
w1 <- 2.01
sigma_1 <- cov(yields[c("aapl_r", "bac_r")])

for(i in c(1:200)){
  w1 <- w1 - 0.01
  w2 <- 1 - w1
  W_1 <- c(w1, w2)
  volat <- t(W_1) %*% (sigma_1 %*% W_1)
  yield <- W_1 %*% R_1
  
  frontera_eficiente <- rbind(frontera_eficiente, c(w1, w2, yield, volat))
}
names(frontera_eficiente) <- c("w1", "w2", "rendimiento", "volatilidad")

#Gráfica de la curva de eficiencia
front_efic_plot <- ggplot(frontera_eficiente, aes(x = volatilidad, y = rendimiento)) + geom_point()


#==============PORTAFOLIO DE MÍNIMA VARIANZA Y MÁXIMO RENDIMIENTO=============================
#Obtenemos la inversa de la matriz de varianzas y covarianzas
inv_sigma <- solve(sigma)
#Definimos el vector R de rendimientos anualizados
R <- portafolio$yields_y

#Generamos el vector de unos
vector_1 <- matrix(1, nrow = nrow(portafolio), ncol = 1)

#Multiplicamos la inversa de sigma por el vector de unos
inv_sigma_1 <- inv_sigma %*% vector_1

#Multiplicamos la transpuesta del vector de unos por lo anterior y lo volvemos escalar
A <- as.numeric(t(vector_1) %*% inv_sigma_1)

#Dividimos inv_vc_1 entre t_1_inv_vc_1 y obtenemos el vector de pesos de minima varianza
W_min_var <- inv_sigma_1 / A

#Cálculo del portafolio de máximo rendimiento
#Multiplicamos la inversa de matriz var-cov por el vector de rendimientos anualizados
inv_sigma_r <- inv_sigma %*% R

#Multiplicamos la transpuesta del vector de unos por lo anterior y lo volvemos escalar
B <- as.numeric(t(vector_1) %*% (inv_sigma %*% R))

#Dividimos los elementos de inv_sigma_r entre B y obtenemos el vector de pesos de maximo rendimiento
W_max_r <- inv_sigma_r / B


#===============FRONTERA DE EFICIENCIA========================
#Calculamos C
C <- as.numeric(t(R) %*% inv_sigma %*% R)

#Generamos lambda_R
#Definimos la función a aplicar para cada r*
lambda_r_i <- function(r_star_i){
  ((C - B*r_star_i) * A ) / (A * C - B^2)
}

#Definimos el vector de r* que queremos explorar
r_star <- seq(-1, 1, by=0.01)

lambda_r <- sapply(r_star, lambda_r_i)

#Dado el vector de lambda_R, para cada lambda_R calculamos el vector de pesos y su riesgo con un for

#Generamos el dataframe donde va a estar toda la informacion
ef_fr <- data.frame("rendimiento" = r_star, "lambda_r" = lambda_r, "complemento lambda_r" = 1 - lambda_r)

#Una matriz de pesos, las columnas son los instrumentos y los renglones son cada rendimiento
W_ef <- data.frame(matrix(0, ncol = nrow(portafolio), nrow = nrow(ef_fr)))

#Cambiamos los nombres de dicha matriz por el numero de peso que es
for (n in 1:nrow(portafolio)) names(W_ef)[[n]] <- paste0("w_", n)

#Un vector de volatilidad donde se agregaran los valores calculados, que mas tarde agregaremos al dataframe previo
sd_p <- c()

#Por cada rendimiento i:
for (i in 1:nrow(ef_fr)){
  #El vector de pesos para ese peso esta dado por esta formula
  W_i <- lambda_r[[i]]*W_min_var + (1-lambda_r[[i]])* W_max_r
  #Y se calcula la desviación dado ese vector de pesos
  sd_i <- t(W_i) %*% (sigma %*%W_i)
  
  #Al renglón i de la matriz se le asigna el vector de pesos para el rendimiento i
  W_ef[i,] <- W_i
  #Y a la posición i del vector de volatilidad se le asigna el escalar de la sd para el rendimiento i
  sd_p[[i]] <- sd_i
}

#Al dataframe generado al inicio se le agregan las columnas de los pesos
ef_fr <- cbind(ef_fr, W_ef)

#Y se le agrega la columna de la volatilidad
ef_fr$volatilidad <- sd_p

#Ahora podemos generar nuestra grafiquita
ef_fr_graph <- ggplot(ef_fr, aes(x = volatilidad, y = rendimiento)) + geom_point()
ef_fr_graph

r_max_r <- R %*% W_max_r
r_min_var <- R %*% W_min_var

volat_max_r <- t(W_max_r) %*% sigma %*% W_max_r
volat_min_var <- t(W_min_var) %*% sigma %*% W_min_var


ef_fr_graph <- ggplot(ef_fr, aes(x = volatilidad, y = rendimiento)) + geom_point() +
  geom_point(aes(x = volat_max_r, y = r_max_r), color = "red3", size = 3) +
  geom_point(aes(x = volat_min_var, y = r_min_var), color = "steelblue2", size = 3) +
  ylim(0,1)
ef_fr_graph

##Calculo del VaR para ambos portafolios para 3 meses
VaR_max_r <- f * monto * volat_max_r * sqrt(0.25)
VaR_min_var <- f * monto * volat_min_var * sqrt(0.25)


#===========CÁLCULO VAR NO PARAMÉTRICO: HISTÓRICO===========================

#Tomamos el ultimo precio observado
prices_T <- prices[nrow(prices),]

#Calculamos los vectores de rendimientos esperados a partir de los rendimientos observados
prices_hat <- data.frame()

for(t in 1:nrow(yields)) prices_hat <- rbind(prices_hat, prices_T*exp(yields[t,]/360))

#Calculo de la ganancia/perdida diaria estimada
pi_hat <- data.frame()

for(t in 2:nrow(prices_hat)) pi_hat <- rbind(pi_hat, prices_hat[t,] - prices_hat[t-1,])

VaR_hist <- sapply(pi_hat, quantile, 0.95)
names(VaR_hist) <- names(prices)

#===========CÁLCULO VAR NO PARAMÉTRICO: MONTE CARLO===========================

#Importar las bases historicas con mas observaciones
AAPL_hist <- read.csv("AAPL_Hist.csv", stringsAsFactors = FALSE)
AAPL_hist$Date <- as.Date(AAPL_hist$Date, format = "%d/%m/%Y")
BAC_hist <- read.csv("BAC_Hist.csv", stringsAsFactors = FALSE)
BAC_hist$Date <- as.Date(BAC_hist$Date, format = "%d/%m/%Y")
KO_hist <- read.csv("KO_Hist.csv", stringsAsFactors = FALSE)
KO_hist$Date <- as.Date(KO_hist$Date, format = "%d/%m/%Y")
WMT_hist <- read.csv("WMT_Hist.csv", stringsAsFactors = FALSE)
WMT_hist$Date <- as.Date(WMT_hist$Date, format = "%d/%m/%Y")
IBM_hist <- read.csv("IBM_Hist.csv", stringsAsFactors = FALSE)
IBM_hist$Date <- as.Date(IBM_hist$Date, format = "%d/%m/%Y")

#Como Apple es el que tiene menos observaciones, construimos a partir de ahi
date_N <- min(AAPL_hist$Date)

#Ajustar las fechas para el periodo a partir del cual Apple tiene observaciones
AAPL_hist <- filter(AAPL_hist, Date >= date_N)
BAC_hist <- filter(BAC_hist, Date >= date_N)
KO_hist <- filter(KO_hist, Date >= date_N)
WMT_hist <- filter(WMT_hist, Date >= date_N)
IBM_hist <- filter(IBM_hist, Date >= date_N)


#Calcular el df con los rendimientos para todas las observaciones historicas
#Definir los vectores con las series de precios
aapl_h <- as.numeric(AAPL_hist$Close)
bac_h <- as.numeric(BAC_hist$Close)
ko_h <- as.numeric(KO_hist$Close)
wmt_h <- as.numeric(WMT_hist$Close)
ibm_h <- as.numeric(IBM_hist$Close)

prices_h <- data.frame("aapl" = aapl_h, "bac" = bac_h, "ko" = ko_h,
                       "wmt" = wmt_h, "ibm" = ibm_h)
#Sustituimos el NA por la media
aapl_h[is.na(aapl_h)] <- mean(aapl_h, na.rm = TRUE)

#Calcular un df con los rendimientos historicos
yields_h = data.frame("aapl_r" = ( (aapl_h / lag(aapl_h) ) - 1),
                    "bac_r" = ( (bac_h / lag(bac_h) ) - 1),
                    "ko_r" = ( (ko_h / lag(ko_h) ) - 1),
                    "ibm_r" = ( (ibm_h / lag(ibm_h) ) - 1),
                    "wmt_r" = ( (wmt_h / lag(wmt_h) ) - 1))

#Eliminar el renglón nulo
yields_h <- na.omit(yields_h)


#Numero de observaciones/simulaciones
N <- nrow(yields_h)
K <- length(yields_h)

#Generar las casi 10 mil simulaciones aleatorias de los rendimientos para cada instrumento,
#una por cada precio real
random_yields <- data.frame("aapl" = rnorm(N), "bac" = rnorm(N),
                            "ko" = rnorm(N), "ibm" = rnorm(N), "wmt" = rnorm(N))


#Calcular la matriz de varianzas covarianzas de los rendimientos historicos observados
V = cov(yields_h)

#Calcular matriz de cholesky observada lower triangular
V_chol <- t(chol(V)) #chol() devuelve upper triangular. Transponer para obtener la lower triangular

#Calculo de la matriz de varianzas covarianzas de las simulaciones, cholesky lower triangular  y inversa
Vrandom <- cov(random_yields)
Vrandom_chol <- t(chol(Vrandom))  #chol() devuelve una matriz upper triangular. transponer para obtener la lower triangular
Vrandom_chol_inv <- solve(Vrandom_chol)

#Calculo del ajuste por media
ajuste_media <- as.data.frame(matrix(ncol = K, nrow =N))
for(k in 1:K){
  mean_k <- mean(random_yields[,k])
  ajuste_media[,k] <- random_yields[,k] - mean_k
}
names(ajuste_media) <- names(random_yields)

#Para comprobar, cada columna debe tener media cercana a cero
for(k in 1:K) print(mean(ajuste_media[,k]))

#Calculo del ajuste por covarianza
ajuste_covarianza <- as.data.frame(matrix(ncol = K, nrow = N))
for(n in 1:N){
  ajuste_covarianza[n,] <- as.matrix(ajuste_media[n,]) %*% t(Vrandom_chol_inv)
}

#Vectores correlacionados (rendimientos diarios)
vectores_cor <- as.data.frame(matrix(ncol = K, nrow = N))
for(n in 1:N){
  vectores_cor[n,] <- as.matrix(ajuste_covarianza[n,]) %*% V_chol
}

#Para comprobar que todo esta bien, la matriz de covarianzas de los vectores correlacionados
#debe ser similar a la observada
V_vcor <- var(vectores_cor)

V - V_vcor

prices_N <- prices_h[nrow(prices_h),]

prices_est <- as.data.frame(matrix(ncol = K, nrow = N))
pi_est <- prices_est
for(k in 1:K){
  for(n in 1:N){
    price_nk <- prices_N[[k]]*exp(vectores_cor[[n,k]]/252)
    prices_est[n,k] <- price_nk
    pi_est[n,k] <- prices_N[[k]] - price_nk
  }
}

VaR_montecarlo <- sapply(pi_est, quantile, 0.95)
names(VaR_montecarlo) <- names(prices)

VaR_hist
VaR_montecarlo