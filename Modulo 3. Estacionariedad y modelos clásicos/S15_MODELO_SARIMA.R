#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Modelos estacionales                      #
# Tema: Modelo SARIMA                               #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 15                                        #
# SciData                                           #
#===================================================#

# Limpieza de memoria o consola -----

rm(list = ls())

# Verificador de pacman ------

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else {
  install.packages("pacman", dependencies = T)
}

# Carga / instalación de paquetes necesarios ----

pacman::p_load(
  "tidyverse",
  "tseries",
  "urca",
  "forecast",
  "zoo",
  "lubridate",
  "xts",
  "tsbox",
  "seasonal",
  "uroot",
  "scales",
  "fDMA",
  "lmtest",
  "nortest",
  "cowplot"
)


# Carga de script de R -----

source("Funciones/SIE_INEGI.R")

# Consulta de serie de tiempo desde API INEGI ----

## Asignación de token ----

token <- "af847734-746b-8eb8-f0e6-4070cc851e47" # Reemplazar por tu token asignado

## Producto Interno Bruto real (no desestacionalizado) -----

PIB_2018 <- Series_INEGI_BIE(
  id_serie = 735879,
  token = token,
  periodo = c("Trimestral")
)


## Calcular la tasa de crecimiento -----

PIB_2018 <- PIB_2018 %>% 
  mutate(TC = c(NA,diff(log(Serie))))


# Visualización inicial de la serie de tiempo -----

PIB_2018 %>% 
  ggplot(aes(x = Fecha, y = TC))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = percent)+
  labs(title = "Tasa de crecimiento del Producto Interno Bruto a nivel nacional",
       subtitle = "crecimientos reales en porcentaje, trimestral\n1980-2025",
       caption = "Fuente. INEGI. Banco de Información Económica (BIE).",
       x = "Año",
       y = "Cambio porcentual")+
  theme(axis.text.x = element_text(angle = 90))


# Declarar el objeto temporal -----

min(PIB_2018$Fecha)
max(PIB_2018$Fecha)

PIB_tc_ts <- ts(na.omit(PIB_2018$TC),
                start = c(1980,2),
                end = c(2025,4),
                frequency = 4)

PIB_ts <- ts(na.omit(PIB_2018$Serie),
                start = c(1980,1),
                end = c(2025,4),
                frequency = 4)

# Visualización del ACF -----

Acf(PIB_tc_ts, lag.max = 24, plot=F) %>% 
  autoplot()+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: TC PIB",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

# Gráfico estacional -----

ggseasonplot(PIB_tc_ts,
             year.labels = T,
             year.labels.left = T)+
  scale_y_continuous(labels = percent)+
  labs(title = "Gráfico estacional de la tasa de crecimiento del PIB",
       subtitle = "De 1980 a 2025 trimestral",
       x = "Trimestre",
       y = "Tasa de crecimiento del PIB")


# Pruebas de estacionalidad -----

## Regresión auxiliar ----

modelo <- lm(PIB_tc_ts ~ factor(cycle(PIB_tc_ts)))
summary(modelo)

## Kruskal - Wallis -----

kruskal.test(PIB_tc_ts ~ factor(cycle(PIB_tc_ts)))

## Prueba de Hegy ----

hegy.test(PIB_tc_ts) # No se debe usar la diferencia

hegy.test(PIB_ts) # La forma correcta

## Prueba de Canova-Hansen -----

ch.test(PIB_tc_ts)

# Diferencia estacional -----

PIB_2018 <- PIB_2018 %>% 
  mutate(TC_anual = c(rep(NA, 4),
                      diff(log(Serie), lag = 4)))

PIB_tc_anual <- diff(log(PIB_ts), lag=4)


## Visualización de la serie en crecimiento anualizado -----

PIB_2018 %>% 
  ggplot(aes(x = Fecha, y = TC_anual))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = percent)+
  labs(title = "Tasa de crecimiento anualizada del Producto Interno Bruto a nivel nacional",
       subtitle = "crecimientos reales en porcentaje, trimestral\n1981-2025",
       caption = "Fuente. INEGI. Banco de Información Económica (BIE).",
       x = "Año",
       y = "Cambio porcentual anualizado")+
  theme(axis.text.x = element_text(angle = 90))


##  Visualización del ACF -----

Acf(PIB_tc_anual, lag.max = 24, plot=F) %>% 
  autoplot()+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: TC anual PIB",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")


##  Visualización del PACF -----

pacf(PIB_tc_anual, lag.max = 24, plot=F) %>% 
  autoplot()+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: TC anual PIB",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")


## Pruebas estadísticas ----

### ADF ----

adf.test(PIB_tc_anual)

### PP ----

pp.test(PIB_tc_anual)

### KPSS ----

kpss.test(PIB_tc_anual)

# Estimación del modelo ----

# P = 1,3-4
# Q = 1-2
# d = 1
# D = 0
# p = 1-3
# q = 1-3

Modelo <- auto.arima(
  log(PIB_ts), d = 1, D = 0,
  max.p = 3, max.q = 3, max.P = 4,
  max.Q = 2, max.order = 4,
  seasonal = T, trace = T, allowdrift = F,
  allowmean = F
)

# Validación del modelo estacional ----


## Prueba de coeficientes ------

coeftest(Modelo)

## Circulo unitario -----

autoplot(Modelo)

## Pruebas de autocorrelación -----

Box.test(Modelo$residuals, lag = 1)
Box.test(Modelo$residuals, lag = 5)
Box.test(Modelo$residuals, lag = 10)
Box.test(Modelo$residuals, lag = 15)
Box.test(Modelo$residuals, lag = 24)

## Normalidad ----

jarque.bera.test(Modelo$residuals)
ad.test(Modelo$residuals)
lillie.test(Modelo$residuals)

## Prueba de heterocedasticidad ----

archtest(Modelo$residuals, lag = 1)
archtest(Modelo$residuals, lag = 5)
archtest(Modelo$residuals, lag = 10)
archtest(Modelo$residuals, lag = 15)
archtest(Modelo$residuals, lag = 24)

## Estacionariedad del residual -----

adf.test(Modelo$residuals)
pp.test(Modelo$residuals)
kpss.test(Modelo$residuals)

# Pronóstico ----

Pronostico <- forecast(Modelo, h = 12, level = 0.95)

## Revertir logarítmos ----

# NOTA: RECUERDA QUE SE USA UNA DIFERENCIA LOGARÍTMICA
# POR LO TANTO EL PRONOSTICO DEVUELTO ESTA EN TERMINOS
# LOGARÍTMICOS

Pronostico$mean <- exp(Pronostico$mean)
Pronostico$upper <- exp(Pronostico$upper)
Pronostico$lower <- exp(Pronostico$lower)
Pronostico$x <- exp(Pronostico$x)
Pronostico$fitted <- exp(Pronostico$fitted)

Pronostico

## Gráfico ----

autoplot(Pronostico)
autoplot(Pronostico, include = 50)+
  scale_y_continuous(labels = comma)





