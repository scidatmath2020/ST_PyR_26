#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estacionariedad y modelos clásicos        #
# Tema: Modelo ARIMA                                #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 14                                        #
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
  "scales",
  "fDMA",
  "lmtest",
  "nortest",
  "cowplot"
)

# Cargar datos desde CSV ----

IDFCE <- read.csv("Bases de datos/Estatal-Delitos-2015-2025_feb2026.csv",
                  check.names = F,
                  encoding = "latin1")

# Transformación de los datos para convertir en formato largo ------

Meses <- data.frame(
  Mes_texto = names(IDFCE)[8:19],
  Mes_num = 1:12
)

Robos <- IDFCE %>% 
  filter(`Tipo de delito` == "Robo") %>% 
  select(all_of(c(
    "Año",
    "Entidad",
    Meses$Mes_texto
  ))) %>% 
  pivot_longer(cols = -c("Año", "Entidad"),
               names_to = "Mes_texto",
               values_to = "Robos") %>% 
  left_join(Meses,
            by = c("Mes_texto")) %>% 
  mutate(Mes_num = sprintf("%02d", Mes_num),
         Fecha = as.Date(paste0(Año, "-", Mes_num, "-01"),
                         format = "%Y-%m-%d")) %>% 
  group_by(Fecha) %>% 
  summarise(Robos = sum(Robos, na.rm = FALSE)) %>% 
  ungroup()



# Visualización inicial de la Robos de tiempo -----

Robos %>% 
  ggplot(aes(x = Fecha, y = Robos))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(breaks = seq(25000, (max(Robos$Robos)+100),
                                  5000),
                     limits = c(25000,(max(Robos$Robos)+100)),
                     labels = comma)+
  labs(title = "Cantidad de robos registrados en las Carpetas de Investigación iniciadas",
       subtitle = "número de carpetas, mensual\n2015-2025",
       caption = "Fuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.",
       x = "Año",
       y = "Carpetas de Investigación")


# Declarar el objeto temporal -----

min(Robos$Fecha)
max(Robos$Fecha)

Robos_ts <- ts(Robos$Robos,
             start = c(2015,1),
             end = c(2025,12),
             frequency = 12)


# Análisis de estacionariedad en I(0) -----

## Análisis gráfico -----

#### Funciones de autocorrelación ----

##### Simple ----

### Gráfico 

Acf(Robos_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: Robos",
       caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.")

### Tabla 

ACF_obj <- Acf(Robos_ts, lag.max = 24, plot = F)
n <- length(Robos_ts)

ACF_tab <- data.frame(
  "Rezago" = ACF_obj$lag,
  "Rho" = ACF_obj$acf
) %>% 
  filter(Rezago > 0) %>% 
  mutate(
    t_stat = Rho*sqrt(n-Rezago)/sqrt(1-Rho^2),
    pvalue = format(
      2*(1-pt(abs(t_stat), df = n-Rezago-2)),
      nsmall=4
    )
  )


#### Parcial ----

### Gráfico 

Pacf(Robos_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: Robos",
       caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.")

### Tabla 

PACF_obj <- Pacf(Robos_ts, lag.max = 24, plot = F)
n <- length(Robos_ts)

PACF_tab <- data.frame(
  "Rezago" = PACF_obj$lag,
  "PACF" = PACF_obj$acf
) %>% 
  filter(Rezago > 0) %>% 
  mutate(
    t_stat = PACF*sqrt(n-Rezago)/sqrt(1-PACF^2),
    pvalue = round(
      2*(1-pt(abs(t_stat), df = n-Rezago-2)),
      4
    )
  )

## Análisis estadístico -----

### Dickey-Fuller Aumentada (ADF) ----

#### Con tseries 

adf.test(Robos_ts, alternative = "stationary")

#### Urca 

ADF <- ur.df(
  y = Robos_ts,
  type = "trend",
  lags = trunc((length(Robos_ts)-1)^(1/3)),
  selectlags = "AIC"
)

summary(ADF)

### Phillips-Perron ----

#### Con tseries

pp.test(Robos_ts, 
        type = "Z(t_alpha)", 
        lshort = TRUE)

#### urca 

PP <- ur.pp(
  x = Robos_ts,
  type = "Z-tau",
  model = "trend",
  lags = "short"
)

summary(PP)

### KPSS ----

#### Con tseries

kpss.test(Robos_ts,
          null = "Level",
          lshort = TRUE)

#### Urca 

KPSS = ur.kpss(
  y = Robos_ts,
  type = "mu",
  lags = "short"
)

summary(KPSS)

# Proceso de primera diferencia I(1) usando logarítmos ----

## Cálculando en dataframe ----

Robos <- Robos %>% 
  mutate(TC = c(NA, diff(log(Robos))))

mean(Robos$TC, na.rm=T)

## En objeto ts ----

Robo_tc_ts <- diff(log(Robos_ts))

# Visualización inicial de la Robos de tiempo en I(1) log -----

Robos %>% 
  ggplot(aes(x = Fecha, y = TC))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = percent)+
  labs(title = "Tasa de crecimiento de los robos registrados en las carpetas de investigación iniciadas",
       subtitle = "porcentaje, mensual\n2015-2025",
       caption = "Fuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.",
       x = "Año",
       y = "Porcentaje")+
  theme(axis.text.x = element_text(angle = 90))


# Análisis de estacionariedad en I(1) -----

## Análisis gráfico -----

#### Funciones de autocorrelación ----

##### Simple ----

### Gráfico 

Acf(Robo_tc_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: Robos en I(1) log",
       caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.")

### Tabla 

ACF_obj <- Acf(Robo_tc_ts, lag.max = 24, plot = F)
n <- length(Robo_tc_ts)

ACF_tab <- data.frame(
  "Rezago" = ACF_obj$lag,
  "Rho" = ACF_obj$acf
) %>% 
  filter(Rezago > 0) %>% 
  mutate(
    t_stat = Rho*sqrt(n-Rezago)/sqrt(1-Rho^2),
    pvalue = format(
      2*(1-pt(abs(t_stat), df = n-Rezago-2)),
      nsmall=4
    )
  )


#### Parcial ----

### Gráfico 

Pacf(Robo_tc_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: Robos en I(1) log",
       caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025.")

### Tabla 

PACF_obj <- Pacf(Robo_tc_ts, lag.max = 24, plot = F)
n <- length(Robo_tc_ts)

PACF_tab <- data.frame(
  "Rezago" = PACF_obj$lag,
  "PACF" = PACF_obj$acf
) %>% 
  filter(Rezago > 0) %>% 
  mutate(
    t_stat = PACF*sqrt(n-Rezago)/sqrt(1-PACF^2),
    pvalue = round(
      2*(1-pt(abs(t_stat), df = n-Rezago-2)),
      4
    )
  )

## Análisis estadístico -----

### Dickey-Fuller Aumentada (ADF) ----

#### Con Con tseries

adf.test(Robo_tc_ts, alternative = "stationary")

#### Urca 

ADF <- ur.df(
  y = Robo_tc_ts,
  type = "none",
  lags = trunc((length(Robo_tc_ts)-1)^(1/3)),
  selectlags = "AIC"
)

summary(ADF)

### Phillips-Perron ----

#### Con tseries

pp.test(Robo_tc_ts, 
        type = "Z(t_alpha)", 
        lshort = TRUE)

#### urca 

PP <- ur.pp(
  x = Robo_tc_ts,
  type = "Z-tau",
  model = "constant",
  lags = "short"
)

summary(PP)

### KPSS ----

#### Con tseries

kpss.test(Robo_tc_ts,
          null = "Level",
          lshort = TRUE)

#### Urca 

KPSS = ur.kpss(
  y = Robo_tc_ts,
  type = "mu",
  lags = "short"
)

summary(KPSS)

# ¿Cuál puede ser el orden AR, MA? -----

plot_grid(

  Acf(Robo_tc_ts, lag.max = 24, plot = F) %>% 
    autoplot()+
    scale_x_continuous(breaks = seq(1,24,1))+
    scale_y_continuous(breaks = seq(-1,1,0.1),
                       limits = c(-0.5,0.5))+
    labs(title = "Función de autocorrelación simple",
         subtitle = "Variable: Robos en I(1) log",
         caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025."),
  Pacf(Robo_tc_ts, lag.max = 24, plot = F) %>% 
    autoplot()+
    scale_x_continuous(breaks = seq(1,24,1))+
    scale_y_continuous(breaks = seq(-1,1,0.1),
                       limits = c(-0.5,0.5))+
    labs(title = "Función de autocorrelación parcial",
         subtitle = "Variable: Robos en I(1) log",
         caption = "Elaboración propia\nFuente. SESNSP. Incidencia delictiva del fueron común estatal, metodología 2015-2025."),
  nrow = 2
)

# Estimaciones del modelo ARIMA con diferentes ordenes ----

## Definir ordenes ----

orden_p <- c(1,4)
orden_q <- c(1,2,4)
d <- 1

## Crear combinaciones -----

combinaciones <- expand.grid(p = orden_p,
                             q = orden_q)

## Dataframe de resultados ----

Resultados <- data.frame()

## Bucle/loop para estimación de los modelos -----

for(i in 1:nrow(combinaciones)){
  p_i <- combinaciones$p[i]
  q_i <- combinaciones$q[i]
  
  modelo <- tryCatch({
    arima(log(Robos_ts),
          order = c(p_i, d, q_i))
  }, error = function(e) NULL)
  
  if(!is.null(modelo)){
    aic_val <- AIC(modelo)
    residuales <- residuals(modelo)
    rmse_val <- sqrt(mean(residuales^2, na.rm=TRUE))
    Resultados <- rbind(
      Resultados,
      data.frame(
        p = p_i,
        d = d,
        q = q_i,
        AIC = aic_val,
        RMSE = rmse_val))
  }
}

## Consulta de mejor modelo ----

### AIC ----

Resultados %>% 
  arrange((AIC)) %>% 
  slice(1)

### RMSE ----

Resultados %>% 
  arrange(RMSE) %>% 
  slice(1)

## Reestimación ya con el orden adecuado ----

ARIMA <- arima(
  log(Robos_ts),
  order = c(4,1,4)
)

# Validación del modelo ----

## Prueba de coeficientes ------

coeftest(ARIMA)

## Circulo unitario -----

autoplot(ARIMA)

## Pruebas de autocorrelación -----

Box.test(ARIMA$residuals, lag = 1)
Box.test(ARIMA$residuals, lag = 5)
Box.test(ARIMA$residuals, lag = 10)
Box.test(ARIMA$residuals, lag = 12)
Box.test(ARIMA$residuals, lag = 15)
Box.test(ARIMA$residuals, lag = 24)

## Normalidad ----

jarque.bera.test(ARIMA$residuals)
ad.test(ARIMA$residuals)
lillie.test(ARIMA$residuals)

## Prueba de heterocedasticidad ----

archtest(ARIMA$residuals, lag = 1)
archtest(ARIMA$residuals, lag = 5)
archtest(ARIMA$residuals, lag = 10)
archtest(ARIMA$residuals, lag = 12)
archtest(ARIMA$residuals, lag = 24)

## Estacionariedad del residual -----

adf.test(ARIMA$residuals)
pp.test(ARIMA$residuals)
kpss.test(ARIMA$residuals)

# Pronóstico ----

Pronostico <- forecast(ARIMA, h = 12, level = 0.95)

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
autoplot(Pronostico, include = 50)
