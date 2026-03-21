#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estacionariedad y modelos clásicos        #
# Tema: Análisis de estacionariedad                 #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 12                                        #
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
  "scales"
)

# Carga de script de R -----

source("Funciones/SIE_INEGI.R")

# Consulta de serie de tiempo desde API INEGI ----

## Asignación de token ----

token <- "af847734-746b-8eb8-f0e6-4070cc851e47" # Reemplazar por tu token asignado

## Importaciones de bienes y servicios (desestacionalizado) -----

IMP_2018 <- Series_INEGI_BIE(
  id_serie = 737459,
  token = token,
  periodo = c("Trimestral")
)


# Visualización inicial de la serie de tiempo -----

IMP_2018 %>% 
  ggplot(aes(x = Fecha, y = Serie))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(breaks = seq(2000000, (max(IMP_2018$Serie)+100),
                                  1000000),
                     limits = c(2000000,(max(IMP_2018$Serie)+1000000)),
                     labels = comma)+
  labs(title = "Importaciones de bienes y servicios",
       subtitle = "a precios del 2018, trimestral\n1993-2025",
       caption = "Fuente. INEGI. Banco de Información Económica (BIE).",
       x = "Año",
       y = "IMP\n(millones de pesos)")+
  theme(axis.text.x = element_text(angle = 90))


# Declarar el objeto temporal -----

min(IMP_2018$Fecha)
max(IMP_2018$Fecha)

IMP_ts <- ts(IMP_2018$Serie,
             start = c(1993,1),
             end = c(2025,3),
             frequency = 4)


# Análisis de estacionariedad en I(0) -----

## Análisis gráfico -----

#### Funciones de autocorrelación ----

##### Simple ----

### Gráfico 

Acf(IMP_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: IMP",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla 

ACF_obj <- Acf(IMP_ts, lag.max = 24, plot = F)
n <- length(IMP_ts)

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

Pacf(IMP_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: IMP",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla 

PACF_obj <- Pacf(IMP_ts, lag.max = 24, plot = F)
n <- length(IMP_ts)

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

adf.test(IMP_ts, alternative = "stationary")

#### Urca 

ADF <- ur.df(
  y = IMP_ts,
  type = "trend",
  lags = trunc((length(IMP_ts)-1)^(1/3)),
  selectlags = "AIC"
)

summary(ADF)

### Phillips-Perron ----

#### tseries 

pp.test(IMP_ts, 
        type = "Z(t_alpha)", 
        lshort = TRUE)

#### urca 

PP <- ur.pp(
  x = IMP_ts,
  type = "Z-tau",
  model = "trend",
  lags = "short"
)

summary(PP)

### KPSS ----

#### tseries 

kpss.test(IMP_ts,
          null = "Level",
          lshort = TRUE)

#### Urca 

KPSS = ur.kpss(
  y = IMP_ts,
  type = "mu",
  lags = "short"
)

summary(KPSS)

# Proceso de primera diferencia I(1) usando logarítmos ----

## Cálculando en dataframe ----

IMP_2018 <- IMP_2018 %>% 
  mutate(TC = c(NA, diff(log(Serie))))

mean(IMP_2018$TC, na.rm=T)

## En objeto ts ----

IMP_tc_ts <- diff(log(IMP_ts))

# Visualización inicial de la serie de tiempo en I(1) log -----

IMP_2018 %>% 
  ggplot(aes(x = Fecha, y = TC))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = percent)+
  labs(title = "Tasa de crecimiento de las importaciones de bienes y servicios",
       subtitle = "porcentaje, trimestral\n1993-2025",
       caption = "Fuente. INEGI. Banco de Información Económica (BIE).",
       x = "Año",
       y = "Porcentaje")+
  theme(axis.text.x = element_text(angle = 90))


# Análisis de estacionariedad en I(1) -----

## Análisis gráfico -----

#### Funciones de autocorrelación ----

##### Simple ----

### Gráfico 

Acf(IMP_tc_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: IMP en I(1) log",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla 

ACF_obj <- Acf(IMP_tc_ts, lag.max = 24, plot = F)
n <- length(IMP_tc_ts)

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

Pacf(IMP_tc_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: IMP en I(1) log",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla 

PACF_obj <- Pacf(IMP_tc_ts, lag.max = 24, plot = F)
n <- length(IMP_tc_ts)

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

adf.test(IMP_tc_ts, alternative = "stationary")

#### Urca 

ADF <- ur.df(
  y = IMP_tc_ts,
  type = "none",
  lags = trunc((length(IMP_tc_ts)-1)^(1/3)),
  selectlags = "AIC"
)

summary(ADF)

### Phillips-Perron ----

#### tseries 

pp.test(IMP_tc_ts, 
        type = "Z(t_alpha)", 
        lshort = TRUE)

#### urca 

PP <- ur.pp(
  x = IMP_tc_ts,
  type = "Z-tau",
  model = "constant",
  lags = "short"
)

summary(PP)

### KPSS ----

#### tseries 

kpss.test(IMP_tc_ts,
          null = "Level",
          lshort = TRUE)

#### Urca 

KPSS = ur.kpss(
  y = IMP_tc_ts,
  type = "mu",
  lags = "short"
)

summary(KPSS)
