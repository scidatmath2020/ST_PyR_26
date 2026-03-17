#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estructura y dependencia temporal         #
# Tema: Dependencia temporal, ACF y PACF            #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 10                                        #
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

## Producto Interno Bruto real (no desestacionalizado) -----

PIB_2018 <- Series_INEGI_BIE(
  id_serie = 735879,
  token = token,
  periodo = c("Trimestral")
)


# Visualización inicial de la serie de tiempo -----

PIB_2018 %>% 
  ggplot(aes(x = Fecha, y = Serie))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(breaks = seq(8000000, (max(PIB_2018$Serie)+100),
                                  1000000),
                     limits = c(8000000,(max(PIB_2018$Serie)+1000000)),
                     labels = comma)+
  labs(title = "Producto Interno Bruto a nivel nacional",
       subtitle = "a precios del 2018, trimestral\n1980-2025",
       caption = "Fuente. INEGI. Banco de Información Económica (BIE).",
       x = "Año",
       y = "PIB\n(millones de pesos)")+
  theme(axis.text.x = element_text(angle = 90))


# Declarar el objeto temporal -----

min(PIB_2018$Fecha)
max(PIB_2018$Fecha)

PIB_ts <- ts(PIB_2018$Serie,
             start = c(1980,1),
             end = c(2025,4),
             frequency = 4)


# Funciones de autocorrelación ----

## Simple ----

### Gráfico ----

Acf(PIB_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación simple",
       subtitle = "Variable: PIB",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla -----

ACF_obj <- Acf(PIB_ts, lag.max = 24, plot = F)
n <- length(PIB_ts)

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


## Parcial ----

### Gráfico ----

Pacf(PIB_ts, lag.max = 24, plot = F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  scale_y_continuous(breaks = seq(-1,1,0.1),
                     limits = c(-1,1))+
  labs(title = "Función de autocorrelación parcial",
       subtitle = "Variable: PIB",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica.")

### Tabla -----

PACF_obj <- Pacf(PIB_ts, lag.max = 24, plot = F)
n <- length(PIB_ts)

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

# Prueba de Ljung-Box -----

Box.test(PIB_ts, lag = 1, type = "Ljung-Box")
Box.test(PIB_ts, lag = 5, type = "Ljung-Box")
Box.test(PIB_ts, lag = 10, type = "Ljung-Box")
Box.test(PIB_ts, lag = 15, type = "Ljung-Box")
Box.test(PIB_ts, lag = 20, type = "Ljung-Box")
Box.test(PIB_ts, lag = 25, type = "Ljung-Box")





