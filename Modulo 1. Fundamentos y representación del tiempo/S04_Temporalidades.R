#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Fundamentos y representación del tiempo   #
# Tema: Operaciones con temporalidades              #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 04                                        #
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

options(timeout = 1000)

pacman::p_load(
  "tidyverse",
  "tseries",
  "quantmod",
  "zoo",
  "lubridate",
  "xts",
  "tsbox"
)

#===============================================#
#                                               #
# Ejemplo con datos de YF mediante quantmod     #
# Nota: No se usa ningun archivo                #
#                                               #
#===============================================#

# Cargar datos desde YF con quantmod ----

NFLX <- getSymbols(
  Symbols = "NFLX", 
  env = NULL,
  src = "yahoo",
  from = "2015-03-05",
  to = "2026-03-05"
)

## Transformar xts a data.frame ----

NFLX_df <- data.frame(
  "Fecha" = index(NFLX),
  coredata(NFLX)
)

# Comprobar formato de la columna fecha -----

class(NFLX_df$Fecha)

# Sustracciones de elementos o formatos de la fecha ----

# NOTA: PARA LOS SIGUIENTES PROCEDIMIENTOS, ES NECESARIO
# QUE EL FORMATO DE LA COLUMNA SEA "DATE" DE LO CONTRARIO
# LAS OPERACIONES QUE SE REALICEN A CONTINUACIÓN
# NO SE PODRÁN EFECTUAR 

# Operador pipe: %>% ctrl+shift+m

NFLX_df <- NFLX_df %>% 
  mutate(Año = format(Fecha, "%Y"), # Año en yyyy
         Año2g = format(Fecha, "%y"), # Año en yy
         Mes_num = format(Fecha, "%m"), # Mes numérico
         Mes_txt = format(Fecha, "%B"), # Mes texto,
         Mex_txt2 = format(Fecha, "%b"), #Mex texto abreviado,
         Dia_num = format(Fecha, "%d"), # Día del mes numérico,
         Dia_año = format(Fecha, "%j"), # Día del año
         Dia_txt = format(Fecha, "%A"), # Día texto
         Dia_txt2 = format(Fecha, "%a"), # Día texto abreviado
         Semana_D = format(Fecha, "%U"), # Semana numérica (incio en domingo)
         Semana_L = format(Fecha, "%W"), # Semana numérica (inicio en lunes)
         Trimestre = quarter(Fecha), # Trimestre numérico
         TrimestreQ = as.yearqtr(Fecha), # Trimestre formato yyyy Qq
         Semestre_num = semester(Fecha)) # Semestre numérico


## Exportar dataframe para posteriores usos ----

write.csv(x = NFLX_df,
          file = "Bases de datos/Datos_NFLX_temp.csv",
          fileEncoding = "latin1",
          row.names = F)
  
# Cálculos usando las temporalidades -----

## Cálculo de rendimientos o tasa de crecimiento -----

NFLX_df <- NFLX_df %>%
  mutate(TC_NFLX = c(NA, diff(log(NFLX.Adjusted))*100))

## Rendimientos promedio por mes ----

NFLX_df %>% 
  group_by(Año, Mes_num) %>% 
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T)) %>% 
  View()

## Rendimientos anualizados por cada año -----

NFLX_df %>% 
  group_by(Año) %>% 
  summarise(n())

NFLX_df %>% 
  group_by(Año) %>% 
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T),
            Dias_año = n()) %>% 
  ungroup() %>% 
  mutate(TC_NFLX = TC_NFLX*Dias_año)

## Rendimientos por días de la semana -----

NFLX_df %>% 
  group_by(Año, Dia_txt) %>% 
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Dia_txt = factor(Dia_txt, 
                          levels = c("lunes",
                                     "martes",
                                     "miércoles",
                                     "jueves",
                                     "viernes"))) %>% 
  arrange(Año, Dia_txt) %>% 
  pivot_wider(id_cols = c("Año"),
              names_from = c("Dia_txt"),
              values_from = c("TC_NFLX"))






