#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Fundamentos y representación del tiempo   #
# Tema: Series de tiempo                            #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 02                                        #
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
# Ejemplo con datos de FRED mediante quantmod   #
# nota: No se usa ningun archivo                #
#                                               #
#===============================================#

# Cargar datos desde FRED con quantmod ----

# Si da problemas: 

# setSymbolLookup(GDPC1 = list(src = "FRED"))

# Si lo anterior no funciona :

# options(download.file.method = "libcurl")

GDPR <- getSymbols(
  Symbols = "GDPC1", 
  env = NULL,
  src = "FRED"
)

## Transformar xts a data.frame ----

GDPR_df <- data.frame(
  "Fecha" = index(GDPR),
  coredata(GDPR)
)

## Transformar directamente a ts -----

GDPR_ts <- ts_ts(GDPR)


#===============================================#
#                                               #
# Ejemplo con datos de FRED mediante archivo    #
#                                               #
#===============================================#


# Cargar desde csv ------

GDPR2 <- read.csv("Bases de datos/GDPC1.csv")

## Transformación de la fecha de carácter a date ----

GDPR2$observation_date <- as.Date(
  GDPR2$observation_date,
  format = "%Y-%m-%d"
)

## Transformación a xts ----

GDPR_xts <- xts(x = GDPR2$GDPC1,
                order.by = GDPR2$observation_date)

## Transformación a ts -----

GDPR_ts2 <- ts_ts(GDPR_xts)

## Manualmente con ts -----

GDPR_ts2 <- ts(data = GDPR2$GDPC1,
               start = 1947,
               end = 2025,
               frequency = 4)

