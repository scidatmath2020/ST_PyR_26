#========================================================#
# API BANXICO e INEGI (solo para BIE)                    #
# Elaboro: Alexis Adonai Morales Alberto                 #
# Fecha: Marzo 2025                                      #
# Versión: 1.3                                           #
# Contacto: alexis.adonai.morales.a@gmail.com            #
#========================================================#

# Verificar si pacman está instalado en la versión R ----

if(!require("pacman", quietly = T)){
  install.packages("pacman", dependencies = TRUE)
}else{
  cat("El paquete pacman se encuentra instalado en tu versión de R")
}

# Llamado de paquetes básicos para el tema -----

pacman::p_load(
  "httr",
  "jsonlite",
  "tidyverse",
  "curl",
  "zoo",
  "xts",
  "tseries"
)


## Construcción de función API INEGI ----

Series_INEGI_BIE <- function(id_serie, token, 
                             periodo = c("Mensual", "Trimestral")){
  url_data <- paste0(
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
    id_serie, "/es/00/false/BIE-BISE/2.0/",
    token, "?type=json"
  )
  if(periodo == "Trimestral"){
    ## Llamando API 
    respuesta <- GET(url_data)
    datos <- content(respuesta, "text")
    flujodatos <- paste(datos, collapse = " ")
    
    ## Obtener lista de observaciones 
    flujodatos <- fromJSON(flujodatos)
    flujodatos <- flujodatos$Series
    datos2 <- 0;
    for(i in 1:length(flujodatos)){
      datos2[i] <- flujodatos[[i]]
    }
    datos2 <- as.data.frame(datos2[[10]])
    datos2 <- datos2 %>% 
      dplyr::select(TIME_PERIOD, OBS_VALUE) %>% 
      dplyr::rename(Fecha = TIME_PERIOD,
             Serie = OBS_VALUE) %>% 
      dplyr::mutate(Año = substr(Fecha, 1,4),
             Trim = substr(Fecha, 6,7),
             Trim = case_when(Trim=="01"~"01",
                              Trim=="02"~"04",
                              Trim=="03"~"07",
                              Trim=="04"~"10"),
             Fecha = paste0(Año,
                            "-",
                            Trim,
                            "-01"),
             Fecha = as.Date(Fecha, format="%Y-%m-%d"),
             Serie = as.numeric(Serie)) %>% 
      arrange(Fecha)
  }
  else{
    
  }
  if(periodo == "Mensual"){
    ## Llamando API 
    respuesta <- GET(url_data)
    datos <- content(respuesta, "text")
    flujodatos <- paste(datos, collapse = " ")
    
    ## Obtener lista de observaciones 
    flujodatos <- fromJSON(flujodatos)
    flujodatos <- flujodatos$Series
    datos2 <- 0;
    for(i in 1:length(flujodatos)){
      datos2[i] <- flujodatos[[i]]
    }
    datos2 <- as.data.frame(datos2[[10]])
    datos2 <- datos2 %>% 
      dplyr::select(TIME_PERIOD, OBS_VALUE) %>% 
      dplyr::rename(Fecha = TIME_PERIOD,
             Serie = OBS_VALUE) %>% 
      dplyr::mutate(Año = substr(Fecha, 1,4),
             Mes = substr(Fecha, 6,7),
             Fecha = paste0(Año,
                            "-",
                            Mes,
                            "-01"),
             Fecha = as.Date(Fecha, format="%Y-%m-%d"),
             Serie = as.numeric(Serie)) %>% 
      arrange(Fecha)
  }
  else{
    
  }
  datos2
}

### Ejemplo de importación de datos -----

# Enlace para obtener token
# https://www.inegi.org.mx/app/desarrolladores/generatoken/Usuarios/token_Verify

# token_id <- "5a6f32b3-e104-1ae8-8cc3-d4b2244e2993"
# 
# PIB_2013 <- Series_INEGI_BIE(
#   id_serie = 493911,
#   token = token_id,
#   periodo = "Trimestral"
# )
# 
# 
# PIB_2018 <- Series_INEGI_BIE(
#   id_serie = 735879,
#   token = token_id,
#   periodo = "Trimestral"
# )



# Crear función para conectar con API Banxico ----

## Código base ----

url <- "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SP74665/datos/2020-01-01/2023-01-01?token=16da0b2481c0a11aedb6719017362a7b44302787c35f05328de0b53f96ef6962"

Respuesta <- GET(url)
Datos <- content(Respuesta, "text", encoding = "latin1")
Datos <- paste(Datos)
Datos <- fromJSON(Datos)
Datos <- Datos[["bmx"]][["series"]][["datos"]][[1]]


## Programación de la función ----

BANXICO_api_series <- function(
    id_serie,
    inicio,
    final,
    token){
  ## Lectura de web
  url_API <- paste0(
    "https://www.banxico.org.mx/SieAPIRest/service/v1/series/",
    id_serie, "/datos/", inicio, "/", final, "?token=", token
  )
  ## Llamando info del API
  Respuesta <- GET(url_API)
  Datos <- content(Respuesta, "text", encoding = "latin1")
  Datos <- paste(Datos)
  ## Obteneción de la lista de observaciones
  Datos <- fromJSON(Datos)
  ## Manipulación de datos
  Datos <-   Datos <- Datos[["bmx"]][["series"]][["datos"]][[1]] %>%
    as.data.frame() %>% 
    rename(Fecha=fecha,
           Serie=dato) %>% 
    mutate(
      Año = substr(Fecha, 7,10),
      Mes = substr(Fecha, 4,5),
      Serie = as.numeric(Serie),
      Fecha = as.Date(Fecha, format = "%d/%m/%Y")
    ) %>% 
    dplyr::select(Fecha, Serie, Año, Mes)
  
  Datos
}

### Ejemplo ----

# Inflacion <- BANXICO_api_series(id_serie = "SP74665", 
#                          inicio = "1990-01-01",
#                          final = "2023-10-04",
#                          token = " b87a6d5384f76a1e233ef447a640560d089aa8b4603dbdea9b1d4087c9befb56")
# 
