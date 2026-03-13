#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estructura y dependencia temporal         #
# Tema: Descomposición de series de tiempo          #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 07                                        #
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


## Descomposición temporal aditiva ----

### Básico ----

plot(decompose(PIB_ts))

### Intermedio -----

PIB_des <- data.frame(
  "Fecha" = PIB_2018$Fecha,
  "Observado" = PIB_2018$Serie,
  "Tendencia" = decompose(PIB_ts, 
                          type = "additive")$trend,
  "Estacional" = decompose(PIB_ts, 
                           type = "additive")$seasonal,
  "Aleatorio" = decompose(PIB_ts, 
                          type = "additive")$random
)


PIB_des %>% 
  pivot_longer(cols = -c("Fecha"),
               names_to = "Componente",
               values_to = "Valor") %>% 
  mutate(Componente = factor(Componente,
                             levels = c(
                               "Observado",
                               "Tendencia",
                               "Estacional",
                               "Aleatorio"
                             ))) %>% 
  ggplot(aes(x = Fecha, y = Valor))+
  geom_line(aes(color = Componente))+
  scale_x_date(date_breaks = "3 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = comma)+
  scale_color_manual(values = c(
    "Observado" = "blue4",
    "Tendencia" = "green4",
    "Estacional" = "orange4", 
    "Aleatorio"  = "red4"
  ))+
  facet_wrap(~Componente, scales = "free", nrow = 4)+
  labs(title = "Descomposición temporal del producto interno bruto",
       subtitle = "método aditivo a precios de 2018\n1980-2025",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica (BIE)",
       x = "",
       y = "")+
  theme(legend.position = "none")


## Descomposición temporal multiplicativa ----

### Intermedio -----

PIB_des_mul <- data.frame(
  "Fecha" = PIB_2018$Fecha,
  "Observado" = PIB_2018$Serie,
  "Tendencia" = decompose(PIB_ts, 
                          type = "multiplicative")$trend,
  "Estacional" = decompose(PIB_ts, 
                           type = "multiplicative")$seasonal,
  "Aleatorio" = decompose(PIB_ts, 
                          type = "multiplicative")$random
)


PIB_des_mul %>% 
  pivot_longer(cols = -c("Fecha"),
               names_to = "Componente",
               values_to = "Valor") %>% 
  mutate(Componente = factor(Componente,
                             levels = c(
                               "Observado",
                               "Tendencia",
                               "Estacional",
                               "Aleatorio"
                             ))) %>% 
  ggplot(aes(x = Fecha, y = Valor))+
  geom_line(aes(color = Componente))+
  scale_x_date(date_breaks = "3 year",
               date_labels = "%Y")+
  scale_y_continuous(labels = comma)+
  scale_color_manual(values = c(
    "Observado" = "blue4",
    "Tendencia" = "green4",
    "Estacional" = "orange4", 
    "Aleatorio"  = "red4"
  ))+
  facet_wrap(~Componente, scales = "free", nrow = 4)+
  labs(title = "Descomposición temporal del producto interno bruto",
       subtitle = "método multiplicativo a precios de 2018 y multiplicadores\n1980-2025",
       caption = "Elaboración propia\nFuente. INEGI. Banco de Información Económica (BIE)",
       x = "",
       y = "")+
  theme(legend.position = "none")







