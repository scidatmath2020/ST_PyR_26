#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Fundamentos y representación del tiempo   #
# Tema: Operaciones con temporalidades              #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 05                                        #
# SciData                                           #
#===================================================#

# Modulos a cargar/llamar 

import pandas as pd 
import numpy as np
import os 
from datetime import datetime

# Verificar ruta o directorio de donde se ejecutando

os.getcwd()

# Lectura de archivo csv 

NFLX_df = pd.read_csv(
  "Bases de datos/Datos_NFLX_temp.csv",
  encoding = 'latin1',
  usecols = range(6)
)

# Verificar formato de columna 

NFLX_df.columns

NFLX_df['Fecha'].dtype

# Transformación a fecha diaria en formato %Y-%m-%d 

NFLX_df['Fecha'] = pd.to_datetime(
  NFLX_df['Fecha'],
  format = "%Y-%m-%d"
)

## Reverificar la columna 

NFLX_df['Fecha'].dtype

## Revisión de las primeras 6 filas 

NFLX_df.head(5)

# Obtención de formatos de la fecha 

NFLX_df = NFLX_df.assign(
  Año = NFLX_df['Fecha'].dt.strftime("%Y"),
  Año2g = NFLX_df['Fecha'].dt.strftime("%y"),
  Mes_num = NFLX_df['Fecha'].dt.strftime("%m"),
  Mes_txt = NFLX_df['Fecha'].dt.strftime("%B"),
  Mes_txt2 = NFLX_df['Fecha'].dt.strftime("%b"),
  Dia_num = NFLX_df['Fecha'].dt.strftime("%d"),
  Dia_año = NFLX_df['Fecha'].dt.strftime("%j"),
  Dia_txt = NFLX_df['Fecha'].dt.strftime("%A"),
  Dia_txt2 = NFLX_df['Fecha'].dt.strftime("%a"),
  Semana_D = NFLX_df['Fecha'].dt.strftime("%U"),
  Semana_L = NFLX_df['Fecha'].dt.strftime("%W"),
  Trimestre = NFLX_df['Fecha'].dt.quarter,
  TrimestreQ = NFLX_df['Fecha'].dt.to_period("Q"),
  Semestre_num = (NFLX_df['Fecha'].dt.month.sub(1)//6+1)
)

NFLX_df.head(5)

# Cálculo de la tasa de rendimiento 

NFLX_df['TC_NFLX'] = np.log(NFLX_df["NFLX.Close"]).diff()*100

# Rendimiento promedio por mes 

rend_men = (
  NFLX_df
  .groupby(["Año", "Mes_num"])['TC_NFLX']
  .mean()
  .reset_index()
)

# Número de observaciones de cada año 

(
  NFLX_df
  .groupby(["Año"])
  .size()
  .reset_index(name = "n")
)

# Rendimientos anualizados 

rend_anual = (
  NFLX_df
  .groupby(["Año"])
  .agg(
    TC_NFLX = ("TC_NFLX", "mean"),
    Dias_año = ("TC_NFLX", "size")
  )
  .reset_index()
)

rend_anual['TC_NFLX'] = rend_anual['TC_NFLX'] * rend_anual['Dias_año']

# Rendimientos día semana 

rend_dia = (
  NFLX_df
  .groupby(['Año', "Dia_txt"])["TC_NFLX"]
  .mean()
  .reset_index()
)

## Ordenar por año y día 

orden = ["lunes", "martes", "miércoles", "jueves", "viernes"]

rend_dia['Dia_txt'] = pd.Categorical(
  rend_dia['Dia_txt'],
  categories = orden,
  ordered = True
)

rend_dia = rend_dia.sort_values(['Año', 'Dia_txt'])

## De largo a ancho 

tablas_dias = (
  rend_dia
  .pivot(index = "Año", columns = "Dia_txt", values = "TC_NFLX")
  .reset_index()
)

tablas_dias
