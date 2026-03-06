#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Fundamentos y representación del tiempo   #
# Tema: Series de tiempo                            #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 03                                        #
# SciData                                           #
#===================================================#

# ¿Cómo importar un modulo?

import pandas as pd

# Si te aparece el error "no module named pandas"

!pip install pandas

# Carga de modulos para el ejercicio 

import pandas as pd
import numpy as np
from datetime import datetime # Importar una clase de un modulo
import os

# Llamado de datos desde csv 

## Verificar ruta o directorio 

os.getcwd()

## Cargar datos

GDPR = pd.read_csv("Bases de datos/GDPC1.csv")
GDPR.head(5)

# Conversión de columna "observation_date" a fecha 

GDPR["observation_date"] = pd.to_datetime(
  GDPR["observation_date"],
  format = "%Y-%m-%d",
  dayfirst = True
)

GDPR.head(5)

## Comprobar tipo de variable 

GDPR["observation_date"].dtype
GDPR["GDPC1"].dtype

## Crear variable de fecha (conociendo bien las secuencias)

GDPR2 = GDPR.copy()

GDPR2["observation_date"] = pd.date_range(
  start = "1947-01-01",
  periods = 316,
  freq = "QS"
)

GDPR2.head(5)

# Construcción del pd.Series equivalente a ts 

GDPR_ts = pd.Series(
  GDPR["GDPC1"].values,
  index = GDPR["observation_date"]
)

GDPR_ts.head(5)

# Comprobar con un gráfico básico 

import matplotlib.pyplot as plt

plt.figure(figsize = (12,5), dpi = 500)
GDPR_ts.plot()
plt.show()

