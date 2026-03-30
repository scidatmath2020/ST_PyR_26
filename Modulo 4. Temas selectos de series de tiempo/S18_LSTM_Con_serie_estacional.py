#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Temas selectos de series de tiempo        #
# Tema: Red Neuronal Recurrente tipo LSTM           #
#       Long Short-Term Memory                      #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 18                                        #
# SciData                                           #
#===================================================#

# Modulos a cargar 

## Modulos de importación completa 

!pip install pandas statsmodels matplotlib seaborn numpy 

import pandas as pd 
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import math
import warnings
import sys
import os 
import importlib.util

## Clases de modulos 

!pip install tensorflow
!pip install scikit-learn

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from sklearn.preprocessing import StandardScaler


# Procedimiento para cargar script bie_inegi.py 

ruta_api = "Funciones/bie_inegi.py"

# Cargar script como modulo 

spec = importlib.util.spec_from_file_location("bie_inegi", ruta_api)
bie_inegi = importlib.util.module_from_spec(spec)
sys.modules["bie_inegi"] = bie_inegi
spec.loader.exec_module(bie_inegi)

# Cargar datos del BIE 

token_id = "af847734-746b-8eb8-f0e6-4070cc851e47"

PIB = bie_inegi.Series_INEGI_BIE(
  id_serie = 735879,
  token = token_id,
  periodo = "Trimestral"
)

# Comprobar tipo de información 

PIB.dtypes

# Convertir en pd.series 

PIB_t = PIB.set_index("Fecha")
PIB_t = pd.Series(PIB_t.Serie, index = PIB_t.index)
PIB_t


# Escalar el PIB_t

PIB_array = PIB_t.values.reshape(-1,1)
scaler = StandardScaler()
PIB_scaled = scaler.fit_transform(PIB_array)
PIB_scaled

# Ventanas deslizantes

def crear_secuencias(data, pasos=10):
    X, y = [], []
    for i in range(len(data)-pasos):
        X.append(data[i:i+pasos])
        y.append(data[i+pasos])
    return np.array(X), np.array(y)

pasos = 10
X, y = crear_secuencias(PIB_scaled , pasos)

X = X.reshape((X.shape[0], X.shape[1], 1))

# Construcción del modelo LSTM

modelo = Sequential()
modelo.add(LSTM(50, activation='tanh', input_shape=(pasos, 1)))
modelo.add(Dense(1))

modelo.compile(optimizer='adam', loss='mse')

modelo.summary()

# Entrenamiento (estimación)

modelo.fit(X, y, epochs = 50, batch_size = 16, verbose = 1)

# Valores estimados vs reales

predicciones = modelo.predict(X)
y_original = scaler.inverse_transform(y.reshape(-1,1))
pred_original = scaler.inverse_transform(predicciones)

## Gráfico visual de la estimación 

plt.figure(figsize = (10,5), dpi = 300)
plt.plot(y_original, label = "Real")
plt.plot(pred_original, label = "Predicción")
plt.legend()
plt.show()


# Pronostico

def pronostico_multi_pasos(modelo, ultima_ventana, pasos_futuros):
    ventana = ultima_ventana.copy()
    predicciones = []

    for _ in range(pasos_futuros):
        pred = modelo.predict(ventana.reshape(1, ventana.shape[0], 1), verbose=0)
        predicciones.append(pred[0,0])

        # actualizar ventana
        ventana = np.append(ventana[1:], pred[0,0])

    return np.array(predicciones)

# última ventana conocida
ultima_ventana = X[-1].flatten()

forecast_5 = pronostico_multi_pasos(modelo, ultima_ventana, 5)

print("Pronóstico 5 pasos:", forecast_5)

# Inversa del escalamiento ----

Pronostico_5 = scaler.inverse_transform(forecast_5.reshape(-1,1))
Pronostico_5

# Aplanar arrays

y_original = y_original.flatten()
pred_original = pred_original.flatten()
Pronostico_5 = Pronostico_5.flatten()

# Crear indices explicitos

n = len(y_original)

x_real = np.arange(n)
x_Pronostico = np.arange(n, n + len(Pronostico_5))

# Gráfico

plt.figure(figsize=(10,5), dpi=300)

plt.plot(x_real, y_original, label="Real")
plt.plot(x_real, pred_original, label="Predicción")
plt.plot(x_Pronostico, Pronostico_5, marker='o', label="Forecast 5 pasos")

plt.legend()
plt.show()

# Ultimas 30 observaciones

ultimas_30_real = y_original[-30:]
ultimas_30_pred = pred_original[-30:]

n_total = len(y_original)

x_real = np.arange(n_total-30, n_total)
x_forecast = np.arange(n_total, n_total + len(Pronostico_5))

plt.figure(figsize=(10,5), dpi=300)

plt.plot(x_real, ultimas_30_real, label="Real")
plt.plot(x_real, ultimas_30_pred, label="Predicción")
plt.plot(x_forecast, Pronostico_5, marker='o', linestyle='--', label="Forecast 5 pasos")

plt.legend()
plt.show()
