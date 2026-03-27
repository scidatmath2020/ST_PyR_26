#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estacionariedad y modelos clásicos        #
# Tema: Modelo ARIMA                                #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 15                                        #
# SciData                                           #
#===================================================#

# Modulos a cargar 

import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import math
import warnings
import arch

# Modulos

## Importación completa

import pandas as pd
import polars as ps
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import math
import warnings
import importlib.util
import sys
import os
import arch

## Clases / Funciones para pruebas de estacionariedad

### Dickey-Fuller

from statsmodels.tsa.stattools import adfuller
from arch.unitroot import ADF

### Phillips-Perron

from arch.unitroot import PhillipsPerron

### KPSS

from arch.unitroot import KPSS
from statsmodels.tsa.stattools import kpss
def kpss_test(series, regression='c', lags='auto'):
  
    """
    KPSS Test
    H0: La serie es estacionaria
    H1: La serie NO es estacionaria
    """
    statistic, p_value, n_lags, critical_values = kpss(series, regression=regression, nlags=lags)
    
    print("Resultados del Test KPSS")
    print("-" * 40)
    print(f"Estadístico KPSS : {statistic:.3f}")
    print(f"P-valor          : {p_value:.4f}")
    print(f"Número de rezagos: {n_lags}")
    print("\nValores críticos:")
    
    for key, value in critical_values.items():
        print(f"   {key} : {value}")
    
    # Interpretación automática
    print("\nInterpretación:")
    if p_value < 0.05:
        print("Rechazamos H0 → La serie NO es estacionaria")
    else:
        print("No rechazamos H0 → La serie es estacionaria")


### ACF / PACF

# ACF/PACF

from statsmodels.tsa.stattools import acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from scipy.stats import t 

## Clases para estimación del modelo ARIMA

from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error

# Carga de datos 

Data = pd.read_csv(
  "Bases de datos/datosMONITOREAA_012026.csv"
)

# Convertir la columna mes en formato de fecha 

## Diccionario de meses 

meses = {
  "Enero":1, "Febrero":2, "Marzo":3, 
  "Abril":4, "Mayo":5, "Junio":6,
  "Julio":7, "Agosto":8, "Septiembre":9,
  "Octubre":10, "Noviembre":11, "Diciembre":12
}

## Separar texto 

Data[["Mes", "Año"]] = Data['mes'].str.split(" de ", expand = True)

## Convertir meses a número según diccionario 

Data["Mes_num"] = Data["Mes"].map(meses)

## Convertir años a numérico 

Data["Año"] = Data["Año"].astype(int)

## Crear columna de fecha 

Data["Fecha"] = pd.to_datetime(
  dict(year = Data["Año"],
       month = Data["Mes_num"],
       day = 1)
)

## Generar subcojunto para la serie temporal 

Data2 = Data.copy()
Data2 = Data2[["Fecha", "costo_turbosina_pesos_litro"]]

Data2 = Data2.rename(columns = {
  "costo_turbosina_pesos_litro":"Costo_turbosina"
})


# Construcción del pd.Series equivalente a ts 

Data2_ts = pd.Series(
  Data2["Costo_turbosina"].values,
  index = Data2["Fecha"]
)

Data2_ts.head(5)


# Gráficar / visualización inicial 

plt.figure(figsize=(12,7),dpi = 500)
plt.plot(Data2_ts)
plt.title("Precio mensual de la turbosina\n(pesos por litro)\n2014-2026")
plt.xlabel("Fecha")
plt.ylabel("Precio de tubosina (pesos por litro)")
plt.show()

# Análisis de estacionariedad en I(0)

## Función de autocorrelación (pruebas gráficas)

### Simple 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_acf(Data2_ts, lags = 24)
ax.set_title("Función de autocorrelación simple")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(Data2_ts)

acf_vals = acf(Data2_ts, nlags =24)

rezagos = np.arange(len(acf_vals))

ACF_tab = pd.DataFrame({
  "Rezago": rezagos,
  "Rho":acf_vals
})

ACF_tab = ACF_tab[ACF_tab['Rezago']>0]

ACF_tab['t_stat'] = (
  ACF_tab['Rho'] * np.sqrt(n-ACF_tab['Rezago'])
  /np.sqrt(1-ACF_tab['Rho']**2)
)

ACF_tab["pvalue"] = (
  2*(1-t.cdf(np.abs(ACF_tab["t_stat"]), df = n-ACF_tab["Rezago"]-2))
).round(4)

ACF_tab


### Parcial 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_pacf(Data2_ts, lags = 24)
ax.set_title("Función de autocorrelación parcial")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(Data2_ts)

pacf_vals = pacf(Data2_ts, nlags =24)

rezagos = np.arange(len(pacf_vals))

PACF_tab = pd.DataFrame({
  "Rezago": rezagos,
  "PACF":pacf_vals
})

PACF_tab = PACF_tab[PACF_tab['Rezago']>0]

PACF_tab['t_stat'] = (
  PACF_tab['PACF'] * np.sqrt(n-PACF_tab['Rezago'])
  /np.sqrt(1-PACF_tab['PACF']**2)
)

PACF_tab["pvalue"] = (
  2*(1-t.cdf(np.abs(PACF_tab["t_stat"]), df = n-PACF_tab["Rezago"]-2))
).round(4)

PACF_tab

## Pruebas estadísticas

### ADF 

#### Rezago óptimo 

n = len(Data2_ts)
k_max = int(12*(n/100)**0.25)


#### Con statsmodels 

adf = adfuller(
  Data2_ts,
  maxlag = k_max,
  regression = 'ct'
)

print(
  f"ADF: {adf[0]:.2f}\nP-value: {adf[1]:.2f}"
)

#### Con arch 

adf = ADF(Data2_ts, trend = 'ct', lags = k_max)
print(adf.summary().as_text())

### Phillips-Perron 

#### Con urca 

PP = PhillipsPerron(Data2_ts, 
                    trend = 'ct',
                    test_type = "rho")
                    
print(PP.summary().as_text())

### KPSS 

#### kpss de statsmodels mediante definción 

kpss_test(Data2_ts, 
          regression='ct',
          lags='auto')

#### Con urca 

KPSS_test = KPSS(Data2_ts, trend = "ct")
print(KPSS_test.summary().as_text())

# Proceso de diferenciación 

Data2_diff = Data2_ts.diff(1)
Data2_diff = Data2_diff.dropna()

# Gráficar / visualización inicial 

plt.figure(figsize=(12,7),dpi = 500)
plt.plot(Data2_diff)
plt.title("Variación del precio de la turbosina\nmensual(pesos por litro)\n2014-2026")
plt.xlabel("Fecha")
plt.ylabel("Variación del precio (en pesos)")
plt.show()

# Análisis de estacionariedad en I(1)

## Función de autocorrelación (pruebas gráficas)

### Simple 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_acf(Data2_diff, lags = 24, zero = False)
ax.set_title("Función de autocorrelación simple")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(Data2_diff)

acf_vals = acf(Data2_diff, nlags =24)

rezagos = np.arange(len(acf_vals))

ACF_tab = pd.DataFrame({
  "Rezago": rezagos,
  "Rho":acf_vals
})

ACF_tab = ACF_tab[ACF_tab['Rezago']>0]

ACF_tab['t_stat'] = (
  ACF_tab['Rho'] * np.sqrt(n-ACF_tab['Rezago'])
  /np.sqrt(1-ACF_tab['Rho']**2)
)

ACF_tab["pvalue"] = (
  2*(1-t.cdf(np.abs(ACF_tab["t_stat"]), df = n-ACF_tab["Rezago"]-2))
).round(4)

ACF_tab


### Parcial 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_pacf(Data2_diff, lags = 24, zero = False)
ax.set_title("Función de autocorrelación parcial")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(Data2_diff)

pacf_vals = pacf(Data2_diff, nlags =24)

rezagos = np.arange(len(pacf_vals))

PACF_tab = pd.DataFrame({
  "Rezago": rezagos,
  "PACF":pacf_vals
})

PACF_tab = PACF_tab[PACF_tab['Rezago']>0]

PACF_tab['t_stat'] = (
  PACF_tab['PACF'] * np.sqrt(n-PACF_tab['Rezago'])
  /np.sqrt(1-PACF_tab['PACF']**2)
)

PACF_tab["pvalue"] = (
  2*(1-t.cdf(np.abs(PACF_tab["t_stat"]), df = n-PACF_tab["Rezago"]-2))
).round(4)

PACF_tab

## Pruebas estadísticas

### ADF 

#### Rezago óptimo 

n = len(Data2_diff)
k_max = int(12*(n/100)**0.25)


#### Con statsmodels 

adf = adfuller(
  Data2_diff,
  maxlag = k_max,
  regression = 'n'
)

print(
  f"ADF: {adf[0]:.2f}\nP-value: {adf[1]:.2f}"
)

#### Con urca 

adf = ADF(Data2_diff, trend = 'n', lags = k_max)
print(adf.summary().as_text())

### Phillips-Perron 

#### Con urca 

PP = PhillipsPerron(Data2_diff, 
                    trend = 'c',
                    test_type = "rho")
                    
print(PP.summary().as_text())

### KPSS 

#### kpss de statsmodels mediante definción 

kpss_test(Data2_diff, 
          regression='c',
          lags='auto')

#### Con urca 

KPSS_test = KPSS(Data2_diff, trend = "c")
print(KPSS_test.summary().as_text())

# Estimación de los modelos ARIMA considerando p = 1-4; q = 1-4

## Definir función para determinar ordenes p y q

def arima_grid_search(y, p_range, d_range, q_range):

  results = []

  for p in p_range:
    for d in d_range:
      for q in q_range:
        try:
          model = ARIMA(y, order = (p, d,q))
          fitted = model.fit()

          # RMSE

          rmse = np.sqrt(fitted.mse)

          # AIC

          aic = fitted.aic

          # Resultados

          results.append({
              "p": p,
              "d":d,
              "q":q,
              "AIC" : aic,
              "RMSE": rmse
          })

        except:
          continue

  return pd.DataFrame(results)

## Realizar prueba para identificar combinación correcta

### Ordenes a evaluar 

p_range = range(1,4)
d_range = [1]
q_range = range(1,4)

resultados_df = arima_grid_search(
  y = Data2_ts,
  p_range = p_range,
  d_range = d_range,
  q_range = q_range
)

resultados_df

##  Selección del modelo optimo según AIC

best_aic = resultados_df.loc[resultados_df['AIC'].idxmin()]
best_aic

## Modelo optimo mediante RMSE

best_rmse = resultados_df.loc[resultados_df['RMSE'].idxmin()]
best_rmse

## Estimar modelo final basado en AIC

best_order = (int(best_aic.p), int(best_aic.d), int(best_aic.q))

Modelo = ARIMA(Data2_ts, order = best_order).fit()

# Revisión de significancia de los coeficientes

print(Modelo.summary())

# Circulo de raíces invertidas

def plot_unit_circle_inverted(inv_ar_roots=None, inv_ma_roots=None):

    fig, ax = plt.subplots(figsize=(10,7), dpi = 500)

    # Círculo unitario
    theta = np.linspace(0, 2*np.pi, 400)
    ax.plot(np.cos(theta), np.sin(theta), linestyle='--')

    # Ejes
    ax.axhline(0)
    ax.axvline(0)

    # Raíces AR invertidas
    if inv_ar_roots is not None:
        ax.scatter(inv_ar_roots.real, inv_ar_roots.imag,
                   marker='o', label='AR (raíces invertidas)')

    # Raíces MA invertidas
    if inv_ma_roots is not None:
        ax.scatter(inv_ma_roots.real, inv_ma_roots.imag,
                   marker='x', label='MA (raíces invertidas)')

    ax.set_aspect('equal')
    ax.set_xlim(-1.2, 1.2)
    ax.set_ylim(-1.2, 1.2)
    ax.set_title("Círculo unitario – raíces invertidas (ARIMA)")
    ax.legend()
    plt.show()
    
## Uso de la función 

ar_roots = Modelo.arroots
ma_roots = Modelo.maroots

plot_unit_circle_inverted(inv_ar_roots=1/ar_roots,
                          inv_ma_roots=1/ma_roots)

## Pruebas de autocorrelación 

sm.stats.acorr_ljungbox(Modelo.resid, lags = 24, return_df =True)

## Prueba de heterocedasticidad (ARCH)

lm_s, p_value_lm, f_s, f_p_value = sm.stats.diagnostic.het_arch(
  Modelo.resid,
  nlags=1
)

print(f"Estadístico LM: {lm_s:.2f}")
print(f"P-value LM: {p_value_lm:.2f}")

## Normalidad 

from scipy.stats import jarque_bera 

JB, p_value = jarque_bera(Modelo.resid)
print(f"Estadístico JB: {JB:.2f}")
print(f"P-value JB: {p_value:.2f}")

from scipy.stats import anderson

AD = anderson(Modelo.resid)
print(f"Estadístico AD: {AD.statistic:.2f}")
print(f"P-value AD: {AD.critical_values}")
print(f"Niveles de confianza AD: {AD.significance_level}")

## Pruebas estacionariedad

### ADF

adf_resid = ADF(Modelo.resid, trend = 'c')
print(adf_resid.summary().as_text())

### KPSS 

kpss_resid = KPSS(Modelo.resid, trend = 'c')
print(kpss_resid.summary().as_text())

# Elaboración de pronostico

proyeccion = Modelo.get_forecast(steps = 12)
proyeccion_ci = proyeccion.conf_int()

puntual = proyeccion.predicted_mean
Ci = proyeccion_ci

puntual


## Gráfico de pronostico 

Data1 = Data2_ts[100:134]

plt.figure(figsize = (12,7), dpi = 500)
ax = Data1.plot(label = 'Real')
puntual.plot(ax = ax, label = 'Pronostico')
ax.fill_between(Ci.index,
                Ci.iloc[:,0],
                Ci.iloc[:,1],
                color = "k",
                alpha = 0.25)
ax.set_xlabel('Fecha')
ax.set_ylabel('Precio de turbosina por litro')
plt.title('Pronostico del precio de turbosina a a 12 meses\nARIMA(2,1,3)')
plt.savefig("Pronostico_ARIMA_py.png", dpi = 500)
plt.tight_layout()
plt.show()
