#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estacionariedad y modelos clásicos        #
# Tema: Análisis de estacionariedad                 #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 13                                        #
# SciData                                           #
#===================================================#

# Modulos a cargar 

import pandas as pd 
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import sys
import os 
import importlib.util

# ACF/PACF

from statsmodels.tsa.stattools import acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from scipy.stats import t 

# ADF

from statsmodels.tsa.stattools import adfuller
from arch.unitroot import ADF 

# Phillips-Perron

from arch.unitroot import PhillipsPerron

# KPSS 

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

# Importación de los datos 

WTI = pd.read_excel("Bases de datos/WTI.xlsx",
                    usecols = "A:B",
                    skiprows = 5)


# Revisar el formato de las columnas 

WTI.dtypes

# Filtro de tiempo (trabajar con datos igual o posterio a 2021-01-01)

WTI = WTI[WTI['Dates'] >= '2021-01-01']

# Construcción del pd.Series equivalente a ts 

WTI_ts = pd.Series(
  WTI["PX_LAST"].values,
  index = WTI["Dates"]
)

WTI_ts.head(5)


# Gráficar / visualización inicial 

plt.figure(figsize=(12,7),dpi = 500)
plt.plot(WTI_ts)
plt.title("Precio del WTI al cierre del día\ndiario(dólares por barril)\n2021-2026")
plt.xlabel("Fecha")
plt.ylabel("WTI")
plt.show()

# Análisis de estacionariedad en I(0)

## Función de autocorrelación (pruebas gráficas)

### Simple 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_acf(WTI_ts, lags = 24)
ax.set_title("Función de autocorrelación simple")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(WTI_ts)

acf_vals = acf(WTI_ts, nlags =24)

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
plot_pacf(WTI_ts, lags = 24)
ax.set_title("Función de autocorrelación parcial")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(WTI_ts)

pacf_vals = pacf(WTI_ts, nlags =24)

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

n = len(WTI_ts)
k_max = int(12*(n/100)**0.25)


#### Con statsmodels 

adf = adfuller(
  WTI_ts,
  maxlag = k_max,
  regression = 'ct'
)

print(
  f"ADF: {adf[0]:.2f}\nP-value: {adf[1]:.2f}"
)

#### Con urca 

adf = ADF(WTI_ts, trend = 'ct', lags = k_max)
print(adf.summary().as_text())

### Phillips-Perron 

#### Con urca 

PP = PhillipsPerron(WTI_ts, 
                    trend = 'ct',
                    test_type = "rho")
                    
print(PP.summary().as_text())

### KPSS 

#### kpss de statsmodels mediante definción 

kpss_test(WTI_ts, 
          regression='ct',
          lags='auto')

#### Con urca 

KPSS_test = KPSS(WTI_ts, trend = "ct")
print(KPSS_test.summary().as_text())

# Proceso de diferenciación 

WTI_diff = WTI_ts.diff(1)
WTI_diff = WTI_diff.dropna()

# Gráficar / visualización inicial 

plt.figure(figsize=(12,7),dpi = 500)
plt.plot(WTI_diff)
plt.title("Variación nominal del WTI al cierre del día\ndiario(dólares por barril)\n2021-2026")
plt.xlabel("Fecha")
plt.ylabel("WTI")
plt.show()

# Análisis de estacionariedad en I(1)

## Función de autocorrelación (pruebas gráficas)

### Simple 

#### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_acf(WTI_diff, lags = 24, zero = False)
ax.set_title("Función de autocorrelación simple")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(WTI_diff)

acf_vals = acf(WTI_diff, nlags =24)

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
plot_pacf(WTI_diff, lags = 24, zero = False)
ax.set_title("Función de autocorrelación parcial")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

#### Tabla 

n = len(WTI_diff)

pacf_vals = pacf(WTI_diff, nlags =24)

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

n = len(WTI_diff)
k_max = int(12*(n/100)**0.25)


#### Con statsmodels 

adf = adfuller(
  WTI_diff,
  maxlag = k_max,
  regression = 'n'
)

print(
  f"ADF: {adf[0]:.2f}\nP-value: {adf[1]:.2f}"
)

#### Con urca 

adf = ADF(WTI_diff, trend = 'n', lags = k_max)
print(adf.summary().as_text())

### Phillips-Perron 

#### Con urca 

PP = PhillipsPerron(WTI_diff, 
                    trend = 'c',
                    test_type = "rho")
                    
print(PP.summary().as_text())

### KPSS 

#### kpss de statsmodels mediante definción 

kpss_test(WTI_diff, 
          regression='c',
          lags='auto')

#### Con urca 

KPSS_test = KPSS(WTI_diff, trend = "c")
print(KPSS_test.summary().as_text())










