#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estructura y dependencia temporal         #
# Tema: Dependencia temporal, ACF y PACF            #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 10                                        #
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

from statsmodels.tsa.stattools import acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.stats.diagnostic import acorr_ljungbox
from scipy.stats import t 


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

# Gráficar 

plt.figure(figsize=(12,7),dpi = 500)
plt.plot(PIB_t)
plt.title("Producto Interno Bruto a nivel nacional\ntrimestral (millones de pesos del 2018)\n1980-2025")
plt.xlabel("Fecha")
plt.ylabel("PIB")
plt.show()

# Función de autocorrelación 

## Simple 

### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_acf(PIB_t, lags = 24)
ax.set_title("Función de autocorrelación simple")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

### Tabla 

n = len(PIB_t)

acf_vals = acf(PIB_t, nlags =24)

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


## Parcial 

### Gráfico 

fig, ax = plt.subplots(figsize = (10,5), dpi = 300)
plot_pacf(PIB_t, lags = 24)
ax.set_title("Función de autocorrelación parcial")
ax.set_xlabel("Rezago")
ax.set_ylabel("ACF")
ax.set_xticks(np.arange(1,25,1))
ax.set_ylim(-1,1)
plt.show()

### Tabla 

n = len(PIB_t)

pacf_vals = pacf(PIB_t, nlags =24)

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

# Prueba de autocorrelación 

lags = [1,5,10,15,20,25]

ljung_box = acorr_ljungbox(PIB_t, lags=lags, return_df=True)

print(ljung_box)
