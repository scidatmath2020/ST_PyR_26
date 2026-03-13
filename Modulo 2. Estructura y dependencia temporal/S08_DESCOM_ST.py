#===================================================#
# Diplomado: Series de tiempo con R y Python        #
# Modulo: Estructura y dependencia temporal         #
# Tema: Descomposición de series de tiempo          #
# Docente: Alexis Adonai Morales Alberto            #
# Sesión: 08                                        #
# SciData                                           #
#===================================================#

# Modulos a cargar 

import pandas as pd 
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import sys
import os 
import importlib.util

from statsmodels.tsa.seasonal import seasonal_decompose

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
  id_serie = 736181,
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
plt.title("Producto Interno Bruto a nivel nacional\ntrimestral (millones de pesos del 2018)\n1980-2025 desestacionalizada")
plt.xlabel("Fecha")
plt.ylabel("PIB")
plt.show()

# Descomposición de la serie 

## Método aditivo 

### Data frame con la descomposición 

Descom = seasonal_decompose(
  PIB_t,
  model = "additive",
  period = 4
)

PIB_descomp = pd.DataFrame(
  {
    'Observación': Descom.observed,
    'Tendencia': Descom.trend,
    'Estacionalidad': Descom.seasonal,
    'Aleatorio': Descom.resid
  }
)

PIB_descomp

### Gráfica de descomposición temporal

fig, axes = plt.subplots(4,1, figsize = (14,9), dpi = 500)
axes[0].plot(PIB_descomp.index, PIB_descomp["Observación"])
axes[0].set_title("PIB Observado")
axes[1].plot(PIB_descomp.index, PIB_descomp["Tendencia"])
axes[1].set_title("Tendencia")
axes[2].plot(PIB_descomp.index, PIB_descomp["Estacionalidad"])
axes[2].set_title("Estacional")
axes[3].plot(PIB_descomp.index, PIB_descomp["Aleatorio"])
axes[3].set_title("Aleatorio")
plt.savefig("Descomposición_ad.png", dpi = 300, bbox_inches = 'tight')
plt.tight_layout()
plt.show()

## Método multiplicativo 

### Data frame con la descomposición 

Descom2 = seasonal_decompose(
  PIB_t,
  model = "multipicative",
  period = 4
)

PIB_descomp2 = pd.DataFrame(
  {
    'Observación': Descom2.observed,
    'Tendencia': Descom2.trend,
    'Estacionalidad': Descom2.seasonal,
    'Aleatorio': Descom2.resid
  }
)

PIB_descomp2

### Gráfica de descomposición temporal

fig, axes = plt.subplots(4,1, figsize = (14,9), dpi = 500)
axes[0].plot(PIB_descomp2.index, PIB_descomp2["Observación"])
axes[0].set_title("PIB Observado")
axes[1].plot(PIB_descomp2.index, PIB_descomp2["Tendencia"])
axes[1].set_title("Tendencia")
axes[2].plot(PIB_descomp2.index, PIB_descomp2["Estacionalidad"])
axes[2].set_title("Estacional")
axes[3].plot(PIB_descomp2.index, PIB_descomp2["Aleatorio"])
axes[3].set_title("Aleatorio")
plt.savefig("Descomposición_mul.png", dpi = 300, bbox_inches = 'tight')
plt.tight_layout()
plt.show()


