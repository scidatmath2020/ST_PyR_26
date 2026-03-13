# -*- coding: utf-8 -*-
"""
Codigo para conexion de API's de INEGI y BANXICO 
Autor: Alexis Adonai Morales Alberto 
Curso: Series de tiempo con  Python 
Fecha: Diciembre, 2025 
Codigo de R 
"""

import requests
import pandas as pd
from datetime import datetime


def Series_INEGI_BIE(id_serie="", token="", periodo="Mensual"):
    """
    Descarga series del BIE del INEGI vía API

    Parámetros
    ----------
    id_serie : str o int
        Identificador de la serie en INEGI
    token : str
        Token de desarrollador INEGI
    periodo : str
        'Mensual' o 'Trimestral'

    Retorna
    -------
    DataFrame de pandas con columnas:
    Fecha | Serie
    """

    url_data = (
        "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/"
        f"jsonxml/INDICATOR/{id_serie}/es/00/false/BIE-BISE/2.0/"
        f"{token}?type=json"
    )

    # Llamada a la API
    response = requests.get(url_data)
    response.raise_for_status()
    data_json = response.json()

    # Acceso a la serie (estructura del JSON del INEGI)
    series = data_json["Series"][0]["OBSERVATIONS"]

    df = pd.DataFrame(series)[["TIME_PERIOD", "OBS_VALUE"]]
    df.columns = ["Fecha", "Serie"]

    # Conversión numérica
    df["Serie"] = pd.to_numeric(df["Serie"], errors="coerce")

    if periodo == "Mensual":
        df["Año"] = df["Fecha"].str[:4]
        df["Mes"] = df["Fecha"].str[5:7]

        df["Fecha"] = pd.to_datetime(
            df["Año"] + "-" + df["Mes"] + "-01",
            format="%Y-%m-%d"
        )

    elif periodo == "Trimestral":
        df["Año"] = df["Fecha"].str[:4]
        df["Trim"] = df["Fecha"].str[5:7]

        # Conversión de trimestre a mes inicial
        trim_map = {
            "01": "01",
            "02": "04",
            "03": "07",
            "04": "10"
        }

        df["Mes"] = df["Trim"].map(trim_map)

        df["Fecha"] = pd.to_datetime(
            df["Año"] + "-" + df["Mes"] + "-01",
            format="%Y-%m-%d"
        )

    else:
        raise ValueError("El periodo debe ser 'Mensual' o 'Trimestral'")

    df = df[["Fecha", "Serie"]].sort_values("Fecha").reset_index(drop=True)

    return df

