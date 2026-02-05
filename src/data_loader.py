from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable

import pandas as pd
import streamlit as st


@dataclass(frozen=True)
class DataConfig:
    path: str = "data/NovaRetail.csv"
    region_labels: tuple[str, ...] = ("Norte", "Sur", "Este", "Oeste")


def _assign_regions(df: pd.DataFrame, labels: Iterable[str]) -> pd.Series:
    labels = list(labels)
    if "distancia_tienda" in df.columns:
        try:
            return pd.qcut(df["distancia_tienda"], q=len(labels), labels=labels, duplicates="drop")
        except ValueError:
            pass

    indexer = pd.Series(range(len(df))) % len(labels)
    return indexer.map(dict(enumerate(labels)))


def _ensure_total_sales(df: pd.DataFrame) -> pd.Series:
    if "total_sales" in df.columns:
        return df["total_sales"]
    if {"ticket_promedio", "freq_visitas"}.issubset(df.columns):
        return df["ticket_promedio"] * df["freq_visitas"]
    numeric_cols = df.select_dtypes(include="number")
    if not numeric_cols.empty:
        return numeric_cols.sum(axis=1)
    return pd.Series([0.0] * len(df))


@st.cache_data
def load_data(config: DataConfig = DataConfig()) -> pd.DataFrame:
    df = pd.read_csv(config.path)
    if "region" not in df.columns:
        df["region"] = _assign_regions(df, config.region_labels)
    df["total_sales"] = _ensure_total_sales(df)
    return df
