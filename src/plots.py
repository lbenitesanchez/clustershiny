from __future__ import annotations

import matplotlib.pyplot as plt
import pandas as pd


def sales_by_region(df: pd.DataFrame) -> pd.DataFrame:
    grouped = (
        df.groupby("region", dropna=False)["total_sales"]
        .sum()
        .sort_values(ascending=False)
        .reset_index()
    )
    return grouped


def sales_by_region_chart(df: pd.DataFrame) -> plt.Figure:
    grouped = sales_by_region(df)
    fig, ax = plt.subplots()
    ax.bar(grouped["region"].astype(str), grouped["total_sales"], color="#4C78A8")
    ax.set_xlabel("Region")
    ax.set_ylabel("Total Sales")
    ax.set_title("Total Sales by Region")
    ax.tick_params(axis="x", rotation=30)
    fig.tight_layout()
    return fig
