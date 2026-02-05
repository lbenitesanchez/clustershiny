from __future__ import annotations

import io

import pandas as pd
import streamlit as st

from src.data_loader import load_data
from src.plots import sales_by_region_chart


st.set_page_config(page_title="Nova Retail Dashboard", layout="wide")

st.title("Nova Retail Dashboard")

raw_data = load_data()
regions = sorted(raw_data["region"].astype(str).unique().tolist())

st.sidebar.header("Filters")
selected_regions = st.sidebar.multiselect(
    "Select region(s)",
    options=regions,
    default=regions,
)

filtered_data = raw_data[raw_data["region"].astype(str).isin(selected_regions)].copy()

st.subheader("Filtered Data")
st.dataframe(filtered_data, use_container_width=True)

st.subheader("Total Sales by Region")
chart = sales_by_region_chart(filtered_data)
st.pyplot(chart, use_container_width=True)

csv_buffer = io.StringIO()
filtered_data.to_csv(csv_buffer, index=False)

st.download_button(
    label="Download filtered data as CSV",
    data=csv_buffer.getvalue(),
    file_name="nova_retail_filtered.csv",
    mime="text/csv",
)

st.caption(f"Rows: {len(filtered_data)}")
