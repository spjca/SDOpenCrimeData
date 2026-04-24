"""Aggregation helpers for SDPD calls."""
from __future__ import annotations

import logging
from typing import Iterable

import pandas as pd

from . import config

logger = logging.getLogger(__name__)

WEEKDAY_ORDER = [
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
]


def normalize_call_type_series(series: pd.Series) -> pd.Series:
    """Normalize call-type codes before grouping or joining reference labels."""
    return series.astype("string").str.strip().str.upper()


def citywide_by_year(df: pd.DataFrame) -> pd.DataFrame:
    """Return total calls per year."""
    return (
        df.dropna(subset=["year"])
        .groupby("year")
        .size()
        .reset_index(name="call_count")
        .sort_values("year")
    )


def beat_by_year(df: pd.DataFrame) -> pd.DataFrame:
    """Return total calls per beat per year."""
    return (
        df.dropna(subset=[config.BEAT_COLUMN, "year"])
        .groupby([config.BEAT_COLUMN, "year"])
        .size()
        .reset_index(name="call_count")
    )


def beat_calltype_by_year(df: pd.DataFrame, categories: dict[str, str] | None = None) -> pd.DataFrame:
    """Counts per beat, per call_type (or mapped category), per year."""
    df_copy = df.copy()
    df_copy["call_category"] = normalize_call_type_series(df_copy["call_type"])
    if categories:
        normalized_categories = {
            str(key).strip().upper(): str(value).strip()
            for key, value in categories.items()
            if str(key).strip() and str(value).strip()
        }
        df_copy["call_category"] = df_copy["call_category"].map(normalized_categories).fillna(df_copy["call_category"])
    df_copy = df_copy[df_copy["call_category"].notna() & (df_copy["call_category"] != "")]
    grouped = (
        df_copy.dropna(subset=[config.BEAT_COLUMN, "year", "call_category"])
        .groupby([config.BEAT_COLUMN, "year", "call_category"])
        .size()
        .reset_index(name="call_count")
    )
    return grouped


def _beat_attribute_by_year(df: pd.DataFrame, attribute_column: str, output_column: str) -> pd.DataFrame:
    """Counts per beat, per year, for a categorical attribute."""
    subset = df.dropna(subset=[config.BEAT_COLUMN, "year", attribute_column]).copy()
    subset[output_column] = subset[attribute_column].astype(str).str.strip()
    subset = subset[subset[output_column] != ""]
    return (
        subset.groupby([config.BEAT_COLUMN, "year", output_column])
        .size()
        .reset_index(name="call_count")
    )


def beat_priority_by_year(df: pd.DataFrame) -> pd.DataFrame:
    """Counts per beat, per year, and priority."""
    return _beat_attribute_by_year(df, "priority", "priority_label")


def beat_disposition_by_year(df: pd.DataFrame) -> pd.DataFrame:
    """Counts per beat, per year, and disposition."""
    return _beat_attribute_by_year(df, "disposition", "disposition_label")


def hourly_by_weekday(df: pd.DataFrame) -> pd.DataFrame:
    """Pivot table of calls by hour and weekday name."""
    subset = df.dropna(subset=["hour", "day_of_week_name"])
    counts = subset.groupby(["day_of_week_name", "hour"]).size().reset_index(name="call_count")
    pivot = counts.pivot_table(index="day_of_week_name", columns="hour", values="call_count", fill_value=0)
    existing = [w for w in WEEKDAY_ORDER if w in pivot.index]
    return pivot.loc[existing] if existing else pivot


def beat_hourly_by_weekday(df: pd.DataFrame) -> pd.DataFrame:
    """Return counts by beat, year, weekday, and hour for linked heatmaps."""
    subset = df.dropna(subset=[config.BEAT_COLUMN, "year", "day_of_week_name", "hour"])
    grouped = (
        subset.groupby([config.BEAT_COLUMN, "year", "day_of_week_name", "hour"])
        .size()
        .reset_index(name="call_count")
    )
    return grouped


def summarize(df: pd.DataFrame) -> dict:
    """Return a JSON-friendly summary of high-level stats."""
    citywide = citywide_by_year(df)
    total_records = len(df)
    first_year = int(citywide["year"].min()) if not citywide.empty else None
    last_year = int(citywide["year"].max()) if not citywide.empty else None
    summary = {
        "records": int(total_records),
        "first_year": first_year,
        "last_year": last_year,
    }
    if first_year is not None and last_year is not None:
        first_count = int(citywide.loc[citywide["year"] == first_year, "call_count"].iloc[0])
        last_count = int(citywide.loc[citywide["year"] == last_year, "call_count"].iloc[0])
        change_pct = ((last_count - first_count) / first_count * 100) if first_count else None
        summary.update(
            {
                "first_year_calls": first_count,
                "last_year_calls": last_count,
                "pct_change": change_pct,
            }
        )
    return summary
