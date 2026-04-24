"""Preprocess SDPD call data."""
from __future__ import annotations

import logging
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

from . import config, data_download

logger = logging.getLogger(__name__)

ESSENTIAL_COLUMNS = [
    "incident_num",
    config.DATE_COLUMN,
    "call_type",
    "disposition",
    config.BEAT_COLUMN,
    "priority",
]

OPTIONAL_COLUMNS = [*config.COORD_COLUMNS]


def _generate_sample_data(years: Iterable[int]) -> pd.DataFrame:
    """Create a small synthetic dataset used when downloads are unavailable."""
    rng = np.random.default_rng(42)
    records = []
    for year in years:
        for beat in [112, 224, 313]:
            for _ in range(50):
                month = int(rng.integers(1, 13))
                day = int(rng.integers(1, 28))
                hour = int(rng.integers(0, 24))
                timestamp = pd.Timestamp(year=year, month=month, day=day, hour=hour)
                records.append(
                    {
                        "incident_num": f"{year}{beat}{rng.integers(1,1_000_000):06d}",
                        config.DATE_COLUMN: timestamp,
                        "call_type": rng.choice(["TROUBLE W/SUBJECT", "DISTURBANCE", "VEHICLE STOP"]),
                        "disposition": rng.choice(["F", "G", "P"]),
                        config.BEAT_COLUMN: beat,
                        "priority": int(rng.integers(1, 5)),
                        "x": -117.16 + rng.normal(scale=0.01),
                        "y": 32.72 + rng.normal(scale=0.01),
                    }
                )
    sample_df = pd.DataFrame.from_records(records)
    logger.warning("Using synthetic sample data (%s rows) because no raw files were found.", len(sample_df))
    return sample_df


def load_raw_data(years: Iterable[int] | None = None) -> pd.DataFrame:
    """Load and concatenate all raw CSVs for the given years."""
    year_list = list(years) if years is not None else config.years_to_process()
    frames: list[pd.DataFrame] = []
    for year in year_list:
        path = config.RAW_DIR / f"pd_calls_for_service_{year}_datasd.csv"
        if not path.exists():
            logger.warning("Raw file missing for %s; skipping", year)
            continue
        try:
            df_year = data_download.load_raw_csv(path)
            df_year["year"] = year
            frames.append(df_year)
        except Exception as exc:  # pragma: no cover - I/O
            logger.warning("Could not load %s: %s", path, exc)
    if not frames:
        return _generate_sample_data(year_list)
    combined = pd.concat(frames, ignore_index=True)
    return combined


def _normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df.columns = [c.strip().lower() for c in df.columns]
    return df


def derive_time_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Add handy datetime-derived columns."""
    df = df.copy()
    if config.DATE_COLUMN in df.columns:
        datetime_values = pd.to_datetime(df[config.DATE_COLUMN], errors="coerce")
    else:
        datetime_values = pd.Series(pd.NaT, index=df.index, dtype="datetime64[ns]")
    df[config.DATE_COLUMN] = datetime_values
    derived_year = datetime_values.dt.year
    if "year" in df.columns:
        df["year"] = derived_year.fillna(df["year"])
    else:
        df["year"] = derived_year
    df["month"] = datetime_values.dt.month
    df["day"] = datetime_values.dt.day
    df["hour"] = datetime_values.dt.hour
    df["day_of_week_name"] = datetime_values.dt.day_name()
    return df


def preprocess(years: Iterable[int] | None = None) -> pd.DataFrame:
    """Clean raw CSVs and save a unified Parquet file."""
    config.ensure_directories()
    raw_df = load_raw_data(years)
    clean_df = _normalize_columns(raw_df)
    before_dedup = len(clean_df)
    clean_df = clean_df.drop_duplicates()
    removed_duplicates = before_dedup - len(clean_df)
    if removed_duplicates:
        logger.warning("Removed %s exact duplicate rows from raw extracts.", removed_duplicates)

    # Keep only essential columns if present
    missing_cols = [c for c in ESSENTIAL_COLUMNS if c not in clean_df.columns]
    if missing_cols:
        logger.warning("Missing expected columns: %s", ", ".join(missing_cols))
    selected_cols = ESSENTIAL_COLUMNS + OPTIONAL_COLUMNS
    present_cols = [c for c in selected_cols if c in clean_df.columns]
    non_essential = [c for c in clean_df.columns if c not in present_cols]
    ordered_cols = present_cols + non_essential
    clean_df = clean_df[ordered_cols]

    clean_df = derive_time_columns(clean_df)

    output_path = config.PROCESSED_DIR / "calls_clean.parquet"
    try:
        clean_df.to_parquet(output_path, index=False)
        logger.info("Saved cleaned data to %s", output_path)
    except Exception as exc:  # pragma: no cover - parquet dependency issues
        fallback = output_path.with_suffix(".csv")
        clean_df.to_csv(fallback, index=False)
        logger.warning("Parquet export failed (%s); saved CSV to %s", exc, fallback)
        output_path = fallback
    return clean_df


def load_processed(path: Path | None = None) -> pd.DataFrame:
    """Load the processed dataset, defaulting to the parquet path."""
    default_path = config.PROCESSED_DIR / "calls_clean.parquet"
    target = path or default_path
    if not target.exists():
        raise FileNotFoundError("Processed dataset not found. Run preprocess() first.")
    if target.suffix == ".csv":
        return pd.read_csv(target, parse_dates=[config.DATE_COLUMN])
    return pd.read_parquet(target)
