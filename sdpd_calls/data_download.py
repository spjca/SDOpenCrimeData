"""Download SDPD calls-for-service CSVs."""
from __future__ import annotations

import logging
from pathlib import Path
from typing import Iterable

import pandas as pd
import requests

from . import config

logger = logging.getLogger(__name__)

STRING_COLUMNS = (
    "incident_num",
    "address_dir_primary",
    "address_road_primary",
    "address_sfx_primary",
    "address_dir_intersecting",
    "address_road_intersecting",
    "address_sfx_intersecting",
    "call_type",
    "disposition",
)

NUMERIC_COLUMNS = (
    "day_of_week",
    "address_number_primary",
    "beat",
    "priority",
)

CALL_TYPE_REFERENCE_FILES = (
    (config.CALL_TYPES_HISTORICAL_URL, config.RAW_DIR / "pd_cfs_calltypes_historical_datasd.csv"),
    (config.CALL_TYPES_CURRENT_URL, config.RAW_DIR / "pd_cfs_calltypes_datasd.csv"),
)


def _download_file(url: str, dest: Path) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    try:
        response = requests.get(url, headers={"User-Agent": "Mozilla/5.0"}, timeout=60)
        response.raise_for_status()
        dest.write_bytes(response.content)
        logger.info("Downloaded %s", dest)
    except Exception as exc:  # pragma: no cover - network errors
        logger.warning("Could not download %s: %s", url, exc)


def download_year(year: int) -> Path:
    """Download a single year's CSV if missing and return the local path."""
    url = config.BASE_CALLS_URL.format(year=year)
    dest = config.RAW_DIR / f"pd_calls_for_service_{year}_datasd.csv"
    if dest.exists():
        logger.info("Skipping download for %s (already exists)", year)
        return dest
    _download_file(url, dest)
    return dest


def download_all(years: Iterable[int] | None = None) -> list[Path]:
    """Download all configured years, returning the list of local file paths."""
    config.ensure_directories()
    year_list = list(years) if years is not None else config.years_to_process()
    paths: list[Path] = []
    for year in year_list:
        paths.append(download_year(year))
    return paths


def _normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df.columns = [column.strip().lower() for column in df.columns]
    return df


def load_raw_csv(path: Path, nrows: int | None = None) -> pd.DataFrame:
    """Load a raw CSV with sensible defaults."""
    df = pd.read_csv(path, nrows=nrows, low_memory=False)
    df = _normalize_columns(df)

    if config.DATE_COLUMN in df.columns:
        df[config.DATE_COLUMN] = pd.to_datetime(df[config.DATE_COLUMN], errors="coerce")

    for column in STRING_COLUMNS:
        if column in df.columns:
            df[column] = df[column].astype("string")

    for column in NUMERIC_COLUMNS:
        if column in df.columns:
            df[column] = pd.to_numeric(df[column], errors="coerce").astype("Int64")

    return df


def download_call_type_reference() -> list[Path]:
    """Download call-type reference CSVs if missing and return local paths."""
    config.ensure_directories()
    paths: list[Path] = []
    for url, dest in CALL_TYPE_REFERENCE_FILES:
        if dest.exists():
            paths.append(dest)
            continue
        _download_file(url, dest)
        if dest.exists():
            paths.append(dest)
    return paths


def _normalize_call_type_reference(df: pd.DataFrame) -> pd.DataFrame:
    df = _normalize_columns(df)
    df = df.rename(columns={"\ufeffcall_type": "call_type"})
    keep = [column for column in ("call_type", "description") if column in df.columns]
    if len(keep) < 2:
        return pd.DataFrame(columns=["call_type", "description"])
    normalized = df[keep].copy()
    normalized["call_type"] = normalized["call_type"].astype("string").str.strip().str.upper()
    normalized["description"] = normalized["description"].astype("string").str.strip()
    normalized = normalized.dropna(subset=["call_type", "description"])
    normalized = normalized[normalized["call_type"] != ""]
    normalized = normalized[normalized["description"] != ""]
    return normalized


def load_call_type_reference() -> pd.DataFrame:
    """Load the combined call-type reference table, preferring current definitions."""
    config.ensure_directories()
    download_call_type_reference()

    frames: list[pd.DataFrame] = []
    for _, path in CALL_TYPE_REFERENCE_FILES:
        if not path.exists():
            continue
        try:
            frames.append(_normalize_call_type_reference(pd.read_csv(path)))
        except Exception as exc:  # pragma: no cover - I/O
            logger.warning("Could not load call-type reference %s: %s", path, exc)

    if not frames:
        return pd.DataFrame(columns=["call_type", "description"])

    combined = pd.concat(frames, ignore_index=True)
    combined = combined.drop_duplicates(subset=["call_type"], keep="last")
    combined = combined.sort_values("call_type").reset_index(drop=True)
    return combined
