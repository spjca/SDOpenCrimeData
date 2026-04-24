"""Configuration for SDPD calls-for-service pipeline."""
from __future__ import annotations

import os
from pathlib import Path
from typing import List

BASE_CALLS_URL = (
    "https://seshat.datasd.org/police_calls_for_service/pd_calls_for_service_{year}_datasd.csv"
)
CALL_TYPES_CURRENT_URL = "https://seshat.datasd.org/police_calls_for_service/pd_cfs_calltypes_datasd.csv"
CALL_TYPES_HISTORICAL_URL = "https://seshat.datasd.org/police_calls_for_service/pd_cfs_calltypes_historical_datasd.csv"
BEATS_GEOJSON_URL = "https://seshat.datasd.org/gis_police_beats/pd_beats_datasd.geojson"
COUNCIL_DISTRICTS_GEOJSON_URL = "https://seshat.datasd.org/gis_city_council_districts/council_districts_datasd.geojson"

DATA_DIR = Path("data")
RAW_DIR = DATA_DIR / "raw"
PROCESSED_DIR = DATA_DIR / "processed"
OUTPUT_DIR = Path("outputs")

DEFAULT_YEARS = list(range(2016, 2026))

DATE_COLUMN = "date_time"
COORD_COLUMNS = ("x", "y")
BEAT_COLUMN = "beat"

CRS_WGS84 = "EPSG:4326"
CRS_PROJECTED = "EPSG:3857"

HEX_SIZE_METERS = 800


def years_to_process() -> List[int]:
    """Return the list of years to process, reading the SDPD_YEARS env var if set."""
    env_years = os.getenv("SDPD_YEARS")
    if env_years:
        parsed: List[int] = []
        for part in env_years.split(","):
            part = part.strip()
            if not part:
                continue
            try:
                parsed.append(int(part))
            except ValueError:
                continue
        if parsed:
            return parsed
    return DEFAULT_YEARS


def ensure_directories() -> None:
    """Create local data/output directories if missing."""
    for path in (RAW_DIR, PROCESSED_DIR, OUTPUT_DIR):
        path.mkdir(parents=True, exist_ok=True)
