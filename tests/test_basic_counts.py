import pathlib
import sys

import geopandas as gpd
import pytest
import pandas as pd
from shapely.geometry import Polygon

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.append(str(ROOT))

from sdpd_calls import aggregations, config, data_download, preprocess, spatial, visualization


def test_processed_exists():
    target = config.PROCESSED_DIR / "calls_clean.parquet"
    alt = target.with_suffix(".csv")
    if not target.exists() and not alt.exists():
        pytest.skip("Processed dataset missing; run python run_all.py first.")
    df = preprocess.load_processed(target if target.exists() else alt)
    expected_cols = {config.DATE_COLUMN, "call_type", config.BEAT_COLUMN, "year", "hour", "day_of_week_name"}
    missing = expected_cols - set(df.columns)
    assert not missing, f"Missing columns: {missing}"
    assert len(df) > 0


def test_load_raw_csv_handles_uppercase_headers_and_mixed_numeric_values(tmp_path):
    sample = tmp_path / "sample.csv"
    sample.write_text(
        "\n".join(
            [
                "INCIDENT_NUM,DATE_TIME,DAY_OF_WEEK,CALL_TYPE,DISPOSITION,BEAT,PRIORITY",
                "E1,2025-01-01 00:00:00.000,3,INFO,R,CC,2",
                "E2,2025-01-01 01:00:00.000,3,PLATE,R,233,1",
            ]
        ),
        encoding="utf-8",
    )

    result = data_download.load_raw_csv(sample)

    assert config.DATE_COLUMN in result.columns
    assert pd.api.types.is_datetime64_any_dtype(result[config.DATE_COLUMN])
    assert pd.isna(result.loc[0, config.BEAT_COLUMN])
    assert result.loc[1, config.BEAT_COLUMN] == 233


def test_preprocess_handles_missing_columns(monkeypatch):
    """Test that preprocess handles missing columns gracefully."""
    df = pd.DataFrame(
        {
            "incident_num": [12345, 67890],
            "call_type": ["DISTURBANCE", "VEHICLE STOP"],
            "priority": [2, 1],
            "x": [-117.16, -117.17],
            "y": [32.72, 32.73],
        }
    )

    monkeypatch.setattr(preprocess, "load_raw_data", lambda years: df)

    result = preprocess.preprocess(years=[2026])

    assert "incident_num" in result.columns
    assert config.DATE_COLUMN in result.columns
    assert "call_type" in result.columns
    assert "priority" in result.columns
    assert result[config.DATE_COLUMN].isna().all()
    assert len(result) == 2


def test_preprocess_deduplicates_exact_rows(monkeypatch):
    df = pd.DataFrame(
        {
            "incident_num": ["A1", "A1"],
            config.DATE_COLUMN: ["2024-01-01 00:00:00", "2024-01-01 00:00:00"],
            "call_type": ["INFO", "INFO"],
            "disposition": ["R", "R"],
            config.BEAT_COLUMN: [111, 111],
            "priority": [1, 1],
        }
    )

    monkeypatch.setattr(preprocess, "load_raw_data", lambda years: df)

    result = preprocess.preprocess(years=[2024])

    assert len(result) == 1
    assert result["year"].iloc[0] == 2024


def test_beat_calltype_by_year_normalizes_duplicate_codes():
    df = pd.DataFrame(
        {
            config.BEAT_COLUMN: [611, 611, 611, 611],
            "year": [2025, 2025, 2025, 2025],
            "call_type": ["plate", "Plate", " PLATE ", "911p"],
        }
    )

    result = aggregations.beat_calltype_by_year(df)
    lookup = result.set_index("call_category")["call_count"].to_dict()

    assert lookup["PLATE"] == 3
    assert lookup["911P"] == 1


def test_hex_choropleth_returns_placeholder_for_missing_geometry():
    fig = visualization.hex_choropleth(gpd.GeoDataFrame(), 2025)

    assert fig.layout.title.text == "Calls for Service Hex Density"
    assert fig.layout.annotations
    assert "x/y coordinates" in fig.layout.annotations[0].text


def test_beat_choropleth_html_contains_calltype_controls():
    beat_geo = gpd.GeoDataFrame(
        {
            "beat": [101, 101, 102, 102],
            "div": [1, 1, 2, 2],
            "name": ["ALPHA", "ALPHA", "BRAVO", "BRAVO"],
            "council_districts": ["1", "1", "2", "2"],
            "year": [2024, 2025, 2024, 2025],
            "call_count": [50, 60, 40, 55],
            "geometry": [
                Polygon([(0, 0), (1, 0), (1, 1), (0, 1)]),
                Polygon([(0, 0), (1, 0), (1, 1), (0, 1)]),
                Polygon([(1, 0), (2, 0), (2, 1), (1, 1)]),
                Polygon([(1, 0), (2, 0), (2, 1), (1, 1)]),
            ],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )
    beat_calltypes = pd.DataFrame(
        {
            "beat": [101, 101, 102, 102],
            "year": [2024, 2025, 2024, 2025],
            "call_category": ["415", "415", "911", "911"],
            "call_count": [10, 12, 7, 9],
        }
    )
    beat_priorities = pd.DataFrame(
        {
            "beat": [101, 101, 102, 102],
            "year": [2025, 2025, 2025, 2025],
            "priority_label": ["1", "2", "1", "3"],
            "call_count": [22, 18, 20, 11],
        }
    )
    beat_dispositions = pd.DataFrame(
        {
            "beat": [101, 101, 102, 102],
            "year": [2025, 2025, 2025, 2025],
            "disposition_label": ["K", "R", "R", "G"],
            "call_count": [14, 31, 20, 10],
        }
    )
    beat_hourly = pd.DataFrame(
        {
            "beat": [101, 101, 102, 102],
            "year": [2025, 2025, 2025, 2025],
            "day_of_week_name": ["Monday", "Tuesday", "Monday", "Tuesday"],
            "hour": [8, 9, 10, 11],
            "call_count": [5, 6, 4, 3],
        }
    )
    call_type_reference = pd.DataFrame(
        {
            "call_type": ["415", "911"],
            "description": ["DISTURBANCE", "EMERGENCY"],
        }
    )
    council_districts = gpd.GeoDataFrame(
        {
            "district": [1, 2],
            "geometry": [
                Polygon([(0, 0), (1.5, 0), (1.5, 1.2), (0, 1.2)]),
                Polygon([(1.5, 0), (2.5, 0), (2.5, 1.2), (1.5, 1.2)]),
            ],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )

    html = visualization.beat_choropleth_html(
        beat_geo,
        beat_calltypes,
        beat_priorities,
        beat_dispositions,
        beat_hourly,
        call_type_reference,
        2025,
        council_districts=council_districts,
    )

    assert "call-type-select" in html
    assert "call-type-search" in html
    assert "metric-mode-select" in html
    assert "year-range" in html
    assert "secondary-plot-select" in html
    assert "top-insight" in html
    assert "selected-beat-label" in html
    assert "beat-heatmap-chart" in html
    assert "top-calltypes-chart" in html
    assert "priority-mix-chart" in html
    assert "disposition-mix-chart" in html
    assert "Beat-Level Call Type Choropleth" in html
    assert "All Calls" in html
    assert "415 - DISTURBANCE" in html
    assert "911 - EMERGENCY" in html
    assert "Division: ${meta.division}" in html
    assert "Council district(s): ${meta.councilDistricts}" in html
    assert "Priority Mix" in html
    assert "Disposition Mix" in html
    assert "callTypeGroups" in html
    assert "Index vs city" in html
    assert "Calls for service are dispatch and response records, not confirmed crimes." in html
    assert "districtBoundaryCoords" in html
    assert "Council district borders are drawn as dark outlines" in html
    assert "plotly_unhover" in html
    assert "curveNumber !== 0" in html
    assert "state.hoverBeatIndex = null" in html


def test_attach_council_district_membership_lists_intersecting_districts():
    beats = gpd.GeoDataFrame(
        {
            "beat": [101, 102],
            "geometry": [
                Polygon([(0, 0), (2, 0), (2, 1), (0, 1)]),
                Polygon([(2, 0), (3, 0), (3, 1), (2, 1)]),
            ],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )
    council_districts = gpd.GeoDataFrame(
        {
            "district": [1, 2, 3],
            "geometry": [
                Polygon([(0, 0), (1, 0), (1, 1), (0, 1)]),
                Polygon([(1, 0), (2, 0), (2, 1), (1, 1)]),
                Polygon([(2, 0), (3, 0), (3, 1), (2, 1)]),
            ],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )

    result = spatial.attach_council_district_membership(beats, council_districts)

    district_lookup = result.set_index("beat")["council_districts"].to_dict()
    assert district_lookup[101] == "1, 2"
    assert district_lookup[102] == "3"


def test_beat_choropleth_adds_council_district_overlay():
    beat_geo = gpd.GeoDataFrame(
        {
            "beat": [101, 102],
            "year": [2025, 2025],
            "call_count": [60, 55],
            "total_calls": [240, 220],
            "geometry": [
                Polygon([(0, 0), (1, 0), (1, 1), (0, 1)]),
                Polygon([(1, 0), (2, 0), (2, 1), (1, 1)]),
            ],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )
    council_districts = gpd.GeoDataFrame(
        {
            "district": [1],
            "geometry": [Polygon([(0, 0), (2, 0), (2, 1), (0, 1)])],
        },
        geometry="geometry",
        crs="EPSG:4326",
    )

    fig = visualization.beat_choropleth(beat_geo, 2025, council_districts=council_districts)

    assert len(fig.data) == 2
    assert fig.data[1].type == "scattermapbox"
    assert fig.data[1].name == "Council District Boundaries"
