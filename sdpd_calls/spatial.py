"""Spatial utilities for beats, council districts, and hex grids."""
from __future__ import annotations

import logging
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
from shapely.geometry import Polygon

from . import aggregations, config

logger = logging.getLogger(__name__)


def _load_geojson(url: str, label: str) -> gpd.GeoDataFrame:
    """Load a GeoJSON resource and normalize its column names."""
    try:
        gdf = gpd.read_file(url)
        gdf = gdf.rename(columns={c: c.lower() for c in gdf.columns})
        return gdf
    except Exception as exc:  # pragma: no cover - network
        logger.warning("Could not load %s polygons: %s", label, exc)
        return gpd.GeoDataFrame()


def load_beats() -> gpd.GeoDataFrame:
    """Load SDPD beat polygons from the configured GeoJSON URL."""
    return _load_geojson(config.BEATS_GEOJSON_URL, "beat")


def load_council_districts() -> gpd.GeoDataFrame:
    """Load City of San Diego council district polygons."""
    return _load_geojson(config.COUNCIL_DISTRICTS_GEOJSON_URL, "council district")


def attach_council_district_membership(
    beats: gpd.GeoDataFrame,
    council_districts: gpd.GeoDataFrame,
) -> gpd.GeoDataFrame:
    """Attach intersecting council district labels to each beat."""
    if beats.empty:
        return beats

    enriched = beats.copy()
    if "council_districts" not in enriched.columns:
        enriched["council_districts"] = pd.NA

    if council_districts.empty or "district" not in council_districts.columns:
        return enriched

    beat_shapes = (
        beats.dropna(subset=["beat"])
        .drop_duplicates(subset=["beat"])
        [["beat", "geometry"]]
        .copy()
    )
    district_shapes = council_districts.dropna(subset=["district"])[["district", "geometry"]].copy()
    if beat_shapes.empty or district_shapes.empty:
        return enriched

    try:
        intersections = gpd.overlay(
            beat_shapes.to_crs(config.CRS_PROJECTED),
            district_shapes.to_crs(config.CRS_PROJECTED),
            how="intersection",
            keep_geom_type=True,
        )
    except Exception as exc:  # pragma: no cover - geometry engine edge cases
        logger.warning("Could not attach council district membership: %s", exc)
        return enriched

    if intersections.empty:
        return enriched

    intersections["intersection_area"] = intersections.geometry.area
    intersections = intersections[intersections["intersection_area"] > 0]
    if intersections.empty:
        return enriched

    district_lists = (
        intersections.assign(district=lambda df: df["district"].astype(str).str.strip())
        .sort_values(["beat", "intersection_area", "district"], ascending=[True, False, True])
        .groupby("beat")["district"]
        .agg(lambda values: ", ".join(dict.fromkeys(values)))
        .reset_index(name="council_districts")
    )
    merged = enriched.merge(district_lists, how="left", on="beat", suffixes=("", "_computed"))
    merged["council_districts"] = merged["council_districts_computed"].fillna(merged["council_districts"])
    return merged.drop(columns=["council_districts_computed"])


def attach_beat_stats(beats: gpd.GeoDataFrame, beat_counts: pd.DataFrame) -> gpd.GeoDataFrame:
    """Join beat polygons to aggregated stats."""
    if beats.empty:
        return beats
    beat_counts = beat_counts.rename(columns={config.BEAT_COLUMN: "beat"})
    merged = beats.merge(beat_counts, how="left", left_on="beat", right_on="beat")
    merged["call_count"] = merged["call_count"].fillna(0)
    return merged


def _hexagon(center_x: float, center_y: float, size: float) -> Polygon:
    angles = np.deg2rad(np.arange(0, 360, 60))
    points = [
        (center_x + size * np.cos(a), center_y + size * np.sin(a))
        for a in angles
    ]
    return Polygon(points)


def generate_hex_grid(boundary: gpd.GeoSeries, size_m: float = config.HEX_SIZE_METERS) -> gpd.GeoDataFrame:
    """Generate a hex grid covering the provided boundary geometry."""
    if boundary.empty:
        return gpd.GeoDataFrame(columns=["geometry"], geometry="geometry", crs=config.CRS_PROJECTED)
    boundary_merged = boundary.unary_union
    boundary_projected = gpd.GeoSeries([boundary_merged], crs=config.CRS_WGS84).to_crs(config.CRS_PROJECTED).iloc[0]
    minx, miny, maxx, maxy = boundary_projected.bounds

    dx = size_m * 3**0.5
    dy = size_m * 1.5

    hexes = []
    y = miny - dy
    row = 0
    while y < maxy + dy:
        x = minx - dx
        if row % 2:
            x += dx / 2
        while x < maxx + dx:
            hex_poly = _hexagon(x, y, size_m)
            if hex_poly.intersects(boundary_projected):
                hexes.append(hex_poly.intersection(boundary_projected))
            x += dx
        y += dy
        row += 1

    grid = gpd.GeoDataFrame({"hex_id": range(len(hexes)), "geometry": hexes}, crs=config.CRS_PROJECTED)
    grid = grid.to_crs(config.CRS_WGS84)
    return grid


def points_from_calls(df: pd.DataFrame) -> gpd.GeoDataFrame:
    """Create a GeoDataFrame of call points using configured coordinate columns."""
    if not set(config.COORD_COLUMNS).issubset(df.columns):
        logger.warning("Coordinate columns %s not found; skipping point geometry", config.COORD_COLUMNS)
        return gpd.GeoDataFrame()
    pts = gpd.GeoDataFrame(
        df,
        geometry=gpd.points_from_xy(df[config.COORD_COLUMNS[0]], df[config.COORD_COLUMNS[1]], crs=config.CRS_WGS84),
    )
    return pts


def hex_counts(points: gpd.GeoDataFrame, grid: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """Spatial join calls to hex grid and count per year."""
    if points.empty or grid.empty:
        logger.warning("Cannot compute hex counts; missing points or grid")
        return gpd.GeoDataFrame()
    points_proj = points.to_crs(config.CRS_PROJECTED)
    grid_proj = grid.to_crs(config.CRS_PROJECTED)

    joined = gpd.sjoin(points_proj, grid_proj, how="inner", predicate="within")
    grouped = (
        joined.groupby(["hex_id", "year"])
        .size()
        .reset_index(name="call_count")
    )
    merged = grid_proj.merge(grouped, on="hex_id", how="left").fillna({"call_count": 0, "year": points_proj["year"].min()})
    return merged.to_crs(config.CRS_WGS84)


def export_geojson(gdf: gpd.GeoDataFrame, path: Path | str) -> None:
    """Export a GeoDataFrame to GeoJSON."""
    if gdf.empty:
        logger.warning("GeoDataFrame is empty; nothing to export to %s", path)
        return
    gdf.to_file(path, driver="GeoJSON")


def build_spatial_products(df: pd.DataFrame) -> dict[str, gpd.GeoDataFrame]:
    """Create beat-level and hex-level GeoDataFrames ready for visualization."""
    beats = load_beats()
    council_districts = load_council_districts()
    beat_counts = aggregations.beat_by_year(df)
    beat_geo = attach_beat_stats(beats, beat_counts)

    beat_summary = (
        beat_counts.groupby(config.BEAT_COLUMN)["call_count"].sum().reset_index(name="total_calls")
    )
    if not beat_summary.empty and not beat_geo.empty:
        beat_geo = beat_geo.merge(beat_summary, how="left", on=config.BEAT_COLUMN)
    if not beat_geo.empty:
        beat_geo = attach_council_district_membership(beat_geo, council_districts)

    points = points_from_calls(df)
    hex_grid = gpd.GeoDataFrame()
    hex_stats = gpd.GeoDataFrame()
    if not points.empty:
        boundary = beat_geo.geometry if not beat_geo.empty else points.geometry
        hex_grid = generate_hex_grid(boundary)
        hex_stats = hex_counts(points, hex_grid)

    return {
        "beats": beat_geo,
        "council_districts": council_districts,
        "hex_grid": hex_grid,
        "hex_stats": hex_stats,
    }
