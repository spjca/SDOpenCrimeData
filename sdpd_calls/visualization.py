"""Visualization helpers using Plotly."""
from __future__ import annotations

import json

import geopandas as gpd
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import plotly.io as pio
from shapely.geometry import mapping

from . import aggregations, config

WEB_GEOMETRY_PRECISION = 5
WEB_GEOMETRY_SIMPLIFY_TOLERANCE = 0.00002


def _json_safe_value(value: object) -> object:
    if pd.isna(value):
        return None
    if hasattr(value, "item"):
        return value.item()
    return value


def _round_nested_coordinates(value: object, precision: int) -> object:
    if isinstance(value, float):
        return round(value, precision)
    if isinstance(value, (list, tuple)):
        return [_round_nested_coordinates(item, precision) for item in value]
    if isinstance(value, dict):
        return {key: _round_nested_coordinates(item, precision) for key, item in value.items()}
    return value


def _geojson_from_gdf(
    gdf: gpd.GeoDataFrame,
    property_columns: list[str] | None = None,
    simplify_tolerance: float = 0.0,
    precision: int | None = None,
) -> dict:
    if property_columns is None and not simplify_tolerance and precision is None:
        return json.loads(gdf.to_json())

    features = []
    for row in gdf.itertuples(index=False):
        geometry = row.geometry
        if geometry is None or geometry.is_empty:
            continue
        if simplify_tolerance:
            geometry = geometry.simplify(simplify_tolerance, preserve_topology=True)
        geometry_mapping = mapping(geometry)
        if precision is not None:
            geometry_mapping = _round_nested_coordinates(geometry_mapping, precision)
        properties = {
            column: _json_safe_value(getattr(row, column))
            for column in (property_columns or [])
        }
        features.append(
            {
                "type": "Feature",
                "properties": properties,
                "geometry": geometry_mapping,
            }
        )
    return {"type": "FeatureCollection", "features": features}


def _web_geojson_from_gdf(gdf: gpd.GeoDataFrame, property_columns: list[str]) -> dict:
    return _geojson_from_gdf(
        gdf,
        property_columns=property_columns,
        simplify_tolerance=WEB_GEOMETRY_SIMPLIFY_TOLERANCE,
        precision=WEB_GEOMETRY_PRECISION,
    )


def _full_geojson_from_gdf(gdf: gpd.GeoDataFrame) -> dict:
    return json.loads(gdf.to_json())


def _message_figure(title: str, message: str) -> go.Figure:
    fig = go.Figure()
    fig.add_annotation(
        text=message,
        x=0.5,
        y=0.5,
        xref="paper",
        yref="paper",
        showarrow=False,
        align="center",
        font={"size": 16},
    )
    fig.update_layout(
        title=title,
        template="plotly_white",
        xaxis={"visible": False},
        yaxis={"visible": False},
    )
    return fig


def _build_beat_history(beat_geo: gpd.GeoDataFrame) -> dict[object, str]:
    if beat_geo.empty or "year" not in beat_geo.columns:
        return {}

    history_df = (
        beat_geo[["beat", "year", "call_count"]]
        .dropna(subset=["beat", "year"])
        .copy()
    )
    history_df["year"] = history_df["year"].astype(int)
    history_df["call_count"] = history_df["call_count"].fillna(0).astype(int)
    history_lookup: dict[object, str] = {}
    for beat, group in history_df.sort_values(["beat", "year"]).groupby("beat"):
        history_lookup[beat] = "<br>".join(
            f"{int(row.year)}: {int(row.call_count):,}"
            for row in group.itertuples(index=False)
        )
    return history_lookup


def _prepare_beat_shapes(beat_geo: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    beat_shapes = (
        beat_geo.dropna(subset=["beat"])
        .sort_values("beat")
        .drop_duplicates(subset=["beat"])
        .copy()
    )
    beat_shapes["beat_key"] = beat_shapes["beat"].astype(str)
    return beat_shapes


def _prepare_council_district_shapes(council_districts: gpd.GeoDataFrame | None) -> gpd.GeoDataFrame:
    if council_districts is None or council_districts.empty or "district" not in council_districts.columns:
        return gpd.GeoDataFrame()

    district_shapes = (
        council_districts.dropna(subset=["district"])
        .sort_values("district")
        .drop_duplicates(subset=["district"])
        .copy()
    )
    district_shapes["district"] = district_shapes["district"].astype(str)
    return district_shapes


def _line_coordinates_from_shapes(shapes: gpd.GeoDataFrame) -> dict[str, list[float | None]]:
    latitudes: list[float | None] = []
    longitudes: list[float | None] = []

    if shapes.empty:
        return {"lat": latitudes, "lon": longitudes}

    for geometry in shapes.geometry:
        if geometry is None or geometry.is_empty:
            continue
        geometry = geometry.simplify(WEB_GEOMETRY_SIMPLIFY_TOLERANCE, preserve_topology=True)
        boundary = geometry.boundary
        if boundary.geom_type == "LineString":
            lines = [boundary]
        elif boundary.geom_type == "MultiLineString":
            lines = list(boundary.geoms)
        else:
            continue

        for line in lines:
            xs, ys = line.xy
            longitudes.extend(round(value, WEB_GEOMETRY_PRECISION) for value in xs)
            latitudes.extend(round(value, WEB_GEOMETRY_PRECISION) for value in ys)
            longitudes.append(None)
            latitudes.append(None)

    return {"lat": latitudes, "lon": longitudes}


def _district_outline_trace(council_districts: gpd.GeoDataFrame | None) -> go.Scattermapbox | None:
    district_shapes = _prepare_council_district_shapes(council_districts)
    coordinates = _line_coordinates_from_shapes(district_shapes)
    if not coordinates["lat"]:
        return None

    return go.Scattermapbox(
        lat=coordinates["lat"],
        lon=coordinates["lon"],
        mode="lines",
        line={"color": "#1b2633", "width": 2},
        hoverinfo="skip",
        name="Council District Boundaries",
        showlegend=False,
    )


def _format_meta_value(value: object, fallback: str = "Unavailable") -> str:
    if pd.isna(value):
        return fallback
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    text = str(value).strip()
    return text if text else fallback


def _build_beat_meta(beat_shapes: gpd.GeoDataFrame) -> dict[str, dict[str, str]]:
    beat_meta: dict[str, dict[str, str]] = {}
    for row in beat_shapes.itertuples(index=False):
        beat_meta[str(row.beat_key)] = {
            "name": _format_meta_value(getattr(row, "name", None)),
            "division": _format_meta_value(getattr(row, "div", None)),
            "councilDistricts": _format_meta_value(getattr(row, "council_districts", None)),
        }
    return beat_meta


def _build_total_counts_matrix(
    beat_geo: gpd.GeoDataFrame,
    beat_keys: list[str],
    years: list[int],
) -> list[list[int]]:
    total_counts = (
        beat_geo[["beat", "year", "call_count"]]
        .dropna(subset=["beat", "year"])
        .drop_duplicates(subset=["beat", "year"])
        .copy()
    )
    total_counts["beat_key"] = total_counts["beat"].astype(str)
    total_pivot = (
        total_counts.pivot(index="beat_key", columns="year", values="call_count")
        .reindex(index=beat_keys, columns=years)
        .fillna(0)
        .astype(int)
    )
    return total_pivot.T.values.tolist()


def _build_category_counts(
    count_df: pd.DataFrame,
    beat_keys: list[str],
    years: list[int],
    category_column: str,
    numeric_sort: bool = False,
) -> tuple[list[str], dict[str, list[list[int]]]]:
    if count_df.empty:
        return [], {}

    counts = count_df.dropna(subset=[category_column, "beat", "year"]).copy()
    if counts.empty:
        return [], {}

    counts[category_column] = counts[category_column].astype(str).str.strip()
    counts["beat_key"] = counts["beat"].astype(str)

    if numeric_sort:
        ordered_categories = sorted(
            counts[category_column].unique().tolist(),
            key=lambda value: (int(value), value) if value.isdigit() else (9999, value),
        )
    else:
        category_totals = (
            counts.groupby(category_column)["call_count"]
            .sum()
            .sort_values(ascending=False)
        )
        ordered_categories = [str(category) for category in category_totals.index.tolist()]

    counts_by_category: dict[str, list[list[int]]] = {}
    for category, group in counts.groupby(category_column, sort=False):
        category_key = str(category)
        pivot = (
            group.pivot(index="beat_key", columns="year", values="call_count")
            .reindex(index=beat_keys, columns=years)
            .fillna(0)
            .astype(int)
        )
        counts_by_category[category_key] = pivot.T.values.tolist()

    return ordered_categories, counts_by_category


def _build_calltype_counts(
    beat_calltype_counts: pd.DataFrame,
    beat_keys: list[str],
    years: list[int],
) -> tuple[list[str], dict[str, list[list[int]]]]:
    calltype_counts = (
        beat_calltype_counts.dropna(subset=["call_category", "beat", "year"])
        .copy()
    )
    calltype_counts["call_category"] = aggregations.normalize_call_type_series(calltype_counts["call_category"])
    calltype_counts = calltype_counts[calltype_counts["call_category"].notna() & (calltype_counts["call_category"] != "")]
    calltype_counts["beat_key"] = calltype_counts["beat"].astype(str)
    calltype_totals = (
        calltype_counts.groupby("call_category")["call_count"]
        .sum()
        .sort_values(ascending=False)
    )

    ordered_call_types = [str(call_type) for call_type in calltype_totals.index.tolist()]
    counts_by_type: dict[str, list[list[int]]] = {}
    for call_type, group in calltype_counts.groupby("call_category", sort=False):
        call_type_key = str(call_type)
        pivot = (
            group.pivot(index="beat_key", columns="year", values="call_count")
            .reindex(index=beat_keys, columns=years)
            .fillna(0)
            .astype(int)
        )
        counts_by_type[call_type_key] = pivot.T.values.tolist()

    return ordered_call_types, counts_by_type


def _build_call_type_labels(
    ordered_call_types: list[str],
    call_type_reference: pd.DataFrame | None,
    all_calls_label: str,
) -> dict[str, str]:
    labels = {all_calls_label: all_calls_label}
    if call_type_reference is not None and not call_type_reference.empty:
        lookup = (
            call_type_reference.dropna(subset=["call_type", "description"])
            .assign(call_type=lambda df: aggregations.normalize_call_type_series(df["call_type"]))
            .assign(description=lambda df: df["description"].astype(str).str.strip())
            .drop_duplicates(subset=["call_type"], keep="last")
            .set_index("call_type")["description"]
            .to_dict()
        )
    else:
        lookup = {}

    for call_type in ordered_call_types:
        description = lookup.get(call_type, "").strip()
        labels[call_type] = f"{call_type} - {description}" if description else call_type
    return labels


CALL_TYPE_GROUP_ORDER = [
    "All Calls",
    "Emergency, Welfare, and Medical",
    "Violence and Threats",
    "Theft, Burglary, and Property",
    "Traffic and Vehicle",
    "Noise and Disturbance",
    "Drugs, Alcohol, and Vice",
    "Admin, Info, and Follow-up",
    "Other",
]


def _call_type_group(call_type: str, label: str) -> str:
    text = f"{call_type} {label}".upper()
    if call_type == "All Calls":
        return "All Calls"
    if any(token in text for token in ("911", "WELFARE", "MENTAL", "SUICIDE", "MEDICAL", "MISSING", "FOUND PERSON")):
        return "Emergency, Welfare, and Medical"
    if any(token in text for token in ("ASSAULT", "BATTERY", "ROBBERY", "WEAPON", "STABB", "SHOOT", "THREAT", "FIGHT", "DOMESTIC")):
        return "Violence and Threats"
    if any(token in text for token in ("THEFT", "BURGL", "BURGLARY", "SHOPLIFT", "PROPERTY", "VANDAL", "TRESP", "FRAUD", "STOLEN")):
        return "Theft, Burglary, and Property"
    if any(token in text for token in ("TRAFFIC", "VEH", "VEHICLE", "PLATE", "PARK", "TOW", "ACCIDENT", "COLLISION", "DUI", "PED", "BICYCLE")):
        return "Traffic and Vehicle"
    if any(token in text for token in ("DISTURB", "NOISE", "PARTY", "MUSIC", "LOUD", "NUISANCE")):
        return "Noise and Disturbance"
    if any(token in text for token in ("DRUG", "NARC", "ALCOHOL")):
        return "Drugs, Alcohol, and Vice"
    if any(token in text for token in ("INFO", "FOLLOW", "ADVICE", "REPORT", "CASE", "OFFICER", "SUBPOENA", "ADMIN")):
        return "Admin, Info, and Follow-up"
    return "Other"


def _build_call_type_groups(
    ordered_call_types: list[str],
    call_type_labels: dict[str, str],
    all_calls_label: str,
) -> list[dict[str, object]]:
    grouped: dict[str, list[str]] = {group: [] for group in CALL_TYPE_GROUP_ORDER}
    grouped[all_calls_label] = [all_calls_label]

    for call_type in ordered_call_types:
        group = _call_type_group(call_type, call_type_labels.get(call_type, call_type))
        grouped.setdefault(group, []).append(call_type)

    ordered_groups = [
        {"name": group, "callTypes": grouped[group]}
        for group in CALL_TYPE_GROUP_ORDER
        if grouped.get(group)
    ]
    ordered_groups.extend(
        {"name": group, "callTypes": call_types}
        for group, call_types in grouped.items()
        if group not in CALL_TYPE_GROUP_ORDER and call_types
    )
    return ordered_groups


def _build_beat_heatmaps(
    beat_hourly_counts: pd.DataFrame,
    beat_keys: list[str],
    years: list[int],
) -> list[list[list[list[int]]]]:
    if beat_hourly_counts.empty:
        return [
            [
                [[0 for _ in range(24)] for _ in aggregations.WEEKDAY_ORDER]
                for _ in years
            ]
            for _ in beat_keys
        ]

    data = beat_hourly_counts.dropna(
        subset=["beat", "year", "day_of_week_name", "hour", "call_count"]
    ).copy()
    data["beat_key"] = data["beat"].astype(str)
    data["weekday_index"] = data["day_of_week_name"].map(
        {day: index for index, day in enumerate(aggregations.WEEKDAY_ORDER)}
    )
    data["hour"] = data["hour"].astype(int)
    data = data[
        data["beat_key"].isin(beat_keys)
        & data["year"].isin(years)
        & data["weekday_index"].notna()
        & data["hour"].between(0, 23)
    ]

    beat_index = {beat_key: index for index, beat_key in enumerate(beat_keys)}
    year_index = {year: index for index, year in enumerate(years)}
    heatmaps = [
        [
            [[0 for _ in range(24)] for _ in aggregations.WEEKDAY_ORDER]
            for _ in years
        ]
        for _ in beat_keys
    ]

    for row in data.itertuples(index=False):
        heatmaps[beat_index[row.beat_key]][year_index[int(row.year)]][int(row.weekday_index)][int(row.hour)] = int(row.call_count)

    return heatmaps


def beat_choropleth_html(
    beat_geo: gpd.GeoDataFrame,
    beat_calltype_counts: pd.DataFrame,
    beat_priority_counts: pd.DataFrame,
    beat_disposition_counts: pd.DataFrame,
    beat_hourly_counts: pd.DataFrame,
    call_type_reference: pd.DataFrame | None,
    latest_year: int,
    council_districts: gpd.GeoDataFrame | None = None,
) -> str:
    """Return a standalone HTML explorer for beat-level call-type maps."""
    if beat_geo.empty:
        return pio.to_html(
            _message_figure("Calls for Service by Beat", "Beat boundary data is unavailable."),
            include_plotlyjs="cdn",
            full_html=True,
        )

    beat_shapes = _prepare_beat_shapes(beat_geo)
    if beat_shapes.empty:
        return pio.to_html(
            _message_figure("Calls for Service by Beat", "No beat geometry is available for mapping."),
            include_plotlyjs="cdn",
            full_html=True,
        )

    years = sorted(
        beat_geo["year"].dropna().astype(int).unique().tolist()
        if "year" in beat_geo.columns
        else [latest_year]
    )
    beat_keys = beat_shapes["beat_key"].tolist()
    total_counts = _build_total_counts_matrix(beat_geo, beat_keys, years)
    ordered_call_types, counts_by_type = _build_calltype_counts(beat_calltype_counts, beat_keys, years)
    priority_categories, priority_counts = _build_category_counts(
        beat_priority_counts,
        beat_keys,
        years,
        "priority_label",
        numeric_sort=True,
    )
    disposition_categories, disposition_counts = _build_category_counts(
        beat_disposition_counts,
        beat_keys,
        years,
        "disposition_label",
    )
    all_calls_label = "All Calls"
    call_type_labels = _build_call_type_labels(ordered_call_types, call_type_reference, all_calls_label)
    call_type_groups = _build_call_type_groups(ordered_call_types, call_type_labels, all_calls_label)
    priority_labels = {priority: f"Priority {priority}" for priority in priority_categories}
    beat_heatmaps = _build_beat_heatmaps(beat_hourly_counts, beat_keys, years)
    district_boundary_coords = _line_coordinates_from_shapes(
        _prepare_council_district_shapes(council_districts)
    )
    default_year = latest_year if latest_year in years else years[-1]
    default_year_index = years.index(default_year)
    default_beat_index = max(
        range(len(beat_keys)),
        key=lambda index: total_counts[default_year_index][index],
    )

    spec = {
        "title": "SDPD Calls for Service by Beat",
        "allCallsLabel": all_calls_label,
        "callTypes": [all_calls_label, *ordered_call_types],
        "callTypeLabels": call_type_labels,
        "callTypeGroups": call_type_groups,
        "priorityCategories": priority_categories,
        "priorityLabels": priority_labels,
        "priorityCounts": priority_counts,
        "dispositionCategories": disposition_categories,
        "dispositionCounts": disposition_counts,
        "weekdayOrder": aggregations.WEEKDAY_ORDER,
        "years": years,
        "beatKeys": beat_keys,
        "defaultYearIndex": default_year_index,
        "defaultBeatIndex": default_beat_index,
        "geojson": _web_geojson_from_gdf(beat_shapes, ["beat_key"]),
        "beatMeta": _build_beat_meta(beat_shapes),
        "districtBoundaryCoords": district_boundary_coords,
        "totalCounts": total_counts,
        "countsByType": counts_by_type,
        "beatHeatmaps": beat_heatmaps,
    }
    spec_json = json.dumps(spec, separators=(",", ":"))

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>SDPD Beat Call Type Explorer</title>
  <script charset="utf-8" src="https://cdn.plot.ly/plotly-3.5.0.min.js"></script>
  <style>
    body {{
      margin: 0;
      font-family: Arial, sans-serif;
      background: #f6f7f9;
      color: #16202a;
    }}
    .page {{
      padding: 16px;
      display: grid;
      gap: 16px;
    }}
    .controls {{
      display: grid;
      gap: 12px;
      grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
      background: #ffffff;
      padding: 16px;
      border-radius: 12px;
      box-shadow: 0 4px 18px rgba(19, 33, 68, 0.08);
    }}
    .wide-control {{
      grid-column: span 2;
    }}
    .control label {{
      display: block;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      margin-bottom: 6px;
      color: #566273;
    }}
    .control select,
    .control input {{
      width: 100%;
      box-sizing: border-box;
      min-height: 34px;
      margin-bottom: 6px;
    }}
    .insight-strip {{
      display: grid;
      gap: 10px;
      grid-template-columns: minmax(0, 1.5fr) auto minmax(220px, 0.8fr);
      align-items: center;
      background: #ffffff;
      padding: 12px 16px;
      border-radius: 12px;
      box-shadow: 0 4px 18px rgba(19, 33, 68, 0.08);
      font-size: 14px;
    }}
    .insight {{
      color: #16202a;
      font-weight: 700;
      line-height: 1.4;
    }}
    .state-chip {{
      justify-self: start;
      border: 1px solid #cad2dd;
      border-radius: 999px;
      padding: 6px 10px;
      color: #2d3742;
      background: #f7f9fc;
      white-space: nowrap;
    }}
    .caveat {{
      color: #617083;
      font-size: 12px;
      line-height: 1.4;
    }}
    .layout {{
      display: grid;
      gap: 16px;
      grid-template-columns: minmax(0, 1.15fr) minmax(460px, 0.95fr);
      grid-template-areas:
        "map detail";
    }}
    .panel {{
      background: #ffffff;
      border-radius: 12px;
      box-shadow: 0 4px 18px rgba(19, 33, 68, 0.08);
      overflow: hidden;
    }}
    .map-panel {{
      grid-area: map;
    }}
    .detail-panel {{
      grid-area: detail;
    }}
    .topcalls-panel {{
      grid-area: detail;
    }}
    .mix-panel {{
      grid-area: detail;
    }}
    .heatmap-panel {{
      grid-area: detail;
    }}
    .secondary-panel {{
      display: none;
    }}
    .secondary-panel.is-active {{
      display: block;
    }}
    .panel-header {{
      padding: 16px 16px 0 16px;
    }}
    .panel-header h1,
    .panel-header h2,
    .panel-header p {{
      margin: 0 0 8px 0;
    }}
    .panel-body {{
      padding: 0 8px 8px 8px;
    }}
    #beat-map {{
      min-height: 720px;
    }}
    #beat-detail-chart {{
      min-height: 420px;
    }}
    #top-calltypes-chart {{
      min-height: 560px;
    }}
    #beat-heatmap-chart {{
      min-height: 480px;
    }}
    .mix-grid {{
      display: grid;
      gap: 16px;
      grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
      padding: 0 8px 8px 8px;
    }}
    .mix-chart {{
      min-height: 340px;
    }}
    .summary {{
      padding: 0 16px 8px 16px;
      font-size: 14px;
      line-height: 1.5;
      color: #2d3742;
    }}
    .note {{
      padding: 0 16px 16px 16px;
      font-size: 12px;
      line-height: 1.5;
      color: #617083;
    }}
    @media (max-width: 960px) {{
      .layout {{
        grid-template-columns: 1fr;
        grid-template-areas:
          "map"
          "detail";
      }}
      .wide-control {{
        grid-column: span 1;
      }}
      .insight-strip {{
        grid-template-columns: 1fr;
      }}
      #beat-map {{
        min-height: 560px;
      }}
    }}
  </style>
</head>
<body>
  <div class="page">
    <div class="controls">
      <div class="control wide-control">
        <label for="call-type-select">Call Type</label>
        <input id="call-type-search" type="search" placeholder="Search grouped call types, codes, or descriptions">
        <select id="call-type-select"></select>
      </div>
      <div class="control">
        <label for="metric-mode-select">Metric</label>
        <select id="metric-mode-select">
          <option value="count">Count</option>
          <option value="share">Share of beat</option>
          <option value="yoy">YoY change</option>
          <option value="index">Index vs city</option>
        </select>
      </div>
      <div class="control">
        <label for="year-range">Year: <span id="current-year-label"></span></label>
        <input id="year-range" type="range" min="0" max="{len(years) - 1}" value="{default_year_index}" step="1">
      </div>
      <div class="control">
        <label for="secondary-plot-select">Secondary Plot</label>
        <select id="secondary-plot-select">
          <option value="detail">Trend</option>
          <option value="topcalls">Top Call Types</option>
          <option value="mix">Priority and Disposition</option>
          <option value="heatmap">Timing Pattern</option>
        </select>
      </div>
    </div>
    <div class="insight-strip">
      <div id="top-insight" class="insight"></div>
      <div id="selected-beat-label" class="state-chip"></div>
      <div class="caveat">Calls for service are dispatch and response records, not confirmed crimes.</div>
    </div>
    <div class="layout">
      <div class="panel map-panel">
        <div class="panel-header">
          <h1>Beat-Level Call Type Choropleth</h1>
          <p>Use the call type selector and year slider to compare how calls are distributed across SDPD beats.</p>
        </div>
        <div class="panel-body">
          <div id="beat-map"></div>
        </div>
        <div class="note">
          Council district borders are drawn as dark outlines so you can see how police beats line up with elected districts.
        </div>
      </div>
      <div class="panel detail-panel secondary-panel is-active" data-secondary-panel="detail">
        <div class="panel-header">
          <h2 id="beat-detail-title">Beat Detail</h2>
        </div>
        <div class="summary" id="beat-summary"></div>
        <div class="panel-body">
          <div id="beat-detail-chart"></div>
        </div>
        <div class="note">
          Hover or click a beat on the map to update this trend chart. Count mode also shows all calls as a dashed comparison line.
        </div>
      </div>
      <div class="panel topcalls-panel secondary-panel" data-secondary-panel="topcalls">
        <div class="panel-header">
          <h2 id="top-calltypes-title">Top Call Types</h2>
        </div>
        <div class="summary" id="top-calltypes-summary"></div>
        <div class="panel-body">
          <div id="top-calltypes-chart"></div>
        </div>
        <div class="note">
          This chart shows up to 20 ranked call types for the selected beat and year using the active metric. If the map is filtered to a specific call type, that category is highlighted when it appears in the list.
        </div>
      </div>
      <div class="panel mix-panel secondary-panel" data-secondary-panel="mix">
        <div class="panel-header">
          <h2 id="mix-title">Priority and Disposition Mix</h2>
        </div>
        <div class="summary" id="mix-summary"></div>
        <div class="mix-grid">
          <div id="priority-mix-chart" class="mix-chart"></div>
          <div id="disposition-mix-chart" class="mix-chart"></div>
        </div>
        <div class="note">
          These charts stay locked to the selected beat and year to show operational mix rather than map-wide totals.
        </div>
      </div>
      <div class="panel heatmap-panel secondary-panel" data-secondary-panel="heatmap">
        <div class="panel-header">
          <h2 id="beat-heatmap-title">Beat Timing Pattern</h2>
        </div>
        <div class="summary" id="beat-heatmap-summary"></div>
        <div class="panel-body">
          <div id="beat-heatmap-chart"></div>
        </div>
        <div class="note">
          This heatmap stays linked to the selected beat and year. It shows the full all-call timing pattern for that beat/year so the explorer remains fast across hundreds of call types.
        </div>
      </div>
    </div>
  </div>
  <script>
    const spec = {spec_json};
    const state = {{
      yearIndex: spec.defaultYearIndex,
      callType: spec.allCallsLabel,
      metricMode: "count",
      selectedBeatIndex: spec.defaultBeatIndex,
      hoverBeatIndex: null,
    }};

    const callTypeSearch = document.getElementById("call-type-search");
    const callTypeSelect = document.getElementById("call-type-select");
    const metricModeSelect = document.getElementById("metric-mode-select");
    const yearRange = document.getElementById("year-range");
    const secondaryPlotSelect = document.getElementById("secondary-plot-select");
    const currentYearLabel = document.getElementById("current-year-label");
    const topInsight = document.getElementById("top-insight");
    const selectedBeatLabel = document.getElementById("selected-beat-label");
    const beatSummary = document.getElementById("beat-summary");
    const beatDetailTitle = document.getElementById("beat-detail-title");
    const topCallTypesTitle = document.getElementById("top-calltypes-title");
    const topCallTypesSummary = document.getElementById("top-calltypes-summary");
    const mixTitle = document.getElementById("mix-title");
    const mixSummary = document.getElementById("mix-summary");
    const beatHeatmapTitle = document.getElementById("beat-heatmap-title");
    const beatHeatmapSummary = document.getElementById("beat-heatmap-summary");

    function formatNumber(value) {{
      return Number(value || 0).toLocaleString("en-US");
    }}

    function labelForCallType(callType) {{
      return spec.callTypeLabels[callType] || callType;
    }}

    function metricLabel(mode = state.metricMode) {{
      const labels = {{
        count: "Count",
        share: "Share of beat",
        yoy: "YoY change",
        index: "Index vs city",
      }};
      return labels[mode] || labels.count;
    }}

    function metricAxisTitle(mode = state.metricMode) {{
      const labels = {{
        count: "Calls",
        share: "Share of beat (%)",
        yoy: "YoY change (%)",
        index: "Index vs city (100 = city average)",
      }};
      return labels[mode] || labels.count;
    }}

    function formatMetricValue(value, mode = state.metricMode) {{
      if (value === null || value === undefined || !Number.isFinite(Number(value))) {{
        return "n/a";
      }}
      if (mode === "count") {{
        return formatNumber(Math.round(Number(value)));
      }}
      if (mode === "index") {{
        return `${{Number(value).toFixed(0)}}`;
      }}
      return `${{Number(value).toFixed(1)}}%`;
    }}

    function currentBeatIndex() {{
      return state.hoverBeatIndex ?? state.selectedBeatIndex;
    }}

    function rawValue(callType, yearIndex, beatIndex) {{
      return matrixValue(selectedMatrix(callType), yearIndex, beatIndex);
    }}

    function beatTotal(yearIndex, beatIndex) {{
      return matrixValue(spec.totalCounts, yearIndex, beatIndex);
    }}

    function cityTotalForMatrix(matrix, yearIndex) {{
      if (!matrix || !matrix[yearIndex]) {{
        return 0;
      }}
      return matrix[yearIndex].reduce((total, value) => total + (Number(value) || 0), 0);
    }}

    function cityTotalForCallType(callType, yearIndex) {{
      return cityTotalForMatrix(selectedMatrix(callType), yearIndex);
    }}

    function metricValue(callType, yearIndex, beatIndex) {{
      const raw = rawValue(callType, yearIndex, beatIndex);
      const total = beatTotal(yearIndex, beatIndex);

      if (state.metricMode === "count") {{
        return raw;
      }}

      if (state.metricMode === "share") {{
        return total ? (raw / total) * 100 : null;
      }}

      if (state.metricMode === "yoy") {{
        if (yearIndex === 0) {{
          return null;
        }}
        const previousRaw = rawValue(callType, yearIndex - 1, beatIndex);
        return previousRaw ? ((raw - previousRaw) / previousRaw) * 100 : null;
      }}

      if (state.metricMode === "index") {{
        const cityAllTotal = cityTotalForCallType(spec.allCallsLabel, yearIndex);
        if (!cityAllTotal) {{
          return null;
        }}
        if (callType === spec.allCallsLabel) {{
          const activeBeatCount = Math.max(1, spec.totalCounts[yearIndex].filter((value) => value > 0).length);
          return ((raw / cityAllTotal) / (1 / activeBeatCount)) * 100;
        }}
        const citySelectedTotal = cityTotalForCallType(callType, yearIndex);
        const beatShare = total ? raw / total : null;
        const cityShare = citySelectedTotal ? citySelectedTotal / cityAllTotal : null;
        return beatShare !== null && cityShare ? (beatShare / cityShare) * 100 : null;
      }}

      return raw;
    }}

    function metricValuesForYear(callType, yearIndex) {{
      return spec.beatKeys.map((_, beatIndex) => metricValue(callType, yearIndex, beatIndex));
    }}

    function metricSeriesForBeat(callType, beatIndex) {{
      return spec.years.map((_, yearIndex) => metricValue(callType, yearIndex, beatIndex));
    }}

    function mapColorSettings(zValues) {{
      const finiteValues = zValues.filter((value) => Number.isFinite(Number(value)));
      if (state.metricMode === "yoy") {{
        const maxAbs = Math.max(1, ...finiteValues.map((value) => Math.abs(Number(value))));
        return {{
          colorscale: "RdBu",
          reversescale: true,
          zmin: -maxAbs,
          zmax: maxAbs,
          zmid: 0,
          colorbar: {{ title: {{ text: metricAxisTitle() }} }},
        }};
      }}
      const maxValue = Math.max(1, ...finiteValues.map((value) => Number(value)));
      return {{
        colorscale: "Viridis",
        reversescale: false,
        zmin: 0,
        zmax: maxValue,
        zmid: null,
        colorbar: {{ title: {{ text: metricAxisTitle() }} }},
      }};
    }}

    function updateSelectedBeatState() {{
      const selectedBeat = spec.beatKeys[state.selectedBeatIndex];
      const activeBeat = spec.beatKeys[currentBeatIndex()];
      selectedBeatLabel.textContent =
        activeBeat !== selectedBeat
          ? `Selected Beat ${{selectedBeat}}; previewing Beat ${{activeBeat}}`
          : `Selected Beat ${{selectedBeat}}`;
    }}

    function updateTopInsight() {{
      const beatIndex = currentBeatIndex();
      const beatKey = spec.beatKeys[beatIndex];
      const year = spec.years[state.yearIndex];
      const raw = rawValue(state.callType, state.yearIndex, beatIndex);
      const total = beatTotal(state.yearIndex, beatIndex);
      const share = total ? (raw / total) * 100 : 0;
      const metric = metricValue(state.callType, state.yearIndex, beatIndex);
      topInsight.textContent =
        `In ${{year}}, Beat ${{beatKey}} had ${{formatNumber(raw)}} selected calls, representing ${{share.toFixed(1)}}% of all calls in that beat. ` +
        `${{metricLabel()}}: ${{formatMetricValue(metric)}}.`;
    }}

    function updateSharedUiState() {{
      currentYearLabel.textContent = spec.years[state.yearIndex];
      updateSelectedBeatState();
      updateTopInsight();
    }}

    function updateSecondaryPlots() {{
      updateBeatDetail();
      updateTopCallTypes();
      updateMixCharts();
      updateBeatHeatmap();
    }}

    function updateSecondaryPlotVisibility() {{
      document.querySelectorAll(".secondary-panel").forEach((panel) => {{
        panel.classList.toggle("is-active", panel.dataset.secondaryPanel === secondaryPlotSelect.value);
      }});
      window.requestAnimationFrame(() => {{
        const activePanel = document.querySelector(".secondary-panel.is-active");
        if (!activePanel) {{
          return;
        }}
        ["beat-detail-chart", "top-calltypes-chart", "priority-mix-chart", "disposition-mix-chart", "beat-heatmap-chart"].forEach((id) => {{
          const plot = document.getElementById(id);
          if (plot && activePanel.contains(plot)) {{
            Plotly.Plots.resize(plot);
          }}
        }});
      }});
    }}

    function beatMeta(beatIndex) {{
      const beatKey = spec.beatKeys[beatIndex];
      return spec.beatMeta[beatKey] || {{
        name: "Unavailable",
        division: "Unavailable",
        councilDistricts: "Unavailable",
      }};
    }}

    function matrixValue(matrix, yearIndex, beatIndex) {{
      if (!matrix || !matrix[yearIndex] || matrix[yearIndex][beatIndex] === undefined) {{
        return 0;
      }}
      return matrix[yearIndex][beatIndex];
    }}

    function selectedMatrix(callType) {{
      return callType === spec.allCallsLabel ? spec.totalCounts : spec.countsByType[callType];
    }}

    function seriesForBeat(callType, beatIndex) {{
      const matrix = selectedMatrix(callType);
      return spec.years.map((_, yearIndex) => matrixValue(matrix, yearIndex, beatIndex));
    }}

    function historyHtml(callType, beatIndex) {{
      const matrix = selectedMatrix(callType);
      return spec.years
        .map((year, yearIndex) => `${{year}}: ${{formatNumber(matrixValue(matrix, yearIndex, beatIndex))}}`)
        .join("<br>");
    }}

    function hoverText(callType, yearIndex) {{
      const year = spec.years[yearIndex];
      return spec.beatKeys.map((beatKey, beatIndex) => {{
        const meta = beatMeta(beatIndex);
        const value = rawValue(callType, yearIndex, beatIndex);
        const metric = metricValue(callType, yearIndex, beatIndex);
        const total = beatTotal(yearIndex, beatIndex);
        const share = total ? ((value / total) * 100).toFixed(1) : "0.0";
        return [
          `<b>Beat ${{beatKey}}</b>`,
          `Name: ${{meta.name}}`,
          `Division: ${{meta.division}}`,
          `Council district(s): ${{meta.councilDistricts}}`,
          `Year: ${{year}}`,
          `Call Type: ${{labelForCallType(callType)}}`,
          `Metric: ${{metricLabel()}} = ${{formatMetricValue(metric)}}`,
          `Calls: ${{formatNumber(value)}}`,
          `Share of beat: ${{share}}%`,
          `Beat total: ${{formatNumber(total)}}`,
          "",
          "<b>Yearly history</b>",
          historyHtml(callType, beatIndex),
        ].join("<br>");
      }});
    }}

    function updateBeatDetail() {{
      const beatIndex = currentBeatIndex();
      const beatKey = spec.beatKeys[beatIndex];
      const meta = beatMeta(beatIndex);
      const rawSelectedSeries = seriesForBeat(state.callType, beatIndex);
      const selectedSeries = metricSeriesForBeat(state.callType, beatIndex);
      const totalSeries = seriesForBeat(spec.allCallsLabel, beatIndex);
      const currentValue = rawSelectedSeries[state.yearIndex] || 0;
      const currentMetric = selectedSeries[state.yearIndex];
      const currentTotal = totalSeries[state.yearIndex] || 0;
      const currentShare = currentTotal ? ((currentValue / currentTotal) * 100).toFixed(1) : "0.0";
      const selectedLabel = labelForCallType(state.callType);

      beatDetailTitle.textContent = `Beat ${{beatKey}} Trend`;
      beatSummary.innerHTML = [
        `<strong>${{meta.name}}</strong>`,
        `Division: ${{meta.division}}`,
        `Council district(s): ${{meta.councilDistricts}}`,
        `<strong>${{selectedLabel}}</strong> in <strong>${{spec.years[state.yearIndex]}}</strong>: ${{formatNumber(currentValue)}} calls`,
        `Share of beat calls that year: ${{currentShare}}%`,
        `${{metricLabel()}} shown on chart: ${{formatMetricValue(currentMetric)}}`,
        `All calls in beat ${{beatKey}} that year: ${{formatNumber(currentTotal)}}`,
      ].join("<br>");

      const traces = [
        {{
          type: "scatter",
          mode: "lines+markers",
          x: spec.years,
          y: selectedSeries,
          name: selectedLabel,
          line: {{ color: "#0b5cab", width: 3 }},
          marker: {{ size: 8 }},
          customdata: rawSelectedSeries,
          hovertemplate: "%{{x}}<br>" + metricAxisTitle() + ": %{{y}}<br>Raw calls: %{{customdata:,}}<extra></extra>",
        }},
      ];

      if (state.callType !== spec.allCallsLabel && state.metricMode === "count") {{
        traces.push({{
          type: "scatter",
          mode: "lines+markers",
          x: spec.years,
          y: totalSeries,
          name: spec.allCallsLabel,
          line: {{ color: "#7a8794", width: 2, dash: "dash" }},
          marker: {{ size: 6 }},
          hovertemplate: "%{{x}}<br>All calls: %{{y:,}}<extra></extra>",
        }});
      }}

      Plotly.react(
        "beat-detail-chart",
        traces,
        {{
          template: "plotly_white",
          margin: {{ l: 50, r: 24, t: 40, b: 50 }},
          legend: {{ orientation: "h", y: 1.12 }},
          xaxis: {{ title: "Year", tickmode: "array", tickvals: spec.years }},
          yaxis: {{ title: metricAxisTitle() }},
          hovermode: "x unified",
        }},
        {{ responsive: true }}
      );
    }}

    function topCallTypesForBeatYear(beatIndex, yearIndex, limit) {{
      return spec.callTypes
        .filter((callType) => callType !== spec.allCallsLabel)
        .map((callType) => ({{
          callType: callType,
          label: labelForCallType(callType),
          raw: matrixValue(spec.countsByType[callType], yearIndex, beatIndex),
          value: metricValue(callType, yearIndex, beatIndex),
        }}))
        .filter((item) => item.raw > 0)
        .sort((left, right) => {{
          const leftValue = Number.isFinite(Number(left.value)) ? Number(left.value) : -Infinity;
          const rightValue = Number.isFinite(Number(right.value)) ? Number(right.value) : -Infinity;
          return rightValue - leftValue || right.raw - left.raw || left.label.localeCompare(right.label);
        }})
        .slice(0, limit);
    }}

    function updateTopCallTypes() {{
      const beatIndex = currentBeatIndex();
      const beatKey = spec.beatKeys[beatIndex];
      const meta = beatMeta(beatIndex);
      const year = spec.years[state.yearIndex];
      const currentTotal = matrixValue(spec.totalCounts, state.yearIndex, beatIndex);
      const items = topCallTypesForBeatYear(beatIndex, state.yearIndex, 20);

      topCallTypesTitle.textContent = `Beat ${{beatKey}} Top Call Types`;

      if (!items.length) {{
        topCallTypesSummary.innerHTML = [
          `<strong>${{meta.name}}</strong> in <strong>${{year}}</strong> has no ranked call-type breakdown.`,
        ].join("<br>");
        Plotly.react(
          "top-calltypes-chart",
          [],
          {{
            template: "plotly_white",
            margin: {{ l: 40, r: 24, t: 24, b: 40 }},
            xaxis: {{ visible: false }},
            yaxis: {{ visible: false }},
            annotations: [{{
              text: "No call-type data available",
              x: 0.5,
              y: 0.5,
              xref: "paper",
              yref: "paper",
              showarrow: false,
            }}],
          }},
          {{ responsive: true }}
        );
        return;
      }}

      const topItem = items[0];
      const topShare = currentTotal ? ((topItem.raw / currentTotal) * 100).toFixed(1) : "0.0";
      topCallTypesSummary.innerHTML = [
        `<strong>${{meta.name}}</strong> in <strong>${{year}}</strong>`,
        `Current map filter: <strong>${{labelForCallType(state.callType)}}</strong>`,
        `Top call type by ${{metricLabel().toLowerCase()}}: <strong>${{topItem.label}}</strong> with ${{formatNumber(topItem.raw)}} calls (${{topShare}}% of beat total)`,
      ].join("<br>");

      Plotly.react(
        "top-calltypes-chart",
        [{{
          type: "bar",
          x: items.map((item) => Number.isFinite(Number(item.value)) ? item.value : 0).reverse(),
          y: items.map((item) => item.label).reverse(),
          orientation: "h",
          customdata: items.map((item) => item.raw).reverse(),
          marker: {{
            color: items
              .map((item) => item.callType === state.callType && state.callType !== spec.allCallsLabel ? "#0b5cab" : "#7f8ea3")
              .reverse(),
          }},
          hovertemplate: "%{{y}}<br>" + metricAxisTitle() + ": %{{x}}<br>Raw calls: %{{customdata:,}}<extra></extra>",
        }}],
        {{
          template: "plotly_white",
          margin: {{ l: 210, r: 24, t: 24, b: 50 }},
          xaxis: {{ title: metricAxisTitle() }},
          yaxis: {{ title: "" }},
        }},
        {{ responsive: true }}
      );
    }}

    function categoryRawValue(countsLookup, category, yearIndex, beatIndex) {{
      return matrixValue(countsLookup[category], yearIndex, beatIndex);
    }}

    function categoryBeatTotal(categories, countsLookup, yearIndex, beatIndex) {{
      return categories.reduce((total, category) => total + categoryRawValue(countsLookup, category, yearIndex, beatIndex), 0);
    }}

    function categoryCityTotal(categories, countsLookup, yearIndex) {{
      return categories.reduce((total, category) => total + cityTotalForMatrix(countsLookup[category], yearIndex), 0);
    }}

    function categoryMetricValue(categories, countsLookup, category, yearIndex, beatIndex) {{
      const raw = categoryRawValue(countsLookup, category, yearIndex, beatIndex);
      if (state.metricMode === "count") {{
        return raw;
      }}
      const beatCategoryTotal = categoryBeatTotal(categories, countsLookup, yearIndex, beatIndex);
      if (state.metricMode === "share") {{
        return beatCategoryTotal ? (raw / beatCategoryTotal) * 100 : null;
      }}
      if (state.metricMode === "yoy") {{
        if (yearIndex === 0) {{
          return null;
        }}
        const previousRaw = categoryRawValue(countsLookup, category, yearIndex - 1, beatIndex);
        return previousRaw ? ((raw - previousRaw) / previousRaw) * 100 : null;
      }}
      if (state.metricMode === "index") {{
        const cityCategoryTotal = cityTotalForMatrix(countsLookup[category], yearIndex);
        const cityAllCategories = categoryCityTotal(categories, countsLookup, yearIndex);
        const beatShare = beatCategoryTotal ? raw / beatCategoryTotal : null;
        const cityShare = cityAllCategories ? cityCategoryTotal / cityAllCategories : null;
        return beatShare !== null && cityShare ? (beatShare / cityShare) * 100 : null;
      }}
      return raw;
    }}

    function mixItems(categories, countsLookup, beatIndex, yearIndex, labelLookup) {{
      return categories
        .map((category) => ({{
          category: category,
          label: labelLookup && labelLookup[category] ? labelLookup[category] : category,
          raw: categoryRawValue(countsLookup, category, yearIndex, beatIndex),
          value: categoryMetricValue(categories, countsLookup, category, yearIndex, beatIndex),
        }}))
        .filter((item) => item.raw > 0);
    }}

    function renderMixChart(targetId, items, title, color) {{
      if (!items.length) {{
        Plotly.react(
          targetId,
          [],
          {{
            template: "plotly_white",
            title: {{ text: title, font: {{ size: 16 }} }},
            margin: {{ l: 40, r: 24, t: 40, b: 40 }},
            xaxis: {{ visible: false }},
            yaxis: {{ visible: false }},
            annotations: [{{
              text: "No data available",
              x: 0.5,
              y: 0.5,
              xref: "paper",
              yref: "paper",
              showarrow: false,
            }}],
          }},
          {{ responsive: true }}
        );
        return;
      }}

      const sortedItems = [...items].sort((left, right) => {{
        const leftValue = Number.isFinite(Number(left.value)) ? Number(left.value) : -Infinity;
        const rightValue = Number.isFinite(Number(right.value)) ? Number(right.value) : -Infinity;
        return rightValue - leftValue || right.raw - left.raw || left.label.localeCompare(right.label);
      }});
      Plotly.react(
        targetId,
        [{{
          type: "bar",
          x: sortedItems.map((item) => Number.isFinite(Number(item.value)) ? item.value : 0).reverse(),
          y: sortedItems.map((item) => item.label).reverse(),
          orientation: "h",
          customdata: sortedItems.map((item) => item.raw).reverse(),
          marker: {{ color: color }},
          hovertemplate: "%{{y}}<br>" + metricAxisTitle() + ": %{{x}}<br>Raw calls: %{{customdata:,}}<extra></extra>",
        }}],
        {{
          template: "plotly_white",
          title: {{ text: title, font: {{ size: 16 }} }},
          margin: {{ l: 110, r: 24, t: 40, b: 40 }},
          xaxis: {{ title: metricAxisTitle() }},
          yaxis: {{ title: "" }},
        }},
        {{ responsive: true }}
      );
    }}

    function topMixItemLabel(items) {{
      if (!items.length) {{
        return "unavailable";
      }}
      return items.reduce((best, item) => {{
        const itemValue = Number.isFinite(Number(item.value)) ? Number(item.value) : item.raw;
        const bestValue = Number.isFinite(Number(best.value)) ? Number(best.value) : best.raw;
        return itemValue > bestValue ? item : best;
      }}).label;
    }}

    function updateMixCharts() {{
      const beatIndex = currentBeatIndex();
      const beatKey = spec.beatKeys[beatIndex];
      const meta = beatMeta(beatIndex);
      const year = spec.years[state.yearIndex];
      const priorityItems = mixItems(
        spec.priorityCategories,
        spec.priorityCounts,
        beatIndex,
        state.yearIndex,
        spec.priorityLabels,
      );
      const dispositionItems = mixItems(
        spec.dispositionCategories,
        spec.dispositionCounts,
        beatIndex,
        state.yearIndex,
        null,
      );

      mixTitle.textContent = `Beat ${{beatKey}} Priority and Disposition`;
      mixSummary.innerHTML = [
        `<strong>${{meta.name}}</strong> in <strong>${{year}}</strong>`,
        `Metric shown: <strong>${{metricLabel()}}</strong>`,
        `Top priority: <strong>${{topMixItemLabel(priorityItems)}}</strong>`,
        `Top disposition: <strong>${{topMixItemLabel(dispositionItems)}}</strong>`,
      ].join("<br>");

      renderMixChart("priority-mix-chart", priorityItems, "Priority Mix", "#1a5a91");
      renderMixChart("disposition-mix-chart", dispositionItems, "Disposition Mix", "#b35c1e");
    }}

    function updateBeatHeatmap() {{
      const beatIndex = currentBeatIndex();
      const beatKey = spec.beatKeys[beatIndex];
      const meta = beatMeta(beatIndex);
      const year = spec.years[state.yearIndex];
      const selectedLabel = labelForCallType(state.callType);
      const zValues = spec.beatHeatmaps[beatIndex][state.yearIndex];
      const currentValue = rawValue(state.callType, state.yearIndex, beatIndex);
      const currentMetric = metricValue(state.callType, state.yearIndex, beatIndex);

      beatHeatmapTitle.textContent = `Beat ${{beatKey}} Timing Pattern`;
      beatHeatmapSummary.innerHTML = [
        `<strong>${{meta.name}}</strong>`,
        `Heatmap year: <strong>${{year}}</strong>`,
        `Highlighted map selection: <strong>${{selectedLabel}}</strong> with ${{formatNumber(currentValue)}} calls in beat ${{beatKey}}`,
        `Current map metric: <strong>${{metricLabel()}}</strong> = ${{formatMetricValue(currentMetric)}}`,
        `Heatmap values show <strong>all calls</strong> by weekday and hour for beat ${{beatKey}} in ${{year}}.`,
      ].join("<br>");

      Plotly.react(
        "beat-heatmap-chart",
        [{{
          type: "heatmap",
          z: zValues,
          x: [...Array(24).keys()],
          y: spec.weekdayOrder,
          colorscale: "Viridis",
          hovertemplate: "Day: %{{y}}<br>Hour: %{{x}}:00<br>Calls: %{{z:,}}<extra></extra>",
          colorbar: {{ title: {{ text: "Calls" }} }},
        }}],
        {{
          template: "plotly_white",
          margin: {{ l: 70, r: 24, t: 20, b: 50 }},
          xaxis: {{ title: "Hour of Day", tickmode: "array", tickvals: [...Array(24).keys()] }},
          yaxis: {{ title: "Day of Week", autorange: "reversed" }},
        }},
        {{ responsive: true }}
      );
    }}

    function updateMap() {{
      const year = spec.years[state.yearIndex];
      const zValues = metricValuesForYear(state.callType, state.yearIndex);
      const colorSettings = mapColorSettings(zValues);
      const selectedLabel = labelForCallType(state.callType);

      Plotly.restyle(
        "beat-map",
        {{
          z: [zValues],
          text: [hoverText(state.callType, state.yearIndex)],
          zmin: [colorSettings.zmin],
          zmax: [colorSettings.zmax],
          zmid: [colorSettings.zmid],
          colorscale: [colorSettings.colorscale],
          reversescale: [colorSettings.reversescale],
          colorbar: [colorSettings.colorbar],
        }},
        [0]
      );

      Plotly.relayout("beat-map", {{
        "title.text": `Calls for Service by Beat: ${{selectedLabel}} (${{year}}, ${{metricLabel()}})`,
      }});

      updateSharedUiState();
      updateSecondaryPlots();
    }}

    function attachMapHandlers() {{
      const mapDiv = document.getElementById("beat-map");
      mapDiv.on("plotly_hover", (eventData) => {{
        if (!eventData.points.length || eventData.points[0].curveNumber !== 0) {{
          return;
        }}
        state.hoverBeatIndex = eventData.points[0].pointIndex;
        updateSharedUiState();
        updateSecondaryPlots();
      }});
      mapDiv.on("plotly_click", (eventData) => {{
        if (!eventData.points.length || eventData.points[0].curveNumber !== 0) {{
          return;
        }}
        state.selectedBeatIndex = eventData.points[0].pointIndex;
        state.hoverBeatIndex = eventData.points[0].pointIndex;
        updateSharedUiState();
        updateSecondaryPlots();
      }});
      mapDiv.on("plotly_unhover", () => {{
        state.hoverBeatIndex = null;
        updateSharedUiState();
        updateSecondaryPlots();
      }});
    }}

    function populateCallTypeSelect(searchTerm = "") {{
      const query = searchTerm.trim().toLowerCase();
      callTypeSelect.innerHTML = "";

      spec.callTypeGroups.forEach((group) => {{
        const matches = group.callTypes.filter((callType) => {{
          const haystack = `${{group.name}} ${{callType}} ${{labelForCallType(callType)}}`.toLowerCase();
          return !query || haystack.includes(query);
        }});
        if (!matches.length) {{
          return;
        }}

        const optgroup = document.createElement("optgroup");
        optgroup.label = group.name;
        matches.forEach((callType) => {{
          const option = document.createElement("option");
          option.value = callType;
          option.textContent = labelForCallType(callType);
          optgroup.appendChild(option);
        }});
        callTypeSelect.appendChild(optgroup);
      }});

      const options = Array.from(callTypeSelect.options);
      if (!options.length) {{
        const option = document.createElement("option");
        option.value = state.callType;
        option.textContent = "No matching call types";
        option.disabled = true;
        callTypeSelect.appendChild(option);
        return false;
      }}

      const hasCurrent = options.some((option) => option.value === state.callType);
      callTypeSelect.value = hasCurrent ? state.callType : options[0].value;
      return hasCurrent;
    }}

    function initControls() {{
      populateCallTypeSelect();
      callTypeSelect.value = state.callType;
      metricModeSelect.value = state.metricMode;
      yearRange.value = state.yearIndex;

      callTypeSearch.addEventListener("input", (event) => {{
        const hasCurrent = populateCallTypeSelect(event.target.value);
        if (!hasCurrent && callTypeSelect.options.length && !callTypeSelect.options[0].disabled) {{
          state.callType = callTypeSelect.value;
          updateMap();
        }}
      }});

      callTypeSelect.addEventListener("change", (event) => {{
        state.callType = event.target.value;
        updateMap();
      }});

      metricModeSelect.addEventListener("change", (event) => {{
        state.metricMode = event.target.value;
        updateMap();
      }});

      yearRange.addEventListener("input", (event) => {{
        state.yearIndex = Number(event.target.value);
        updateMap();
      }});

      secondaryPlotSelect.addEventListener("change", () => {{
        updateSecondaryPlotVisibility();
        updateSecondaryPlots();
      }});
    }}

    function initMap() {{
      const zValues = metricValuesForYear(state.callType, state.yearIndex);
      const colorSettings = mapColorSettings(zValues);
      const traces = [{{
        type: "choroplethmapbox",
        geojson: spec.geojson,
        featureidkey: "properties.beat_key",
        locations: spec.beatKeys,
        z: zValues,
        text: hoverText(state.callType, state.yearIndex),
        hovertemplate: "%{{text}}<extra></extra>",
        colorscale: colorSettings.colorscale,
        reversescale: colorSettings.reversescale,
        zmin: colorSettings.zmin,
        zmax: colorSettings.zmax,
        zmid: colorSettings.zmid,
        marker: {{ opacity: 0.68 }},
        colorbar: colorSettings.colorbar,
      }}];

      if (spec.districtBoundaryCoords.lat.length) {{
        traces.push({{
          type: "scattermapbox",
          lat: spec.districtBoundaryCoords.lat,
          lon: spec.districtBoundaryCoords.lon,
          mode: "lines",
          line: {{ color: "#1b2633", width: 2 }},
          hoverinfo: "skip",
          showlegend: false,
        }});
      }}

      Plotly.newPlot(
        "beat-map",
        traces,
        {{
          template: "plotly_white",
          title: {{ text: `Calls for Service by Beat: ${{labelForCallType(state.callType)}} (${{spec.years[state.yearIndex]}}, ${{metricLabel()}})` }},
          margin: {{ l: 0, r: 0, t: 50, b: 0 }},
          mapbox: {{
            style: "carto-positron",
            center: {{ lat: 32.7157, lon: -117.1611 }},
            zoom: 10,
          }},
          uirevision: "beat-calltype-map",
        }},
        {{ responsive: true }}
      ).then(attachMapHandlers);
    }}

    initControls();
    initMap();
    updateSecondaryPlotVisibility();
    updateSharedUiState();
    updateSecondaryPlots();
  </script>
</body>
</html>
"""


def beat_choropleth(
    beat_geo: gpd.GeoDataFrame,
    year: int,
    color_scale: str = "Viridis",
    council_districts: gpd.GeoDataFrame | None = None,
) -> go.Figure:
    """Interactive choropleth for the latest year with beat-level history in hover."""
    if beat_geo.empty:
        return _message_figure("Calls for Service per Beat", "Beat boundary data is unavailable.")

    data = beat_geo.copy()
    history_lookup = _build_beat_history(data)
    data_year = data[data["year"] == year] if "year" in data.columns else data
    if data_year.empty:
        return _message_figure(
            "Calls for Service per Beat",
            f"No beat-level records are available for {year}.",
        )

    data_year = data_year.copy()
    data_year["beat_key"] = data_year["beat"].astype(str)
    data_year["beat_history"] = data_year["beat"].map(history_lookup).fillna("No history available")
    data_year["year_label"] = year
    if "total_calls" not in data_year.columns:
        data_year["total_calls"] = data_year["call_count"]

    beat_shapes = data.drop_duplicates(subset=["beat"]).copy()
    beat_shapes["beat_key"] = beat_shapes["beat"].astype(str)

    fig = px.choropleth_mapbox(
        data_year,
        geojson=_web_geojson_from_gdf(beat_shapes, ["beat_key"]),
        locations="beat_key",
        featureidkey="properties.beat_key",
        color="call_count",
        mapbox_style="carto-positron",
        color_continuous_scale=color_scale,
        center={"lat": 32.7157, "lon": -117.1611},
        zoom=10,
        opacity=0.6,
        custom_data=["beat", "year_label", "call_count", "total_calls", "beat_history"],
        range_color=(0, max(1.0, float(data_year["call_count"].max()))),
    )
    fig.update_traces(
        hovertemplate=(
            "<b>Beat %{customdata[0]}</b><br>"
            "Year: %{customdata[1]}<br>"
            "Calls: %{customdata[2]:,}<br>"
            "Study-period total: %{customdata[3]:,}<br>"
            "<br><b>Yearly history</b><br>%{customdata[4]}"
            "<extra></extra>"
        )
    )
    district_overlay = _district_outline_trace(council_districts)
    if district_overlay is not None:
        fig.add_trace(district_overlay)
    fig.update_layout(title=f"Calls for Service per Beat ({year})", margin={"l": 0, "r": 0, "t": 50, "b": 0})
    return fig


def hex_choropleth(hex_stats: gpd.GeoDataFrame, year: int, color_scale: str = "Viridis") -> go.Figure:
    if hex_stats.empty:
        return _message_figure(
            "Calls for Service Hex Density",
            "Hex map unavailable because the source CSVs do not include x/y coordinates.",
        )
    subset = hex_stats[hex_stats["year"] == year] if "year" in hex_stats.columns else hex_stats
    if subset.empty:
        return _message_figure(
            "Calls for Service Hex Density",
            f"No hex-level records are available for {year}.",
        )
    fig = px.choropleth_mapbox(
        subset,
        geojson=_geojson_from_gdf(subset),
        locations=subset.index,
        color="call_count",
        mapbox_style="carto-positron",
        hover_data={"hex_id": True, "call_count": True},
        color_continuous_scale=color_scale,
        center={"lat": 32.7157, "lon": -117.1611},
        zoom=10,
        opacity=0.6,
    )
    fig.update_layout(title=f"Calls for Service Hex Density ({year})")
    return fig


def citywide_line(citywide_counts: pd.DataFrame) -> go.Figure:
    fig = px.line(citywide_counts, x="year", y="call_count", markers=True, title="Citywide Calls for Service")
    fig.update_layout(yaxis_title="Calls", xaxis_title="Year")
    return fig


def hourly_heatmap(pivot_df: pd.DataFrame) -> go.Figure:
    if pivot_df.empty:
        return go.Figure()
    fig = px.imshow(
        pivot_df,
        aspect="auto",
        labels=dict(x="Hour", y="Day of Week", color="Calls"),
        title="Calls by Hour and Day of Week",
        color_continuous_scale="Viridis",
    )
    fig.update_yaxes(autorange="reversed")
    return fig


def beat_time_series(beat_counts: pd.DataFrame, beat_id: int) -> go.Figure:
    subset = beat_counts[beat_counts[config.BEAT_COLUMN] == beat_id]
    fig = px.line(subset, x="year", y="call_count", markers=True, title=f"Beat {beat_id} Calls by Year")
    return fig
