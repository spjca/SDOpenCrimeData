# SDPD Calls for Service (Python)

This project rebuilds the SDPD calls-for-service analysis originally authored in R into a reusable Python toolkit. It downloads annual CSVs from the San Diego open data portal, cleans and aggregates the records, produces beat-level and hexbin spatial summaries, and exports interactive HTML plus PNG snapshots for easy embedding on Squarespace or social media.

## Quick start

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python run_all.py
```

By default the pipeline targets years 2016–2025. Override with an environment variable:

```bash
SDPD_YEARS=2024,2025 python run_all.py
```

Outputs are written to `outputs/`:

- `choropleth_beats.html` / `.png`: beat-level map for the most recent year
- `hex_map.html` / `.png`: hexbin density map for the most recent year
- `citywide_trend.html` / `.png`: line chart of calls per year
- `hourly_heatmap.html`: hourly × weekday pattern
- `beat_stats.geojson`, `hex_stats.geojson`: spatial data for web maps
- `summary_stats.json`: quick totals and percent change over the study period

## Embedding on Squarespace

1. Open an exported HTML file in a text editor.
2. Copy the `<div>…</div>` and `<script>…</script>` blocks into a Squarespace **Code** block, or host the HTML and embed it in an iframe.
3. Upload the PNGs to Squarespace or other channels for social sharing.

See `outputs/HOW_TO_EMBED.md` for a brief reminder that is written during export.

## Module layout

- `sdpd_calls/config.py` – constants for URLs, paths, coordinate systems, and year selection
- `sdpd_calls/data_download.py` – download and cache CSVs
- `sdpd_calls/preprocess.py` – clean, standardize columns, derive time parts, and persist Parquet
- `sdpd_calls/aggregations.py` – citywide and beat-level rollups, call-type splits, hourly heatmap prep
- `sdpd_calls/spatial.py` – beat boundary loading, hex grid generation, spatial joins, GeoJSON export
- `sdpd_calls/visualization.py` – Plotly-based interactive figures (maps, line chart, heatmap)
- `sdpd_calls/export_for_web.py` – orchestrates the pipeline and writes HTML/PNG/JSON outputs
- `run_all.py` – simple CLI entrypoint to run everything

## Testing

A minimal smoke test lives in `tests/test_basic_counts.py`. Run with:

```bash
python -m pytest
```

The test confirms the processed dataset exists and carries expected columns after running the pipeline. If the processed file is missing, it will remind you to run `python run_all.py` first.
