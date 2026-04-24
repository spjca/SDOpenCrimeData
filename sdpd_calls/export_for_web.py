"""Export pipeline outputs for web embedding and social sharing."""
from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Iterable

import plotly.io as pio
from PIL import Image, ImageDraw, ImageFont

from . import aggregations, config, data_download, preprocess, spatial, visualization

logger = logging.getLogger(__name__)


OUTPUT_README = """How to embed
=================

* Open the generated HTML file in a text editor.
* Copy the `<div>...</div>` and `<script>...</script>` snippets into a Squarespace Code block.
* For PNGs, upload them to your media library or include them as image blocks.
"""


def save_plotly_html(fig, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pio.write_html(fig, file=path, include_plotlyjs="cdn", full_html=True)
    logger.info("Saved HTML to %s", path)


def save_html_document(html: str, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(html, encoding="utf-8")
    logger.info("Saved HTML to %s", path)


def _placeholder_png(path: Path, message: str) -> None:
    width, height = 1200, 675
    image = Image.new("RGB", (width, height), color=(240, 240, 240))
    draw = ImageDraw.Draw(image)
    text = "PNG export not available\n" + message
    draw.multiline_text((50, 50), text, fill=(0, 0, 0))
    image.save(path)
    logger.info("Wrote placeholder PNG to %s", path)


def save_plotly_png(fig, path: Path, width: int = 1200, height: int = 675) -> None:
    try:
        fig.write_image(path, width=width, height=height)
        logger.info("Saved PNG to %s", path)
    except Exception as exc:  # pragma: no cover - optional dependency
        logger.warning(
            "PNG export failed (%s). Install kaleido/Chrome for true rendering. Writing placeholder instead.",
            exc,
        )
        _placeholder_png(path, str(exc))


def export_summaries(df, output_dir: Path) -> None:
    summary_path = output_dir / "summary_stats.json"
    summary = aggregations.summarize(df)
    summary_path.write_text(json.dumps(summary, indent=2))
    (output_dir / "HOW_TO_EMBED.md").write_text(OUTPUT_README)


def generate_outputs(years: Iterable[int] | None = None) -> None:
    config.ensure_directories()
    years_to_use = list(years) if years is not None else config.years_to_process()

    # Download and preprocess
    data_download.download_all(years_to_use)
    clean_df = preprocess.preprocess(years_to_use)

    # Aggregations
    citywide = aggregations.citywide_by_year(clean_df)
    beat_counts = aggregations.beat_by_year(clean_df)
    beat_calltypes = aggregations.beat_calltype_by_year(clean_df)
    beat_priorities = aggregations.beat_priority_by_year(clean_df)
    beat_dispositions = aggregations.beat_disposition_by_year(clean_df)
    beat_hourly = aggregations.beat_hourly_by_weekday(clean_df)
    hourly = aggregations.hourly_by_weekday(clean_df)
    call_type_reference = data_download.load_call_type_reference()

    # Spatial products
    spatial_products = spatial.build_spatial_products(clean_df)
    beat_geo = spatial_products.get("beats")
    council_districts = spatial_products.get("council_districts")
    hex_stats = spatial_products.get("hex_stats")

    # Visuals
    latest_year = max(clean_df["year"].dropna().astype(int)) if not clean_df.empty else years_to_use[-1]
    beat_map = visualization.beat_choropleth(beat_geo, latest_year, council_districts=council_districts)
    beat_map_html = visualization.beat_choropleth_html(
        beat_geo,
        beat_calltypes,
        beat_priorities,
        beat_dispositions,
        beat_hourly,
        call_type_reference,
        latest_year,
        council_districts=council_districts,
    )
    hex_map = visualization.hex_choropleth(hex_stats, latest_year)
    citywide_fig = visualization.citywide_line(citywide)
    heatmap_fig = visualization.hourly_heatmap(hourly)

    # Exports
    save_html_document(beat_map_html, config.OUTPUT_DIR / "choropleth_beats.html")
    save_plotly_html(hex_map, config.OUTPUT_DIR / "hex_map.html")
    save_plotly_html(citywide_fig, config.OUTPUT_DIR / "citywide_trend.html")
    save_plotly_html(heatmap_fig, config.OUTPUT_DIR / "hourly_heatmap.html")

    save_plotly_png(beat_map, config.OUTPUT_DIR / "choropleth_beats.png")
    save_plotly_png(hex_map, config.OUTPUT_DIR / "hex_map.png")
    save_plotly_png(citywide_fig, config.OUTPUT_DIR / "citywide_trend.png")

    export_summaries(clean_df, config.OUTPUT_DIR)

    # Save GeoJSONs if present
    if beat_geo is not None and not beat_geo.empty:
        spatial.export_geojson(beat_geo, config.OUTPUT_DIR / "beat_stats.geojson")
    if hex_stats is not None and not hex_stats.empty:
        spatial.export_geojson(hex_stats, config.OUTPUT_DIR / "hex_stats.geojson")

    logger.info("All outputs generated in %s", config.OUTPUT_DIR)
