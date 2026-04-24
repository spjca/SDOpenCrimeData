# Notes from 2024SDPDAnalysis.RMD

## Data sources
- Years covered: 2016 through 2024.
- CSV URL pattern: `https://seshat.datasd.org/police_calls_for_service/pd_calls_for_service_<YEAR>_datasd.csv`.

## Cleaning and augmentation
- Loads each year via `read_csv` with explicit column types (incident number, `date_time`, day of week, address pieces, `call_type`, `disposition`, `beat`, `priority`).
- Adds a `year` column for aggregation.

## Aggregations
- Binds all years together into `pd_combined`.
- Groups by `beat` and `year`, counts records, and pivots wide so each year becomes `ct_<year>`.

## Spatial join
- Loads beat boundaries from `https://seshat.datasd.org/gis_police_beats/pd_beats_datasd.geojson` using `geojson_sf`.
- Joins beat polygons to the aggregated call counts on the `beat` field.

## Visualization logic
- Leaflet choropleth of beats showing call counts for 2024 with a viridis palette.
- Hover tooltip shows the beat name plus a table of calls and year-over-year percent change across the full year list.
- Shiny UI with a dropdown to pick a beat (or ALL) and a table of call counts by `call_type` and year for the selection.

## Python parity goals
- Mirror the year range and URL pattern.
- Recreate beat-level aggregations and choropleth.
- Extend with additional exports (HTML + PNG) and a hexbin analysis not present in the RMD.
