"""Command-line entrypoint to run the full pipeline."""
from __future__ import annotations

import logging

from sdpd_calls import config, export_for_web


logging.basicConfig(level=logging.INFO, format="%(levelname)s:%(name)s:%(message)s")


def main() -> None:
    years = config.years_to_process()
    logging.info("Running pipeline for years: %s", years)
    export_for_web.generate_outputs(years)


if __name__ == "__main__":
    main()
