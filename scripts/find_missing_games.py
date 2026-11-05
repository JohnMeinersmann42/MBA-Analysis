"""
find_missing_games.py

Compares umpires_2025_2026.csv against the MLB schedule API and prints
any game PKs that are missing from the CSV.

Usage:
  python find_missing_games.py
  python find_missing_games.py --csv my_file.csv --season 2025 2026
  python find_missing_games.py --output missing.txt   # save PKs to file
"""

import argparse
import csv
import sys
from pathlib import Path

import requests

SCHEDULE_URL = (
    "https://statsapi.mlb.com/api/v1/schedule"
    "?sportId=1&gameType=R&season={season}&fields=dates,games,gamePk,status,detailedState"
)

INCOMPLETE_STATES = {"Scheduled", "Pre-Game", "Warmup", "Postponed", "Cancelled"}


def fetch_completed_pks(season: int) -> set[int]:
    url = SCHEDULE_URL.format(season=season)
    print(f"  Fetching {season} schedule...", end=" ", flush=True)
    try:
        resp = requests.get(url, timeout=30)
        resp.raise_for_status()
        data = resp.json()
    except requests.RequestException as e:
        print(f"FAILED: {e}", file=sys.stderr)
        return set()

    pks = set()
    for date_entry in data.get("dates", []):
        for game in date_entry.get("games", []):
            pk = game.get("gamePk")
            state = game.get("status", {}).get("detailedState", "")
            if pk and state not in INCOMPLETE_STATES:
                pks.add(pk)

    print(f"{len(pks)} completed games found")
    return pks


def load_csv_pks(csv_path: Path) -> set[int]:
    if not csv_path.exists():
        print(f"CSV not found: {csv_path}", file=sys.stderr)
        sys.exit(1)
    pks = set()
    with open(csv_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            try:
                pks.add(int(row["game_pk"]))
            except (KeyError, ValueError):
                pass
    return pks


def main():
    parser = argparse.ArgumentParser(description="Find game PKs missing from the umpire CSV.")
    parser.add_argument("--csv", type=Path, default=Path("umpires_2025_2026.csv"),
                        help="Umpire CSV to check (default: umpires_2025_2026.csv)")
    parser.add_argument("--season", nargs="+", type=int, default=[2025, 2026],
                        metavar="YEAR", help="Seasons to check (default: 2025 2026)")
    parser.add_argument("--output", type=Path,
                        help="Write missing PKs to this file (one per line)")
    args = parser.parse_args()

    print(f"Loading {args.csv}...")
    have = load_csv_pks(args.csv)
    print(f"  {len(have)} game PKs in CSV\n")

    print("Fetching schedules from MLB API...")
    expected: set[int] = set()
    for yr in args.season:
        expected |= fetch_completed_pks(yr)

    missing = sorted(expected - have)

    print(f"\nResults:")
    print(f"  Expected : {len(expected)}")
    print(f"  Have     : {len(have)}")
    print(f"  Missing  : {len(missing)}")

    if missing:
        print(f"\nMissing game PKs:")
        for pk in missing:
            print(f"  {pk}")

        if args.output:
            args.output.write_text("\n".join(str(pk) for pk in missing) + "\n")
            print(f"\nSaved to {args.output}")
    else:
        print("\nNo missing games — CSV is complete.")


if __name__ == "__main__":
    main()
