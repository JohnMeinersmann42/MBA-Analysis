"""
mlb_players_dim.py

Reads statcast CSV files, collects all unique pitcher/batter MLBAM IDs,
then fetches player dimensional data from the MLB Stats API and writes
a single players_dim.csv.

Works for both MLB and AAA player IDs (same people endpoint covers both).

Usage:
  # Auto-detect all statcast_*.csv files in current directory:
  python mlb_players_dim.py

  # Specific files:
  python mlb_players_dim.py --files statcast_mlb_2025.csv statcast_aaa_2025.csv

  # Glob pattern:
  python mlb_players_dim.py --files statcast_*.csv

  # Custom output:
  python mlb_players_dim.py --output dim/players.csv
"""

import argparse
import csv
import sys
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed, TimeoutError as FutureTimeout
from pathlib import Path

import pandas as pd
import requests

PEOPLE_URL = (
    "https://statsapi.mlb.com/api/v1/people"
    "?personIds={ids}&hydrate=currentTeam,drafts"
)

BATCH_SIZE = 150  # IDs per API request (well within URL length limits)

OUTPUT_COLUMNS = [
    "player_id",
    "full_name",
    "first_name",
    "last_name",
    "use_name",          # preferred first name (e.g. "CJ" instead of "Carl")
    "jersey_number",
    "birth_date",
    "current_age",
    "birth_city",
    "birth_country",
    "height",
    "weight",
    "position",
    "position_code",
    "bat_side",          # L / R / S
    "pitch_hand",        # L / R
    "active",
    "mlb_debut_date",
    "draft_year",
    "current_team_id",
    "current_team_name",
]


def collect_player_ids(files: list[Path]) -> set[int]:
    """Read only the pitcher/batter columns from each statcast CSV."""
    ids: set[int] = set()
    for path in files:
        print(f"  Reading IDs from {path.name} ...", end=" ", flush=True)
        try:
            df = pd.read_csv(path, usecols=["pitcher", "batter"], dtype={"pitcher": "Int64", "batter": "Int64"})
            before = len(ids)
            ids.update(df["pitcher"].dropna().astype(int).tolist())
            ids.update(df["batter"].dropna().astype(int).tolist())
            print(f"+{len(ids) - before} new IDs ({len(ids)} total)")
        except Exception as e:
            print(f"ERROR: {e}", file=sys.stderr)
    return ids


def fetch_batch(player_ids: list[int]) -> list[dict]:
    """Fetch one batch of up to BATCH_SIZE player IDs from the API."""
    url = PEOPLE_URL.format(ids=",".join(str(i) for i in player_ids))
    for attempt in range(1, 4):
        try:
            resp = requests.get(url, timeout=(5, 20))
            resp.raise_for_status()
            return resp.json().get("people", [])
        except requests.HTTPError as e:
            print(f"  HTTP error: {e}", file=sys.stderr)
            return []
        except requests.RequestException as e:
            if attempt < 3:
                time.sleep(2 ** attempt)
            else:
                print(f"  Batch failed after 3 attempts: {e}", file=sys.stderr)
                return []
    return []


def parse_person(p: dict) -> dict:
    drafts = p.get("drafts", [])
    # Take the highest-round Rule 4 draft entry (most meaningful)
    rule4 = [d for d in drafts if "Rule 4" in d.get("description", "")]
    draft_year = rule4[-1]["year"] if rule4 else (drafts[-1]["year"] if drafts else "")

    current_team = p.get("currentTeam", {})

    return {
        "player_id":        p.get("id", ""),
        "full_name":        p.get("fullName", ""),
        "first_name":       p.get("firstName", ""),
        "last_name":        p.get("lastName", ""),
        "use_name":         p.get("useName", ""),
        "jersey_number":    p.get("primaryNumber", ""),
        "birth_date":       p.get("birthDate", ""),
        "current_age":      p.get("currentAge", ""),
        "birth_city":       p.get("birthCity", ""),
        "birth_country":    p.get("birthCountry", ""),
        "height":           p.get("height", ""),
        "weight":           p.get("weight", ""),
        "position":         p.get("primaryPosition", {}).get("name", ""),
        "position_code":    p.get("primaryPosition", {}).get("code", ""),
        "bat_side":         p.get("batSide", {}).get("code", ""),
        "pitch_hand":       p.get("pitchHand", {}).get("code", ""),
        "active":           p.get("active", ""),
        "mlb_debut_date":   p.get("mlbDebutDate", ""),
        "draft_year":       draft_year,
        "current_team_id":  current_team.get("id", ""),
        "current_team_name": current_team.get("name", ""),
    }


def main():
    parser = argparse.ArgumentParser(description="Build player dimension table from statcast CSVs.")
    parser.add_argument("--files", nargs="+", type=Path, metavar="CSV",
                        help="Statcast CSV files to scan (default: all statcast_*.csv in current dir)")
    parser.add_argument("--output", type=Path, default=Path("players_dim.csv"),
                        help="Output CSV path (default: players_dim.csv)")
    parser.add_argument("--workers", type=int, default=8,
                        help="Concurrent API batch requests (default: 8)")
    args = parser.parse_args()

    # Resolve input files
    if args.files:
        files = [p for f in args.files for p in (Path(".").glob(str(f)) if "*" in str(f) else [f])]
    else:
        files = sorted(Path(".").glob("statcast_*.csv"))

    if not files:
        print("No statcast CSV files found. Use --files to specify them.", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {len(files)} file(s) for player IDs...")
    player_ids = collect_player_ids(files)
    print(f"\n{len(player_ids)} unique player IDs found across all files.")

    # Split into batches
    id_list = sorted(player_ids)
    batches = [id_list[i:i + BATCH_SIZE] for i in range(0, len(id_list), BATCH_SIZE)]
    total_batches = len(batches)
    print(f"Fetching {total_batches} batch(es) of up to {BATCH_SIZE} from MLB Stats API...")

    rows = []
    completed = 0
    lock = threading.Lock()

    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = {pool.submit(fetch_batch, batch): batch for batch in batches}
        for future in as_completed(futures):
            batch = futures[future]
            try:
                people = future.result(timeout=90)
            except FutureTimeout:
                future.cancel()
                people = []
                print(f"  Batch timed out (90s), skipping {len(batch)} IDs", file=sys.stderr)
            except Exception as e:
                people = []
                print(f"  Batch error: {e}", file=sys.stderr)

            with lock:
                completed += 1
                parsed = [parse_person(p) for p in people]
                rows.extend(parsed)
                print(f"  [{completed}/{total_batches}] {len(people)} players fetched "
                      f"({len(rows)} total so far)")

    if not rows:
        print("No player data retrieved.", file=sys.stderr)
        sys.exit(1)

    # Deduplicate on player_id (shouldn't happen but be safe)
    seen: set[int] = set()
    deduped = []
    for row in rows:
        if row["player_id"] not in seen:
            seen.add(row["player_id"])
            deduped.append(row)

    deduped.sort(key=lambda r: r["full_name"])

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with open(args.output, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(deduped)

    print(f"\nDone. {len(deduped)} players written to {args.output}")


if __name__ == "__main__":
    main()
