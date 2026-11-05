"""
mlb_games_dim.py

Reads statcast CSV files, collects all unique game_pk values per league,
fetches game-level dimensional data from the MLB Stats API, and writes
games_dim.csv.

League is tagged per file: filenames containing "_aaa_" → AAA, otherwise → MLB.
MLB and AAA are fetched independently (Baseball Savant uses overlapping PK
ranges across sports, so deduplicating across leagues would drop AAA rows).

Usage:
  # Auto-detect all statcast_*.csv files in current directory:
  python mlb_games_dim.py

  # Specific files:
  python mlb_games_dim.py --files statcast_2025.csv statcast_aaa_2025.csv

  # Custom output:
  python mlb_games_dim.py --output dim/games.csv
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

# sportId tells the API which sport's schedule to search
SCHEDULE_URL = (
    "https://statsapi.mlb.com/api/v1/schedule"
    "?gamePks={pks}&sportId={sport_id}&hydrate=venue,weather,gameInfo,linescore,teams"
)

SPORT_IDS = {"MLB": 1, "AAA": 11}
BATCH_SIZE = 100

OUTPUT_COLUMNS = [
    "game_pk",
    "league",
    "game_date",
    "day_night",
    "status",
    "venue_id",
    "venue_name",
    "home_team_id",
    "home_team_name",
    "away_team_id",
    "away_team_name",
    "home_score",
    "away_score",
    "attendance",
    "game_duration_minutes",
    "delay_duration_minutes",
    "weather_condition",
    "weather_temp_f",
    "weather_wind",
]


def infer_league(path: Path) -> str:
    return "AAA" if "_aaa_" in path.name.lower() else "MLB"


def collect_pks_by_league(files: list[Path]) -> dict[str, set[int]]:
    """Returns {"MLB": {pk, ...}, "AAA": {pk, ...}} — processed independently."""
    by_league: dict[str, set[int]] = {"MLB": set(), "AAA": set()}
    for path in files:
        league = infer_league(path)
        print(f"  Reading {path.name} ({league}) ...", end=" ", flush=True)
        try:
            df = pd.read_csv(path, usecols=["game_pk"], dtype={"game_pk": "Int64"})
            pks = set(df["game_pk"].dropna().astype(int).tolist())
            before = len(by_league[league])
            by_league[league].update(pks)
            print(f"+{len(by_league[league]) - before} PKs ({len(by_league[league])} {league} total)")
        except Exception as e:
            print(f"ERROR: {e}", file=sys.stderr)
    return by_league


def fetch_batch(pks: list[int], league: str) -> list[dict]:
    url = SCHEDULE_URL.format(
        pks=",".join(str(p) for p in pks),
        sport_id=SPORT_IDS[league],
    )
    for attempt in range(1, 4):
        try:
            resp = requests.get(url, timeout=(5, 30))
            resp.raise_for_status()
            games = []
            for date_entry in resp.json().get("dates", []):
                games.extend(date_entry.get("games", []))
            return games
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


def parse_game(g: dict, league: str) -> dict:
    home = g.get("teams", {}).get("home", {})
    away = g.get("teams", {}).get("away", {})
    venue = g.get("venue", {})
    weather = g.get("weather", {})
    info = g.get("gameInfo", {})

    raw_date = g.get("gameDate", "")
    game_date = raw_date[:10] if raw_date else ""

    return {
        "game_pk":                g.get("gamePk", ""),
        "league":                 league,
        "game_date":              game_date,
        "day_night":              g.get("dayNight", ""),
        "status":                 g.get("status", {}).get("detailedState", ""),
        "venue_id":               venue.get("id", ""),
        "venue_name":             venue.get("name", ""),
        "home_team_id":           home.get("team", {}).get("id", ""),
        "home_team_name":         home.get("team", {}).get("name", ""),
        "away_team_id":           away.get("team", {}).get("id", ""),
        "away_team_name":         away.get("team", {}).get("name", ""),
        "home_score":             home.get("score", ""),
        "away_score":             away.get("score", ""),
        "attendance":             info.get("attendance", ""),
        "game_duration_minutes":  info.get("gameDurationMinutes", ""),
        "delay_duration_minutes": info.get("delayDurationMinutes", ""),
        "weather_condition":      weather.get("condition", ""),
        "weather_temp_f":         weather.get("temp", ""),
        "weather_wind":           weather.get("wind", ""),
    }


def fetch_league(league: str, pks: set[int], workers: int) -> list[dict]:
    """Fetch all games for one league, returning parsed rows."""
    pk_list = sorted(pks)
    batches = [pk_list[i:i + BATCH_SIZE] for i in range(0, len(pk_list), BATCH_SIZE)]
    total_batches = len(batches)
    print(f"\n  {league}: {len(pk_list)} PKs in {total_batches} batch(es)...")

    rows: list[dict] = []
    completed = 0
    lock = threading.Lock()

    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = {pool.submit(fetch_batch, batch, league): batch for batch in batches}
        for future in as_completed(futures):
            try:
                games = future.result(timeout=90)
            except FutureTimeout:
                future.cancel()
                games = []
                print(f"  [{league}] Batch timed out, skipping.", file=sys.stderr)
            except Exception as e:
                games = []
                print(f"  [{league}] Batch error: {e}", file=sys.stderr)

            with lock:
                completed += 1
                parsed = [parse_game(g, league) for g in games]
                rows.extend(parsed)
                print(f"  [{league}] [{completed}/{total_batches}] "
                      f"{len(games)} games ({len(rows)} total)")

    return rows


def main():
    parser = argparse.ArgumentParser(description="Build game dimension table from statcast CSVs.")
    parser.add_argument("--files", nargs="+", type=Path, metavar="CSV",
                        help="Statcast CSVs to scan (default: all statcast_*.csv in current dir)")
    parser.add_argument("--output", type=Path, default=Path("games_dim.csv"),
                        help="Output CSV path (default: games_dim.csv)")
    parser.add_argument("--workers", type=int, default=8,
                        help="Concurrent API batch requests per league (default: 8)")
    args = parser.parse_args()

    if args.files:
        files = [p for f in args.files for p in (Path(".").glob(str(f)) if "*" in str(f) else [f])]
    else:
        files = sorted(Path(".").glob("statcast_*.csv"))

    if not files:
        print("No statcast CSV files found. Use --files to specify them.", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {len(files)} file(s) for game PKs...")
    by_league = collect_pks_by_league(files)

    mlb_count = len(by_league["MLB"])
    aaa_count = len(by_league["AAA"])
    overlap = len(by_league["MLB"] & by_league["AAA"])
    print(f"\nMLB: {mlb_count} PKs | AAA: {aaa_count} PKs | Overlap: {overlap} PKs")
    print("Fetching each league independently from MLB Stats API...")

    all_rows: list[dict] = []
    if by_league["MLB"]:
        all_rows.extend(fetch_league("MLB", by_league["MLB"], args.workers))
    if by_league["AAA"]:
        all_rows.extend(fetch_league("AAA", by_league["AAA"], args.workers))

    if not all_rows:
        print("No game data retrieved.", file=sys.stderr)
        sys.exit(1)

    # Deduplicate within each league on game_pk
    seen: set[tuple] = set()
    deduped = []
    for row in all_rows:
        key = (row["league"], row["game_pk"])
        if key not in seen:
            seen.add(key)
            deduped.append(row)

    deduped.sort(key=lambda r: (r["league"], r["game_date"]))

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with open(args.output, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(deduped)

    mlb_ct = sum(1 for r in deduped if r["league"] == "MLB")
    aaa_ct = sum(1 for r in deduped if r["league"] == "AAA")
    print(f"\nDone. {len(deduped)} rows written to {args.output} "
          f"(MLB: {mlb_ct}, AAA: {aaa_ct})")


if __name__ == "__main__":
    main()
