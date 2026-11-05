"""
mlb_umpire_pull.py

Fetches MLB or AAA game data from the Stats API live feed and writes a CSV with:
  game_pk, date, away_team, home_team, away_score, home_score, winner,
  hp_umpire, 1b_umpire, 2b_umpire, 3b_umpire

Usage:
  # MLB 2025 + 2026 (default output: umpire_games.csv):
  python mlb_umpire_pull.py --season 2025 2026

  # AAA 2025 + 2026:
  python mlb_umpire_pull.py --level aaa --season 2025 2026

  # Tune concurrency (default 10 workers):
  python mlb_umpire_pull.py --season 2025 --workers 20

  # Pass game PKs directly:
  python mlb_umpire_pull.py 745456 745457 745458

  # Read game PKs from a file (one per line):
  python mlb_umpire_pull.py --file game_pks.txt

  # Specify output file:
  python mlb_umpire_pull.py --level aaa --season 2025 --output umpires_aaa_2025.csv

  # Resume an interrupted run (skips game PKs already in the output file):
  python mlb_umpire_pull.py --season 2025 --resume
"""

import argparse
import csv
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed, TimeoutError as FutureTimeout
from pathlib import Path

import requests

SPORT_IDS = {"mlb": 1, "aaa": 11}

SCHEDULE_URL = (
    "https://statsapi.mlb.com/api/v1/schedule"
    "?sportId={sport_id}&gameType=R&season={season}&fields=dates,games,gamePk,status,detailedState"
)
GAME_FEED_URL = "https://statsapi.mlb.com/api/v1.1/game/{game_pk}/feed/live"

OUTPUT_COLUMNS = [
    "game_pk",
    "date",
    "away_team",
    "home_team",
    "away_score",
    "home_score",
    "winner",
    "hp_umpire",
    "1b_umpire",
    "2b_umpire",
    "3b_umpire",
]

UMPIRE_TYPE_MAP = {
    "Home Plate": "hp_umpire",
    "First Base": "1b_umpire",
    "Second Base": "2b_umpire",
    "Third Base": "3b_umpire",
}


def fetch_season_game_pks(season: int, level: str = "mlb") -> list[int]:
    url = SCHEDULE_URL.format(sport_id=SPORT_IDS[level], season=season)
    print(f"  Fetching {level.upper()} {season} schedule from MLB API...")
    try:
        resp = requests.get(url, timeout=30)
        resp.raise_for_status()
        data = resp.json()
    except requests.RequestException as e:
        print(f"  Failed to fetch schedule for {season}: {e}", file=sys.stderr)
        return []

    pks = []
    for date_entry in data.get("dates", []):
        for game in date_entry.get("games", []):
            pk = game.get("gamePk")
            if pk:
                state = game.get("status", {}).get("detailedState", "")
                if state not in ("Scheduled", "Pre-Game", "Warmup", "Postponed", "Cancelled"):
                    pks.append(pk)
    print(f"  Found {len(pks)} completed/in-progress games for {level.upper()} {season}")
    return pks


def fetch_and_parse(game_pk: int, retries: int = 3) -> tuple[int, dict | None]:
    """Fetch and parse a single game. Returns (game_pk, row_or_None)."""
    url = GAME_FEED_URL.format(game_pk=game_pk)
    for attempt in range(1, retries + 1):
        try:
            # (connect_timeout, read_timeout) — read timeout is per-chunk so also
            # cap the total call with future.result(timeout=) in the caller.
            resp = requests.get(url, timeout=(5, 20))
            resp.raise_for_status()
            data = resp.json()
            break  # success
        except requests.HTTPError as e:
            print(f"  HTTP error gamePk {game_pk}: {e}", file=sys.stderr)
            return game_pk, None  # 4xx/5xx won't fix themselves, don't retry
        except requests.RequestException as e:
            if attempt < retries:
                time.sleep(2 ** attempt)  # 2s, 4s back-off
            else:
                print(f"  Failed gamePk {game_pk} after {retries} attempts: {e}", file=sys.stderr)
                return game_pk, None
    else:
        return game_pk, None

    try:
        game_data = data["gameData"]
        live_data = data["liveData"]

        pk = game_data["game"]["pk"]
        date = game_data["datetime"].get("originalDate", "")
        away_team = game_data["teams"]["away"]["name"]
        home_team = game_data["teams"]["home"]["name"]

        linescore = live_data.get("linescore", {})
        ls_teams = linescore.get("teams", {})
        away_score = ls_teams.get("away", {}).get("runs", "")
        home_score = ls_teams.get("home", {}).get("runs", "")

        status = game_data.get("status", {}).get("detailedState", "")
        if status == "Final" and away_score != "" and home_score != "":
            if home_score > away_score:
                winner = home_team
            elif away_score > home_score:
                winner = away_team
            else:
                winner = "Tie"
        else:
            winner = status

        officials = live_data.get("boxscore", {}).get("officials", [])
        umpires = {col: "" for col in ["hp_umpire", "1b_umpire", "2b_umpire", "3b_umpire"]}
        for entry in officials:
            col = UMPIRE_TYPE_MAP.get(entry.get("officialType", ""))
            if col:
                umpires[col] = entry.get("official", {}).get("fullName", "")

        return game_pk, {
            "game_pk": pk,
            "date": date,
            "away_team": away_team,
            "home_team": home_team,
            "away_score": away_score,
            "home_score": home_score,
            "winner": winner,
            **umpires,
        }

    except (KeyError, TypeError) as e:
        print(f"  Parse error gamePk {game_pk}: {e}", file=sys.stderr)
        return game_pk, None


def load_existing_pks(output_path: Path) -> set[int]:
    if not output_path.exists():
        return set()
    existing = set()
    with open(output_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            try:
                existing.add(int(row["game_pk"]))
            except (KeyError, ValueError):
                pass
    return existing


def main():
    parser = argparse.ArgumentParser(description="Pull MLB/AAA umpire data by season or game PK.")
    parser.add_argument("game_pks", nargs="*", type=int, help="One or more game PKs")
    parser.add_argument("--level", choices=["mlb", "aaa"], default="mlb",
                        help="League level: mlb (default) or aaa")
    parser.add_argument("--season", nargs="+", type=int, metavar="YEAR",
                        help="One or more seasons (e.g. --season 2025 2026)")
    parser.add_argument("--file", type=Path, help="Text file with one game PK per line")
    parser.add_argument("--output", type=Path, default=None,
                        help="Output CSV path (default: umpire_<level>_games.csv)")
    parser.add_argument("--workers", type=int, default=10,
                        help="Concurrent requests (default: 10)")
    parser.add_argument("--resume", action="store_true",
                        help="Skip game PKs already present in the output file")
    args = parser.parse_args()

    # Default output name reflects the level
    if args.output is None:
        args.output = Path(f"umpire_{args.level}_games.csv")

    game_pks: list[int] = list(args.game_pks)

    if args.season:
        for yr in args.season:
            game_pks.extend(fetch_season_game_pks(yr, args.level))

    if args.file:
        with open(args.file) as f:
            for line in f:
                line = line.strip()
                if line and line.isdigit():
                    game_pks.append(int(line))

    if not game_pks:
        parser.error("Provide at least one game PK, --file, or --season YEAR.")

    game_pks = list(dict.fromkeys(game_pks))

    if args.resume:
        already_done = load_existing_pks(args.output)
        if already_done:
            print(f"Resuming: {len(already_done)} game(s) already done, skipping.")
        game_pks = [pk for pk in game_pks if pk not in already_done]

    if not game_pks:
        print("Nothing new to fetch.")
        return

    total = len(game_pks)
    print(f"Fetching {total} game(s) with {args.workers} workers -> {args.output}")

    write_header = not args.output.exists() or args.output.stat().st_size == 0
    completed = 0
    failed = 0
    lock = threading.Lock()

    with open(args.output, "a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        if write_header:
            writer.writeheader()

        with ThreadPoolExecutor(max_workers=args.workers) as pool:
            futures = {pool.submit(fetch_and_parse, pk): pk for pk in game_pks}
            for future in as_completed(futures):
                pk = futures[future]
                try:
                    # Wall-clock cap: if a thread stalls past 90s total, cancel it
                    # rather than letting it block the executor shutdown.
                    pk, row = future.result(timeout=90)
                except FutureTimeout:
                    future.cancel()
                    row = None
                    print(f"  gamePk {pk} timed out (90s wall-clock)", file=sys.stderr)
                except Exception as e:
                    row = None
                    print(f"  gamePk {pk} unexpected error: {e}", file=sys.stderr)
                with lock:
                    completed += 1
                    if row:
                        writer.writerow(row)
                        f.flush()
                        print(f"  [{completed}/{total}] {row['away_team']} {row['away_score']} @ {row['home_team']} {row['home_score']}")
                    else:
                        failed += 1
                        print(f"  [{completed}/{total}] gamePk {pk} failed")

    written = total - failed
    print(f"\nDone. {written} row(s) written to {args.output}" +
          (f" ({failed} failed)" if failed else ""))


if __name__ == "__main__":
    main()
