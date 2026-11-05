"""
mlb_statcast_pull.py

Pull Statcast pitch-by-pitch data for any season(s) and level (MLB or AAA).

Requires: pip install pybaseball pandas requests

Usage:
  # MLB 2025 + 2026 (default):
  python mlb_statcast_pull.py

  # Specific seasons:
  python mlb_statcast_pull.py --season 2021 2022 2023

  # AAA data:
  python mlb_statcast_pull.py --level aaa --season 2024 2025

  # Custom output directory:
  python mlb_statcast_pull.py --season 2025 --outdir data/statcast

  # Skip combined file:
  python mlb_statcast_pull.py --season 2025 2026 --no-combined
"""

import argparse
import io
import time
from datetime import date, timedelta
from pathlib import Path

import pandas as pd
import pybaseball
import requests
from pybaseball import statcast

pybaseball.cache.enable()

# ---------------------------------------------------------------------------
# Known season date ranges — anything not listed falls back to Mar 20–Oct 5
# ---------------------------------------------------------------------------
SEASON_DATES = {
    2015: ("2015-04-05", "2015-10-04"),
    2016: ("2016-04-03", "2016-10-02"),
    2017: ("2017-04-02", "2017-10-01"),
    2018: ("2018-03-29", "2018-10-01"),
    2019: ("2019-03-28", "2019-09-29"),
    2020: ("2020-07-23", "2020-09-27"),  # COVID shortened season
    2021: ("2021-04-01", "2021-10-03"),
    2022: ("2022-04-07", "2022-10-05"),
    2023: ("2023-03-30", "2023-10-01"),
    2024: ("2024-03-20", "2024-09-29"),
    2025: ("2025-03-27", "2025-09-28"),
    2026: ("2026-03-26", str(date.today())),
}

SAVANT_AAA_URL = (
    "https://baseballsavant.mlb.com/statcast_search/csv"
    "?all=true&type=details&player_type=pitcher&game_type=R"
    "&game_date_gt={start}&game_date_lt={end}&season={year}&minors=true"
)

COLUMNS_OF_INTEREST = [
    "game_date", "game_pk", "game_year", "game_type",
    "home_team", "away_team",
    "inning", "inning_topbot",
    "at_bat_number", "pitch_number",
    "batter", "pitcher",
    "stand", "p_throws",
    "events", "description",
    "ab_type", "abs_call", "abs_challenge_team", "challenge_result",
    "pitch_type", "pitch_name",
    "release_speed", "effective_speed", "release_spin_rate", "spin_axis",
    "release_extension", "release_pos_x", "release_pos_y", "release_pos_z",
    "vx0", "vy0", "vz0", "ax", "ay", "az",
    "pfx_x", "pfx_z",
    "plate_x", "plate_z",
    "sz_top", "sz_bot",
    "zone",
    "balls", "strikes",
    "on_1b", "on_2b", "on_3b",
    "outs_when_up",
    "score_differential",
    "type", "bb_type",
    "launch_speed", "launch_angle",
    "estimated_ba_using_speedangle", "estimated_woba_using_speedangle",
    "woba_value", "delta_run_exp",
]


def season_date_range(year: int) -> tuple[str, str]:
    if year in SEASON_DATES:
        return SEASON_DATES[year]
    # Reasonable fallback for unknown years
    return (f"{year}-03-20", f"{year}-10-05")


def pull_mlb_season(year: int, start_str: str, end_str: str) -> pd.DataFrame:
    """Pull MLB Statcast via pybaseball, chunked by month."""
    start = date.fromisoformat(start_str)
    end   = date.fromisoformat(end_str)
    chunks = []
    chunk_start = start

    print(f"\n=== MLB {year} ({start_str} to {end_str}) ===")

    while chunk_start <= end:
        if chunk_start.month == 12:
            next_month = chunk_start.replace(year=chunk_start.year + 1, month=1, day=1)
        else:
            next_month = chunk_start.replace(month=chunk_start.month + 1, day=1)
        chunk_end = min(next_month - timedelta(days=1), end)

        s, e = chunk_start.isoformat(), chunk_end.isoformat()
        print(f"  {s} to {e} ...", end=" ", flush=True)

        try:
            df_chunk = statcast(start_dt=s, end_dt=e)
            if df_chunk is not None and not df_chunk.empty:
                print(f"{len(df_chunk):,} pitches")
                chunks.append(df_chunk)
            else:
                print("no data")
        except Exception as exc:
            print(f"ERROR: {exc}")

        chunk_start = next_month
        time.sleep(2)

    if not chunks:
        return pd.DataFrame()

    df = pd.concat(chunks, ignore_index=True)
    print(f"  Total: {len(df):,} pitches across {df['game_pk'].nunique():,} games")
    return df


SAVANT_ROW_CAP = 25000  # Baseball Savant truncates responses at this many rows


def pull_aaa_season(year: int, start_str: str, end_str: str) -> pd.DataFrame:
    """Pull AAA Statcast from Baseball Savant using weekly chunks.

    Monthly chunks regularly exceed Baseball Savant's 25k-row cap and silently
    truncate. Weekly chunks stay safely under the cap for AAA volume.
    """
    start = date.fromisoformat(start_str)
    end   = date.fromisoformat(end_str)
    chunks = []
    chunk_start = start

    print(f"\n=== AAA {year} ({start_str} to {end_str}) ===")

    while chunk_start <= end:
        chunk_end = min(chunk_start + timedelta(days=2), end)
        s, e = chunk_start.isoformat(), chunk_end.isoformat()
        print(f"  {s} to {e} ...", end=" ", flush=True)

        url = SAVANT_AAA_URL.format(start=s, end=e, year=year)
        df_chunk = None
        for attempt in range(1, 5):
            try:
                resp = requests.get(url, timeout=60)
                if resp.status_code in (500, 502, 503, 504):
                    # Transient server error — wait and retry
                    wait = 10 * attempt
                    print(f"[{resp.status_code}] retrying in {wait}s (attempt {attempt}/4)...", end=" ", flush=True)
                    time.sleep(wait)
                    continue
                resp.raise_for_status()  # raise on any other 4xx
                content = resp.text.strip()
                lines = content.splitlines()
                if len(lines) > 1 and content != "Error":
                    df_chunk = pd.read_csv(io.StringIO(content), low_memory=False)
                    if df_chunk.empty:
                        df_chunk = None
                break
            except requests.HTTPError as exc:
                print(f"ERROR (permanent): {exc}")
                break
            except requests.RequestException as exc:
                wait = 10 * attempt
                print(f"ERROR: {exc} — retrying in {wait}s (attempt {attempt}/4)...", end=" ", flush=True)
                time.sleep(wait)

        if df_chunk is not None and not df_chunk.empty:
            if len(df_chunk) >= SAVANT_ROW_CAP:
                print(f"{len(df_chunk):,} pitches  *** HIT ROW CAP — data truncated, reduce chunk size ***")
            else:
                print(f"{len(df_chunk):,} pitches")
            chunks.append(df_chunk)
        else:
            print("no data")

        chunk_start = chunk_end + timedelta(days=1)
        time.sleep(2)

    if not chunks:
        return pd.DataFrame()

    df = pd.concat(chunks, ignore_index=True)
    game_col = "game_pk" if "game_pk" in df.columns else df.columns[0]
    print(f"  Total: {len(df):,} pitches across {df[game_col].nunique():,} games")
    return df


def select_and_clean(df: pd.DataFrame, level: str) -> pd.DataFrame:
    available = [c for c in COLUMNS_OF_INTEREST if c in df.columns]
    missing   = [c for c in COLUMNS_OF_INTEREST if c not in df.columns]

    if missing:
        print(f"  Note: {len(missing)} requested columns absent from this pull:")
        for col in missing:
            print(f"    - {col}")

    df = df[available].copy()

    if "game_date" in df.columns:
        df["game_date"] = pd.to_datetime(df["game_date"])

    # For MLB, restrict to regular season; AAA feed is already regular season only
    if level == "mlb" and "game_type" in df.columns:
        before = len(df)
        df = df[df["game_type"] == "R"].copy()
        print(f"  Filtered to regular season: {before:,} -> {len(df):,} pitches")

    return df


def summarize(df: pd.DataFrame, label: str):
    print(f"\n--- {label} Summary ---")
    print(f"  Pitches : {len(df):,}")
    if "game_pk" in df.columns:
        print(f"  Games   : {df['game_pk'].nunique():,}")
    if "game_date" in df.columns:
        print(f"  Dates   : {df['game_date'].min().date()} to {df['game_date'].max().date()}")
    if "pitch_type" in df.columns:
        print("  Pitch types (top 10):")
        for ptype, cnt in df["pitch_type"].value_counts().head(10).items():
            print(f"    {ptype:<6} {cnt:>8,}")


def main():
    parser = argparse.ArgumentParser(description="Pull Statcast data by season and level.")
    parser.add_argument("--season", nargs="+", type=int, default=[2025, 2026],
                        metavar="YEAR", help="Season year(s) to pull (default: 2025 2026)")
    parser.add_argument("--level", choices=["mlb", "aaa"], default="mlb",
                        help="League level: mlb (default) or aaa")
    parser.add_argument("--outdir", type=Path, default=Path("."),
                        help="Directory for output CSV files (default: current dir)")
    parser.add_argument("--no-combined", action="store_true",
                        help="Skip writing the combined multi-season CSV")
    args = parser.parse_args()

    args.outdir.mkdir(parents=True, exist_ok=True)

    pull_fn = pull_aaa_season if args.level == "aaa" else pull_mlb_season
    all_frames = {}

    for year in args.season:
        start_str, end_str = season_date_range(year)

        df_raw = pull_fn(year, start_str, end_str)
        if df_raw.empty:
            print(f"Skipping {year} — no data returned.")
            continue

        df_clean = select_and_clean(df_raw, args.level)
        summarize(df_clean, f"{args.level.upper()} {year}")

        out_path = args.outdir / f"statcast_{args.level}_{year}.csv"
        df_clean.to_csv(out_path, index=False)
        print(f"\n  Saved -> {out_path}")
        all_frames[year] = df_clean

    if not args.no_combined and len(all_frames) > 1:
        df_combined = pd.concat(all_frames.values(), ignore_index=True)
        seasons_str = "_".join(str(y) for y in sorted(all_frames))
        combined_path = args.outdir / f"statcast_{args.level}_{seasons_str}_combined.csv"
        df_combined.to_csv(combined_path, index=False)
        print(f"\nCombined saved -> {combined_path}  ({len(df_combined):,} total pitches)")

    if all_frames:
        sample_df = next(iter(all_frames.values()))
        print("\n=== Columns in output ===")
        for col in sorted(sample_df.columns):
            print(f"  {col}")


if __name__ == "__main__":
    main()
