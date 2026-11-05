"""
aaa_venues.py

Pulls every stadium used for AAA games from 2021 to present, including
venue metadata and date-based active/inactive status derived from the schedule.

Outputs two files:
  aaa_venues.csv       — one row per venue with full metadata
  aaa_venue_games.csv  — one row per game_pk linking game_pk → venue_id

Usage:
  python aaa_venues.py
  python aaa_venues.py --seasons 2021 2022 2023 2024 2025 2026
  python aaa_venues.py --output aaa_venues.csv --games-output aaa_venue_games.csv
"""

import argparse
import csv
import sys
import time
from collections import defaultdict
from datetime import date
from pathlib import Path

import requests

SCHEDULE_URL = (
    "https://statsapi.mlb.com/api/v1/schedule"
    "?sportId=11&gameType=R&season={season}"
    "&fields=dates,date,games,gamePk,venue,id,name,teams,home,team"
)
VENUES_URL = (
    "https://statsapi.mlb.com/api/v1/venues"
    "?venueIds={ids}&hydrate=location,fieldInfo,timezone"
)

VENUE_BATCH = 50

VENUE_COLUMNS = [
    "venue_id",
    "venue_name",
    "active",
    "first_game_date",
    "last_game_date",
    "seasons_active",
    "home_teams",
    "city",
    "state",
    "state_abbrev",
    "country",
    "address",
    "latitude",
    "longitude",
    "timezone_id",
    "timezone_abbrev",
    "capacity",
    "turf_type",
    "roof_type",
    "lf_line",
    "lf_center",
    "center",
    "rf_center",
    "rf_line",
]

GAME_COLUMNS = [
    "game_pk",
    "game_date",
    "season",
    "venue_id",
    "venue_name",
    "home_team",
]


def fetch_schedule(season: int) -> list[dict]:
    url = SCHEDULE_URL.format(season=season)
    print(f"  Fetching {season} AAA schedule...", end=" ", flush=True)
    for attempt in range(1, 4):
        try:
            resp = requests.get(url, timeout=30)
            resp.raise_for_status()
            games = []
            for date_entry in resp.json().get("dates", []):
                for g in date_entry.get("games", []):
                    g["_date"] = date_entry.get("date", "")
                    g["_season"] = season
                    games.append(g)
            print(f"{len(games)} games")
            return games
        except requests.RequestException as e:
            if attempt < 3:
                time.sleep(5 * attempt)
            else:
                print(f"FAILED: {e}", file=sys.stderr)
    return []


def fetch_venue_details(venue_ids: list[int]) -> dict[int, dict]:
    details: dict[int, dict] = {}
    batches = [venue_ids[i:i + VENUE_BATCH] for i in range(0, len(venue_ids), VENUE_BATCH)]
    for batch in batches:
        url = VENUES_URL.format(ids=",".join(str(v) for v in batch))
        for attempt in range(1, 4):
            try:
                resp = requests.get(url, timeout=30)
                resp.raise_for_status()
                for v in resp.json().get("venues", []):
                    details[v["id"]] = v
                break
            except requests.RequestException as e:
                if attempt < 3:
                    time.sleep(5 * attempt)
                else:
                    print(f"  Venue batch failed: {e}", file=sys.stderr)
    return details


def build_venue_record(
    venue_id: int,
    venue_name: str,
    game_dates: list[str],
    home_teams: set[str],
    seasons: set[int],
    detail: dict,
) -> dict:
    sorted_dates = sorted(game_dates)
    first = sorted_dates[0] if sorted_dates else ""
    last  = sorted_dates[-1] if sorted_dates else ""

    api_active = detail.get("active", None)
    if api_active is None:
        api_active = (date.fromisoformat(last).year >= date.today().year - 1) if last else False

    loc    = detail.get("location", {})
    coords = loc.get("defaultCoordinates", {})
    tz     = detail.get("timeZone", {})
    fi     = detail.get("fieldInfo", {})

    return {
        "venue_id":        venue_id,
        "venue_name":      detail.get("name", venue_name),
        "active":          api_active,
        "first_game_date": first,
        "last_game_date":  last,
        "seasons_active":  "|".join(str(y) for y in sorted(seasons)),
        "home_teams":      "|".join(sorted(home_teams)),
        "city":            loc.get("city", ""),
        "state":           loc.get("state", ""),
        "state_abbrev":    loc.get("stateAbbrev", ""),
        "country":         loc.get("country", ""),
        "address":         loc.get("address1", ""),
        "latitude":        coords.get("latitude", ""),
        "longitude":       coords.get("longitude", ""),
        "timezone_id":     tz.get("id", ""),
        "timezone_abbrev": tz.get("tz", ""),
        "capacity":        fi.get("capacity", ""),
        "turf_type":       fi.get("turfType", ""),
        "roof_type":       fi.get("roofType", ""),
        "lf_line":         fi.get("leftLine", ""),
        "lf_center":       fi.get("leftCenter", ""),
        "center":          fi.get("center", ""),
        "rf_center":       fi.get("rightCenter", ""),
        "rf_line":         fi.get("rightLine", ""),
    }


def main():
    parser = argparse.ArgumentParser(description="Pull AAA venue/stadium data by season range.")
    parser.add_argument("--seasons", nargs="+", type=int,
                        default=list(range(2021, date.today().year + 1)),
                        metavar="YEAR", help="Seasons to scan (default: 2021 to current year)")
    parser.add_argument("--output", type=Path, default=Path("aaa_venues.csv"),
                        help="Venue metadata CSV (default: aaa_venues.csv)")
    parser.add_argument("--games-output", type=Path, default=Path("aaa_venue_games.csv"),
                        help="Game-to-venue mapping CSV (default: aaa_venue_games.csv)")
    args = parser.parse_args()

    # Collect game-level info from the schedule
    venue_dates:   dict[int, list[str]] = defaultdict(list)
    venue_teams:   dict[int, set[str]]  = defaultdict(set)
    venue_seasons: dict[int, set[int]]  = defaultdict(set)
    venue_names:   dict[int, str]       = {}
    game_rows:     list[dict]           = []

    print(f"Fetching AAA schedules for {args.seasons}...")
    for season in args.seasons:
        games = fetch_schedule(season)
        for g in games:
            v    = g.get("venue", {})
            vid  = v.get("id")
            if not vid:
                continue
            vname     = v.get("name", "")
            game_pk   = g.get("gamePk", "")
            game_date = g.get("_date", "")
            home_team = g.get("teams", {}).get("home", {}).get("team", {}).get("name", "")

            venue_names[vid] = vname
            venue_dates[vid].append(game_date)
            venue_seasons[vid].add(season)
            if home_team:
                venue_teams[vid].add(home_team)

            game_rows.append({
                "game_pk":    game_pk,
                "game_date":  game_date,
                "season":     season,
                "venue_id":   vid,
                "venue_name": vname,
                "home_team":  home_team,
            })
        time.sleep(0.5)

    venue_ids = sorted(venue_names.keys())
    print(f"\n{len(venue_ids)} unique venues, {len(game_rows)} games. Fetching venue details...")
    details = fetch_venue_details(venue_ids)

    venue_rows = []
    for vid in venue_ids:
        row = build_venue_record(
            venue_id=vid,
            venue_name=venue_names[vid],
            game_dates=venue_dates[vid],
            home_teams=venue_teams[vid],
            seasons=venue_seasons[vid],
            detail=details.get(vid, {}),
        )
        venue_rows.append(row)

    venue_rows.sort(key=lambda r: (not r["active"], r["venue_name"]))
    game_rows.sort(key=lambda r: (r["game_date"], r["game_pk"]))

    # Write venue metadata
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with open(args.output, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=VENUE_COLUMNS)
        writer.writeheader()
        writer.writerows(venue_rows)

    # Write game-to-venue mapping
    args.games_output.parent.mkdir(parents=True, exist_ok=True)
    with open(args.games_output, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=GAME_COLUMNS)
        writer.writeheader()
        writer.writerows(game_rows)

    active_ct   = sum(1 for r in venue_rows if r["active"])
    inactive_ct = len(venue_rows) - active_ct
    print(f"\nVenues  -> {args.output}  ({len(venue_rows)} venues: {active_ct} active, {inactive_ct} inactive)")
    print(f"Games   -> {args.games_output}  ({len(game_rows)} game-venue rows)")


if __name__ == "__main__":
    main()
