# AIML 451 coursework and baseball analysis

Source code for AIML 451 assignments and projects.

- `code/`: baseball ABS/OBP Quarto analysis and Triple-A metadata collection.
- `scripts/`: Python scripts for collecting baseball Statcast, game, player, venue, and umpire data.
- `api_pulls/code/`: Reddit collection and classification scripts, including earlier versions in `archive/`.
- `api_pulls/writeup/`: Reddit analysis Quarto documents.
- `case_888/`: R case study scripts.
- Root R and Quarto files: homework and the case 505 analysis.
- `skills-tutorial/`: R package installation exercise.

## Running the code

R scripts and Quarto documents declare their required packages in the source. The Python collection scripts describe their dependencies and command-line options in their module documentation. Run baseball scripts from this repository's root; the case study and Reddit projects have their own `.here` markers for R project paths. Some coursework contains original local file paths that may need adjustment on another machine.

Reddit API collection reads `REDDIT_CLIENT_ID`, `REDDIT_CLIENT_SECRET`, and `REDDIT_USER_AGENT` from the environment. OpenAI classification scripts read `OPENAI_API_KEY`. Keep credentials in your local environment and out of Git.

Downloaded datasets, generated reports, plots, and checkpoints are excluded. Collect or supply the input data before running analyses; this repository contains source code, not a complete execution environment.
