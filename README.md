# Smpltrax LCD Agent

Utilities and data for the Smpltrax LCD agent. R scripts live in `agent/`, while source datasets and exports are organized under `data/`. The repo is now platform-neutral; Windows wrappers remain, and macOS/Linux can use the provided shell scripts.

## Layout
- `agent/` - R entrypoints and helpers (`lcd_agent.R`, `configurator_app.R`, `bootstrap_conda.R`, *.bat and *.sh wrappers).
- `data/LabSolutions/` - Raw LabSolutions `.lcd` files (as provided).
- `data/Smpltrax/Exports/` - Generated CSV exports and processing log/state.
- `libs/`, `R-4.5.1/`, `r-miniconda/` - Optional local R/Python runtimes (ignored).

## Setup (macOS/Linux or Windows)
1) Install R (macOS: `brew install r`, Windows: R installer).
2) Install required R packages locally:
```bash
Rscript agent/setup_env.R
```
3) (Recommended) Create the Python env for `chromConverter` via reticulate:
```bash
Rscript agent/bootstrap_conda.R
```
This installs Miniconda under `r-miniconda/` (ignored by git) and creates env `r-reticulate` with `olefile`.
4) Edit `agent/config.yaml`:
   - `input_dirs` default to `../data/LabSolutions/Data`
   - `output_root` default to `../data/Smpltrax/Exports`
   Paths are resolved relative to the repo root unless absolute.

## Running the agent
- macOS/Linux:
  - Make scripts executable once: `chmod +x agent/run_agent*.sh`
  - Run with jittered delay: `./agent/run_agent.sh`
  - Run immediately/verbose: `./agent/run_agent_verbose.sh`
- Windows:
  - `agent/run_agent.bat` (with jitter) or `agent/run_agent_verbose.bat`
  - `agent/run_once.bat` to skip jitter
- The main entrypoint is `agent/lcd_agent.R`; it writes logs and state to `output_root` (`auto_convert.log`, `processed_state.csv`).

## Notes
- Logs are ignored except `data/Smpltrax/Exports/auto_convert.log` to keep processing history.
- If you add new large data drops, keep them under `data/` to avoid cluttering the root.
