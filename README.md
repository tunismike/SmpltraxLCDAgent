# Smpltrax LCD Agent

Utilities and data for the Smpltrax LCD agent. R scripts live in `agent/`, while source datasets and exports are organized under `data/`.

## Layout
- `agent/` – R entrypoints and helpers (`lcd_agent.R`, `configurator_app.R`, `bootstrap_conda.R`, *.bat wrappers).
- `data/LabSolutions/` – Raw LabSolutions `.lcd` files (as provided).
- `data/Smpltrax/Exports/` – Generated CSV exports and processing log/state.
- `libs/`, `R-4.5.1/` – Local R runtime and vendored packages (ignored).

## Running the agent
1. Update `agent/config.yaml` with the paths and settings you need.
2. Use the provided wrappers:
   - `agent/run_agent.bat` – normal run
   - `agent/run_agent_verbose.bat` – verbose logging
   - `agent/run_once.bat` – single-run helper
3. For fresh setups, use `agent/bootstrap_conda.R` to create the environment and `agent/configure.bat` to register the scheduled task (`register_task.ps1`).

## Notes
- Logs are ignored except `data/Smpltrax/Exports/auto_convert.log` to keep processing history.
- If you add new large data drops, keep them under `data/` to avoid cluttering the root.
