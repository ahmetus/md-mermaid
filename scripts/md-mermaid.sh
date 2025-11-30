#!/usr/bin/env bash

########################################################################
# Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>		       #
# This program is free software; you can redistribute it and/or modify #
# it under the terms of the GNU General Public License as published by #
# the Free Software Foundation, either version 3 of the License, or    #
# (at your option) any later version.				       #
# 								       #
# Author:  Ahmet Usal <ahmetusal@gmail.com>			       #
# Collaborators: OpenAI Assistant, Claude Assistant		       #
########################################################################

set -euo pipefail

# Markdown Mermaid renderer wrapper (md-mermaid)
# Presets:
#   -svg        → SVG for browser/grip (neutral, transparent)
#   -png1280    → PNG 1280px wide for Emacs (default theme, white bg)
#   -png1400    → PNG 1400px wide
#   -png1800    → PNG 1800px wide
#   -pngW NUM   → PNG custom width (pixels)
# Common flags:
#   -i/--input  INPUT.md   (required)
#   -o/--output OUTPUT.md  (optional; auto-derived if missing)
#   -d/--assets-dir DIR    (default: assets/mermaid)
#   -f/--force             (force re-render)
#   --config PATH          (mermaid config JSON; defaults to md-mermaid/mermaid-config.json for PNG presets if present)
#   --theme THEME          (override theme)
#   --background COLOR     (override background)

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
pkg_root=$(cd "$script_dir/.." && pwd)

PY=${PYTHON_BIN:-python}
command -v "$PY" >/dev/null 2>&1 || PY=python3

input=""; output=""; assets_dir=""; profile=""; width=""; force=false
config=""; theme=""; background=""; puppeteer_cfg=""

usage(){
  cat <<USAGE
Usage:
  bash md-mermaid/scripts/md-mermaid.sh -i INPUT.md [-o OUTPUT.md] [-d DIR] [-f]
    -svg | -png1280 | -png1400 | -png1800 | -pngW WIDTH [--config FILE] [--theme THEME] [--background COLOR]

Examples:
  bash md-mermaid/scripts/md-mermaid.sh -i PROJECT_REPORT.md -svg
  bash md-mermaid/scripts/md-mermaid.sh -i PROJECT_REPORT.md -o PROJECT_REPORT-emacs.md -png1800 -f
  bash md-mermaid/scripts/md-mermaid.sh -i x.md -pngW 1600 --background white --config "$pkg_root/mermaid-config.json"
USAGE
}

[[ $# -eq 0 ]] && { usage; exit 1; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    -i|--input) input=${2:-}; shift 2;;
    -o|--output) output=${2:-}; shift 2;;
    -d|--assets-dir) assets_dir=${2:-}; shift 2;;
    -f|--force) force=true; shift;;
    --config) config=${2:-}; shift 2;;
    --theme) theme=${2:-}; shift 2;;
    --background) background=${2:-}; shift 2;;
    --puppeteer-config) puppeteer_cfg=${2:-}; shift 2;;
    -svg) profile=svg; shift;;
    -png1280) profile=png; width=1280; shift;;
    -png1400) profile=png; width=1400; shift;;
    -png1800) profile=png; width=1800; shift;;
    -pngW) profile=png; width=${2:-}; shift 2;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown option: $1"; usage; exit 2;;
  esac
done

[[ -z "$input" ]] && { echo "Error: -i/--input is required"; usage; exit 2; }
[[ ! -f "$input" ]] && { echo "Error: input file not found: $input"; exit 2; }

# Basic tool checks
if ! command -v mmdc >/dev/null 2>&1; then
  echo "Error: 'mmdc' (Mermaid CLI) not found. Install with: npm i -g @mermaid-js/mermaid-cli" >&2
  exit 2
fi

# Defaults
assets_dir=${assets_dir:-assets/mermaid}

if [[ -z "$output" ]]; then
  base="${input%.*}"
  if [[ "$profile" == svg ]]; then
    output="${base}-images.md"
  else
    output="${base}-emacs.md"
  fi
fi

# Default config for PNG presets if present
if [[ "$profile" == png && -z "$config" ]]; then
  if [[ -f "$pkg_root/mermaid-config.json" ]]; then
    config="$pkg_root/mermaid-config.json"
  fi
fi

renderer="$script_dir/md_mermaid_render.py"

args=("$renderer" "$input" -o "$output" -d "$assets_dir")

if [[ "$profile" == svg ]]; then
  args+=( --format svg --theme "${theme:-neutral}" --background "${background:-transparent}" )
else
  # PNG profile
  args+=( --format png )
  if [[ -n "$width" ]]; then
    if [[ "$width" =~ ^[0-9]+$ ]]; then
      args+=( --width "$width" )
    else
      echo "Error: width must be an integer (pixels), got '$width'" >&2
      exit 2
    fi
  fi
  args+=( --theme "${theme:-default}" --background "${background:-white}" )
  [[ -n "$config" ]] && args+=( --config "$config" )
  [[ -n "$puppeteer_cfg" ]] && args+=( --puppeteer-config "$puppeteer_cfg" )
fi

if [ "$force" = true ]; then
  args+=( --force )
fi

echo "Running: $PY ${args[*]}" >&2
exec "$PY" "${args[@]}"
