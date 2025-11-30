#!/usr/bin/env python3
"""
Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Author:  Ahmet Usal <ahmetusal@gmail.com>
Collaborators: OpenAI Assistant, Claude Assistant

Render a single Mermaid snippet from stdin using mermaid-cli (mmdc).

- Reuses the same caching idea as the batch renderer: the output filename is
  sha1(snippet + options_digest). If the image exists, it will not re-render.
  The absolute output path is printed on stdout.

This file is new and does not modify the existing batch renderer.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
from pathlib import Path

from friendly_errors import classify_mermaid_error


def sha1(s: str) -> str:
    return hashlib.sha1(s.encode("utf-8")).hexdigest()


def ensure_cmd(cmd: str) -> None:
    if shutil.which(cmd) is None:
        print(f"Error: '{cmd}' not found on PATH. Install @mermaid-js/mermaid-cli (mmdc).", file=sys.stderr)
        sys.exit(1)


def options_digest(fmt: str, theme: str, background: str, config: Path | None, scale: float | None, width: int | None) -> str:
    parts = [
        f"fmt={fmt}",
        f"theme={theme}",
        f"bg={background}",
        f"scale={scale}",
        f"width={width}",
    ]
    if config is not None:
        try:
            cfg = Path(config).read_text(encoding="utf-8")
            parts.append(f"config={sha1(cfg)}")
        except Exception:
            parts.append(f"config-path={config}")
    return sha1("|".join(parts))


def render(tmp_mmd: Path, out_path: Path, fmt: str, theme: str, background: str, config: Path | None, scale: float | None, width: int | None, puppeteer_config: Path | None) -> tuple[bool, str]:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = ["mmdc", "-i", str(tmp_mmd), "-o", str(out_path)]
    if theme:
        cmd += ["-t", theme]
    if background:
        cmd += ["-b", background]
    if config is not None:
        cmd += ["-c", str(config)]
    if scale is not None:
        cmd += ["--scale", str(scale)]
    if width is not None:
        cmd += ["--width", str(width)]
    if puppeteer_config is not None:
        cmd += ["-p", str(puppeteer_config)]
    completed = subprocess.run(cmd, capture_output=True, text=True)
    if completed.returncode == 0:
        return True, ""
    stderr_text = completed.stderr or ""
    if completed.stdout:
        stderr_text = f"{stderr_text}\n{completed.stdout}".strip()
    return False, stderr_text.strip()


def main() -> None:
    ap = argparse.ArgumentParser(description="Render a single Mermaid snippet from stdin using mmdc")
    ap.add_argument("--assets-dir", type=Path, default=Path("assets/mermaid"), help="Directory for .mmd and rendered images")
    ap.add_argument("--format", choices=["png", "svg"], default="png", help="Output format")
    ap.add_argument("--theme", default="default", help="Mermaid theme")
    ap.add_argument("--background", default="white", help="Background color or 'transparent'")
    ap.add_argument("--config", type=Path, help="Path to mermaid config JSON")
    ap.add_argument("--scale", type=float, help="Scale factor (e.g., 1.25)")
    ap.add_argument("--width", type=int, help="Width in pixels")
    ap.add_argument("--puppeteer-config", type=Path, help="Puppeteer launch config JSON")

    args = ap.parse_args()

    ensure_cmd("mmdc")

    # Normalize config
    if args.config is not None:
        cfg_path = Path(args.config).expanduser().resolve()
        if not cfg_path.exists():
            print(f"Error: --config file not found: {cfg_path}", file=sys.stderr)
            sys.exit(2)
        args.config = cfg_path

    assets_dir: Path = args.assets_dir.expanduser().resolve()
    try:
        snippet = sys.stdin.read()
    except Exception as e:
        print(f"Error reading stdin: {e}", file=sys.stderr)
        sys.exit(2)

    if not snippet.strip():
        print("Error: empty Mermaid snippet on stdin", file=sys.stderr)
        sys.exit(2)

    digest = sha1(snippet + "|" + options_digest(args.format, args.theme, args.background, args.config, args.scale, args.width))
    mmd_path = assets_dir / f"{digest}.mmd"
    out_path = assets_dir / f"{digest}.{args.format}"

    # Write snippet .mmd and render if needed
    assets_dir.mkdir(parents=True, exist_ok=True)
    try:
        mmd_path.write_text(snippet, encoding="utf-8")
    except Exception as e:
        print(f"Error writing {mmd_path}: {e}", file=sys.stderr)
        sys.exit(2)

    ok = True
    stderr_text = ""
    if not out_path.exists():
        ok, stderr_text = render(mmd_path, out_path, args.format, args.theme, args.background, args.config, args.scale, args.width, args.puppeteer_config)
    if ok and out_path.exists():
        print(str(out_path.resolve()))
        sys.exit(0)
    else:
        friendly = classify_mermaid_error(stderr_text or "")
        print(json.dumps(friendly, ensure_ascii=False), file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
