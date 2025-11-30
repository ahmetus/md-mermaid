#!/usr/bin/env python3
"""
Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Author:  Ahmet Usal <ahmetusal@gmail.com>
Collaborators: OpenAI Assistant, Claude Assistant
Version: 1.0
Render Mermaid code fences in a Markdown file to images using mermaid-cli (mmdc).

Usage:
  python3 md-mermaid/scripts/md_mermaid_render.py INPUT.md [-o OUTPUT.md] [-d assets/mermaid] [--format svg|png]

Behavior:
  - Scans INPUT.md for ```mermaid fences
  - Writes each fence to <assets-dir>/<sha1>.mmd
  - Runs: mmdc -i <.mmd> -o <assets-dir>/<sha1>.<ext>
  - Outputs Markdown where each fence is replaced by an image link:
      ![Mermaid diagram](assets/mermaid/<sha1>.<ext>)

Requirements:
  - Node.js >= 18
  - @mermaid-js/mermaid-cli installed and available as `mmdc` on PATH

Notes:
  - Use PUPPETEER_EXECUTABLE_PATH environment variable to point to Chromium/Chrome if needed.
"""

import argparse
import hashlib
import json
import os
import re
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


def render_mmd(
    mmd_path: Path,
    out_path: Path,
    fmt: str,
    theme: str,
    background: str,
    config: Path | None,
    scale: float | None,
    width: int | None,
    puppeteer_config: Path | None,
) -> tuple[bool, str]:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = ["mmdc", "-i", str(mmd_path), "-o", str(out_path)]
    # Theme and background
    if theme:
        cmd += ["-t", theme]
    if background:
        cmd += ["-b", background]
    # Extra controls
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


def _options_digest(fmt: str, theme: str, background: str, config: Path | None, scale: float | None, width: int | None) -> str:
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


def process(
    md_in: Path,
    md_out: Path,
    assets_dir: Path,
    fmt: str,
    theme: str,
    background: str,
    config: Path | None,
    scale: float | None,
    width: int | None,
    force: bool = False,
    puppeteer_config: Path | None = None,
) -> tuple[bool, str]:
    # Ensure output directory exists before writing .mmd or images
    assets_dir.mkdir(parents=True, exist_ok=True)

    content = md_in.read_text(encoding="utf-8")
    lines = content.splitlines(keepends=True)

    out_lines = []
    i = 0
    # Match ```mermaid and optional attributes on the same line
    fence_start_re = re.compile(r"^```mermaid(?:\s.*)?$")
    fence_end_re = re.compile(r"^```\s*$")

    first_error: str | None = None

    while i < len(lines):
        line = lines[i]
        if fence_start_re.match(line):
            # Capture mermaid fence
            i += 1
            block_lines = []
            found_end = False
            while i < len(lines):
                if fence_end_re.match(lines[i]):
                    found_end = True
                    i += 1
                    break
                block_lines.append(lines[i])
                i += 1
            if not found_end:
                # Unterminated fence; write back as-is
                out_lines.append(line)
                out_lines.extend(block_lines)
                break

            mmd = "".join(block_lines)
            digest = sha1(mmd + "|" + _options_digest(fmt, theme, background, config, scale, width))
            mmd_file = assets_dir / f"{digest}.mmd"
            img_file = assets_dir / f"{digest}.{fmt}"
            # Write .mmd and render if missing
            if not mmd_file.exists() or force:
                mmd_file.write_text(mmd, encoding="utf-8")
            rendered = True
            stderr_text = ""
            if force or not img_file.exists():
                rendered, stderr_text = render_mmd(
                    mmd_file,
                    img_file,
                    fmt,
                    theme,
                    background,
                    config,
                    scale,
                    width,
                    puppeteer_config,
                )
            if rendered and img_file.exists():
                # Prefer a path relative to the output Markdown for web previews
                try:
                    rel = os.path.relpath(str(img_file), start=str(md_out.parent))
                    link = rel.replace('\\\\', '/')
                except Exception:
                    link = img_file.as_posix()
                out_lines.append(f"![Mermaid diagram]({link})\n")
            else:
                # Fallback: keep original fence if rendering failed
                out_lines.append("```mermaid\n")
                out_lines.append(mmd)
                out_lines.append("```\n")
                if first_error is None:
                    first_error = stderr_text or f"Rendering failed for digest {digest}"
        else:
            out_lines.append(line)
            i += 1

    md_out.write_text("".join(out_lines), encoding="utf-8")
    return (first_error is None), (first_error or "")


def main():
    p = argparse.ArgumentParser(description="Render Mermaid fences in Markdown to images")
    p.add_argument("input", type=Path, help="Input Markdown file")
    p.add_argument("-o", "--output", type=Path, help="Output Markdown file (default: <input>-images.md)")
    p.add_argument("-d", "--assets-dir", type=Path, default=Path("assets/mermaid"), help="Directory to write images (.mmd/.svg/.png)")
    p.add_argument("--format", choices=["svg", "png"], default="svg", help="Image format")
    p.add_argument("--theme", default="neutral", help="Mermaid theme (default, dark, forest, neutral, base)")
    p.add_argument("--background", default="transparent", help="Background color or 'transparent'")
    p.add_argument("--config", type=Path, help="Path to mermaid config JSON (themeVariables, etc.)")
    p.add_argument("--scale", type=float, help="Scale factor for rendering (e.g., 1.25)")
    p.add_argument("--width", type=int, help="Output width in pixels (SVG/PNG)")
    p.add_argument("-f", "--force", action="store_true", help="Force re-render even if output image exists")
    p.add_argument("--puppeteer-config", type=Path, help="Path to Puppeteer launch config JSON (e.g., no-sandbox)")
    args = p.parse_args()

    ensure_cmd("mmdc")

    # Normalize and validate config path early so mmdc doesn't fail with a vague message
    if args.config is not None:
        cfg_path = Path(args.config).expanduser().resolve()
        if not cfg_path.exists():
            print(f"Error: --config file not found: {cfg_path}", file=sys.stderr)
            sys.exit(2)
        args.config = cfg_path

    md_in = args.input
    md_out = args.output or md_in.with_name(md_in.stem + "-images.md")
    assets_dir = args.assets_dir
    fmt = args.format

    ok, stderr_text = process(
        md_in,
        md_out,
        assets_dir,
        fmt,
        args.theme,
        args.background,
        args.config,
        args.scale,
        args.width,
        args.force,
        args.puppeteer_config,
    )
    if ok:
        print(f"Rendered Mermaid images to: {assets_dir}")
        print(f"Wrote Markdown with image links: {md_out}")
    else:
        friendly = classify_mermaid_error(stderr_text or "")
        print(json.dumps(friendly, ensure_ascii=False), file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
