"""
Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Author:  Ahmet Usal <ahmetusal@gmail.com>
Collaborators: OpenAI Assistant, Claude Assistant
Version : 1.0
Friendly Mermaid error classifier used by both batch and live renderers.
"""

from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple

# Patterns used to detect line numbers in stderr.
LINE_PATTERNS = [
    re.compile(r"(?:Parse|Lexical|Syntax) error.*line\s+(\d+)", re.IGNORECASE),
    re.compile(r"[Ll]ine\s+(\d+)[^\d]"),
    re.compile(r"at\s+line\s+(\d+)", re.IGNORECASE),
]


def _find_line(stderr: str) -> Optional[int]:
    for rp in LINE_PATTERNS:
        match = rp.search(stderr)
        if match:
            try:
                return int(match.group(1))
            except Exception:
                continue
    return None


# Rule tuples: (tag, regex, title, explanation, [suggestions...])
RULES: List[Tuple[str, re.Pattern[str], str, str, List[str]]] = [
    (
        "parse-error",
        re.compile(r"(Parse|Syntax) error", re.IGNORECASE),
        "Syntax error in Mermaid diagram",
        "Mermaid couldn't parse the snippet around this location.",
        [
            "Ensure the first non-empty line is a valid header (e.g., `flowchart TD`).",
            "Check for unbalanced brackets `()[]{}` and correct arrow forms `-->`/`---`.",
            "Avoid reserved ids like `end` unless quoted inside text.",
        ],
    ),
    (
        "lex-error",
        re.compile(r"Lexical error", re.IGNORECASE),
        "Lexical error in Mermaid diagram",
        "Invalid token or character sequence encountered.",
        [
            "Look for stray backticks, tabs, or non-ASCII punctuation.",
            "Simplify the line to isolate the unexpected token.",
        ],
    ),
    (
        "unknown-type",
        re.compile(r"Unknown diagram type|No diagram type detected|expected (diagram|graph) type", re.IGNORECASE),
        "Unknown or missing diagram type",
        "The first line must declare the diagram type.",
        [
            "Start with `flowchart TD`, `sequenceDiagram`, `classDiagram`, `erDiagram`, `stateDiagram-v2`, `gantt`, `journey`, or `pie`.",
            "Do not leave blank lines before the header.",
        ],
    ),
    (
        "unclosed-structure",
        re.compile(r"unexpected end of input|unclosed|unterminated", re.IGNORECASE),
        "Unclosed structure",
        "Mermaid reached the end while still expecting a closing token.",
        [
            "Close every open `(` `[` `{` and each `subgraph ... end` block.",
            "Check `class { ... }`, `state`, and `subgraph` sections for closures.",
        ],
    ),
    (
        "unexpected-token",
        re.compile(r"Unexpected (token|string|identifier)", re.IGNORECASE),
        "Unexpected token",
        "A token does not fit the expected grammar at this position.",
        [
            "Check for stray `:`/`;` or wrong arrow syntax (use `-->`, `---`, `-.->`).",
            "Quote labels that might be parsed as identifiers.",
        ],
    ),
    (
        "flowchart-dir",
        re.compile(r"flowchart.*must specify.*(TD|TB|LR|RL|BT)|missing.*direction", re.IGNORECASE),
        "Flowchart direction missing or invalid",
        "Flowcharts need a direction immediately after the keyword.",
        ["Use `flowchart TD` (top-down) or `flowchart LR` (left-right), etc."],
    ),
    (
        "subgraph-not-closed",
        re.compile(r"subgraph\b.*(not closed|missing end)", re.IGNORECASE),
        "Subgraph not closed",
        "A `subgraph` block is missing its `end`.",
        [
            "Add `end` that matches the `subgraph` you opened.",
            "Avoid using `end` as an ID; quote it inside labels if needed.",
        ],
    ),
    (
        "link-style-invalid",
        re.compile(r"(linkStyle|style)\s+\d+.*(invalid|unknown)", re.IGNORECASE),
        "Invalid link/style directive",
        "A style directive references an unknown index or property.",
        ["Ensure the edge index exists and CSS-like attributes are valid."],
    ),
    (
        "sequence-actor",
        re.compile(r"(participant|actor).*(already defined|not defined)", re.IGNORECASE),
        "Sequence participant issue",
        "Participants must be defined once and referenced consistently.",
        ["Define participants with `participant Alice` before using them."],
    ),
    (
        "sequence-syntax",
        re.compile(r"(message|activation|deactivation).*(invalid|unknown)", re.IGNORECASE),
        "Sequence diagram syntax issue",
        "A message/activation line is malformed.",
        ["Use `Alice->>Bob: text` or `Note over Alice: text` syntax."],
    ),
    (
        "class-syntax",
        re.compile(r"classDiagram.*(malformed|invalid|unexpected)", re.IGNORECASE),
        "Class diagram syntax issue",
        "A class/member line does not match the expected pattern.",
        ["Use `Class : +method(args)` or `Class : field` style entries."],
    ),
    (
        "er-syntax",
        re.compile(r"erDiagram.*(malformed|invalid|unexpected)", re.IGNORECASE),
        "ER diagram syntax issue",
        "Relationship or entity line seems malformed.",
        ["Use `A ||--o{ B : has` style relationships and valid cardinalities."],
    ),
    (
        "state-syntax",
        re.compile(r"stateDiagram.*(malformed|invalid|unexpected)", re.IGNORECASE),
        "State diagram syntax issue",
        "A transition or state block is malformed.",
        ["Use `[*] --> State` for start and close composite states properly."],
    ),
    (
        "gantt-date",
        re.compile(r"gantt.*(date|time).*(invalid|parse|format)", re.IGNORECASE),
        "Gantt date/time parse error",
        "A date/time could not be parsed.",
        [
            "Use ISO-like dates (`YYYY-MM-DD`).",
            "Ensure `dateFormat` matches the dates you specify.",
        ],
    ),
    (
        "pie-value",
        re.compile(r"pie.*(value|number).*(invalid|NaN)", re.IGNORECASE),
        "Pie value must be numeric",
        "All pie slice values must be numbers.",
        ["Use `pie\\n  \"Cats\" : 40` (number, not string) for counts."],
    ),
    (
        "include-missing",
        re.compile(r"(include|init).*file.*(not found|ENOENT)", re.IGNORECASE),
        "Included file not found",
        "A referenced file could not be located.",
        [
            "Check relative paths vs the working directory used by the renderer.",
            "Avoid leading `~`; prefer absolute or project-relative paths.",
        ],
    ),
    (
        "config-json",
        re.compile(r"(init|config).*JSON.*(parse|invalid)", re.IGNORECASE),
        "Init/config JSON parse error",
        "Mermaid initialization/config JSON is invalid.",
        [
            "Validate the JSON; remove trailing commas.",
            "Keep keys quoted and booleans lowercase (`true`/`false`).",
        ],
    ),
    (
        "theme-not-found",
        re.compile(r"(theme|themeVariables).*(unknown|not found)", re.IGNORECASE),
        "Theme not found or invalid variables",
        "Theme name or variable reference is invalid.",
        [
            "Use built-ins like `default`, `forest`, `dark`, `neutral` or supply a valid custom theme file.",
        ],
    ),
    (
        "font-missing",
        re.compile(r"(font|fontFamily).*(not found|missing)", re.IGNORECASE),
        "Font not found",
        "The specified font is unavailable to the renderer.",
        [
            "Install the font on the system or choose a web-safe fallback.",
        ],
    ),
    (
        "puppeteer-missing",
        re.compile(r"(Chromium|Chrome|Puppeteer).*(not found|executable)", re.IGNORECASE),
        "Chromium/Chrome not found",
        "mermaid-cli (Puppeteer) needs a Chromium/Chrome executable.",
        [
            "Install Chromium/Chrome or set PUPPETEER_EXECUTABLE_PATH to its binary.",
            "Optionally pass `-p puppeteer-config.json` for launch args.",
        ],
    ),
    (
        "sandbox-failure",
        re.compile(r"(sandbox|no-sandbox).*not allowed|seccomp|namespace", re.IGNORECASE),
        "Chromium sandbox issue",
        "Headless browser failed due to sandbox restrictions.",
        ["Try puppeteer args: `--no-sandbox --disable-setuid-sandbox` (CI/containers)."],
    ),
    (
        "timeout",
        re.compile(r"Timeout|timed out|navigation timeout", re.IGNORECASE),
        "Renderer timed out",
        "The headless browser exceeded the allowed time.",
        [
            "Simplify or split the diagram.",
            "Run on a machine with more memory/CPU; then retry.",
        ],
    ),
    (
        "disk-full",
        re.compile(r"ENOSPC|No space left on device", re.IGNORECASE),
        "No disk space (ENOSPC)",
        "Temporary or output storage is full.",
        [
            "Free space (especially `/tmp`) or set TMPDIR to a larger location.",
        ],
    ),
    (
        "permission",
        re.compile(r"EPERM|EACCES|permission denied", re.IGNORECASE),
        "Permission denied",
        "The renderer could not access a file/path.",
        [
            "Ensure output directories are writable and include paths readable.",
        ],
    ),
    (
        "file-io",
        re.compile(r"(EIO|I/O error|read error|write error)", re.IGNORECASE),
        "I/O error",
        "A filesystem operation failed.",
        [
            "Check filesystem health or write to another path.",
        ],
    ),
    (
        "version-mismatch",
        re.compile(r"(requires Mermaid|incompatible version|unknown directive)", re.IGNORECASE),
        "Mermaid version mismatch",
        "The diagram uses features not supported by the installed Mermaid version.",
        [
            "Update `mermaid` and `@mermaid-js/mermaid-cli` to recent versions.",
            "Avoid experimental directives unless supported.",
        ],
    ),
]


def classify_mermaid_error(stderr: str) -> Dict[str, Any]:
    """Return a structured description with tags, issues, and summary."""
    text = (stderr or "").strip()
    issues: List[Dict[str, Any]] = []
    tags: List[str] = []
    line = _find_line(text)

    for tag, regex, title, explanation, suggestions in RULES:
        if regex.search(text):
            issue: Dict[str, Any] = {
                "tag": tag,
                "title": title,
                "explanation": explanation,
                "suggestions": suggestions,
            }
            if line is not None and tag in {
                "parse-error",
                "lex-error",
                "unexpected-token",
                "unclosed-structure",
                "sequence-syntax",
                "class-syntax",
                "er-syntax",
                "state-syntax",
                "gantt-date",
            }:
                issue["where"] = f"line {line}"
            issues.append(issue)
            tags.append(tag)

    if not issues:
        issues.append(
            {
                "tag": "generic-failure",
                "title": "Rendering failed",
                "explanation": "Mermaid or the renderer reported an error.",
                "suggestions": [
                    "Confirm the header line and basic syntax.",
                    "Try rendering just this snippet with `mmdc`.",
                    "Update Mermaid and mermaid-cli to compatible versions.",
                ],
            }
        )
        summary = "Rendering failed"
    else:
        summary = issues[0]["title"]

    return {"ok": False, "summary": summary, "issues": issues, "raw": text, "tags": tags}
