#!/usr/bin/env python
# pylint: disable=broad-exception-caught,duplicate-code
"""
Elisp Syntax Validator (generalized)
====================================

Purpose
-------
- Gate all Emacs Lisp writes/edits by ensuring parentheses and strings are
  balanced before the file is persisted.
- Works in two modes:
  1) Hook mode (stdin JSON) for IDE/agent hooks (Claude Code-compatible)
  2) CLI mode for general use by humans and agents (Codex CLI, Copilot, CI)

Validation tiers (in order)
---------------------------
- Emacs check-parens via batch (authoritative, fastest accurate check)
- Optional: lisp-paren-check diagnose when available (richer diagnostics)
- Fallback: fast scanner (comments/strings aware) if Emacs not available

Exit codes
----------
- 0: success (valid) or non-Elisp/no-op in hook mode
- 1: invalid elisp detected (including optional batch load/compile/lint failures)
- 2: invocation error (bad args / invalid JSON)
- 3: internal error

Examples
--------
- CLI validate file:            `python validate_elisp_syntax.py --files agent-cli.el`
- Read content from stdin:      `cat foo.el | python validate_elisp_syntax.py --stdin`
- Simulate single edit:         `python validate_elisp_syntax.py --simulate-edit --file path.el \
                                 --old "(defun x () )" --new "(defun x () (message \"hi\"))"`
- JSON for tools:               `--json`
- Prefer lisp-paren-check too:  `--with-lpc`

Claude Code hook integration (backwards compatible)
---------------------------------------------------
Put this command in a PreToolUse hook for Write/Edit/MultiEdit. If the edit
would produce invalid Elisp, the tool prints a JSON denial and exits 0 to
avoid crashing the orchestrator.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from typing import Iterable, Optional, Tuple, List, Dict, Any


# ------------------------------
# Low-level validators
# ------------------------------

def _emacs_check_parens(content: str, timeout: float = 5.0) -> Tuple[bool, Optional[str]]:
    """Validate content using Emacs' check-parens in batch mode.

    Returns (is_valid, error_message).
    """
    emacs = shutil.which("emacs") or shutil.which("emacsclient")
    if not emacs:
        return False, "Emacs not found"

    emacs_cmd = [
        emacs,
        "-batch",
        "--eval",
        """(progn
             (insert-file-contents \"/dev/stdin\")
             (emacs-lisp-mode)
             (goto-char (point-min))
             (condition-case err
                 (progn (check-parens) (message \"SYNTAX_OK\"))
               (error (message \"SYNTAX_ERROR: %s\" err) (kill-emacs 1))))""",
    ]

    try:
        result = subprocess.run(
            emacs_cmd,
            input=content,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return False, "Validation timeout"
    except Exception as e:  # noqa: BLE001
        return False, f"Validation error: {e}"

    if result.returncode == 0 and "SYNTAX_OK" in (result.stderr or ""):
        return True, None
    # Derive best-effort error
    error_msg = (result.stderr or result.stdout or "").strip()
    if "SYNTAX_ERROR:" in error_msg:
        error_msg = error_msg.split("SYNTAX_ERROR:", 1)[1].strip()
    return False, error_msg or "Unknown Emacs error"


# ------------------------------
# Emacs batch helpers (optional)
# ------------------------------

def _run_emacs(args: List[str], timeout: float = 60.0, stdin: Optional[str] = None) -> subprocess.CompletedProcess:
    exe = shutil.which("emacs") or shutil.which("emacsclient")
    if not exe:
        raise FileNotFoundError("Emacs not found in PATH")
    cmd = [exe, "-Q", "--batch"] + args
    return subprocess.run(cmd, input=stdin, capture_output=True, text=True, timeout=timeout, check=False)


def _emacs_batch_load(files: List[str], load_paths: List[str], eval_forms: Optional[List[str]] = None, timeout: float = 60.0) -> Tuple[bool, str]:
    args: List[str] = []
    for lp in load_paths:
        args += ["-L", lp]
    for f in files:
        args += ["-l", f]
    for ev in (eval_forms or ["(message \"OK\")"]):
        args += ["-eval", ev]
    proc = _run_emacs(args, timeout=timeout)
    ok = proc.returncode == 0
    out = (proc.stdout or "") + ("\n" + proc.stderr if proc.stderr else "")
    return ok, out.strip()


def _emacs_byte_compile(files: List[str], load_paths: List[str], warnings_as_errors: bool = False, timeout: float = 120.0) -> Tuple[bool, str]:
    args: List[str] = []
    for lp in load_paths:
        args += ["-L", lp]
    if warnings_as_errors:
        args += ["--eval", "(setq byte-compile-error-on-warn t)"]
    args += ["-f", "batch-byte-compile"] + files
    proc = _run_emacs(args, timeout=timeout)
    ok = proc.returncode == 0
    out = (proc.stdout or "") + ("\n" + proc.stderr if proc.stderr else "")
    return ok, out.strip()


def _emacs_package_lint(files: List[str], load_paths: List[str], timeout: float = 120.0) -> Tuple[bool, str]:
    args: List[str] = []
    for lp in load_paths:
        args += ["-L", lp]
    # Load package-lint and run the batch entry
    args += ["-l", "package-lint", "-f", "package-lint-batch-and-exit"] + files
    proc = _run_emacs(args, timeout=timeout)
    ok = proc.returncode == 0
    out = (proc.stdout or "") + ("\n" + proc.stderr if proc.stderr else "")
    return ok, out.strip()


def _emacs_checkdoc(files: List[str], load_paths: List[str], timeout: float = 120.0) -> Tuple[bool, str]:
    """Run checkdoc over the given files and capture *Warnings* output.

    Returns (ok, output). ok is True when no warnings were produced.
    """
    args: List[str] = []
    for lp in load_paths:
        args += ["-L", lp]
    # Build a single eval form that iterates files and prints collected warnings
    file_list_lisp = "(" + " ".join([f'\"{f}\"' for f in files]) + ")"
    eval_form = (
        "(progn "
        "(require 'checkdoc) "
        "(let ((files '" + file_list_lisp + ") (first t)) "
        "  (dolist (f files) "
        "    (let ((inhibit-message t)) "
        "      (when (get-buffer \"*Warnings*\") (with-current-buffer \"*Warnings*\" (erase-buffer))) "
        "      (checkdoc-file f) "
        "      (when (get-buffer \"*Warnings*\") "
        "        (with-current-buffer \"*Warnings*\" "
        "          (princ (buffer-string)))))))"  # print raw warnings
        ")"
    )
    args += ["--eval", eval_form]
    proc = _run_emacs(args, timeout=timeout)
    out = (proc.stdout or "") + ("\n" + proc.stderr if proc.stderr else "")
    # If any content was produced, treat as warnings present
    ok = (out.strip() == "")
    return ok, out.strip()


def _fast_paren_scanner(content: str) -> Tuple[bool, Optional[str]]:
    """Quick balance check aware of strings and comments.

    Not a full parser; used only when Emacs is unavailable.
    """
    depth = 0
    i = 0
    in_string = False
    escape = False
    in_comment = False
    line = 1
    col = 1
    last_open_line_col = []

    while i < len(content):
        ch = content[i]
        nxt = content[i + 1] if i + 1 < len(content) else ""

        if ch == "\n":
            line += 1
            col = 0
            in_comment = False

        if in_comment:
            i += 1
            col += 1
            continue

        if in_string:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == '"':
                in_string = False
            i += 1
            col += 1
            continue

        # Not in string/comment
        if ch == ";" and not in_string:
            in_comment = True
            i += 1
            col += 1
            continue

        if ch == '"':
            in_string = True
            i += 1
            col += 1
            continue

        if ch == "(":
            depth += 1
            last_open_line_col.append((line, col))
        elif ch == ")":
            if depth == 0:
                return False, f"Unmatched ')' at {line}:{col}"
            depth -= 1
            if last_open_line_col:
                last_open_line_col.pop()

        i += 1
        col += 1

    if in_string:
        return False, "Unterminated string literal"
    if depth != 0:
        if last_open_line_col:
            l, c = last_open_line_col[-1]
            return False, f"Unclosed '(' from {l}:{c}"
        return False, "Parentheses not balanced"
    return True, None


def _lpc_diagnose(file_path: str, timeout: float = 10.0) -> Tuple[Optional[bool], Optional[str]]:
    """Run lisp-paren-check diagnose if available.

    Returns (is_valid, diagnostics). is_valid may be None when tool is missing.
    """
    exe = shutil.which("lisp-paren-check")
    if not exe:
        return None, None

    try:
        r = subprocess.run(
            [exe, "diagnose", file_path],
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return None, "lisp-paren-check timeout"
    except Exception as e:  # noqa: BLE001
        return None, f"lisp-paren-check error: {e}"

    out = (r.stdout or "") + ("\n" + r.stderr if r.stderr else "")
    # Heuristic: exit 0 => OK, non-zero => issues
    return (r.returncode == 0), out.strip() or None


# ------------------------------
# Edit simulation helpers (for hook/CLI)
# ------------------------------

def _apply_edit(content: str, old_string: str, new_string: str, replace_all: bool = False) -> str:
    if old_string == "":  # File creation or full overwrite
        return new_string
    if replace_all:
        return content.replace(old_string, new_string)
    idx = content.find(old_string)
    if idx == -1:
        raise ValueError("Could not find old_string in content")
    return content[:idx] + new_string + content[idx + len(old_string) :]


def _apply_tool_edits(tool_name: str, tool_input: dict, current_content: str) -> str:
    if tool_name == "Write":
        return tool_input.get("content", "")
    if tool_name == "Edit":
        return _apply_edit(
            current_content,
            tool_input.get("old_string", ""),
            tool_input.get("new_string", ""),
            bool(tool_input.get("replace_all", False)),
        )
    if tool_name == "MultiEdit":
        res = current_content
        for edit in tool_input.get("edits", []) or []:
            res = _apply_edit(
                res,
                edit.get("old_string", ""),
                edit.get("new_string", ""),
                bool(edit.get("replace_all", False)),
            )
        return res
    raise ValueError(f"Unexpected tool name: {tool_name}")


# ------------------------------
# Public entry points
# ------------------------------

def validate_content(content: str, use_emacs: bool = True) -> Tuple[bool, Optional[str]]:
    """Validate content using Emacs when available; fallback to fast scan."""
    if use_emacs:
        ok, msg = _emacs_check_parens(content)
        if ok:
            return True, None
        # If Emacs missing, msg is informative; try fallback
        if msg and "Emacs not found" not in msg:
            return False, msg
    # Fallback
    return _fast_paren_scanner(content)


def hook_mode(stdin_json: dict) -> int:
    """Claude Code compatible hook mode.

    Returns a process exit code. Always 0 to avoid crashing orchestrators.
    """
    tool_name = stdin_json.get("tool_name", "")
    tool_input = stdin_json.get("tool_input", {})
    file_path = tool_input.get("file_path", "")

    if not file_path.endswith(".el"):
        return 0

    # Read the current file contents if present
    current_content = ""
    if os.path.exists(file_path):
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                current_content = f.read()
        except Exception:  # noqa: BLE001
            return 0

    try:
        resulting = _apply_tool_edits(tool_name, tool_input, current_content)
    except Exception:  # noqa: BLE001
        return 0

    is_valid, err = validate_content(resulting, use_emacs=True)
    if not is_valid:
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": (
                    "Blocked: This edit would create invalid Elisp syntax. "
                    f"Emacs/check-parens: {err}. "
                    "Please ensure parentheses and quotes are balanced. "
                    "Try again via Edit/MultiEdit/Write. Do NOT bypass using shell."
                ),
            }
        }
        print(json.dumps(output))
        return 0
    return 0


@dataclass
class CliResult:
    ok: bool
    message: Optional[str]
    lpc: Optional[str] = None


def cli_mode(args: argparse.Namespace) -> int:
    """CLI mode for general usage and CI."""
    contents: list[tuple[str, str]] = []  # (label, content)

    if args.stdin:
        contents.append(("<stdin>", sys.stdin.read()))

    for f in args.files or []:
        try:
            with open(f, "r", encoding="utf-8") as fh:
                contents.append((f, fh.read()))
        except OSError as e:
            print(f"ERROR: cannot read {f}: {e}", file=sys.stderr)
            return 2

    if args.simulate_edit:
        if not args.file:
            print("--simulate-edit requires --file", file=sys.stderr)
            return 2
        base = ""
        if os.path.exists(args.file):
            base = open(args.file, "r", encoding="utf-8").read()
        try:
            sim = _apply_edit(base, args.old or "", args.new or "", args.replace_all)
        except Exception as e:  # noqa: BLE001
            print(f"ERROR applying simulated edit: {e}", file=sys.stderr)
            return 2
        contents = [(args.file + " (simulated)", sim)]

    if not contents:
        print("Nothing to validate", file=sys.stderr)
        return 2

    overall_ok = True
    reports: list[CliResult] = []

    for label, content in contents:
        ok, msg = validate_content(content, use_emacs=not args.no_emacs)
        lpc_out = None
        if args.with_lpc:
            # write temp file then diagnose
            with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as tf:
                tf.write(content)
                tmp = tf.name
            try:
                lpc_ok, lpc_out = _lpc_diagnose(tmp)
            finally:
                try:
                    os.unlink(tmp)
                except OSError:
                    pass
            # If Emacs missing but lpc_ok is True, treat as OK
            if not ok and lpc_ok is True and (msg and "Emacs not found" in msg):
                ok, msg = True, None

        overall_ok = overall_ok and ok
        reports.append(CliResult(ok=ok, message=(msg or None), lpc=lpc_out))

        if args.gha_annotate and not ok:
            # GitHub Actions annotations (best effort)
            print(f"::error file={label},title=Elisp syntax::" + (msg or "invalid"))

    # Optional Emacs batch checks (load/compile/lint)
    emacs_checks: Dict[str, Dict[str, Any]] = {}
    if args.batch_load or args.byte_compile or args.package_lint or args.checkdoc:
        try:
            load_paths = args.emacs_L or ["."]
            files_list = [lbl for (lbl, _c) in contents if lbl != "<stdin>"]
            if args.batch_load and files_list:
                ok, out = _emacs_batch_load(files_list, load_paths, eval_forms=args.emacs_eval)
                overall_ok = overall_ok and ok
                emacs_checks["batch_load"] = {"ok": ok, "output": out}
            if args.byte_compile and files_list:
                ok, out = _emacs_byte_compile(files_list, load_paths, warnings_as_errors=args.warnings_as_errors)
                overall_ok = overall_ok and ok
                emacs_checks["byte_compile"] = {"ok": ok, "output": out}
            if args.package_lint and files_list:
                ok, out = _emacs_package_lint(files_list, load_paths)
                overall_ok = overall_ok and ok
                emacs_checks["package_lint"] = {"ok": ok, "output": out}
            if args.checkdoc and files_list:
                ok, out = _emacs_checkdoc(files_list, load_paths)
                # allow opt-in failure on any checkdoc output
                if args.checkdoc_fail:
                    overall_ok = overall_ok and ok
                emacs_checks["checkdoc"] = {"ok": ok, "output": out}
        except FileNotFoundError as e:
            overall_ok = False
            emacs_checks["error"] = {"ok": False, "output": str(e)}
        except subprocess.TimeoutExpired:
            overall_ok = False
            emacs_checks["error"] = {"ok": False, "output": "Emacs batch step timed out"}
        except Exception as e:  # noqa: BLE001
            overall_ok = False
            emacs_checks["error"] = {"ok": False, "output": f"Unexpected Emacs batch failure: {e}"}

    if args.json:
        print(
            json.dumps(
                {
                    "ok": overall_ok,
                    "results": [
                        {"label": l, "ok": r.ok, "message": r.message, "lpc": r.lpc}
                        for (l, _c), r in zip(contents, reports, strict=False)
                    ],
                    "emacs_checks": emacs_checks,
                }
            )
        )
    else:
        for (label, _c), r in zip(contents, reports, strict=False):
            status = "OK" if r.ok else "INVALID"
            print(f"[{status}] {label}")
            if r.message:
                print(f"  -> {r.message}")
            if r.lpc and args.with_lpc:
                print("  -- lisp-paren-check --\n" + r.lpc)
        # Print Emacs batch summaries
        for key, val in emacs_checks.items():
            ok = val.get("ok")
            head = f"[{'OK' if ok else 'INVALID'}] emacs:{key}"
            print(head)
            out = (val.get("output") or "").strip()
            if out:
                print(out)

    return 0 if overall_ok else 1


def parse_args(argv: Iterable[str]) -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Validate Emacs Lisp syntax")
    mode = p.add_mutually_exclusive_group()
    mode.add_argument("--hook", action="store_true", help="Read Claude hook JSON from stdin")
    mode.add_argument("--stdin", action="store_true", help="Read raw elisp from stdin")
    p.add_argument("--files", nargs="*", help="Elisp files to validate")
    p.add_argument("--simulate-edit", action="store_true", help="Simulate a single edit on --file")
    p.add_argument("--file", help="File path used with --simulate-edit")
    p.add_argument("--old", help="Old string for simulate-edit")
    p.add_argument("--new", help="New string for simulate-edit")
    p.add_argument("--replace-all", action="store_true", help="Replace all occurrences in simulate-edit")
    p.add_argument("--no-emacs", action="store_true", help="Disable Emacs batch; use fast scanner")
    p.add_argument("--with-lpc", action="store_true", help="Also run lisp-paren-check diagnose when available")
    p.add_argument("--json", action="store_true", help="Output JSON summary")
    p.add_argument("--gha-annotate", action="store_true", help="Emit GitHub Actions error annotations on failure")
    # Emacs batch extensions (optional)
    p.add_argument("--batch-load", action="store_true", help="Run Emacs batch load for --files (adds -l file for each)")
    p.add_argument("--byte-compile", action="store_true", help="Run Emacs batch byte-compile on --files")
    p.add_argument("--package-lint", action="store_true", help="Run package-lint batch on --files")
    p.add_argument("--warnings-as-errors", action="store_true", help="When byte-compiling, set byte-compile-error-on-warn=t")
    p.add_argument("--emacs-L", dest="emacs_L", action="append", help="Additional -L load-path entries (repeatable)")
    p.add_argument("--emacs-eval", dest="emacs_eval", action="append", help="Additional --eval forms for batch load (repeatable)")
    p.add_argument("--checkdoc", action="store_true", help="Run checkdoc on --files and capture *Warnings*")
    p.add_argument("--checkdoc-fail", action="store_true", help="Treat any checkdoc output as failure (overall)")
    return p.parse_args(list(argv))


def main(argv: Optional[Iterable[str]] = None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    # Auto-detect hook mode if stdin appears to be JSON with tool_name
    if not argv:
        try:
            # Peek stdin non-destructively by reading; if not JSON, fall back to CLI
            data = sys.stdin.read()
            if data.strip().startswith("{") and '"tool_name"' in data:
                try:
                    payload = json.loads(data)
                except Exception:
                    print("Error: Invalid JSON input", file=sys.stderr)
                    return 2
                return hook_mode(payload)
            # If stdin not JSON, treat as raw elisp content
            sys.stdin = open(0)  # reset stdin for argparse misuse safety
            args = parse_args(["--stdin"])
            return cli_mode(args)
        except Exception as e:  # noqa: BLE001
            print(f"Internal error: {e}", file=sys.stderr)
            return 3

    args = parse_args(argv)
    if args.hook:
        try:
            payload = json.load(sys.stdin)
        except Exception as e:  # noqa: BLE001
            print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
            return 2
        return hook_mode(payload)

    return cli_mode(args)


if __name__ == "__main__":
    sys.exit(main())
