## CRITICAL INSTRUCTION: Lisp Evaluation & Diagnostics (Do Not Skip)

These rules are mandatory whenever touching Emacs Lisp. Follow this exact order to avoid shipping broken code and to keep Org/Markdown flows stable.

1)  - You can use python with your own created script for counting and arranging parens.

- Before writing or modifying any Emacs Lisp files, always run the syntax validator:
  - `python validate_elisp_syntax.py --files FILE.el`
- Block the change if the tool reports issues. Do not attempt to bypass the gate with shell edits.
- For multi-file edits, validate every touched `.el` file. For simulated changes, use `--simulate-edit` to test the result before applying.

Tools and usage
- `validate_elisp_syntax.py` (generalized; CLI + hook)
  - Validate files: `python validate_elisp_syntax.py --files file1.el file2.el`
  - Validate stdin: `cat file.el | python validate_elisp_syntax.py --stdin`
  - Simulate edit: `python validate_elisp_syntax.py --simulate-edit --file file.el --old OLD --new NEW [--replace-all]`
  - JSON output for agents: add `--json`.
  - In Claude/Copilot/Codex hooks, use `--hook` or default stdin JSON auto-detect. The tool denies malformed edits and exits 0 to avoid crashing orchestrators.

Gate philosophy and order
- Fast loop: `validate_elisp_syntax.py` → batch load (see “CRITICAL INSTRUCTION: Lisp Evaluation & Diagnostics”).
- Treat failures as hard stops; fix locally, then re-run the steps before proceeding.
- This rule applies to all agents (Claude, Copilot, Codex) and to manual edits.

### Batch Lint/Compile Workflow (Emacs 30.x, package-lint)

Use these exact commands so CI and local runs behave the same and don’t fail due to missing `load-path` or ignored warnings.

- Add project to `load-path` for batch runs:
  - `-L .` or set `EMACSLOADPATH`.
- Elevate warnings to errors during byte-compile to catch issues early:
  - `--eval '(setq byte-compile-error-on-warn t)'`

Recipes:

1) Batch load (smoke):
   - `emacs -Q --batch -L . -l md-mermaid-tools.el -l md-mermaid.el -l md-mermaid-live.el -eval '(message "OK")'`

2) Byte-compile with warnings as errors:
   - `emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile md-mermaid.el md-mermaid-live.el md-mermaid-tools.el`
   - If compiling locally without MELPA providing deps, add: `--eval '(require \''transient)'`

3) package-lint:
   - `emacs -Q --batch -L . -l package-lint -f package-lint-batch-and-exit md-mermaid.el md-mermaid-live.el md-mermaid-tools.el`

Notes:
- `md-mermaid.el` depends on `transient` for menus (hard dep). Ensure it is available in CI (MELPA builders will handle it via `Package-Requires`).
- Internal cross-file deps are soft-required at top-level to keep `-Q --batch` executions working without custom `load-path`; commands do hard `require` just-in-time.

### validate_elisp_syntax.py (extended Emacs checks)

The validator supports optional Emacs batch steps beyond check-parens. Recommended invocations:

- Syntax + Emacs batch load:
  - `python validate_elisp_syntax.py --files md-mermaid.el md-mermaid-live.el md-mermaid-tools.el --batch-load --emacs-L . --json`

- Syntax + byte-compile (warnings-as-errors):
  - `python validate_elisp_syntax.py --files md-mermaid.el md-mermaid-live.el md-mermaid-tools.el --byte-compile --warnings-as-errors --emacs-L . --json`
  - For local runs without MELPA, you may add: `--emacs-eval '(require \''transient)'`

- Syntax + package-lint:
-  - `python validate_elisp_syntax.py --files md-mermaid.el md-mermaid-live.el md-mermaid-tools.el --package-lint --emacs-L . --json`
  - Use the same command line as the package-lint invocation during builds, and make sure package-lint runs immediately after --checkdoc so both validations execute together.

- Validator run policy:
  - Always invoke validate_elisp_syntax.py with `--checkdoc --checkdoc-fail` and `--package-lint` when the tool is used
  - After the tool finishes, clean all `.elc` files generated during the run with `find . -name '*.elc' -delete` so artifacts don’t linger in the workspace

- Syntax + checkdoc (fail on any output):
  - `python validate_elisp_syntax.py --files md-mermaid-live.el --checkdoc --checkdoc-fail --emacs-L . --json`

Flags:
- `--emacs-L PATH`  Add `-L PATH` (repeatable)
- `--emacs-eval FORM`  Extra `--eval FORM` for batch-load (repeatable)
- `--warnings-as-errors`  Elevate compiler warnings to errors


2) Batch evaluation (no UI, full stderr)
   - Always batch‑load the edited set in a clean emacs:
     - `emacs -Q --batch -l md-mermaid.el -l md-mermaid-live.el -l mermaid-cli-tools.el -eval '(message "OK")'`
   - Treat any stderr output as a blocker. Record full stdout/stderr in the chat so it is visible and auditable.

3) Escaping rules (common runtime traps)
   - Docstrings/text with quotes: escape inner quotes using backslash, not backticks.
     - Example: `"Org uses \"* User\"; Markdown uses \"## User\"."`
   - Regex inside strings must double‑escape backslashes:
     - Example: `"^\\* User"`, `"^## User"`
   - Do NOT rely on backticks to escape in Lisp; they are literal characters, not escapes.

4) When apply_patch cannot match (escaping issues)
   - If a patch fails due to escaping (e.g., `\n`, `\*`, quotes), use a tiny scripted helper locally to replace safely:
     - Python example:
       ```bash
       python - <<'PY'
       import re
       p='md-mermaid.el'
       s=open(p).read()
       s=s.replace('(re-search-backward "^\\* User"','(re-search-backward "^\\\\* User"')
       open(p,'w').write(s)
       PY
       ```
   - Re‑run paren check + batch eval immediately after any scripted change.

- 6) Runtime guards and one-shots (last resort)
   - If an ensure/insert path is re-entered at runtime, use a buffer-local guard flag around the dispatcher to prevent recursion.
   - Keep such guards minimal and scoped to the specific first-turn path; do not alter formatting semantics.

## Outline files (auto-generate; CRITICAL)
- This step is mandatory and non-interactive. Immediately after you modify any of the files below, regenerate the outlines so references remain accurate—even if you are still awaiting approval to commit. When a commit is eventually made, include the refreshed outline artifacts.
- `md-mermaid.el`
- `md-mermaid-live.el`
- `md-mermaid-tools.el`
- `scripts/md_mermaid_render.py`
- `scripts/md_mermaid_snippet.py`
- Use the batch script:
  - `scripts/gen-outlines.sh`
    - Generates Emacs imenu outlines for Lisp and Python files.
    - Generates Python outlines via Tree‑sitter when available; falls back to stdlib AST otherwise.
    - If Tree‑sitter and AST both run, prefer the Tree‑sitter `*-pyts-outline.md` results.
- Resulting files (Markdown):
  - `md-mermaid-outline.md`, `md-mermaid-live-outline.md`, `md-mermaid-tools-outline.md`
  - `md_mermaid_render-outline.md`, `md_mermaid_snippet-outline.md`
  - Python details: `*-pyts-outline.md` (Tree‑sitter) or `*-py-outline.md` (AST fallback)
- This is a CRITICAL INSTRUCTION: do not skip, do not leave to the user.

## Outline‑First Navigation (CRITICAL)
- Before opening or scanning any source file, first consult its outline. Treat the outline as the authoritative index of symbols and line numbers.
- How to locate the outline for a file:
  - Emacs Lisp: `<stem>-outline.md` in the repo root (e.g., `md-mermaid.el` → `md-mermaid-outline.md`).
  - Python: prefer Tree‑sitter outline `<stem>-pyts-outline.md`; if not present, use `<stem>-py-outline.md`.
- If the expected outline is missing or stale, you MUST run `scripts/gen-outlines.sh` to (re)generate before proceeding.
- Use the outline to jump directly to the requested symbol and line(s). Do not full‑scan large files when the outline provides the location.
- When modifying code, update the outline afterward (see section above) and review the new outline to verify symbol positions changed as expected.

## CRITICAL INSTRUCTION: Codebase Grounding and Verification
**Rule:** Never assume the existence, signature, or definition of any function, variable, or code construct. Before using an existing code element in any new implementation, you must first verify its exact details (e.g., name, parameters, return type) by reading the relevant outline files directly then if necessary the source file. Do not guess or infer implementation details; **always confirm by checking the outline then code.**

## Context & References
- `PROJECT-ARCHITECTURE.md` captures the full component diagram + data flow—consult it when you need a refresher on how Emacs, Python, and CLI layers interact.
- When you add new diagnostics, limits, or workflows, cross-reference all four documents so the next agent can trace decisions end-to-end.
- Whenever you introduce or rely on a runtime toggle (e.g., a defcustom that we expect to flip during testing), also provide an interactive helper function/command so testers can toggle it on the fly without editing Lisp variables manually. Document the helper in `AGENTS.md` if it is new.

## Commit discipline (CRITICAL)
- After implementing changes, request evaluation first and defer commits until the current agent confirms the work passes checks or the user explicitly approves. This keeps room for rollback or adjustments before history is updated. Because latest commit contains solid working version, User prefers snapshot approach for iterations until they are polished enough. Next instruction gives necessary details about that.

## CRITICAL INSTRUCTION: Session Snapshot at Start
At this stage of the project; Agent MUST NOT run any Git commands UNTIL User asks.
Before making any changes in a new working session (first action after opening the repo), create a local rollback snapshot using file copies only.
- Create a timestamped snapshot directory:
  - `mkdir -p snapshots/$(date +%Y%m%d-%H%M%S)`
- Copy relevant files (Elisp + integration files):
  - `cp -a md-mermaid.el md-mermaid-live.el md-mermaid-tools.el md-mermaid-tools-outline.md md-mermaid-live-outline.md md-mermaid-outline.md README.md AGENTS.md  scripts/ snapshots/$(date +%Y%m%d-%H%M%S)/ 2>/dev/null || true`
 (and any file you intend to touch).
- Always show the full Stdout/Stderr of the snapshot commands in the chat for auditability.
- When reverting is requested during the same session:
  - Restore from the matching `snapshots/<timestamp>/` directory via `cp -a`.
Rationale: This guarantees a quick, local rollback point for bug‑hunting iterations without invoking Git from the agent.
