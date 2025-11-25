# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

md-mermaid is an Emacs package that renders Mermaid diagrams within Markdown files. It provides:
- Batch rendering (entire files → sibling Markdown + assets)
- Live preview mode (real-time overlays in Emacs buffers)
- CLI dependency management (installer/updater for mmdc, Puppeteer, Chromium)
- Customizable transient UI with keybinding management

**Tech Stack**: Emacs Lisp (core), Python 3 (rendering scripts), Node.js (Mermaid CLI), Chromium/Puppeteer (PNG generation)

**Core Files**:
- `md-mermaid.el` - main package, batch rendering, transient menu
- `md-mermaid-live.el` - live preview mode with overlays
- `md-mermaid-tools.el` - CLI dependency installer/manager
- `scripts/md-mermaid.sh` - shell wrapper with presets
- `scripts/md_mermaid_render.py` - batch processor
- `scripts/md_mermaid_snippet.py` - snippet renderer for live mode
- `scripts/friendly_errors.py` - error classification

## Development Commands

### Validation (MANDATORY before any commit)
```bash
# Syntax validation for Emacs Lisp (run for every touched .el file)
python validate_elisp_syntax.py \
    --files md-mermaid.el md-mermaid-live.el md-mermaid-tools.el \
    --checkdoc --checkdoc-fail --package-lint \
    --emacs-L . \
    --emacs-eval "(progn (require 'package) (setq package-user-dir (expand-file-name \"~/.emacs.d/elpa\")) (package-initialize))"

# Clean artifacts after validation
find . -name '*.elc' -delete
```

### Testing
```bash
# Batch load test (smoke test for runtime errors)
emacs -Q --batch -L . \
    -l md-mermaid-tools.el \
    -l md-mermaid.el \
    -l md-mermaid-live.el \
    -eval '(message "OK")'

# Byte-compile with warnings-as-errors
emacs -Q --batch -L . \
    --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile \
    md-mermaid.el md-mermaid-live.el md-mermaid-tools.el
```

### Outline Regeneration (MANDATORY after modifying core files)
Run after touching: `md-mermaid.el`, `md-mermaid-live.el`, `md-mermaid-tools.el`, `scripts/md_mermaid_render.py`, `scripts/md_mermaid_snippet.py`

```bash
scripts/gen-outlines.sh
```

This creates/updates: `md-mermaid-outline.md`, `md-mermaid-live-outline.md`, `md-mermaid-tools-outline.md`, `md_mermaid_render-outline.md`, `md_mermaid_snippet-outline.md`

### Command-Line Rendering
```bash
# PNG rendering (1400px width, force re-render)
bash scripts/md-mermaid.sh -i examples/mermaid-quick-test.md -png1400 -f

# SVG rendering (ideal for browsers/git hosting)
bash scripts/md-mermaid.sh -i examples/mermaid-quick-test.md -svg -f

# Custom width PNG
bash scripts/md-mermaid.sh -i FILE.md -pngW 2000 -f
```

Common flags:
- `-i FILE` - input Markdown file
- `-o FILE` - override output path (optional)
- `-d DIR` - assets directory (default: `assets/mermaid/`)
- `-svg` / `-png1280` / `-png1400` / `-png1800` / `-pngW WIDTH`
- `-f` - force re-render (ignore cache)

## Architecture Essentials

### Data Flow: Batch Rendering
1. User runs `md-mermaid-render-current` (or `M-x md-mermaid-render-current`)
2. `md-mermaid.el` invokes `scripts/md-mermaid.sh` with preset flags
3. Shell wrapper calls `md_mermaid_render.py`
4. Python script:
   - Parses Markdown for ````mermaid` fences
   - Generates SHA1 signature for each block (caching key)
   - Checks `assets/mermaid/` for cached images
   - Calls `mmdc` (Mermaid CLI) for missing diagrams
   - Writes sibling Markdown (`*-images.md` or `*-emacs.md`) with image references
5. Assets land under `assets/mermaid/`

### Data Flow: Live Preview
1. User toggles `md-mermaid-live-mode`
2. `md-mermaid-live.el` watches scroll/edit events
3. Queues render jobs for visible `mermaid` fences
4. Calls `md_mermaid_snippet.py` per fence
5. Creates Emacs overlays displaying cached/fresh PNGs
6. Manages overlay visibility, scroll stabilizers, and cache

### CLI Tools Management
- `md-mermaid-tools.el` provides transient menu (`t` in main menu)
- Installs/updates/checks: `mmdc`, Puppeteer, Chromium, npm itself
- Spawns async processes, streams logs to `*md-mermaid-cli*` buffers
- Supports notification (Emacs + OS) for long-running installs
- Detects preferred npm client: npm/pnpm/yarn/bun
- Auto-fixes PATH when tools are installed but not visible

## Critical Workflow Rules

### ALWAYS Create Snapshot Before Changes
At the start of ANY work session:
```bash
timestamp=$(date +%Y%m%d-%H%M%S)
mkdir -p snapshots/$timestamp
cp -a md-mermaid.el md-mermaid-live.el md-mermaid-tools.el \
      README.md AGENTS.md PROJECT-ARCHITECTURE.md scripts/ \
      snapshots/$timestamp/
```

**Do NOT run git commands until user explicitly requests**. Snapshots provide rollback points during iteration.

### Outline-First Navigation
- NEVER open/scan a source file without first reading its outline
- Outlines are the authoritative index for symbol locations
- If outline is missing/stale, run `scripts/gen-outlines.sh` first
- For Python: prefer `*-pyts-outline.md` (Tree-sitter), fallback to `*-py-outline.md` (AST)

### Validation Gate
- Run `validate_elisp_syntax.py` for EVERY touched `.el` file before proceeding
- Treat failures as hard stops; fix locally, then re-run
- Always use `--checkdoc --checkdoc-fail --package-lint` flags
- Clean `.elc` artifacts after validation: `find . -name '*.elc' -delete`

### Elisp Escaping Rules (Common Traps)
- Docstrings with quotes: `"Org uses \"* User\"; Markdown uses \"## User\"."`
- Regex in strings: double-escape backslashes → `"^\\* User"`
- NO backticks for escaping in Lisp (they are literal characters)

### Dependencies
- `md-mermaid.el` requires `transient` (hard dep, declared in `Package-Requires`)
- Cross-file deps use soft `require` at top-level + hard `require` just-in-time
- For batch tests without MELPA: add `--eval '(require 'transient)'` to load-path

## Project-Specific Patterns

### Transient Menu Toggles
Live mode toggle example (shows ON/OFF state inline):
```elisp
(transient-define-suffix md-mermaid--transient-toggle-live-mode ()
  :description
  (lambda ()
    (concat "Toggle live overlays"
            (if md-mermaid-live-mode " [ON]" " [OFF]")))
  :key "l"
  (interactive)
  (call-interactively #'md-mermaid-live-mode)
  (transient-setup 'md-mermaid--transient-menu))
```

### Async Command Pattern (CLI Tools)
All CLI installer actions use:
```elisp
(md-mermaid-cli--run-async-command
  COMMAND
  :buffer-name "*md-mermaid-cli-install*"
  :on-success (lambda () (message "Install succeeded"))
  :on-error (lambda () (message "Install failed")))
```

### Asset Caching
- All diagrams cached under `assets/mermaid/` (configurable via `md-mermaid-assets-dir`)
- Cache key: SHA1 of Mermaid source
- Force re-render: pass `-f` flag or `C-u` prefix in Emacs commands

## Configuration Hooks

Users can set these in `init.el`:
```elisp
(setq md-mermaid-default-preset 'png1400)         ; svg / png1280 / png1400 / png1800
(setq md-mermaid-assets-dir "assets/mermaid")     ; where to store diagrams
(setq md-mermaid-open-browser 'grip)              ; http / grip / browse-url
(setq md-mermaid-cli-preferred-npm "npm")         ; or "pnpm" / "yarn" / "bun"
(setq md-mermaid-notify-method 'both)             ; 'emacs / 'os / 'both
(setq md-mermaid-install-global-keybindings t)    ; auto-bind C-c m and C-c M
```

## Common Pitfalls

1. **Forgetting outlines**: If you modify `.el` or `.py` files, regenerate outlines immediately
2. **Skipping validation**: Never commit without running `validate_elisp_syntax.py` with full flags
3. **Git commands**: Do NOT run git unless user explicitly asks; use snapshots instead
4. **Escaping in Lisp**: Backticks do NOT escape; use `\"` for quotes, `\\` for backslashes in regex
5. **PATH issues**: If `mmdc` not found, use CLI Tools menu (`t` → `I`) or manually set PATH
6. **Chromium missing**: Puppeteer needs Chromium/Chrome; installer handles this, or user installs system package
7. **Stale cache**: Use `-f` flag (CLI) or `C-u` prefix (Emacs) to force re-render

## Documentation References

- `README.md` - user guide, installation, examples
- `PROJECT-ARCHITECTURE.md` - component diagrams, data flows, ERDs
- `AGENTS.md` - validation rules, batch workflow, commit discipline
- `CONTRIBUTING.md` - PR checklist, coding guidelines, snapshot workflow

## Testing Checklist

Before opening a PR:
1. Create snapshot of current state
2. Make changes
3. Run `validate_elisp_syntax.py` with full flags
4. Run batch load test
5. Regenerate outlines with `scripts/gen-outlines.sh`
6. Test on example files: `examples/mermaid-quick-test.md`, `examples/parallel-merge-sort.md`
7. Update documentation (README, PROJECT-ARCHITECTURE) if behavior changed
8. Clean `.elc` artifacts
9. Attach validator output and screenshots to PR

## Performance Notes

- Live mode works best with 1-4 small/medium diagrams
- Large diagrams (exceeding viewport height) can degrade performance
- Sequential processing: 6+ diagrams may show rendering delays
- Cache helps: repeated renders of unchanged content are fast
- Throttling/debouncing: live mode debounces refresh to reduce CPU load
