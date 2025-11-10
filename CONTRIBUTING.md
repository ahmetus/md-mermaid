# Contributing to md-mermaid

Thanks for helping keep md-mermaid healthy. This document summarizes the guardrails from `AGENTS.md`, the architecture references, and the expected workflow for new patches.

## Before You Start

1. **Clone the repo** (or add it as a submodule inside your Emacs setup):
   ```bash
   git clone https://github.com/ahmetus/md-mermaid.git
   cd md-mermaid
   ```
2. **Install runtime dependencies** (Node.js + npm, Python 3, Mermaid CLI, Puppeteer/Chromium). The easiest path is to use the in-Emacs CLI menu (`M-x md-mermaid-transient` → `t` → `I`).
3. **Skim the docs**:
   - `PROJECT-ARCHITECTURE.md` outlines the data flow.
   - `AGENTS.md` describes mandatory validation steps.

## Workflow Checklist

1. **Create a snapshot** (local rollback point):
   ```bash
   timestamp=$(date +%Y%m%d-%H%M%S)
   mkdir -p snapshots/$timestamp
   cp -a md-mermaid.el md-mermaid-live.el md-mermaid-tools.el \
         README.md PROJECT-ARCHITECTURE.md AGENTS.md scripts/ \
         snapshots/$timestamp/
   ```
2. **Make your changes** (code, docs, scripts).
3. **Validate Emacs Lisp** for *every* touched `.el` file:
   ```bash
   python validate_elisp_syntax.py \
       --files md-mermaid.el md-mermaid-live.el md-mermaid-tools.el \
       --checkdoc --checkdoc-fail --package-lint \
       --emacs-L . \
       --emacs-eval "(progn (require 'package) (setq package-user-dir (expand-file-name \"~/.emacs.d/elpa\")) (package-initialize))"
   ```
   - For quicker iterations you can start with `python validate_elisp_syntax.py --files FILE.el`, but the full command above must pass before you open a PR.
4. **Run the required Emacs batch steps** if you added new dependencies:
   ```bash
   emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
         -f batch-byte-compile md-mermaid.el md-mermaid-live.el md-mermaid-tools.el
   ```
5. **Regenerate outlines** every time you touch the listed files:
   ```bash
   scripts/gen-outlines.sh
   ```
6. **Update documentation** whenever behavior changes:
   - `README.md` for user-facing features / screenshots.
   - `PROJECT-ARCHITECTURE.md` if data flows or components change.
7. **Keep commits focused**. Each commit should address one logical change (e.g., “feat(cli): add PATH fixer”).
8. **Include screenshots/GIFs** in the PR when they help reviewers verify UI tweaks (drop them in `examples/` and reference them in the README if they’re evergreen).

## Coding Guidelines

- Maintain lexical-binding friendliness; avoid introducing global state where hooks or buffer-local variables would suffice.
- Prefer small helper functions when transient entries or menus need extra work—`md-mermaid--transient-toggle-live-mode` is a good pattern to follow.
- Keep PNG/SVG outputs deterministic; when working on render scripts, run the sample Markdown files under `examples/` and verify assets update.
- Respect the CLI abstraction (`md-mermaid-tools.el`) when adding installer logic; all shell + npm commands should originate there so logs and notifications stay consistent.

## Reporting / Reviewing

When opening a PR:
1. Describe the change (what, why, how to test).
2. Attach validator output (or note “validator run: PASS”).
3. Add screenshots if UI changed.
4. Mention any documentation updates included (README, architecture, etc.).

Thanks for contributing—keeping these guardrails in place lets everyone ship improvements without breaking the rendering experience. For questions, ping the maintainers via GitHub issues.***
