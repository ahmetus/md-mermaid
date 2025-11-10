#!/usr/bin/env bash
set -euo pipefail

# Generate outline files for key sources using Emacs batch + imenu.

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)

emacs -Q --batch -l "$repo_root/scripts/outline-export.el" \
  --eval "(outline-export-batch :format \"markdown\" :outdir \"$repo_root\" :files (list \"$repo_root/md-mermaid.el\" \"$repo_root/md-mermaid-live.el\" \"$repo_root/md-mermaid-tools.el\" \"$repo_root/scripts/md_mermaid_render.py\" \"$repo_root/scripts/md_mermaid_snippet.py\"))"

# Prefer tree-sitter based Python outline; fallback to AST if unavailable
if python3 - <<'PY'
try:
    import tree_sitter
    ok=1
except Exception:
    ok=0
import sys; sys.exit(0 if ok else 1)
PY
then
  python3 "$repo_root/scripts/py_tree_sitter_outline.py" -o "$repo_root" \
    "$repo_root/scripts/md_mermaid_render.py" "$repo_root/scripts/md_mermaid_snippet.py" >/dev/null || true
else
  python3 "$repo_root/scripts/py_ast_outline.py" -o "$repo_root" \
    "$repo_root/scripts/md_mermaid_render.py" "$repo_root/scripts/md_mermaid_snippet.py" >/dev/null || true
fi

echo "Outlines generated in $repo_root (imenu + Python AST)"
