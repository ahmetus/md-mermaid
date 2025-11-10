#!/usr/bin/env python3
"""
Generate a Markdown outline for Python files using the stdlib `ast` module.
Includes imports, classes -> methods, top-level functions, and simple
top-level variables with line numbers. No external deps.

Usage:
  python3 scripts/py_ast_outline.py -o OUTDIR file1.py file2.py ...
"""
from __future__ import annotations

import argparse
import ast
from pathlib import Path
from typing import List, Tuple


def gather(path: Path):
    src = path.read_text(encoding="utf-8")
    tree = ast.parse(src, filename=str(path))

    imports: List[Tuple[int, str]] = []
    classes: List[Tuple[str, int, List[str], List[Tuple[str, int]]]] = []
    funcs: List[Tuple[str, int]] = []
    vars_: List[Tuple[str, int]] = []

    for node in tree.body:
        if isinstance(node, ast.Import):
            parts = [f"import {', '.join([a.name for a in node.names])}"]
            imports.append((node.lineno, parts[0]))
        elif isinstance(node, ast.ImportFrom):
            mod = node.module or ""
            names = ", ".join([a.name for a in node.names])
            imp = f"from {mod} import {names}"
            imports.append((node.lineno, imp))
        elif isinstance(node, ast.ClassDef):
            bases = []
            for b in node.bases:
                try:
                    bases.append(ast.unparse(b))
                except Exception:
                    bases.append(getattr(getattr(b, "id", None), "__str__", lambda: "?")())
            methods: List[Tuple[str, int]] = []
            for item in node.body:
                if isinstance(item, ast.FunctionDef):
                    methods.append((item.name, item.lineno))
            classes.append((node.name, node.lineno, bases, methods))
        elif isinstance(node, ast.FunctionDef):
            funcs.append((node.name, node.lineno))
        elif isinstance(node, ast.Assign):
            # top-level simple variables
            for t in node.targets:
                name = None
                if isinstance(t, ast.Name):
                    name = t.id
                elif isinstance(t, ast.Attribute) and isinstance(t.value, ast.Name) and t.value.id == "__all__":
                    name = "__all__"
                if name:
                    vars_.append((name, node.lineno))

    return imports, classes, funcs, vars_


def render_md(path: Path, data) -> str:
    imports, classes, funcs, vars_ = data
    out = []
    out.append("# Python AST Outline\n")
    out.append(f"## File: {path}\n\n")
    out.append("## Imports\n")
    for ln, text in imports:
        out.append(f"- line {ln}: {text}\n")
    out.append("\n## Classes\n")
    for name, ln, bases, methods in classes:
        base_txt = f" : {', '.join(bases)}" if bases else ""
        out.append(f"- {name} (line {ln}){base_txt}\n")
        for mname, mln in methods:
            out.append(f"  - {mname} (line {mln})\n")
    out.append("\n## Functions\n")
    for name, ln in funcs:
        out.append(f"- {name} (line {ln})\n")
    out.append("\n## Variables\n")
    for name, ln in vars_:
        out.append(f"- {name} (line {ln})\n")
    return "".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("-o", "--outdir", type=Path, default=Path("."))
    args = ap.parse_args()

    for f in args.files:
        data = gather(f)
        out = render_md(f, data)
        out_path = args.outdir / f"{f.stem}-py-outline.md"
        out_path.write_text(out, encoding="utf-8")
        print(f"Wrote {out_path}")


if __name__ == "__main__":
    main()

