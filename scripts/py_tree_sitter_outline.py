#!/usr/bin/env python3
"""
Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Author:  Ahmet Usal <ahmetusal@gmail.com>
Collaborators: OpenAI Assistant, Claude Assistant

Python outline generator using py-tree-sitter.

Produces a Markdown outline with line numbers for:
- Imports (dependencies)
- Classes (with base classes) and their methods
- Top-level functions
- Top-level variables (simple identifiers on the left of assignments)
- Best-effort call references (per def/method) using call nodes

If tree-sitter Python is unavailable, exit with a clear message so callers can
fallback to the stdlib AST variant.
"""
from __future__ import annotations

import argparse
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional

try:
    from tree_sitter import Language, Parser  # py-tree-sitter (supports Parser(lang))
except Exception as e:  # pragma: no cover
    raise SystemExit(f"py-tree-sitter not available: {e}")


def _load_python_language() -> Language:
    """Load a Python Language for py-tree-sitter 0.25+ (pointer-based).

    Tries, in order:
      - tree_sitter_python.language() → pointer
      - tree_sitter_languages.get_language('python') → Language
      - Env var pointing to libtree-sitter-python.so via ctypes → pointer
      - Scan common paths (site-packages, ~/.emacs.d/tree-sitter, /usr/lib) for
        libtree-sitter-python.so and load its tree_sitter_python() via ctypes.
    """
    # 1) Direct Python package
    try:
        import tree_sitter_python as tsp  # type: ignore

        return Language(tsp.language())  # pointer → Language
    except Exception:
        pass

    # 2) Prebuilt languages bundle
    try:
        from tree_sitter_languages import get_language  # type: ignore

        lang = get_language("python")
        if isinstance(lang, Language):
            return lang
        # Some bundles might return a pointer-like object
        try:
            return Language(lang)
        except Exception:
            pass
    except Exception:
        pass

    # 3) Env variable to a .so; load symbol tree_sitter_python()
    import os
    import sysconfig
    import glob
    from ctypes import CDLL, c_void_p

    env_vars = ("TREE_SITTER_PYTHON_LIB", "TS_PYTHON_LIB", "TS_LANG_LIB")
    for var in env_vars:
        p = os.environ.get(var)
        if p and Path(p).exists():
            try:
                lib = CDLL(str(p))
                lang_fn = getattr(lib, "tree_sitter_python")
                lang_fn.restype = c_void_p
                return Language(lang_fn())
            except Exception:
                pass

    # 4) Scan for the .so and use ctypes as above
    candidates: list[str] = []
    for base in (sysconfig.get_paths().get("purelib"), sysconfig.get_paths().get("platlib")):
        if base:
            candidates += glob.glob(str(Path(base) / "**/libtree-sitter-python*.so"), recursive=True)
            candidates += glob.glob(str(Path(base) / "**/*languages*.so"), recursive=True)
    candidates += glob.glob("/usr/lib/**/libtree-sitter-python*.so", recursive=True)
    candidates += glob.glob("/usr/local/lib/**/libtree-sitter-python*.so", recursive=True)
    ts_user = Path.home() / ".emacs.d" / "tree-sitter"
    if ts_user.exists():
        candidates += glob.glob(str(ts_user / "**/libtree-sitter-python*.so"), recursive=True)
    for c in candidates:
        try:
            lib = CDLL(c)
            lang_fn = getattr(lib, "tree_sitter_python")
            lang_fn.restype = c_void_p
            return Language(lang_fn())
        except Exception:
            continue
    raise SystemExit(
        "tree-sitter Python language not found. Set TREE_SITTER_PYTHON_LIB to the "
        "compiled language .so, or install `tree-sitter-python`/`tree-sitter-languages`."
    )


def _node_text(src: bytes, node) -> str:
    return src[node.start_byte : node.end_byte].decode("utf-8", errors="replace")


def _line(node) -> int:
    # start_point is (row, col), row is 0-based
    return node.start_point[0] + 1


def _parent_type(node) -> Optional[str]:
    p = node.parent
    return p.type if p is not None else None


@dataclass
class ClassInfo:
    name: str
    line: int
    bases: List[str] = field(default_factory=list)
    methods: List[tuple[str, int, List[str]]] = field(default_factory=list)  # (name, line, calls)


def _collect_calls(src: bytes, node) -> List[str]:
    calls: List[str] = []
    # Walk descendants and collect call "function" texts
    stack = [node]
    while stack:
        n = stack.pop()
        # push children
        for i in range(n.child_count - 1, -1, -1):
            stack.append(n.children[i])
        if n.type == "call":
            # In python grammar, child_by_field_name("function") gives the callee
            fun = n.child_by_field_name("function")
            if fun is not None:
                callee = _node_text(src, fun).strip()
                if callee:
                    calls.append(callee)
    # Deduplicate while preserving order
    seen = set()
    out: List[str] = []
    for c in calls:
        if c not in seen:
            out.append(c)
            seen.add(c)
    return out


def _make_parser(lang: Language) -> Parser:
    """Create a Parser compatible with both older/newer py-tree-sitter APIs."""
    try:
        # Newer API: Parser(lang)
        return Parser(lang)
    except TypeError:
        # Older API: Parser(); set_language(lang)
        p = Parser()
        try:
            p.set_language(lang)  # type: ignore[attr-defined]
        except AttributeError:
            pass
        return p


def outline_python(path: Path) -> str:
    src = path.read_bytes()
    lang = _load_python_language()
    parser = _make_parser(lang)
    tree = parser.parse(src)
    root = tree.root_node

    imports: List[tuple[int, str]] = []  # (line, module) from `import x`
    classes: List[ClassInfo] = []       # local class definitions
    funcs: List[tuple[str, int, List[str]]] = []  # local top-level functions (rare here)
    vars_simple: List[tuple[str, int]] = []  # module-level variables (name, line)
    from_imports: List[tuple[str, int]] = []  # imported symbols from `from ... import ...`

    def iter_descendants(n):
        stack = [n]
        while stack:
            x = stack.pop()
            yield x
            for i in range(x.child_count - 1, -1, -1):
                stack.append(x.children[i])

    # Helpers for import extraction
    def _import_modules(n) -> List[str]:
        names: List[str] = []
        for d in iter_descendants(n):
            if d.type == "dotted_name":
                txt = _node_text(src, d)
                names.append(txt.split(".")[0].strip())
            elif d.type == "aliased_import":
                name = d.child_by_field_name("name")
                if name is not None:
                    txt = _node_text(src, name).strip()
                    names.append(txt.split(".")[0])
        # dedupe preserving order
        seen = set()
        out = []
        for nm in names:
            if nm and nm not in seen:
                out.append(nm)
                seen.add(nm)
        return out

    # name -> first line number
    imported_class_like: dict[str, int] = {}

    for n in root.children:
        t = n.type
        if t == "import_statement":
            for mod in _import_modules(n):
                imports.append((_line(n), mod))
        elif t == "import_from_statement":
            # capture imported targets only (right side of 'import')
            names_set: set[str] = set()
            for d in iter_descendants(n):
                ty = d.type
                if ty == "aliased_import":
                    name = d.child_by_field_name("name")
                    if name is not None:
                        nm = _node_text(src, name).strip().split(".")[-1]
                        if nm:
                            names_set.add(nm)
                elif ty in ("identifier", "dotted_name"):
                    # ensure we are on the import targets, not module path
                    # naive heuristic: skip if text contains 'import'
                    txt = _node_text(src, d).strip()
                    if txt and txt != "import":
                        nm = txt.split(".")[-1]
                        if nm:
                            names_set.add(nm)
            for nm in names_set:
                if nm and nm[0].isupper() and nm not in imported_class_like:
                    imported_class_like[nm] = _line(n)
                if nm:
                    from_imports.append((nm, _line(n)))
        elif t == "class_definition":
            name_node = n.child_by_field_name("name")
            name = _node_text(src, name_node).strip() if name_node else "<anon>"
            bases_node = n.child_by_field_name("superclasses") or n.child_by_field_name("argument_list")
            bases_txt = _node_text(src, bases_node).strip() if bases_node else ""
            bases = []
            if bases_txt:
                # Heuristic: strip parens and split by comma
                bt = bases_txt.strip()
                if bt.startswith("(") and bt.endswith(")"):
                    bt = bt[1:-1]
                bases = [b.strip() for b in bt.split(",") if b.strip()]
            body = n.child_by_field_name("body")
            info = ClassInfo(name=name, line=_line(n), bases=bases)
            if body is not None:
                for child in body.children:
                    target = None
                    if child.type == "function_definition":
                        target = child
                    elif child.type == "decorated_definition":
                        # find enclosed function_definition
                        for ch2 in child.children:
                            if ch2.type == "function_definition":
                                target = ch2
                                break
                    if target is not None:
                        mname_node = target.child_by_field_name("name")
                        mname = _node_text(src, mname_node) if mname_node else "<anon>"
                        calls = _collect_calls(src, target)
                        info.methods.append((mname, _line(target), calls))
            classes.append(info)
        elif t == "function_definition":
            # top-level only
            if _parent_type(n) == "module":
                fname_node = n.child_by_field_name("name")
                fname = _node_text(src, fname_node) if fname_node else "<anon>"
                calls = _collect_calls(src, n)
                funcs.append((fname, _line(n), calls))
        elif t == "assignment":
            # top-level simple variables: first identifier on LHS
            if _parent_type(n) == "module":
                # Find first identifier child
                ident = None
                for d in iter_descendants(n):
                    if d.type == "identifier":
                        ident = d
                        break
                if ident is not None:
                    vars_simple.append((_node_text(src, ident), _line(ident)))

    # Build per-function and per-method local variables (assignments) and __init__ fields
    func_locals: dict[str, List[tuple[str, int]]] = {}
    class_fields: List[tuple[str, int]] = []  # (__init__ field, line)

    def collect_locals(fn_node, name: str):
        body = fn_node.child_by_field_name("body")
        if body is None:
            return
        for d in iter_descendants(body):
            # Cover plain, augmented, and annotated assignments
            if ("assign" in d.type) or d.type in ("assignment", "augmented_assignment", "ann_assignment", "typed_default_parameter"):
                left = (
                    d.child_by_field_name("left")
                    or d.child_by_field_name("target")
                    or d.child_by_field_name("name")
                    or d
                )
                # collect identifiers under left side; skip self.attr here
                stack = [left]
                while stack:
                    x = stack.pop()
                    if x.type == "identifier":
                        func_locals.setdefault(name, []).append((_node_text(src, x), _line(x)))
                    elif x.type == "attribute":
                        obj = x.child_by_field_name("object")
                        attr = x.child_by_field_name("attribute")
                        if obj is not None and _node_text(src, obj) == "self" and attr is not None and name == "__init__":
                            class_fields.append((_node_text(src, attr), _line(attr)))
                    else:
                        for i in range(x.child_count - 1, -1, -1):
                            stack.append(x.children[i])

    for n in root.children:
        if n.type == "function_definition" and _parent_type(n) == "module":
            fname_node = n.child_by_field_name("name")
            f_name = _node_text(src, fname_node) if fname_node else "<anon>"
            collect_locals(n, f_name)
        elif n.type == "class_definition":
            body = n.child_by_field_name("body")
            if body is not None:
                for child in body.children:
                    target = None
                    if child.type == "function_definition":
                        target = child
                    elif child.type == "decorated_definition":
                        for ch2 in child.children:
                            if ch2.type == "function_definition":
                                target = ch2
                                break
                    if target is not None:
                        mname_node = target.child_by_field_name("name")
                        mname = _node_text(src, mname_node) if mname_node else "<anon>"
                        collect_locals(target, mname)

    # Determine imported functions: names used as calls or decorators
    called_names: set[str] = set()
    # calls already collected per function/method; scan whole tree for decorator names
    def _collect_calls_global(node):
        stack = [node]
        while stack:
            x = stack.pop()
            if x.type == "call":
                fun = x.child_by_field_name("function")
                if fun is not None:
                    nm = _node_text(src, fun).split(".")[-1].strip()
                    if nm:
                        called_names.add(nm)
            elif x.type == "decorated_definition":
                # children before definition are decorators
                for ch in x.children:
                    if ch.type == "decorator":
                        name = ch.child_by_field_name("name")
                        if name is not None:
                            nm = _node_text(src, name).split(".")[-1].strip()
                            if nm:
                                called_names.add(nm)
            for i in range(x.child_count - 1, -1, -1):
                stack.append(x.children[i])
    _collect_calls_global(root)

    imported_functions: List[tuple[str, int]] = []
    for nm, ln in from_imports:
        if (nm in called_names or nm.endswith("method") or nm.endswith("function")) and (nm and nm[0].islower()):
            imported_functions.append((nm, ln))

    # Render: Emacs imenu-like format
    out: List[str] = []
    out.append(f"# Imenu Export for {path.name}\n")
    out.append(f"## File: {path.resolve()}\n\n")

    # Module
    out.append("# Module\n")
    for ln, mod in sorted(imports, key=lambda t: t[0]):
        out.append(f"## {mod} (line {ln})\n")

    # Class
    out.append("# Class\n")
    # imported class-like
    for nm, ln in sorted(imported_class_like.items(), key=lambda t: t[1]):
        out.append(f"## {nm} (line {ln})\n")
    for cls in classes:
        out.append(f"## {cls.name} (line {cls.line})\n")

    # Method (only if local classes exist)
    if classes:
        out.append("# Method\n")
        for cls in classes:
            out.append(f"## {cls.name}\n")
            for mname, mln, _calls in cls.methods:
                out.append(f"### {mname} (line {mln})\n")

    # Field
    if class_fields:
        out.append("# Field\n")
        out.append("## __init__\n")
        for nm, ln in sorted(class_fields, key=lambda t: t[1]):
            out.append(f"### {nm} (line {ln})\n")

    # Function
    out.append("# Function\n")
    # imported callable-like
    for nm, ln in sorted(imported_functions, key=lambda t: t[1]):
        out.append(f"## {nm} (line {ln})\n")
    for name, ln, _calls in funcs:
        out.append(f"## {name} (line {ln})\n")

    # Variable (module-level and per function)
    out.append("# Variable\n")
    if vars_simple:
        out.append("## <module>\n")
        for nm, ln in sorted(vars_simple, key=lambda t: t[1]):
            out.append(f"### {nm} (line {ln})\n")
    # class methods
    for cls in classes:
        for mname, _mln, _ in cls.methods:
            if func_locals.get(mname):
                out.append(f"## {mname}\n")
                for nm, ln in sorted(func_locals.get(mname, []), key=lambda t: t[1]):
                    out.append(f"### {nm} (line {ln})\n")
    # top-level functions
    for name, _ln, _ in funcs:
        if func_locals.get(name):
            out.append(f"## {name}\n")
            for nm, ln in sorted(func_locals.get(name, []), key=lambda t: t[1]):
                out.append(f"### {nm} (line {ln})\n")

    return "".join(out)


def main():
    ap = argparse.ArgumentParser(description="Tree-sitter Python outline with line numbers")
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("-o", "--outdir", type=Path, default=Path("."))
    args = ap.parse_args()

    for f in args.files:
        md = outline_python(f)
        out_path = args.outdir / f"{f.stem}-pyts-outline.md"
        out_path.write_text(md, encoding="utf-8")
        print(f"Wrote {out_path}")


if __name__ == "__main__":
    main()
