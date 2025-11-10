;;; py-treesit-outline.el --- Treesit Python outline with lines  -*- lexical-binding: t; -*-

;; Batch-friendly Python outline using Emacs treesit. Requires the Python
;; grammar (treesit python) which is available in modern Emacs.

;; Usage (batch):
;; emacs -Q --batch -l scripts/py-treesit-outline.el \
;;   --eval '(py-outline-batch :outdir "." :files (list "scripts/md_mermaid_render.py" "scripts/md_mermaid_snippet.py"))'

(require 'treesit)

(defun py-outline--ensure-parser ()
  (unless (treesit-parser-list)
    (ignore-errors (treesit-parser-create 'python))))

(defun py-outline--node-line (node)
  (line-number-at-pos (treesit-node-start node)))

(defun py-outline--child-by-field-text (node field)
  (let ((n (treesit-node-child-by-field-name node field)))
    (when n (treesit-node-text n (current-buffer)))))

(defun py-outline--collect-imports ()
  (let ((out '()))
    (dolist (n (treesit-query-capture (treesit-buffer-root-node)
                                      "[(import_statement) (import_from_statement)] @imp"))
      (let* ((node (cdr n))
             (text (string-trim (treesit-node-text node (current-buffer))))
             (ln (py-outline--node-line node)))
        (push (list ln text) out)))
    (nreverse out)))

(defun py-outline--collect-classes ()
  (let (classes)
    (dolist (n (treesit-query-capture (treesit-buffer-root-node) "(class_definition) @cls"))
      (let* ((node (cdr n))
             (name (py-outline--child-by-field-text node "name"))
             (bases (py-outline--child-by-field-text node "superclasses"))
             (ln (py-outline--node-line node))
             (block (treesit-node-child-by-field-name node "body"))
             (methods '()))
        (when block
          (dolist (m (treesit-query-capture block "(function_definition) @m"))
            (let* ((mn (cdr m))
                   (mname (py-outline--child-by-field-text mn "name"))
                   (mln (py-outline--node-line mn)))
              (push (list mname mln) methods))))
        (push (list name ln (and bases (string-trim bases)) (nreverse methods)) classes)))
    (nreverse classes)))

(defun py-outline--top-level-p (node)
  (eq (treesit-node-type (treesit-node-parent node)) "module"))

(defun py-outline--collect-funcs ()
  (let (funcs)
    (dolist (n (treesit-query-capture (treesit-buffer-root-node) "(function_definition) @f"))
      (let* ((node (cdr n)))
        (when (py-outline--top-level-p node)
          (let* ((name (py-outline--child-by-field-text node "name"))
                 (ln (py-outline--node-line node)))
            (push (list name ln) funcs)))))
    (nreverse funcs)))

(defun py-outline--collect-vars ()
  (let (vars)
    ;; Top-level assignments: take first identifier on the left side best-effort.
    (dolist (cap (treesit-query-capture (treesit-buffer-root-node) "(assignment) @a"))
      (let* ((assign (cdr cap)))
        (when (py-outline--top-level-p assign)
          (let ((ids (treesit-query-capture assign "(identifier) @id")))
            (when ids
              (let* ((id (cdr (car ids)))
                     (name (treesit-node-text id (current-buffer)))
                     (ln (py-outline--node-line id)))
                (push (list name ln) vars))))))
    (nreverse vars)))

(defun py-outline--render-md (file imports classes funcs vars)
  (let ((buf (current-buffer))
        (out ""))
    (setq out (concat out "# Python Treesit Outline\n\n"))
    (setq out (concat out "## File: " file "\n\n"))
    (setq out (concat out "## Imports\n"))
    (dolist (i imports)
      (setq out (concat out (format "- line %d: %s\n" (nth 0 i) (nth 1 i)))))
    (setq out (concat out "\n## Classes\n"))
    (dolist (c classes)
      (let ((name (nth 0 c)) (ln (nth 1 c)) (bases (nth 2 c)) (methods (nth 3 c)))
        (setq out (concat out (format "- %s (line %d)%s\n"
                                      name ln (if bases (format " : %s" bases) ""))))
        (dolist (m methods)
          (setq out (concat out (format "  - %s (line %d)\n" (nth 0 m) (nth 1 m))))))
    (setq out (concat out "\n## Functions\n"))
    (dolist (f funcs)
      (setq out (concat out (format "- %s (line %d)\n" (nth 0 f) (nth 1 f)))))
    (setq out (concat out "\n## Variables\n"))
    (dolist (v vars)
      (setq out (concat out (format "- %s (line %d)\n" (nth 0 v) (nth 1 v)))))
    out)))

(defun py-outline--one (file outdir)
  (let* ((buf (find-file-noselect file))
         (out (expand-file-name (concat (file-name-sans-extension (file-name-nondirectory file))
                                        "-py-outline.md") outdir)))
    (with-current-buffer buf
      (py-outline--ensure-parser)
      (let ((imports (py-outline--collect-imports))
            (classes (py-outline--collect-classes))
            (funcs (py-outline--collect-funcs))
            (vars (py-outline--collect-vars)))
        (with-temp-file out
          (insert (py-outline--render-md (or (buffer-file-name buf) file) imports classes funcs vars)))))
    out))

(defun py-outline-batch (&rest plist)
  "Batch entry: (py-outline-batch :outdir \".\" :files (list ...))."
  (let* ((outdir (or (plist-get plist :outdir) default-directory))
         (files (plist-get plist :files)))
    (unless (listp files)
      (error "py-outline-batch: :files must be a list"))
    (dolist (f files)
      (let ((p (py-outline--one f outdir)))
        (princ (format "Wrote %s\n" p))))))

(provide 'py-treesit-outline)

;; End of file balance
;; )
