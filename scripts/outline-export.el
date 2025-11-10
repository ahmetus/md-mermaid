;;; outline-export.el --- Export imenu to Org/Markdown (batch) -*- lexical-binding: t; -*-

;; Minimal, batch-friendly imenu exporter with line numbers.
;; Safe to load with: emacs -Q --batch -l scripts/outline-export.el

(require 'imenu)
(require 'seq)

(defun outline-export--mkline (buf pos)
  (with-current-buffer buf
    (line-number-at-pos (if (markerp pos) (marker-position pos) pos))))

(defun outline-export--alist->org (alist level buf &optional root)
  (let ((out "") (items ""))
    (dolist (it alist)
      (let ((name (car it))
            (val (cdr it)))
        (cond
         ((and (listp val) (not (number-or-marker-p val)))
          (setq out (concat out
                            (make-string level ?*) " " name "\n"
                            (outline-export--alist->org val (1+ level) buf nil))))
         ((number-or-marker-p val)
          (let* ((ln (outline-export--mkline buf val))
                 (pref (make-string (if root (1+ level) level) ?*)))
            (setq items (concat items (format "%s %s (line %d)\n" pref name ln)))))
         (t
          (let ((pref (make-string (if root (1+ level) level) ?*)))
            (setq items (concat items (format "%s %s\n" pref name))))))))
    (if (and root (> (length items) 0))
        (setq out (concat out (make-string level ?*) " Functions\n" items))
      (setq out (concat out items)))
    out))

(defun outline-export--alist->md (alist level buf &optional root)
  (let ((out "") (items ""))
    (dolist (it alist)
      (let ((name (car it))
            (val (cdr it)))
        (cond
         ((and (listp val) (not (number-or-marker-p val)))
          (setq out (concat out
                            (make-string level ?#) " " name "\n"
                            (outline-export--alist->md val (1+ level) buf nil))))
         ((number-or-marker-p val)
          (let* ((ln (outline-export--mkline buf val))
                 (pref (make-string (if root (1+ level) level) ?#)))
            (setq items (concat items (format "%s %s (line %d)\n" pref name ln)))))
         (t
          (let ((pref (make-string (if root (1+ level) level) ?#)))
            (setq items (concat items (format "%s %s\n" pref name))))))))
    (if (and root (> (length items) 0))
        (setq out (concat out (make-string level ?#) " Functions\n" items))
      (setq out (concat out items)))
    out))

(defun outline-export--one (file format outdir)
  "Export FILE's imenu to OUTDIR as FORMAT symbol: 'markdown or 'org."
  (let* ((buf (find-file-noselect file))
         (ext (if (eq format 'markdown) ".md" ".org"))
         (base (concat (file-name-sans-extension (file-name-nondirectory file)) "-outline"))
         (out (expand-file-name (concat base ext) outdir)))
    (with-current-buffer buf
      (imenu--make-index-alist)
      (let* ((raw (imenu--make-index-alist))
             (index (seq-filter (lambda (it) (not (string= (car it) "*Rescan*"))) raw)))
        (with-temp-file out
          (if (eq format 'markdown)
              (progn
                (insert "# Imenu Export for " (buffer-name buf) "\n")
                (insert "## File: " (or (buffer-file-name buf) "Unnamed Buffer") "\n\n")
                (insert (outline-export--alist->md index 1 buf t)))
            (progn
              (insert "* Imenu Export for " (buffer-name buf) "\n")
              (insert "** File: " (or (buffer-file-name buf) "Unnamed Buffer") "\n\n")
              (insert (outline-export--alist->org index 1 buf t)))))))
    out))

(defun outline-export-batch (&rest plist)
  "Batch entry: (outline-export-batch :format \"markdown\" :outdir \".\" :files (list ...))."
  (let* ((fmt (intern (downcase (or (plist-get plist :format) "markdown"))))
         (outdir (or (plist-get plist :outdir) default-directory))
         (files (plist-get plist :files)))
    (unless (listp files)
      (error "outline-export-batch: :files must be a list of paths"))
    (dolist (f files)
      (let ((p (outline-export--one f fmt outdir)))
        (princ (format "Wrote %s\n" p))))))

(provide 'outline-export)
