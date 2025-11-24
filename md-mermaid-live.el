;;; md-mermaid-live.el --- Inline live Mermaid overlays (extension)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is an extension for md-mermaid. It does not modify existing
;; behavior; it adds an optional inline overlay preview similar to ob-mermaid.

;; Author:  Ahmet Usal <ahmetusal@gmail.com>
;; Collaborators: OpenAI Assistant, Claude Assistant
;; Version: 1.0
;; Keywords: markdown, mermaid, tools, images
;; URL: https://github.com/ahmetus/md-mermaid

;; Usage:
;;   (add-to-list 'load-path (expand-file-name "md-mermaid" user-emacs-directory))
;;   (require 'md-mermaid)
;;   (require 'md-mermaid-live)
;;   ;; Toggle in Markdown buffers:
;;   ;; M-x md-mermaid-live-mode

;;; Commentary:
;; Renders ```mermaid code fences asynchronously and displays PNG overlays
;; directly below each fence.  Keeps the existing md-mermaid pipeline intact.
;; Snippet rendering is delegated to scripts/md_mermaid_snippet.py so the
;; original Python/Bash files remain untouched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
;; Do not hard-require `md-mermaid' at load-time to keep batch linters
;; (running with -Q and without load-path tweaks) from failing. Commands
;; that need it will require it just-in-time.
(require 'md-mermaid nil 'noerror)

(declare-function treesit-available-p "treesit")
(declare-function treesit-language-available-p "treesit")
(declare-function treesit-parser-p "treesit")
(declare-function treesit-parser-language "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-parser-create "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-parser-root-node "treesit")
(declare-function treesit-query-capture "treesit")

;; Silence byte-compiler for references before the minor mode is defined.
(defvar md-mermaid-live-mode nil)

;; Functions used from md-mermaid are declared for the byte-compiler.
(declare-function md-mermaid--json-first-issue "md-mermaid")
(declare-function md-mermaid--extract-first-json "md-mermaid")
(declare-function md-mermaid--format-friendly "md-mermaid")
(declare-function md-mermaid--dispatch-quickfix "md-mermaid")

(defun md-mermaid-live--lib-root ()
  "Directory where `md-mermaid-live.el` resides."
  (file-name-directory
   (or load-file-name
       (ignore-errors (locate-library "md-mermaid-live"))
       buffer-file-name
       default-directory)))

(defgroup md-mermaid-live nil
  "Live, async rendering overlays for Mermaid fences in Markdown."
  :group 'md-mermaid)

(defcustom md-mermaid-live-idle-delay 0.8
  "Seconds of idle time before re-rendering changed Mermaid blocks."
  :type 'number)

(defcustom md-mermaid-live-edit-delay 0.25
  "Idle delay after edits before re-rendering.
Must be <= `md-mermaid-live-idle-delay' to keep edits responsive."
  :type 'number)

(defcustom md-mermaid-live-scroll-delay 0.6
  "Idle delay after scroll events before re-rendering visible fences."
  :type 'number)

(defcustom md-mermaid-live-scroll-conservatively nil
  "Value assigned to `scroll-conservatively` while live mode is active.
When nil, the variable is left unchanged.  Increasing the value reduces the
chance that Emacs recenters the window while scrolling past tall overlays."
  :type '(choice (const :tag "Leave unchanged" nil)
                 integer))

(defcustom md-mermaid-live-disable-auto-window-vscroll nil
  "When non-nil, set `auto-window-vscroll` to nil while the mode is active.
Disabling auto window vscroll prevents Emacs from automatically jumping when
lines (or images) are taller than the window, which improves stability when
scrolling through large diagrams."
  :type 'boolean)

(defconst md-mermaid-live--scroll-conservative-default 101
  "Default `scroll-conservatively` value used by toggle helpers.")

(defcustom md-mermaid-live-width 1400
  "PNG width (pixels) used for live overlays."
  :type 'integer)

(defcustom md-mermaid-live-background "white"
  "Background color for PNG overlays (e.g., \"white\" or \"transparent\")."
  :type 'string)

(defcustom md-mermaid-live-theme nil
  "Optional Mermaid theme for live rendering.  When nil, uses default."
  :type '(choice (const :tag "Default" nil) string))

(defcustom md-mermaid-live-known-headers
  (copy-sequence (or (bound-and-true-p md-mermaid-known-headers)
                     '("flowchart TD" "flowchart LR" "sequenceDiagram" "classDiagram"
                       "erDiagram" "stateDiagram-v2" "gantt" "journey" "pie")))
  "Diagram headers offered when automatically inserting missing headers."
  :type '(repeat string))

(defcustom md-mermaid-live-scope 'visible
  "Scope to scan for fences: `visible' or `buffer'."
  :type '(choice (const visible) (const buffer)))

(defcustom md-mermaid-live-max-procs 1
  "Max concurrent mmdc jobs for live rendering."
  :type 'integer)

(defcustom md-mermaid-live-assets-dir nil
  "Assets directory for live images.  When nil, uses `md-mermaid-assets-dir'."
  :type '(choice (const :tag "Use md-mermaid-assets-dir" nil) string))

(defcustom md-mermaid-live-hide-code nil
  "When non-nil, hide Mermaid fence bodies behind rendered overlays.
Default is nil (code visible).
Toggle with `md-mermaid-live-toggle-code-visibility'."
  :type 'boolean)

(defcustom md-mermaid-live-hidden-placeholder
  "[Mermaid code hidden – toggle visibility to edit]"
  "Placeholder text shown when Mermaid code is hidden.
The string is indented to match the fence and wrapped between the original
opening/closing fence markers."
  :type 'string)

(defcustom md-mermaid-live-hidden-max-height 6
  "Maximum line height applied to hidden Mermaid code placeholders.
When nil, do not adjust placeholder height.  When a positive integer, use it as
an upper bound for the `line-height` property so collapsing large fences does
not shrink the buffer drastically."
  :type '(choice (const :tag "No adjustment" nil)
                 integer))

(defcustom md-mermaid-live-debug t
  "When non-nil, emit detailed job diagnostics into `*md-mermaid-live*'."
  :type 'boolean)

(defcustom md-mermaid-live-max-lines 8000
  "Disable automatic live rendering when buffer has more than this many lines.
Set to nil to skip the guard."
  :type '(choice (const :tag "No guard" nil) integer))

(defcustom md-mermaid-live-max-fences 40
  "Disable automatic live rendering when more than this many Mermaid fences exist.
Set to nil to skip the guard."
  :type '(choice (const :tag "No guard" nil) integer))

(defcustom md-mermaid-live-refresh-limit 20
  "Maximum number of fences to process per refresh cycle.
Set to nil to disable throttling."
  :type '(choice (const :tag "No limit" nil) integer))

(defcustom md-mermaid-live-job-timeout 8.0
  "Seconds to wait before considering a running render job stuck.
When nil, jobs never time out."
  :type '(choice (const :tag "Never" nil) number))

(defcustom md-mermaid-live-max-attempts 3
  "Maximum number of attempts to render a single fence before giving up."
  :type 'integer)

(defvar-local md-mermaid-live--timer nil)
(defvar-local md-mermaid-live--queue nil)
(defvar-local md-mermaid-live--running nil)
(defvar-local md-mermaid-live--overlays nil)
(defvar-local md-mermaid-live--code-overlays nil)
(defvar-local md-mermaid-live--cache nil)
(defvar-local md-mermaid-live--saved-scroll-settings nil)
(defvar-local md-mermaid-live--resolved-root nil)
(defvar-local md-mermaid-live--resolved-snippet nil)
(defvar-local md-mermaid-live--resolved-context nil)
(defvar-local md-mermaid-live--next-job 1)
(defvar-local md-mermaid-live--large-buffer nil)
(defvar-local md-mermaid-live--pending-refresh nil)
(defvar-local md-mermaid-live--bootstrap-ran nil)
(defvar-local md-mermaid-live--monitor-timer nil)
(defvar-local md-mermaid-live--invisibility-added nil)
(defvar-local md-mermaid-live--ts-parser nil)
(defvar-local md-mermaid-live--last-window-start nil)
(defvar-local md-mermaid-live--last-window-end nil)
(defvar-local md-mermaid-live--last-tick nil)
(defvar-local md-mermaid-live--hide-code nil)
(defvar-local md-mermaid-live--last-scroll-pos nil)
(defvar-local md-mermaid-live--scroll-direction nil)

(defvar md-mermaid-live--global-root-cache (make-hash-table :test 'equal)
  "Shared cache of resolved project roots keyed by directory context.")

(defvar md-mermaid-live--global-snippet-cache (make-hash-table :test 'equal)
  "Shared cache of snippet script locations keyed by project root.")

(defvar md-mermaid-live--last-errors nil
  "List of (buffer beg end json timestamp) entries for recent failures.")

(cl-defstruct md-mermaid-live-job
  id block overlay sig pos buffer proc started)

(defun md-mermaid-live--record-error (job json)
  "Record friendly error JSON for JOB's block."
  (let* ((buf (md-mermaid-live-job-buffer job))
         (block (md-mermaid-live-job-block job))
         (beg (plist-get block :beg))
         (end (plist-get block :end)))
    (when (and (buffer-live-p buf) beg end)
      (setq md-mermaid-live--last-errors
            (cons (list buf beg end json (float-time))
                  (cl-remove-if
                   (lambda (entry)
                     (and (eq (nth 0 entry) buf)
                          (not (or (< (nth 2 entry) beg)
                                   (> (nth 1 entry) end)))))
                   md-mermaid-live--last-errors))))))

(defun md-mermaid-live--clear-error-for-job (job)
  "Remove stored errors for JOB's block."
  (let* ((buf (md-mermaid-live-job-buffer job))
         (block (md-mermaid-live-job-block job))
         (beg (plist-get block :beg))
         (end (plist-get block :end)))
    (when (and (buffer-live-p buf) beg end)
      (setq md-mermaid-live--last-errors
            (cl-remove-if
             (lambda (entry)
               (and (eq (nth 0 entry) buf)
                    (<= (nth 1 entry) beg)
                    (>= (nth 2 entry) end)))
             md-mermaid-live--last-errors)))))

(defun md-mermaid-live--errors-for-range (buf beg end)
  "Return the newest error entry overlapping BEG..END in BUF."
  (seq-find
   (lambda (entry)
     (and (eq (nth 0 entry) buf)
          (not (or (< (nth 2 entry) beg)
                   (> (nth 1 entry) end)))))
   md-mermaid-live--last-errors))

(defmacro md-mermaid-live--with-job-buffer (job &rest body)
  "Execute BODY in the live buffer associated with JOB when possible."
  (declare (indent 1))
  `(let ((buf (md-mermaid-live-job-buffer ,job)))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@body))))

(defun md-mermaid-live--block-bounds-at-point ()
  "Return (BEG END) for the ```mermaid block at point, or nil."
  (save-excursion
    (let ((pt (point))
          beg end)
      (when (re-search-backward "^```\\(mermaid\\)\\b" nil t)
        (setq beg (match-end 0))
        (goto-char beg)
        (when (re-search-forward "^```\\s-*$" nil t)
          (setq end (match-beginning 0))
          (when (and (<= beg pt) (<= pt end))
            (list beg end)))))))

(defun md-mermaid-live--block-string (beg end)
  "Return block text between BEG and END."
  (buffer-substring-no-properties beg end))

(defun md-mermaid-live--first-nonblank-line (string)
  "Return the first nonblank line in STRING, or nil."
  (car (seq-remove (lambda (line) (string-match-p "\\`\\s-*\\'" line))
                   (split-string string "\n"))))

(defun md-mermaid-live--has-header-p (string)
  "Return non-nil when STRING begins with a known Mermaid header."
  (let ((line (md-mermaid-live--first-nonblank-line string)))
    (seq-some (lambda (header)
                (and line (string-prefix-p header line)))
              md-mermaid-live-known-headers)))

(defun md-mermaid-live--insert-header (beg)
  "Prompt for a header and insert at BEG."
  (let* ((choice (completing-read "Diagram header: "
                                  md-mermaid-live-known-headers nil t nil nil
                                  (car md-mermaid-live-known-headers))))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\n\t ")
      (insert choice "\n"))
    choice))

(defun md-mermaid-live--quote-bare-end-on-line ()
  "Quote bare `end` on the current line; return replacements."
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        (count 0))
    (save-excursion
      (goto-char bol)
      (unless (looking-at-p "^```")
        (while (re-search-forward "\\_<end\\_>" eol t)
          (replace-match "\"end\"" t t)
          (cl-incf count))))
    count))

(defun md-mermaid-live--auto-close-one-bracket (beg end)
  "Close a single unmatched bracket between BEG and END if possible."
  (let* ((text (md-mermaid-live--block-string beg end))
         (pairs '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))
         (closer nil))
    (dolist (pair pairs)
      (let* ((open (car pair))
             (close (cdr pair))
             (opens (cl-count open text))
             (closes (cl-count close text)))
        (when (= (- opens closes) 1)
          (setq closer close))))
    (when closer
      (save-excursion
        (goto-char end)
        (insert (string closer) "\n"))
      closer)))

(defun md-mermaid-live--jump-to-block-line (beg where)
  "Jump to WHERE (e.g., \"line 5\") relative to block BEG."
  (when (and where (string-match "\\([0-9]+\\)" where))
    (goto-char beg)
    (forward-line (1- (string-to-number (match-string 1 where))))
    (recenter 3)))

(defun md-mermaid-live--rerender-range (beg end)
  "Cancel jobs overlapping BEG..END and refresh."
  (md-mermaid-live--cancel-jobs-in-range beg end)
  (md-mermaid-live-refresh t))

(defun md-mermaid-live--friendly-summary (json)
  "Return a short summary string for JSON payload."
  (let* ((summary (alist-get 'summary json))
         (issue (md-mermaid--json-first-issue json))
         (where (and issue (alist-get 'where issue))))
    (string-join
     (delq nil
           (list "Mermaid error"
                 (and summary (concat " — " summary))
                 (and where (concat " (" where ")"))))
     "")))

(defun md-mermaid-live--project-root ()
  "Return the best guess for the md-mermaid project root in this buffer.
Searches the snippet script location and caches the nearest directory."
  (let* ((script "scripts/md_mermaid_snippet.py")
         (context (list (when buffer-file-name (file-name-directory buffer-file-name))
                        default-directory
                        (md-mermaid-live--lib-root)))
         (context-hash (sxhash context)))
    (unless (and md-mermaid-live--resolved-root
                 (eq md-mermaid-live--resolved-context context-hash)
                 (file-exists-p (expand-file-name script md-mermaid-live--resolved-root)))
      (let* ((explicit (when (fboundp 'md-mermaid--project-root)
                         (ignore-errors (md-mermaid--project-root))))
             (search-paths (delq nil (list explicit (md-mermaid-live--lib-root)
                                           (car context) (cadr context))))
             (cache-key (mapconcat #'identity (mapcar (lambda (d) (or d "")) search-paths) "|"))
             (cached (gethash cache-key md-mermaid-live--global-root-cache))
             (found (or (and cached (file-exists-p (expand-file-name script cached)) cached)
                        (seq-some (lambda (dir)
                                    (when dir
                                      (locate-dominating-file dir script)))
                                  search-paths))))
        (setq md-mermaid-live--resolved-root (or found explicit (md-mermaid-live--lib-root)
                                                 (car context) (cadr context) user-emacs-directory))
        (setq md-mermaid-live--resolved-context context-hash)
        (setq md-mermaid-live--resolved-snippet nil)
        (puthash cache-key md-mermaid-live--resolved-root md-mermaid-live--global-root-cache)))
    md-mermaid-live--resolved-root))

(defun md-mermaid-live--python-binary ()
  "Return the Python interpreter to use for live rendering jobs.
Falls back from md-mermaid config, then python3, python, and finally \"python3\"."
  (cond
   ((fboundp 'md-mermaid--python-binary) (md-mermaid--python-binary))
   ((executable-find "python3"))
   ((executable-find "python"))
   (t "python3")))

(defun md-mermaid-live--snippet-script ()
  "Locate the snippet renderer script robustly.  Return absolute path or nil."
  (or md-mermaid-live--resolved-snippet
      (let* ((root (md-mermaid-live--project-root))
             (cached (and root (gethash root md-mermaid-live--global-snippet-cache)))
             (lib (md-mermaid-live--lib-root))
             (install (when (fboundp 'md-mermaid--lib-root)
                        (ignore-errors (md-mermaid--lib-root))))
             (candidates (delete-dups
                          (delq nil
                                (append (list cached)
                                        (when root
                                          (list (expand-file-name "scripts/md_mermaid_snippet.py" root)
                                                (expand-file-name "md-mermaid/scripts/md_mermaid_snippet.py" root)))
                                        (when lib
                                          (list (expand-file-name "scripts/md_mermaid_snippet.py" lib)
                                                (expand-file-name "../scripts/md_mermaid_snippet.py" lib)))
                                        (when install
                                          (list (expand-file-name "scripts/md_mermaid_snippet.py" install)
                                                (expand-file-name "../scripts/md_mermaid_snippet.py" install))))))))
        (setq md-mermaid-live--resolved-snippet (seq-find #'file-exists-p candidates))
        (unless md-mermaid-live--resolved-snippet
          (md-mermaid-live--debug "snippet script missing root=%s lib=%s candidates=%S"
                                  root lib candidates))
        (when (and root md-mermaid-live--resolved-snippet)
          (puthash root md-mermaid-live--resolved-snippet md-mermaid-live--global-snippet-cache))
        md-mermaid-live--resolved-snippet)))

(defun md-mermaid-live--resolve-config (root)
  "Return the user md-mermaid config file found within ROOT.
Returns nil when the configured file does not exist."  ; place actual text
  (when (and (boundp 'md-mermaid-config) md-mermaid-config)
    (let ((path (expand-file-name md-mermaid-config root)))
      (when (file-exists-p path) path))))

(defun md-mermaid-live--resolve-puppeteer-config (root)
  "Return the Puppeteer configuration JSON under ROOT when present."  ; need text
  (let ((candidates (list (expand-file-name "md-mermaid/puppeteer-no-sandbox.json" root)
                          (expand-file-name "puppeteer-no-sandbox.json" root))))
    (seq-find #'file-exists-p candidates)))

(defun md-mermaid-live--job-name (job)
  "Return a short job identifier string for JOB."
  (format "md-mermaid-live-%04d-%s"
          (md-mermaid-live-job-id job)
          (or (md-mermaid-live--sig-prefix (md-mermaid-live-job-sig job)) "none")))

(defun md-mermaid-live--job-command (job)
  "Return plist describing python command for JOB or nil when unavailable."
  (let* ((sig (md-mermaid-live-job-sig job))
         (root (md-mermaid-live--project-root))
         (script (md-mermaid-live--snippet-script)))
    (when script
      (let* ((python (md-mermaid-live--python-binary))
             (assets (md-mermaid-live--assets-dir))
             (config (md-mermaid-live--resolve-config root))
             (ppcfg (md-mermaid-live--resolve-puppeteer-config root))
             (args (append (list script
                                 "--format" "png"
                                 "--assets-dir" assets
                                 "--background" md-mermaid-live-background)
                           (when (numberp md-mermaid-live-width)
                             (list "--width" (number-to-string md-mermaid-live-width)))
                           (when md-mermaid-live-theme
                             (list "--theme" md-mermaid-live-theme))
                           (when config (list "--config" config))
                           (when ppcfg (list "--puppeteer-config" ppcfg)))))
        (list :python python
              :args args
              :sig sig
              :name (md-mermaid-live--job-name job))))))

(defun md-mermaid-live--extended-path ()
  "Return PATH extended with common user tool directories.
Includes npm, pnpm, yarn, and ~/.local/bin so subprocesses inherit those bins."
  (let* ((orig (or (getenv "PATH") ""))
         (extra (list (expand-file-name "~/.local/bin")
                      (expand-file-name "~/.config/yarn/global/node_modules/.bin")
                      (expand-file-name "~/.yarn/bin")
                      (expand-file-name "~/.npm-global/bin")
                      (expand-file-name "~/.local/share/pnpm")
                      (expand-file-name "~/.pnpm-global/bin")))
         (parts (delete-dups (append extra (split-string orig path-separator t)))))
    (mapconcat #'identity parts path-separator)))

(defun md-mermaid-live--read-process-first-line (proc)
  "Return the first trimmed line emitted by PROC's buffer if it exists."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (string-trim (buffer-substring (point-min) (line-end-position))))))

(defun md-mermaid-live--spawn-job-process (job python args name)
  "Start a process for JOB with PYTHON, ARGS, and NAME in a dedicated buffer."
  (let* ((buffer-name (format "*%s*" name))
         (proc-buf (generate-new-buffer buffer-name))
         (process-environment (cons (concat "PATH=" (md-mermaid-live--extended-path))
                                    process-environment)))
    (let ((proc (make-process
                 :name name
                 :buffer proc-buf
                 :command (cons python args)
                 :noquery t
                 :connection-type 'pipe
                 :sentinel #'md-mermaid-live--job-sentinel)))
      (process-put proc 'md-mermaid-job job)
      proc)))

(defun md-mermaid-live--show-friendly-error (job proc)
  "Insert readable error details for JOB using PROC's buffer output.
Returns the parsed JSON payload when available."
  (let* ((payload (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (buffer-string))))
         (json (md-mermaid--extract-first-json payload))
         (text (if json
                   (md-mermaid--format-friendly json)
                 (concat (propertize "Mermaid render failed (no JSON details)\n" 'face 'error)
                         (when payload
                           (concat (propertize "Raw output:\n" 'face 'shadow) payload))))))
    (with-current-buffer (get-buffer-create "*md-mermaid-live*")
      (goto-char (point-max))
      (insert (format "\n[%s]\n" (md-mermaid-live--job-name job)))
      (insert text)
      (insert "\n"))
    (when json
      (md-mermaid-live--record-error job json))
    json))

(defun md-mermaid-live--handle-job-exit (job proc)
  "Handle JOB termination for running PROC and refresh overlays.
Signal status, handle failure output, and drain the queue when PROC exits."
  (let* ((status (process-status proc))
         (exit-code (process-exit-status proc))
         (ok (and (eq status 'exit) (zerop exit-code)))
         (ov (md-mermaid-live-job-overlay job))
         (sig (md-mermaid-live-job-sig job))
         (file (when ok (md-mermaid-live--read-process-first-line proc)))
         (cancel (process-get proc 'md-mermaid-cancelled)))
    (cond
     (cancel
      (md-mermaid-live--job-reset-overlay job)
      (md-mermaid-live--log-status job cancel))
     ((not ok)
      (when (md-mermaid-live--job-overlay-live-p job)
        (overlay-put ov 'after-string
                     (concat (propertize "\n" 'face 'error)
                             "md-mermaid: rendering failed (see *md-mermaid-live*)\n")))
      (md-mermaid-live--show-friendly-error job proc)
      (md-mermaid-live--job-reset-overlay job)
      (md-mermaid-live--log-status job (format "fail(%s)" exit-code)))
     ((not (and (md-mermaid-live--job-overlay-live-p job)
                (eq (overlay-get ov 'md-mermaid-job)
                    (md-mermaid-live-job-id job))
                (equal (overlay-get ov 'md-mermaid-target-sig) sig)
                (= (or (overlay-get ov 'md-mermaid-pos) -1)
                   (md-mermaid-live-job-pos job))
                file
                (file-exists-p file)))
      (md-mermaid-live--job-reset-overlay job)
      (md-mermaid-live--log-status job "stale" file))
     (t
      (md-mermaid-live--clear-error-for-job job)
      (md-mermaid-live--apply-image ov file sig)
      (md-mermaid-live--log-status job "ok" file)))
    (md-mermaid-live--drain-queue)
    (when (and (zerop (md-mermaid-live--active-jobs))
               md-mermaid-live--pending-refresh)
      (md-mermaid-live--kick t))))

(defun md-mermaid-live--job-sentinel (proc _event)
  "Handle PROC state change by cleaning up its job metadata."
  (let ((job (process-get proc 'md-mermaid-job)))
    (when job
      (md-mermaid-live--with-job-buffer job
        (setq md-mermaid-live--running (delq job md-mermaid-live--running))
        (md-mermaid-live--handle-job-exit job proc)))
    (when (buffer-live-p (process-buffer proc))
      (kill-buffer (process-buffer proc)))))

(defun md-mermaid-live--sig-prefix (sig)
  "Return the first eight characters of SIG, or SIG when shorter."
  (if (and (stringp sig) (>= (length sig) 8))
      (substring sig 0 8)
    sig))

(defun md-mermaid-live--log-raw (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to the md-mermaid log buffer."
  (with-current-buffer (get-buffer-create "*md-mermaid-live*")
    (goto-char (point-max))
    (insert (apply #'format format-string args))
    (unless (bolp)
      (insert "\n"))))

(defun md-mermaid-live--log-status (job status &optional file)
  "Log JOB STATUS along with FILE, if provided, to the md-mermaid log."
  (let* ((elapsed (when (md-mermaid-live-job-started job)
                    (- (float-time) (md-mermaid-live-job-started job))))
         (sig8 (md-mermaid-live--sig-prefix (md-mermaid-live-job-sig job))))
    (md-mermaid-live--log-raw "#%04d pos=%s sig=%s status=%s%s%s"
                              (md-mermaid-live-job-id job)
                              (or (md-mermaid-live-job-pos job) "-")
                              (or sig8 "-")
                              status
                              (if elapsed (format " t=%.2fs" elapsed) "")
                              (if file (format " file=%s" file) ""))))

(defun md-mermaid-live--debug (format-string &rest args)
  "Log FORMAT-STRING with ARGS only when `md-mermaid-live-debug' is non-nil."
  (when md-mermaid-live-debug
    (apply #'md-mermaid-live--log-raw format-string args)))

(defun md-mermaid-live--active-jobs ()
  "Return the count of live and queued md-mermaid-live jobs."
  (+ (length md-mermaid-live--queue) (length md-mermaid-live--running)))

(defun md-mermaid-live--maybe-init-cache ()
  "Ensure `md-mermaid-live--cache' is a hash table before use."
  (unless (hash-table-p md-mermaid-live--cache)
    (setq md-mermaid-live--cache (make-hash-table :test 'equal))))

(defun md-mermaid-live--cache-put (sig file)
  "Store FILE for SIG in the md-mermaid-live cache."
  (when (and (stringp sig) file)
    (md-mermaid-live--maybe-init-cache)
    (puthash sig (list :file file :time (float-time)) md-mermaid-live--cache)))

(defun md-mermaid-live--cache-get (sig)
  "Retrieve a cached entry for SIG if present."
  (when (and (hash-table-p md-mermaid-live--cache) (stringp sig))
    (gethash sig md-mermaid-live--cache)))

(defun md-mermaid-live--cache-apply (ov sig)
  "Apply cached image for OV when SIG has a stored file path."
  (let* ((entry (md-mermaid-live--cache-get sig))
         (file (plist-get entry :file)))
    (when (and file (file-exists-p file))
      (md-mermaid-live--apply-image ov file sig)
      (md-mermaid-live--debug "cache hit sig=%s → %s" (md-mermaid-live--sig-prefix sig) file)
      t)))

(defun md-mermaid-live--capture-scroll-setting (symbol)
  "Remember SYMBOL's current value in `md-mermaid-live--saved-scroll-settings'."
  (push (cons symbol (if (local-variable-p symbol)
                         (symbol-value symbol)
                       :global))
        md-mermaid-live--saved-scroll-settings))

(defun md-mermaid-live--apply-scroll-settings ()
  "Apply the configured scroll settings locally in the current buffer."
  (setq md-mermaid-live--saved-scroll-settings nil)
  (when md-mermaid-live-scroll-conservatively
    (md-mermaid-live--capture-scroll-setting 'scroll-conservatively)
    (setq-local scroll-conservatively md-mermaid-live-scroll-conservatively))
  (when md-mermaid-live-disable-auto-window-vscroll
    (md-mermaid-live--capture-scroll-setting 'auto-window-vscroll)
    (setq-local auto-window-vscroll nil)))

(defun md-mermaid-live--restore-scroll-settings ()
  "Restore scroll-related variables from `md-mermaid-live--saved-scroll-settings'."
  (dolist (entry md-mermaid-live--saved-scroll-settings)
    (let ((symbol (car entry))
          (value (cdr entry)))
      (if (eq value :global)
          (kill-local-variable symbol)
        (set (make-local-variable symbol) value))))
  (setq md-mermaid-live--saved-scroll-settings nil))

(defun md-mermaid-live--refresh-scroll-settings ()
  "Reapply scroll settings when values change in the active buffer."
  (when md-mermaid-live-mode
    (md-mermaid-live--restore-scroll-settings)
    (md-mermaid-live--apply-scroll-settings)))

(defun md-mermaid-live--start-monitor ()
  "Start the job timeout monitor if it is not already running."
  (unless (or md-mermaid-live--monitor-timer (not md-mermaid-live-job-timeout))
    (setq md-mermaid-live--monitor-timer
          (run-with-timer 1 1 #'md-mermaid-live--check-timeouts (current-buffer)))))

(defun md-mermaid-live--stop-monitor ()
  "Cancel the job timeout monitor when it is active."
  (when md-mermaid-live--monitor-timer
    (cancel-timer md-mermaid-live--monitor-timer)
    (setq md-mermaid-live--monitor-timer nil)))

(defun md-mermaid-live--check-timeouts (buf)
  "Check BUF for stale jobs and cancel them when deadlines pass."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and md-mermaid-live-mode md-mermaid-live-job-timeout)
        (let ((now (float-time)))
          (dolist (job (copy-sequence md-mermaid-live--running))
            (let ((started (md-mermaid-live-job-started job)))
              (when (and started (> (- now started) md-mermaid-live-job-timeout))
                (md-mermaid-live--debug "timeout #%04d (%.2fs)" (md-mermaid-live-job-id job) (- now started))
                (md-mermaid-live--cancel-running-job job "timeout")
                (md-mermaid-live--retry-job job "timeout")))))))))

(defun md-mermaid-live--prune-overlays (blocks range)
  "Remove overlays that no longer correspond to BLOCKS within RANGE.
Overlays outside RANGE are left intact so off-screen diagrams stay rendered.
When scrolling upward, preserve overlays just above the visible region
to reduce flicker."
  (let* ((beg (or (car-safe range) (point-min)))
         (end (or (cdr-safe range) (point-max)))
         ;; If scrolling upward, expand the safe zone above the range
         (is-scrolling-up md-mermaid-live--scroll-direction)
         (safe-above (if is-scrolling-up 3000 0)))
    (dolist (ov (copy-sequence md-mermaid-live--overlays))
      (let ((pos (overlay-get ov 'md-mermaid-pos)))
        (cond
         ((not (overlay-buffer ov))
          (setq md-mermaid-live--overlays (delq ov md-mermaid-live--overlays)))
         ;; Only prune overlays well outside the safe range
         ((and pos (>= pos (+ beg safe-above)) (<= pos end)
               (not (seq-some (lambda (b) (= (plist-get b :pos) pos)) blocks)))
          (let* ((code-pos (overlay-get ov 'md-mermaid-code-pos))
                 (has-code-overlay
                  (and code-pos
                       (seq-some (lambda (cov)
                                   (and (overlay-buffer cov)
                                        (= (overlay-get cov 'md-mermaid-code-pos) code-pos)))
                                 md-mermaid-live--code-overlays))))
            (unless has-code-overlay
              (md-mermaid-live--cancel-jobs-for-overlay ov "prune")
              (delete-overlay ov)
              (setq md-mermaid-live--overlays (delq ov md-mermaid-live--overlays))))))))))

(defun md-mermaid-live--prune-code-overlays (&optional blocks)
  "Remove code overlays that no longer correspond to scanned BLOCKS."
  (let* ((positions (and blocks (mapcar (lambda (b) (plist-get b :beg)) blocks)))
         (keep nil))
    (dolist (ov md-mermaid-live--code-overlays)
      (if (and (overlay-buffer ov)
               (or (null positions)
                   (member (overlay-get ov 'md-mermaid-code-pos) positions)))
          (push ov keep)
        (when (overlayp ov)
          (delete-overlay ov))))
    (setq md-mermaid-live--code-overlays (nreverse keep)))
  (unless (and md-mermaid-live--hide-code
               (seq-some (lambda (ov)
                           (and (overlay-buffer ov)
                                (overlay-get ov 'md-mermaid-live-hidden)))
                         md-mermaid-live--code-overlays))
    (md-mermaid-live--maybe-remove-invisibility)))

(defun md-mermaid-live--buffer-line-count ()
  "Return the total number of lines in the current buffer."
  (count-lines (point-min) (point-max)))

(defun md-mermaid-live--count-fences ()
  "Count how many Mermaid fences appear between the buffer limits."
  (length (md-mermaid-live--scan (point-min) (point-max))))

(defun md-mermaid-live--large-buffer-message ()
  "Warn when the buffer is too large so live refresh pauses temporarily.
Messages mention the REASON and COUNT stored in `md-mermaid-live--large-buffer'."
  (when (and (listp md-mermaid-live--large-buffer)
             (not (plist-get md-mermaid-live--large-buffer :notified)))
    (let ((reason (plist-get md-mermaid-live--large-buffer :reason))
          (count (plist-get md-mermaid-live--large-buffer :count)))
      (message "md-mermaid-live: buffer too large (%s=%s); live refresh paused. Use `md-mermaid-live-render-visible' or adjust limits."
               (symbol-name reason) count)
      (setq md-mermaid-live--large-buffer
            (plist-put md-mermaid-live--large-buffer :notified t)))))

(defun md-mermaid-live--guard-large-buffer (force)
  "Return non-nil when live refresh should be skipped due to size limits.
When FORCE is non-nil, bypass the guard."
  (cond
   (force nil)
   ((plist-get md-mermaid-live--large-buffer :reason)
    (md-mermaid-live--large-buffer-message)
    t)
   (t
    (let ((line-limit md-mermaid-live-max-lines)
          (fence-limit md-mermaid-live-max-fences)
          (result nil))
      (when line-limit
        (let ((lc (md-mermaid-live--buffer-line-count)))
          (when (> lc line-limit)
            (setq md-mermaid-live--large-buffer
                  (list :reason 'lines :count lc))
            (md-mermaid-live--large-buffer-message)
            (setq result t))))
      (when (and (not result) fence-limit)
        (let ((n (md-mermaid-live--count-fences)))
          (when (> n fence-limit)
            (setq md-mermaid-live--large-buffer
                  (list :reason 'fences :count n))
            (md-mermaid-live--large-buffer-message)
            (setq result t))))
      result))))

(defun md-mermaid-live--bootstrap (&optional context)
  "Seed overlays across the buffer.  CONTEXT describes why the bootstrap run."
  (let ((ctx (or context "enable")))
    (setq md-mermaid-live--bootstrap-ran nil)
    (if (md-mermaid-live--guard-large-buffer nil)
        (let* ((reason (plist-get md-mermaid-live--large-buffer :reason))
               (count (plist-get md-mermaid-live--large-buffer :count)))
          (md-mermaid-live--log-raw "bootstrap skipped ctx=%s reason=%s count=%s"
                                    ctx reason count)
          nil)
      (let ((md-mermaid-live-scope 'buffer))
        (md-mermaid-live-refresh t)
        (setq md-mermaid-live--bootstrap-ran t)
        t))))

(defun md-mermaid-live--make-job (block ov)
  "Build a job record from BLOCK and overlay OV for live rendering."
  (let* ((sig (plist-get block :sig))
         (pos (plist-get block :end))
         (job (make-md-mermaid-live-job
               :id md-mermaid-live--next-job
               :block block
               :overlay ov
               :sig sig
               :pos pos
               :buffer (current-buffer))))
    (cl-incf md-mermaid-live--next-job)
    job))

(defun md-mermaid-live--job-in-range-p (job beg end)
  "Return non-nil when JOB lies within the span BEG..END."
  (let* ((block (md-mermaid-live-job-block job))
         (start (plist-get block :beg))
         (finish (plist-get block :end)))
    (and start finish
         (< start end)
         (> finish beg))))

(defun md-mermaid-live--job-overlay-live-p (job)
  "Return non-nil when JOB's overlay still exists and is visible."
  (let ((ov (md-mermaid-live-job-overlay job)))
    (and (overlayp ov)
         (overlay-buffer ov)
         (eq (overlay-buffer ov) (md-mermaid-live-job-buffer job)))))

(defun md-mermaid-live--job-reset-overlay (job)
  "Reset the overlay markers associated with JOB."
  (let ((ov (md-mermaid-live-job-overlay job)))
    (when (overlayp ov)
      (overlay-put ov 'md-mermaid-job nil)
      (overlay-put ov 'md-mermaid-target-sig nil)
      (overlay-put ov 'md-mermaid-job-start nil)
      (overlay-put ov 'md-mermaid-pending nil))))

(defun md-mermaid-live--cancel-running-job (job reason)
  "Cancel JOB's running process with optional REASON."
  (let ((proc (md-mermaid-live-job-proc job)))
    (when (process-live-p proc)
      (process-put proc 'md-mermaid-cancelled (or reason "cancel"))
      (md-mermaid-live--debug "cancel #%04d (%s)" (md-mermaid-live-job-id job) reason)
      (delete-process proc)))
  (setq md-mermaid-live--running (delq job md-mermaid-live--running))
  (md-mermaid-live--job-reset-overlay job))

(defun md-mermaid-live--cancel-queued-job (job reason)
  "Drop queued JOB for REASON and reset its overlay."
  (setq md-mermaid-live--queue (delq job md-mermaid-live--queue))
  (md-mermaid-live--job-reset-overlay job)
  (md-mermaid-live--debug "drop queued #%04d (%s)" (md-mermaid-live-job-id job) reason))

(defun md-mermaid-live--cancel-jobs-in-range (beg end)
  "Cancel pending and running jobs whose ranges overlap BEG..END."
  (let ((dropped 0))
    (dolist (job (copy-sequence md-mermaid-live--queue))
      (when (md-mermaid-live--job-in-range-p job beg end)
        (cl-incf dropped)
        (md-mermaid-live--cancel-queued-job job "edit")))
    (dolist (job (copy-sequence md-mermaid-live--running))
      (when (md-mermaid-live--job-in-range-p job beg end)
        (cl-incf dropped)
        (md-mermaid-live--cancel-running-job job "edit")))
    dropped))

(defun md-mermaid-live--cancel-jobs-for-overlay (ov reason)
  "Cancel jobs whose overlay matches OV, logging REASON."
  (dolist (job (copy-sequence md-mermaid-live--queue))
    (when (eq (md-mermaid-live-job-overlay job) ov)
      (md-mermaid-live--cancel-queued-job job reason)))
  (dolist (job (copy-sequence md-mermaid-live--running))
    (when (eq (md-mermaid-live-job-overlay job) ov)
      (md-mermaid-live--cancel-running-job job reason))))

(defun md-mermaid-live--retry-job (job reason)
  "Retry JOB after logging REASON, or abort if attempts exceed the limit."
  (let* ((block (md-mermaid-live-job-block job))
         (ov (md-mermaid-live-job-overlay job)))
    (when (and block (overlayp ov))
      (let* ((attempt (or (overlay-get ov 'md-mermaid-attempt) 0))
             (next (1+ attempt)))
        (if (and md-mermaid-live-max-attempts
                 (> next md-mermaid-live-max-attempts))
            (progn
              (overlay-put ov 'after-string
                           (concat (propertize "\n" 'face 'error)
                                   "md-mermaid: rendering timed out; giving up.\n"))
              (md-mermaid-live--log-status job (format "%s-abort" reason)))
          (let ((new (md-mermaid-live--make-job block ov)))
            (md-mermaid-live--enqueue-job new next)
            (md-mermaid-live--log-status new (format "%s-retry" reason))
            (md-mermaid-live--kick t)))))))

(defun md-mermaid-live--enqueue-job (job &optional attempt)
  "Add JOB to the queue, track ATTEMPT, and update its overlay metadata."
  (setq md-mermaid-live--queue
        (if md-mermaid-live--queue
            (nconc md-mermaid-live--queue (list job))
          (list job)))
  (let ((ov (md-mermaid-live-job-overlay job)))
    (let* ((prev (or (overlay-get ov 'md-mermaid-attempt) 0))
           (next (or attempt (1+ prev))))
      (overlay-put ov 'md-mermaid-attempt next))
    (overlay-put ov 'md-mermaid-pending (md-mermaid-live-job-id job))
    (overlay-put ov 'md-mermaid-target-sig (md-mermaid-live-job-sig job))
    (overlay-put ov 'md-mermaid-job nil)
    (overlay-put ov 'md-mermaid-job-start nil))
  (md-mermaid-live--debug "enqueue #%04d pos=%s sig=%s"
                          (md-mermaid-live-job-id job)
                          (or (md-mermaid-live-job-pos job) "-")
                          (md-mermaid-live--sig-prefix (md-mermaid-live-job-sig job))))

(defvar-local md-mermaid-live--next-job 1)
(defvar-local md-mermaid-live--large-buffer nil)

(defun md-mermaid-live--visible-range ()
  "Return cons of (beg . end) for current window, widened with adaptive margin.
Detects scroll direction and expands prefetch to prevent upward-scroll flicker."
  (let* ((w (selected-window))
         (beg (window-start w))
         (end (window-end w t))
         (curr-pos (or (window-start w) (point-min)))
         ;; Detect scroll direction: upward = negative delta
         (is-scrolling-up (and md-mermaid-live--last-scroll-pos
                               (< curr-pos md-mermaid-live--last-scroll-pos)))
         ;; Use larger margin when scrolling upward to prevent flicker
         (base-pad 2000)
         (pad (if is-scrolling-up 3000 base-pad)))
    (cons (max (point-min) (- beg pad)) (min (point-max) (+ end pad)))))

(defconst md-mermaid-live--ts-fence-query
  '((fenced_code_block) @fence)
  "Tree-sitter query capturing fenced code blocks.")

(defun md-mermaid-live--treesit-available-p ()
  "Return non-nil when Tree-sitter markdown parsing is available in this Emacs."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p 'markdown)))

(defun md-mermaid-live--ensure-ts-parser ()
  "Return a markdown parser, creating one if none is cached."
  (when (md-mermaid-live--treesit-available-p)
    (unless (and (treesit-parser-p md-mermaid-live--ts-parser)
                 (treesit-parser-language md-mermaid-live--ts-parser))
      (let ((existing (seq-find (lambda (parser)
                                  (eq (treesit-parser-language parser) 'markdown))
                                (treesit-parser-list))))
        (setq md-mermaid-live--ts-parser
              (or existing
                  (condition-case err
                      (treesit-parser-create 'markdown)
                    (error
                     (md-mermaid-live--debug "treesit parser error: %s" err)
                     nil))))))
    md-mermaid-live--ts-parser))

(defun md-mermaid-live--ts-node-mermaid-p (node)
  "Return non-nil when NODE corresponds to a mermaid code fence line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (string-match-p "^[\\t ]*```\\s-*mermaid\\b" line))))

(defun md-mermaid-live--ts-build-block (node opt)
  "Build a block plist from NODE using OPT to keep signature metadata."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (code-beg (save-excursion
                     (goto-char beg)
                     (forward-line 1)
                     (point)))
         (closing-beg (save-excursion
                        (goto-char end)
                        (forward-line -1)
                        (while (and (> (point) beg)
                                    (not (looking-at "^[\\t ]*```\\s-*$")))
                          (forward-line -1))
                        (point)))
         (valid-close (save-excursion
                         (goto-char closing-beg)
                         (looking-at "^[\\t ]*```")))
         (code-end (max beg (min closing-beg end)))
         (indent (save-excursion
                   (goto-char beg)
                   (buffer-substring-no-properties (line-beginning-position) beg)))
         (fence-line (save-excursion
                       (goto-char beg)
                       (buffer-substring-no-properties (point)
                                                       (line-end-position))))
         (closing-line (save-excursion
                         (goto-char code-end)
                         (buffer-substring-no-properties (point)
                                                         (line-end-position))))
         (content (buffer-substring-no-properties code-beg code-end))
         (code-lines (max 1 (count-lines code-beg code-end)))
         (sig (secure-hash 'sha1 (concat content "|" opt)))
         (pos (save-excursion
                (goto-char code-end)
                (line-end-position)))
         (block-end (save-excursion
                      (goto-char end)
                      (line-end-position))))
    (when (and valid-close (<= code-beg code-end))
      (list :beg beg :code-beg code-beg :code-end code-end
            :end block-end :pos pos
            :indent indent :fence-line fence-line :closing-line closing-line
            :content content :sig sig :code-lines code-lines))))

(defun md-mermaid-live--scan-ts (beg end opt)
  "Scan the region BEG..END via Tree-sitter and return block plists using OPT."
  (let ((parser (md-mermaid-live--ensure-ts-parser)))
    (if (not parser)
        :ts-unavailable
      (condition-case err
          (let* ((root (treesit-parser-root-node parser))
                 (captures (treesit-query-capture root md-mermaid-live--ts-fence-query
                                                  beg end))
                 (blocks nil))
            (dolist (capture captures)
              (pcase-let ((`(,name . ,node) capture))
                (when (and (eq name 'fence)
                           (md-mermaid-live--ts-node-mermaid-p node))
                  (when-let ((block (md-mermaid-live--ts-build-block node opt)))
                    (push block blocks)))))
            (nreverse blocks))
        (error
         (md-mermaid-live--debug "treesit scan error: %s" err)
         (setq md-mermaid-live--ts-parser nil)
         :ts-error)))))

(defun md-mermaid-live--scan-regex (beg end opt)
  "Fallback to regex scanning between BEG and END using OPT.
when Tree-sitter is unavailable."
  (save-excursion
    (save-restriction
      (narrow-to-region (or beg (point-min)) (or end (point-max)))
      (goto-char (point-min))
      (let* ((re-start "^[\\t ]*```\\s-*mermaid\\b.*$")
             (re-end   "^[\\t ]*```\\s-*$")
             (blocks nil))
        (while (re-search-forward re-start nil t)
          (let* ((fence-beg (match-beginning 0))
                 (fence-line (buffer-substring-no-properties fence-beg (line-end-position)))
                 (indent (buffer-substring-no-properties (line-beginning-position) fence-beg))
                 (code-beg (progn (forward-line 1) (point)))
                 (close-pos (and (re-search-forward re-end nil t) (match-beginning 0))))
            (when close-pos
              (let* ((code-end close-pos)
                     (closing-line (buffer-substring-no-properties close-pos (line-end-position)))
                     (content (buffer-substring-no-properties code-beg code-end))
                     (code-lines (max 1 (count-lines code-beg code-end)))
                     (sig (secure-hash 'sha1 (concat content "|" opt))))
                (push (list :beg fence-beg :code-beg code-beg :code-end code-end
                            :end (match-end 0) :pos (match-end 0)
                            :indent indent :fence-line fence-line :closing-line closing-line
                            :content content :sig sig :code-lines code-lines)
                      blocks)))))
        (nreverse blocks)))))

(defun md-mermaid-live--scan (&optional beg end)
  "Return a list of block plists between BEG and END.
Each plist contains keys :beg, :code-beg, :code-end, :end, :content, and :sig.
SIG is a content+options digest used to skip re-renders."
  (let ((opt (format "W=%s|B=%s|T=%s" md-mermaid-live-width
                     md-mermaid-live-background
                     (or md-mermaid-live-theme "default"))))
    (let ((ts-blocks (md-mermaid-live--scan-ts beg end opt)))
      (cond
       ((or (eq ts-blocks :ts-unavailable)
            (eq ts-blocks :ts-error))
        (md-mermaid-live--debug "treesit fallback (%s)"
                                (if (eq ts-blocks :ts-unavailable) 'unavailable 'error))
        (md-mermaid-live--scan-regex beg end opt))
       (ts-blocks ts-blocks)
       (t
        (md-mermaid-live--scan-regex beg end opt))))))

(defun md-mermaid-live--assets-dir ()
  "Return the assets directory under the resolved project root."
  (let* ((root (if (fboundp 'md-mermaid--project-root)
                   (md-mermaid--project-root)
                 default-directory))
         (dir (or md-mermaid-live-assets-dir
                  (and (boundp 'md-mermaid-assets-dir) md-mermaid-assets-dir)
                  "assets/mermaid")))
    (expand-file-name dir root)))

(defun md-mermaid-live--maybe-add-invisibility ()
  "Ensure `md-mermaid-live-code' is present in `invisibility-spec'."
  (unless md-mermaid-live--invisibility-added
    (add-to-invisibility-spec 'md-mermaid-live-code)
    (setq md-mermaid-live--invisibility-added t)))

(defun md-mermaid-live--maybe-remove-invisibility ()
  "Remove the md-mermaid-live-code entry from `invisibility-spec'."
  (when md-mermaid-live--invisibility-added
    (remove-from-invisibility-spec 'md-mermaid-live-code)
    (setq md-mermaid-live--invisibility-added nil)))

(defun md-mermaid-live--code-placeholder (ov)
  "Build a placeholder string for OV encoding the mermaid fence text."
  (let* ((indent (or (overlay-get ov 'md-mermaid-indent) ""))
         (open (or (overlay-get ov 'md-mermaid-fence-line)
                   (concat indent "```mermaid")))
         (close (or (overlay-get ov 'md-mermaid-closing-line)
                    (concat indent "```")))
         (body (split-string md-mermaid-live-hidden-placeholder "\n")))
    (concat
     (propertize open 'face 'shadow) "\n"
     (mapconcat (lambda (line)
                  (propertize (concat indent line) 'face 'shadow))
                body
                "\n")
     "\n"
     (propertize close 'face 'shadow)
     "\n")))

(defun md-mermaid-live--maybe-apply-placeholder-height (ov)
  "Apply line-height to OV when a hidden placeholder limits height."
  (let* ((lines (overlay-get ov 'md-mermaid-code-lines))
         (limit md-mermaid-live-hidden-max-height)
         (height (and limit lines (max 1 (min limit lines)))))
    (overlay-put ov 'line-height height)))

(defun md-mermaid-live--refresh-code-placeholder (ov)
  "Update OV's stored placeholder and inject it when hidden."
  (let ((placeholder (md-mermaid-live--code-placeholder ov)))
    (unless (equal placeholder (overlay-get ov 'md-mermaid-live-placeholder))
      (overlay-put ov 'md-mermaid-live-placeholder placeholder)
      (when (overlay-get ov 'md-mermaid-live-hidden)
        (overlay-put ov 'before-string placeholder)))))

(defun md-mermaid-live--hide-code-overlay (ov)
  "Hide overlay OV by marking it invisible and showing the placeholder."
  (unless (overlay-get ov 'md-mermaid-live-hidden)
    (md-mermaid-live--maybe-add-invisibility)
    (overlay-put ov 'invisible 'md-mermaid-live-code)
    (md-mermaid-live--refresh-code-placeholder ov)
    (overlay-put ov 'before-string (overlay-get ov 'md-mermaid-live-placeholder))
    (md-mermaid-live--maybe-apply-placeholder-height ov)
    (overlay-put ov 'md-mermaid-live-hidden t)))

(defun md-mermaid-live--show-code-overlay (ov)
  "Reveal overlay OV and clear the placeholder decorations."
  (when (overlay-get ov 'md-mermaid-live-hidden)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'line-height nil)
    (overlay-put ov 'md-mermaid-live-hidden nil)))

(defun md-mermaid-live--apply-code-visibility (&optional overlays)
  "Enforce code visibility for OVERLAYS or the cached code overlays.
When `md-mermaid-live--hide-code' is set, hide each overlay.
and track the invisibility spec."
  (let* ((ovs (or overlays md-mermaid-live--code-overlays))
         (live-ovs nil)
         (any-hidden nil))
    (dolist (ov ovs)
      (when (overlay-buffer ov)
        (push ov live-ovs)
        (md-mermaid-live--refresh-code-placeholder ov)
        (if md-mermaid-live--hide-code
            (progn
              (md-mermaid-live--hide-code-overlay ov)
              (setq any-hidden t))
          (md-mermaid-live--show-code-overlay ov))))
    (setq md-mermaid-live--code-overlays (nreverse live-ovs))
    (if (and md-mermaid-live--hide-code any-hidden)
        (md-mermaid-live--maybe-add-invisibility)
      (md-mermaid-live--maybe-remove-invisibility))))

(defun md-mermaid-live--ensure-code-overlay (block)
  "Return the code overlay for BLOCK, creating it when absent."
  (let* ((beg (plist-get block :beg))
         (end (plist-get block :end))
         (pos beg)
         (ov (seq-find (lambda (o)
                         (and (overlay-buffer o)
                              (overlay-get o 'md-mermaid-live-code)
                              (= (or (overlay-get o 'md-mermaid-code-pos) -1) pos)))
                       md-mermaid-live--code-overlays)))
    (unless ov
      (setq ov (make-overlay beg end nil nil t))
      (overlay-put ov 'md-mermaid-live-code t)
      (overlay-put ov 'md-mermaid-code-pos pos)
      (push ov md-mermaid-live--code-overlays))
    (move-overlay ov beg end)
    (overlay-put ov 'md-mermaid-code-pos pos)
    (overlay-put ov 'md-mermaid-indent (plist-get block :indent))
    (overlay-put ov 'md-mermaid-fence-line (plist-get block :fence-line))
    (overlay-put ov 'md-mermaid-closing-line (plist-get block :closing-line))
    (overlay-put ov 'md-mermaid-code-lines (plist-get block :code-lines))
    (md-mermaid-live--refresh-code-placeholder ov)
    (if md-mermaid-live--hide-code
        (md-mermaid-live--hide-code-overlay ov)
      (md-mermaid-live--show-code-overlay ov))
    ov))

(defun md-mermaid-live--ensure-overlay (block)
  "Return an overlay anchored at end of BLOCK; create if necessary.  \nAvoid `overlays-at` for zero-length overlays (it can miss them).  \nWe track our own overlay list and a stored marker position."
  (let* ((pos (plist-get block :pos))
         (ov (seq-find (lambda (o)
                         (and (overlay-buffer o)
                              (overlay-get o 'md-mermaid-live)
                              (= (or (overlay-get o 'md-mermaid-pos) -1) pos)))
                       md-mermaid-live--overlays)))
    (unless ov
      ;; Try to reuse any old live overlay at this position
      (let ((old (seq-find (lambda (o)
                             (and (overlay-buffer o)
                                  (overlay-get o 'md-mermaid-live)
                                  (= (overlay-start o) pos)
                                  (= (overlay-end o) pos)))
                           (overlays-at pos))))
        (when old
          (setq ov old)
          (overlay-put ov 'md-mermaid-pos pos)
          (unless (memq ov md-mermaid-live--overlays)
            (push ov md-mermaid-live--overlays)))))
    (unless ov
      (setq ov (make-overlay pos pos nil t t))
      (overlay-put ov 'md-mermaid-live t)
      (overlay-put ov 'md-mermaid-pos pos)
      (overlay-put ov 'md-mermaid-code-pos (plist-get block :beg))
      (overlay-put ov 'md-mermaid-job nil)
      (overlay-put ov 'md-mermaid-target-sig nil)
      (overlay-put ov 'md-mermaid-job-start nil)
      (overlay-put ov 'md-mermaid-sig nil)
      (overlay-put ov 'md-mermaid-file nil)
      (overlay-put ov 'md-mermaid-pending nil)
      (push ov md-mermaid-live--overlays))
    (overlay-put ov 'md-mermaid-pos pos)
    (overlay-put ov 'md-mermaid-code-pos (plist-get block :beg))
    ov))

(defun md-mermaid-live--dedupe-at (pos keep)
  "Delete any extra md-mermaid-live overlays at POS except KEEP."
  (dolist (o (overlays-at pos))
    (when (and (overlay-get o 'md-mermaid-live) (not (eq o keep)))
      (delete-overlay o)
      (setq md-mermaid-live--overlays (delq o md-mermaid-live--overlays)))))

(defun md-mermaid-live--apply-image (ov file sig)
  "Attach FILE image to overlay OV and update metadata for SIG."
  (when (and (overlay-buffer ov) (file-exists-p file))
    (let* ((img (create-image file nil nil :max-width md-mermaid-live-width))
           (str (propertize "\n" 'display img)))
      (overlay-put ov 'after-string (concat str "\n"))
      (overlay-put ov 'md-mermaid-sig sig)
      (overlay-put ov 'md-mermaid-file file)
      (overlay-put ov 'md-mermaid-job nil)
      (overlay-put ov 'md-mermaid-target-sig nil)
      (overlay-put ov 'md-mermaid-job-start nil)
      (overlay-put ov 'md-mermaid-pending nil)
      (overlay-put ov 'md-mermaid-attempt nil)
      (md-mermaid-live--cache-put sig file)
      t)))

(defun md-mermaid-live--start-job (job)
  "Start asynchronous render JOB and update its overlay on completion."
  (if (not (md-mermaid-live--job-overlay-live-p job))
      (progn
        (md-mermaid-live--debug "job #%04d skipped (overlay missing)" (md-mermaid-live-job-id job))
        (md-mermaid-live--job-reset-overlay job)
        nil)
    (let* ((command (md-mermaid-live--job-command job))
           (block (md-mermaid-live-job-block job))
           (ov (md-mermaid-live-job-overlay job))
           (sig (md-mermaid-live-job-sig job)))
      (if (not command)
          (progn
            (md-mermaid-live--job-reset-overlay job)
            (when (md-mermaid-live--job-overlay-live-p job)
              (overlay-put ov 'after-string
                           (concat (propertize "\n" 'face 'error)
                                   "md-mermaid: snippet renderer not found\n")))
            (md-mermaid-live--log-status job "no-script")
            nil)
        (let* ((python (plist-get command :python))
               (args (plist-get command :args))
               (name (plist-get command :name))
               (proc (md-mermaid-live--spawn-job-process job python args name))
               (existing-file (overlay-get ov 'md-mermaid-file)))
          (setf (md-mermaid-live-job-proc job) proc
                (md-mermaid-live-job-started job) (float-time))
          (setq md-mermaid-live--running (delq job md-mermaid-live--running))
          (push job md-mermaid-live--running)
          (overlay-put ov 'md-mermaid-job (md-mermaid-live-job-id job))
          (overlay-put ov 'md-mermaid-target-sig sig)
          (overlay-put ov 'md-mermaid-job-start (md-mermaid-live-job-started job))
          (overlay-put ov 'md-mermaid-pending (md-mermaid-live-job-id job))
          (unless existing-file
            (overlay-put ov 'after-string
                         (concat (propertize "\n" 'face 'shadow)
                                 (format "Rendering #%04d %s…\n"
                                         (md-mermaid-live-job-id job)
                                         (or (md-mermaid-live--sig-prefix sig) "")))))
          (process-send-string proc (plist-get block :content))
          (process-send-eof proc)
          (md-mermaid-live--debug "start #%04d pos=%s sig=%s"
                                  (md-mermaid-live-job-id job)
                                  (or (md-mermaid-live-job-pos job) "-")
                                  (md-mermaid-live--sig-prefix sig))
          (md-mermaid-live--log-status job "start")
          job)))))

(defun md-mermaid-live--drain-queue ()
  "Start queued jobs up to `md-mermaid-live-max-procs'."
  (while (and md-mermaid-live--queue
              (< (length md-mermaid-live--running) md-mermaid-live-max-procs))
    (let ((job (car md-mermaid-live--queue)))
      (setq md-mermaid-live--queue (cdr md-mermaid-live--queue))
      (if (md-mermaid-live--job-overlay-live-p job)
          (md-mermaid-live--start-job job)
        (md-mermaid-live--debug "skip stale queue #%04d" (md-mermaid-live-job-id job))))))

(defun md-mermaid-live-refresh (&optional force)
  "Scan for fences and enqueue renders for stale overlays.
With prefix FORCE, bypass size guards to refresh immediately."
  (interactive "P")
  (setq md-mermaid-live--pending-refresh nil)
  (if (md-mermaid-live--guard-large-buffer force)
      (md-mermaid-live--debug "refresh skipped: size guard")
    (let* ((range (if (eq md-mermaid-live-scope 'visible)
                      (md-mermaid-live--visible-range)
                    (cons (point-min) (point-max))))
           (current-tick (buffer-modified-tick)))
      (if (and (not force)
               md-mermaid-live--last-tick
               (= current-tick md-mermaid-live--last-tick)
               (= (car range) (or md-mermaid-live--last-window-start -1))
               (= (cdr range) (or md-mermaid-live--last-window-end -1)))
          (progn
            (md-mermaid-live--debug "refresh skipped: window unchanged")
            (md-mermaid-live--drain-queue))
        (let ((blocks (md-mermaid-live--scan (car range) (cdr range)))
              (processed 0)
              (deferred nil))
          (md-mermaid-live--prune-overlays blocks range)
          (dolist (block blocks)
            (if (and md-mermaid-live-refresh-limit
                     (>= processed md-mermaid-live-refresh-limit))
                (setq deferred t)
              (setq processed (1+ processed))
              (let* ((_code-ov (md-mermaid-live--ensure-code-overlay block))
                     (ov (md-mermaid-live--ensure-overlay block))
                     (sig (plist-get block :sig))
                     (pos (plist-get block :pos))
                     (current (overlay-get ov 'md-mermaid-sig))
                     (target (overlay-get ov 'md-mermaid-target-sig)))
                (md-mermaid-live--dedupe-at pos ov)
                (unless (or (and current (string= current sig))
                            (and target (string= target sig)))
                  (when (md-mermaid-live--cache-apply ov sig)
                    (setq current sig)))
                (cond
                 ((and current (string= current sig))
                  nil)
                 ((and target (string= target sig))
                  nil)
                 (t
                  (md-mermaid-live--cancel-jobs-for-overlay ov "update")
                  (let ((job (md-mermaid-live--make-job block ov)))
                    (md-mermaid-live--enqueue-job job)))))))
          (md-mermaid-live--prune-code-overlays blocks)
          (if deferred
              (progn
                (md-mermaid-live--debug "refresh deferred after %d fences" processed)
                (setq md-mermaid-live--last-tick nil
                      md-mermaid-live--last-window-start nil
                      md-mermaid-live--last-window-end nil)
                (md-mermaid-live--kick t))
            (setq md-mermaid-live--last-tick current-tick
                  md-mermaid-live--last-window-start (car range)
                  md-mermaid-live--last-window-end (cdr range)))
          (md-mermaid-live--drain-queue))))))

(defun md-mermaid-live-clear ()
  "Remove all live overlays in the current buffer."
  (interactive)
  (when md-mermaid-live--timer
    (cancel-timer md-mermaid-live--timer)
    (setq md-mermaid-live--timer nil))
  (setq md-mermaid-live--pending-refresh nil)
  (setq md-mermaid-live--large-buffer nil)
  (setq md-mermaid-live--bootstrap-ran nil)
  (setq md-mermaid-live--last-tick nil
        md-mermaid-live--last-window-start nil
        md-mermaid-live--last-window-end nil
        md-mermaid-live--last-scroll-pos nil
        md-mermaid-live--scroll-direction nil)
  ;; Remove tracked overlays
  (dolist (ov (prog1 md-mermaid-live--overlays (setq md-mermaid-live--overlays nil)))
    (when (overlayp ov) (delete-overlay ov)))
  (dolist (ov (prog1 md-mermaid-live--code-overlays (setq md-mermaid-live--code-overlays nil)))
    (when (overlayp ov) (delete-overlay ov)))
  (md-mermaid-live--maybe-remove-invisibility)
  ;; Remove any stray live overlays not in our list (from older sessions)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'md-mermaid-live)
      (delete-overlay ov)))
  (dolist (job (copy-sequence md-mermaid-live--running))
    (md-mermaid-live--cancel-running-job job "clear"))
  (setq md-mermaid-live--running nil)
  (dolist (job (copy-sequence md-mermaid-live--queue))
    (md-mermaid-live--cancel-queued-job job "clear"))
  (setq md-mermaid-live--queue nil)
  (setq md-mermaid-live--ts-parser nil))

(defun md-mermaid-live--schedule-refresh (delay)
  "Schedule an idle timer to trigger after DELAY seconds.
Cancels any existing timer before scheduling."
  (when md-mermaid-live--timer
    (cancel-timer md-mermaid-live--timer)
    (setq md-mermaid-live--timer nil))
  (setq md-mermaid-live--timer
        (run-with-idle-timer delay nil #'md-mermaid-live--idle-trigger (current-buffer))))

(defun md-mermaid-live--idle-trigger (buf)
  "Run when Emacs has been idle.  Refresh if BUF has pending work."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq md-mermaid-live--timer nil)
      ;; Clear scroll direction flag after idle period to allow normal pruning
      (setq md-mermaid-live--scroll-direction nil)
      (when (and md-mermaid-live-mode md-mermaid-live--pending-refresh)
        (if (> (md-mermaid-live--active-jobs) 0)
            (md-mermaid-live--schedule-refresh (* 1.5 md-mermaid-live-idle-delay))
          (md-mermaid-live-refresh))))))

(defun md-mermaid-live--kick (&optional immediate reason)
  "Schedule a refresh, coalescing when jobs are running.
When IMMEDIATE is non-nil, run soon even if jobs just finished.
REASON can be `edit' or `scroll' to adjust debounce timers."
  (setq md-mermaid-live--pending-refresh t)
  (let* ((active (md-mermaid-live--active-jobs))
         (delay (cond
                 (immediate 0.05)
                 ((eq reason 'edit)
                  (min md-mermaid-live-idle-delay md-mermaid-live-edit-delay))
                 ((eq reason 'scroll)
                  (or md-mermaid-live-scroll-delay md-mermaid-live-idle-delay))
                 ((> active 0) (* 1.5 md-mermaid-live-idle-delay))
                 (t md-mermaid-live-idle-delay))))
    (md-mermaid-live--schedule-refresh delay)))

(defun md-mermaid-live--after-change (beg end _len)
  "React to buffer edits occurring between BEG and END."
  (when md-mermaid-live-mode
    (when (> (md-mermaid-live--cancel-jobs-in-range beg end) 0)
      (md-mermaid-live--debug "cancelled jobs in %s-%s" beg end))
    (md-mermaid-live--kick nil 'edit)))

(defun md-mermaid-live--on-scroll (_win _start)
  "Track scroll direction for live pruning whenever the window scrolls."
  (when md-mermaid-live-mode
    ;; Track scroll direction for adaptive pruning
    (let* ((curr-pos (window-start (selected-window)))
           (prev-pos (or md-mermaid-live--last-scroll-pos curr-pos)))
      (setq md-mermaid-live--scroll-direction (< curr-pos prev-pos))
      (setq md-mermaid-live--last-scroll-pos curr-pos))
    (md-mermaid-live--kick nil 'scroll)))

(defun md-mermaid-live-restart (&optional force)
  "Cancel all jobs and rebuild overlays.
With prefix FORCE, bypass size guards during the first refresh."
  (interactive "P")
  (md-mermaid-live-clear)
  (when force
    (setq md-mermaid-live--large-buffer nil))
  (md-mermaid-live--bootstrap "restart")
  (md-mermaid-live-refresh force))

(defun md-mermaid-live-toggle-code-visibility (&optional arg)
  "Toggle visibility of Mermaid fence source in the current buffer.
With prefix ARG, show the code when ARG is positive and hide it otherwise."
  (interactive "P")
  (unless (derived-mode-p 'markdown-mode 'gfm-mode)
    (md-mermaid-live--debug "toggle called outside markdown-derived mode"))
  (let* ((raw (prefix-numeric-value arg))
         (hide (cond
                ((null arg) (not md-mermaid-live--hide-code))
                ((> raw 0) nil)
                (t t))))
    (setq md-mermaid-live--hide-code hide)
    (md-mermaid-live--apply-code-visibility)
    (message "md-mermaid-live: Mermaid code %s"
             (if hide "hidden" "visible"))))

(defun md-mermaid-live-toggle-scroll-conservatively (&optional value)
  "Toggle or set `md-mermaid-live-scroll-conservatively` for this buffer.
With no prefix, flip between nil and a sensible default.  With a numeric
prefix VALUE, use that value instead.  Reapply the setting immediately when the
mode is active."
  (interactive
   (list (when current-prefix-arg
           (if (numberp current-prefix-arg)
               current-prefix-arg
             (read-number "scroll-conservatively value: "
                          md-mermaid-live--scroll-conservative-default)))))
  (let* ((enable-value (cond
                        ((numberp value) (truncate value))
                        ((numberp md-mermaid-live-scroll-conservatively) nil)
                        (t md-mermaid-live--scroll-conservative-default)))
         (new (if (numberp enable-value)
                  enable-value
                nil)))
    (setq-local md-mermaid-live-scroll-conservatively new)
    (when md-mermaid-live-mode
      (md-mermaid-live--refresh-scroll-settings))
    (message "md-mermaid-live: scroll-conservatively %s (buffer-local)"
             (if new (number-to-string new) "disabled"))
    new))

(defun md-mermaid-live-toggle-auto-window-vscroll (&optional arg)
  "Toggle `md-mermaid-live-disable-auto-window-vscroll` for this buffer.
With positive prefix ARG, enable the stabiliser; with zero/negative ARG,
disable it.  Without ARG, flip the current state."
  (interactive "P")
  (let* ((requested (cond
                     ((null arg) 'toggle)
                     ((> (prefix-numeric-value arg) 0) t)
                     (t nil)))
         (new (if (eq requested 'toggle)
                  (not md-mermaid-live-disable-auto-window-vscroll)
                requested)))
    (setq-local md-mermaid-live-disable-auto-window-vscroll (and new t))
    (when md-mermaid-live-mode
      (md-mermaid-live--refresh-scroll-settings))
    (message "md-mermaid-live: auto-window-vscroll %s (buffer-local)"
             (if new "disabled" "default"))
    new))

(defun md-mermaid-live-toggle-scroll-stabilizers ()
  "Toggle both scroll stabiliser settings together.
\nApply a conservative scroll value and disable
`auto-window-vscroll` when enabling.
\nRestore the defaults when disabling."
  (interactive)
  (let* ((currently-on (and (numberp md-mermaid-live-scroll-conservatively)
                            md-mermaid-live-disable-auto-window-vscroll))
         (enable (not currently-on))
         (scroll (and enable md-mermaid-live--scroll-conservative-default))
         (auto (and enable t)))
    (setq-local md-mermaid-live-scroll-conservatively scroll)
    (setq-local md-mermaid-live-disable-auto-window-vscroll auto)
    (when md-mermaid-live-mode
      (md-mermaid-live--refresh-scroll-settings))
    (message "md-mermaid-live: scroll stabilisers %s (buffer-local)"
             (if enable "enabled" "disabled"))
    (cons scroll auto)))

(defun md-mermaid-live-render-visible (&optional force)
  "Render the current window region once without enabling the mode.
With prefix FORCE, bypass size guards."
  (interactive "P")
  (let ((md-mermaid-live-scope 'visible))
    (md-mermaid-live-refresh (or force t))))

;;;###autoload
(defun md-mermaid-live-apply-quickfix ()
  "Apply quick fixes to the failing ```mermaid block at point."
  (interactive)
  (let ((bounds (md-mermaid-live--block-bounds-at-point)))
    (unless bounds
      (user-error "Point is not inside a ```mermaid block"))
    (cl-destructuring-bind (beg end) bounds
      (let* ((entry (md-mermaid-live--errors-for-range (current-buffer) beg end))
             (json (and entry (nth 3 entry))))
        (unless json
          (user-error "No recorded friendly error for this block yet"))
        (message "%s" (md-mermaid-live--friendly-summary json))
        (md-mermaid--dispatch-quickfix
         json bounds
         :insert-header-fn #'md-mermaid-live--insert-header
         :jump-to-where-fn #'md-mermaid-live--jump-to-block-line
         :quote-end-line-fn #'md-mermaid-live--quote-bare-end-on-line
         :close-one-bracket-fn #'md-mermaid-live--auto-close-one-bracket
         :rerender-fn #'md-mermaid-live--rerender-range
         :has-header-p-fn (lambda (b e)
                            (md-mermaid-live--has-header-p
                             (md-mermaid-live--block-string b e))))))))

;;;###autoload
(defun md-mermaid-live-show-last-error ()
  "Jump to the most recent recorded live-mode error in this buffer."
  (interactive)
  (let ((entry (seq-find (lambda (it) (eq (nth 0 it) (current-buffer)))
                         md-mermaid-live--last-errors)))
    (unless entry
      (user-error "No recorded Mermaid errors for this buffer"))
    (goto-char (nth 1 entry))
    (push-mark (nth 2 entry) t t)
    (let ((json (nth 3 entry)))
      (if json
          (with-help-window "*md-mermaid-live-last-error*"
            (princ (md-mermaid--format-friendly json)))
        (message "No friendly JSON available for this block")))))

(define-minor-mode md-mermaid-live-mode
  "Toggle live Mermaid rendering overlays in Markdown buffers.  \nWhen enabled, Mermaid code fences are rendered asynchronously to PNG and\ndisplayed directly below each fence using overlays.  Does not modify the file."
  :lighter " Mermaid⇔"
  (if md-mermaid-live-mode
      (progn
        (setq md-mermaid-live--last-tick nil
              md-mermaid-live--last-window-start nil
              md-mermaid-live--last-window-end nil)
        (setq md-mermaid-live--hide-code md-mermaid-live-hide-code)
        (md-mermaid-live--apply-scroll-settings)
        (add-hook 'after-change-functions #'md-mermaid-live--after-change nil t)
        (add-hook 'window-scroll-functions #'md-mermaid-live--on-scroll nil t)
        (md-mermaid-live--start-monitor)
        (md-mermaid-live--bootstrap "enable")
        (md-mermaid-live-refresh))
    (remove-hook 'after-change-functions #'md-mermaid-live--after-change t)
    (remove-hook 'window-scroll-functions #'md-mermaid-live--on-scroll t)
    (md-mermaid-live--stop-monitor)
    (md-mermaid-live--restore-scroll-settings)
    (md-mermaid-live-clear)
    (setq md-mermaid-live--hide-code nil)))

(provide 'md-mermaid-live)
;;; md-mermaid-live.el ends here
