;;; md-mermaid.el --- Render Mermaid in Markdown via presets  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Author:  Ahmet Usal <ahmetusal@gmail.com>
;; Collaborators: OpenAI Assistant, Claude Assistant
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;; Keywords: markdown, mermaid, tools, images
;; URL: https://github.com/ahmetus/md-mermaid
;;
;;; Commentary:
;;
;; md-mermaid provides a simple interactive command to render Mermaid code
;; fences in Markdown files to SVG/PNG using Mermaid CLI (mmdc), with
;; sensible presets for browser (SVG) and Emacs (large PNG).
;;
;; Dependencies:
;;   - Node.js >= 18
;;   - @mermaid-js/mermaid-cli (mmdc) available on PATH
;;   - Optional: Python 3 (used by bundled renderer) and Chromium/Chrome for Puppeteer
;;
;; Files expected in the same directory tree:
;;   md-mermaid/scripts/md_mermaid_render.py
;;   md-mermaid/scripts/md-mermaid.sh
;;   md-mermaid/mermaid-config.json (optional)
;;
;; Quick start:
;;   1) Place the entire `md-mermaid/` directory in your Emacs load-path
;;      (e.g., copy to ~/.emacs.d/md-mermaid/).
;;   2) In init.el:
;;        (add-to-list 'load-path (expand-file-name "md-mermaid" user-emacs-directory))
;;        (require 'md-mermaid)
;;   3) Open a Markdown file with ```mermaid fences.
;;   4) M-x md-mermaid-render-current
;;      - RET, RET accepts defaults (PNG 1800px, white background, opens output)
;;      - C-u M-x md-mermaid-render-current to force re-render
;;
;; Keybindings:
;;   This package does not set global keybindings by default.
;;   To enable the recommended bindings, add the following to your init.el:
;;
;;     (global-set-key (kbd "C-c m") 'md-mermaid-transient)
;;     (global-set-key (kbd "C-c M") 'md-mermaid-prefix)
;;
;;   Or customize `md-mermaid-transient-menu-keybinding` and
;;   `md-mermaid-keymap-prefix`.
;;
;;   You can also use the menu option (M-x md-mermaid-customize-keys) to
;;   bind these keys interactively and save them to your custom file.
;;
;; Presets offered:
;;   - SVG (browser/grip)      → neutral theme, transparent background
;;   - PNG 1280/1400/1800      → default theme, white background
;;   - PNG custom width (px)
;;
;; The command writes a sibling file:
;;   INPUT.md → INPUT-images.md (svg) or INPUT-emacs.md (png)
;; and places images under assets/mermaid (customizable).
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'transient)
;; Do not hard-require internal tools at load time so linters/batch loads
;; without proper load-path do not fail. Tools are required lazily where used.
(require 'md-mermaid-tools nil 'noerror)

(declare-function md-mermaid-live--errors-for-range "md-mermaid-live" (buffer beg end))
(declare-function md-mermaid-live--rerender-range "md-mermaid-live" (beg end))
(declare-function md-mermaid--transient-menu "md-mermaid" ())

;; Silence byte-compiler about symbols defined via transient later.
(defvar md-mermaid-cli-tools-menu nil
  "Transient entry command for CLI tools, defined at runtime.")

(defvar md-mermaid--transient-menu nil
  "Top-level transient menu command, defined at runtime.")


(defgroup md-mermaid nil
  "Render Mermaid diagrams in Markdown via external wrapper."
  :group 'tools)

(defcustom md-mermaid-default-preset 'png1400
  "Default preset: one of `svg, `png1280, `png1400, `png1800, or (pngW . WIDTH)."
  :type '(choice (const svg) (const png1280) (const png1400) (const png1800)
                 (cons (const pngW) integer)))

(defcustom md-mermaid-assets-dir "assets/mermaid"
  "Default assets directory for rendered images."
  :type 'string)

(defcustom md-mermaid-config "md-mermaid/mermaid-config.json"
  "Default Mermaid config JSON used for PNG presets when present.
If the file exists, it is passed to the renderer for PNG presets."
  :type 'string)

(defcustom md-mermaid-open-output t
  "When non-nil, open the output Markdown after rendering."
  :type 'boolean)

(defcustom md-mermaid-python-binary nil
  "Optional Python binary.  If nil, wrapper chooses."
  :type '(choice (const :tag "Auto" nil) string))

(defcustom md-mermaid-open-browser 'grip
  "When non-nil, open a browser after successful render.
Value can be:
- nil        → do not open a browser (default)
- http       → serve project via python http.server and open URL
- grip       → use the external `grip -b` command (if available)"
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Python http.server" http)
                 (const :tag "Grip (GitHub renderer)" grip)))

(defcustom md-mermaid-server-bind "127.0.0.1"
  "Bind address for the preview HTTP server when using `http` method."
  :type 'string)

(defvar md-mermaid--server-proc nil
  "Background http server process for preview, or nil if not running.")

(defvar md-mermaid--server-port nil
  "Port where the preview http server listens, or nil if unknown.")

(defvar md-mermaid--global-root-cache (make-hash-table :test 'equal)
  "Global cache of resolved project roots.")

(defvar md-mermaid--resolved-root nil
  "Buffer-local cache of current project root.")

(defvar md-mermaid--resolved-context nil
  "Hash of the context used to resolve the root; cache invalidates if changed.")

(defcustom md-mermaid-guide-on-missing-deps t
  "When non-nil, show guidance and offer to open the README.
if a dependency is missing."
  :type 'boolean)

(defcustom md-mermaid-install-global-keybindings t
  "When non-nil, install the global prefix and keybinding."
  :type 'boolean)

;; Autoload live-facing commands so bindings work without manual require.
(autoload 'md-mermaid-live-mode "md-mermaid-live" "Toggle live Mermaid overlays." t)
(autoload 'md-mermaid-live-apply-quickfix "md-mermaid-live" "Quickfix current Mermaid block in live mode." t)
(autoload 'md-mermaid-live-show-last-error "md-mermaid-live" "Jump to last live Mermaid error." t)
(autoload 'md-mermaid-live-restart "md-mermaid-live" "Restart md-mermaid-live-mode overlays." t)
(autoload 'md-mermaid-live-render-visible "md-mermaid-live" "Render visible region once." t)
(autoload 'md-mermaid-live-toggle-code-visibility "md-mermaid-live" "Toggle Mermaid code visibility." t)
(autoload 'md-mermaid-live-toggle-scroll-stabilizers "md-mermaid-live" "Toggle scroll stabilisers." t)
(autoload 'md-mermaid-live-toggle-scroll-conservatively "md-mermaid-live" "Toggle conservative scroll setting." t)
(autoload 'md-mermaid-live-toggle-auto-window-vscroll "md-mermaid-live" "Toggle auto-window-vscroll." t)
(autoload 'md-mermaid-live-clear "md-mermaid-live" "Clear all live overlays." t)

(defvar md-mermaid-live-mode nil
  "Non-nil when `md-mermaid-live-mode' overlays are active (autoload placeholder).")

(defvar md-mermaid--ext-available nil
  "Non-nil when optional transient extensions can be used.")

(defvar md-mermaid-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") #'md-mermaid-apply-quickfix)
    (define-key map (kbd "f") #'md-mermaid-live-apply-quickfix)
    map)
  "Prefix map for `C-c M` bindings (quick fixes, etc.).")

;; Declared for the byte-compiler; ensure the prefix command exists during compile/load.
(defvar md-mermaid-prefix nil
  "Prefix command created by `define-prefix-command' for md-mermaid bindings.")
(eval-and-compile
  (define-prefix-command 'md-mermaid-prefix))

(defun md-mermaid--lib-root ()
  "Return the directory containing this library."
  (file-name-directory
   (or load-file-name
       (ignore-errors (locate-library "md-mermaid"))
       buffer-file-name
       default-directory)))

(defun md-mermaid--project-root ()
  "Heuristic project root using multi-context search with caching."
  (let* ((script "scripts/md-mermaid.sh")
         (context (list (when buffer-file-name (file-name-directory buffer-file-name))
                        default-directory
                        (md-mermaid--lib-root)))
         (context-hash (sxhash context)))
    (unless (and md-mermaid--resolved-root
                 (eq md-mermaid--resolved-context context-hash)
                 (file-exists-p (expand-file-name script md-mermaid--resolved-root)))
      (let* ((search-paths (delq nil (list (md-mermaid--lib-root)
                                           (car context) (cadr context))))
             (cache-key (mapconcat #'identity (mapcar (lambda (d) (or d "")) search-paths) "|"))
             (cached (gethash cache-key md-mermaid--global-root-cache))
             (found (or (and cached (file-exists-p (expand-file-name script cached)) cached)
                        (seq-some (lambda (dir)
                                    (when dir
                                      (locate-dominating-file dir script)))
                                  search-paths))))
        (setq md-mermaid--resolved-root (or found (md-mermaid--lib-root)
                                             (car context) (cadr context) user-emacs-directory))
        (setq md-mermaid--resolved-context context-hash)
        (puthash cache-key md-mermaid--resolved-root md-mermaid--global-root-cache)))
    md-mermaid--resolved-root))

(defun md-mermaid--python-binary ()
  "Return a Python binary name/path to use."
  (or md-mermaid-python-binary
      (executable-find "python3")
      (executable-find "python")
      "python"))

(defun md-mermaid--free-port ()
  "Return an available TCP port as an integer, or 0 on failure."
  (let* ((py (md-mermaid--python-binary))
         (cmd (list py "-c" "import socket; s=socket.socket(); s.bind(('',0)); print(s.getsockname()[1])"))
         (out (with-temp-buffer
                (if (= 0 (apply #'call-process (car cmd) nil t nil (cdr cmd)))
                    (string-trim (buffer-string))
                  ""))))
    (string-to-number (or out "0"))))

(defun md-mermaid-stop-server ()
  "Stop the background preview server if running."
  (interactive)
  (when (process-live-p md-mermaid--server-proc)
    (delete-process md-mermaid--server-proc))
  (setq md-mermaid--server-proc nil
        md-mermaid--server-port nil)
  (message "md-mermaid: preview server stopped"))

(defun md-mermaid--start-http-server (root)
  "Ensure a preview http.server is running serving ROOT.
Returns (port . proc).  Reuses existing server if alive."
  (if (process-live-p md-mermaid--server-proc)
      (cons md-mermaid--server-port md-mermaid--server-proc)
    (let* ((port (or (and (integerp md-mermaid--server-port) (> md-mermaid--server-port 0)
                          md-mermaid--server-port)
                     (md-mermaid--free-port)))
           (py (md-mermaid--python-binary))
           (buf (get-buffer-create "*md-mermaid-httpd*"))
           (default-directory root)
           (proc (make-process
                  :name "md-mermaid-httpd"
                  :buffer buf
                  :command (list py "-m" "http.server" (number-to-string port) "--bind" md-mermaid-server-bind)
                  :noquery t
                  :stderr buf
                  :sentinel (lambda (p _e)
                              (unless (process-live-p p)
                                (setq md-mermaid--server-proc nil
                                      md-mermaid--server-port nil))))))
      (setq md-mermaid--server-proc proc
            md-mermaid--server-port port)
      (cons port proc))))

(defcustom md-mermaid-grip-port nil
  "Port to use for grip browser preview.  When nil, choose a free port."
  :type '(choice (const :tag "Auto" nil) integer))

(defvar md-mermaid--last-svg-md nil
  "Absolute path of the last rendered or previewed SVG Markdown (-images.md).
Used by `md-mermaid-preview-last-svg'.")

(defcustom md-mermaid-known-headers
  '("flowchart TD" "flowchart LR" "sequenceDiagram" "classDiagram"
    "erDiagram" "stateDiagram-v2" "gantt" "journey" "pie")
  "Diagram headers offered by quickfix helpers."
  :type '(repeat string))

(defvar md-mermaid--last-run-context nil
  "Plist storing the last batch render invocation (cmd/args/out).")

(defun md-mermaid--json--find-object-end (string start)
  "Return index of matching STRING for object starting at START or nil.
Properly skips over quoted strings and escaped characters."
  (let ((depth 0)
        (i start)
        (len (length string))
        (in-string nil)
        (escape nil)
        end)
    (while (and (< i len) (null end))
      (let ((ch (aref string i)))
        (cond
         (escape
          (setq escape nil))
         ((eq ch ?\\)
          (setq escape t))
         ((eq ch ?\")
          (setq in-string (not in-string)))
         ((not in-string)
          (cond
           ((eq ch ?{)
            (setq depth (1+ depth)))
           ((eq ch ?})
            (setq depth (1- depth))
            (when (zerop depth)
              (setq end i)))))))
      (setq i (1+ i)))
    end))

(defun md-mermaid--extract-first-json (string)
  "Return the first JSON object found in STRING, or nil."
  (when (and (stringp string) (not (string-empty-p string)))
    (let ((pos 0)
          result)
      (while (and (not result)
                  (setq pos (string-match "{" string pos)))
        (let ((end (md-mermaid--json--find-object-end string pos)))
          (when end
            (let ((snippet (substring string pos (1+ end))))
              (setq result
                    (ignore-errors
                      (json-parse-string snippet
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object :json-false))))))
        (setq pos (1+ pos)))
      result)))

(defun md-mermaid--json-first-issue (json)
  "Return the first issue entry from JSON payload."
  (let ((issues (alist-get 'issues json)))
    (when (and (listp issues) issues)
      (car issues))))

(defun md-mermaid--json-from-batch-buffer ()
  "Extract friendly-error JSON from *md-mermaid* buffer, if available."
  (when-let ((buf (get-buffer "*md-mermaid*")))
    (with-current-buffer buf
      (md-mermaid--extract-first-json
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun md-mermaid--maybe-live-error-json (bounds)
  "Return friendly-error JSON recorded by md-mermaid-live for BOUNDS, if any."
  (when (featurep 'md-mermaid-live)
    (when (fboundp 'md-mermaid-live--errors-for-range)
      (let* ((beg (car bounds))
             (end (cadr bounds))
             (entry (md-mermaid-live--errors-for-range (current-buffer) beg end)))
        (when entry (nth 3 entry))))))

(defun md-mermaid--quickfix-rerender-fn (source)
  "Return a rerender function appropriate for SOURCE (`live or `batch)."
  (pcase source
    ('live
     (lambda (beg end)
       (if (and (featurep 'md-mermaid-live)
                (fboundp 'md-mermaid-live--rerender-range)
                (boundp 'md-mermaid-live-mode)
                (buffer-local-value 'md-mermaid-live-mode (current-buffer)))
           (md-mermaid-live--rerender-range beg end)
         (message "Quickfix applied. Re-enable md-mermaid-live-mode to re-render this block."))))
    (_ #'md-mermaid--rerun-batch-here)))

(defun md-mermaid--format-friendly (payload)
  "Return a human-readable string for friendly error PAYLOAD."
  (let* ((summary (alist-get 'summary payload))
         (issues (alist-get 'issues payload))
         (raw (alist-get 'raw payload))
         (lines (list (propertize (concat "Mermaid render failed — "
                                          (or summary "unknown error"))
                                   'face 'error)
                      "")))
    (dolist (issue issues)
      (push (concat "• " (alist-get 'title issue)
                    (let ((where (alist-get 'where issue)))
                      (if where (format " (%s)" where) "")))
            lines)
      (let ((explanation (alist-get 'explanation issue)))
        (when explanation
          (push (concat "  - " explanation) lines)))
      (dolist (suggestion (alist-get 'suggestions issue))
        (push (concat "  - " suggestion) lines))
      (push "" lines))
    (when (and raw (not (string-empty-p raw)))
      (push "Raw error:" lines)
      (push (propertize raw 'face 'font-lock-comment-face) lines)
      (push "" lines))
    (string-join (nreverse lines) "\n")))

(defun md-mermaid--append-friendly-error (payload raw)
  "Append friendly error PAYLOAD (or RAW text) to `*md-mermaid*'."
  (with-current-buffer (get-buffer-create "*md-mermaid*")
    (goto-char (point-max))
    (insert "\n---\n")
    (if payload
        (insert (md-mermaid--format-friendly payload))
      (insert (propertize "Mermaid render failed (no JSON details)\n" 'face 'error))
      (when (and raw (not (string-empty-p raw)))
        (insert (propertize "Raw output:\n" 'face 'shadow))
        (insert raw)
        (insert "\n")))))

(defun md-mermaid--bounds-at-point ()
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

(defun md-mermaid--first-nonblank-line (string)
  "Return the first nonblank line in STRING."
  (car (seq-remove (lambda (line) (string-match-p "\\`\\s-*\\'" line))
                   (split-string string "\n"))))

(defun md-mermaid--has-header-p (string)
  "Return non-nil when STRING appears to start with a Mermaid header."
  (let ((line (md-mermaid--first-nonblank-line string)))
    (seq-some (lambda (header)
                (and line (string-prefix-p header line)))
              md-mermaid-known-headers)))

(defun md-mermaid--insert-header (beg)
  "Prompt for a diagram header and insert it at BEG."
  (let* ((choice (completing-read "Diagram header: "
                                  md-mermaid-known-headers nil t nil nil
                                  (car md-mermaid-known-headers))))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\n\t ")
      (insert choice "\n"))
    choice))

(defun md-mermaid--quote-bare-end-on-line ()
  "Quote bare `end` tokens on the current line.  Return replacements."
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

(defun md-mermaid--auto-close-one-bracket (beg end)
  "If there is exactly one unmatched bracket between BEG and END, close it.
Returns the inserted closing character or nil."
  (let* ((text (buffer-substring-no-properties beg end))
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

(defun md-mermaid--jump-to-block-line (beg where)
  "Jump to WHERE (e.g., \"line 5\") relative to block BEG."
  (when (and where (string-match "\\([0-9]+\\)" where))
    (goto-char beg)
    (forward-line (1- (string-to-number (match-string 1 where))))
    (recenter 3)))

(defun md-mermaid--rerun-batch-here (&rest _)
  "Re-run the last batch render invocation, if available."
  (let* ((cmd (plist-get md-mermaid--last-run-context :cmd))
         (args (plist-get md-mermaid--last-run-context :args))
         (out (plist-get md-mermaid--last-run-context :out)))
    (if (and cmd args out)
        (md-mermaid--start-render-process cmd args out)
      (message "md-mermaid: no previous render to rerun"))))

(cl-defun md-mermaid--dispatch-quickfix
    (json bounds &key insert-header-fn jump-to-where-fn quote-end-line-fn
          close-one-bracket-fn rerender-fn has-header-p-fn)
  "Arguments:
- JSON: alist describing issues and tags from the renderer.
- BOUNDS: cons cell (BEG END) delimiting the diagram block.

Keyword function arguments:
- :INSERT-HEADER-FN — called as (FN BEG); insert a header at block start.
- :JUMP-TO-WHERE-FN — called as (FN BEG WHERE); move point
 to a location derived from JSON.
- :QUOTE-END-LINE-FN — called as (FN); quote a bare end on the current line;
should return the number of edits made.
- :CLOSE-ONE-BRACKET-FN — called as (FN BEG END);
try to close one unmatched bracket within the block.
- :RERENDER-FN — called as (FN BEG END); re-render the block after edits.
- :HAS-HEADER-P-FN — called as (FN BEG END); non-nil when the
block already has a header.

Return non-nil if any quick fix was applied."
  (cl-destructuring-bind (beg end) bounds
    (let* ((tags (or (alist-get 'tags json) '()))
	   (issue (md-mermaid--json-first-issue json))
	   (where (and issue (alist-get 'where issue)))
	   (did nil))
      (cl-labels ((maybe-insert-header ()
		    (when (and insert-header-fn has-header-p-fn
			       (not (funcall has-header-p-fn beg end))
			       (y-or-n-p "Insert a diagram header at top? "))
		      (funcall insert-header-fn beg)
		      (setq did t)))
		  (maybe-jump ()
		    (when (and jump-to-where-fn where)
		      (funcall jump-to-where-fn beg where)))
		  (maybe-quote-end ()
		    (when (and quote-end-line-fn
			       (y-or-n-p "Quote bare `end` on this line if present? "))
		      (when (> (funcall quote-end-line-fn) 0)
			(setq did t))))
		  (maybe-close-bracket ()
		    (when (and close-one-bracket-fn
			       (y-or-n-p "Try to close an unmatched bracket in this block? "))
		      (when (funcall close-one-bracket-fn beg end)
			(setq did t)))))
	(cond
	 ((seq-some (lambda (tag) (member tag '("unknown-type" "version-mismatch"))) tags)
	  (maybe-insert-header)
	  (maybe-close-bracket))
	 ((seq-some (lambda (tag) (member tag '("parse-error" "lex-error" "unexpected-token" "unclosed-structure"))) tags)
	  (maybe-jump)
	  (maybe-quote-end)
	  (maybe-close-bracket)
	  (maybe-insert-header))
	 ((seq-some (lambda (tag)
		      (member tag '("sequence-actor" "sequence-syntax" "class-syntax"
				    "er-syntax" "state-syntax" "gantt-date")))
		    tags)
	  (maybe-jump)
	  (maybe-insert-header))
	 ((member "flowchart-dir" tags)
	  (maybe-insert-header))
	 ((member "subgraph-not-closed" tags)
	  (maybe-quote-end)
	  (maybe-close-bracket))
	 ((member "link-style-invalid" tags)
	  (maybe-insert-header)
	  (maybe-close-bracket))
	 ((seq-some (lambda (tag)
		      (member tag '("puppeteer-missing" "sandbox-failure" "timeout"
				    "disk-full" "permission" "file-io")))
		    tags)
	  (message "Environment issue: %s — fix runtime settings and retry."
		   (alist-get 'summary json)))
	 (t
	  (maybe-insert-header)
	  (maybe-close-bracket)))
	(when rerender-fn
	  (funcall rerender-fn beg end))
	did))))

(defun md-mermaid--open-http-preview (output-file)
  "Open OUTPUT-FILE via the local Python http.server preview."
  (let* ((root (md-mermaid--project-root))
         (rel (file-relative-name (expand-file-name output-file) (expand-file-name root)))
         (pair (md-mermaid--start-http-server root))
         (port (car pair))
         (url (format "http://%s:%d/%s" md-mermaid-server-bind port rel)))
    (browse-url url)
    (message "md-mermaid: preview at %s (M-x md-mermaid-stop-server to stop)" url)
    (with-current-buffer (get-buffer-create "*md-mermaid*")
      (goto-char (point-max))
      (insert (format "\nPreview URL: %s\n" url)))))

(defun md-mermaid--maybe-open-browser (output-file)
  "Open OUTPUT-FILE in a browser if it ends with \"-images.md\".
Respects `md-mermaid-open-browser` (symbols `grip' or `http', or nil)."
  (when (string-suffix-p "-images.md" output-file)
    (pcase md-mermaid-open-browser
      ('grip
       (if (executable-find "grip")
           (let* ((buf (get-buffer-create "*md-mermaid-grip*"))
                  (root-abs (expand-file-name (md-mermaid--project-root)))
                  (out-abs (expand-file-name output-file))
                  (md-dir (file-name-directory out-abs))
                  (default-directory md-dir)
                  (port (or md-mermaid-grip-port (md-mermaid--free-port)))
                  (addr (format "%s:%d" md-mermaid-server-bind port))
                  (proc-name (format "md-mermaid-grip-%d" port))
                  (assets-link (expand-file-name "assets" md-dir))
                  (assets-target (expand-file-name "assets" root-abs))
                  (fallback-done nil))
             (with-current-buffer buf (erase-buffer))
             ;; Ensure assets accessible under md directory for grip file mode
             (ignore-errors
               (unless (file-exists-p assets-link)
                 (make-symbolic-link assets-target assets-link t)))
             ;; Serve the specific file so grip doesn't require a README
             (make-process :name proc-name
                           :buffer buf
                           :command (list "grip" out-abs addr)
                           :noquery t
                           :stderr buf
                           :sentinel (lambda (p _e)
                                       (unless (or fallback-done (process-live-p p) (eq 0 (process-exit-status p)))
                                         (setq fallback-done t)
                                         (md-mermaid--open-http-preview out-abs))))
            (run-at-time 0.3 nil (lambda ()
                                    (if (process-live-p (get-process proc-name))
                                        (progn (browse-url (format "http://%s/" addr))
                                               (message "md-mermaid: opening with grip at http://%s/ …" addr)
                                               (with-current-buffer (get-buffer-create "*md-mermaid*")
                                                 (goto-char (point-max))
                                                 (insert (format "\nGrip URL: http://%s/\n" addr))))
                                      (unless fallback-done
                                        (setq fallback-done t)
                                        (md-mermaid--open-http-preview out-abs))))))
         (md-mermaid--open-http-preview output-file)))
      ('http (md-mermaid--open-http-preview output-file))
      (_ nil))))

(defun md-mermaid--start-render-process (cmd final-args out)
  "Start the md-mermaid wrapper CMD with FINAL-ARGS writing to OUT."
  (let ((buf (get-buffer-create "*md-mermaid*")))
    (setq md-mermaid--last-run-context (list :cmd cmd :args final-args :out out))
    (md-mermaid--maybe-warn-missing-deps)
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Running: %s %s\n\n" cmd (mapconcat #'identity final-args " "))))
    (let* ((orig-path (or (getenv "PATH") ""))
           (extra (list (expand-file-name "~/.local/bin")
                        (expand-file-name "~/.config/yarn/global/node_modules/.bin")
                        (expand-file-name "~/.yarn/bin")
                        (expand-file-name "~/.npm-global/bin")
                        (expand-file-name "~/.local/share/pnpm")
                        (expand-file-name "~/.pnpm-global/bin")))
           (new-path (mapconcat #'identity (delete-dups (append extra (split-string orig-path path-separator t)))
                                path-separator))
           (process-environment (cons (concat "PATH=" new-path) process-environment)))
      (make-process
       :name "md-mermaid"
       :buffer buf
       :command (cons cmd final-args)
       :stderr buf
       :noquery t
       :sentinel
       (lambda (p _e)
         (when (memq (process-status p) '(exit signal))
           (with-current-buffer (process-buffer p)
             (goto-char (point-max)))
           (let ((status (process-exit-status p)))
             (if (zerop status)
                 (progn
                   (when (and md-mermaid-open-output (file-exists-p out))
                     (find-file-other-window out))
                   (message "md-mermaid finished with status %s" status)
                   (when (string-suffix-p "-images.md" out)
                     (setq md-mermaid--last-svg-md (expand-file-name out)))
                   (md-mermaid--maybe-open-browser out))
               (let* ((raw (when (buffer-live-p (process-buffer p))
                             (with-current-buffer (process-buffer p)
                               (buffer-string))))
                      (json (md-mermaid--extract-first-json raw)))
                 (display-buffer (process-buffer p))
                 (md-mermaid--append-friendly-error json raw)
                 (message "md-mermaid failed (status %s). See *md-mermaid* for details."
                          status))))))))
    (message "Rendering Mermaid…")))

(defun md-mermaid--wrapper ()
  "Absolute path to the wrapper script."
  (let* ((root (md-mermaid--project-root))
         (cand1 (expand-file-name "md-mermaid/scripts/md-mermaid.sh" root))
         (cand2 (expand-file-name "scripts/md-mermaid.sh" (md-mermaid--lib-root))))
    (cond
     ((file-exists-p cand1) cand1)
     ((file-exists-p cand2) cand2)
     (t (user-error "The md-mermaid wrapper not found (expected %s or %s)" cand1 cand2)))))

(defun md-mermaid--readme-path ()
  "Default README path for grip."
  (expand-file-name "md-mermaid/README.md" (md-mermaid--project-root)))

(defun md-mermaid-open-readme ()
  "Open md-mermaid README."
  (interactive)
  (find-file (md-mermaid--readme-path)))

(defun md-mermaid--maybe-warn-missing-deps ()
  "Warn about missing dependencies and optionally guide to README."
  (let (msgs)
    (unless (executable-find "mmdc")
      (push "- Mermaid CLI 'mmdc' not found. Install: npm i -g @mermaid-js/mermaid-cli" msgs))
    (when (and (eq md-mermaid-open-browser 'grip)
               (not (executable-find "grip")))
      (push "- grip not found. Install: pipx install grip (or pip install --user grip)" msgs))
    (when (and (eq md-mermaid-open-browser 'http)
               (not (md-mermaid--python-binary)))
      (push "- Python not found for http.server preview." msgs))
    (when msgs
      (with-current-buffer (get-buffer-create "*md-mermaid*")
        (goto-char (point-max))
        (insert "\nDependencies missing:\n" (mapconcat #'identity msgs "\n")
                        (if md-mermaid-guide-on-missing-deps
                            (format "\n\nSee: %s (M-x md-mermaid-open-readme)\n" (md-mermaid--readme-path))
                          "\n"))))))

(defun md-mermaid--preset->args (preset)
  "Convert PRESET symbol/cons to wrapper args."
  (pcase preset
    ('svg '("-svg"))
    ('png1280 '("-png1280"))
    ('png1400 '("-png1400"))
    ('png1800 '("-png1800"))
    (`(pngW . ,w) (list "-pngW" (number-to-string w)))
    (_ (user-error "Unknown preset: %S" preset))))

;;;###autoload
(defun md-mermaid-render-current (&optional force)
  "Render Mermaid fences in the current Markdown file.
With prefix arg FORCE command Ctrl u, re-render even if images exist."
  (interactive "P")
  (let* ((file (or (buffer-file-name)
                   (read-file-name "Markdown file: " nil nil t)))
         (_ (unless (and file (file-exists-p file))
              (user-error "Input file not found: %s" file)))
         (preset (let* ((choices '("SVG (browser)" "PNG 1280" "PNG 1400" "PNG 1800" "PNG custom width"))
                        (default (pcase md-mermaid-default-preset
                                   ('svg "SVG (browser)")
                                   ('png1280 "PNG 1280")
                                   ('png1400 "PNG 1400")
                                   ('png1800 "PNG 1800")
                                   (`(pngW . ,_) "PNG custom width")))
                        (sel (completing-read (format "Preset [%s]: " default) choices nil t nil nil default)))
                   (cond
                    ((string-prefix-p "SVG" sel) 'svg)
                    ((string-match-p "1280" sel) 'png1280)
                    ((string-match-p "1400" sel) 'png1400)
                    ((string-match-p "1800" sel) 'png1800)
                    (t (cons 'pngW (read-number "PNG width (px): " 1600))))))
         (assets-default (expand-file-name md-mermaid-assets-dir (md-mermaid--project-root)))
         ;; Use DIR as the starting directory; do not pass INITIAL to avoid duplicated paths
         (assets (read-directory-name "Assets directory: " assets-default assets-default nil))
         (base (file-name-sans-extension file))
         (out (if (eq preset 'svg)
                  (concat base "-images.md")
                (concat base "-emacs.md")))
         (wrapper (md-mermaid--wrapper))
         (puppeteer-config (expand-file-name "md-mermaid/puppeteer-no-sandbox.json" (md-mermaid--project-root)))
         ;; Ensure absolute paths and pre-create assets dir for reliability
         (file (expand-file-name file))
         (assets (expand-file-name assets))
         (_mk (ignore-errors (make-directory assets t)))
         (args (append (list wrapper "-i" file "-o" out "-d" assets)
                       (md-mermaid--preset->args preset)
                       (when force '("-f"))))
         (cfg (expand-file-name md-mermaid-config (md-mermaid--project-root))))
    (when (and (not (eq preset 'svg)) (file-exists-p cfg))
      (setq args (append args (list "--config" cfg) (list "--background" "white"))))
    ;; Default to no-sandbox puppeteer config if available; user can delete/replace that file if unwanted.
    (when (and (not (eq preset 'svg)) (file-exists-p puppeteer-config))
      (setq args (append args (list "--puppeteer-config" puppeteer-config))))
    (let ((cmd "bash")
          (final-args args))
      (when (and md-mermaid-python-binary (stringp md-mermaid-python-binary))
        (setenv "PYTHON_BIN" md-mermaid-python-binary))
      (condition-case err
          (md-mermaid--start-render-process cmd final-args out)
        (error
         (message "md-mermaid error: %s" (error-message-string err)))))))

;;;###autoload
(defun md-mermaid-apply-quickfix ()
  "Apply quick fixes to the ```mermaid block at point, then rerun the renderer."
  (interactive)
  (let* ((bounds (md-mermaid--bounds-at-point)))
    (unless bounds
      (user-error "Point is not inside a ```mermaid block"))
    (let* ((live-json (md-mermaid--maybe-live-error-json bounds))
           (json (or live-json (md-mermaid--json-from-batch-buffer))))
      (unless json
        (user-error "No friendly error JSON found (enable live mode or inspect *md-mermaid*)"))
      (when live-json
        (message "Using md-mermaid-live diagnostics for quickfix."))
      (md-mermaid--dispatch-quickfix
       json bounds
       :insert-header-fn #'md-mermaid--insert-header
       :jump-to-where-fn #'md-mermaid--jump-to-block-line
       :quote-end-line-fn #'md-mermaid--quote-bare-end-on-line
       :close-one-bracket-fn #'md-mermaid--auto-close-one-bracket
       :rerender-fn (md-mermaid--quickfix-rerender-fn (if live-json 'live 'batch))
       :has-header-p-fn (lambda (beg end)
                          (md-mermaid--has-header-p
                           (buffer-substring-no-properties beg end)))))))

(defvar md-mermaid--transient-menu-defined nil
  "Tracks whether the md-mermaid transient menu has been defined.")

(defvar md-mermaid--active-prefix-keybinding nil
  "Track the currently active binding for the md-mermaid prefix command.")
(defvar md-mermaid-transient-menu-keybinding nil
  "Track the currently active global keybinding for the transient menu.")

(defun md-mermaid--ensure-prefix-keymap ()
  "Ensure `md-mermaid-prefix` is parented to `md-mermaid-prefix-map`."
  (when (keymapp md-mermaid-prefix)
    (set-keymap-parent md-mermaid-prefix md-mermaid-prefix-map))
  md-mermaid-prefix)

(defun md-mermaid--apply-keymap-prefix (key)
  "Bind KEY to `md-mermaid-prefix'; remove the binding when KEY is nil."
  (condition-case err
      (progn
        (when md-mermaid--active-prefix-keybinding
          (global-unset-key (kbd md-mermaid--active-prefix-keybinding))
          (setq md-mermaid--active-prefix-keybinding nil))
        (when (and key (not (string-empty-p key)))
          (md-mermaid--ensure-prefix-keymap)
          (global-set-key (kbd key) #'md-mermaid-prefix)
          (setq md-mermaid--active-prefix-keybinding key)))
    (error
     (message "md-mermaid--apply-keymap-prefix error: %s"
              (error-message-string err)))))

(defun md-mermaid--set-keymap-prefix (symbol value)
  "Setter for `md-mermaid-keymap-prefix`.
Update the global prefix binding when SYMBOL is set to VALUE."
  (set-default symbol value)
  (md-mermaid--apply-keymap-prefix value))

(defun md-mermaid--apply-transient-menu-keybinding (key)
  "Bind KEY to `md-mermaid-transient'; remove the binding when KEY is nil."
  (condition-case err
      (progn
        (when md-mermaid-transient-menu-keybinding
          (global-unset-key (kbd md-mermaid-transient-menu-keybinding))
          (setq md-mermaid-transient-menu-keybinding nil))
        (when (and key (not (string-empty-p key)))
          (global-set-key (kbd key) #'md-mermaid-transient)
          (setq md-mermaid-transient-menu-keybinding key)))
    (error
     (message "md-mermaid--apply-transient-menu-keybinding error: %s"
              (error-message-string err)))))

(defun md-mermaid--set-transient-menu-keybinding (symbol value)
  "Setter for `md-mermaid--transient-menu-keybinding`.
Update the transient menu binding when SYMBOL is set to VALUE."
  (set-default symbol value)
  (md-mermaid--apply-transient-menu-keybinding value))

(defcustom md-mermaid-keymap-prefix nil
  "Global prefix keybinding for md-mermaid commands.
Set this to nil to disable the automatic binding."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'md-mermaid
  :set #'md-mermaid--set-keymap-prefix)

(defcustom md-mermaid-transient-menu-keybinding nil
  "Global keybinding for `md-mermaid-transient`.
Set this to nil to disable the automatic binding."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'md-mermaid
  :set #'md-mermaid--set-transient-menu-keybinding)

(defun md-mermaid--read-keybinding-input (prompt current)
  "Prompt with PROMPT for a keyboard shortcut, using CURRENT as the default.
Returns a normalized key string or nil when the user disables the binding."
  (let* ((display (if (and (stringp current) (not (string-empty-p current)))
                      current
                    "disabled"))
         (default (and (stringp current) current))
         (input (read-string (format "%s (blank disables) [%s]: " prompt display)
                             nil nil default))
         (trimmed (string-trim input)))
    (if (string-empty-p trimmed)
        nil
      (condition-case err
          (let ((keys (kbd trimmed)))
            (ignore keys)
            trimmed)
        (error
         (user-error "Invalid key \"%s\": %s"
                     trimmed (error-message-string err)))))))

(defun md-mermaid-customize-keys ()
  "Interactively customize md-mermaid global keybindings."
  (interactive)
  (condition-case err
      (let* ((menu-key (md-mermaid--read-keybinding-input "Main menu key"
                                                         md-mermaid-transient-menu-keybinding))
             (ext-current (and md-mermaid--ext-available
                               md-mermaid-keymap-prefix))
             (ext-key (and md-mermaid--ext-available
                           (md-mermaid--read-keybinding-input "Extension prefix key"
                                                              ext-current))))
        (customize-set-variable 'md-mermaid-transient-menu-keybinding menu-key)
        (if md-mermaid--ext-available
            (progn
              (customize-set-variable 'md-mermaid-keymap-prefix ext-key)
              (message "Updated menu key: %s; extension prefix: %s"
                       (or menu-key "disabled")
                       (or ext-key "disabled"))
              (when (yes-or-no-p "Save these keybindings for future sessions? ")
                (customize-save-variable 'md-mermaid-transient-menu-keybinding menu-key)
                (customize-save-variable 'md-mermaid-keymap-prefix ext-key)
                (message "Keybindings saved to %s" (or custom-file user-init-file))))
          (message "Updated menu key: %s; extension prefix unchanged (extensions unavailable)."
                   (or menu-key "disabled"))
          (when (yes-or-no-p "Save the menu keybinding for future sessions? ")
            (customize-save-variable 'md-mermaid-transient-menu-keybinding menu-key)
            (message "Menu key saved to %s" (or custom-file user-init-file)))))
    (error
     (message "md-mermaid-customize-keys error: %s"
              (error-message-string err))))) ; close md-mermaid-customize-keys

(defun md-mermaid--transient-toggle-live-mode ()
  "Toggle `md-mermaid-live-mode' from the transient menu."
  (interactive)
  (let ((next (if (bound-and-true-p md-mermaid-live-mode) 0 1)))
    (md-mermaid-live-mode next)
    (message "Live: %s" (if (bound-and-true-p md-mermaid-live-mode) "ON" "OFF"))))

(defun md-mermaid--transient-menu--desc-with-help (label help)
  "Return LABEL with inline HELP suffix in gray."
  (concat label
          (propertize (concat " → " help)
                      'face '(:foreground "#666666"))))

(defun md-mermaid--transient-menu--desc-toggle (label var &optional help)
  "Return LABEL annotated with VAR's state; optionally append HELP."
  (let ((state (if (and (boundp var) (symbol-value var))
                   (propertize "ON" 'face '(:foreground "green"))
                 (propertize "OFF" 'face '(:foreground "red")))))
    (if help
        (format "%s [%s] → %s" label state (propertize help 'face '(:foreground "#666666")))
      (format "%s [%s]" label state))))

(defun md-mermaid--ensure-transient-menu ()
  "Load dependencies and define the md-mermaid transient menu."
  (unless (require 'transient nil 'noerror)
    (user-error "Package `transient' is required for the md-mermaid menu"))
  ;; Ensure live helpers are available so suffixes stay commandp.
  (require 'md-mermaid-live nil 'noerror)
  (unless md-mermaid--transient-menu-defined
    ;; Local wrappers to avoid transient errors when target commands are not yet fboundp.
    (defun md-mermaid--menu-show-versions ()
      "Wrapper: ensure CLI tools loaded, then show versions table."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-show-versions)
        (call-interactively #'md-mermaid-cli-show-versions)))

    (defun md-mermaid--menu-cycle-post-verify-show ()
      "Wrapper: ensure CLI tools loaded, then cycle versions display preference."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-cycle-post-verify-show)
        (call-interactively #'md-mermaid-cli-cycle-post-verify-show)))

    (defun md-mermaid--menu-set-preferred-npm ()
      "Wrapper: ensure CLI tools loaded, then set preferred npm client."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-set-preferred-npm)
        (call-interactively #'md-mermaid-cli-set-preferred-npm)))

    (defun md-mermaid--menu-cycle-preferred-npm ()
      "Wrapper: ensure CLI tools loaded, then cycle npm client."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-cycle-preferred-npm)
        (call-interactively #'md-mermaid-cli-cycle-preferred-npm)))

    (defun md-mermaid--menu-add-global-node-bin-to-path ()
      "Wrapper: ensure CLI tools loaded, then add global Node bin to PATH."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-add-global-node-bin-to-path)
        (call-interactively #'md-mermaid-cli-add-global-node-bin-to-path)))

    (defun md-mermaid--menu-verify-and-fix-path ()
      "Wrapper: ensure CLI tools loaded, then verify tool and fix PATH."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-verify-and-fix-path)
        (call-interactively #'md-mermaid-cli-verify-and-fix-path)))

    (defun md-mermaid--menu-show-log ()
      "Wrapper: show CLI tools log buffer."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-show-log)
        (call-interactively #'md-mermaid-cli-show-log)))

    (defun md-mermaid--menu-show-debug ()
      "Wrapper: show CLI tools debug buffer."
      (interactive)
      (require 'md-mermaid-tools)
      (when (fboundp 'md-mermaid-cli-debug-show)
        (call-interactively #'md-mermaid-cli-debug-show)))

    (transient-define-prefix md-mermaid-cli-tools-menu ()
      "CLI Tools installation and management menu.\n\nKeys: s=Show versions, S=Cycle versions display; m/M=Set/Cycle npm client; P=Add global Node bin to PATH; F=Verify tool and fix PATH."
      ["Install & Update"
       ("i" "Install tool" md-mermaid-cli-install-tool)
       ("u" "Update tool" md-mermaid-cli-update-tool)
       ("v" "Check version" md-mermaid-cli-check-version)
       ("I" "Install all tools" md-mermaid-cli-install-all)
       ("U" "Update all tools" md-mermaid-cli-update-all)
       ("V" "Check all versions" md-mermaid-cli-check-all-versions)]
      ["Documentation & Settings"
       ("d" "View tool docs" md-mermaid-cli-view-tool-docs)
       ("n" "Toggle notifications" md-mermaid-cli-toggle-notifications)
       ("N" "Cycle notify method" md-mermaid-cli-cycle-notify-method)
       ("B" "Cycle batch notify" md-mermaid-cli-cycle-batch-notify)
       ("D" "Toggle debug mode" md-mermaid-cli-toggle-debug)]
      ["Versions"
       ("s" "Show versions table" md-mermaid--menu-show-versions)]
      ["UI & Display"
       ("S" "Cycle post-verify display" md-mermaid--menu-cycle-post-verify-show)]
      ["Package Manager"
       ("m" "Set preferred npm client" md-mermaid--menu-set-preferred-npm)
       ("M" "Cycle npm client" md-mermaid--menu-cycle-preferred-npm)]
      ["Utilities & PATH"
       ("P" "Add global Node bin to PATH" md-mermaid--menu-add-global-node-bin-to-path)
       ("F" "Verify tool and fix PATH" md-mermaid--menu-verify-and-fix-path)
       ("L" "Show CLI tools log" md-mermaid--menu-show-log)
       ("G" "Show debug buffer" md-mermaid--menu-show-debug)]
      ["Navigation"
       ("q" "Back to main menu" md-mermaid-transient)
       ("Q" "Quit" transient-quit-one)])



    (transient-define-prefix md-mermaid--transient-menu ()
      "Top-level control surface for md-mermaid commands."
      ["CLI Tools"
       ("t" "CLI Tools menu" md-mermaid-cli-tools-menu)]
      ["Customize"
       ("M-k" (lambda ()
		(md-mermaid--transient-menu--desc-with-help "Customize keys" "Main menu + prefix"))
	md-mermaid-customize-keys :transient nil)]
      ["Render & Preview"
       ("r" "Render current file" md-mermaid-render-current)
       ("p" "Preview last SVG" md-mermaid-preview-last-svg)
       ("l" (lambda () (md-mermaid--transient-menu--desc-toggle "Live Mode" 'md-mermaid-live-mode))
	md-mermaid--transient-toggle-live-mode :transient t)
       ("R" "Restart live mode" md-mermaid-live-restart)
       ("g" "Render visible region" md-mermaid-live-render-visible)]
      ["Diagnostics"
       ("e" "Show last live error" md-mermaid-live-show-last-error)
       ("x" "Clear live overlays" md-mermaid-live-clear)]
      ["Visibility & Scroll"
       ("v" "Toggle code visibility" md-mermaid-live-toggle-code-visibility)
       ("s" "Toggle scroll stabilisers" md-mermaid-live-toggle-scroll-stabilizers)
       ("c" "Toggle conservative scroll" md-mermaid-live-toggle-scroll-conservatively)
       ("a" "Toggle auto-window-vscroll" md-mermaid-live-toggle-auto-window-vscroll)]
      ["Session"
       ("q" "Quit" transient-quit-one)])
    (setq md-mermaid--transient-menu-defined t)))

;;;###autoload
(defun md-mermaid-transient ()
  "Show the md-mermaid top-level transient menu."
  (interactive)
  (md-mermaid--ensure-transient-menu)
  (md-mermaid--transient-menu))

;;;###autoload
(defun md-mermaid-preview-last-svg ()
  "Open the last rendered SVG Markdown (-images.md) in a browser.
If none tracked yet, prompt for a `-images.md' file under the project root."
  (interactive)
  (let* ((root (md-mermaid--project-root))
         (start (and md-mermaid--last-svg-md (file-name-directory md-mermaid--last-svg-md)))
         (candidate (cond
                     ((and md-mermaid--last-svg-md (file-exists-p md-mermaid--last-svg-md))
		      md-mermaid--last-svg-md)
                     (t (read-file-name "SVG Markdown (-images.md): " (or start root) nil t nil
                                        (lambda (f)
                                          (string-suffix-p "-images.md" f)))))))
    (if (and candidate (file-exists-p candidate))
        (progn
          (setq md-mermaid--last-svg-md (expand-file-name candidate))
          (md-mermaid--maybe-open-browser md-mermaid--last-svg-md))
      (message "md-mermaid: No SVG Markdown chosen/found. Render with SVG first."))))

;; Install global keybindings if requested.
(when md-mermaid-install-global-keybindings
  (when md-mermaid-keymap-prefix
    (md-mermaid--apply-keymap-prefix md-mermaid-keymap-prefix))
  (when md-mermaid-transient-menu-keybinding
    (md-mermaid--apply-transient-menu-keybinding md-mermaid-transient-menu-keybinding)))

(provide 'md-mermaid)
;;; md-mermaid.el ends here
