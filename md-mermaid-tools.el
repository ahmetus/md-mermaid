;;; md-mermaid-tools.el --- CLI tools management for md-mermaid -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ahmet Usal <ahmetusal@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;; Author:  Ahmet Usal <ahmetusal@gmail.com>
;; Collaborators: OpenAI Assistant, Claude Assistant
;; Version: 1.0.0
;; Keywords: tools, mermaid, cli
;; URL: https://github.com/ahmetus/md-mermaid

;;; Commentary:
;; Clean implementation of CLI tools installation and management for md-mermaid.
;; Provides async installation, version checking, and notifications.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; Configuration

(defgroup md-mermaid-tools nil
  "CLI tools management for md-mermaid."
  :group 'md-mermaid
  :prefix "md-mermaid-cli-")

(defcustom md-mermaid-cli-preferred-npm "npm"
  "Preferred Node package manager."
  :type '(choice (const "npm") (const "pnpm") (const "yarn") (const "bun"))
  :group 'md-mermaid-tools)

(defcustom md-mermaid-cli-confirm-installers t
  "If non-nil, require confirmation before installing tools."
  :type 'boolean
  :group 'md-mermaid-tools)

(defvar md-mermaid-debug-mode nil
  "When non-nil, enables debug output.")

(defcustom md-mermaid-notify-on-complete nil
  "When non-nil, notify after installations complete."
  :type 'boolean
  :group 'md-mermaid-tools)

(defcustom md-mermaid-notify-method 'emacs
  "Notification delivery method."
  :type '(choice (const :tag "Emacs" emacs)
                 (const :tag "OS" os)
                 (const :tag "Both" both))
  :group 'md-mermaid-tools)

(defcustom md-mermaid-notify-batch-when 'focus-away
  "When to notify for batch operations."
  :type '(choice (const :tag "Always" always)
                 (const :tag "Focus away" focus-away)
                 (const :tag "Never" never))
  :group 'md-mermaid-tools)

(defcustom md-mermaid-cli-post-verify-show 'single
  "Control versions table display after verification."
  :type '(choice (const :tag "None" none)
                 (const :tag "Single" single)
                 (const :tag "All" all))
  :group 'md-mermaid-tools)

;;; Tool Registry

(defvar md-mermaid-cli-tools-registry
  `((:name mermaid-cli
	   :executable "mmdc"
	   :aliases ()
	   :version-args ("--version" "-v")
	   :installers ((:method npm :platforms (linux darwin windows wsl)
				 :command "npm install -g @mermaid-js/mermaid-cli"))
	   :updaters ((:method npm :platforms (linux darwin windows wsl)
			       :command "npm install -g @mermaid-js/mermaid-cli@latest"))
	   :homepage "https://github.com/mermaid-js/mermaid-cli"
	   :docs "Mermaid CLI (mmdc) - Render Mermaid diagrams to SVG/PNG.")

    (:name puppeteer
	   :executable "puppeteer"
	   :aliases ()
	   :version-args ("--version" "-v")
	   :installers ((:method npm :platforms (linux darwin windows wsl)
				 :command "npm install -g puppeteer"))
	   :updaters ((:method npm :platforms (linux darwin windows wsl)
			       :command "npm install -g puppeteer@latest"))
	   :homepage "https://pptr.dev"
	   :docs "Puppeteer - Headless Chrome for rendering.")

    (:name svgo
	   :executable "svgo"
	   :aliases ()
	   :version-args ("--version" "-v")
	   :installers ((:method npm :platforms (linux darwin windows wsl)
				 :command "npm install -g svgo"))
	   :updaters ((:method npm :platforms (linux darwin windows wsl)
			       :command "npm install -g svgo@latest"))
	   :homepage "https://github.com/svg/svgo"
	   :docs "SVGO - Optimize SVG files."))
  "Registry of CLI tools with installation metadata.")

;;; Helper Functions

(defun md-mermaid-cli--debug (format-string &rest args)
  "Print debug message if `md-mermaid-debug-mode' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when md-mermaid-debug-mode
    (apply #'message (concat "[md-mermaid-tools] " format-string) args)))

(defun md-mermaid-cli--get-tool (tool-name)
  "Get tool entry for TOOL-NAME from registry."
  (cl-find-if (lambda (entry) (eq (plist-get entry :name) tool-name))
              md-mermaid-cli-tools-registry))

(defun md-mermaid-cli--detect-platform ()
  "Detect current platform.
Returns one of: linux, darwin, windows, wsl."
  (cond
   ((and (eq system-type 'gnu/linux)
         (file-exists-p "/proc/version")
         (with-temp-buffer
           (insert-file-contents "/proc/version")
           (re-search-forward "Microsoft\\|WSL" nil t)))
    'wsl)
   ((eq system-type 'gnu/linux) 'linux)
   ((eq system-type 'darwin) 'darwin)
   ((memq system-type '(windows-nt ms-dos cygwin)) 'windows)
   (t 'linux)))

(defun md-mermaid-cli--strip-ansi (str)
  "Remove ANSI escape sequences from STR."
  (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" str))

(defun md-mermaid-cli--probe-npm-managers ()
  "Return list of available Node package managers."
  (let ((out nil))
    (dolist (mgr '("npm" "pnpm" "yarn" "bun"))
      (when (executable-find mgr)
        (setq out (append out (list mgr)))))
    out))

(defun md-mermaid-cli--get-preferred-npm ()
  "Get preferred npm client, honoring user preference."
  (let* ((available (md-mermaid-cli--probe-npm-managers))
         (preferred (and (member md-mermaid-cli-preferred-npm available)
                         md-mermaid-cli-preferred-npm)))
    (or preferred (car available) "npm")))

(defun md-mermaid-cli--node-global-bin ()
  "Return global bin directory for the chosen Node manager.
Uses `md-mermaid-cli-preferred-npm' to decide which command to run."
  (let* ((mgr (md-mermaid-cli--get-preferred-npm))
         (cmd (cond
               ((string= mgr "npm") "npm bin -g 2>/dev/null || true")
               ((string= mgr "pnpm") "pnpm bin -g 2>/dev/null || true")
               ((string= mgr "yarn") "yarn global bin 2>/dev/null || true")
               ((string= mgr "bun") "bun pm bin 2>/dev/null || true")
               (t "npm bin -g 2>/dev/null || true")))
         (out (ignore-errors (string-trim (shell-command-to-string cmd)))))
    (if (and out (not (string-empty-p out))) out nil)))

(defun md-mermaid-cli--maybe-windows-exe (name)
  "Return candidate executable names for NAME considering Windows suffixes."
  (if (eq (md-mermaid-cli--detect-platform) 'windows)
      (list name (concat name ".cmd") (concat name ".exe"))
    (list name)))

(defun md-mermaid-cli--executable-find (tool-entry)
  "Find executable for TOOL-ENTRY, checking primary and aliases."
  (let ((primary (plist-get tool-entry :executable))
        (aliases (plist-get tool-entry :aliases)))
    (or (executable-find primary)
        (cl-some #'executable-find aliases))))




;;; Shell and Process Helpers

(defun md-mermaid-cli--select-shell-command (cmd)
  "Return command vector for `make-process' to run CMD.
On Linux/macOS/WSL uses bash -lc; on Windows uses PowerShell or cmd."
  (pcase (md-mermaid-cli--detect-platform)
    ((or 'linux 'darwin 'wsl)
     (list "bash" "-lc" (format "%s 2>&1" cmd)))
    ('windows
     (cond
      ((executable-find "powershell") (list "powershell" "-Command" cmd))
      (t (list "cmd" "/c" cmd))))
    (_ (list "bash" "-lc" (format "%s 2>&1" cmd)))))

(defun md-mermaid-cli--validate-command (cmd method)
  "Validate environment before running CMD for METHOD.
Return nil if OK, or error string explaining the problem."
  (let* ((platform (md-mermaid-cli--detect-platform))
         (shell-error
          (pcase platform
            ((or 'linux 'darwin 'wsl)
             (unless (executable-find "bash")
               (format "bash not found in PATH (required to run: %s)" cmd)))
            ('windows
             (unless (or (executable-find "powershell") (executable-find "cmd"))
               "Neither PowerShell nor cmd.exe found in PATH"))
            (_ nil)))
         (mgr-error
          (when (memq method '(npm pnpm yarn bun))
            (let ((mgr (md-mermaid-cli--get-preferred-npm)))
              (unless (executable-find mgr)
                (format "%s not found in PATH" mgr))))))
    (or shell-error mgr-error)))

;;; Output Buffer Management

(defun md-mermaid-cli--create-output-buffer ()
  "Create or return the shared output buffer for CLI operations."
  (or (get-buffer "*md-mermaid-cli-tools*")
      (with-current-buffer (generate-new-buffer "*md-mermaid-cli-tools*")
        (let ((inhibit-read-only t))
          (insert "=== md-mermaid CLI Tools Log ===\n")
          (insert (format "Session started: %s\n\n" (format-time-string "%F %T")))
          (current-buffer)))))

(defun md-mermaid-cli--display-output-buffer ()
  "Display the CLI tools output buffer."
  (interactive)
  (let* ((buf (md-mermaid-cli--create-output-buffer))
         (win (get-buffer-window buf t)))  ; Check all visible frames
    (unless win
      (setq win (display-buffer buf '((display-buffer-reuse-window
                                       display-buffer-below-selected)
                                      (window-height . 0.3)
                                      (reusable-frames . t)))))
    (when win
      (select-window win))))

(defun md-mermaid-cli-show-log ()
  "Show the CLI tools log buffer."
  (interactive)
  (md-mermaid-cli--display-output-buffer))

(defun md-mermaid-cli-debug-show ()
  "Show the CLI tools debug buffer if it exists."
  (interactive)
  (let ((buf (get-buffer "*md-mermaid-cli-debug*")))
    (if buf
        (display-buffer buf '((display-buffer-below-selected)
                              (window-height . 0.3)))
      (message "md-mermaid: no debug buffer yet"))))

;;; Async Command Runner

(defun md-mermaid-cli--run-async-command (command on-success on-error &optional timeout)
  "Run COMMAND asynchronously with TIMEOUT (default 10s).
Call ON-SUCCESS with output or ON-ERROR with error message.
Uses `condition-case' and `unwind-protect' for robust error handling."
  (condition-case err
      (let* ((timeout-secs (or timeout 10))
             (output-buffer (generate-new-buffer " *md-mermaid-cli-output*"))
             (timer nil)
             (proc nil))
        (unwind-protect
            (progn
              (setq proc
                    (start-process-shell-command
                     "md-mermaid-cli-async"
                     output-buffer
                     command))

              (setq timer
                    (run-with-timer
                     timeout-secs nil
                     (lambda ()
                       (when (and proc (process-live-p proc))
                         (delete-process proc)
                         (when (buffer-live-p output-buffer)
                           (kill-buffer output-buffer))
                         (funcall on-error (format "Command timed out after %ds: %s" timeout-secs command))))))

              (set-process-sentinel
               proc
               (lambda (_process event)
                 (condition-case sentinel-err
                     (unwind-protect
                         (progn
                           (when (timerp timer)
                             (cancel-timer timer))

                           (cond
                            ((string-match-p "finished" event)
                             (let ((output (with-current-buffer output-buffer
                                             (md-mermaid-cli--strip-ansi (buffer-string)))))
                               (funcall on-success output)))

                            (t
                             (funcall on-error (format "Process failed: %s" (string-trim event))))))

                       (when (buffer-live-p output-buffer)
                         (kill-buffer output-buffer)))
                   (error
                    (funcall on-error (format "Sentinel error: %s"
                                              (error-message-string sentinel-err))))))))

          ;; Cleanup in case of early error
          (when (and (timerp timer) (not (process-live-p proc)))
            (cancel-timer timer))
          (when (and (not (process-live-p proc)) (buffer-live-p output-buffer))
            (kill-buffer output-buffer))))
    (error
     (funcall on-error (format "Async command error: %s" (error-message-string err))))))

(defun md-mermaid-cli--check-version (tool-name callback)
  "Check version of TOOL-NAME asynchronously, call CALLBACK with result.
CALLBACK receives (tool-name version-string error-string)."
  (condition-case err
      (let* ((tool-entry (md-mermaid-cli--get-tool tool-name))
             (exe (md-mermaid-cli--executable-find tool-entry))
             (check-cmd (plist-get tool-entry :check-command))
             (version-args (plist-get tool-entry :version-args)))
        (unless tool-entry
          (funcall callback tool-name nil (format "Tool %s not in registry" tool-name))
          (cl-return-from md-mermaid-cli--check-version))

        (cond
         ;; Custom check command (e.g., for puppeteer)
         (check-cmd
          (md-mermaid-cli--run-async-command
           check-cmd
           (lambda (output)
             (funcall callback tool-name (string-trim output) nil))
           (lambda (error-msg)
             (funcall callback tool-name nil error-msg))))

         ;; Standard version check
         ((and exe version-args)
          (md-mermaid-cli--run-async-command
           (format "%s %s" exe (car version-args))
           (lambda (output)
             (funcall callback tool-name (string-trim output) nil))
           (lambda (error-msg)
             (funcall callback tool-name nil error-msg))))

         ;; No version check available
         (t
          (if exe
              (funcall callback tool-name "installed" nil)
            (funcall callback tool-name nil "Not found")))))
    (error
     (funcall callback tool-name nil (format "Check version error: %s" (error-message-string err))))))

;;; Process Management

(defun md-mermaid-cli--process-filter ()
  "Return a process filter that append output to the tools buffer."
  (lambda (proc chunk)
    (md-mermaid-cli--debug "FILTER %s: %d bytes" (process-name proc) (length chunk))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert chunk))))))

(defun md-mermaid-cli--format-elapsed (start-time)
  "Return human readable elapsed time since START-TIME."
  (when start-time
    (let* ((secs (float-time (time-subtract (current-time) start-time)))
           (mins (/ secs 60.0)))
      (cond
       ((< secs 1) (format "%.2fs" secs))
       ((< mins 1) (format "%.1fs" secs))
       (t (format "%.1fm" mins))))))

(defun md-mermaid-cli--create-process-sentinel (tool-name cmd callback)
  "Create a sentinel for TOOL-NAME running CMD.
CALLBACK is called as (tool-name success error-msg)."
  (lambda (proc _event)
    (let* ((exit (process-exit-status proc))
           (output (with-current-buffer (process-buffer proc) (buffer-string)))
           (elapsed (md-mermaid-cli--format-elapsed (process-get proc 'mm-start)))
           (ok (= exit 0)))
      (md-mermaid-cli--debug "Process %s exited %d (%s)" (process-name proc) exit elapsed)
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n<<< Process exited with code %d%s\n---\n"
                          exit (if elapsed (format " in %s" elapsed) "")))))
      (if ok
          (funcall callback tool-name t nil)
        (let* ((trimmed (md-mermaid-cli--strip-ansi (string-trim output)))
               (msg (if (string-empty-p trimmed)
                        (format "Command failed (exit %d): %s" exit cmd)
                      (format "Exit %d: %s" exit trimmed))))
          (funcall callback tool-name nil msg))))))

(defun md-mermaid-cli--make-process (tool-name cmd callback)
  "Create and start a process for TOOL-NAME to run CMD.
CALLBACK is invoked by sentinel as (tool-name success error-msg)."
  (let* ((output-buf (md-mermaid-cli--create-output-buffer))
         (shell-cmd (md-mermaid-cli--select-shell-command cmd))
         proc)
    (md-mermaid-cli--debug "make-process for %s: %S" tool-name shell-cmd)
    (let ((default-directory (or (getenv "HOME") default-directory))
          (process-connection-type nil))
      (setq proc (make-process
                  :name (format "md-mermaid-cli-%s" tool-name)
                  :buffer output-buf
                  :command shell-cmd
                  :connection-type 'pipe
                  :noquery t
                  :filter (md-mermaid-cli--process-filter)
                  :sentinel (md-mermaid-cli--create-process-sentinel tool-name cmd callback))))
    (process-put proc 'mm-start (current-time))
    (md-mermaid-cli--debug "Spawned process: %s status=%s" (process-name proc) (process-status proc))
    proc))


;;; Installation and Update - Helper Functions

(defun md-mermaid-cli--log-to-buffer (buf message)
  "Append MESSAGE to BUF with `inhibit-read-only'."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert message))))

(defun md-mermaid-cli--prepare-install-command (installer)
  "Prepare install command from INSTALLER entry, substituting npm client."
  (let* ((base (plist-get installer :command))
         (method (plist-get installer :method))
         (command (if (memq method '(npm pnpm yarn bun))
                      (replace-regexp-in-string "\\bnpm\\b" (md-mermaid-cli--get-preferred-npm) base)
                    base)))
    (if (memq method '(npm pnpm yarn bun))
        (concat command " --no-progress --no-audit --loglevel=error")
      command)))

(defun md-mermaid-cli--handle-install-error (buf tool-name error-msg callback)
  "Handle installation error for TOOL-NAME by logging to BUF and calling CALLBACK.
ERROR-MSG contains the error details."
  (md-mermaid-cli--log-to-buffer buf (format ">>> ERROR: %s\n---\n" error-msg))
  (funcall callback tool-name nil error-msg))

(defun md-mermaid-cli--start-install-process (tool-name command buf callback)
  "Start installation process for TOOL-NAME with COMMAND.
Logs to BUF and invokes CALLBACK when complete."
  (message "Installing %s..." tool-name)
  (md-mermaid-cli--log-to-buffer buf (format ">>> Installing %s\n---\n" tool-name))
  (md-mermaid-cli--make-process tool-name command callback))

(defun md-mermaid-cli--install-tool (tool-name callback)
  "Install TOOL-NAME asynchronously.
Call CALLBACK with (tool-name success error-msg)."
  (let* ((tool-entry (md-mermaid-cli--get-tool tool-name))
         (platform (md-mermaid-cli--detect-platform))
         (installers (and tool-entry (plist-get tool-entry :installers)))
         (installer (cl-find-if (lambda (inst) (memq platform (plist-get inst :platforms))) installers))
         (buf (md-mermaid-cli--create-output-buffer)))

    (md-mermaid-cli--display-output-buffer)

    (cond
     ((not tool-entry)
      (md-mermaid-cli--handle-install-error buf tool-name
                                            (format "Tool %s not in registry" tool-name)
                                            callback))

     ((not installer)
      (md-mermaid-cli--handle-install-error buf tool-name
                                            (format "No installer for %s on platform %s" tool-name platform)
                                            callback))

     (t
      (let* ((command (md-mermaid-cli--prepare-install-command installer))
             (method (plist-get installer :method))
             (validation-error (md-mermaid-cli--validate-command command method)))

        (md-mermaid-cli--log-to-buffer buf
                                       (format ">>> Preparing install for %s\nCommand: %s\nPlatform: %s\n"
                                               tool-name command platform))

        (cond
         (validation-error
          (md-mermaid-cli--handle-install-error buf tool-name
                                                (format "VALIDATION ERROR: %s" validation-error)
                                                callback))

         ((and md-mermaid-cli-confirm-installers
               (not (y-or-n-p (format "Install %s with: %s? " tool-name command))))
          (funcall callback tool-name nil "Installation cancelled by user"))

         (t
          (md-mermaid-cli--start-install-process tool-name command buf callback))))))))

(defun md-mermaid-cli--prepare-update-command (updater)
  "Prepare update command from UPDATER entry, substituting npm client."
  (md-mermaid-cli--prepare-install-command updater))

(defun md-mermaid-cli--start-update-process (tool-name command buf callback)
  "Start update process for TOOL-NAME with COMMAND.
Logs to BUF and invokes CALLBACK when complete."
  (message "Updating %s..." tool-name)
  (md-mermaid-cli--log-to-buffer buf (format ">>> Updating %s\n---\n" tool-name))
  (md-mermaid-cli--make-process tool-name command callback))

(defun md-mermaid-cli--update-tool (tool-name callback)
  "Update TOOL-NAME asynchronously.
Call CALLBACK with (tool-name success error-msg)."
  (let* ((tool-entry (md-mermaid-cli--get-tool tool-name))
         (platform (md-mermaid-cli--detect-platform))
         (updaters (and tool-entry (plist-get tool-entry :updaters)))
         (updater (cl-find-if (lambda (upd) (memq platform (plist-get upd :platforms))) updaters))
         (buf (md-mermaid-cli--create-output-buffer)))

    (md-mermaid-cli--display-output-buffer)

    (cond
     ((not tool-entry)
      (md-mermaid-cli--handle-install-error buf tool-name
                                            (format "Tool %s not in registry" tool-name)
                                            callback))

     ((not updater)
      (md-mermaid-cli--handle-install-error buf tool-name
                                            (format "No updater for %s on platform %s" tool-name platform)
                                            callback))

     (t
      (let* ((command (md-mermaid-cli--prepare-update-command updater))
             (method (plist-get updater :method))
             (validation-error (md-mermaid-cli--validate-command command method)))

        (cond
         (validation-error
          (md-mermaid-cli--handle-install-error buf tool-name
                                                (format "VALIDATION ERROR: %s" validation-error)
                                                callback))

         (t
          (md-mermaid-cli--log-to-buffer buf
                                         (format ">>> Preparing update for %s\nCommand: %s\nPlatform: %s\n"
                                                 tool-name command platform))
          (md-mermaid-cli--start-update-process tool-name command buf callback))))))))


;;; Interactive Commands

(defun md-mermaid-cli-install-tool (tool-name)
  "Install TOOL-NAME interactively."
  (interactive
   (list (intern (completing-read "Install tool: "
                                   (mapcar (lambda (entry) (symbol-name (plist-get entry :name)))
                                           md-mermaid-cli-tools-registry)
                                   nil t))))
  (md-mermaid-cli--install-tool
   tool-name
   (lambda (tool success error-msg)
     (if success
         (message "md-mermaid: %s installed successfully" tool)
       (message "md-mermaid: %s installation failed: %s" tool error-msg)))))

(defun md-mermaid-cli-update-tool (tool-name)
  "Update TOOL-NAME interactively."
  (interactive
   (list (intern (completing-read "Update tool: "
                                   (mapcar (lambda (entry) (symbol-name (plist-get entry :name)))
                                           md-mermaid-cli-tools-registry)
                                   nil t))))
  (md-mermaid-cli--update-tool
   tool-name
   (lambda (tool success error-msg)
     (if success
         (message "md-mermaid: %s updated successfully" tool)
       (message "md-mermaid: %s update failed: %s" tool error-msg)))))

(defun md-mermaid-cli-check-version (tool-name)
  "Check version of TOOL-NAME."
  (interactive
   (list (intern (completing-read "Check version: "
                                   (mapcar (lambda (entry) (symbol-name (plist-get entry :name)))
                                           md-mermaid-cli-tools-registry)
                                   nil t))))
  (md-mermaid-cli--check-version
   tool-name
   (lambda (_tool version error-msg)
     (if error-msg
         (message "md-mermaid: %s - %s" tool-name error-msg)
       (message "md-mermaid: %s - %s" tool-name version)))))

(defun md-mermaid-cli-install-all ()
  "Install all tools in registry."
  (interactive)
  (let ((tools (mapcar (lambda (entry) (plist-get entry :name))
                       md-mermaid-cli-tools-registry)))
    (message "Installing %d tools..." (length tools))
    (dolist (tool tools)
      (md-mermaid-cli--install-tool
       tool
       (lambda (tool-name success _error-msg)
         (message "md-mermaid: %s %s" tool-name (if success "installed" "failed")))))))

(defun md-mermaid-cli-update-all ()
  "Update all tools in registry."
  (interactive)
  (let ((tools (mapcar (lambda (entry) (plist-get entry :name))
                       md-mermaid-cli-tools-registry)))
    (message "Updating %d tools..." (length tools))
    (dolist (tool tools)
      (md-mermaid-cli--update-tool
       tool
       (lambda (tool-name success _error-msg)
         (message "md-mermaid: %s %s" tool-name (if success "updated" "failed")))))))

(defun md-mermaid-cli-check-all-versions ()
  "Check versions of all tools in registry."
  (interactive)
  (md-mermaid-cli-show-versions))

(defun md-mermaid-cli-view-tool-docs (tool-name)
  "View documentation for TOOL-NAME."
  (interactive
   (list (intern (completing-read "View docs for: "
                                   (mapcar (lambda (entry) (symbol-name (plist-get entry :name)))
                                           md-mermaid-cli-tools-registry)
                                   nil t))))
  (let* ((entry (md-mermaid-cli--get-tool tool-name))
         (docs (and entry (plist-get entry :docs)))
         (homepage (and entry (plist-get entry :homepage))))
    (with-current-buffer (get-buffer-create "*md-mermaid-tool-docs*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== %s Documentation ===\n\n" tool-name))
        (when homepage
          (insert (format "Homepage: %s\n\n" homepage)))
        (when docs
          (insert docs))
        (goto-char (point-min))
        (view-mode))
      (display-buffer (current-buffer)))))

;;; Settings and Toggles

(defun md-mermaid-cli-cycle-preferred-npm ()
  "Cycle preferred Node package manager."
  (interactive)
  (setq md-mermaid-cli-preferred-npm
        (pcase md-mermaid-cli-preferred-npm
          ("npm" "pnpm")
          ("pnpm" "yarn")
          ("yarn" "bun")
          (_ "npm")))
  (message "md-mermaid preferred npm client: %s" md-mermaid-cli-preferred-npm))

(defun md-mermaid-cli-set-preferred-npm (client)
  "Set preferred Node package manager CLIENT."
  (interactive
   (list (completing-read "Preferred npm client: " '("npm" "pnpm" "yarn" "bun")
                          nil t md-mermaid-cli-preferred-npm)))
  (setq md-mermaid-cli-preferred-npm client)
  (message "md-mermaid preferred npm client set to: %s" client))

(defun md-mermaid-cli-cycle-post-verify-show ()
  "Cycle versions table display after verification."
  (interactive)
  (setq md-mermaid-cli-post-verify-show
        (pcase md-mermaid-cli-post-verify-show
          ('none 'single)
          ('single 'all)
          (_ 'none)))
  (message "md-mermaid post-verify display: %s"
           (pcase md-mermaid-cli-post-verify-show
             ('none "NONE") ('single "SINGLE") ('all "ALL") (x x))))

(defun md-mermaid-cli-add-global-node-bin-to-path ()
  "Add the Node manager global bin directory to PATH.
This change applies for the current Emacs session."
  (interactive)
  (let ((bin (md-mermaid-cli--node-global-bin)))
    (if (and bin (not (string-empty-p bin)))
        (let* ((sep (if (eq (md-mermaid-cli--detect-platform) 'windows) ";" ":"))
               (cur (or (getenv "PATH") ""))
               (new (if (string-match (regexp-quote bin) cur) cur (concat bin sep cur))))
          (unless (member bin exec-path) (add-to-list 'exec-path bin))
          (setenv "PATH" new)
          (message "md-mermaid: added %s to PATH and exec-path" bin))
      (message "md-mermaid: could not determine global Node bin (check selected manager)"))))

(defun md-mermaid-cli--tool-found-in-path-p (tool-entry)
  "Check if TOOL-ENTRY executable is found in current PATH."
  (if (md-mermaid-cli--executable-find tool-entry) t nil))

(defun md-mermaid-cli--try-fix-path-with-global-bin ()
  "Attempt to add Node global bin to PATH if not already present."
  (let ((bin (md-mermaid-cli--node-global-bin)))
    (when (and bin (not (string-empty-p bin)))
      (let* ((sep (if (eq (md-mermaid-cli--detect-platform) 'windows) ";" ":"))
             (cur (or (getenv "PATH") ""))
             (already-in-path (string-match (regexp-quote bin) cur)))
        (unless already-in-path
          (setenv "PATH" (concat bin sep cur))
          (unless (member bin exec-path)
            (add-to-list 'exec-path bin))
          t)))))

(defun md-mermaid-cli--verify-and-fix-sentinel (tool-name tool-entry)
  "Check TOOL-NAME with TOOL-ENTRY and attempt PATH fix if needed.
Returns status message string."
  (if (md-mermaid-cli--tool-found-in-path-p tool-entry)
      (format "%s: found in PATH" tool-name)
    (if (md-mermaid-cli--try-fix-path-with-global-bin)
        (if (md-mermaid-cli--tool-found-in-path-p tool-entry)
            (format "%s: found after adding global bin to PATH" tool-name)
          (format "%s: not found (tried global bin fallback)" tool-name))
      (format "%s: not found (no global bin available)" tool-name))))

(defun md-mermaid-cli-verify-and-fix-path (tool-name)
  "Verify TOOL-NAME; if PATH fails, try global-bin fallback and add it to PATH."
  (interactive
   (list (intern (completing-read "Verify tool: "
                                   (mapcar (lambda (entry) (symbol-name (plist-get entry :name)))
                                           md-mermaid-cli-tools-registry)
                                   nil t))))
  (let ((tool-entry (md-mermaid-cli--get-tool tool-name)))
    (if tool-entry
        (message (md-mermaid-cli--verify-and-fix-sentinel tool-name tool-entry))
      (message "md-mermaid: tool %s not found in registry" tool-name))))

(defun md-mermaid-cli-show-versions (&optional tools)
  "Show a small table with detected versions for TOOLS.
When TOOLS is nil, show for all registered tools.
Appends results as they arrive."
  (interactive)
  (let* ((tool-list (or tools (mapcar (lambda (e) (plist-get e :name)) md-mermaid-cli-tools-registry)))
         (buf (get-buffer-create "*md-mermaid Versions*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "md-mermaid CLI Versions\n\n")
        (insert (format "%-18s | %-10s\n" "Tool" "Version"))
        (insert (make-string 32 ?-) "\n")
        (dolist (tname tool-list)
          (insert (format "%-18s | detecting...\n" (upcase (symbol-name tname))))))
      (read-only-mode 1)
      (goto-char (point-min))
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") (lambda () (interactive) (kill-buffer)))
        (define-key map (kbd "g") (lambda () (interactive)
                                    (kill-buffer)
                                    (md-mermaid-cli-show-versions)))
        (use-local-map map)))
    (display-buffer buf '((display-buffer-below-selected) (window-height . 0.3)))
    (dolist (tname tool-list)
      (md-mermaid-cli--check-version
       tname
       (lambda (_tool version error-msg)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (save-excursion
                 (goto-char (point-min))
                 (when (re-search-forward (format "^%s[[:space:]]*|" (upcase (symbol-name tname))) nil t)
                   (end-of-line)
                   (let ((eol (point)))
                     (search-backward "|")
                     (forward-char 1)
                     (delete-region (point) eol)
                     (insert (format " %-10s" (if error-msg (string-trim error-msg) (string-trim (or version "unknown")))))))))))))
      buf)))

(defun md-mermaid-cli-toggle-notifications ()
  "Toggle notifications on/off."
  (interactive)
  (setq md-mermaid-notify-on-complete (not md-mermaid-notify-on-complete))
  (message "md-mermaid notifications: %s" (if md-mermaid-notify-on-complete "ON" "OFF")))

(defun md-mermaid-cli-cycle-notify-method ()
  "Cycle notification method."
  (interactive)
  (setq md-mermaid-notify-method
        (pcase md-mermaid-notify-method
          ('emacs 'os)
          ('os 'both)
          (_ 'emacs)))
  (message "md-mermaid notify method: %s" md-mermaid-notify-method))

(defun md-mermaid-cli-cycle-batch-notify ()
  "Cycle batch notification mode."
  (interactive)
  (setq md-mermaid-notify-batch-when
        (pcase md-mermaid-notify-batch-when
          ('always 'focus-away)
          ('focus-away 'never)
          ('never 'always)
          (_ 'focus-away)))
  (message "md-mermaid batch notifications: %s" md-mermaid-notify-batch-when))

(defun md-mermaid-cli-toggle-debug ()
  "Toggle debug mode on/off."
  (interactive)
  (setq md-mermaid-debug-mode (not md-mermaid-debug-mode))
  (message "md-mermaid debug mode: %s" (if md-mermaid-debug-mode "ON" "OFF")))

(provide 'md-mermaid-tools)
;;; md-mermaid-tools.el ends here
