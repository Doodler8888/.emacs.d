(unless (package-installed-p 'f)
  (package-install 'f))
(require 'f)
(require 'subr-x)


(defun my/org-fold-all-headings ()
  "Fold all headings in the current Org buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p) (outline-next-heading))
      (while (not (eobp))
        (outline-hide-subtree)
        (outline-next-heading)))))

(defvar my/org-debug-log-file "/home/wurfkreuz/org-fold-debug.log"
  "File to store debug logs for org fold state operations.")

(defun my/org-debug-log (message &rest args)
  "Write a debug MESSAGE with ARGS to the debug log file."
  (let ((log-message (format "[%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S") (apply #'format message args))))
    (with-temp-buffer
      (insert log-message)
      (write-region (point-min) (point-max) my/org-debug-log-file t 'silent))))

;; Add these helper functions for better debugging
(defun my/org-debug-heading-info ()
  "Return detailed info about current heading for debugging."
  (when (org-at-heading-p)
    (let ((heading (org-get-heading t t t t))
          (level (org-outline-level))
          (pos (point))
          (has-content (save-excursion
                        (forward-line)
                        (not (org-at-heading-p)))))
      (format "Level: %d, Pos: %d, Has-content: %s, Heading: %s"
              level pos has-content (substring-no-properties heading)))))

(defun my/org-debug-fold-state-info (fold-state)
  "Return detailed info about fold state for debugging."
  (format "Total headings: %d, First heading pos: %s, Last heading pos: %s"
          (length fold-state)
          (plist-get (car fold-state) :position)
          (plist-get (car (last fold-state)) :position)))

;; Modify my/org-heading-folded-p with more debugging
(defun my/org-heading-folded-p ()
  "Check if the current Org heading is folded."
  (let* ((at-heading (org-at-heading-p))
         (result (when at-heading
                  (save-excursion
                    (end-of-line)
                    (let ((eol-pos (point))
                          (is-folded (when (and (not (eobp))
                                              (org-fold-folded-p))
                                     (forward-char)
                                     (org-invisible-p))))
                      (my/org-debug-log "Fold check details - EOL pos: %d, Is-folded: %s"
                                      eol-pos is-folded)
                      is-folded)))))
    (my/org-debug-log "Heading fold check at %d: At-heading: %s, Result: %s, Info: %s"
                     (point) at-heading result (my/org-debug-heading-info))
    (if result t nil)))

(defvar my/org-fold-state-dir (expand-file-name "org-fold-states" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory to store org fold state files.")

(defun my/org-save-fold-state ()
  "Save the fold state of all headings in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-debug-log "Starting fold state save for buffer: %s (buffer size: %d)"
                     (buffer-name) (buffer-size))
    (unless (file-exists-p my/org-fold-state-dir)
      (make-directory my/org-fold-state-dir t))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (f-join my/org-fold-state-dir (concat (f-base file-name) "-" file-hash ".foldstate")))
           (fold-state '())
           (heading-count 0)
           (processed-positions '()))
      (my/org-debug-log "Saving fold state to file: %s" fold-state-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((level (org-outline-level))
                 (heading (org-get-heading t t t t))
                 (folded (my/org-heading-folded-p))
                 (begin (line-beginning-position)))
            (cl-incf heading-count)
            (push begin processed-positions)
            (my/org-debug-log "Processing heading %d: Pos %d, Level %d, Folded %s, Next-pos %s, Heading: %s"
                            heading-count
                            begin
                            level
                            folded
                            (save-excursion 
                              (outline-next-heading)
                              (if (eobp) "EOF" (point)))
                            (substring-no-properties heading))
            (push (list :level level
                       :heading (substring-no-properties heading)
                       :folded folded
                       :position begin)
                  fold-state)))
        (my/org-debug-log "Processed positions in order: %S" (nreverse processed-positions)))
      
      (setq fold-state (nreverse fold-state))
      (with-temp-file fold-state-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 (list :file file-name :state fold-state) (current-buffer))))
      (my/org-debug-log "Fold state saved for %s with %d headings" 
                       (f-filename file-name) 
                       (length fold-state))
      (my/org-debug-log "Completed fold state save for %s" (buffer-name)))))

(defun my/org-restore-fold-state ()
  "Restore the fold state of all headings in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-debug-log "Starting fold state restore for buffer: %s (buffer size: %d)"
                     (buffer-name) (buffer-size))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (f-join my/org-fold-state-dir (concat (f-base file-name) "-" file-hash ".foldstate"))))
      (my/org-debug-log "Looking for fold state file: %s" fold-state-file)
      (if (file-exists-p fold-state-file)
          (let* ((fold-data (with-temp-buffer
                             (insert-file-contents fold-state-file)
                             (read (current-buffer))))
                 (stored-file (plist-get fold-data :file))
                 (fold-state (plist-get fold-data :state)))
            (my/org-debug-log "Fold state file found. Stored file: %s, Current file: %s" 
                            stored-file file-name)
            (if (string= stored-file file-name)
                (save-excursion
                  (org-show-all)
                  (my/org-debug-log "Showing all headings before restoring state")
                  (let ((restore-count 0)
                        (failed-restores 0))
                    (dolist (heading fold-state)
                      (let ((pos (plist-get heading :position))
                            (level (plist-get heading :level))
                            (folded (plist-get heading :folded)))
                        (cl-incf restore-count)
                        (goto-char pos)
                        (if (org-at-heading-p)
                            (progn
                              (let ((current-level (org-outline-level)))
                                (my/org-debug-log "Restoring heading %d: Pos %d, Expected level %d, Actual level %d, Folded %s"
                                                restore-count pos level current-level folded)
                                (if folded
                                    (outline-hide-subtree)
                                  (outline-show-entry)
                                  (outline-show-children))))
                          (cl-incf failed-restores)
                          (my/org-debug-log "Failed to restore heading at position %d (heading %d of %d)"
                                          pos restore-count (length fold-state)))))
                    (my/org-debug-log "Restore complete: %d processed, %d failed"
                                    restore-count failed-restores)))
              (my/org-debug-log "Stored file name does not match current file name")))
        (my/org-debug-log "Fold state file not found: %s" fold-state-file)))
    (my/org-debug-log "Completed fold state restore for %s" (buffer-name))))

(defun my/org-save-fold-state-after-cycle (&rest _)
  "Save fold state after cycling a heading."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-debug-log "Saving fold state after cycling in buffer: %s" (buffer-name))
    (my/org-save-fold-state)))

(defun my/org-maybe-restore-fold-state ()
  "Restore fold state if the current buffer is an Org file."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-debug-log "Attempting to restore fold state for newly opened file: %s" (buffer-name))
    (my/org-restore-fold-state)))

;; Hook to save fold state after cycling
(advice-add 'org-cycle :after #'my/org-save-fold-state-after-cycle)

;; Hook to restore fold state when opening a file
(add-hook 'find-file-hook #'my/org-maybe-restore-fold-state)

;; Optional: Save fold state when killing an Org buffer
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode) (buffer-file-name))
              (my/org-debug-log "Saving fold state before killing buffer: %s" (buffer-name))
              (my/org-save-fold-state))))


