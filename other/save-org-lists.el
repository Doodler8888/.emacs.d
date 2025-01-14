(defvar my/org-list-fold-state-dir
  (expand-file-name "org-list-fold-states" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory to store Org list fold state files.")

(defun my/org-debug (message &rest args)
  "Log a debug MESSAGE with ARGS to the *Messages* buffer."
  (apply #'message (concat "[my/org-list-debug] " message) args))

(defun my/org-get-list-item-signature ()
  "Get a unique signature for the current list item based on its content."
  (when (org-at-item-p)
    (let ((item (substring-no-properties (org-get-entry))))
      (my/org-debug "Captured signature for item: %s" item)
      (list :item item))))

(defun my/org-save-list-fold-state ()
  "Save the fold state of all list items in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (unless (file-exists-p my/org-list-fold-state-dir)
      (make-directory my/org-list-fold-state-dir t))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (expand-file-name
                             (concat (file-name-base file-name) "-" file-hash ".listfoldstate")
                             my/org-list-fold-state-dir))
           fold-state)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-list-full-item-re nil t)
          (let* ((signature (my/org-get-list-item-signature))
                 (folded (org-list-at-item-p)))
            (when signature
              (push (append signature (list :folded folded)) fold-state)))))
      (setq fold-state (nreverse fold-state))
      (with-temp-file fold-state-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 (list :file file-name :state fold-state) (current-buffer))))
      (my/org-debug "Saved list fold state to %s" fold-state-file))))

(defun my/org-restore-list-fold-state ()
  "Restore the fold state of all list items in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (expand-file-name
                             (concat (file-name-base file-name) "-" file-hash ".listfoldstate")
                             my/org-list-fold-state-dir)))
      (if (not (file-exists-p fold-state-file))
          (my/org-debug "No fold state file found for %s" file-name)
        (let* ((fold-data (with-temp-buffer
                            (insert-file-contents fold-state-file)
                            (read (current-buffer))))
               (stored-file (plist-get fold-data :file))
               (fold-state (plist-get fold-data :state)))
          (if (not (string= stored-file file-name))
              (my/org-debug "Fold state file mismatch for %s" file-name)
            (save-excursion
              (goto-char (point-min))
              (dolist (stored-item fold-state)
                (while (re-search-forward org-list-full-item-re nil t)
                  (let ((current-sig (my/org-get-list-item-signature)))
                    (when (and current-sig
                               (equal (plist-get stored-item :item)
                                      (plist-get current-sig :item)))
                      (if (plist-get stored-item :folded)
                          (org-cycle)
                        (org-cycle)))))))))))))

(defun my/org-save-list-fold-state-after-cycle (&rest _)
  "Save list fold state after cycling a list item."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-save-list-fold-state)))

(advice-add 'org-cycle :after #'my/org-save-list-fold-state-after-cycle)
(add-hook 'find-file-hook #'my/org-restore-list-fold-state)
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode) (buffer-file-name))
              (my/org-save-list-fold-state))))
