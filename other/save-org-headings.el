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

(defvar my/org-fold-state-dir 
  (expand-file-name "org-fold-states" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory to store org fold state files.")

(defun my/org-heading-folded-p ()
  "Check if the current Org heading is folded."
  (when (org-at-heading-p)
    (save-excursion
      (end-of-line)
      (and (not (eobp))
           (org-fold-folded-p)
           (progn (forward-char) (org-invisible-p))))))

(defun my/org-get-heading-signature ()
  "Get a unique signature for current heading based on content and structure."
  (when (org-at-heading-p)
    (let* ((heading (substring-no-properties (org-get-heading t t t t)))
           (level (org-outline-level))
           (parent-heading (save-excursion
                           (when (> level 1)
                             (outline-up-heading 1 t)
                             (substring-no-properties (org-get-heading t t t t))))))
      (list :heading heading :level level :parent parent-heading))))

(defun my/org-save-fold-state ()
  "Save the fold state of all headings in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (unless (file-exists-p my/org-fold-state-dir)
      (make-directory my/org-fold-state-dir t))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (f-join my/org-fold-state-dir 
                                   (concat (f-base file-name) "-" file-hash ".foldstate")))
           fold-state)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((signature (my/org-get-heading-signature))
                 (folded (my/org-heading-folded-p)))
            (when signature
              (push (append signature (list :folded folded)) fold-state)))))
      
      (setq fold-state (nreverse fold-state))
      (with-temp-file fold-state-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 (list :file file-name :state fold-state) (current-buffer)))))))

(defun my/org-restore-fold-state ()
  "Restore the fold state of all headings in the current Org buffer."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-hash (md5 (expand-file-name file-name)))
           (fold-state-file (f-join my/org-fold-state-dir 
                                   (concat (f-base file-name) "-" file-hash ".foldstate"))))
      (when (file-exists-p fold-state-file)
        (let* ((fold-data (with-temp-buffer
                           (insert-file-contents fold-state-file)
                           (read (current-buffer))))
               (stored-file (plist-get fold-data :file))
               (fold-state (plist-get fold-data :state)))
          (when (string= stored-file file-name)
            (save-excursion
              (org-show-all)
              (dolist (stored-heading fold-state)
                (goto-char (point-min))
                (while (and (not (eobp))
                           (re-search-forward org-heading-regexp nil t))
                  (let ((current-sig (my/org-get-heading-signature)))
                    (when (and current-sig
                             (equal (plist-get stored-heading :heading) 
                                    (plist-get current-sig :heading))
                             (equal (plist-get stored-heading :level)
                                    (plist-get current-sig :level))
                             (equal (plist-get stored-heading :parent)
                                    (plist-get current-sig :parent)))
                      (when (plist-get stored-heading :folded)
                        (outline-hide-subtree))
                      (goto-char (point-max)))))))))))))

(defun my/org-save-fold-state-after-cycle (&rest _)
  "Save fold state after cycling a heading."
  (when (and (eq major-mode 'org-mode) (buffer-file-name))
    (my/org-save-fold-state)))

(advice-add 'org-cycle :after #'my/org-save-fold-state-after-cycle)
(add-hook 'find-file-hook #'my/org-restore-fold-state)
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode) (buffer-file-name))
              (my/org-save-fold-state))))


