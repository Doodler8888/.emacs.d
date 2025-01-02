(defun my/dired-sudo-symlink (&optional arg)
  "Create a symlink with sudo capability if needed."
  (interactive "P")
  (let* ((src-files (dired-get-marked-files t arg))
         (dest (read-file-name (format "Symlink %s to: " 
                                       (if (cdr src-files) "files" "file"))
                               (dired-dwim-target-directory)))
         (dest-dir (if (file-directory-p dest) dest (file-name-directory dest)))
         (use-sudo (not (file-writable-p dest-dir)))
         (hostname (system-name)))
    (if (not use-sudo)
        (my/create-symlinks src-files dest)
      (if (yes-or-no-p "Insufficient permissions. Use sudo? ")
          (let ((sudo-dest (format "/sudo:root@%s:%s" hostname dest)))
            (my/create-symlinks src-files sudo-dest t))
        (message "Symlink creation cancelled.")))))

(defun my/create-symlinks (src-files dest &optional use-sudo)
  "Create symlinks for SRC-FILES at DEST. Use sudo if USE-SUDO is non-nil."
  (let ((target-is-dir (file-directory-p dest)))
    (dolist (src src-files)
      (let* ((src-name (file-name-nondirectory src))
             (full-dest (if target-is-dir
                            (expand-file-name src-name dest)
                          dest))
             (sudo-src (if use-sudo
                           (concat "/sudo::" (expand-file-name src))
                         (expand-file-name src)))
             (sudo-dest (if use-sudo
                            (replace-regexp-in-string "^/sudo::[^:]+:" "" full-dest)
                          full-dest)))
        (condition-case err
            (progn
              (if use-sudo
                  (with-temp-buffer
                    (let ((default-directory "/sudo::"))
                      (make-symbolic-link sudo-src sudo-dest t)))
                (make-symbolic-link sudo-src sudo-dest t))
              (message "Created symlink: %s -> %s" 
                       (if use-sudo (concat "/sudo::..." (file-name-nondirectory src)) src)
                       (if use-sudo (concat "/sudo::..." (file-name-nondirectory sudo-dest)) sudo-dest)))
          (file-error
           (message "Error creating symlink: %s" (error-message-string err))))))))


(defun my/dired-toggle-bak-extension ()
  "Toggle '.bak' extension for marked files/directories in Dired.
If an item doesn't end with '.bak', add it; if it does, remove it.
Prompts whether to keep the original or not."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (num-files (length files))
         (msg-prefix (if (= num-files 1) "Item" "Items"))
         (keep-original (y-or-n-p "Make copy? ")))
    (dolist (file files)
      (let* ((dir (file-name-directory file))
             (name (file-name-nondirectory file))
             (ext (file-name-extension name t))
             new-name)
        (if (string= ext ".bak")
            (setq new-name (file-name-sans-extension name))
          (setq new-name (concat name ".bak")))
        (let ((new-file (expand-file-name new-name dir)))
          (when (or (not (file-exists-p new-file))
                    (yes-or-no-p (format "%s already exists. Overwrite? " new-file)))
            (if (file-directory-p file)
                (if keep-original
                    (copy-directory file new-file t t t)
                  (rename-file file new-file t))
              (if keep-original
                  (copy-file file new-file t)
                (rename-file file new-file t)))))))
    (revert-buffer)
    (message "%s %s." msg-prefix (if keep-original "copied" "renamed"))))


(defun my/dired-create-empty-files ()
  "Create multiple empty files in current dired directory."
  (interactive)
  (let (files done)
    (while (not done)
      (condition-case nil
          (let ((file (completing-read
                      (format "File %s (C-g when done): "
                             (if files
                                 (format "[added: %s]"
                                         (mapconcat #'identity files " "))
                               ""))
                      #'completion-file-name-table
                      nil nil)))
            (push file files))
        (quit (setq done t))))
    (when files
      (dolist (file (nreverse files))
        (let ((filepath (expand-file-name file default-directory)))
          (dired-create-empty-file filepath)))
      (revert-buffer))))


(defun my/dired-get-size-with-du ()
  "Get size of file/directory at point using du."
  (interactive)
  (let* ((file (dired-get-filename))
         (size (string-trim (shell-command-to-string 
                            (format "du -sh %s" (shell-quote-argument file))))))
    (message "%s" size)))


(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "S") 'my/dired-sudo-symlink)
  (define-key dired-mode-map (kbd "s") 'my/dired-get-size-with-du)
  (define-key dired-mode-map (kbd "b") 'my/dired-toggle-bak-extension)
  (define-key dired-mode-map (kbd "T") 'my/dired-create-empty-files))

