(defun tramp-file-name-with-doas (filename)
  "Convert FILENAME into a multi-hop file name with \"doas\"."
  (let ((tramp-file-name-with-method "doas"))
    (tramp-file-name-with-sudo filename)))

(defun tramp-revert-buffer-with-doas ()
  "Revert current buffer to visit with \"doas\" permissions."
  (interactive)
  (let ((tramp-file-name-with-method "doas"))
    (tramp-revert-buffer-with-sudo)))


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

(defun my/dired-sudo-delete (&optional arg)
  "Delete files in Dired, using sudo if needed and respecting the trash settings.
If any file isn't writable, prompt with a sudo message.
If not using sudo, prompt normally.
When `delete-by-moving-to-trash' is non-nil, files are moved to trash rather than
being deleted permanently. For sudo files, the remote trash directory is used."
  (interactive "P")
  (let* ((files (mapcar #'expand-file-name (dired-get-marked-files t arg)))
         (needs-sudo (cl-some (lambda (f)
                                (let ((dir (file-name-directory f)))
                                  (and dir (not (file-writable-p dir)))))
                              files))
         (do-delete (if needs-sudo
                        (yes-or-no-p "Insufficient permissions. Use sudo? ")
                      (yes-or-no-p "Proceed with deletion? ")))
         (current-point (point)))
    (when do-delete
      (dolist (abs-file files)
        (let* ((dir (file-name-directory abs-file))
               (use-sudo (and dir (not (file-writable-p dir))))
               (tramp-file (if use-sudo
                               (format "/sudo::%s" abs-file)
                             abs-file)))
          (condition-case err
              (progn
                (if delete-by-moving-to-trash
                    (if use-sudo
                        ;; When using sudo, let-bind trash-directory to the remote trash directory.
                        (let ((trash-directory "/sudo::~/.local/share/trash/"))
                          (move-file-to-trash tramp-file))
                      (move-file-to-trash tramp-file))
                  (delete-file tramp-file))
                ;; Remove the entry from the buffer without full revert
                (dired-remove-entry abs-file))
            (error
             (message "Error deleting %s: %s" abs-file (error-message-string err))))))
      ;; Restore exact point position
      (goto-char current-point))))

(defun my/delete-file (file use-sudo)
  "Delete FILE using sudo if USE-SUDO is non-nil."
  (let ((target (if use-sudo
                    (format "/sudo::%s" (expand-file-name file))
                  file)))
    (condition-case err
        (delete-file target)
      (error (message "Error deleting %s: %s" file (error-message-string
                                                    err))))))


(defun my/dired-toggle-bak-extension ()
  "Toggle '.bak' extension for marked files/directories in Dired.
If an item doesn't end with '.bak', add it and prompt for copying;
if it ends with '.bak', remove it by renaming.
If the target file/directory exists, prompt to delete it before proceeding."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (num-files (length files))
         (msg-prefix (if (= num-files 1) "Item" "Items"))
         any-copied)
    (setq any-copied nil)
    (dolist (file files)
      (let* ((dir (file-name-directory file))
             (name (file-name-nondirectory file))
             (ext (file-name-extension name t))
             (is-bak (string= ext ".bak"))
             (keep-original (and (not is-bak) (y-or-n-p "Make copy? ")))
             (new-name (if is-bak
                           (file-name-sans-extension name)
                         (concat name ".bak")))
             (new-file (expand-file-name new-name dir))
             (proceed t)
             old-path)  ;; will hold the pre-rename path for directories
        ;; If target exists, ask to delete it.
        (when (file-exists-p new-file)
          (if (yes-or-no-p (format "Target %s already exists. Delete it? " new-file))
              (if (file-directory-p new-file)
                  (delete-directory new-file t)
                (delete-file new-file))
            (progn
              (message "Skipping %s because target exists." file)
              (setq proceed nil))))
        (when proceed
          (if (and (not is-bak) keep-original)
              (progn
                (if (file-directory-p file)
                    (copy-directory file new-file t t t)
                  (copy-file file new-file t))
                (setq any-copied t))
            (progn
              ;; For directories, compute old-path before renaming.
              (when (file-directory-p file)
                (setq old-path (file-truename file)))
              ;; Rename the file/directory
              (rename-file file new-file t)
              ;; Update any buffer visiting this file
              (let ((buffer (get-file-buffer file)))
                (when buffer
                  (with-current-buffer buffer
                    (set-visited-file-name new-file nil t))))
              ;; If it's a directory, update all buffers within that directory
              (when (file-directory-p new-file)
                (let ((new-path (file-truename new-file)))
                  (dolist (buf (buffer-list))
                    (let ((buf-file (buffer-file-name buf)))
                      (when (and buf-file old-path (string-prefix-p old-path buf-file))
                        (with-current-buffer buf
                          (let ((relative-path (substring buf-file (length old-path))))
                            (set-visited-file-name (concat new-path relative-path) nil t))))))))))))
    ;; Refresh the dired buffer and show a summary message.
    (revert-buffer)
    (message "%s %s." msg-prefix (if any-copied "copied" "renamed")))))

	
(defun my/dired-create-empty-files ()
  "Create multiple empty files in current dired directory.
Creates each file immediately after it is entered."
  (interactive)
  (let (done)
    (while (not done)
      (condition-case nil
          (let* ((file (completing-read
                       (format "File to create (C-g when done): ")
                       #'completion-file-name-table
                       nil nil))
                 (filepath (expand-file-name file default-directory)))
            (dired-create-empty-file filepath)
            (revert-buffer))
        (quit (setq done t))))))


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
  ;; (define-key dired-mode-map (kbd "D") 'my/dired-sudo-delete)
  ;; Disabled, because my delete function doesn't work with the delete marks,
  ;; only on usual marks, and it creates accidental deletions of files i'm not
  ;; supposed to delete.
  ;; (define-key dired-mode-map (kbd "d") nil)
  (define-key dired-mode-map (kbd "b") 'my/dired-toggle-bak-extension)
  ;; (define-key dired-mode-map (kbd "j") 'dired-next-line-preserve-column)
  ;; (define-key dired-mode-map (kbd "k") 'dired-previous-line-preserve-column)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (define-key dired-mode-map (kbd "SPC") 'my-space-as-ctrl-c)
  (define-key dired-mode-map (kbd "I") 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "i") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "/") 'isearch-forward)
  (define-key dired-mode-map (kbd "?") 'isearch-backward)
  (define-key dired-mode-map (kbd "T") 'my/dired-create-empty-files))

