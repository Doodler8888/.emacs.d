;; Sessions - Simplified using built-in desktop functionality

(defvar current-desktop-session-name nil
  "The name of the currently loaded desktop session.")

;; Eshell buffer handling for desktop save/restore
(defun save-eshell-buffer (desktop-dirname)
  ;; Save the current working directory
  default-directory)

(defun restore-eshell-buffer (_file-name buffer-name misc)
  "Restore an eshell buffer with BUFFER-NAME in directory MISC."
  (let ((default-directory misc))
    (eshell buffer-name)))

;; Setup handlers for eshell-mode buffers
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local desktop-save-buffer #'save-eshell-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(eshell-mode . restore-eshell-buffer))

;; Set desktop parameters
(setq desktop-files-not-to-save
      (concat "\\(^/[^/:]*:\\|(ftp)$\\)\\|" desktop-files-not-to-save))

;; Disable the automatic desktop saving
(setq desktop-auto-save-timeout nil)

;; Save desktop session functions
(defun save-current-desktop-session ()
  "Save the current desktop session using the current session name.
If no session is loaded, prompt to create a new one."
  (interactive)
  (if (and current-desktop-session-name (not (string-empty-p current-desktop-session-name)))
      (let ((desktop-dir (concat user-emacs-directory "desktop/" current-desktop-session-name "/")))
        (unless (file-exists-p desktop-dir)
          (make-directory desktop-dir t))
        (desktop-save desktop-dir t) ;; Use built-in desktop-save with 'release' set to t
        (message "Session '%s' saved." current-desktop-session-name))
    ;; No session is loaded, prompt to create a new one
    (when (called-interactively-p 'any)
      (let ((new-session-name (read-string "Enter new session name: ")))
        (if (string-empty-p new-session-name)
            (message "Session name cannot be empty.")
          (setq current-desktop-session-name new-session-name)
          (let ((new-desktop-dir (concat user-emacs-directory "desktop/" new-session-name "/")))
            (make-directory new-desktop-dir t)
            (desktop-save new-desktop-dir t) ;; Use built-in desktop-save
            (message "Session '%s' created and saved." new-session-name)))))))

(defun load-desktop-session (session-name)
  "Load a desktop session by name."
  (interactive "sSession name: ")
  (when current-desktop-session-name
    ;; Save the current session before loading a new one
    (save-current-desktop-session))
  (let ((desktop-dir (concat user-emacs-directory "desktop/" session-name "/")))
    (if (file-exists-p desktop-dir)
        (progn
          (setq current-desktop-session-name session-name)
          (desktop-change-dir desktop-dir) ;; Use built-in desktop-change-dir
          (message "Loaded session '%s'." session-name))
      (message "Session '%s' does not exist." session-name))))

(defun load-desktop-with-name ()
  "Load a desktop session by name, chosen from available sessions."
  (interactive)
  (when current-desktop-session-name
    ;; Save the current session before loading a new one
    (save-current-desktop-session))
  (let* ((desktop-dir (concat user-emacs-directory "desktop/"))
         (session-dirs (directory-files desktop-dir nil "^[^.]"))  ; List directories excluding hidden ones
         (session-name (completing-read "Choose desktop session: " session-dirs nil t)))
    (when (and session-name (not (string-empty-p session-name)))
      (setq current-desktop-session-name session-name)
      (desktop-change-dir (concat desktop-dir session-name "/"))
      (message "Loaded session '%s'." session-name))))

(defun delete-desktop-session ()
  "Delete a desktop session by name, chosen from available sessions."
  (interactive)
  (let* ((desktop-dir (concat user-emacs-directory "desktop/"))
         (session-dirs (directory-files desktop-dir nil "^[^.]"))
         (session-name (completing-read "Choose desktop session to delete: " session-dirs nil t)))
    (when (and session-name 
               (not (string-empty-p session-name))
               (yes-or-no-p (format "Are you sure you want to delete the '%s' session? " session-name)))
      (let ((session-path (concat desktop-dir session-name)))
        (if (file-directory-p session-path)
            (progn
              (delete-directory session-path t)
              ;; Reset current session if we're deleting the active one
              (when (and current-desktop-session-name 
                         (string= current-desktop-session-name session-name))
                (setq current-desktop-session-name nil)
                (desktop-clear))
              (message "Deleted desktop session '%s'." session-name))
          (message "No such desktop session '%s'." session-name))))))

(defun rename-desktop-session ()
  "Renames the currently loaded desktop session."
  (interactive)
  (if (not current-desktop-session-name)
      (message "No desktop session is currently loaded.")
    (let* ((new-name (read-string "New session name: "))
           (old-dir (concat user-emacs-directory "desktop/" current-desktop-session-name))
           (new-dir (concat user-emacs-directory "desktop/" new-name)))
      (if (or (string-empty-p new-name)
              (file-exists-p new-dir))
          (message "Invalid new session name or session already exists.")
        (rename-file old-dir new-dir)
        (setq current-desktop-session-name new-name)
        (message "Session renamed to '%s'." new-name)))))

;; Save session on exit
(add-hook 'kill-emacs-hook 'save-current-desktop-session)

;; Save buffers before saving session
(advice-add 'save-current-desktop-session :before
            (lambda (&rest _)
              (save-some-buffers t)))

;; Explicitly disable desktop-save-mode to prevent automatic saving
(desktop-save-mode 0)
