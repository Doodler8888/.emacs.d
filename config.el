;; -*- lexical-binding: t -*-

;; Visuals

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq minibuffer-message-timeout 0)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'visual
      display-line-numbers-type 'relative)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-space-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defun enable-line-numbers-in-messages-buffer ()
  (with-current-buffer "*Messages*"
    (display-line-numbers-mode 1)))
(add-hook 'after-init-hook 'enable-line-numbers-in-messages-buffer)
(advice-add 'message :after 
            (lambda (&rest _) 
              (when (get-buffer "*Messages*")
                (with-current-buffer "*Messages*"
                  (display-line-numbers-mode 1)))))

(defun my-mode-line-major-mode ()
  "Returns a clean name of the current major mode."
  (let ((mode (format "%s" major-mode)))
    (replace-regexp-in-string "-mode$" "" mode)))

(defun my-vc-branch ()
  "Get the current Git branch name, if any."
  (unless (or (eq major-mode 'eshell-mode)
              (eq major-mode 'special-mode)  ; for *scratch*, *Messages*, etc
              (string-prefix-p "*" (buffer-name)))  ; any special buffer
    (with-temp-buffer
      (condition-case nil
          (when (zerop (call-process "git" nil t nil "branch" "--show-current"))
            (let ((branch (string-trim (buffer-string))))
              (unless (string-empty-p branch)
                (if (> (length branch) 40)
                    (concat (substring branch 0 37) "...")
                  branch))))
        (error nil)))))

(defun my-window-number ()
  "Get the current window number."
  (let* ((windows (window-list-1 (frame-first-window) 'nomini t))
         (num (cl-position (selected-window) windows)))
    (format "%d" (1+ (or num 0)))))

(setq-default mode-line-format
              '("%e"
                (:eval (my-window-number))
                ""  ; Single space after the window number
                mode-line-front-space
                (:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b"))
                " | "
                (:eval (my-mode-line-major-mode))
                " | "
                (:eval (or (my-vc-branch) ""))
                (:eval (propertize " " 'display '(space :align-to (- right 12))))
                mode-line-end-spaces))


;; ;; Tabs

(setq tab-bar-tab-name-format-function #'my-tab-bar-vim-name-format-function)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-separator "\u200B")  ;; Zero width space to fix color bleeding
(setq tab-bar-tab-hints nil)  ;; Tab numbers of the left of the label
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-auto-width nil)

(defun my-tab-name-format-function (tab i)
  (defface my-active-tab-face
    '((t :background "#2e2c3d" :foreground "#e0def4"))
    "Face for the active tab.")
  (defface my-inactive-tab-face
    '((t :background "#1d1f21" :foreground "#6e6a86"))
    "Face for the inactive tab.")
  (let ((current-p (eq (car tab) 'current-tab))
        (tab-name (format "%d %s" i (alist-get 'name (cdr tab)))))
    ;; Add padding around the tab name
    (setq tab-name (format " %s " tab-name))  ;; Add a space before and after the tab name
    (if current-p
        (propertize tab-name 'face 'my-active-tab-face)
      (propertize tab-name 'face 'my-inactive-tab-face))))

(setq tab-bar-tab-name-format-function #'my-tab-name-format-function)

(dotimes (i 9)
  (let ((n (1+ i)))  ; Tab numbers start from 1
    (global-set-key (kbd (format "M-%d" n))
                    `(lambda () (interactive) (tab-bar-select-tab ,n)))))

;; System

(setq erc-nick "wurfkreuz")
(global-set-key (kbd "C-x u") 'windmove-up)
(save-some-buffers t)

(recentf-mode)

(setq vc-follow-symlinks t)

(setq dired-recursive-deletes 'always)

(setq desktop-load-locked-desktop t)
(setq backup-inhibited t)

(add-hook 'prog-mode-hook (show-paren-mode t))

;; Auto pairing
(add-hook 'prog-mode-hook (electric-pair-mode t))
;; I don't know what it does exactly, it's more like a test
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; Don't pair '<'
(setq electric-pair-inhibit-predicate
      `(lambda (c)
        (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))

;; Break lines after a certain length
(setq sentence-end-double-space nil)
(auto-fill-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)


(setq python-shell-interpreter "/usr/bin/python3")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))

;; Executable on save if starts with '#!'
(add-hook 'after-save-hook
        'executable-make-buffer-file-executable-if-script-p)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-history"))))
(make-directory (concat user-emacs-directory "auto-saves") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))
(setq auto-save-list-file-prefix (concat user-emacs-directory "auto-saves/.saves-"))
;; There was a situation where emacs created an autosave file in a directory
;; that i was currently for an eshell buffer.
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local auto-save-default nil)))
(make-directory (concat user-emacs-directory "lock-files") t)
(setq lock-file-name-transforms
      `((".*" ,(concat user-emacs-directory "lock-files/") t)))
(setq desktop-dirname (concat user-emacs-directory "desktop/"))
(make-directory (concat user-emacs-directory "backups") t)
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))

(defun my-disable-auto-save-for-scratch ()
(when (string= (buffer-name) "*scratch*")
  (auto-save-mode -1)))
(add-hook 'lisp-interaction-mode-hook 'my-disable-auto-save-for-scratch)

(defun disable-auto-save-for-eshell ()
  "Disable auto-save for eshell buffers."
  (when (eq major-mode 'eshell-mode)
    (setq-local auto-save-default nil)))
(add-hook 'eshell-mode-hook #'disable-auto-save-for-eshell)

(defun disable-auto-save-for-messages-buffer ()
  "Disable auto-save for the *Messages* buffer."
  (when (string= (buffer-name) "*Messages*")
    (setq-local auto-save-default nil)
    (auto-save-mode -1)))
(add-hook 'after-change-major-mode-hook #'disable-auto-save-for-messages-buffer)

(defun disable-auto-save-for-non-file-buffers ()
  "Disable auto-save for buffers not associated with a file."
  (unless (buffer-file-name)
    (setq-local auto-save-default nil)
    (auto-save-mode -1)))

(add-hook 'after-change-major-mode-hook #'disable-auto-save-for-non-file-buffers)

;; Save sessions
(unless (file-exists-p desktop-dirname)
  (make-directory desktop-dirname))
(desktop-save-mode 1)
(setq desktop-save 't)
(setq desktop-path (list desktop-dirname))
(setq desktop-auto-save-timeout 30)
(setq desktop-auto-save-timeout nil)

(auto-save-mode 1)
(setq auto-save-interval 1)  ; Auto-save every 1 second
(setq auto-save-timeout 10)  ; Auto-save after 10 seconds of idle time
(setq auto-save-no-message t)

(setq save-place-file (concat user-emacs-directory "saveplace/places"))

;; Save cursor position
(unless (file-exists-p (concat user-emacs-directory "saveplace/"))
  (make-directory (concat user-emacs-directory "saveplace/")))
(save-place-mode 1)

;; (if (version< emacs-version "29.0")
;;     (pixel-scroll-mode)
;;   (pixel-scroll-precision-mode 1)
;;   (setq pixel-scroll-precision-large-scroll-height 35.0))

(setq scroll-conservatively 101)
(setq scroll-margin 5)
(setq scroll-step 1)

(scroll-bar-mode -1)
(setq-default display-line-numbers-width 3)

(setq use-dialog-box nil)
(fringe-mode '(1 . 1))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(set-default 'truncate-lines t)
;; (add-hook 'prog-mode-hook (lambda ()
;;                            ;; (setq-local truncate-lines t)
;;                            (toggle-truncate-lines 1)))
;; ;; Specific for emacs-lisp-mode
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq-local truncate-lines t)
;;             (toggle-truncate-lines 1)))
;; ;; ;; Also, you can check what's changing the setting
;; ;; (add-variable-watcher 'truncate-lines
;; ;;                      (lambda (sym val op where)
;; ;;                        (message "truncate-lines changed to %s in %s" val where)))


(setq enable-local-variables t)
(setq enable-dir-local-variables t)

(let ((paths '("/home/wurfkreuz/.nix-profile/bin"
              "/home/wurfkreuz/.ghcup/bin"
              "/home/wurfkreuz/test-dir/"
              "/usr/bin")))
  ;; (setq exec-path (append paths exec-path))
  (setenv "PATH" (concat (string-join paths ":")
                        ":"
                        (getenv "PATH"))))

(require 'midnight)
(midnight-delay-set 'midnight-delay "10:00pm")

(setq auto-revert-verbose nil)

(setq display-buffer-base-action '(nil . ((some-window . mru))))

(minibuffer-regexp-mode 1)

(setq ielm-history-file-name "~/.emacs.d/.ielm-history")

(defun my-tramp-cleanup ()
  "Clean up TRAMP buffers and connections on Emacs exit."
  (when (fboundp 'tramp-cleanup-all-buffers)
    (tramp-cleanup-all-buffers))
  (when (fboundp 'tramp-cleanup-all-connections)
    (tramp-cleanup-all-connections)))

(add-hook 'kill-emacs-hook #'my-tramp-cleanup)

;; Save unexuted minibuffer input

(defvar my-last-unexecuted-minibuffer-input nil
  "Stores the last unexecuted minibuffer input.")

(defun my-save-unexecuted-minibuffer-input ()
  "Save the current minibuffer input if it's not empty."
  (let ((input (minibuffer-contents)))
    (when (and (not (string-empty-p input))
               (not (eq input my-last-unexecuted-minibuffer-input)))
      (setq my-last-unexecuted-minibuffer-input input))))

(add-hook 'minibuffer-exit-hook #'my-save-unexecuted-minibuffer-input)

(defun my-insert-last-unexecuted-minibuffer-input ()
  "Insert the last unexecuted minibuffer input at point."
  (interactive)
  (when my-last-unexecuted-minibuffer-input
    (insert my-last-unexecuted-minibuffer-input)))

;; Bind it to C-r in minibuffer-local-map
(define-key minibuffer-local-map (kbd "C-r") #'my-insert-last-unexecuted-minibuffer-input)


;; Cursor

(blink-cursor-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)


;; Theme

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))(put 'eval 'safe-local-variable #'identity)
(load-theme 'rose-pine t)

;; SQL mode
(defun my-sql-mode-custom-faces ()
  "Customize faces for SQL mode."
  (face-remap-add-relative 'font-lock-builtin-face :foreground "#9ccfd8"))

(add-hook 'sql-mode-hook 'my-sql-mode-custom-faces)
(add-hook 'sql-interactive-mode-hook 'my-sql-mode-custom-faces)

;; Terraform mode
(defun my-terraform-mode-custom-faces ()
  "Customize faces for terraform mode."
  (face-remap-add-relative 'font-lock-type-face :foreground "#9ccfd8"))

(add-hook 'terraform-mode-hook 'my-terraform-mode-custom-faces)

(when (member "NotoSansM Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "NotoSansM Nerd Font Mono-12:weight=medium")

  ;; Set a different font for italics
  (set-face-attribute 'italic nil
                      :family "NotoSans Nerd Font"
                      :slant 'italic
                      :weight 'normal
                      :height 130)

  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'org-verbatim nil
                                  ;; :family "NotoSerifNerdFontPropo-CondensedExtraLight"
                                  :family "NotoSerifNerdFont"
                                  :height 130
                                  ;; :foreground "#8bc34a"  ; Adjust the color as desired
                                  :weight 'normal))))

;; This is a code that tries to fix a sitaution where commented characters
;; inside single quotes have their own face
(defun in-string-or-sexp-or-keyword-p (pos)
  "Return t if POS is inside a string, sexp, or keyword argument."
  (save-excursion
    (goto-char pos)
    (let* ((ppss (syntax-ppss))
           (in-string (nth 3 ppss))
           (sexp-pos (nth 1 ppss)))
      (or in-string
          (and sexp-pos
               (save-excursion
                 (goto-char sexp-pos)
                 (looking-at-p "(:.*")))))))  ; check if inside keyword list

(defun force-comment-face ()
  "Force comment face for comments, handling both line comments and inline comments."
  (font-lock-add-keywords
   nil
   `((,(lambda (limit)
         (let (found-pos)
           (while (and (not found-pos)
                      (re-search-forward ";.*$" limit t))
             (let* ((semi-pos (- (match-beginning 0) 1))
                    (check-pos (1+ semi-pos))
                    (is-special (in-string-or-sexp-or-keyword-p check-pos)))
               (when (not is-special)
                 (setq found-pos (point)))))
           found-pos))
      (0 font-lock-comment-face prepend)))
   'append))

;; This is a code that tries to fix a sitaution where characters within double
;; quotes and inside single quotes have there own fontification characters
;; within double quotes and inside single quotes have there own face
(defun in-double-quotes-p (pos)
  "Return t if POS is inside double quotes."
  (save-excursion
    (goto-char pos)
    (let* ((state (syntax-ppss))
           (in-string (nth 3 state))
           (string-start (nth 8 state))
           (double-quote (and string-start 
                            (eq ?\" (char-after string-start)))))
      (and in-string double-quote))))

(defun fix-quotes-in-string-face ()
  "Force string face for quoted text inside strings."
  (font-lock-add-keywords
   nil
   `((,(lambda (limit)
         (let (found-pos)
           (while (and (not found-pos)
                      (re-search-forward "'[^'\n]*'" limit t))
             (let* ((quote-start (match-beginning 0))
                    (quote-end (match-end 0))
                    (in-double (in-double-quotes-p quote-start)))
               (when in-double
                 (setq found-pos (point))
                 (remove-text-properties quote-start quote-end '(face nil))
                 (put-text-property quote-start quote-end 'face 'font-lock-string-face))))
           found-pos))
      (0 'font-lock-string-face keep)))
   t))

(add-hook 'emacs-lisp-mode-hook 'fix-quotes-in-string-face)
(add-hook 'emacs-lisp-mode-hook 'force-comment-face) ;; aorisetn 'aorisetn'
(add-hook 'lisp-mode-hook 'force-comment-face)


;; Highlighting

(add-to-list 'auto-mode-alist '("sshd_config\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("ssh_config\\'" . conf-mode))


(defun debug-comment-lines ()
  "Print out comment lines around cursor position."
  (interactive)
  (save-excursion
    (let ((orig-point (point))
          (comment-char (string (char-after (comment-beginning))))
          (block-start nil)
          (lines '()))
      
      (message "=== Comment Block Detection ===")
      (message "Using comment char: %s" comment-char)
      
      ;; First go up to find start
      (beginning-of-line)
      (while (and (not (bobp))
                  (not (looking-at "^[[:space:]]*$"))
                  (looking-at (format "^[[:space:]]*%s" comment-char)))
        (forward-line -1))
      (unless (looking-at (format "^[[:space:]]*%s" comment-char))
        (forward-line 1))
      (setq block-start (point))
      
      ;; Now print from start to end
      (message "Comment block from line %d:" (line-number-at-pos))
      (while (and (not (eobp))
                  (not (looking-at "^[[:space:]]*$"))
                  (looking-at (format "^[[:space:]]*%s" comment-char)))
        (message "%d: %s" 
                (line-number-at-pos)
                (buffer-substring (line-beginning-position) (line-end-position)))
        (forward-line 1)))))

;; Cron

;; For some reason doesn't want to load the downloaded package, so i donwloaded it with the macro, commented it out and then just load manually using add-to-list.
;; (use-package emacs-crontab-mode
;;   :vc (:url "https://gitlab.com/Bacaliu/emacs-crontab-mode"
;;        :rev :newest))

(add-to-list 'load-path (expand-file-name "emacs-crontab-mode" user-emacs-directory))


;; Treesitter

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package clojure-ts-mode
 :ensure t)

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(add-hook 'yaml-ts-mode-hook (lambda () 
  (auto-fill-mode -1)))

;; Go to last change

(use-package goto-chg)


;; Daemons
(use-package daemons
  :ensure t
  :config
  (defun my/show-daemon-bindings ()
    "Show available keybindings for daemons mode."
    (interactive)
    (message "Daemons mode bindings:
RET - Show status
s   - Start service
S   - Stop service
r   - Reload service
R   - Restart service
e   - Enable service
d   - Disable service
g   - Refresh list
?   - Show this help"))
  
  (define-key daemons-mode-map (kbd "?") #'my/show-daemon-bindings))


;; Avy

(use-package avy
  :ensure t
  )

(defun avy-jump-to-window ()
  "Use avy to jump to a specific window."
  (interactive)
  (let ((avy-all-windows 'all-frames))
    (avy-with avy-jump-to-window
      (avy--process
       (mapcar (lambda (w)
                 (cons (window-start w) w))
               (avy-window-list))
       #'avy--overlay-post))))

(with-eval-after-load 'avy
  (defun avy-action-copy-word (pt)
    "Copy word at PT and paste at current point (like evil's iw)."
    (let ((original-window (selected-window))
          (original-point (point)))
      (save-excursion
        (goto-char pt)
        (let ((bounds (evil-inner-word)))
          (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
      (select-window original-window)
      (goto-char original-point)
      (yank))
    t)

  (defun avy-action-copy-WORD (pt)
    "Copy WORD at PT and paste at current point (like evil's iW)."
    (let ((original-window (selected-window))
          (original-point (point)))
      (save-excursion
        (goto-char pt)
        (let ((bounds (evil-inner-WORD)))
          (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
      (select-window original-window)
      (goto-char original-point)
      (yank))
    t)

  (defun avy-action-copy-quoted (pt)
    "Copy quoted text at PT and paste at current point."
    (let ((original-window (selected-window))
          (original-point (point)))
      (save-excursion
        (goto-char pt)
        (let ((bounds (evil-select-quote ?\" t t)))
          (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
      (select-window original-window)
      (goto-char original-point)
      (yank))
    t)

  ;; Add to dispatch alist
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-word
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-WORD
        (alist-get ?\" avy-dispatch-alist) 'avy-action-copy-quoted))


;; Docker

(use-package docker
  :ensure t
  ;; :config
  ;; It was defined in xterm block for some reason
  ;; (with-eval-after-load 'vterm
  ;;   (setq docker-vterm-support t)
  ;;   (setq docker-container-shell-file-name "vterm"))
  )

(defun container-map-id (container-name)
  "Display the UID and GID maps of a Docker container.
Ask for the name of a Docker container, retrieve its PID, and display the UID and GID maps."
  (interactive "sContainer name: ")
  (let* ((pid (string-trim (shell-command-to-string (format "docker inspect --format '{{.State.Pid}}' %s" container-name))))
         (uid-map-file (format "/proc/%s/uid_map" pid))
         (gid-map-file (format "/proc/%s/gid_map" pid)))
    (if (and (not (string-empty-p pid))
             (file-exists-p uid-map-file)
             (file-exists-p gid-map-file))
        (with-output-to-temp-buffer "*Docker ID Maps*"
          (princ (format "UID and GID maps for container '%s' (PID: %s):\n\n" container-name pid))
          (princ "UID map:\n")
          (princ (with-temp-buffer
                   (insert-file-contents uid-map-file)
                   (buffer-string)))
          (princ "\nGID map:\n")
          (princ (with-temp-buffer
                   (insert-file-contents gid-map-file)
                   (buffer-string))))
      (message "Failed to retrieve UID and/or GID maps for container '%s'" container-name))))

;; (defun docker-template ()
;;   "Create docker.el windows with a specific layout"
;;   (interactive)
;;   (delete-other-windows)
;;   (docker-images)
;;   (docker-containers)
;;   (transpose-frame)
;;   (docker-volumes)
;; )

(defun docker-template ()
  "Create docker.el windows with a specific layout"
  (interactive)
  (delete-other-windows)
  (docker-images)
  (docker-containers)
  (transpose-frame)
  (evil-window-move-very-bottom)
)

(defun toggle-docker-layout ()
  "Toggle between docker layout and previous layout."
  (interactive)
  (let ((mode-name (symbol-name major-mode)))
    ;; (message "Current mode: %s, contains 'docker': %s" 
    ;;          mode-name
    ;;          (if (string-match-p "docker" mode-name) "yes" "no"))
    (condition-case nil
        (if (not (get-register ?d))
            ;; First time setup
            (progn
              ;; (message "First time setup: creating docker layout")
              (window-configuration-to-register ?p)
              (docker-template)
              (window-configuration-to-register ?d))
          ;; Docker register exists - check if we're in docker mode
          (if (string-match-p "docker" mode-name)
              (progn
                ;; (message "In docker mode, switching to previous layout")
                (jump-to-register ?p))
            (progn
              ;; (message "Not in docker mode, switching to docker layout")
              (window-configuration-to-register ?p)
              (jump-to-register ?d))))
      ;; Handle invalid register by doing first-time setup
      (error
       ;; (message "Invalid register detected, doing first-time setup")
       (set-register ?p nil)
       (set-register ?d nil)
       (window-configuration-to-register ?p)
       (docker-template)
       (window-configuration-to-register ?d)))))

;; (defun my-docker-shell ()
;;   (interactive)
;;   (let ((container-id (read-string "Enter container ID: ")))
;;     (comint-run (format "docker exec -it %s /bin/sh" container-id))))


;; Which-key

(which-key-mode)
(setq which-key-max-description-length 40)


;; Wgrep

(use-package wgrep
  :ensure t)


;; Xterm-color

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;; Snippets

(defun my/select-placeholder ()
  "Select the placeholder text on the current line."
  (set-mark (point))
  (end-of-line)
  (point))


(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  :config
  (setq tempel-path "~/.emacs.d/other/templates")
)

(use-package tempel-collection
  :ensure t)


;; Orderless

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

  
;; Corfu/Cape

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))


(defun my-eshell-has-argument-p ()
  "Check if the current Eshell input has an argument."
  (let* ((input (eshell-get-old-input))
         (trimmed-input (string-trim-right input))
         (args (split-string trimmed-input " " t)))
    (or (> (length args) 1)
        (not (string-equal input trimmed-input)))))

(defun my-eshell-directory-completions ()
  "Generate a list of all directories in the current working directory, including hidden ones."
  (let ((current-dir (eshell/pwd)))
    (cl-remove-if-not
     #'file-directory-p
     (directory-files current-dir t nil t))))

(defun my-eshell-completion-at-point ()
  "Provide completion for Eshell using custom directory completions with a whitespace."
  (unless (my-eshell-has-argument-p)
    (let ((bounds (bounds-of-thing-at-point 'filename)))
      (when bounds
        (let* ((start (car bounds))
               (end (cdr bounds))
               (input (buffer-substring-no-properties start end))
               (completions (my-eshell-directory-completions))
               (matches (cl-remove-if-not
                         (lambda (dir)
                           (string-prefix-p input (file-name-nondirectory dir)))
                         completions)))
          (when matches
            (list start end
                  ;; Add a space to each completion candidate
                  (mapcar (lambda (dir) (concat (file-name-nondirectory dir) " "))
                          matches)
                  :exclusive 'no)))))))

(defun my-eshell-setup ()
  "Set up custom completions and key bindings for Eshell."
  (add-to-list 'completion-at-point-functions 'my-eshell-completion-at-point))

(add-hook 'eshell-mode-hook 'my-eshell-setup)

;; Corfu setup
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;; :custom
  ;; (corfu-auto nil)
  ;; (corfu-min-length 2)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (corfu-echo-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "RET") nil))

(defun my/dabbrev-capf ()
  (let* ((bounds (or (bounds-of-thing-at-point 'filename)  ; try filename first
                     (bounds-of-thing-at-point 'word)))     ; fallback to word
         (start (if bounds (car bounds) (point)))
         (end (if bounds (cdr bounds) (point)))
         (current-word (when bounds (buffer-substring-no-properties start end)))
         ;; Get all words from current buffer, excluding the exact current word
         (words (when current-word
                 (let ((case-fold-search t)
                       (words-list '()))
                   (save-excursion
                     (goto-char (point-min))
                     (while (re-search-forward (concat "\\<" (regexp-quote current-word) "\\w*") nil t)
                       (let ((found-word (match-string-no-properties 0)))
                         ;; Only add if it's not exactly the same as current-word
                         (unless (string= found-word current-word)
                           (push found-word words-list)))))
                   (delete-dups words-list)))))
    (when current-word
      (list start
            end
            (completion-table-in-turn
             (lambda (string pred action)
               (complete-with-action action words string pred)))))))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq cape-dabbrev-check-other-buffers nil)
  
  ;; Default for all buffers
  (setq completion-at-point-functions
        (list #'tempel-complete  ; or #'tempel-expand
              #'cape-file))

  ;; For ALL buffers except elisp and eshell
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (unless (or (derived-mode-p 'emacs-lisp-mode)
                         (derived-mode-p 'eshell-mode))
                (setq-local completion-at-point-functions
                            (list #'tempel-complete  ; or #'tempel-expand
                                  #'cape-file
                                  #'my/dabbrev-capf)))))

  ;; For Elisp modes
  (dolist (mode '(emacs-lisp-mode
                 ielm-mode
                 lisp-interaction-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (setq-local completion-at-point-functions
                            (list #'tempel-complete  ; or #'tempel-expand
                                  #'elisp-completion-at-point
                                  #'cape-file)))))
  
  ;; Special case for eval-expression-minibuffer
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'tempel-complete  ; or #'tempel-expand
                                #'elisp-completion-at-point
                                #'cape-file)))))

(use-package fish-completion
  :vc (:url "https://github.com/LemonBreezes/emacs-fish-completion.git"
       :rev :newest))

(when (and (executable-find "fish")
         (require 'fish-completion nil t))
(global-fish-completion-mode))


;; Access to ENV variables

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil) ; Remove -i for faster startup
  (setq exec-path-from-shell-variables
        '("PATH"
          "FZF_DEFAULT_COMMAND"
          "SSH_AUTH_SOCK"
          "NOTIFY_TOKEN"
          "SHELF_TOKEN"
          "SHELF_DB_USER"
          "SHELF_DB_NAME"
          "SHELF_DB_PASS"
          "SHELF_DB_PORT"))
  (exec-path-from-shell-initialize))


;; Zoxide

(use-package zoxide
  :vc (:url "https://gitlab.com/Vonfry/zoxide.el"
       :rev :newest))


;; Project.el

(require 'project)
(setq project-list-file-name-function #'project-files-find-function)

(defun project-find-file-all ()
  "Like project-find-file but always includes all files regardless of VCS status."
  (interactive)
  (project-find-file t))

(defcustom project-root-markers
  '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
    "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers' with additional conditions."
  (cond
   ;; Check if the path is within the specific directory
   ((string-prefix-p "/home/wurfkreuz/.secret_dotfiles/org" (expand-file-name path))
    (cons 'transient "/home/wurfkreuz/.secret_dotfiles/org/"))

   ;; Fall back to the original project root detection
   (t (when-let* ((root (locate-dominating-file path #'project-root-p)))
        (cons 'transient (expand-file-name root))))))


;; Embark

(use-package embark
  :ensure t
  :bind
  ("C-M-;" . embark-act))

(defun my/find-file-embark (region)
  "Find file using the selected REGION as filename."
  (find-file (string-trim region)))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders
               (lambda ()
                 (when (use-region-p)
                   (cons 'region (buffer-substring-no-properties
                                (region-beginning)
                                (region-end))))))
  
  (add-to-list 'embark-pre-action-hooks
               '(find-file embark--mark-target))
  
  (add-to-list 'embark-keymap-alist '(region . embark-region-map))
  
  (define-key embark-region-map (kbd "RET") #'my/find-file-embark))


;; Vertico/Consult

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  ;; :vertico
  ;; (custom-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  ;; It clears the current path in the minibuffer if it's overshadowed
  ;; :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  ;; (icomplete-vertical-mode)
  ;; (setq icomplete-compute-delay 0)
  ;; (setq icomplete-show-matches-on-no-input t)
  :config
  ;; (setq vertico-preselect 'first)
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input))

(defvar my/vertico-face-remap nil "Cookie for face remapping.")

(defun my/vertico-handle-cmdline-face (&rest _)
  "Toggle vertico-current face based on index."
  (when (bound-and-true-p vertico--index)
    (if (= vertico--index -1)
        (when (null my/vertico-face-remap)
          (setq my/vertico-face-remap
                (face-remap-add-relative 'vertico-current 'default)))
      (when my/vertico-face-remap
        (face-remap-remove-relative my/vertico-face-remap)
        (setq my/vertico-face-remap nil)))))

(advice-add 'vertico--exhibit :after #'my/vertico-handle-cmdline-face)


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult)
  ;; :ensure t
  ;; :config
  ;; (with-eval-after-load 'org
  ;;   (define-key org-mode-map (kbd "C-s C-o") 'consult-imenu)))

;; Variable to store the last search string
(defvar my/last-search-pattern nil
  "Stores the last search pattern from consult-line.")

;; Advice to store the search string
(defun my/store-search-string (&rest args)
  "Store the search string used in consult-line."
  (setq my/last-search-pattern (car consult--line-history)))

(advice-add 'consult-line :after #'my/store-search-string)

(defun my/search-next ()
  "Search forward using last search pattern."
  (interactive)
  (when my/last-search-pattern
    (let ((case-fold-search t))
      (if (re-search-forward my/last-search-pattern nil t)
          (message "Match found: %s" (match-string 0))
        (message "No more matches")))))

(defun my/search-previous ()
  "Search backward using last search pattern."
  (interactive)
  (when my/last-search-pattern
    (let ((case-fold-search t))
      (if (re-search-backward my/last-search-pattern nil t)
          (message "Match found: %s" (match-string 0))
        (message "No more matches")))))

;; Disable preview for consult-recent-file
(advice-add 'consult-recent-file :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'consult--file-preview) #'ignore))
                (apply orig-fun args))))

(use-package embark-consult
  :ensure t)

(defun consult-line-visible-region ()
  "Search for a matching line only in the visible portion of the current buffer."
  (interactive)
  (save-restriction
    (narrow-to-region (window-start) (window-end))
    (consult-line)))

(defun my-vertico-shell-command-history ()
  "Use `completing-read` to search through shell command history and return the selected command."
  (let ((history shell-command-history))
    (completing-read "Shell command history: " history nil nil nil 'shell-command-history)))

(defun my-insert-selected-command (selected-command)
  "Insert the selected command into the minibuffer and print a message."
  (when selected-command
    (insert selected-command)))

(defun my-shell-command-history-and-insert ()
  "Search shell command history and insert the selected command into the minibuffer."
  (interactive)
  (let ((selected-command (my-vertico-shell-command-history)))
    (my-insert-selected-command selected-command)))

(defun my-vertico-eval-history ()
  "Use `completing-read` to search through Emacs Lisp evaluation history and return the selected expression."
  (let ((history read-expression-history))
    (completing-read "Eval history: " history nil nil nil 'read-expression-history)))

(defun my-insert-selected-expression (selected-expression)
  "Insert the selected expression into the minibuffer and print a message."
  (when selected-expression
    (insert selected-expression)))

(defun my-eval-history-and-insert ()
  "Search Emacs Lisp evaluation history and insert the selected expression into the minibuffer."
  (interactive)
  (let ((selected-expression (my-vertico-eval-history)))
    (my-insert-selected-expression selected-expression)))

(defun my-smart-history-and-insert ()
  "Insert history item based on the current minibuffer context."
  (interactive)
  (let ((prompt (minibuffer-prompt)))
    (cond
     ;; For shell and async-shell commands
     ((or (string-match-p "Shell command:" prompt)
          (string-match-p "Async shell command:" prompt))
      (my-shell-command-history-and-insert))
     
     ;; For eval expressions
     ((string-match-p "Eval:" prompt)
      (my-eval-history-and-insert))
     
     ;; Default case: combine both histories
     (t
      (let* ((combined-history (append shell-command-history read-expression-history))
             (selected-item (completing-read "Combined history: " combined-history nil nil nil)))
        (insert selected-item))))))

(defun setup-minibuffer-keys ()
  (define-key minibuffer-local-map (kbd "M-r") 'my-smart-history-and-insert))

;; Add the setup function to minibuffer-setup-hook
(add-hook 'minibuffer-setup-hook 'setup-minibuffer-keys)

(defun my-eshell-history-choose ()
  "Select an item from eshell history using Vertico and insert it into the eshell prompt."
  (interactive)
  (let* ((history (ring-elements eshell-history-ring))
         (history (delete-dups history))
         (initial-command (eshell-get-old-input))
         (finale-command (consult--read history
                                 :prompt "Eshell history: "
                                 :initial initial-command
                                 :sort nil
                                 :require-match t)))
    (when finale-command
      (eshell-kill-input)
      (insert finale-command))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-hist-mode-map (kbd "M-r") 'my-eshell-history-choose)))


;; With-editor

(use-package with-editor
  :ensure t
  :init
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor))

(defun suppress-with-editor-export-message (orig-fun &rest args)
  (let ((inhibit-message t))
    (apply orig-fun args)))

(with-eval-after-load 'with-editor
  (advice-add 'with-editor-export-editor :around #'suppress-with-editor-export-message))


;; Hydra

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-size (:color red)
    "window size"
    ("+" evil-window-increase-height "increase height")
    ("-" evil-window-decrease-height "decrease height")
    (">" evil-window-increase-width "increase width")
    ("<" evil-window-decrease-width "decrease width")
    ("t" transpose-frame "transpose windows")
    ("q" nil "quit")))


;; Dired

;; Basic trash settings
(setq delete-by-moving-to-trash t)
(setq dired-recursive-copies 'always)

;; Configure connection-local variables for sudo operations
(connection-local-set-profile-variables
 'remote-trash-directory
 '((trash-directory . "/sudo::~/.local/share/trash/")))

(connection-local-set-profiles
 `(:application tramp :protocol "sudo" :machine ,(system-name))
 'remote-trash-directory)

;; Auto-revert for sudo files
(add-to-list 'auto-revert-remote-files "/sudo:root@localhost:/")

;; This setting isn't about the ability to change permission bits, but about
;; disabling confirmation on file renaming.
(setq wdired-allow-to-change-permissions t)

;; If i have another pane with dired in the same tab, dired will try to guess
;; that i want to perform action there
(setq dired-dwim-target t)

(defun my/pwd (&optional insert)
  "Like `pwd', but without printing any additional stuff except the path itself"
  (interactive "P")
  (if insert
      (insert default-directory)
    (message default-directory)))

(defun dired-next-line-preserve-column (arg)
  "Move to the next line in Dired, preserving the current column position."
  (interactive "p")
  (let ((col (current-column)))
    (dired-next-line arg)
    (move-to-column col)))

(defun dired-previous-line-preserve-column (arg)
  "Move to the previous line in Dired, preserving the current column position."
  (interactive "p")
  (let ((col (current-column)))
    (dired-previous-line arg)
    (move-to-column col)))


;; Magit

(use-package magit
  :ensure t
  :config
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  (define-key magit-mode-map (kbd "M-5") nil)
  (define-key magit-mode-map (kbd "M-6") nil)
  )

;; (defun my-git-commit-setup ()
;;   "Set up the default commit message."
;;   (insert "n"))

;; (add-hook 'git-commit-setup-hook 'my-git-commit-setup)


;; Async shell command

;; Execute async shell command on a current file
(defun async-shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))

(defun async-shell-command-filter-hook ()
"Filter async shell command output via `comint-output-filter'."
  (when (equal (buffer-name (current-buffer)) "*Async Shell Command*")
    ;; When `comint-output-filter' is non-nil, the carriage return characters ^M
    ;; are displayed
    (setq-local comint-inhibit-carriage-motion nil)
    (when-let* ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc 'comint-output-filter))))

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook 'async-shell-command-filter-hook))


;; Sessions

(defvar current-desktop-session-name nil
  "The name of the currently loaded desktop session.")

(defvar desktop-autosave-timer nil
  "Timer object for desktop autosave, to avoid multiple timers running.")

(defun save-eshell-buffer (desktop-dirname)
  ;; Save the current working directory.
  default-directory)

(defun restore-eshell-buffer (_file-name buffer-name misc)
  "MISC is the value returned by `save-eshell-buffer'.
                _FILE-NAME is nil."
  (let ((default-directory misc))
    ;; Create an eshell buffer named BUFFER-NAME in directory MISC.
    (eshell buffer-name)))

;; Save all eshell-mode buffers.
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local desktop-save-buffer #'save-eshell-buffer)))

;; Restore all eshell-mode buffers.
(add-to-list 'desktop-buffer-mode-handlers '(eshell-mode . restore-eshell-buffer))

(defun save-current-desktop-session (&optional show-message)
  "Save the current desktop session using the current session name.
If no session is loaded, prompt to create a new one. SHOW-MESSAGE controls whether a save message is displayed."
  (interactive "p") ; "p" passes a prefix argument, which is non-nil when called interactively
  (if (and current-desktop-session-name (not (string-empty-p current-desktop-session-name)))
      (let ((desktop-dir (concat user-emacs-directory "desktop/" current-desktop-session-name "/")))
        (unless (file-exists-p desktop-dir)
          (make-directory desktop-dir t))
        (desktop-save desktop-dir)
        (when (and show-message (or (called-interactively-p 'any) (eq show-message 1)))
          (message "Session '%s' saved." current-desktop-session-name)))
    ;; No session is loaded or the session name is empty, prompt to create a new one (only when called interactively)
    (when (called-interactively-p 'any)
      (let ((new-session-name (read-string "Enter new session name: ")))
        (if (string-empty-p new-session-name)
            (message "Session name cannot be empty.")
          (progn
            (setq current-desktop-session-name new-session-name)
            (let ((new-desktop-dir (concat user-emacs-directory "desktop/" new-session-name "/")))
              (make-directory new-desktop-dir t)
              (desktop-save new-desktop-dir)
              (message "Session '%s' created and saved." new-session-name))))))))

(defun setup-desktop-autosave-timer ()
  "Set up or reset the desktop autosave timer."
  (when desktop-autosave-timer
    (cancel-timer desktop-autosave-timer))
  ;; Pass nil to save-current-desktop-session to avoid showing the message during autosaves.
  (setq desktop-autosave-timer (run-with-timer 30 30 (lambda () (save-current-desktop-session nil)))))

(defun load-desktop-session (session-name)
  "Load a desktop session by name."
  (let ((desktop-dir (concat user-emacs-directory "desktop/")))
    (setq current-desktop-session-name session-name)
    (desktop-change-dir (concat desktop-dir session-name "/"))
    (setup-desktop-autosave-timer)))

(defun load-desktop-with-name ()
  "Load a desktop session by name, chosen from available sessions."
  (interactive)
  (when current-desktop-session-name
    ;; Save the current session before loading a new one, but only if a session is already loaded.
    (save-current-desktop-session))
  (let* ((desktop-dir (concat user-emacs-directory "desktop/"))
         (session-dirs (directory-files desktop-dir nil "^[^.]"))  ; List directories excluding hidden ones
         (session-name (completing-read "Choose desktop session: " session-dirs nil t)))
    (setq current-desktop-session-name session-name)  ; Save the session name globally
    (desktop-change-dir (concat desktop-dir session-name "/"))
    (setup-desktop-autosave-timer)))

;; Disable the default desktop save mode
(desktop-save-mode 0)

(setq desktop-files-not-to-save
    (concat "\\(^/[^/:]*:\\|(ftp)$\\)\\|" desktop-files-not-to-save))

(defun delete-desktop-session ()
  "Delete a desktop session by name, chosen from available sessions."
  (interactive)
  (let* ((desktop-dir (concat user-emacs-directory "desktop/"))
         (session-dirs (directory-files desktop-dir nil "^[^.]"))  ; List directories excluding hidden ones
         (session-name (completing-read "Choose desktop session to delete: " session-dirs nil t)))
    (when (yes-or-no-p (format "Are you sure you want to delete the '%s' session? " session-name))
      (let ((session-path (concat desktop-dir session-name)))
        (if (file-directory-p session-path)
            (progn
              (delete-directory session-path t)  ; 't' for recursive delete
              (message "Deleted desktop session '%s'." session-name))
          (message "No such desktop session '%s'." session-name))))))

(defun rename-desktop-session ()
  "Renames the currently loaded desktop session."
  (interactive)
  ;; Check if there's a session loaded.
  (if (not current-desktop-session-name)
      (message "No desktop session is currently loaded.")
    (let* ((new-name (read-string "New session name: "))
           (old-dir (concat user-emacs-directory "desktop/" current-desktop-session-name))
           (new-dir (concat user-emacs-directory "desktop/" new-name)))
      ;; Check if the new session name is empty or the session already exists.
      (if (or (string-empty-p new-name)
              (file-exists-p new-dir))
          (message "Invalid new session name or session already exists.")
        ;; Rename the directory and update the session name.
        (rename-file old-dir new-dir)
        (setq current-desktop-session-name new-name)
        (message "Session renamed to '%s'." new-name)))))

;; I need a better check
;; (defun exit-docker-layout-if-active ()
;;   "Switch from docker layout to previous layout if docker layout is active."
;;   (condition-case nil
;;       (when (and (get-register ?d)
;;                  (seq-some (lambda (window)
;;                             (string-match-p "docker"
;;                                           (symbol-name (with-current-buffer (window-buffer window)
;;                                                        major-mode))))
;;                           (window-list)))
;;         (jump-to-register ?p))
;;     (error nil)))

;; (add-hook 'kill-emacs-hook #'exit-docker-layout-if-active)
;; (add-hook 'kill-emacs-hook 'clean-buffer-list)
(add-hook 'kill-emacs-hook 'save-current-desktop-session)

(advice-add 'save-current-desktop-session :before
            (lambda (&rest _)
              (save-some-buffers t)))


;; ;; Buffers
;; ;; Spawn and Pointer

;; ;; If i refactor this, then corfu might start to work incorrectly when i
;; ;; invoke a popup, then wait for an echo popup and press C-n
(setq display-buffer-alist
      (mapcar (lambda (name)
                `(,name display-buffer-same-window (nil)))
              '("*Faces*"
                "*info*"
                "*helpful*"
                "*Help*"
                "*Warnings*"
                "*Async Shell Command*"
                "*vc-git*"
                "*compilation*"
                "*debug*")))

(add-to-list 'display-buffer-alist
             '("\\*\\(Man\\|Help\\) "
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (post-command-select-window . t)))


;; Eshell buffer

(defun open-eshell-in-current-directory ()
  "Open eshell in the directory of the current buffer.
    If an eshell buffer for the directory already exists, switch to it."
  (interactive)
  (let* ((buffer-dir (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
         (eshell-buffer-name (concat "*eshell:" buffer-dir "*"))
         (existing-eshell-buffer (get-buffer eshell-buffer-name)))
    (if existing-eshell-buffer
        (switch-to-buffer existing-eshell-buffer)
      (let ((eshell-buffer (eshell 'N)))
        (with-current-buffer eshell-buffer
          (rename-buffer eshell-buffer-name)
          (eshell/cd buffer-dir))))))

(defun SpawnEshellInProjectRoot ()
  "Open eshell in the project's root directory or toggle it if already open."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (progn
        (when eshell-toggle-window-configuration
          (set-window-configuration eshell-toggle-window-configuration)
          (setq eshell-toggle-window-configuration nil))
        (when eshell-toggle-selected-window
          (select-window eshell-toggle-selected-window)
          (setq eshell-toggle-selected-window nil)))
    (setq eshell-toggle-window-configuration (current-window-configuration))
    (setq eshell-toggle-selected-window (selected-window))
    ;; Calculate one third of the total window height
    (let ((one-third-height (/ (window-total-height) 3)))
      ;; Ensure the height is at least 1 to avoid errors
      (setq one-third-height (max one-third-height 1))
      (split-window-below (- one-third-height))
      (other-window 1)
      (let ((project-root (projectile-project-root)))
        (open-eshell-in-directory project-root)))))

(defun open-eshell-in-directory (dir)
  "Open eshell in the specified directory DIR.
If an eshell buffer for the directory already exists, switch to it."
  (interactive "DDirectory: ")
  (let* ((eshell-buffer-name (concat "*eshell:" dir "*"))
         (existing-eshell-buffer (get-buffer eshell-buffer-name)))
    (if existing-eshell-buffer
        (switch-to-buffer existing-eshell-buffer)
      (let ((eshell-buffer (eshell 'N)))
        (with-current-buffer eshell-buffer
          (rename-buffer eshell-buffer-name)
          (eshell/cd dir))))))

(defun kill-all-eshell-buffers ()
  "Kill all Eshell buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*eshell\\*" (buffer-name buffer))
      (kill-buffer buffer))))


; Transpose frame

(use-package transpose-frame
  :ensure t)


;; Emacs-eat

(use-package eat
  :ensure t
  :vc (:url "https://github.com/kephale/emacs-eat"
       :rev :newest)
  :config
  (add-hook 'eshell-first-time-mode-hook
            #'eat-eshell-visual-command-mode)
  (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode))


;; Multiple cursors

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t))


;; Buffer termination

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

;; Envrc

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; Flymake

;; (setq flymake-show-diagnostics-at-end-of-line t)

(use-package flymake-ansible-lint
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode)))

;; (defun enable-flymake-mode ()
;;   "Enable flymake-mode in dockerfile-mode."
;;   (if (string-equal major-mode "dockerfile-mode")
;;       (flymake-mode 1)))

;; ;;   ;; Add the hook to enable flymake-mode when entering dockerfile-mode
;; (add-hook 'dockerfile-mode-hook 'enable-flymake-mode)

(use-package flymake-hadolint
  :ensure t)

(add-hook 'dockerfile-mode-hook #'flymake-hadolint-setup)

;; If i need to run hadolint with dockerfile-ts-mode
;; (defun my/dockerfile-ts-mode-setup ()
;;   "Setup for dockerfile-ts-mode with flymake-hadolint."
;;   (message "Running dockerfile-ts-mode setup")
;;   (flymake-hadolint-setup)
;;   (flymake-mode 1))

;; (add-hook 'dockerfile-mode-hook #'flymake-hadolint-setup)
;; (add-hook 'dockerfile-ts-mode-hook #'my/dockerfile-ts-mode-setup)

;; ;; Optional: Ensure flymake is enabled
;; (add-hook 'dockerfile-ts-mode-hook 'flymake-mode)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


;; Modes

(use-package raku-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package terraform-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package nix-mode
  :ensure t)
(use-package systemd
  :ensure t)
;; (use-package markdown-mode ;; can't be found by the package installer
;;   :ensure t)

(define-generic-mode kdl-mode
  ;; Comment style
  '("//" ("/*" . "*/"))
  ;; Keywords
  nil
  ;; Additional font-lock pairs
  nil
  ;; File extension
  '("\\.kdl\\'")
  ;; Additional function calls
  nil
  "Major mode for editing KDL files.")

(provide 'kdl-mode)


(when (require 'dockerfile-mode nil 'noerror)
  ;; Add a hook to automatically use dockerfile-mode for Dockerfiles
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; yaml-ts doesn't work because of this hook?
;; (use-package ansible
;;   :config
;;   (add-hook 'yaml-ts-mode-hook '(lambda () (ansible 1))))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             'yaml)
(add-to-list 'compilation-error-regexp-alist-alist
             '(yaml "^\\(.*?\\):\\([0-9]+\\)" 1 2)
             )

                                        ; Replace make -k with ansible-lint, with an UTF-8 locale to avoid crashes
(defun ansible-lint-errors ()
  (make-local-variable 'compile-command)
  (let ((ansiblelint_command "ansible-lint ") (loc "LANG=C.UTF-8 "))
    (setq compile-command (concat loc ansiblelint_command buffer-file-name)))
  )
(add-hook 'yaml-ts-mode-hook 'ansible-lint-errors)

;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command "multimarkdown")
;;   :bind (:map markdown-mode-map
;;         ("C-c C-e" . markdown-do)))

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hls\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))


;; ;; Flycheck

(defun my/set-flycheck-faces ()
  (with-eval-after-load 'flycheck
    ;; Customize Flycheck error face
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style line :color "#e0def4")) ; rose-pine-gold
    ;; Customize Flycheck warning face
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style line :color "#f6c177")) ; rose-pine-gold
    ;; Customize Flycheck info (note) face
    (set-face-attribute 'flycheck-info nil
                        :underline `(:style line :color "#c4a7e7")))) ; rose-pine-iris

(add-hook 'after-init-hook 'my/set-flycheck-faces)


;; Eldoc

;; Disable globally
(global-eldoc-mode -1)

;; Or if you want to be extra thorough:
(setq global-eldoc-mode nil)
(setq eldoc-mode nil)


;; Eglot

(require 'eglot)


;; (setq eglot-events-buffer-size 0)

(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
(add-hook 'nix-mode-hook 'eglot-ensure)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
;; I need to prevent a situation where ansible-lint and eglot work on the same buffer.
;; (add-hook 'yaml-ts-mode-hook #'eglot-ensure)
(add-hook 'terraform-mode-hook #'eglot-ensure)

(add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))

(set-face-attribute 'eldoc-highlight-function-argument nil
                    :inherit 'unspecified' :foreground 'unspecified' :weight 'medium)
(set-face-attribute 'eglot-highlight-symbol-face nil
                    :inherit 'unspecified' :foreground 'unspecified' :weight 'medium)


;; Direnv

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda ()
  ;;             (message "Eglot started with env: VIRTUAL_ENV=%s" (getenv "VIRTUAL_ENV"))
  ;;             (message "Python path: %s" (executable-find "python")))))


;; Example for dir-locals:

;; ((yaml-ts-mode . ((eglot-workspace-configuration . ((yaml schemas . ((
;;     https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json ".gitlab-ci.yml"
;;     ./argocd-application.schema.json [
;;         "/Apps/*"
;;         "/apps.yaml"
;;     ]
;;     Kubernetes ["k8s-*.yaml"]
;; ))))))))


;; Org Mode
;; General


(defvar browse-url-default-browser-executable "/usr/bin/vivaldi"
  "Path to the default browser executable.")

(defun my/browse-url-default-browser (url &rest _args)
  "Browse URL using the default browser."
  (if (and browse-url-default-browser-executable
           (file-executable-p browse-url-default-browser-executable))
      (start-process 
       browse-url-default-browser-executable
       nil
       browse-url-default-browser-executable
       url)
    (message "Browser executable not found or not executable. Falling back to system default.")
    (browse-url-default-handler url)))

(setq browse-url-browser-function 'my/browse-url-default-browser)

(use-package org
  :config
  (setq browse-url-browser-function 'my/browse-url-default-browser)
  ;; (setq browse-url-browser-function 'browse-url-default-browser) ;; Make links to open a default web browser.
  (setq org-startup-with-inline-images t)
  (setq org-edit-src-content-indentation 0)
  (setq org-blank-before-new-entry
        '((heading . nil)
          (plain-list-item . nil)))
  
  ;; Disable auto-blank-lines in self-insert-command
  (setq org-list-empty-line-terminates-plain-lists nil)
  (setq org-empty-line-terminates-plain-lists nil)
  )

(define-key org-mode-map (kbd "C-x p") #'yank-media)

(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images)))

(setq org-yank-image-save-method "~/.secret_dotfiles/pictures/org/")


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


;; Prevent org-meta-return from folding images
(defun my/preserve-images-advice (orig-fun &rest args)
  "Preserve inline image display when inserting new items."
  (let ((org-inline-image-overlays (copy-sequence org-inline-image-overlays)))
    (apply orig-fun args)
    (org-display-inline-images)))

(advice-add 'org-insert-item :around #'my/preserve-images-advice)

(advice-add 'yank-media :after
            (lambda (&rest _)
              (when (eq major-mode 'org-mode)
                (org-display-inline-images))))


;; Prevent org-meta-return from folding images
(defun my/preserve-images-advice (orig-fun &rest args)
  "Preserve inline image display when inserting new items."
  (let ((org-inline-image-overlays (copy-sequence org-inline-image-overlays)))
    (apply orig-fun args)
    (org-display-inline-images)))

(advice-add 'org-insert-item :around #'my/preserve-images-advice)

(advice-add 'yank-media :after
            (lambda (&rest _)
              (when (eq major-mode 'org-mode)
                (org-display-inline-images))))


;; Where to save images if using 'save method
(setq org-yank-image-dir "~/.secret_dotfiles/org/images/")

(defun org-insert-top-level-heading ()
    "Insert a new top-level heading with two empty lines before it."
    (interactive)
    (end-of-line)
    (insert "\n\n\n* ")
    (end-of-line))

;; (define-key org-mode-map (kbd "M-o M-h") 'org-insert-top-level-heading)

(defun create-list-in-region ()
  "Convert the highlighted text into a single org-mode list item, properly formatting multiline text."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             ;; Adjust `end` to exclude the newline at the end of the selection if present.
             (end (if (and (> end beg)
                           (save-excursion
                             (goto-char end)
                             (beginning-of-line)
                             (<= (point) end)))
                      (progn
                        (goto-char end)
                        (backward-char)
                        (point))
                    end))
             (region-text (buffer-substring beg end))
             (lines (split-string region-text "\n")))
        (delete-region beg end)
        (when lines
          (insert (concat "- " (car lines)))
          (dolist (line (cdr lines))
            (insert (concat "\n  " line)))))
    (message "No region active")))

(defun my/org-smart-heading ()
  "Create a new heading intelligently:
If on a heading line, create a subheading (M-S-RET).
Otherwise, create a same-level heading (M-RET)."
  (interactive)
  (if (org-at-heading-p)
      (org-insert-subheading nil)
    (org-meta-return)))

;; Bind it to a key of your choice, for example:
(define-key org-mode-map (kbd "M-RET") #'my/org-smart-heading)


;; Org appear
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))


;; Org-drill

(use-package org-drill
  :ensure t
  :config
  (setq org-drill-maximum-items-per-session 100))

(defun org-drill-present-sequence (session)
  (org-drill-with-hidden-comments
   (let ((drill-sections (org-drill-hide-all-subheadings-except nil))
         (current-section 0))
     (org-drill--show-latex-fragments)
     (ignore-errors
       (org-display-inline-images t))
     (org-cycle-hide-drawers 'all)
     ;; Show the main heading content first
     (org-drill-presentation-prompt session)
     ;; Then iterate through subheadings
     (while (< current-section (length drill-sections))
       (save-excursion
         (goto-char (nth current-section drill-sections))
         (org-fold-show-subtree))
       (org-drill-presentation-prompt session)
       (setq current-section (1+ current-section)))
     ;; Hide all subheadings at the end
     (org-drill-hide-subheadings-if (lambda () t)))))

(with-eval-after-load 'org-drill
  (add-to-list 'org-drill-card-type-alist
               '("sequence" org-drill-present-sequence nil t)))


;; Templates

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sb" . "src bash-ts"))
(add-to-list 'org-structure-template-alist '("se" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sr" . "src raku"))
(add-to-list 'org-structure-template-alist '("sf" . "src fundamental"))
(add-to-list 'org-structure-template-alist '("st" . "src text"))
(add-to-list 'org-structure-template-alist '("ss" . "src sql"))
(add-to-list 'org-structure-template-alist '("sg" . "src go-ts"))
(add-to-list 'org-structure-template-alist '("sc" . "src clojure-ts"))

(add-to-list 'org-structure-template-alist
           '("t" . "src TODO\n\n* TODO \n\n?"))


;; Visuals

(setq org-hide-emphasis-markers t)

(defun toggle-org-emphasis-markers ()
  "Toggle the visibility of Org emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-mode-restart))

(define-key org-mode-map (kbd "M-o t m") 'toggle-org-emphasis-markers)
(define-key org-mode-map (kbd "M-o t l") 'org-toggle-link-display)


;; Custom functions

(defun my/kill-current-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))


(defun my/copy-kill-ring-to-clipboard ()
  "Show kill ring entries and copy selected one to system clipboard."
  (interactive)
  (let* ((candidates (cl-remove-duplicates kill-ring :test #'string=))
         (selected (completing-read "Copy to clipboard: " candidates)))
    (kill-new selected)
    (set-clipboard-text selected)
    (message "Copied to clipboard: %s" (truncate-string-to-width selected 60 nil
                                                                 nil "..."))))

(defun my-org-outline ()
  "Jump to an org heading using completion, showing headings in order from top to bottom."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  (let ((candidates '())
        (current-pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let* ((pos (point-at-bol))
               (level (org-outline-level))
               (heading (org-get-heading t t t t))
               (display (format "%5d %s%s" 
                                (line-number-at-pos pos)
                                (make-string (* 2 (1- level)) ?\s)
                                heading)))
          (push (cons display pos) candidates))))
    (setq candidates (nreverse candidates))
    (if candidates
        (let* ((collection
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity))
                    (complete-with-action action candidates string pred))))
               (selection (completing-read "Go to heading: " collection nil t))
               (position (cdr (assoc selection candidates))))
          (when position
            (goto-char position)
            (org-show-context)
            (recenter)))
      (message "No headings found in this buffer."))))

(defun my-package-isolate ()
  "Isolate packages with better completion."
  (interactive)
  (let (packages done)
    (while (not done)
      (condition-case nil
          (let ((package (completing-read 
                         (format "Package %s (C-g when done): "
                                (if packages 
                                    (format "[added: %s]" 
                                            (mapconcat #'identity packages " "))
                                  ""))
                         (mapcar #'car package-alist)
                         nil t)))
            (push package packages))
        (quit (setq done t))))
    (when packages
      (package-isolate 
       (mapcar (lambda (name)
                 (cadr (assoc (intern name) package-alist)))
               (nreverse packages))))))

(defun toggle-special-buffer (buffer-name-pattern show-buffer-fn &optional select-window)
  "Generic function to toggle special buffers.
BUFFER-NAME-PATTERN is a regex to match buffer name.
SHOW-BUFFER-FN is the function to call to show the buffer.
SELECT-WINDOW if non-nil, select the window after showing buffer."
  (let* ((buffer (seq-find (lambda (buf)
                            (string-match-p buffer-name-pattern (buffer-name buf)))
                          (buffer-list)))
         (window (and buffer (get-buffer-window buffer))))
    ;; If we're already in the special buffer, quit it
    (if (string-match-p buffer-name-pattern (buffer-name))
        (quit-window)
      ;; Otherwise toggle the window
      (if window
          (quit-window nil window)
        (when show-buffer-fn
          (funcall show-buffer-fn)
          (when select-window
            (select-window (get-buffer-window 
                          (seq-find (lambda (buf)
                                    (string-match-p buffer-name-pattern (buffer-name buf)))
                                  (buffer-list))))))))))

(defun toggle-flymake-diagnostics ()
  "Toggle the display of Flymake diagnostics buffer."
  (interactive)
  (toggle-special-buffer "\\*Flymake diagnostics.*\\*"
                        (lambda ()
                          (when (bound-and-true-p flymake-mode)
                            (flymake-show-diagnostics-buffer)))
                        t))

(defun toggle-messages-buffer ()
  "Toggle the display of Messages buffer."
  (interactive)
  (toggle-special-buffer "\\*Messages\\*"
                        (lambda () 
                          (display-buffer "*Messages*"))
                        t))

(defun org-insert-row-with-floor ()
  "Insert a new row with a 'floor' above in an Org mode table."
  (interactive)
  (org-table-next-field)
  (beginning-of-line)
  (insert "|-")
  (org-table-align)
  (org-return))

;; (define-key org-mode-map (kbd "C-c f") 'org-insert-row-with-floor)

(defun my-org-beginning-of-block ()
  "Move to the beginning of the current block and then one line down."
  (interactive)
  (let ((element (org-element-at-point)))
    (when (memq (org-element-type element) '(src-block quote-block example-block center-block special-block))
      (goto-char (org-element-property :begin element))
      (forward-line))))  ; Added this line to move one line down

(defun my-org-end-of-block ()
  "Move to the end of the current block and then two lines up."
  (interactive)
  (let ((element (org-element-at-point)))
    (when (memq (org-element-type element) '(src-block quote-block example-block center-block special-block))
      (goto-char (org-element-property :end element))
      (forward-line -3))))  ; Changed -1 to -3 to move two lines up


(defvar my-selected-kill nil
  "Stores the last kill-ring entry selected via `my-browse-kill-ring'.")

(defun my-browse-kill-ring ()
  "Browse kill ring without yanking. Selected text can be yanked later with regular yank command."
  (interactive)
  (let* ((candidates (cl-remove-duplicates kill-ring :test #'equal))
         (selected (completing-read "Select text: " candidates)))
    (when selected
      ;; Remove the selected text from kill-ring if it's there
      (setq kill-ring (delete selected kill-ring))
      ;; Add it to the front of kill-ring
      (kill-new selected)
      (message "Selected text is now at the top of kill-ring. Use C-y to paste."))))

(defun my-vc-switch-branch ()
  "Switch branch using repository root directory."
  (interactive)
  (let* ((dir (or (vc-root-dir)
                  default-directory))
         (backend (vc-responsible-backend dir))
         (branch-name (vc-read-revision 
                      "Switch to branch: " 
                      (list dir)
                      backend)))
    (vc-switch-branch dir branch-name)))

(defun copy-buffer-to-new-buffer ()
  "Create a copy of the current buffer, placing the contents in a new named buffer."
  (interactive)
  (let ((content (buffer-string))  ; Get the content of the current buffer
        (name (generate-new-buffer-name "BufferCopy")))  ; Generate a new buffer name
    (switch-to-buffer name)  ; Create and switch to the new buffer
    (insert content)  ; Insert the original content into the new buffer
    (set-buffer-major-mode (other-buffer))  ; Set the major mode based on the original buffer
    (message "Buffer copied to %s" name)))

(defun print-commands-starting-with (input)
  "Print all Emacs commands starting with INPUT to a scratch buffer."
  (interactive "sInput: ")
  (let ((command-list (apropos-internal input 'commandp))
        (output-buffer (get-buffer-create "*Commands*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Commands starting with '%s':\n\n" input))
      (dolist (command command-list)
        (insert (format "%s\n" command)))
      (goto-char (point-min)))
    (display-buffer output-buffer)))

;; Increment
(defun my/increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my/change-number-at-point '+ (or increment 2)))

;; Decrement
(defun my/decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my/change-number-at-point '- (or increment 1)))

(defun run-ansible-check-and-lint ()
  "Run ansible-lint and ansible-playbook --check on the current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (async-shell-command (format "ansible-lint %s" filename))
          (async-shell-command (format "ansible-playbook --check %s" filename)))
      (message "No file associated with this buffer"))))

(defun my-eshell-snippet-files ()
  (interactive)
  "Return a list of file names (not directories) in '~/.emacs.d/snippets/eshell-mode/eshell' and print it."
  (let* ((snippet-dir (expand-file-name "~/.emacs.d/snippets/eshell-mode/eshell"))
         (snippet-files (directory-files snippet-dir t nil nil)))
    (setq snippet-files (mapcar 'file-name-nondirectory snippet-files))
    (setq snippet-files (remove-if (lambda (file) (member file '("." ".."))) snippet-files))  ; Remove "." and ".."
    (message "Snippet files: %s" snippet-files)
    snippet-files))

;; if i paste this code:
;; (defun print-keypress ()
;;   (interactive)
;;   (message "Key pressed: %s" (this-command-keys-vector)))
;; (global-set-key (kbd "<S-tab>") 'print-keypress)
;; over a line using visual-line, then the pasting will be incorrect
(defun vim-like-paste ()
  "Paste (yank) replacing the selected region, similar to Vim's paste behavior."
  (interactive)
  (let ((text (current-kill 0)))
    (if (and (evil-visual-state-p)
             (eq evil-visual-selection 'line))
        ;; Handle evil visual line selection
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (delete-region start (min (point-max) (1+ end)))  ; include newline
          (goto-char start)
          (insert text)
          (when (not (string-suffix-p "\n" text))
            (insert "\n")))
      ;; Handle normal paste
      (if (string-match-p "\n$" text)
          (progn
            (beginning-of-line)
            (insert text))
        (insert text)))))

(defun my-completion-preview-insert ()
  "Completes the previewed suggestion and deletes the trailing whitespace."
  (interactive)
  (completion-preview-insert)
  (delete-backward-char 1))

(defun scroll-down-and-recenter (arg)
  "Scroll up ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-up-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-up-and-recenter (arg)
  "Scroll down ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-down-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-half-up-and-recenter ()
  "Scroll up half screen and recenter, preserving horizontal position."
  (interactive)
  (let ((col (current-column))    ; Save the column position
        (half-height (/ (window-height) 2)))
    (scroll-up-command (- half-height 2))  ; -2 for some context
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-half-down-and-recenter ()
  "Scroll down half screen and recenter, preserving horizontal position."
  (interactive)
  (let ((col (current-column))    ; Save the column position
        (half-height (/ (window-height) 2)))
    (scroll-down-command (- half-height 2))  ; -2 for some context
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun my-previous-history-element (arg)
  "Insert the previous history element, moving the cursor to the end."
  (interactive "p")
  (previous-history-element arg)
  (move-end-of-line 1))

(defun my-next-history-element (arg)
  "Insert the next history element, moving the cursor to the end."
  (interactive "p")
  (next-history-element arg)
  (move-end-of-line 1))

(defun add-execute-permissions-to-current-file ()
  "Add execute permissions to the file associated with the current buffer."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-truename buffer-file-name)))
      (shell-command (concat "chmod +x " (shell-quote-argument filename)))
      (message "Execute permissions added to %s" filename))))

(defun add-write-permissions-to-current-file ()
  "Add execute permissions to the file associated with the current buffer."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-truename buffer-file-name)))
      (shell-command (concat "chmod +w " (shell-quote-argument filename)))
      (revert-buffer)
      (message "Write permissions added to %s" filename))))

;; Wrap edit

(defvar-local my-edit-long-lines-region nil
  "Stores the region (beg . end) that was filled when `my-edit-long-lines-mode` was activated.")

(defun my-fill-region (beg end)
  "Fill the region between BEG and END."
  (interactive "r")
  (fill-region beg end))

(defun unfill-region (start end)
  "Unfill the region, joining text paragraphs into a single line."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(defun wrap-edit-exit ()
  "Exit `wrap-edit-mode`, unfill the region, and save the buffer."
  (interactive)
  (when wrap-edit-region
    (unfill-region (car wrap-edit-region) (cdr wrap-edit-region)))
  (save-buffer)
  (wrap-edit-mode -1))

(global-set-key (kbd "C-c g e") 'wrap-edit-mode)


;; Custom commands

(defun Cp ()
  "Copy full path of the current buffer"
  (interactive)
  (let ((path-to-copy nil))
    (cond
     ((eq major-mode 'dired-mode)    ; Dired buffer
      (setq path-to-copy (my/pwd)))
     ((eq major-mode 'eshell-mode)   ; Eshell buffer
      (setq path-to-copy (eshell/pwd)))
     (t                              ; Default: Regular File buffer
      (setq path-to-copy (buffer-file-name))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message "Copied path '%s' to the clipboard." path-to-copy))
      (message "Current buffer has no associated path to copy.")))) ;

(defun Cpn ()
  "Copy the full path of the current item under cursor"
  (interactive)
  (let ((path-to-copy nil))
    (cond
     ((eq major-mode 'dired-mode)    ; Dired buffer
      (setq path-to-copy (if (dired-get-file-for-visit)
                             (expand-file-name (dired-get-file-for-visit))
                           (expand-file-name default-directory)))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message "Copied path '%s' to the clipboard." path-to-copy))
      (message "Is this a dired-mode buffer?")))) ;

(defun home ()
  "Open a specific file."
  (interactive)
  (find-file "~/"))

(defun alias ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/eshell/alias"))

(defun root ()
  "Open a specific file."
  (interactive)
  (find-file "/"))

(defun theme ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/themes/rose-pine-theme.el"))

(defun trash ()
  "Open a specific file."
  (interactive)
  (find-file "~/.local/share/Trash/files"))

(defun strash ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/trash/emacs/"))

(defun ngrok ()
  "Open a terminal and execute 'ngrok http http://localhost:8080'."
  (interactive)
  (let ((term-buffer-name "*ngrok-http-8080*"))
    ;; Check if the buffer already exists
    (if (get-buffer term-buffer-name)
        ;; If it does, switch to it
        (switch-to-buffer term-buffer-name)
      ;; Otherwise, create a new terminal and execute the command
      (progn
        (ansi-term "/bin/bash" "ngrok-http-8080")
        (rename-buffer term-buffer-name)
        (comint-send-string nil "ngrok http http://localhost:8080\n")
        (popper-toggle-type)))))

(defun bin ()
  "Open a specific file."
  (interactive)
  (find-file "/usr/local/bin"))

(defun books ()
  "Open a specific file."
  (interactive)
  (find-file "~/Downloads/books"))

(defun D ()
  "Open a specific file."
  (interactive)
  (find-file "~/Downloads"))

(defun backup ()
  "Open a specific file."
  (interactive)
  (find-file "~/.backups/"))

(defun backup ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/org/projects"))

(defun dot ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles"))

(defun etc ()
  "Open a specific file."
  (interactive)
  (find-file "/etc"))

(defun snip ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/snippets/"))

;; When i had this function as 'log', i had an unexplained org mode bug.
(defun logs ()
  "Open a specific file."
  (interactive)
  (find-file "/var/log"))

(defun source ()
  "Open a specific file."
  (interactive)
  (find-file "~/.source"))

(defun colors ()
  "Open a specific file."
  (interactive)
  (list-faces-display))

(defun tmp ()
  "Open a specific file."
  (interactive)
  (find-file "/tmp"))

(defun passwd ()
  "Open a specific file."
  (interactive)
  (find-file "/etc/passwd"))

(defun service ()
  "Open a specific file."
  (interactive)
  (find-file "/etc/systemd/system"))

(defun crontab-edit ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun sway ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/sway")))

(defun i3 ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/i3")))

(defun zellij ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/zellij")))

(defun nix ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/nix")))

(defun date ()
  "Display the current date and time in the minibuffer using the shell's 'date' command."
  (interactive)
  (let ((date-output (shell-command-to-string "date")))
    (message (string-trim date-output))))

(defun off ()
  "Shutdown the system."
  (interactive)
  (call-process "poweroff"))

(defun init ()
  "Shutdown the system."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun reboot ()
  "Reboot the system."
  (interactive)
  (call-process "reboot"))

(defun notes ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.secret_dotfiles/org")))

(defun org ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.secret_dotfiles/org")))

(defun drill ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.secret_dotfiles/org/drill")))

(defun cards ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.secret_dotfiles/org/drill/general.org")))

(defun nvm ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/nvim/lua/user/")))

(defun emc ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/config.el"))

(defun zlj ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/zellij/config.kdl"))

(defun zsh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/zsh/.zshrc"))

(defun bsh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/bash/.bashrc"))

(defun scr ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles"))

(defun ssh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.ssh"))

(defun szsh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/zsh/.zshrc"))

(defun scripts ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/scripts/"))

(defun sdot ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/"))

(defun manager ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/nix/home.nix"))

(defun nix ()
  "Open a specific file."
  (interactive)
  (find-file "/etc/nixos/configuration.nix")
  (tramp-revert-buffer-with-sudo))

(defun so ()
  "Reload the Emacs configuration."
  (interactive)
  (save-some-buffers t)
  (load-file "~/.emacs.d/init.el")
  (load-file "~/.emacs.d/init.el"))

(defun messages ()
  "Switch to *Messages* buffer and ensure normal state."
  (interactive)
  (switch-to-buffer "*Messages*")
  (evil-force-normal-state)) ;; Because otherwise non-evil q binding doesn't work

(defun config ()
  "Open a specific file."
  (interactive)
  (find-file "~/.config"))

(defun evil ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/evil.el"))

(defun meow ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/meow.el"))

(defun todo ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/org/emacs/todo/todo.org"))

(defun templates ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/templates"))

(defun q ()
  "Save all modified buffers without prompting, then kill Emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))


