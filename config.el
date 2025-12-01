;; -*- lexical-binding: t -*-

(use-package emacs
  :custom
    ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  (help-window-select t)
  (file-name-shadow-mode 1)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  (xref-search-program 'ripgrep)
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  ;; (help-window-select t)
  ; allows us to type a new path without having to delete the current one
  ;; (file-name-shadow-mode 1)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
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


;; Visuals

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq minibuffer-message-timeout 0)
(setq inhibit-startup-screen t)

;; On Terminal: changes the vertical separator to a full vertical line
;;              and truncation symbol to a right arrow
(set-display-table-slot standard-display-table 'vertical-border ?\u2502)
(set-display-table-slot standard-display-table 'truncation ?\u2192)

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

(defun my-rectangle-mode-cursor-change ()
  "Change cursor color when entering or leaving rectangle-mark-mode."
  (if rectangle-mark-mode
      (set-cursor-color "#c4a7e7") ; Color when rectangle-mark-mode is active
    (set-cursor-color "white")))  ; Default cursor color

;; Add hook to run when rectangle-mark-mode is toggled
(add-hook 'rectangle-mark-mode-hook 'my-rectangle-mode-cursor-change)

;; Statusline

(defun my-mode-line-major-mode ()
  "Returns a clean name of the current major mode."
  (let ((mode (format "%s" major-mode)))
    (replace-regexp-in-string "-mode$" "" mode)))

(defun my-window-number ()
  "Get current window number (TRAMP-safe)."
  (when (not (file-remote-p default-directory))
    (let* ((windows (window-list-1 (frame-first-window) 'nomini t))
           (num (cl-position (selected-window) windows)))
      (format "%d " (1+ (or num 0))))))

(defun my-mode-line-right-side ()
  "Generate the Right Side string (Mode + Branch) to measure it."
  (let ((branch (my-vc-branch)))
    (if (and branch (not (string-empty-p branch)))
        (concat (my-mode-line-major-mode) " " branch)
      (my-mode-line-major-mode))))

(defun my-mode-line-right-width ()
  "Measure the width of the right side + 1 char padding."
  (+ 1 (length (my-mode-line-right-side))))

(defun my-buffer-name-display ()
  "Display path, truncated to ensure a 3-char gap + safety margin."
  (let* ((win-width (window-body-width))
         ;; SAFETY #1: Assume window is 2 chars smaller to account for fringes/borders
         (fringe-safety 3)

         (right-width (my-mode-line-right-width))

         (mod-len (if (and (buffer-modified-p) (buffer-file-name)) 4 0))

         ;; SAFETY #2: The Math
         ;; Reserve = RightSide + Mod + 3 (Gap) + FringeSafety
         (reserved-space (+ right-width mod-len 3 fringe-safety))

         (name (or (my-project-relative-path) "%b"))
         (avail-width (- win-width reserved-space))
         (final-width (max 10 avail-width)))

    (if (> (length name) final-width)
        (propertize (concat "..." (substring name (- (length name) final-width)))
                    'face 'mode-line-buffer-id)
      (propertize name 'face 'mode-line-buffer-id))))

;; (defun my-buffer-name-display ()
;;   "Display project-relative path for files/dired, or buffer name otherwise."
;;   (if-let* ((name (my-project-relative-path)))
;;       (propertize name 'face 'mode-line-buffer-id)
;;     (propertize "%b" 'face 'mode-line-buffer-id)))

(defun my-vc-branch ()
  "Get the current Git branch name, resilient to deleted directories."
  (when (and (or (buffer-file-name)
                 (eq major-mode 'dired-mode))
             (not (file-remote-p (or (buffer-file-name) default-directory)))
             (not (or (eq major-mode 'eshell-mode)
                      (eq major-mode 'special-mode)
                      (string-prefix-p "*" (buffer-name)))))
    (with-temp-buffer
      (condition-case nil
          ;; FIX: Temporarily switch to the Project Root to run the git command.
          ;; This ensures it works even if the current sub-folder was deleted.
          (let ((default-directory
                  (if-let ((proj (project-current)))
                      (project-root proj)
                    default-directory)))
            (when (zerop (call-process "git" nil t nil "branch" "--show-current"))
              (let ((branch (string-trim (buffer-string))))
                (unless (string-empty-p branch)
                  (let ((final-branch
                         (if (> (length branch) 15)
                             (concat (substring branch 0 12) "...")
                           branch)))
                    (format "[%s]" final-branch))))))
        (error nil)))))

(require 'project)

(defun my-project-relative-path ()
  "Return buffer path relative to project root, or full path if not in project."
  (let* ((file (buffer-file-name))
         (dir (and (eq major-mode 'dired-mode) default-directory))
         (target (or file dir)))
    (when target
      (if-let* ((proj (project-current)))
          (file-relative-name target (project-root proj))
        target))))

(defun my-modified-indicator ()
  "Return '[+]' if buffer is modified, otherwise empty string."
  (if (and (buffer-modified-p)
           (buffer-file-name)) ;; только для файлов
      " [+]"
    ""))

(setq-default mode-line-format
              '("%e"
                ;; Left: Path
                (:eval (my-buffer-name-display))

                ;; Left: Mod Indicator
                (:eval (my-modified-indicator))

                ;; Spacer: Align to (Right Edge - Right Width)
                ;; Since 'my-buffer-name-display' already subtracted this width PLUS 3,
                ;; there will physically be blank characters here before the alignment kicks in.
                (:eval (propertize " " 'display
                                   (list 'space :align-to
                                         `(- right ,(my-mode-line-right-width)))))

                ;; Right: The Text
                (:eval (my-mode-line-right-side))

                ;; Final padding
                " "))

;; Tabs

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

;; Deindent mode causes incorrect indentation on pasting (?)
;; (kill-ring-deindent-mode 1)
;; ;; try eshell-history-append
;; ;; set 'remote-file-name-access-timeout'

(setq erc-nick "wurfkreuz")
(global-set-key (kbd "C-x u") 'windmove-up)
(save-some-buffers t)

;; "Fixes" the bug with with the additional space at the end of a window
(setq rectangle-indicate-zero-width-rectangle nil)

(setq-default tab-width 4)
;; (add-hook 'yaml-ts-mode-hook (lambda () (setq tab-width 2)))
;; (add-hook 'my-yaml-mode-hook (lambda () (setq tab-width 2)))
(setq-default indent-tabs-mode t)

(recentf-mode)

(setq remote-file-name-access-timeout 120)

(setq vc-follow-symlinks t)

(setq dired-recursive-deletes 'always)
(setq dired-movement-style 'bounded)

(setq desktop-load-locked-desktop t)
(setq backup-inhibited t)

(add-hook 'prog-mode-hook (show-paren-mode t))

;; Bookmarks
(setq bookmark-default-file "~/.emacs.d/.bookmarks")
(setq bookmark-save-flag 1)

;; Auto pairing
(add-hook 'prog-mode-hook (electric-pair-mode t))
;; I don't know what it does exactly, it's more like a test
;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; Don't pair '<'
;; I've tried to disable it only for org-mode and wasn't succesfull.
(setq electric-pair-inhibit-predicate
      `(lambda (c)
        (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))


;; Break lines after a certain length
(setq sentence-end-double-space nil)
(auto-fill-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; (add-hook 'go-ts-mode-hook 'auto-fill-mode)


(setq python-shell-interpreter "/usr/bin/python3")

(defalias 'yes-or-no-p 'y-or-n-p)
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
			 (list "d"
				   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
				   "show diff between the buffer and its file"))

(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))

;; ;; Executable on save if starts with '#!'
(add-hook 'after-save-hook
        'executable-make-buffer-file-executable-if-script-p)

(make-directory (concat user-emacs-directory "messages") t)
(setq message-auto-save-directory "~/.emacs.d/messages")

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

(defun my-disable-lockfiles-in-tramp ()
  "Disable lockfiles when editing remote files via TRAMP."
  (when (tramp-tramp-file-p (buffer-file-name))
    (setq-local create-lockfiles nil)))

(add-hook 'find-file-hook #'my-disable-lockfiles-in-tramp)

;; Disable lockfiles, gives errors in general
(setq create-lockfiles nil)

;; (defun fov/disable-backups-for-gpg ()
;;   "Disable backups and autosaving for files ending in \".gpg\"."
;;   (when (and (buffer-file-name)
;;              (s-ends-with-p ".gpg" (buffer-file-name) t))
;;     (setq-local backup-inhibited t)
;;     (setq-local undo-tree-auto-save-history nil)
;;     (auto-save-mode -1)))
;; (add-hook 'find-file-hook #'fov/disable-backups-for-gpg)

;; ;; First, disable auto-save globally
;; (setq auto-save-default nil)
(setq auto-save-timer 3600)
(setq auto-save-interval 3000)
;; (auto-save-mode -1)

;; Save sessions
(unless (file-exists-p desktop-dirname)
  (make-directory desktop-dirname))
(desktop-save-mode 1)
;; (setq desktop-save 't)
(setq desktop-path (list desktop-dirname))
(setq desktop-auto-save-timeout 3600)

(setq save-place-file (concat user-emacs-directory "saveplace/places"))

;; Save cursor position
(unless (file-exists-p (concat user-emacs-directory "saveplace/"))
  (make-directory (concat user-emacs-directory "saveplace/")))
(save-place-mode 1)


(setq scroll-conservatively 101)
(setq scroll-margin 5)
;; (setq scroll-step 1)
;; I disabled '(setq scroll-step 1)' because i don't know the exact point of
;; why i was having this setting in the first place

(scroll-bar-mode -1)
;; (setq-default display-line-numbers-width 0)

(setq use-dialog-box nil)

;; Otherwise it will apply only partially leaving space between line numbers and the fringe area
;; (run-with-idle-timer 0 nil (lambda () (fringe-mode '(1 . 1))))
;; (fringe-mode '(1 . 1))
;; (fringe-mode 'minimal)
;; (fringe-mode 0)

(defun my-set-fringe-style (style-name)
  "Set fringe style using a named style from fringe-styles."
  (fringe-mode (cdr (assq style-name fringe-styles))))
;; Then use it like:
(my-set-fringe-style 'no-fringes)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(set-default 'truncate-lines t)
(add-hook 'special-mode-hook (lambda () (setq truncate-lines nil)))
;; List of modes that should have word-wrap enabled
(dolist (mode '(compilation-mode
                ;; Add other modes here
                ))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (setq-local truncate-lines nil))))

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

;; (winner-mode 1)

(setq enable-local-variables t)
(setq enable-dir-local-variables t)

(setenv "PERL5LIB" (concat (getenv "HOME") "/perl5/lib/perl5"))
(setenv "PATH" (concat (getenv "HOME") "/perl5/bin:" (getenv "PATH")))
(let ((paths '("/home/wurfkreuz/.nix-profile/bin"
              "/home/wurfkreuz/.ghcup/bin"
              "/home/wurfkreuz/go/bin/"
              "/home/wurfkreuz/test-dir/"
              "/home/wurfkreuz/.dotfiles/scripts/python"
              "/home/wurfkreuz/.dotfiles/scripts/sh"
			  "/home/wurfkreuz/perl5/bin"
              "/usr/bin")))
  ;; (setq exec-path (append paths exec-path))
  (setenv "PATH" (concat (string-join paths ":")
                        ":"
                        (getenv "PATH"))))

(require 'midnight)
(midnight-delay-set 'midnight-delay "07:00pm")

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

;; I haven't tried them yet
(setq isearch-lazy-count t)
(setq isearch-lazy-highlight t)


;; Cursor

;; No delay when deleting pairs (i don't know what it does really, just testing)
(setopt delete-pair-blink-delay 0)
;; Same here
(setopt show-paren-context-when-offscreen 'overlay) ; Emacs 29

(blink-cursor-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; ;; Enable Completion Preview mode in code buffers
;; (add-hook 'prog-mode-hook #'completion-preview-mode)
;; ;; also in text buffers
;; (add-hook 'text-mode-hook #'completion-preview-mode)
;; ;; and in \\[shell] and friends
;; (with-eval-after-load 'comint
;;   (add-hook 'comint-mode-hook #'completion-preview-mode))

;; ;; (global-completion-preview-mode 1)

;; (with-eval-after-load 'completion-preview
;;   ;; Show the preview already after two symbol characters
;;   (setq completion-preview-minimum-symbol-length 2)
;;   ;; Non-standard commands to that should show the preview:
;;   ;; Org mode has a custom `self-insert-command'
;;   (push 'org-self-insert-command completion-preview-commands)
;;   ;; Paredit has a custom `delete-backward-char' command
;;   (push 'paredit-backward-delete completion-preview-commands)
;;   ;; Bindings that take effect when the preview is shown:
;;   ;; Cycle the completion candidate that the preview shows
;;   (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
;;   (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
;;   ;; Convenient alternative to C-i after typing one of the above
;;   (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))


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
                                  ;; :family "NotoSerifNerdFont"
                                  :family "NotoSerif Nerd Font"
                                  :height 130
                                  ;; :foreground "#8bc34a"  ; Adjust the color as desired
                                  ;; :weight 'normal))))
                                  ;; :weight 'bold))))
                                  :weight 'regular))))

;; Emoji support
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))


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

;; Cron

(use-package crontab-mode
  :vc (:url "https://gitlab.com/Bacaliu/emacs-crontab-mode"
       :rev :newest))


;; Treesitter

(setq treesit-font-lock-level 4) ;; The default value is 3

(customize-set-variable 'treesit-enabled-modes
                        '(python-ts-mode
                          js-ts-mode
                          css-ts-mode
						  bash-ts-mode
						  dockerfile-ts-mode
						  go-ts-mode
                          json-ts-mode))

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (add-to-list 'global-treesit-auto-modes '(not yaml-ts-mode))
;;   (global-treesit-auto-mode))

(use-package clojure-ts-mode)

(setq treesit-language-source-alist
      '((lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (zig "https://github.com/maxxnino/tree-sitter-zig")
        (lua "https://github.com/tjdevries/tree-sitter-lua")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (c3 "https://github.com/c3lang/tree-sitter-c3")))

(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode"
            :rev :newest))

;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; (add-hook 'yaml-ts-mode-hook (lambda ()


;; Undo fu

(use-package undo-fu
  :config
  (setq undo-limit 67108864)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory
                                                            "undo-tree-history")))))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

;; Create undo directory if it doesn't exist
(make-directory "~/.emacs.d/undo-tree-history" t)


;; (defvar-local change-history nil
;;   "Ring of change positions for this buffer, most recent first.")

;; (defvar-local change-history-index 0
;;   "Current index in change history for navigation.")

;; (defconst change-history-max 100
;;   "Maximum number of change positions to remember.")

;; (defun record-change-position (beg end length)
;;   "Record change position in history.
;; Added to `after-change-functions'."
;;   (let ((pos (if (= length 0) end beg))) ; end for insertions, beg for deletions
;;     (unless (and change-history
;;                  (= pos (car change-history)))
;;       (push pos change-history)
;;       (when (> (length change-history) change-history-max)
;;         (setq change-history (butlast change-history)))
;;       (setq change-history-index 0))))

;; (add-hook 'after-change-functions #'record-change-position)

;; (defun my/goto-last-change (&optional n)
;;   "Move cursor through change history.
;; Without argument: go to previous change
;; With numeric prefix:
;;   - Positive N: go N steps back in history
;;   - Negative N: go N steps forward in history"
;;   (interactive "P")
;;   (if (null change-history)
;;       (message "No change history available.")
;;     (let* ((len (length change-history))
;;            (n (or n 1))
;;            (new-index (+ change-history-index n))
;;            (new-index (max 0 (min new-index (1- len)))))
;;       (setq change-history-index new-index)
;;       (goto-char (nth new-index change-history))
;;       (message "Position %d/%d" (1+ new-index) len))))

;; (defun goto-next-change ()
;;   "Move forward through change history."
;;   (interactive)
;;   (my/goto-last-change -1))


(use-package goto-chg)

;; (defadvice goto-last-change (around repeat-if-same-pos activate)
;;   "Repeat goto-last-change if cursor position doesn't change."
;;   (let ((old-pos (point)))
;;     ad-do-it
;;     (when (= old-pos (point))
;;       (goto-last-change 1))))

;; ;; I've being using this code, but i don't remember why
;; (defvar-local my-jump-ring '()
;;   "Ring of positions from goto-last-change jumps.")

;; (defvar-local my-jump-index 0
;;   "Current position in jump ring.")

;; (defun my-goto-last-change ()
;;   "Wrapper for goto-last-change that stores jump positions."
;;   (interactive)
;;   (let ((old-pos (point)))
;;     (call-interactively 'goto-last-change)
;;     (push old-pos my-jump-ring)
;;     (setq my-jump-index 0)))

;; (defun my-goto-last-change-reverse ()
;;   "Go back through stored jump positions."
;;   (interactive)
;;   (if (null my-jump-ring)
;;     (let ((pos (nth my-jump-index my-jump-ring)))
;;       (when pos
;;         (goto-char pos)
;;         (setq my-jump-index (1+ my-jump-index))
;;         (when (>= my-jump-index (length my-jump-ring))
;;           (setq my-jump-index 0))))))


;; ;; Avy

;; (use-package avy
;;   :ensure t
;;   )

;; (defun avy-jump-to-window ()
;;   "Use avy to jump to a specific window."
;;   (interactive)
;;   (let ((avy-all-windows 'all-frames))
;;     (avy-with avy-jump-to-window
;;       (avy--process
;;        (mapcar (lambda (w)
;;                  (cons (window-start w) w))
;;                (avy-window-list))
;;        #'avy--overlay-post))))

;; (with-eval-after-load 'avy
;;   (defun avy-action-copy-word (pt)
;;     "Copy word at PT and paste at current point (like evil's iw)."
;;     (let ((original-window (selected-window))
;;           (original-point (point)))
;;       (save-excursion
;;         (goto-char pt)
;;         (let ((bounds (evil-inner-word)))
;;           (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
;;       (select-window original-window)
;;       (goto-char original-point)
;;       (yank))
;;     t)

;;   (defun avy-action-copy-WORD (pt)
;;     "Copy WORD at PT and paste at current point (like evil's iW)."
;;     (let ((original-window (selected-window))
;;           (original-point (point)))
;;       (save-excursion
;;         (goto-char pt)
;;         (let ((bounds (evil-inner-WORD)))
;;           (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
;;       (select-window original-window)
;;       (goto-char original-point)
;;       (yank))
;;     t)

;;   (defun avy-action-copy-quoted (pt)
;;     "Copy quoted text at PT and paste at current point."
;;     (let ((original-window (selected-window))
;;           (original-point (point)))
;;       (save-excursion
;;         (goto-char pt)
;;         (let ((bounds (evil-select-quote ?\" t t)))
;;           (kill-ring-save (nth 0 bounds) (nth 1 bounds))))
;;       (select-window original-window)
;;       (goto-char original-point)
;;       (yank))
;;     t)

;;   ;; Add to dispatch alist
;;   (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-word
;;         (alist-get ?W avy-dispatch-alist) 'avy-action-copy-WORD
;;         (alist-get ?\" avy-dispatch-alist) 'avy-action-copy-quoted))


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


;; Xterm-color

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;; Ibuffer

(setq ibuffer-saved-filter-groups
	'(("default"
	   ("org" (or
			   (mode . org-mode)
			   (name . "^\\*Org Src")
			   (name . "^\\*Org Agenda\\*$")))
	   ("tramp" (name . "^\\*tramp.*"))
	   ("emacs" (or
				 (name . "^\\*scratch\\*$")
				 (name . "^\\*Messages\\*$")
				 (name . "^\\*Warnings\\*$")
				 (name . "^\\*Shell Command Output\\*$")
				 (name . "^\\*Async-native-compile-log\\*$")
				 (name . "^\\*straight-")))
	   ("ediff" (or
				 (name . "^\\*ediff.*")
				 (name . "^\\*Ediff.*")))
	   ("dired" (mode . dired-mode))
	   ("terminal" (or
					(mode . term-mode)
					(mode . shell-mode)
					(mode . eshell-mode)))
	   ("help" (or
				(name . "^\\*Help\\*$")
				(name . "^\\*info\\*$")
				(name . "^\\*helpful"))))))

(add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups


;; Proced

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))


;; Snippets/Tempel

(require 'base64)

(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         ("C-j" . tempel-previous)
         ("C-l" . tempel-next))

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

(with-eval-after-load 'tempel
  (defun tempel-base64 (elt fields)
	"Custom Tempel element that encodes the value of a field in Base64.
     Usage in a template: (base64 FIELD-NAME)"
	(pcase elt
      (`(base64 ,field)
       (let ((value (alist-get field fields)))
		 (if value
			 (base64-encode-string value t)
           "")))))
  (add-to-list 'tempel-user-elements #'tempel-base64))

;; (use-package tempel-collection
;;   :ensure t)

;; Orderless

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Completion

;; It makes completion-on-point to behave simpler, so that it doesn't takes into
;; account what goes after the pointer.
(defun my-completion-at-point-advice (orig-fun &rest args)
  "Insert a whitespace after the cursor before showing completion candidates, and clean it up afterward."
  (let ((inserted-whitespace (not (eq (char-after) ?\s))))
    (when inserted-whitespace
      (save-excursion
        (insert " ")))
    (unwind-protect
        (apply orig-fun args)
      (when inserted-whitespace
        (save-excursion
          (delete-char 1))))))

(advice-add 'completion-at-point :around #'my-completion-at-point-advice)

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

;; ;; Corfu setup
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;; :custom
  ;; (corfu-auto nil)
  ;; (corfu-min-length 2)
  :config
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (corfu-echo-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; (with-eval-after-load 'corfu
;;   (define-key corfu-map (kbd "RET") nil))

(defun buffer-words-completion ()
  "Generate completion candidates from words of 4+ characters in the buffer."
  (let ((words (list)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\_<[[:alnum:]_\\-]\\{4,\\}\\_>" nil t)
        (push (match-string-no-properties 0) words)))
    (delete-dups words)))

(defun my/buffer-words-capf ()
  "Native CAPF for buffer words (4+ chars)."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (current-input (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (candidates (buffer-words-completion)))
    (when bounds
      ;; Ensure we remove `current-input` before returning candidates
      (setq candidates (remove current-input candidates))
      (list (car bounds)          ; Start position
            (cdr bounds)          ; End position
            (lambda (string pred action)
              (complete-with-action action candidates string pred))
            :exclusive 'no))))    ; Allow combining with other CAPFs

(use-package cape
  :ensure t
  ;; :after corfu
  :config
  ;; (setq cape-dabbrev-check-other-buffers nil)

  ;; Default for all buffers
  (setq completion-at-point-functions
        (list #'tempel-expand  ; or #'tempel-expand
              #'cape-file))

  ;; For ALL buffers except elisp and eshell
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (unless (or (derived-mode-p 'emacs-lisp-mode)
                         (derived-mode-p 'eshell-mode))
                (setq-local completion-at-point-functions
                            (list #'tempel-expand  ; or #'tempel-expand
                                  #'cape-file
								  ;; These one are probably for icomplete
								  ;; (add-hook 'completion-at-point-functions #'tempel-complete nil t)
								  ;; (add-hook 'completion-at-point-functions #'cape-file nil t)
								  ;; (add-hook 'completion-at-point-functions #'my/buffer-words-capf nil t)
                                  ;; ;; #'my/dabbrev-capf)))))
                                  ;; #'dabbrev-capf)))))
                                  ;; #'cape-dabbrev)))))
                                  #'my/buffer-words-capf
								  (lambda ()
									(when (and (boundp 'eglot--managed-mode)
                                               eglot--managed-mode)
                                      (eglot-completion-at-point))))))))

  ;; For Elisp modes
  (dolist (mode '(emacs-lisp-mode
                 ielm-mode
                 lisp-interaction-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (setq-local completion-at-point-functions
                            (list #'tempel-expand  ; or #'tempel-expand
                                  #'elisp-completion-at-point
                                  #'cape-file)))))

  ;; Special case for eval-expression-minibuffer
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'tempel-expand  ; or #'tempel-expand
                                #'elisp-completion-at-point
                                #'cape-file)))))

(use-package bash-completion
  :config
  (bash-completion-setup))

;; advice for bash-completion
(defun my/bash-completion-fix-docker (orig-fun str comp single)
  "Pre-clean Docker candidates to remove descriptions before escaping.
This fixes the 'compose\\ \\ \\ (Docker Compose)' issue."
  (let* ((command (bash-completion--command comp))
         (clean-str
          (if (member command '("docker" "helm" "kubectl" "k"))
              (replace-regexp-in-string "\\(?:  +\\|\t\\).*$" "" str)
            str)))
    (funcall orig-fun clean-str comp single)))

(advice-add 'bash-completion-fix :around #'my/bash-completion-fix-docker)



;; ;; ;; For icomplete (?)
;; (defun my/force-completions ()
;;   (when (derived-mode-p 'bash-ts-mode 'sh-mode)
;;     (setq-local completion-at-point-functions
;;                 (list #'tempel-complete
;;                       #'cape-file
;;                       #'my/buffer-words-capf))))

;; (add-hook 'after-change-major-mode-hook #'my/force-completions)

;; (use-package fish-completion
;;   :vc (:url "https://github.com/LemonBreezes/emacs-fish-completion.git"
;;        :rev :newest))

;; (when (and (executable-find "fish")
;;          (require 'fish-completion nil t))
;; (global-fish-completion-mode))


;; ;; Access to ENV variables

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (setq exec-path-from-shell-arguments nil) ; Remove -i for faster startup
;;   (setq exec-path-from-shell-variables
;;         '("PATH"
;;           "FZF_DEFAULT_COMMAND"
;;           "SSH_AUTH_SOCK"
;;           "NOTIFY_TOKEN"
;;           "SHELF_TOKEN"
;;           "SHELF_DB_USER"
;;           "SHELF_DB_NAME"
;;           "SHELF_DB_PASS"
;;           "SHELF_DB_PORT"))
;;   (exec-path-from-shell-initialize))


;; Project.el

(require 'project)
(setq project-list-file-name-function #'project-files-find-function)

(defun project-find-file-all ()
  "Like project-find-file but always includes all files regardless of VCS status."
  (interactive)
  (project-find-file t))

;; test
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

;; second
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
                 (when (use-region-p) ;; third
                   (cons 'region (buffer-substring-no-properties
                                (region-beginning)
                                (region-end))))))

  (add-to-list 'embark-pre-action-hooks
               '(find-file embark--mark-target))

  (add-to-list 'embark-keymap-alist '(region . embark-region-map))

  (define-key embark-region-map (kbd "RET") #'my/find-file-embark))

;; Auto-enable grep-edit-mode for all grep buffers (when not running)
(defun my/auto-grep-edit-mode ()
  "Automatically enter grep-edit-mode after using embark-export."
  (when (and (derived-mode-p 'grep-mode)
             (not (get-buffer-process (current-buffer))))
    (run-with-idle-timer 0.1 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (grep-change-to-grep-edit-mode))))
                         (current-buffer))))

(add-hook 'grep-mode-hook #'my/auto-grep-edit-mode)

(defvar my/occur-edit-mode-enabled nil
  "Flag to prevent recursive occur-edit-mode activation.")

(defun my/auto-occur-edit-mode ()
  "Automatically enter occur-edit-mode after using embark-export."
  (unless my/occur-edit-mode-enabled
    (when (derived-mode-p 'occur-mode)
      (let ((my/occur-edit-mode-enabled t))
        (occur-edit-mode)))))

(add-hook 'occur-mode-hook #'my/auto-occur-edit-mode)


;; Vertico/Consult

(use-package vertico
  :ensure t
  ;; It's unset because it will change the keybinding for every minibuffer which
  ;; isn't what i want.
  ;; :bind (:map vertico-map
  ;;             ("C-<backspace>" . vertico-directory-up))
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

(defun my/vertico-handle-cmdline-face (&rest _)
  "Toggle vertico-current face based on index.
Prevents highlighting of the minibuffer command line itself."
  (when (and (bound-and-true-p vertico--index)
             (minibufferp))  ; Ensure we're in minibuffer
    (cond
     ;; Case 1: At command line (index -1) but no face remap active
     ((and (= vertico--index -1)
           (null my/vertico-face-remap))
      (setq my/vertico-face-remap
            (face-remap-add-relative 'vertico-current 'default)))

     ;; Case 2: Not at command line but face remap still active
     ((and (/= vertico--index -1)
           my/vertico-face-remap)
      (face-remap-remove-relative my/vertico-face-remap)
      (setq my/vertico-face-remap nil)))))

;; Clean up face remap when exiting minibuffer
(defun my/vertico-cleanup-face-remap ()
  "Clean up face remapping when exiting minibuffer."
  (when (and (minibufferp)
             my/vertico-face-remap)
    (face-remap-remove-relative my/vertico-face-remap)
    (setq my/vertico-face-remap nil)))

;; Add our advice and hooks
(advice-add 'vertico--exhibit :after #'my/vertico-handle-cmdline-face)
(add-hook 'minibuffer-exit-hook #'my/vertico-cleanup-face-remap)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :init
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden"))
;; :ensure t
  ;; :config
  ;; (with-eval-after-load 'org
  ;;   (define-key org-mode-map (kbd "C-s C-o") 'consult-imenu)))

;; Variable to store the last search string
(defvar my/last-search-pattern nil
  "Stores the last search pattern from consult-line, consult-ripgrep, or isearch.")

(defun my/store-search-string-line (&rest args)
  "Store the search string used in consult-line."
  (let ((pattern (car consult--line-history)))
    (setq my/last-search-pattern pattern)
    (message "Line pattern stored: %s" pattern)))

(defun my/store-search-string-ripgrep (&rest args)
  "Store the search string used in consult-ripgrep."
  (let* ((pattern (car consult--grep-history))
         (clean-pattern (replace-regexp-in-string "^#" "" pattern)))
    (setq my/last-search-pattern clean-pattern)
    (message "Ripgrep pattern stored: %s" clean-pattern)))

(defun my/store-search-string-isearch ()
  "Store the last Isearch string."
  (when (and (not isearch-regexp) isearch-string)
    (setq my/last-search-pattern isearch-string)
    (message "Isearch pattern stored: %s" isearch-string)))

(advice-add 'consult-line :after #'my/store-search-string-line)
(advice-add 'consult-ripgrep :after #'my/store-search-string-ripgrep)
(add-hook 'isearch-mode-end-hook #'my/store-search-string-isearch)

;; Define a variable to track the original search direction
(defvar my/search-direction 'forward
  "Direction of the last search: 'forward or 'backward.")

;; Advice to set the search direction when using isearch
(defadvice isearch-forward (before my/set-search-direction activate)
  "Set the search direction to forward before starting isearch."
  (setq my/search-direction 'forward))

(defadvice isearch-backward (before my/set-search-direction activate)
  "Set the search direction to backward before starting isearch."
  (setq my/search-direction 'backward))

;; Modify your existing functions to respect the search direction
(defun my/search-next ()
  "Search in the same direction as the last search."
  (interactive)
  (when my/last-search-pattern
    (let ((case-fold-search t)
          (current-point (point)))
      (if (eq my/search-direction 'forward)
          (progn
            ;; Move one character forward to avoid finding the current match
            (forward-char)
            (if (re-search-forward my/last-search-pattern nil t)
                (progn
                  (goto-char (match-beginning 0))
                  (message "Match found: %s" (match-string 0)))
              (goto-char current-point)
              (message "No more matches")))
        ;; If original direction was backward
        (if (re-search-backward my/last-search-pattern nil t)
            (progn
              (goto-char (match-beginning 0))
              (message "Match found: %s" (match-string 0)))
          (goto-char current-point)
          (message "No more matches"))))))

(defun my/search-previous ()
  "Search in the opposite direction of the last search."
  (interactive)
  (when my/last-search-pattern
    (let ((case-fold-search t)
          (current-point (point)))
      (if (eq my/search-direction 'forward)
          ;; If original direction was forward, go backward
          (if (re-search-backward my/last-search-pattern nil t)
              (progn
                (goto-char (match-beginning 0))
                (message "Match found: %s" (match-string 0)))
            (goto-char current-point)
            (message "No more matches"))
        ;; If original direction was backward, go forward
        (progn
          (forward-char)
          (if (re-search-forward my/last-search-pattern nil t)
              (progn
                (goto-char (match-beginning 0))
                (message "Match found: %s" (match-string 0)))
            (goto-char current-point)
            (message "No more matches")))))))

;; Disable preview for consult-recent-file
(advice-add 'consult-recent-file :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'consult--file-preview) #'ignore))
                (apply orig-fun args))))

(use-package embark-consult
  :ensure t)

(defun consult-line-visible-windows ()
  "Search for a matching line in all buffers displayed in current windows."
  (interactive)
  (let* ((buffers (delete-dups (mapcar #'window-buffer (window-list))))
         (names (mapcar #'buffer-name buffers)))
    (consult-line-multi
     `(:predicate ,(lambda (buf) (member (buffer-name buf) names))))))

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


;; Repeat-fu

(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)

  :config
  (setq repeat-fu-preset 'meow)

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "C-.") 'repeat-fu-execute)
       (define-key meow-insert-state-keymap (kbd "C-.") 'repeat-fu-execute)))))


;; Hydra

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-size (:color red)
    "window size"
    ("+" enlarge-window "increase height")
    ("-" shrink-window "decrease height")
    (">" enlarge-window-horizontally "increase width")
    ("<" shrink-window-horizontally "decrease width")
    ("t" transpose-window-layout "transpose windows")
    ("f" flip-window-layout-horizontally "flip windows")
    ;; ("t" transpose-frame "transpose windows")
    ;; ("t" emacs-solo/transpose-split "transpose windows")
    ("r" rotate-windows "rotate windows")
    ("q" nil "quit")))


;; Save fold

;; It causes 'end of file' error. If something like this will happen again, try
;; to check the savefold folder and delete files one by one trying to pres 'C-x
;; C-c'.
(use-package savefold
  :vc (:url "https://github.com/jcfk/savefold.el"
			:rev :newest)
  :init
  (setq savefold-backends '(outline org))
  (setq savefold-directory (locate-user-emacs-file "savefold"))  ;; default
  :config
  (savefold-mode 1))


;; Tramp

;; It can cause an error in some containers that loosk like this:
;; File error: ‘tramp-histfile-override’ uses invalid file ‘/.tramp_history’
;; signal: ‘tramp-histfile-override’ uses invalid file ‘/.tramp_history’
;; This is why the setting is disabled
(setq tramp-histfile-override nil)

;; Dired

;; Basic trash settings
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.local/share/Trash/files/")

(setq dired-recursive-copies 'always)

;; Configure connection-local variables for sudo operations
(connection-local-set-profile-variables
 'remote-trash-directory
 '((trash-directory . "/sudo::~/.local/share/trash/")))

(connection-local-set-profiles
 `(:application tramp :protocol "sudo" :machine ,(system-name))
 'remote-trash-directory)

;; Auto-revert for sudo files
;; Hardcoding it like this causes problems
;; (add-to-list 'auto-revert-remote-files "/sudo:root@localhost:/")

;; (setq auto-revert-remote-files '("/ssh:" "/scp:" "/sudo:" "/sftp:"))
(setq auto-revert-remote-files nil)

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

(require 'smerge-mode)

;; For magit, otherwise the magit-format-file-nerd-icons option wont work
(use-package nerd-icons)

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

;; Icons
(setopt magit-format-file-function #'magit-format-file-nerd-icons)


;; Custom option for stash, because i don't know how else to execute 'stash
;; apply' without the '--index' flag.
(transient-define-suffix magit-stash-apply-no-index (stash)
  "Apply a stash to the working tree without --index."
  :description "apply (no index)"
  (interactive (list (magit-read-stash "Apply stash")))
  (magit-run-git "stash" "apply" stash))

(transient-append-suffix 'magit-stash "a"
  '("A" magit-stash-apply-no-index))

(with-eval-after-load 'magit-process
  (define-key magit-process-mode-map (kbd "j") 'magit-section-forward))

;; (defun my-git-commit-setup ()
;;   "Set up the default commit message."
;;   (insert "n"))

;; (add-hook 'git-commit-setup-hook 'my-git-commit-setup)

(with-eval-after-load 'magit
  (defun my-magit-branch-rename (old new &optional force)
    "Rename branch OLD to NEW, prefilling NEW with OLD."
    (interactive
     (let ((branch (magit-read-local-branch "Rename branch")))
       (list branch
             (magit-read-string-ns
              (format "Rename branch '%s' to" branch)
              branch   ;; дефолт — старое имя
              'magit-revision-history)
             current-prefix-arg)))
    ;; здесь вызываем git напрямую, без рекурсии
    (magit-call-git "branch" (if force "-M" "-m") old new)
    ;; обновляем статус буфера
    (magit-refresh))

  ;; заменить оригинал
  (advice-add 'magit-branch-rename :override #'my-magit-branch-rename))


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


;; ;; Buffers
;; ;; Spawn and Pointer

;; This parameter makes these buffers to spawn at the current window
(setq display-buffer-alist
      (mapcar (lambda (name)
                `(,name display-buffer-same-window (nil)))
              '("*Faces*"
                "*info*"
                "*helpful*"
                ;; "*Help*"
                "*Warnings*"
                "*Async Shell Command*"
                "*vc-git*"
                "*compilation*"
                "*debug*")))

;; (add-to-list 'display-buffer-alist
;;              '("\\*\\(Man\\|Help*\\|special\\) "
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (post-command-select-window . t)))


(setq display-buffer-alist
      `(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . visible))

        ("\\*Man "
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (post-command-select-window . t))

        ("\\*Async Shell Command\\*"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (post-command-select-window . t))))

        ;; Causes a bug with corfu
        ;; ("\\*Help\\*"
        ;;  (display-buffer-reuse-window display-buffer-pop-up-window)
        ;;  (post-command-select-window . t))))

;; I commented the code because i found the '(help-window-select t)' option
;; ;; Function to focus help buffer after it's displayed
;; (defun my/focus-help-buffer (&rest _)
;;   "Focus the help buffer after it's displayed."
;;   (when (get-buffer "*Help*")
;;     (let ((help-window (get-buffer-window "*Help*")))
;;       (when help-window
;;         (select-window help-window)))))
;; ;; Add advice to help-related functions
;; (advice-add 'help-buffer-toggle :after #'my/focus-help-buffer)
;; (advice-add 'describe-function :after #'my/focus-help-buffer)
;; (advice-add 'describe-variable :after #'my/focus-help-buffer)
;; (advice-add 'describe-key :after #'my/focus-help-buffer)
;; (advice-add 'describe-mode :after #'my/focus-help-buffer)
;; (advice-add 'describe-package :after #'my/focus-help-buffer)
;; (advice-add 'view-echo-area-messages :after #'my/focus-help-buffer)

(defun my-eldoc-print-and-switch ()
  "Print eldoc info and switch to its buffer."
  (interactive)
  (eldoc 1)
  (eldoc-print-current-symbol-info)
  (sit-for 0.1) ;; Small delay to ensure buffer creation
  (when (get-buffer "*eldoc*")
    (switch-to-buffer-other-window "*eldoc*")))


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


;; ;; Buffer termination

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))


;; Envrc (direnv)

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; Flymake

(require 'flymake)

(set-face-attribute 'flymake-error nil :underline nil)
(set-face-attribute 'flymake-warning nil :underline nil)
(set-face-attribute 'flymake-note nil :underline nil)

;; ;; (setq flymake-show-diagnostics-at-end-of-line t)

;; (use-package flymake-ansible-lint
;;   :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
;;          ((yaml-ts-mode yaml-mode) . flymake-mode)))

;; ;; (defun enable-flymake-mode ()
;; ;;   "Enable flymake-mode in dockerfile-mode."
;; ;;   (if (string-equal major-mode "dockerfile-mode")
;; ;;       (flymake-mode 1)))

;; ;; ;;   ;; Add the hook to enable flymake-mode when entering dockerfile-mode
;; ;; (add-hook 'dockerfile-mode-hook 'enable-flymake-mode)

;; (use-package flymake-hadolint
;;   :ensure t)

;; (add-hook 'dockerfile-mode-hook #'flymake-hadolint-setup)

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


;; Compilation mode

(use-package compile
  :ensure nil
  :hook
  (;; Not ideal, but I do not want this poluting the modeline
   (compilation-start . (lambda () (setq compilation-in-progress nil))))
  :custom
  (compilation-always-kill t)
  ;; (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (setq compilation-error-screen-columns nil)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))


;; (add-to-list 'compilation-error-regexp-alist
;;              'yaml)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(yaml "^\\(.*?\\):\\([0-9]+\\)" 1 2)
;;              )

; Replace make -k with ansible-lint, with an UTF-8 locale to avoid crashes
(defun ansible-lint-errors ()
  (make-local-variable 'compile-command)
  (let ((ansiblelint_command "ansible-lint ") (loc "LANG=C.UTF-8 "))
    (setq compile-command (concat loc ansiblelint_command buffer-file-name)))
  )
(add-hook 'yaml-ts-mode-hook 'ansible-lint-errors)


(add-to-list 'compilation-error-regexp-alist 'gcc-clang-col)
(add-to-list 'compilation-error-regexp-alist-alist
             '(gcc-clang-col
               "^\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):.*\\(?:error\\|warning\\|note\\)"
               1 2 3))

(add-to-list 'compilation-error-regexp-alist 'c3c)
(add-to-list 'compilation-error-regexp-alist-alist
             '(c3c
               "\\(/[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\).*\\(?:Error\\|Warning\\)"
               1 2 3))


(add-to-list 'compilation-error-regexp-alist 'go)
(add-to-list 'compilation-error-regexp-alist-alist
             '(go
               "^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\\s-"
               1  ; File name group
               2  ; Line number group
               3  ; Column number group
               ))


(defun my/next-error-with-column ()
  "Move to the next error and jump to the correct column."
  (interactive)
  (let ((error-info (compilation-next-error 1 t)))  ;; Get the next error
    (when error-info
      (let ((line (nth 0 error-info))      ;; Get the line number from the error
            (column (nth 2 error-info)))   ;; Get the column number from the error
        (goto-line line)
        (move-to-column column)))))

;; Essential column handling configuration
;; (setq compilation-error-screen-columns t)  ; Account for tab characters
;; (setq compilation-auto-jump-to-first-error nil)  ; Prevent interference

;; (add-to-list 'compilation-error-regexp-alist 'go)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(go
;;                "^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\): "  ; Matches file:line:column:
;;                ;; "^\\(?:[^0]\\|0\\(?:[^/]\\|$\\)\\)\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\): "
;;                ;; "^\\(?!2025/\\).+?:\\([0-9]+\\):\\([0-9]+\\): "  ; Exclude dates at start
;;                ;; "^\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):.*\\(?:cannot\\|warning\\|note\\)"
;;                1  ; File name group
;;                2  ; Line number group
;;                3  ; Column number group
;;                ))


;; ;; If you want to keep the original gcc pattern as fallback
;; ;; but give priority to the new one that includes columns
;; (setq compilation-error-regexp-alist
;;       (cons 'gcc-clang-col
;;             (delq 'gcc-clang-col compilation-error-regexp-alist)))

(defun my/set-compile-command ()
  "Set compilation command based on major mode"
  (setq-local compile-command
              (cond
               ((eq major-mode 'bash-ts-mode)
                (format "shellcheck -f gcc %s" buffer-file-name))

               ((eq major-mode 'python-ts-mode)
                (format "python %s" buffer-file-name))

               ((eq major-mode 'c-ts-mode)
                (format "clang %s" buffer-file-name))

			   ((eq major-mode 'dockerfile-mode)
                (format "hadolint -f gnu %s" buffer-file-name))

			   ((and (eq major-mode 'nix-mode)
					 (string-equal (buffer-name) "home.nix"))
				(format "home-manager switch"))

               ;; Fallback to default compile command
               (t compile-command))))

;; Add hook to set compile command when buffer is loaded
(add-hook 'bash-ts-mode-hook #'my/set-compile-command)
(add-hook 'python-ts-mode-hook #'my/set-compile-command)
(add-hook 'c-ts-mode-hook #'my/set-compile-command)
(add-hook 'nix-mode-hook #'my/set-compile-command)


;; (add-to-list 'compilation-error-regexp-alist 'shellcheck)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(shellcheck
;;                "^In \\(.*\\) line \\([0-9]+\\):" ;; Matches "In <file> line <line-number>:"
;;                1 2)) ;; 1=file path, 2=line number

;; (defun shellcheck-errors ()
;;   "Set up `compile-command` to use shellcheck for the current buffer."
;;   (make-local-variable 'compile-command)
;;   (let ((file (buffer-file-name)))
;;     (setq default-directory (file-name-directory file))
;;     (setq compile-command (concat "LANG=C.UTF-8 shellcheck -f gcc "
;;                                   (shell-quote-argument file)))))

;; ;; Add the hook for shell script mode
;; (add-hook 'bash-ts-mode-hook 'shellcheck-errors)

;; (defun my-compilation-highlight-errors-shellcheck ()
;;   "Add custom font-lock rules for highlighting (info), (error), and (warning) in compilation buffers."
;;   (font-lock-add-keywords
;;    nil
;;    '(("^.*\\(\\^--\\|\\^------\\).*\\((info)\\):.*$" . 'compilation-info)
;;      ("^.*\\(\\^--\\|\\^------\\).*\\((error)\\):.*$" . 'compilation-error)
;;      ("^.*\\(\\^--\\|\\^------\\).*\\((warning)\\):.*$" . 'compilation-warning))))

;; ;; Hook the custom highlighting into `compilation-mode`
;; (add-hook 'compilation-mode-hook 'my-compilation-highlight-errors-shellcheck)

(defun my-perl-set-compile-command ()
  "Set `compile-command` to run `perl -c && perlcritic` on the current file when in `perl-mode`."
  (when (eq major-mode 'perl-mode)
    (setq-local compile-command
                ;; (format "perl -c %s && perlcritic %s"
                ;; (format "perlcritic %s && perl %s"
                (format "perl %s && perlcritic %s"
                        (shell-quote-argument (buffer-file-name))
                        (shell-quote-argument (buffer-file-name))))))

(add-hook 'perl-mode-hook #'my-perl-set-compile-command)


;; Modes

(use-package perl-mode)
(use-package lua-mode)
(use-package terraform-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package nginx-mode
  :custom
  (nginx-indent-level 2))
  ;; :mode ("\\.ya?ml\\'" . yaml-mode)
  ;; :hook (yaml-mode . (lambda ()
  ;;                      (auto-fill-mode -1))))
(use-package nix-mode)
;; (use-package systemd
;;   :ensure t)
(use-package markdown-mode) ;; can't be found by the package installer


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


;; ;; Eglot

;; (require 'eglot)

;; (setq eglot-stay-out-of
;;       '(
;;         (lambda ()
;;           (when buffer-file-name
;;             (file-remote-p buffer-file-name)))
;;         ))

;; ;; Aplying this config breaks something when i use yaml lsp
;; ;; (use-package eglot
;; ;;   :ensure nil
;; ;;   :custom
;; ;;   (eglot-autoshutdown t)
;; ;;   (eglot-events-buffer-size 0)
;; ;;   (eglot-events-buffer-config '(:size 0 :format full))
;; ;;   (eglot-prefer-plaintext t)
;; ;;   (jsonrpc-event-hook nil)
;; ;;   (eglot-code-action-indications nil) ;; Emacs 31 -- annoying as hell
;; ;;   :init
;; ;;   ;; This is to make lua-language-server to not stutter when i execute the
;; ;;   ;; 'newline' command
;; ;;   (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
;; ;;   (fset #'jsonrpc--log-event #'ignore))

;; ;; (setq eglot-events-buffer-size 0)

;; (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
;; (add-hook 'nix-mode-hook 'eglot-ensure)

;; ;; (add-to-list 'eglot-server-programs '((c-ts-mode) "clangd"))
;; ;; (add-hook 'c-ts-mode-hook 'eglot-ensure)

;; (add-hook 'python-ts-mode-hook 'eglot-ensure)
;; ;; I need to prevent a situation where ansible-lint and eglot work on the same buffer.
;; ;; (add-hook 'yaml-ts-mode-hook #'eglot-ensure)
;; (add-hook 'terraform-mode-hook #'eglot-ensure)

;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))

;; (set-face-attribute 'eldoc-highlight-function-argument nil
;;                     :inherit 'unspecified' :foreground 'unspecified' :weight 'medium)
;; (set-face-attribute 'eglot-highlight-symbol-face nil
;;                     :inherit 'unspecified' :foreground 'unspecified' :weight 'medium)


;;   ;; (add-hook 'eglot-managed-mode-hook
;;   ;;           (lambda ()
;;   ;;             (message "Eglot started with env: VIRTUAL_ENV=%s" (getenv "VIRTUAL_ENV"))
;;   ;;             (message "Python path: %s" (executable-find "python")))))


;; ;; Yaml

;; (add-to-list 'eglot-server-programs
;;              ;; '((yaml-mode) "yaml-language-server" "--stdio"))
;;              '((my-yaml-mode) "yaml-language-server" "--stdio")
;;              '((yaml-ts-mode) "yaml-language-server" "--stdio"))

;; ;; Configure filetypes equivalent
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . my-yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . my-yaml-mode))

;; ;; Configure the schema mappings for Kubernetes
;; (setq-default eglot-workspace-configuration
;;               '((yaml
;;                  (schemas . ((kubernetes . ["*/templates/*.yaml"
;;                                            "*/kubernetes/*.yaml"
;;                                            "*/templates/*.yml"]))))))

;; ;; Hook to start eglot automatically with yaml files
;; (add-hook 'yaml-mode-hook 'eglot-ensure)
;; (add-hook 'my-yaml-mode-hook 'eglot-ensure)
;; (add-hook 'yaml-ts-mode-hook 'eglot-ensure)

;; ;; Example for dir-locals:

;; ;; ((yaml-ts-mode . ((eglot-workspace-configuration . ((yaml schemas . ((
;; ;;     https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json ".gitlab-ci.yml"
;; ;;     ./argocd-application.schema.json [
;; ;;         "/Apps/*"
;; ;;         "/apps.yaml"
;; ;;     ]
;; ;;     Kubernetes ["k8s-*.yaml"]
;; ;; ))))))))

;; ;; SQL

;; (add-to-list 'eglot-server-programs
;;              '(sql-mode . ("sqls")))
;; (add-hook 'sql-mode-hook 'eglot-ensure)


;; Org Mode

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
  (setq org-src-fontify-natively t)
  (setq org-startup-with-inline-images t)
  ;; ;; The idea here is to make org-babel to use bash-ts-mode instead of sh-mode
  ;; (setq org-babel-sh-command "/bin/bash")
  ;; (setq org-edit-src-content-indentation 0)
  (setq org-blank-before-new-entry
        '((heading . nil)
          (plain-list-item . nil)))

  ;; Disable auto-blank-lines in self-insert-command
  (setq org-list-empty-line-terminates-plain-lists nil)
  (setq org-empty-line-terminates-plain-lists nil)
  (setq org-startup-folded 'showeverything) ;; it's set for the savefold package
  )

;; Fix org babel with bash-ts-mode
(defun my-sh--guess-shell ()
  "/bin/bash")
(advice-add 'sh--guess-shell :override #'my-sh--guess-shell)

(define-key org-mode-map (kbd "C-x p") #'yank-media)

(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images)))

(setq org-yank-image-save-method "~/.secret_dotfiles/pictures/org/")


;; A code that potentially can help me to update incorrect image links, but i
;; haven't tried it.
(defun update-image-links (old-dir new-dir)
  (interactive "DEnter old directory: \nDEnter new directory: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "file:\\([^]]+\\)" nil t)
      (let* ((old-path (match-string 1))
             (new-path (replace-regexp-in-string
                       (regexp-quote old-dir)
                       new-dir
                       old-path)))
        (replace-match (concat "file:" new-path))))))


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

;; (defun my/org-smart-meta-return ()
;;   "Intelligently create a new heading or list item.
;; If on a heading line, create a subheading (like M-S-RET).
;; Otherwise, if the previous line is blank, simply insert a new line at point.
;; In all other cases, behave like `org-meta-return` (M-RET)."
;;   (interactive)
;;   (if (org-at-heading-p)
;;       (org-insert-subheading nil)
;;     (if (save-excursion
;;           (forward-line -1)
;;           (looking-at-p "^[[:space:]]*$"))
;;         (progn
;;           (org-meta-return)
;;           (back-to-indentation)
;;           (newline)
;;           (move-end-of-line 1))
;;       (org-meta-return))))

;; (define-key org-mode-map (kbd "M-RET") #'my/org-smart-meta-return)

;; (defun my/org-smart-heading ()
;;   "Create a new heading intelligently:
;; If on a heading line, create a subheading (M-S-RET).
;; Otherwise, create a same-level heading (M-RET)."
;;   (interactive)
;;   (if (org-at-heading-p)
;;       (org-insert-subheading nil)
;;     (org-meta-return)))

;; ;; Bind it to a key of your choice, for example:
;; (define-key org-mode-map (kbd "M-RET") #'my/org-smart-heading)

;; (defun my-org-meta-return ()
;;   "Custom version of `org-meta-return' that, when the previous line is blank,
;; simply inserts a new line at point instead of moving the cursor and inserting
;; a new list item."
;;   (interactive)
;;   (if (save-excursion
;;         (forward-line -1)
;;         (looking-at-p "^[[:space:]]*$"))
;;       (progn
;; 		(org-meta-return)
;;         (back-to-indentation)
;;         (newline)
;;         (move-end-of-line 1))
;;     (progn
;;       (org-meta-return))))


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
(add-to-list 'org-structure-template-alist '("bash" . "src bash-ts"))
(add-to-list 'org-structure-template-alist '("py" . "src python-ts"))
(add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("fund" . "src fundamental"))
(add-to-list 'org-structure-template-alist '("text" . "src text"))
(add-to-list 'org-structure-template-alist '("toml" . "src toml"))
;; (add-to-list 'org-structure-template-alist '("yaml" . "src yaml-ts"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json-ts"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("go" . "src go-ts"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf"))
(add-to-list 'org-structure-template-alist '("docker" . "src dockerfile"))
(add-to-list 'org-structure-template-alist '("hcl" . "src hcl"))
(add-to-list 'org-structure-template-alist '("md" . "src markdown"))
;; (add-to-list 'org-structure-template-alist '("sc" . "src clojure-ts"))

;; It's displayed incorrectly in icomplete-vertical and causes a stutter with
;; vertico
;; (add-to-list 'org-structure-template-alist
;;            '("t" . "src TODO\n\n* TODO \n\n?"))

(defun my/org-tempo-insert-block ()
  "Insert a source block and maintain indentation."
  (interactive)
  (let* ((indent (current-indentation))
         (indent-str (make-string indent ?\s)))
    (org-tempo-complete-tag)
    ;; Fix begin tag indentation
    (beginning-of-line 0)  ; go to beginning of #+begin line
    (delete-horizontal-space)  ; remove any existing indentation
    (insert indent-str)
    ;; Add indentation for cursor position
    (forward-line)
    (delete-horizontal-space)
    (insert indent-str)
    ;; Fix end tag indentation
    (forward-line)
    (delete-horizontal-space)  ; remove any existing indentation
    (insert indent-str)
    ;; Go back to content line
    (forward-line -1)
    (end-of-line)))


;; Visuals

(setq org-hide-emphasis-markers t)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n")
    (lambda () (interactive)
      (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
      (org-mode-restart))))


;; Org appear

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))


;; Custom commands

(defun Cp ()
  "Copy full path of the current buffer, stripping Tramp prefix if present."
  (interactive)
  (let ((path-to-copy nil))
    (cond
     ((eq major-mode 'dired-mode)    ; Dired buffer
      (setq path-to-copy (my/pwd)))
     ((eq major-mode 'eshell-mode)   ; Eshell buffer
      (setq path-to-copy (eshell/pwd)))
     (t                              ; Default: Regular File buffer
      (setq path-to-copy (buffer-file-name))))
    ;; Strip Tramp prefix if present
    (when (and path-to-copy (tramp-tramp-file-p path-to-copy))
      (setq path-to-copy (tramp-file-name-localname (tramp-dissect-file-name path-to-copy))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message "Copied path '%s' to the clipboard." path-to-copy))
      (message "Current buffer has no associated path to copy."))))

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
  (find-file "~/.secret_dotfiles/trash/"))

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
  (find-file (expand-file-name "~/.dotfiles/zellij/config.kdl")))

(defun nix ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/nix/")))

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

(defun md ()
  "Open org notes directory."
  (interactive)
  (find-file (expand-file-name "~/.secret_dotfiles/markdown/")))

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

(defun alc ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/alacritty/alacritty.toml"))

(defun foot ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/foot/foot.ini"))

(defun zsh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/zsh/.zshrc"))

(defun bsh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/bash/.bashrc"))

(defun py ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/scripts/python/"))

(defun sh ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/scripts/sh/"))

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
  (find-file "~/.dotfiles/home-manager/home.nix"))

(defun so ()
  "Reload the Emacs configuration."
  (interactive)
  (save-some-buffers t)
  ;; (load-file "~/.emacs.d/init.el")
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

(defun tmux ()
  "Open a specific file."
  (interactive)
  (find-file "~/.dotfiles/tmux/.tmux.conf"))

(defun evil ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/evil.el"))

(defun meow ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/meow.el"))

(defun projects ()
  "Open a specific file."
  (interactive)
  (find-file "~/.projects/"))

(defun todo ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/org/emacs/todo/todo-emacs.org"))

(defun steam ()
  "Open a specific file."
  (interactive)
  (find-file "~/.local/share/Steam/steamapps/common/"))

(defun templates ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/other/templates"))

(defun plan ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/plan/"))

(defun q ()
  "Save all modified buffers without prompting, then kill Emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun tasks ()
  "Open a specific file."
  (interactive)
  (find-file "~/.secret_dotfiles/job/tasks/"))


;; I probably must set it after enabling line numbers. Enabling it from the
;; original position make it to not work in many modes.
(run-with-idle-timer 0 nil (lambda () (fringe-mode '(1 . 1))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
