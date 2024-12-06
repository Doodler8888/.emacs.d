;; -*- lexical-binding: t -*-

;; Visuals

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'visual
      display-line-numbers-type 'relative)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-space-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

(setq-default mode-line-format
              '("%e"
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

;; (server-start)
(setq erc-nick "wurfkreuz")
(global-set-key (kbd "C-x u") 'windmove-up)
;; (setq evil-want-keybinding nil)

(recentf-mode)

(setq vc-follow-symlinks t)

(setq dired-recursive-deletes 'always)

(setq desktop-load-locked-desktop t)
(setq backup-inhibited t)

(add-hook 'prog-mode-hook (show-paren-mode t))

;; Auto pairing
(add-hook 'prog-mode-hook (electric-pair-mode t))

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

;; Autoinsertion on the search buffer
(setq ivy-initial-inputs-alist nil)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
;; (savehist-mode 1)
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

(setq-default truncate-lines t)

(setq enable-local-variables t)
(setq enable-dir-local-variables t)

(let ((paths '("/home/wurfkreuz/.nix-profile/bin"
              "/home/wurfkreuz/.ghcup/bin"
              "/usr/bin")))
  (setq exec-path (append paths exec-path))
  (setenv "PATH" (concat (string-join paths ":")
                        ":"
                        (getenv "PATH"))))

(require 'midnight)
(midnight-delay-set 'midnight-delay "10:00pm")

;; (setq comint-use-prompt-regexp nil)

(setq auto-revert-verbose nil)

(setq display-buffer-base-action '(nil . ((some-window . mru))))

;; (with-eval-after-load 'comint
;;   (add-hook 'comint-mode-hook #'completion-preview-mode))
;; (add-hook 'eshell-mode-hook #'completion-preview-mode)
;; (add-hook 'minibuffer-mode-hook #'completion-preview-mode)

;; (defun my-conditional-completion-preview ()
;;   "Enable completion-preview-mode selectively."
;;   (if (or (eq this-command 'eval-expression)
;;           (eq this-command 'async-shell-command)
;;           (eq this-command 'shell-command))
;;           ;; (eq this-command 'evil-ex))
;;       (completion-preview-mode 1)
;;     (completion-preview-mode -1)))

;; (add-hook 'minibuffer-setup-hook #'my-conditional-completion-preview)


(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2))
  
(minibuffer-regexp-mode 1)

(setq ielm-history-file-name "~/.emacs.d/.ielm_history")

(delete-selection-mode 1)


;; Cursor

(blink-cursor-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)


;; Undo tree

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))


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

;; For some reason this code doesn't allow to handle the case with backtick + single quote.
;; (defun fix-backtick-quotes-in-string-face ()
;;   "Force string face for backtick-quoted text inside strings."
;;   (font-lock-add-keywords
;;    nil
;;    `((,(lambda (limit)
;;          (let (found-pos)
;;            (message "Searching for backtick-quotes up to position: %s" limit)
;;            (while (and (not found-pos)
;;                       (re-search-forward "`'[^'`\n]*'`" limit t))
;;              (let* ((quote-start (match-beginning 0))
;;                     (quote-end (match-end 0))
;;                     (matched-text (buffer-substring-no-properties quote-start quote-end))
;;                     (in-double (in-double-quotes-p quote-start)))
;;                (message "Found match: '%s' at %s-%s (in-double: %s)" 
;;                        matched-text quote-start quote-end in-double)
;;                (when in-double
;;                  (setq found-pos (point))
;;                  (message "Applying face to: %s" matched-text)
;;                  (remove-text-properties quote-start quote-end '(face nil))
;;                  (put-text-property quote-start quote-end 'face 'font-lock-string-face))))
;;            found-pos))
;;       (0 'font-lock-string-face keep)))
;;    t))

;; (add-hook 'emacs-lisp-mode-hook 'fix-backtick-quotes-in-string-face)

(add-hook 'emacs-lisp-mode-hook 'fix-quotes-in-string-face)
(add-hook 'emacs-lisp-mode-hook 'force-comment-face) ;; aorisetn 'aorisetn'
(add-hook 'lisp-mode-hook 'force-comment-face)


;; ;; Icons

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda ()
                        (when (not (file-remote-p default-directory))
                          (all-the-icons-dired-mode t)))))


;; Highlighting

(add-to-list 'auto-mode-alist '("sshd_config\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("ssh_config\\'" . conf-mode))


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


;; Avy

(use-package avy
  :ensure t
  )


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


;; Commander

(use-package commander
  :ensure t)


;; Xterm-color

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; ;; Disabled it because it renders incorrect output in esheell when i do this:
;; ;; (setq mylist '(1 2 3))
;; ;; (1 2 3)
;; ;; echo 0 $mylist
;; ;; (0 "")

;; (use-package xterm-color
;;   :ensure t)

;; ;; ;; Breaks rendering inside docker shells entered using 'shell-command'.
;; ;; ;; (setq comint-output-filter-functions
;; ;; ;;       (remove 'ansi-color-process-output comint-output-filter-functions))

;; ;; ;; (defun my/setup-docker-buffer ()
;; ;; ;;   "Set up a buffer for Docker output."
;; ;; ;;   (setq-local ansi-color-for-comint-mode t)
;; ;; ;;   (setq-local xterm-color-preserve-properties t)
;; ;; ;;   (font-lock-mode -1)  ; Disable font-lock to improve performance
;; ;; ;;   )

;; ;; ;; (add-hook 'docker-container-logs-mode-hook #'my/setup-docker-buffer)

;; ;; (add-hook 'shell-mode-hook
;; ;;           (lambda ()
;; ;;             ;; Disable font-locking in this buffer to improve performance
;; ;;             (font-lock-mode -1)
;; ;;             ;; Prevent font-locking from being re-enabled in this buffer
;; ;;             (make-local-variable 'font-lock-function)
;; ;;             (setq font-lock-function (lambda (_) nil))
;; ;;             (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; ;; Compilation buffers
;; (setq compilation-environment '("TERM=xterm-256color"))

;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))

;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             ;; Disable font-locking in this buffer to improve performance
;;             (font-lock-mode -1)
;;             ;; Prevent font-locking from being re-enabled in this buffer
;;             (make-local-variable 'font-lock-function)
;;             (setq font-lock-function (lambda (_) nil))
;;             (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


;; Completion
;; Snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (yas-reload-all))


;; Orderless

(use-package orderless
  :ensure t
  :init
  ;; ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
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
        (list #'cape-file))

  ;; For ALL buffers except elisp and eshell
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (unless (or (derived-mode-p 'emacs-lisp-mode)
                         (derived-mode-p 'eshell-mode))
                (setq-local completion-at-point-functions
                            (list #'cape-file
                                  #'my/dabbrev-capf)))))

  ;; For Elisp modes
  (dolist (mode '(emacs-lisp-mode
                 ielm-mode
                 lisp-interaction-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (setq-local completion-at-point-functions
                            (list #'elisp-completion-at-point
                                  #'cape-file)))))
  
  ;; Special case for eval-expression-minibuffer
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'elisp-completion-at-point
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


;; Sudo Edit

(use-package sudo-edit
  :ensure t)


;; Zoxide

(use-package zoxide
  :vc (:url "https://gitlab.com/Vonfry/zoxide.el"
       :rev :newest))


;; Project/Projectile

(require 'project)

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
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input))

(defun my/consult-line-with-evil ()
  "Run consult-line and set up evil search pattern."
  (interactive)
  (let ((selected (consult-line)))
    (when-let* ((search-string (car consult--line-history)))
      (message "Search string: %S" search-string)
      (let ((search-string-prop 
             (propertize search-string 
                        'isearch-case-fold-search t)))
        (push search-string-prop regexp-search-ring)
        ;; Set search direction to forward
        (setq isearch-forward t)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t)

(use-package embark-consult
  :ensure t)

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

(defun my-eshell-history-choose ()
  "Select an item from eshell history using Vertico and insert it into the eshell prompt."
  (interactive)
  (let* ((history (ring-elements eshell-history-ring))
         (history (delete-dups history))
         (command (consult--read history
                                 :prompt "Eshell history: "
                                 :sort nil
                                 :require-match t)))
    (when command
      (eshell-kill-input)
      (insert command))))


;; ;; Ivy


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

(defun my-enlarge-window-horizontally ()
  "Enlarge the current window horizontally in a more intuitive way."
  (interactive)
  (if (window-at-side-p (selected-window) 'right)
      (shrink-window-horizontally 5)
    (enlarge-window-horizontally 5)))

(defun my-shrink-window-horizontally ()
  "Shrink the current window horizontally in a more intuitive way."
  (interactive)
  (if (window-at-side-p (selected-window) 'right)
      (enlarge-window-horizontally 5)
    (shrink-window-horizontally 5)))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-size (:color red)
    "window size"
    ("h" my-shrink-window-horizontally "shrink horizontally")
    ("l" my-enlarge-window-horizontally "enlarge horizontally")
    ("k" (lambda () (interactive) (shrink-window 3)) "shrink vertically")
    ("j" (lambda () (interactive) (enlarge-window 3)) "enlarge vertically")
    ("t" transpose-frame "transpose windows")
    ("q" nil "quit")))


;; Dired

;; Basic trash settings
(setq delete-by-moving-to-trash t)

;; Configure connection-local variables for sudo operations
(connection-local-set-profile-variables
 'remote-trash-directory
 '((trash-directory . "/sudo::~/.local/share/trash/")))

(connection-local-set-profiles
 `(:application tramp :protocol "sudo" :machine ,(system-name))
 'remote-trash-directory)

;; Auto-revert for sudo files
(add-to-list 'auto-revert-remote-files "/sudo:root@localhost:/")

(setq wdired-allow-to-create-files t)
;; (setq wdired-allow-to-change-permissions t)

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

(defun OpenDiredBufferInCurrentWindow ()
  (interactive)
  (let ((current-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (dired current-dir)))


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


;; Tramp

;; (require 'tramp)

;; ;; (setq tramp-direct-async-process t)


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

;; (setq desktop-restore-eager 10)

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

(add-hook 'kill-emacs-hook 'clean-buffer-list)
(add-hook 'kill-emacs-hook 'save-current-desktop-session)


;; Buffers
;; Spawn and Pointer

(add-to-list 'display-buffer-alist
             '("*Faces*" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("*info*" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("*helpful*" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; (add-to-list 'display-buffer-alist
;;              '("^\\*\\(Man\\|Faces\\) "
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (post-command-select-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Man "
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (post-command-select-window . t)))

;; (add-to-list 'display-buffer-alist
;;              '("*Faces*"
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (post-command-select-window . t)))


;; Shell buffer

(defun my-shell-mode-hook ()
  (setq-local scroll-margin 0))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setq explicit-shell-file-name "/usr/bin/zsh")  ; your shell path here
(setq explicit-bash-args '("--login" "-i"))

(defun my-shell-mode-hook ()
  "Custom shell-mode hook to remove the first line of output."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (delete-line))))

(add-hook 'shell-mode-hook
          (lambda ()
            (run-with-timer 0.1 nil 'my-shell-mode-hook)))


;; Eshell buffer

(defvar eshell-buffer-name "*eshell*"
  "The name of the eshell buffer.")

(defvar eshell-toggle-window-configuration nil
  "Variable to store the window configuration before opening eshell.")

(defvar eshell-toggle-selected-window nil
  "Variable to store the selected window before opening eshell.")

(defun SpawnEshellSplitBelow ()
  "Open a shell in a small split below or toggle it if already open."
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
      (open-eshell-in-current-directory))))

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

;; Don't set on 'M-p'
;; (with-eval-after-load 'evil
;;   (define-key evil-normal-state-map (kbd "M-p") 'SpawnEshellInProjectRoot))

(defun kill-all-eshell-buffers ()
  "Kill all Eshell buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*eshell\\*" (buffer-name buffer))
      (kill-buffer buffer))))


(defvar my-saved-tab-configurations (make-hash-table :test 'equal)
  "Hash table to store the saved window configurations per tab name.")

(defvar my-fullscreen-eshell-active nil
  "Flag to indicate if we're currently in a fullscreen Eshell.")

(defun my-current-tab-name ()
  "Get the current tab's name."
  (alist-get 'name (tab-bar--current-tab)))

(defun my-eshell-fullscreen ()
  "Replace the current window layout with a fullscreen Eshell for the current tab."
  (interactive)
  (let* ((tab-name (my-current-tab-name))
         (eshell-buffer-name (format "*eshell<%s>*" tab-name)))
    (if tab-name
        (if my-fullscreen-eshell-active
            (message "Already in fullscreen Eshell. Use M-j to restore previous layout.")
          (progn
            (puthash tab-name (current-window-configuration) my-saved-tab-configurations)
            ;; (message "Saved window configuration for tab: %s" tab-name)
            (delete-other-windows)
            ;; Switch to or create the Eshell buffer
            (if (get-buffer eshell-buffer-name)
                (switch-to-buffer eshell-buffer-name)
              (progn
                (switch-to-buffer (get-buffer-create eshell-buffer-name))
                (eshell)))
            (setq my-fullscreen-eshell-active t)))
      (message "Failed to get tab name. Is the tab-bar-mode enabled?"))))

(defun my-restore-window-configuration ()
  "Restore the previously saved window configuration for the current tab."
  (interactive)
  (let* ((tab-name (my-current-tab-name))
         (config (gethash tab-name my-saved-tab-configurations)))
    (if config
        (progn
          (set-window-configuration config)
          (setq my-fullscreen-eshell-active nil))
          ;; (message "Restored window configuration for tab: %s" tab-name))
      (message "No saved window configuration for this tab: %s" tab-name))))

(global-set-key (kbd "M-k") 'my-eshell-fullscreen)
(global-set-key (kbd "M-j") 'my-restore-window-configuration)

(defun display-current-tab-name ()
  "Display the name of the current tab in tab-bar-mode."
  (interactive)
  (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
    (if tab-name
        (message "Current tab name: %s" tab-name)
      (message "Current tab has no name"))))


;; Transpose frame

(use-package transpose-frame
  :ensure t)


;; Popper

(defun my/show-popper-echo-line ()
  "Briefly toggle popper to show the echo line."
  (interactive)
  ;; Ensure popper-mode and popper-echo-mode are active
  (when (and popper-mode popper-echo-mode)
    ;; Toggle a popper window and immediately toggle it back
    (popper-toggle-latest)
    (popper-toggle-latest)))

(use-package popper
  :ensure t
  :bind (("M-`" . my/show-popper-echo-line))
         ;; ("M-f"   . popper-toggle))
         ;; ("M-~"   . popper-cycle))
  :init
  (setq popper-window-height 0.33)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          ;; "Output\\*$"
          ;; "\\*Async Shell Command\\*"
          "*Flymake diagnostics.*"
          ;; "*Flycheck errors.*"
          ;; "*Python.*"
          ;; "\\* docker container logs .*\\*"
          ;; "\\* docker inspect .*\\*"
          ;; "\\*daemons-output for .*\\*"
          ;; "\\*kubernetes logs.*\\*"
          ;; "\\*compilation\\*"
          ;; "\\*eshell\\*.*"
          ;; "\\*persistent-shell\\*.*"
          "\\*cider-repl.*"
          "\\*cider-doc.*"
          "\\*cider-error.*"
          ;; "\\*helpful.*"
          ;; "\\*man.*"
          ;; "\\*grep.*"
          ;; "\\*eshell:.*"
          ;; "\\*Warnings\\*"
          ;; "\\*xref\\*"
          ;; "\\*Backtrace\\*"
          ;; "\\*eldoc\\*"
          ;; "\\*Ement Notifications\\*"
          ;; "Output\\*$"
          ;; "\\*Dtache Shell Command\\*"
          ;; "\\*mu4e-update\\*"
          ;; help-mode
          ;; compilation-mode
          ))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1))

(defun my-buffer-is-popper-popup-p ()
  "Check if the current buffer is considered a Popper popup."
  (and (boundp 'popper-popup-status)
      (buffer-local-value 'popper-popup-status (current-buffer))))

(defun my-check-current-buffer-popper-status ()
  "Print whether the current buffer is a Popper popup."
  (interactive)
  (if (my-buffer-is-popper-popup-p)
      (message "Current buffer IS a Popper popup.")
    (message "Current buffer is NOT a Popper popup.")))

;; (define-advice popper-raise-popup (:override (&optional buffer) switch-and-stay)
;;   (when-let* ((buf (get-buffer (or buffer (current-buffer)))))
;;     (with-current-buffer buf
;;       (if (popper-popup-p buf)
;;           (setq popper-popup-status 'raised)
;;         (setq popper-popup-status nil))
;;       (setq mode-line-format (default-value 'mode-line-format)))))

(defadvice popper-raise-popup (around switch-and-stay (&optional buffer) activate)
  "Advice to modify popper-raise-popup behavior."
  (when-let* ((buf (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer buf
      (if (popper-popup-p buf)
          (setq popper-popup-status 'raised)
        (setq popper-popup-status nil))
      (setq mode-line-format (default-value 'mode-line-format)))))

(defun popper-toggle-type-original ()
  "Run popper-toggle-type with the original behavior by temporarily disabling the advice."
  (interactive)
  (ad-disable-advice 'popper-raise-popup 'around 'switch-and-stay)
  (ad-activate 'popper-raise-popup)
  (unwind-protect
      (call-interactively 'popper-toggle-type)
    (ad-enable-advice 'popper-raise-popup 'around 'switch-and-stay)
    (ad-activate 'popper-raise-popup)))

;; (global-set-key (kbd "M-f") 'm-f-toggle-or-forward-word)
(global-set-key (kbd "M-f") 'popper-toggle)

(defun popper-flymake-diagnostics ()
  "Popper window specifically for Flymake diagnostics buffer."
  (interactive)
  (if (string-match-p "\\*.*Flymake diagnostics.*\\*" (buffer-name))
      (popper-toggle)
    (flymake-show-buffer-diagnostics)))

(defun popper-flycheck-diagnostics ()
  "Popper window specifically for Flycheck errors buffer."
  (interactive)
  (if (string-match-p "\\*Flycheck errors\\*" (buffer-name))
      (popper-toggle)
    (flycheck-list-errors)))

;; (defun fix-cycle ()
;;   (interactive)
;;   (popper-cycle 1))

;; (defun fix-cycle-backwards ()
;;   (interactive)
;;   (popper-cycle-backwards -1))


;; Language Support
;; Pyvenv

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode 1))
  
;; (pyvenv-activate "/home/wurfkreuz/.projects/python-server/server-python/.venv")

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; Flymake

;; (use-package flymake
;;   :ensure t
;;   :config
;;   ;; Define a function to enable flymake-mode in dockerfile-mode
;;   (defun enable-flymake-mode ()
;;     "Enable flymake-mode in dockerfile-mode."
;;     (if (string-equal major-mode "dockerfile-mode")
;;         (flymake-mode 1)))

;; (setq flymake-show-diagnostics-at-end-of-line t)

(defun enable-flymake-mode ()
  "Enable flymake-mode in dockerfile-mode."
  (if (string-equal major-mode "dockerfile-mode")
      (flymake-mode 1)))

;;   ;; Add the hook to enable flymake-mode when entering dockerfile-mode
(add-hook 'dockerfile-mode-hook 'enable-flymake-mode)

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


;; Flycheck

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
(add-hook 'yaml-ts-mode-hook #'eglot-ensure)
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
  (envrc-global-mode)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (message "Eglot started with env: VIRTUAL_ENV=%s" (getenv "VIRTUAL_ENV"))
              (message "Python path: %s" (executable-find "python")))))


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

(setq org-edit-src-content-indentation 0)

;; (use-package org
;;   :config
;;   (setq org-startup-with-inline-images t)
;;   (advice-add 'yank-media :after
;;             (lambda (&rest _)
;;               (when (eq major-mode 'org-mode)
;;                 (org-display-inline-images)))))


;;   ;; Where to save images if using 'save method
;;   (setq org-yank-image-dir "/home/wurfkreuz/.secret_dotfiles/org/images/")

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

(defun install-org-from-source ()
  "Install org-mode from source."
  (interactive)
  (let* ((current-org-path (locate-library "org"))
         (emacs-path (expand-file-name invocation-name invocation-directory))
         (source-dir (expand-file-name "~/.source")))
    
    (message "Using Emacs from: %s" emacs-path)
    
    (unless (file-exists-p source-dir)
      (if (y-or-n-p "~/.source directory doesn't exist. Create it? ")
          (make-directory source-dir t)
        (error "Aborted: source directory is required")))
    
    (let ((org-dir (expand-file-name "org-mode" source-dir)))
      (if (file-exists-p org-dir)
          (when (y-or-n-p "org-mode directory exists. Update it? ")
            (async-shell-command (format "cd %s && git pull" org-dir) "*org-update*"))
        (when (y-or-n-p "Clone org-mode repository? ")
          (async-shell-command 
           (format "git clone https://git.savannah.gnu.org/git/emacs/org-mode.git %s" org-dir)
           "*org-clone*")))
      
      (when (and (file-exists-p org-dir)
                (y-or-n-p "Compile org-mode? "))
        (let ((process (start-process-shell-command 
                       "org-compile" "*org-compile*"
                       (format "cd %s && make clean && make" org-dir))))
          (set-process-sentinel 
           process
           (lambda (proc event)
             (when (string= event "finished\n")
               (let ((load-path-line (format "(add-to-list 'load-path \"%s/lisp\")" org-dir)))
                 (message "Compilation finished. Add this line to your init.el to use the new org-mode:\n%s" 
                         load-path-line))))))))))


;; Org capture

;; Set the path to your Org notes file
(setq org-default-notes-file "/home/wurfkreuz/.secret_dotfiles/org/notes/quick_notes.org")

(setq org-capture-templates
      '(("n" "Note" plain (file org-default-notes-file)
        "%?\nEntered on %U\n" :append t :empty-lines-before 1)))


;; Org download

(use-package org-download
  :ensure t
  :init
  ;; (setq org-download-image-dir "/home/wurfkreuz/.local/share/images")
  (setq org-download-image-dir "/home/wurfkreuz/.secret_dotfiles/org/images/")
  :config
  (add-hook 'org-mode-hook 'org-download-enable)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-display-inline-images))))


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


;; Org Agenda

(setq org-agenda-files
      '("~/.secret_dotfiles/org/todo_list.org"))


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

(add-hook 'org-mode-hook 'prettify-symbols-mode)
  (defun my-org-prettify-symbols ()
  (push '("#+begin_src" . ">") prettify-symbols-alist)
    (push '("#+end_src" . ">") prettify-symbols-alist))

(eval-after-load 'org
  '(add-hook 'org-mode-hook 'my-org-prettify-symbols))


;; Bullets

(use-package org-bullets
  :ensure t)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; Enabling Table of Contents

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))


;; Keybindings

(defun my-bind-keys (keymap-prefix bindings)
  "Bind keys in KEYMAP-PREFIX.
BINDINGS is an alist of (KEY . COMMAND) pairs."
  (dolist (binding bindings)
    (global-set-key (kbd (concat keymap-prefix (car binding))) (cdr binding))))

(my-bind-keys "C-c "
  '(
    ("ff" . project-find-file)
    ("fd" . project-find-dir)
    ("fb" . ido-switch-buffer)
    ;; ("ff" . ivy-fzf-project)
    ;; ("fh" . ivy-fzf-home)
    ;; ("fc" . ivy-fzf-current-directory)
    ;; ("fr" . ivy-fzf-root)
    ("fr" . consult-recent-file)
    ("fs" . consult-ripgrep)

    ("ss" . save-current-desktop-session)
    ("sd" . delete-desktop-session)
    ("sl" . load-desktop-with-name)
    ("sr" . rename-desktop-session)

    ("k"  . kill-buffer)
    ("w"  . write-file)
    ("bc" . ido-kill-buffer)
    ("bx" . kill-current-buffer)

    ("tn" . tab-bar-new-tab)
    ("tx" . tab-bar-close-tab)
    ("tr" . tab-bar-rename-tab)

    ("D"  . docker-template)

    ("do" . daemons-stop)
    ("ds" . daemons-start)
    ("de" . daemons-enable)
    ("dd" . daemons-disable)
    ("du" . daemons-status)
    ("dr" . daemons-restart)

    ("w"  . hydra-window-size/body)

    ("pt" . popper-toggle-type)
    ("pe" . popper-toggle-type-original)
    ("pr" . my-remove-popper-status-from-frame-buffers)

    ("rr" . my-refresh-command)

    ("er" . eval-region)

    ("E"  . eshell)
    ("en" . eshell-new)
    ("ep" . eshell-pop) 

    ("gm" . pop-global-mark) 

    ("fe" . OpenDiredBufferInCurrentWindow)

    ("xx" . add-execute-permissions-to-current-file)
    ("xr" . add-write-permissions-to-current-file)

    ("mm" . messages)

    ("gbs" . my-vc-switch-branch)
    ("gbc" . vc-create-branch)

    ))

;; (global-unset-key (kbd "M-;"))

(defun my-noop ()
"A no-op function that does nothing."
(interactive))

(global-set-key (kbd "M-;") 'my-noop)

(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "M-TAB"))
(global-set-key (kbd "C-s C-l") 'load-desktop-with-name)
(global-set-key (kbd "C-s C-s") 'my/consult-line-with-evil)
;; (global-set-key (kbd "C-s C-s") 'consult-line)
(global-set-key (kbd "C-s C-q") 'my-sql-connect-with-buffer)
(global-set-key (kbd "C-s C-b") 'sql-send-buffer)
;; (global-set-key (kbd "C-S C-k") 'kill-whole-line)
(define-key minibuffer-local-map (kbd "C-S C-k") 'backward-kill-sentence)  ; Example function
;; (define-key minibuffer-local-map (kbd "C-S C-k") 'kill-whole-line)
(global-set-key (kbd "C-h M-f") 'describe-face)

;; (global-unset-key (kbd "C-t"))
;; (global-unset-key (kbd "C-y"))

(defun my-yas-complete-or-expand ()
  "Expand Yasnippet if possible, otherwise trigger completion-at-point."
  (interactive)
  (if (and (bound-and-true-p yas-minor-mode)
           (fboundp 'yas-expand) 
          (yas-expand))
      t  ; Yasnippet expanded successfully
    (vertico-insert)
    (completion-at-point)))

(defun my-org-cycle-or-preview ()
  "Cycle in Org mode or show the next completion preview candidate."
  (interactive)
  (yas-expand)
  (org-cycle))

(defun my-smart-tab ()
  "Custom tab behavior based on context."
  (interactive)
  (cond
   ;; In Vertico buffers, use the default Vertico behavior
   ((and (boundp 'vertico-map)
         (eq (current-local-map) vertico-map))
    (call-interactively (lookup-key vertico-map (kbd "TAB"))))

   ;; Check if Corfu is available and enabled
   ((and (featurep 'corfu) corfu-mode)
    (if (and (boundp 'corfu--frame) (frame-live-p corfu--frame))
        ;; If Corfu popup is already active, insert the candidate
        (corfu-insert)
      ;; Otherwise, trigger completion-at-point, which Corfu will handle
      (completion-at-point)))

   ;; In Eshell, minibuffers, or any other context, use completion-at-point
   (t
    (completion-at-point))))

(global-unset-key (kbd "C-<tab>"))
(global-set-key (kbd "<C-tab>") 'previous-buffer)

(defun my-refresh-command ()
  "Choose the appropriate refresh command based on the major mode."
  (interactive)
  (if (eq major-mode 'kubernetes-overview-mode)
      (kubernetes-refresh)
    (revert-buffer)))

(defun my-disable-magit-keybindings ()
  "Disable specific keybindings in Magit."
  (define-key magit-mode-map (kbd "M-1") nil) ; Disable M-1
  (define-key magit-mode-map (kbd "M-2") nil) ; Disable M-2
  ;; Add more keybindings as needed
  )

(define-prefix-command 'my-window-map)
(global-set-key (kbd "C-w") 'my-window-map)

(global-set-key (kbd "C-w C-l") 'windmove-right)
(global-set-key (kbd "C-w C-h") 'windmove-left)
(global-set-key (kbd "C-w C-k") 'windmove-up)
(global-set-key (kbd "C-w C-j") 'windmove-down)

(global-set-key (kbd "C-w C-s") 'split-window-below) 
(global-set-key (kbd "C-w C-v") 'split-window-right)  
(global-set-key (kbd "C-w C-c") 'delete-window)        


;; Custom functions

(defun org-insert-row-with-floor ()
  "Insert a new row with a 'floor' above in an Org mode table."
  (interactive)
  (org-table-next-field)
  (beginning-of-line)
  (insert "|-")
  (org-table-align)
  (org-return))

;; (define-key org-mode-map (kbd "C-c f") 'org-insert-row-with-floor)

(defun FormatToThreshold (char-threshold)
  "Formats the selected text to not exceed CHAR-THRESHOLD characters per line."
  (interactive "nCharacter Threshold: ")
  (let ((start (region-beginning))
        (end (region-end))
        all-text words formatted-text)
    (save-excursion
      (setq all-text (buffer-substring start end))
      (setq words (split-string all-text))
      (let ((current-line "")
            (current-length 0))
        (dolist (word words)
          (if (> (+ current-length (length word) 1) char-threshold)
              (progn
                (setq formatted-text (concat formatted-text current-line "\n"))
                (setq current-line word)
                (setq current-length (length word)))
            (progn
              (setq current-line (if (string= "" current-line)
                                     word
                                   (concat current-line " " word)))
              (setq current-length (+ current-length (length word) 1)))))
        (setq formatted-text (concat formatted-text current-line)))
      (delete-region start end)
      (goto-char start)
      (insert formatted-text))))

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

(defun my-eshell-current-input ()
  (interactive)
  "Return the current characters written before the cursor in Eshell and print it."
  (let ((input (if (eq major-mode 'eshell-mode)
                   (buffer-substring-no-properties (line-beginning-position) (point))
                 "")))
    (message "Current input: %s" input)
    input))  ; Still return the input for programmatic use if needed

(defun my-completion-preview-insert ()
  "Completes the previewed suggestion and deletes the trailing whitespace."
  (interactive)
  (completion-preview-insert)
  (delete-backward-char 1))

(defun scroll-up-and-recenter (arg)
  "Scroll up ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-up-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-down-and-recenter (arg)
  "Scroll down ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-down-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position


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
  "Copy the full path of the current buffer's file to the clipboard (or appropriate path)."
  (interactive)
  (let ((path-to-copy nil))
    (cond
     ((eq major-mode 'dired-mode)    ; Dired buffer
      (setq path-to-copy (if (dired-get-file-for-visit)
                             (expand-file-name (dired-get-file-for-visit))
                           (expand-file-name default-directory))))
     ((eq major-mode 'eshell-mode)   ; Eshell buffer
      (setq path-to-copy (eshell/pwd)))
     (t                              ; Default: Regular File buffer
      (setq path-to-copy (buffer-file-name))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message "Copied path '%s' to the clipboard." path-to-copy))
      (message "Current buffer has no associated path to copy.")))) ;

(defun z (q)
  "Query zoxide and launch dired or change directory in Eshell."
  (interactive "sZoxide: ")
  (if-let
      ((zoxide (executable-find "zoxide"))
       (target
        (with-temp-buffer
          (if (= 0 (call-process zoxide nil t nil "query" q))
              (string-trim (buffer-string))))))
      (if (derived-mode-p 'eshell-mode)
          (eshell/cd target)
        (funcall-interactively #'dired target))
    (unless zoxide (error "Install zoxide"))
    (unless target (error "No Match"))))

;; Creats errors sometimes in emacs launch for some reason
;;(defun push ()
;;  "Execute git add, commit, and push in sequence asynchronously."
;;  (interactive)
;;  ;; Execute 'push' asynchronously and display output in a separate buffer.
;;  (async-shell-command "push"))

(defun push-all ()
  "Execute git add, commit, and push in sequence asynchronously."
  (interactive)
  ;; Execute 'push' asynchronously and display output in a separate buffer.
  (async-shell-command "push -a"))

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
  (find-file "~/.local/share/trash"))

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

(defun crontab-edit ()
  "Run `crontab -e' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun sway ()
  "Open sway config file."
  (interactive)
  (find-file (expand-file-name "~/.dotfiles/sway")))

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

(defun alc ()
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
  "Switch to the *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun config ()
  "Open a specific file."
  (interactive)
  (find-file "~/.config"))

(defun evil ()
  "Open a specific file."
  (interactive)
  (find-file "~/.emacs.d/evil.el"))

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
