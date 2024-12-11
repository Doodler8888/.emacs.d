;; -*- lexical-binding: t -*-

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-integration t
        evil-want-minibuffer t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'custom-theme-choose-mode 'normal)
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-normal-state-map (kbd "C-f") 'scroll-up-and-recenter)
  (define-key evil-normal-state-map (kbd "C-b") 'scroll-down-and-recenter)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-half-scroll-up-and-recenter)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-half-scroll-down-and-recenter)
  (define-key evil-normal-state-map (kbd "l") 'forward-char)
  (define-key evil-normal-state-map (kbd "h") 'backward-char)
  (define-key evil-normal-state-map (kbd "j") 'next-line)
  (define-key evil-normal-state-map (kbd "k") 'previous-line)
  (define-key evil-normal-state-map (kbd "M-f") 'toggle-messages-buffer)
  (define-key evil-normal-state-map (kbd "M-d") 'toggle-docker-layout)
  (setq evil-shift-width 2))

;; (add-hook 'eshell-mode-hook (lambda () (undo-tree-mode 1)))
;; (add-hook 'wdired-mode-hook (lambda () (undo-tree-mode 1)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; Add custom surround pairs
  (setq-default evil-surround-pairs-alist
                (append evil-surround-pairs-alist
                        '((?/ . ("/" . "/"))
                          (?~ . ("~" . "~"))
                          (?* . ("*" . "*"))
                          (?= . ("=" . "="))
                          (?+ . ("+" . "+"))))))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired wdired ibuffer org term ansi
                                   lsp-ui-imenu elpaca minibuffer ivy proced
                                   docker magit package-menu debug))
  
  ;; Force normal state for package-menu-mode
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'debugger-mode 'normal)
  
  (evil-collection-init))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inside defun."
  :type line
  (when (derived-mode-p 'emacs-lisp-mode 
                       'lisp-mode 
                       'scheme-mode 
                       'clojure-mode
                       'lisp-interaction-mode)
    (evil-select-inner-object 'defun beg end type count)))

(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  :type line
  (when (derived-mode-p 'emacs-lisp-mode 
                       'lisp-mode 
                       'scheme-mode 
                       'clojure-mode
                       'lisp-interaction-mode)
    (evil-select-an-object 'defun beg end type count)))

(define-key evil-outer-text-objects-map "d" 'evil-a-defun)
(define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

(defun my-evil-yank-to-end-of-line ()
  "Yank text from the current point to the end of the line."
  (interactive)
  (evil-yank (point) (line-end-position)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "Y") 'my-evil-yank-to-end-of-line))

;; (defun my-evil-insert-state-minibuffer-setup ()
;;   (define-key evil-insert-state-local-map (kbd "<backspace>") 'ivy-backward-delete-char)
;;   (define-key evil-insert-state-local-map (kbd "TAB") 'ivy-partial-or-done))

;; (add-hook 'minibuffer-setup-hook 'my-evil-insert-state-minibuffer-setup)

(evil-global-set-key 'insert (kbd "C-l") 'forward-char)
(evil-global-set-key 'insert (kbd "C-h") 'backward-char)

(with-eval-after-load 'evil
  (define-key evil-ex-completion-map (kbd "<insert-state> C-n") nil))

(with-eval-after-load 'evil
  (define-key evil-ex-completion-map (kbd "<insert-state> C-p") nil))

(with-eval-after-load 'evil
  (define-key evil-ex-completion-map (kbd "<insert-state> <up>") 'previous-complete-history-element)
  (define-key evil-ex-completion-map (kbd "<insert-state> <down>") 'next-complete-history-element))

(defun my/setup-daemons-output-keymap ()
  "Set up custom keybindings for daemons-output-mode."
  (evil-local-set-key 'normal (kbd "RET") 'daemons-status-at-point)
  (evil-local-set-key 'motion (kbd "RET") 'daemons-status-at-point))

(add-hook 'daemons-output-mode-hook 'my/setup-daemons-output-keymap)

(defun my/setup-docker-mark-keymap ()
  "Set up custom keybindings for daemons-output-mode."
  (evil-local-set-key 'normal (kbd "m") 'tablist-mark-forward)
  (evil-local-set-key 'normal (kbd "M") 'tablist-mark-backward))

(add-hook 'docker-container-mode-hook 'my/setup-docker-mark-keymap)
(add-hook 'docker-image-mode-hook 'my/setup-docker-mark-keymap)

(defun my-comment-on-region (beg end)
  "Comment or uncomment the region between BEG and END."
  (interactive "r")
  (comment-or-uncomment-region beg end))

(evil-define-operator my-evil-comment (beg end type)
  "Comment or uncomment the text from BEG to END."
  (interactive "<R>")
  (my-comment-on-region beg end))

;; Bind the custom comment operator to "gc"
(define-key evil-normal-state-map (kbd "gc") 'my-evil-comment)
(define-key evil-visual-state-map (kbd "gc") 'my-evil-comment)

(evil-define-operator my-evil-fill (beg end type)
  "Fill the text from BEG to END."
  (interactive "<R>")
  (my-fill-region beg end))

(defun my-evil-paste-before ()
  "Paste before cursor without overwriting kill ring."
  (interactive)
  (let ((text (current-kill 0 t)))
    (if (region-active-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (insert text))
      (progn
        (evil-insert-state)
        (insert text)
        (evil-normal-state)
        (backward-char)))))

(with-eval-after-load 'evil
  (define-key evil-visual-state-map "P" 'my-evil-paste-before))

;; Always use blank lines as paragraph delimiters in motions/text objects
(define-advice forward-evil-paragraph (:around (orig-fun &rest args))
  (let ((paragraph-start (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate))
        (paragraph-ignore-fill-prefix t))
    (apply orig-fun args)))

(evil-define-key 'normal minibuffer-local-map (kbd "M-p") 'my-previous-history-element)
(evil-define-key 'normal evil-command-line-map (kbd "M-p") 'my-previous-history-element)
(evil-define-key 'normal minibuffer-local-map (kbd "M-n") 'my-next-history-element)
(evil-define-key 'normal evil-command-line-map (kbd "M-n") 'my-next-history-element)

(evil-define-key 'insert minibuffer-local-map (kbd "M-p") 'my-previous-history-element)
(evil-define-key 'insert evil-command-line-map (kbd "M-p") 'my-previous-history-element)
(evil-define-key 'insert minibuffer-local-map (kbd "M-n") 'my-next-history-element)
(evil-define-key 'insert evil-command-line-map (kbd "M-n") 'my-next-history-element)


;; Custom keybindings

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-S-v") 'yank)
  (define-key evil-visual-state-map (kbd "{") 'evil-backward-paragraph)
  (define-key evil-visual-state-map (kbd "}") 'evil-forward-paragraph)
  (define-key evil-insert-state-map (kbd "M-w") 'evil-forward-word-begin)
  (define-key evil-insert-state-map (kbd "M-b") 'evil-backward-word-begin)
  (define-key evil-insert-state-map (kbd "M-W") 'evil-forward-WORD-begin)
  (define-key evil-insert-state-map (kbd "M-B") 'evil-backward-WORD-begin)

  (define-key evil-normal-state-map (kbd "gq") 'FormatToThreshold)
  (define-key evil-visual-state-map (kbd "gq") 'FormatToThreshold))

;; I use back-to-indentation instead now
;; (defun my-move-beginning-of-line ()
;;   "Move point to the first non-whitespace character of the line and enter insert mode."
;;   (interactive)
;;   (evil-first-non-blank)
;;   (evil-insert-state))

(defun my-move-end-of-line ()
  "Move point to the very end of the line and enter insert mode."
  (interactive)
  (evil-end-of-line)
  (evil-insert-state)
  (unless (eolp)
    (evil-append-line 1)))

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "M-i") 'back-to-indentation)
  (define-key evil-insert-state-map (kbd "M-a") 'my-move-end-of-line))


;; Package keybindings

;; Avy

(defun avy-goto-char-all-windows ()
  "Invoke `avy-goto-char` across all windows in the current frame, except in Dired buffers."
  (interactive)
  (let ((avy-all-windows t))
    (unless (derived-mode-p 'dired-mode)
      (call-interactively 'evil-avy-goto-char))))

(defun my/conditional-search-or-avy ()
  "Use `evil-search-forward` in Dired buffers, otherwise use `avy-goto-char-all-windows`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (evil-search-forward)
    (avy-goto-char-all-windows)))

(with-eval-after-load 'evil
  ;; First, ensure help-mode uses normal state instead of motion state
  (evil-set-initial-state 'help-mode 'normal)
  
  ;; Then set our keybindings
  (define-key evil-visual-state-map (kbd "/") 'my/conditional-search-or-avy)
  (define-key evil-normal-state-map (kbd "/") 'my/conditional-search-or-avy))


;; Docker

;; (defun docker-template ()
;;   "Create docker.el windows with a specific layout"
;;   (interactive)
;;   (delete-other-windows)
;;   (evil-window-split)
;;   (evil-window-split)
;;   (docker-volumes)
;;   (docker-containers)
;;   (docker-images)
;;   (delete-window (nth 1 (window-list)))
;;   (delete-window (nth 2 (window-list)))
;;   (delete-window (nth 3 (window-list)))
;;   )


;; Cider

(with-eval-after-load 'evil
;; CIDER
(with-eval-after-load 'cider
  ;; Define C-M-x for normal state to evaluate the top-level form around point (function)
  (evil-define-key 'normal cider-mode-map (kbd "C-M-x") 'cider-eval-defun-at-point)
  (evil-define-key 'normal cider-repl-mode-map (kbd "C-M-x") 'cider-eval-defun-at-point)
  ;; Define C-M-x for visual state to evaluate the selected region
  (evil-define-key 'visual cider-mode-map (kbd "C-M-x") 'cider-eval-region)
  (evil-define-key 'visual cider-repl-mode-map (kbd "C-M-x") 'cider-eval-region))

;; Emacs Lisp
(with-eval-after-load 'elisp-mode
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-M-x") 'eval-defun)
  (evil-define-key 'visual emacs-lisp-mode-map (kbd "C-M-x") 'eval-region))

;; Org Mode

(defun my/evil-smart-open-above ()
  "Open line above intelligently:
If on first item of list, use evil-open-above with number formatting.
If on other numbered items, go up and use org-meta-return.
If not on a numbered item, just use evil-open-above."
  (interactive)
  (if (and (org-in-item-p)
           (save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*[0-9]+\\."))) ; check if it's a numbered item
      (if (save-excursion
            (beginning-of-line)
            (looking-at "^[ \t]*1\\.")) ; if it's the first item
          (progn
            (evil-open-above 1)
            (insert "1. ")
            (org-list-repair))
        (progn
          (previous-line)
          (end-of-line)
          (org-meta-return)
          (evil-insert-state)))
    (evil-open-above 1)))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "C-M-x") 'org-babel-execute-src-block)
  (evil-define-key 'normal org-mode-map (kbd "O") 'my/evil-smart-open-above)
  
  ;; For visual state in org-mode, you might want to keep the default behavior
  ;; or define a custom function to evaluate a region if needed.
  ))

;; Vertico

(evil-define-key 'insert minibuffer-local-map (kbd "M-r") 'my-shell-command-history-and-insert)
(evil-define-key 'normal minibuffer-local-map (kbd "M-r") 'my-shell-command-history-and-insert)

(evil-define-key 'normal eshell-mode-map (kbd "M-r") 'my-eshell-history-choose)
(evil-define-key 'insert eshell-mode-map (kbd "M-r") 'my-eshell-history-choose)

;; Ivy/Counsel

(defun my/ivy-evil-delete-line ()
  "Delete the current line in Ivy minibuffer without affecting the newline."
  (interactive)
  (let ((inhibit-read-only t))
    (evil-delete-line (line-beginning-position) (line-end-position))))

(evil-define-key 'normal ivy-minibuffer-map (kbd "dd") 'my/ivy-evil-delete-line)

;; (with-eval-after-load 'evil
;;   (evil-define-key 'insert eshell-mode-map (kbd "M-r") 'counsel-esh-history)
;;   (evil-define-key 'normal eshell-mode-map (kbd "M-r") 'counsel-esh-history))


;; Dired

(setq evil-move-cursor-back nil)
(add-hook 'wdired-mode-hook #'evil-normal-state)

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "j" 'dired-next-line-preserve-column
    "T" 'my/dired-create-empty-files
    "k" 'dired-previous-line-preserve-column))

;; Eshell

(with-eval-after-load 'eshell
  (with-eval-after-load 'evil
    (evil-define-key 'insert eshell-mode-map (kbd "M-l") 'eshell-clear-buffer)
    (evil-define-key 'normal eshell-mode-map (kbd "C-l") 'eshell-clear-buffer)
    (evil-define-key 'normal eshell-mode-map (kbd "C-f C-p") 'eshell/insert-pwd-at-point)
    (evil-define-key 'insert eshell-mode-map (kbd "C-f C-p") 'eshell/insert-pwd-at-point)
    
    ;; Make these bindings higher priority
    (evil-local-set-key 'normal (kbd "M-,") 'eshell/insert-pwd-at-point)
    (evil-local-set-key 'insert (kbd "M-,") 'eshell/insert-pwd-at-point)))

(with-eval-after-load 'eshell
  (evil-define-key 'normal eshell-mode-map
    "j" 'next-line
    "k" 'previous-line)
  (evil-define-key 'visual eshell-mode-map
    "j" 'next-line
    "k" 'previous-line))

(defun my-eshell-evil-setup ()
  (evil-define-key 'normal eshell-mode-map (kbd "0") 'beginning-of-line))

(add-hook 'eshell-mode-hook 'my-eshell-evil-setup)

(add-hook 'eshell-mode-hook
          (lambda ()
            (evil-local-set-key 'insert (kbd "RET") 'eshell-send-input)
            (evil-local-set-key 'normal (kbd "RET") 'find-file-last-word-of-current-line)))

;; Eshell buffer

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-e") 'SpawnEshellSplitBelow)
  (define-key evil-insert-state-map (kbd "M-e") 'SpawnEshellSplitBelow))
;; (define-key evil-normal-state-map (kbd "M-e") 'open-eshell-in-current-directory))

;; (evil-define-key 'normal diff-mode-map (kbd "M-k") 'my-eshell-fullscreen)
(evil-define-key 'insert diff-mode-map (kbd "M-j") 'my-restore-window-configuration)

;; Popper

(with-eval-after-load 'evil
  ;;   (define-key evil-normal-state-map (kbd "M-k") 'fix-cycle-backwards)
  ;;   (define-key evil-normal-state-map (kbd "M-j") 'fix-cycle)
  (define-key evil-normal-state-map (kbd "M-y") 'toggle-flymake-diagnostics)
  (define-key evil-normal-state-map (kbd "M-t M-c") 'popper-flycheck-diagnostics))

;; Eglot

(define-key evil-normal-state-map (kbd "K") 'eldoc)

;; Custom functions

;; This function doesn't work in visual-line
(with-eval-after-load 'evil
  (dolist (state '(normal visual visual-line))
    (evil-define-key state org-mode-map (kbd "[[") 'my-org-beginning-of-block)
    (evil-define-key state org-mode-map (kbd "]]") 'my-org-end-of-block)))

;; Keybindings

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-t") nil)
  (evil-define-key 'normal 'global (kbd "M-^") 'projectile-run-async-shell-command-in-root)
  (define-key evil-normal-state-map (kbd "gz") 'zoxide-travel)
  (define-key evil-insert-state-map (kbd "C-f C-n") 'eshell-expand-filename-at-point))

(with-eval-after-load 'evil
  (evil-define-key 'insert global-map (kbd "C-a") 'my-org-cycle-or-preview))

;; Wrap edit

(evil-define-operator my-evil-fill (beg end type)
  "Fill the text from BEG to END."
  (interactive "<R>")
  (my-fill-region beg end))

(define-minor-mode wrap-edit-mode
  "Minor mode to conveniently edit long lines with automatic filling and unfilling."
  :init-value nil
  :lighter " Wrap-edit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'wrap-edit-exit)
            map)
  (if wrap-edit-mode
      (progn
        ;; Store the current region or line
        (setq wrap-edit-region
              (if (evil-visual-state-p)
                  (cons (region-beginning) (region-end))
                (cons (line-beginning-position) (line-end-position))))
        ;; Fill the current region or line
        (my-evil-fill (car wrap-edit-region) (cdr wrap-edit-region) 'line))
    ;; When the mode is deactivated, clear the stored region
    (setq wrap-edit-region nil)))

(define-key evil-normal-state-map (kbd "gw") 'my-evil-fill)
(define-key evil-visual-state-map (kbd "gw") 'my-evil-fill)

;; Custom commands

(with-eval-after-load 'evil
  (evil-ex-define-cmd "so" 'so))

(defun evil-half-scroll-up-and-recenter (arg)
  "Scroll up ARG lines and recenter."
  (interactive "P")
  (evil-scroll-up arg)
  (recenter))

(defun evil-half-scroll-down-and-recenter (arg)
  "Scroll down ARG lines and recenter."
  (interactive "P")
  (evil-scroll-down arg)
  (recenter))

;; Function to reuse
(defun my-space-as-ctrl-c ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events
        (listify-key-sequence (kbd "C-c"))))

;; Normal state binding
(define-key evil-normal-state-map (kbd "SPC") #'my-space-as-ctrl-c)

;; Motion state binding (for help-mode and similar)
(define-key evil-motion-state-map (kbd "SPC") #'my-space-as-ctrl-c)

;; Dired mode specific binding
(evil-define-key 'normal dired-mode-map (kbd "SPC") #'my-space-as-ctrl-c)

(with-eval-after-load 'evil
  ;; Pointer in shell-command-mode is spawned in the insert state for some reason
  (evil-set-initial-state 'shell-command-mode 'normal))

(with-eval-after-load 'evil
  ;; For help-mode
  (evil-define-key 'normal help-mode-map
    "q" #'quit-window)
  
  (evil-define-key 'normal Info-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal helpful-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal special-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal debugger-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal messages-buffer-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal compilation-mode-map
    "q" #'quit-window)

  (evil-define-key 'normal shell-command-mode-map
    "q" #'quit-window)

  (with-eval-after-load 'package
    (evil-define-key 'normal package-menu-mode-map
      "q" #'quit-window)))

(with-eval-after-load 'evil
  (evil-define-key 'insert prog-mode-map
    (kbd "TAB") #'completion-at-point))

(with-eval-after-load 'evil
  (evil-define-key 'insert org-mode-map
    (kbd "TAB") #'completion-at-point))

(with-eval-after-load 'evil
  (evil-define-key 'insert text-mode-map
    (kbd "TAB") #'completion-at-point))

(with-eval-after-load 'evil
  (evil-define-key 'normal prog-mode-map
    (kbd "M-.") #'xref-find-definitions))

;; (with-eval-after-load 'evil
;;   (evil-define-key 'insert minibuffer-mode-map
;;     (kbd "C-n") #'next-line
;;     (kbd "C-p") #'previous-line
;;     ))

(with-eval-after-load 'evil
  (evil-define-key 'insert global-map
    (kbd "M-`") (lambda () (interactive) (call-interactively #'tempel-next))
    (kbd "M-~") (lambda () (interactive) (call-interactively #'tempel-previous))))


;; The problem here is that even after enabling wgrep, i couldn't still use edit keys
(add-hook 'grep-mode-hook
          (lambda ()
            (if buffer-read-only
                (evil-normal-state)
              (evil-insert-state))))
