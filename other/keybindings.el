;; Keybindings

(defun my-bind-keys (keymap-prefix bindings)
  "Bind keys in KEYMAP-PREFIX.
BINDINGS is an alist of (KEY . COMMAND) pairs."
  (dolist (binding bindings)
    (global-set-key (kbd (concat keymap-prefix (car binding))) (cdr binding))))

(my-bind-keys "C-c "
  '(
    ;; ("ff" . project-find-file)
    
    ("ff" . project-find-file-all)
    ("fd" . project-find-dir)
    ("fb" . ido-switch-buffer)
    ("fy" . my/copy-kill-ring-to-clipboard)
    ;; ("ff" . ivy-fzf-project)
    ;; ("fh" . ivy-fzf-home)
    ;; ("fc" . ivy-fzf-current-directory)
    ;; ("fr" . ivy-fzf-root)
    ("fr" . consult-recent-file)
    ("fs" . consult-ripgrep)

    ("fe" . dired-jump)

    ("ss" . save-current-desktop-session)
    ("sd" . delete-desktop-session)
    ("sl" . load-desktop-with-name)
    ("sr" . rename-desktop-session)

    ("k"  . kill-buffer)
    ("w"  . write-file)
    ("bb" . ibuffer)
    ("bx" . my/kill-current-buffer)

    ("tn" . tab-bar-new-tab)
    ("tx" . tab-bar-close-tab)
    ("tr" . tab-bar-rename-tab)

    ;; ("D"  . docker-template)
    ;; ("d"  . toggle-docker-layout)

    ("di" . docker-images)
    ("dc" . docker-containers)
    ;; ("de" . daemons-enable)
    ;; ("dd" . daemons-disable)
    ;; ("du" . daemons-status)
    ;; ("dr" . daemons-restart)

    ("dd" . daemons)
    
    ("w"  . hydra-window-size/body)

    ("pt" . popper-toggle-type)
    ("pe" . popper-toggle-type-original)
    ("pr" . my-remove-popper-status-from-frame-buffers)

    ("rr" . revert-buffer)

    ("vr" . eval-region)
    ("vo" . eval-defun)

    ("j" . avy-jump-to-window)
    
    ("E"  . eshell)
    ("e" . open-eshell-in-current-directory)
    ;; ("ep" . eshell-pop) 

    ("m" . toggle-messages-buffer)

    ("cc" . Cp)

    ;; ("cn" . next-error)
    ;; ("cp" . previous-error)

    ("uu" . tramp-revert-buffer-with-sudo)
    ("ud" . tramp-revert-buffer-with-doas)
    ("ue" . my-tramp-cleanup)

    ("gm" . pop-global-mark) 

    ("xx" . add-execute-permissions-to-current-file)
    ("xr" . add-write-permissions-to-current-file)

    ("za" . my/zoxide-add)
    
    ;; ("mm" . messages)
    ;; ("mm" . toggle-messages-buffer)

    ("gbs" . my-vc-switch-branch)
    ("gbc" . vc-create-branch)

    ))

;; (global-unset-key (kbd "M-;"))

(defun my-noop ()
"A no-op function that does nothing."
(interactive))

(global-set-key (kbd "M-;") 'my-noop)

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s C-l") 'load-desktop-with-name)
;; (global-set-key (kbd "C-s C-s") 'my-occur-like)
(global-set-key (kbd "C-s C-s") 'consult-line)
(global-set-key (kbd "C-s C-w") 'consult-line-visible-region)
(global-set-key (kbd "C-s C-t") 'consult-line-visible-windows)
(global-set-key (kbd "C-s C-t") 'consult-line-multi)
(global-set-key (kbd "C-s C-q") 'my-sql-connect-with-buffer)
(global-set-key (kbd "C-s C-b") 'sql-send-buffer)
;; (global-set-key (kbd "C-S C-k") 'kill-whole-line)
(define-key minibuffer-local-map (kbd "C-S C-k") 'backward-kill-sentence)  ; Example function
;; (define-key minibuffer-local-map (kbd "C-S C-k") 'kill-whole-line)
(global-set-key (kbd "C-h M-f") 'describe-face)
(global-set-key (kbd "C-s C-y") 'yank-pop)
(global-set-key (kbd "M-<f1>") 'tab-bar-move-tab-backward)
(global-set-key (kbd "M-<f2>") 'tab-bar-move-tab)
(global-set-key (kbd "C-x s") (lambda () (interactive) (save-some-buffers t)))
(global-set-key (kbd "C-x 2") 'winner-undo)
(define-key occur-mode-map (kbd "C-x C-q") 'occur-edit-mode)
(define-key compilation-mode-map (kbd "/") 'my/conditional-search-or-avy)
(global-set-key (kbd "C-x C-o") 'so)
(define-key prog-mode-map (kbd "M-n") 'next-error)
(define-key prog-mode-map (kbd "M-p") 'previous-error)
(define-key prog-mode-map (kbd "C-x cc") 'compile-last)

;; Can't use it, because i have C-w binding for the insert mode
;; (defun my/setup-global-window-keys ()
;;   "Set up global window management keybindings."
;;   (define-prefix-command 'my-window-map)
;;   (global-set-key (kbd "C-w") 'my-window-map)

;;   (global-set-key (kbd "C-w C-l") 'windmove-right)
;;   (global-set-key (kbd "C-w C-h") 'windmove-left)
;;   (global-set-key (kbd "C-w C-k") 'windmove-up)
;;   (global-set-key (kbd "C-w C-j") 'windmove-down)

;;   (global-set-key (kbd "C-w C-s") 'split-window-below)
;;   (global-set-key (kbd "C-w C-v") 'split-window-right)
;;   (global-set-key (kbd "C-w C-c") 'delete-window)
;;   (global-set-key (kbd "C-w c") 'delete-window)
;;   (define-key meow-normal-state-keymap (kbd "C-w C-w") 'my-select-window-by-number))

;; ;; Call the setup function
;; (my/setup-global-window-keys)

(defun my/setup-window-keys (mode-map)
  "Set up window management keybindings for the given MODE-MAP."
  (define-prefix-command 'my-window-map)
  (define-key mode-map (kbd "C-w") 'my-window-map)

  ;; Move between windows
  (define-key mode-map (kbd "C-w C-l") 'windmove-right)
  (define-key mode-map (kbd "C-w C-h") 'windmove-left)
  (define-key mode-map (kbd "C-w C-k") 'windmove-up)
  (define-key mode-map (kbd "C-w C-j") 'windmove-down)

  ;; Split and delete windows
  (define-key mode-map (kbd "C-w C-s") 'split-window-below)
  (define-key mode-map (kbd "C-w C-v") 'split-window-right)
  (define-key mode-map (kbd "C-w C-c") 'delete-window)
  (define-key mode-map (kbd "C-w c") 'delete-window)

  ;; Swap windows
  (define-key mode-map (kbd "C-w H") 'windmove-swap-states-left)
  (define-key mode-map (kbd "C-w J") 'windmove-swap-states-down)
  (define-key mode-map (kbd "C-w K") 'windmove-swap-states-up)
  (define-key mode-map (kbd "C-w L") 'windmove-swap-states-right)

  ;; Select windows by number
  (define-key meow-normal-state-keymap (kbd "C-w C-w") 'my-select-window-by-number))

(with-eval-after-load 'magit
  (my/setup-window-keys magit-mode-map))
(with-eval-after-load 'dired
  (my/setup-window-keys dired-mode-map))
(with-eval-after-load 'daemons
  (my/setup-window-keys daemons-mode-map))
;; Added through the hook, because the compilation package probably loads
;; dynamically in some way
(add-hook 'compilation-mode-hook
          (lambda ()
            (my/setup-window-keys compilation-mode-map)))

;; Without the condition i might get an error about an undefined binding
(with-eval-after-load 'eshell
  (define-key eshell-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "q") #'quit-window))

;; Commented out as in your original code
;; (with-eval-after-load 'helpful
;;   (define-key helpful-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'simple
  (define-key special-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'debug
  (define-key debugger-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'message
  (define-key messages-buffer-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'shell
  (define-key shell-command-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'package
  (define-key package-menu-mode-map (kbd "q") #'quit-window))
;; (with-eval-after-load 'shell
;;   (when (boundp 'shell-command-mode-map)
;;     (define-key shell-command-mode-map (kbd "q") #'quit-window)))


;; (global-unset-key (kbd "C-t"))
;; (global-unset-key (kbd "C-y"))

(defun my-org-cycle-or-preview ()
  "Cycle in Org mode or expand Tempel snippet."
  (interactive)
  (call-interactively #'tempel-expand)  ; Call it as if typed directly, because otherwise it doesn't work
  (when (derived-mode-p 'org-mode)
    (org-cycle)))

(global-unset-key (kbd "C-<tab>"))
(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-unset-key (kbd "S-<tab>"))
(global-set-key (kbd "S-<iso-lefttab>") 'next-buffer)

(defun my-disable-magit-keybindings ()
  "Disable specific keybindings in Magit."
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  ;; Add more keybindings as needed
  )

(defun my-space-as-ctrl-c ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events
        (listify-key-sequence (kbd "C-c"))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-s C-o") 'my-org-outline))
