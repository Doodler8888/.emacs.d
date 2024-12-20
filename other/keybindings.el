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

    ;; ("D"  . docker-template)
    ;; ("d"  . toggle-docker-layout)

    ("di" . docker-images)
    ("dc" . docker-containers)
    ;; ("de" . daemons-enable)
    ;; ("dd" . daemons-disable)
    ;; ("du" . daemons-status)
    ;; ("dr" . daemons-restart)

    ("w"  . hydra-window-size/body)

    ("pt" . popper-toggle-type)
    ("pe" . popper-toggle-type-original)
    ("pr" . my-remove-popper-status-from-frame-buffers)

    ("rr" . revert-buffer)

    ("vr" . eval-region)
    ("vo" . eval-defun)

    ("E"  . eshell)
    ("e" . SpawnEshellCurrentWindow)
    ;; ("ep" . eshell-pop) 

    ("m" . toggle-messages-buffer)

    ("uu" . tramp-revert-buffer-with-sudo)
    ("ue" . my-tramp-cleanup)

    ("gm" . pop-global-mark) 

    ("fe" . dired-jump)

    ("xx" . add-execute-permissions-to-current-file)
    ("xr" . add-write-permissions-to-current-file)

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
(global-set-key (kbd "C-s C-s") 'my/consult-line-with-evil)
(global-set-key (kbd "C-s C-c") 'consult-line-visible-region)
;; (global-set-key (kbd "C-s C-s") 'consult-line)
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

;; (define-prefix-command 'my-window-map)
;; (global-set-key (kbd "C-w") 'my-window-map)

;; (global-set-key (kbd "C-w C-l") 'windmove-right)
;; (global-set-key (kbd "C-w C-h") 'windmove-left)
;; (global-set-key (kbd "C-w C-k") 'windmove-up)
;; (global-set-key (kbd "C-w C-j") 'windmove-down)

;; (global-set-key (kbd "C-w C-s") 'split-window-below)
;; (global-set-key (kbd "C-w C-v") 'split-window-right)
;; (global-set-key (kbd "C-w C-c") 'delete-window)
;; (global-set-key (kbd "C-w c") 'delete-window)


(with-eval-after-load 'magit
  (define-prefix-command 'my-window-map)
  (define-key magit-mode-map (kbd "C-w") 'my-window-map)

  (define-key magit-mode-map (kbd "C-w C-l") 'windmove-right)
  (define-key magit-mode-map (kbd "C-w C-h") 'windmove-left)
  (define-key magit-mode-map (kbd "C-w C-k") 'windmove-up)
  (define-key magit-mode-map (kbd "C-w C-j") 'windmove-down)

  (define-key magit-mode-map (kbd "C-w C-s") 'split-window-below)
  (define-key magit-mode-map (kbd "C-w C-v") 'split-window-right)
  (define-key magit-mode-map (kbd "C-w C-c") 'delete-window)
  (define-key magit-mode-map (kbd "C-w c") 'delete-window)
  (define-key meow-normal-state-keymap (kbd "C-w C-w") 'my-select-window-by-number))

(with-eval-after-load 'dired
  (define-prefix-command 'my-window-map)
  (define-key dired-mode-map (kbd "C-w") 'my-window-map)

  (define-key dired-mode-map (kbd "C-w C-l") 'windmove-right)
  (define-key dired-mode-map (kbd "C-w C-h") 'windmove-left)
  (define-key dired-mode-map (kbd "C-w C-k") 'windmove-up)
  (define-key dired-mode-map (kbd "C-w C-j") 'windmove-down)

  (define-key dired-mode-map (kbd "C-w C-s") 'split-window-below)
  (define-key dired-mode-map (kbd "C-w C-v") 'split-window-right)
  (define-key dired-mode-map (kbd "C-w C-c") 'delete-window)
  (define-key dired-mode-map (kbd "C-w c") 'delete-window)
  (define-key meow-normal-state-keymap (kbd "C-w C-w") 'my-select-window-by-number))

(define-key eshell-mode-map (kbd "q") #'quit-window)
(define-key help-mode-map (kbd "q") #'quit-window)
(define-key Info-mode-map (kbd "q") #'quit-window)
;; (define-key helpful-mode-map (kbd "q") #'quit-window)
(define-key special-mode-map (kbd "q") #'quit-window)
(define-key debugger-mode-map (kbd "q") #'quit-window)
(define-key messages-buffer-mode-map (kbd "q") #'quit-window)
(define-key compilation-mode-map (kbd "q") #'quit-window)
(define-key shell-command-mode-map (kbd "q") #'quit-window)
(define-key package-menu-mode-map (kbd "q") #'quit-window)
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

(define-key org-mode-map (kbd "C-s C-o") 'my-org-outline)
