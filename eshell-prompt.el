  ;; ;; Store original PATH to restore it later
  ;; (setq my/original-path (getenv "PATH"))
  
  ;; (setq pyvenv-post-activate-hooks
  ;;       (list (lambda ()
  ;;               (let ((venv-bin-dir (expand-file-name "bin" pyvenv-virtual-env)))
  ;;                 ;; Update PATH
  ;;                 (setenv "PATH" (concat venv-bin-dir path-separator (getenv "PATH")))
  ;;                 ;; Update exec-path
  ;;                 (setq exec-path (cons venv-bin-dir exec-path))
  ;;                 ;; Update Eshell's PATH
  ;;                 (setq eshell-path-env (getenv "PATH"))
  ;;                 ;; Set VIRTUAL_ENV environment variable
  ;;                 (setenv "VIRTUAL_ENV" pyvenv-virtual-env)
  ;;                 ;; Update Python path for LSP
  ;;                 (setenv "PYTHONPATH" (expand-file-name "lib/python3.*/site-packages" pyvenv-virtual-env))
  ;;                 ;; Restart LSP if active
  ;;                 (when (bound-and-true-p lsp-mode)
  ;;                   (lsp-restart-workspace))))))
  
  ;; (setq pyvenv-post-deactivate-hooks
  ;;       (list (lambda ()
  ;;               (setenv "PATH" my/original-path)
  ;;               (setq exec-path (split-string my/original-path path-separator))
  ;;               (setq eshell-path-env my/original-path)
  ;;               (setenv "VIRTUAL_ENV" nil)
  ;;               (setenv "PYTHONPATH" nil)
  ;;               (when (bound-and-true-p lsp-mode)
  ;;                 (lsp-restart-workspace))))))

;; (defun my/find-venv-in-project ()
;;   "Find .venv directory in project structure."
;;   (when-let ((project-root (directory-file-name (project-root (project-current)))))
;;     (message "Searching for .venv in: %s" project-root)
;;     (let* ((default-directory project-root)
;;            (venv-path (locate-dominating-file 
;;                       default-directory
;;                       (lambda (dir)
;;                         (file-exists-p (expand-file-name "server-python/.venv" dir))))))
;;       (when venv-path
;;         (expand-file-name "server-python/.venv" venv-path)))))

;; (defun my/activate-python-venv ()
;;   "Activate venv if found in project structure."
;;   (when-let ((venv-path (my/find-venv-in-project)))
;;     (message "Found venv at: %s" venv-path)
;;     (pyvenv-activate venv-path)))

;; ;; Activate on Python files
;; (add-hook 'python-mode-hook #'my/activate-python-venv)

;; ;; Activate on directory change in Eshell
;; (defun my/eshell-activate-venv ()
;;   "Activate venv when changing directories in Eshell."
;;   (my/activate-python-venv))

;; (add-hook 'eshell-directory-change-hook #'my/eshell-activate-venv)

;; ;; Eshell prompt configuration
;; (defun my/eshell-prompt-virtual-env ()
;;   "Return virtual env name if any."
;;   (when pyvenv-virtual-env-name
;;     (format "(%s) " pyvenv-virtual-env-name)))

;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat
;;          (my/eshell-prompt-virtual-env)          ; venv name
;;          (abbreviate-file-name (eshell/pwd))     ; current directory
;;          " $ ")))                                ; prompt character

;; (setq eshell-highlight-prompt nil)


;; (defun my/eshell-prompt-virtual-env ()
;;   "Return virtual env name if any."
;;   (when pyvenv-virtual-env-name
;;     (format "(%s) " pyvenv-virtual-env-name)))

;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat
;;          (my/eshell-prompt-virtual-env)          ; venv name
;;          (abbreviate-file-name (eshell/pwd))     ; current directory
;;          " $ ")))                                ; prompt character

;; (setq eshell-highlight-prompt nil)
