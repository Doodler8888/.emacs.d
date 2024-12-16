(require 'esh-cmd)  ;; Ensure that eshell command functions are loaded
(require 'em-tramp) ; to load eshell’s sudo


;; Eshell

(use-package eshell
  :ensure nil
  :hook ((eshell-mode . eshell-specific-outline-regexp)
         (eshell-mode . eshell-hist-mode)
         (eshell-mode . (lambda () (setq-local scroll-margin 0))))
  :config
  (setq eshell-hist-move-to-end nil)
  (setq eshell-rc-script (concat user-emacs-directory "eshell/eshelrc")
        eshell-history-size 100000
        eshell-buffer-maximum-lines 5000
        ;; eshell-save-history-on-exit t
        eshell-history-file-name "~/.emacs.d/eshell_history"
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-banner-message ""
        eshell-visual-commands'("htop" "ssh" "top" "gpg" "paru" "ngrok"))
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (define-key eshell-mode-map (kbd "C-s C-o") 'consult-outline))
  ;; (define-key eshell-mode-map (kbd "M-r") 'my-eshell-history-choose))

(global-set-key (kbd "M-r") 'my-eshell-history-choose)

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode  ;; don't change to 'eshell-mode'
  :config
  (eshell-syntax-highlighting-global-mode +1))

(with-eval-after-load 'eshell
  (setq eshell-save-history-on-exit nil)

;; Define eshell-append-history function
(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

;; Add eshell-append-history to eshell-pre-command-hook
(add-hook 'eshell-pre-command-hook #'eshell-append-history))

(defvar-local lw-eshell-last-inserted-arg nil)

(defun eshell-insert-last-argument ()
  "Insert the last argument of the last command"
  (interactive)
  (if (eq last-command this-command)
      (let ((completions (cl-mapcan
                          (lambda (cmd) (cl-remove-if (lambda (s) (string-prefix-p "-" s)) (cdr (string-split cmd))))
                          (ring-elements eshell-history-ring))))
        (set-mark (max (point-min) (- (point) (length lw-eshell-last-inserted-arg))))
        (let ((result (completing-read
                       "Insert:"
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             `(metadata (display-sort-function . ,#'identity))
                           (complete-with-action action completions string pred))))))
          (when (region-active-p) (delete-region (region-beginning) (region-end)))
          (insert result)))
    (when-let ((last-cmd (eshell-get-history 0)))
      (insert (setq lw-eshell-last-inserted-arg (car (last (string-split last-cmd))))))))

(global-unset-key (kbd "M-s"))

(defun setup-eshell-keys ()
  (define-key eshell-mode-map (kbd "M-.") 'eshell-insert-last-argument)
  (define-key eshell-mode-map (kbd "M-s") 'my/eshell-clear-ls)
  (define-key eshell-mode-map (kbd "M-,") 'eshell/insert-pwd-at-point))

(add-hook 'eshell-mode-hook 'setup-eshell-keys)

(defun eshell/edit (filename)
  "Open FILENAME in the current buffer, using the current TRAMP address."
  (interactive "sEnter the filename to edit: ")
  ;; Extract the current TRAMP address from the Eshell buffer's default directory
  (let ((tramp-address (file-remote-p default-directory)))
    (if tramp-address
        ;; If we're in a TRAMP directory, use the extracted address
        (find-file (concat tramp-address filename))
      ;; If not in a TRAMP directory, fall back to a default address or prompt the user
      (message "Not in a TRAMP directory. Please specify the TRAMP address manually.")
      ;; Optionally, you can add a fallback mechanism here, e.g., prompting the user for a TRAMP address
      )))

(defalias 'e 'eshell/edit)


(defun eshell/touch (file)
  "Create a file using TRAMP-aware touch implementation."
  (write-region "" nil (expand-file-name file) nil 0))


;; THE LIST WONT BE UPDATED IF YOU USE THE `so' COMMAND
(defvar eshell-elisp-commands '("ls" "cp" "mv" "touch" "rm")
  "List of Eshell commands implemented in Elisp that need /sudo:: prefix.")

(defun eshell/s (&rest args)
  "Wrapper for sudo. Usage: s ls /path or s apt install package"
  (let* ((command (car args))
         (args (cdr args)))
    (message "Command: '%s'" command)
    (message "List contains: %s" eshell-elisp-commands)
    (message "Member check result: %s" (member command eshell-elisp-commands))
    (if (member command eshell-elisp-commands)
        ;; Handle Eshell's Elisp-implemented commands that need /sudo:: prefix
        (let ((sudo-path (mapcar (lambda (arg)
                                  (if (file-name-absolute-p arg)
                                      (concat "/sudo::" arg)
                                    arg))
                                args)))
          (eshell-command-result (concat command " " (string-join sudo-path " "))))
      ;; Default case: use direct sudo for system commands
      (progn
        (message (concat "eshell/sudo " command " " (string-join args " ")))
        (eshell-command-result (concat "eshell/sudo " command " " (string-join args " ")))))))

;; (defun eshell/s (&rest args)
;;   "Wrapper for sudo. Usage: s ls /path or s apt install package"
;;   (let* ((command (car args))
;;          (args (cdr args)))
;;     (cond
;;      ;; Handle rm command
;;      ((string= command "rm")
;;       (eshell-command-result (concat "eshell/sudo /usr/bin/rm " (string-join args " "))))
     
;;      ;; Handle apt and its subcommands
;;      ((string= command "apt")
;;       (eshell-command-result (concat "eshell/sudo apt " (string-join args " "))))
     
;;      ((string= command "swapoff")
;;       (eshell-command-result (concat "eshell/sudo swapoff " (string-join args " "))))

;;      ;; Handle other commands
;;      (t
;;       (let ((sudo-path (mapcar (lambda (arg)
;;                                 (if (file-name-absolute-p arg)
;;                                     (concat "/sudo::" arg)
;;                                   arg))
;;                               args)))
;;         (eshell-command-result (concat command " " (string-join sudo-path " "))))))))

;; (defun eshell/s (&rest args)
;;   "Wrapper for sudo. Usage: s ls /path or s apt install package"
;;   (let* ((command (car args))
;;          (args (cdr args)))
;;     (cond
;;      ((string= command "rm")
;;       (let ((default-directory 
;;              (if (and (car args) (file-name-absolute-p (car args)))
;;                  (file-name-directory (concat "/sudo::" (car args)))
;;                default-directory)))
;;         (eshell-named-command 
;;          "rm"
;;          (list (if (and (car args) (file-name-absolute-p (car args)))
;;                    (concat "/sudo::" (car args))
;;                  (car args))))))
;;      ((string= command "apt")
;;       (let ((default-directory "/sudo::/"))
;;         (eshell-named-command 
;;          "apt"
;;          args)))
;;      (t
;;       (let ((default-directory 
;;              (if (and (car args) (file-name-absolute-p (car args)))
;;                  (file-name-directory (concat "/sudo::" (car args)))
;;                default-directory)))
;;         (eshell-named-command 
;;          command
;;          (list (if (and (car args) (file-name-absolute-p (car args)))
;;                    (concat "/sudo::" (car args))
;;                  (car args)))))))))


(defun eshell-clear-buffer ()
  "Clear the current Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Move to the beginning of the buffer
    (goto-char (point-min))
    ;; Reinsert the prompt at the correct position
    (eshell-reset)))

(defun eshell-new-instance ()
  "Create a new Eshell buffer with a unique name and open it in the current window."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*another eshell buffer*")))
    (eshell)
    (switch-to-buffer eshell-buffer-name)))

(defun eshell-expand-filename-at-point ()
  "Expand the filename at point to its absolute path in eshell."
  (interactive)
  (let* ((filename (thing-at-point 'filename t))
         (expanded (and filename (expand-file-name filename))))
    (if expanded
        (let ((bounds (bounds-of-thing-at-point 'filename)))
          (delete-region (car bounds) (cdr bounds))
          (insert expanded))
      (message "No valid filename at point!"))))

(defun eshell/cat-with-syntax-highlighting (filename)
  "Like cat(1) but with syntax highlighting.
   Stole from aweshell"
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))
(advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


;; Probably this is used for 'consutl-outline' to work in eshell.
(defun eshell-specific-outline-regexp ()
  (setq-local outline-regexp eshell-prompt-regexp))


;; Code for accessing files in eshell on a la output
(defun echo-last-word-of-current-line ()
  "Echo the last word of the current line to the echo area and return it."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (words (split-string line "[ \t\n]+" t))
         (last-word (car (last words))))
    (message "%s" last-word)
    last-word))  ; Return the last word

(defun find-file-last-word-of-current-line ()
  "Open the file named by the last word of the current line.
If the file doesn't exist, display an error message."
  (interactive)
  (let ((last-word (echo-last-word-of-current-line)))
    (if (file-exists-p last-word)
        (find-file last-word)
      (message "File or directory not found: %s" last-word))))


;; Advice that updates buffers after i use eshell utilities that change state of a filesystem
(defun my/eshell-file-operation-advice (orig-fun &rest args)
  "Generic advice for eshell file operations to update buffers."
  (let* ((source (directory-file-name (expand-file-name (car args))))  ; Remove trailing slash
         (dest (when (cadr args) 
                (directory-file-name (expand-file-name (cadr args)))))  ; Remove trailing slash
         (affected-buffers nil))
    
    ;; Store affected buffers before the operation
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when buffer-file-name
          (let ((buf-path (directory-file-name (expand-file-name buffer-file-name))))
            (when (string-prefix-p source buf-path)
              (push (cons buf-path buf) affected-buffers))))))
    
    ;; Call original function
    (apply orig-fun args)
    
    ;; Update stored buffers
    (dolist (buf-info affected-buffers)
      (let* ((old-path (car buf-info))
             (buf (cdr buf-info))
             (relative-path (substring old-path (length source)))
             (new-path (concat dest relative-path)))
        (with-current-buffer buf
          (set-visited-file-name new-path)
          (set-buffer-modified-p nil)
          (message "Updated buffer %s to %s" (buffer-name) new-path))))))

;; (advice-add 'eshell/mv :around #'my/eshell-file-operation-advice)
;; (advice-add 'eshell/cp :around #'my/eshell-file-operation-advice)
;; (advice-add 'eshell/rm :around #'my/eshell-file-operation-advice)


;; Disable "History item: x" messages when scrolling history items.
(advice-add 'eshell-previous-matching-input :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))

(advice-add 'eshell-next-matching-input :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))


;; Allows to use zoxide
(defun eshell/z (q)
  "Query zoxide and change directory in Eshell."
  (if-let*
      ((zoxide (executable-find "zoxide"))
       (target
        (with-temp-buffer
          (if (= 0 (call-process zoxide nil t nil "query" q))
              (string-trim (buffer-string))))))
      (eshell/cd target)
    (unless zoxide (error "Install zoxide"))
    (unless target (error "No Match"))))


(defun eshell/insert-pwd-at-point ()
  "Insert the current directory path at point in current eshell command line."
  (interactive)
  (insert (expand-file-name default-directory)))

(defun eshell-insert-last-output ()
  "Insert the output of the last command."
  (interactive)
  (let* ((start (save-excursion
                  (eshell-previous-prompt 1)
                  (forward-line 1)
                  (point)))
         (end (save-excursion
                (eshell-previous-prompt 0)
                (line-beginning-position)))
         (output (with-current-buffer "*eshell*"
                  (buffer-substring-no-properties start end))))
    (when (and output (not (string-empty-p output)))
      (insert (string-trim-right 
               (replace-regexp-in-string "^[^\n]*\\$ ?" "" output))))))

(defun eshell-get-output-from-last-command ()
  "Get the output of the last command in eshell."
  (save-excursion
    (let* ((input-start (eshell-beginning-of-input))
           (input-end (eshell-end-of-input))
           (input (buffer-substring input-start input-end))
           (output (and (not (string-empty-p input))
                       (string-trim (eshell-previous-output-string 0)))))
      output)))


;; Eshell prompt

(defun git-prompt-branch-name ()
  "Get current git branch name"
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun my-eshell-prompt ()
  "Custom eshell prompt with git branch info and continuation prompt."
  (let* ((default-directory (eshell/pwd))
         (pwd (abbreviate-file-name default-directory)))
    (concat
     pwd  ; Remove the propertize here to keep default eshell directory coloring
     (let ((branch (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null")))
       (if (string-match-p "^[A-Za-z]" branch)
           (propertize (format " [%s]" (string-trim branch)) 'face 'font-lock-function-name-face)
         ""))
     "\n"
     (propertize ">" 'face 'eshell-prompt)
     " ")))

;; Update the prompt regexp to match the new format
(setq eshell-prompt-function #'my-eshell-prompt)
(setq eshell-prompt-regexp "^> ")


(defun my/eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (eshell/clear-scrollback))

(defun my/eshell-clear-ls ()
  "Clear screen, list directory contents, and show a new prompt."
  (interactive)
  (let ((inhibit-read-only t))  ; Allow modification of read-only parts
    (eshell/clear-scrollback)
    (eshell/cd (eshell/pwd))  ; Ensure we're in the correct directory
    (goto-char (point-max))
    (eshell-kill-input)
    (insert "ls -la")
    (eshell-send-input)
    (goto-char (point-max))))
