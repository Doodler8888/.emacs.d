;; Eshell

(use-package eshell
  :ensure nil
  :hook ((eshell-mode . eshell-specific-outline-regexp))
          ;; (eshell-directory-change . sync-dir-in-buffer-name)
  ;; :custom
  ;; (eshell-input-filter 'my-eshell-input-filter)
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (define-key eshell-mode-map (kbd "C-s C-o") 'consult-outline))

;; (setq eshell-history-append t)

;; (setq eshell-destroy-buffer-when-process-dies t)

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode  ;; don't change to 'eshell-mode'
  :config
  (eshell-syntax-highlighting-global-mode +1))

(add-hook 'eshell-mode-hook 'eshell-hist-mode)  ; Enable Eshell history mode
;; ;; ;;(add-hook 'eshell-mode-hook 'eshell-toggle-direct-send) ;; !!! very careful !!!

(setq eshell-rc-script (concat user-emacs-directory "eshell/eshelrc")
      eshell-history-size 100000
      eshell-buffer-maximum-lines 5000
      ;; eshell-save-history-on-exit t
      eshell-history-file-name "~/.emacs.d/eshell_history"
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-banner-message ""
      eshell-visual-commands'("htop" "ssh" "top" "gpg" "paru" "ngrok"))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local scroll-margin 0)))

(with-eval-after-load 'eshell
  ;; Set eshell-save-history-on-exit to nil
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

(defun eshell-insert-last-argument ()
  "Insert the last argument of the previous command."
  (interactive)
  (let* ((last-command (eshell-previous-input-string 0))
         (args (split-string-and-unquote last-command))
         (last-arg (car (last args))))
    (when last-arg
      (insert last-arg))))

(defun setup-eshell-keys ()
  (define-key eshell-mode-map (kbd "M-.") 'eshell-insert-last-argument))
;; (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history))

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

(defun eshell/touch (file)
  "Create a file using TRAMP-aware touch implementation."
  (write-region "" nil (expand-file-name file) nil 0))

(defun eshell/s (&rest args)
  "Wrapper for sudo. Usage: s ls /path or s apt install package"
  (let* ((command (car args))
         (args (cdr args)))
    (cond
     ;; Handle rm command
     ((string= command "rm")
      (eshell-command-result (concat "eshell/sudo /usr/bin/rm " (string-join args " "))))
     
     ;; Handle apt and its subcommands
     ((string= command "apt")
      (eshell-command-result (concat "eshell/sudo apt " (string-join args " "))))
     
     ;; Handle other commands
     (t
      (let ((sudo-path (mapcar (lambda (arg)
                                (if (file-name-absolute-p arg)
                                    (concat "/sudo::" arg)
                                  arg))
                              args)))
        (eshell-command-result (concat command " " (string-join sudo-path " "))))))))

(defalias 'e 'eshell/edit)

(require 'em-tramp) ; to load eshell’s sudo
;; (setq eshell-prefer-lisp-functions t)
;; (setq eshell-prefer-lisp-variables t)
;; (setq password-cache t) ; enable password caching
;; (setq password-cache-expiry 10)
;; (add-hook 'eshell-load-hook (lambda () (add-to-list 'eshell-modules-list 'eshell-tramp)))

;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (eshell/alias "sudo" "eshell/sudo $*")))

(defun eshell-clear-buffer ()
  "Clear the current Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Move to the beginning of the buffer
    (goto-char (point-min))
    ;; Reinsert the prompt at the correct position
    (eshell-reset)))

(defun eshell-new ()
  "Create a new Eshell buffer with a unique name and open it in the current window."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*another eshell buffer*")))
    (eshell)
    (switch-to-buffer eshell-buffer-name)))

(defun eshell-new-pop ()
  "Create a new Eshell buffer with a unique name, open it in the current window, and toggle popper type if popper-mode is active."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*another eshell buffer*")))
    (eshell)
    (switch-to-buffer eshell-buffer-name)
    ;; Check if popper-mode is enabled and popper-toggle-type is available
    (when (and (featurep 'popper) (bound-and-true-p popper-mode))
      (popper-toggle-type eshell-buffer-name))))

(defun eshell-pop ()
  "Execute the eshell command and launch eshell as a popper buffer"
  (interactive)
  (eshell)
  (popper-toggle-type))

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

(defun eshell-specific-outline-regexp ()
  (setq-local outline-regexp eshell-prompt-regexp))

(defun eshell-redirect-to-buffer (buffer)
  "Auto create command for redirecting to buffer."
  (interactive (list (read-buffer "Redirect to buffer: ")))
  (insert (format " >>> #<%s>" buffer)))

(defun echo-current-line ()
  "Echo the entire current line to the echo area."
  (interactive)
  (message "%s" (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))

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

;; Function to get the last 10 recently visited directories
(defun my-recent-directories ()
  "Get a list of the last 10 recently visited directories."
  (let ((dirs (delete-dups
               (mapcar 'file-name-directory recentf-list))))
    (seq-filter #'identity
                (cl-remove-if-not #'file-directory-p dirs))))

;; Function to prompt user to select a directory and change to it in Eshell
(defun my-eshell-change-to-recent-directory ()
  "Prompt user to select a recent directory and change to it in Eshell."
  (interactive)
  (let* ((recent-dirs (my-recent-directories))
         (selected-dir (completing-read "Choose recent directory: " recent-dirs nil t)))
    (when (and selected-dir (file-directory-p selected-dir))
      ;; Change directory
      (eshell/cd selected-dir)
      ;; Remove the old prompt
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          ;; Move to the previous prompt and delete it
          (when (re-search-backward eshell-prompt-regexp nil t)
            (delete-region (point) (point-max)))))
      ;; Display the new prompt
      (eshell-emit-prompt))))


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
