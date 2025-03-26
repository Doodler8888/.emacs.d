;; -*- lexical-binding: t; -*-

(use-package meow
  :ensure t
  :init
  (delete-selection-mode -1)
  (setq meow-use-clipboard t))
  ;; (setq meow-use-dynamic-face-color nil))

(load "~/.emacs.d/other/meow-parse.el")


;; This command is bound to M-i
(define-key meow-insert-state-keymap (kbd "C-t") 'tab-to-tab-stop)


(with-eval-after-load 'tempel
  (add-hook 'meow-insert-exit-hook
            (lambda ()
              (when tempel--active
                (tempel-done)))))

(define-key meow-insert-state-keymap (kbd "M-=") 'tempel-expand)


;; ;; A variable to hold the change points
;; (defvar my/last-change-points '()
;;   "List of markers representing points before changes were made.")

;; (defvar my/forward-change-points '()
;;   "List of markers representing points after changes were made.")

;; (defun my/record-change-point (&rest _)
;;   "Record the current location in `my/last-change-points'.
;; We use a marker so that if the buffer is modified, the position still tracks correctly.
;; Accepts and ignores any arguments to work with advice."
;;   (push (copy-marker (point)) my/last-change-points))

;; (defun my/new-goto-last-change ()
;;   "Go to the most recent change point."
;;   (interactive)
;;   (when my/last-change-points
;;     ;; Save the current position into the forward stack
;;     (push (copy-marker (point)) my/forward-change-points)
;;     (let ((marker (pop my/last-change-points))) ;; Get the last change point
;;       (if (marker-position marker)
;;           (goto-char marker) ;; Jump to the position
;;         (message "Invalid marker position")))))

;; (defun my/new-goto-next-change ()
;;   "Go forward to the next change point if available."
;;   (interactive)
;;   (when my/forward-change-points
;;     (let ((marker (pop my/forward-change-points))) ;; Get the forward point
;;       (when (marker-position marker)
;;         (push (copy-marker (point)) my/last-change-points) ;; Save current before moving
;;         (goto-char marker)))))

;; ;; Add advice to record the position before these commands execute
;; (advice-add 'my/meow-smart-delete :before #'my/record-change-point)
;; (advice-add 'my/meow-smart-comment  :before #'my/record-change-point)
;; (advice-add 'my/meow-smart-paste  :before #'my/record-change-point)
;; (advice-add 'my/meow-smart-paste-no-save  :before #'my/record-change-point)
;; (add-hook 'meow-insert-exit-hook #'my/record-change-point)


;; (use-package meow-tree-sitter
;;   :init
;;   (meow-tree-sitter-register-defaults)
;;   :hook
;;   ((bash-ts-mode
;;     c-ts-mode
;;     c++-ts-mode
;;     cmake-ts-mode
;;     css-ts-mode
;;     json-ts-mode
;;     python-ts-mode
;;     rust-ts-mode
;;     typescript-ts-mode
;;     yaml-ts-mode) . meow-tree-sitter-mode))

;; (with-eval-after-load 'meow
;;   (define-key rectangle-mark-mode-map [remap meow-insert] 'string-rectangle))

;; This settings causes me an error message when auto-fill-line is triggered and
;; probably the same thing with using meow-open-above in the text-mode. So i
;; don't know why i enabled it in the first place.
;; (setq-default indent-line-function nil)


(global-set-key (kbd "TAB") #'completion-at-point)

;; Argument count doesn't get reset right away when i use meow-next/prev
(defun my/reset-prefix-arg (&rest _)
  "Reset the prefix argument to nil."
  (setq prefix-arg nil)
  (setq current-prefix-arg nil))

;; (advice-add 'my/next-line-close-selection :after #'my/reset-prefix-arg)
;; (advice-add 'my/previous-line-close-selection :after #'my/reset-prefix-arg)
;; (advice-add 'meow-next :after #'my/reset-prefix-arg)
;; (advice-add 'meow-prev :after #'my/reset-prefix-arg)

;; For number hints to work in org mode
(setq meow-expand-exclude-mode-list
      (remove 'org-mode meow-expand-exclude-mode-list))


;; ;; Using meow-open-above in the text-mode gives an error message regarding
;; ;; applying indentation. This is an attempt to disable this message by removing
;; ;; the (indent-according-to-mode) function that is used there.
;; ;; But i probably don't need this advice because the error was caused by
;; ;; using '(setq-default indent-line-function nil)'
;; (advice-add 'meow-open-above :around
;;             (lambda (orig-fun &rest args)
;;               "Disable indentation in `text-mode` for `meow-open-above`."
;;               (let ((indent-inhibit (derived-mode-p 'text-mode)))
;;                 (cl-letf (((symbol-function 'indent-according-to-mode)
;;                            (lambda () (unless indent-inhibit
;;                                         (funcall (symbol-function 'indent-according-to-mode))))))
;;                   (apply orig-fun args)))))


(defun my/smart-replace ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'query-replace)
    (call-interactively 'replace-regexp)))

;; (global-set-key (kbd "M-%") 'my/smart-replace)
(global-set-key (kbd "M-%") 'query-replace)


(defun my-forward-char-with-selection (&optional arg)
  "Move forward ARG characters, activating the region."
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))  ; Start selection if region is not active
  (forward-char arg))

(defun my-backward-char-with-selection (&optional arg)
  "Move forward ARG characters, activating the region."
  (interactive "p")
  (unless (region-active-p)
    (set-mark (point)))  ; Start selection if region is not active
  (backward-char arg))

(defun my/forward-list ()
  "Mark text within the next balanced group of parentheses/brackets.
   Cursor ends at the closing bracket."
  (interactive)
  (when (re-search-forward "\\s(" nil t)
    (backward-char)
    (set-mark (point))
    (forward-list)))

(defun my/backward-list ()
  "Mark text within the previous balanced group of parentheses/brackets.
   Cursor ends at the opening bracket."
  (interactive)
  (when (re-search-backward "\\s)" nil t)
    (forward-char)
    (set-mark (point))
    (backward-list)))

(global-set-key (kbd "C-M-n") #'my/forward-list)
(global-set-key (kbd "C-M-p") #'my/backward-list)

(defvar my/vertical-motion-goal-column 0
  "The tracked column for vertical motion.")

(defun my/buffer-has-word-wrap-p ()
  "Return non-nil if the current buffer has word-wrap enabled."
  (let ((truncate-lines-value truncate-lines)
        (word-wrap-value word-wrap)
        (visual-line-mode-value visual-line-mode)
        (word-wrap-by-category (and (boundp 'word-wrap-by-category)
                                    word-wrap-by-category))
        (wrap-prefix-value (and (boundp 'wrap-prefix) wrap-prefix)))

    ;; Print debug info to message buffer
  ;;   (message "Word wrap detection:
  ;; truncate-lines: %s (nil means wrapping is ON)
  ;; word-wrap: %s
  ;; visual-line-mode: %s
  ;; word-wrap-by-category: %s
  ;; wrap-prefix: %s"
  ;;            truncate-lines-value
  ;;            word-wrap-value
  ;;            visual-line-mode-value
  ;;            word-wrap-by-category
  ;;            wrap-prefix-value)

    ;; Logic:
    ;; - truncate-lines = nil means line wrapping is ON
    ;; - visual-line-mode being active also means wrapping is ON
    (or (not truncate-lines-value)
        word-wrap-value
        visual-line-mode-value)))


(defun my/simple-next-line (&optional arg)
  "Simple function to move to the next line and close selection if any."
  (interactive "P")
  (when (region-active-p)
    (deactivate-mark))
  (setq this-command 'next-line)
  (next-line (or arg 1)))

(defun my/simple-previous-line (&optional arg)
  "Simple function to move to the previous line and close selection if any."
  (interactive "P")
  (when (region-active-p)
    (deactivate-mark))
  (setq this-command 'previous-line)
  (previous-line (or arg 1)))

;; (defun my/simple-next-line (&optional arg)
;;   "Simple function to move to the next line and close selection if any."
;;   (interactive "P")
;;   (when (region-active-p)
;;     (deactivate-mark))
;;   (next-line (or arg 1)))

;; (defun my/simple-previous-line (&optional arg)
;;   "Simple function to move to the previous line and close selection if any."
;;   (interactive "P")
;;   (when (region-active-p)
;;     (deactivate-mark))
;;   (previous-line (or arg 1)))

;; (defun my/next-line-close-selection (&optional arg)
;;   "Move to the next visual line while preserving the tracked column.
;; If the point is not at the end of the line, update the tracked column.
;; With prefix ARG, move that many lines."
;;   (interactive "p")
;;   (if (my/buffer-has-word-wrap-p)
;;       (my/simple-next-line arg)
;;     ;; Original logic for non-wrapped buffers
;;     (when (region-active-p)
;;       (deactivate-mark))
;;     ;; Update tracked column only if not at the end of the line.
;;     (unless (eolp)
;;       (setq my/vertical-motion-goal-column (current-column)))
;;     ;; Move ARG visual lines down, handling wrapped lines
;;     (let ((next-line-add-newlines nil)) ; Don't add newlines when at end of buffer
;;       (next-line (or arg 1)))
;;     ;; Move to the tracked column (defaulting to 0 if nil)
;;     (move-to-column (or my/vertical-motion-goal-column 0))))

;; (defun my/previous-line-close-selection (&optional arg)
;;   "Move to the previous visual line while preserving the tracked column.
;; If the point is not at the end of the line, update the tracked column.
;; With prefix ARG, move that many lines."
;;   (interactive "p")
;;   (if (my/buffer-has-word-wrap-p)
;;       (my/simple-previous-line arg)
;;     ;; Original logic for non-wrapped buffers
;;     (when (region-active-p)
;;       (deactivate-mark))
;;     ;; Update tracked column only if not at the end of the line.
;;     (unless (eolp)
;;       (setq my/vertical-motion-goal-column (current-column)))
;;     ;; Move ARG visual lines up, handling wrapped lines
;;     (let ((next-line-add-newlines nil)) ; Don't add newlines when at end of buffer
;;       (previous-line (or arg 1)))
;;     ;; Move to the tracked column (defaulting to 0 if nil)
;;     (move-to-column (or my/vertical-motion-goal-column 0))))


(defun my-match-paren-with-selection (arg)
  "Go to the matching parenthesis and select the enclosed text, including the delimiters.
If not on a parenthesis, insert % or do nothing if in Meow mode."
  (interactive "p")
  (cond
   ((looking-at "\\s(")
    (let ((start (point)))
      (forward-sexp 1)
      (set-mark start)))
   ((looking-back "\\s)" 1)
    (let ((end (point)))
      (backward-sexp 1)
      (set-mark end)))
   ((looking-at "\\s)")
    (let ((end (1+ (point))))
      (forward-char 1)
      (backward-sexp 1)
      (set-mark end)))
   ((looking-back "\\s(" 1)
    (let ((start (1- (point))))
      (backward-char 1)
      (forward-sexp 1)
      (set-mark start)))
   (t
    (if (bound-and-true-p meow-mode)
        (message "Not on a delimiter")
      (self-insert-command (or arg 1)))))
  (when (region-active-p)
    (activate-mark)))

(defun my/meow-line-beginning ()
  "Move to the first non-whitespace character of the line and apply selection."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (region-active-p)
      (meow-cancel-selection))
    (meow--make-selection '(select . transient) start (point))
    (meow--select (meow--make-selection '(select . transient) start (point)))))

(defun my/meow-line-end ()
  "Move to the end of the line and apply selection."
  (interactive)
  (let ((start (point)))
    (end-of-line)
    (when (region-active-p)
      (meow-cancel-selection))
    (meow--make-selection '(select . transient) start (point))
    (meow--select (meow--make-selection '(select . transient) start (point)))))

(defun my/meow-line-start ()
  "Move to the start of the line and apply selection."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (when (region-active-p)
      (meow-cancel-selection))
    (meow--make-selection '(select . transient) start (point))
    (meow--select (meow--make-selection '(select . transient) start (point)))))

(defun my-meow-line-or-digit-0 ()
  "Activate meow-line if no selection, otherwise call my-meow-digit with 0."
  (interactive)
  (if (region-active-p)
      (my-meow-digit ?0)
    (meow-line)))

(defun my/delete-char-to-kill-ring ()
  "Delete the character at point and add it to the kill ring."
  (interactive)
  (when (not (eobp))
    (kill-new (buffer-substring-no-properties (point) (1+ (point))))
    (delete-char 1)))

(defun my/meow-setup-extra ()
  ;; Don't ignore cursor shape changes in minibuffer
  (delete (cons 'minibufferp 'meow--update-cursor-default)
	  meow-update-cursor-functions-alist)
  ;; Remove defalt minibuffer setup
  (remove-hook 'minibuffer-setup-hook 'meow--minibuffer-setup)
  ;; Use INSERT state in minibuffer by default,
  ;; then later we can switch to NORMAL with ESC
  (add-hook 'minibuffer-setup-hook 'meow-insert-mode))

;; Apply the patch after meow is activated
(add-hook 'meow-global-mode-hook 'my/meow-setup-extra)

(defun my-meow-digit ( digit )
  (interactive)
  (if (not (and meow--expand-nav-function
                (region-active-p)
                (meow--selection-type)))
      (progn
        (universal-argument)
        (meow-digit-argument))
    (meow-expand digit)))

(global-set-key (kbd "H-w") 'kill-region) ;; It wont help if i have C-w bound in the main file
(global-set-key (kbd "H-u") 'universal-argument)
(global-set-key (kbd "H-d") 'delete-char)
(global-set-key (kbd "H-f") 'forward-char)
(global-set-key (kbd "H-b") 'backward-char)
(global-set-key (kbd "H-h") 'mark-paragraph)
(global-set-key (kbd "H-r") 'mark-paragraph)
(setq meow--kbd-kill-region "H-w")
(setq meow--kbd-universal-argument "H-u")
(setq meow--kbd-delete-char "H-d")
(setq meow--kbd-mark-paragraph "H-h")
(setq meow--kbd-forward-char "H-f")
(setq meow--kbd-backward-char "H-b")
(setq meow--kbd-mark-paragraph "H-r")


(defun my/select-line-for-up (&optional arg)
  "Select ARG + 1 lines upward if ARG is provided, otherwise select just the current line.
If ARG is provided, it selects the current line and ARG lines above it."
  (interactive "P")
  (if (null arg)
      ;; No argument: select just the current line
      (progn
        (end-of-line)
        (push-mark (point) nil t)
        (beginning-of-line))
    ;; With argument: select current line and ARG lines above (ARG + 1 total)
    (let ((num-lines (1+ (prefix-numeric-value arg))))
      (end-of-line)
      (push-mark (point) nil t)
      (beginning-of-line)
      (forward-line (- (1- num-lines))))))

(defun my/select-line-for-down (&optional arg)
  "Select ARG + 1 lines downward if ARG is provided, otherwise select just the current line.
If ARG is provided, it selects the current line and ARG lines below it."
  (interactive "P")
  (if (null arg)
      ;; No argument: select just the current line
      (progn
        (beginning-of-line)
        (push-mark (point) nil t)
        (forward-line 1))
    ;; With argument: select current line and ARG lines below (ARG + 1 total)
    (let ((num-lines (1+ (prefix-numeric-value arg))))
      (beginning-of-line)
      (push-mark (point) nil t)
      (forward-line num-lines))))


(defun my-select-window-by-number (number)
  "Select the window specified by NUMBER.
Numbered from top-left to bottom-right."
  (interactive "P")
  (let* ((windows (window-list-1 (frame-first-window) 'nomini t))
         (window-count (length windows))
         (index (if (numberp number)
                    (1- number)
                  (let ((char (read-char "Window: ")))
                    (if (and (>= char ?1) (<= char ?9))
                        (- char ?1)
                      (error "Invalid window number"))))))
    (if (>= index window-count)
        (error "Window number %d out of range" (1+ index))
      (select-window (nth index windows)))))


(define-key meow-insert-state-keymap (kbd "C-u") 'my/smart-backspace)

;; (define-key meow-insert-state-keymap (kbd "C-w") 'backward-kill-word)
(define-key meow-insert-state-keymap (kbd "C-w") 'backward-kill-sexp)

(define-prefix-command 'my-window-map)
(define-key meow-normal-state-keymap (kbd "C-w") 'my-window-map)

(define-key meow-normal-state-keymap (kbd "C-w C-l") 'windmove-right)
(define-key meow-normal-state-keymap (kbd "C-w C-h") 'windmove-left)
(define-key meow-normal-state-keymap (kbd "C-w C-k") 'windmove-up)
(define-key meow-normal-state-keymap (kbd "C-w C-j") 'windmove-down)

(define-key meow-normal-state-keymap (kbd "C-w C-s") 'split-window-below)
(define-key meow-normal-state-keymap (kbd "C-w C-v") 'split-window-right)
(define-key meow-normal-state-keymap (kbd "C-w C-c") 'delete-window)
(define-key meow-normal-state-keymap (kbd "C-w c") 'delete-window)

(define-key meow-normal-state-keymap (kbd "C-w C-w") 'my-select-window-by-number)


;; (define-key meow-motion-state-keymap (kbd "C-w") 'my-window-map)

;; (define-key meow-motion-state-keymap (kbd "C-w C-l") 'windmove-right)
;; (define-key meow-motion-state-keymap (kbd "C-w C-h") 'windmove-left)
;; (define-key meow-motion-state-keymap (kbd "C-w C-k") 'windmove-up)
;; (define-key meow-motion-state-keymap (kbd "C-w C-j") 'windmove-down)

;; (define-key meow-motion-state-keymap (kbd "C-w C-s") 'split-window-below)
;; (define-key meow-motion-state-keymap (kbd "C-w C-v") 'split-window-right)
;; (define-key meow-motion-state-keymap (kbd "C-w C-c") 'delete-window)
;; (define-key meow-motion-state-keymap (kbd "C-w c") 'delete-window)


(define-key minibuffer-local-map (kbd "M-n") 'my-next-history-element)
(define-key minibuffer-local-map (kbd "M-p") 'my-previous-history-element)


(defun surround-region-with-symbol (&optional char)
  "Surround the currently active region with a symbol input by the user.
Adds spaces when using right brackets."
  (interactive (list (read-char "Enter symbol: ")))
  (unless (use-region-p)
    (error "No active region to surround"))
  (let* ((start (region-beginning))
         (end (region-end))
         (char-str (char-to-string char))
         (pairs '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")))
         (left-char (or (car (rassoc char-str pairs)) char-str))
         (right-char (or (cdr (assoc left-char pairs)) char-str))
         (add-spaces (member char-str '(")" "]" "}" ">"))))
    (save-excursion
      (goto-char end)
      (when add-spaces (insert " "))
      (insert right-char)
      (goto-char start)
      (insert left-char)
      (when add-spaces (insert " "))))
  ;; Return only the command name and the character to store in history
  (list 'surround-region-with-symbol char))

(defun change-surrounding-symbol (&optional char)
  "Change the symbols surrounding the region, handling both left and right inputs."
  (interactive (list (read-char "Enter new symbol: ")))
  (let* ((start (region-beginning))
         (end (region-end))
         (region-text (buffer-substring-no-properties start end))
         (first-char (substring region-text 0 1))
         (last-char (substring region-text -1))
         (content (substring region-text 1 -1))
         (new-symbol (char-to-string char))
         (pairs '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")
                 ("\"" . "\"") ("'" . "'") ("`" . "`") ("*" . "*") ("/" . "/") ("=" . "=")))
         (reverse-pairs (mapcar (lambda (p) (cons (cdr p) (car p))) pairs))
         (is-opening (assoc new-symbol pairs))
         (is-closing (assoc new-symbol reverse-pairs))
         (new-open (or (car is-opening) (cdr is-closing) new-symbol))
         (new-close (or (cdr is-opening) (car is-closing) new-symbol)))
    (when (or
           (and (equal first-char (car (assoc first-char pairs)))
                (equal last-char (cdr (assoc first-char pairs))))
           (and (member first-char '("\"" "'" "`" "/"))
                (equal first-char last-char)))
      (delete-region start end)
      (insert new-open content new-close))
    (list 'change-surrounding-symbol char)))

(defun delete-surrounding-symbol (&optional start end)
  "Delete the symbols surrounding the region, with added debugging.
START and END are optional boundaries (if nil, use current region)."
  (interactive "r")
  (unless (use-region-p)
    (error "No active region to delete surrounding symbols from"))
  (let* ((actual-start (or start (region-beginning)))
         (actual-end (or end (region-end)))
         (region-text (buffer-substring-no-properties actual-start actual-end)))
    ;; (message "delete-surrounding-symbol: Initial region: %d - %d, text: \"%s\""
    ;;          actual-start actual-end region-text)
    (let ((content (substring region-text 1 -1)))
      ;; (message "delete-surrounding-symbol: Content after removing first and last char: \"%s\"" content)
      (delete-region actual-start actual-end)
      (insert content))
    ;; (message "delete-surrounding-symbol: Buffer after deletion: %s" (buffer-string))
    ;; Return command info including boundaries for replay
    (list 'delete-surrounding-symbol actual-start actual-end)))


(defun my/meow-find-backward (arg ch)
  "Combine negative argument with meow-find to search backward in one keybinding."
  (interactive "p\ncFind backward:")
  (meow-find (- arg) ch))

(defun my/meow-till-backward (arg ch)
  "Combine negative argument with meow-till to search backward in one keybinding."
  (interactive "p\ncTill backward:")
  (meow-till (- arg) ch))



(defun select-inside-whitespace ()
  "Select the current symbol without surrounding whitespace."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds)))))

(defun select-around-whitespace ()
  "Select the current symbol and surrounding whitespace."
  (interactive)
  (let ((start (save-excursion
                 (skip-syntax-backward "^-")
                 (skip-syntax-backward "-")
                 (point)))
        (end (save-excursion
               (skip-syntax-forward "^-")
               (skip-syntax-forward "-")
               (point))))
    (goto-char start)
    (set-mark end)))

(defun meow--parse-single-quote (outer)
  "Parse the bounds for single quote selection."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           start end)
      (setq start (search-backward "'" line-start t))
      (when start
        (setq end (search-forward "'" line-end t))
        (when (and end (= (1+ start) end))
          ;; If quotes are adjacent, search for the next set
          (setq end (search-forward "'" line-end t))
          (unless end
            ;; If no closing quote found, search backward from initial point
            (goto-char start)
            (setq start (search-backward "'" line-start t)))))
      (when (and start end)
        (if outer
            (cons start end)
          (cons (1+ start) (1- end)))))))

(defun select-inside-single-quote ()
  "Select the text inside single quotes."
  (interactive)
  (let ((bounds (meow--parse-single-quote nil)))
    (when bounds
      (goto-char (cdr bounds))  ; Move point to the end
      (set-mark (car bounds))))) ; Set mark at the start

(defun select-around-single-quote ()
  "Select the text including single quotes."
  (interactive)
  (let ((bounds (meow--parse-single-quote t)))
    (when bounds
      (goto-char (cdr bounds))  ; Move point to the end
      (set-mark (car bounds)))))

(defun my/indent-region-right ()
  "Indent region or current line to the right by 2 spaces (like Vim's >>)."
  (interactive)
  (if (region-active-p)
      (let ((deactivate-mark nil))  ; Preserve the active region
        (indent-rigidly (region-beginning) (region-end) 2))
    (indent-rigidly (line-beginning-position) (line-end-position) 2)))

(defun my/indent-region-left ()
  "Indent region or current line to the left by 2 spaces (like Vim's <<)."
  (interactive)
  (if (region-active-p)
      (let ((deactivate-mark nil))  ; Preserve the active region
        (indent-rigidly (region-beginning) (region-end) -2))
    (indent-rigidly (line-beginning-position) (line-end-position) -2)))

;; (defun my/smart-comment ()
;;   "Comment or uncomment region if active, otherwise comment/uncomment current line."
;;   (interactive)
;;   (let ((start (if (region-active-p) (region-beginning) (line-beginning-position)))
;;         (end (if (region-active-p) (region-end) (line-end-position))))
;;     (comment-or-uncomment-region start end)))

(defun my-meow-paste-before ()
  "Paste before cursor without overwriting kill ring, similar to Vim's 'P'."
  (interactive)
  (let* ((content (current-kill 0 t))
         (lines (split-string content "\n")))
    (cond
     ;; For multi-line content
     ((> (length lines) 1)
      (beginning-of-line)
      (save-excursion
        (insert (string-join lines "\n"))
        (unless (string-suffix-p "\n" content)
          (insert "\n"))))
     ;; For single-line content ending with newline
     ((string-suffix-p "\n" content)
      (beginning-of-line)
      (save-excursion
        (insert content)))
     ;; For single-line content without newline
     (t
      (save-excursion
        (backward-char)
        (insert content))))))

(defun save-and-paste ()
  "Copy the current line to the kill ring."
  (interactive)
  (move-beginning-of-line 1)
  (copy-whole-line)
  (yank))


(defun my/meow-smart-paste-no-save (&optional arg)
  "Paste like Vim, handling both line-wise and regular pastes.
With numeric prefix ARG, paste that many times.
With raw prefix argument (C-u without a number), paste from the kill ring.
When pasting over a selection, the replaced text is NOT saved to the kill ring."
  (interactive "P")
  (let* ((raw-prefix (equal arg '(4)))
         (numeric-prefix (and (integerp arg) (> arg 0)))
         (repeat-count (if numeric-prefix arg 1))
         ;; Get system clipboard content if available, otherwise fall back to kill ring
         (text-to-paste (if raw-prefix
                            (current-kill (if (listp last-command-event)
                                              0
                                            (mod (- (aref (this-command-keys) 0) ?0)
                                                 kill-ring-max))
                                          t)
                          (or (gui-get-selection 'CLIPBOARD)
                              (current-kill 0 t)))))

    (when (region-active-p)
      ;; Simply delete the region without saving it to the kill ring
      (delete-region (region-beginning) (region-end)))

    (dotimes (_ repeat-count)
      (if (string-suffix-p "\n" text-to-paste)
          (progn
            (forward-line)
            (beginning-of-line)
            (insert text-to-paste)
            (forward-line -1)
            (beginning-of-line))
        (insert text-to-paste)))))

(defun my/rectangle-smart-paste-alt ()
  "Paste clipboard content over a rectangle selection.
If a rectangle region is active, replace it with the clipboard content,
preserving the start column position.
If clipboard contains a single line, it pastes that line to all rows in the selected rectangle."
  (interactive)
  (if (not (use-region-p))
      (yank)
    (let* ((clipboard-text (or (gui-get-selection 'CLIPBOARD) (current-kill 0 t)))
           (lines (split-string clipboard-text "\n"))
           (start-col (save-excursion
                        (goto-char (region-beginning))
                        (current-column)))
           (start-line (line-number-at-pos (region-beginning)))
           (end-line (line-number-at-pos (region-end)))
           (num-lines (max 1 (- end-line start-line -1)))  ;; Ensure at least 1 line
           (start-pos (region-beginning))
           ;; Handle the single-line clipboard case
           (single-line-mode (= 1 (length lines))))

      ;; Delete rectangle
      (delete-rectangle (region-beginning) (region-end))

      ;; Go to starting position
      (goto-char start-pos)

      ;; Insert each line at the correct position
      (dotimes (i num-lines)
        (move-to-column start-col t)
        ;; If single line mode, always insert the first line
        ;; Otherwise, insert the corresponding line from clipboard (if available)
        (if single-line-mode
            (insert (car lines))
          (when (< i (length lines))
            (insert (nth i lines))))
        (when (< i (1- num-lines))
          (forward-line 1)))

      ;; Position cursor at the beginning of the pasted content
      (goto-char start-pos)
      (move-to-column start-col))))

(defun my/super-smart-paste ()
  (interactive)
  (if (use-region-p)
	  (my/meow-smart-paste-no-save)
	(my/rectangle-smart-paste-alt)))

(defun my/meow-smart-paste (&optional arg)
  "Paste like Vim, handling both line-wise and regular pastes.
With numeric prefix ARG, paste that many times.
With raw prefix argument (C-u without a number), paste from the kill ring.
When pasting over a selection, it's replaced and the replaced text is saved to the kill ring."
  (interactive "P")
  (let* ((raw-prefix (equal arg '(4)))
         (numeric-prefix (and (integerp arg) (> arg 0)))
         (repeat-count (if numeric-prefix arg 1))
         (text-to-paste (if raw-prefix
                            (current-kill (if (listp last-command-event)
                                              0
                                            (mod (- (aref (this-command-keys) 0) ?0)
                                                 kill-ring-max))
                                          t)
                          (or (gui-get-selection 'CLIPBOARD)
                              (current-kill 0 t))))
         (ends-with-newline (string-suffix-p "\n" text-to-paste))
         (full-line-p (and ends-with-newline
                           (string-match-p "^\n*.*\n$" text-to-paste)
                           (or (bolp) (not (region-active-p))))))
    ;; If region is active, kill it first to update the kill ring
    (when (region-active-p)
      (let ((region-text (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (kill-new region-text)))

    ;; Move forward only if at the start of a line and pasting something with a newline
    ;; AND we're not at the end of the buffer
    (when (and (bolp) ends-with-newline (not (eobp)))
      (forward-char))

    ;; Record initial position for line pastes
    (let ((paste-start-pos (point)))
      ;; Do the paste
      (dotimes (_ repeat-count)
        (if full-line-p
            (progn
              (unless (bolp) (forward-line) (beginning-of-line))
              (setq paste-start-pos (point))
              (insert text-to-paste)
              (when (and (not (bolp)) (> repeat-count 1))
                (forward-line)))
          (insert text-to-paste)))

      ;; Fix cursor position for line pastes to be more Vim-like
      (when (and full-line-p ends-with-newline)
        (goto-char paste-start-pos)
        (back-to-indentation)))))


;; It's a simplified version that doesn't work on a selection
(defun my/meow-replace-char ()
  "Replace character with input character, like Vim's r."
  (interactive)
  (let ((char (read-char "Replace with: ")))
    (delete-char 1)
    (insert-char char)
    (backward-char)))


(defvar my/last-selection-start nil "Start position of last visual selection.")
(defvar my/last-selection-end nil "End position of last visual selection.")
(defvar my/last-selection-type nil "Type of last visual selection.")
(defvar my/last-selection-direction nil "Direction of last visual selection.")

(defun my/save-selection (&rest _)
  "Save the current selection coordinates."
  (when (and (region-active-p)
             (meow--selection-type))  ; Only save if we have both region and type
    (let ((start (region-beginning))
          (end (region-end))
          (type (meow--selection-type))
          (direction (if (< (mark) (point)) 'forward 'backward)))
      (message "Saving selection: start=%s end=%s type=%s dir=%s"
               start end type direction)
      (setq my/last-selection-start start
            my/last-selection-end end
            my/last-selection-type type
            my/last-selection-direction direction))))


(defun my/copy-to-end-of-line ()
  "Copy text from the current cursor position to the end of the line."
  (interactive)
  (let ((start (point))
        (end (line-end-position)))
    (kill-ring-save start end)))

(defun copy-whole-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2)))

(defun my/meow-delete-to-end-of-line ()
  "Delete from the current cursor position to the end of the line and add to kill ring."
  (interactive)
  (kill-region (point) (line-end-position)))

;; (defun my/meow-delete-to-end-of-line ()
;;   "Delete from the current cursor position to the end of the line."
;;   (interactive)
;;   (delete-region (point) (line-end-position)))

(defun my/meow-change-to-end-of-line ()
  "Delete from the current cursor position to the end of the line, add to kill ring, and enter insert mode."
  (interactive)
  (kill-region (point) (line-end-position))
  (deactivate-mark)
  (meow-insert))

(defun my/meow-line-up (&optional arg)
  "Select lines upward. If ARG is provided, select ARG+1 lines up. Otherwise, select one line up."
  (interactive "P")
  (if arg
      (meow-line (- (1+ (prefix-numeric-value arg))))
    (meow-line -1)))

(defun my/meow-line-down (&optional arg)
  "Select lines downward. If ARG is provided, select ARG+1 lines down. Otherwise, select one line down."
  (interactive "P")
  (if arg
      (meow-line (1+ (prefix-numeric-value arg)))
    (meow-line 1)))

(defun meow-my-go-to-line ()
  (interactive)
  (meow-line 1)
  (avy-goto-line))

(defun my/generic-insert ()
  "Like Vim's insert. Cancels any current selection."
  (interactive)
  (deactivate-mark)
  (meow--switch-state 'insert))

(defun meow-insert-line-start ()
  "Like Vim's I: move to first non-whitespace character and enter insert mode."
  (interactive)
  (back-to-indentation)
  (meow-insert))

(defun my/generic-append ()
  "Like Vim's append: move forward one character then enter insert mode, without crossing lines."
  (interactive)
  (unless (eolp)
    (forward-char))
  (meow-insert))

(defun my/meow-append ()
  "If selection is active, use meow-append. If not, use generic append."
  (interactive)
  (if (region-active-p)
      (meow-append)
    (my/generic-append)))

(defun meow-append-line-end ()
  "Like Vim's A: move to end of line and enter insert mode."
  (interactive)
  (end-of-line)
  (meow-insert))

(defun meow-goto-first-line ()
  "Like Vim's gg: go to the first line of buffer."
  (interactive)
  (goto-char (point-min))
  (back-to-indentation))

(defun meow-goto-last-line ()
  "Like Vim's G: go to the last line of buffer."
  (interactive)
  (goto-char (point-max))
  (back-to-indentation))

(defun my/set-mark-command-inclusive ()
  "Set mark command that's always inclusive regardless of direction."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (push-mark (point) t t)))

(defun my/meow-find (char)
  "Enhanced meow-find that handles the case when cursor is already at CHAR."
  (interactive (list (read-char "Find: " t)))
  (if (and (char-after) (= (char-after) char))
      ;; If cursor is already on the character, pass the expand flag.
      (meow-find 2 char t)
    ;; Otherwise, just call meow-find normally.
    (meow-find 1 char)))

(defun my/meow-search-backward ()
  "Search backward for the last search string."
  (interactive)
  (meow--search-intern -1))

(defun my-multi-comment-dwim (arg)
  "Comment or uncomment lines ARG times."
  (interactive "p")
  (dotimes (_ arg)
    (comment-dwim nil)))

;; Bind it to your preferred key
(global-set-key (kbd "C-x C-;") 'my-multi-comment-dwim)

(defvar my/motion-alist
  '((?j . next-line)
    (?k . previous-line)
    (?g . (((?g . beginning-of-buffer))))
    (?G . end-of-buffer)
    (?\M-{ . backward-paragraph)
    (?\M-} . forward-paragraph)
    (?\C-\M-a . beginning-of-defun)
    (?\C-\M-e . end-of-defun))
  "Alist of line-wise motions.")

(defun my/get-motion-info (key)
  "Get motion info for a key, handling multi-char sequences."
  (let ((motion-info (alist-get key my/motion-alist)))
    (if (and (listp motion-info)
             (listp (car motion-info)))
      (let ((next-key (read-key)))
        (alist-get next-key (car motion-info)))
      motion-info)))

(defun my/get-line-bounds (motion-cmd &optional count)
  "Get the bounds for line-wise motion."
  (let ((count (or count 1)))
    (save-excursion
      (cond
       ;; Handle gg (beginning of buffer)
       ((eq motion-cmd 'beginning-of-buffer)
        (cons (point-min)
              (min (point-max)
                   (1+ (line-end-position)))))

       ;; Handle G (end of buffer)
       ((eq motion-cmd 'end-of-buffer)
        (cons (line-beginning-position)
              (point-max)))

       ;; Handle k (up)
       ((eq motion-cmd 'previous-line)
        (cons (save-excursion
                (forward-line (- count))
                (line-beginning-position))
              (min (point-max)
                   (1+ (line-end-position)))))


       ;; Handle j (down) with special last-line handling
       ((eq motion-cmd 'next-line)
        (let ((start (line-beginning-position))
              (end (save-excursion
                     (forward-line (1+ count))
                     (if (eobp)
                         (point)  ; If at buffer end, use current point
                       (min (point-max)
                            (line-beginning-position))))))
          (cons start end)))

       ;; Handle M-{ (backward-paragraph)
       ((eq motion-cmd 'backward-paragraph)
        (let ((start (save-excursion
                       (backward-paragraph count)
                       (line-beginning-position)))
              (end (line-end-position))) ; Ensure the current line is included
          (cons start end)))

       ;; Handle M-} (forward-paragraph)
       ((eq motion-cmd 'forward-paragraph)
        (cons (line-beginning-position)
              (progn
                (forward-paragraph count)
                (line-end-position))))

       ;; Handle C-M-a (beginning-of-defun)
       ((eq motion-cmd 'beginning-of-defun)
        (let ((start (save-excursion
                       (beginning-of-defun count)
                       (line-beginning-position)))
              (end (line-end-position))) ; Include the current line
          (cons start end)))

       ;; Handle C-M-e (end-of-defun)
       ((eq motion-cmd 'end-of-defun)
        (cons (line-beginning-position)
              (progn
                (end-of-defun count)
                (line-end-position))))))))

(defun my/handle-operator (operator &optional special-handler)
  "Handle motion selection and apply operator."
  (let ((key (read-key "Enter motion: "))
        (current-column (current-column)))
    (cond
     ((and special-handler (funcall special-handler key current-column)))

     ((and (>= key ?0) (<= key ?9))
      (let* ((num-str (string key))
             (next-key (read-key))
             (_ (while (and (>= next-key ?0) (<= next-key ?9))
                  (setq num-str (concat num-str (string next-key))
                        next-key (read-key))))
             (count (string-to-number num-str))
             (motion-cmd (my/get-motion-info next-key)))
        (when motion-cmd
          (let* ((bounds (my/get-line-bounds motion-cmd count))
                 (start (car bounds))
                 (end (cdr bounds)))
            (funcall operator start end)
            (when current-column
              (move-to-column current-column))))))

     (t
      (when-let* ((motion-cmd (my/get-motion-info key)))
        (let* ((bounds (my/get-line-bounds motion-cmd 1))
               (start (car bounds))
               (end (cdr bounds)))
          (funcall operator start end)
          (when current-column
            (move-to-column current-column))))))))

(defun my/make-special-handler (key-char operation)
  "Create a special handler for a given key and operation."
  (lambda (key _)
    (when (eq key key-char)
      (funcall operation (line-beginning-position) (line-end-position))
      t)))

(defun my/make-smart-operator (operation special-key)
  "Create a smart operator function for a given operation and special key."
  (lambda ()
    (interactive)
    (if (region-active-p)
        (funcall operation (region-beginning) (region-end))
      (my/handle-operator operation
                          (my/make-special-handler special-key operation)))))

;; Define the smart operators directly
(defalias 'my/meow-smart-comment (my/make-smart-operator #'comment-or-uncomment-region ?c))
(defalias 'my/meow-smart-fill (my/make-smart-operator #'fill-region ?w))
(defalias 'my/meow-smart-indent (my/make-smart-operator #'indent-region ?=))
;; (defalias 'my/meow-smart-indent (my/make-smart-operator #'indent-region ?=))

;; Special handlers for each operator
(defun my/delete-special-handler (key column)
  "Handle special cases for delete operator."
  (when (eq key ?d)
    (kill-region (line-beginning-position)
                 (min (point-max)
                      (1+ (line-end-position))))
    (move-to-column column)
    t))

;; Operator functions with their special cases
;; (defun my/meow-smart-delete ()
;;   "Enhanced delete command with Vim-like behavior."
;;   (interactive)
;;   (if (region-active-p)
;;       (my/generic-meow-smart-delete)
;;     (my/handle-operator 'kill-region #'my/delete-special-handler)))

(defun my/generic-meow-smart-delete ()
  "If selection is active, use meow-kill. If not, use meow-delete."
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-delete)))

(defvar my/last-deleted-text nil
  "Stores the last deleted text.")

(defun my/kill-region-and-save (beg end)
  "Kill the region between BEG and END and save it to `my/last-deleted-text`."
  (setq my/last-deleted-text (buffer-substring-no-properties beg end))
  (kill-region beg end))

(defun my/meow-smart-delete ()
  "Enhanced delete command with Vim-like behavior."
  (interactive)
  (if (region-active-p)
      (my/generic-meow-smart-delete)
    (my/handle-operator 'my/kill-region-and-save #'my/delete-special-handler)
    (when my/last-deleted-text
      (kill-new my/last-deleted-text)
      (setq my/last-deleted-text nil))))


;; I found a bug that after using rectangle mode with my current settings,
;; meow-change stops going into the insert mode.
(defun my/meow-change ()
  "Delete the active region and enter `meow-insert` mode."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (meow-insert))

(defun my/meow-smart-change ()
  "Enhanced change command with Vim-like behavior."
  (interactive)
  (if (region-active-p)
      (my/meow-change)
    (my/handle-operator 'my/change-operator #'my/change-special-handler)))

(defun my/change-operator (start end)
  "Delete region and enter insert mode."
  (let ((indentation (save-excursion
                      (goto-char start)
                      (current-indentation)))
        (line-wise (save-excursion
                    (goto-char start)
                    (= start (line-beginning-position)))))
    (delete-region start end)
    (when line-wise
      (insert "\n")  ; Insert a new line for line-wise operations
      (forward-line -1))  ; Move to the empty line
    (when (looking-at-p "^$")
      (indent-to indentation))
    (meow-insert)))

(defun my/change-special-handler (key _)
  "Handle special cases for change operator."
  (when (eq key ?c)
    (let ((indentation (current-indentation)))
      (delete-region (line-beginning-position)
                    (line-end-position))
      (indent-to indentation)
      (meow-insert))
    t))

(defun my/save-special-handler (key _)
  "Handle special cases for yank operator."
  (when (eq key ?y)
    (kill-ring-save (line-beginning-position)
                    (min (point-max)
                         (1+ (line-end-position))))
    t))

(defun my/meow-smart-save ()
  "Enhanced save (yank) command with Vim-like behavior."
  (interactive)
  (if (region-active-p)
      (meow-save)
    (my/handle-operator 'kill-ring-save #'my/save-special-handler)))

(defun my/meow-delete-insert ()
  "Uses meow-delete and meow-insert sequentially"
  (interactive)
  (if (region-active-p)
      (progn
        (meow-cancel-selection)
        (meow-delete)
        (meow-insert))
    (progn
      (meow-delete)
      (meow-insert))))

(defun my/generic-meow-smart-delete ()
  "If selection is active, use meow-kill. If not, use meow-delete."
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-delete)))

(defun avy-goto-char-all-windows ()
  "Invoke `avy-goto-char` across all windows in the current frame, except in Dired buffers."
  (interactive)
  (let ((avy-all-windows t))
    (unless (derived-mode-p 'dired-mode)
      (call-interactively 'avy-goto-char))))

(defun my/conditional-search-or-avy ()
  "Use `evil-search-forward` in Dired buffers, otherwise use `avy-goto-char-all-windows`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (isearch-forward)
    (avy-goto-char-all-windows)))
    ;; (call-interactively 'meow-mark-word)))


;; (defun forward-symbol-no-dot (n)
;;   "Move forward N symbols, treating standalone dots as delimiters
;; but preserving dots within identifiers and strings."
;;   (interactive "p")
;;   (let ((count (abs n))
;;         (direction (if (< n 0) -1 1)))
;;     (dotimes (_ count)
;;       (if (> direction 0)
;;           ;; Moving forward
;;           (progn
;;             (skip-syntax-forward " ")
;;             (when (and (eq (char-after) ?.)
;;                        (looking-at "\\.\\s-")) ; dot followed by whitespace
;;               (forward-char)
;;               (skip-syntax-forward " "))
;;             (unless (eobp)
;;               (if (derived-mode-p 'org-mode)
;;                   (with-syntax-table (copy-syntax-table (syntax-table))
;;                     (modify-syntax-entry ?. "w")
;;                     (forward-symbol 1))
;;                 (forward-symbol 1))))
;;         ;; Moving backward
;;         (progn
;;           (skip-syntax-backward " ")
;;           (when (and (eq (char-before) ?.)
;;                      (looking-back "\\s-\\." (max (point-min) (- (point) 2))))
;;             (backward-char)
;;             (skip-syntax-backward " "))
;;           (unless (bobp)
;;             (if (derived-mode-p 'org-mode)
;;                 (with-syntax-table (copy-syntax-table (syntax-table))
;;                   (modify-syntax-entry ?. "w")
;;                   (forward-symbol -1))
;;               (forward-symbol -1))))))))

;; (defun backward-symbol-no-dot (n)
;;   "Move backward N symbols, treating standalone dots as delimiters."
;;   (forward-symbol-no-dot (- n)))

;; (put 'symbol-no-dot 'forward-op 'forward-symbol-no-dot)
;; (put 'symbol-no-dot 'backward-op 'backward-symbol-no-dot)

;; (defvar meow-symbol-no-dot-thing 'symbol-no-dot
;;   "The thing used for marking and movement by symbols when ignoring standalone dots.")

;; (defun meow-next-symbol-no-dot (n)
;;   "Select to the end of the next Nth symbol, ignoring standalone dots."
;;   (interactive "p")
;;   (meow-next-thing meow-symbol-no-dot-thing 'symbol n))

;; (defun meow-back-symbol-no-dot (n)
;;   "Select to the beginning of the previous Nth symbol, ignoring standalone dots."
;;   (interactive "p")
;;   (meow-next-thing meow-symbol-no-dot-thing 'symbol (- n)))


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   ;; '("B" . meow-back-symbol-no-dot)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   ;; '("E" . meow-next-symbol-no-dot)
   ;; '("f" . meow-find)
   '("f" . my/meow-find)
   '("t" . meow-till)
   '("T" . my/meow-till-backward)
   '("F" . my/meow-find-backward)
   '("^" . my/meow-line-beginning)
   '("$" . my/meow-line-end)
   ;; '("0" . my/meow-line-start)
   ;; '("0" . my-meow-line-or-digit-0)
   '("g" . nil)
   ;; '("g ;" . my/new-goto-last-change)
   ;; '("g :" . my/new-goto-next-changee)
   '("g ;" . goto-last-change)
   '("g :" . goto-last-change-reverse)
   '("g v" . (lambda () (interactive) (set-mark-command 4)))
   '("g c" . my/meow-smart-comment)
   '("g g" . beginning-of-buffer)
   ;; '("g z" . my/zoxide-switch)
   '("g z" . my/dir-switch)
   '("G" . end-of-buffer)
   ;; '("h" . meow-left)
   '("h" . backward-char)
   '("i" . meow-insert)
   '("j" . meow-next)
   '("k" . meow-prev)
   ;; '("l" . meow-right) ;; For some reason it works strangely in dired
   '("l" . forward-char)
   '("n" . my/search-next)
   '("N" . my/search-previous)
   '("v" . my/meow-line-up)
   '("V" . my/meow-line-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("y" . my/meow-smart-save)
   '("Y" . my/copy-to-end-of-line)
   ;; '("'" . meow-find-and-select-inner)
   ;; '("\"" . meow-find-and-select-outer)
   '("'" . find-and-select-inner)
   '("\"" . find-and-select-outer)
   ;; '("C-y" . my/yank-with-selection)
   ;; '(":" . execute-extended-command)
   '("<escape>" . meow-cancel-selection))
   ;; '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   ;; '("1" . meow-digit-argument)
   ;; '("2" . meow-digit-argument)
   ;; '("3" . meow-digit-argument)
   ;; '("4" . meow-digit-argument)
   ;; '("5" . meow-digit-argument)
   ;; '("6" . meow-digit-argument)
   ;; '("7" . meow-digit-argument)
   ;; '("8" . meow-digit-argument)
   ;; '("9" . meow-digit-argument)
   ;; '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  ;; (meow-insert-define-key
  ;;  '("C-w" . backward-kill-word)
  ;;  )
  (meow-normal-define-key
   ;; '("0" . meow-expand-0)
   ;; '("9" . meow-expand-9)
   ;; '("8" . meow-expand-8)
   ;; '("7" . meow-expand-7)
   ;; '("6" . meow-expand-6)
   ;; '("5" . meow-expand-5)
   ;; '("4" . meow-expand-4)
   ;; '("3" . meow-expand-3)
   ;; '("2" . meow-expand-2)
   ;; '("1" . meow-expand-1)
   '("1" . (lambda () (interactive) (my-meow-digit 1)))
   '("2" . (lambda () (interactive) (my-meow-digit 2)))
   '("3" . (lambda () (interactive) (my-meow-digit 3)))
   '("4" . (lambda () (interactive) (my-meow-digit 4)))
   '("5" . (lambda () (interactive) (my-meow-digit 5)))
   '("6" . (lambda () (interactive) (my-meow-digit 6)))
   '("7" . (lambda () (interactive) (my-meow-digit 7)))
   '("8" . (lambda () (interactive) (my-meow-digit 8)))
   '("9" . (lambda () (interactive) (my-meow-digit 9)))
   '("0" . (lambda () (interactive) (my-meow-digit 0)))
   ;; '("0" . (lambda () (interactive) (my-meow-line-or-digit-0)))
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; '("%" . my-match-paren-with-selection)
   ;; '("a" . my/meow-append)
   '("a" . my/generic-append)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   ;; '("B" . meow-back-symbol-no-dot)
   ;; '("c" . meow-change)
   '("c" . my/meow-smart-change)
   ;; '("d" . meow-delete)
   ;; '("d" . my/generic-meow-smart-delete)
   '("d" . my/meow-smart-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   ;; '("E" . meow-next-symbol-no-dot)
   ;; '("f" . meow-find)
   '("f" . my/meow-find)
   ;; '("f" . find-char-in-line-forward)
   '("t" . meow-till)
   '("T" . my/meow-till-backward)
   '("F" . my/meow-find-backward)
   ;; '("g" . meow-cancel-selection)
   ;; '("^" . my/meow-line-beginning)
   ;; '("$" . my/meow-line-end)
   ;; '("0" . my/meow-line-start)
   '("g" . nil)
   '("g ;" . goto-last-change)
   '("g :" . goto-last-change-reverse)
   ;; '("g ;" . my-goto-last-change)
   ;; '("g :" . my-goto-last-change-reverse)
   ;; '("g ;" . my/new-goto-last-change)
   ;; '("g :" . my/new-goto-next-change)
   '("g v" . (lambda () (interactive) (set-mark-command 4)))
   '("g c" . my/meow-smart-comment)
   '("g w" . my/meow-smart-fill)
   '("g g" . beginning-of-buffer)
   ;; '("g z" . my/zoxide-switch)
   '("g z" . my/dir-switch)
   '("G" . end-of-buffer)
   '("s s" . surround-region-with-symbol)
   '("s c" . change-surrounding-symbol)
   '("s d" . delete-surrounding-symbol)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   ;; '("i" . meow-insert)
   '("i" . my/generic-insert)
   ;; '("A" . meow-open-below)
   ;; '("I" . meow-open-above)
   '("A" . meow-append-line-end)
   '("I" . meow-insert-line-start)
   ;; '("j" . meow-next)
   '("j" . my/simple-next-line)
   ;; '("j" . next-line)
   '("J" . meow-next-expand)
   ;; '("k" . meow-prev)
   '("k" . my/simple-previous-line)
   ;; '("k" . previous-line)
   ;; '("K" . meow-prev-expand)
   ;; '("K" . eldoc-print-current-symbol-info)
   '("K" . my-eldoc-print-and-switch)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   ;; '("m" . meow-block)
   '("m" . rectangle-mark-mode)
   ;; '("m" . meow-join)
   '("n" . my/search-next)
   '("N" . my/search-previous)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   ;; '("o" . meow-block)
   ;; '("O" . meow-to-block)
   ;; '("p" . meow-yank)
   '("p" . my/meow-smart-paste)
   ;; '("P" . my/meow-smart-paste-no-save)
   '("P" . my/super-smart-paste)
   ;; '("P" . placeholder)
   ;; '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . my/meow-replace-char)
   '("R" . dot-mode-execute)
   ;; '("s" . meow-kill)
   '("z" . my/meow-delete-insert)
   ;; '("S" . meow-kill-whole-line)
   ;; '("u" . meow-undo)
   ;; '("U" . meow-undo-in-selection)
   '("U" . my/toggle-case-based-on-first)
   '("u" . undo-fu-only-undo)
   ;; '("v" . meow-visit)
   ;; '("v" . my/meow-line-up)
   '("v" . my/select-line-for-up)
   ;; '("V" . my/meow-line-down)
   '("V" . my/select-line-for-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   ;; '("x" . meow-line)
   '("x" . my/delete-char-to-kill-ring)
   ;; '("y" . meow-save)
   '("y" . my/meow-smart-save)
   '("Y" . meow-sync-grab)
   '(">" . my/indent-region-right)
   '("<" . my/indent-region-left)
   ;; '("M" . mc/edit-lines)
   '("C" . my/meow-change-to-end-of-line)
   '("Y" . my/copy-to-end-of-line)
   '("D" . my/meow-delete-to-end-of-line)
   '("C-." . my-replay-commands)
   ;; '("C-." . my-replay-last-operation)
   ;; '("C-r" . undo-tree-redo)
   '("C-r" . undo-fu-only-redo)
   ;; '("C-r" . undo-redo)
   ;; '("C-M-m" . toggle-messages-buffer) ;; It's transaltes to M-RET
   '("M-y" . toggle-flymake-diagnostics)
   ;; '("'" . repeat)
   ;; '("'" . meow-find-and-select-inner)
   ;; '("\"" . meow-find-and-select-outer)
   '("'" . find-and-select-inner)
   '("\"" . find-and-select-outer)
   ;; '("/" . my/conditional-search-or-avy)
   ;; '("/" . avy-goto-char-all-windows)
   '("/" . isearch-forward)
   '("?" . isearch-backward)
   '("M-v" . scroll-up-and-recenter)
   '("C-v" . scroll-down-and-recenter)
   '("C-M-y" . save-and-paste)
   ;; '("C-y" . my/yank-with-selection)
   '("C-f" . my-forward-char-with-selection)
   '("C-b" . my-backward-char-with-selection)
   '("=" . my/meow-smart-indent)
   ;; '(":" . execute-extended-command)
   ;; '("C-d" . scroll-half-up-and-recenter)
   ;; '("C-u" . scroll-half-down-and-recenter)
   '("<escape>" . meow-cancel-selection)))
   ;; '("<escape>" . ignore)))

(defun meow-magit-mode ()
  "Custom Meow state for Magit modes."
  (meow--switch-state 'magit)
  (setq meow--current-state 'magit))

(meow-define-state magit
  "Custom state for Magit."
  ;; Use the full keyboard
  :lighter " [MG]"
  :keymap (make-keymap))

(with-eval-after-load 'meow
  (setq meow-mode-state-list
        (append '((messages-buffer-mode . normal)
                  (help-mode . normal)
                  (helpful-mode . normal)
                  (Info-mode . normal)
                  (special-mode . normal)
                  (shell-command-mode . normal)
                  (debugger-mode . normal)
                  (emacs-lisp-compilation-mode . normal)
                  (magit-status-mode . magit)
                  (magit-log-mode . magit)
                  (magit-diff-mode . magit)
                  (magit-process-mode . magit)
                  (magit-stash-mode . magit)
                  (magit-refs-mode . magit)
                  (dired-mode . magit)
                  (daemons-mode . magit)
                  (docker-container-mode . motion)
                  (docker-image-mode . motion)
                  (docker-network-mode . motion)
                  (docker-volume-mode . motion)
                  (docker-context-mode . motion)
                  (docker-machine-mode . motion))
                meow-mode-state-list)))

(with-eval-after-load 'dired
  (define-prefix-command 'my-dired-g-map)
  (define-key dired-mode-map (kbd "g") 'my-dired-g-map)
  (define-key my-dired-g-map (kbd "z") 'my/dir-switch)
  (define-key my-dired-g-map (kbd "g") 'beginning-of-buffer)
  ;; (define-key dired-mode-map (kbd "G") 'end-of-buffer)
  (define-key dired-mode-map (kbd "n") 'my/search-next)
  (define-key dired-mode-map (kbd "N") 'my/search-previous)
  (define-key dired-mode-map (kbd "G") 'dired-goto-last-line)
  (define-key dired-mode-map (kbd "N") 'my/search-previous)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "SPC") 'my-space-as-ctrl-c)
  (define-key dired-mode-map (kbd "/") 'isearch-forward)
  (define-key dired-mode-map (kbd "?") 'isearch-backward)
  ;; (define-key dired-mode-map (kbd "/") 'avy-goto-char-all-windows)
  (define-key dired-mode-map (kbd "W") 'Cpn)
  ;; (define-key daemons-mode-map (kbd ":") 'execute-extended-command)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(with-eval-after-load 'magit
  ;; (define-key magit-status-mode-map (kbd "j") 'magit-section-forward)
  ;; (define-key magit-mode-map (kbd "k") 'magit-section-backward)
  ;; (define-key daemons-mode-map (kbd ":") 'execute-extended-command)
  (define-key magit-mode-map (kbd "SPC") 'my-space-as-ctrl-c))

(with-eval-after-load 'daemons
  ;; (define-key daemons-mode-map (kbd "s") 'daemons-start-at-point)
  (define-key daemons-mode-map (kbd "j") 'next-line)
  (define-key daemons-mode-map (kbd "k") 'previous-line)
  ;; (define-key daemons-mode-map (kbd ":") 'execute-extended-command)
  (define-key daemons-mode-map (kbd "?") 'my/show-daemon-bindings))

(add-hook 'compilation-mode-hook
          (lambda ()
            (define-key compilation-mode-map (kbd "/") 'my/conditional-search-or-avy)))

;; Conciliate meow with the rectangle mode
(define-minor-mode my-rectangle-override-mode
  "Override Meow keybindings in `rectangle-mark-mode`."
  :lighter " Rect-Override"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "e") 'forward-word)
            (define-key map (kbd "E") 'forward-symbol)
            (define-key map (kbd "b") 'backward-word)
            (define-key map (kbd "B") 'backward-symbol)
            (define-key map (kbd "l") 'forward-char)
            (define-key map (kbd "h") 'backward-char)
			(define-key map (kbd "t") 'meow-till-expand)
            (define-key map (kbd "f") 'meow-find-expand)
            (define-key map (kbd "y") 'my/meow-smart-save)
			(define-key map (kbd "p") 'my/rectangle-smart-paste-alt)
			(define-key map (kbd "P") 'my/rectangle-smart-paste-alt)
			(define-key map (kbd "R") 'my/rectangle-replace-with-numbers)
			;; (define-key map (kbd "d") 'backward-delete-char-untabify)
            (define-key map (kbd "d") 'kill-rectangle)
            (define-key map (kbd "i") 'string-rectangle)
            (define-key map (kbd "Y") 'my/copy-rectangle-to-eol)
            (define-key map (kbd "D") 'my/delete-rectangle-to-eol)
            (define-key map (kbd "C") 'my/change-string-rectangle-to-eol)
			(define-key map (kbd "A") 'my/insert-at-eol-rectangle)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "G") 'end-of-buffer)
            (define-key map (kbd "g g") 'beginning-of-buffer)
            map))

(defun my-toggle-rectangle-overrides ()
  "Enable or disable overrides based on rectangle-mark-mode."
  (if rectangle-mark-mode
      (progn
        (meow-global-mode -1)
        (my-rectangle-override-mode 1))
    (progn
      (meow-global-mode 1)
      (my-rectangle-override-mode -1))))

(add-hook 'rectangle-mark-mode-hook #'my-toggle-rectangle-overrides)


(setq meow-paren-keymap (make-keymap))
(meow-define-state paren
  "meow state for interacting with smartparens"
  :lighter " [P]"
  :keymap meow-paren-keymap)

;; meow-define-state creates the variable
;; This variable probably doesn't exist. I took this code from the repo of the
;; package.
(setq meow-cursor-type-paren 'hollow)

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("l" . sp-forward-sexp)
  '("h" . sp-backward-sexp)
  '("j" . sp-down-sexp)
  '("k" . sp-up-sexp)
  '("n" . sp-forward-slurp-sexp)
  '("b" . sp-forward-barf-sexp)
  '("v" . sp-backward-barf-sexp)
  '("c" . sp-backward-slurp-sexp)
  '("u" . meow-undo))

;; Makes functions like meow-next-word to ignore whitespaces
(setq meow-next-thing-include-syntax
      '((word "[:space:]" "[:space:]")
        (symbol "[:space:]" "[:space:]")))

;; This code disables triggering C-x on pressing 'space x'
(setq meow-keypad-describe-delay 3)

(defun my/meow-always-c-c-dispatch ()
  "Make keypad always dispatch to C-c."
  (setq meow--keypad-this "C-c")
  (meow--keypad-start))

(setq meow-keypad-start-keys nil)  ; Clear default special keys
(setq meow-keypad-leader-dispatch "C-c")  ; Set leader to C-c
(setq meow-keypad-self-insert-undefined nil)  ; Don't fall back to undefined

(meow-define-state keypad
  "KEYPAD state"
  :keymap (let ((keymap (make-sparse-keymap)))
            (suppress-keymap keymap t)
            (define-key keymap [remap self-insert-command] #'my/meow-always-c-c-dispatch)
            keymap))


;; Meow has an advice that puts emacs into the motion state, but i need the
;; magit state instead.
(defun my-wdired-finish-edit-advice (&rest _)
  "Switch to the appropriate Meow state after exiting wdired."
  (if (derived-mode-p 'dired-mode)
      (meow--switch-state 'magit) ;; Switch to magit state for dired
    (meow--switch-state 'motion))) ;; Default to motion otherwise

(advice-add 'meow--switch-to-motion :around #'my-wdired-finish-edit-advice)

;; ;; Ignore slashes
;; (defun my-forward-symbol (&optional arg)
;;   "Move forward across one balanced expression.
;; Treats slashes as part of the symbol."
;;   (interactive "^p")
;;   (let ((arg (or arg 1))
;;         (sym-syntax (string-to-syntax "_")))  ; treat slash as symbol constituent
;;     (with-syntax-table (copy-syntax-table (syntax-table))
;;       (modify-syntax-entry ?/ "_")  ; make slash a symbol constituent
;;       (forward-symbol arg))))
;; ;; Register our new symbol movement function
;; (put 'my-symbol 'forward-op #'my-forward-symbol)
;; ;; Tell Meow to use our custom symbol definition
;; (setq meow-symbol-thing 'my-symbol)

(defadvice my/meow-smart-comment (after make-trackable-for-goto-chg activate)
  "Make commenting trackable by goto-chg by performing a small tracked operation."
  (let ((pos (point)))
    ;; Create a small change that goto-chg will detect
    (insert " ")
    (delete-char -1)
    ;; Return to original position
    (goto-char pos)))

(meow-setup)
(meow-global-mode 1)
