;; -*- lexical-binding: t; -*-

(use-package meow
  :ensure t
  :init
  (delete-selection-mode -1)
  (setq meow-use-clipboard t))
  ;; (setq meow-use-dynamic-face-color nil))

;; Argument count doesn't get reset right away when i use meow-next/prev
(defun my/reset-prefix-arg (&rest _)
  "Reset the prefix argument to nil."
  (setq prefix-arg nil)
  (setq current-prefix-arg nil))

(advice-add 'meow-next :after #'my/reset-prefix-arg)
(advice-add 'meow-prev :after #'my/reset-prefix-arg)

;; Delete the 'window' option for selection
(setq meow-char-thing-table
      (assq-delete-all ?w meow-char-thing-table))

;; For number hints to work in org mode
(setq meow-expand-exclude-mode-list 
      (remove 'org-mode meow-expand-exclude-mode-list))


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
  (when (not (eobp))  ; Check if we're not at the end of the buffer
    (kill-region (point) (+ (point) 1))))

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


(defun my-select-window-by-number (number)
  "Select the window specified by NUMBER.
Numbered from top-left to bottom-right."
  (interactive "P")
  (let* ((windows (window-list-1 nil nil t))
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


(define-key meow-insert-state-keymap (kbd "C-w") 'backward-kill-word)

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


(defun surround-region-with-symbol (start end)
  "Surround the region with a symbol input by the user."
  (interactive "r")
  (let* ((input (read-char "Enter symbol: "))
         (char (char-to-string input))
         (pairs '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")))
         (closing (cdr (assoc char pairs))))
    (save-excursion
      (goto-char end)
      (insert (or closing char))
      (goto-char start)
      (insert char))))

(defun change-surrounding-symbol (start end)
  "Change the symbols surrounding the region."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (first-char (substring region-text 0 1))
         (last-char (substring region-text -1))
         (content (substring region-text 1 -1))
         (new-symbol (char-to-string (read-char "Enter new symbol: ")))
         (pairs '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")))
         (closing (cdr (assoc new-symbol pairs))))
    (delete-region start end)
    (insert new-symbol content (or closing new-symbol))))

(defun delete-surrounding-symbol (start end)
  "Delete the symbols surrounding the region."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (content (substring region-text 1 -1)))
    (delete-region start end)
    (insert content)))


(defun my/meow-find-backward (arg ch)
  "Combine negative argument with meow-find to search backward in one keybinding."
  (interactive "p\ncFind backward:")
  (meow-find (- arg) ch))

(defun my/meow-till-backward (arg ch)
  "Combine negative argument with meow-till to search backward in one keybinding."
  (interactive "p\ncTill backward:")
  (meow-till (- arg) ch))

(defun meow-find-and-select-inner (n ch)
  "Find the next N occurrence of CH and select its inner content within current line only.
If no forward match is found, search backward."
  (interactive "p\ncFind and select inner:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (line-start (line-beginning-position))
         (line-end (line-end-position))
         (pos (point))
         forward-pos
         backward-pos
         (thing-char (cond
                      ((memq ch '(?\( ?\))) ?r)
                      ((memq ch '(?\[ ?\])) ?s)
                      ((memq ch '(?\{ ?\})) ?c)
                      ((memq ch '(?' ?\" ?`)) ?g)
                      (t nil))))
    ;; Try forward search first
    (save-excursion
      (setq forward-pos (search-forward ch-str line-end t n)))
    
    ;; If forward search fails, try backward search
    (when (not forward-pos)
      (save-excursion
        (setq backward-pos (search-backward ch-str line-start t n))))
    
    (cond
     ((not (or forward-pos backward-pos))
      (message "char %s not found in current line" ch-str))
     (forward-pos
      (goto-char forward-pos)
      (if (eq ch ?')
          (let ((bounds (meow--parse-single-quote nil)))
            (when bounds
              (set-mark (car bounds))
              (goto-char (cdr bounds))))
        (when thing-char
          (if (memq ch '(?\) ?\] ?\}))
              (progn
                (backward-sexp)
                (forward-char)
                (set-mark (point))
                (backward-char)
                (forward-sexp)
                (backward-char))
            (meow-inner-of-thing thing-char)))))
     (backward-pos
      (goto-char backward-pos)
      (if (eq ch ?')
          (let ((bounds (meow--parse-single-quote nil)))
            (when bounds
              (set-mark (car bounds))
              (goto-char (cdr bounds))))
        (when thing-char
          (if (memq ch '(?\) ?\] ?\}))
              (progn
                (forward-char)
                (backward-sexp)
                (forward-char)
                (set-mark (point))
                (backward-char)
                (forward-sexp)
                (backward-char))
            (meow-inner-of-thing thing-char))))))))

(defun meow-find-and-select-outer (n ch)
  "Find the next N occurrence of CH and select its outer content within current line only.
If no forward match is found, search backward."
  (interactive "p\ncFind and select outer:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (line-start (line-beginning-position))
         (line-end (line-end-position))
         (pos (point))
         forward-pos
         backward-pos
         (pair-char (cond
                     ((eq ch ?\() ?\))
                     ((eq ch ?\)) ?\()
                     ((eq ch ?\[) ?\])
                     ((eq ch ?\]) ?\[)
                     ((eq ch ?\{) ?\})
                     ((eq ch ?\}) ?\{)
                     ((memq ch '(?' ?\" ?`)) ch)
                     (t nil))))
    ;; Try forward search first
    (save-excursion
      (setq forward-pos (search-forward ch-str line-end t n)))
    
    ;; If forward search fails, try backward search
    (when (not forward-pos)
      (save-excursion
        (setq backward-pos (search-backward ch-str line-start t n))))
    
    (cond
     ((not (or forward-pos backward-pos))
      (message "char %s not found in current line" ch-str))
     (forward-pos
      (goto-char forward-pos)
      (if (memq ch '(?' ?\" ?`))
          (let ((end-pos (save-excursion
                           (when (search-forward ch-str line-end t)
                             (point)))))
            (if end-pos
                (progn
                  (set-mark (1- forward-pos))
                  (goto-char end-pos))
              (message "No closing %s found in current line" ch-str)))
        (when pair-char
          (if (memq ch '(?\) ?\] ?\}))
              (progn
                (set-mark (point))
                (condition-case nil
                    (backward-sexp)
                  (scan-error (goto-char line-start)))
                (when (< (point) line-start)
                  (goto-char line-start)))
            (progn
              (backward-char)
              (set-mark (point))
              (condition-case nil
                  (forward-sexp)
                (scan-error (goto-char line-end)))
              (when (> (point) line-end)
                (goto-char line-end)))))))
     (backward-pos
      (goto-char backward-pos)
      (if (memq ch '(?' ?\" ?`))
          (let ((start-pos (save-excursion
                             (when (search-backward ch-str line-start t)
                               (point)))))
            (if start-pos
                (progn
                  (set-mark (1+ backward-pos))
                  (goto-char start-pos))
              (message "No opening %s found in current line" ch-str)))
        (when pair-char
          (if (memq ch '(?\) ?\] ?\}))
              (progn
                (forward-char)
                (set-mark (point))
                (condition-case nil
                    (backward-sexp)
                  (scan-error (goto-char line-start)))
                (when (< (point) line-start)
                  (goto-char line-start)))
            (progn
              (set-mark (point))
              (condition-case nil
                  (forward-sexp)
                (scan-error (goto-char line-end)))
              (when (> (point) line-end)
                (goto-char line-end))))))))))

(defun meow--parse-inside-whitespace (inner)
  "Parse the bounds for inside whitespace selection."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (start (progn
                    (skip-syntax-backward "^-" line-start)
                    (point)))
           (end (progn
                  (skip-syntax-forward "^-" line-end)
                  (point))))
      (cons start end))))

(add-to-list 'meow-char-thing-table '(?w . inside-whitespace))

(advice-add 'meow--parse-inner-of-thing-char :around
            (lambda (orig-fun ch)
              (if (eq ch ?w)
                  (meow--parse-inside-whitespace t)
                (funcall orig-fun ch))))

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

(add-to-list 'meow-char-thing-table '(?' . single-quote))

(advice-add 'meow--parse-inner-of-thing-char :around
            (lambda (orig-fun ch)
              (cond
               ((eq ch ?w)
                (meow--parse-inside-whitespace t))
               ((eq ch ?')
                (meow--parse-single-quote nil))
               (t
                (funcall orig-fun ch)))))

(advice-add 'meow--parse-bounds-of-thing-char :around
            (lambda (orig-fun ch)
              (if (eq ch ?')
                  (meow--parse-single-quote t)
                (funcall orig-fun ch))))

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

(defun my/meow-smart-paste (&optional arg)
  "Paste like Vim, handling both line-wise and regular pastes.
With numeric prefix ARG, paste that many times.
With raw prefix argument (C-u without a number), paste from the kill ring."
  (interactive "P")
  (if (region-active-p)
      (meow-replace)  ; Just use meow-replace for selections
    (let* ((raw-prefix (equal arg '(4)))
           (numeric-prefix (and (integerp arg) (> arg 0)))
           (text (if raw-prefix
                     (current-kill (if (listp last-command-event)
                                       0
                                     (mod (- (aref (this-command-keys) 0) ?0)
                                          kill-ring-max))
                                   t)
                   (current-kill 0 t)))
           (repeat-count (if numeric-prefix arg 1)))
      (dotimes (_ repeat-count)
        (if (string-suffix-p "\n" text)
            (progn
              (forward-line)
              (beginning-of-line)
              (insert text)
              (forward-line -1)
              (beginning-of-line))
          (insert text))))))

;; ;; Work like in vim
;; (defun my/meow-replace-char ()
;;   "Replace character(s) with input character, like Vim's r."
;;   (interactive)
;;   (let ((char (read-char "Replace with: ")))
;;     (if (region-active-p)
;;         (let ((start (region-beginning))
;;               (end (region-end)))
;;           (delete-region start end)
;;           (insert (make-string (- end start) char)))
;;       (delete-char 1)
;;       (insert-char char)
;;       (backward-char))))

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
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (type (meow--selection-type))
           (direction (if (< (mark) (point)) 'forward 'backward)))
      (setq my/last-selection-start start
            my/last-selection-end end
            my/last-selection-type type
            my/last-selection-direction direction)
               start end type direction
               (buffer-substring-no-properties start end))))

(defun my/restore-selection ()
  "Restore the last saved selection, similar to Vim's gv."
  (interactive)
  (when (and my/last-selection-start my/last-selection-end 
             my/last-selection-type my/last-selection-direction)
    (let* ((back (eq my/last-selection-direction 'backward))
           (bounds (cons my/last-selection-start my/last-selection-end))
           (selection (meow--make-selection '(select . transient)
                                            (if back (cdr bounds) (car bounds))
                                            (if back (car bounds) (cdr bounds)))))
      (meow--select selection)
      (setq meow--selection-type my/last-selection-type)
               (region-beginning) (region-end) 
               my/last-selection-type my/last-selection-direction
               (buffer-substring-no-properties (region-beginning) (region-end)))))

;; Add advice to save selection before deactivating
(advice-add 'meow--cancel-selection :before #'my/save-selection)

(defun my/copy-to-end-of-line ()
  "Copy text from the current cursor position to the end of the line."
  (interactive)
  (kill-ring-save (point) (line-end-position)))

(defun copy-whole-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2)))

(defun my/meow-delete-to-end-of-line ()
  "Delete from the current cursor position to the end of the line and add to kill ring."
  (interactive)
  (kill-region (point) (line-end-position)))

(defun my/meow-change-to-end-of-line ()
  "Delete from the current cursor position to the end of the line, add to kill ring, and enter insert mode."
  (interactive)
  (kill-region (point) (line-end-position))
  (meow-insert))

(defun my/meow-delete-to-end-of-line ()
  "Delete from the current cursor position to the end of the line."
  (interactive)
  (delete-region (point) (line-end-position)))

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

(defun my/generic-append ()
  "Like Vim's append: move forward one character then enter insert mode."
  (interactive)
  (forward-char)
  (meow-insert))

(defun my/meow-append ()
  "If selection is active, use meow-kill. If not, use meow-delete."
  (interactive)
  (if (not (and meow--expand-nav-function
                (region-active-p)
                (meow--selection-type)))
      (my/generic-append)
    (meow-append)))

(defun meow-insert-line-start ()
  "Like Vim's I: move to first non-whitespace character and enter insert mode."
  (interactive)
  (back-to-indentation)
  (meow-insert))

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
       
       ;; Handle j (down)
       ((eq motion-cmd 'next-line)
        (cons (line-beginning-position)
              (save-excursion
                (forward-line (1+ count))
                (min (point-max)
                     (line-beginning-position)))))

       ;; Handle M-{ (backward-paragraph)
       ((eq motion-cmd 'backward-paragraph)
        (cons (progn
                (backward-paragraph count)
                (line-beginning-position))
              (line-end-position)))

       ;; Handle M-} (forward-paragraph)
       ((eq motion-cmd 'forward-paragraph)
        (cons (line-beginning-position)
              (progn
                (forward-paragraph count)
                (line-end-position))))

       ;; Handle C-M-a (beginning-of-defun)
       ((eq motion-cmd 'beginning-of-defun)
        (cons (progn
                (beginning-of-defun count)
                (line-beginning-position))
              (line-end-position)))

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

(defun my/meow-smart-change ()
  "Enhanced change command with Vim-like behavior."
  (interactive)
  (if (region-active-p)
      (meow-change)
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

;; (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (setq meow--kbd-kill-region "C-w C-w")
;;   (meow-motion-overwrite-define-key
;;    '("j" . meow-next)
;;    '("x" . meow-line)
;;    '("k" . meow-prev))
;;    ;; '("<escape>" . ignore))
;;   (meow-leader-define-key
;;    ;; SPC j/k will run the original command in MOTION state.
;;    ;; '("j" . "H-j")
;;    ;; '("k" . "H-k")
;;    ;; Use SPC (0-9) for digit arguments.
;;    ;; '("1" . meow-digit-argument)
;;    ;; '("2" . meow-digit-argument)
;;    ;; '("3" . meow-digit-argument)
;;    ;; '("4" . meow-digit-argument)
;;    ;; '("5" . meow-digit-argument)
;;    ;; '("6" . meow-digit-argument)
;;    ;; '("7" . meow-digit-argument)
;;    ;; '("8" . meow-digit-argument)
;;    ;; '("9" . meow-digit-argument) 
;;    ;; '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("C-f" . scroll-up-and-recenter)
;;    '("C-b" . scroll-down-and-recenter)
;;    '("C-d" . scroll-half-up-and-recenter)
;;    '("C-u" . scroll-half-down-and-recenter)
;;    ;; '("C-y" . copy-whole-line)
;;    ;; '("C-x c" . my/smart-comment)
;;    ;; '("gc" . my/smart-comment)
;;    '("gw" . my-fill-region)
;;    ;; '("P" . save-and-paste)
;;    '("C" . my/meow-change-to-end-of-line)
;;    '("/" . avy-goto-char)
;;    '("X" . my/meow-revers-line)
;;    '("T" . my/meow-till-backward)
;;    '("F" . my/meow-find-backward)
;;    ;; '("0" . meow-expand-0)
;;    ;; '("9" . meow-expand-9)
;;    ;; '("8" . meow-expand-8)
;;    ;; '("7" . meow-expand-7)
;;    ;; '("6" . meow-expand-6)
;;    ;; '("5" . meow-expand-5)
;;    ;; '("4" . meow-expand-4)
;;    ;; '("3" . meow-expand-3)
;;    ;; '("2" . meow-expand-2)
;;    ;; '("1" . meow-expand-1)
;;    '("0" . meow-digit-argument)
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("-" . negative-argument)
;;    ;; '(";" . meow-reverse)
;;    '(";" . meow-expand-1)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    ;; '("a" . meow-append)
;;    '("a" . my-append)
;;    '("i" . meow-insert)
;;    '("A" . meow-append-line-end)
;;    '("I" . meow-insert-line-start)
;;    '("gg" . meow-goto-first-line)
;;    '("G" . meow-goto-last-line)
;;    ;; '("A" . meow-open-below)
;;    ;; '("I" . meow-open-above)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    ;; '("g" . meow-cancel-selection)
;;    ;; '("G" . meow-grab)
;;    '("H" . meow-left-expand)
;;    ;; '("j" . meow-next)
;;    ;; '("k" . meow-prev)
;;    '("j" . next-line)
;;    '("k" . previous-line)
;;    '("J" . meow-next-expand)
;;    ;; '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("h" . left-char)
;;    '("l" . right-char)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    ;; '("o" . meow-block)
;;    ;; '("O" . meow-to-block)
;;    '("o" . meow-open-below)
;;    '("O" . meow-open-above)
;;    '("p" . vim-like-paste)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    ;; '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    ;; '("x" . meow-line)
;;    ;; '("V" . meow-line)
;;    ;; '("v" . set-mark-command)
;;    '("v" . my/set-mark-command-inclusive)
;;    '("V" . meow-line)
;;    ;; '("X"
;;    '("y" . meow-save)
;;    ;; '("Y" . meow-sync-grab)
;;    '("Y" . my/copy-to-end-of-line)
;;    ;; '("z" . meow-pop-selection)
;;    '("\"" . meow-pop-selection)
;;    '("z" . meow-find-and-select-inner)
;;    '("Z" . my/meow-find-backward-and-select-inner)
;;    '("'" . repeat)
;;    '("?" . meow-my-go-to-line)
;;    ;; '("<escape>" . ignore)))
;;    '("<escape>" . meow-cancel-selection)))

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
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("t" . meow-till)
   '("T" . my/meow-till-backward)
   '("F" . my/meow-find-backward)
   '("^" . my/meow-line-beginning)
   '("$" . my/meow-line-end)
   ;; '("0" . my/meow-line-start)
   ;; '("0" . my-meow-line-or-digit-0)
   '("g" . nil)
   '("g ;" . goto-last-change)
   '("g v" . my/restore-selection)
   '("g c" . my/meow-smart-comment)
   '("g g" . beginning-of-buffer)
   '("g z" . zoxide-travel)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("i" . meow-insert)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right) ;; For some reason it works strangely in dired
   '("n" . my/search-next)
   '("N" . my/search-previous)
   '("v" . my/meow-line-up)
   '("V" . my/meow-line-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("y" . my/meow-smart-save)
   '("'" . meow-find-and-select-inner)
   '("\"" . meow-find-and-select-outer)
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
   '("3" . (lambda () (interactive) (my-meow-digit 3)))  ;; was 2, fixed to 3
   '("4" . (lambda () (interactive) (my-meow-digit 4)))  ;; was 2, fixed to 4
   '("5" . (lambda () (interactive) (my-meow-digit 5)))  ;; was 2, fixed to 5
   '("6" . (lambda () (interactive) (my-meow-digit 6)))  ;; was 2, fixed to 6
   '("7" . (lambda () (interactive) (my-meow-digit 7)))  ;; was 2, fixed to 7
   '("8" . (lambda () (interactive) (my-meow-digit 8)))  ;; was 2, fixed to 8
   '("9" . (lambda () (interactive) (my-meow-digit 9)))  ;; was 2, fixed to 9
   '("0" . (lambda () (interactive) (my-meow-digit 0)))
   ;; '("0" . (lambda () (interactive) (my-meow-line-or-digit-0)))
   ;; '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("%" . my-match-paren-with-selection)
   '("a" . my/meow-append)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   ;; '("c" . meow-change)
   '("c" . my/meow-smart-change)
   ;; '("d" . meow-delete)
   ;; '("d" . my/generic-meow-smart-delete)
   '("d" . my/meow-smart-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("t" . meow-till)
   '("T" . my/meow-till-backward)
   '("F" . my/meow-find-backward)
   ;; '("g" . meow-cancel-selection)
   '("^" . my/meow-line-beginning)
   '("$" . my/meow-line-end)
   ;; '("0" . my/meow-line-start)
   '("g" . nil)
   '("g ;" . goto-last-change)
   '("g ;" . goto-last-change)
   '("g v" . my/restore-selection)
   '("g c" . my/meow-smart-comment)
   '("g w" . my/meow-smart-fill)
   '("g g" . beginning-of-buffer)
   '("g z" . zoxide-travel)
   '("G" . end-of-buffer)
   '("S s" . surround-region-with-symbol)
   '("S c" . change-surrounding-symbol)
   '("S d" . delete-surrounding-symbol)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   ;; '("A" . meow-open-below)
   ;; '("I" . meow-open-above)
   '("A" . meow-append-line-end)
   '("I" . meow-insert-line-start)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . my/search-next)
   '("N" . my/search-previous)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   ;; '("o" . meow-block)
   ;; '("O" . meow-to-block)
   ;; '("p" . meow-yank)
   '("p" . my/meow-smart-paste)
   ;; '("P" . placeholder)
   ;; '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . my/meow-replace-char)
   '("R" . dot-mode-execute)
   ;; '("s" . meow-kill)
   '("s" . my/meow-delete-insert)
   ;; '("S" . meow-kill-whole-line)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; '("v" . meow-visit)
   '("v" . my/meow-line-up)
   '("V" . my/meow-line-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   ;; '("x" . meow-line)
   '("x" . my/delete-char-to-kill-ring)
   ;; '("y" . meow-save)
   '("y" . my/meow-smart-save)
   '("Y" . meow-sync-grab)
   '(">" . my/indent-region-right)
   '("<" . my/indent-region-left)
   '("M" . mc/edit-lines)
   '("C" . my/meow-change-to-end-of-line)
   '("Y" . my/copy-to-end-of-line)
   '("D" . my/meow-delete-to-end-of-line)
   '("C-r" . undo-tree-redo)
   ;; '("C-M-m" . toggle-messages-buffer) ;; It's transaltes to M-RET
   '("M-y" . toggle-flymake-diagnostics)
   ;; '("'" . repeat)
   '("'" . meow-find-and-select-inner)
   '("\"" . meow-find-and-select-outer)
   '("/" . my/conditional-search-or-avy)
   '("?" . isearch-forward)
   '("M-v" . scroll-up-and-recenter)
   '("C-v" . scroll-down-and-recenter)
   '("C-M-y" . save-and-paste)
   '("=" . my/meow-smart-indent)
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
                  ;; Docker modes
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
  (define-key my-dired-g-map (kbd "z") 'zoxide-travel)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "SPC") 'my-space-as-ctrl-c)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "j") 'magit-section-forward)
  (define-key magit-mode-map (kbd "k") 'magit-section-backward)
  (define-key magit-mode-map (kbd "SPC") 'my-space-as-ctrl-c))

;; Makes functions like meow-next-word to ignore whitespaces
(setq meow-next-thing-include-syntax
      '((word "[:space:]" "[:space:]")
        (symbol "[:space:]" "[:space:]")))


(meow-setup)
(meow-global-mode 1)
