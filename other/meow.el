(use-package meow
  :ensure t
  :init
  (setq meow-use-clipboard t))

(define-prefix-command 'my-window-map)
(global-set-key (kbd "C-w") 'my-window-map)

(global-set-key (kbd "C-w C-l") 'windmove-right)
(global-set-key (kbd "C-w C-h") 'windmove-left)
(global-set-key (kbd "C-w C-k") 'windmove-up)
(global-set-key (kbd "C-w C-j") 'windmove-down)

(global-set-key (kbd "C-w C-s") 'split-window-below) 
(global-set-key (kbd "C-w C-v") 'split-window-right)  
(global-set-key (kbd "C-w C-c") 'delete-window)        

(global-set-key (kbd "C-w C-w") 'kill-region)
(global-set-key (kbd "H-u") 'universal-argument)
(global-set-key (kbd "H-d") 'delete-char)
(global-set-key (kbd "H-f") 'forward-char)
(global-set-key (kbd "H-b") 'backward-char)
(global-set-key (kbd "H-h") 'mark-paragraph)
(global-set-key (kbd "H-r") 'mark-paragraph)
(setq meow--kbd-universal-argument "H-u")
(setq meow--kbd-delete-char "H-d")
(setq meow--kbd-mark-paragraph "H-h")
(setq meow--kbd-forward-char "H-f")
(setq meow--kbd-backward-char "H-b")
(setq meow--kbd-mark-paragraph "H-r")

(define-key minibuffer-local-map (kbd "M-n") 'my-next-history-element)
(define-key minibuffer-local-map (kbd "M-p") 'my-previous-history-element)

(defun my/meow-find-backward (arg ch)
  "Combine negative argument with meow-find to search backward in one keybinding."
  (interactive "p\ncFind backward:")
  (meow-find (- arg) ch))

(defun my/meow-till-backward (arg ch)
  "Combine negative argument with meow-till to search backward in one keybinding."
  (interactive "p\ncTill backward:")
  (meow-till (- arg) ch))

(defun my/meow-find-backward-and-select-inner (arg ch)
  "Find the previous occurrence of CH and select its inner content."
  (interactive "p\ncFind backward and select inner:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (end (point))
         beg
         (thing-char (cond
                      ((memq ch '(?\( ?\))) ?r)  ; 'r' for round brackets
                      ((memq ch '(?\[ ?\])) ?s)  ; 's' for square brackets
                      ((memq ch '(?\{ ?\})) ?c)  ; 'c' for curly braces
                      ((memq ch '(?' ?\" ?`)) ?g)  ; 'g' for quotes (single, double, and backticks)
                      (t nil))))
    (save-mark-and-excursion
      (setq beg (search-backward ch-str nil t arg)))
    (if (not beg)
        (message "char %s not found" ch-str)
      (goto-char beg)
      (if thing-char
          (meow-inner-of-thing thing-char)
        (message "No inner selection defined for this character")))))

(defun meow-find-and-select-inner (n ch)
  "Find the next N occurrence of CH and select its inner content."
  (interactive "p\ncFind and select inner:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end
         (thing-char (cond
                      ((memq ch '(?\( ?\))) ?r)  ; 'r' for round brackets
                      ((memq ch '(?\[ ?\])) ?s)  ; 's' for square brackets
                      ((memq ch '(?\{ ?\})) ?c)  ; 'c' for curly braces
                      ((memq ch '(?' ?\" ?`)) ?g)  ; 'g' for quotes (single, double, and backticks)
                      (t nil))))
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (goto-char end)
      (if thing-char
          (meow-inner-of-thing thing-char)
        (message "No inner selection defined for this character")))))

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

(defun my/smart-comment ()
  "Comment or uncomment region if active, otherwise comment/uncomment current line."
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (line-beginning-position)))
        (end (if (region-active-p) (region-end) (line-end-position))))
    (comment-or-uncomment-region start end)))

(defun save-and-paste ()
  "Copy the current line to the kill ring."
  (interactive)
  (move-beginning-of-line 1)
  (copy-whole-line)
  (vim-like-paste))

(defun copy-whole-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2)))

(defun my/copy-to-end-of-line ()
  "Copy text from the current cursor position to the end of the line."
  (interactive)
  (kill-ring-save (point) (line-end-position)))

(defun my/meow-change-to-end-of-line ()
  "Highlight from the current cursor position to the end of the line and execute 'meow-change'."
  (interactive)
  (let ((start (point))
        (end (line-end-position)))
    (set-mark start)
    (goto-char end)
    (meow-reverse)
    (meow-change)))

(defun my/meow-delete-to-end-of-line ()
  "Highlight from the current cursor position to the end of the line and execute 'meow-change'."
  (interactive)
  (let ((start (point))
        (end (line-end-position)))
    (set-mark start)
    (goto-char end)
    (meow-reverse)
    (meow-delete)))

(defun my/meow-revers-line ()
  "Reverse meow-line"
  (interactive)
  (meow-line 1)
  (meow-reverse))

(defun my/meow-revers-line ()
  "Reverse meow-line"
  (interactive)
  (meow-line 1)
  (meow-reverse))

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

(defun meow-my-go-to-line ()
  (interactive)
  (meow-line 1)
  (avy-goto-line))

(defun my-append ()
  "Like Vim's append: move forward one character then enter insert mode."
  (interactive)
  (forward-char)
  (meow-insert))

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
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument) 
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("C-f" . scroll-up-and-recenter)
;;    '("C-b" . scroll-down-and-recenter)
;;    '("C-d" . scroll-half-up-and-recenter)
;;    '("C-u" . scroll-half-down-and-recenter)
;;    ;; '("C-y" . copy-whole-line)
;;    ;; '("C-x c" . my/smart-comment)
;;    '("gc" . my/smart-comment)
;;    '("gw" . my-fill-region)
;;    '("P" . save-and-paste)
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
;;    '(";" . meow-reverse)
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
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
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
;;    '("v" . evil-visual-char)
;;    '("V" . evil-visual-line)
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
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("t" . meow-till)
   '("T" . my/meow-till-backward)
   '("F" . my/meow-find-backward)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("C" . my/meow-change-to-end-of-line)
   '("Y" . my/copy-to-end-of-line)
   '("D" . my/delete-to-end-of-line)
   '("M-f" . toggle-messages-buffer)
   '("'" . repeat)
   '("C-f" . scroll-up-and-recenter)
   '("C-b" . scroll-down-and-recenter)
   '("C-d" . scroll-half-up-and-recenter)
   '("C-u" . scroll-half-down-and-recenter)
   '("<escape>" . meow-cancel-selection)))
   ;; '("<escape>" . ignore)))

(with-eval-after-load 'meow
  (setq meow-mode-state-list
        (append '((messages-buffer-mode . normal)
                 (help-mode . normal)
                 (helpful-mode . normal)
                 (Info-mode . normal)
                 (special-mode . normal)
                 (shell-command-mode . normal)
                 (debugger-mode . normal))
                meow-mode-state-list)))

(meow-setup)

(setq meow-expand-exclude-mode-list
      (remove 'org-mode meow-expand-exclude-mode-list))

(meow-global-mode 1)
