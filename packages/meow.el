(use-package meow
  :ensure t
  :init
  (setq meow-use-clipboard t))

(defun vim-like-paste ()
  "Paste (yank) replacing the selected region, similar to Vim's paste behavior."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (delete-region start end)
        (goto-char start)
        (yank))
    (yank)))

(defun scroll-up-and-recenter (arg)
  "Scroll up ARG lines and recenter."
  (interactive "P")
  (scroll-up-command arg)
  (recenter))

(defun scroll-down-and-recenter (arg)
  "Scroll down ARG lines and recenter."
  (interactive "P")
  (scroll-down-command arg)
  (recenter))

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
(global-set-key (kbd "H-f") 'forward-char)
(global-set-key (kbd "H-b") 'backward-char)
(setq meow--kbd-forward-char "H-f")
(setq meow--kbd-backward-char "H-b")

(define-key minibuffer-local-map (kbd "M-n") 'my-next-history-element)
(define-key minibuffer-local-map (kbd "M-p") 'my-previous-history-element)

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

(global-set-key (kbd "C-y") 'copy-whole-line)

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

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow--kbd-kill-region "C-w C-w")
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
   '("C-f" . scroll-up-and-recenter)
   '("C-b" . scroll-down-and-recenter)
   '("P" . save-and-paste)
   '("C" . my/meow-change-to-end-of-line)
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
   '("p" . vim-like-paste)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   ;; '("Y" . meow-sync-grab)
   '("Y" . my/copy-to-end-of-line)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

  (meow-setup)

  (meow-global-mode 1)
