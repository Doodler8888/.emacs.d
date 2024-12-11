(defun my-copy-to-end-of-line ()
  "Copy from point to end of line."
  (interactive)
  (copy-region-as-kill (point) (line-end-position)))

(global-set-key (kbd "M-y") 'my-copy-to-end-of-line)
