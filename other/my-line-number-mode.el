;;; relative-line-numbers.el --- Minimal relative line numbers mode

(defgroup relative-line-numbers nil
  "Display relative line numbers in the buffer."
  :group 'convenience
  :group 'display)

(defcustom relative-line-numbers-type 'relative
  "The type of line numbers to use in `my-relative-line-numbers-mode'.
Defaults to relative numbering."
  :group 'relative-line-numbers
  :type '(choice (const relative))
  :version "26.1")

(defun relative-line-numbers-update ()
  "Update the display of relative line numbers."
  (setq display-line-numbers 'relative))

;;;###autoload
(define-minor-mode my-relative-line-numbers-mode
  "Toggle relative line numbers in the buffer."
  :lighter nil
  (if my-relative-line-numbers-mode
      (relative-line-numbers-update)
    (setq display-line-numbers nil)))

(defun relative-line-numbers--turn-on ()
  "Enable `my-relative-line-numbers-mode' unless in minibuffer."
  (unless (minibufferp)
    (my-relative-line-numbers-mode)))

;;;###autoload
(define-globalized-minor-mode my-global-relative-line-numbers-mode
  my-relative-line-numbers-mode relative-line-numbers--turn-on)

(provide 'my-relative-line-numbers)

;;; relative-line-numbers.el ends here





















