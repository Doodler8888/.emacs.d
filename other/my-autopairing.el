;; Define a function to insert a pair of double quotes.
(defun my-insert-double-quote ()
  "Insert a pair of double quotes, leaving point in between.
If the previous character is a backslash (escaping), insert a literal quote."
  (interactive)
  (if (and (char-before) (eq (char-before) ?\\))
      (insert "\"")
    (progn
      (insert "\"\"")
      (backward-char))))

;; Define a function for smart deletion.
(defun my-smart-backspace ()
  "If point is between a pair of double quotes, delete both.
Otherwise, perform a normal backspace."
  (interactive)
  (if (and (char-before) (char-after)
           (eq (char-before) ?\")
           (eq (char-after) ?\"))
      ;; Delete the closing quote and then the opening quote.
      (progn
        (delete-char 1)
        (delete-char -1))
    (delete-backward-char 1)))

;; Create a minor mode that binds our custom functions.
(define-minor-mode my-pair-mode
  "A simple substitute for electric pair mode for double quotes.
When enabled, typing \" automatically inserts a matching quote,
and deleting an opening quote when immediately paired deletes both."
  :lighter " myPair"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Bind the " key to our insertion function.
            (define-key map "\"" 'my-insert-double-quote)
            ;; Bind DEL (backspace) to our smart deletion.
            (define-key map (kbd "DEL") 'my-smart-backspace)
            map))

;; Optionally, enable the mode globally (or add it to specific modes)
(my-pair-mode 1)
