(defun left-version-of (ch)
  "Return the left version of a mirrored character if applicable; otherwise return CH."
  (cond ((eq ch ?\)) ?\() 
        ((eq ch ?\}) ?\{) 
        ((eq ch ?\]) ?\[) 
        ((eq ch ?\>) ?\<)
        (t ch)))

(defun right-version-of (ch)
  "Return the right version of a mirrored character if applicable; otherwise return CH."
  (cond ((eq ch ?\() ?\)) 
        ((eq ch ?\{) ?\}) 
        ((eq ch ?\[) ?\]) 
        ((eq ch ?\<) ?>)
        (t ch)))

(defun mirrored-p (ch)
  "Return t if CH is one of the mirrored characters."
  (memq ch '(?\( ?\) ?\{ ?\} ?\[ ?\] ?\< ?\>)))

(defun adjusted-count (n)
  "Adjust N according to the special translation rule: new_count = 2*n - 1."
  (- (* n 2) 1))

(defun find-char-in-line-forward (char &optional count)
  "Move forward to the COUNT-th occurrence of CHAR within the current line.
Return t if found, nil otherwise."
  (interactive "cFind char forward: \nP")
  (setq count (or count 1))
  (let ((end (line-end-position)))
    (catch 'not-found
      (dotimes (i count)
        (unless (search-forward (char-to-string char) end t)
          (message "Character not found in current line")
          (throw 'not-found nil)))
      (backward-char)
      t)))

(defun find-char-in-line-backward (char &optional count)
  "Move backward to the COUNT-th occurrence of CHAR within the current line.
Return t if found, nil otherwise."
  (interactive "cFind char backward: \nP")
  (setq count (or count 1))
  (let ((start (line-beginning-position)))
    (catch 'not-found
      (dotimes (i count)
        (unless (search-backward (char-to-string char) start t)
          (message "Character not found in current line")
          (throw 'not-found nil)))
      (forward-char)
      t)))

(defun find-and-select-inner (n ch)
  "Find the next occurrence of CH and select inside a pair of matching characters within the current line.
If CH is not found forward, fall back to backward search.
For mirrored characters, forward search always uses the left version,
but if falling back, backward search uses the right version.
The universal argument N is translated using the rule: adjusted count = 2*N - 1 for non-mirrored chars,
or used raw (n) for mirrored ones."
  (interactive "p\ncFind and select inside:")
  (let* ((is-mirrored (mirrored-p ch))
         (forward-char-to-search (if is-mirrored (left-version-of ch) ch))
         (backward-char-to-search (if is-mirrored (right-version-of ch) ch))
         (effective-count (if is-mirrored n (adjusted-count n)))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (found (save-excursion (find-char-in-line-forward forward-char-to-search effective-count))))
    (if found
        (progn
          (find-char-in-line-forward forward-char-to-search effective-count)
          (forward-char)
          (my/select-inside-char (if is-mirrored (left-version-of ch) ch)))
      (message "Character %s not found forward, trying backward" ch-str)
      (if (save-excursion (find-char-in-line-backward backward-char-to-search effective-count))
          (progn
            (find-char-in-line-backward backward-char-to-search effective-count)
            (backward-char)
            (my/select-inside-char (if is-mirrored (left-version-of ch) ch)))
        (message "Character %s not found backward either" ch-str)))))

(defun find-and-select-outer (n ch)
  "Find the next occurrence of CH and select outside a pair of matching characters within the current line.
If CH is not found forward, fall back to backward search.
For mirrored characters, forward search uses the left version,
but backward search uses the right version.
The universal argument N is translated using the rule: adjusted count = 2*N - 1 for non-mirrored chars,
or used raw (n) for mirrored ones."
  (interactive "p\ncFind and select outside:")
  (let* ((is-mirrored (mirrored-p ch))
         (forward-char-to-search (if is-mirrored (left-version-of ch) ch))
         (backward-char-to-search (if is-mirrored (right-version-of ch) ch))
         (effective-count (if is-mirrored n (adjusted-count n)))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (found (save-excursion (find-char-in-line-forward forward-char-to-search effective-count))))
    (if found
        (progn
          (find-char-in-line-forward forward-char-to-search effective-count)
          (forward-char)
          (my/select-outside-char (if is-mirrored (left-version-of ch) ch)))
      (message "Character %s not found forward, trying backward" ch-str)
      (if (save-excursion (find-char-in-line-backward backward-char-to-search effective-count))
          (progn
            (find-char-in-line-backward backward-char-to-search effective-count)
            (backward-char)
            (my/select-outside-char (if is-mirrored (left-version-of ch) ch)))
        (message "Character %s not found backward either" ch-str)))))


(defun my/select-inside-char (char)
  "Select region inside a pair of CHARs.
Examples: () {} [] <> \"\" '' ``"
  (interactive "cSelect inside char: ")
  (let ((start-char char)
        (end-char char))
    ;; Determine matching end character
    (cond
     ((eq char ?\() (setq end-char ?\)))
     ((eq char ?\{) (setq end-char ?\}))
     ((eq char ?\[) (setq end-char ?\]))
     ((eq char ?<) (setq end-char ?>)))
    ;; Search for opening character
    (when (re-search-backward (regexp-quote (char-to-string start-char)) nil t)
      (forward-char 1)
      (let ((beg (point)))
        ;; Search for closing character
        (if (re-search-forward (regexp-quote (char-to-string end-char)) nil t)
            (progn
              (backward-char 1)
              (set-mark beg)
              (activate-mark))
          (message "Closing character not found"))))))

(defun my/select-outside-char (char)
  "Select region including a pair of CHARs.
For mirrored characters, always convert to the left version.
The region selected will include both the opening and closing delimiters."
  (interactive "cSelect outside char: ")
  ;; If passed a right delimiter, always convert to its left version.
  (setq char (cond ((memq char '(?\) ?\} ?\] ?\>))
                    (cond ((eq char ?\)) ?\() 
                          ((eq char ?\}) ?\{)
                          ((eq char ?\]) ?\[)
                          ((eq char ?\>) ?\<)))
                   (t char)))
  (let* ((start-char char)
         (end-char (cond ((eq start-char ?\() ?\))
                         ((eq start-char ?\{) ?\})
                         ((eq start-char ?\[) ?\])
                         ((eq start-char ?<)  ?>)
                         (t char)))
         (orig-point (point))
         open-pos close-pos)
    ;; Find the nearest opening delimiter (to the left)
    (unless (re-search-backward (regexp-quote (char-to-string start-char)) nil t)
      (error "Opening delimiter not found"))
    (setq open-pos (point))
    ;; Now, from the original point, find the matching closing delimiter (to the right)
    (goto-char orig-point)
    (unless (re-search-forward (regexp-quote (char-to-string end-char)) nil t)
      (error "Closing delimiter not found"))
    (setq close-pos (point))
    (if (and open-pos close-pos (< open-pos orig-point) (< orig-point close-pos))
        (progn
          (goto-char open-pos)
          (push-mark close-pos t t)
          (activate-mark))
      (error "Cursor not inside a valid delimiter pair"))))


;;     ;; If forward search fails, try backward search
;;     (when (not forward-pos)
;;       (save-excursion
;;         (let ((found 0)
;;               (limit line-start))
;;           (goto-char line-end)
;;           (while (and (> (point) limit)
;;                       (search-backward ch-str limit t))
;;             (setq found (1+ found))
;;             (when (and (= (mod found 2) 1)
;;                        (= (/ (+ found 1) 2) n))
;;               (setq backward-pos (point)))))))
    
;;     (cond
;;      ((not (or forward-pos backward-pos))
;;       (message "char %s not found in current line" ch-str))
;;      (forward-pos
;;       (goto-char forward-pos)
;;       (cond 
;;        ;; Handle single quotes
;;        ((eq ch ?')
;;         (let ((bounds (meow--parse-single-quote nil)))
;;           (when bounds
;;             (set-mark (car bounds))
;;             (goto-char (cdr bounds)))))
;;        ;; Handle backticks explicitly
;;        ((eq ch ?`)
;;         (let ((start-pos (point)))
;;           (backward-char)  ;; Move back to the opening backtick
;;           (save-excursion
;;             ;; Find the matching backtick
;;             (forward-sexp)   ;; Move to the position after the closing backtick
;;             (set-mark (1- (point))))  ;; Set mark one char before, excluding the closing backtick
;;           (goto-char start-pos)))  ;; Return to the inner content (after opening backtick)
;;        ;; Handle other paired delimiters
;;        (thing-char
;;         (if (memq ch '(?\) ?\] ?\}))
;;             (progn
;;               (backward-sexp)
;;               (forward-char)
;;               (set-mark (point))
;;               (backward-char)
;;               (forward-sexp)
;;               (backward-char))
;;           (meow-inner-of-thing thing-char)))))
;;      (backward-pos
;;       (goto-char backward-pos)
;;       (cond
;;        ;; Handle single quotes
;;        ((eq ch ?')
;;         (let ((bounds (meow--parse-single-quote nil)))
;;           (when bounds
;;             (set-mark (car bounds))
;;             (goto-char (cdr bounds)))))
;;        ;; Handle backticks explicitly
;;        ((eq ch ?`)
;;         (let ((start-pos (point)))
;;           (save-excursion
;;             ;; Find the matching backtick
;;             (forward-sexp)   ;; Move to the matching closing backtick
;;             (backward-char)  ;; Position before the closing backtick
;;             (set-mark (point)))  ;; Set mark at end of inner content
;;           ;; Now go to the beginning of inner content
;;           (forward-char)))  ;; Move to position after opening backtick
;;        ;; Handle other paired delimiters
;;        (thing-char
;;         (if (memq ch '(?\) ?\] ?\}))
;;             (progn
;;               (forward-char)
;;               (backward-sexp)
;;               (forward-char)
;;               (set-mark (point))
;;               (backward-char)
;;               (forward-sexp)
;;               (backward-char))
;;           (meow-inner-of-thing thing-char))))))))

;; (defun meow-find-and-select-outer (n ch)
;;   "Find the next N occurrence of CH and select its outer content within current line only.
;; If no forward match is found, search backward."
;;   (interactive "p\ncFind and select outer:")
;;   (let* ((case-fold-search nil)
;;          (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
;;          (line-start (line-beginning-position))
;;          (line-end (line-end-position))
;;          (pos (point))
;;          forward-pos
;;          backward-pos
;;          (pair-char (cond
;;                      ((eq ch ?\() ?\))
;;                      ((eq ch ?\)) ?\()
;;                      ((eq ch ?\[) ?\])
;;                      ((eq ch ?\]) ?\[)
;;                      ((eq ch ?\{) ?\})
;;                      ((eq ch ?\}) ?\{)
;;                      ((memq ch '(?' ?\" ?`)) ch)
;;                      (t nil))))
;;     ;; Try forward search first
;;     (save-excursion
;;       (setq forward-pos (search-forward ch-str line-end t n)))
    
;;     ;; If forward search fails, try backward search
;;     (when (not forward-pos)
;;       (save-excursion
;;         (setq backward-pos (search-backward ch-str line-start t n))))
    
;;     (cond
;;      ((not (or forward-pos backward-pos))
;;       (message "char %s not found in current line" ch-str))
;;      (forward-pos
;;       (goto-char forward-pos)
;;       (if (memq ch '(?' ?\" ?`))
;;           (let ((end-pos (save-excursion
;;                            (when (search-forward ch-str line-end t)
;;                              (point)))))
;;             (if end-pos
;;                 (progn
;;                   (set-mark (1- forward-pos))
;;                   (goto-char end-pos))
;;               (message "No closing %s found in current line" ch-str)))
;;         (when pair-char
;;           (if (memq ch '(?\) ?\] ?\}))
;;               (progn
;;                 (set-mark (point))
;;                 (condition-case nil
;;                     (backward-sexp)
;;                   (scan-error (goto-char line-start)))
;;                 (when (< (point) line-start)
;;                   (goto-char line-start)))
;;             (progn
;;               (backward-char)
;;               (set-mark (point))
;;               (condition-case nil
;;                   (forward-sexp)
;;                 (scan-error (goto-char line-end)))
;;               (when (> (point) line-end)
;;                 (goto-char line-end)))))))
;;      (backward-pos
;;       (goto-char backward-pos)
;;       (if (memq ch '(?' ?\" ?`))
;;           (let ((start-pos (save-excursion
;;                              (when (search-backward ch-str line-start t)
;;                                (point)))))
;;             (if start-pos
;;                 (progn
;;                   (set-mark (1+ backward-pos))
;;                   (goto-char start-pos))
;;               (message "No opening %s found in current line" ch-str)))
;;         (when pair-char
;;           (if (memq ch '(?\) ?\] ?\}))
;;               (progn
;;                 (forward-char)
;;                 (set-mark (point))
;;                 (condition-case nil
;;                     (backward-sexp)
;;                   (scan-error (goto-char line-start)))
;;                 (when (< (point) line-start)
;;                   (goto-char line-start)))
;;             (progn
;;               (set-mark (point))
;;               (condition-case nil
;;                   (forward-sexp)
;;                 (scan-error (goto-char line-end)))
;;               (when (> (point) line-end)
;;                 (goto-char line-end))))))))))

