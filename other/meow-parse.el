;; -*- lexical-binding: t -*-

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

(defun meow--parse-outside-whitespace (inner)
  "Parse the bounds for outside whitespace selection."
  (save-excursion
	(let* ((line-start (line-beginning-position))
		   (line-end (line-end-position))
		   ;; First find the inner bounds
		   (inner-start (progn
						 (skip-syntax-backward "^-" line-start)
						 (point)))
		   (inner-end (progn
					   (skip-syntax-forward "^-" line-end)
					   (point)))
		   ;; Then extend to include surrounding whitespace
		   (outer-start (progn
						 (goto-char inner-start)
						 (skip-syntax-backward "-" line-start)
						 (point)))
		   (outer-end (progn
					   (goto-char inner-end)
					   (skip-syntax-forward "-" line-end)
					   (point))))
	  (cons outer-start outer-end))))

(defun meow--comment-line-p ()
  "Return non-nil if the current line is a comment line in Emacs Lisp."
  (save-excursion
	(back-to-indentation)
	(looking-at-p ";+")))

(defun meow--parse-inside-comment (inner)
  "Parse the bounds for inside comment block selection."
  (save-excursion
	(let ((comment-char (string (char-after (comment-beginning))))
		  block-start block-end start-line)
	  (message "=== Meow Comment Selection ===")
	  (message "Using comment char: %s" comment-char)

	  ;; First go up to find start
	  (beginning-of-line)
	  (message "Starting at line: %d" (line-number-at-pos))
	  (while (and (not (bobp))
				  (not (looking-at "^[[:space:]]*$"))
				  (looking-at (format "^[[:space:]]*%s" comment-char)))
		(message "Going up, current line: %s"
				(buffer-substring (line-beginning-position) (line-end-position)))
		(forward-line -1))

	  ;; Move one line forward if we stopped at non-comment
	  (unless (looking-at (format "^[[:space:]]*%s" comment-char))
		(forward-line 1))

	  ;; Remember where we started
	  (setq start-line (line-number-at-pos))

	  ;; Set start position right after comment marker
	  (skip-chars-forward "[:space:]")
	  (forward-char (length comment-char))
	  (backward-char 1)  ;; Move back one character to include first letter
	  (setq block-start (point))
	  (message "Block start at line %d, pos %d: '%s'"
			  (line-number-at-pos) block-start
			  (buffer-substring (line-beginning-position) (line-end-position)))

	  ;; Go back to start line and then go down
	  (goto-line start-line)
	  (while (and (not (eobp))
				  (not (looking-at "^[[:space:]]*$"))
				  (looking-at (format "^[[:space:]]*%s" comment-char)))
		(message "Going down, current line: %s"
				(buffer-substring (line-beginning-position) (line-end-position)))
		(forward-line 1))

	  ;; Move back one line and to its end
	  (forward-line -1)
	  (end-of-line)
	  (setq block-end (point))
	  (message "Block end at line %d, pos %d: '%s'"
			  (line-number-at-pos) block-end
			  (buffer-substring (line-beginning-position) (line-end-position)))

	  (message "Returning selection: (%d . %d)" block-start block-end)
	  (cons block-start block-end))))


(defun meow--parse-outside-comment (inner)
  "Parse the bounds for outside comment block selection, including empty lines until next content."
  (save-excursion
	(let ((comment-char (string (char-after (comment-beginning))))
		  comment-start comment-end block-start block-end current-line)
	  ;; Remember current line
	  (setq current-line (line-number-at-pos))

	  ;; First find the start of current comment block
	  (beginning-of-line)
	  (while (and (not (bobp))
				  (looking-at (format "^[[:space:]]*%s" comment-char)))
		(forward-line -1))
	  (unless (bobp)
		(forward-line 1))
	  (setq comment-start (point))

	  ;; Find end of current comment block
	  (while (and (not (eobp))
				  (looking-at (format "^[[:space:]]*%s" comment-char)))
		(forward-line 1))
	  (setq comment-end (point))

	  ;; Go back to comment start and look for empty lines above
	  (goto-char comment-start)
	  (while (and (not (bobp))
				  (save-excursion
					(forward-line -1)
					(looking-at "^[[:space:]]*$")))
		(forward-line -1))
	  (setq block-start (point))

	  ;; Go to comment end and look for empty lines below
	  (goto-char comment-end)
	  (while (and (not (eobp))
				  (looking-at "^[[:space:]]*$"))
		(forward-line 1))
	  (setq block-end (point))

	  (cons block-start block-end))))


(defun meow--parse-inside-sentence (inner)
  "Parse the bounds for inside sentence selection across multiple lines."
  (save-excursion
	(let* ((start (progn
					(forward-char) ;; Move forward 1 char to handle Emacs cursor behavior
					(if (eq major-mode 'org-mode)
						(org-backward-sentence)
					  (backward-sentence))
					(skip-syntax-forward " >")
					(point)))
		   (end (progn
				  (forward-sentence)
				  (skip-syntax-backward " >")
				  (point))))
	  (cons start end))))

(defun meow--parse-outside-sentence (inner)
  "Parse the bounds for outside sentence selection across multiple lines."
  (save-excursion
	(let* (;; First find the inner bounds
		   (inner-start (progn
						 (backward-sentence)
						 (skip-syntax-forward " >")
						 (point)))
		   (inner-end (progn
					   (forward-sentence)
					   (skip-syntax-backward " >")
					   (point)))
		   ;; Then extend to include surrounding whitespace/punctuation
		   (outer-start (progn
						 (goto-char inner-start)
						 (skip-syntax-backward " >.")
						 (point)))
		   (outer-end (progn
					   (goto-char inner-end)
					   (skip-syntax-forward " >.")
					   (point))))
	  (cons outer-start outer-end))))

(defun meow--parse-inside-quotes (inner quote-char)
  "Parse the bounds for inside quotes selection. Supports multi-line strings.
INNER is unused parameter for consistency with other parse functions.
QUOTE-CHAR is the character used for quoting."
  (save-excursion
    (let* ((buffer-end (point-max))
           (buffer-start (point-min))
           (current-pos (point))
           ;; Find all quote pairs that contain the current position
           (pairs nil))
      ;; Find quote pair containing current position
      (when (save-excursion
              (goto-char current-pos)
              (when (search-backward quote-char buffer-start t)
                (let ((start-pos (point))
                      (end-pos (save-excursion
                                 (goto-char (1+ (point)))
                                 (search-forward quote-char buffer-end t))))
                  (when (and end-pos (>= end-pos current-pos))
                    (push (list (intern quote-char) start-pos end-pos) pairs)))))
          nil)
      ;; Find the innermost quote pair (the one with the highest start position)
      (if pairs
          (let* ((innermost (car (sort pairs (lambda (a b) (> (nth 1 a) (nth 1 b))))))
                 (type (nth 0 innermost))
                 (start (nth 1 innermost))
                 (end (nth 2 innermost)))
            (cons (1+ start) (1- end)))
        nil))))

(defun meow--parse-inside-double-quotes (inner)
  "Parse the bounds for inside double quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "\""))

(defun meow--parse-inside-single-quotes (inner)
  "Parse the bounds for inside single quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "'"))

(defun meow--parse-inside-backtick-quotes (inner)
  "Parse the bounds for inside backtick quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "`"))

(defun meow--parse-inside-slashes (inner)
  "Parse the bounds for inside backtick quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "/"))

(defun meow--parse-inside-tildas (inner)
  "Parse the bounds for inside backtick quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "~"))

(defun meow--parse-inside-equals (inner)
  "Parse the bounds for inside backtick quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "="))

(defun meow--parse-inside-asterisks (inner)
  "Parse the bounds for inside backtick quotes selection. Supports multi-line strings."
  (meow--parse-inside-quotes inner "*"))

(defun meow--parse-inside-angles (inner)
  "Parse the bounds for inside angle brackets selection. Supports multi-line strings."
  (save-excursion
    (let* ((buffer-end (point-max))
           (buffer-start (point-min))
           (current-pos (point))
           (start-pos nil)
           (end-pos nil))
      ;; Find opening < before current position
      (when (search-backward "<" buffer-start t)
        (setq start-pos (point))
        ;; Find closing > after opening
        (when (search-forward ">" buffer-end t)
          (setq end-pos (point))
          ;; Check if current position is within this pair
          (when (and (>= current-pos start-pos) (<= current-pos end-pos))
            (cons (1+ start-pos) (1- end-pos))))))))

(defun meow--parse-outside (delim)
  "Parse the bounds for a string enclosed by DELIM. Supports multi-line strings."
  (save-excursion
    (let* ((buffer-start (point-min))
           (buffer-end (point-max))
           (start (search-backward (char-to-string delim) buffer-start t))
           (end (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char 1)
                    (search-forward (char-to-string delim) buffer-end t)))))
      (when (and start end)
        (cons start end)))))

(defun meow--parse-outside-double-quotes (inner)
  "Parse the bounds for outside double quotes selection. Supports multi-line strings."
  (meow--parse-outside ?\"))

(defun meow--parse-outside-slashes (inner)
  "Parse the bounds for outside slashes selection. Supports multi-line strings."
  (meow--parse-outside ?/))

(defun meow--parse-outside-single-quotes (inner)
  "Parse the bounds for outside single quotes selection. Supports multi-line strings."
  (meow--parse-outside ?'))

(defun meow--parse-outside-backtick-quotes (inner)
  "Parse the bounds for outside backtick quotes selection. Supports multi-line strings."
  (meow--parse-outside ?`))

(defun meow--parse-outside-tildas (inner)
  "Parse the bounds for outside backtick quotes selection. Supports multi-line strings."
  (meow--parse-outside ?~))

(defun meow--parse-outside-equals (inner)
  "Parse the bounds for outside backtick quotes selection. Supports multi-line strings."
  (meow--parse-outside ?=))

(defun meow--parse-outside-asterisks (inner)
  "Parse the bounds for outside backtick quotes selection. Supports multi-line strings."
  (meow--parse-outside ?*))

(defun meow--parse-outside-angles (inner)
  "Parse the bounds for outside angle brackets selection. Supports multi-line strings."
  (save-excursion
    (let* ((buffer-start (point-min))
           (buffer-end (point-max))
           (start (search-backward "<" buffer-start t))
           (end (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char 1)
                    (search-forward ">" buffer-end t)))))
      (when (and start end)
        (cons start end)))))


;; You might also want a combined function that finds the closest quotes
(defun meow--parse-outside-quotes (inner)
  "Parse the bounds for outside quotes selection, finding the closest quote.
Supports double quotes, single quotes, and backticks."
  (save-excursion
	(let* ((double-quotes (meow--parse-outside-double-quotes inner))
		   (single-quotes (meow--parse-outside-single-quotes inner))
		   (backtick-quotes (meow--parse-outside-backtick-quotes inner))
		   (quotes-list (remove nil (list double-quotes single-quotes backtick-quotes))))
	  (when quotes-list
		;; Find the quotes with the highest start position (closest to point)
		(car (sort quotes-list (lambda (a b) (> (car a) (car b)))))))))

(defun meow--parse-inside-org (inner)
  "Parse the bounds for inside org emphasis selection.
Returns a cons cell (BEGIN . END) for the text inside the emphasis markers,
excluding the markers themselves, or nil if no valid emphasis is found."
  (when (derived-mode-p 'org-mode)
	(let* ((element (org-element-context))
		   (type (org-element-type element))
		   (begin (org-element-property :begin element))
		   (end (org-element-property :end element))
		   (contents-begin (org-element-property :contents-begin element))
		   (contents-end (org-element-property :contents-end element)))
	  (cond
	   ;; Special handling for code/verbatim elements
	   ((memq type '(code verbatim))
		(when (and begin end)
		  ;; Get the raw text to examine what's happening
		  (let* ((raw-text (buffer-substring-no-properties begin end))
				 (first-tilde (string-match "~" raw-text))
				 (last-tilde (string-match "~" raw-text (1+ first-tilde))))
			(when (and first-tilde last-tilde)
			  ;; Calculate proper boundaries excluding both markers
			  (cons (+ begin (1+ first-tilde))
					(+ begin last-tilde))))))

	   ;; Regular handling for other emphasis types
	   ((memq type '(bold italic strike-through underline))
		(when (and contents-begin contents-end)
		  (cons contents-begin contents-end)))

	   ;; No valid emphasis found
	   (t nil)))))

(defun meow--parse-outside-org (inner)
  "Parse the bounds for outside org emphasis selection.
Returns a cons cell (BEGIN . END) for the text including the emphasis markers,
or nil if no valid emphasis is found."
  (when (derived-mode-p 'org-mode)
    (let* ((element (org-element-context))
           (type (org-element-type element))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element)))
      (cond
       ;; Special handling for code/verbatim elements
       ((memq type '(code verbatim))
        (when (and begin end)
          ;; Get the raw text to examine what's happening
          (let* ((raw-text (buffer-substring-no-properties begin end))
                 (first-tilde (string-match "~" raw-text))
                 (last-tilde (string-match "~" raw-text (1+ first-tilde))))
            (when (and first-tilde last-tilde)
              ;; Calculate proper boundaries including both markers
              ;; but excluding any trailing whitespace
              (cons begin (+ begin (1+ last-tilde)))))))

       ;; Regular handling for other emphasis types
       ((memq type '(bold italic strike-through underline))
        (when (and begin end)
          (let* ((raw-text (buffer-substring-no-properties begin end))
                 (trimmed-text (replace-regexp-in-string "[ \t\n]+\\'" "" raw-text)))
            (cons begin (+ begin (length trimmed-text))))))

       ;; No valid emphasis found
       (t nil)))))

;; Optional helper function for debugging
(defun meow-org-emphasis-info ()
  "Display information about the org emphasis at point."
  (interactive)
  (let* ((element (org-element-context))
		 (type (org-element-type element))
		 (begin (org-element-property :begin element))
		 (end (org-element-property :end element))
		 (raw-text (when (and begin end)
					 (buffer-substring-no-properties begin end))))
	(message "Type: %s, Begin: %s, End: %s, Raw text: %s"
			 type begin end raw-text)))


;; Delete the 'window' option for selection
(setq meow-char-thing-table (assq-delete-all ?\. meow-char-thing-table))
(setq meow-char-thing-table (assq-delete-all ?w meow-char-thing-table))
(setq meow-char-thing-table (assq-delete-all ?g meow-char-thing-table))
;; (setq meow-char-thing-table (assq-delete-all ?\' meow-char-thing-table))

(add-to-list 'meow-char-thing-table '(?m . sentence))
(add-to-list 'meow-char-thing-table '(?g . double-quotes))
;; (add-to-list 'meow-char-thing-table '(?' . single-quotes))
;; (add-to-list 'meow-char-thing-table '(?\. . my-sentence))
(add-to-list 'meow-char-thing-table '(?w . whitespace))
(add-to-list 'meow-char-thing-table '(?a . comment))
;; (add-to-list 'meow-char-thing-table '(?o . org))

(advice-add 'meow--parse-inner-of-thing-char :around
			(lambda (orig-fun ch)
			  (cond
			   ((eq ch ?m) (meow--parse-inside-sentence t))
			   ((eq ch ?w) (meow--parse-inside-whitespace t))
			   ((eq ch ?g) (meow--parse-inside-double-quotes t))
			   ((eq ch ?') (meow--parse-inside-single-quotes t))
			   ((eq ch ?`) (meow--parse-inside-backtick-quotes t))
			   ((eq ch ?/) (meow--parse-inside-slashes t))
			   ((eq ch ?#) (meow--parse-inside-comment t))
			   ((eq ch ?~) (meow--parse-inside-tildas t))
			   ((eq ch ?=) (meow--parse-inside-equals t))
			   ((eq ch ?*) (meow--parse-inside-asterisks t))
			   ((eq ch ?<) (meow--parse-inside-angles t))
			   ((eq ch ?>) (meow--parse-inside-angles t))
			   ;; ((eq ch ?o) (meow--parse-inside-org t))
			   (t (funcall orig-fun ch)))))

(advice-add 'meow--parse-bounds-of-thing-char :around
			(lambda (orig-fun ch)
			  (cond
			   ((eq ch ?w) (meow--parse-outside-whitespace t))
			   ((eq ch ?m) (meow--parse-outside-sentence t))
			   ((eq ch ?g) (meow--parse-outside-double-quotes t))
			   ((eq ch ?') (meow--parse-outside-single-quotes t))
			   ((eq ch ?`) (meow--parse-outside-backtick-quotes t))
			   ((eq ch ?/) (meow--parse-outside-slashes t))
			   ((eq ch ?#) (meow--parse-outside-comment t))
			   ;; ((eq ch ?o) (meow--parse-outside-org t))
			   ((eq ch ?~) (meow--parse-outside-tildas t))
			   ((eq ch ?=) (meow--parse-outside-equals t))
			   ((eq ch ?*) (meow--parse-outside-asterisks t))
			   ((eq ch ?<) (meow--parse-outside-angles t))
			   ((eq ch ?>) (meow--parse-outside-angles t))
			   (t (funcall orig-fun ch)))))
