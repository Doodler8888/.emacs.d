(defvar my/last-search-type nil
  "Stores the type of the last search performed.")

(defun my/consult-line-with-evil ()
  "Run consult-line and set up evil search pattern."
  (interactive)
  (let ((selected (consult-line)))
    (when-let* ((search-string (car consult--line-history)))
      (message "Search string: %S" search-string)
      (let ((search-string-prop 
             (propertize search-string 
                         'isearch-case-fold-search t)))
        (push search-string-prop regexp-search-ring)
        ;; Set search direction to forward
        (setq isearch-forward t)
        (setq my/last-search-type 'consult)))))

;; (defun my/get-last-search-pattern ()
;;   "Get the last search pattern based on the last search type."
;;   (cond
;;    ((eq my/last-search-type 'consult)
;;     (car regexp-search-ring))
;;    ((and (boundp 'isearch-string) (not (string-empty-p isearch-string)))
;;     (if isearch-regexp isearch-string (regexp-quote isearch-string)))
;;    (t "")))

(defun my/search-next2 ()
  "Search forward using last search pattern."
  (interactive)
  (when-let* ((pattern (my/get-last-search-pattern)))
    (let ((case-fold-search t)
          (current-pos (point)))
      ;; Move past current match if we're at one
      (when (looking-at pattern)
        (goto-char (match-end 0)))
      (if (and (re-search-forward pattern nil t)
               (match-beginning 0))
          (let ((match-pos (match-beginning 0)))
            (goto-char match-pos)
            (message "Match found: %s" (match-string 0))
            t)
        (message "No more matches")
        nil))))

(defun my/search-previous ()
  "Search backward using last search pattern."
  (interactive)
  (when-let* ((pattern (my/get-last-search-pattern)))
    (let ((case-fold-search t))
      (if (re-search-backward pattern nil t)
          (let ((match-pos (match-beginning 0)))
            (goto-char match-pos)
            (message "Match found: %s" (match-string 0))
            t)
        (message "No more matches")
        nil))))

(advice-add 'isearch-forward :after
            (lambda (&rest _)
              (setq my/last-search-type 'isearch)))

(advice-add 'isearch-backward :after
            (lambda (&rest _)
              (setq my/last-search-type 'isearch)))



(defun my-occur-like ()
  "Prompt for non-empty lines in the current buffer using `completing-read'."
  (interactive)
  (let* ((lines (split-string (buffer-string) "\n"))
         (numbered-lines (cl-loop for line in lines
                                for n from 1
                                unless (string-match-p "^[[:space:]]*$" line)
                                collect (format "%4d: %s" n line)))
         (orig-point (point))
         (orig-window-start (window-start))
         (user-input nil)
         (restore-position (lambda ()
                           (goto-char orig-point)
                           (set-window-start (selected-window) orig-window-start)))
         (occur-update-function (lambda (&rest _)
                                (when-let* ((index vertico--index)
                                          (candidates vertico--candidates)
                                          ((seq-find (lambda (c) 
                                                     (string-match "^[[:space:]]*[0-9]+:" c))
                                                   candidates))
                                          (cand (and (>= index 0)
                                                    (nth index candidates)))
                                          (match (string-match "^[[:space:]]*\\([0-9]+\\):" cand))
                                          (line-num (match-string 1 cand)))
                                  (setq user-input (car vertico--input))
                                  (with-selected-window (minibuffer-selected-window)
                                    (goto-char (point-min))
                                    (forward-line (1- (string-to-number line-num)))
                                    (recenter))))))
    
    (advice-add 'vertico--update :after occur-update-function)
    
    (unwind-protect
        (condition-case nil
            (let ((choice (completing-read "Select line: " numbered-lines)))
              (if (or (null choice) (string-empty-p choice))
                  (funcall restore-position)
                (let ((match (string-match "^[[:space:]]*\\([0-9]+\\):" choice)))
                  (if (not match)
                      (funcall restore-position)
                    (let* ((line-num (match-string 1 choice))
                           (search-string user-input))
                      (message "Search string: %S" search-string)
                      (when search-string
                        (push (propertize search-string
                                        'isearch-case-fold-search t)
                              regexp-search-ring)
                        (setq my/last-search-type 'occur))
                      (goto-char (point-min))
                      (forward-line (1- (string-to-number line-num)))
                      (recenter))))))
          (quit (funcall restore-position)))
      
      (advice-remove 'vertico--update occur-update-function))))

;; Add occur to get-last-search-pattern
(defun my/get-last-search-pattern ()
  "Get the last search pattern based on the last search type."
  (cond
   ((eq my/last-search-type 'occur)
    (car regexp-search-ring))
   ((eq my/last-search-type 'consult)
    (car regexp-search-ring))
   ((and (boundp 'isearch-string) (not (string-empty-p isearch-string)))
    (if isearch-regexp isearch-string (regexp-quote isearch-string)))
   (t "")))

