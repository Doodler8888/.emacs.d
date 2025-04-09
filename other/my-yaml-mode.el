(defgroup yaml nil
  "YAML major mode"
  :group 'languages)

;; Define faces
(defface yaml-key-face
  '((t :foreground "#9ccfd8"))
  "Face for YAML keys (properties)."
  :group 'yaml)

(defface yaml-template-var-face
  '((t :foreground "#c4a7e7"))
  "Face for template variables."
  :group 'yaml)

;; Simple regular expression for keys
(defconst yaml-key-re "^[ \t]*\\(.*?\\):"
  "Regexp matching YAML keys - everything up to the first colon.")

(defconst yaml-template-var-re "{{[^}]*}}"
  "Regexp matching template variables like {{ .Release.Name }}")

;; Syntax table
(defvar yaml-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `yaml-mode'.")

;; Font-lock keywords
(defvar yaml-font-lock-keywords
  `(
    (,yaml-key-re 1 'yaml-key-face)
    (,yaml-template-var-re 0 'yaml-template-var-face)
   )
  "Font lock keywords for `yaml-mode'.")

;; ;; Define the mode
;; (define-derived-mode my-yaml-mode prog-mode "YAML"
;;   "Major mode for editing YAML files."
;;   :syntax-table yaml-mode-syntax-table
;;   (set (make-local-variable 'font-lock-defaults)
;;        '(yaml-font-lock-keywords nil nil nil nil))
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "#+ *"))

(defun my-yaml-indent-line ()
  "Indent current line as YAML code.
If the last non-whitespace character of the previous line is a colon,
indent one tab-width further than that line.
Otherwise, indent to the same indentation as the previous line."
  (interactive)
  (let (target-indent)
    (if (bobp)
        (setq target-indent 0)  ; If at beginning of buffer, no indentation.
      (save-excursion
        (forward-line -1)  ; Move to the previous line.
        (let ((prev-indent (current-indentation)))
          (end-of-line)
          (skip-chars-backward " \t")
          (if (eq (char-before) ?:)
              (setq target-indent (+ prev-indent tab-width))
            (setq target-indent prev-indent)))))
    (indent-line-to target-indent)))

(define-derived-mode my-yaml-mode prog-mode "YAML"
  "Major mode for editing YAML files."
  :syntax-table yaml-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(yaml-font-lock-keywords nil nil nil nil))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+ *")
    ;; Disable electric indent
  (set (make-local-variable 'electric-indent-inhibit) t)
  ;; Set our custom indentation function
  (set (make-local-variable 'indent-line-function) 'my-yaml-indent-line))

;; Add file associations
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . my-yaml-mode))

(provide 'my-yaml-mode)
