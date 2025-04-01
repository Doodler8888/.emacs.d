(defgroup yaml nil
  "YAML major mode"
  :group 'languages)

;; Define faces
(defface yaml-key-face
  '((t :foreground "blue" :weight bold))
  "Face for YAML keys (properties)."
  :group 'yaml)

(defface yaml-template-var-face
  '((t :foreground "purple" :slant italic))
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

;; Define the mode
(define-derived-mode my-yaml-mode prog-mode "YAML"
  "Major mode for editing YAML files."
  :syntax-table yaml-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(yaml-font-lock-keywords nil nil nil nil))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+ *"))

;; Add file associations
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . my-yaml-mode))

(provide 'my-yaml-mode)
