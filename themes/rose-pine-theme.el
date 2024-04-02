(deftheme rose-pine "Rose Pine theme")

(let ((class '((class color) (min-colors 89)))
      (rose-pine-fg "#e0def4")
      (rose-pine-fg2 "#b2aec2")
      (rose-pine-blendedbg "#17191a")
      (rose-pine-bg "#1d1f21")
      (rose-pine-rose "#ebbcba")
      (rose-pine-love "#eb6f92")
      (rose-pine-blendedlove "#2e202f")
      (rose-pine-gold "#f6c177")
      (rose-pine-pine "#31748f")
      (rose-pine-foam "#9ccfd8")
      (rose-pine-iris "#c4a7e7")
      ;; (rose-pine-highlight "#26233a")
      (rose-pine-highlight "#403d52")
      (rose-pine-subtext "#6e6a86")
      (rose-pine-subtext0 "#908caa")
      (rose-pine-blendedfoam "#403d52"))

  (custom-theme-set-faces
   'rose-pine

   ;; General
   `(default ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg))))
   ;; `(bold ((,class (:foreground ,rose-pine-fg2 :weight bold))))
   `(bold ((,class (:foreground ,rose-pine-iris :weight bold))))
   `(italic ((,class (:foreground ,rose-pine-fg2 :slant italic))))

   ;; Line highlight
   ;; `(hl-line ((,class (:background ,rose-pine-highlight))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,rose-pine-love))))
   `(font-lock-constant-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-function-name-face ((,class (:foreground ,rose-pine-rose))))
   `(font-lock-keyword-face ((,class (:foreground ,rose-pine-pine))))
   `(font-lock-string-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-type-face ((,class (:foreground ,rose-pine-iris))))
   `(font-lock-variable-name-face ((,class (:foreground ,rose-pine-fg))))
   `(font-lock-comment-face ((,class (:foreground ,rose-pine-subtext))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,rose-pine-subtext))))
   `(sh-quoted-exec ((,class (:foreground ,rose-pine-iris))))

   ;; `(font-lock-doc-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-warning-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-regexp-grouping-construct ((t (:foreground ,rose-pine-fg :bold t))))
   ;; `(font-lock-regexp-grouping-backslash ((t (:foreground ,rose-pine-fg :bold t))))

   ;; Miscellanious
    (set-face-attribute 'vertical-border nil :foreground rose-pine-subtext)
    (set-face-attribute 'tab-bar nil :background "color1")
    (set-face-attribute 'tab-bar nil :height 1.05)
    (set-face-attribute 'tab-bar nil
                    :font "NotoSansM Nerd Font Mono-12:weight=medium")

    (set-face-attribute 'tab-bar-tab nil
			:font "NotoSansM Nerd Font Mono-12:weight=medium")


   ;; Haskell mode specific faces
   `(haskell-keyword-face ((,class (:foreground ,rose-pine-pine))))  ; For keywords like 'do'
   `(haskell-constructor-face ((,class (:foreground ,rose-pine-foam))))  ; For type constructors like 'IO'
   `(haskell-operator-face ((,class (:foreground ,rose-pine-subtext))))  ; For operators
   `(haskell-definition-face ((,class (:foreground ,rose-pine-rose))))  ; For function definitions
   `(haskell-type-face ((,class (:foreground ,rose-pine-foam))))  ; For type annotations

   ;; Haskell mode specific faces
   ;; `(haskell-keyword-face ((,class (:foreground ,rose-pine-pine))))  ; For keywords like 'do'
   ;; `(haskell-constructor-face ((,class (:foreground ,rose-pine-foam))))  ; For type constructors like 'IO'
   ;; `(haskell-operator-face ((,class (:foreground ,rose-pine-subtext))))  ; For operators
   ;; `(haskell-definition-face ((,class (:foreground ,rose-pine-rose))))  ; For function definitions
   ;; `(haskell-type-face ((,class (:foreground ,rose-pine-foam))))  ; For type annotations

   ;; Dired mode
   `(dired-directory ((,class (:foreground ,rose-pine-gold))))
   `(dired-symlink ((,class (:foreground ,rose-pine-foam))))
   `(dired-ignored ((,class (:foreground ,rose-pine-subtext))))

   ;; Org mode
   `(org-block ((t (:background ,rose-pine-blendedbg))))  ; Set background color for org blocks
   `(org-block-begin-line ((t (:foreground ,rose-pine-subtext :background ,rose-pine-blendedbg))))  ; Set colors for begin line
   `(org-block-end-line ((t (:foreground ,rose-pine-subtext :background ,rose-pine-blendedbg))))
    (set-face-attribute 'org-level-2 nil :foreground rose-pine-foam)
    (set-face-attribute 'org-level-3 nil :foreground rose-pine-iris)
   `(org-table ((,class (:foreground ,rose-pine-fg))))
   `(org-document-info-keyword ((,class (:foreground ,rose-pine-fg))))
   `(org-document-info ((,class (:foreground ,rose-pine-gold))))
   `(org-document-title ((,class (:foreground ,rose-pine-gold))))
   `(org-drawer ((,class (:foreground ,rose-pine-subtext))))
   `(org-date ((,class (:foreground ,rose-pine-gold))))
   `(org-code ((,class (:foreground ,rose-pine-fg2 :background ,rose-pine-blendedbg))))
   ;; `(org-verbatim ((,class (:foreground ,rose-pine-fg2 :background ,rose-pine-blendedbg))))
   `(org-verbatim ((,class (:foreground ,rose-pine-fg2))))

   ;; `(org-block-begin-line ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))
   ;; `(org-block-end-line ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))
   ;; `(org-code ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))

   ;; Ivy
   `(ivy-current-match ((,class (:background ,rose-pine-subtext))))
   ;; `(ivy-minibuffer-match-face-1 ((,class (:foreground ,rose-pine-fg))))
   ;; `(ivy-minibuffer-match-face-2 ((,class (:foreground ,rose-pine-fg))))
   ;; `(ivy-minibuffer-match-face-3 ((,class (:foreground ,rose-pine-fg))))
   ;; `(ivy-minibuffer-match-face-4 ((,class (:foreground ,rose-pine-fg))))

   ;; Line numbers
   `(linum ((,class (:foreground ,rose-pine-subtext))))
   `(line-number ((,class (:foreground ,rose-pine-subtext))))

   ;; Region
   `(region ((,class (:background ,rose-pine-highlight))))

   ;; Mode line
   `(mode-line-buffer-id ((,class (:weight normal))))
   `(mode-line ((,class (:foreground ,rose-pine-love
			      :background ,rose-pine-blendedlove
			      :box (:line-width 1 :color ,rose-pine-bg)
			      :weight normal))))  ; Set the weight to normal
   `(mode-line-inactive ((,class (:foreground ,rose-pine-love
			      :background ,rose-pine-blendedlove
			      :box (:line-width 1 :color ,rose-pine-bg)
			      :weight normal))))  ; Set the weight to normal

   ;; More faces
   `(cursor ((,class (:foreground ,rose-pine-fg :background ,rose-pine-fg))))
   `(show-paren-match ((t (:background ,rose-pine-subtext :foreground ,rose-pine-fg :weight bold))))  ; Customize matching bracket appearance
   `(match ((,class (:background ,rose-pine-love :foreground ,rose-pine-bg))))
   `(minibuffer-prompt ((,class (:foreground ,rose-pine-iris))))
   `(trailing-whitespace ((,class (:background ,rose-pine-love))))
   `(link ((,class (:foreground ,rose-pine-foam :underline t))))
   `(link-visited ((,class (:foreground ,rose-pine-iris :underline t))))
   `(highlight ((,class (:background ,rose-pine-subtext))))
   `(success ((,class (:foreground ,rose-pine-foam))))
   `(warning ((,class (:foreground ,rose-pine-gold))))
   `(error ((,class (:foreground ,rose-pine-love))))

))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)
