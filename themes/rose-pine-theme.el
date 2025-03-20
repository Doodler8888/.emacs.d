(deftheme rose-pine "Rose Pine theme")

(let ((class '((class color) (min-colors 89)))
      (rose-pine-fg "#e0def4")
      (rose-pine-fg2 "#b2aec2")
      (rose-pine-blendedbg "#17191a")
      (rose-pine-bg "#1d1f21")
      (rose-pine-bg2 "#2e2c3d")
      (meow-region "#262431")
      (rose-pine-rose "#ebbcba")
      (rose-pine-love "#eb6f92")
      (rose-pine-blendedlove "#2e202f")
      (rose-pine-gold "#f6c177")
      (rose-pine-pine "#31748f")
      (rose-pine-foam "#9ccfd8")
      (rose-pine-iris "#c4a7e7")
      (rose-pine-highlight "#403d52")
      (rose-pine-highlight2 "#26233a")
      (rose-pine-subtext "#6e6a86")
      (rose-pine-subtext0 "#908caa")
      (rose-pine-subtle "#2e2c3d")
      (rose-pine-blendedfoam "#403d52")
      (nord-black "#2E3440")
      (nord-bright-black "#3B4252")
      (nord-blue "#5E81AC")
      (nord-bright-blue "#81A1C1")
      (nord-cyan "#8FBCBB")
      (nord-bright-cyan "#88C0D0")
      (nord-green "#A3BE8C")
      (nord-magenta "#B48EAD")
      (nord-red "#BF616A")
      (nord-white "#E5E9F0")
      (nord-bright-white "#ECEFF4")
      (nord-yellow "#EBCB8B")
      )

  (custom-theme-set-faces
   'rose-pine

   ;; General
   `(default ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg))))
   ;; `(bold ((,class (:foreground ,rose-pine-fg2 :weight bold))))
   `(bold ((,class (:foreground ,rose-pine-subtext :weight bold))))
   `(italic ((,class (:foreground ,rose-pine-fg2 :slant italic))))

   ;; Line highlight
   ;; `(hl-line ((,class (:background ,rose-pine-highlight))))

   ;; Fringe
   ;; `(fringe ((,class (:foreground ,rose-pine-blendedbg :foreground ,rose-pine-blendedbg))))
   `(fringe ((,class (:foreground ,rose-pine-blendedbg))))

   ;; Font lock
   
   ;; `(show-paren-match ((,class (:foreground ,rose-pine-fg :weight bold :background ,rose-pine-subtle))))
   `(font-lock-builtin-face ((,class (:foreground ,rose-pine-love))))
   ;; `(font-lock-constant-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-function-name-face ((,class (:foreground ,rose-pine-rose))))
   `(font-lock-keyword-face ((,class (:foreground ,rose-pine-pine))))
   `(font-lock-string-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-type-face ((,class (:foreground ,rose-pine-iris))))
   `(font-lock-variable-name-face ((,class (:foreground ,rose-pine-fg))))
   `(font-lock-comment-face ((,class (:foreground ,rose-pine-subtext))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,rose-pine-subtext))))
   `(font-lock-escape-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-property-use-face ((,class (:foreground ,rose-pine-foam))))

   
   `(sh-quoted-exec ((,class (:foreground ,rose-pine-iris))))
   `(sh-heredoc ((,class (:foreground ,rose-pine-iris))))

   ;; `(font-lock-doc-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-warning-face ((,class (:foreground ,rose-pine-fg))))
   ;; `(font-lock-regexp-grouping-construct ((t (:foreground ,rose-pine-fg :bold t))))
   ;; `(font-lock-regexp-grouping-backslash ((t (:foreground ,rose-pine-fg :bold t))))

   ;; Miscellanious
    (set-face-attribute 'vertical-border nil :foreground rose-pine-subtext)
    ;; (set-face-attribute 'tab-bar nil :background "color1")
    (set-face-attribute 'tab-bar nil :background "#1d1f21")
    (set-face-attribute 'tab-bar nil :height 1.05)
    (set-face-attribute 'tab-bar nil
                    :font "NotoSansM Nerd Font Mono-12:weight=medium")

    (set-face-attribute 'tab-bar-tab nil
        		:font "NotoSansM Nerd Font Mono-12:weight=medium")

    ;; Hydra
   `(hydra-face-amaranth ((,class (:foreground ,rose-pine-love))))
   `(hydra-face-red ((,class (:foreground ,rose-pine-love))))
   `(hydra-face-rose ((,class (:foreground ,rose-pine-rose))))
   `(hydra-face-blue ((,class (:foreground ,rose-pine-iris))))
       
    ;; Ivy
    `(ivy-current-match ((,class (:background ,rose-pine-bg2))))
    `(ivy-modified-buffer ((,class (:foreground ,rose-pine-fg))))
    `(ivy-modified-outside-buffer ((,class (:foreground ,rose-pine-fg))))
    `(ivy-virtual ((,class (:foreground ,rose-pine-fg))))
    `(all-the-icons-ivy-rich-time-face ((,class (:foreground ,rose-pine-subtext))))
    `(all-the-icons-ivy-rich-package-status-installed-face ((,class (:foreground ,rose-pine-subtext))))
    `(all-the-icons-ivy-rich-off-face ((,class (:foreground ,rose-pine-subtext))))
    `(all-the-icons-ivy-rich-file-priv-no ((,class (:foreground ,rose-pine-subtext))))
    `(all-the-icons-completion-dir-face ((,class (:foreground ,rose-pine-gold))))
    `(all-the-icons-dsilver ((,class (:foreground ,rose-pine-fg))))

    ;; Evil
    `(evil-ex-info ((,class (:foreground ,rose-pine-love))))
    `(evil-ex-substitute-replacement ((,class (:foreground ,rose-pine-love))))
    `(shadow ((,class (:foreground ,rose-pine-subtext))))

    ;; Preview mode
    `(completion-preview ((,class (:foreground ,rose-pine-subtext))))
    `(completion-preview-common ((,class (:foreground ,rose-pine-subtext))))
    `(completion-preview-exact ((,class (:foreground ,rose-pine-subtext :underline ,rose-pine-subtext))))

    ;; Vterm
    `(all-the-icons-ivy-rich-file-priv-no ((,class (:foreground ,rose-pine-subtext))))
    
    ;; Avy
    `(avy-lead-face ((,class (:background ,rose-pine-bg2))))
    `(avy-lead-face-0 ((,class (:background ,rose-pine-bg2))))
    `(avy-lead-face-1 ((,class (:background ,rose-pine-bg2))))
    `(avy-lead-face-2 ((,class (:background ,rose-pine-bg2))))

    ;; Kubernetes
    `(kubernetes-context-name ((,class (:foreground ,rose-pine-fg))))
    `(kubernetes-namespace ((,class (:foreground ,rose-pine-fg))))
    `(kubernetes-dimmed ((,class (:foreground ,rose-pine-subtext))))
    `(kubernetes-json-key ((,class (:foreground ,rose-pine-gold))))
    `(kubernetes-pending-deletion ((,class (:foreground ,rose-pine-love))))
    `(kubernetes-progress-indicator ((,class (:foreground ,rose-pine-foam))))
    `(kubernetes-selector ((,class (:foreground ,rose-pine-fg))))
    `(kubernetes-delete-mark ((,class (:foreground ,rose-pine-iris))))
    `(magit-section-heading ((,class (:foreground ,rose-pine-foam))))
    `(magit-section-highlight ((,class (:background ,rose-pine-subtle))))
    
    ;; Docker
    `(docker-face-dangling ((,class (:foreground ,rose-pine-subtext))))

    ;; Eglot
    `(eglot-diagnostic-tag-unnecessary-face ((,class (:foreground ,rose-pine-subtext))))
    
    ;; Ansi
    `(ansi-color-black ((,class (:foreground ,nord-black :background ,nord-black))))
    `(ansi-color-bright-black ((,class (:foreground ,nord-bright-black :background ,nord-bright-black))))
    `(ansi-color-blue ((,class (:foreground ,nord-blue :background ,nord-blue))))
    `(ansi-color-bright-blue ((,class (:foreground ,nord-bright-blue :background ,nord-bright-blue))))
    `(ansi-color-cyan ((,class (:foreground ,nord-cyan :background ,nord-cyan))))
    `(ansi-color-bright-cyan ((,class (:foreground ,nord-bright-cyan :background ,nord-bright-cyan))))
    `(ansi-color-green ((,class (:foreground ,rose-pine-foam :background ,rose-pine-foam))))
    ;; `(ansi-color-bright-green ((,class (:foreground ,nord-green :background ,nord-green))))
    `(ansi-color-bright-green ((,class (:foreground ,rose-pine-foam :background ,rose-pine-foam))))
    ;; `(ansi-color-magenta ((,class (:foreground ,nord-magenta :background ,nord-magenta))))
    `(ansi-color-magenta ((,class (:foreground ,rose-pine-iris :background ,rose-pine-iris))))
    `(ansi-color-bright-magenta ((,class (:foreground ,rose-pine-iris :background ,rose-pine-iris))))
    ;; `(ansi-color-red ((,class (:foreground ,nord-red :background ,nord-red))))
    `(ansi-color-red ((,class (:foreground ,rose-pine-love :background ,rose-pine-love))))
    `(ansi-color-bright-red ((,class (:foreground ,rose-pine-love :background ,rose-pine-love))))
    ;; `(ansi-color-white ((,class (:foreground ,nord-white :background ,nord-white))))
    `(ansi-color-white ((,class (:foreground ,rose-pine-fg :background ,rose-pine-fg))))
    `(ansi-color-bright-white ((,class (:foreground ,rose-pine-fg :background ,rose-pine-fg))))
    `(ansi-color-yellow ((,class (:foreground ,rose-pine-gold :background ,rose-pine-gold))))
    `(ansi-color-bright-yellow ((,class (:foreground ,rose-pine-gold :background ,rose-pine-gold))))
    `(ansi-color-bold ((,class (:foreground ,rose-pine-gold))))

    ;; Haskell mode
    `(haskell-keyword-face ((,class (:foreground ,rose-pine-pine))))  ; For keywords like 'do'
    `(haskell-constructor-face ((,class (:foreground ,rose-pine-foam))))  ; For type constructors like 'IO'
    `(haskell-operator-face ((,class (:foreground ,rose-pine-subtext))))  ; For operators
    `(haskell-definition-face ((,class (:foreground ,rose-pine-rose))))  ; For function definitions
    `(haskell-type-face ((,class (:foreground ,rose-pine-foam))))  ; For type annotations
 
    ;; Terraform mode
    `(terraform-resource-type-face ((,class (:foreground ,rose-pine-iris))))
    `(terraform-builtin-face ((,class (:foreground ,rose-pine-rose))))
    `(terraform-resource-name-face ((,class (:foreground ,rose-pine-pine))))

    ;; Clojure ts mode
    `(clojure-ts-keyword-face ((,class (:foreground ,rose-pine-foam))))
 
    ;; Raku mode
    `(raku-operator ((,class (:foreground ,rose-pine-subtext))))

    ;; Dired mode
    `(dired-directory ((,class (:foreground ,rose-pine-gold))))
    `(dired-symlink ((,class (:foreground ,rose-pine-subtext))))
    `(dired-ignored ((,class (:foreground ,rose-pine-subtext))))
    `(dired-marked ((,class (:foreground ,rose-pine-iris))))
    `(dired-flagged ((,class (:foreground ,rose-pine-iris))))
    `(dired-async-failures ((,class (:foreground ,rose-pine-love))))
    `(dired-async-message ((,class (:foreground ,rose-pine-gold))))
    `(dired-async-mode-message ((,class (:foreground ,rose-pine-gold))))
    `(dired-broken-symlink ((,class (:foreground ,rose-pine-love))))
 
    ;; Ansible mode
    `(ansible-task-label-face ((,class (:foreground ,rose-pine-foam))))
    `(ansible-section-face ((,class (:foreground ,rose-pine-iris))))

	;; Perl mode
    `(perl-non-scalar-variable ((,class (:foreground ,rose-pine-fg))))
	
    ;; Org mode
    ;; `(org-block ((t (:background ,rose-pine-blendedbg))))  ; Set background color for org blocks
    `(org-block ((t (:background unspecified))))  ; Set background color for org blocks
    ;; `(org-block-begin-line ((t (:foreground ,rose-pine-subtext :background ,rose-pine-blendedbg))))  ; Set colors for begin line
    `(org-block-begin-line ((t (:foreground ,rose-pine-subtext :background unspecified))))  ; Set colors for begin line
    ;; `(org-block-end-line ((t (:foreground ,rose-pine-subtext :background ,rose-pine-blendedbg))))
    `(org-block-end-line ((t (:foreground ,rose-pine-subtext :background unspecified))))
     ;; (set-face-attribute 'org-level-2 nil :foreground rose-pine-foam)
     ;; (set-face-attribute 'org-level-3 nil :foreground rose-pine-iris)
    `(org-level-1 ((,class (:foreground ,rose-pine-rose))))
    `(org-level-2 ((,class (:foreground ,rose-pine-foam))))
    `(org-level-3 ((,class (:foreground ,rose-pine-iris))))
    `(org-level-4 ((,class (:foreground ,rose-pine-subtext))))
    `(org-level-5 ((,class (:foreground ,rose-pine-pine))))
    ;; `(org-level-6 ((,class (:foreground ,rose-pine-pine))))
    `(org-table ((,class (:foreground ,rose-pine-fg))))
    `(org-document-info-keyword ((,class (:foreground ,rose-pine-fg))))
    `(org-document-info ((,class (:foreground ,rose-pine-gold))))
    `(org-document-title ((,class (:foreground ,rose-pine-gold))))
    `(org-drawer ((,class (:foreground ,rose-pine-subtext))))
    `(org-date ((,class (:foreground ,rose-pine-gold))))
    `(org-code ((,class (:foreground ,rose-pine-fg2 :background ,rose-pine-blendedbg :weight regular))))
    ;; `(org-code ((,class (:foreground ,rose-pine-foam :background ,rose-pine-blendedbg :weight regular))))
    ;; `(org-code ((,class (:foreground ,rose-pine-fg2 :background ,"#2a2c2d" :weight regular))))
	
    ;; `(org-code ((,class (:foreground ,rose-pine-fg2 :background ,rose-pine-blendedbg :box (:line-width 1 :color ,rose-pine-fg2)))))
    `(org-verbatim ((,class (:foreground ,rose-pine-fg2))))

    ;; Python ts mode
    `(font-lock-number-face ((,class (:foreground ,rose-pine-subtext0))))
    `(font-lock-constant-face ((,class (:foreground ,rose-pine-foam))))
 
    ;; Flymake
    `(flymake-end-of-line-diagnostics-face ((,class (:box nil))))
    `(flymake-error-echo-at-eol ((,class (:box nil))))
    `(flymake-note-echo-at-eol ((,class (:box nil))))
    `(flymake-warning-echo-at-eol ((,class (:box nil))))

    ;; Vimish mode
    `(vimish-fold-overlay ((,class (:background ,rose-pine-bg2))))

    ;; `(org-block-begin-line ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))
    ;; `(org-block-end-line ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))
    ;; `(org-code ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-bg))))

    ;; Completion
    `(completions-highlight ((,class (:background ,rose-pine-subtle))))
    `(icomplete-selected-match ((,class (:background ,rose-pine-subtle))))
    `(highlight ((,class (:background ,rose-pine-subtle))))
    
    ;; Vertico
    `(vertico-current ((,class (:background ,rose-pine-subtle))))
    ;; `(vertico-current ((,class (:box (:line-width -1 :color ,rose-pine-rose)))))

    ;; Consult
    `(consult-highlight-mark ((,class (:foreground unspecified :background ,rose-pine-subtle))))
    `(consult-highlight-match ((,class (:foreground unspecified :background ,rose-pine-subtle))))
    ;; `(consult-preview-match ((,class (:foreground nil :background ,rose-pine-iris))))

    ;; Orderless
    `(orderless-match-face-0 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-subtext))))
    `(orderless-match-face-1 ((,class (:foreground ,rose-pine-iris :background ,rose-pine-subtext))))
    `(orderless-match-face-2 ((,class (:foreground ,rose-pine-foam :background ,rose-pine-subtext))))
    `(orderless-match-face-3 ((,class (:foreground ,rose-pine-gold :background ,rose-pine-subtext))))
    
    ;; Ivy
    `(ivy-current-match ((,class (:background ,rose-pine-subtle))))
    `(ivy-minibuffer-match-face-1 ((,class (:background ,rose-pine-subtext))))
    `(ivy-minibuffer-match-face-2 ((,class (:background ,rose-pine-subtext))))
    `(ivy-minibuffer-match-face-3 ((,class (:background ,rose-pine-subtext))))
    `(ivy-minibuffer-match-face-4 ((,class (:background ,rose-pine-subtext))))
    ;; `(ivy-minibuffer-match-face-1 ((,class (:foreground ,rose-pine-fg))))
    ;; `(ivy-minibuffer-match-face-2 ((,class (:foreground ,rose-pine-fg))))
    ;; `(ivy-minibuffer-match-face-3 ((,class (:foreground ,rose-pine-fg))))
    ;; `(ivy-minibuffer-match-face-4 ((,class (:foreground ,rose-pine-fg))))
 
    ;; Yassnippets
    `(yas-field-highlight-face ((,class (:background unspecified))))

    ;; Tempel
    `(tempel-field ((,class (:foreground ,rose-pine-fg :background unspecified))))
    `(tempel-default ((,class (:foreground ,rose-pine-subtext :background ,rose-pine-blendedbg))))
    ;; `(tempel-form ((,class (:foreground ,rose-pine-fg :background ,rose-pine-iris))))

    ;; Swiper
    `(swiper-line-face ((,class (:background ,rose-pine-subtle))))
    ;; `(swiper-background-match-face-1 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-subtle))))
    ;; `(swiper-background-match-face-2 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-subtle))))
    ;; `(swiper-background-match-face-3 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-subtle))))
    ;; `(swiper-background-match-face-4 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-subtle))))
    ;; `(swiper-match-face-1 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg2))))
    ;; `(swiper-match-face-2 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg2))))
    ;; `(swiper-match-face-2 ((,class (:foreground ,rose-pine-fg :background ,palevioletred2))))
    ;; `(swiper-match-face-3 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg2))))
    ;; `(swiper-match-face-4 ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg2))))

    ;; Line numbers
    `(linum ((,class (:foreground ,rose-pine-subtext))))
    `(line-number ((,class (:foreground ,rose-pine-subtext))))
 
    ;; Region
    ;; `(region ((,class (:background ,rose-pine-subtle))))
    `(region ((,class (:background ,"#262431"))))
 
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
 
    ;; Eshell
    `(eshell-ls-special ((,class (:foreground ,rose-pine-iris))))
    `(eshell-ls-unreadable ((,class (:foreground ,rose-pine-fg2))))
    `(eshell-ls-missing ((,class (:foreground ,rose-pine-love))))
    `(eshell-ls-backup ((,class (:foreground ,rose-pine-subtext))))
    `(eshell-ls-executable ((,class (:foreground ,rose-pine-pine))))
    `(eshell-ls-readonly ((,class (:foreground ,rose-pine-subtext))))
    `(eshell-ls-archive ((,class (:foreground ,rose-pine-iris))))

    ;; Meow
    ;; `(meow-position-highlight-number ((,class (:foreground ,rose-pine-bg :background ,rose-pine-rose))))
    ;; `(meow-position-highlight-number-1 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-highlight))))
    ;; `(meow-position-highlight-number-2 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-highlight2))))
    ;; `(meow-position-highlight-number-3 ((,class (:foreground nil :background nil))))
    ;; `(meow-position-highlight-reverse-number-1 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-highlight))))
    ;; `(meow-position-highlight-reverse-number-2 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-highlight2))))
    ;; `(meow-position-highlight-reverse-number-3 ((,class (:foreground nil :background nil))))
    ;; `(meow-region-cursor-1 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-rose))))
    ;; `(meow-region-cursor-2 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-rose))))
    ;; `(meow-region-cursor-3 ((,class (:foreground ,rose-pine-bg :background ,rose-pine-rose))))
    ;; meow-region-cursor-1
    
    ;; More faces
    `(cursor ((,class (:foreground ,rose-pine-fg :background ,rose-pine-fg))))
    `(show-paren-mismatch ((,class (:foreground ,rose-pine-fg :background ,rose-pine-love))))
    `(show-paren-match ((t (:background ,rose-pine-subtext :foreground ,rose-pine-fg :weight bold))))
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

;; (defun my/set-flymake-faces ()
;;   (with-eval-after-load 'flymake
;;     (set-face-attribute 'flymake-error nil
;;                         ;; :underline `(:style line :color "#e0def4")) ; rose-pine-gold
;; 						:underline `(:style line :color "#e0def4" :underline undefined)) ; rose-pine-gold
;;     (set-face-attribute 'flymake-warning nil
;;                         :underline `(:style line :color "#f6c177" :underline undefined)) ; rose-pine-gold
;;     (set-face-attribute 'flymake-note nil
;;                         :underline `(:style line :color "#c4a7e7" :underline undefined)))) ; rose-pine-iris

;; (add-hook 'after-init-hook 'my/set-flymake-faces)

(add-hook 'yaml-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-variable-name-face
                                    :foreground "#9ccfd8")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)
