(deftheme rose-pine "Rose Pine theme")

(let ((class '((class color) (min-colors 89)))
      (rose-pine-fg "#e0def4")
      (rose-pine-bg "#1d1f21")
      (rose-pine-love "#eb6f92") 
      (rose-pine-gold "#f6c177")
      (rose-pine-pine "#31748f")
      (rose-pine-foam "#9ccfd8")
      (rose-pine-iris "#c4a7e7")
      (rose-pine-highlight "#26233a")
      (rose-pine-subtext1 "#6e6a86")
      (rose-pine-subtext0 "#908caa"))

  (custom-theme-set-faces
   'rose-pine
   
   ;; Default faces
   `(default ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg))))
   `(fringe ((,class (:foreground ,rose-pine-fg :background ,rose-pine-bg))))
   
   ;; Line highlight
   `(hl-line ((,class (:background ,rose-pine-highlight))))
   
   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,rose-pine-love))))
   `(font-lock-constant-face ((,class (:foreground ,rose-pine-gold))))
   `(font-lock-function-name-face ((,class (:foreground ,rose-pine-fg))))
   `(font-lock-keyword-face ((,class (:foreground ,rose-pine-pine))))
   `(font-lock-string-face ((,class (:foreground ,rose-pine-foam))))
   `(font-lock-type-face ((,class (:foreground ,rose-pine-iris))))
   `(font-lock-variable-name-face ((,class (:foreground ,rose-pine-fg))))
   `(font-lock-comment-face ((,class (:foreground ,rose-pine-subtext1))))

   ;; Line numbers
   `(linum ((,class (:foreground ,rose-pine-subtext1))))

   ;; Region
   `(region ((,class (:foreground ,rose-pine-bg :background ,rose-pine-love))))

   ;; Mode line
   `(mode-line ((,class (:foreground ,rose-pine-iris 
                                    :background ,rose-pine-highlight
                                    :box (:line-width 1 :color ,rose-pine-bg)))))
   `(mode-line-inactive ((,class (:foreground ,rose-pine-subtext1
                                             :background ,rose-pine-bg
                                             :box (:line-width 1 :color ,rose-pine-bg)))))

   ;; More faces
   `(cursor ((,class (:foreground ,rose-pine-fg :background ,rose-pine-fg))))
   `(match ((,class (:background ,rose-pine-love :foreground ,rose-pine-bg))))
   `(minibuffer-prompt ((,class (:foreground ,rose-pine-iris))))
   `(trailing-whitespace ((,class (:background ,rose-pine-love))))
   `(link ((,class (:foreground ,rose-pine-foam :underline t))))
   `(link-visited ((,class (:foreground ,rose-pine-iris :underline t))))
   `(highlight ((,class (:background ,rose-pine-subtext1))))
   `(success ((,class (:foreground ,rose-pine-foam))))
   `(warning ((,class (:foreground ,rose-pine-gold))))
   `(error ((,class (:foreground ,rose-pine-love))))

))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)
