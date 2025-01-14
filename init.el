;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.source/org-mode/lisp")

(require 'package)
(setq use-package-always-ensure t)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (pcakage-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (org-babel-load-file
;;   (expand-file-name
;;     "config.org"
;;     user-emacs-directory))

(load "~/.emacs.d/config.el")
(load "~/.emacs.d/other/custom-functions.el")
(load "~/.emacs.d/other/meow.el")
(load "~/.emacs.d/eshell/eshell.el")
(load "~/.emacs.d/other/dired.el")
(load "~/.emacs.d/other/keybindings.el")
(load "~/.emacs.d/other/save-org-headings.el")
(load "~/.emacs.d/other/scripts/install-org.el")

;; (load "~/.emacs.d/eshell/eshell-doc.el")
;; (load "~/.emacs.d/eshell/bak.el")

;; (custom-set-variables
;;  '(auth-source-save-behavior nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(avy buffer-terminator c3-ts-mode cape clojure-ts-mode corfu docker
         dockerfile-mode embark-consult envrc exec-path-from-shell f
         fish-completion go-mode goto-chg haskell-mode hydra lua-mode magit
         marginalia meow nix-mode orderless org-appear org-drill raku-mode
         tempel-collection terraform-mode transpose-frame treesit-auto undo-tree
         vertico wgrep yaml-mode))
 '(package-vc-selected-packages
   '((fish-completion :url
                      "https://github.com/LemonBreezes/emacs-fish-completion.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
