;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.source/org-mode/lisp")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package no-littering
  :ensure t
  :init
  (require 'no-littering)
  (no-littering-theme-backups))

;; (org-babel-load-file
;;   (expand-file-name
;;     "config.org"
;;     user-emacs-directory))

(load "~/.emacs.d/config.el")
(load "~/.emacs.d/other/evil.el")
(load "~/.emacs.d/eshell/eshell.el")
(load "~/.emacs.d/other/scripts/install-org.el")

;; (load "~/.emacs.d/eshell/eshell-doc.el")
;; (load "~/.emacs.d/eshell/bak.el")

;; (custom-set-variables
;;  '(auth-source-save-behavior nil))

;; (use-package org-modern
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'org (global-org-modern-mode))
;;   (setq org-modern-fold-stars
;;         '(("◉" . "○")            ; diamonds
;;           (" ◆" . " ◇")          ; flowers
;;           ("  ✦" . "  ✧")        ; stars
;;           ("   ❂" . "   ☸")      ; wheels
;;           ("    ✤" . "    ❃"))))  ; more flowers/stars

;; (defun load-org ()
;;   (interactive)
;;   (load-file "~/.emacs.d/init.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-dired avy buffer-terminator cape
                              clojure-ts-mode commander corfu docker
                              dockerfile-mode eat embark-consult envrc
                              eshell-syntax-highlighting evil-collection
                              evil-surround exec-path-from-shell fish-completion
                              flymake-hadolint flymake-shellcheck go-mode
                              haskell-mode hydra lua-mode magit marginalia
                              nix-mode no-littering orderless org-download
                              org-drill org-modern page-break-lines popper
                              raku-mode shackle sudo-edit systemd
                              tempel-collection terraform-mode toc-org
                              transpose-frame treesit-auto undo-tree vertico
                              wgrep yaml-mode yasnippet zoxide))
 '(package-vc-selected-packages
   '((eat :url "https://github.com/kephale/emacs-eat")
     (emacs-eat :url "https://github.com/kephale/emacs-eat")
     (zoxide :url "https://gitlab.com/Vonfry/zoxide.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
