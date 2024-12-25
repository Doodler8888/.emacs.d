;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.source/org-mode/lisp")

(require 'package)
(setq use-package-always-ensure t)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (org-babel-load-file
;;   (expand-file-name
;;     "config.org"
;;     user-emacs-directory))

(load "~/.emacs.d/config.el")
;; (load "~/.emacs.d/other/evil.el")
(load "~/.emacs.d/other/meow.el")
(load "~/.emacs.d/eshell/eshell.el")
(load "~/.emacs.d/other/dired.el")
(load "~/.emacs.d/other/keybindings.el")
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
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((tab-bookmark :url "https://github.com/minad/tab-bookmark")
     (eat :url "https://github.com/kephale/emacs-eat")
     (emacs-eat :url "https://github.com/kephale/emacs-eat")
     (zoxide :url "https://gitlab.com/Vonfry/zoxide.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
