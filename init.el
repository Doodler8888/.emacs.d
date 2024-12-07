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

;; (org-babel-load-file
;;   (expand-file-name
;;     "config.org"
;;     user-emacs-directory))

;; (load "~/.emacs.d/packages/baremeow.el")
;; (load "~/.emacs.d/packages/meow.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/eshell/eshell.el")
(load "~/.emacs.d/eshell/eshell-doc.el")
(load "~/.emacs.d/eshell/bak.el")

;; (custom-set-variables
;;  '(auth-source-save-behavior nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-dired avy cape clojure-ts-mode
                              commander corfu docker dockerfile-mode eat
                              emacs-eat embark-consult envrc
                              eshell-syntax-highlighting evil-collection
                              evil-org evil-surround exec-path-from-shell
                              fish-completion flymake-ansible-lint
                              flymake-hadolint flymake-shellcheck go-mode
                              haskell-mode hydra lua-mode magit marginalia meow
                              nix-mode orderless org-bullets org-download
                              org-drill org-modern popper pyvenv raku-mode
                              shackle sudo-edit swiper systemd terraform-mode
                              toc-org transpose-frame treesit-auto undo-tree
                              vertico wgrep yaml-mode yasnippet zoxide))
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
