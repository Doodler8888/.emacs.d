;; -*- lexical-binding: t -*-

(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

(global-set-key (kbd "s-x") 'execute-extended-command)
