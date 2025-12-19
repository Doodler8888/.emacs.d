;; -*- lexical-binding: t -*-

(add-hook 'bash-ts-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))
