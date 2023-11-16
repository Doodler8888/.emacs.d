   (defun counsel-find-file-check-dir ()
     "Like `counsel-find-file', but use `find-file-check-dir' instead of `find-file'."
     (interactive)
     (ivy-read "Find file: " #'read-file-name-internal
               :matcher #'counsel--find-file-matcher
               :action #'find-file-check-dir
               :preselect (or (buffer-file-name)
                             (expand-file-name default-directory))
               :require-match 'confirm-after-completion
               :history 'file-name-history
               :keymap counsel-find-file-map
               :caller 'counsel-find-file))
