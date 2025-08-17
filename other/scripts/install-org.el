;; -*- lexical-binding: t -*-

(defun install-org-from-source ()
  "Install org-mode from source."
  (interactive)
  (let* ((current-org-path (locate-library "org"))
         (emacs-path (expand-file-name invocation-name invocation-directory))
         (source-dir (expand-file-name "~/.source")))
    
    (message "Using Emacs from: %s" emacs-path)
    
    (unless (file-exists-p source-dir)
      (if (y-or-n-p "~/.source directory doesn't exist. Create it? ")
          (make-directory source-dir t)
        (error "Aborted: source directory is required")))
    
    (let ((org-dir (expand-file-name "org-mode" source-dir)))
      (if (file-exists-p org-dir)
          (when (y-or-n-p "org-mode directory exists. Update it? ")
            (async-shell-command (format "cd %s && git pull" org-dir) "*org-update*"))
        (when (y-or-n-p "Clone org-mode repository? ")
          (async-shell-command 
           (format "git clone https://git.savannah.gnu.org/git/emacs/org-mode.git %s" org-dir)
           "*org-clone*")))
      
      (when (and (file-exists-p org-dir)
                (y-or-n-p "Compile org-mode? "))
        (let ((process (start-process-shell-command 
                       "org-compile" "*org-compile*"
                       (format "cd %s && make clean && make" org-dir))))
          (set-process-sentinel 
           process
           (lambda (proc event)
             (when (string= event "finished\n")
               (let ((load-path-line (format "(add-to-list 'load-path \"%s/lisp\")" org-dir)))
                 (message "Compilation finished. Add this line to your init.el to use the new org-mode:\n%s" 
                         load-path-line))))))))))
