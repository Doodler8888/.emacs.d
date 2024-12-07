(require 'commander)
(require 'esh-mode)

(defun add-bak (file)
  (eshell-printn (format "Adding .bak to: %s" file)))

(defun remove-bak (file)
  (eshell-printn (format "Removing .bak from: %s" file)))

(defun eshell/bak (&rest args)
  "CLI utility to handle .bak files. Only available in eshell.
Usage: bak --add file.txt or bak --remove file.txt.bak"
  (let ((commander-args args))
    (commander
     (option "--add <file>" "Add .bak extension to file" 
             (lambda (file) (eshell-printn (format "Adding .bak to: %s" file))))
     (option "--remove <file>" "Remove .bak extension from file" 
             (lambda (file) (eshell-printn (format "Removing .bak from: %s" file)))))))
