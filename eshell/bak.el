(require 'commander)

(defun add-bak (file)
  (eshell-printn (format "Adding .bak to: %s" file)))

(defun remove-bak (file)
  (eshell-printn (format "Removing .bak from: %s" file)))

(defun bak (&rest args)
  (interactive)
  (let ((commander-args args))
    (commander
     (option "--add <file>" "Add .bak extension to file" add-bak)
     (option "--remove <file>" "Remove .bak extension from file" remove-bak))))
