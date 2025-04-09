;; Store the last valid combination (selection + action)
(defvar my-last-combination nil
  "Stores the last valid combination of selection and action commands.")

;; Store last two executed commands (for building combinations)
(defvar my-command-history nil
  "List to store the last two commands with their arguments.")

;; Track whether the last action was a command or insert
(defvar my-last-action 'command)

;; Expansion count (set via my-meow-digit)
(defvar my-meow-expand-count nil
  "Stores the number of times to expand the selection.")

;; Define groups for selection and action commands using command names
(defvar my-selection-commands '(meow-inner-of-thing meow-bounds-of-thing meow-mark-word meow-next-word meow-next-symbol meow-find meow-till meow-back-word meow-beginning-of-thing meow-end-of-thing)
  "Commands that create selections.")

(defvar my-action-commands '(my/meow-smart-delete my/generic-meow-smart-delete
  surround-region-with-symbol change-surrounding-symbol delete-surrounding-symbol)
  "Commands that operate on a selection.")

(defun my-command-name (cmd)
  "Get the command name from a command object."
  (cond
   ((symbolp cmd) cmd)
   ((subrp cmd)
    (intern (replace-regexp-in-string "#<subr \\(.+?\\)>" "\\1"
                                      (prin1-to-string cmd))))
   ((functionp cmd)
    (let ((cmd-string (prin1-to-string cmd)))
      (cond
       ((string-match-p "my/generic-meow-smart-delete" cmd-string)
        'my/meow-smart-delete)
       ((and (string-match-p "meow-kill" cmd-string)
             (string-match-p "meow-delete" cmd-string))
        'my/meow-smart-delete)
       ((string-match-p "surround-region-with-symbol" cmd-string)
        'surround-region-with-symbol)
	   ((string-match-p "change-surrounding-symbol" cmd-string)
		'change-surrounding-symbol)
       ((string-match-p "Delete the symbols surrounding" cmd-string)
        'delete-surrounding-symbol)
       (t 'unknown-command))))
   (t 'unknown-command)))

(defun my-valid-combination-p (history)
  "Check if the command history forms a valid combination."
  (when (>= (length history) 2)
    (let* ((first-cmd (my-command-name (caar history)))
           (last-cmd (my-command-name (caar (last history)))))
      (and (member first-cmd my-selection-commands)
           (member last-cmd my-action-commands)))))

(defun my-store-command (cmd args)
  "Store a command and its arguments in the history.
For delete-surrounding-symbol, force ARGS to nil."
  (when (eq (my-command-name cmd) 'delete-surrounding-symbol)
    (setq args nil))
  (let ((command-entry (cons cmd args)))
    (setq my-command-history
          (if (< (length my-command-history) 2)
              (append my-command-history (list command-entry))
            (append (cdr my-command-history) (list command-entry))))

    (when (my-valid-combination-p my-command-history)
      (setq my-last-combination
            (list (car my-command-history)
                  my-meow-expand-count
                  (cadr my-command-history))))

    (when (member (my-command-name cmd) my-selection-commands)
      (setq my-meow-expand-count nil))
    (setq my-last-action 'command)))

(defun my-replay-commands ()
  "Replay the last valid combination."
  (interactive)
  (when my-last-combination
    (let* ((selection (nth 0 my-last-combination))
           (expand-count (nth 1 my-last-combination))
           (action (nth 2 my-last-combination)))
      (apply (car selection) (cdr selection))
      (when expand-count
        (meow-expand expand-count))
      (apply (car action) (cdr action)))))

;; --- Command Tracking ---
(defun my-track-command (orig-fun &rest args)
  "Advice function to track command execution.
For delete-surrounding-symbol, store no arguments."
  (let ((cmd (my-command-name orig-fun)))
    (if (eq cmd 'delete-surrounding-symbol)
        (progn
          (my-store-command orig-fun nil)
          (apply orig-fun args))
      (progn
        (my-store-command orig-fun args)
        (apply orig-fun args)))))

(defun my-track-meow-expand (digit)
  "Store expansion count for later replay."
  (setq my-meow-expand-count digit))

;; Advice specific Meow commands for tracking
(dolist (cmd my-selection-commands)
  (advice-add cmd :around #'my-track-command))
(dolist (cmd my-action-commands)
  (advice-add cmd :around #'my-track-command))

;; Advice to track expansion count
(advice-add 'my-meow-digit :after (lambda (&rest args)
                                   (my-track-meow-expand (car args))))

;; Reset expansion count for selection commands
(defun my-reset-expand-count (&rest args)
  "Reset the expansion count for selection commands."
  (setq my-meow-expand-count nil))

(dolist (cmd my-selection-commands)
  (advice-add cmd :before #'my-reset-expand-count))
