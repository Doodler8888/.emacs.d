;; ---------- EXISTING COMBINATION CODE ----------
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
(defvar my-selection-commands '(meow-inner-of-thing meow-mark-word meow-next-word meow-next-symbol meow-find meow-till my-forward-char-with-selection my-backward-char-with-selection meow-back-word my/forward-list my/backward-list)
  "Commands that create selections.")
(defvar my-action-commands '(my/meow-smart-delete my/generic-meow-smart-delete
  surround-region-with-symbol change-surrounding-symbol delete-surrounding-symbol)
  "Commands that operate on a selection.")

;; ---------- INSERT MODE RECORDING ----------
;; Variables to store insert mode recording
(defvar my-insert-recording nil
  "Stores the current recording of insert mode keystrokes.")
(defvar my-is-recording nil
  "Whether we are currently recording keystrokes.")
(defvar my-recording-start-point nil
  "Point position when recording started.")

;; ---------- INTEGRATED SYSTEM ----------
;; Add insert operation as a special action type
(defvar my-last-operation nil
  "Stores the last operation: either a command combination or an insert operation.")

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
        'surround-region-with-symbol)
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
                  (cadr my-command-history)))
      ;; Store as the last operation (command combination)
      (setq my-last-operation (list 'commands my-last-combination)))
    
    (when (member (my-command-name cmd) my-selection-commands)
      (setq my-meow-expand-count nil))
    (setq my-last-action 'command)))

;; Function to handle pair characters
(defun my-handle-pairs (str)
  "Check if string starts with a pair character and handle accordingly."
  (cond
   ;; Check for double quotes
   ((and (> (length str) 0)
         (string-equal (substring str 0 1) "\""))
    ;; If string doesn't end with a quote or the last quote is escaped, add one
    (if (or (not (string-equal (substring str -1) "\""))
            (and (> (length str) 1) (string-equal (substring str -2 -1) "\\")))
        (concat str "\"")
      str))
   ;; Add more conditions for other pair characters if needed
   ;; Default case - return string unchanged
   (t str)))

;; Function to start recording when entering insert mode
(defun my-start-recording ()
  "Start recording keystrokes when entering insert mode."
  (setq my-is-recording t
        my-insert-recording nil
        my-recording-start-point (point))
  (message "Recording insert mode keystrokes..."))

;; Function to stop recording when exiting insert mode
(defun my-stop-recording ()
  "Stop recording keystrokes when exiting insert mode."
  (when my-is-recording
    ;; Capture what was actually inserted during this insert session
    (setq my-insert-recording 
          (buffer-substring-no-properties my-recording-start-point (point)))
    (setq my-is-recording nil)
    ;; Store as the last operation (insert)
    (setq my-last-operation (list 'insert my-insert-recording))
    (message "Recording stopped. Use my-replay-last-operation to replay.")))

;; Integrated replay function
(defun my-replay-last-operation ()
  "Replay the last operation (either command combination or insert)."
  (interactive)
  (if (not my-last-operation)
      (message "No operation to replay")
    (let ((op-type (car my-last-operation))
          (op-data (cadr my-last-operation)))
      (cond
       ;; Replay command combination
       ((eq op-type 'commands)
        (let* ((selection (nth 0 op-data))
               (expand-count (nth 1 op-data))
               (action (nth 2 op-data)))
          (apply (car selection) (cdr selection))
          (when expand-count
            (meow-expand expand-count))
          (apply (car action) (cdr action))))
       
       ;; Replay insert operation
       ((eq op-type 'insert)
        (let ((my-pair-mode-was-on (and (boundp 'my-pair-mode) my-pair-mode)))
          (when my-pair-mode-was-on
            (my-pair-mode -1))  ; Temporarily disable the pair mode
          
          ;; Insert the text with smart pair handling
          (insert (my-handle-pairs op-data))
          
          (when my-pair-mode-was-on
            (my-pair-mode 1))))  ; Restore it if it was enabled
       
       (t (message "Unknown operation type: %s" op-type))))))

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

;; Add our recording functions to Meow hooks
(add-hook 'meow-insert-enter-hook #'my-start-recording)
(add-hook 'meow-insert-exit-hook #'my-stop-recording)

;; ;; Bind the replay function to a key of your choice
;; (global-set-key (kbd "C-x r r") #'my-replay-last-operation)


;; ;; Variables to store our recorded keystrokes
;; (defvar my/insert-recording nil
;;   "Stores the current recording of insert mode keystrokes.")
;; (defvar my/is-recording nil
;;   "Whether we are currently recording keystrokes.")
;; (defvar my/recording-start-point nil
;;   "Point position when recording started.")

;; ;; Function to start recording when entering insert mode
;; (defun my/start-recording ()
;;   "Start recording keystrokes when entering insert mode."
;;   (setq my/is-recording t
;;         my/insert-recording nil
;;         my/recording-start-point (point))
;;   (message "Recording insert mode keystrokes..."))

;; ;; Function to stop recording when exiting insert mode
;; (defun my/stop-recording ()
;;   "Stop recording keystrokes when exiting insert mode."
;;   (when my/is-recording
;;     ;; Capture what was actually inserted during this insert session
;;     (setq my/insert-recording 
;;           (buffer-substring-no-properties my/recording-start-point (point)))
;;     (setq my/is-recording nil)
;;     (message "Recording stopped. Use my/repeat-insert to replay.")))

;; ;; Function to handle pair characters
;; (defun my/handle-pairs (str)
;;   "Check if string starts with a pair character and handle accordingly.
;; Handles double quotes, single quotes, parentheses, square brackets,
;; curly braces, and angle brackets."
;;   (cond
;;    ;; Handle double quotes
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "\""))
;;     (if (or (not (string-equal (substring str -1) "\""))
;;             (and (> (length str) 1) (string-equal (substring str -2 -1) "\\")))
;;         (concat str "\"")
;;       str))
;;    ;; Handle single quotes
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "'"))
;;     (if (not (string-equal (substring str -1) "'"))
;;         (concat str "'")
;;       str))
;;    ;; Handle parentheses
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "("))
;;     (if (not (string-equal (substring str -1) ")"))
;;         (concat str ")")
;;       str))
;;    ;; Handle square brackets
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "["))
;;     (if (not (string-equal (substring str -1) "]"))
;;         (concat str "]")
;;       str))
;;    ;; Handle curly braces
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "{"))
;;     (if (not (string-equal (substring str -1) "}"))
;;         (concat str "}")
;;       str))
;;    ;; Handle angle brackets
;;    ((and (> (length str) 0)
;;          (string-equal (substring str 0 1) "<"))
;;     (if (not (string-equal (substring str -1) ">"))
;;         (concat str ">")
;;       str))
;;    ;; Default case - return string unchanged
;;    (t str)))

;; ;; Function to replay recorded keystrokes
;; (defun my/repeat-insert ()
;;   "Repeat the last recorded insert mode text with smart pair handling."
;;   (interactive)
;;   (if (not my/insert-recording)
;;       (message "No recorded text to replay")
;;     (let ((my-pair-mode-was-on (and (boundp 'my-pair-mode) my-pair-mode)))
;;       (when my-pair-mode-was-on
;;         (my-pair-mode -1))  ; Temporarily disable the pair mode
      
;;       ;; Insert the text with smart pair handling
;;       (insert (my/handle-pairs my/insert-recording))
      
;;       (when my-pair-mode-was-on
;;         (my-pair-mode 1)))))  ; Restore it if it was enabled

;; ;; Add our functions to Meow hooks
;; (add-hook 'meow-insert-enter-hook #'my/start-recording)
;; (add-hook 'meow-insert-exit-hook #'my/stop-recording)

;; ;; Bind the repeat function to a key of your choice
;; (global-set-key (kbd "C-x r r") #'my/repeat-insert)
