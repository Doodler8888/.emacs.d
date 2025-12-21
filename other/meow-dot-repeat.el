;; -*- lexical-binding: t -*-

;;;; ------------------------------------------------------------
;;;; 1. GLOBAL STATE & VARIABLES
;;;; ------------------------------------------------------------

;; Determines what to replay: 'command, 'insert, or 'change
(defvar my-last-op-type nil
  "Tracks the type of the last operation: 'command, 'insert, or 'change.")

;; --- Command State ---
(defvar my-last-combination nil
  "Stores the last valid combination: (selection expand-count action).")

(defvar my-command-history nil
  "List to store the last two commands for validation.")

(defvar my-meow-expand-count nil
  "Stores the number of times to expand the selection.")

;; --- Insert State ---
(defvar my-insert-repeat--start-marker nil)
(defvar my-insert-repeat--end-marker nil)
(defvar my-insert-repeat--text nil)

;; --- Change State (The new logic) ---
(defvar my-pending-change-flag nil
  "Flag set to t when a change command is executed, pending text insertion.")
(defvar my-last-change-text nil
  "Stores the text typed during the last change operation.")

;; --- Definitions ---
(defvar my-selection-commands
  '(meow-inner-of-thing meow-bounds-of-thing meow-mark-word
    meow-next-word meow-next-symbol meow-find meow-till
    meow-back-word meow-beginning-of-thing meow-end-of-thing)
  "Commands that create selections.")

;; ADDED: my/meow-smart-change and my/meow-change
(defvar my-action-commands
  '(my/meow-smart-delete my/generic-meow-smart-delete
    surround-region-with-symbol change-surrounding-symbol
    delete-surrounding-symbol
    my/meow-smart-change my/meow-change)
  "Commands that operate on a selection.")

;;;; ------------------------------------------------------------
;;;; 2. HELPER FUNCTIONS
;;;; ------------------------------------------------------------

(defun my-command-name (cmd)
  "Get the normalized symbol name from a command object."
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
       ;; ADDED: Identifiers for change commands
       ((string-match-p "my/meow-smart-change" cmd-string)
        'my/meow-smart-change)
       ((string-match-p "my/meow-change" cmd-string)
        'my/meow-change)
       (t 'unknown-command))))
   (t 'unknown-command)))

(defun my-valid-combination-p (history)
  "Check if the command history forms a valid combination."
  (when (>= (length history) 2)
    (let* ((first-cmd (my-command-name (caar history)))
           (last-cmd (my-command-name (caar (last history)))))
      (and (member first-cmd my-selection-commands)
           (member last-cmd my-action-commands)))))

;;;; ------------------------------------------------------------
;;;; 3. COMMAND RECORDING LOGIC
;;;; ------------------------------------------------------------

(defun my-store-command (cmd args)
  "Store a command and update state."
  (when (eq (my-command-name cmd) 'delete-surrounding-symbol)
    (setq args nil))

  (let ((command-entry (cons cmd args))
        (cmd-name (my-command-name cmd)))

    ;; Update history buffer
    (setq my-command-history
          (if (< (length my-command-history) 2)
              (append my-command-history (list command-entry))
            (append (cdr my-command-history) (list command-entry))))

    ;; Check for valid combination
    (when (my-valid-combination-p my-command-history)
      (setq my-last-combination
            (list (car my-command-history)
                  my-meow-expand-count
                  (cadr my-command-history)))

      ;; LOGIC BRANCHING:
      (if (or (eq cmd-name 'my/meow-smart-change)
              (eq cmd-name 'my/meow-change))
          ;; If it's a change command, prepare for incoming text
          (setq my-pending-change-flag t)
        ;; Otherwise, it's a standard command (like delete)
        (setq my-pending-change-flag nil)
        (setq my-last-op-type 'command)))

    (when (member cmd-name my-selection-commands)
      (setq my-meow-expand-count nil))))

(defun my-track-command (orig-fun &rest args)
  (let ((cmd (my-command-name orig-fun)))
    (if (eq cmd 'delete-surrounding-symbol)
        (progn (my-store-command orig-fun nil) (apply orig-fun args))
      (progn (my-store-command orig-fun args) (apply orig-fun args)))))

(defun my-track-meow-expand (digit) (setq my-meow-expand-count digit))
(defun my-reset-expand-count (&rest _) (setq my-meow-expand-count nil))

;;;; ------------------------------------------------------------
;;;; 4. INSERT RECORDING LOGIC (Marker Based)
;;;; ------------------------------------------------------------

(defun my-insert-repeat-start ()
  "Start recording insert."
  (unless my-insert-repeat--start-marker
    (setq my-insert-repeat--start-marker (make-marker))
    (setq my-insert-repeat--end-marker (make-marker)))

  (set-marker-insertion-type my-insert-repeat--start-marker nil)
  (set-marker my-insert-repeat--start-marker (point))
  (set-marker-insertion-type my-insert-repeat--end-marker t)
  (set-marker my-insert-repeat--end-marker (point)))

(defun my-insert-repeat-stop ()
  "Stop recording. Distinguish between 'Insert' and 'Change'."
  (when (and my-insert-repeat--start-marker
             my-insert-repeat--end-marker)
    (let ((text (buffer-substring-no-properties
                 my-insert-repeat--start-marker
                 my-insert-repeat--end-marker)))

      ;; If we are in a pending change operation
      (if my-pending-change-flag
          (progn
            (setq my-last-change-text text) ;; Allow empty text (e.g. change to nothing)
            (setq my-last-op-type 'change)
            (setq my-pending-change-flag nil))

        ;; If we are in a normal insert operation
        (when (not (string-empty-p text))
          (setq my-insert-repeat--text text)
          (setq my-last-op-type 'insert))))

    (set-marker my-insert-repeat--start-marker nil)
    (set-marker my-insert-repeat--end-marker nil)))

(defun my-insert-repeat--insert-verbatim (text)
  (let ((electric-pair-was-on (bound-and-true-p electric-pair-mode)))
    (when electric-pair-was-on (electric-pair-mode -1))
    (undo-boundary)
    (insert text)
    (undo-boundary)
    (when electric-pair-was-on (electric-pair-mode 1))))

;;;; ------------------------------------------------------------
;;;; 5. UNIFIED REPLAY FUNCTION
;;;; ------------------------------------------------------------

(defun my/replay-last-operation ()
  "Replay the last operation (Insert, Command, or Change)."
  (interactive)
  (cond
   ;; CASE 1: Replay Text Insert
   ((eq my-last-op-type 'insert)
    (if my-insert-repeat--text
        (my-insert-repeat--insert-verbatim my-insert-repeat--text)
      (message "No insert text to replay")))

   ;; CASE 2: Replay Command (e.g. Delete)
   ((eq my-last-op-type 'command)
    (if my-last-combination
        (my/execute-combo my-last-combination)
      (message "No command combination to replay")))

   ;; CASE 3: Replay Change (Select + Delete + Insert Text)
   ((eq my-last-op-type 'change)
    (if my-last-combination
        (progn
          ;; 1. Execute the command (e.g. Select Word -> Change)
          ;; This will leave us in Insert Mode
          (my/execute-combo my-last-combination)

          ;; 2. Insert the recorded text
          (when my-last-change-text
            (my-insert-repeat--insert-verbatim my-last-change-text))

          ;; 3. Exit Insert Mode (return to Normal)
          (meow-insert-exit))
      (message "No change combination to replay")))

   (t (message "Nothing recorded yet"))))

(defun my/execute-combo (combo)
  "Helper to execute a selection+action combination."
  (let ((selection (nth 0 combo))
        (expand-count (nth 1 combo))
        (action (nth 2 combo)))
    (apply (car selection) (cdr selection))
    (when expand-count
      (meow-expand expand-count))
    (apply (car action) (cdr action))))

;;;; ------------------------------------------------------------
;;;; 6. HOOKS AND ADVICE SETUP
;;;; ------------------------------------------------------------

(add-hook 'meow-insert-enter-hook #'my-insert-repeat-start)
(add-hook 'meow-insert-exit-hook  #'my-insert-repeat-stop)

(dolist (cmd my-selection-commands)
  (advice-add cmd :around #'my-track-command)
  (advice-add cmd :before #'my-reset-expand-count))

(dolist (cmd my-action-commands)
  (advice-add cmd :around #'my-track-command))

(advice-add 'my-meow-digit :after (lambda (&rest args)
                                    (my-track-meow-expand (car args))))

;; Bind to dot
(define-key meow-normal-state-keymap "." #'my/replay-last-operation)
