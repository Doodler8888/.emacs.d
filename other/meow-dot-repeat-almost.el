;; -*- lexical-binding: t; -*-

;;;; ============================================================
;;;; DEBUG
;;;; ============================================================

(defvar my-dot-debug t)

(defun my-dot-log (fmt &rest args)
  (when my-dot-debug
    (with-current-buffer (get-buffer-create "*my-dot-repeat-debug*")
      (goto-char (point-max))
      (insert (apply #'format
                     (concat (format-time-string "[%H:%M:%S] ")
                             fmt "\n")
                     args)))))

;;;; ============================================================
;;;; STATE VARIABLES
;;;; ============================================================

(defvar my-last-operation nil
  "Stores the final operation to be replayed.
Structure options:
1. (commands (selection count action))
2. (insert text)
3. (change (selection count action) text)")

(defvar my-pending-change-combo nil
  "Temporarily holds a combo if it was a change operation.
This tells the insert-stop hook to merge this combo with the text.")

(defvar my-last-combination nil)
(defvar my-command-history nil)
(defvar my-meow-expand-count nil)
(defvar my-last-action 'command)

;;;; ============================================================
;;;; COMMAND DEFINITIONS
;;;; ============================================================

(defvar my-selection-commands
  '(meow-inner-of-thing
    meow-bounds-of-thing
    meow-mark-word
    meow-next-word
    meow-next-symbol
    meow-find
    meow-till
    meow-back-word
    meow-beginning-of-thing
    meow-end-of-thing))

(defvar my-action-commands
  '(my/meow-smart-delete
    my/generic-meow-smart-delete
    surround-region-with-symbol
    change-surrounding-symbol
    delete-surrounding-symbol
    ;; Change Commands added here:
    my/meow-smart-change
    my/meow-change))

(defun my-command-name (cmd)
  (cond
   ((symbolp cmd) cmd)
   ((subrp cmd)
    (intern
     (replace-regexp-in-string
      "#<subr \\(.+?\\)>" "\\1"
      (prin1-to-string cmd))))
   ((functionp cmd)
    (let ((s (prin1-to-string cmd)))
      (cond
       ((string-match-p "my/generic-meow-smart-delete" s)
        'my/meow-smart-delete)
       ((and (string-match-p "meow-kill" s)
             (string-match-p "meow-delete" s))
        'my/meow-smart-delete)
       ((string-match-p "surround-region-with-symbol" s)
        'surround-region-with-symbol)
       ((string-match-p "change-surrounding-symbol" s)
        'change-surrounding-symbol)
       ((string-match-p "Delete the symbols surrounding" s)
        'delete-surrounding-symbol)
       ;; Recognize Change Commands
       ((string-match-p "my/meow-smart-change" s)
        'my/meow-smart-change)
       ((string-match-p "my/meow-change" s)
        'my/meow-change)
       (t 'unknown-command))))
   (t 'unknown-command)))

;;;; ============================================================
;;;; COMMAND TRACKING LOGIC
;;;; ============================================================

(defun my-valid-combination-p (history)
  (when (>= (length history) 2)
    (let ((a (my-command-name (caar history)))
          (b (my-command-name (caar (last history)))))
      (and (member a my-selection-commands)
           (member b my-action-commands)))))

(defun my-store-command (cmd args)
  (let ((entry (cons cmd args))
        (cmd-name (my-command-name cmd)))
    (setq my-command-history
          (if (< (length my-command-history) 2)
              (append my-command-history (list entry))
            (append (cdr my-command-history) (list entry))))

    (when (my-valid-combination-p my-command-history)
      (setq my-last-combination
            (list (car my-command-history)
                  my-meow-expand-count
                  (cadr my-command-history)))

      ;; CHECK FOR CHANGE COMMAND
      (if (or (eq cmd-name 'my/meow-smart-change)
              (eq cmd-name 'my/meow-change))
          (progn
            ;; It's a change. Store internally, don't set last-op yet.
            (setq my-pending-change-combo my-last-combination)
            (my-dot-log "PENDING CHANGE set => %S" my-last-combination))

        ;; It's a normal action.
        (setq my-pending-change-combo nil)
        (setq my-last-operation
              (list 'commands my-last-combination))
        (my-dot-log "COMBO recorded => %S" my-last-operation)))

    (when (member cmd-name my-selection-commands)
      (setq my-meow-expand-count nil))

    (setq my-last-action 'command)
    (setq my-insert-recording-valid nil)))

(defun my-track-command (orig &rest args)
  (my-dot-log "CMD seen => %S" orig)
  (my-store-command orig args)
  (apply orig args))

(defun my-track-meow-expand (digit)
  (setq my-meow-expand-count digit))

(dolist (cmd my-selection-commands)
  (advice-add cmd :around #'my-track-command))

(dolist (cmd my-action-commands)
  (advice-add cmd :around #'my-track-command))

(advice-add 'my-meow-digit
            :after (lambda (&rest args)
                     (my-track-meow-expand (car args))))

;;;; ============================================================
;;;; INSERT RECORDING
;;;; ============================================================

(defvar my-insert-start-marker nil)
(defvar my-insert-end-marker nil)
(defvar my-insert-recording-valid nil)

(defun my-insert-repeat-start ()
  (my-dot-log "INSERT start")
  (setq my-insert-recording-valid t)
  (setq my-last-action 'insert)

  (setq my-insert-start-marker (copy-marker (point) nil))
  (set-marker-insertion-type my-insert-start-marker nil)

  (setq my-insert-end-marker   (copy-marker (point) t))
  (set-marker-insertion-type my-insert-end-marker t))

(defun my-insert-repeat-stop ()
  (my-dot-log "INSERT stop (valid=%S)" my-insert-recording-valid)

  (when my-insert-recording-valid
    (let ((text
           (buffer-substring-no-properties
            my-insert-start-marker
            my-insert-end-marker)))

      (unless (string-empty-p text)
        (if my-pending-change-combo
            ;; CASE: MERGE CHANGE + INSERT
            (progn
              (setq my-last-operation (list 'change my-pending-change-combo text))
              (setq my-pending-change-combo nil)
              (my-dot-log "CHANGE COMBO recorded => %S" my-last-operation))

          ;; CASE: NORMAL INSERT
          (setq my-last-operation (list 'insert text))
          (my-dot-log "INSERT recorded => %S" my-last-operation)))))

  (set-marker my-insert-start-marker nil)
  (set-marker my-insert-end-marker nil))

(add-hook 'meow-insert-enter-hook #'my-insert-repeat-start)
(add-hook 'meow-insert-exit-hook  #'my-insert-repeat-stop)

;;;; ============================================================
;;;; REPLAY LOGIC
;;;; ============================================================

(defun my-replay-command-combo (combo)
  (let ((sel (nth 0 combo))
        (cnt (nth 1 combo))
        (act (nth 2 combo)))
    (apply (car sel) (cdr sel))
    (when cnt (meow-expand cnt))
    (apply (car act) (cdr act))))

(defun my-replay-insert (text)
  (let ((pair (bound-and-true-p electric-pair-mode)))
    (when pair (electric-pair-mode -1))
    (insert text)
    (when pair (electric-pair-mode 1))))

(defun my-repeat-last-operation ()
  (interactive)
  (my-dot-log "DOT => %S" my-last-operation)
  (pcase my-last-operation
    ;; 1. Replay Selection + Action
    (`(commands ,combo)
     (my-replay-command-combo combo))

    ;; 2. Replay Insert
    (`(insert ,text)
     (my-replay-insert text))

    ;; 3. Replay Selection + Change + Insert
    (`(change ,combo ,text)
     (my-replay-command-combo combo) ;; Executes Change -> Enters Insert
     (my-replay-insert text)         ;; Inserts text
     (meow-insert-exit))             ;; Exits Insert

    (_
     (message "Nothing to repeat"))))

;;;; ============================================================
;;;; KEY BINDING
;;;; ============================================================


;; User
;; [18:49:24] DOT START. Operation=(change ((#<subr meow-next-word> 1) 2 (#[nil ((if (region-active-p) (my/meow-change) (my/handle-operator 'my/change-operator #'my/change-special-handler))) (t) nil "Enhanced change command with Vim-like behavior (Normal Mode logic)." nil])) ""test"")
;; [18:49:24] REPLAY COMBO: ((#<subr meow-next-word> 1) 2 (#[nil ((if (region-active-p) (my/meow-change) (my/handle-operator 'my/change-operator #'my/change-special-handler))) (t) nil "Enhanced change command with Vim-like behavior (Normal Mode logic)." nil]))
;; [18:49:24] STORE: Processing #[nil ((if (region-active-p) (progn (delete-region (region-beginning) (region-end)))) (meow-insert)) (t) nil "Delete the active region and enter meow-insert mode." nil] args=nil
;; [18:49:24] STORE: Invalidated insert recording.
;; [18:49:24] INSERT start. Previous valid=nil
;; [18:49:24] REPLAY INSERT: '"test"'
;; [18:49:24] INSERT stop. Valid=t PendingChange="NO"
;; [18:49:24] INSERT captured text: '"test"'
;; [18:49:24] INSERT recorded => (insert ""test"")
;; [18:49:26] --------------------------------------------------
;; [18:49:26] DOT START. Operation=(insert ""test"")
;; [18:49:26] REPLAY INSERT: '"test"'
;; [18:49:41] INSERT start. Previous valid=t
