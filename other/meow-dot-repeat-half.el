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
;;;; LAST OPERATION
;;;; ============================================================

(defvar my-last-operation nil)

;;;; ============================================================
;;;; ORIGINAL COMMAND COMBO LOGIC (UNCHANGED)
;;;; ============================================================

(defvar my-last-combination nil)
(defvar my-command-history nil)
(defvar my-meow-expand-count nil)
(defvar my-last-action 'command)

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
    delete-surrounding-symbol))

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
       (t 'unknown-command))))
   (t 'unknown-command)))

(defun my-valid-combination-p (history)
  (when (>= (length history) 2)
    (let ((a (my-command-name (caar history)))
          (b (my-command-name (caar (last history)))))
      (and (member a my-selection-commands)
           (member b my-action-commands)))))

(defun my-store-command (cmd args)
  (let ((entry (cons cmd args)))
    (setq my-command-history
          (if (< (length my-command-history) 2)
              (append my-command-history (list entry))
            (append (cdr my-command-history) (list entry))))

    (when (my-valid-combination-p my-command-history)
      (setq my-last-combination
            (list (car my-command-history)
                  my-meow-expand-count
                  (cadr my-command-history)))
      (setq my-last-operation
            (list 'commands my-last-combination))
      (my-dot-log "COMBO recorded => %S" my-last-operation))

    (when (member (my-command-name cmd) my-selection-commands)
      (setq my-meow-expand-count nil))

    ;; COMMAND ALWAYS INVALIDATES INSERT
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

(defun my-replay-command-combo (combo)
  (let ((sel (nth 0 combo))
        (cnt (nth 1 combo))
        (act (nth 2 combo)))
    (apply (car sel) (cdr sel))
    (when cnt (meow-expand cnt))
    (apply (car act) (cdr act))))

;;;; ============================================================
;;;; INSERT RECORDING (MINIMAL, SAFE)
;;;; ============================================================

(defvar my-insert-start-marker nil)
(defvar my-insert-end-marker nil)
(defvar my-insert-recording-valid nil)

(defun my-insert-repeat-start ()
  (my-dot-log "INSERT start")
  (setq my-insert-recording-valid t)
  (setq my-last-action 'insert)

  (setq my-insert-start-marker (copy-marker (point) nil))
  (setq my-insert-end-marker   (copy-marker (point) t)))

(defun my-insert-repeat-stop ()
  (my-dot-log "INSERT stop (valid=%S)" my-insert-recording-valid)

  (when my-insert-recording-valid
    (let ((text
           (buffer-substring-no-properties
            my-insert-start-marker
            my-insert-end-marker)))
      (unless (string-empty-p text)
        (setq my-last-operation (list 'insert text))
        (my-dot-log "INSERT recorded => %S" my-last-operation))))

  (set-marker my-insert-start-marker nil)
  (set-marker my-insert-end-marker nil))

(defun my-replay-insert (text)
  (let ((pair electric-pair-mode))
    (when pair (electric-pair-mode -1))
    (insert text)
    (when pair (electric-pair-mode 1))))

(add-hook 'meow-insert-enter-hook #'my-insert-repeat-start)
(add-hook 'meow-insert-exit-hook  #'my-insert-repeat-stop)

;;;; ============================================================
;;;; DOT REPEAT
;;;; ============================================================

(defun my-repeat-last-operation ()
  (interactive)
  (my-dot-log "DOT => %S" my-last-operation)
  (pcase my-last-operation
    (`(commands ,combo)
     (my-replay-command-combo combo))
    (`(insert ,text)
     (my-replay-insert text))
    (_
     (message "Nothing to repeat"))))

;;;; ============================================================
;;;; KEY
;;;; ============================================================

;; (define-key meow-normal-state-keymap "." #'my-repeat-last-operation)
