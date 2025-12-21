;; -*- lexical-binding: t; -*-

(defvar my-insert-repeat--start-marker nil)
(defvar my-insert-repeat--end-marker nil)
(defvar my-insert-repeat--text nil)

;;;; ------------------------------------------------------------
;;;; Recording
;;;; ------------------------------------------------------------

(defun my-insert-repeat-start ()
  "Start recording. Use markers to track the true bounds of insertion."
  ;; Initialize markers if they don't exist
  (unless my-insert-repeat--start-marker
    (setq my-insert-repeat--start-marker (make-marker))
    (setq my-insert-repeat--end-marker (make-marker)))

  ;; Set start position. Normal insertion type (nil) means it stays put.
  (set-marker-insertion-type my-insert-repeat--start-marker nil)
  (set-marker my-insert-repeat--start-marker (point))

  ;; Set end position.
  ;; CRITICAL: insertion-type 't' means text inserted at this marker
  ;; pushes the marker forward. This captures auto-closed pairs.
  (set-marker-insertion-type my-insert-repeat--end-marker t)
  (set-marker my-insert-repeat--end-marker (point)))

(defun my-insert-repeat-stop ()
  "Stop recording. Capture text between start and the floating end marker."
  (when (and my-insert-repeat--start-marker
             my-insert-repeat--end-marker)
    (let ((text (buffer-substring-no-properties
                 my-insert-repeat--start-marker
                 my-insert-repeat--end-marker)))

      (unless (string-empty-p text)
        (setq my-insert-repeat--text text)))

    ;; Clean up markers (good practice)
    (set-marker my-insert-repeat--start-marker nil)
    (set-marker my-insert-repeat--end-marker nil)))

;;;; ------------------------------------------------------------
;;;; Replay
;;;; ------------------------------------------------------------

(defun my-insert-repeat ()
  "Repeat last insert-mode change."
  (interactive)
  (if (not my-insert-repeat--text)
      (message "Nothing to repeat")
    (my-insert-repeat--insert-verbatim my-insert-repeat--text)))

(defun my-insert-repeat--insert-verbatim (text)
  "Insert TEXT exactly as recorded, disabling electric modes temporarily."
  (let ((electric-pair-was-on electric-pair-mode))
    ;; Disable electric-pair to prevent double-closing
    (when electric-pair-was-on
      (electric-pair-mode -1))

    (undo-boundary)
    (insert text)
    (undo-boundary)

    (when electric-pair-was-on
      (electric-pair-mode 1))))

;;;; ------------------------------------------------------------
;;;; Hooks
;;;; ------------------------------------------------------------

(add-hook 'meow-insert-enter-hook #'my-insert-repeat-start)
(add-hook 'meow-insert-exit-hook  #'my-insert-repeat-stop)
