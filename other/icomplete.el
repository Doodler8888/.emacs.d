(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
			  ("<tab>" . icomplete-force-complete)
			  ("SPC" . self-insert-command)
              ("RET" . icomplete-force-complete-and-exit))
  :hook
  (after-init . (lambda ()
                  (fido-mode -1)
                  (icomplete-vertical-mode 1)))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions)

  (defcustom icomplete-vertical-selected-prefix-marker "» "
    "Prefix string used to mark the selected completion candidate.
If `icomplete-vertical-render-prefix-marker' is t, the string
setted here is used as a prefix of the currently selected entry in the
list.  It can be further customized by the face
`icomplete-vertical-selected-prefix-face'."
    :type 'string
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-unselected-prefix-marker "  "
    "Prefix string used on the unselected completion candidates.
If `icomplete-vertical-render-prefix-marker' is t, the string
setted here is used as a prefix for all unselected entries in the list.
list.  It can be further customized by the face
`icomplete-vertical-unselected-prefix-face'."
    :type 'string
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-in-buffer-adjust-list t
    "Control whether in-buffer completion should align the cursor position.
If this is t and `icomplete-in-buffer' is t, and `icomplete-vertical-mode'
is activated, the in-buffer vertical completions are shown aligned to the
cursor position when the completion started, not on the first column, as
the default behaviour."
    :type 'boolean
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-render-prefix-marker t
    "Control whether a marker is added as a prefix to each candidate.
If this is t and `icomplete-vertical-mode' is activated, a marker,
controlled by `icomplete-vertical-selected-prefix-marker' is shown
as a prefix to the current under selection candidate, while the
remaining of the candidates will receive the marker controlled
by `icomplete-vertical-unselected-prefix-marker'."
    :type 'boolean
    :group 'icomplete
    :version "31")

  (defface icomplete-vertical-selected-prefix-face
    '((t :inherit font-lock-keyword-face :weight bold :foreground "#c4a7e7"))
    "Face used for the prefix set by `icomplete-vertical-selected-prefix-marker'."
    :group 'icomplete
    :version "31")

  (defface icomplete-vertical-unselected-prefix-face
    '((t :inherit font-lock-keyword-face :weight normal :foreground "gray"))
    "Face used for the prefix set by `icomplete-vertical-unselected-prefix-marker'."
    :group 'icomplete
    :version "31")

  (defun icomplete-vertical--adjust-lines-for-column (lines buffer data)
    "Adjust the LINES to align with the column in BUFFER based on DATA."
    (if icomplete-vertical-in-buffer-adjust-list
        (let ((column
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char (car data))
                   (current-column)))))
          (dolist (l lines)
            (add-text-properties
             0 1 `(display ,(concat (make-string column ?\s) (substring l 0 1)))
             l))
          lines)
      lines))

  (defun icomplete-vertical--add-marker-to-selected (comp)
    "Add markers to the selected/unselected COMP completions."
    (if (and icomplete-vertical-render-prefix-marker
             (get-text-property 0 'icomplete-selected comp))
        (concat (propertize icomplete-vertical-selected-prefix-marker
                            'face 'icomplete-vertical-selected-prefix-face)
                comp)
      (concat (propertize icomplete-vertical-unselected-prefix-marker
                          'face 'icomplete-vertical-unselected-prefix-face)
              comp)))

  (cl-defun icomplete--render-vertical
      (comps md &aux scroll-above scroll-below
             (total-space ; number of mini-window lines available
              (1- (min
                   icomplete-prospects-height
                   (truncate (max-mini-window-lines) 1)))))
    ;; Welcome to loopapalooza!
    ;;
    ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
    ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
    ;; are:
    ;;
    ;; - both nil, there is no manual scroll;
    ;; - both non-nil, there is a healthy manual scroll that doesn't need
    ;;   to be readjusted (user just moved around the minibuffer, for
    ;;   example);
    ;; - non-nil and nil, respectively, a refiltering took place and we
    ;;   may need to readjust them to the new filtered `comps'.
    (when (and icomplete-scroll
               icomplete--scrolled-completions
               (null icomplete--scrolled-past))
      (cl-loop with preds
               for (comp . rest) on comps
               when (equal comp (car icomplete--scrolled-completions))
               do
               (setq icomplete--scrolled-past preds
                     comps (cons comp rest))
               (completion--cache-all-sorted-completions
                (icomplete--field-beg)
                (icomplete--field-end)
                comps)
               and return nil
               do (push comp preds)
               finally (setq icomplete--scrolled-completions nil)))
    ;; Then, in this pretty ugly loop, collect completions to display
    ;; above and below the selected one, considering scrolling
    ;; positions.
    (cl-loop with preds = icomplete--scrolled-past
             with succs = (cdr comps)
             with space-above = (- total-space
                                   1
                                   (cl-loop for (_ . r) on comps
                                            repeat (truncate total-space 2)
                                            while (listp r)
                                            count 1))
             repeat total-space
             for neighbor = nil
             if (and preds (> space-above 0)) do
             (push (setq neighbor (pop preds)) scroll-above)
             (cl-decf space-above)
             else if (consp succs) collect
             (setq neighbor (pop succs)) into scroll-below-aux
             while neighbor
             finally (setq scroll-below scroll-below-aux))
    ;; Halfway there...
    (let* ((selected (propertize (car comps) 'icomplete-selected t))
           (chosen (append scroll-above (list selected) scroll-below))
           (tuples (icomplete--augment md chosen))
           max-prefix-len max-comp-len lines nsections)
      (add-face-text-property 0 (length selected)
                              'icomplete-selected-match 'append selected)
      ;; Figure out parameters for horizontal spacing
      (cl-loop
       for (comp prefix) in tuples
       maximizing (length prefix) into max-prefix-len-aux
       maximizing (length comp) into max-comp-len-aux
       finally (setq max-prefix-len max-prefix-len-aux
                     max-comp-len max-comp-len-aux))
      ;; Serialize completions and section titles into a list
      ;; of lines to render
      (cl-loop
       for (comp prefix suffix section) in tuples
       when section
       collect (propertize section 'face 'icomplete-section) into lines-aux
       and count 1 into nsections-aux
       for comp = (icomplete-vertical--add-marker-to-selected comp)
       when (get-text-property 0 'icomplete-selected comp)
       do (add-face-text-property 0 (length comp)
                                  'icomplete-selected-match 'append comp)
       collect (concat prefix
                       (make-string (max 0 (- max-prefix-len (length prefix))) ? )
                       (completion-lazy-hilit comp)
                       (make-string (max 0 (- max-comp-len (length comp))) ? )
                       suffix)
       into lines-aux
       finally (setq lines lines-aux
                     nsections nsections-aux))
      ;; Kick out some lines from the beginning due to extra sections.
      ;; This hopes to keep the selected entry more or less in the
      ;; middle of the dropdown-like widget when `icomplete-scroll' is
      ;; t.  Funky, but at least I didn't use `cl-loop'
      (setq lines
            (nthcdr
             (cond ((<= (length lines) total-space) 0)
                   ((> (length scroll-above) (length scroll-below)) nsections)
                   (t (min (ceiling nsections 2) (length scroll-above))))
             lines))
      (when icomplete--in-region-buffer
        (setq lines (icomplete-vertical--adjust-lines-for-column
                     lines icomplete--in-region-buffer completion-in-region--data)))
      ;; At long last, render final string return value.  This may still
      ;; kick out lines at the end.
      (concat " \n"
              (cl-loop for l in lines repeat total-space concat l concat "\n")))))


;; (with-eval-after-load 'icomplete
;; ;; ...
;;   (define-key icomplete-minibuffer-map (kbd "C-n") 'my-icomplete-forward)
;;   (define-key icomplete-minibuffer-map (kbd "C-p") 'my-icomplete-backward))
