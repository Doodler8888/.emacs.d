(defun my/zoxide-add ()
  "Add directory to zoxide database using interactive selection."
  (interactive)
  (let ((dir (expand-file-name 
              (read-directory-name "Add directory to zoxide: "))))
    (start-process "zoxide-add" nil "zoxide" "add" dir)
    (message "Added to zoxide: %s" dir)))

(defun my/zoxide-switch ()
  "Switch to a directory from zoxide database using completion."
  (interactive)
  (let* ((dirs (split-string (shell-command-to-string "zoxide query -l") "\n" t))
         (selected-dir (completing-read "Zoxide directory: " dirs nil t)))
    (when selected-dir
      (dired selected-dir))))


(defun my/get-block-devices ()
  "Get list of block devices using lsblk."
  (let ((output (shell-command-to-string "lsblk --nodeps --output NAME,SIZE,TYPE,MOUNTPOINT")))
    (split-string output "\n" t "[ \t\n\r]+")))

(defun my/dd-write ()
  "Interactive wrapper for dd command.
Lists available devices using lsblk, prompts for input file and output device,
shows the command before execution, and runs it asynchronously with progress status."
  (interactive)
  (let* ((default-directory (expand-file-name "~/Downloads/iso/"))
         (input-file (expand-file-name (read-file-name "Input file: " default-directory)))
         (devices (my/get-block-devices))
         ;; Remove the header line
         (devices (cdr devices)))
    (if (null devices)
        (error "No block devices found")
      (let* ((device-choice (completing-read "Select output device: " devices nil t))
             (device-name (car (split-string device-choice)))
             (output-device (concat "/dev/" device-name))
             (dd-command (format "dd status=progress if=%s of=%s" 
                               input-file output-device)))
        
        ;; Show the command and ask for confirmation
        (when (yes-or-no-p (format "Execute command:\n%s\n\nProceed? " dd-command))
          ;; Run the command asynchronously
          (async-shell-command dd-command "*dd output*"))))))


(defun my/kill-current-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))


(defun my/copy-kill-ring-to-clipboard ()
  "Show kill ring entries and copy selected one to system clipboard."
  (interactive)
  (let* ((candidates (cl-remove-duplicates kill-ring :test #'string=))
         (selected (completing-read "Copy to clipboard: " candidates)))
    (kill-new selected)
    (set-clipboard-text selected)
    (message "Copied to clipboard: %s" (truncate-string-to-width selected 60 nil
                                                                 nil "..."))))

(defun my-org-outline ()
  "Jump to an org heading using completion, showing headings in order from top to bottom."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  (let ((candidates '())
        (current-pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let* ((pos (point-at-bol))
               (level (org-outline-level))
               (heading (org-get-heading t t t t))
               (display (format "%5d %s%s" 
                                (line-number-at-pos pos)
                                (make-string (* 2 (1- level)) ?\s)
                                heading)))
          (push (cons display pos) candidates))))
    (setq candidates (nreverse candidates))
    (if candidates
        (let* ((collection
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity))
                    (complete-with-action action candidates string pred))))
               (selection (completing-read "Go to heading: " collection nil t))
               (position (cdr (assoc selection candidates))))
          (when position
            (goto-char position)
            (org-show-context)
            (recenter)))
      (message "No headings found in this buffer."))))

(defun my-package-isolate ()
  "Isolate packages with better completion."
  (interactive)
  (let (packages done)
    (while (not done)
      (condition-case nil
          (let ((package (completing-read 
                         (format "Package %s (C-g when done): "
                                (if packages 
                                    (format "[added: %s]" 
                                            (mapconcat #'identity packages " "))
                                  ""))
                         (mapcar #'car package-alist)
                         nil t)))
            (push package packages))
        (quit (setq done t))))
    (when packages
      (package-isolate 
       (mapcar (lambda (name)
                 (cadr (assoc (intern name) package-alist)))
               (nreverse packages))))))

(defun toggle-special-buffer (buffer-name-pattern show-buffer-fn &optional select-window)
  "Generic function to toggle special buffers.
BUFFER-NAME-PATTERN is a regex to match buffer name.
SHOW-BUFFER-FN is the function to call to show the buffer.
SELECT-WINDOW if non-nil, select the window after showing buffer."
  (let* ((buffer (seq-find (lambda (buf)
                            (string-match-p buffer-name-pattern (buffer-name buf)))
                          (buffer-list)))
         (window (and buffer (get-buffer-window buffer))))
    ;; If we're already in the special buffer, quit it
    (if (string-match-p buffer-name-pattern (buffer-name))
        (quit-window)
      ;; Otherwise toggle the window
      (if window
          (quit-window nil window)
        (when show-buffer-fn
          (funcall show-buffer-fn)
          (when select-window
            (select-window (get-buffer-window 
                          (seq-find (lambda (buf)
                                    (string-match-p buffer-name-pattern (buffer-name buf)))
                                  (buffer-list))))))))))

(defun toggle-flymake-diagnostics ()
  "Toggle the display of Flymake diagnostics buffer."
  (interactive)
  (toggle-special-buffer "\\*Flymake diagnostics.*\\*"
                        (lambda ()
                          (when (bound-and-true-p flymake-mode)
                            (flymake-show-diagnostics-buffer)))
                        t))

(defun toggle-messages-buffer ()
  "Toggle the display of Messages buffer."
  (interactive)
  (toggle-special-buffer "\\*Messages\\*"
                        (lambda () 
                          (display-buffer "*Messages*"))
                        t))

(defun org-insert-row-with-floor ()
  "Insert a new row with a 'floor' above in an Org mode table."
  (interactive)
  (org-table-next-field)
  (beginning-of-line)
  (insert "|-")
  (org-table-align)
  (org-return))

;; (define-key org-mode-map (kbd "C-c f") 'org-insert-row-with-floor)

(defun my-org-beginning-of-block ()
  "Move to the beginning of the current block and then one line down."
  (interactive)
  (let ((element (org-element-at-point)))
    (when (memq (org-element-type element) '(src-block quote-block example-block center-block special-block))
      (goto-char (org-element-property :begin element))
      (forward-line))))  ; Added this line to move one line down

(defun my-org-end-of-block ()
  "Move to the end of the current block and then two lines up."
  (interactive)
  (let ((element (org-element-at-point)))
    (when (memq (org-element-type element) '(src-block quote-block example-block center-block special-block))
      (goto-char (org-element-property :end element))
      (forward-line -3))))  ; Changed -1 to -3 to move two lines up


(defvar my-selected-kill nil
  "Stores the last kill-ring entry selected via `my-browse-kill-ring'.")

(defun my-browse-kill-ring ()
  "Browse kill ring without yanking. Selected text can be yanked later with regular yank command."
  (interactive)
  (let* ((candidates (cl-remove-duplicates kill-ring :test #'equal))
         (selected (completing-read "Select text: " candidates)))
    (when selected
      ;; Remove the selected text from kill-ring if it's there
      (setq kill-ring (delete selected kill-ring))
      ;; Add it to the front of kill-ring
      (kill-new selected)
      (message "Selected text is now at the top of kill-ring. Use C-y to paste."))))

(defun my-vc-switch-branch ()
  "Switch branch using repository root directory."
  (interactive)
  (let* ((dir (or (vc-root-dir)
                  default-directory))
         (backend (vc-responsible-backend dir))
         (branch-name (vc-read-revision 
                      "Switch to branch: " 
                      (list dir)
                      backend)))
    (vc-switch-branch dir branch-name)))

(defun copy-buffer-to-new-buffer ()
  "Create a copy of the current buffer, placing the contents in a new named buffer."
  (interactive)
  (let ((content (buffer-string))  ; Get the content of the current buffer
        (name (generate-new-buffer-name "BufferCopy")))  ; Generate a new buffer name
    (switch-to-buffer name)  ; Create and switch to the new buffer
    (insert content)  ; Insert the original content into the new buffer
    (set-buffer-major-mode (other-buffer))  ; Set the major mode based on the original buffer
    (message "Buffer copied to %s" name)))

(defun print-commands-starting-with (input)
  "Print all Emacs commands starting with INPUT to a scratch buffer."
  (interactive "sInput: ")
  (let ((command-list (apropos-internal input 'commandp))
        (output-buffer (get-buffer-create "*Commands*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Commands starting with '%s':\n\n" input))
      (dolist (command command-list)
        (insert (format "%s\n" command)))
      (goto-char (point-min)))
    (display-buffer output-buffer)))

;; Increment
(defun my/increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my/change-number-at-point '+ (or increment 2)))

;; Decrement
(defun my/decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my/change-number-at-point '- (or increment 1)))

(defun run-ansible-check-and-lint ()
  "Run ansible-lint and ansible-playbook --check on the current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (async-shell-command (format "ansible-lint %s" filename))
          (async-shell-command (format "ansible-playbook --check %s" filename)))
      (message "No file associated with this buffer"))))

(defun my-eshell-snippet-files ()
  (interactive)
  "Return a list of file names (not directories) in '~/.emacs.d/snippets/eshell-mode/eshell' and print it."
  (let* ((snippet-dir (expand-file-name "~/.emacs.d/snippets/eshell-mode/eshell"))
         (snippet-files (directory-files snippet-dir t nil nil)))
    (setq snippet-files (mapcar 'file-name-nondirectory snippet-files))
    (setq snippet-files (remove-if (lambda (file) (member file '("." ".."))) snippet-files))  ; Remove "." and ".."
    (message "Snippet files: %s" snippet-files)
    snippet-files))

;; if i paste this code:
;; (defun print-keypress ()
;;   (interactive)
;;   (message "Key pressed: %s" (this-command-keys-vector)))
;; (global-set-key (kbd "<S-tab>") 'print-keypress)
;; over a line using visual-line, then the pasting will be incorrect
(defun vim-like-paste ()
  "Paste (yank) replacing the selected region, similar to Vim's paste behavior."
  (interactive)
  (let ((text (current-kill 0)))
    (if (and (evil-visual-state-p)
             (eq evil-visual-selection 'line))
        ;; Handle evil visual line selection
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (delete-region start (min (point-max) (1+ end)))  ; include newline
          (goto-char start)
          (insert text)
          (when (not (string-suffix-p "\n" text))
            (insert "\n")))
      ;; Handle normal paste
      (if (string-match-p "\n$" text)
          (progn
            (beginning-of-line)
            (insert text))
        (insert text)))))

(defun my-completion-preview-insert ()
  "Completes the previewed suggestion and deletes the trailing whitespace."
  (interactive)
  (completion-preview-insert)
  (delete-backward-char 1))

(defun scroll-down-and-recenter (arg)
  "Scroll up ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-up-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-up-and-recenter (arg)
  "Scroll down ARG lines and recenter, preserving horizontal position."
  (interactive "P")
  (let ((col (current-column)))    ; Save the column position
    (scroll-down-command arg)
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-half-up-and-recenter ()
  "Scroll up half screen and recenter, preserving horizontal position."
  (interactive)
  (let ((col (current-column))    ; Save the column position
        (half-height (/ (window-height) 2)))
    (scroll-up-command (- half-height 2))  ; -2 for some context
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun scroll-half-down-and-recenter ()
  "Scroll down half screen and recenter, preserving horizontal position."
  (interactive)
  (let ((col (current-column))    ; Save the column position
        (half-height (/ (window-height) 2)))
    (scroll-down-command (- half-height 2))  ; -2 for some context
    (recenter)
    (move-to-column col)))         ; Restore the column position

(defun my-previous-history-element (arg)
  "Insert the previous history element, moving the cursor to the end."
  (interactive "p")
  (previous-history-element arg)
  (move-end-of-line 1))

(defun my-next-history-element (arg)
  "Insert the next history element, moving the cursor to the end."
  (interactive "p")
  (next-history-element arg)
  (move-end-of-line 1))

(defun add-execute-permissions-to-current-file ()
  "Add execute permissions to the file associated with the current buffer."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-truename buffer-file-name)))
      (shell-command (concat "chmod +x " (shell-quote-argument filename)))
      (message "Execute permissions added to %s" filename))))

(defun add-write-permissions-to-current-file ()
  "Add execute permissions to the file associated with the current buffer."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-truename buffer-file-name)))
      (shell-command (concat "chmod +w " (shell-quote-argument filename)))
      (revert-buffer)
      (message "Write permissions added to %s" filename))))

;; Wrap edit

(defvar-local my-edit-long-lines-region nil
  "Stores the region (beg . end) that was filled when `my-edit-long-lines-mode` was activated.")

(defun my-fill-region (beg end)
  "Fill the region between BEG and END."
  (interactive "r")
  (fill-region beg end))

(defun unfill-region (start end)
  "Unfill the region, joining text paragraphs into a single line."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(defun wrap-edit-exit ()
  "Exit `wrap-edit-mode`, unfill the region, and save the buffer."
  (interactive)
  (when wrap-edit-region
    (unfill-region (car wrap-edit-region) (cdr wrap-edit-region)))
  (save-buffer)
  (wrap-edit-mode -1))

(global-set-key (kbd "C-c g e") 'wrap-edit-mode)


;; Save unexuted minibuffer input

(defvar my-last-unexecuted-minibuffer-input nil
  "Stores the last unexecuted minibuffer input.")

(defun my-save-unexecuted-minibuffer-input ()
  "Save the current minibuffer input if it's not empty."
  (let ((input (minibuffer-contents)))
    (when (and (not (string-empty-p input))
               (not (eq input my-last-unexecuted-minibuffer-input)))
      (setq my-last-unexecuted-minibuffer-input input))))

(add-hook 'minibuffer-exit-hook #'my-save-unexecuted-minibuffer-input)

(defun my-insert-last-unexecuted-minibuffer-input ()
  "Insert the last unexecuted minibuffer input at point."
  (interactive)
  (when my-last-unexecuted-minibuffer-input
    (insert my-last-unexecuted-minibuffer-input)))

(define-key minibuffer-local-map (kbd "C-r") #'my-insert-last-unexecuted-minibuffer-input)


(defun delete-two-chars-back ()
  "Delete the previous two characters."
  (interactive)
  (delete-char -2))

(define-key prog-mode-map (kbd "C-<backspace>") #'delete-two-chars-back)


(defun dired-goto-last-line ()
  "Move to the last line of the Dired buffer, preserving the current column position."
  (interactive)
  (let ((current-col (current-column)))
    (goto-char (point-max))       ;; Move to the end of the buffer
    (forward-line -1)            ;; Move to the last line
    (move-to-column current-col))) ;; Restore the column position


(defun compile-last ()
  "Run the last compile command stored in `compile-command`."
  (interactive)
  (if compile-command
      (compile compile-command)
    (message "No previous compile command found.")))


(defun man-function-at-point ()
  "Look up the man page for the function at point."
  (interactive)
  (let ((function (thing-at-point 'symbol t)))
    (if function
        (man function)
      (message "No function at point."))))
