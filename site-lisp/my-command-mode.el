;;; my-command-mode.el --- Minor mode for command entering  -*- lexical-binding: t; -*-

(add-hook 'after-change-major-mode-hook 'my-command-mode-if-appropriate)

(defvar my-command-exempt-major-modes
  '(Custom-mode
    Info-mode
    ag-mode
    calculator-mode
    calendar-mode
    cider-test-report-mode
    compilation-mode
    debugger-mode
    dired-mode
    edebug-mode
    ediff-mode
    eww-mode
    geben-breakpoint-list-mode
    git-commit-mode                     ; For versions prior to Magit 2.1.0
    grep-mode
    ibuffer-mode
    magit-popup-mode
    org-agenda-mode
    pdf-outline-buffer-mode
    recentf-dialog-mode
    sldb-mode
    sly-db-mode
    vc-annotate-mode
    occur-mode
    wdired-mode
    xref--xref-buffer-mode)
  "List of major modes that should not start with `my-command-mode' enabled.")

(defun my-command-mode-if-appropriate (&optional arg)
  "Toggle my-command-mode unless the buffer is excluded"
  (when (and
         (not (minibufferp))
         (not (memq major-mode my-command-exempt-major-modes))
    (my-command-mode arg))))

(defun my-command-mode-all (&optional arg)
  "Toggle `my-command-mode' in all buffers.

Toggle the mode if ARG is nil. Enable the mode If ARG is zero or a positive number. Disable the mode ARG is a negative number."
  (interactive)
  (let ((new-status
	 (cond
	  ((null arg) (if (bound-and-true-p my-command-mode) -1 1))
	  ((> 0 arg) -1)
	  (t 1))))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (my-command-mode-if-appropriate new-status)))
          (buffer-list))))

(defvar my-command-mode-map (make-sparse-keymap))
(define-key my-command-mode-map (kbd "0") #'digit-argument)
(define-key my-command-mode-map (kbd "1") #'digit-argument)
(define-key my-command-mode-map (kbd "2") #'digit-argument)
(define-key my-command-mode-map (kbd "3") #'digit-argument)
(define-key my-command-mode-map (kbd "4") #'digit-argument)
(define-key my-command-mode-map (kbd "5") #'digit-argument)
(define-key my-command-mode-map (kbd "6") #'digit-argument)
(define-key my-command-mode-map (kbd "7") #'digit-argument)
(define-key my-command-mode-map (kbd "8") #'digit-argument)
(define-key my-command-mode-map (kbd "9") #'digit-argument)
(define-key my-command-mode-map (kbd "-") #'negative-argument)

(defvar my-command-mode-cursor-type '(box . 4)
  "`cursor-type' while `my-command-mode' is on.")

(defvar my-command-mode-insert-cursor-type '(box . 4)
  "`cursor-type' while `my-command-mode' is off, i.e. typing text.")

(defun my-command-mode-current-cursor-type ()
  "The `cursor-type' for the current state of `my-command-mode'."
  (if my-command-mode my-command-mode-cursor-type my-command-mode-insert-cursor-type))

(defvar my-command-mode-insert-cursor-color "spring green"
  "Cursor color while `my-command-mode' is off.
While it is on, the cursor has its default color (the `cursor' face's spec).")

(defun my-command-mode-update-cursor-color (&optional frame)
  "Color FRAME's cursor by `my-command-mode' in its selected window's buffer.
The cursor color is per frame, not per buffer, so this runs whenever the
mode toggles in the visible buffer or the window/buffer selection changes."
  (let* ((frame (if (framep frame) frame (selected-frame)))
         (insert (with-current-buffer (window-buffer (frame-selected-window frame))
                   (not (bound-and-true-p my-command-mode)))))
    ;; Only touch the face on a state change, not on every window switch.
    (unless (eq insert (frame-parameter frame 'my-command-mode--insert-cursor))
      (set-frame-parameter frame 'my-command-mode--insert-cursor insert)
      (if insert
          (set-face-background 'cursor my-command-mode-insert-cursor-color frame)
        ;; Back to the default: recompute the face from its theme/defface spec.
        (face-spec-recalc 'cursor frame)))))

(add-hook 'window-selection-change-functions #'my-command-mode-update-cursor-color)
(add-hook 'window-buffer-change-functions #'my-command-mode-update-cursor-color)

;;;###autoload
(define-minor-mode my-command-mode "My command mode for modal editing"
  :lighter " Command"
  :keymap my-command-mode-map
  (if my-command-mode
      (progn
        (hl-line-mode +1)
        (overwrite-mode -1))
    (hl-line-mode -1))
  (setq-local cursor-type (my-command-mode-current-cursor-type))
  (when (eq (current-buffer) (window-buffer (selected-window)))
    (my-command-mode-update-cursor-color)))

(provide 'my-command-mode)
