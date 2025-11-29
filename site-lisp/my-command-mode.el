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
    wdired-mode)
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

;;;###autoload
(define-minor-mode my-command-mode "My command mode for modal editing"
  :lighter " Command"
  :keymap my-command-mode-map
  (if my-command-mode
      (progn
        (hl-line-mode +1)
        (overwrite-mode -1))
    (hl-line-mode -1)))

(provide 'my-command-mode)
