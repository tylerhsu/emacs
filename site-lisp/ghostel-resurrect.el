;;; ghostel-resurrect.el --- Restore ghostel terminals and their scrollback with tabspaces -*- lexical-binding: t; -*-

;;; Commentary:

;; tmux-resurrect for ghostel terminals, built on tabspaces sessions.
;;
;; Tabspaces saves each tab's buffers and window layout to its session
;; file, and lets packages register how to save and restore buffer kinds
;; it doesn't know.  This file registers a `ghostel' kind:
;;
;; - Save: write the terminal's scrollback (as plain text, soft-wrapped
;;   lines joined) to `ghostel-resurrect-directory'/<id>.txt, and record
;;   the buffer's name, directory, identity and id in the session.
;; - Restore: start a new ghostel terminal in the saved directory, and
;;   before its shell starts, write the saved scrollback into the
;;   terminal, so old output sits above the new prompt.
;;
;; Tabspaces only saves when Emacs exits, which doesn't always happen
;; cleanly on reboot, so `ghostel-resurrect-mode' also saves every
;; `ghostel-resurrect-save-interval' seconds, once Emacs is idle.
;;
;; Each terminal also gets GHOSTEL_RESURRECT_ID in its environment,
;; stable across restores.  To give each terminal its own shell command
;; history, as tmux panes have, name the shell's HISTFILE after it (see
;; `getHistFile' in ~/.zshrc).
;;
;; Limits: colors are not kept (ghostel's module can only export plain
;; text), and command buffers (`ghostel-exec', `ghostel-compile') and
;; remote (TRAMP) terminals are not saved, as an unattended restore
;; should not re-run commands or open connections.

;;; Code:

(require 'ghostel)
(require 'tabspaces)

(defgroup ghostel-resurrect nil
  "Restore ghostel terminals and their scrollback across restarts."
  :group 'ghostel)

(defcustom ghostel-resurrect-directory
  (expand-file-name "ghostel-sessions/" user-emacs-directory)
  "Directory holding saved terminal scrollback.
Files are readable only by you, but scrollback can hold secrets that
were printed in a terminal."
  :type 'directory)

(defcustom ghostel-resurrect-max-lines 10000
  "Most lines of scrollback to save per terminal; nil saves all of it."
  :type '(choice integer (const :tag "All" nil)))

(defcustom ghostel-resurrect-save-interval 300
  "Seconds between periodic session saves; nil saves only on exit."
  :type '(choice integer (const :tag "Only on exit" nil)))

(defcustom ghostel-resurrect-separator "\e[2m── restored %Y-%m-%d %H:%M ──\e[0m"
  "Line written below restored scrollback, or nil for none.
A `format-time-string' format; it may contain terminal escape codes."
  :type '(choice string (const :tag "None" nil)))

(defvar-local ghostel-resurrect--id nil
  "This terminal's id, naming its scrollback file.  Kept across restores.")
(put 'ghostel-resurrect--id 'permanent-local t)

(defvar-local ghostel-resurrect--saved-tick nil
  "`buffer-modified-tick' when this terminal's scrollback was last saved.")

(defvar ghostel-resurrect--saved-ids nil
  "Ids saved during the current session save, or t when not inside one.
Scrollback files of other ids are deleted after a save.")

(defvar ghostel-resurrect--pending nil
  "During a restore, (ID . SCROLLBACK) for the terminal about to spawn.")

(defun ghostel-resurrect--file (id)
  "Return the scrollback file for terminal ID."
  (expand-file-name (concat id ".txt") ghostel-resurrect-directory))

(defun ghostel-resurrect--ensure-id ()
  "Return the current terminal's id, making one if it has none."
  (or ghostel-resurrect--id
      (setq ghostel-resurrect--id
            (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S")
                    (random #x10000)))))

(defun ghostel-resurrect--saveable-p ()
  "Return non-nil when the current buffer is a terminal worth restoring."
  (and (derived-mode-p 'ghostel-mode)
       ghostel--term
       (not (file-remote-p default-directory))
       (not (alist-get 'command ghostel-identity))
       (not (memq (alist-get 'kind ghostel-identity) '(compile exec eshell)))))

(defun ghostel-resurrect--scrollback ()
  "Return the current terminal's scrollback, trimmed to the line limit."
  (let ((text (or (ghostel--copy-all-text ghostel--term) "")))
    (if (not ghostel-resurrect-max-lines)
        text
      (let ((lines (split-string text "\n")))
        (if (<= (length lines) ghostel-resurrect-max-lines)
            text
          (string-join (last lines ghostel-resurrect-max-lines) "\n"))))))

(defun ghostel-resurrect--write-scrollback (id)
  "Write the current terminal's scrollback to ID's file, unless unchanged."
  (let ((file (ghostel-resurrect--file id)))
    (unless (and (eql ghostel-resurrect--saved-tick (buffer-modified-tick))
                 (file-exists-p file))
      (let ((text (ghostel-resurrect--scrollback)))
        (with-file-modes #o700 (make-directory ghostel-resurrect-directory t))
        (with-file-modes #o600
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region text nil file nil 'silent))))
      (setq ghostel-resurrect--saved-tick (buffer-modified-tick)))))

(defun ghostel-resurrect-save-buffer (buffer)
  "Return BUFFER's tabspaces session record, saving its scrollback.
Return nil for buffers that are not restorable terminals."
  (with-current-buffer buffer
    (when (ghostel-resurrect--saveable-p)
      (let ((id (ghostel-resurrect--ensure-id)))
        (condition-case err
            (ghostel-resurrect--write-scrollback id)
          (error (message "ghostel-resurrect: scrollback of %s not saved: %S"
                          (buffer-name) err)))
        (when (listp ghostel-resurrect--saved-ids)
          (push id ghostel-resurrect--saved-ids))
        (list :kind 'ghostel
              :dir default-directory
              :name (buffer-name)
              :id id
              :identity ghostel-identity)))))

(defun ghostel-resurrect--replay ()
  "Write the pending saved scrollback into this terminal.
Runs from `ghostel-pre-spawn-hook', after the terminal exists and
before its shell starts."
  (when-let* ((pending ghostel-resurrect--pending))
    (setq ghostel-resurrect--pending nil
          ghostel-resurrect--id (car pending))
    (let ((text (string-trim-right (cdr pending))))
      (unless (string-empty-p text)
        (ghostel--write-vt
         ghostel--term
         (concat (replace-regexp-in-string "\n" "\r\n" text)
                 "\r\n"
                 (when ghostel-resurrect-separator
                   (concat (format-time-string ghostel-resurrect-separator)
                           "\r\n"))))))))

(defun ghostel-resurrect--export-id ()
  "Put this terminal's id in the environment of the shell about to start."
  (setenv "GHOSTEL_RESURRECT_ID" (ghostel-resurrect--ensure-id)))

(defun ghostel-resurrect-restore-buffer (record)
  "Start a terminal from tabspaces session RECORD and return its buffer."
  (let* ((dir (plist-get record :dir))
         (id (plist-get record :id))
         (identity (let ((i (plist-get record :identity))) (and (consp i) i)))
         (file (and (stringp id) (ghostel-resurrect--file id)))
         (text (and file (file-readable-p file)
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (buffer-string)))))
    (cond
     ;; A terminal shown in several tabs is in each tab's record; restore
     ;; it once and give the later tabs the same buffer.
     ((and (stringp id) (ghostel-resurrect--find id)))
     ((and (stringp dir) (file-directory-p dir))
      (let ((default-directory (file-name-as-directory dir))
            (ghostel-resurrect--pending (and id (cons id (or text "")))))
        ;; A live terminal with the same identity (a restore run twice)
        ;; would make `ghostel-create' clash; let it pick a fresh slot.
        (when (and identity
                   (ghostel-find-buffer-by-identity identity))
          (setq identity nil))
        (ghostel-create (plist-get record :name) nil identity))))))

(defun ghostel-resurrect--find (id)
  "Return the live terminal whose id is ID, or nil."
  (seq-find (lambda (b)
              (and (eq (buffer-local-value 'major-mode b) 'ghostel-mode)
                   (equal (buffer-local-value 'ghostel-resurrect--id b) id)))
            (buffer-list)))

(defun ghostel-resurrect--around-save (save &rest args)
  "Run tabspaces' session SAVE with ARGS, then delete stale scrollback."
  (let ((ghostel-resurrect--saved-ids nil))
    (prog1 (apply save args)
      (when (file-directory-p ghostel-resurrect-directory)
        (dolist (file (directory-files ghostel-resurrect-directory t "\\.txt\\'"))
          (unless (member (file-name-base file) ghostel-resurrect--saved-ids)
            (delete-file file)))))))

(defun ghostel-resurrect-save-now ()
  "Save the tabspaces session, including terminal scrollback."
  (interactive)
  (tabspaces--save-session-smart))

(defvar ghostel-resurrect--timer nil)

(defun ghostel-resurrect--periodic-save ()
  "Save the session once Emacs has been idle briefly.
Saving switches through every tab, so wait for a pause in typing and
skip while the minibuffer is in use."
  (run-with-idle-timer
   3 nil
   (lambda ()
     (unless (active-minibuffer-window)
       (with-demoted-errors "ghostel-resurrect: periodic save failed: %S"
         (let ((inhibit-message t))
           (tabspaces--save-session-smart)))))))

;;;###autoload
(define-minor-mode ghostel-resurrect-mode
  "Save ghostel terminals and their scrollback in tabspaces sessions."
  :global t
  (when ghostel-resurrect--timer
    (cancel-timer ghostel-resurrect--timer)
    (setq ghostel-resurrect--timer nil))
  (cond
   (ghostel-resurrect-mode
    (tabspaces-register-buffer-kind 'ghostel
                                    #'ghostel-resurrect-save-buffer
                                    #'ghostel-resurrect-restore-buffer)
    (add-hook 'ghostel-pre-spawn-hook #'ghostel-resurrect--replay)
    (add-hook 'ghostel-pre-spawn-hook #'ghostel-resurrect--export-id 90)
    (advice-add 'tabspaces--save-session-smart :around
                #'ghostel-resurrect--around-save)
    (when ghostel-resurrect-save-interval
      (setq ghostel-resurrect--timer
            (run-with-timer ghostel-resurrect-save-interval
                            ghostel-resurrect-save-interval
                            #'ghostel-resurrect--periodic-save))))
   (t
    (setq tabspaces--buffer-kind-handlers
          (assq-delete-all 'ghostel tabspaces--buffer-kind-handlers))
    (remove-hook 'ghostel-pre-spawn-hook #'ghostel-resurrect--replay)
    (remove-hook 'ghostel-pre-spawn-hook #'ghostel-resurrect--export-id)
    (advice-remove 'tabspaces--save-session-smart
                   #'ghostel-resurrect--around-save))))

(provide 'ghostel-resurrect)
;;; ghostel-resurrect.el ends here
