;;; Tyler's emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Tyler's Emacs config

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package treesit
  :preface
  (defun tyler/treesit-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; js, ts, and tsx are pinned because the immediate subsequent versions resulted in broken syntax highlighting.
             '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (go "https://github.com/tree-sitter/tree-sitter-go")
               (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
               (css "https://github.com/tree-sitter/tree-sitter-css")
               (html "https://github.com/tree-sitter/tree-sitter-html")
               (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (vue "https://github.com/ikatyang/tree-sitter-vue")
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it≈
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (tyler/treesit-install-grammars))

(use-package avy
  :ensure t
  :init
  (bind-key* "C-c C-j" 'avy-isearch)
  :config
  (setq avy-background t)
  :custom-face
  (avy-lead-face ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-0 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-1 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-2 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-goto-char-timer-face ((t (:foreground "black")))))

(use-package eglot
  :ensure t
  :config
  (setopt eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs '(vue-ts-mode . ("vls" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  :hook ((typescript-ts-mode tsx-ts-mode python-ts-mode go-ts-mode svelte-mode vue-ts-mode js-ts-mode) . #'eglot-ensure))

(use-package flymake
  :init
  (bind-key* "C-c f l" 'flymake-show-project-diagnostics)
  (bind-key* "C-c f n" 'flymake-goto-next-error)
  (bind-key* "C-c f p" 'flymake-goto-prev-error))

(use-package flymake-eslint
  :ensure t
  :hook
  (eglot-managed-mode
   . (lambda ()
       (let ((eslint-executable (concat (locate-dominating-file buffer-file-name "node_modules") "node_modules/.bin/eslint")))
         (when (and (or (derived-mode-p 'web-mode)
                        (derived-mode-p 'js-mode)
                        (derived-mode-p 'typescript-ts-mode)
                        (derived-mode-p 'tsx-ts-mode))
                    (file-exists-p eslint-executable))
           (setq-local flymake-eslint-executable-name eslint-executable)
           (flymake-eslint-enable))))))

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package tsx-ts-mode
  :mode "\\.[jt]sx?\\'")

(use-package js
  :config
  (setq js-indent-level 2))

(use-package terraform-mode
  :ensure t)

(use-package vue-ts-mode
  :mode "\\.vue\\'"
  :hook (vue-ts-mode . (lambda()
                         ;; vue-ts-mode does not derive from prog-mode, so prog-mode hook is copied here.

                         ;; electric indent mode indents both the new line and the previous line when
                         ;; you press enter. Indent only the new line.
                         (electric-indent-mode 0)
                         (subword-mode)
                         ; Enable popup autocomplete
                         (corfu-mode +1)))
  :vc (:url "https://github.com/8uff3r/vue-ts-mode"
       :branch "main"))

(use-package python-ts-mode
  :mode "\\.py\\'")

(use-package html-ts-mode
  :mode "\\.html\\'"
  :load-path ("site-lisp/html-ts-mode"))

(use-package css-ts-mode
  :mode "\\.s?css\\'")

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'")

(use-package go-ts-mode
  :mode "\\.go\\'"
  :config
  (setq go-ts-mode-indent-offset 2))

;; popup UI for in-buffer autocompletion
(use-package corfu
  :ensure t
  :init
	(setq corfu-auto t)
	(setq corfu-auto-delay 0.01)
  ;; Let corfu work in terminal UI. No longer needed in emacs 31 maybe.
	(unless (display-graphic-p)
		(add-to-list 'load-path (directory-file-name (expand-file-name "~/.emacs.d/site-lisp/emacs-corfu-terminal")))
		(add-to-list 'load-path (directory-file-name (expand-file-name "~/.emacs.d/site-lisp/emacs-popon")))
		(defvar corfu-terminal-mode nil "non-nil if Corfu Terminal mode is enabled")
		(require 'corfu-terminal)
    (corfu-terminal-mode)))

(use-package multiple-cursors
  :ensure t
  :config
  (bind-key* "C-c C-n" 'mc/mark-more-like-this)
  (bind-key* "C-c C-p" 'mc/mark-previous-like-this)
  (bind-key* "C-c C-=" 'mc/mark-all-like-this))

;; Minibuffer autocomplete UI that shows options in a vertical list.
;; Similar to builtin fido-vertical-mode, but a little nicer
;; because it uses async 
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom-face
  ;; highlighted option
  (vertico-current ((t (:background "#1d3131"))))
  :init
  (vertico-mode))

;; Adds various annotations beside minibuffer autocomplete options
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Minibuffer autocomplete backend. consult-* functions enhance
;; commands by providing completions that make sense
;; for the given command.
(use-package consult
  :ensure t
  :bind (("<remap> <switch-to-buffer>" . consult-buffer)
         ("<remap> <yank-pop>" . consult-yank-replace)
				 ("<remap> <imenu>" . consult-imenu-multi)))

;; Fuzzy selection of autocomplete options
(use-package orderless
  :ensure t
  :custom
  ;; completion-styles is a builtin emacs variable controlling how completion candidates are decided.
  ;; basic is the default. This package adds 'orderless'.
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
	:init
	(recentf-mode +1))

(use-package dockerfile-ts-mode
  :mode "Dockerfile")

(use-package markdown-mode
  :ensure t)

(use-package expand-region
  :ensure t
  :init
  (bind-key* "C-c SPC" 'er/expand-region))

(use-package rainbow-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode)
  :bind (("C-c RET" . restclient-http-send-current))
  :hook (restclient-mode . (lambda()
                             (setq-local tab-width 2))))

(use-package prog-mode
  :bind ("RET" . 'electric-newline-and-maybe-indent)
  :hook (prog-mode . (lambda()
                       ;; electric indent mode indents both the new line and the previous line when
                       ;; you press enter. Indent only the new line.
                       (electric-indent-mode 0)
                       (subword-mode)
                       ; Enable popup autocomplete
                       (corfu-mode +1)
                       (setq-local tab-width 2))))

;; EasyPG
;; Requires `brew install gpg2`
(use-package epa-file
  :init
  (setq epa-pinentry-mode 'loopback)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package eldoc
  :preface
  (defun tyler/set-max-eldoc-height-then-display (oldfunc &rest args)
    "Wrap `eldoc-display-in-echo-area' such that it displays multiline
docs but never pops up in a way that causes the window to
scroll. Such scrolling happens when the echo area grows enough to
occupy the row at point. Prevent it by setting the maximum eldoc
height no greater than point's distance from window bottom."
    (let* ((point-y (or (cdr (posn-x-y (posn-at-point))) 0))
           (point-height (-
                          (window-height)
                          (window-mode-line-height)
                          (window-height (minibuffer-window))
                          point-y))
           (available (min (1+ point-height) 10))
           (old max-mini-window-height))
      (setq-default max-mini-window-height available)
      (apply oldfunc args)
      (setq-default max-mini-window-height old)))
  (defun tyler/eldoc--echo-area-substring (available)
    "Override `eldoc--echo-area-substring' so it uses visual lines
instead of buffer lines."
    (let ((start (prog1 (progn
                          (goto-char (point-min))
                          (skip-chars-forward " \t\n")
                          (point))
                   (line-move-visual (1- available) t)
                   (end-of-visual-line)
                   (goto-char (1- (point)))))
          (truncated (save-excursion
                       (goto-char (1+ (point)))
                       (skip-chars-forward " \t\n")
                       (not (eobp)))))
      (cond ((eldoc--echo-area-prefer-doc-buffer-p truncated)
             nil)
            ((and truncated
                  (> available 1)
                  eldoc-echo-area-display-truncation-message)
             (line-move-visual -1 t)
             (end-of-visual-line)
             (concat (buffer-substring start (point))
                     (format
                      "\n(Documentation truncated. Use `%s' to see rest)"
                      (substitute-command-keys "\\[eldoc-doc-buffer]"))))
            (t
             (buffer-substring start (point))))))
  :config
  (setopt eldoc-echo-area-prefer-doc-buffer t)
  (advice-add 'eldoc-display-in-echo-area :around 'tyler/set-max-eldoc-height-then-display)
  (advice-add 'eldoc--echo-area-substring :override 'tyler/eldoc--echo-area-substring))

(use-package prisma-mode
  :mode "\\.prisma\\'"
  :load-path ("site-lisp/prisma-mode"))

(defun tyler/invert-command (command)
  "Return a version of COMMAND that inverts the sign on the prefix."
  (lambda ()
    (interactive)
    (let ((current-prefix-arg (- (prefix-numeric-value current-prefix-arg))))
      (call-interactively command))))

(defun tyler/repeat-command (command &optional negative-command)
  "Return a version of COMMAND that repeats a number of times equal to the prefix. If NEGATIVE-COMMAND is supplied, invoke that instead of COMMAND when the prefix is negative."
  (lambda (arg)
    (interactive "p")
    (let ((func-to-run (if (and negative-command (< arg 0))
                           negative-command
                         command)))
      (dotimes (_ (abs arg))
        (funcall func-to-run)))))

(defun tyler/kill-ring-save (region-start-func region-end-func)
  "Return a command that copies text from point after evaluating REGION-START-FUNC to point after evaluating REGION-END-FUNC."
  (lambda ()
    (interactive)
    (let ((beginning (save-excursion
                       (unless (null region-start-func)
                         (call-interactively region-start-func))
                       (point)))
          (end (save-excursion
                 (unless (null region-end-func)
                   (call-interactively region-end-func))
                 (point))))
      (kill-ring-save beginning end)
      (message "Copied"))))

(use-package my-command-mode
  :load-path ("site-lisp")
  :bind (("C-;" . #'my-command-mode-all))
  :config
  ;; move
  (define-key my-command-mode-map (kbd "n") #'next-line)
  (define-key my-command-mode-map (kbd "p") #'previous-line)
  (define-key my-command-mode-map (kbd "f") #'forward-word)
  (define-key my-command-mode-map (kbd "b") #'backward-word)
  (define-key my-command-mode-map (kbd "j") #'forward-sexp)
  (define-key my-command-mode-map (kbd "h") #'backward-sexp)
  (define-key my-command-mode-map (kbd "a") #'back-to-indentation)
  (define-key my-command-mode-map (kbd "e") #'end-of-line)
  (define-key my-command-mode-map (kbd "d") #'down-list)
  (define-key my-command-mode-map (kbd "u") #'up-list)
  (define-key my-command-mode-map (kbd ",") #'beginning-of-defun)
  (define-key my-command-mode-map (kbd ".") #'end-of-defun)
  (define-key my-command-mode-map (kbd "<") #'beginning-of-buffer)
  (define-key my-command-mode-map (kbd ">") #'end-of-buffer)
  (define-key my-command-mode-map (kbd "s") #'isearch-forward)
  (define-key my-command-mode-map (kbd "r") #'isearch-backward)
  (define-key my-command-mode-map (kbd "v") #'scroll-up)
  (define-key my-command-mode-map (kbd "o") #'scroll-down)
  (define-key my-command-mode-map (kbd "l") #'recenter-top-bottom)
  (define-key my-command-mode-map (kbd "xx") #'exchange-point-and-mark)
  (define-key my-command-mode-map (kbd "[") #'point-to-register)
  (define-key my-command-mode-map (kbd "]") #'jump-to-register)
  (define-key my-command-mode-map (kbd "co") (lambda () (interactive) (call-interactively 'isearch-occur) (call-interactively 'other-window)))
  ;; misc
  (define-key my-command-mode-map (kbd "gs") #'query-replace-regexp)
  (define-key my-command-mode-map (kbd ";") #'comment-line)
  (define-key my-command-mode-map (kbd "z") #'undo)
  (define-key my-command-mode-map (kbd "/") #'fixup-whitespace)
  (define-key my-command-mode-map (kbd "(") #'kmacro-start-macro)
  (define-key my-command-mode-map (kbd ")") #'kmacro-end-and-call-macro)
  ;; insert
  (define-key my-command-mode-map (kbd "i") (lambda () (interactive) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "P") (lambda () (interactive) (beginning-of-line) (open-line 1) (indent-for-tab-command) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "N") (lambda () (interactive) (end-of-line) (electric-newline-and-maybe-indent) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "E") (lambda () (interactive) (end-of-line) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "A") (lambda () (interactive) (beginning-of-line) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "M") (lambda () (interactive) (back-to-indentation) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "O") (lambda () (interactive) (overwrite-mode) (my-command-mode-all -1)))
  ;; mark
  (define-key my-command-mode-map (kbd "SPC") #'set-mark-command)
  (define-key my-command-mode-map (kbd "mf") #'mark-word)
  (define-key my-command-mode-map (kbd "mb") (tyler/invert-command 'mark-word))
  (define-key my-command-mode-map (kbd "mw") (lambda () (interactive) (backward-word) (call-interactively 'mark-word)))
  (define-key my-command-mode-map (kbd "mj") #'mark-sexp)
  (define-key my-command-mode-map (kbd "mh") (tyler/invert-command 'mark-sexp))
  (define-key my-command-mode-map (kbd "ms") (lambda () (interactive) (backward-sexp) (call-interactively 'mark-sexp)))
  (define-key my-command-mode-map (kbd "me") (lambda () (interactive) (push-mark (line-end-position) nil t)))
  (define-key my-command-mode-map (kbd "mm") (lambda () (interactive) (save-excursion (back-to-indentation) (push-mark nil nil t))))
  (define-key my-command-mode-map (kbd "ma") (lambda () (interactive) (push-mark (line-beginning-position) nil t)))
  (define-key my-command-mode-map (kbd "ml") #'tyler/mark-whole-line)
  (define-key my-command-mode-map (kbd "m.") #'mark-defun)
  ;; kill/yank
  (define-key my-command-mode-map (kbd "kd") #'delete-char)
  (define-key my-command-mode-map (kbd "kf") #'kill-word)
  (define-key my-command-mode-map (kbd "kb") #'backward-kill-word)
  (define-key my-command-mode-map (kbd "kw") (lambda () (interactive) (backward-word) (call-interactively 'kill-word)))
  (define-key my-command-mode-map (kbd "kj") #'kill-sexp)
  (define-key my-command-mode-map (kbd "kh") #'backward-kill-sexp)
  (define-key my-command-mode-map (kbd "ks") (lambda () (interactive) (backward-sexp) (call-interactively 'kill-sexp)))
  (define-key my-command-mode-map (kbd "ke") #'kill-line)
  (define-key my-command-mode-map (kbd "km") (lambda () (interactive) (kill-line 0) (call-interactively 'indent-for-tab-command)))
  (define-key my-command-mode-map (kbd "ka") (lambda () (interactive) (kill-line 0)))
  (define-key my-command-mode-map (kbd "kl") #'tyler/kill-whole-line)
  (define-key my-command-mode-map (kbd "kp") (tyler/repeat-command 'delete-indentation))
  (define-key my-command-mode-map (kbd "kr") #'kill-region)
  (define-key my-command-mode-map (kbd "y") #'yank)
  (define-key my-command-mode-map (kbd "gy") #'consult-yank-replace)
  ;; copy
  (define-key my-command-mode-map (kbd "cf") (tyler/kill-ring-save nil 'forward-word))
  (define-key my-command-mode-map (kbd "cb") (tyler/kill-ring-save 'backward-word nil))
  (define-key my-command-mode-map (kbd "cw") (tyler/kill-ring-save 'backward-word 'forward-word))
  (define-key my-command-mode-map (kbd "cj") (tyler/kill-ring-save nil 'forward-sexp))
  (define-key my-command-mode-map (kbd "ch") (tyler/kill-ring-save 'backward-sexp nil))
  (define-key my-command-mode-map (kbd "cs") (tyler/kill-ring-save 'backward-sexp 'forward-sexp))
  (define-key my-command-mode-map (kbd "ce") (tyler/kill-ring-save nil 'end-of-line))
  (define-key my-command-mode-map (kbd "cm") (tyler/kill-ring-save 'back-to-indentation nil))
  (define-key my-command-mode-map (kbd "ca") (tyler/kill-ring-save 'beginning-of-line nil))
  (define-key my-command-mode-map (kbd "cl") #'tyler/copy-whole-line)
  (define-key my-command-mode-map (kbd "cr") #'kill-ring-save)
  (define-key my-command-mode-map (kbd "cn") #'duplicate-dwim)
  ;; transpose
  (define-key my-command-mode-map (kbd "tf") #'transpose-words)
  (define-key my-command-mode-map (kbd "tb") (tyler/invert-command 'transpose-words))
  (define-key my-command-mode-map (kbd "tj") #'transpose-sexps)
  (define-key my-command-mode-map (kbd "th") (tyler/invert-command 'transpose-sexps))
  (define-key my-command-mode-map (kbd "tn") #'tyler/transpose-lines)
  (define-key my-command-mode-map (kbd "tp") (tyler/invert-command 'tyler/transpose-lines))
  ;; file/buffer/window
  (define-key my-command-mode-map (kbd "xb") #'switch-to-buffer)
  (define-key my-command-mode-map (kbd "xs") #'save-buffer)
  (define-key my-command-mode-map (kbd "xf") #'find-file)
  (define-key my-command-mode-map (kbd "xk") #'kill-buffer)
  (define-key my-command-mode-map (kbd "xd") #'dired-jump)
  (define-key my-command-mode-map (kbd "x1") #'delete-other-windows)
  (define-key my-command-mode-map (kbd "x2") #'split-window-below)
  (define-key my-command-mode-map (kbd "x3") #'split-window-right)
  (define-key my-command-mode-map (kbd "xo") #'other-window)
  (define-key my-command-mode-map (kbd "xpf") #'project-find-file)
  (define-key my-command-mode-map (kbd "xpg") #'project-find-regexp)
  (define-key my-command-mode-map (kbd "xpp") #'project-switch-project))

;; Custom functions
(defun tyler/transpose-lines (arg)
  "Move the current line down one. With prefix, move that many lines down. With negative prefix, move up."
  (interactive "p")
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines arg)
    (forward-line -1)
    (move-to-column col)))

(defun tyler/kill-whole-line ()
  "Kill the current line. With prefix, kill current line and that many lines from current."
  (interactive)
  (let* ((numeric-prefix (prefix-numeric-value current-prefix-arg))
         (current-prefix-arg (cond
                              ((null current-prefix-arg) current-prefix-arg)
                              ((< numeric-prefix 0) (- numeric-prefix 1))
                              ((> numeric-prefix 0) (+ numeric-prefix 1))
                              (t numeric-prefix))))
    (call-interactively 'kill-whole-line)))

(defun tyler/mark-whole-line ()
  "Mark the current line starting from indentation. With prefix, mark current line and that many lines from current."
  (interactive)
  (let ((negative (< (prefix-numeric-value current-prefix-arg) 0)))
    (if negative (end-of-line) (back-to-indentation))
    (push-mark nil nil t)
    (or (null current-prefix-arg) (call-interactively 'next-line))
    (if negative
        (back-to-indentation)
      (progn
        (end-of-line)
        (exchange-point-and-mark)))))

(defun tyler/copy-whole-line (arg)
  "Copy the current line. With prefix, copy current line and that many lines from current."
  (interactive "P")
  (let* ((numeric-prefix (prefix-numeric-value arg))
         (beginning (line-beginning-position (when (< numeric-prefix 0) (+ numeric-prefix 1))))
         (end (line-end-position (when
                                     (and
                                      (not (null arg))
                                      (> numeric-prefix 0))
                                   (+ 1 numeric-prefix)))))
         (kill-ring-save beginning end))
  (message "Copied"))

;; (defun tyler/swap-windows (&optional other-win)
;;   "Swap the buffers displayed in the current window and OTHER-WIN.

;; If no argument is supplied, OTHER-WIN defaults to the window
;; returned by `next-window'.

;; When called interactively and more than two windows are open,
;; prompt for a buffer name and swap with the window containing that
;; buffer.

;; After I wrote this I discovered `window-swap-states', but I'm
;; keeping it because it's the first real command I wrote!"
;;   (interactive
;;    (list (let ((num-windows (length (window-list))))
;;            (cond ((<= num-windows 1)
;;                   (user-error "No other window to swap with"))
;;                  ((> num-windows 2)
;;                   (get-buffer-window
;;                    (read-buffer
;;                     "Swap current buffer with: "
;;                     (window-buffer (next-window))
;;                     nil
;;                     (lambda (buf)
;;                       (and
;;                        (get-buffer-window buf)
;;                        (not (eq buf (buffer-name))))))))))))
;;   (if (eq other-win nil)
;;       (setq other-win (next-window)))
;;   (let ((current-buf (current-buffer)))
;;     (switch-to-buffer (window-buffer other-win))
;;     (select-window other-win)
;;     (switch-to-buffer current-buf)))

(defun tyler/split-window-sensibly (&optional window)
  "Copy/paste of `split-window-sensibly', but whereas that function
prefers a horizontal divider when horizontal and vertical are both
possible, this function prefers a vertical one."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
            ;; Split window horizontally.
            (with-selected-window window
              (split-window-right)))
        (and (window-splittable-p window)
            ;; Split window vertically.
            (with-selected-window window
              (split-window-below)))
       (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it vertically disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame nil 'nomini)
              t)))
        (not (window-minibuffer-p window))
        (let ((split-height-threshold 0))
          (when (window-splittable-p window)
            (with-selected-window window
              (split-window-below))))))))
(setopt split-window-preferred-function 'tyler/split-window-sensibly)


;; (defun tyler/expand-region ()
;;   (interactive)
;;   (if (and (boundp 'combobulate-mode) combobulate-mode)
;;       (command-execute 'combobulate-mark-node-dwim)
;;     (command-execute 'er/expand-region)))

(defun tyler/transpose-table ()
  (interactive)
  (command-execute 'org-table-create-or-convert-from-region)
  (command-execute 'org-table-transpose-table-at-point))

(defun tyler/scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun tyler/scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(use-package emacs
  :init
  (setq-default display-line-numbers 'relative)
  ;; prefer spaces-only indentation
  (setq-default indent-tabs-mode nil)
  ;; bind-key* makes sure this binding overrides any other mode's bindings.
  (bind-key* "C-c p" (tyler/invert-command 'tyler/transpose-lines))
  (bind-key* "C-c n" 'tyler/transpose-lines)
  (bind-key* "C-c j" 'duplicate-dwim)
  (bind-key* "<up>" 'scroll-down-line)
  (bind-key* "<down>" 'scroll-up-line)
  (bind-key* "C-c C-SPC" 'set-rectangular-region-anchor)
  (bind-key* "C-c C-s" 'isearch-forward-thing-at-point)
  (bind-key* "C-c s" 'window-swap-states)
  (bind-key* "C-h z" 'shortdoc-display-group)
  (bind-key* "C-c b" 'scratch-buffer)
  (bind-key* "C-c l" 'display-line-numbers-mode)
  (bind-key* "C-c i" 'ibuffer)
  (bind-key* "M-%" 'query-replace-regexp)
  (bind-key* "C-z" 'undo)
  (bind-key* "<remap> <scroll-up-command>" 'tyler/scroll-half-page-up)
  (bind-key* "<remap> <scroll-down-command>" 'tyler/scroll-half-page-down)
  
  ;; remove toolbar, menu bar
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (menu-bar-mode 0)

  ;; Automatically revert buffers when they change on disk
  (global-auto-revert-mode 1)

  ;; Put autosave files (ie #foo#) in one place, *not*
  ;; scattered all over the file system!
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "autosaves"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "autosaves/") t)))

  ;; typing replaces selection
  (delete-selection-mode 1)

  ;; go right to an empty buffer
  (setq inhibit-startup-message t)

  ;; Make all "yes or no" prompts show "y or n" instead
  (fset 'yes-or-no-p 'y-or-n-p)

  ; Don't want any backup files
  (setq make-backup-files nil)

  ; Enable completion-preview-mode (causes gray text to appear after point when autocompleting).
  ;; (global-completion-preview-mode)

  :custom-face
  (completions-annotations ((t (:foreground "powderblue" :underline nil))))
  (error ((t (:foreground "tomato" :weight bold))))
  (mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "burlywood1" :background "#56514c"))))
  (mode-line-inactive ((t (:foreground "gray60" :background "gray15" :inherit mode-line)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy corfu embark-consult expand-region flymake-eslint god-mode
         marginalia markdown-mode multiple-cursors orderless
         rainbow-mode restclient svelte-mode terraform-mode vertico
         vue-ts-mode))
 '(package-vc-selected-packages
   '((vue-ts-mode :url "https://github.com/8uff3r/vue-ts-mode" :branch
                  "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray24")))))
