;;; Tyler's emacs config

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
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
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
  :bind (("C-c C-j" . avy-isearch))
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
  :bind (("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)))

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
  :mode "\\.[jt]sx\\'")

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package js-ts-mode
  :mode "\\.js\\'")

(use-package vue-ts-mode
  :mode "\\.vue\\'"
  :load-path ("site-lisp/vue-ts-mode"))

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
  (global-corfu-mode)
	(setq corfu-auto t)
	(setq corfu-auto-delay 0.01)
  ;; Let corfu work in terminal UI. No longer needed in emacs 31 maybe.
	(unless (display-graphic-p)
		(add-to-list 'load-path (directory-file-name (expand-file-name "~/.emacs.d/site-lisp/emacs-corfu-terminal")))
		(add-to-list 'load-path (directory-file-name (expand-file-name "~/.emacs.d/site-lisp/emacs-popon")))
		(defvar corfu-terminal-mode nil "non-nil if Corfu Terminal mode is enabled")
		(require 'corfu-terminal)
    (corfu-terminal-mode))
	:custom-face
	(corfu-default ((t (:background "color-236")))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-n" . mc/mark-more-like-this-extended)
         ("C-c C-p" . mc/mark-previous-like-this)
         ("C-c C-l" . mc/edit-lines)
         ("C-c =" . mc/mark-all-like-this)))

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
  (vertico-current ((t (:background "darkslategray"))))
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
				 ("<remap> <imenu>" . consult-imenu)))

;; Fuzzy selection of minibuffer autocomplete options
(use-package orderless
  :ensure t
  :custom
  ;; completion-styles is a builtin emacs variable controlling how completion candidates are decided.
  ;; basic is the default. This package adds 'orderless'.
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Provide context actions. Like a right-click menu for whatever is at point.
;; Mainly for minibuffer completion candidates, e.g. delete a file selected during C-x C-f.
(use-package embark
  :ensure t
  :bind (:map vertico-map
							("TAB" . embark-act)
							("<remap> <set-mark-command>" . embark-select)))

(use-package embark-consult
  :ensure t)

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
  :bind (("C-c SPC" . er/expand-region)))

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

;; Custom functions

(defun my-move-line-up()
  "Move the current line up one."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (if (> col 0)
        (forward-line -1))
    (move-to-column col)))

(defun my-move-line-down()
  "Move the current line down one."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

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

;; (defun tyler/split-window-sensibly (&optional window)
;;   "Copy/paste of `split-window-sensibly', but whereas that function
;; prefers a horizontal divider when horizontal and vertical are both
;; possible, this function prefers a vertical one."
;;   (let ((window (or window (selected-window))))
;;     (or (and (window-splittable-p window t)
;;             ;; Split window horizontally.
;;             (with-selected-window window
;;               (split-window-right)))
;;         (and (window-splittable-p window)
;;             ;; Split window vertically.
;;             (with-selected-window window
;;               (split-window-below)))
;;        (and
;;          ;; If WINDOW is the only usable window on its frame (it is
;;          ;; the only one or, not being the only one, all the other
;;          ;; ones are dedicated) and is not the minibuffer window, try
;;          ;; to split it vertically disregarding the value of
;;          ;; `split-height-threshold'.
;;          (let ((frame (window-frame window)))
;;            (or
;;             (eq window (frame-root-window frame))
;;             (catch 'done
;;               (walk-window-tree (lambda (w)
;;                                   (unless (or (eq w window)
;;                                               (window-dedicated-p w))
;;                                     (throw 'done nil)))
;;                                 frame nil 'nomini)
;;               t)))
;;         (not (window-minibuffer-p window))
;;         (let ((split-height-threshold 0))
;;           (when (window-splittable-p window)
;;             (with-selected-window window
;;               (split-window-below))))))))
;; (setopt split-window-preferred-function 'tyler/split-window-sensibly)


;; (defun tyler/expand-region ()
;;   (interactive)
;;   (if (and (boundp 'combobulate-mode) combobulate-mode)
;;       (command-execute 'combobulate-mark-node-dwim)
;;     (command-execute 'er/expand-region)))

(defun tyler/transpose-table ()
  (interactive)
  (command-execute 'org-table-create-or-convert-from-region)
  (command-execute 'org-table-transpose-table-at-point))

(use-package emacs
  :bind (("C-z" . undo)
         ("C-c p" . my-move-line-up)
         ("C-c n" . my-move-line-down)
         ("C-c j" . duplicate-dwim)
         ("<up>" . scroll-down-line)
         ("<down>" . scroll-up-line)
         ("C-c C-SPC" . set-rectangular-region-anchor)
         ("C-c C-s" . isearch-forward-thing-at-point)
         ("C-c s" . window-swap-states)
         ("C-h z" . shortdoc-display-group)
         ("C-c b" . scratch-buffer)
         ("C-c l" . display-line-numbers-mode)
				 ("M-%" . query-replace-regexp))
  :config
  ;; prefer spaces-only indentation
  (setq indent-tabs-mode nil)

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

  :custom-face
  (completions-annotations ((t (:foreground "powderblue" :underline nil))))
  (error ((t (:foreground "tomato" :weight bold))))
  (header-line ((t (:background "color-235" :inverse-video nil :underline t))))
  (mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "burlywood1" :background "#56514c"))))
  (mode-line-inactive ((t (:foreground "gray60" :background "gray15" :inherit mode-line)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(corfu wfnames vertico svelte-mode restclient orderless multiple-cursors markdown-mode marginalia flymake-eslint expand-region embark-consult company avy async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
