;;; Tyler's emacs config

;;; Commentary:
;; Tyler's Emacs config

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; Add :defer t to use-package expressions by default
(setq use-package-always-defer t)

; Make garbage collection trigger less often
(setq gc-cons-threshold (* 1024 1024 2))

; Increase chunk size when reading from subprocesses
(setq read-process-output-max (* 1024 1024))

; Defer fontification while there is input pending
(setq jit-lock-defer-time 0)

(use-package treesit
  :preface
  (defun tyler/treesit-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
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
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (tyler/treesit-install-grammars))

(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  :custom-face
  (avy-lead-face ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-0 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-1 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-lead-face-2 ((t (:foreground "#e52b50" :background "unset"))))
  (avy-goto-char-timer-face ((t (:foreground "black")))))

(use-package combobulate
  :commands combobulate-mode
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode python-ts-mode yaml-ts-mode css-ts-mode) . #'combobulate-mode)
  :bind (:map combobulate-proffer-map
              ("SPC" . 'next)
              ("p" . 'prev))
  :config
  (setopt combobulate-beginning-of-defun-behavior 'self-and-sibling-first)
  (setopt combobulate-js-ts-enable-attribute-envelopes nil) ; don't auto-insert quotes after = is typed
  :load-path ("site-lisp/combobulate"))

(use-package dash-at-point
  :ensure t)

(use-package eglot
  :config
  (setopt eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs '(vue-ts-mode . ("vls" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  :hook ((typescript-ts-mode tsx-ts-mode python-ts-mode go-ts-mode svelte-mode vue-ts-mode js-ts-mode) . #'eglot-ensure))

(use-package flymake
  :bind (("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)))

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package flymake-eslint
  :ensure t
  :hook ((tsx-ts-mode typescript-ts-mode js-ts-mode vue-ts-mode) . (lambda ()
                                                         (when (not (eq buffer-file-name nil))
                                                           (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js")))
                                                         ; It would make the most sense to call (flymake-eslint-enable) here,
                                                         ; but some interaction with eglot makes that fail. This is a workaround.
                                                         ; https://github.com/orzechowskid/flymake-eslint/issues/23
                                                         (add-hook 'eglot-managed-mode-hook (lambda ()
                                                                                              (flymake-eslint-enable))))))

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

(use-package company
  :ensure t
  :defer nil
  :bind ("M-/" . company-complete)
  :config
  (setq company-idle-delay 0.2))
  ;; (global-company-mode))
  ;; :hook (eglot-mode . company-mode))

(use-package multiple-cursors
  :ensure t)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'right)
  (setq helm-buffer-max-length 50)
  (setq helm-buffers-fuzzy-matching t)
  :bind
  (("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-c C-o" . helm-occur)
   :map helm-map
   ("<up>" . previous-history-element)
   ("<down>" . next-history-element)))

(use-package dockerfile-ts-mode
  :mode "Dockerfile")

(use-package expand-region
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
                       (setq-local tab-width 2))))

;; EasyPG
(use-package epa-file
  :init
  (setq epa-pinentry-mode 'loopback)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package markdown-mode
  :ensure t)

(use-package org-mode
  :init
  (setopt org-agenda-files '("/Users/tyler/Dropbox/notes"))
  (setopt org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"))

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

;; Display

;; remove toolbar, menu bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(menu-bar-mode 0)

;; colors
(set-background-color "#000000")
(set-cursor-color "#ff0000")
(set-foreground-color "#ffffff")

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

(defun tyler/swap-windows (&optional other-win)
  "Swap the buffers displayed in the current window and OTHER-WIN.

If no argument is supplied, OTHER-WIN defaults to the window
returned by `next-window'.

When called interactively and more than two windows are open,
prompt for a buffer name and swap with the window containing that
buffer.

After I wrote this I discovered `window-swap-states', but I'm
keeping it because it's the first real command I wrote!"
  (interactive
   (list (let ((num-windows (length (window-list))))
           (cond ((<= num-windows 1)
                  (user-error "No other window to swap with"))
                 ((> num-windows 2)
                  (get-buffer-window
                   (read-buffer
                    "Swap current buffer with: "
                    (window-buffer (next-window))
                    nil
                    (lambda (buf)
                      (and
                       (get-buffer-window buf)
                       (not (eq buf (buffer-name))))))))))))
  (if (eq other-win nil)
      (setq other-win (next-window)))
  (let ((current-buf (current-buffer)))
    (switch-to-buffer (window-buffer other-win))
    (select-window other-win)
    (switch-to-buffer current-buf)))

(defun tyler/expand-region ()
  (interactive)
  (if (and (boundp 'combobulate-mode) combobulate-mode)
      (command-execute 'combobulate-mark-node-dwim)
    (command-execute 'er/expand-region)))

(defun tyler/transpose-table ()
  (interactive)
  (command-execute 'org-table-create-or-convert-from-region)
  (command-execute 'org-table-transpose-table-at-point))

;; my key bindings
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-c p") 'my-move-line-up)
    (define-key map (kbd "C-c n") 'my-move-line-down)
    (define-key map (kbd "C-c j") 'duplicate-dwim)
    (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
    (define-key map (kbd "C-l") 'goto-line)
    (define-key map (kbd "<up>") 'scroll-down-line)
    (define-key map (kbd "<down>") 'scroll-up-line)
    (define-key map (kbd "C-c C-n") 'mc/mark-more-like-this-extended)
    (define-key map (kbd "C-c C-p") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-l") 'mc/edit-lines)
    (define-key map (kbd "C-c =") 'mc/mark-all-like-this)
    (define-key map (kbd "C-c SPC") 'tyler/expand-region)
    (define-key map (kbd "C-c C-SPC") 'set-rectangular-region-anchor)
    (define-key map (kbd "C-c C-s") 'isearch-forward-symbol-at-point)
    (define-key map (kbd "C-c /") 'completion-at-point)
    (define-key map (kbd "M-/") 'dabbrev-completion)
    (define-key map (kbd "M-.") 'xref-find-definitions)
    (define-key map (kbd "M-?") 'xref-find-references)
    (define-key map (kbd "M-,") 'xref-pop-marker-stack)
    (define-key map (kbd "C-c C-o") 'helm-occur)
    (define-key map (kbd "C-c s") 'window-swap-states)
    (define-key map (kbd "C-c d") 'dash-at-point)
    (define-key map (kbd "C-h a") 'helm-apropos)
    (define-key map (kbd "C-h z") 'shortdoc-display-group)
    (define-key map (kbd "C-c r") 're-builder)
    (define-key map (kbd "C-c C-j") 'avy-goto-char)
    (define-key map [f3] 'kill-buffer)
    (define-key map [f4] 'display-line-numbers-mode)
    (define-key map [f12] 'compile)
    map)
  "My-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

;; Miscellaneous

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

;; use spaces-only indentation
(setq-default indent-tabs-mode nil)

; Don't want any backup files
(setq make-backup-files nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-annotations ((t (:foreground "brightblack"))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit warning))))
 '(flycheck-delimited-error ((t (:background "color-52"))))
 '(header-line ((t (:background "color-235" :inverse-video nil :underline t))))
 '(helm-ff-directory ((t (:foreground "color-25"))))
 '(helm-ff-dotted-directory ((t (:foreground "brightblack"))))
 '(helm-selection ((t (:inverse-video t))))
 '(lsp-headerline-breadcrumb-path-face ((t nil)))
 '(lsp-headerline-breadcrumb-project-prefix-face ((t nil)))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-function-name-face :weight normal))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((default nil) (nil (:background "#111111"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green3"))))
 '(speedbar-directory-face ((default nil) (nil (:foreground "#4400cc" :height 0.8))))
 '(speedbar-file-face ((default nil) (nil (:foreground "light blue" :height 0.8))))
 '(speedbar-selected-face ((default nil) (nil (:foreground "red" :underline t :height 0.8))))
 '(tide-hl-identifier-face ((t (:background "blue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightblack")))))

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Tylers-MacBook-Pro.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(css-indent-offset 2)
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace nil)
 '(eldoc-mode-hook '(eldoc-mode-set-explicitly))
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*.+\\*"))
 '(helm-boring-file-regexp-list
   '("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)" "\\.bzr\\(/\\|$\\)" "CVS\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$"))
 '(helm-display-header-line t)
 '(helm-ff-skip-boring-files nil)
 '(helm-imenu-execute-action-at-once-if-one 'ignore)
 '(helm-recentf-fuzzy-match t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(package-selected-packages
   '(company-jedi vue-ts-mode eldoc-box markdown-mode avy helm-dash flymake-eslint go-mode use-package tree-sitter-langs tree-sitter tide expand-region typescript-mode projectile terraform-mode json-mode flycheck web-mode seq pkg-info multiple-cursors let-alist dash))
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd" ".log" "build" "coverage" "yarn.lock" "package-lock.json" "pnpm-lock.yaml"))
 '(python-flymake-command nil)
 '(python-indent-guess-indent-offset nil)
 '(safe-local-variable-values
   '((flymake-eslint-project-root . "/Users/tyler/pittbos-fe")
     (lsp-python-ms-extra-paths .
                                ["/Users/tyler/ebcs/clients/modules" "/opt/web2py" "/opt/web2py/gluon/packages/dal"])
     (lsp-python-ms-extra-paths .
                                ["/Users/tyler/ebcs/clients/modules" "/opt/web2py" "/opt/web2py/gluon/packages"])
     (lsp-python-ms-extra-paths .
                                ["/Users/tyler/ebcs/clients/modules" "/opt/web2py"])
     (require-final-newline nil)
     (mode-require-final-newline nil)
     (content-type . "jsx")
     (web-mode-content-type . "jsx")))
 '(typescript-indent-level 2))
(put 'narrow-to-region 'disabled nil)
