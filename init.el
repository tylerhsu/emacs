;;; Tyler's emacs config

;;; Commentary:
;; Tyler's Emacs config

;;; Code:

(setq byte-compile-warnings '(cl-functions))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package eglot
  :ensure t
  :config
  (setq eglot-events-buffer-size 0)
  ;; https://github.com/joaotavora/eglot/discussions/898
  ;; show docs and diagnostics both at once in echo area when hovering
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(use-package tree-sitter
  :ensure t
  :config
  (use-package tree-sitter-langs :ensure t)
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; use tree-sitter for syntax highlighting
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(defun my-maybe-set-eslint-project-root ()
  "Set flymake-eslint-project-root if possible"
  (when (not (eq buffer-file-name nil))
    (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js"))))

(use-package typescript-mode
  :after (tree-sitter eglot)
  :init
  ;; The particular name typescriptreact-mode is necessary to get
  ;; eglot to send the correct languageId to the lsp server.  See
  ;; https://github.com/joaotavora/eglot/issues/624.  Without this,
  ;; tsx is not recognized, so you get millions of diagnostic errors
  ;; from the lsp server wherever there is tsx in the file.
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  ;; tree-sitter-based indentation for typescript/tsx
  ;; not on melpa, so I've installed it in .emacs.d/site-lisp
  ;; https://github.com/orzechowskid/tsi.el/
  (use-package tsi-typescript :load-path "site-lisp")
  ;; Have flymake report eslint diagnostics
  ;; https://github.com/orzechowskid/flymake-eslint
  (use-package flymake-eslint :ensure t)
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  :mode ("\\.[jt]sx?\\'" . typescriptreact-mode)
  :hook (typescript-mode . (lambda()
                             (eglot-ensure)
                             ;; Enable flymake-eslint. Getting this to work required an edit to flymake-eslint.el.
                             ;; https://github.com/orzechowskid/flymake-eslint/issues/23
                             (my-maybe-set-eslint-project-root)
                             (flymake-eslint-enable)
                             (tsi-typescript-mode)
                             (subword-mode))))

(use-package vue-mode
  :after eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls" "--stdio")))
  :hook (vue-mode . (lambda()
                      (eglot-ensure)
                      (set-face-background 'mmm-default-submode-face nil))))

(use-package python-mode
  :hook (python-mode . eglot-ensure))

(use-package svelte-mode
  :after eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  :hook (svelte-mode . eglot-ensure))

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure))

;; (use-package company
;;   :ensure t
;;   :bind ("M-/" . company-complete)
;;   :hook (eglot-mode . company-mode))

(use-package multiple-cursors
  :ensure t)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-buffer-max-length 50)
  (setq helm-buffers-fuzzy-matching t)
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c C-o" . helm-occur)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  :bind (("C-x p f" . helm-projectile)
         ("C-x p s" . helm-projectile-ack)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package expand-region
  :ensure t)

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package ace-jump-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode)
  :bind (("C-c RET" . restclient-http-send-current))
  :hook (restclient-mode . (lambda()
                             (setq-local tab-width 2))))

;; electric indent mode indents both the new line and the previous line when
;; you press enter. Indent only the new line.
(use-package prog-mode
  :init
  (electric-indent-mode 0)
  :bind ("RET" . 'electric-newline-and-maybe-indent)
  :hook (prog-mode . (lambda()
                       (setq-local tab-width 2))))

;; EasyPG
(use-package epa-file
  :init
  (setq epa-pinentry-mode 'loopback)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package flymake
  :bind (("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f b" . flymake-goto-prev-error)))

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

(defun duplicate-line()
  "Duplicate the line at point."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; Minor modes

;; my key bindings
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-c p") 'my-move-line-up)
    (define-key map (kbd "C-c n") 'my-move-line-down)
    (define-key map (kbd "C-c j") 'duplicate-line)
    (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
    (define-key map (kbd "C-l") 'goto-line)
    (define-key map (kbd "<up>") 'scroll-down-line)
    (define-key map (kbd "<down>") 'scroll-up-line)
    (define-key map (kbd "C-c C-n") 'mc/mark-more-like-this-extended)
    (define-key map (kbd "C-c C-p") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-l") 'mc/edit-lines)
    (define-key map (kbd "C-c =") 'mc/mark-all-like-this)
    (define-key map (kbd "C-c SPC") 'er/expand-region)
    (define-key map (kbd "C-c C-SPC") 'set-rectangular-region-anchor)
    (define-key map (kbd "C-c C-s") 'isearch-forward-symbol-at-point)
    (define-key map (kbd "C-c /") 'completion-at-point)
    (define-key map (kbd "M-/") 'dabbrev-completion)
    (define-key map (kbd "M-.") 'xref-find-definitions)
    (define-key map (kbd "M-?") 'xref-find-references)
    (define-key map (kbd "M-,") 'xref-pop-marker-stack)
    (define-key map (kbd "C-c C-j") 'ace-jump-mode)
    (define-key map (kbd "C-c C-o") 'helm-occur)
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
 '(org-agenda-files
   '("~/notes/sencap.org.gpg" "/Users/tyler/notes/ebcs.org.gpg" "/Users/tyler/notes/pittbos.org.gpg"))
 '(package-selected-packages
   '(flymake-eslint go-mode use-package tree-sitter-langs tree-sitter tide expand-region typescript-mode projectile terraform-mode json-mode flycheck web-mode seq pkg-info multiple-cursors let-alist dash))
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
