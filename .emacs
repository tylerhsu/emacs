;;; .emacs --- Tyler's emacs config

;;; Commentary:
;; Tyler's Emacs config

;;; Code:

(setq byte-compile-warnings '(cl-functions))

;; load MELPA packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package web-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode web-mode "Typescript[TSX]")
  :mode ("\\.tsx?\\'" . typescript-tsx-mode)
  :hook (web-mode . (lambda()
                      (setq web-mode-markup-indent-offset 2)
                      (setq web-mode-code-indent-offset 2)
                      (setq standard-indent 2)
                      (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
                      (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
                      (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
                      (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
                      (setq web-mode-enable-current-element-highlight t)
                      (setq web-mode-enable-current-column-highlight t)
                      ;; subword mode - stop at camelcase word boundaries
                      (subword-mode)
                      ))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx")
                                       ("javascript"  . "[jt]s")))
  :commands web-mode)

;; Installed servers (M-x lsp-install-server RET <server> RET):
;; ts-ls, eslint, dockerfile-ls, css-ls
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp)
         (typescript-tsx-mode . lsp)
         (js-mode . lsp)
         (css-mode . lsp)
         (dockerfile-mode . lsp))
  :bind (("M-." . lsp-ui-peek-find-definitions)
         ("M-?" . lsp-ui-peek-find-references))
  :config
  ;; performance suggestions from https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :bind ("M-/" . company-complete)
  :hook (lsp-mode . company-mode))

(use-package flycheck
  :ensure t
  :config
  ; enable flycheck in all buffers where checking is possible
  (add-hook 'after-init-hook #'global-flycheck-mode))

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

(use-package rjsx-mode
  :ensure t
  :mode ("\\.m?jsx?\\'" . js-jsx-mode)
  
  :init
  (advice-add 'js-jsx-enable :override #'my-js-jsx-enable)
  (defun my-js-jsx-enable ()
    "Use `rjsx-mode' instead of `js-jsx-mode'."
    (cl-letf (((symbol-function 'js-jsx--detect-and-enable) (lambda () t)))
      (rjsx-mode)))
  ;; Prevent the `rjsx-mode' ancestor `js-mode' from continuing to check
  ;; for JSX code.
  :hook (rjsx-mode . (lambda ()
                       (local-unset-key (kbd "C-c C-o"))
                       (subword-mode)
                       (remove-hook 'after-change-functions
                                    #'js-jsx--detect-after-change t))))


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
    (define-key map (kbd "C-c C-c") 'er/expand-region)
    (define-key map (kbd "C-c C-SPC") 'set-rectangular-region-anchor)
    (define-key map (kbd "C-c C-s") 'isearch-forward-symbol-at-point)
    (define-key map (kbd "M-.") 'lsp-ui-peek-find-definitions)
    (define-key map (kbd "M-?") 'lsp-ui-peek-find-references)
    (define-key map [f3] 'kill-buffer)
    (define-key map [f4] 'linum-mode)
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
(if (eq system-type 'windows-nt)
    (defvar autosave-dir
      (concat (getenv "emacs_dir") "/emacs-autosaves/"))
  (defvar autosave-dir
    (concat "~/.emacs-autosaves/")))

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
 '(flycheck-delimited-error ((t (:background "color-52"))))
 '(header-line ((t (:background "color-235" :inverse-video nil :underline t))))
 '(helm-ff-directory ((t (:foreground "color-25"))))
 '(helm-ff-dotted-directory ((t (:foreground "brightblack"))))
 '(helm-selection ((t (:inverse-video t))))
 '(linum ((t (:inherit default :foreground "brightblack"))))
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
 '(company-idle-delay nil)
 '(css-indent-offset 2)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*.+\\*"))
 '(helm-display-header-line t)
 '(helm-recentf-fuzzy-match t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(linum-format "%d ")
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
 '(lsp-ui-sideline-show-diagnostics t)
 '(package-selected-packages
   '(use-package tree-sitter-langs tree-sitter tide expand-region typescript-mode projectile terraform-mode json-mode flycheck web-mode seq pkg-info multiple-cursors let-alist dash))
 '(safe-local-variable-values
   '((require-final-newline nil)
     (mode-require-final-newline nil)
     (content-type . "jsx")
     (web-mode-content-type . "jsx")))
 '(typescript-indent-level 2)
 '(web-mode-comment-formats
   '(("java" . "/*")
     ("javascript" . "//")
     ("typescript" . "//")
     ("php" . "/*")
     ("css" . "/*"))))
