;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;EXTERNAL MODULES;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set load path depending on OS.
(if (eq system-type 'windows-nt)
    (defvar elisp-dir
      (concat (getenv "emacs_dir") "/lisp/"))
  (defvar elisp-dir
    (concat "~/.emacs.d/lisp")))

(add-to-list 'load-path elisp-dir)

; load MELPA packages
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
(package-initialize)

; Automatically revert buffers when they change on disk
(global-auto-revert-mode 1)

; Set default tab width
(setq default-tab-width 4)

;;MAJOR MODES

; Mustache
(autoload 'mustache-mode "mustache-mode" nil t)

; VB6
(autoload 'visual-basic-mode "visual-basic-mode" nil t)

; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

; actionscript
(autoload 'actionscript-mode "actionscript-mode" nil t)

; web mode
(autoload 'web-mode "web-mode" nil t)

; yaml mode
(autoload 'yaml-mode "yaml-mode" nil t)

; jade mode
(autoload 'jade-mode "jade-mode" nil t)

; set major mode file associations (in addition to defaults)
(setq auto-mode-alist
  (append '(("\\.php$" . php-mode)
            ("\\.inc$" . php-mode)
            ("\\.js.erb$" . web-mode)
            ("\\.as$" . actionscript-mode)
            ("\\.html$" . web-mode)
            ("\\.html.erb$" . web-mode)
            ("\\.djhtml$" . web-mode)
            ("\\.mustache$" . web-mode)
            ("\\.js$" . web-mode)
            ("\\.ts$" . web-mode)
            ("\\.mjs$" . web-mode)
            ("\\.jsx$" . web-mode)
            ("\\.tsx$" . web-mode)
            ("\\.\\(frm\\|bas\\|cls\\)$" . visual-basic-mode)
            ("\\.pt$" . web-mode)
            ("\\.yml$" . yaml-mode)
            ("\\.jade$" . jade-mode)
            ("\\.pug$" . jade-mode)
            ("\\.css$" . css-mode)
            ("\\.scss$" . css-mode)
            ) auto-mode-alist))

; tell web-mode to use the "jsx" content type for all .js files under the 'harvest' directory
(setq web-mode-content-types-alist
      '(("jsx"  . ".*/harvest/.*\\.js[x]?$")))

;;OTHER MODULES

;tabbar (enables top tabs)
(require 'tabbar)

;uniquify buffer names by appending part of the full path after them
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; enable multiple cursors
(require 'multiple-cursors)

; enable expand-region
(require 'expand-region)

; org-mode Markdown export
(require 'ox-md)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;DISPLAY;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default font
(set-default-font "Inconsolata 12")

;; remove toolbar, menu bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(menu-bar-mode 0)

;; colors
(set-background-color "#000000")
(set-cursor-color "#ff0000")
(set-foreground-color "#ffffff")

;; display buffer tab bar
(tabbar-mode)
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; tab bar display settings
(set-face-attribute
 'tabbar-default-face nil
 :background "gray20")
(set-face-attribute
 'tabbar-unselected-face nil
 :background "gray30"
 :foreground "gray"
 :box nil)
(set-face-attribute
 'tabbar-selected-face nil
 :background "black"
 :foreground "yellow"
 :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
 :height 0.7)

; show system name and buffer's full path
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;CUSTOM FUNCTIONS;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-majmodpri-apply ()
  (interactive)
  (majmodpri-apply))

(defun my-move-line-up() 
  "Move the current line up one."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (next-line)
      (transpose-lines -1))
    (previous-line)
    (if (> col 0)
        (previous-line))
    (move-to-column col)))

(defun my-move-line-down() 
  "Move the current line down one."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun my-kill-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (indent-for-tab-command))

(defun previous-error (n)
  "Visit previous compilation error message and corresponding source code."
  (interactive "p")
  (next-error (- n)))

(defun tabbar-buffer-groups (buffer)
   "Group buffers into 4 tabbar groups: nxhtml indent, emacs, dired, and user buffers."
   (with-current-buffer (get-buffer buffer)
     (cond
      ((string-match-p "^.+\.html-template-indent-buffer$" (buffer-name))
       '("nxhtml Indent Buffer"))
      ((string-equal "*" (substring (buffer-name) 0 1))
       '("Emacs Buffer"))
      ((eq major-mode 'dired-mode)
       '("Dired"))
      (t
       '("User Buffer"))
      )))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
    (filename (buffer-file-name))
    (dir
    (if (string-match dir "\\(?:/\\|\\\\)$")
      (substring dir 0 -1) dir))
      (newname (concat dir "/" name)))
    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;MODE HOOKS;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-actionscript-mode-hook ()
  (setq standard-indent 2)
  (define-key actionscript-mode-map (kbd "C-c g") 'my-as3-getter)
  (define-key actionscript-mode-map (kbd "C-c s") 'my-as3-setter)
  (define-key actionscript-mode-map (kbd "C-c x") 'my-as3-getter-setter)
  (define-key actionscript-mode-map (kbd "C-c f") 'my-as3-import-graphical-asset)
  (define-key actionscript-mode-map (kbd "C-c a") 'as3-code-assets))

;; (defun my-text-mode-hook ()
;;   ;; Set 4-space indentation
;;   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;;   (define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop))

(defun my-emacs-startup-hook ()
  (linum-mode 0))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ; subword mode - stop at camelcase word boundaries
  (subword-mode 1)
  ; hideshow mode - code folding
  (hs-minor-mode 1)
  )

(defun my-json-mode-hook ()
  (setq js-indent-level 2))

; Add hooks
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'actionscript-mode-hook 'my-actionscript-mode-hook)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'snippet-mode-hook 'my-snippet-mode-hook)
(add-hook 'json-mode-hook 'my-json-mode-hook)
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;MINOR MODES;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my key bindings
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-c p") 'my-move-line-up)
    (define-key map (kbd "C-c n") 'my-move-line-down)
    (define-key map (kbd "C-c j") 'duplicate-line)
    (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
    (define-key map (kbd "C-c ]") 'tabbar-forward-tab)
    (define-key map (kbd "C-c [") 'tabbar-backward-tab)
    (define-key map (kbd "C-c }") 'tabbar-forward-group)
    (define-key map (kbd "C-c {") 'tabbar-backward-group)
    (define-key map (kbd "C-l") 'goto-line)
    (define-key map (kbd "C-c r") 'rename-file-and-buffer)
    (define-key map (kbd "C-c m") 'move-buffer-file)
    (define-key map (kbd "C-c , f") 'semantic-ia-fast-jump)
    (define-key map (kbd "C-c k") 'my-kill-line)
    (define-key map (kbd "<up>") 'scroll-down-line)
    (define-key map (kbd "<down>") 'scroll-up-line)
    (define-key map (kbd "C-c C-n") 'mc/mark-more-like-this-extended)
    (define-key map (kbd "C-c C-p") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-l") 'mc/edit-lines)
    (define-key map (kbd "C-c =") 'mc/mark-all-like-this)
    (define-key map (kbd "C-c C-c") 'er/expand-region)
    (define-key map (kbd "C-c C-SPC") 'set-rectangular-region-anchor)
    (define-key map (kbd "C-c C-r") 'recentf-open-files)
    (define-key map (kbd "C-c s") 'isearch-forward-symbol-at-point)
    (define-key map [f3] 'kill-buffer)
    (define-key map [f4] 'linum-mode)
    (define-key map [f7] 'tabbar-backward-tab)
    (define-key map [f8] 'tabbar-forward-tab)
    ;(define-key map [f9] 'speedbar)
    (define-key map [f12] 'compile)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; recent files
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;MISCELLANEOUS;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TRAMP default method
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
             '("\\." "\\`mp-janitor\\'" "/ssh:%h:"))

;; save everything when saving a desktop -- including tramp buffers.
(setq desktop-files-not-to-save "^$")

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(if (eq system-type 'windows-nt)
    (defvar autosave-dir
      (concat (getenv "emacs_dir") "/emacs-autosaves/"))
  (defvar autosave-dir
    (concat "~/.emacs-autosaves/")))

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

; make text-mode the default major mode
(setq default-major-mode 'text-mode)

;;turn on auto-fill mode by default       
;(setq-default auto-fill-function 'do-auto-fill)

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
 '(linum ((t (:inherit default :foreground "brightblack"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((default nil) (nil (:background "#111111"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green3"))))
 '(speedbar-directory-face ((default nil) (nil (:foreground "#4400cc" :height 0.8))))
 '(speedbar-file-face ((default nil) (nil (:foreground "light blue" :height 0.8))))
 '(speedbar-selected-face ((default nil) (nil (:foreground "red" :underline t :height 0.8))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightblack")))))

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-format "%d ")
 '(package-selected-packages
   (quote
    (terraform-mode json-mode flycheck web-mode seq pkg-info multiple-cursors let-alist js2-mode expand-region dash))))
