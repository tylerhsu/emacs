;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;EXTERNAL MODULES;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set load path depending on OS.
(if (eq system-type 'windows-nt)
    (defvar elisp-dir
      (concat (getenv "emacs_dir") "/lisp/"))
  (defvar elisp-dir
    (concat "~/.emacs.d/")))

(add-to-list 'load-path elisp-dir)

;;MAJOR MODES
; JS2 mode
(autoload 'js2-mode "js2-mode" nil t)

; Mustache
(autoload 'mustache-mode "mustache-mode" nil t)

; VB6
(autoload 'visual-basic-mode "visual-basic-mode" nil t)

; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

; nxhtml
(autoload 'nxhtml-mode "nxhtml/autostart" nil t)
(autoload 'django-nxhtml-mumamo-mode "nxhtml/autostart")
(eval-after-load 'nxhtml-mode '(nxhtml-toggle-visible-warnings))

; actionscript
(autoload 'actionscript-mode "actionscript-mode" nil t)

; set major mode file associations (in addition to defaults)
(setq auto-mode-alist
  (append '(("\\.php$" . php-mode)
            ("\\.inc$" . php-mode)
            ("\\.js.erb$" . eruby-javascript-mumamo-mode)
            ("\\.as$" . actionscript-mode)
            ("\\.html$" . nxhtml-mode)
            ("\\.html.erb$" . eruby-html-mumamo-mode)
            ("\\.djhtml$" . django-nxhtml-mumamo-mode)
            ("\\.mustache$" . mustache-mode)
            ("\\.js$" . js2-mode)
            ("\\.\\(frm\\|bas\\|cls\\)$" . visual-basic-mode)
            ) auto-mode-alist))

;;OTHER MODULES
;Cedet
(load-file (concat elisp-dir "cedet-1.0.1/common/cedet.el"))
(semantic-load-enable-code-helpers)

;Emacs Code Browser (ECB)
(add-to-list 'load-path (concat elisp-dir "ecb-2.40"))
(require 'ecb-autoloads)
;inhibit startup messages
(setq ecb-tip-of-the-day nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ecb-layout-name "left2")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote (("/" "/") ("/dev-thsu01.chicago.mintel.ad:~/local" "dev-thsu01/local"))))
 '(ecb-windows-width 0.2)
 '(js2-bounce-indent-p t)
 '(majmodpri-sort-after-load nil)
 '(mumamo-chunk-coloring 1)
 '(nxhtml-skip-welcome t)
 '(scroll-bar-mode (quote right))
 '(speedbar-before-visiting-file-hook nil)
 '(speedbar-directory-button-trim-method (quote span))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 25) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-query-confirmation-method (quote none-but-delete))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(sr-speedbar-max-width 24)
 '(sr-speedbar-right-side nil))

;YASnippet (code completion)
(add-to-list 'load-path (concat elisp-dir "yasnippet"))
(require 'yasnippet)
(setq yas/snippet-dirs (concat elisp-dir "yasnippet/snippets"))
(setq yas/indent-line 'fixed)
(yas/initialize)

;tabbar (enables top tabs)
(require 'tabbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;DISPLAY;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default font
(set-default-font "Inconsolata")

;; remove toolbar, menu bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; show column number
(setq column-number-mode t)

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
 :foreground "gray"
 :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
 :height 0.7)

; set default window dimensions
(if (window-system)
    (set-frame-size (selected-frame) 81 65))

; show system name and buffer's full path
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;KEY BINDINGS;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-p") 'my-move-line-up)
(global-set-key (kbd "C-M-n") 'my-move-line-down)
(global-set-key (kbd "C-j") 'duplicate-line)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-.") 'tabbar-forward-tab)
(global-set-key (kbd "C-,") 'tabbar-backward-tab)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-c C-x r") 'rename-file-and-buffer)
(global-set-key (kbd "C-c C-x m") 'move-buffer-file)
(global-set-key (kbd "C-c , f") 'semantic-ia-fast-jump)
(global-set-key (kbd "M-.") 'tabbar-forward-group)
(global-set-key (kbd "M-,") 'tabbar-backward-group)
(global-set-key (kbd "M-k") 'my-kill-line)
(global-set-key [f3] 'kill-buffer)
(global-set-key [f4] 'linum-mode)
;(global-set-key [f5] 'my-majmodpri-apply)
(global-set-key [f8] 'goto-line)
;(global-set-key [f9] 'speedbar)
(global-set-key [f9] 'ecb-minor-mode)
(global-set-key [f12] 'compile)

(if (eq system-type 'gnu/linux)
    (global-set-key [f11] 'gnome-fullscreen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;CUSTOM FUNCTIONS;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gnome-fullscreen ()
  "Active fullscreen mode under X-window"
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun my-majmodpri-apply ()
  (interactive)
  (majmodpri-apply))

(defun my-move-line-up() 
  "Move the current line up one."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (next-line)
      (transpose-lines -1))
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
  (yank)
)

(defun my-kill-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (indent-for-tab-command)
)

(defun previous-error (n)
  "Visit previous compilation error message and corresponding source code."
  (interactive "p")
  (next-error (- n)))

(defun my-ruby-newline()
  (interactive)
  (open-line 2)
  (next-line 2)
  (indent-for-tab-command)
  (delete-backward-char 2)
  (insert "end")
  (previous-line 1)
  (indent-for-tab-command))

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
(defun my-php-mode-hook ()
  (define-key php-mode-map (kbd "C-.") 'tabbar-forward-tab)
  (define-key php-mode-map (kbd "C-,") 'tabbar-backward-tab)
  (define-key php-mode-map (kbd "M-.") 'tabbar-forward-group)
  (define-key php-mode-map (kbd "M-,") 'tabbar-backward-group))

; Ruby mode hook
(defun my-ruby-mode-hook ()
  (define-key ruby-mode-map (kbd "C-j") 'duplicate-line)
  (define-key ruby-mode-map (kbd "C-M-p") 'my-move-line-up)
  (define-key ruby-mode-map (kbd "C-M-n") 'my-move-line-down)
  (define-key ruby-mode-map (kbd "C-<return>") 'my-ruby-newline))

(defun my-js-mode-hook ()
  (setq yas/mode-symbol 'javascript-mode)
  (setq javascript-indent-level 2))

(defun my-actionscript-mode-hook ()
  (setq standard-indent 2)
  (define-key actionscript-mode-map (kbd "C-c g") 'my-as3-getter)
  (define-key actionscript-mode-map (kbd "C-c s") 'my-as3-setter)
  (define-key actionscript-mode-map (kbd "C-c x") 'my-as3-getter-setter)
  (define-key actionscript-mode-map (kbd "C-c f") 'my-as3-import-graphical-asset)
  (define-key actionscript-mode-map (kbd "C-c a") 'as3-code-assets))

(defun my-nxhtml-mode-hook ()
  (define-key nxhtml-mode-map (kbd "C-M-p") 'my-move-line-up)
  (define-key nxhtml-mode-map (kbd "C-M-n") 'my-move-line-down))

(defun my-html-mode-hook ()
  (set (make-local-variable 'indent-line-function) 'nxml-indent-line))

(defun my-django-nxhtml-mumamo-mode-hook ()
  (setq yas/mode-symbol 'django-html-mode))

(defun my-snippet-mode-hook ()
  (setq require-final-newline nil))

(defun my-text-mode-hook ()
  ;; Set 4-space indentation
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
  (define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop))

(defun my-emacs-startup-hook ()
  (linum-mode 0))

; Add hooks
(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'js-mode-hook 'my-js-mode-hook)
(add-hook 'actionscript-mode-hook 'my-actionscript-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'nxhtml-mode-hook 'my-nxhtml-mode-hook)
(add-hook 'html-mode-hook 'my-html-mode-hook)
(add-hook 'django-nxhtml-mumamo-mode-hook 'my-django-nxhtml-mumamo-mode-hook)
(add-hook 'snippet-mode-hook 'my-snippet-mode-hook)
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;MISCELLANEOUS;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mumamo is making emacs 23.3 freak out:
;; (http://stackoverflow.com/a/5470584)
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

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

; make copying/pasting work like it should.  Sort of.
(load "copypaste.el")

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
 '(mumamo-background-chunk-submode1 ((default nil) (nil (:background "#111111"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green3"))))
 '(speedbar-directory-face ((default nil) (nil (:foreground "#4400cc" :height 0.8))))
 '(speedbar-file-face ((default nil) (nil (:foreground "light blue" :height 0.8))))
 '(speedbar-selected-face ((default nil) (nil (:foreground "red" :underline t :height 0.8)))))

(put 'downcase-region 'disabled nil)