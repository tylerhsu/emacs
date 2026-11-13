;;; Tyler's emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Tyler's Emacs config

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; GUI Emacs and the daemon are launched by launchd, not a shell, so they get a minimal
;; PATH. Import PATH (and exec-path) from the login shell.
(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

;; Emacs 30's built-in eglot (1.17) never asks servers for diagnostics, and TypeScript 7's
;; server only gives them when asked, so TS files would show no errors at all. `:ensure t'
;; counts the built-in as installed, so upgrade it from GNU ELPA explicitly.
(unless (package-installed-p 'eglot '(1 26))
  (unless package-archive-contents (package-refresh-contents))
  (let ((package-install-upgrade-built-in t))
    (package-install 'eglot)))

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

;; JS/TS language server, always using the project's own TypeScript (never a global one):
;; - TypeScript < 7: typescript-language-server, pointed explicitly at the project's tsserver.js.
;; - TypeScript >= 7 (the Go rewrite, which has no tsserver): its own server, `tsc --lsp --stdio`,
;;   from the project's native binary package (e.g. @typescript/typescript-darwin-arm64).
;; - No TypeScript: in a .ts/.tsx buffer, ask whether to install it in the project; JS buffers
;;   just get no server. If TypeScript 7 is there but without this machine's binary (node_modules
;;   installed in a Linux container), ask whether to download the matching binary.

(defvar my/typescript-declined nil
  "Directories where I said no to installing TypeScript or its binary (asked once per session).")

(defun my/typescript-version (root)
  "Version string of ROOT/node_modules/typescript, or nil if it isn't installed."
  (let ((pkg (expand-file-name "node_modules/typescript/package.json" root)))
    (when (file-readable-p pkg)
      (with-temp-buffer
        (insert-file-contents pkg)
        (gethash "version" (json-parse-buffer))))))

(defun my/typescript-native-package ()
  "TypeScript 7's binary package for this machine, e.g. \"@typescript/typescript-darwin-arm64\"."
  (format "@typescript/typescript-%s-%s"
          (if (eq system-type 'darwin) "darwin" "linux")
          (if (string-prefix-p "aarch64" system-configuration) "arm64" "x64")))

(defun my/typescript-run (dir program &rest args)
  "Run PROGRAM with ARGS in DIR, showing its output and signaling an error if it fails."
  (unless (executable-find program)
    (user-error "Can't find `%s' to install TypeScript with" program))
  (let ((buf (get-buffer-create "*typescript install*"))
        (default-directory (file-name-as-directory dir)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ cd %s && %s\n" dir (string-join (cons program args) " "))))
    (message "Running %s %s..." program (string-join args " "))
    (unless (eql 0 (apply #'process-file program nil buf t args))
      (display-buffer buf)
      (user-error "`%s %s' failed; see *typescript install*" program (string-join args " ")))))

(defun my/typescript-ask (dir prompt)
  "Ask PROMPT once per session for DIR; a no is remembered and signals an error."
  (when (or (member dir my/typescript-declined)
            (not (y-or-n-p prompt)))
    (add-to-list 'my/typescript-declined dir)
    (user-error "No project TypeScript for %s; not starting a language server" dir)))

(defun my/typescript-install (project)
  "Offer to install TypeScript in the current buffer's project; return the directory it's in."
  (let ((dir (abbreviate-file-name
              (or (locate-dominating-file default-directory "package.json")
                  (and project (project-root project))
                  default-directory))))
    (unless (and buffer-file-name (string-match-p "\\.[cm]?tsx?\\'" buffer-file-name))
      (user-error "No project TypeScript for %s; not starting a language server" dir))
    (let ((command (cond ((or (file-exists-p (expand-file-name "bun.lock" dir))
                              (file-exists-p (expand-file-name "bun.lockb" dir)))
                          '("bun" "add" "--dev" "typescript"))
                         ((file-exists-p (expand-file-name "pnpm-lock.yaml" dir))
                          '("pnpm" "add" "--save-dev" "typescript"))
                         ((file-exists-p (expand-file-name "yarn.lock" dir))
                          '("yarn" "add" "--dev" "typescript"))
                         (t '("npm" "install" "--save-dev" "typescript")))))
      (my/typescript-ask dir (format "No TypeScript found for %s. Install it there (%s)? "
                                     dir (string-join command " ")))
      (apply #'my/typescript-run dir command)
      (unless (my/typescript-version dir)
        (user-error "Installed, but %snode_modules/typescript still isn't there" dir))
      dir)))

(defun my/typescript-fetch-native (root version)
  "Offer to download TypeScript VERSION's binary for this machine into ROOT/node_modules."
  (let* ((package (my/typescript-native-package))
         (spec (format "%s@%s" package version))
         (dest (expand-file-name (concat "node_modules/" package) root))
         (tmp (make-temp-file "typescript-native" t)))
    (my/typescript-ask root (format "TypeScript %s in %s has no binary for this machine (node_modules \
installed elsewhere, e.g. in a container?). Download %s into its node_modules? " version root spec))
    (unwind-protect
        (progn
          (my/typescript-run tmp "npm" "pack" "--silent" spec)
          (make-directory dest t)
          (my/typescript-run dest "tar" "-xzf" (car (directory-files tmp t "\\.tgz\\'"))
                             "--strip-components=1"))
      (delete-directory tmp t))))

(defun my/typescript-server-contact (&optional _interactive project)
  "Eglot server command for JS/TS, using the project's own TypeScript (see the comment above)."
  (let* ((root (abbreviate-file-name
                (or (locate-dominating-file default-directory "node_modules/typescript/package.json")
                    (my/typescript-install project))))
         (version (my/typescript-version root)))
    (if (< (string-to-number version) 7)
        `("typescript-language-server" "--stdio"
          :initializationOptions
          (:tsserver (:path ,(expand-file-name "node_modules/typescript/lib/tsserver.js" root))))
      (let ((tsc (expand-file-name (concat "node_modules/" (my/typescript-native-package) "/lib/tsc")
                                   root)))
        (unless (file-executable-p tsc)
          (my/typescript-fetch-native root version))
        (list tsc "--lsp" "--stdio")))))

(use-package eglot
  :ensure t
  :config
  (setopt eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs '(vue-ts-mode . ("vls" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; Picks the server per project; see my/typescript-server-contact.
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 . my/typescript-server-contact))
  :hook ((typescript-ts-mode tsx-ts-mode python-ts-mode go-ts-mode svelte-mode vue-ts-mode js-ts-mode) . #'eglot-ensure))

(use-package flymake
  :init
  (defvar-keymap tyler/flymake-prefix-map
    :doc "Keymap for flymake commands, on C-c f and in my-command-mode."
    "l" #'flymake-show-project-diagnostics
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)
  (bind-key* "C-c f" tyler/flymake-prefix-map))

(use-package project
  :config
  ;; Files that signal a project root. Eglot uses project to find the project root, then the language server starts looking for configs like tsconfig.json from that root.
  ;; Without this, eglot only sets up correctly when the project is a git repo.
  (add-to-list 'project-vc-extra-root-markers "package.json")
  (add-to-list 'project-vc-extra-root-markers "tsconfig.json"))

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
  :vc (:url "https://github.com/8uff3r/vue-ts-mode" :branch "main")
  :mode "\\.vue\\'"
  :hook (vue-ts-mode . (lambda()
                         ;; vue-ts-mode does not derive from prog-mode, so prog-mode hook is copied here.

                         ;; electric indent mode indents both the new line and the previous line when
                         ;; you press enter. Indent only the new line.
                         ;; (electric-indent-mode 0)
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
  (bind-key* "C-c C-n" 'mc/mark-more-like-this-extended)
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
  :init
  ;; Open xref stuff in a consult minibuffer instead of an *xref* buffer.
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-xrefs-definitions #'consult-xref)
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
  ;; :bind ("RET" . 'electric-newline-and-maybe-indent)
  :hook (prog-mode . (lambda()
                       ;; electric indent mode indents both the new line and the previous line when
                       ;; you press enter. Indent only the new line.
                       ;; (electric-indent-mode 0)
                       ;; Auto-insert closing brackets
                       ;; (electric-pair-mode 1)
                       ;; Enable popup autocomplete
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

;; (use-package electric
;;   :config
;;   (defun tyler/electric-pair-indent-closing-brace ()
;;     "Indents the next line if it contains a closing delimiter.
;; Only runs if `electric-pair-mode' is enabled."
;;     (when (and electric-pair-mode
;;                (eq last-command-event ?\n))
;;       (save-excursion
;;         (forward-line 1)
;;         (skip-chars-forward " \t")
;;         (when (eq (char-syntax (char-after)) ?\))
;;           (indent-according-to-mode)))))

;;   (add-hook 'post-self-insert-hook #'my/electric-pair-indent-closing-brace))

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
  :config
  (bind-key* "C-j" (lambda () (interactive) (my-command-mode-all 1)))
  ;; move
  (define-key my-command-mode-map (kbd "n") #'next-line)
  (define-key my-command-mode-map (kbd "p") #'previous-line)
  (define-key my-command-mode-map (kbd "f") #'forward-word)
  (define-key my-command-mode-map (kbd "b") #'backward-word)
  (define-key my-command-mode-map (kbd "j") #'forward-sexp)
  (define-key my-command-mode-map (kbd "h") #'backward-sexp)
  (define-key my-command-mode-map (kbd "a") #'beginning-of-line)
  (define-key my-command-mode-map (kbd "m") #'back-to-indentation)
  (define-key my-command-mode-map (kbd "e") #'end-of-line)
  (define-key my-command-mode-map (kbd "d") #'down-list)
  (define-key my-command-mode-map (kbd "u") #'up-list)
  (define-key my-command-mode-map (kbd "ga") #'beginning-of-defun)
  (define-key my-command-mode-map (kbd "ge") #'end-of-defun)
  (define-key my-command-mode-map (kbd ",") (tyler/repeat-command #'xref-go-back #'xref-go-forward))
  (define-key my-command-mode-map (kbd ".") #'xref-find-definitions)
  (define-key my-command-mode-map (kbd "g.") #'xref-find-apropos)
  (define-key my-command-mode-map (kbd "?") #'xref-find-references)
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
  (define-key my-command-mode-map (kbd "g9") #'kmacro-start-macro)
  (define-key my-command-mode-map (kbd "g0") #'kmacro-end-or-call-macro)
  ;; mark
  (define-key my-command-mode-map (kbd "SPC") #'set-mark-command)
  (define-key my-command-mode-map (kbd "g SPC") #'set-rectangular-region-anchor)
  (define-key my-command-mode-map (kbd "'") #'mc/mark-more-like-this-extended)
  ;; insert
  (define-key my-command-mode-map (kbd "i") (lambda () (interactive) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "P") (lambda () (interactive) (beginning-of-line) (open-line 1) (indent-for-tab-command) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "N") (lambda () (interactive) (end-of-line) (newline) (indent-for-tab-command) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "E") (lambda () (interactive) (end-of-line) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "A") (lambda () (interactive) (beginning-of-line) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "M") (lambda () (interactive) (back-to-indentation) (my-command-mode-all -1)))
  (define-key my-command-mode-map (kbd "O") (lambda () (interactive) (overwrite-mode) (my-command-mode-all -1)))
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
  (define-key my-command-mode-map (kbd "kk") #'kill-region)
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
  (define-key my-command-mode-map (kbd "cc") #'kill-ring-save)
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
  (define-key my-command-mode-map (kbd "x0") #'delete-window)
  (define-key my-command-mode-map (kbd "xo") #'other-window)
  ;;; open ghostel buffer
  (define-key my-command-mode-map (kbd "`") #'ghostel-project)
  ;; prefixes into other keymaps
  (define-key my-command-mode-map (kbd "x TAB") 'tabspaces-command-map)
  (define-key my-command-mode-map (kbd "xn") tyler/flymake-prefix-map)
  (define-key my-command-mode-map (kbd "xp") project-prefix-map))

;; Custom functions
(defun tyler/open-line-between-delimiters ()
  "Open an indented line between matching delimiters.
Point should follow one or more opening delimiters (e.g. {, ([).
Inserts a newline, indents point, then inserts the matching
closing delimiters in reverse order on the next line at the
appropriate indentation."
  (interactive)
  (let ((close-chars '()))
    (save-excursion
      (while (let ((c (char-before)))
               (and c (matching-paren c)))
        (setq close-chars (append close-chars (list (matching-paren (char-before)))))
        (backward-char)))
    (unless close-chars
      (user-error "No opening delimiters before point"))
    (newline)
    (save-excursion
      (newline)
      (insert (apply #'string close-chars))
      (indent-according-to-mode))
    (indent-according-to-mode)))

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

(defun tyler/mark-whole-line (arg)
  "Mark the current line starting from indentation. With prefix, mark current line and that many lines from current."
  (interactive "P")
  (let* ((numeric-prefix (prefix-numeric-value arg))
         (beginning (line-beginning-position (when (< numeric-prefix 0) (+ numeric-prefix 1))))
         (end (line-end-position (when
                                     (and
                                      (not (null arg))
                                      (> numeric-prefix 0))
                                   (+ 1 numeric-prefix)))))
    (goto-char beginning)
    (push-mark end nil t)))

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

(defun tyler/terminal-key-or (command)
  "Return a command that runs COMMAND, except in a ghostel terminal taking input.
There the key goes to the terminal, as ghostel binds it, so <up> and <down>
browse shell history.  The keys are bound with `bind-key*', which would
otherwise win over ghostel's bindings."
  (lambda ()
    (interactive)
    (if (and (derived-mode-p 'ghostel-mode)
             (memq (bound-and-true-p ghostel--input-mode) '(semi-char char)))
        (call-interactively #'ghostel--send-event)
      (call-interactively command))))

;; A tab per project (tabspaces), and terminals in ghostel.
(use-package ghostel
  :ensure t
  :custom
  ;; Fetch the prebuilt native module on first use instead of asking.
  (ghostel-module-auto-install 'download)
  ;; Inside ghostel, $EDITOR opens files in this Emacs (via the server below)
  ;; instead of a nested `emacs -nw'.  C-x # finishes the edit.  Uses the
  ;; emacsclient that ships with the running Emacs.
  (ghostel-environment
   (let ((client (or (locate-file "emacsclient"
                                  (list invocation-directory
                                        (expand-file-name "bin" invocation-directory)))
                     "emacsclient")))
     (list (concat "EDITOR=" client)
           (concat "VISUAL=" client))))
  :config
  ;; ghostel resets `cursor-type' to the terminal's requested shape (a box by
  ;; default) after every redraw, undoing my-command-mode's cursor.  Reapply
  ;; it afterwards, but keep the cursor hidden when a program hides it.
  (defun tyler/ghostel-command-mode-cursor (&rest _)
    (when (and cursor-type (fboundp 'my-command-mode-current-cursor-type))
      (setq cursor-type (my-command-mode-current-cursor-type))))
  (advice-add 'ghostel--apply-cursor-style :after
              #'tyler/ghostel-command-mode-cursor)

  ;; Copy mode while my-command-mode is on: the terminal is frozen and
  ;; read-only, so command keys move around and copy its text.  Turning
  ;; my-command-mode off returns to the input mode from before, and leaving
  ;; copy mode (however ghostel does it) turns my-command-mode off.
  (defvar tyler/ghostel--leaving-copy nil
    "The buffer whose copy mode is being left, while that is underway.")

  (defun tyler/ghostel-sync-copy-mode (&optional buffer tries)
    "Put BUFFER in ghostel copy mode exactly while `my-command-mode' is on.
A new buffer turns on `my-command-mode' before its terminal exists, and
copy mode would freeze it blank, so entry waits until the terminal has
drawn something; TRIES counts the attempts."
    (setq buffer (or buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'ghostel-mode)
                   (not (eq tyler/ghostel--leaving-copy buffer)))
          (let ((inhibit-message t))
            (cond
             ((not (bound-and-true-p my-command-mode))
              (when (eq ghostel--input-mode 'copy)
                (ghostel-readonly-exit)))
             ((memq ghostel--input-mode '(copy emacs)))
             ((and ghostel--term
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "[^ \t\n]" nil t)))
              ;; Follows `ghostel-readonly-fast-exit': when it's on, q, C-g,
              ;; yanking, M-w and printable keys my-command-mode doesn't
              ;; bind all exit (and so leave my-command-mode too).
              (ghostel-copy-mode))
             ((< (or tries 0) 40)
              (run-with-timer 0.25 nil #'tyler/ghostel-sync-copy-mode
                              buffer (1+ (or tries 0))))))
          ;; Switching input modes resets `cursor-type'.
          (tyler/ghostel-command-mode-cursor)))))
  (add-hook 'my-command-mode-hook #'tyler/ghostel-sync-copy-mode)

  ;; Every way out of copy (or Emacs) mode runs `ghostel--leave-readonly-state',
  ;; while `ghostel--input-mode' still names the mode being left.
  (defun tyler/ghostel-leave-command-mode-with-copy (orig &rest args)
    "Run ORIG with ARGS; when that leaves copy mode, leave `my-command-mode'."
    (let ((leaving-copy (eq ghostel--input-mode 'copy)))
      (apply orig args)
      (when (and leaving-copy (bound-and-true-p my-command-mode))
        ;; This buffer is already on its way out of copy mode, so the
        ;; my-command-mode hook must not try to exit it again.
        (let ((tyler/ghostel--leaving-copy (current-buffer)))
          (my-command-mode-all -1)))))
  (advice-add 'ghostel--leave-readonly-state :around
              #'tyler/ghostel-leave-command-mode-with-copy)

  ;; New terminals open ready for typing.  my-command-mode turns itself on
  ;; from `after-change-major-mode-hook', which runs after `ghostel-mode-hook',
  ;; so turn it back off from a buffer-local entry appended after the global
  ;; ones.
  (defun tyler/ghostel-start-without-command-mode ()
    "Turn `my-command-mode' off in a new ghostel buffer."
    (add-hook 'after-change-major-mode-hook
              (lambda () (my-command-mode -1))
              t t))
  (add-hook 'ghostel-mode-hook #'tyler/ghostel-start-without-command-mode)

  ;; Switching to a terminal (another buffer in the window, or another window)
  ;; leaves it ready for typing.  Only in that buffer: other buffers keep
  ;; their state.  The minibuffer doesn't count as a switch, so M-x from a
  ;; terminal in command mode comes back to command mode.
  (defvar tyler/ghostel--last-buffer nil
    "The buffer of the selected window when it last changed.")
  (defun tyler/ghostel-command-mode-off-on-switch (&optional _frame)
    "Turn `my-command-mode' off on switching to a ghostel buffer."
    (let ((buffer (window-buffer (selected-window))))
      (unless (or (minibufferp buffer)
                  (eq buffer tyler/ghostel--last-buffer))
        (setq tyler/ghostel--last-buffer buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'ghostel-mode)
                     (bound-and-true-p my-command-mode))
            (my-command-mode -1))))))
  (add-hook 'window-selection-change-functions
            #'tyler/ghostel-command-mode-off-on-switch)
  (add-hook 'window-buffer-change-functions
            #'tyler/ghostel-command-mode-off-on-switch)

  ;; Sexp motion (forward-sexp, backward-sexp, and the kill/copy commands built
  ;; on them) moves by prompt.
  (defun tyler/ghostel-forward-sexp (&optional n)
    "Move forward N prompts, or backward if N is negative."
    (setq n (or n 1))
    (if (< n 0)
        (ghostel-previous-prompt (- n))
      (ghostel-next-prompt n)))
  (defun tyler/ghostel-setup-sexp ()
    (setq-local forward-sexp-function #'tyler/ghostel-forward-sexp))
  (add-hook 'ghostel-mode-hook #'tyler/ghostel-setup-sexp))

;; Let emacsclient (e.g. $EDITOR in ghostel) reach this Emacs.  GUI only, so a
;; terminal Emacs (say, one opened as $EDITOR) doesn't take the server.
(use-package server
  :if (display-graphic-p)
  :config
  (unless (server-running-p) (server-start)))

(use-package ghostel-project
  :after ghostel
  :bind (:map project-prefix-map
              ("m" . ghostel-project)
              ("M" . ghostel-project-list-buffers))
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-kill-buffer-conditions '(derived-mode . ghostel-mode)))

(use-package tabspaces
  :ensure t
  :custom
  ;; C-c TAB o: open a project in its own tab; C-c TAB s: switch tabs.
  ;; nil so C-x b stays `consult-buffer' (with preview); the workspace consult
  ;; source below filters it to this tab's buffers instead.
  (tabspaces-use-filtered-buffers-as-default nil)
  ;; Off until `tyler/load-session' restores the session, so nothing saves before.
  (tabspaces-session nil)
  ;; One session file in ~/.emacs.d, not a session file in every project root.
  (tabspaces-session-project-session-store nil)
  :config
  ;; TAB L (and C-c TAB L): load the saved session.
  (define-key tabspaces-command-map (kbd "L") #'tyler/load-session)
  ;; consult-buffer lists this tab's buffers by default; `b SPC' shows all.
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda ()
                        (consult--buffer-query
                         :predicate #'tabspaces--local-buffer-p
                         :sort 'visibility
                         :as #'buffer-name)))
      "Buffers in the current tabspaces tab, for `consult-buffer'.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

;; The tab bar is hidden, so show the current tabspace's name in the mode line,
;; just before the buffer name: "[ebcs] init.el".
(defvar tyler/mode-line-tabspace
  '(:eval (when tab-bar-mode
            (propertize (format "[%s] " (alist-get 'name (tab-bar--current-tab)))
                        'face 'bold)))
  "Mode line construct showing the current tab's name.")
(put 'tyler/mode-line-tabspace 'risky-local-variable t)
(unless (memq 'tyler/mode-line-tabspace (default-value 'mode-line-format))
  (let* ((fmt (default-value 'mode-line-format))
         (tail (memq 'mode-line-buffer-identification fmt)))
    (setq-default mode-line-format
                  (if tail
                      (append (butlast fmt (length tail))
                              '(tyler/mode-line-tabspace)
                              tail)
                    (cons 'tyler/mode-line-tabspace fmt)))))

;; Saves each terminal and its scrollback with the tabspaces session; see the
;; file's commentary.  Its mode is enabled by `tyler/load-session'.
(use-package ghostel-resurrect
  :load-path ("site-lisp")
  :config
  ;; tabspaces needs `tab-bar-mode' on, but the tab bar itself stays hidden.
  (setq tab-bar-show nil)
  (tab-bar-mode 1)
  (tabspaces-mode 1))

(defun tyler/load-session ()
  "Restore the saved tabspaces session, terminals included, and keep saving it.
Emacs starts without the session, and doesn't save it until this has run, so
an Emacs that never loaded it (say, one opened as $EDITOR) can't overwrite it."
  (interactive)
  (when tabspaces-session
    (user-error "Session already loaded"))
  ;; Before restoring, so the terminals in the session are restored too.
  (ghostel-resurrect-mode 1)
  (tabspaces-restore-session)
  (setq tabspaces-session t)
  (add-hook 'kill-emacs-hook #'tabspaces--save-session-smart))

;; On startup, remind me how to load the session.  On an idle timer so the
;; startup echo-area message doesn't replace it.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             0 nil
             (lambda ()
               (unless tabspaces-session
                 (message "Load workspaces: x TAB L (or C-c TAB L, M-x tyler/load-session)"))))))

;; Built into Emacs 30+: after a prefix key (e.g. C-x), shows the keys that can follow.
(use-package which-key
  :config
  (which-key-mode 1))

(use-package emacs
  :init
  ;; prefer spaces-only indentation
  (setq-default indent-tabs-mode nil)
  ;; bind-key* makes sure this binding overrides any other mode's bindings.
  (bind-key* "C-c p" (tyler/invert-command 'tyler/transpose-lines))
  (bind-key* "C-c n" 'tyler/transpose-lines)
  (bind-key* "C-c j" 'duplicate-dwim)
  ;; cmd +/-/0 zoom everything (mode line, minibuffer, tab bar, all buffers), not
  ;; just the current buffer as Emacs.app's default `text-scale-adjust' does.
  (bind-key "s-+" #'global-text-scale-adjust)
  (bind-key "s-=" #'global-text-scale-adjust)
  (bind-key "s--" #'global-text-scale-adjust)
  (bind-key "s-0" #'global-text-scale-adjust)
  (bind-key* "<up>" (tyler/terminal-key-or 'scroll-down-line))
  (bind-key* "<down>" (tyler/terminal-key-or 'scroll-up-line))
  (bind-key* "C-c C-SPC" 'set-rectangular-region-anchor)
  (bind-key* "C-c C-s" 'isearch-forward-thing-at-point)
  (bind-key* "C-c s" 'window-swap-states)
  (bind-key* "C-h z" 'shortdoc-display-group)
  (bind-key* "C-c b" 'scratch-buffer)
  (bind-key* "C-c l" 'display-line-numbers-mode)
  (bind-key* "C-c L" (lambda () (interactive) (setq display-line-numbers (if (eq display-line-numbers 'absolute) 'relative 'absolute))))
  (bind-key* "C-c i" 'ibuffer)
  (bind-key* "M-%" 'query-replace-regexp)
  (bind-key* "M-RET" 'tyler/open-line-between-delimiters)
  (bind-key* "C-z" 'undo)
  (bind-key* "<remap> <scroll-up-command>" 'tyler/scroll-half-page-up)
  (bind-key* "<remap> <scroll-down-command>" 'tyler/scroll-half-page-down)

  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1)
  
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

  (global-subword-mode 1)

  ;; go right to an empty buffer
  (setq inhibit-startup-message t)

  ;; Make all "yes or no" prompts show "y or n" instead
  (fset 'yes-or-no-p 'y-or-n-p)

  ; Don't want any backup files
  (setq make-backup-files nil)

  ; Enable completion-preview-mode (causes gray text to appear after point when autocompleting).
  ;; (global-completion-preview-mode)

  :custom-face
  ;; Ghostty's "Material Darker" theme colors, so GUI Emacs looks like the terminal.
  (default ((t (:background "#212121" :foreground "#eeffff"))))
  (completions-annotations ((t (:foreground "powderblue" :underline nil))))
  (error ((t (:foreground "tomato" :weight bold))))
  (mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "burlywood1" :background "#56514c"))))
  (mode-line-inactive ((t (:foreground "gray60" :background "gray15" :inherit mode-line)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((vue-ts-mode :url "https://github.com/8uff3r/vue-ts-mode" :branch
                  "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray24")))))
