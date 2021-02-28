
;; All in .config
(setq user-emacs-directory "~/.config/emacs/")

;; Speed up init time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
(setq load-prefer-newer t)

;; Straight
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use Package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)  ;; :straight t by default
(straight-use-package 'bind-key)          ;; enable :bind
(setq-default use-package-always-defer t) ;; lazy load all

;; Base
(use-package emacs
  :straight (:type built-in)
  :init
  ;; preffered settings
  (setq-default indent-tabs-mode nil
                ring-bell-function 'ignore
                line-spacing 4
                initial-scratch-message "*scratch*\n"
                frame-resize-pixelwise t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode t)
  (setq auto-save-default nil
        make-backup-files nil)
  (setq x-select-enable-clipboard t)
  (setq epa-pinentry-mode 'loopback)

  ;; paths
  (defconst mar-on-mac (eq system-type 'darwin)
    "Working on a mac.")
  (defconst mar-on-linux (eq system-type 'gnu/linux)
    "Working on a linux machine.")
  (defvar mar-path-library (if mar-on-mac "~/Dropbox/lib/" "~/Dropbox/lib/")
    "Path to library directory.")
  (defvar mar-path-library-cards `(,(concat mar-path-library "bib/master.bib"))
    "List of path to bibtex file with all the documents.")
  (defvar mar-path-library-files `(,(concat mar-path-library "pdf/econ")
                                   ,(concat mar-path-library "pdf/hist")
                                   ,(concat mar-path-library "pdf/prog")
                                   ,(concat mar-path-library "pdf/psyc")
                                   ,(concat mar-path-library "pdf/stat"))
    "List of paths to directories with all the documents.")
  (defvar mar-default-python (if mar-on-mac "3.8.6" "emacs")
    "Default Python installation or environment.")
  (defvar mar-path-python (if mar-on-mac "~/.pyenv/versions/3.8.6/bin/"
                            "~/.pyenv/versions/emacs/bin/")
    "Path to Python binaries.")
  (defvar mar-path-python-lsp (concat mar-path-python "pyls")
    "Path to the language server for python.")
  (defvar mar-path-python-black (concat mar-path-python "black")
    "Path to python's black.")

  ;; Secrets
  (defvar mar-path-secrets (concat user-emacs-directory "secrets.el")
    "Path to any non public local variables.")
  (if (file-exists-p mar-path-secrets) (load mar-path-secrets))

  ;; Custom
  (setq custom-file (concat user-emacs-directory "custom.el"))

  ;; some functions to better manage buffer
  (defun mar-close-this-buffer ()
    "Close the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun mar-close-other-buffer ()
    "Close buffer in other window."
    (interactive)
    (unless (one-window-p)
      (other-window 1)
      (kill-this-buffer)
      (if (not (one-window-p))
          (other-window 1))))

  (defun mar-close-and-kill-this-pane ()
    "Kill this window and its buffer."
    (interactive)
    (kill-buffer (current-buffer))
    (if (not (one-window-p))
        (delete-window)))

  (defun mar-close-and-kill-other-pane ()
    "Kill other window and its buffer."
    (interactive)
    (other-window 1)
    (kill-this-buffer)
    (if (not (one-window-p))
        (delete-window)))

  (defun mar-cleanup-tramp-buffers ()
    "Close all tramp buffers and cleanup connections."
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (switch-to-buffer "*scratch*"))

  (defun mar-cleanup-dired-buffers ()
    (interactive)
    "Closes all open dired buffers"
    (mapc (lambda (buffer)
            (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer)))
          (buffer-list))))

(use-package evil
  :init
  (evil-mode +1)
  ;; Use esc to leave M-x buffers
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)
  ;; leader key
  (define-prefix-command 'my-leader-map)
  (define-key evil-motion-state-map "," 'my-leader-map)
  ;; toggle full frame
  (define-key my-leader-map "tf" 'toggle-frame-fullscreen)
  ;; windows bindings
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-motion-state-map (kbd "C-w _") 'window-swap-states)
  (define-key evil-motion-state-map (kbd "C-w <left>") 'evil-window-decrease-width)
  (define-key evil-motion-state-map (kbd "C-w <right>") 'evil-window-increase-width)
  (define-key evil-motion-state-map (kbd "C-w <up>") 'evil-window-decrease-height)
  (define-key evil-motion-state-map (kbd "C-w <down>") 'evil-window-increase-height)
  ;; manage buffer bindings, defined in Emacs
  (define-key my-leader-map "kb" 'mar-close-this-buffer)
  (define-key my-leader-map "ko" 'mar-close-other-buffer)
  (define-key my-leader-map "kB" 'mar-close-and-kill-this-pane)
  (define-key my-leader-map "kO" 'mar-close-and-kill-other-pane)
  (define-key my-leader-map "kt" 'mar-cleanup-tramp-buffers)
  (define-key my-leader-map "kd" 'mar-cleanup-dired-buffers)
  ;; a minimal evil mode line
  (setq mar-mode-line-format
                '((:eval (propertize evil-mode-line-tag 'face '(:foreground "red")))
                  mode-line-modified              ;; modified
                  "R:"
                  mode-line-remote                ;; local/remote
                  "|"
                  (:eval (alist-get 'name (tab-bar--current-tab)))
                  "|"
                  mode-line-buffer-identification ;; buffer name, etc
                  mode-line-position
                  (vc-mode vc-mode)               ;; git info
                  "|"
                  mode-name
                  "|"
                  mode-line-misc-info))
  (setq evil-mode-line-format nil)
  (setq-default mode-line-position '("|%l:%c|"))
  (setq-default mode-line-format mar-mode-line-format)
  (setq mode-line-format mar-mode-line-format)
  ;; take care of cursors when in terminal mode
  (unless (display-graphic-p)
    (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\e[6 q")))
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (send-string-to-terminal "\e[6 q")))
    (add-hook 'evil-normal-state-entry-hook
              (lambda () (send-string-to-terminal "\e[1 q")))))

(use-package evil-nerd-commenter
  :init
  (define-key my-leader-map (kbd "c <SPC>") 'evilnc-comment-or-uncomment-lines))

(use-package undo-fu
  :init
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))

(use-package avy
  :init
  (setq avy-all-windows nil)
  (define-key my-leader-map "w" 'avy-goto-word-1)
  (define-key my-leader-map "f" 'avy-goto-char)
  (define-key my-leader-map "W" 'avy-goto-char-2))

(use-package evil-matchit
  :init (global-evil-matchit-mode 1))

(use-package sentence-navigation
  :init
 (define-key evil-motion-state-map ")" 'sentence-nav-evil-forward)
 (define-key evil-motion-state-map "(" 'sentence-nav-evil-backward)
 ;; e.g. das (outer) and dis (inner)
 (define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-a-sentence)
 (define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence))

;; Project.el is an Emacs core package; however, the one included is old
;; and missing some functions, like project-root which is used by other modes.
;; If the package that comes with Emacs is loaded instead of the one installed
;; with straight, you need to remove it.
(use-package project
  :init
  (define-key my-leader-map "sp" 'project-find-regexp)
  (define-key my-leader-map "sf" 'project-find-file))

(use-package counsel
  :init
  (counsel-mode 1)
  ;; extra functions
  (defun mar-rg-directory ()
    "rg (wgrep) in a directory"
    (interactive)
    (let* ((dir (read-file-name "Directory:" default-directory))
           (search ""))
      (counsel-rg search dir)))
  ;; help
  (define-key my-leader-map "hv" 'counsel-describe-variable)
  (define-key my-leader-map "hf" 'counsel-describe-function)
  (define-key my-leader-map "hl" 'counsel-find-library)
  (define-key my-leader-map "hu" 'counsel-unicode-char)
  (define-key my-leader-map "hb" 'counsel-descbinds)
  (define-key my-leader-map "ha" 'counsel-apropos)
  ;; search/switch
  (define-key my-leader-map "sm" 'counsel-mark-ring)
  (define-key my-leader-map "sb" 'counsel-bookmark)
  (define-key my-leader-map "sd" 'mar-rg-directory)
  (define-key my-leader-map "sg" 'counsel-git-grep)
  (if mar-on-linux  (define-key my-leader-map "sa" 'counsel-linux-app))
  ;; kill ring
  (define-key my-leader-map "y" 'counsel-yank-pop)
  ;; find
  (evil-define-command mar-evil-find (filename)
    "Open a file"
    :repeat nil
    :move-point nil
    (interactive "<f>")
    (counsel-find-file filename))
  (evil-ex-define-cmd "find" 'mar-evil-find))

(use-package counsel-osx-app
  :if mar-on-mac
  :init
  (define-key my-leader-map "sa" 'counsel-osx-app))

(use-package ivy
  :init
  (ivy-mode 1)
  ;; add recent files and bookmars to switch buffer
  (setq ivy-use-virtual-buffers t)
  ;; shows total in the prompt
  (setq ivy-count-format "(%d/%d) ")
  ;; height of minibuffer
  (setq ivy-height 30)
  ;; minibuffer navigation
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-partial-or-done)
  ;; toggle search between regex plus and fuzzy, fuzzy gets more results
  (define-key my-leader-map "ts" 'ivy-toggle-fuzzy)
  (evil-ex-define-cmd "ls" 'ivy-switch-buffer)
  (define-key my-leader-map "b" 'ivy-switch-buffer)
  (define-key my-leader-map "/" 'swiper-isearch)
  (define-key my-leader-map "sr" 'ivy-resume))

;; ivy plugins
(use-package ivy-prescient
  :requires (ivy)
  :init (ivy-prescient-mode 1))

(use-package ivy-rich
  :requires (ivy)
  :init
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

;; This package lets you work on search results
;; 1) Use counsel-rg or counsel-git-grep
;; 2) Use C-c C-o to enter ivy-occur mode, followed by 'w'
;; 3) press C-x C-s to save or C-x C-k to abort.
(use-package wgrep)

(use-package auth-source-pass
  :straight (:type built-in)
  :init (auth-source-pass-enable))

(use-package ivy-pass
  :requires (ivy password-store))

(use-package engine-mode
  :init
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"))

(use-package tab-bar
  :straight (:type built-in)
  :init
  ;; don't show the tabs on top
  (setq tab-bar-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  ;; bindings
  (evil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabclose" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabonly" 'tab-bar-close-other-tabs)
  (evil-ex-define-cmd "tabrename" 'tab-bar-rename-tab))

;; Manage Very Large Files
(use-package vlf
  :init (require 'vlf-setup))

(use-package autorevert
  :straight (:type built-in)
  :hook ((dired-mode) . auto-revert-mode))

(use-package company
  :hook ((prog-mode tex-mode TeX-mode markdown-mode) . company-mode)
  :bind (:map company-active-map (">" . company-filter-candidates))
  :init
  (defun company-emacs-lisp-mode ()
    "Setup company mode for emacs-lisp-mode"
    (set (make-local-variable 'company-backends)
         '((company-elisp
            company-files
            company-yasnippet
            company-dabbrev-code))))
  (add-hook 'emacs-lisp-mode-hook 'company-emacs-lisp-mode)
  :config
  (evil-define-key 'insert company-mode-map
    (kbd "C-c c") 'company-yasnippet)
  (setq company-tooltip-limit 10
        company-show-numbers t
        company-selection-wrap-around t
        company-dabbrev-other-buffers t))

(use-package company-math
  :init
  (defun company-latex-mode ()
    "Setup company mode for TeX-mode and tex-mode"
    (set (make-local-variable 'company-backends)
         '((company-math-symbols-latex
            company-latex-commands
            company-yasnippet
            company-dabbrev
            company-files))))
  (add-hook 'gfm-mode-hook 'company-latex-mode)
  (add-hook 'markdown-mode-hook 'company-latex-mode)
  (add-hook 'LaTeX-mode-hook 'company-latex-mode))

(use-package eglot
  :hook ((c-mode c++-mode python-mode ess-r-mode sh-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	               `(python-mode . (,mar-path-python-lsp)))
  (setq eglot-autoshutdown t))

(use-package display-line-numbers
  :straight (:type built-in)
  :hook ((prog-mode) . display-line-numbers-mode)
  :init
  (evil-ex-define-cmd "nu" 'display-line-numbers-mode)
  (evil-ex-define-cmd "number" 'display-line-numbers-mode)
  :config
  (defun mar-lnumbers ()
    (interactive)
    "Toggle line number styles between absolute and relative"
    (if (equal display-line-numbers 'relative)
        (setq display-line-numbers 'absolute)
      (setq display-line-numbers 'relative)))
  (define-key my-leader-map "n" 'mar-lnumbers))

(use-package olivetti
  :init
  (defvar mar-attention-on nil
    "True if attention mode is toggled on")
  (defun mar-toggle-attention ()
    (interactive)
    "Sets a calmer writting environment."
    (if mar-attention-on
        (progn (olivetti-mode -1)
               (text-scale-increase 0)
               (setq mode-line-format mar-mode-line-format)
               (setq-local mar-attention-on nil))
      (progn (text-scale-increase 1)
             (olivetti-mode +1)
             (setq mode-line-format nil)
             (setq-local mar-attention-on t))))
  (define-key my-leader-map "ta" 'mar-toggle-attention)
  :config
  (setq olivetti-body-width 80))

(use-package flyspell
  :straight (:type built-in)
  :hook
  ((gfm-mode markdown-mode LaTeX-mode) . flyspell-mode))

(use-package ispell
  :config
  (setq ispell-personal-dictionary "~/.config/personal.dic")
  ;; Use Hunspell if available.
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-dictionary "en_US")
  ;; ignore code blocks
  (add-to-list 'ispell-skip-region-alist '("^```" . "^```"))
  (add-to-list 'ispell-skip-region-alist '("^$$" . "^$$")))

(use-package yasnippet
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package faces
  :straight (:type built-in)
  :init
  (defconst mar-font-fp (if mar-on-mac "Menlo" "DejaVu Sans Mono")
    "Default fix point font")

  (defconst mar-font-vp (if mar-on-mac "Helvetica" "DejaVu Sans")
    "Default variable point font")

  (defun mar-reset-fonts ()
    "Resets fonts to defaults set based on system and monitor"
    (interactive)
    (let* ((height (if (display-graphic-p)
                       (if (> (x-display-pixel-width) 2000) 240 110)
                     120))
           (heigvp (+ height 10)))
      (set-face-attribute 'default nil :family mar-font-fp :height height :weight 'normal)
      (set-face-attribute 'fixed-pitch nil :family mar-font-fp :height height :weight 'normal)
      (set-face-attribute 'variable-pitch nil :family mar-font-vp :height heigvp :weight 'normal)))

  (mar-reset-fonts))

(use-package modus-themes
  :init
  (when (display-graphic-p)
    (modus-themes-load-vivendi)))

(use-package visual-line
  :straight (:type built-in)
  :hook ((gfm-mode markdown-mode LaTeX-mode) . visual-line-mode))

(use-package imenu-list
  :hook (imenu-list-major-mode . evil-emacs-state)
  :commands (imenu-list-smart-toggle)
  :init
  (define-key my-leader-map "tt" 'imenu-list-smart-toggle))

(use-package imenu-anywhere
  :init
  (define-key my-leader-map "m" 'ivy-imenu-anywhere)
  (defun mar-imenu-use-package ()
    "Add use-package to imenu"
    (add-to-list 'imenu-generic-expression
                 '("Use Package"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'mar-imenu-use-package)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t))

(use-package beancount
  :straight (:host github :repo "beancount/beancount-mode")
  :magic ("%beancount" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode)
  :bind (:map beancount-mode-map ("C-z" . beancount-tab-dwim)))

(use-package cmake-mode
  ;; package comes with the system cmake installation
  :straight nil
  :commands (cmake-mode))

(use-package clang-format+
  :hook ((c-mode c++-mode) . clang-format+-mode)
  :config
  (if mar-on-mac (setq "/usr/local/opt/llvm/bin/clang-format")))

(use-package json-mode)

(use-package tex
  :straight auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . flymake-mode)
         (LaTeX-mode . TeX-source-correlate-mode))
  :config
  ;; auctex provides semantic checker with flymake and chktex
  (evil-define-key 'normal flymake-mode-map
    "]l" 'flymake-goto-next-error
    "[l" 'flymake-goto-prev-error
    "[d" 'flymake-goto-diagnostic)
  (setq-default TeX-master nil)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t
        TeX-source-correlate-start-server t
        prettify-symbols-unprettify-at-point t))

(use-package markdown-mode
  :mode (("README.md" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq-default markdown-asymmetric-header t
                ;; code blocks only
                markdown-fontify-code-blocks-natively t
                markdown-enable-math t)
  ;; bindings
  (evil-define-key 'normal markdown-mode-map
    "K" 'markdown-outline-next
    "J" 'markdown-outline-next
    (kbd "C-<") 'markdown-promote
    (kbd "C->") 'markdown-demote
    (kbd "C-c r") 'markdown-check-refs
    (kbd "C-c i") 'markdown-table-insert-column
    (kbd "C-c l") 'markdown-cleanup-list-numbers))

(use-package python
  :hook (inferior-python-mode . evil-emacs-state)
  :config (setq python-indent-guess-indent-offset nil))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-executable mar-path-python-black)
  (setq blacken-line-length 79))

(use-package pyenv-mode
  :hook (python-mode . mar-try-set-pyenv)
  :commands (pyenv-mode-version)
  :init
  (defun mar-run-ipython ()
    "Try to run ipython if present in a suitable environment"
    (interactive)
    (let ((python-shell-interpreter "ipython")
          (python-shell-interpreter-args "--simple-prompt -i"))
      (unless (pyenv-mode-version)
        (call-interactively #'pyenv-mode-set))
      (run-python)))

  (defun mar-try-set-pyenv ()
    "try to start a default environment"
    (pyenv-mode 1)
    (unless (pyenv-mode-version)
      (if (member mar-default-python (pyenv-mode-versions))
          (pyenv-mode-set mar-default-python)
        (message "WARNING: Default pyenv not found (see pyenv-mode)")))))

(use-package ess
  :mode ("\\.R\\'" . ess-r-mode))

(use-package dired
  :straight (:type built-in)
  :init
  (if mar-on-mac (setq dired-use-ls-dired nil))
  :config
  (setq delete-by-moving-to-trash nil
        dired-dwim-target t)
  (evil-define-key 'normal dired-mode-map
    ;; Navigation
    ";s" 'dired-sort-toggle-or-edit ;; sort by name or date
    ";h" 'dired-hide-details-mode ;; show/hide details
    ";f" 'dired-show-file-type
    "za" 'dired-subtree-toggle
    "r" 'revert-buffer
    ;; Actions
    ";cp" 'dired-do-copy ;; if multiple files, to new dir
    ";mv" 'dired-do-rename ;; if multiple files, move to dir
    "+" 'dired-create-directory
    ";z" 'dired-do-compress ;; compress/extract each file
    ";Z" 'dired-do-compress-to ;; compress to dir (name needs extension)
    ;; Mark
    "d" 'dired-flag-file-deletion
    "m" 'dired-mark
    "u" 'dired-unmark
    "U" 'dired-unmark-all-marks
    "q" 'kill-this-buffer))

;; pluggins
(use-package dired-subtree
  :config (setq dired-subtree-line-prefix "->  "))

;; nicer color
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package bibtex
  :config
  (setq bibtex-align-at-equal-sign t
        bibtex-entry-format t))

(use-package ivy-bibtex
  :config
  ;; paths
  (setq bibtex-completion-bibliography mar-path-library-cards
        bibtex-completion-library-path mar-path-library-files
        bibtex-completion-notes-path (concat org-directory "book-notes/"))
  ;; completion
  (setq bibtex-completion-pdf-extension '(".pdf" ".epub")
        bibtex-completion-pdf-symbol "#"
        bibtex-completion-notes-symbol "*"
        bibtex-completion-notes-extension ".md"
        bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-format-citation-functions
        '((latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  ;; open with
  (defun mar-bibtex-open-function (fpath)
    (let* ((extension (file-name-extension fpath)))
      (cond ((string= extension "pdf") (call-process "xdg-open" nil 0 nil "-a" "skim" fpath))
            ((string= extension "epub") (call-process "xdg-open" nil 0 nil "-a" "books" fpath))
            (t (find-file fpath)))))
  (setq bibtex-completion-pdf-open-function 'mar-bibtex-open-function))
