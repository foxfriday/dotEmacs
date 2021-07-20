;;; init.el -- my personal init file

;;; Commentary:
;; Hopefully, this should work for linux and osX.

;;; Code:
;; Straight
(setq-default comp-deferred-compilation-deny-list nil)
(setq-default straight-use-package-by-default t
              straight-vc-git-default-clone-depth 1
              straight-check-for-modifications nil
              use-package-always-defer t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; Emacs Defaults
(use-package emacs
  :straight (:type built-in)
  :init
  ;; Custom file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (if (file-exists-p custom-file) (load custom-file))
  ;; System
  (defconst mar-on-mac (eq system-type 'darwin)
    "Working on a mac.")
  ;; Preffered settings, mostly remove noise
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil
                ring-bell-function 'ignore
                line-spacing 4
                initial-scratch-message ";; scratch buffer\n"
                frame-resize-pixelwise t
                epa-pinentry-mode 'loopback
                load-prefer-newer t
                select-enable-clipboard t
                tab-bar-show nil
                tab-bar-new-tab-choice "*scratch*")
  (blink-cursor-mode 0)
  (show-paren-mode t)
  (auth-source-pass-enable)
  ;; no littering
  (setq auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)
  ;; Writing and prog modes
  (setf sentence-end-double-space nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook
            (lambda ()
              (setq prettify-symbols-unprettify-at-point 'right-edge)
              (prettify-symbols-mode)
              (flyspell-mode)
              (visual-line-mode)
              (setq fill-column 80)))
  ;; Fonts
  (defconst mar-font-fp (if mar-on-mac "Menlo" "DejaVu Sans Mono")
    "Default fix point font")
  (defconst mar-font-vp (if mar-on-mac "Helvetica" "DejaVu Sans")
    "Default variable point font")
  (defun mar-reset-fonts ()
    "Resets fonts to defaults set based on system and monitor"
    (interactive)
    (let* ((height (if (display-graphic-p)
                       (if (> (x-display-mm-height) 300) 190 140)
                     120))
           (heigvp (+ height 10)))
      (set-face-attribute
       'default nil :family mar-font-fp :height height :weight 'normal)
      (set-face-attribute
       'fixed-pitch nil :family mar-font-fp :height height :weight 'normal)
      (set-face-attribute
       'variable-pitch nil :family mar-font-vp :height heigvp :weight 'normal)))
  (mar-reset-fonts))

(use-package modus-themes
  :init
  (when (display-graphic-p)
    (modus-themes-load-vivendi)))

(use-package olivetti
  :hook (text-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 82)
  (defun mar-olivetti-recenter ()
    "Toggle olivetti from relative to absolute size."
    (interactive)
    (setq olivetti-body-width (if (< olivetti-body-width 1.0) 82 0.8))
    (redraw-frame)))

(use-package evil
  :init
  (evil-mode +1)
  ;; Cursors in terminal
  (unless (display-graphic-p)
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (send-string-to-terminal "\033[5 q")))
    (add-hook 'evil-normal-state-entry-hook
              (lambda () (send-string-to-terminal "\033[0 q"))))
  ;; Make esc a little more global
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
  ;; Bindings
  (define-prefix-command 'my-leader-map)
  (evil-define-key 'motion 'global
    ;; leader
    "," 'my-leader-map
    ;; windows
    (kbd "C-h") 'evil-window-left
    (kbd "C-j") 'evil-window-down
    (kbd "C-k") 'evil-window-up
    (kbd "C-l") 'evil-window-right
    ;; sentence navigator
    (kbd ")") 'evil-forward-sentence-begin
    (kbd "(") 'evil-backward-sentence-begin)
  ;; sentence navigation e.g. das (outer) and dis (inner)
  (define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
  ;; Missing commands
  (evil-ex-define-cmd "nu[mber]" 'display-line-numbers-mode)
  (evil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabclose" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabonly" 'tab-bar-close-other-tabs)
  (evil-ex-define-cmd "tabrename" 'tab-bar-rename-tab)
  ;; No fuzz mode line with evil indicator
  (defvar mar-mode-line-format
                '((:eval (propertize evil-mode-line-tag 'face '(:foreground "red")))
                  mode-line-modified
                  "R:"
                  mode-line-remote
                  "|"
                  (:eval (alist-get 'name (tab-bar--current-tab)))
                  "|"
                  mode-line-buffer-identification
                  mode-line-position
                  (vc-mode vc-mode)  ;; git info
                  "|"
                  mode-name
                  "|"
                  mode-line-misc-info))
  (setq-default evil-mode-line-format nil)
  (setq-default mode-line-position '("|%l:%c|"))
  (setq-default mode-line-format mar-mode-line-format))

(use-package evil-nerd-commenter
  :init
  (evil-define-key nil my-leader-map
    (kbd "c <SPC>") 'evilnc-comment-or-uncomment-lines))

(use-package evil-matchit
  :init (global-evil-matchit-mode 1))

(use-package undo-fu
  :init
  (evil-define-key 'normal 'global
    (kbd "u") 'undo-fu-only-undo
    (kbd "C-r") 'undo-fu-only-redo))

;; Common
(use-package eplus
  :straight (eplus :type git :host github :repo "foxfriday/eplus")
  :demand t
  :init
  ;;(define-key global-map "\M-Q" 'ep-unfill-paragraph)
  (evil-define-key nil 'global
    (kbd "M-Q") 'ep-unfill-paragraph)
  (evil-define-key nil my-leader-map
    ;; buffers
    (kbd "kb") 'ep-close-this-buffer
    (kbd "ko") 'ep-close-other-buffer
    (kbd "kB") 'ep-close-and-kill-this-pane
    (kbd "kO") 'ep-close-and-kill-other-pane
    (kbd "kt") 'ep-cleanup-tramp-buffers
    (kbd "kd") 'ep-cleanup-dired-buffers
    ;; other
    (kbd "tf") 'toggle-frame-fullscreen))

(use-package outline
  :straight (:type built-in)
  :config
  (evil-define-key 'normal 'outline-minor-mode-map
    "]]" 'outline-next-visible-heading
    "[[" 'outline-previous-visible-heading
    "]u" 'outline-up-heading
    (kbd "M-h") 'outline-promote
    (kbd "M-j") 'outline-move-subtree-down
    (kbd "M-k") 'outline-move-subtree-up
    (kbd "M-l") 'outline-demote
    "zb" 'outline-show-children
    "zB" 'outline-show-branches))

(use-package avy
  :init
  (setq avy-all-windows nil)
  (evil-define-key nil my-leader-map
    "w" 'avy-goto-word-1
    "W" 'avy-goto-char-2))

(use-package counsel
  :init
  (counsel-mode 1)
  ;; extra functions
  (defun mar-rg-directory ()
    "rg (wgrep) in a directory"
    (interactive)
    (let* ((dir (read-file-name "Directory:" default-directory)))
      (counsel-rg nil dir)))
  (defun mar-rg-notes ()
    "rg (wgrep) in notes directory"
    (interactive)
    (let* ((counsel-rg-base-command "rg -H --no-heading --color never %s"))
      (counsel-rg nil "~/Dropbox/notes/mxd")))
  ;; Find
  (evil-define-command mar-evil-find (filename)
    "Open a file"
    :repeat nil
    :move-point nil
    (interactive "<f>")
    (let* ((fname (if filename filename default-directory)))
      (if (file-directory-p fname)
          (counsel-find-file fname)
        (find-file fname))))
  (evil-ex-define-cmd "find" 'mar-evil-find)
  ;; Bindings
  (evil-define-key nil my-leader-map
    ;; help
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "hl" 'counsel-find-library
    "hu" 'counsel-unicode-char
    "hb" 'counsel-descbinds
    "ha" 'counsel-apropos
    ;; search/switch
    "sp" 'project-find-file
    "sf" 'counsel-fzf
    "sr" 'counsel-recentf
    "sm" 'counsel-evil-marks
    "sb" 'counsel-bookmark
    "sd" 'mar-rg-directory
    "sg" 'counsel-git-grep
    "sn" 'mar-rg-notes
    "se" 'counsel-flycheck
    ;; kill ring
    "y" 'counsel-yank-pop)
  (unless mar-on-mac (define-key my-leader-map "sa" 'counsel-linux-app)))

(use-package counsel-osx-app
  :if mar-on-mac
  :init (define-key my-leader-map "sa" 'counsel-osx-app))

(use-package ivy
  :init
  (ivy-mode 1)
  ;; Recent file in switch buffer
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format "(%d/%d) "
                ivy-height 30)
  ;; Minibuffer navigation
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-partial-or-done)
  ;; Bindings
  (evil-define-key nil my-leader-map
    "b" 'ivy-switch-buffer
    "/" 'swiper-isearch))

(use-package ivy-prescient
  :init (ivy-prescient-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setq-default ivy-rich-path-style 'abbrev))

(use-package ivy-pass)

;; Edit search results
;; 1) Use counsel-rg or counsel-git-grep
;; 2) Use C-c C-o to enter ivy-occur mode, followed by 'w'
;; 3) press C-x C-s to save or C-x C-k to abort.
(use-package wgrep)

(use-package vlf
  ;; Manage Very Large Files
  :init (require 'vlf-setup))

(use-package company
  :hook ((prog-mode markdown-mode) . company-mode)
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
  (setq company-minimum-prefix-length 1
        company-idle-delay 1
        company-show-numbers t
        company-selection-wrap-around t
        company-dabbrev-other-buffers t)
  (evil-define-key 'insert company-mode-map
    (kbd "C-c y") 'company-yasnippet))

(use-package yasnippet
  :hook ((prog-mode LaTeX-mode beancount-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package imenu-list
  :hook (imenu-list-major-mode . evil-emacs-state)
  :commands (imenu-list-smart-toggle)
  :init (define-key my-leader-map "M" 'imenu-list-smart-toggle))

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

(use-package dired
  :straight (:type built-in)
  :init (if mar-on-mac (setq dired-use-ls-dired nil))
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq delete-by-moving-to-trash nil
        dired-dwim-target t)
  (evil-define-key 'normal dired-mode-map
    ";h" 'dired-hide-details-mode
    ";f" 'dired-show-file-type
    "za" 'dired-subtree-toggle
    "r" 'revert-buffer
    ;; Actions
    ";cp" 'dired-do-copy
    ";mv" 'dired-do-rename
    "+" 'dired-create-directory
    ";z" 'dired-do-compress-to ;; new name needs extension
    ;; Mark
    "d" 'dired-flag-file-deletion
    "m" 'dired-mark
    "u" 'dired-unmark
    "U" 'dired-unmark-all-marks
    "q" 'kill-this-buffer))

(use-package dired-subtree
  :config (setq dired-subtree-line-prefix "->  "))

(use-package diredfl
  ;; nicer dired colors
  :hook (dired-mode . diredfl-mode))

(use-package magit
  :init (global-set-key (kbd "C-c m") 'magit-dispatch))

(use-package compile
  :straight (:type built-in)
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq-default compilation-scroll-output t)
  (evil-define-key 'motion compilation-mode-map
    (kbd "h") nil
    (kbd "g") nil
    (kbd "r") 'recompile))

(use-package flycheck
  :hook ((prog-mode lsp-mode) . flycheck-mode)
  :config
  (evil-define-key 'normal flycheck-mode-map
    "ge" 'flycheck-list-errors
    "]l" 'flycheck-next-error
    "[l" 'flycheck-previous-error))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :demand t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode)
  (require 'tree-sitter-langs))

(use-package lsp-mode
  :hook ((sh-mode tex-mode latex-mode) . lsp-deferred)
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024)
        lsp-headerline-breadcrumb-enable nil
        lsp-eldoc-enable-hover nil
        lsp-signature-render-documentation nil)
  ;; latex + zathura + texlab + latexmk
  ;; For backward search with Zathura use C-<mouse 1>
  (defun lsp-latex-msg (result)
    "Message from synctex."
    (if (eq 0 (plist-get result :status))
        (message "Command succeeded")
      (message "Command failed")))
  (defun lsp-latex-build ()
    "Build TeX file and start a server process"
    (interactive)
    (unless (boundp 'server-process)
      (server-start))
    (lsp-request-async "textDocument/build"
                       (list :textDocument (lsp--text-document-identifier))
                       #'lsp-latex-msg))
  (defun lsp-latex-forward-search ()
    "Forward search on preview."
    (interactive)
    (lsp-request-async "textDocument/forwardSearch"
                       (lsp--text-document-position-params)
                       #'lsp-latex-msg))
  (add-hook 'latex-mode-hook
            (lambda ()
              (lsp-register-custom-settings
               '(("latex.build.executable" "latexmk")
                 ("latex.build.args"  ["-pdf"
                                       "-interaction=nonstopmode"
                                       "-synctex=1"
                                       "-pvc"
                                       "%f"])
                 ("latex.forwardSearch.executable" "zathura")
                 ("latex.forwardSearch.args" ["--synctex-forward"
                                              "%l:1:%f"
                                              "%p"])
                 ("latex.lint.onChange" t t)
                 ("bibtex.formatting.lineLength" 80)
                 ("bibtex.formatting.formatter" "texlab")))))
  ;; bindings
  (evil-define-key 'normal 'latex-mode-map
    (kbd "C-c C-c") 'lsp-latex-build
    (kbd "C-c C-v") 'lsp-latex-forward-search)
  (evil-define-key 'normal 'lsp-mode-map
    ;; documentation
    "gh" 'lsp-describe-thing-at-point
    ;; navigation
    "gs" 'lsp-ivy-workspace-symbol
    "gS" 'lsp-ivy-global-workspace-symbol
    "go" 'lsp-clangd-find-other-file
    "gd" 'lsp-find-definition
    "gR" 'lsp-find-references))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

;; Python
(use-package python
  :hook (inferior-python-mode . evil-emacs-state)
  :config (setq python-indent-guess-indent-offset nil))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config
  (setq lsp-pyright-venv-path (expand-file-name "~/.pyenv/versions")))

(use-package blacken
  :straight (blacken :type git :host github :repo "pythonic-emacs/blacken"
                     :fork (:host github
                                 :repo "foxfriday/blacken"))
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-executable "~/.pyenv/versions/emacs/bin/black"))

;; C++
(use-package ccls
  :hook (c-mode-common . (lambda () (require 'ccls) (lsp)))
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil
        ccls-sem-highlight-method 'font-lock))

(use-package clang-format+
  :hook (c-mode-common . clang-format+-mode)
  :config
  (if mar-on-mac (setq "/usr/local/opt/llvm/bin/clang-format")))

(use-package cmake-mode
  ;; package comes with the system cmake installation
  :straight nil
  :commands (cmake-mode))

;; MD
(use-package markdown-mode
  :mode (("README.md" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (evil-define-key 'normal markdown-mode-map
    "]]" 'outline-next-visible-heading
    "[[" 'outline-previous-visible-heading
    "]u" 'outline-up-heading
    (kbd "M-h") 'markdown-promote
    (kbd "M-j") 'outline-move-subtree-down
    (kbd "M-k") 'outline-move-subtree-up
    (kbd "M-l") 'markdown-demote
    "zb" 'outline-show-children
    "zB" 'outline-show-branches)
  (setq-default markdown-fontify-code-blocks-natively t
                markdown-enable-math t))

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
  (add-to-list 'ispell-skip-region-alist '("^```" . "^```")))

;; ledger-beancount
(use-package beancount
  :straight (:host github :repo "beancount/beancount-mode")
  :magic ("%beancount" . beancount-mode)
  :config
  (defun mar-beancount-align ()
    "Align postings under the point's paragraph."
    (interactive)
    (let ((a (point-min))
          (b (point-max)))
      (beancount-align-numbers a b 60)))
  (defun mar-beancount-check ()
    "Runs a check using the primary file."
    (interactive)
    (let ((compilation-read-command nil))
      (beancount--run beancount-check-program
                      (file-relative-name "main.beancount"))))
  (evil-define-key nil beancount-mode-map
    (kbd "C-c C-a") 'mar-beancount-align
    (kbd "C-c l") 'mar-beancount-check))

;; LaTex - BibTex (also see lsp-mode)
(use-package ink
  :straight (ink :type git :host github :repo "foxfriday/ink")
  :commands (ink-make-figure ink-edit-figure))

(use-package bibtex
  :config
  (setq bibtex-align-at-equal-sign t
        bibtex-entry-format t))

(use-package ivy-bibtex
  :config
  ;; paths
  (setq bibtex-completion-bibliography "~/Dropbox/lib/bib/master.bib"
        bibtex-completion-library-path (list "~/Dropbox/lib/pdf/econ"
                                             "~/Dropbox/lib/pdf/hist"
                                             "~/Dropbox/lib/pdf/prog"
                                             "~/Dropbox/lib/pdf/psyc"
                                             "~/Dropbox/lib/pdf/stat")
        bibtex-completion-notes-path "~/Dropbox/notes/books/")
  ;; completion
  (setq bibtex-completion-pdf-extension '(".pdf" ".epub")
        bibtex-completion-pdf-symbol "#"
        bibtex-completion-notes-symbol "*"
        bibtex-completion-notes-extension ".md"
        bibtex-completion-notes-template-multiple-files "# ${title} #\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-format-citation-functions
        '((latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  ;; open with
  (defun mar-bibtex-open-function (fpath)
    (let* ((extension (file-name-extension fpath)))
      (cond ((string= extension "pdf") (call-process "xdg-open" nil 0 nil nil fpath))
            ((string= extension "epub") (call-process "xdg-open" nil 0 nil nil fpath))
            (t (find-file fpath)))))
  (setq bibtex-completion-pdf-open-function 'mar-bibtex-open-function))

(use-package org
  :init
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'counsel-org-capture)
  (global-set-key (kbd "C-c h") 'counsel-org-agenda-headlines)
  (defvar mar-org-todo "~/Dropbox/notes/agenda/todo.org")
  :config
  (setq org-pretty-entities t
        org-todo-keywords ;; ! time stamp @ note with time stamp
        '((sequence "TODO(t)" "MEET(m)" "WAIT(w!@)" "|" "DONE(d!@)"))
        org-agenda-files '("~/Dropbox/notes/agenda")
        org-agenda-window-setup 'current-window
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
        org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2))
        org-clock-out-when-done t
        org-clock-out-remove-zero-time-clocks t
        org-clock-report-include-clocking-task t
        org-deadline-warning-days 7)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline mar-org-todo "Tasks")
           "* TODO %^{TODO} \n%U\n%?" :empty-lines-after 1)
          ("n" "Now" entry (file+headline mar-org-todo "Tasks")
           "* TODO %^{TODO}\n%U\n%?" :clock-in t :clock-keep t :empty-lines-after 1)
          ("m" "Meeting" entry (file+headline  mar-org-todo "Meetings")
           "* %U: With %^{WITH}\n\n%?" :clock-in t :clock-resume t :empty-lines-after 1))))

(use-package org-pomodoro
  :config
  (setq org-pomodoro-manual-break t
        org-pomodoro-time-format "%.2m"))
;; init.el ends here
