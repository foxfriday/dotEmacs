;; All in .config
(setq user-emacs-directory "~/.config/emacs/")

;; Speed emacs and lsp
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Straight
(setq load-prefer-newer t)
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

;; Emacs Defaults
(use-package emacs
 :straight (:type built-in)
 :init
  ;; preffered settings
  (setq-default indent-tabs-mode nil
                ring-bell-function 'ignore
                line-spacing 4
                initial-scratch-message ";; scratch buffer\n"
                frame-resize-pixelwise t)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode t)
  (auth-source-pass-enable)
  (setq auto-save-default nil
        make-backup-files nil)
  (setq x-select-enable-clipboard t)
  (setq epa-pinentry-mode 'loopback)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; major text mode
  (add-hook 'text-mode-hook
            (lambda ()
              (setq prettify-symbols-unprettify-at-point 'right-edge)
              (prettify-symbols-mode)
              (flyspell-mode)
              (visual-line-mode)
              (setq fill-column 80)))

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  (define-key global-map "\M-Q" 'unfill-paragraph)

  ;; major prog modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; system
  (defconst mar-on-mac (eq system-type 'darwin)
    "Working on a mac.")

  ;; Custom
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (if (file-exists-p custom-file) (load custom-file))

  ;; don't show the tabs on top
  (setq tab-bar-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")

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
  ;; sentence navigation
  (setf sentence-end-double-space nil)
  (define-key evil-motion-state-map ")" 'evil-forward-sentence-begin)
  (define-key evil-motion-state-map "(" 'evil-backward-sentence-begin)
  ;; e.g. das (outer) and dis (inner)
  (define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
  ;; windows bindings
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
  ;; manage buffer bindings, defined in Emacs
  (define-key my-leader-map "kb" 'mar-close-this-buffer)
  (define-key my-leader-map "ko" 'mar-close-other-buffer)
  (define-key my-leader-map "kB" 'mar-close-and-kill-this-pane)
  (define-key my-leader-map "kO" 'mar-close-and-kill-other-pane)
  (define-key my-leader-map "kt" 'mar-cleanup-tramp-buffers)
  (define-key my-leader-map "kd" 'mar-cleanup-dired-buffers)
  ;; line numbers
  (evil-ex-define-cmd "nu[mber]" 'display-line-numbers-mode)
  ;; tabs
  (evil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabclose" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabonly" 'tab-bar-close-other-tabs)
  (evil-ex-define-cmd "tabrename" 'tab-bar-rename-tab)
  ;; no fuzz mode line
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
  (setq mode-line-format mar-mode-line-format))

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
  (define-key my-leader-map "W" 'avy-goto-char-2))

(use-package evil-matchit
  :init (global-evil-matchit-mode 1))

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
    (let* ((counsel-rg-base-command "rg --with-filename --no-heading --color never %s"))
      (counsel-rg nil "~/Dropbox/md/")))
  ;; help
  (define-key my-leader-map "hv" 'counsel-describe-variable)
  (define-key my-leader-map "hf" 'counsel-describe-function)
  (define-key my-leader-map "hl" 'counsel-find-library)
  (define-key my-leader-map "hu" 'counsel-unicode-char)
  (define-key my-leader-map "hb" 'counsel-descbinds)
  (define-key my-leader-map "ha" 'counsel-apropos)
  ;; search/switch
  (define-key my-leader-map "sf" 'project-find-file)
  (define-key my-leader-map "sm" 'counsel-mark-ring)
  (define-key my-leader-map "sb" 'counsel-bookmark)
  (define-key my-leader-map "sd" 'mar-rg-directory)
  (define-key my-leader-map "sg" 'counsel-git-grep)
  (define-key my-leader-map "sn" 'mar-rg-notes)
  (unless mar-on-mac  (define-key my-leader-map "sa" 'counsel-linux-app))
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
  (setq ivy-use-virtual-buffers t)    ;; recent files in switch buffer
  (setq ivy-count-format "(%d/%d) ")  ;; total results in prompt
  (setq ivy-height 30)
  ;; minibuffer navigation
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-partial-or-done)
  (define-key my-leader-map "b" 'ivy-switch-buffer)
  (define-key my-leader-map "/" 'swiper-isearch))

;; ivy plugins
(use-package ivy-prescient
  :init (ivy-prescient-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

;; Edit search results
;; 1) Use counsel-rg or counsel-git-grep
;; 2) Use C-c C-o to enter ivy-occur mode, followed by 'w'
;; 3) press C-x C-s to save or C-x C-k to abort.
(use-package wgrep)

(use-package ivy-pass)

;; Manage Very Large Files
(use-package vlf
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
  (evil-define-key 'insert company-mode-map
    (kbd "C-c c") 'company-yasnippet)
  (setq company-show-numbers t
        company-selection-wrap-around t
        company-dabbrev-other-buffers t))

(use-package flycheck
  :hook ((prog-mode lsp-mode) . flycheck-mode))

(use-package lsp-mode
  :hook ((python-mode sh-mode tex-mode latex-mode) . lsp-deferred)
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil)

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
  ;; Backward search with Zathura C-<mouse 1>
  (defun lsp-latex-forward-search ()
    "Forward search on preview."
    (interactive)
    (lsp-request-async "textDocument/forwardSearch"
                       (lsp--text-document-position-params)
                       #'lsp-latex-msg))

  (add-hook 'latex-mode-hook (lambda ()
                               (define-key latex-mode-map (kbd "C-c C-c")
                                 'lsp-latex-build)
                               (define-key latex-mode-map (kbd "C-c C-v")
                                 'lsp-latex-forward-search)))

  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_black.enabled" t t)
     ("latex.build.executable" "latexmk")
     ("latex.build.args"  ["-pdf" "-interaction=nonstopmode" "-synctex=1" "-pvc" "%f"])
     ("latex.forwardSearch.executable" "zathura")
     ("latex.forwardSearch.args" ["--synctex-forward" "%l:1:%f" "%p"])
     ("latex.lint.onChange" t t)  ;; lint using chktex
     ("bibtex.formatting.lineLength" 80)
     ("bibtex.formatting.formatter" "texlab"))))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package ccls
  :hook (c-mode-common . (lambda () (require 'ccls) (lsp)))
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil
        ccls-sem-highlight-method 'font-lock))

(use-package olivetti
  :hook (text-mode . olivetti-mode)
  :config
  (defun mar-olivetti-recenter ()
    "Toggle olivetti from relative to absolute size."
    (interactive)
    (setq olivetti-body-width (if (< olivetti-body-width 1.0) 80 0.8))
    (redraw-frame))
  (setq olivetti-body-width 80))

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
                       (if (< (x-display-pixel-height) 3300) 140 220)
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

(use-package imenu-list
  :hook (imenu-list-major-mode . evil-emacs-state)
  :commands (imenu-list-smart-toggle)
  :init (define-key my-leader-map "to" 'imenu-list-smart-toggle))

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

(use-package markdown-mode
  :mode (("README.md" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook (lambda ()
                                  (prettify-symbols-mode nil)
                                  (push '("\\leq" . 8804) prettify-symbols-alist)
                                  (push '("\\neq" . 8800) prettify-symbols-alist)
                                  (push '("\\geq" . 8805) prettify-symbols-alist)
                                  (push '("\\alpha" . 945) prettify-symbols-alist)
                                  (push '("\\beta" . 946) prettify-symbols-alist)
                                  (push '("\\gamma" . 947) prettify-symbols-alist)
                                  (push '("\\delta" . 948) prettify-symbols-alist)
                                  (push '("\\epsilon" . 1013) prettify-symbols-alist)
                                  (prettify-symbols-mode t)))
  :config
  (setq-default markdown-asymmetric-header t
                markdown-fontify-code-blocks-natively t
                markdown-enable-math t))

(use-package python
  :hook (inferior-python-mode . evil-emacs-state)
  :config (setq python-indent-guess-indent-offset nil))

(use-package dired
  :straight (:type built-in)
  :init (if mar-on-mac (setq dired-use-ls-dired nil))
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq delete-by-moving-to-trash nil
        dired-dwim-target t)
  (evil-define-key 'normal dired-mode-map
    ";s" 'dired-sort-toggle-or-edit ;; sort by name or date
    ";h" 'dired-hide-details-mode ;; show/hide details
    ";f" 'dired-show-file-type
    "za" 'dired-subtree-toggle
    "r" 'revert-buffer
    ;; Actions
    ";cp" 'dired-do-copy ;; if multiple files, to new dir
    ";mv" 'dired-do-rename ;; if multiple files, move to dir
    "+" 'dired-create-directory
    ";z" 'dired-do-compress-to ;; compress to dir (name needs extension)
    ;; Mark
    "d" 'dired-flag-file-deletion
    "m" 'dired-mark
    "u" 'dired-unmark
    "U" 'dired-unmark-all-marks
    "q" 'kill-this-buffer))

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
  (setq bibtex-completion-bibliography "~/Dropbox/lib/bib/master.bib"
        bibtex-completion-library-path (list "~/Dropbox/lib/pdf/econ"
                                             "~/Dropbox/lib/pdf/hist"
                                             "~/Dropbox/lib/pdf/prog"
                                             "~/Dropbox/lib/pdf/psyc"
                                             "~/Dropbox/lib/pdf/stat")
        bibtex-completion-notes-path "~/Dropbox/md/books/")
  ;; completion
  (setq bibtex-completion-pdf-extension '(".pdf" ".epub")
        bibtex-completion-pdf-symbol "#"
        bibtex-completion-notes-symbol "*"
        bibtex-completion-notes-extension ".md"
        bibtex-completion-notes-template-multiple-files "# ${title}\n"
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
