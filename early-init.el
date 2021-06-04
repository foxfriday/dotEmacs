;;; early-init.el -- loaded before init.el on Emacs 28

;;; Commentary:
;; Do some things to speed initiation.

;;; Code:
;; Defer garbage collection
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent package.el loading packages prior to their init-file
;; loading
(setq package-enable-at-startup nil)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Compile packages when they are installed, and site files when
;; gccemacs is installed.
(setq comp-deferred-compilation nil)

;; Keep a clean home
(setq user-emacs-directory "~/.config/emacs/")

;; Keep a clean screen
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
