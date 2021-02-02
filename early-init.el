(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
