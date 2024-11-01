;; Don't show that splash screen
(setq inhibit-startup-message t)

;; Don't show the titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Turn off the menu bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Turn off the tool-bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Turn off the scroll-bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Temporarily increase the garbage collection threshold
(setq gc-cons-threshold most-positive-fixnum)
;; Reset it after startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000)))
