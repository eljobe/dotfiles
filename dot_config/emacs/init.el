;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load-path with packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define System Identificaiton Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun system-match (system)
  "Matches system against the system-configuration variable."
  (if (string-match system system-configuration)
      't))

(defun is-osx ()
  (system-match "apple-darwin"))

(defun is-linux ()
  (system-match "linux-gnu"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Machine-Specific Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond ((is-osx)
       (message "Loading OS X specific customizations...")
       ;; Make the command and control key not go to the system.
       (setq mac-pass-command-to-system nil)
       (setq mac-pass-control-to-system nil)

       ;; Make it to where the terminal emulator will play nicely.
       (set-terminal-coding-system 'utf-8)
       (set-keyboard-coding-system 'utf-8)
       (prefer-coding-system 'utf-8)

       ;; Make shell mode equivilant to a login shell.
       (setenv "SHELL" "/bin/zsh")
       (setq explicit-zsh-args (append '("-l"))))
      ((is-linux)
       (message "Loading Linux specific customizations...")
       ;; Set up the keyboard so the delete key on both the regular keyboard
       ;; and the keypad delete the character under the cursor and to the right
       ;; under X, instead of the default, backspace behavior.
       (global-set-key [delete] 'delete-char)
       (global-set-key [kp-delete] 'delete-char)

       ;; enable visual feedback on selections
       (setq-default transient-mark-mode t))
      (t
       (message "unrecognized version!")))

;;;;;;;;;;;;;;;;;;;;;
;; Enable narrowing
;;;;;;;;;;;;;;;;;;;;;
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the kill-filename function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-filename()
 (interactive)
 (kill-new (buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deal with nasty whitespace better
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq require-final-newline t)
(defvar elj-delete-trailing-space t)
(make-variable-buffer-local 'elj-delete-trailing-space)
(defun elj-keep-trailing-space-hook()
  (setq elj-delete-trailing-space nil))
(add-hook 'before-save-hook
  (lambda () (when elj-delete-trailing-space
	       (delete-trailing-whitespace)
	       (message "Deleted Trailing Space."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the toggel-print-faces function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq pepper-print-faces nil)
(setq default-face-fg (face-foreground 'default))
(setq default-face-bg (face-background 'default))
(defun toggle-print-faces()
  (interactive)
  (defvar ps-build-face-reference)
  (cond (pepper-print-faces
	 (set-face-foreground 'default default-face-fg)
	 (set-face-background 'default default-face-bg))
	((not pepper-print-faces)
	 (set-face-foreground 'default "black")
	 (set-face-background 'default "white")))
  (setq ps-build-face-reference "t")
  (setq pepper-print-faces (not pepper-print-faces)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Buffer Names More Useful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the pepper-print function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pepper-print()
  (interactive)
  (toggle-print-faces)
  (ps-print-buffer-with-faces)
  (toggle-print-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the shell-recenter function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shell-recenter()
  (interactive)
  (local-set-key "" #'(lambda () (interactive) (recenter 2))))

;;;;;;;;;;;;;;;;;
;; Rails Module
;;;;;;;;;;;;;;;;;
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
   '(try-complete-abbrev
     try-complete-file-name
     try-expand-dabbrev))

;;;;;;;;;;;;;;
;; java-mode
;;;;;;;;;;;;;;
(add-hook 'java-mode-hook (lambda ()
  (interactive)
  (defvar c-basic-offset)
  (defvar indet-tabs-mode)
  (setq c-basic-offset 2)
  (setq indet-tabs-mode nil)
  (setq fill-column 100)))

;;;;;;;;;;;;;;;;;;;;;;
;; Set Up Mode Hooks
;;;;;;;;;;;;;;;;;;;;;;

; Turn on syntax colouring in all modes supporting it
(global-font-lock-mode t)

(require 'font-lock)

(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (show-paren-mode 1)))

(add-hook 'shell-mode-hook 'shell-recenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f6]
		'kill-filename)

(global-set-key [f5]
		#'(lambda () (interactive) (revert-buffer t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default fill-column to 80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fill-column 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config gnu-elpa-keyring-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gnu-elpa-keyring-update
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config diffview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diffview
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config vterm
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :ensure t
  :defer t
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd"
	  (lambda (path)
	    (setq default-directory path))))
  (setq vterm-shell "/bin/zsh -l")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 4096)
  (add-hook 'vterm-mode-hook
    (lambda ()
      (set (make-local-variable 'buffer-face-mode-face) '(:family "MesloLGS NF"))
      (buffer-face-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config multi-vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multi-vterm
  :ensure t
  :bind (("C-c t" . multi-vterm-next)
	 ("C-c T" . multi-vterm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-command (concat user-emacs-directory "/bin/flavor.rb"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'elj-keep-trailing-space-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and config paradox
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paradox
  :ensure t
  :defer t
  :init
  (setq paradox-github-token       "824960c8349032fcf4195ffaf87a6fd98d014fe5"
        paradox-automatically-star nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and configure magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and configure groovy-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package groovy-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and configure go-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :ensure t
  :defer t
  :init
  (defun elj-go-mode-hook()
    (setq tab-width 2))
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'elj-go-mode-hook)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use and configure rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the crux package for extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crux
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the chezmoi package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package chezmoi
  :ensure t
  :init
  (global-set-key (kbd "C-c C f")  #'chezmoi-find)
  (global-set-key (kbd "C-c C s")  #'chezmoi-write))

;; Load my favorite new theme
(load-theme 'tango-dark t)

;; Turn on column number mode
(column-number-mode 1)

;; Turn off the tool-bar
(tool-bar-mode 0)

;; Disable backup~ files.
(setq make-backup-files nil)

;; Don't show that splash screen
(setq inhibit-startup-message t)

;; big shell history!
(setq comint-input-ring-size 500)

;; Make shell mode hide password prompts
(setq comint-output-filter-functions '(comint-watch-for-password-prompt))

;; Start a emacs buffer server.
(server-start)

;; Something about the new way emacs is compiled
;; means that it just hard-codes the exec-path.
(setq load-path (cons "~/.config/emacs/lisp" load-path))
(load-library "path-fix")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(chezmoi yaml-mode rust-mode paradox multi-vterm markdown-mode magit groovy-mode go-mode gnu-elpa-keyring-update diffview crux)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
