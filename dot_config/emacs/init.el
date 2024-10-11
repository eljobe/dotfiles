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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Buffer Names More Useful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;;
;; Set Up Mode Hooks
;;;;;;;;;;;;;;;;;;;;;;

; Turn on syntax colouring in all modes supporting it
(global-font-lock-mode t)

(require 'font-lock)

(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (show-paren-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default fill-column to 80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fill-column 80)

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package catppuccin-theme
  :ensure t
  :defer t
  :init
  (load-theme 'catppuccin :no-confirm))

(use-package diffview
  :ensure t)

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
      (display-line-numbers-mode -1)
      (set (make-local-variable 'buffer-face-mode-face) '(:family "JetbrainsMonoNL Nerd Font Mono"))
      (buffer-face-mode t))))

(use-package multi-vterm
  :ensure t
  :bind (("C-c t" . multi-vterm-next)
	 ("C-c T" . multi-vterm)))

(use-package bazel
  :defer t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-command (concat user-emacs-directory "/bin/flavor.rb"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'elj-keep-trailing-space-hook))

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package magit
  :ensure t
  :init
  (setq magit-define-global-key-bindings 'recommended))

(use-package groovy-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

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

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package crux
  :ensure t
  :defer t)

(use-package chezmoi
  :ensure t
  :init
  (global-set-key (kbd "C-c C f")  #'chezmoi-find)
  (global-set-key (kbd "C-c C s")  #'chezmoi-write))

;; Set the default font for emacs
(let ((font-name "JetbrainsMonoNL Nerd Font Mono")
      (font-size 16))
  (when (find-font (font-spec :name font-name))
    (set-face-attribute 'default nil :family font-name :height (* font-size 10))))

;; Turn on column number mode
(column-number-mode 1)

;; Turn off the tool-bar
(tool-bar-mode 0)

;; Turn off the menu bar
(menu-bar-mode 0)

;; Turn off the scroll-bar
(scroll-bar-mode 0)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Add line numbers to every buffer
(global-display-line-numbers-mode t)

;; Set up ERC defaults
;; Note to self invoke with:
;; C-u 2 M-x erc
(setq erc-server "irc.technodabble.com"
      erc-port 6697
      erc-nick "pepper"
      erc-use-tls t
      erc-user-full-name "Pepper")

;; Disable backup~ files.
(setq make-backup-files nil)

;; Don't show that splash screen
(setq inhibit-startup-message t)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Start a emacs buffer server.
(server-start)

;; Something about the new way emacs is compiled
;; means that it just hard-codes the exec-path.
(setq load-path (cons "~/.config/emacs/lisp" load-path))
(load-library "path-fix")
