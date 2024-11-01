(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))
;; Ensure that use-package always installs missing packages
(setq use-package-always-ensure t)

;; All of the emacs-wide stuff to set up from the start.
(use-package emacs
  :init
  (setenv "SHELL" "/bin/zsh")
  ;; Turn on syntax colouring in all modes supporting it
  (global-font-lock-mode t)
  ;; Turn on column number mode
  (column-number-mode 1)
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  ;; Add line numbers to every buffer
  (global-display-line-numbers-mode t)
  ;; Move customization variables to a separate file and load it
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  ;; Start a emacs buffer server.
  (server-start)
  ;; Something about the new way emacs is compiled
  ;; means that it just hard-codes the exec-path.
  (setq load-path (cons "~/.config/emacs/lisp" load-path))
  (load-library "path-fix")
  :config
  (put 'narrow-to-region 'disabled nil)
  ;; Make buffer names more useful
  (setq uniquify-buffer-name-style 'post-forward)
  (setq fill-column 80)
  ;; Make zsh a login shell.
  (setq explicit-zsh-args (append '("-l")))
  ;; Set the default font for emacs
  (let ((font-name "JetbrainsMonoNL Nerd Font Mono")
	(font-size 16))
    (when (find-font (font-spec :name font-name))
      (set-face-attribute 'default nil :family font-name :height (* font-size 10))))
  ;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t)
  ;; Disable backup~ files.
  (setq make-backup-files nil)
)

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
	       (delete-trailing-whitespace))))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (show-paren-mode 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only non use-package part remainig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windresize)

(use-package gnu-elpa-keyring-update)

(use-package catppuccin-theme
  :defer t
  :init
  (load-theme 'catppuccin :no-confirm))

(use-package diffview)

(use-package vterm
  :defer t
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd"
	  (lambda (path)
	    (setq default-directory path))))
  (setq vterm-shell "/bin/zsh -l")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 20000)
  (add-hook 'vterm-mode-hook
    (lambda ()
      (display-line-numbers-mode -1)
      (set (make-local-variable 'buffer-face-mode-face) '(:family "JetbrainsMonoNL Nerd Font Mono"))
      (buffer-face-mode t))))

(use-package multi-vterm
  :bind (("C-c t" . multi-vterm-next)
	 ("C-c T" . multi-vterm)))

(use-package bazel
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command (concat user-emacs-directory "/bin/flavor.rb"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'elj-keep-trailing-space-hook))

(use-package yaml-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package magit
  :init
  (setq magit-define-global-key-bindings 'recommended))

(use-package groovy-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(use-package go-mode
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
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package crux
  :defer t)

(use-package chezmoi
  :init
  (global-set-key (kbd "C-c C f")  #'chezmoi-find)
  (global-set-key (kbd "C-c C s")  #'chezmoi-write))
