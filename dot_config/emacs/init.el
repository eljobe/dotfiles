(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))

;; Ensure that use-package always installs missing packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; All of the emacs-wide stuff to set up from the start.
(use-package emacs
  :hook (emacs-lisp-mode . (lambda() (show-paren-mode 1)))
  :hook (before-save . (lambda()
			 (when elj-delete-trailing-space
			   (delete-trailing-whitespace))))
  :init
  ;; Prepare to deal with trailing spaces.
  (defvar elj-delete-trailing-space t)
  (make-variable-buffer-local 'elj-delete-trailing-space)
  (defun elj-keep-trailing-space-hook()
    (setq elj-delete-trailing-space nil))
  ;; Set zsh as my shell
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
  ;; Enable a vertical fuzzy command
  (fido-vertical-mode)
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
  ;; Always add a final-newline to files.
  (setq require-final-newline t)
  ;; Set up treesitter languages
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-gomod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
)

(use-package windresize)

(use-package gnu-elpa-keyring-update)

(use-package catppuccin-theme
  :init
  (load-theme 'catppuccin :no-confirm))

(use-package diffview)

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package vterm
  :hook (vterm-mode . (lambda ()
			(display-line-numbers-mode -1)
			(set (make-local-variable 'buffer-face-mode-face)
			     '(:family "JetbrainsMonoNL Nerd Font Mono"))
			(buffer-face-mode t)))
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd"
	  (lambda (path)
	    (setq default-directory path))))
  :custom
  (vterm-shell "/bin/zsh -l")
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 20000))

(use-package multi-vterm
  :bind (("C-c t" . multi-vterm-next)
	 ("C-c T" . multi-vterm)))

(use-package bazel
  :defer t)

(use-package markdown-ts-mode
  :hook (markdown-mode . elj-keep-trailing-space-hook)
  :custom
  (markdown-command (concat user-emacs-directory "/bin/flavor.rb")))

(use-package yaml-ts-mode
  :mode "\\.yml\\'")

(use-package magit
  :init
  (setq magit-define-global-key-bindings 'recommended))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :mode ("go.mod\\'" . go-dot-mod-mode)
  :init
  (defun elj-go-mode-hook()
    (setq tab-width 2))
  :hook (go-mode . elj-go-mode-hook)
  :hook (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(use-package chezmoi
  :init
  (global-set-key (kbd "C-c C f")  #'chezmoi-find)
  (global-set-key (kbd "C-c C s")  #'chezmoi-write))
