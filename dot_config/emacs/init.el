;;; init.el --- Pepper's Emacs World -*- lexical-binding: t; -*-
;;; Commentary:
;;;    This is my own configuration.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))

;; Ensure that use-package always installs missing packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; All of the emacs-wide stuff to set up from the start.
(use-package emacs
  :defines (crm-separator treesit-language-source-alist)
  :functions (crm-indicator)
  :hook (emacs-lisp-mode . (lambda() (show-paren-mode 1)))
  :hook (minibuffer-setup . cursor-intangible-mode)
  :load-path ("~/.config/emacs/lisp")
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
      (replace-regexp-in-string
       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Set zsh as my shell
  (setenv "SHELL" "/bin/zsh")
  ;; Load the custom file emacs manages.
  (load custom-file 'noerror 'nomessage)
  ;; Set up treesitter languages
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-gomod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
		 (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  ;; Turn on syntax colouring in all modes supporting it
  (global-font-lock-mode t)
  ;; Turn on column number mode
  (column-number-mode t)
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode t)
  ;; Add line numbers to every buffer
  (global-display-line-numbers-mode t)
  ;; Set the default font for emacs
  (let ((font-name "JetbrainsMonoNL Nerd Font Mono")
  (font-size 18))
    (when (find-font (font-spec :name font-name))
      (set-face-attribute 'default nil :family font-name :height (* font-size 10))))
  ;; Start a emacs buffer server.
  (server-start)
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Let's standardize on 80 columns
  (fill-column 80)
  ;; Don't add two spaces after a period.
  (sentence-end-double-space nil)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Make buffer names more useful
  (uniquify-buffer-name-style 'post-forward)
  ;; Make zsh a login shell.
  (explicit-zsh-args (append '("-l")))
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Disable backup~ files.
  (make-backup-files nil)
  ;; Move customization variables to a separate file
  (custom-file (locate-user-emacs-file "custom-vars.el"))
  ;; Always add a final-newline to files.
  (require-final-newline t)
  ;; Turn on all the treesit fanciness
  (treesit-font-lock-level 4)
	;; Initial scratch buffer message
	(initial-scratch-message
      ";; -*- lexical-binding: t -*-
;; Hey, Pepper, you've got this. Let's code up a storm."))

;; Let's not mess around. Grep faster.
(use-package ripgrep)

(use-package exec-path-from-shell
	:commands (exec-path-from-shell-initialize)
	:init
	(exec-path-from-shell-initialize))

(use-package flycheck-projectile
	:after projectile
	:defer t)

(use-package flycheck-eglot
	:commands (global-flycheck-eglot-mode)
  :after (flycheck eglot)
	:custom (flycheck-eglot-exclusive nil)
	:defines (flycheck-checkers)
  :init
  (global-flycheck-eglot-mode 1))

(use-package flycheck-golangci-lint
	:after flycheck-eglot)

;; Enable vertical fuzzy completion in the minibuffer
(use-package vertico
  :commands (vertico-mode)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Set up a completion style that matches by words in any order.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable integration between embark and consult
(use-package embark-consult)

;; Enable location-specific actions
(use-package embark)

;; Enable rich annotations
(use-package marginalia
  :commands (marginalia-mode)
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Enable deep project-specific information
(use-package projectile
  :preface
  :defines (projectile-mode-map)
  :commands (projectile-mode global-flycheck-mode)
  :init
  (projectile-mode t)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/dev/github.com/" . 2)))
  (projectile-enable-caching t))

;; Install and enable Flycheck
(use-package flycheck
  :hook (flycheck-mode . elj-disable-flymake)
  :init
  (defun elj-disable-flymake ()
    (flymake-mode -1))
  (global-flycheck-mode))

;; Enable the best completion framework.
(use-package consult
  :commands (consult-customize
       consult-register-format
       consult-register-window
       consult-xref
       projectile-project-root)
  :preface
  (defvar consult--source-bookmark)
  (defvar consult--source-file-register)
  (defvar consult--source-recent-file)
  (defvar consult--source-project-recent-file)
  (defvar consult-project-function)
  (defvar xref-show-definitions-function)
  (defvar xref-show-xrefs-function)
  (defvar consult-bookmark)
  (defvar consult-git-grep)
  (defvar consult-grep)
  (defvar consult-narrow-key)
  (defvar consult-recent-file)
  (defvar consult-ripgrep)
  (defvar consult-theme)
  (defvar consult-xref)
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-fd)                    ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
  register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package corfu
  :commands global-corfu-mode
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package windresize)

(use-package gnu-elpa-keyring-update)

(use-package catppuccin-theme
	:commands (catppuccin-load-flavor)
  :init
  (load-theme 'catppuccin :no-confirm)
	(catppuccin-load-flavor `macchiato)
  (set-face-attribute 'font-lock-variable-use-face nil :foreground "#8aadf4" :italic nil)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#ed8796" :italic t))

(use-package diffview)

(use-package eglot
  :commands (eglot-format-buffer)
  :init
  (defun eglot-format-buffer-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
	:config
  (add-to-list 'eglot-server-programs
               '(zig-ts-mode . ("zls"))))

(use-package dape)

(use-package vterm
  :defines (vterm-eval-cmds)
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

(use-package markdown-mode
	:mode ("README\\.md\\'" . gfm-mode)
	:mode ("LICENSE\\.md\\'" . gfm-mode)
	:custom
	(markdown-command (concat user-emacs-directory "/bin/flavor.rb")))

(use-package cmake-ts-mode
  :mode "Makefile\\'")

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'")

(use-package yaml-ts-mode
  :mode "\\.yml\\'")

(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :mode ("go.mod\\'" . go-dot-mod-mode)
	:functions flycheck-add-next-checker
	:hook (go-ts-mode . (lambda ()
												(flycheck-add-next-checker 'eglot-check 'go-gofmt t)))
	:hook (go-ts-mode . flycheck-golangci-lint-setup)
  :hook (go-ts-mode . eglot-format-buffer-before-save)
  :hook (go-ts-mode . copilot-mode)
  :hook (go-ts-mode . eglot-ensure)
  :init
  :custom
  (tab-width 2)
  (go-ts-mode-indent-offset 2)
  :config
  (put 'go-ts-mode-build-tags 'safe-local-variable #'listp))

(use-package zig-mode
	:mode "\\.zig\\'"
	:hook (zig-mode . eglot-ensure)
  :hook (zig-mode . eglot-format-buffer-before-save)
  :hook (zig-mode . copilot-mode))
	

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure)
  :hook (rust-ts-mode . eglot-format-buffer-before-save))

(use-package solidity-mode)

(use-package nerd-icons-corfu)

(use-package nerd-icons
  :custom
	(nerd-icons-scale-factor 0.9))

(use-package nerd-icons-completion
  :after marginalia
  :commands (nerd-icons-completion-mode
       nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired)

(use-package diff-hl
  :commands (global-diff-hl-mode diff-hl-margin-local-mode)
	:hook (diff-hl-mode-on . (lambda ()
														 (unless (window-system)
															 (diff-hl-margin-local-mode))))
	:init
	(global-diff-hl-mode t)
	:custom
	(diff-hl-flydiff-mode t))

(use-package doom-modeline
  :commands (doom-modeline-mode)
  :init
  (doom-modeline-mode t)
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-vcs-max-length 21))

(use-package kkp
  :commands (global-kkp-mode)
  :init
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package copilot
   :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
   :defines (copilot-completion-map copilot-indentation-alist)
   :bind (:map copilot-completion-map
               ("<tab>" . copilot-accept-completion)
               ("TAB" . copilot-accept-completion))
   :config
   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
   (add-to-list 'copilot-indentation-alist '(org-mode 2))
   (add-to-list 'copilot-indentation-alist '(text-mode 2))
   (add-to-list 'copilot-indentation-alist '(closure-mode 2))
   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package chezmoi
  :bind ("C-c C f" . chezmoi-find)
  :bind ("C-c C s" . chezmoi-write))
;;; init.el ends here
