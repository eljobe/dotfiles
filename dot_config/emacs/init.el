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

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :config
  (put 'narrow-to-region 'disabled nil)
  ;; Make buffer names more useful
  (setq uniquify-buffer-name-style 'post-forward)
  (setq fill-column 80)
  ;; Make zsh a login shell.
  (setq explicit-zsh-args (append '("-l")))
  ;; Set the default font for emacs
  (let ((font-name "JetbrainsMonoNL Nerd Font Mono")
	(font-size 18))
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
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
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
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Enable vertical fuzzy completion in the minibuffer
(use-package vertico
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
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Enable deep project-specific information
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/dev/github.com/" . 2)))
  (projectile-enable-caching t))

;; Enable the best completion framework.
(use-package consult
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

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'")

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
  :config
  (put 'go-ts-mode-build-tags 'safe-local-variable #'listp))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(use-package nerd-icons-corfu)

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  (nerd-icons-font-family "JetbrainsMonoNL Nerd Font Mono"))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package demap)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-vcs-max-length 21))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode))

;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;            :rev :newest
;;            :branch "main")
;;  :hook (prog-mode copilot-mode)
;;  :config
;;  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package chezmoi
  :init
  (global-set-key (kbd "C-c C f")  #'chezmoi-find)
  (global-set-key (kbd "C-c C s")  #'chezmoi-write))
