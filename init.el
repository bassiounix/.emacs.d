(use-package autothemer
  :ensure t)

(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "Dank Mono-14"))

  (setq-default auto-composition-mode t)
  (setq use-default-font-for-symbols nil
	inhibit-compacting-font-caches t
	custom-file (expand-file-name "custom.el" user-emacs-directory)
	display-line-numbers-type 'relative
	backup-directory-alist '(("." . "~/.emacs.d/backups")))

  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (global-display-line-numbers-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'github-dark t)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (load custom-file 'noerror)

  :custom
  (window-sides-vertical t)

  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package which-key
    :config (which-key-mode 1)
    :ensure t
    :pin gnu)

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . flymake-show-buffer-diagnostics)
              ("C-c r" . eglot-rename))
  :ensure t
  :pin gnu)

(use-package rust-mode
  :init (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :hook
  (rust-ts-mode . eglot-ensure)
  :config
  (add-to-list 'exec-path "~/.cargo/bin")
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  :ensure t)

(use-package python
  :init (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :bind (:map python-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook
  (python-ts-mode . eglot-ensure))

(use-package cc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  :config (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  :hook
  ((c-ts-mode c++-ts-mode) . eglot-ensure))

(use-package vertico
  :config (vertico-mode 1)
  :ensure t
  :pin melpa)

(use-package marginalia
  :config (marginalia-mode 1)
  :ensure t
  :pin melpa)

(use-package highlight-indent-guides
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "gray")
  (setq highlight-indent-guides-method 'character)
  :ensure t
  :pin melpa)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (vterm-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode 1)
  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  (corfu-popupinfo-mode)
  :ensure t
  :pin gnu)

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode 1)

  ;; Info buffers to the right
  ;; (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  ;; (dape-buffer-window-arrangement 'gud)
  ;; (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)
  :ensure t)

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode 1)
  :ensure t)

(use-package vterm
  :ensure t
  :init
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (require 'vterm)))

(use-package magit
  :ensure t)
