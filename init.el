(add-to-list 'default-frame-alist '(font . "Dank Mono-14"))

(setq-default auto-composition-mode t)
(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)

(setq display-line-numbers-type 'relative)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(ido-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'github-dark t)

(use-package which-key
    :config (which-key-mode 1)
    :ensure t)

(use-package autothemer
  :ensure t)

(use-package company
  :config (setq company-idle-delay 0.0
		company-minimum-prefix-length 1)
  :ensure t)

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . flymake-show-buffer-diagnostics)
              ("C-c r" . eglot-rename))
  :config (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook 
  (c-mode . eglot-ensure)
  (c++mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :ensure t)

(use-package rust-mode
  :init (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-ts-mode . company-mode)
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
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . company-mode))

(use-package vertico
  :config (vertico-mode 1)
  :ensure t)

(use-package marginalia
  :config (marginalia-mode 1)
  :ensure t)

;; require MELPA
;; ONLY add melpa packages after this line
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package highlight-indent-guides
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "gray")
  (setq highlight-indent-guides-method 'character)
  :ensure t)
