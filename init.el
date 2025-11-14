(setq display-line-numbers-type 'relative)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'github-dark t)
(add-to-list 'default-frame-alist '(font . "Dank Mono-14"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(ido-mode)
(desktop-save-mode 1)

(use-package which-key
    :config (which-key-mode)
    :ensure t)
(use-package autothemer :ensure t)
(use-package company :ensure t)
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . flymake-show-buffer-diagnostics)
              ("C-c r" . eglot-rename))
  :config (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook (
	 (c-mode . eglot-ensure)
	 (c++mode . eglot-ensure)
	 (python-mode . eglot-ensure))
  :ensure t)
(use-package vertico
  :config (vertico-mode 1)
  :ensure t)
(use-package marginalia
  :config (marginalia-mode 1)
  :ensure t)
