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

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode)
    :ensure t)

(use-package autothemer :ensure t)
(use-package company :ensure t)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
