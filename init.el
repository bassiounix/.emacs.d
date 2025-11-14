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
    (which-key-mode))

(use-package autothemer)

;;(add-hook 'prog-mode-hook #'lsp-deferred)
;;(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp-deferred))
