(setq display-line-numbers-type 'relative)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'github-dark t)

(set-face-attribute 'default nil
  :font "Dank Mono" ;; font name
  :height 120)      ;; font size (1/10 pt units: 120 = 12pt)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(ido-mode)
