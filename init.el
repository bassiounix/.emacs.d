(use-package autothemer
  :ensure t)

(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "Dank Mono-14"))

  (setq-default auto-composition-mode t
		TeX-master nil)
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

  (dolist (mode '(doc-view-mode-hook
                  image-mode-hook
                  pdf-view-mode-hook)) ; if using pdf-tools
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package auctex
  :config
  (setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil  ;; Don't ask to save before compiling
      TeX-show-compilation t
      TeX-parse-self t)
  :ensure t)

(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-toc-split-windows-horizontally t)                       ; *toc*buffer on left。
(setq reftex-toc-split-windows-fraction 0.2)                         ; *toc*buffer ratio。
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(add-hook 'LaTeX-mode-hook (lambda ()
                             (pdf-tools-install)
                             (require 'tex-site)
                             (setq pdf-view-use-scaling t)
                             (TeX-fold-mode 1)
                             (auto-fill-mode 1)

                             (flyspell-mode 1)
                             (setq flyspell-sort-corrections nil)
                             (setq flyspell-doublon-as-error-flag nil)

                             (setq split-width-threshold 80) ;  pdf-tool to open a pdf in the right side
                             (turn-on-auto-fill)             ; LaTeX mode，turn off auto fold
                             (latex-math-mode 1)
                             (outline-minor-mode 1)
                             ;;(imenu-add-menubar-index)

                             (setq TeX-show-compilation nil) ; NOT display compilation windows
                             (setq TeX-global-PDF-mode t)    ; PDF mode enable, not plain
                             ;;(setq TeX-engine 'default)      ; use xelatex default
                             (setq TeX-clean-confirm nil)
                             (setq TeX-save-query nil)

                             (setq font-latex-fontify-script t)
                             (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
                             ;;(setq TeX-electric-escape t)      ; press \ then, jump to mini-buffer to input commands
                             ;;(setq TeX-view-program-list '(("Evince" "evince %o"))) ;;
                             ;;(setq TeX-view-program-selection '((output-pdf "Evince")))
                             (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                                   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                                   TeX-source-correlate-start-server t)
                             ;;(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             ;;(setq TeX-command-default "XeLaTeX")
                             (add-to-list 'TeX-command-list '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "LaTeX")
                             ;;(setq TeX-command-default "pdflatex --synctex=1")

                             (setq TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[figure]" ("figure")) ("[table]" ("table"))("[itemize]"("itemize"))("[enumerate]"("enumerate"))("[description]"("description"))("[overpic]"("overpic"))("[tabularx]"("tabularx"))("[code]"("code"))("[shell]"("shell")))))


                             (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
                             (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)

                             (setq LaTeX-section-hook
                                   '(LaTeX-section-heading
                                     LaTeX-section-title
                                     LaTeX-section-toc
                                     LaTeX-section-section
                                     LaTeX-section-label))

                             (setq pdf-sync-backward-display-action t
                                   pdf-sync-forward-display-action t
                                   TeX-source-correlate-mode t
                                   TeX-source-correlate-method '(
                                                                 (dvi . source-specials)
                                                                 (pdf . synctex))
                                   TeX-source-correlate-start-server t  ; [C-c C-g] to switch between source code and PDF
                                   reftex-plug-into-AUCTeX t)
                             (add-hook 'TeX-after-compilation-finished-functions
                                       #'TeX-revert-document-buffer) ;
                             (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
                             ))
