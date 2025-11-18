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
  :init
  (vertico-mode 1)
  (setq vertico-cycle t)
  :ensure t
  :pin melpa)

(use-package marginalia
  :init (marginalia-mode 1)
  :ensure t
  :pin melpa)

(use-package orderless
  :custom (completion-styles '(orderless))
  :ensure t)

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
  (corfu-auto t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
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
      TeX-parse-self t)
  :ensure t)

(use-package reftex
  :ensure t
  :hook (LaTeX-mode . turn-on-reftex)
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction 0.2)
  :commands (reftex-mode turn-on-reftex reftex-citation reftex-index-phrase-mode))

;; please make sure to run (pdf-tools-install) after the first launch of emacs
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package tex
  :ensure auctex
  :hook
  ((LaTeX-mode . pdf-tools-install)
   (LaTeX-mode . TeX-fold-mode)
   (LaTeX-mode . auto-fill-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . latex-math-mode)
   (LaTeX-mode . outline-minor-mode)
   (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)) ;; Automatically revert PDF after compilation
  :init
  ;; General AUCTeX & PDF settings
  (setq TeX-show-compilation nil
        TeX-global-PDF-mode t
        TeX-clean-confirm nil
        TeX-save-query nil
        split-width-threshold 80 ;  pdf-tool to open a pdf in the right side
        pdf-view-use-scaling t
        flyspell-sort-corrections nil
        flyspell-doublon-as-error-flag nil
        font-latex-fontify-script t
        TeX-source-correlate-mode t
        TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex))
        TeX-source-correlate-start-server t ;; [C-c C-g] to switch between source code and PDF
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  :config
  ;; Keybindings
  (with-eval-after-load 'latex

    (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
    (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
    (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search))

  ;; TeX folding
  (setq TeX-fold-env-spec-list
        '(("[comment]" ("comment"))
          ("[figure]" ("figure"))
          ("[table]" ("table"))
          ("[itemize]" ("itemize"))
          ("[enumerate]" ("enumerate"))
          ("[description]" ("description"))
          ("[overpic]" ("overpic"))
          ("[tabularx]" ("tabularx"))
          ("[code]" ("code"))
          ("[shell]" ("shell"))))

  ;; Section hook
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))

  ;; LaTeX commands (must be after AUCTeX loaded)
  (setq TeX-command-default "LaTeX")
  (add-to-list 'TeX-command-list
               '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t)))

;; Example configuration for Consult
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
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
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
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  :ensure t
  :pin gnu)

(use-package project
  :custom
  (project-mode-line t)
  (project-kill-buffers-display-buffer-list t))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
