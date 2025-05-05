;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Font configuration
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; Theme
(setq doom-theme 'doom-one)
;; Fix for WSL file completion with Vertico/Marginalia
(when (getenv "WSL_DISTRO_NAME")
  ;; Use a more compatible completion style in WSL
  (setq completion-styles '(basic partial-completion emacs22 initials))

  ;; Disable some fancy completion features that might cause issues
  (after! vertico
    (setq vertico-cycle nil)
    (setq vertico-resize nil)
    (setq vertico-count 10))

  ;; Ensure marginalia annotations don't cause width calculation errors
  (after! marginalia
    (setq marginalia-field-width 80)
    (setq marginalia-align 'left))

  ;; Fix file-name-handler behavior in WSL
  (when (not (display-graphic-p))
    (setq file-name-handler-alist
          (remq (rassq 'file-name-non-special file-name-handler-alist)
                file-name-handler-alist))))

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Org directory
(setq org-directory "~/org/")

;; Make sure LSP starts with C++ mode
(add-hook 'c++-mode-hook #'lsp!)

;; Basic C++ indentation settings
(after! cc-mode
  (set-company-backend! 'c-mode
                        '(:separate company-irony-c-headers company-irony))
  ;; Add more specific highlighting
  (font-lock-add-keywords
   'c++-mode
   '(("\\([a-zA-Z0-9_]+\\)\\s-*(" 1 '(:foreground "#56b6c2" :weight bold))  ;; Function calls
     ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>\\s-*::" 1 '(:foreground "#e5c07b")) ;; Namespaces
     )))

(after! lsp-mode
  (setq lsp-clients-clangd-executable "clangd")

  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "--all-scopes-completion"
          "--completion-style=detailed"
          "--function-arg-placeholders=false"
          "--background-index"
          "--clang-tidy"
          "--suggest-missing-includes"
          "--pch-storage=memory"
          "--ranking-model=heuristics"))
  (setq lsp-ui-sideline-enable nil)

  ;; Enable flycheck with improved styling
  (setq lsp-diagnostics-provider :flycheck)

  ;; Configure lsp-ui-doc for hovering information
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)

  ;; Ensure parameter hints still work but are better styled
  (setq lsp-inlay-hint-enable t)
  (setq lsp-inlay-hints-parameters t)
  (setq lsp-inlay-hints-types nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t))

;; Configure flycheck to appear inline with colored backgrounds like Neovim
(after! flycheck
  ;; Make flycheck overlay warnings more visible
  (set-face-attribute 'flycheck-warning nil
                      :background "#3f3c2f"  ;; Dark yellow background
                      :foreground "#e7c547"  ;; Bright yellow text
                      :underline nil)        ;; No underline

  ;; Make flycheck overlay errors more visible
  (set-face-attribute 'flycheck-error nil
                      :background "#4b2c31"  ;; Dark red background
                      :foreground "#ff6c6b"  ;; Bright red text
                      :underline nil)        ;; No underline

  ;; Configure flycheck to show diagnostics inline
  (setq flycheck-indication-mode nil)        ;; Disable fringe indicators
  (setq flycheck-highlighting-mode 'lines)   ;; Highlight entire lines

  ;; Show diagnostics immediately
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.1)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; Style the inlay hints to be less intrusive
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#777777" :background nil :slant italic :height 0.9))
(after! company
  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; Setup clipboard on wayland
(cond
 ;; Fedora with Wayland
 ((and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda ()
          (let ((clipboard-text (shell-command-to-string "wl-paste -n 2>/dev/null")))
            (unless (string= clipboard-text "")
              clipboard-text))))))
;; Configure flycheck-inline for inline diagnostics
;; Simpler flycheck-inline configuration
(use-package! flycheck-inline
  :after flycheck
  :config
  (global-flycheck-inline-mode))

;; Improve diagnostics display
(after! flycheck
  ;; Use lines highlighting mode
  (setq flycheck-highlighting-mode 'lines)
  ;; Make errors display immediately
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.1)

  ;; Custom styling for errors/warnings
  (set-face-attribute 'flycheck-error nil
                      :background "#472a2a"
                      :foreground "#ff6c6b")

  (set-face-attribute 'flycheck-warning nil
                      :background "#3d3a2a"
                      :foreground "#ecbe7b"))
;; Make the type info hover display prettier
(after! lsp-ui
  ;; Configure hover card appearance
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-doc-max-height 20)
  (setq lsp-ui-doc-max-width 80)

  ;; Add some styling to the doc popup
  (custom-set-faces!
    '(lsp-ui-doc-background :background "#202030" :foreground nil)
    '(lsp-ui-doc-header :background "#3b3b4b" :foreground "#eeeeee" :height 0.9)
    '(lsp-ui-doc-url :foreground "#7788aa"))

  ;; Improve code example display in documentation
  (setq lsp-ui-doc-include-signature t))

;; Style the inlay hints (the parameter/type annotations)
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#5d8cc0" :background nil :slant italic :height 0.9 :inherit nil)
  '(lsp-inlay-parameter-hint-face :foreground "#6a9955" :background nil :slant italic :height 0.9)
  '(lsp-inlay-type-hint-face :foreground "#ce9178" :background nil :slant italic :height 0.9))
;; Configure EIN for better Jupyter notebook support
(after! ein
  ;; Basic EIN settings
  (setq ein:output-area-inlined-images t)
  (setq ein:slice-image t)

  ;; Use jupyter command rather than older ipython
  (setq ein:jupyter-default-server-command "jupyter")

  ;; Enable websocket support
  (setq ein:jupyter-server-use-containers nil)

  ;; Set default notebook directory
  (setq ein:jupyter-default-notebook-directory "~/notebooks")

  ;; Add keybindings for common operations
  (map! :map ein:notebook-mode-map
        :localleader
        "," #'ein:worksheet-execute-cell
        "." #'ein:worksheet-execute-cell-and-goto-next
        "b" #'ein:worksheet-insert-cell-below
        "a" #'ein:worksheet-insert-cell-above))
