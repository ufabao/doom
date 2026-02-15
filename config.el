;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This file contains all custom configurations for Doom Emacs
;; It includes font settings, themes, LSP configuration, and various plugins

;; ===== APPEARANCE CONFIGURATION =====
;; Font settings - using JetBrains Mono with Nerd Font support for icons
(setq doom-font (font-spec
                 :family "JetBrainsMono Nerd Font"
                 :size 16
                 :weight 'regular))

;; Color theme - Tokyo Night provides a dark, modern appearance
;; (setq doom-theme 'doom-tokyo-night)
(setq doom-theme 'doom-dracula)

;; Line numbers - relative numbering for easier navigation (vim-style)
(setq display-line-numbers-type 'relative)

;; ===== ORG MODE CONFIGURATION =====
;; Default directory for org-mode files
(setq org-directory "~/org/")

;; ===== C/C++ DEVELOPMENT CONFIGURATION =====
;; Enable Language Server Protocol (LSP) for C++ development
(add-hook 'c++-mode-hook #'lsp!)

;; C++ code style and syntax highlighting configuration
(after! cc-mode
  (setq c-basic-offset 2)
  (setq c-default-style "linux")
  ;; Add more specific highlighting
  (font-lock-add-keywords
   'c++-mode
   '(("\\([a-zA-Z0-9_]+\\)\\s-*(" 1 '(:foreground "#56b6c2" :weight bold))  ;; Function calls
     ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>\\s-*::" 1 '(:foreground "#e5c07b")) ;; Namespaces
     )))

;; Advanced LSP configuration for better code intelligence
(after! lsp-mode
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-restart 'auto-restart)
  (setq lsp-keep-workspace-alive nil)

  (add-hook 'c++-mode-hook #'lsp-inlay-hints-mode)

  (setq lsp-clients-clangd-args
        '("-j=4"
          "--header-insertion=never"
          "--all-scopes-completion"
          "--completion-style=detailed"
          "--background-index"
          "--clang-tidy"
          "--suggest-missing-includes"
          "--pch-storage=memory"
          "--ranking-model=heuristics"
          "--pretty"                        ;; Make clangd output more human-readable
          "--header-insertion-decorators=false" ;; Don't add decorators to headers
          "--log=verbose"                   ;; Changed from error to verbose
          "--enable-config"))               ;; Enable .clangd config files

  ;; Enable sideline temporarily to get more diagnostic info
  (setq lsp-ui-sideline-enable t)  ;; Changed from nil to t
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-diagnostic-max-lines 3)  ;; Show more lines of diagnostic info
  (setq lsp-ui-sideline-show-code-actions t)


  (add-to-list 'lsp-language-id-configuration '(cmake-mode . "cmake"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                    :major-modes '(cmake-mode)
                    :server-id 'cmake-ls))
  ;; Enable flycheck with improved styling
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-diagnostic-package :flycheck)  ;; Additional setting for diagnostics
  (setq lsp-ui-flycheck-enable t)  ;; Enable flycheck integration

  ;; Configure lsp-ui-doc for hovering information
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.2              ;; Show faster
        lsp-ui-doc-max-width 120          ;; Wider to show more info
        lsp-ui-doc-max-height 30          ;; Taller to show more lines
        lsp-ui-doc-include-signature t    ;; Include function signatures
        lsp-ui-doc-show-with-cursor nil   ;; Don't show automatically with cursor
        lsp-ui-doc-show-with-mouse t)     ;; Show on mouse hover

  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)

  ;; Ensure parameter hints still work but are better styled
  (setq lsp-inlay-hint-enable t)
  (setq lsp-inlay-hints-parameters t)
  (setq lsp-inlay-hints-types t)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all t)  ;; Show all eldoc information
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t))

;; Enable LSP for CMake files
(add-hook 'cmake-mode-hook #'lsp!)

;; ===== ERROR/WARNING DISPLAY CONFIGURATION =====
;; Configure flycheck for inline error/warning display with visual styling
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
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; Enable detailed error display
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-help-echo-function #'flycheck-help-echo-all-error-messages))

;; Customize appearance of LSP inlay hints (parameter/type annotations)
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#777777" :background nil :slant italic :height 0.9))
;; ===== CODE COMPLETION CONFIGURATION =====
;; Configure company-mode for auto-completion behavior
(after! company
  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)

  ;; Add these settings to fix the cursor jump issue
  ;; Prevent company from moving cursor after completion
  (setq company-selection-wrap-around t)
  (setq company-auto-commit nil)

  ;; Fix alignment of annotation/documentation
  (setq company-tooltip-align-annotations t)

  ;; Prevent automatic scrolling when completing
  (setq company-selection-default 0)
  (setq company-auto-complete-chars nil)

  ;; Only complete common part on first TAB
  (setq company-require-match nil)

  ;; Use a consistent popup width to avoid jumping
  (setq company-tooltip-minimum-width 30)
  (setq company-tooltip-maximum-width 60)

  ;; Handle formatting edge cases for C++ templates
  (setq company-clang-insert-arguments t)
  (setq company-dabbrev-downcase nil))

;; ===== SYSTEM INTEGRATION =====
;; Clipboard integration for Wayland systems
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

;; Enable inline display of error messages
(use-package! flycheck-inline
  :after flycheck
  :config
  (global-flycheck-inline-mode))

;; Enhanced error/warning display with custom styling
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
;; Customize LSP documentation popups and hover information
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

;; Advanced styling for different types of inlay hints
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#5d8cc0" :background nil :slant italic :height 0.9 :inherit nil)
  '(lsp-inlay-parameter-hint-face :foreground "#6a9955" :background nil :slant italic :height 0.9)
  '(lsp-inlay-type-hint-face :foreground "#ce9178" :background nil :slant italic :height 0.9))


;; Enhanced LSP diagnostic keybindings
(map! :after lsp-mode
      :map lsp-mode-map
      :localleader
      :desc "Show diagnostics list" "d" #'lsp-ui-flycheck-list
      :desc "Describe thing at point" "D" #'lsp-describe-thing-at-point
      :desc "Show hover doc" "h" #'lsp-ui-doc-show
      :desc "Show all diagnostics" "x" #'lsp-treemacs-errors-list)  ;; If treemacs is available

;; Additional keybinding for flycheck error list
(map! :leader
      :desc "Show error list" "c e" #'flycheck-list-errors)

;; Function to force show detailed diagnostic info
(defun show-detailed-diagnostic-at-point ()
  "Show detailed diagnostic information at point using multiple methods."
  (interactive)
  (when (lsp-workspaces)
    ;; Try multiple ways to show diagnostic info
    (lsp-describe-thing-at-point)
    ;; Also try to show the flycheck error
    (when (flycheck-overlay-errors-at (point))
      (flycheck-display-error-messages (flycheck-overlay-errors-at (point))))))

(map! :after lsp-mode
      :map lsp-mode-map
      :localleader
      :desc "Show detailed diagnostics" "i" #'show-detailed-diagnostic-at-point)

;; ===== IMPROVED NAVIGATION & VISUALS =====

;; 1. Harpoon Configuration (The "Alt-Tab" for code)
;; Quick jumps to your 4 most active files
(map! :n "C-1" #'harpoon-go-to-1
      :n "C-2" #'harpoon-go-to-2
      :n "C-3" #'harpoon-go-to-3
      :n "C-4" #'harpoon-go-to-4
      :leader
      :desc "Harpoon menu" "h m" #'harpoon-quick-menu-hydra
      :desc "Harpoon add file"   "h a" #'harpoon-add-file
      :desc "Harpoon clear"      "h c" #'harpoon-clear)

;; 2. Rainbow Delimiters
;; Helps visualize nested brackets in C++ templates and complex macros
;; (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

;; 3. Tree-Sitter Support
;; Ensure we prioritize the better syntax highlighting if available
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (-mode . python-ts-mode)))

;; ===== ORG & JUPYTER CONFIGURATION =====

(use-package! jupyter
  :after org
  :config
  (require 'ob-jupyter))

(after! org
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (jupyter . t)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages)

  (setq org-confirm-babel-evaluate nil)

  (add-hook 'org-babel-after-execute-hook
            #'org-display-inline-images))

;; Add a snippet to insert a Jupyter  block
(defun my-org-insert-jupyter-cell ()
  "Insert a new jupyter- src block."
  (interactive)
  (insert "#+begin_src jupyter :kernel pyvenv :session py :results output\n\n#+end_src\n")
  (forward-line -1))

;; Bind to local leader or a key
(map! :leader
      :desc "Insert Jupyter cell" "i j" #'my-org-insert-jupyter-cell)

(setq ob-jupyter-default-server-command
      "/home/micah/projects//.venv/bin/jupyter")
;; Display inline images automatically after evaluating a src block
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Optional: refresh inline images when opening the org buffer
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-startup-with-inline-images t)
(setq org-display-remote-inline-images 'download)
(setq org-babel-results-separator "\n")

(after! ligature
  (ligature-set-ligatures
   't
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
     "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
     "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
     "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
     ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
     "/**" "/=" "/==" "/>" "//" "///" "&&" "||"
     "||=" "|=" "|>" "^=" "$>" "++" "+++" "+>"
     "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-"
     ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$"
     "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
     "<<-" "<<=" "<<<" "<~" "<~~" "</"
     "</>" "~@" "~-" "~=" "~>" "~~" "~~>"
     "%%"))

  (global-ligature-mode t))


;; When editing src blocks, use real language major mode
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

;; Start LSP automatically in  src edit buffers
(after! org
  (add-hook 'org-src-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (lsp-deferred)))))

(after! org
  ;; Map 'jupyter' source blocks to 'python-mode'
  ;; This ensures your existing LSP hook fires when editing these blocks
  (add-to-list 'org-src-lang-modes '("jupyter" . python))
  (add-to-list 'org-src-lang-modes '("jupyter" . python)))
(defun +org-src-enable-lsp-if-python ()
  "Enable LSP for python src blocks in Org."
  (when (derived-mode-p 'python-mode)
    (lsp-deferred)))

(add-hook 'org-src-mode-hook #'+org-src-enable-lsp-if-python)
(add-hook 'org-src-mode-hook #'+org-jupyter-enable-lsp)
