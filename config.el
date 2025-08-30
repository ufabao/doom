;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This file contains all custom configurations for Doom Emacs
;; It includes font settings, themes, LSP configuration, and various plugins

;; ===== APPEARANCE CONFIGURATION =====
;; Font settings - using JetBrains Mono with Nerd Font support for icons
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; Color theme - Tokyo Night provides a dark, modern appearance
(setq doom-theme 'doom-tokyo-night)

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
;; ===== JUPYTER NOTEBOOK SUPPORT =====
;; EIN (Emacs IPython Notebook) configuration for Jupyter integration
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

;; ===== BUILD SYSTEM INTEGRATION (BAZEL) =====
;; Comprehensive Bazel build system integration with multiple configurations

;; Define available build configurations
(defcustom bazel-build-configs
  '(("gcc-debug" . "--config=gcc --compilation_mode=dbg")
    ("gcc-release" . "--config=gcc --compilation_mode=opt")
    ("clang-debug" . "--config=clang --compilation_mode=dbg")
    ("clang-release" . "--config=clang --compilation_mode=opt")
    ("default-debug" . "--compilation_mode=dbg")
    ("default-release" . "--compilation_mode=opt"))
  "List of Bazel build configurations."
  :type '(alist :key-type string :value-type string)
  :group 'bazel)

(defcustom bazel-default-config "default-debug"
  "Default Bazel build configuration."
  :type 'string
  :group 'bazel)

(defcustom bazel-common-build-flags
  '("--jobs=auto"
    "--verbose_failures"
    "--sandbox_debug")
  "Common flags to add to all Bazel builds."
  :type '(repeat string)
  :group 'bazel)

;; Store the last used configuration
(defvar bazel-last-config bazel-default-config
  "Last used Bazel configuration.")

;; Enhanced Bazel functions
(defun bazel-get-targets ()
  "Get list of available Bazel targets."
  (let* ((default-directory (or (locate-dominating-file "." "WORKSPACE")
                                (locate-dominating-file "." "WORKSPACE.bazel")
                                (locate-dominating-file "." "MODULE.bazel")
                                default-directory))
         (cmd "bazel query //... --output=label 2>/dev/null")
         (output (shell-command-to-string cmd)))
    (split-string output "\n" t)))

(defun bazel-format-command (action target config-flags extra-flags)
  "Format a Bazel command with the given parameters."
  (let ((common-flags (mapconcat 'identity bazel-common-build-flags " ")))
    (string-trim
     (format "bazel %s %s %s %s %s"
             action
             target
             (or config-flags "")
             common-flags
             (or extra-flags "")))))

;; Simple build with last configuration (for SPC B b)
(defun bazel-build-with-completion ()
  "Build a Bazel target with completion using the last configuration."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Build target: " targets))
         (config-flags (cdr (assoc bazel-last-config bazel-build-configs))))
    (when target
      (compile (bazel-format-command "build" target config-flags nil))
      (message "Building %s with config: %s" target bazel-last-config))))

;; Advanced build with configuration selection
(defun bazel-build-advanced ()
  "Build a Bazel target with configuration selection."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Build target: " targets))
         (config (completing-read
                  (format "Build configuration (current: %s): " bazel-last-config)
                  (mapcar 'car bazel-build-configs)
                  nil t nil nil bazel-last-config))
         (config-flags (cdr (assoc config bazel-build-configs)))
         (extra-flags (read-string "Extra flags (optional): ")))
    (when target
      (setq bazel-last-config config)
      (compile (bazel-format-command "build" target config-flags extra-flags))
      (message "Building %s with config: %s" target config))))

;; Build with custom flags
(defun bazel-build-custom ()
  "Build a Bazel target with completely custom flags."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Build target: " targets))
         (custom-flags (read-string "Custom build flags: ")))
    (when target
      (compile (format "bazel build %s %s" target custom-flags)))))

;; Test functions with configuration
(defun bazel-test-with-completion ()
  "Test a Bazel target with completion using the last configuration."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Test target: " targets))
         (config-flags (cdr (assoc bazel-last-config bazel-build-configs))))
    (when target
      (compile (bazel-format-command "test" target config-flags "--test_output=all")))))

(defun bazel-test-advanced ()
  "Test a Bazel target with configuration selection."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Test target: " targets))
         (config (completing-read
                  (format "Test configuration (current: %s): " bazel-last-config)
                  (mapcar 'car bazel-build-configs)
                  nil t nil nil bazel-last-config))
         (config-flags (cdr (assoc config bazel-build-configs)))
         (test-flags (read-string "Extra test flags (optional): ")))
    (when target
      (setq bazel-last-config config)
      (compile (bazel-format-command "test" target config-flags
                                     (format "--test_output=all %s" test-flags))))))

;; Run functions with configuration
(defun bazel-run-with-completion ()
  "Run a Bazel target with completion using the last configuration."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Run target: " targets))
         (config-flags (cdr (assoc bazel-last-config bazel-build-configs))))
    (when target
      (compile (bazel-format-command "run" target config-flags nil)))))

(defun bazel-run-advanced ()
  "Run a Bazel target with configuration selection."
  (interactive)
  (let* ((targets (bazel-get-targets))
         (target (completing-read "Run target: " targets))
         (config (completing-read
                  (format "Run configuration (current: %s): " bazel-last-config)
                  (mapcar 'car bazel-build-configs)
                  nil t nil nil bazel-last-config))
         (config-flags (cdr (assoc config bazel-build-configs)))
         (run-args (read-string "Program arguments (optional): ")))
    (when target
      (setq bazel-last-config config)
      (if (string-empty-p run-args)
          (compile (bazel-format-command "run" target config-flags nil))
        (compile (bazel-format-command "run" target config-flags (format "-- %s" run-args)))))))

;; Configuration management functions
(defun bazel-set-default-config ()
  "Set the default Bazel configuration."
  (interactive)
  (let ((config (completing-read
                 (format "Set default configuration (current: %s): " bazel-last-config)
                 (mapcar 'car bazel-build-configs)
                 nil t nil nil bazel-last-config)))
    (setq bazel-last-config config)
    (message "Default configuration set to: %s" config)))

(defun bazel-show-current-config ()
  "Show the current Bazel configuration."
  (interactive)
  (message "Current Bazel configuration: %s" bazel-last-config))

;; Clean functions
(defun bazel-clean ()
  "Clean Bazel build artifacts."
  (interactive)
  (compile "bazel clean"))

(defun bazel-clean-expunge ()
  "Clean Bazel build artifacts and remove the entire working tree."
  (interactive)
  (when (yes-or-no-p "This will remove the entire Bazel working tree. Continue? ")
    (compile "bazel clean --expunge")))

;; Other utility functions
(defun bazel-list-targets ()
  "List all available Bazel targets."
  (interactive)
  (let ((targets (bazel-get-targets)))
    (with-output-to-temp-buffer "*Bazel Targets*"
      (dolist (target targets)
        (princ (format "%s\n" target))))))

(defun bazel-query ()
  "Run a Bazel query."
  (interactive)
  (let ((query (read-string "Bazel query: ")))
    (when (not (string-empty-p query))
      (compile (format "bazel query '%s'" query)))))

(defun bazel-info ()
  "Show Bazel workspace info."
  (interactive)
  (compile "bazel info"))

;; Generate compile_commands.json
(defun bazel-refresh-compile-commands ()
  "Refresh compile_commands.json using Hedron's refresh_compile_commands."
  (interactive)
  (compile "bazel run //:refresh_compile_commands")
  (message "Refreshing compile_commands.json..."))

;; Updated keybindings
(map! :leader
      (:prefix ("B" . "bazel")
       ;; Quick actions (use last configuration)
       :desc "Build target" "b" #'bazel-build-with-completion
       :desc "Run target" "r" #'bazel-run-with-completion
       :desc "Test target" "t" #'bazel-test-with-completion

       ;; Advanced actions (with configuration selection)
       :desc "Build (advanced)" "B" #'bazel-build-advanced
       :desc "Run (advanced)" "R" #'bazel-run-advanced
       :desc "Test (advanced)" "T" #'bazel-test-advanced

       ;; Custom and configuration
       :desc "Build (custom flags)" "C" #'bazel-build-custom
       :desc "Set default config" "c" #'bazel-set-default-config
       :desc "Show current config" "s" #'bazel-show-current-config

       ;; Utility commands
       :desc "Clean" "x" #'bazel-clean
       :desc "Clean (expunge)" "X" #'bazel-clean-expunge
       :desc "List targets" "l" #'bazel-list-targets
       :desc "Query" "q" #'bazel-query
       :desc "Info" "i" #'bazel-info
       :desc "Refresh compile_commands" "g" #'bazel-refresh-compile-commands))

;; Also add keybindings for BUILD files
(use-package! bazel
  :defer t
  :config
  ;; File associations
  (add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-build-mode))
  (add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-build-mode))
  (add-to-list 'auto-mode-alist '("BUILD\\.bazel\\'" . bazel-build-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-workspace-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\.bazel\\'" . bazel-workspace-mode))
  (add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-starlark-mode))

  ;; Local keybindings for BUILD files
  (map! :after bazel
        :map bazel-build-mode-map
        :localleader
        :desc "Build target" "b" #'bazel-build-with-completion
        :desc "Build (advanced)" "B" #'bazel-build-advanced
        :desc "Test target" "t" #'bazel-test-with-completion
        :desc "Test (advanced)" "T" #'bazel-test-advanced
        :desc "Run target" "r" #'bazel-run-with-completion
        :desc "Run (advanced)" "R" #'bazel-run-advanced
        :desc "Set config" "c" #'bazel-set-default-config
        :desc "Clean" "x" #'bazel-clean
        :desc "List targets" "l" #'bazel-list-targets))

;; Display current configuration in modeline (optional)
(defun bazel-modeline-config ()
  "Return current Bazel config for modeline."
  (when (and (boundp 'bazel-last-config)
             (or (derived-mode-p 'bazel-build-mode)
                 (derived-mode-p 'c++-mode)
                 (derived-mode-p 'c-mode)))
    (format " Bazel[%s]" bazel-last-config)))

;; Add to modeline if desired
;; (add-to-list 'global-mode-string '(:eval (bazel-modeline-config)) t)
