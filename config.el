;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This file contains all custom configurations for Doom Emacs
;; It includes font settings, themes, LSP configuration, and various plugins

;; ===== APPEARANCE CONFIGURATION =====
;; Font settings - using JetBrains Mono with Nerd Font support for icons
(setq doom-font (font-spec
                 :family "JetBrainsMono Nerd Font"
                 :size 14
                 :weight 'regular))

;; Color theme - Tokyo Night provides a dark, modern appearance
;; (setq doom-theme 'doom-tokyo-night)
(setq doom-theme 'doom-dracula)

;; Line numbers - relative numbering for easier navigation (vim-style)
(setq display-line-numbers-type 'relative)

;; ===== ORG MODE CONFIGURATION =====
;; Default directory for org-mode files
(setq org-directory "~/org/")

;; ===== GLOBAL INDENTATION =====
;; Use 2-space indentation everywhere, no hard tabs
(setq-default tab-width 2
              indent-tabs-mode nil
              standard-indent 2
              evil-shift-width 2)

;; ===== PYTHON DEVELOPMENT CONFIGURATION =====
;; Enable LSP for Python files
(add-hook 'python-mode-hook #'lsp!)

;; Python indentation settings
(after! python
  (setq python-indent-offset 2)
  (setq python-indent-guess-indent-offset nil)
  ;; Ensure proper indentation behavior
  (setq electric-indent-mode t))

;; Configure Python LSP server (using pyright by default in Doom)
(after! lsp-pyright
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-diagnostic-mode "workspace"))


;; ===== PYTHON .PY FILE + JUPYTER REPL WORKFLOW =====

;; FIX 1: Emacs 30 pcase-let*/state void-variable bug.
;; Must use after! jupyter-repl (not after! jupyter) because the function lives
;; in jupyter-repl.el, which jupyter.el requires before providing 'jupyter.
;; The function is defined early but only *called* when a REPL starts.
(defun my/jupyter-repl-sync-safe (fn &rest args)
  (condition-case nil (apply fn args) (void-variable nil)))

(after! jupyter-repl
  (advice-add 'jupyter-repl-sync-execution-state :around #'my/jupyter-repl-sync-safe))

;; FIX 2: python-mode vs python-ts-mode association mismatch.
;; jupyter-kernel-language-mode detects the mode by running set-auto-mode on a
;; temp .py buffer, which may return python-ts-mode if tree-sitter is active,
;; while the user's file is in python-mode (or vice versa).
;; The eq check in jupyter-repl-associate-buffer then silently skips association.
(defun my/jupyter-repl-associate-python-a (fn client)
  (if (and (object-of-class-p client 'jupyter-repl-client)
           (memq major-mode '(python-mode python-ts-mode))
           (memq (jupyter-kernel-language-mode client) '(python-mode python-ts-mode)))
      (progn
        (setq-local jupyter-current-client client)
        (unless jupyter-repl-interaction-mode
          (jupyter-repl-interaction-mode)))
    (funcall fn client)))

(after! jupyter-repl
  (advice-add 'jupyter-repl-associate-buffer :around #'my/jupyter-repl-associate-python-a))

;; FIX 3: Route eval output to the REPL buffer (VS Code interactive window style)
;; rather than as overlays in the source buffer.
(add-hook 'jupyter-repl-interaction-mode-hook
          (lambda ()
            (when jupyter-repl-interaction-mode
              (setq-local jupyter-repl-echo-eval-p t))))

(use-package! code-cells
  :hook (python-mode . code-cells-mode))

(defun my/python-start-repl ()
  "Start a Jupyter REPL, associate this Python buffer, and show the REPL sidebar."
  (interactive)
  (let ((src-buf (current-buffer)))
    ;; call-interactively sets associate-buffer=t; our advice makes the mode check lenient
    (call-interactively #'jupyter-run-repl)
    ;; jupyter-run-repl switched to the REPL buffer; switch back and open sidebar
    (switch-to-buffer src-buf)
    (my/python-toggle-repl-side-window)))

(defun my/python-insert-cell ()
  "Insert a # %% cell separator at the start of the current line."
  (interactive)
  (beginning-of-line)
  (insert "# %%\n"))

(defun my/python-eval-cell ()
  "Evaluate the current # %% cell in the Jupyter REPL."
  (interactive)
  (unless (bound-and-true-p jupyter-repl-interaction-mode)
    (user-error "No Jupyter REPL associated — run SPC j j first"))
  ;; no-header=t strips the # %% separator line from the sent code
  (let ((bounds (code-cells--bounds nil nil t)))
    (jupyter-eval-region nil (car bounds) (cadr bounds))))

(defun my/python-eval-cell-and-next ()
  "Evaluate the current # %% cell and jump to the next one.
If at the last cell, insert a new cell below and jump to it."
  (interactive)
  (my/python-eval-cell)
  (if (save-excursion
        (end-of-line)
        (re-search-forward "^# %%" nil t))
      (code-cells-forward-cell)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n# %%\n")))

(defun my/python-eval-region ()
  "Evaluate the selected region in the Jupyter REPL."
  (interactive)
  (unless (bound-and-true-p jupyter-repl-interaction-mode)
    (user-error "No Jupyter REPL associated — run SPC j j first"))
  (if (use-region-p)
      (jupyter-eval-region nil (region-beginning) (region-end))
    (user-error "No active region")))

(defun my/python-eval-buffer ()
  "Evaluate the entire buffer in the Jupyter REPL."
  (interactive)
  (unless (bound-and-true-p jupyter-repl-interaction-mode)
    (user-error "No Jupyter REPL associated — run SPC j j first"))
  (jupyter-eval-region nil (point-min) (point-max)))

(defun my/python-show-repl ()
  "Pop to the associated Jupyter REPL buffer."
  (interactive)
  (if (bound-and-true-p jupyter-repl-interaction-mode)
      (jupyter-repl-pop-to-buffer)
    (user-error "No Jupyter REPL associated — run SPC j j first")))

(defun my/python-toggle-repl-side-window ()
  "Show the associated Jupyter REPL in a right-side window, or close it."
  (interactive)
  (let ((repl-buf
         (or (when (bound-and-true-p jupyter-current-client)
               (ignore-errors (oref jupyter-current-client buffer)))
             (cl-find-if (lambda (b)
                           (with-current-buffer b
                             (eq major-mode 'jupyter-repl-mode)))
                         (buffer-list)))))
    (if repl-buf
        (let ((repl-win (get-buffer-window repl-buf)))
          (if repl-win
              (delete-window repl-win)
            (display-buffer-in-side-window
             repl-buf '((side . right) (slot . 0) (window-width . 0.4)))))
      (user-error "No Jupyter REPL running — use SPC j j to start one"))))

(after! python
  (map! :map python-mode-map
        :leader
        :prefix ("j" . "jupyter")
        :desc "Start REPL"           "j" #'my/python-start-repl
        :desc "Associate buffer"     "a" #'jupyter-repl-associate-buffer
        :desc "Show REPL"            "o" #'my/python-show-repl
        :desc "Toggle REPL sidebar"  "t" #'my/python-toggle-repl-side-window
        :desc "Restart kernel"       "r" #'jupyter-repl-restart-kernel
        :desc "Interrupt kernel"     "x" #'jupyter-kernel-interrupt
        :desc "Insert # %% cell"     "i" #'my/python-insert-cell
        :desc "Eval cell & next"     "e" #'my/python-eval-cell-and-next
        :desc "Eval cell (stay)"     "E" #'my/python-eval-cell
        :desc "Eval buffer"          "b" #'my/python-eval-buffer
        :desc "Eval region"          "s" #'my/python-eval-region
        :desc "Next cell"            "n" #'code-cells-forward-cell
        :desc "Prev cell"            "p" #'code-cells-backward-cell))

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
  (setq lsp-keep-workspace-alive t)

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
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 30
        lsp-ui-doc-include-signature t)
  ;; Child frames only work in GUI; fall back to a plain window in terminal
  ;; Auto-show disabled — use K to trigger manually
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-childframe t
            lsp-ui-doc-position 'at-point
            lsp-ui-doc-show-with-cursor nil
            lsp-ui-doc-show-with-mouse nil)
    (setq lsp-ui-doc-use-childframe nil
          lsp-ui-doc-position 'bottom
          lsp-ui-doc-show-with-cursor nil
          lsp-ui-doc-show-with-mouse nil))

  ;; Ensure parameter hints still work but are better styled
  (setq lsp-inlay-hint-enable t)
  (setq lsp-inlay-hints-parameters t)
  (setq lsp-inlay-hints-types t)
  (setq lsp-diagnostics-provider :flycheck)
  ;; lsp-ui-doc handles hover display; don't also dump it into the echo area
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate nil))

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

;; ===== TERMINAL COMPLETION RENDERING =====
;; corfu uses child frames by default which don't work in -nw;
;; corfu-terminal replaces that with overlay-based rendering
(unless (display-graphic-p)
  (after! corfu
    (corfu-terminal-mode +1)))

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
  (global-flycheck-inline-mode)
  (set-face-attribute 'flycheck-inline-error nil :box nil)
  (set-face-attribute 'flycheck-inline-warning nil :box nil)
  (set-face-attribute 'flycheck-inline-info nil :box nil))

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
;; Doom's +tree-sitter flag handles python tree-sitter automatically.
;; Remapping python-mode breaks jupyter REPL association.
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)))

;; ===== ORG & JUPYTER CONFIGURATION =====

(add-to-list 'exec-path "/home/moucah/projects/python/.venv/bin")

(use-package! jupyter
  :after org
  :config
  (require 'ob-jupyter)
  ;; Disable :async for jupyter blocks (incompatible with :session)
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "no")))
  ;; Emacs 30.2 / emacs-jupyter incompatibility: pcase-let* in jupyter-bind
  ;; fails to bind `state` lexically during REPL init sync. Suppress the error
  ;; so the REPL still starts; execution count may not update on first block.
  (defadvice! my/jupyter-sync-state-ignore-void-var-a (fn &rest args)
    :around #'jupyter-repl-sync-execution-state
    (condition-case nil
        (apply fn args)
      (void-variable nil))))

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

;; ===== VSCODE-LIKE JUPYTER BEHAVIOR =====

;; --- Black formatting helpers ---

(defun my/format-with-ruff ()
  "Format the visible buffer content with ruff. No-op on failure."
  (let* ((fname (or buffer-file-name "input.py"))
         (input (buffer-string))
         (formatted
          (with-temp-buffer
            (insert input)
            (condition-case nil
                (when (zerop (call-process-region (point-min) (point-max)
                                                  "ruff" t '(t nil) nil
                                                  "format" "--stdin-filename" fname "-"))
                  (buffer-string))
              (error nil)))))
    (when (and formatted (not (string= input formatted)))
      (let ((p (point)))
        (delete-region (point-min) (point-max))
        (insert formatted)
        (goto-char (min p (point-max)))))))

(defun my/org-format-src-block-with-ruff ()
  "Format the current Python/Jupyter src block in-place with ruff."
  (when (org-in-src-block-p)
    (let* ((info (org-babel-get-src-block-info 'light))
           (lang (car info)))
      (when (member lang '("python" "jupyter" "jupyter-python"))
        (save-excursion
          (re-search-backward "^[ \t]*#\\+begin_src" nil t)
          (forward-line 1)
          (let ((code-beg (point)))
            (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (beginning-of-line)
            (let* ((code-end (point))
                   (fname (or buffer-file-name "input.py"))
                   (input (buffer-substring-no-properties code-beg code-end))
                   (formatted
                    (with-temp-buffer
                      (insert input)
                      (condition-case nil
                          (when (zerop (call-process-region (point-min) (point-max)
                                                            "ruff" t '(t nil) nil
                                                            "format" "--stdin-filename" fname "-"))
                            (buffer-string))
                        (error nil)))))
              (when (and formatted (not (string= input formatted)))
                (delete-region code-beg code-end)
                (goto-char code-beg)
                (insert formatted)))))))))

(defun my-org-insert-jupyter-cell ()
  "Insert a new jupyter-python src block."
  (interactive)
  (insert "#+begin_src jupyter :kernel python3 :session py :results output\n\n#+end_src\n")
  (forward-line -1))

;; Flag for deferred cell creation after async Jupyter execution
(defvar my/org-jupyter-create-next-cell nil
  "When non-nil, create a new cell after the current execution completes.")

;; Execute cell and create new cell below (like Shift-Enter in VSCode)
(defun my-org-jupyter-execute-and-next ()
  "Execute current cell and create/move to next cell (VSCode Shift-Enter behavior)."
  (interactive)
  (my/org-format-src-block-with-ruff)
  (let ((has-next
         (save-excursion
           (when (org-in-src-block-p)
             (re-search-forward "^#\\+end_src" nil t))
           (re-search-forward "^#\\+begin_src" nil t))))
    (if has-next
        (progn
          (org-babel-execute-src-block)
          ;; Move to next existing block
          (when (org-in-src-block-p)
            (re-search-forward "^#\\+end_src" nil t))
          (re-search-forward "^#\\+begin_src" nil t)
          (forward-line 1)
          (beginning-of-line))
      ;; No next block — flag for after-execute hook to create cell
      ;; after results are inserted (handles async Jupyter execution)
      (setq my/org-jupyter-create-next-cell t)
      (org-babel-execute-src-block))))

(defun my/org-jupyter-maybe-create-cell ()
  "If flagged, create a new jupyter cell after the results block.
Runs from `org-babel-after-execute-hook' so results are already inserted."
  (when my/org-jupyter-create-next-cell
    (setq my/org-jupyter-create-next-cell nil)
    (let ((result-pos (org-babel-where-is-src-block-result)))
      (if result-pos
          (progn
            (goto-char result-pos)
            (let ((elem (org-element-at-point)))
              ;; Skip #+RESULTS: keyword
              (when (eq (org-element-type elem) 'keyword)
                (goto-char (org-element-property :end elem))
                (setq elem (org-element-at-point)))
              ;; Skip the actual results content (drawer, fixed-width, etc.)
              (when (memq (org-element-type elem)
                          '(fixed-width example-block drawer latex-environment
                            paragraph plain-list table))
                (goto-char (org-element-property :end elem)))))
        ;; Fallback: go past #+end_src
        (when (org-in-src-block-p)
          (re-search-forward "^#\\+end_src" nil t)))
      (skip-chars-backward "\n ")
      (end-of-line)
      (insert "\n\n\n")
      (my-org-insert-jupyter-cell)
      (forward-line -1))))

(add-hook 'org-babel-after-execute-hook #'my/org-jupyter-maybe-create-cell 'append)

;; Execute cell and stay in place (like Ctrl-Enter in VSCode)
(defun my-org-jupyter-execute-and-stay ()
  "Execute current cell and stay in place (VSCode Ctrl-Enter behavior)."
  (interactive)
  (org-babel-execute-src-block))

;; Jump to next INPUT block (skip results)
(defun my-org-jupyter-next-cell ()
  "Jump to next jupyter src block, skipping results."
  (interactive)
  (let ((start-pos (point)))
    ;; Move past current block if we're in one
    (when (org-in-src-block-p)
      (org-babel-next-src-block))
    ;; Find next src block
    (if (re-search-forward "^#\\+begin_src" nil t)
        (progn
          (forward-line 1)
          (beginning-of-line))
      (goto-char start-pos)
      (message "No next cell"))))

;; Jump to previous INPUT block (skip results)
(defun my-org-jupyter-prev-cell ()
  "Jump to previous jupyter src block, skipping results."
  (interactive)
  (let ((start-pos (point)))
    ;; If we're in a block, go to its start first
    (when (org-in-src-block-p)
      (re-search-backward "^#\\+begin_src" nil t))
    ;; Now find previous block
    (if (re-search-backward "^#\\+begin_src" nil t)
        (progn
          (forward-line 1)
          (beginning-of-line))
      (goto-char start-pos)
      (message "No previous cell"))))

;; Keybindings for Jupyter-like workflow
;; These need to be set with higher priority to work even with polymode
(map! :after org
      :map org-mode-map
      ;; VSCode-like execution (both normal and insert mode)
      :ni "C-<return>" #'my-org-jupyter-execute-and-stay    ;; Ctrl-Enter: execute, stay
      :ni "S-<return>" #'my-org-jupyter-execute-and-next    ;; Shift-Enter: execute, next
      
      ;; Better cell navigation (skip results blocks)
      :n "g j" #'my-org-jupyter-next-cell
      :n "g k" #'my-org-jupyter-prev-cell
      
      ;; Quick insert cell
      :n "g I" #'my-org-insert-jupyter-cell)

;; Also set these in evil normal state for org-mode specifically
(after! evil-org
  (map! :map evil-org-mode-map
        :n "g j" #'my-org-jupyter-next-cell
        :n "g k" #'my-org-jupyter-prev-cell))

;; Also bind via leader key
(map! :leader
      :desc "Insert Jupyter cell" "i j" #'my-org-insert-jupyter-cell
      :desc "Execute and next" "i e" #'my-org-jupyter-execute-and-next)

(setq jupyter-command "/home/moucah/projects/python/.venv/bin/jupyter")
;; Clean carriage-return progress bars (tqdm etc.) after execution.
;; Simulates terminal behavior: only keeps the final state of each line.
(defun my/org-babel-clean-cr-output ()
  "Process carriage returns in results so tqdm etc. display cleanly."
  (save-excursion
    (when-let* ((res-beg (org-babel-where-is-src-block-result)))
      (goto-char res-beg)
      (let ((res-end (copy-marker
                      (if (re-search-forward "^:END:\\|^$\\|^[^:#:|]" nil t)
                          (match-beginning 0)
                        (point-max)))))
        (goto-char res-beg)
        (while (re-search-forward "[\r\xd␍]" res-end t)
          (let* ((lb (line-beginning-position))
                 (le (line-end-position))
                 (line (buffer-substring-no-properties lb le))
                 (parts (split-string line "[\r\xd␍]+"))
                 (last-part (car (last (seq-remove #'string-empty-p parts)))))
            (when last-part
              ;; Preserve org fixed-width prefix ": "
              (when (and (string-prefix-p ": " line)
                         (not (string-prefix-p ": " last-part)))
                (setq last-part (concat ": " (string-trim-left last-part))))
              (delete-region lb le)
              (goto-char lb)
              (insert last-part))))
        (set-marker res-end nil)))))

(add-hook 'org-babel-after-execute-hook #'my/org-babel-clean-cr-output)

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

;; ===== ORG SRC BLOCK IMPROVEMENTS =====

;; When editing src blocks, use real language major mode with proper indentation
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

;; Edit src blocks in the SAME window (no split — feels seamless)
(setq org-src-window-setup 'current-window)

;; Map jupyter source blocks to python-mode for LSP support
(after! org
  (add-to-list 'org-src-lang-modes '("jupyter" . python))
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python)))

;; ===== SEAMLESS LSP EDITING IN ORG SRC BLOCKS =====
;; How it works:
;;   1. Edit buffer gets a .py file identity → pyright fully engages
;;      (hover, completion, goto-def all need a file URI to work)
;;   2. Previous blocks prepended as hidden preamble → cross-block context
;;   3. Buffer narrowed to current block → org save only writes current block
;;   4. LSP widens internally → sees full content for context-aware features
;;
;; Workflow:
;;   gj/gk  — navigate between cells (org-mode, as before)
;;   RET    — enter edit buffer with full LSP (normal mode, in a src block)
;;   gj/gk  — exit edit + navigate to next/prev cell (from inside edit buffer)
;;   C-RET  — execute block (works in both org and edit buffer)
;;   S-RET  — execute + next (works in both org and edit buffer)
;;   C-c '  — toggle edit buffer (standard org keybinding, also works)

(defvar-local my/org-src-preamble-marker nil
  "Marker at end of injected preamble in org-src edit buffer.")

;; --- LSP setup in edit buffers ---
(defun my/org-src-python-lsp-setup ()
  "Set up Python LSP with full features and cross-block context."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (setq-local electric-indent-mode t)
    (setq-local electric-indent-inhibit nil)
    (when-let* ((marker org-src--beg-marker)
                (org-buf (marker-buffer marker))
                (org-file (buffer-file-name org-buf))
                (org-dir (file-name-directory org-file)))
      (setq-local default-directory org-dir)
      ;; Give buffer a .py file name so pyright fully engages
      (setq-local buffer-file-name
                  (expand-file-name
                   (concat ".org-src-" (file-name-base org-file) ".py")
                   org-dir))
      (setq-local backup-inhibited t)
      (setq-local +format-with :none) ;; don't auto-format shadow content
      (set-buffer-modified-p nil)
      ;; Inject previous blocks as hidden preamble for cross-block context
      (my/org-src--inject-preamble)
      ;; Start LSP with full features
      (lsp-deferred)
      (setq-local lsp-ui-doc-enable t)
      (setq-local lsp-ui-sideline-enable t)
      (setq-local lsp-signature-auto-activate t)
      (setq-local lsp-enable-symbol-highlighting t))))

(defun my/org-src--inject-preamble ()
  "Prepend all previous Python/Jupyter blocks, then narrow to current block.
LSP widens internally and sees the full content for context-aware features.
Org-src save/exit respects narrowing and only writes back the current block.
Uses fast regex scan instead of full org-element parse."
  (when-let* ((marker org-src--beg-marker)
              (org-buf (marker-buffer marker))
              (current-pos (marker-position marker)))
    (let ((preamble
           (with-current-buffer org-buf
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (let ((parts nil))
                   (while (re-search-forward
                           "^#\\+begin_src\\s-+\\(jupyter\\(?:-python\\)?\\|python\\)\\b"
                           current-pos t)
                     (forward-line 1)
                     (let ((body-start (point)))
                       (when (re-search-forward "^#\\+end_src\\b" current-pos t)
                         (push (buffer-substring-no-properties
                                body-start (line-beginning-position))
                               parts))))
                   (when parts
                     (mapconcat #'identity (nreverse parts) "\n"))))))))
      (when (and preamble (> (length preamble) 0))
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert preamble "\n")))
        (setq my/org-src-preamble-marker
              (copy-marker (+ (point-min) (length preamble) 1)))
        (narrow-to-region (marker-position my/org-src-preamble-marker)
                          (point-max))))))

;; --- Preamble cleanup: strip before save/exit so org never sees it ---

(defadvice! my/org-src--exit-strip-preamble-a (&rest _)
  "Delete preamble before exit so only current block is written to org."
  :before #'org-edit-src-exit
  (when (and (bound-and-true-p my/org-src-preamble-marker)
             (marker-position my/org-src-preamble-marker))
    (widen)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (marker-position my/org-src-preamble-marker)))
    (setq my/org-src-preamble-marker nil)))

(defadvice! my/org-src--save-strip-preamble-a (fn &rest args)
  "Strip preamble before save, then restore it so LSP keeps context."
  :around #'org-edit-src-save
  (if (and (bound-and-true-p my/org-src-preamble-marker)
           (marker-position my/org-src-preamble-marker))
      (let ((preamble-text
             (save-restriction
               (widen)
               (buffer-substring-no-properties
                (point-min)
                (marker-position my/org-src-preamble-marker)))))
        ;; Strip preamble
        (widen)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (marker-position my/org-src-preamble-marker)))
        (setq my/org-src-preamble-marker nil)
        ;; Save with clean content
        (apply fn args)
        ;; Restore preamble for continued editing with LSP context
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert preamble-text)))
        (setq my/org-src-preamble-marker
              (copy-marker (+ (point-min) (length preamble-text))))
        (narrow-to-region (marker-position my/org-src-preamble-marker)
                          (point-max)))
    (apply fn args)))

(after! org-src
  (add-hook 'org-src-mode-hook #'my/org-src-python-lsp-setup))

;; Fix: LSP completion positions refer to the full (widened) buffer,
;; but org-src narrowing makes those positions inaccessible.
(defadvice! my/lsp-completion-guess-prefix-widen-a (fn &rest args)
  :around #'lsp-completion--guess-prefix
  (save-restriction
    (widen)
    (apply fn args)))

;; --- Pre-warm pyright so org-src edit buffers open instantly ---
;; Creates a hidden buffer with LSP connected on org-mode load.
;; Since lsp-keep-workspace-alive is t, pyright stays running and
;; subsequent org-edit-special calls connect to it immediately.

(defvar-local my/org-src--prewarm-buffer nil
  "Hidden buffer keeping pyright warm for this org file.")

(defun my/org-src-prewarm-lsp ()
  "Start pyright in the background so org-src edit buffers connect instantly."
  (when-let* ((org-file buffer-file-name)
              (org-dir (file-name-directory org-file))
              (buf-name (format " *pyright:%s*" (file-name-base org-file))))
    (unless (and (buffer-live-p my/org-src--prewarm-buffer)
                 (with-current-buffer my/org-src--prewarm-buffer
                   (bound-and-true-p lsp-mode)))
      (let ((buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (unless (bound-and-true-p lsp-mode)
            (python-mode)
            (setq-local buffer-file-name
                        (expand-file-name
                         (concat ".org-src-warm-" (file-name-base org-file) ".py")
                         org-dir))
            (setq-local default-directory org-dir)
            (setq-local backup-inhibited t)
            (setq-local +format-with :none)
            (insert "# prewarm\n")
            (set-buffer-modified-p nil)
            (lsp-deferred)))
        (setq my/org-src--prewarm-buffer buf)))))

(add-hook 'org-mode-hook
          (lambda ()
            ;; Start pyright after 2s idle so it doesn't block org file loading
            (let ((buf (current-buffer)))
              (run-with-idle-timer
               2 nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (my/org-src-prewarm-lsp))))))
            ;; Clean up prewarm buffer when org buffer is killed
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when (buffer-live-p my/org-src--prewarm-buffer)
                          (kill-buffer my/org-src--prewarm-buffer)))
                      nil t)))

;; --- Smart keys: enter edit buffer (with LSP) when in a src block ---
(defun my/org-smart-return ()
  "In a src block, open the edit buffer with LSP. Otherwise, default org RET."
  (interactive)
  (if (org-in-src-block-p)
      (org-edit-special)
    (+org/dwim-at-point)))

(defun my/org-smart-insert ()
  "In a src block, open edit buffer in insert mode. Otherwise normal evil-insert."
  (interactive)
  (if (org-in-src-block-p)
      (progn (org-edit-special) (evil-insert-state))
    (evil-insert-state)))

(defun my/org-smart-append ()
  "In a src block, open edit buffer in insert mode. Otherwise normal evil-append."
  (interactive)
  (if (org-in-src-block-p)
      (progn (org-edit-special) (evil-insert-state))
    (evil-append 1)))

(after! evil-org
  (map! :map evil-org-mode-map
        :n [return] #'my/org-smart-return
        :n "RET"    #'my/org-smart-return
        :n "i"      #'my/org-smart-insert
        :n "a"      #'my/org-smart-append))

;; --- Mirror navigation/execution keys inside the edit buffer ---

(defun my/org-src-exit-and-next ()
  "Exit edit buffer and jump to next cell."
  (interactive)
  (org-edit-src-exit)
  (my-org-jupyter-next-cell))

(defun my/org-src-exit-and-prev ()
  "Exit edit buffer and jump to previous cell."
  (interactive)
  (org-edit-src-exit)
  (my-org-jupyter-prev-cell))

(defun my/org-src-execute-stay ()
  "Execute the block from within the edit buffer, staying in edit mode."
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit)
  (org-babel-execute-src-block)
  (org-edit-special))

(defun my/org-src-execute-and-next ()
  "Execute the block and move to next cell (exit edit buffer)."
  (interactive)
  (my/format-with-ruff)
  (org-edit-src-save)
  (org-edit-src-exit)
  (my-org-jupyter-execute-and-next))

(after! org-src
  (map! :map org-src-mode-map
        :n  "g j"        #'my/org-src-exit-and-next
        :n  "g k"        #'my/org-src-exit-and-prev
        :n  [escape]     #'org-edit-src-exit   ; ESC in normal mode → back to org
        :ni "C-<return>" #'my/org-src-execute-stay
        :ni "S-<return>" #'my/org-src-execute-and-next))

;; ===== LEAN 4 CONFIGURATION =====
(use-package! lean4-mode
  :commands lean4-mode
  :hook (lean4-mode . (lambda ()
                        (set-input-method "Lean")))
  :config
  (setq lean4-keybinding-lean4-toggle-info (kbd "C-c C-i")))

(after! lean4-mode
  (add-hook 'lean4-mode-hook #'lsp!)

  ;; --- Suppress inlayHint/refresh spam ---
  (after! lsp-mode
    (add-to-list 'warning-suppress-types '(lsp-mode)))

  ;; --- Persistent #eval/#check virtual-line overlays ---
  ;;
  ;; Data source: flycheck-current-errors (reliable — squiggles proved it works).
  ;; Triggered by: flycheck-after-syntax-check-hook (primary) and a deferred
  ;; lsp-diagnostics-updated-hook (backup for incremental lean4 elaboration,
  ;; where successive LSP pushes each add one more #eval result).
  ;; Results accumulate in a per-buffer cache (line# -> string) so overlays
  ;; survive between check cycles.
  ;; Cleanup: before-change-functions fires BEFORE text is removed, so we can
  ;; still locate the overlay by position and delete it immediately.
  ;; evaporate t is intentionally omitted — zero-width overlays are already
  ;; "empty" at creation, which causes Emacs to evaporate them immediately.

  (defvar-local my/lean4-eval-overlays nil)
  (defvar-local my/lean4-eval-cache nil)   ; hash: line-number (0-indexed) -> string

  (defun my/lean4-redraw-eval-overlays ()
    "Delete all eval overlays and recreate them from the result cache."
    (mapc #'delete-overlay my/lean4-eval-overlays)
    (setq my/lean4-eval-overlays nil)
    (when my/lean4-eval-cache
      (maphash
       (lambda (line msg)
         (save-excursion
           (goto-char (point-min))
           (forward-line line)
           (when (looking-at ".*#\\(eval\\|check\\|reduce\\)")
             (let ((ov (make-overlay (line-end-position) (line-end-position))))
               (overlay-put ov 'after-string
                            (concat "\n"
                                    (propertize (concat "▶ " msg)
                                                'face '(:foreground "#6a9955"
                                                        :slant italic
                                                        :height 0.9))))
               (overlay-put ov 'my/lean4-eval t)
               (push ov my/lean4-eval-overlays)))))
       my/lean4-eval-cache)))

  (defun my/lean4-update-eval-overlays-from-flycheck ()
    "Populate cache from flycheck info/hint errors on #eval lines, then redraw."
    (when (derived-mode-p 'lean4-mode)
      (dolist (err flycheck-current-errors)
        (when (memq (flycheck-error-level err) '(info hint))
          (let ((line (1- (flycheck-error-line err)))
                (msg  (flycheck-error-message err)))
            (save-excursion
              (goto-char (point-min))
              (forward-line line)
              (when (looking-at ".*#\\(eval\\|check\\|reduce\\)")
                (puthash line msg my/lean4-eval-cache))))))
      (my/lean4-redraw-eval-overlays)))

  (defun my/lean4-maybe-update-overlays ()
    "Deferred overlay update: lets flycheck process the new LSP diagnostics first."
    (when (derived-mode-p 'lean4-mode)
      (let ((buf (current-buffer)))
        (run-with-idle-timer
         0.3 nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (my/lean4-update-eval-overlays-from-flycheck))))))))

  (defun my/lean4-eval-before-change (beg end)
    "Remove overlays and cache entries for lines about to be edited or deleted."
    (when (or my/lean4-eval-overlays my/lean4-eval-cache)
      (save-excursion
        (let ((pos-beg (progn (goto-char beg) (line-beginning-position)))
              (pos-end (progn (goto-char end) (line-end-position))))
          (setq my/lean4-eval-overlays
                (seq-filter
                 (lambda (ov)
                   (if (and (overlay-buffer ov)
                            (<= pos-beg (overlay-start ov) pos-end))
                       (progn (delete-overlay ov) nil)
                     t))
                 my/lean4-eval-overlays))
          (when my/lean4-eval-cache
            (goto-char pos-beg)
            (let ((l1 (1- (line-number-at-pos))))
              (goto-char pos-end)
              (cl-loop for l from l1 to (1- (line-number-at-pos))
                       do (remhash l my/lean4-eval-cache))))))))

  (defun my/lean4-setup-eval-overlays ()
    "Initialize the eval-overlay system for this lean4 buffer."
    (setq-local my/lean4-eval-cache (make-hash-table :test 'eql))
    (add-hook 'flycheck-after-syntax-check-hook
              #'my/lean4-update-eval-overlays-from-flycheck nil t)
    (add-hook 'lsp-diagnostics-updated-hook
              #'my/lean4-maybe-update-overlays nil t)
    (add-hook 'before-change-functions #'my/lean4-eval-before-change nil t)
    (face-remap-add-relative 'flycheck-info '(:underline nil)))

  (add-hook 'lean4-mode-hook #'my/lean4-setup-eval-overlays)

  ;; --- Suppress corfu on #eval/#check lines ---
  (after! corfu
    (defadvice! my/lean4-suppress-corfu-on-eval-a (&rest _)
      :before-until #'corfu--auto-complete
      (and (derived-mode-p 'lean4-mode)
           (save-excursion
             (beginning-of-line)
             (looking-at ".*#\\(eval\\|check\\|reduce\\)"))))))

;; Keep org-modern separate since it's unrelated
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-block-fringe nil
        org-modern-block-name nil
        org-modern-hide-stars t))
