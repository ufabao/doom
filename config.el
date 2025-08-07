;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Font configuration
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; Theme
(setq doom-theme 'doom-tokyo-night)
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
  (setq c-basic-offset 2)
  (setq c-default-style "linux")
  ;; Add more specific highlighting
  (font-lock-add-keywords
   'c++-mode
   '(("\\([a-zA-Z0-9_]+\\)\\s-*(" 1 '(:foreground "#56b6c2" :weight bold))  ;; Function calls
     ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>\\s-*::" 1 '(:foreground "#e5c07b")) ;; Namespaces
     )))

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

(add-hook 'cmake-mode-hook #'lsp!)

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
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; Enable detailed error display
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-help-echo-function #'flycheck-help-echo-all-error-messages))

;; Style the inlay hints to be less intrusive
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#777777" :background nil :slant italic :height 0.9))
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


;; ===== Test Runner Configuration =====

;; Test button types for different frameworks
(define-button-type 'gtest-button
  'action 'run-gtest-at-point
  'follow-link t
  'help-echo "Click to run this test"
  'face '(:foreground "green" :underline t))

(define-button-type 'catch2-button
  'action 'run-catch2-test-at-point
  'follow-link t
  'help-echo "Click to run this test"
  'face '(:foreground "green" :underline t))

(define-button-type 'pytest-button
  'action 'run-pytest-at-point
  'follow-link t
  'help-echo "Click to run this test"
  'face '(:foreground "green" :underline t))

;; Function to find test executables in the build directory
(defun find-test-executables ()
  "Find test executables in the build directory."
  (let* ((default-directory (project-root (project-current t)))
         (build-dir (concat default-directory "build"))
         (executables nil))
    (when (file-directory-p build-dir)
      (dolist (dir (list (concat build-dir "/bin")
                         (concat build-dir "/tests")
                         (concat build-dir "/test")
                         build-dir))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t))
            (when (and (file-executable-p file)
                       (not (file-directory-p file))
                       (or (string-match-p "test" (file-name-nondirectory file))
                           (string-match-p "gtest" (file-name-nondirectory file))))
              (push file executables))))))
    executables))

;; Updated function to run a specific test
(defun run-catch2-test-at-point (button)
  "Run the Catch2 test at point with improved executable finding."
  (interactive)
  (let* ((test-name (button-get button 'test-name))
         (file-name (buffer-file-name))
         (default-directory (project-root (project-current t)))
         (test-executables (find-test-executables))
         (test-executable (if test-executables (car test-executables) nil)))
    (if test-executable
        (progn
          (message "Running test: %s using %s" test-name test-executable)
          (compile (format "%s \"%s\"" test-executable test-name)))
      (message "Could not find test executable in build directory"))))

;; Function to run a Google Test
(defun run-gtest-at-point (button)
  "Run the Google Test at point with improved executable finding."
  (interactive)
  (let* ((test-name (button-get button 'test-name))
         (test-class (button-get button 'test-class))
         (file-name (buffer-file-name))
         (default-directory (project-root (project-current t)))
         (test-executables (find-test-executables))
         (test-executable (if test-executables (car test-executables) nil))
         (filter (format "%s.%s" test-class test-name)))
    (if test-executable
        (progn
          (message "Running test: %s using %s" filter test-executable)
          (compile (format "%s --gtest_filter=%s" test-executable filter)))
      (message "Could not find test executable in build directory"))))

;; Function to run a pytest
(defun run-pytest-at-point (button)
  "Run the pytest at point."
  (interactive)
  (let* ((test-name (button-get button 'test-name))
         (file-name (buffer-file-name))
         (default-directory (project-root (project-current t)))
         (cmd (format "cd %s && python -m pytest %s::%s -v"
                      default-directory file-name test-name)))
    (message "Running test: %s" cmd)
    (compile cmd)))

;; Function to add test buttons based on the framework
(defun add-test-buttons ()
  "Add test buttons based on detected test framework."
  (interactive)
  (remove-overlays (point-min) (point-max) 'test-button t)
  (save-excursion
    (goto-char (point-min))
    (cond
     ;; Google Test
     ((re-search-forward "\\<TEST\\(_[FP]\\)?\\>" nil t)
      (goto-char (point-min))
      (while (re-search-forward "\\<TEST\\(_[FP]\\)?\\s-*(\\s-*\\([[:alnum:]_]+\\)\\s-*,\\s-*\\([[:alnum:]_]+\\)" nil t)
        (let ((test-class (match-string-no-properties 2))
              (test-name (match-string-no-properties 3))
              (end (line-end-position)))
          (make-button
           end end
           'type 'gtest-button
           'test-class test-class
           'test-name test-name)
          (add-text-properties
           end (1+ end)
           '(display (image :type xpm :data "/* XPM */
static char * run_icon[] = {
\"10 10 2 1\",
\"  c None\",
\". c #22DD22\",
\"    ..    \",
\"   ....   \",
\"  ......  \",
\" ........ \",
\"  ......  \",
\"   ....   \",
\"    ..    \",
\"          \",
\"          \",
\"          \"};") rear-nonsticky t)))))

     ;; Catch2
     ((re-search-forward "\\<TEST_CASE\\>" nil t)
      (goto-char (point-min))
      (while (re-search-forward "TEST_CASE\\s-*\\(\\s-*\"\\([^\"]+\\)\"\\)" nil t)
        (let ((test-name (match-string-no-properties 2))
              (end (line-end-position)))
          (make-button
           end end
           'type 'catch2-button
           'test-name test-name)
          (add-text-properties
           end (1+ end)
           '(display (image :type xpm :data "/* XPM */
static char * run_icon[] = {
\"10 10 2 1\",
\"  c None\",
\". c #22DD22\",
\"    ..    \",
\"   ....   \",
\"  ......  \",
\" ........ \",
\"  ......  \",
\"   ....   \",
\"    ..    \",
\"          \",
\"          \",
\"          \"};") rear-nonsticky t)))))

     ;; pytest
     ((re-search-forward "\\<def\\s-+test_" nil t)
      (goto-char (point-min))
      (while (re-search-forward "\\<def\\s-+\\(test_[[:alnum:]_]+\\)" nil t)
        (let ((test-name (match-string-no-properties 1))
              (end (line-end-position)))
          (make-button
           end end
           'type 'pytest-button
           'test-name test-name)
          (add-text-properties
           end (1+ end)
           '(display (image :type xpm :data "/* XPM */
static char * run_icon[] = {
\"10 10 2 1\",
\"  c None\",
\". c #22DD22\",
\"    ..    \",
\"   ....   \",
\"  ......  \",
\" ........ \",
\"  ......  \",
\"   ....   \",
\"    ..    \",
\"          \",
\"          \",
\"          \"};") rear-nonsticky t))))))))

;; Function to run all tests in a file
(defun run-tests-in-file ()
  "Run all tests in the current file with improved framework detection."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (default-directory (project-root (project-current t)))
         (test-executables (find-test-executables))
         (test-executable (if test-executables (car test-executables) nil)))

    (if (not test-executable)
        (message "Could not find test executable in build directory")

      ;; Look for specific test frameworks markers
      (save-excursion
        (goto-char (point-min))
        (cond
         ;; Catch2 - more specific pattern matching
         ((re-search-forward "\\<TEST_CASE\\>" nil t)
          (let* ((file-basename (file-name-nondirectory file-name))
                 (cmd (format "%s \"[#%s]\"" test-executable (file-name-base file-basename))))
            (message "Running Catch2 tests in file: %s" cmd)
            (compile cmd)))

         ;; Google Test
         ((re-search-forward "\\<TEST\\(_[FP]\\)?\\s-*(\\s-*\\([[:alnum:]_]+\\)\\s-*," nil t)
          (let* ((file-basename (file-name-nondirectory file-name))
                 (cmd (format "%s --gtest_filter=*%s*" test-executable (file-name-base file-basename))))
            (message "Running Google tests in file: %s" cmd)
            (compile cmd)))

         ;; pytest
         ((re-search-forward "\\<def\\s-+test_" nil t)
          (let ((cmd (format "cd %s && python -m pytest %s -v" default-directory file-name)))
            (message "Running pytest tests in file: %s" cmd)
            (compile cmd)))

         ;; Unknown framework - try running the executable directly
         (t
          (message "Framework not specifically detected, running test executable directly")
          (compile test-executable)))))))
;; Create a VS Code-like test explorer
(defun create-test-explorer ()
  "Open a buffer to display and run tests similar to VS Code's Test Explorer."
  (interactive)
  (let ((buffer (get-buffer-create "*Test Explorer*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "╭── Test Explorer ──╮\n\n")

        ;; Find test executables
        (let ((executables (find-test-executables)))
          (if executables
              (progn
                (insert "  Test Executables:\n")
                (dolist (exec executables)
                  (let ((name (file-name-nondirectory exec)))
                    (insert (format "   [Run] %s\n" name))
                    ;; Make the [Run] text clickable
                    (save-excursion
                      (forward-line -1)
                      (re-search-forward "\\[Run\\]" (line-end-position) t)
                      (make-button (match-beginning 0) (match-end 0)
                                   'action (lambda (_)
                                             (let ((cmd (format "%s" exec)))
                                               (message "Running all tests: %s" cmd)
                                               (compile cmd)))
                                   'follow-link t
                                   'help-echo "Run all tests")))))
            (insert "  No test executables found.\n  Build your project first.\n")))

        ;; Add CTest integration
        (let ((default-directory (project-root (project-current t)))
              (build-dir (concat (project-root (project-current t)) "build")))
          (when (file-exists-p (concat build-dir "/CTestTestfile.cmake"))
            (insert "\n  CTest:\n")
            (insert "   [Run All] Run all CTest tests\n")
            (save-excursion
              (forward-line -1)
              (re-search-forward "\\[Run All\\]" (line-end-position) t)
              (make-button (match-beginning 0) (match-end 0)
                           'action (lambda (_)
                                     (let ((cmd (format "cd %s && ctest --output-on-failure" build-dir)))
                                       (message "Running all tests: %s" cmd)
                                       (compile cmd)))
                           'follow-link t
                           'help-echo "Run all tests with CTest"))))

        ;; Footer
        (insert "\n╰──────────────────╯\n")
        (insert "\nPress 'r' to refresh, 'q' to quit\n")

        ;; Set up keybindings for the test explorer
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "r") 'create-test-explorer)
        (local-set-key (kbd "q") 'quit-window)

        (special-mode)))

    ;; Display buffer on the side
    (display-buffer-in-side-window
     buffer
     '((side . right)
       (slot . 0)
       (window-width . 40)
       (window-parameters . ((no-delete-other-windows . t)))))

    buffer))

;; Add hooks for programming modes with test file detection
(dolist (hook '(c++-mode-hook c-mode-hook python-mode-hook))
  (add-hook hook
            (lambda ()
              (when (and (buffer-file-name)
                         (or (string-match-p "test" (buffer-file-name))
                             (string-match-p "_test" (buffer-file-name))))
                (add-test-buttons)))))

;; Key bindings for test functions
(map! :leader
      (:prefix ("t" . "tests")
       :desc "Run all tests in file" "f" #'run-tests-in-file
       :desc "Refresh test buttons" "r" #'add-test-buttons
       :desc "Open test explorer" "e" #'create-test-explorer
       :desc "Run all project tests" "a" (lambda ()
                                           (interactive)
                                           (let* ((default-directory (project-root (project-current t)))
                                                  (build-dir (concat default-directory "build")))
                                             (compile (format "cd %s && ctest --output-on-failure" build-dir))))))

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

;; ===== Enhanced Bazel Configuration with Build Options =====

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
