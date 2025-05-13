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
          "--log=error"))
  (setq lsp-ui-sideline-enable nil)
  (add-to-list 'lsp-language-id-configuration '(cmake-mode . "cmake"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "cmake-language-server")
                    :major-modes '(cmake-mode)
                    :server-id 'cmake-ls))
  ;; Enable flycheck with improved styling
  (setq lsp-diagnostics-provider :flycheck)

  ;; Configure lsp-ui-doc for hovering information
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)

  ;; Ensure parameter hints still work but are better styled
  (setq lsp-inlay-hint-enable t)
  (setq lsp-inlay-hints-parameters t)
  (setq lsp-inlay-hints-types t)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-eldoc-enable-hover t)
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

