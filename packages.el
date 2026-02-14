;; ===== ADDITIONAL PACKAGES =====
;; Custom packages not included in Doom modules

;; Build system support
(package! cmake-mode)     ; CMake build files syntax highlighting
(package! bazel :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))  ; Bazel build system

;; Development tools
(package! flycheck-inline)  ; Inline error display
(package! irony)           ; C/C++ completion backend
(package! hydra)           ; Create repeatable key sequences
(package! jsonrpc)         ; JSON-RPC communication protocol
(package! harpoon)
(package! jupyter)
